#!/usr/bin/env python3
"""macOS process-tree memory watchdog and accounting, without a profiling build.

Usage: scripts/run-memory-guard.py --output FILE [--seconds N] [--heap 768m]
                                 [--process-mib 900] [--tree-mib 4096] -- COMMAND...

The sampled RSS guard is a safety net, not a hard OS memory limit. Children
inherit GHCRTS=-M768m by default; --heap existing measures older executables
without RTS option support under the RSS guard alone. Logs go to FILE.log.
Session scans include reparented children and separate child process groups.
Commands must not daemonize with setsid; this is not a security boundary.
"""
import argparse
import ctypes as C
import hashlib
import errno
import math
import re
import shutil
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import time
import threading
import select

U32, I32, U64 = C.c_uint32, C.c_int32, C.c_uint64
class Bsd(C.Structure):
    _fields_ = [(n,U32) for n in ('flags','status','xstatus','pid','ppid','uid','gid','ruid','rgid','svuid','svgid','rfu')] + [('comm',C.c_char*16),('name',C.c_char*32)] + [(n,U32) for n in ('nfiles','pgid','jobc','tdev','tpgid')] + [('nice',I32),('start_sec',U64),('start_usec',U64)]
class Task(C.Structure):
    _fields_ = [(n,U64) for n in ('virtual','resident','user','system','threads_user','threads_system')] + [(n,I32) for n in ('policy','faults','pageins','cow','sent','received','machcalls','unixcalls','switches','threads','running','priority')]
class All(C.Structure):
    _fields_ = [('bsd',Bsd),('task',Task)]
class Timebase(C.Structure):
    _fields_ = [('numer',U32),('denom',U32)]
assert (C.sizeof(Bsd),C.sizeof(Task),C.sizeof(All))==(136,96,232)
lib=C.CDLL('/usr/lib/libproc.dylib',use_errno=True)
lib.proc_pidinfo.argtypes=[C.c_int,C.c_int,U64,C.c_void_p,C.c_int]
lib.proc_pidinfo.restype=C.c_int
lib.proc_listpids.argtypes=[U32,U32,C.c_void_p,C.c_int]
lib.proc_listpids.restype=C.c_int
lib.proc_pidpath.argtypes=[C.c_int,C.c_void_p,U32]
lib.proc_pidpath.restype=C.c_int
system=C.CDLL('/usr/lib/libSystem.B.dylib')
tb=Timebase()
assert system.mach_timebase_info(C.byref(tb))==0
seconds_per_tick=tb.numer/tb.denom/1e9

def info(pid):
    value=All()
    C.set_errno(0)
    got=lib.proc_pidinfo(pid,2,0,C.byref(value),C.sizeof(value))
    error=C.get_errno()
    if got==C.sizeof(value): return value
    if error==errno.ESRCH: return None
    # Exited but unreaped processes have no live task/RSS. Their BSD identity
    # can remain visible until their parent reaps them (SZOMB = 5).
    bsd=Bsd()
    bsd_got=lib.proc_pidinfo(pid,3,0,C.byref(bsd),C.sizeof(bsd))
    if bsd_got==C.sizeof(bsd) and bsd.status==5: return None
    try: os.kill(pid,0)
    except ProcessLookupError: return None
    raise OSError(error or errno.EIO, 'cannot sample live process', pid)

def list_pids(kind, argument):
    capacity=64
    while True:
        values=(C.c_int*capacity)()
        C.set_errno(0)
        got=lib.proc_listpids(kind,argument,values,C.sizeof(values))
        error=C.get_errno()
        if got<0 or (got==0 and error):
            if kind==6 and error==errno.ESRCH: return []
            raise OSError(error or errno.EIO, 'cannot enumerate processes')
        if got<C.sizeof(values): return [int(x) for x in values[:got//C.sizeof(C.c_int)] if x]
        capacity*=2

def executable(pid, fallback):
    buf=C.create_string_buffer(4096)
    return os.path.basename(buf.value.decode(errors='replace')) if lib.proc_pidpath(pid,buf,len(buf))>0 else fallback

def compiler_executable(name):
    # Classify the actual executable, never its shell/parent or command text.
    # Refresh after exec so a sampled shell cannot hide its later GHC lifetime.
    return bool(re.fullmatch(
        r'(?:ghc(?:-[0-9][0-9.]*)?|ghc-iserv(?:-dyn|-prof)?|'
        r'(?:haddock|hsc2hs)(?:-ghc-[0-9][0-9.]*)?|'
        r'clang(?:\+\+)?(?:-[0-9]+)?|gcc(?:-[0-9]+)?|g\+\+(?:-[0-9]+)?|'
        r'cc|c\+\+|ld|ld64\.lld|ld\.lld)', name))

def session_pids(session):
    members=[]
    for pid in list_pids(1,0):
        try:
            if os.getsid(pid)==session: members.append(pid)
        except ProcessLookupError: pass
    return members

def identity(pid,item):
    return f'{pid}:{item.bsd.start_sec}:{item.bsd.start_usec}'

def snapshot(root,known,scan_session=False):
    pending=[(r['pid'],key) for key,r in known.items()]
    pending.append((root,None))
    if scan_session: pending.extend((pid,None) for pid in session_pids(root))
    seen=set(); current={}
    while pending:
        pid,expected=pending.pop()
        if pid in seen: continue
        item=info(pid)
        if item is None: continue
        key=identity(pid,item)
        if expected is not None and key!=expected: continue
        if expected is None:
            try:
                if os.getsid(pid)!=root: continue
            except ProcessLookupError: continue
        seen.add(pid)
        if key not in known:
            fallback=bytes(item.bsd.name).split(b'\0')[0].decode(errors='replace')
            known[key]={'pid':pid,'ppid':item.bsd.ppid,'name':executable(pid,fallback),'start_sec':item.bsd.start_sec,'start_usec':item.bsd.start_usec,'peak_rss_bytes':0,'last_cpu_s':0,'samples':0}
        r=known[key]
        fallback=bytes(item.bsd.name).split(b'\0')[0].decode(errors='replace')
        r['name']=executable(pid,fallback)
        r['compiler']=compiler_executable(r['name'])
        r['peak_rss_bytes']=max(r['peak_rss_bytes'],item.task.resident)
        r['last_cpu_s']=max(r['last_cpu_s'],(item.task.user+item.task.system)*seconds_per_tick)
        r['last_cpu_user_s']=max(r.get('last_cpu_user_s',0),item.task.user*seconds_per_tick)
        r['last_cpu_system_s']=max(r.get('last_cpu_system_s',0),item.task.system*seconds_per_tick)
        r['samples']+=1
        current[key]=item.task.resident
        pending.extend((child,None) for child in list_pids(6,pid))
    return current

def signal_known(known,signum,selected=None,errors=None):
    for key in list(known) if selected is None else selected:
        record=known[key]
        try:
            item=info(record['pid'])
            if item and identity(record['pid'],item)==key: os.kill(record['pid'],signum)
        except ProcessLookupError: pass
        except OSError as error:
            if errors is None: raise
            errors.add(str(error))

def cleanup(child,known):
    errors=set(); usage=None; deadline=time.monotonic()+3
    while True:
        try: snapshot(child.pid,known,scan_session=True)
        except OSError as error: errors.add(str(error))
        # Validate a live group/session member before signalling the root group.
        for key,record in list(known.items()):
            try:
                item=info(record['pid'])
                if item and identity(record['pid'],item)==key and item.bsd.pgid==child.pid and os.getsid(record['pid'])==child.pid:
                    os.killpg(child.pid,signal.SIGKILL)
                    break
            except ProcessLookupError: pass
            except OSError as error: errors.add(str(error))
        signal_known(known,signal.SIGKILL,errors=errors)
        if child.returncode is None:
            # An unreaped direct child cannot have had its PID reused.
            try: os.kill(child.pid,signal.SIGKILL)
            except ProcessLookupError: pass
            except OSError as error: errors.add(str(error))
            try:
                pid,status,observed=os.wait4(child.pid,os.WNOHANG)
                if pid:
                    usage=observed
                    child.returncode=os.waitstatus_to_exitcode(status)
            except OSError as error: errors.add(str(error))
        alive=[]
        for key,record in list(known.items()):
            try:
                item=info(record['pid'])
                if item and identity(record['pid'],item)==key:
                    alive.append({'pid':record['pid'],'name':record['name']})
            except OSError as error:
                errors.add(str(error))
                alive.append({'pid':record['pid'],'name':record['name'],'unreadable':True})
        if (not alive and child.returncode is not None) or time.monotonic()>=deadline:
            return usage,alive,sorted(errors)
        time.sleep(0.05)

def run(argv):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--seconds', type=float, default=180)
    parser.add_argument('--heap', default='768m')
    parser.add_argument('--process-mib', type=float, default=900)
    parser.add_argument('--tree-mib', type=float, default=4096)
    parser.add_argument('--compiler-mib', type=float, help='separate RSS threshold for actual compiler/linker executables')
    parser.add_argument('--rts-stats', action='store_true', help='stream ordinary machine-readable RTS exit statistics from every inheriting process to FILE.rts')
    parser.add_argument('--phase-prefix', action='append', default=[], help='record elapsed and sampled tree CPU when a workload output line contains this marker')
    parser.add_argument('command', nargs=argparse.REMAINDER)
    args = parser.parse_args(argv)
    argv = args.command
    if argv and argv[0] == '--': argv = argv[1:]
    if not argv: parser.error('a command is required')
    try: interval=float(os.environ.get('ECLIPS_RESOURCE_INTERVAL','0.05'))
    except ValueError: parser.error('sample interval must be positive and finite')
    for name,value in [('seconds',args.seconds),('process-mib',args.process_mib),('tree-mib',args.tree_mib),('sample interval',interval)]:
        if not math.isfinite(value) or value<=0: parser.error(name+' must be positive and finite')
    if args.compiler_mib is not None and (not math.isfinite(args.compiler_mib) or args.compiler_mib<=0):
        parser.error('compiler-mib must be positive and finite')
    resolved=shutil.which(argv[0])
    if resolved is None: parser.error('command not found: '+argv[0])
    digest=hashlib.sha256()
    with Path(resolved).open('rb') as binary:
        for chunk in iter(lambda:binary.read(1024*1024),b''): digest.update(chunk)
    out=args.output.resolve()
    out.parent.mkdir(parents=True,exist_ok=True)
    started=time.monotonic(); harness_start=time.process_time()
    environment = os.environ.copy()
    if args.heap != 'existing':
        environment['GHCRTS'] = environment.get('GHCRTS', '') + ' -M' + args.heap
    # A FIFO has no shared seek position to truncate. Every inheriting RTS opens
    # it independently, including test helpers spawned through getExecutablePath.
    # The collector streams bytes to disk; it retains no program state or trace.
    stats_fifo=out.with_suffix('.rts.fifo')
    stats_output=out.with_suffix('.rts')
    stats_stop=threading.Event(); stats_thread=None; stats_fd=None; stats_errors=[]
    if args.rts_stats:
        os.mkfifo(stats_fifo,0o600)
        stats_fd=os.open(stats_fifo,os.O_RDWR|os.O_NONBLOCK)
        def collect_stats():
            try:
                with stats_output.open('wb') as handle:
                    while True:
                        if select.select([stats_fd],[],[],0.05)[0]:
                            chunk=os.read(stats_fd,65536)
                            if chunk: handle.write(chunk)
                        elif stats_stop.is_set(): break
            except BaseException as error:
                stats_errors.append(str(error))
        stats_thread=threading.Thread(target=collect_stats,name='rts-statistics')
        stats_thread.start()
        environment['GHCRTS']=environment.get('GHCRTS','')+' -t'+str(stats_fifo)+' --machine-readable'
    log=timeline=child=usage=phase_reader=phase_output=None
    phase_pending=''
    known={}; count=0; peak=0; peak_members=[]; max_gap=0; last_sample=started
    last_session_scan=float('-inf')
    reason=stopping=failure=None
    cleanup_errors=[]; alive=[]; interrupted=[]; old_handlers={}
    def request_stop(signum, _frame):
        if not interrupted: interrupted.append(signum)
    try:
        for signum in (signal.SIGINT,signal.SIGTERM):
            old_handlers[signum]=signal.signal(signum,request_stop)
        log=out.with_suffix('.log').open('w')
        if args.phase_prefix:
            phase_reader=out.with_suffix('.log').open()
            phase_output=out.with_suffix('.phases.jsonl').open('w',buffering=1)
        timeline=out.with_suffix('.samples.jsonl').open('w',buffering=1)
        child=subprocess.Popen(argv,env=environment,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
        out.with_suffix('.running.json').write_text(json.dumps({'pid':child.pid,'argv':argv,'cwd':os.getcwd(),'started':time.time()}))
        while True:
            now=time.monotonic(); max_gap=max(max_gap,now-last_sample); last_sample=now
            scan_session=now-last_session_scan>=0.5
            current=snapshot(child.pid,known,scan_session); count+=1
            if phase_reader is not None:
                phase_pending+=phase_reader.read()
                lines=phase_pending.split('\n'); phase_pending=lines.pop()
                for line in lines:
                    if any(prefix in line for prefix in args.phase_prefix):
                        phase_output.write(json.dumps({'elapsed_s':now-started,'sampled_tree_cpu_s':sum(p['last_cpu_s'] for p in known.values()),'line':line})+'\n')
            if scan_session: last_session_scan=now
            rss=sum(current.values())
            timeline.write(json.dumps({'elapsed_s': now-started, 'rss': current})+'\n')
            if reason is None:
                excessive = [k for k, v in current.items() if v >
                             (args.compiler_mib if args.compiler_mib is not None and known[k]['compiler'] else args.process_mib)*2**20]
                if excessive: reason = 'per-process RSS threshold: ' + ', '.join(str(known[k]['pid']) for k in excessive)
                elif rss > args.tree_mib*2**20: reason = 'total process-tree RSS threshold'
                elif now-started > args.seconds: reason = 'time limit'
                elif interrupted: reason = 'watchdog interrupted'
                if reason:
                    stopping = now
                    print('MEMORY GUARD: '+reason, file=sys.stderr, flush=True)
                    # Stop a growing offender immediately, then allow the fixture a
                    # short opportunity to unwind its process scopes.
                    signal_known(known,signal.SIGKILL,selected=excessive)
                    signal_known(known,signal.SIGINT)
            if stats_errors:
                raise RuntimeError('RTS statistics collector failed: '+', '.join(stats_errors))
            if stopping is not None and now-stopping >= 2:
                break
            if rss>peak:
                peak=rss
                peak_members=[{'pid':known[k]['pid'],'name':known[k]['name'],'rss_bytes':v} for k,v in current.items()]
            pid,status,observed=os.wait4(child.pid,os.WNOHANG)
            if pid:
                usage=observed
                child.returncode=os.waitstatus_to_exitcode(status)
                break
            time.sleep(max(0,interval-(time.monotonic()-now)))
    except BaseException as error:
        failure=f'{type(error).__name__}: {error}'
        reason=reason or 'watchdog failure'
    finally:
        if child is not None:
            try:
                final_usage,alive,cleanup_errors=cleanup(child,known)
                usage=usage or final_usage
            except BaseException as error:
                cleanup_errors.append(f'cleanup failed: {type(error).__name__}: {error}')
                try:
                    if child.returncode is None: os.kill(child.pid,signal.SIGKILL)
                except OSError: pass
        for handle in (timeline,log,phase_reader,phase_output):
            if handle is not None:
                try: handle.close()
                except OSError as error: cleanup_errors.append(str(error))
        for signum,handler in old_handlers.items(): signal.signal(signum,handler)
        if stats_thread is not None:
            stats_stop.set(); stats_thread.join(timeout=2)
            if stats_thread.is_alive(): cleanup_errors.append('RTS statistics collector did not stop')
            if stats_fd is not None: os.close(stats_fd)
            stats_fifo.unlink(missing_ok=True)
            cleanup_errors.extend(stats_errors)
    elapsed=time.monotonic()-started
    if alive or cleanup_errors: reason=reason or 'watchdog cleanup incomplete'
    result={'label':os.environ.get('ECLIPS_RESOURCE_LABEL',''),'argv':argv,'cwd':os.getcwd(),'exit_code':child.returncode if child is not None else None,'elapsed_s':elapsed,'cpu_user_s':usage.ru_utime if usage else None,'cpu_system_s':usage.ru_stime if usage else None,'cpu_total_s':usage.ru_utime+usage.ru_stime if usage else None,'largest_process_peak_rss_bytes':usage.ru_maxrss if usage else None,'sampled_peak_tree_rss_bytes':peak,'sampled_peak_tree_members':peak_members,'processes':list(known.values()),'sample_interval_s':interval,'sample_count':count,'max_sample_gap_s':max_gap,'harness_cpu_s':time.process_time()-harness_start,'unreaped_descendants_after_exit':alive,'mach_timebase_numer':tb.numer,'mach_timebase_denom':tb.denom,'binary_sha256':digest.hexdigest(),'binary_path':str(Path(resolved).resolve()),'watchdog_failure':failure,'cleanup_errors':cleanup_errors}
    result.update({'guard_reason': reason, 'heap_limit': args.heap,
                   'compiler_rss_limit_mib': args.compiler_mib,
                   'process_rss_limit_mib': args.process_mib, 'tree_rss_limit_mib': args.tree_mib})
    if args.rts_stats: result['rts_statistics_path']=str(stats_output)
    try:
        out.write_text(json.dumps(result,indent=2)+'\n')
        out.with_suffix('.running.json').unlink(missing_ok=True)
    except OSError as error:
        failure=failure or f'could not save watchdog result: {error}'
    print(f"RESOURCE: {result['label']} CPU={result['cpu_total_s']}s elapsed={elapsed:.3f}s treeRSS~={peak/2**20:.1f}MiB exit={result['exit_code']} guard={reason} failure={failure}",file=sys.stderr,flush=True)
    if failure or cleanup_errors or alive or child is None or child.returncode is None: return 125
    return 124 if reason else (child.returncode if child.returncode>=0 else 128-child.returncode)
if __name__=='__main__':
    raise SystemExit(run(sys.argv[1:]))
