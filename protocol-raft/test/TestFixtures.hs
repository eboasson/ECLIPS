{-# LANGUAGE OverloadedStrings #-}

module TestFixtures
  ( genesisDigest,
    otherGenesisDigest,
    localNode,
    remoteNode,
    otherNode,
    connectionContext,
    hello,
    term0,
    term1,
    term2,
    index0,
    index1,
    index2,
    index3,
    noOpEntry,
    applicationEntry,
    stableConfiguration,
    jointConfiguration,
    configurationAppendEntries,
    requestVote,
    requestVoteResponse,
    appendEntries,
    appendEntriesResponse,
    appendEntriesAcceptedResponse,
    appendEntriesMissingPrefixResponse,
    installSnapshot,
    installSnapshotResponse,
    rpcDtos,
    rpcVariantDtos,
    allEnvelopes,
    generatedEnvelopes,
    validConnectionEnvelopes,
    claimBytes,
    mustAdmit,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word64, Word8)
import Eclips.Protocol.Raft.Types

genesisDigest :: RaftGenesisDigestDto
genesisDigest =
  mustAdmit (raftGenesisDigestDto (claimBytes 1))

otherGenesisDigest :: RaftGenesisDigestDto
otherGenesisDigest =
  mustAdmit (raftGenesisDigestDto (claimBytes 2))

localNode :: RaftNodeIdDto
localNode = mustAdmit (raftNodeIdDto (claimBytes 3))

remoteNode :: RaftNodeIdDto
remoteNode = mustAdmit (raftNodeIdDto (claimBytes 4))

otherNode :: RaftNodeIdDto
otherNode = mustAdmit (raftNodeIdDto (claimBytes 5))

connectionContext :: RaftConnectionContext
connectionContext =
  mustAdmit (raftConnectionContext genesisDigest localNode remoteNode)

hello :: RaftHelloClaim
hello = mustAdmit (raftHelloClaim genesisDigest remoteNode localNode)

term0, term1, term2 :: RaftTermDto
term0 = raftTermDto 0
term1 = raftTermDto 1
term2 = raftTermDto 2

index0, index1, index2, index3 :: RaftLogIndexDto
index0 = raftLogIndexDto 0
index1 = raftLogIndexDto 1
index2 = raftLogIndexDto 2
index3 = raftLogIndexDto 3

noOpEntry :: RaftLogEntryDto
noOpEntry = mustAdmit (raftLogEntryDto index1 term1 LeaderNoOpDto)

applicationEntry :: RaftLogEntryDto
applicationEntry =
  mustAdmit
    (raftLogEntryDto index2 term1 (ApplicationBytesDto "opaque-application"))

stableConfiguration :: RaftVotingConfigurationDto
stableConfiguration = mustAdmit (stableRaftConfigurationDto [remoteNode, otherNode])

jointConfiguration :: RaftVotingConfigurationDto
jointConfiguration = mustAdmit (jointRaftConfigurationDto [localNode, remoteNode] [remoteNode, otherNode])

configurationAppendEntries :: RaftRpcDto
configurationAppendEntries =
  mustAdmit
    ( appendEntriesDto
        term1
        remoteNode
        index0
        term0
        [ noOpEntry,
          applicationEntry,
          mustAdmit (raftLogEntryDto index3 term1 (ConfigurationDto jointConfiguration "opaque-joint")),
          mustAdmit (raftLogEntryDto (raftLogIndexDto 4) term1 (ConfigurationDto stableConfiguration "opaque-final"))
        ]
        (raftLogIndexDto 4)
    )

requestVote :: RaftRpcDto
requestVote = mustAdmit (requestVoteDto term2 remoteNode index2 term1)

requestVoteResponse :: RaftRpcDto
requestVoteResponse = mustAdmit (requestVoteResponseDto term2 remoteNode True)

appendEntries :: RaftRpcDto
appendEntries =
  mustAdmit
    ( appendEntriesDto
        term1
        remoteNode
        index0
        term0
        [noOpEntry, applicationEntry]
        index2
    )

appendEntriesResponse :: RaftRpcDto
appendEntriesResponse =
  mustAdmit
    ( appendEntriesResponseDto
        term1
        remoteNode
        False
        index1
        index0
        (Just (ConflictingTermFromDto term1 index2))
    )

appendEntriesAcceptedResponse :: RaftRpcDto
appendEntriesAcceptedResponse =
  mustAdmit
    (appendEntriesResponseDto term1 remoteNode True index2 index0 Nothing)

appendEntriesMissingPrefixResponse :: RaftRpcDto
appendEntriesMissingPrefixResponse =
  mustAdmit
    ( appendEntriesResponseDto
        term1
        remoteNode
        False
        index0
        index0
        (Just (MissingSuffixFromDto index1))
    )

installSnapshot :: RaftRpcDto
installSnapshot = mustAdmit (installSnapshotDto term2 remoteNode (mustAdmit (raftCheckpointDto index3 term1 (Just (index2, term1)) jointConfiguration "application-checkpoint")))

installSnapshotResponse :: RaftRpcDto
installSnapshotResponse = mustAdmit (installSnapshotResponseDto term2 remoteNode index3 True)

rpcDtos :: [RaftRpcDto]
rpcDtos =
  [ requestVote,
    requestVoteResponse,
    appendEntries,
    appendEntriesResponse,
    installSnapshot,
    installSnapshotResponse
  ]

-- | Every RPC constructor plus both append-success and repair-hint variants.
rpcVariantDtos :: [RaftRpcDto]
rpcVariantDtos =
  rpcDtos
    <> [ appendEntriesAcceptedResponse,
         appendEntriesMissingPrefixResponse,
         configurationAppendEntries
       ]

allEnvelopes :: [RaftProtocolEnvelope]
allEnvelopes = RaftHello hello : fmap RaftRpc rpcVariantDtos

-- | Exercise every current envelope arm while varying terms, indexes, payload,
-- vote result, append result, and optional conflict-hint shape.
generatedEnvelopes :: Word64 -> Bool -> [RaftProtocolEnvelope]
generatedEnvelopes counter positiveResult =
  [ RaftHello hello,
    RaftRpc
      (mustAdmit (requestVoteDto generatedTerm remoteNode generatedIndex generatedTerm)),
    RaftRpc
      (mustAdmit (requestVoteResponseDto generatedTerm remoteNode positiveResult)),
    RaftRpc
      ( mustAdmit
          ( appendEntriesDto
              generatedTerm
              remoteNode
              previousIndex
              previousTerm
              [generatedEntry]
              generatedIndex
          )
      ),
    RaftRpc generatedAppendResponse,
    RaftRpc (mustAdmit (installSnapshotDto generatedTerm remoteNode (mustAdmit (raftCheckpointDto generatedIndex generatedTerm (if positiveResult then Just (generatedIndex, generatedTerm) else Nothing) (if positiveResult then jointConfiguration else stableConfiguration) (ByteString.singleton (fromIntegral positive)))))),
    RaftRpc (mustAdmit (installSnapshotResponseDto generatedTerm remoteNode generatedIndex positiveResult)),
    RaftRpc
      ( mustAdmit
          ( appendEntriesDto
              generatedTerm
              remoteNode
              previousIndex
              previousTerm
              [mustAdmit (raftLogEntryDto generatedIndex generatedTerm (ConfigurationDto (if positiveResult && positive > 1 then stableConfiguration else jointConfiguration) (ByteString.singleton (fromIntegral positive))))]
              generatedIndex
          )
      )
  ]
  where
    positive = counter `mod` 4096 + 1
    generatedTerm = raftTermDto positive
    generatedIndex = raftLogIndexDto positive
    previousIndex = raftLogIndexDto (positive - 1)
    previousTerm
      | positive == 1 = term0
      | otherwise = generatedTerm
    generatedEntry =
      mustAdmit
        ( raftLogEntryDto
            generatedIndex
            generatedTerm
            ( if positiveResult
                then ApplicationBytesDto (ByteString.singleton (fromIntegral positive))
                else LeaderNoOpDto
            )
        )
    generatedAppendResponse
      | positiveResult =
          mustAdmit
            (appendEntriesResponseDto generatedTerm remoteNode True generatedIndex index0 Nothing)
      | otherwise =
          mustAdmit
            ( appendEntriesResponseDto
                generatedTerm
                remoteNode
                False
                previousIndex
                index0
                (Just (MissingSuffixFromDto generatedIndex))
            )

-- | One legal stream: matching hello followed by every current RPC arm.
validConnectionEnvelopes :: [RaftProtocolEnvelope]
validConnectionEnvelopes = allEnvelopes

claimBytes :: Word8 -> ByteString
claimBytes value = ByteString.replicate 32 value

mustAdmit :: (Show problem) => Either problem value -> value
mustAdmit = either (error . show) id
