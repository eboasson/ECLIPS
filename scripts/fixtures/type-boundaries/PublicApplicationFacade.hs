module PublicApplicationFacade
  ( portableDescriptor,
    freshEnvironment,
    concurrentEnvironment,
    preparedChild,
    finishChild,
    scopedStartup,
    receiptProgress,
    inspectReceiptProgress,
    consumedReceipt,
  ) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word64)
import Eclips.Application qualified as App
import Eclips.Application.Advanced qualified as Advanced
import Eclips.Application.Types.Access (ApplicationStartupAccess, EnvironmentAccess, PrimordialSelection)
import Eclips.Application.Types.Lifecycle (HeraldLocator, PreparedChild)
import Eclips.Public.Types.ReceiptRetirement
  ( ReceiptRetirement,
    ReceiptRetirementError,
    receiptIsRetired,
    receiptRetirement,
    receiptRetirementExceptions,
    receiptRetirementHighWater,
  )

portableDescriptor :: Text -> Either App.ConnectionDescriptorError App.ConnectionDescriptor
portableDescriptor = App.decodeConnectionDescriptor

freshEnvironment :: App.Herald -> IO (Either App.CallError EnvironmentAccess)
freshEnvironment = App.newenv

concurrentEnvironment :: App.Herald -> IO (Advanced.Call EnvironmentAccess)
concurrentEnvironment herald = Advanced.submit herald Advanced.NewEnvironment

preparedChild :: App.Herald -> HeraldLocator -> PrimordialSelection -> IO (Either App.LifecycleCallError PreparedChild)
preparedChild = App.prepareChild

finishChild :: App.Herald -> IO (Either App.LifecycleCallError ())
finishChild = App.endProcess

scopedStartup :: App.ConnectionDescriptor -> IO (Either App.ConnectError ApplicationStartupAccess)
scopedStartup descriptor = App.withHerald descriptor (pure . App.startup)

receiptProgress :: Maybe Word64 -> Set Word64 -> Either ReceiptRetirementError ReceiptRetirement
receiptProgress = receiptRetirement

inspectReceiptProgress :: ReceiptRetirement -> (Maybe Word64, Set Word64)
inspectReceiptProgress progress = (receiptRetirementHighWater progress, receiptRetirementExceptions progress)

consumedReceipt :: Word64 -> ReceiptRetirement -> Bool
consumedReceipt = receiptIsRetired
