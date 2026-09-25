module AdministrationReceiptRetirementProperties (tests) where

import Data.ByteString qualified as Bytes
import Data.Set qualified as Set
import Data.Word (Word8)
import Eclips.Application.Types.Lifecycle
import Eclips.Domain.Identity (mkHeraldEpoch)
import Eclips.Herald.Administration qualified as Admin
import Eclips.Herald.Administration.State qualified as Owner
import Eclips.Public.Types.ReceiptRetirement
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "administration receipt lifetimes"
    [ testProperty "one pending result does not pin a generated completed suffix" propPendingHole,
      testProperty "fresh bindings cannot retire or receive an older binding's pending work" propScopeIsolation,
      testProperty "closed cleanup matches explicit release across generated pending and completed results" propClosedCleanupMatchesRelease
    ]

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

initial :: Owner.State
initial = Owner.initialState (checked (mkHeraldEpoch (Bytes.replicate 32 31)))

open :: Owner.State -> (Owner.State, Admin.AdministrationBinding)
open = Owner.commitConfiguredAdministrationOpen . checked . Owner.prepareConfiguredAdministrationOpen

preparation :: ChildPreparation
preparation = checked (childPreparation (Bytes.replicate 32 41) 1 1)

pending :: Admin.AdministrationBinding -> Owner.State -> Owner.State
pending binding state = fst (Owner.commitPreparationCancellation (checked (Owner.preparePreparationCancellation (Admin.adminCorrelationIdForBinding binding 0) preparation (LifecyclePending []) state)))

propPendingHole :: Word8 -> Property
propPendingHole seed =
  let count = 1 + fromIntegral seed `mod` 64
      (opened, binding) = open initial
      start = pending binding opened
      correlation = Admin.adminCorrelationIdForBinding binding
      progress index = checked (receiptRetirement (Just index) (Set.singleton 0))
      advance state index =
        let (completed, _) = Owner.retainVoterPreparationRejection (correlation index) state
         in checked (Owner.retireAdministrationReceipts binding (progress index) completed)
      states = scanl advance start [1 .. count]
      final = foldl (\_ state -> state) start states
      stale = checked (receiptRetirement (Just count) (Set.fromList [0, 1]))
      pendingResult = Just (Admin.AdminPreparationCancellation (LifecyclePending []))
   in conjoin
        [ conjoin
            [ counterexample ("retained suffix at " <> show index)
                $ conjoin
                  [ length (Owner.administrationWitnessCommands (Owner.administrationStateWitness state)) === 1,
                    Owner.lookupAdministrationResultStatus (correlation 0) state === pendingResult,
                    length (Owner.preparationCancellationEntries state) === 1,
                    Owner.voterPreparationRejectionEntries state === [],
                    Owner.administrationReceiptRetirement state === progress index
                  ]
            | (index, state) <- zip [1 ..] (drop 1 states)
            ],
          property (Owner.retireAdministrationReceipts binding stale final == Right final),
          property (Owner.retireAdministrationReceipts binding (receiptRetirementPrefix (Just count)) final == Left ()),
          property (Owner.administrationCorrelationRetired binding (correlation 1) final),
          property (not (Owner.administrationCorrelationRetired binding (correlation 0) final))
        ]

propScopeIsolation :: Word8 -> Property
propScopeIsolation seed =
  let ordinal = fromIntegral seed
      (first, bindingA) = open initial
      waiting = pending bindingA first
      correlationA = Admin.adminCorrelationIdForBinding bindingA 0
      (second, bindingB) = open waiting
      cleaned = Owner.retireClosedAdministrationReceipts second
      correlationB = Admin.adminCorrelationIdForBinding bindingB ordinal
      (completed, _) = Owner.retainVoterPreparationRejection correlationB cleaned
      retired = checked (Owner.retireAdministrationReceipts bindingB (receiptRetirementPrefix (Just ordinal)) completed)
      settled = checked (Owner.settlePreparationCancellation correlationA (LifecycleCompleted ChildCancelled) retired)
      final = Owner.retireClosedAdministrationReceipts settled
   in conjoin
        [ property (correlationA /= Admin.adminCorrelationIdForBinding bindingB 0),
          Owner.lookupAdministrationResultStatus correlationA retired === Just (Admin.AdminPreparationCancellation (LifecyclePending [])),
          Owner.lookupAdministrationResultStatus correlationB retired === Nothing,
          property (Owner.retireAdministrationReceipts bindingA (receiptRetirementPrefix (Just 0)) retired == Left ()),
          Owner.preparationCancellationEntries final === [],
          length (Owner.administrationWitnessCommands (Owner.administrationStateWitness final)) === 0,
          property (Owner.retireClosedAdministrationReceipts final == final)
        ]

propClosedCleanupMatchesRelease :: [Bool] -> Property
propClosedCleanupMatchesRelease generated =
  let outcomes = take 40 (False : True : generated)
      (opened, oldBinding) = open initial
      correlation = Admin.adminCorrelationIdForBinding oldBinding
      retain state (ordinal, completed)
        | completed = fst (Owner.retainVoterPreparationRejection (correlation ordinal) state)
        | otherwise = fst (Owner.commitPreparationCancellation (checked (Owner.preparePreparationCancellation (correlation ordinal) preparation (LifecyclePending []) state)))
      original = foldl retain opened (zip [0 ..] outcomes)
      high = fromIntegral (length outcomes - 1)
      pendingOrdinals = Set.fromList [ordinal | (ordinal, False) <- zip [0 ..] outcomes]
      progress = checked (receiptRetirement (Just high) pendingOrdinals)
      explicitlyReleased = checked (Owner.retireAdministrationReceipts oldBinding progress original)
      (expected, _) = open explicitlyReleased
      (rebound, newBinding) = open original
      cleaned = Owner.retireClosedAdministrationReceipts rebound
      replayed = foldl (\state _ -> Owner.retireClosedAdministrationReceipts state) cleaned outcomes
      stale = checked (receiptRetirement (Just high) (Set.fromList [0 .. high]))
   in conjoin
        [ counterexample "current-binding results changed before closure" (property (Owner.retireClosedAdministrationReceipts original == original)),
          counterexample "closed cleanup differs from explicit terminal release" (property (cleaned == expected)),
          counterexample "repeated closed cleanup changes settled owners" (property (replayed == cleaned)),
          property (Owner.retireAdministrationReceipts oldBinding stale replayed == Left ()),
          Owner.administrationReceiptRetirement replayed === mempty,
          Owner.currentConfiguredAdministrationBinding replayed === Just newBinding,
          conjoin
            [ Owner.lookupAdministrationResultStatus (correlation ordinal) replayed
                === if completed then Nothing else Just (Admin.AdminPreparationCancellation (LifecyclePending []))
            | (ordinal, completed) <- zip [0 ..] outcomes
            ]
        ]
