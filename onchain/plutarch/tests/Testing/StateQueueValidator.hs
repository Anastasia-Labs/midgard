{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.StateQueueValidator
Description : Tests for the pure seams of @validators/state-queue.ak@.

Aiken exposes six predicates from this validator as @pub fn@ and tests them
directly, precisely because they are the parts worth isolating: the structural
conditions a committed block must meet, independent of the transaction carrying
it. The first five groups here carry the same names, fixtures and mutations as
the Aiken @q49_l295_*@ tests, so a divergence between the two implementations
fails the same case on each side.

The merge-side groups mirror @validators/state-queue-merge.test.ak@: every
restated commitment, each settlement-presence route, and the exact redeemer
index/purpose binding.
-}
module Testing.StateQueueValidator (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Interval (Extended (Finite), Interval (..), LowerBound (..), UpperBound (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), singleton)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  Datum (..),
  OutputDatum (..),
  POSIXTime (..),
  PubKeyHash (..),
  Redeemer (..),
  ScriptHash (..),
  ScriptPurpose (Minting),
  StakingCredential (..),
  TxOut (..),
 )
import PlutusTx.Builtins (blake2b_224, dataToBuiltinData, fromBuiltin, serialiseData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PRedeemer, PScriptPurpose (..))
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.LedgerState (PConfirmedState, PHeaderV1)
import Midgard.AvailabilityChallenge qualified as Availability
import Midgard.Common.Utils qualified as Utils
import Midgard.StateQueue (PMintRedeemer, PStateQueueNode)
import Midgard.Validators.StateQueue (
  pcommitBlockHeaderCarriesConfirmedStateV1,
  pcommitBlockHeaderCarriesPreviousBlockV1,
  pcommitBlockHeaderOperatorIsLegitimateV1,
  pcommitBlockHeaderOutputIsValidV1,
  pheaderCarriesL2Material,
  pmergeCommitmentsMatchHeader,
  pmergeSettlementBindingMatchesHeader,
  pmergeSettlementIdAtRoute,
  pavailabilityStatusUpdateIsAuthorizedV1,
  pfraudProverRewardOutputIsExactV1,
  pfraudProverRewardRoutingIsExactV1,
  pnoOutputPaysFraudProverRewardV1,
  prouteFraudProverRewardV1,
  pstateQueueHeadAllowsAppendV1,
  punattestedBlockTimeoutElapsedV1,
 )
import Testing.Eval (passertEval, pfails)
import Testing.ScriptContextBuilder (mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "State Queue Validator Tests"
    [ headerShapeTests
    , scalarTests
    , operatorTests
    , appendFenceTests
    , unattestedTimeoutTests
    , fraudProverRewardTests
    , previousHeaderTests
    , confirmedGenesisTests
    , confirmedOrdinaryTests
    , l2MaterialTests
    , mergeCommitmentTests
    , settlementBindingTests
    , settlementRedeemerRouteTests
    , availabilityStatusAuthorizationTests
    ]

fraudProverRewardTests :: TestTree
fraudProverRewardTests =
  testGroup
    "fraud prover reward routing"
    [ testCase "accepts only the exact enterprise ADA-only reward output" $
        holds $
          rewardOutputIsExact (rewardOutput fraudProverAddress rewardAmount NoOutputDatum Nothing)
            #&& pnot # rewardOutputIsExact (rewardOutput fraudProverAddress (rewardAmount + 1) NoOutputDatum Nothing)
            #&& pnot # rewardOutputIsExact (rewardOutput fraudProverAddress (rewardAmount - 1) NoOutputDatum Nothing)
            #&& pnot # rewardOutputIsExact (rewardOutput otherFraudProverAddress rewardAmount NoOutputDatum Nothing)
            #&& pnot # rewardOutputIsExact (rewardOutput delegatedFraudProverAddress rewardAmount NoOutputDatum Nothing)
            #&& pnot # rewardOutputIsExact (rewardOutput scriptFraudProverAddress rewardAmount NoOutputDatum Nothing)
            #&& pnot # rewardOutputIsExact smuggledRewardOutput
            #&& pnot # rewardOutputIsExact (rewardOutput fraudProverAddress rewardAmount NoOutputDatum (Just foreignScriptHash))
            #&& pnot # rewardOutputIsExact (rewardOutput fraudProverAddress rewardAmount (OutputDatum $ Datum $ dataToBuiltinData $ PD.I 0) Nothing)
    , testCase "routes through the exact named output and permits omission only for zero" $
        holds $
          routing rewardOutputs (pcon $ PDJust $ pdata 1) rewardAmount
            #&& pnot # routing rewardOutputs (pcon $ PDJust $ pdata 0) rewardAmount
            #&& routing rewardOutputs (pcon PDNothing) 0
            #&& pnot # routing rewardOutputs (pcon PDNothing) rewardAmount
            #&& pnot
              # routing
                [ rewardOutput fraudProverAddress rewardAmount NoOutputDatum Nothing
                , rewardOutput fraudProverAddress (rewardAmount + 1) NoOutputDatum Nothing
                ]
                (pcon $ PDJust $ pdata 0)
                rewardAmount
    , testCase "rejects an out-of-range reward output index" $
        pfails $ routing [rewardOutput fraudProverAddress rewardAmount NoOutputDatum Nothing] (pcon $ PDJust $ pdata 7) rewardAmount
    , testCase "forbids reward-shaped payments on the already-slashed arm" $
        holds $
          noReward [unrelatedOutput]
            #&& pnot # noReward rewardOutputs
            #&& noReward [rewardOutput fraudProverAddress (rewardAmount + 1) NoOutputDatum Nothing]
    , testCase "route wrapper preserves permissionless reward semantics" $
        holds $
          route rewardOutputs (pcon $ PDJust $ pdata 1) rewardAmount
            #&& route rewardOutputs (pcon PDNothing) 0
            #&& pnot # route rewardOutputs (pcon PDNothing) rewardAmount
    ]
  where
    rewardOutputIsExact output =
      pfraudProverRewardOutputIsExactV1 (pconstant output) (pconstant fraudProver) (pconstant rewardAmount)
    routing outputs index amount =
      pfraudProverRewardRoutingIsExactV1 (pconstant outputs) index (pconstant fraudProver) (pconstant amount)
    noReward outputs =
      pnoOutputPaysFraudProverRewardV1 (pconstant outputs) (pconstant fraudProver) (pconstant rewardAmount)
    route outputs index amount =
      prouteFraudProverRewardV1 (pconstant outputs) index (pconstant fraudProver) (pconstant amount)

appendFenceTests :: TestTree
appendFenceTests =
  testGroup
    "stateQueueHeadAllowsAppendV1"
    [ testCase "accepts an unattested head immediately before the timeout boundary" $
        holds $ appendAllowed unattestedStatus (closed 3_599_199 3_600_199)
    , testCase "rejects an unattested head at the timeout boundary" $
        fails $ appendAllowed unattestedStatus (closed 3_599_200 3_600_200)
    , testCase "accepts an attested head after the timeout boundary" $
        holds $ appendAllowed (attestedStatus daBondAssetName) (closed 3_600_200 3_601_200)
    , testCase "rejects a challenged head before the attestation timeout" $
        fails $
          appendAllowed
            (challengedStatus daBondAssetName challengeAssetName)
            (closed 3_599_199 3_600_199)
    , testCase "accepts a published head after the timeout boundary" $
        holds $ appendAllowed publishedStatus (closed 3_600_200 3_601_200)
    ]
  where
    appendAllowed status validityRange =
      pstateQueueHeadAllowsAppendV1
        (stateQueueNodeTerm control status)
        (pdata $ pconstant availabilityPolicy)
        (pconstant validityRange)

unattestedTimeoutTests :: TestTree
unattestedTimeoutTests =
  testGroup
    "unattestedBlockTimeoutElapsedV1"
    [ testCase "accepts the exact timeout boundary" $
        holds $ elapsed controlHash controlHash unattestedStatus (closed 3_600_200 3_601_200)
    , testCase "rejects one millisecond before the timeout boundary" $
        fails $ elapsed controlHash controlHash unattestedStatus (closed 3_600_199 3_601_199)
    , testCase "rejects a different requested header identity" $
        fails $ elapsed headerHashB controlHash unattestedStatus (closed 3_600_200 3_601_200)
    , testCase "rejects an unauthenticated node key" $
        fails $ elapsed controlHash headerHashB unattestedStatus (closed 3_600_200 3_601_200)
    , testCase "rejects a node whose attestation won the race" $
        fails $ elapsed controlHash controlHash (attestedStatus daBondAssetName) (closed 3_600_200 3_601_200)
    ]
  where
    elapsed requested authenticated status validityRange =
      punattestedBlockTimeoutElapsedV1
        (pconstant requested)
        (pconstant authenticated)
        (stateQueueNodeTerm control status)
        (pconstant validityRange)

availabilityStatusAuthorizationTests :: TestTree
availabilityStatusAuthorizationTests =
  testGroup
    "availabilityStatusUpdateIsAuthorizedV1"
    [ testCase "accepts exact mint-bond binding" $
        holds $ availabilityRoute mintBondAvailabilityRedeemer availabilityPolicy 2 3
    , testCase "decodes the mint-bond input and output positions exactly" $
        holds availabilityMintBondFieldsAreExact
    , testCase "retrieves the exact mint-bond redeemer payload" $
        holds availabilityRedeemerLookupIsExact
    , testCase "decodes exact positions after redeemer lookup" $
        holds availabilityLookupFieldsAreExact
    , testCase "rejects wrong input or output index" $
        holds $
          pnot # availabilityRoute mintBondAvailabilityRedeemer availabilityPolicy 1 3
            #&& pnot # availabilityRoute mintBondAvailabilityRedeemer availabilityPolicy 2 4
    , testCase "rejects timeout removal arm" $
        holds $ pnot # availabilityRoute timeoutAvailabilityRedeemer availabilityPolicy 2 3
    , testCase "rejects wrong policy" $
        pfails $ availabilityRoute mintBondAvailabilityRedeemer otherAvailabilityPolicy 2 3
    ]

availabilityRoute ::
  forall s.
  Redeemer ->
  CurrencySymbol ->
  Integer ->
  Integer ->
  Term s PBool
availabilityRoute redeemer policy inputIndex outputIndex =
  pavailabilityStatusUpdateIsAuthorizedV1
    (redeemersTerm [(Minting availabilityPolicy, redeemer)])
    (pdata $ pconstant policy)
    0
    (pconstant inputIndex)
    (pconstant outputIndex)

mintBondAvailabilityRedeemer, timeoutAvailabilityRedeemer :: Redeemer
mintBondAvailabilityRedeemer =
  Redeemer $ dataToBuiltinData $ PD.Constr 0 $ map PD.I [0, 1, 0, 1, 2, 3]
timeoutAvailabilityRedeemer =
  Redeemer $ dataToBuiltinData $ PD.Constr 4 $ map PD.I [0, 1, 2, 0, 0, 1]

availabilityPolicy, otherAvailabilityPolicy :: CurrencySymbol
availabilityPolicy = CurrencySymbol $ toBuiltin $ BS.replicate 28 0xab
otherAvailabilityPolicy = CurrencySymbol $ toBuiltin $ BS.replicate 28 0xcd

availabilityMintBondFieldsAreExact :: forall s. Term s PBool
availabilityMintBondFieldsAreExact =
  pmatch
    ( pfromData $
        punsafeCoerce @(PAsData Availability.PMintRedeemerV1) $
          pconstant @PData $ PD.Constr 0 $ map PD.I [0, 1, 0, 1, 2, 3]
    )
    $ \case
      Availability.PMintBondFromAttestation _ _ _ _ boundInput boundOutput ->
        pfromData boundInput #== 2 #&& pfromData boundOutput #== 3
      _ -> pconstant False

availabilityRedeemerLookupIsExact :: forall s. Term s PBool
availabilityRedeemerLookupIsExact =
  pto
    ( pfromData $
        Utils.pgetRedeemerAt
          # redeemersTerm [(Minting availabilityPolicy, mintBondAvailabilityRedeemer)]
          # pdata (pcon (PMinting $ pdata $ pconstant availabilityPolicy))
          # 0
    )
    #== pconstant (PD.Constr 0 $ map PD.I [0, 1, 0, 1, 2, 3])

availabilityLookupFieldsAreExact :: forall s. Term s PBool
availabilityLookupFieldsAreExact =
  pmatch
    ( pfromData $
        punsafeCoerce @(PAsData Availability.PMintRedeemerV1) $
          pto $
            pfromData $
              Utils.pgetRedeemerAt
                # redeemersTerm [(Minting availabilityPolicy, mintBondAvailabilityRedeemer)]
                # pdata (pcon (PMinting $ pdata $ pconstant availabilityPolicy))
                # 0
    )
    $ \case
      Availability.PMintBondFromAttestation _ _ _ _ boundInput boundOutput ->
        pfromData boundInput #== 2 #&& pfromData boundOutput #== 3
      _ -> pconstant False

--------------------------------------------------------------------------------
-- commit_block_header_output_is_valid_v1
--------------------------------------------------------------------------------

-- | Aiken @q49_l295_header_shape_control_and_interval_mutations@.
headerShapeTests :: TestTree
headerShapeTests =
  testGroup
    "commitBlockHeaderOutputIsValidV1 / interval and version"
    [ testCase "accepts the control header" $
        holds $ outputIsValid control
    , -- A block must cover a non-empty interval: this is the one thing about a
      -- header's times that depends on nothing outside the header.
      testCase "rejects an empty interval" $
        fails $ outputIsValid control {hStartTime = hEndTime control}
    , testCase "rejects an inverted interval" $
        fails $ outputIsValid control {hStartTime = hEndTime control + 1}
    , testCase "rejects protocol version 0" $
        fails $ outputIsValid control {hProtocolVersion = 0}
    , testCase "rejects protocol version 2" $
        fails $ outputIsValid control {hProtocolVersion = 2}
    ]

-- | Aiken @q49_l295_validation_context_scalar_control_and_mutations@.
scalarTests :: TestTree
scalarTests =
  testGroup
    "commitBlockHeaderOutputIsValidV1 / validation-context scalars"
    [ testCase "accepts the control header" $
        holds $ outputIsValid control
    , testCase "accepts supported network and runtime values" $
        holds $
          outputIsValid
            control
              { hBlockSlot = 1
              , hNetworkId = 1
              , hMinFeeA = 44
              , hMinFeeB = 155_381
              }
    , testCase "rejects a negative block slot" $
        fails $ outputIsValid control {hBlockSlot = -1}
    , testCase "rejects a negative network id" $
        fails $ outputIsValid control {hNetworkId = -1}
    , testCase "rejects network id 2" $
        fails $ outputIsValid control {hNetworkId = 2}
    , testCase "rejects network id 255" $
        fails $ outputIsValid control {hNetworkId = 255}
    , testCase "rejects a negative min_fee_a" $
        fails $ outputIsValid control {hMinFeeA = -1}
    , testCase "rejects a negative min_fee_b" $
        fails $ outputIsValid control {hMinFeeB = -1}
    ]

outputIsValid :: forall s. Header -> Term s PBool
outputIsValid h = pcommitBlockHeaderOutputIsValidV1 # headerTerm h

--------------------------------------------------------------------------------
-- commit_block_header_operator_is_legitimate_v1
--------------------------------------------------------------------------------

{- | Aiken @q49_l295_scheduled_operator_control_and_independent_mutations@.

Four independently-sourced names of the same operator: the header's, the
redeemer's, the scheduler's, and the active set's. Each mutation moves exactly
one of them, so no single source can be the one that decides.
-}
operatorTests :: TestTree
operatorTests =
  testGroup
    "commitBlockHeaderOperatorIsLegitimateV1"
    [ testCase "accepts four agreeing names" $
        holds $ legitimate operator operator operator operator
    , testCase "rejects a header naming another operator" $
        fails $ legitimate other operator operator operator
    , testCase "rejects a scheduler naming another operator" $
        fails $ legitimate operator operator other operator
    , testCase "rejects an active-set spend naming another operator" $
        fails $ legitimate operator operator operator other
    , testCase "rejects a redeemer naming another operator" $
        fails $ legitimate operator other operator operator
    ]
  where
    legitimate a b c d =
      pcommitBlockHeaderOperatorIsLegitimateV1
        (pkh a)
        (pkh b)
        (pkh c)
        (pkh d)
    pkh k = punsafeCoerce (pconstant @PData (PD.B k))

--------------------------------------------------------------------------------
-- commit_block_header_carries_previous_block_v1
--------------------------------------------------------------------------------

{- | Aiken @q49_l295_previous_header_control_and_independent_mutations@.

Appending after another block. Continuity of the ledger state and of the covered
interval is what makes the queue a chain: the new header's @prev_utxos_root@ is
the predecessor's @utxos_root@, and its @start_time@ the predecessor's
@end_time@.
-}
previousHeaderTests :: TestTree
previousHeaderTests =
  testGroup
    "commitBlockHeaderCarriesPreviousBlockV1"
    [ testCase "accepts a header carrying its predecessor" $
        holds $ carriesPrevious output
    , testCase "rejects a wrong previous header hash" $
        fails $ carriesPrevious output {hPrevHeaderHash = headerHashA}
    , testCase "rejects a prev_utxos_root that is not the predecessor's utxos_root" $
        fails $ carriesPrevious output {hPrevUtxosRoot = utxoRootA}
    , -- A gap or an overlap in the covered interval, either way.
      testCase "rejects a start time before the predecessor's end" $
        fails $ carriesPrevious output {hStartTime = hEndTime previous - 1}
    , testCase "rejects a start time after the predecessor's end" $
        fails $ carriesPrevious output {hStartTime = hEndTime previous + 1}
    , testCase "rejects a protocol version that is not the predecessor's" $
        fails $ carriesPrevious output {hProtocolVersion = 2}
    ]
  where
    previous = control
    output =
      control
        { hPrevHeaderHash = headerHashB
        , hPrevUtxosRoot = hUtxosRoot previous
        , hStartTime = hEndTime previous
        , hEndTime = hEndTime previous + 100
        }
    carriesPrevious o =
      pcommitBlockHeaderCarriesPreviousBlockV1
        (headerTerm o)
        (pconstant headerHashB)
        (headerTerm previous)

--------------------------------------------------------------------------------
-- commit_block_header_carries_confirmed_state_v1, genesis route
--------------------------------------------------------------------------------

{- | Aiken @q49_l295_confirmed_genesis_control_and_independent_mutations@.

The first block of the chain. Both sides are mutated: the header's carry-over
fields, and the confirmed state's own authentication. The state has to be the
genesis sentinel exactly — every field pinned — because the sentinel is one
specific value rather than a shape.

The last case is the one that matters most: a state with the sentinel's fields
but @protocol_version = 1@ is rejected. Version zero is what identifies the
sentinel, and accepting a version-1 imitation would let a forged state stand in
for the real chain head.
-}
confirmedGenesisTests :: TestTree
confirmedGenesisTests =
  testGroup
    "commitBlockHeaderCarriesConfirmedStateV1 / genesis"
    [ testCase "accepts a header carrying the genesis sentinel" $
        holds $ carries output genesis
    , testCase "rejects a wrong previous header hash" $
        fails $ carries output {hPrevHeaderHash = headerHashB} genesis
    , testCase "rejects a wrong prev_utxos_root" $
        fails $ carries output {hPrevUtxosRoot = utxoRootB} genesis
    , testCase "rejects a start time before the sentinel's end" $
        fails $ carries output {hStartTime = cEndTime genesis - 1} genesis
    , testCase "rejects a start time after the sentinel's end" $
        fails $ carries output {hStartTime = cEndTime genesis + 1} genesis
    , testCase "rejects a protocol version other than the answered one" $
        fails $ carries output {hProtocolVersion = 2} genesis
    , testCase "rejects a sentinel with a wrong header hash" $
        fails $ carries output genesis {cHeaderHash = headerHashA}
    , testCase "rejects a sentinel with a wrong previous header hash" $
        fails $ carries output genesis {cPrevHeaderHash = headerHashA}
    , testCase "rejects a sentinel with a wrong utxo root" $
        fails $ carries output genesis {cUtxoRoot = utxoRootA}
    , testCase "rejects a sentinel covering an interval rather than an instant" $
        fails $ carries output genesis {cEndTime = cStartTime genesis + 1}
    , -- The sentinel is identified by version zero; a version-1 imitation of it
      -- must not authenticate.
      testCase "rejects a sentinel claiming protocol version 1" $
        fails $ carries output genesis {cProtocolVersion = 1}
    ]
  where
    output =
      control
        { hPrevHeaderHash = genesisHeaderHash
        , hPrevUtxosRoot = emptyRoot
        }

--------------------------------------------------------------------------------
-- commit_block_header_carries_confirmed_state_v1, ordinary route
--------------------------------------------------------------------------------

-- | Aiken @q49_l295_confirmed_ordinary_control_and_independent_mutations@.
confirmedOrdinaryTests :: TestTree
confirmedOrdinaryTests =
  testGroup
    "commitBlockHeaderCarriesConfirmedStateV1 / ordinary"
    [ testCase "accepts a header carrying an ordinary confirmed state" $
        holds $ carries output ordinary
    , testCase "rejects a state whose header hash the block does not name" $
        fails $ carries output ordinary {cHeaderHash = headerHashA}
    , -- An ordinary state may not claim the genesis hash.
      testCase "rejects a state carrying the genesis header hash" $
        fails $ carries output ordinary {cHeaderHash = genesisHeaderHash}
    , testCase "rejects a state at a negative start time" $
        fails $ carries output ordinary {cStartTime = -1}
    , testCase "rejects a state whose end time precedes its start" $
        fails $ carries output ordinary {cStartTime = cEndTime ordinary + 1}
    , testCase "rejects a state at protocol version 0" $
        fails $ carries output ordinary {cProtocolVersion = 0}
    , testCase "rejects a state at protocol version 2" $
        fails $ carries output ordinary {cProtocolVersion = 2}
    ]
  where
    ordinary =
      Confirmed
        { cHeaderHash = headerHashB
        , cPrevHeaderHash = headerHashA
        , cUtxoRoot = utxoRootA
        , cStartTime = 0
        , cEndTime = 100
        , cProtocolVersion = 1
        }
    output = control {hPrevHeaderHash = headerHashB}

carries :: forall s. Header -> Confirmed -> Term s PBool
carries o c = pcommitBlockHeaderCarriesConfirmedStateV1 (headerTerm o) (confirmedTerm c)

--------------------------------------------------------------------------------
-- Merge-side production guards
--------------------------------------------------------------------------------

{- | @header_carries_l2_material@ is the switch deciding whether merging a block
must also spawn a settlement, so each of the four roots must be able to trip it
on its own.
-}
l2MaterialTests :: TestTree
l2MaterialTests =
  testGroup
    "headerCarriesL2Material"
    [ testCase "an all-empty header carries no L2 material" $
        fails $ carriesL2 control
    , testCase "a non-empty transactions root carries L2 material" $
        holds $ carriesL2 control {hTransactionsRoot = utxoRootA}
    , testCase "a non-empty deposits root carries L2 material" $
        holds $ carriesL2 control {hDepositsRoot = utxoRootA}
    , testCase "a non-empty withdrawals root carries L2 material" $
        holds $ carriesL2 control {hWithdrawalsRoot = utxoRootA}
    , testCase "a non-empty forced-transactions root carries L2 material" $
        holds $ carriesL2 control {hForcedTransactionsRoot = utxoRootA}
    , -- The transition-trace and event-to-step roots describe how the block was
      -- processed, not what a user put in it, so they do not trip the switch.
      testCase "a non-empty transition-trace root alone does not" $
        fails $ carriesL2 control {hTransitionTraceRoot = utxoRootA}
    , testCase "a non-empty event-to-step root alone does not" $
        fails $ carriesL2 control {hEventToStepRoot = utxoRootA}
    , testCase "a non-empty validation-traces root alone does not" $
        fails $ carriesL2 control {hValidationTracesRoot = utxoRootA}
    ]
  where
    carriesL2 h = pheaderCarriesL2Material # headerTerm h

{- | The merge redeemer restates all seven roots and all seven counts. The
settlement spawned from the merge reads them from the redeemer, so any one of
them drifting from the header must be caught.
-}
mergeCommitmentTests :: TestTree
mergeCommitmentTests =
  testGroup
    "mergeCommitmentsMatchHeader"
    [ testCase "accepts a redeemer restating the header" $
        holds $ matches control control
    , testCase "rejects a drifted withdrawals root" $
        fails $ matches control control {hWithdrawalsRoot = utxoRootA}
    , testCase "rejects a drifted forced-transactions root" $
        fails $ matches control control {hForcedTransactionsRoot = utxoRootA}
    , testCase "rejects a drifted transactions root" $
        fails $ matches control control {hTransactionsRoot = utxoRootA}
    , testCase "rejects a drifted deposits root" $
        fails $ matches control control {hDepositsRoot = utxoRootA}
    , testCase "rejects a drifted transition-trace root" $
        fails $ matches control control {hTransitionTraceRoot = utxoRootA}
    , testCase "rejects a drifted event-to-step root" $
        fails $ matches control control {hEventToStepRoot = utxoRootA}
    , testCase "rejects a drifted validation-traces root" $
        fails $ matches control control {hValidationTracesRoot = utxoRootA}
    , testCase "rejects a drifted withdrawal count" $
        fails $ matches control control {hWithdrawalCount = 1}
    , testCase "rejects a drifted forced-transaction count" $
        fails $ matches control control {hForcedTransactionCount = 1}
    , testCase "rejects a drifted L2-transaction count" $
        fails $ matches control control {hL2TransactionCount = 1}
    , testCase "rejects a drifted deposit count" $
        fails $ matches control control {hDepositCount = 1}
    , testCase "rejects a drifted total event count" $
        fails $ matches control control {hTotalEventCount = 1}
    , testCase "rejects a drifted transition-step count" $
        fails $ matches control control {hTransitionStepCount = 1}
    , testCase "rejects a drifted validation-trace count" $
        fails $ matches control control {hValidationTraceCount = 1}
    ]
  where
    -- The redeemer's commitments are built from a header, so a mutation to the
    -- second argument is a redeemer that no longer restates the first.
    matches h r = pmergeCommitmentsMatchHeader (headerTerm h) (mergeRedeemerTerm r)

{- | A block carrying L2 material must spawn a settlement keyed by its own hash;
a block carrying none must spawn no settlement at all. Both directions are
load-bearing — the second stops a settlement being spawned against a block that
moved nothing.
-}
settlementBindingTests :: TestTree
settlementBindingTests =
  testGroup
    "mergeSettlementBindingMatchesHeader"
    [ testCase "accepts a settlement keyed by the block's own hash" $
        holds $ binding withMaterial (Just headerHashA)
    , testCase "rejects a settlement keyed by another hash" $
        fails $ binding withMaterial (Just headerHashB)
    , testCase "rejects no settlement for a block carrying L2 material" $
        fails $ binding withMaterial Nothing
    , testCase "accepts no settlement for an empty block" $
        holds $ binding control Nothing
    , testCase "rejects a settlement spawned for an empty block" $
        fails $ binding control (Just headerHashA)
    , testCase "accepts a settlement for a withdrawal-only block" $
        holds $ binding singleWithdrawal (Just headerHashA)
    , testCase "rejects no settlement for a withdrawal-only block" $
        fails $ binding singleWithdrawal Nothing
    , testCase "accepts a settlement for a forced-transaction-only block" $
        holds $ binding singleForcedTransaction (Just headerHashA)
    , testCase "rejects no settlement for a forced-transaction-only block" $
        fails $ binding singleForcedTransaction Nothing
    , testCase "accepts a settlement for a deposit-only block" $
        holds $ binding singleDeposit (Just headerHashA)
    , testCase "rejects no settlement for a deposit-only block" $
        fails $ binding singleDeposit Nothing
    ]
  where
    withMaterial = control {hTransactionsRoot = utxoRootA}
    singleWithdrawal = control {hWithdrawalsRoot = utxoRootA}
    singleForcedTransaction = control {hForcedTransactionsRoot = utxoRootA}
    singleDeposit = control {hDepositsRoot = utxoRootA}
    binding h mId =
      pmergeSettlementBindingMatchesHeader
        (headerTerm h)
        (pconstant headerHashA)
        (maybe (pcon PNothing) (pcon . PJust . pconstant) mId)

{- | Aiken @merge_settlement_production_guard_binds_the_exact_redeemer_route@
and @merge_settlement_production_guard_rejects_the_wrong_redeemer_route@.

The merge's integer is an index into the transaction redeemer ordering. The
entry there must belong to the settlement policy itself; a valid Spawn for a
different minting purpose cannot be substituted.
-}
settlementRedeemerRouteTests :: TestTree
settlementRedeemerRouteTests =
  testGroup
    "mergeSettlementIdAtRoute"
    [ testCase "finds Spawn at the exact settlement redeemer route" $
        holds $ routeMatches canonicalRedeemers (Just 0) (Just headerHashA)
    , testCase "None names no settlement redeemer route" $
        holds $ routeMatches canonicalRedeemers Nothing Nothing
    , testCase "rejects an index belonging to another minting policy" $
        pfails $ routeMatches wrongRouteRedeemers (Just 1) (Just headerHashA)
    ]

routeMatches ::
  forall s.
  [(ScriptPurpose, Redeemer)] ->
  Maybe Integer ->
  Maybe BS.ByteString ->
  Term s PBool
routeMatches redeemers index expected =
  pmatch
    ( pmergeSettlementIdAtRoute
        (redeemersTerm redeemers)
        (pdata (pconstant settlementPolicy))
        (maybe (pcon PDNothing) (pcon . PDJust . pdata . pconstant) index)
    )
    $ \case
      PNothing -> pconstant (expected == Nothing)
      PJust actual -> maybe (pconstant False) (\value -> actual #== pconstant value) expected

redeemersTerm ::
  forall s.
  [(ScriptPurpose, Redeemer)] ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)))
redeemersTerm = pconstant

canonicalRedeemers, wrongRouteRedeemers :: [(ScriptPurpose, Redeemer)]
canonicalRedeemers = [(Minting settlementPolicy, spawnRedeemer)]
wrongRouteRedeemers =
  [ (Minting settlementPolicy, spawnRedeemer)
  , (Minting otherMintPolicy, spawnRedeemer)
  ]

spawnRedeemer :: Redeemer
spawnRedeemer =
  Redeemer . dataToBuiltinData $
    PD.Constr 0 [PD.B headerHashA, PD.I 0, PD.I 0, PD.I 0]

settlementPolicy, otherMintPolicy :: CurrencySymbol
settlementPolicy = CurrencySymbol (toBuiltin (BS.replicate 28 0x11))
otherMintPolicy = CurrencySymbol (toBuiltin (BS.replicate 28 0x22))

--------------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

fails :: (forall s. Term s PBool) -> Assertion
fails b = passertEval (pnot # b)

closed :: Integer -> Integer -> Interval POSIXTime
closed lo hi =
  Interval
    (LowerBound (Finite (POSIXTime lo)) True)
    (UpperBound (Finite (POSIXTime hi)) True)

stateQueueNodeTerm :: forall s. Header -> PD.Data -> Term s PStateQueueNode
stateQueueNodeTerm h status =
  pfromData $
    punsafeCoerce $
      pconstant @PData $
        PD.Constr 0 [headerData h, status]

unattestedStatus :: PD.Data
unattestedStatus = PD.Constr 0 []

attestedStatus :: BS.ByteString -> PD.Data
attestedStatus bondAssetName = PD.Constr 1 [PD.B bondAssetName]

challengedStatus :: BS.ByteString -> BS.ByteString -> PD.Data
challengedStatus bondAssetName challengeName =
  PD.Constr 2 [PD.B bondAssetName, PD.B challengeName]

publishedStatus :: PD.Data
publishedStatus = PD.Constr 3 [PD.B (BS.replicate 32 0xef)]

daBondAssetName, challengeAssetName :: BS.ByteString
daBondAssetName = "DABN" <> BS.replicate 28 0x77
challengeAssetName = "DACH" <> BS.replicate 28 0x88

fraudProver, otherFraudProver :: BS.ByteString
fraudProver = BS.replicate 28 0xee
otherFraudProver = BS.replicate 28 0xff

rewardAmount :: Integer
rewardAmount = 400_000_000

fraudProverAddress, otherFraudProverAddress, delegatedFraudProverAddress, scriptFraudProverAddress :: Address
fraudProverAddress = Address (PubKeyCredential $ PubKeyHash $ toBuiltin fraudProver) Nothing
otherFraudProverAddress = Address (PubKeyCredential $ PubKeyHash $ toBuiltin otherFraudProver) Nothing
delegatedFraudProverAddress =
  Address
    (PubKeyCredential $ PubKeyHash $ toBuiltin fraudProver)
    (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $ toBuiltin otherFraudProver)
scriptFraudProverAddress = Address (ScriptCredential $ ScriptHash $ toBuiltin fraudProver) Nothing

foreignRewardPolicy :: CurrencySymbol
foreignRewardPolicy = CurrencySymbol $ toBuiltin $ BS.replicate 28 0xab

foreignScriptHash :: ScriptHash
foreignScriptHash = ScriptHash $ toBuiltin $ BS.replicate 28 0xac

rewardOutput :: Address -> Integer -> OutputDatum -> Maybe ScriptHash -> TxOut
rewardOutput address lovelace datum referenceScript =
  TxOut address (mkAdaValue $ fromInteger lovelace) datum referenceScript

unrelatedOutput :: TxOut
unrelatedOutput = rewardOutput otherFraudProverAddress rewardAmount NoOutputDatum Nothing

smuggledRewardOutput :: TxOut
smuggledRewardOutput =
  TxOut
    fraudProverAddress
    (mkAdaValue (fromInteger rewardAmount) <> singleton foreignRewardPolicy (TokenName "smuggled") 1)
    NoOutputDatum
    Nothing

rewardOutputs :: [TxOut]
rewardOutputs =
  [ unrelatedOutput
  , rewardOutput fraudProverAddress rewardAmount NoOutputDatum Nothing
  ]

--------------------------------------------------------------------------------
-- Fixtures, mirroring Aiken's q49_ constants
--------------------------------------------------------------------------------

headerHashA, headerHashB, genesisHeaderHash, operator, other :: BS.ByteString
headerHashA = BS.replicate 28 0xaa
headerHashB = BS.replicate 28 0xbb
genesisHeaderHash = BS.replicate 28 0x00
operator = BS.replicate 28 0xcc
other = BS.replicate 28 0xdd

controlHash :: BS.ByteString
controlHash =
  fromBuiltin $
    blake2b_224 $
      serialiseData $
        dataToBuiltinData (headerData control)

utxoRootA, utxoRootB :: BS.ByteString
utxoRootA = BS.replicate 32 0x11
utxoRootB = BS.replicate 32 0x22

emptyRoot :: BS.ByteString
emptyRoot =
  BS.pack
    [ 0x0e, 0x57, 0x51, 0xc0, 0x26, 0xe5, 0x43, 0xb2
    , 0xe8, 0xab, 0x2e, 0xb0, 0x60, 0x99, 0xda, 0xa1
    , 0xd1, 0xe5, 0xdf, 0x47, 0x77, 0x8f, 0x77, 0x87
    , 0xfa, 0xab, 0x45, 0xcd, 0xf1, 0x2f, 0xe3, 0xa8
    ]

data Header = Header
  { hPrevUtxosRoot :: BS.ByteString
  , hUtxosRoot :: BS.ByteString
  , hWithdrawalsRoot :: BS.ByteString
  , hForcedTransactionsRoot :: BS.ByteString
  , hTransactionsRoot :: BS.ByteString
  , hDepositsRoot :: BS.ByteString
  , hTransitionTraceRoot :: BS.ByteString
  , hEventToStepRoot :: BS.ByteString
  , hValidationTracesRoot :: BS.ByteString
  , hWithdrawalCount :: Integer
  , hForcedTransactionCount :: Integer
  , hL2TransactionCount :: Integer
  , hDepositCount :: Integer
  , hTotalEventCount :: Integer
  , hTransitionStepCount :: Integer
  , hValidationTraceCount :: Integer
  , hStartTime :: Integer
  , hEndTime :: Integer
  , hBlockSlot :: Integer
  , hNetworkId :: Integer
  , hMinFeeA :: Integer
  , hMinFeeB :: Integer
  , hPrevHeaderHash :: BS.ByteString
  , hOperatorVkey :: BS.ByteString
  , hProtocolVersion :: Integer
  }

-- | Aiken @q49_header_fixture@, field for field.
control :: Header
control =
  Header
    { hPrevUtxosRoot = utxoRootA
    , hUtxosRoot = utxoRootB
    , hWithdrawalsRoot = emptyRoot
    , hForcedTransactionsRoot = emptyRoot
    , hTransactionsRoot = emptyRoot
    , hDepositsRoot = emptyRoot
    , hTransitionTraceRoot = emptyRoot
    , hEventToStepRoot = emptyRoot
    , hValidationTracesRoot = emptyRoot
    , hWithdrawalCount = 0
    , hForcedTransactionCount = 0
    , hL2TransactionCount = 0
    , hDepositCount = 0
    , hTotalEventCount = 0
    , hTransitionStepCount = 0
    , hValidationTraceCount = 0
    , hStartTime = 100
    , hEndTime = 200
    , hBlockSlot = 0
    , hNetworkId = 0
    , hMinFeeA = 0
    , hMinFeeB = 0
    , hPrevHeaderHash = headerHashA
    , hOperatorVkey = operator
    , hProtocolVersion = 1
    }

headerData :: Header -> PD.Data
headerData h =
  PD.Constr
    0
    [ PD.B (hPrevUtxosRoot h)
    , PD.B (hUtxosRoot h)
    , PD.B (hWithdrawalsRoot h)
    , PD.B (hForcedTransactionsRoot h)
    , PD.B (hTransactionsRoot h)
    , PD.B (hDepositsRoot h)
    , PD.B (hTransitionTraceRoot h)
    , PD.B (hEventToStepRoot h)
    , PD.B (hValidationTracesRoot h)
    , PD.I (hWithdrawalCount h)
    , PD.I (hForcedTransactionCount h)
    , PD.I (hL2TransactionCount h)
    , PD.I (hDepositCount h)
    , PD.I (hTotalEventCount h)
    , PD.I (hTransitionStepCount h)
    , PD.I (hValidationTraceCount h)
    , PD.I (hStartTime h)
    , PD.I (hEndTime h)
    , PD.I (hBlockSlot h)
    , PD.I (hNetworkId h)
    , PD.I (hMinFeeA h)
    , PD.I (hMinFeeB h)
    , PD.B (hPrevHeaderHash h)
    , PD.B (hOperatorVkey h)
    , PD.I (hProtocolVersion h)
    ]

headerTerm :: forall s. Header -> Term s PHeaderV1
headerTerm h = pfromData (punsafeCoerce (pconstant @PData (headerData h)))

{- | A @MergeToConfirmedStateV1@ redeemer restating a header's commitments.

The first four fields are not read by 'pmergeCommitmentsMatchHeader', so they
are placeholders; the fourteen that follow are the restatement.
-}
mergeRedeemerTerm :: forall s. Header -> Term s PMintRedeemer
mergeRedeemerTerm h = pfromData (punsafeCoerce (pconstant @PData dat))
  where
    dat =
      PD.Constr
        6
        ( [ PD.B headerHashA -- header_node_key
          , PD.Constr 0 [PD.Constr 0 [PD.B (BS.replicate 32 0x01)], PD.I 0]
          , PD.I 0 -- confirmed_state_output_index
          , PD.Constr 1 [] -- m_settlement_redeemer_index: None
          ]
            <> [ PD.B (hWithdrawalsRoot h)
               , PD.B (hForcedTransactionsRoot h)
               , PD.B (hTransactionsRoot h)
               , PD.B (hDepositsRoot h)
               , PD.B (hTransitionTraceRoot h)
               , PD.B (hEventToStepRoot h)
               , PD.B (hValidationTracesRoot h)
               , PD.I (hWithdrawalCount h)
               , PD.I (hForcedTransactionCount h)
               , PD.I (hL2TransactionCount h)
               , PD.I (hDepositCount h)
               , PD.I (hTotalEventCount h)
               , PD.I (hTransitionStepCount h)
               , PD.I (hValidationTraceCount h)
               ]
        )

data Confirmed = Confirmed
  { cHeaderHash :: BS.ByteString
  , cPrevHeaderHash :: BS.ByteString
  , cUtxoRoot :: BS.ByteString
  , cStartTime :: Integer
  , cEndTime :: Integer
  , cProtocolVersion :: Integer
  }

-- | Aiken @genesis_confirmed_state_v1(100)@.
genesis :: Confirmed
genesis =
  Confirmed
    { cHeaderHash = genesisHeaderHash
    , cPrevHeaderHash = genesisHeaderHash
    , cUtxoRoot = emptyRoot
    , cStartTime = 100
    , cEndTime = 100
    , cProtocolVersion = 0
    }

confirmedTerm :: forall s. Confirmed -> Term s PConfirmedState
confirmedTerm c = pfromData (punsafeCoerce (pconstant @PData dat))
  where
    dat =
      PD.Constr
        0
        [ PD.B (cHeaderHash c)
        , PD.B (cPrevHeaderHash c)
        , PD.B (cUtxoRoot c)
        , PD.I (cStartTime c)
        , PD.I (cEndTime c)
        , PD.I (cProtocolVersion c)
        ]
