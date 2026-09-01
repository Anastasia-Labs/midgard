{-# LANGUAGE OverloadedStrings #-}

{- | Behavioural tests for "Midgard.FraudProofs.TransitionTrace.Proof".

These are /fault/ predicates, so the polarity is inverted from most of the suite:
returning True means the block is guilty. Every group therefore carries both
directions — a well-formed block whose fault is refused, and the smallest
perturbation that makes it hold — because a fault predicate that always returned
True would pass a suite of guilty blocks alone.

=== The block is real

The fixture is a two-step trace in a real two-entry Merkle-Patricia trie, with
real inclusion proofs (see "Testing.MpfTrie"), under real counted roots. That
matters most for the link fault: with one tree per step each proof would verify
against its own root, which is precisely the forgery the same-tree check exists to
stop, and a fixture built that way would pass while proving nothing.

The four source trees and the event-to-step map are one-entry or empty as each
fault requires, and the header's roots are derived from whichever shape is in
play — so a test that moves a leaf still faces a header committing to the moved
leaf, and fails on the tie it names rather than on membership.
-}
module Testing.TransitionTraceProof (
  tests,

  -- * Convictions, for the dispatch validators above this library
  ConvictingProof (..),
  controlConviction,
  sourceConviction,
  withdrawalConviction,
  forcedConviction,
  acceptedTransactionConviction,
  depositConviction,
  l1EventConviction,
  duplicateConviction,
  depositHubOracleDatum,
  depositReferenceInput,
  depositReferenceOutput,
) where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTokenName)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.TransitionTrace.Proof (
  PAuthenticatedDepositReference (..),
  pcardanoAssetPairsToMidgard,
  pcardanoCredentialToMidgard,
  pcardanoStakeCredentialToMidgard,
  pdepositAddressToMidgard,
  pdepositDatumCbor,
  pgetAuthenticatedDepositReference,
  pprojectedDepositOutputCbor,
  pprojectedDepositValue,
  pforcedTxIsDue,
  ptimedL1EventIsDue,
  pvalidateDepositOneStepBinding,
  pvalidateAcceptedTransactionFaultProof,
  pvalidateControlFaultProof,
  pvalidateDepositFaultProof,
  pvalidateDuplicateFaultProof,
  pvalidateForcedFaultProof,
  pvalidateInvalidOneStepTransition,
  pvalidateL1EventFaultProof,
  pvalidateOmittedDueL1Event,
  pvalidateSourceFaultProof,
  pvalidateTransitionFaultProof,
  pvalidateTransitionFaultProofEnvelope,
  pvalidateWithdrawalFaultProof,
  pvalidateOutOfWindowSourceEvent,
  pvalidateValidDepositTransition,
  PSourceMembershipProof,
  PSourceNonMembershipProof,
  PTraceBoundarySide,
  peventKeyFromSourceMembership,
  peventKeyPhase,
  pheaderCountsAreNonNegative,
  pphaseForStepIndex,
  psourceCountSum,
  psourceMembershipPhase,
  psourceNonMembershipEventKey,
  ptraceHasBadPhase,
  papplyDeleteWitness,
  papplyInsertWitness,
  pdeleteRoot,
  pinsertRoot,
  pledgerOutrefKey,
  pvalidateCountFault,
  pvalidateDuplicateTraceEvent,
  papplyL2Outputs,
  papplyL2Spends,
  pdoorBodyFieldItems,
  pvalidateL2OneStepBinding,
  pvalidateL2TransactionTransition,
  pverifyL2SourceMembership,
  pvalidateInvalidForcedTransactionNoOpTransition,
  pvalidateInvalidWithdrawalNoOpTransition,
  pvalidateValidWithdrawalTransition,
  pvalidateWithdrawalOneStepBinding,
  pverifyLedgerMembership,
  pverifyLedgerNonMembership,
  pvalidateEventToStepMismatch,
  pvalidateSourceMembershipMismatch,
  pvalidateTraceBoundary,
  pvalidateTraceLink,
  pverifySourceMembership,
  pverifySourceNonMembership,
  pverifyTraceBindingToEventToStep,
 )
import Midgard.FraudProofs.FieldOpening (
  PAnchoredNativeTxV1,
  PNativeTxAnchorV1 (..),
  PNativeTxOpeningV1 (..),
  panchoredNativeTx,
 )
import Midgard.LedgerState (PHeaderV1, PTransitionPhase)

import Testing.Eval (passertEval, pfails)
import Testing.FraudProofsFixture (
  arrayHeader,
  blake2b224,
  blake2b256,
  cborInt,
  commitCountedRoot,
  compactWithValidity,
  encodedInput,
  outputItem,
  outputsPreimage,
  serialise,
  sharedInputRef,
  spendInputsPreimage,
  Tx (..),
  tx1,
  txIdOf,
  witnessSetCborOf,
  wrapItem,
 )
import Testing.MpfTrie (emptyMerkleRoot, libraryNullHash, singleEntryRoot, twoLeafProof, twoLeafRoot)

--------------------------------------------------------------------------------
-- Terms from data
--------------------------------------------------------------------------------

fromData :: forall (a :: S -> Type) s. (PIsData a) => PD.Data -> Term s a
fromData d = pfromData (punsafeCoerce (pconstant @PData d))

asDataTerm :: forall (a :: S -> Type) s. PD.Data -> Term s (PAsData a)
asDataTerm d = punsafeCoerce (pconstant @PData d)

-- | A fault predicate that does /not/ hold, as against one that aborts.
prefuses :: (forall s. Term s PBool) -> Assertion
prefuses p = passertEval (pnot # p)

--------------------------------------------------------------------------------
-- Domains
--------------------------------------------------------------------------------

withdrawalsDomain, forcedDomain, transactionsDomain, depositsDomain :: Integer
withdrawalsDomain = 0
forcedDomain = 1
transactionsDomain = 2
depositsDomain = 3

traceDomain, eventToStepDomain :: Integer
traceDomain = 4
eventToStepDomain = 5

--------------------------------------------------------------------------------
-- Event keys and steps
--------------------------------------------------------------------------------

outRef :: Integer -> PD.Data
outRef n = PD.Constr 0 [PD.Constr 0 [PD.B (BS.replicate 32 (fromIntegral n))], PD.I n]

-- | A @WithdrawalEventKey@ — constructor 0, so phase @Withdrawal@.
withdrawalKey :: PD.Data
withdrawalKey = PD.Constr 0 [outRef 0x71]

-- | A @DepositEventKey@ — constructor 3, so phase @Deposit@.
depositKey :: PD.Data
depositKey = PD.Constr 3 [outRef 0x72]

-- | A @L2TransactionEventKey@ — constructor 2, keyed by raw bytes.
l2Key :: PD.Data
l2Key = PD.Constr 2 [PD.B (BS.replicate 32 0x73)]

withdrawalInfo, depositInfo :: PD.Data
withdrawalInfo = PD.Constr 0 [PD.I 1, PD.B (BS.replicate 28 0xd1)]
depositInfo = PD.Constr 0 [PD.I 2, PD.B (BS.replicate 28 0xd2)]

ledgerRoot :: Integer -> BS.ByteString
ledgerRoot n = BS.replicate 32 (fromIntegral n)

{- | @ledger_state.TransitionStep@ — six fields in declaration order.

The two ledger roots are what chain a trace together, so they are the parameters
here; everything else about a step is either fixed or the thing under test.
-}
stepWith :: Integer -> PD.Data -> Integer -> BS.ByteString -> BS.ByteString -> PD.Data
stepWith index eventKey phase pre post =
  PD.Constr
    0
    [ PD.I 1
    , PD.I index
    , eventKey
    , PD.Constr phase []
    , PD.B pre
    , PD.B post
    ]

-- | The block's two steps, meeting at 'midRoot'.
step0, step1 :: PD.Data
step0 = stepWith 0 withdrawalKey 0 (ledgerRoot 0xa0) midRoot
step1 = stepWith 1 withdrawalKey 0 midRoot (ledgerRoot 0xa2)

midRoot :: BS.ByteString
midRoot = ledgerRoot 0xa1

--------------------------------------------------------------------------------
-- The trace tree
--------------------------------------------------------------------------------

entryOf :: Integer -> PD.Data -> (BS.ByteString, BS.ByteString)
entryOf index value = (serialise (PD.I index), serialise value)

{- | A two-entry trace trie holding the two steps given, and an indexed proof for
either of them.
-}
traceRawRootOf :: PD.Data -> PD.Data -> BS.ByteString
traceRawRootOf s0 s1 = twoLeafRoot (entryOf 0 s0) (entryOf 1 s1)

traceProofOf :: PD.Data -> PD.Data -> Integer -> PD.Data
traceProofOf s0 s1 index =
  PD.Constr
    0
    [ PD.Constr traceDomain []
    , PD.B (commitCountedRoot traceDomain raw 2)
    , PD.B raw
    , PD.I 2
    , PD.I index
    , if index == 0 then s0 else s1
    , twoLeafProof (entryOf index self) (entryOf (1 - index) other)
    ]
  where
    raw = traceRawRootOf s0 s1
    self = if index == 0 then s0 else s1
    other = if index == 0 then s1 else s0

adjacentOf :: PD.Data -> PD.Data -> PD.Data
adjacentOf s0 s1 = PD.Constr 0 [traceProofOf s0 s1 0, traceProofOf s0 s1 1]

--------------------------------------------------------------------------------
-- One-entry and empty trees
--------------------------------------------------------------------------------

{- | A one-entry tree's raw root and a membership proof into it, under a committed
count that need not be one — the count a header publishes is a claim about the
block, and the tree only has to be consistent with it.
-}
singleMembership :: Integer -> Integer -> BS.ByteString -> BS.ByteString -> PD.Data -> PD.Data -> PD.Data
singleMembership domain count keyBytes valueBytes key value =
  PD.Constr
    0
    [ PD.Constr domain []
    , PD.B (commitCountedRoot domain raw count)
    , PD.B raw
    , PD.I count
    , key
    , value
    , PD.List []
    ]
  where
    raw = singleEntryRoot keyBytes valueBytes

singleRoot :: Integer -> Integer -> BS.ByteString -> BS.ByteString -> BS.ByteString
singleRoot domain count keyBytes valueBytes =
  commitCountedRoot domain (singleEntryRoot keyBytes valueBytes) count

{- | Absence from an empty tree: the walk folds nothing and lands on the sentinel.

The count is fixed at zero, and that is forced rather than chosen. A counted root
is only consistent when an empty phas root carries a count of zero /and/ the
published root is the empty sentinel itself rather than a commitment over it — so
"this tree is empty" is the only shape in which a non-membership proof with an
empty path exists, and every fixture below that proves an absence publishes the
matching count of zero alongside it.
-}
emptyNonMembership :: Integer -> PD.Data -> PD.Data
emptyNonMembership domain key =
  PD.Constr
    0
    [ PD.Constr domain []
    , PD.B emptyMerkleRoot
    , PD.B emptyMerkleRoot
    , PD.I 0
    , key
    , PD.List []
    ]

-- | The root a header publishes for a tree with nothing in it.
emptyRoot :: BS.ByteString
emptyRoot = emptyMerkleRoot

--------------------------------------------------------------------------------
-- The header
--------------------------------------------------------------------------------

{- | The block's nine roots and seven counts.

Counts are 2 withdrawals, 1 forced, 1 L2 and 1 deposit, so the phase layout is
indices 0–1 withdrawal, 2 forced, 3 L2, 4 deposit — five in total, which is what
makes 'pphaseForStepIndex' testable at every boundary in one fixture.
-}
data Block = Block
  { bPrevUtxosRoot :: BS.ByteString
  , bUtxosRoot :: BS.ByteString
  , bWithdrawalsRoot :: BS.ByteString
  , bForcedRoot :: BS.ByteString
  , bTransactionsRoot :: BS.ByteString
  , bDepositsRoot :: BS.ByteString
  , bTraceRoot :: BS.ByteString
  , bEventToStepRoot :: BS.ByteString
  , bWithdrawalCount :: Integer
  , bForcedCount :: Integer
  , bL2Count :: Integer
  , bDepositCount :: Integer
  , bTotalEventCount :: Integer
  , bStepCount :: Integer
  }

defaultBlock :: Block
defaultBlock =
  Block
    { bPrevUtxosRoot = ledgerRoot 0xa0
    , bUtxosRoot = ledgerRoot 0xa2
    , bWithdrawalsRoot = BS.replicate 32 0x02
    , bForcedRoot = BS.replicate 32 0x03
    , bTransactionsRoot = BS.replicate 32 0x04
    , bDepositsRoot = singleRoot depositsDomain 1 (serialise depositKeyId) (serialise depositInfo)
    , bTraceRoot = commitCountedRoot traceDomain (traceRawRootOf step0 step1) 2
    , bEventToStepRoot =
        singleRoot eventToStepDomain 5 (serialise withdrawalKey) (serialise eventToStep0)
    , bWithdrawalCount = 2
    , bForcedCount = 1
    , bL2Count = 1
    , bDepositCount = 1
    , bTotalEventCount = 5
    , bStepCount = 2
    }

-- | The deposit tree is keyed by the id itself, not by the event key wrapping it.
depositKeyId :: PD.Data
depositKeyId = outRef 0x72

-- | @EventToStepValue@ placing the withdrawal event at step 0, phase @Withdrawal@.
eventToStep0 :: PD.Data
eventToStep0 = PD.Constr 0 [PD.I 0, PD.Constr 0 []]

headerData :: Block -> PD.Data
headerData b =
  PD.Constr
    0
    [ PD.B (bPrevUtxosRoot b)
    , PD.B (bUtxosRoot b)
    , PD.B (bWithdrawalsRoot b)
    , PD.B (bForcedRoot b)
    , PD.B (bTransactionsRoot b)
    , PD.B (bDepositsRoot b)
    , PD.B (bTraceRoot b)
    , PD.B (bEventToStepRoot b)
    , PD.B (BS.replicate 32 0x09)
    , PD.I (bWithdrawalCount b)
    , PD.I (bForcedCount b)
    , PD.I (bL2Count b)
    , PD.I (bDepositCount b)
    , PD.I (bTotalEventCount b)
    , PD.I (bStepCount b)
    , PD.I 1
    , PD.I 100
    , PD.I 200
    , PD.I 77
    , PD.I 0
    , PD.I 44
    , PD.I 155381
    , PD.B (BS.replicate 28 0xaa)
    , PD.B (BS.replicate 28 0xbb)
    , PD.I 1
    ]

headerT :: forall s. Block -> Term s PHeaderV1
headerT = fromData . headerData

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Transition Trace Proof Tests"
    [ testGroup "the header's counts" countTests
    , testGroup "the phase of a step index" phaseTests
    , testGroup "reading a source proof" sourceReadTests
    , testGroup "source membership" sourceMembershipTests
    , testGroup "the trace boundary" boundaryTests
    , testGroup "the trace link" linkTests
    , testGroup "a step's phase" badPhaseTests
    , testGroup "the trace/event-to-step binding" bindingTests
    , testGroup "the event-to-step mismatch fault" mismatchTests
    , testGroup "the source mismatch fault" sourceMismatchTests
    , testGroup "the ledger trie" ledgerTests
    , testGroup "the one-step binding" oneStepBindingTests
    , testGroup "the withdrawal transitions" withdrawalTransitionTests
    , testGroup "the forced no-op transition" forcedTransitionTests
    , testGroup "the count faults" countFaultTests
    , testGroup "the duplicate-event fault" duplicateTests
    , testGroup "the L2 transaction transition" l2Tests
    , testGroup "the deposit transition" depositTests
    , testGroup "the L1-event faults" l1EventTests
    , testGroup "the one-step dispatcher" oneStepDispatchTests
    , testGroup "the fault proof and its entry points" faultProofTests
    ]

--------------------------------------------------------------------------------

countTests :: [TestTree]
countTests =
  [ testCase "a block's six step counts are non-negative" $
      passertEval (pheaderCountsAreNonNegative (headerT defaultBlock))
  , testCase "a negative withdrawal count is not" $
      prefuses $
        pheaderCountsAreNonNegative (headerT defaultBlock {bWithdrawalCount = -1})
  , testCase "a negative total is not" $
      prefuses $
        pheaderCountsAreNonNegative (headerT defaultBlock {bTotalEventCount = -1})
  , testCase "a negative step count is not" $
      prefuses (pheaderCountsAreNonNegative (headerT defaultBlock {bStepCount = -1}))
  , testCase "the four sources sum to the block's event total" $
      passertEval (psourceCountSum (headerT defaultBlock) #== 5)
  , testCase "…and the sum does not include the trace's own steps" $
      passertEval $
        psourceCountSum (headerT defaultBlock {bStepCount = 99}) #== 5
  ]

--------------------------------------------------------------------------------

phaseTests :: [TestTree]
phaseTests =
  [ testCase "index 0 is the first withdrawal" (passertEval (phaseAt 0 #== phase 0))
  , testCase "index 1 is the last withdrawal" (passertEval (phaseAt 1 #== phase 0))
  , testCase "index 2 is the forced transaction" (passertEval (phaseAt 2 #== phase 1))
  , testCase "index 3 is the L2 transaction" (passertEval (phaseAt 3 #== phase 2))
  , testCase "index 4 is the deposit" (passertEval (phaseAt 4 #== phase 3))
  , testCase "aborts past the block's last event" (pfails (phaseAt 5))
  , testCase "aborts on a negative index" (pfails (phaseAt (-1)))
  , testCase "aborts on a header with a negative count" $
      pfails $
        pphaseForStepIndex (headerT defaultBlock {bDepositCount = -1}) 0
  , testCase "a phase with no events is skipped over" $
      passertEval $
        pphaseForStepIndex (headerT defaultBlock {bWithdrawalCount = 0, bTotalEventCount = 3}) 0
          #== phase 1
  , testCase "aborts when the total outruns the four sources" $
      pfails $
        pphaseForStepIndex (headerT defaultBlock {bTotalEventCount = 6}) 5
  , testCase "each event key names its own phase" $
      passertEval $
        foldr
          (#&&)
          (pconstant True)
          [ peventKeyPhase (pconstant k) #== phase t
          | (t, k) <- [(0, withdrawalKey), (2, l2Key), (3, depositKey)]
          ]
  ]
  where
    phaseAt = pphaseForStepIndex (headerT defaultBlock)
    phase :: forall s. Integer -> Term s (PAsData PTransitionPhase)
    phase t = asDataTerm (PD.Constr t [])

--------------------------------------------------------------------------------

sourceReadTests :: [TestTree]
sourceReadTests =
  [ testCase "a withdrawal source proof names a withdrawal event key" $
      passertEval $
        peventKeyFromSourceMembership (withdrawalSource 2)
          #== pconstant (PD.Constr 0 [outRef 0x71])
  , testCase "a deposit source proof names a deposit event key" $
      passertEval $
        peventKeyFromSourceMembership depositSource #== pconstant depositKey
  , testCase "…and its phase follows from that key" $
      passertEval $
        psourceMembershipPhase depositSource #== asDataTerm (PD.Constr 3 [])
  , testCase "a withdrawal source proof's phase is the withdrawal phase" $
      passertEval $
        psourceMembershipPhase (withdrawalSource 2) #== asDataTerm (PD.Constr 0 [])
  , testCase "a non-membership proof names an event key the same way" $
      passertEval $
        psourceNonMembershipEventKey withdrawalAbsent #== pconstant withdrawalKey
  , testCase "aborts on a fifth source arm" $
      pfails $
        peventKeyFromSourceMembership
          (asDataTerm (PD.Constr 4 [PD.Constr 0 []]))
  ]

--------------------------------------------------------------------------------

sourceMembershipTests :: [TestTree]
sourceMembershipTests =
  [ testCase "a deposit is in the block's deposit tree" $
      passertEval (pverifySourceMembership (headerT defaultBlock) depositSource)
  , testCase "…and not in a block that commits another deposit root" $
      prefuses $
        pverifySourceMembership
          (headerT defaultBlock {bDepositsRoot = BS.replicate 32 0x0d})
          depositSource
  , testCase "a deposit proof presented as a withdrawal one is refused" $
      prefuses $
        pverifySourceMembership
          (headerT defaultBlock)
          (asDataTerm (PD.Constr 0 [depositMembership]))
  , testCase "an L2 transaction is keyed by raw bytes, not by CBOR" $
      passertEval $
        pverifySourceMembership (headerT l2Block) l2Source
  , testCase "…and serialising that key addresses another slot" $
      prefuses $
        pverifySourceMembership
          (headerT l2Block {bTransactionsRoot = l2RootCbor})
          l2Source
  , testCase "an absent withdrawal is absent from the empty withdrawal tree" $
      passertEval (pverifySourceNonMembership (headerT noWithdrawalsBlock) withdrawalAbsent)
  , testCase "…but not from a tree the header does not commit" $
      prefuses $
        pverifySourceNonMembership
          (headerT noWithdrawalsBlock {bWithdrawalsRoot = BS.replicate 32 0x0e})
          withdrawalAbsent
  , testCase "a non-membership proof under the wrong domain is refused" $
      prefuses $
        pverifySourceNonMembership
          (headerT noWithdrawalsBlock)
          (asDataTerm (PD.Constr 0 [emptyNonMembership depositsDomain withdrawalKeyId]))
  ]

--------------------------------------------------------------------------------

boundaryTests :: [TestTree]
boundaryTests =
  [ testCase "a trace that starts where the block did is no fault" $
      prefuses (boundary defaultBlock traceStart (traceProofOf step0 step1 0))
  , testCase "a trace that starts elsewhere is" $
      passertEval $
        boundary
          defaultBlock {bPrevUtxosRoot = ledgerRoot 0xb0}
          traceStart
          (traceProofOf step0 step1 0)
  , testCase "a trace that ends where the block says is no fault" $
      prefuses (boundary defaultBlock traceEnd (traceProofOf step0 step1 1))
  , testCase "a trace that ends elsewhere is" $
      passertEval $
        boundary
          defaultBlock {bUtxosRoot = ledgerRoot 0xb2}
          traceEnd
          (traceProofOf step0 step1 1)
  , testCase "the start fault must be stated about the first step" $
      prefuses $
        boundary
          defaultBlock {bPrevUtxosRoot = ledgerRoot 0xb0}
          traceStart
          (traceProofOf step0 step1 1)
  , testCase "the end fault must be stated about the last step" $
      prefuses $
        boundary
          defaultBlock {bUtxosRoot = ledgerRoot 0xb2}
          traceEnd
          (traceProofOf step0 step1 0)
  , testCase "aborts against a block whose trace root is another tree's" $
      pfails $
        boundary
          defaultBlock {bTraceRoot = emptyRoot}
          traceStart
          (traceProofOf step0 step1 0)
  , testCase "aborts on a block with no steps at all" $
      pfails $
        boundary
          defaultBlock {bStepCount = 0, bTraceRoot = emptyRoot}
          traceStart
          (traceProofOf step0 step1 0)
  ]
  where
    boundary block side proof =
      pvalidateTraceBoundary (headerT block) side (fromData proof)
    traceStart, traceEnd :: forall s. Term s (PAsData PTraceBoundarySide)
    traceStart = asDataTerm (PD.Constr 0 [])
    traceEnd = asDataTerm (PD.Constr 1 [])

--------------------------------------------------------------------------------

linkTests :: [TestTree]
linkTests =
  [ testCase "two steps that meet are no fault" $
      prefuses (link defaultBlock (adjacentOf step0 step1))
  , testCase "two steps that do not meet are" $
      passertEval (link brokenBlock (adjacentOf step0 brokenStep1))
  , testCase "refuses a pair that is not the tree the header commits" $
      prefuses (link defaultBlock (adjacentOf step0 brokenStep1))
  ]
  where
    link block adjacent = pvalidateTraceLink (headerT block) (fromData adjacent)
    brokenStep1 = stepWith 1 withdrawalKey 0 (ledgerRoot 0xb1) (ledgerRoot 0xa2)
    brokenBlock =
      defaultBlock
        { bTraceRoot = commitCountedRoot traceDomain (traceRawRootOf step0 brokenStep1) 2
        }

--------------------------------------------------------------------------------

badPhaseTests :: [TestTree]
badPhaseTests =
  [ testCase "a withdrawal step at a withdrawal index is well placed" $
      prefuses (badPhase defaultBlock (traceProofOf step0 step1 0))
  , testCase "a step whose phase is not its index's is not" $
      passertEval (badPhase depositPhaseBlock (traceProofOf depositPhaseStep step1 0))
  , testCase "a step whose phase is not its event key's is not" $
      passertEval (badPhase keyMismatchBlock (traceProofOf keyMismatchStep step1 0))
  , testCase "aborts on a step index the block does not have" $
      pfails (badPhase farStepBlock (traceProofOf farStep step1 0))
  ]
  where
    badPhase block proof = ptraceHasBadPhase (headerT block) (fromData proof)
    -- Index 0 is a withdrawal slot; this step claims to be a deposit.
    depositPhaseStep = stepWith 0 depositKey 3 (ledgerRoot 0xa0) midRoot
    depositPhaseBlock = withTrace depositPhaseStep step1
    -- Index 0's phase is right, but the event key is a deposit's.
    keyMismatchStep = stepWith 0 depositKey 0 (ledgerRoot 0xa0) midRoot
    keyMismatchBlock = withTrace keyMismatchStep step1
    farStep = stepWith 9 withdrawalKey 0 (ledgerRoot 0xa0) midRoot
    farStepBlock = withTrace farStep step1

withTrace :: PD.Data -> PD.Data -> Block
withTrace s0 s1 =
  defaultBlock {bTraceRoot = commitCountedRoot traceDomain (traceRawRootOf s0 s1) 2}

--------------------------------------------------------------------------------

bindingTests :: [TestTree]
bindingTests =
  [ testCase "a step and the map agreeing on event, index and phase" $
      passertEval $
        binding defaultBlock (traceProofOf step0 step1 0) eventToStepMembership
  , testCase "refuses a map entry naming another step index" $
      prefuses $
        binding
          (blockMapping (PD.Constr 0 [PD.I 1, PD.Constr 0 []]))
          (traceProofOf step0 step1 0)
          (mapMembership (PD.Constr 0 [PD.I 1, PD.Constr 0 []]))
  , testCase "refuses a map entry naming another phase" $
      prefuses $
        binding
          (blockMapping (PD.Constr 0 [PD.I 0, PD.Constr 3 []]))
          (traceProofOf step0 step1 0)
          (mapMembership (PD.Constr 0 [PD.I 0, PD.Constr 3 []]))
  , testCase "refuses a map entry keyed by another event" $
      prefuses $
        binding
          otherKeyBlock
          (traceProofOf step0 step1 0)
          ( singleMembership
              eventToStepDomain
              5
              (serialise depositKey)
              (serialise eventToStep0)
              depositKey
              eventToStep0
          )
  , testCase "refuses a binding the header's map root does not commit" $
      prefuses $
        binding
          defaultBlock {bEventToStepRoot = emptyRoot}
          (traceProofOf step0 step1 0)
          eventToStepMembership
  ]
  where
    binding block proof eventToStep =
      pverifyTraceBindingToEventToStep (headerT block) (fromData proof) (fromData eventToStep)
    otherKeyBlock =
      defaultBlock
        { bEventToStepRoot =
            singleRoot eventToStepDomain 5 (serialise depositKey) (serialise eventToStep0)
        }

--------------------------------------------------------------------------------

mismatchTests :: [TestTree]
mismatchTests =
  [ testCase "a step the map agrees with is no fault" $
      prefuses $
        mismatch defaultBlock (traceProofOf step0 step1 0) (membershipArm eventToStepMembership)
  , testCase "a map entry at another index is" $
      passertEval $
        mismatch
          (blockMapping otherIndex)
          (traceProofOf step0 step1 0)
          (membershipArm (mapMembership otherIndex))
  , testCase "a map entry in another phase is" $
      passertEval $
        mismatch
          (blockMapping otherPhase)
          (traceProofOf step0 step1 0)
          (membershipArm (mapMembership otherPhase))
  , testCase "a step the map agrees with but whose phase is wrong is" $
      passertEval $
        mismatch
          badPhaseBlock
          (traceProofOf badPhaseStep step1 0)
          (membershipArm eventToStepMembership)
  , testCase "a map that does not hold the step's event at all is" $
      passertEval $
        mismatch
          emptyMapBlock
          (traceProofOf step0 step1 0)
          (nonMembershipArm (emptyNonMembership eventToStepDomain withdrawalKey))
  , testCase "…but not when the absent key is somebody else's" $
      prefuses $
        mismatch
          emptyMapBlock
          (traceProofOf step0 step1 0)
          (nonMembershipArm (emptyNonMembership eventToStepDomain depositKey))
  , testCase "aborts when the map entry is not the one the header commits" $
      pfails $
        mismatch
          defaultBlock
          (traceProofOf step0 step1 0)
          (membershipArm (mapMembership otherIndex))
  , testCase "aborts when the trace proof is not the one the header commits" $
      pfails $
        mismatch
          defaultBlock {bTraceRoot = emptyRoot}
          (traceProofOf step0 step1 0)
          (membershipArm eventToStepMembership)
  ]
  where
    mismatch block proof eventToStep =
      pvalidateEventToStepMismatch (headerT block) (fromData proof) (fromData eventToStep)
    otherIndex = PD.Constr 0 [PD.I 1, PD.Constr 0 []]
    otherPhase = PD.Constr 0 [PD.I 0, PD.Constr 3 []]
    -- Index 0 is a withdrawal slot; this step files itself as a deposit while
    -- the map still agrees with it entry for entry.
    badPhaseStep = stepWith 0 withdrawalKey 3 (ledgerRoot 0xa0) midRoot
    badPhaseBlock = withTrace badPhaseStep step1

membershipArm, nonMembershipArm :: PD.Data -> PD.Data
membershipArm proof = PD.Constr 0 [proof]
nonMembershipArm proof = PD.Constr 1 [proof]

--------------------------------------------------------------------------------

sourceMismatchTests :: [TestTree]
sourceMismatchTests =
  [ testCase "an event in the trace and in no source is a fault" $
      passertEval $
        sourceMismatch
          noWithdrawalsBlock
          ( PD.Constr
              0
              [ traceProofOf step0 step1 0
              , mapMembershipAt 3 eventToStep0
              , PD.Constr 0 [emptyNonMembership withdrawalsDomain withdrawalKeyId]
              ]
          )
  , testCase "…but not when the absence proven is somebody else's" $
      prefuses $
        sourceMismatch
          noWithdrawalsBlock
          ( PD.Constr
              0
              [ traceProofOf step0 step1 0
              , mapMembershipAt 3 eventToStep0
              , PD.Constr 0 [emptyNonMembership withdrawalsDomain (outRef 0x99)]
              ]
          )
  , testCase "an event in a source and in no trace is a fault" $
      passertEval $
        sourceMismatch
          emptyMapBlock
          ( PD.Constr
              1
              [ depositMembershipArm
              , emptyNonMembership eventToStepDomain depositKey
              ]
          )
  , testCase "…but not when the absence proven is of another event" $
      prefuses $
        sourceMismatch
          emptyMapBlock
          ( PD.Constr
              1
              [ depositMembershipArm
              , emptyNonMembership eventToStepDomain withdrawalKey
              ]
          )
  , testCase "an event filed under a phase that is not its own is a fault" $
      passertEval $
        sourceMismatch
          phaseMismatchBlock
          (PD.Constr 2 [traceProofOf phaseMismatchStep step1 0, depositMembershipArm])
  , testCase "…but not when the step's phase is the event key's own" $
      prefuses $
        sourceMismatch
          agreeingBlock
          (PD.Constr 2 [traceProofOf agreeingStep step1 0, depositMembershipArm])
  , testCase "refuses a phase-mismatch fault about another event entirely" $
      prefuses $
        sourceMismatch
          defaultBlock
          (PD.Constr 2 [traceProofOf step0 step1 0, depositMembershipArm])
  ]
  where
    sourceMismatch block witness =
      pvalidateSourceMembershipMismatch (headerT block) (asDataTerm witness)
    -- A deposit event filed in the trace under the withdrawal phase.
    phaseMismatchStep = stepWith 0 depositKey 0 (ledgerRoot 0xa0) midRoot
    phaseMismatchBlock = withTrace phaseMismatchStep step1
    agreeingStep = stepWith 0 depositKey 3 (ledgerRoot 0xa0) midRoot
    agreeingBlock = withTrace agreeingStep step1

--------------------------------------------------------------------------------
-- Shared proofs
--------------------------------------------------------------------------------

withdrawalKeyId :: PD.Data
withdrawalKeyId = outRef 0x71

eventToStepMembership :: PD.Data
eventToStepMembership = mapMembership eventToStep0

mapMembership :: PD.Data -> PD.Data
mapMembership = mapMembershipAt 5

{- | The map's entry for the withdrawal event, under a stated event total.

The total travels as a parameter because a membership proof must publish the same
count the header does, and not every block here has five events.
-}
mapMembershipAt :: Integer -> PD.Data -> PD.Data
mapMembershipAt count value =
  singleMembership
    eventToStepDomain
    count
    (serialise withdrawalKey)
    (serialise value)
    withdrawalKey
    value

-- | A block whose event-to-step tree holds the given value for the same key.
blockMapping :: PD.Data -> Block
blockMapping value =
  defaultBlock
    { bEventToStepRoot =
        singleRoot eventToStepDomain 5 (serialise withdrawalKey) (serialise value)
    }

depositMembership :: PD.Data
depositMembership =
  singleMembership
    depositsDomain
    1
    (serialise depositKeyId)
    (serialise depositInfo)
    depositKeyId
    depositInfo

depositMembershipArm :: PD.Data
depositMembershipArm = PD.Constr 3 [depositMembership]

depositSource :: forall s. Term s (PAsData PSourceMembershipProof)
depositSource = asDataTerm depositMembershipArm

withdrawalSource :: forall s. Integer -> Term s (PAsData PSourceMembershipProof)
withdrawalSource count =
  asDataTerm
    ( PD.Constr
        0
        [ singleMembership
            withdrawalsDomain
            count
            (serialise withdrawalKeyId)
            (serialise withdrawalInfo)
            withdrawalKeyId
            withdrawalInfo
        ]
    )

withdrawalAbsent :: forall s. Term s (PAsData PSourceNonMembershipProof)
withdrawalAbsent =
  asDataTerm (PD.Constr 0 [emptyNonMembership withdrawalsDomain withdrawalKeyId])

{- | A block with no withdrawals at all, so an absence can be proven against its
withdrawal tree. Its four source counts still sum to the event total.
-}
noWithdrawalsBlock :: Block
noWithdrawalsBlock =
  defaultBlock
    { bWithdrawalCount = 0
    , bTotalEventCount = 3
    , bWithdrawalsRoot = emptyRoot
    , bEventToStepRoot =
        singleRoot eventToStepDomain 3 (serialise withdrawalKey) (serialise eventToStep0)
    }

{- | A block whose event-to-step map is empty.

Its deposit count stays at one, which no predicate here cross-checks against the
event total: the fault being stated is precisely that a source holds an event the
map does not.
-}
emptyMapBlock :: Block
emptyMapBlock = defaultBlock {bTotalEventCount = 0, bEventToStepRoot = emptyRoot}

--------------------------------------------------------------------------------
-- The L2 tree, which keys itself in raw bytes
--------------------------------------------------------------------------------

l2IdBytes :: BS.ByteString
l2IdBytes = BS.replicate 32 0x73

l2ValueBytes :: BS.ByteString
l2ValueBytes = BS.replicate 48 0x74

l2Source :: forall s. Term s (PAsData PSourceMembershipProof)
l2Source =
  asDataTerm
    ( PD.Constr
        2
        [ PD.Constr
            0
            [ PD.Constr transactionsDomain []
            , PD.B (commitCountedRoot transactionsDomain l2Raw 1)
            , PD.B l2Raw
            , PD.I 1
            , PD.B l2IdBytes
            , PD.B l2ValueBytes
            , PD.List []
            ]
        ]
    )
  where
    l2Raw = singleEntryRoot l2IdBytes l2ValueBytes

l2Block :: Block
l2Block =
  defaultBlock
    { bTransactionsRoot =
        commitCountedRoot transactionsDomain (singleEntryRoot l2IdBytes l2ValueBytes) 1
    }

-- | The same tree, keyed as if the id had been serialised first.
l2RootCbor :: BS.ByteString
l2RootCbor =
  commitCountedRoot
    transactionsDomain
    (singleEntryRoot (serialise (PD.B l2IdBytes)) l2ValueBytes)
    1

--------------------------------------------------------------------------------
-- The ledger trie
--------------------------------------------------------------------------------

{- | @proof.ledger_outref_key@, from the native-transaction encoding.

A two-element definite array: the transaction id as a definite byte string, then
the output index in §5.3's __fixed three-byte__ @19 XXXX@ form. Deliberately not
the serialisation of the Plutus @OutputReference@ constructor — the point of the
encoding is that a ledger key and a transaction's own input-set item are the same
bytes.

The fixed index is the one deliberate departure from minimal CBOR in the whole
format, and it is what makes every input item exactly 38 bytes and therefore
arithmetically addressable. Writing @cborInt@ here instead produces the minimal
two-byte form for anything under 256 and a key that addresses a different slot —
which is how this reference was wrong before the ledger tests caught it.
-}
ledgerKey :: PD.Data -> BS.ByteString
ledgerKey (PD.Constr _ [PD.Constr _ [PD.B txId], PD.I index]) =
  BS.concat
    [ "\x82"
    , "\x58\x20"
    , txId
    , BS.pack [0x19, fromIntegral (index `div` 256), fromIntegral (index `mod` 256)]
    ]
ledgerKey d = error ("ledgerKey: not an output reference: " <> show d)

utxoRef :: PD.Data
utxoRef = outRef 0x81

utxoKey, utxoValue :: BS.ByteString
utxoKey = ledgerKey utxoRef
utxoValue = BS.replicate 40 0x82

-- | The ledger trie before the withdrawal's UTxO is spent, and after.
ledgerBefore, ledgerAfter :: BS.ByteString
ledgerBefore = singleEntryRoot utxoKey utxoValue
ledgerAfter = libraryNullHash

-- | @proof.LedgerDeleteWitness@ / @LedgerInsertWitness@ — four fields each.
deleteWitness :: BS.ByteString -> BS.ByteString -> PD.Data
deleteWitness key value = PD.Constr 0 [PD.B key, PD.B value, PD.List [], PD.List []]

insertWitness :: BS.ByteString -> BS.ByteString -> PD.Data
insertWitness key value = PD.Constr 0 [PD.B key, PD.B value, PD.List [], PD.List []]

ledgerTests :: [TestTree]
ledgerTests =
  [ testCase "a UTxO's ledger key is its native-transaction encoding" $
      passertEval $
        pledgerOutrefKey (pconstant utxoRef) #== pconstant utxoKey
  , testCase "a different output index is a different key" $
      passertEval $
        pnot # (pledgerOutrefKey (pconstant (outRef 0x82)) #== pconstant utxoKey)
  , testCase "aborts on a transaction id that is not 32 bytes" $
      pfails $
        pledgerOutrefKey
          (pconstant (PD.Constr 0 [PD.Constr 0 [PD.B (BS.replicate 31 0x01)], PD.I 0]))
  , testCase "a one-entry trie holds its entry" $
      passertEval $
        pverifyLedgerMembership
          (pconstant ledgerBefore)
          (pconstant utxoKey)
          (pconstant utxoValue)
          (fromData (PD.List []))
  , testCase "…under that key and no other value" $
      prefuses $
        pverifyLedgerMembership
          (pconstant ledgerBefore)
          (pconstant utxoKey)
          (pconstant (BS.replicate 40 0x83))
          (fromData (PD.List []))
  , testCase "…and not under another key" $
      prefuses $
        pverifyLedgerMembership
          (pconstant ledgerBefore)
          (pconstant (ledgerKey (outRef 0x84)))
          (pconstant utxoValue)
          (fromData (PD.List []))
  , testCase "an empty trie holds nothing" $
      passertEval $
        pverifyLedgerNonMembership
          (pconstant emptyMerkleRoot)
          (pconstant utxoKey)
          (fromData (PD.List []))
  , testCase "…and the empty sentinel is translated, not taken literally" $
      passertEval $
        pinsertRoot
          (pconstant emptyMerkleRoot)
          (pconstant utxoKey)
          (pconstant utxoValue)
          (fromData (PD.List []))
          #== pconstant ledgerBefore
  , testCase "aborts proving absence of a key the trie holds" $
      pfails $
        pverifyLedgerNonMembership
          (pconstant ledgerBefore)
          (pconstant utxoKey)
          (fromData (PD.List []))
  , testCase "deleting the only entry empties the trie" $
      passertEval $
        pdeleteRoot
          (pconstant ledgerBefore)
          (pconstant utxoKey)
          (pconstant utxoValue)
          (fromData (PD.List []))
          #== pconstant ledgerAfter
  , {- The MPF library's empty root is 32 zero bytes and Midgard's sentinel is
       @blake2b_256("")@. @ledger_trie@ translates on the way /in/, and neither
       @insert_root@ nor @delete_root@ translates back on the way out — so a trace
       that empties the ledger publishes the zero root. That asymmetry is Aiken's
       too, and it is reproduced rather than smoothed over. -}
    testCase "and the emptied root is the library's, not Midgard's sentinel" $
      passertEval $
        pnot # (pconstant @PByteString ledgerAfter #== pconstant emptyMerkleRoot)
  , testCase "a delete witness applies to the root it proves against" $
      passertEval $
        papplyDeleteWitness
          (pconstant ledgerBefore)
          (fromData (deleteWitness utxoKey utxoValue))
          #== pconstant ledgerAfter
  , testCase "aborts when the delete witness names a value the trie does not hold" $
      pfails $
        papplyDeleteWitness
          (pconstant ledgerBefore)
          (fromData (deleteWitness utxoKey (BS.replicate 40 0x85)))
  , testCase "an insert witness applies to an empty trie" $
      passertEval $
        papplyInsertWitness
          (pconstant emptyMerkleRoot)
          (fromData (insertWitness utxoKey utxoValue))
          #== pconstant ledgerBefore
  , testCase "aborts inserting a key the trie already holds" $
      pfails $
        papplyInsertWitness
          (pconstant ledgerBefore)
          (fromData (insertWitness utxoKey utxoValue))
  ]

--------------------------------------------------------------------------------
-- The transition fixture
--------------------------------------------------------------------------------

{- | A block with one event of each class, whose first step is the withdrawal
under test.

The withdrawal and forced trees hold one entry each, so a one-step binding has
something real to bind to; the counts and the event-to-step total move with them.
-}
transitionBlock :: Block
transitionBlock =
  defaultBlock
    { bWithdrawalCount = 1
    , bTotalEventCount = 4
    , bWithdrawalsRoot =
        singleRoot withdrawalsDomain 1 (serialise withdrawalId) (serialise validWithdrawal)
    , bForcedRoot =
        singleRoot forcedDomain 1 (serialise forcedOrderId) (serialise rejectedForced)
    , bEventToStepRoot =
        singleRoot eventToStepDomain 4 (serialise withdrawalKey) (serialise eventToStep0)
    , bTraceRoot = commitCountedRoot traceDomain (traceRawRootOf honestStep step1) 2
    }

withdrawalId, forcedOrderId :: PD.Data
withdrawalId = outRef 0x71
forcedOrderId = outRef 0x75

{- | @ledger_state.WithdrawalInfo@ — body, signature, validity.

Only the body's first field and the validity are read here; the rest are
placeholders, which the port's field access leaves untouched.
-}
withdrawalInfoWith :: PD.Data -> Integer -> PD.Data
withdrawalInfoWith l2Outref validity =
  PD.Constr
    0
    [ PD.Constr 0 [l2Outref, PD.B "owner", PD.Map [], PD.B "addr", PD.Constr 0 []]
    , PD.B "signature"
    , PD.Constr validity []
    ]

validWithdrawal, invalidWithdrawal :: PD.Data
validWithdrawal = withdrawalInfoWith utxoRef 0
invalidWithdrawal = withdrawalInfoWith utxoRef 1

-- | @ledger_state.ForcedInclusionTxV1@ — id, proof source, operator verdict.
forcedWith :: Integer -> PD.Data
forcedWith validity =
  PD.Constr
    0
    [ PD.B (BS.replicate 32 0x76)
    , PD.Constr 0 [PD.B "compact", PD.B "witness", PD.B "lengths"]
    , PD.Constr validity []
    ]

acceptedForced, rejectedForced :: PD.Data
acceptedForced = forcedWith 0
rejectedForced = forcedWith 4

{- | The withdrawal step, in the three shapes the transitions distinguish: the
one that spends the UTxO it should, the one that publishes another root, and the
one that changes nothing.
-}
honestStep, guiltyStep, noOpStep :: PD.Data
honestStep = stepWith 0 withdrawalKey 0 ledgerBefore ledgerAfter
guiltyStep = stepWith 0 withdrawalKey 0 ledgerBefore (ledgerRoot 0xcc)
noOpStep = stepWith 0 withdrawalKey 0 ledgerBefore ledgerBefore

withdrawalMembership :: PD.Data -> PD.Data
withdrawalMembership info =
  singleMembership
    withdrawalsDomain
    1
    (serialise withdrawalId)
    (serialise info)
    withdrawalId
    info

forcedMembership :: PD.Data -> PD.Data
forcedMembership value =
  singleMembership
    forcedDomain
    1
    (serialise forcedOrderId)
    (serialise value)
    forcedOrderId
    value

-- | The block's trace tree rebuilt around a chosen first step.
blockWithStep :: PD.Data -> Block
blockWithStep s0 =
  transitionBlock
    {bTraceRoot = commitCountedRoot traceDomain (traceRawRootOf s0 step1) 2}

{- | The same, for a block whose withdrawal tree holds the /invalid/ withdrawal.

The tree has to move with the leaf: a no-op fault is about a withdrawal the
operator rejected, and a membership proof of one the header does not commit
fails at the binding instead of at the fault.
-}
invalidBlockWithStep :: PD.Data -> Block
invalidBlockWithStep s0 =
  (blockWithStep s0)
    { bWithdrawalsRoot =
        singleRoot withdrawalsDomain 1 (serialise withdrawalId) (serialise invalidWithdrawal)
    }

mapMembership4 :: PD.Data
mapMembership4 = mapMembershipAt 4 eventToStep0

--------------------------------------------------------------------------------

oneStepBindingTests :: [TestTree]
oneStepBindingTests =
  [ testCase "a step, the map and the withdrawal tree agreeing" $
      passertEval $
        binding
          transitionBlock
          (traceProofOf honestStep step1 0)
          mapMembership4
          (withdrawalMembership validWithdrawal)
  , testCase "refuses a step naming an event the source tree does not hold" $
      prefuses $
        binding
          otherIdBlock
          (traceProofOf honestStep step1 0)
          mapMembership4
          otherIdMembership
  , testCase "refuses a step filed under a phase that is not the withdrawal one" $
      prefuses $
        binding
          (blockWithStep depositPhaseStep)
          (traceProofOf depositPhaseStep step1 0)
          mapMembership4
          (withdrawalMembership validWithdrawal)
  , testCase "refuses a withdrawal whose tree the header does not commit" $
      prefuses $
        binding
          transitionBlock {bWithdrawalsRoot = BS.replicate 32 0x0f}
          (traceProofOf honestStep step1 0)
          mapMembership4
          (withdrawalMembership validWithdrawal)
  ]
  where
    binding block proof eventToStep source =
      pvalidateWithdrawalOneStepBinding
        (headerT block)
        (fromData proof)
        (fromData eventToStep)
        (fromData source)
    -- The step still says event 0x71 while the source tree holds 0x99.
    otherId = outRef 0x99
    otherIdMembership =
      singleMembership
        withdrawalsDomain
        1
        (serialise otherId)
        (serialise validWithdrawal)
        otherId
        validWithdrawal
    otherIdBlock =
      transitionBlock
        { bWithdrawalsRoot =
            singleRoot withdrawalsDomain 1 (serialise otherId) (serialise validWithdrawal)
        }
    depositPhaseStep = stepWith 0 withdrawalKey 3 ledgerBefore ledgerAfter

--------------------------------------------------------------------------------

withdrawalTransitionTests :: [TestTree]
withdrawalTransitionTests =
  [ testCase "a valid withdrawal that spends its UTxO is no fault" $
      prefuses $
        valid (blockWithStep honestStep) honestStep (withdrawalMembership validWithdrawal) spend
  , testCase "a valid withdrawal that publishes another root is" $
      passertEval $
        valid (blockWithStep guiltyStep) guiltyStep (withdrawalMembership validWithdrawal) spend
  , testCase "…including one that changed nothing at all" $
      passertEval $
        valid (blockWithStep noOpStep) noOpStep (withdrawalMembership validWithdrawal) spend
  , testCase "aborts when the withdrawal is not one the operator called valid" $
      pfails $
        valid (blockWithStep honestStep) honestStep (withdrawalMembership invalidWithdrawal) spend
  , testCase "aborts when the spend witness names another UTxO" $
      pfails $
        valid
          (blockWithStep honestStep)
          honestStep
          (withdrawalMembership validWithdrawal)
          (deleteWitness (ledgerKey (outRef 0x88)) utxoValue)
  , testCase "aborts when the spend witness names a value the ledger does not hold" $
      pfails $
        valid
          (blockWithStep honestStep)
          honestStep
          (withdrawalMembership validWithdrawal)
          (deleteWitness utxoKey (BS.replicate 40 0x89))
  , testCase "an invalid withdrawal that changed nothing is no fault" $
      prefuses $
        noOp (invalidBlockWithStep noOpStep) noOpStep (withdrawalMembership invalidWithdrawal)
  , testCase "an invalid withdrawal that moved the ledger is" $
      passertEval $
        noOp (invalidBlockWithStep honestStep) honestStep (withdrawalMembership invalidWithdrawal)
  , testCase "aborts stating the no-op fault about a valid withdrawal" $
      pfails $
        noOp (blockWithStep honestStep) honestStep (withdrawalMembership validWithdrawal)
  ]
  where
    spend = deleteWitness utxoKey utxoValue
    valid block s0 source witness =
      pvalidateValidWithdrawalTransition
        (headerT block)
        (fromData (traceProofOf s0 step1 0))
        (fromData mapMembership4)
        (fromData source)
        (fromData witness)
    noOp block s0 source =
      pvalidateInvalidWithdrawalNoOpTransition
        (headerT block)
        (fromData (traceProofOf s0 step1 0))
        (fromData mapMembership4)
        (fromData source)

--------------------------------------------------------------------------------

forcedTransitionTests :: [TestTree]
forcedTransitionTests =
  [ testCase "a rejected forced transaction that changed nothing is no fault" $
      prefuses $
        noOp (blockWithForced forcedNoOpStep) forcedNoOpStep rejectedForced
  , testCase "a rejected forced transaction that moved the ledger is" $
      passertEval $
        noOp (blockWithForced forcedMovingStep) forcedMovingStep rejectedForced
  , testCase "aborts stating the fault about one the operator accepted" $
      pfails $
        noOp (blockWithForced forcedNoOpStep) forcedNoOpStep acceptedForced
  , testCase "refuses a step filed under the withdrawal phase" $
      prefuses $
        noOp (blockWithForced withdrawalPhaseStep) withdrawalPhaseStep rejectedForced
  ]
  where
    forcedEventKey = PD.Constr 1 [forcedOrderId]
    forcedNoOpStep = stepWith 0 forcedEventKey 1 ledgerBefore ledgerBefore
    forcedMovingStep = stepWith 0 forcedEventKey 1 ledgerBefore ledgerAfter
    withdrawalPhaseStep = stepWith 0 forcedEventKey 0 ledgerBefore ledgerAfter
    forcedMap = PD.Constr 0 [PD.I 0, PD.Constr 1 []]
    forcedMapMembership =
      singleMembership
        eventToStepDomain
        4
        (serialise forcedEventKey)
        (serialise forcedMap)
        forcedEventKey
        forcedMap
    blockWithForced s0 =
      transitionBlock
        { bTraceRoot = commitCountedRoot traceDomain (traceRawRootOf s0 step1) 2
        , bEventToStepRoot =
            singleRoot eventToStepDomain 4 (serialise forcedEventKey) (serialise forcedMap)
        }
    noOp block s0 source =
      pvalidateInvalidForcedTransactionNoOpTransition
        (headerT block)
        (fromData (traceProofOf s0 step1 0))
        (fromData forcedMapMembership)
        (fromData (forcedMembership source))

--------------------------------------------------------------------------------

{- | @transition_trace.RootCountProof@ — a root's own domain, root, phas root and
count. A counted root does not reveal its count, so a count fault has to be
stated with one of these.
-}
countProof :: Integer -> BS.ByteString -> Integer -> PD.Data
countProof domain phasRoot count =
  PD.Constr
    0
    [ PD.Constr domain []
    , PD.B (commitCountedRoot domain phasRoot count)
    , PD.B phasRoot
    , PD.I count
    ]

countFaultTests :: [TestTree]
countFaultTests =
  [ testCase "a block whose event total is its four sources' sum is no fault" $
      prefuses (fault coherentBlock (PD.Constr 0 []))
  , testCase "a block whose event total is not that sum is" $
      passertEval (fault coherentBlock {bTotalEventCount = 9} (PD.Constr 0 []))
  , testCase "a block with one step per event is no fault" $
      prefuses (fault coherentBlock (PD.Constr 1 []))
  , testCase "a block with fewer steps than events is" $
      passertEval (fault coherentBlock {bStepCount = 3} (PD.Constr 1 []))
  , testCase "a withdrawal root committing the published count is no fault" $
      prefuses $
        fault coherentBlock (PD.Constr 2 [countProof withdrawalsDomain withdrawalPhas 1])
  , testCase "a withdrawal root committing another count is" $
      passertEval $
        fault
          coherentBlock {bWithdrawalsRoot = commitCountedRoot withdrawalsDomain withdrawalPhas 7}
          (PD.Constr 2 [countProof withdrawalsDomain withdrawalPhas 7])
  , testCase "refuses a source count fault stated under a non-source domain" $
      prefuses $
        fault coherentBlock (PD.Constr 2 [countProof traceDomain withdrawalPhas 7])
  , testCase "refuses a count proof against a root the header does not publish" $
      prefuses $
        fault coherentBlock (PD.Constr 2 [countProof withdrawalsDomain (ledgerRoot 0xee) 7])
  , testCase "an event-to-step root committing the event total is no fault" $
      prefuses $
        fault coherentBlock (PD.Constr 3 [countProof eventToStepDomain mapPhas 4])
  , testCase "an event-to-step root committing another count is" $
      passertEval $
        fault
          coherentBlock {bEventToStepRoot = commitCountedRoot eventToStepDomain mapPhas 6}
          (PD.Constr 3 [countProof eventToStepDomain mapPhas 6])
  , testCase "a trace root committing the step count is no fault" $
      prefuses $
        fault coherentBlock (PD.Constr 4 [countProof traceDomain tracePhas 4])
  , testCase "a trace root committing another count is" $
      passertEval $
        fault
          coherentBlock {bTraceRoot = commitCountedRoot traceDomain tracePhas 8}
          (PD.Constr 4 [countProof traceDomain tracePhas 8])
  , testCase "aborts on a sixth kind of count fault" $
      pfails (fault coherentBlock (PD.Constr 5 []))
  ]
  where
    fault block witness = pvalidateCountFault (headerT block) (asDataTerm witness)
    withdrawalPhas = singleEntryRoot (serialise withdrawalId) (serialise validWithdrawal)
    mapPhas = singleEntryRoot (serialise withdrawalKey) (serialise eventToStep0)
    tracePhas = traceRawRootOf honestStep step1
    -- One event of each class, one step apiece, and every root committing the
    -- count published beside it.
    coherentBlock =
      transitionBlock
        { bStepCount = 4
        , bWithdrawalsRoot = commitCountedRoot withdrawalsDomain withdrawalPhas 1
        , bEventToStepRoot = commitCountedRoot eventToStepDomain mapPhas 4
        , bTraceRoot = commitCountedRoot traceDomain tracePhas 4
        }

--------------------------------------------------------------------------------

duplicateTests :: [TestTree]
duplicateTests =
  [ testCase "a trace whose two steps are different events is no fault" $
      prefuses $
        duplicate
          distinctBlock
          (traceProofOf step0 distinctStep 0)
          (traceProofOf step0 distinctStep 1)
  , {- The default fixture's two steps both apply the same withdrawal, which makes
       it a guilty block for this fault and an innocent one for every other fault
       in this module. Both readings are correct: a duplicate is a fault about the
       trace as a whole, not about either step. -}
    testCase "a trace applying one event at two indices is" $
      passertEval $
        duplicate defaultBlock (traceProofOf step0 step1 0) (traceProofOf step0 step1 1)
  , testCase "…whichever way round the two openings are given" $
      passertEval $
        duplicate defaultBlock (traceProofOf step0 step1 1) (traceProofOf step0 step1 0)
  , testCase "refuses the fault stated twice about the same step" $
      prefuses $
        duplicate defaultBlock (traceProofOf step0 step1 0) (traceProofOf step0 step1 0)
  , testCase "refuses two steps out of two different trees" $
      prefuses $
        duplicate
          defaultBlock
          (traceProofOf step0 step1 0)
          (traceProofOf step0 distinctStep 1)
  ]
  where
    duplicate block left right =
      pvalidateDuplicateTraceEvent (headerT block) (fromData left) (fromData right)
    distinctStep = stepWith 1 (PD.Constr 0 [outRef 0x9a]) 0 midRoot (ledgerRoot 0xa2)
    distinctBlock = withTrace step0 distinctStep

--------------------------------------------------------------------------------
-- The L2 transaction transition
--------------------------------------------------------------------------------

{- | The transaction the L2 fault is about.

'tx1' out of the shared fixture, re-compacted with §2.5 validity code 0: this
rule only fires on a transaction the block declared /valid/, and every fixture
transaction carries code 3 by default. The id is unaffected — §3's preimage is
the body alone.
-}
l2TxId, l2Compact, l2WitnessSet, l2Lengths :: BS.ByteString
l2TxId = txIdOf tx1
l2Compact = compactWithValidity tx1 (blake2b256 (witnessSetCborOf tx1)) 0
l2WitnessSet = witnessSetCborOf tx1
l2Lengths = BS.concat ("\x89" : replicate 9 "\x00")

-- | @ledger_state.NativeTxProofSourceV1@, and the leaf the block commits.
l2Triple :: PD.Data
l2Triple = PD.Constr 0 [PD.B l2Compact, PD.B l2WitnessSet, PD.B l2Lengths]

{- | The transactions tree stores the /bytes/ of the source value, so the leaf is
the canonical serialisation of @L2TransactionSourceV1 { tx_id, source }@ — which
is exactly what the port rebuilds from the supplied triple instead of decoding.
-}
l2Leaf :: BS.ByteString
l2Leaf = serialise (PD.Constr 0 [PD.B l2TxId, l2Triple])

l2EventKey :: PD.Data
l2EventKey = l2EventKeyFor l2TxId

l2EventKeyFor :: BS.ByteString -> PD.Data
l2EventKeyFor txId = PD.Constr 2 [PD.B txId]

{- | The ledger, before and after.

'tx1' spends one input and produces two outputs, so the step deletes one key and
inserts two: the trie goes one entry → empty → one entry → two entries, and the
expected post-root is the two-leaf trie over the outputs.
-}
spentKey, spentValue :: BS.ByteString
spentKey = encodedInput sharedInputRef
spentValue = BS.replicate 40 0x8a

outputEntry :: Integer -> (BS.ByteString, BS.ByteString)
outputEntry i = (ledgerKey (outRefAt l2TxId i), outputItem (fromIntegral i))

outputEntriesFor :: BS.ByteString -> [BS.ByteString] -> [(BS.ByteString, BS.ByteString)]
outputEntriesFor txId =
  zipWith (\index value -> (ledgerKey (outRefAt txId index), value)) [0 ..]

-- | An output reference under a chosen transaction id.
outRefAt :: BS.ByteString -> Integer -> PD.Data
outRefAt txId index = PD.Constr 0 [PD.Constr 0 [PD.B txId], PD.I index]

l2PreRoot, l2PostRoot :: BS.ByteString
l2PreRoot = singleEntryRoot spentKey spentValue
l2PostRoot = twoLeafRoot (outputEntry 0) (outputEntry 1)

{- | The three ledger witnesses: one deletion out of a one-entry trie, one
insertion into an empty one, and one insertion beside an existing leaf.

The last needs a real MPF path in /both/ of its proofs — the exclusion path has
to reproduce the one-entry root and the inclusion path has to produce the
two-leaf one — which is what a second entry costs and why the fixture carries
two outputs rather than one.
-}
l2SpendWitnesses, l2OutputWitnesses :: [PD.Data]
l2SpendWitnesses = [deleteWitness spentKey spentValue]
l2OutputWitnesses = outputWitnessesFor l2TxId [outputItem 0, outputItem 1]

outputWitnessesFor :: BS.ByteString -> [BS.ByteString] -> [PD.Data]
outputWitnessesFor txId values =
  case outputEntriesFor txId values of
    [firstEntry@(firstKey, firstValue), secondEntry@(secondKey, secondValue)] ->
      [ insertWitness firstKey firstValue
      , PD.Constr
          0
          [ PD.B secondKey
          , PD.B secondValue
          , twoLeafProof secondEntry firstEntry
          , twoLeafProof secondEntry firstEntry
          ]
      ]
    _ -> error "outputWitnessesFor: expected exactly two outputs"

l2Step :: BS.ByteString -> PD.Data
l2Step post = stepWith 0 l2EventKey 2 l2PreRoot post

l2Map :: PD.Data
l2Map = PD.Constr 0 [PD.I 0, PD.Constr 2 []]

l2MapMembership :: PD.Data
l2MapMembership = l2MapMembershipFor l2TxId

l2MapMembershipFor :: BS.ByteString -> PD.Data
l2MapMembershipFor txId =
  singleMembership
    eventToStepDomain
    4
    (serialise eventKey)
    (serialise l2Map)
    eventKey
    l2Map
  where
    eventKey = l2EventKeyFor txId

{- | The transactions tree keys by the raw id and stores raw bytes, so neither
side is serialised on the way in.
-}
l2SourceMembership :: BS.ByteString -> PD.Data
l2SourceMembership = l2SourceMembershipFor l2TxId

l2SourceMembershipFor :: BS.ByteString -> BS.ByteString -> PD.Data
l2SourceMembershipFor txId leaf =
  PD.Constr
    0
    [ PD.Constr transactionsDomain []
    , PD.B (commitCountedRoot transactionsDomain raw 1)
    , PD.B raw
    , PD.I 1
    , PD.B txId
    , PD.B leaf
    , PD.List []
    ]
  where
    raw = singleEntryRoot txId leaf

l2FaultBlock :: PD.Data -> BS.ByteString -> Block
l2FaultBlock = l2FaultBlockFor l2TxId

l2FaultBlockFor :: BS.ByteString -> PD.Data -> BS.ByteString -> Block
l2FaultBlockFor txId s0 leaf =
  defaultBlock
    { bTotalEventCount = 4
    , bL2Count = 1
    , bTransactionsRoot =
        commitCountedRoot transactionsDomain (singleEntryRoot txId leaf) 1
    , bEventToStepRoot =
        singleRoot eventToStepDomain 4 (serialise eventKey) (serialise l2Map)
    , bTraceRoot = commitCountedRoot traceDomain (traceRawRootOf s0 step1) 2
    }
  where
    eventKey = l2EventKeyFor txId

l2Tests :: [TestTree]
l2Tests =
  [ testGroup
      "transition-trace/proof Aiken L2 preimage parity"
      [ testCase "rejects_l2_transaction_with_a_substituted_spend_inputs_preimage" $
          pfails $
            acceptedL2Fault
              substitutedSpendStep
              substitutedSpendInputsPreimage
              (outputsPreimage tx1)
              [deleteWitness substitutedSpentKey spentValue]
              l2OutputWitnesses
      , testCase "rejects_l2_transaction_with_a_substituted_outputs_preimage" $
          pfails $
            acceptedL2Fault
              substitutedOutputStep
              (spendInputsPreimage tx1)
              substitutedOutputsPreimage
              l2SpendWitnesses
              substitutedOutputWitnesses
      , testCase "rejects_l2_transaction_with_wrong_authenticated_transaction_id" $
          pfails $
            acceptedL2FaultWithSource
              wrongAuthenticatedTxId
              l2Triple
              wrongAuthenticatedLeaf
              wrongAuthenticatedStep
              (spendInputsPreimage tx1)
              (outputsPreimage tx1)
              l2SpendWitnesses
              wrongAuthenticatedOutputWitnesses
      , testCase "rejects_l2_transaction_with_uncommitted_spend_preimage" $
          pfails $
            acceptedL2Fault
              substitutedOutputStep
              substitutedSpendInputsPreimage
              (outputsPreimage tx1)
              l2SpendWitnesses
              l2OutputWitnesses
      , testCase "rejects_l2_transaction_with_a_miscounted_committed_spend_inputs_preimage" $
          pfails $
            acceptedL2FaultWithSource
              miscountedTxId
              miscountedTriple
              miscountedLeaf
              miscountedStep
              miscountedSpendInputsPreimage
              (outputsPreimage tx1)
              l2SpendWitnesses
              miscountedOutputWitnesses
      ]
  , testCase "the door walks field 0 into the transaction's spend inputs" $
      passertEval $
        pdoorBodyFieldItems anchoredT 0 (pconstant (spendInputsPreimage tx1))
          #== pconstant [encodedInput sharedInputRef]
  , testCase "…and field 2 into its outputs" $
      passertEval $
        pdoorBodyFieldItems anchoredT 2 (pconstant (outputsPreimage tx1))
          #== pconstant [outputItem 0, outputItem 1]
  , testCase "aborts on a preimage the transaction does not commit to" $
      pfails $
        pdoorBodyFieldItems
          anchoredT
          0
          (pconstant (spendInputsPreimage tx1 <> "\x00"))
  , testCase "aborts opening field 0 with field 2's preimage" $
      pfails (pdoorBodyFieldItems anchoredT 0 (pconstant (outputsPreimage tx1)))
  , testCase "spending the declared input empties the ledger" $
      passertEval $
        papplyL2Spends
          # pconstant l2PreRoot
          # pconstant [spentKey]
          # witnessList l2SpendWitnesses
          #== pconstant libraryNullHash
  , testCase "aborts when a spend witness names another UTxO" $
      pfails $
        papplyL2Spends
          # pconstant l2PreRoot
          # pconstant [spentKey]
          # witnessList [deleteWitness (ledgerKey (outRef 0x8b)) spentValue]
  , testCase "aborts when the witness list is shorter than the input list" $
      pfails $
        papplyL2Spends # pconstant l2PreRoot # pconstant [spentKey] # witnessList []
  , testCase "aborts when the witness list is longer" $
      pfails $
        papplyL2Spends
          # pconstant l2PreRoot
          # pconstant ([] :: [BS.ByteString])
          # witnessList l2SpendWitnesses
  , testCase "producing both outputs builds the two-leaf ledger" $
      passertEval $
        papplyL2Outputs
          # pconstant libraryNullHash
          # pconstant l2TxId
          # 0
          # pconstant [outputItem 0, outputItem 1]
          # witnessList l2OutputWitnesses
          #== pconstant l2PostRoot
  , testCase "aborts when an output witness claims another index" $
      pfails $
        papplyL2Outputs
          # pconstant libraryNullHash
          # pconstant l2TxId
          # 1
          # pconstant [outputItem 0]
          # witnessList [head l2OutputWitnesses]
  , testCase "aborts when an output witness names a value that is not the output" $
      pfails $
        papplyL2Outputs
          # pconstant libraryNullHash
          # pconstant l2TxId
          # 0
          # pconstant [outputItem 0]
          # witnessList [insertWitness (fst (outputEntry 0)) (BS.replicate 20 0x8c)]
  , testCase "a step that applied the transaction is no fault" $
      prefuses (transition (l2Step l2PostRoot) l2Leaf)
  , testCase "a step that published another root is" $
      passertEval (transition (l2Step (ledgerRoot 0xcd)) l2Leaf)
  , testCase "…including one that left the ledger untouched" $
      passertEval (transition (l2Step l2PreRoot) l2Leaf)
  , testCase "refuses a leaf the supplied triple does not rebuild" $
      prefuses $
        transition (l2Step l2PostRoot) (serialise (PD.Constr 0 [PD.B l2TxId, PD.B "other"]))
  , testCase "refuses a leaf filed under another transaction id" $
      prefuses $
        transition
          (l2Step l2PostRoot)
          (serialise (PD.Constr 0 [PD.B (BS.replicate 32 0x8d), l2Triple]))
  , testCase "aborts on a transaction the block did not declare valid" $
      pfails $
        pvalidateL2TransactionTransition
          (headerT (l2FaultBlock (l2Step l2PostRoot) invalidLeaf))
          (fromData (traceProofOf (l2Step l2PostRoot) step1 0))
          (fromData l2MapMembership)
          (fromData (l2SourceMembership invalidLeaf))
          (asDataTerm invalidTriple)
          (pconstant (spendInputsPreimage tx1))
          (pconstant (outputsPreimage tx1))
          (witnessList l2SpendWitnesses)
          (witnessList l2OutputWitnesses)
  , testCase "the source is in the block's transactions tree" $
      passertEval $
        pverifyL2SourceMembership
          (headerT (l2FaultBlock (l2Step l2PostRoot) l2Leaf))
          (fromData (l2SourceMembership l2Leaf))
  , testCase "…and the step, the map and that tree agree" $
      passertEval $
        pvalidateL2OneStepBinding
          (headerT (l2FaultBlock (l2Step l2PostRoot) l2Leaf))
          (fromData (traceProofOf (l2Step l2PostRoot) step1 0))
          (fromData l2MapMembership)
          (fromData (l2SourceMembership l2Leaf))
  , testCase "aborts binding a step whose event key is not an L2 transaction's" $
      pfails $
        pvalidateL2OneStepBinding
          (headerT (l2FaultBlock wrongClassStep l2Leaf))
          (fromData (traceProofOf wrongClassStep step1 0))
          (fromData l2MapMembership)
          (fromData (l2SourceMembership l2Leaf))
  ]
  where
    anchoredT :: forall s. Term s PAnchoredNativeTxV1
    anchoredT =
      panchoredNativeTx
        # pcon (PBodyTxOpening (pconstant l2Compact))
        # pcon (PBodyAnchor (pdata (pconstant l2TxId)))
    transition s0 leaf =
      pvalidateL2TransactionTransition
        (headerT (l2FaultBlock s0 leaf))
        (fromData (traceProofOf s0 step1 0))
        (fromData l2MapMembership)
        (fromData (l2SourceMembership leaf))
        (asDataTerm l2Triple)
        (pconstant (spendInputsPreimage tx1))
        (pconstant (outputsPreimage tx1))
        (witnessList l2SpendWitnesses)
        (witnessList l2OutputWitnesses)
    invalidTriple = PD.Constr 0 [PD.B invalidCompact, PD.B l2WitnessSet, PD.B l2Lengths]
    invalidCompact = compactWithValidity tx1 (blake2b256 l2WitnessSet) 4
    invalidLeaf = serialise (PD.Constr 0 [PD.B l2TxId, invalidTriple])
    wrongClassStep = stepWith 0 (PD.Constr 0 [outRef 0x8e]) 2 l2PreRoot l2PostRoot

{- | The two Aiken mutation selectors keep every ledger witness consistent with
the preimages supplied by the prover. The source leaf still commits the
canonical transaction, so only the field door's authenticate-once hash can
reject either fault.
-}
acceptedL2Fault ::
  forall s.
  PD.Data ->
  BS.ByteString ->
  BS.ByteString ->
  [PD.Data] ->
  [PD.Data] ->
  Term s PBool
acceptedL2Fault openedStep suppliedSpendInputs suppliedOutputs spendWitnesses outputWitnesses =
  acceptedL2FaultWithSource
    l2TxId
    l2Triple
    l2Leaf
    openedStep
    suppliedSpendInputs
    suppliedOutputs
    spendWitnesses
    outputWitnesses

acceptedL2FaultWithSource ::
  forall s.
  BS.ByteString ->
  PD.Data ->
  BS.ByteString ->
  PD.Data ->
  BS.ByteString ->
  BS.ByteString ->
  [PD.Data] ->
  [PD.Data] ->
  Term s PBool
acceptedL2FaultWithSource txId sourceTriple sourceLeaf openedStep suppliedSpendInputs suppliedOutputs spendWitnesses outputWitnesses =
  pvalidateAcceptedTransactionFaultProof
    ( fromData $
        faultProof
          block
          ( oneStepFault $
              PD.Constr
                4
                [ traceProofOf openedStep step1 0
                , l2MapMembershipFor txId
                , l2SourceMembershipFor txId sourceLeaf
                , PD.B suppliedSpendInputs
                , PD.B suppliedOutputs
                , PD.List spendWitnesses
                , PD.List outputWitnesses
                , sourceTriple
                ]
          )
    )
    (asDataTerm (PD.B (threadNameFor block)))
  where
    block = l2FaultBlockFor txId openedStep sourceLeaf

substitutedSpendInputsPreimage :: BS.ByteString
substitutedSpendInputsPreimage =
  arrayHeader 1 <> wrapItem (encodedInput substitutedSpendInputRef)

substitutedSpendInputRef :: (BS.ByteString, Integer)
substitutedSpendInputRef = (BS.replicate 32 0x8f, 0)

substitutedSpentKey :: BS.ByteString
substitutedSpentKey = encodedInput substitutedSpendInputRef

substitutedSpendStep :: PD.Data
substitutedSpendStep =
  stepWith
    0
    l2EventKey
    2
    (singleEntryRoot substitutedSpentKey spentValue)
    (ledgerRoot 0xcd)

substitutedOutputsPreimage :: BS.ByteString
substitutedOutputsPreimage =
  arrayHeader 2 <> wrapItem dpOutputCbor <> wrapItem (outputItem 1)

substitutedOutputEntries :: [(BS.ByteString, BS.ByteString)]
substitutedOutputEntries =
  [ (fst (outputEntry 0), dpOutputCbor)
  , outputEntry 1
  ]

substitutedOutputWitnesses :: [PD.Data]
substitutedOutputWitnesses =
  [ insertWitness firstKey firstValue
  , PD.Constr
      0
      [ PD.B secondKey
      , PD.B secondValue
      , twoLeafProof secondEntry firstEntry
      , twoLeafProof secondEntry firstEntry
      ]
  ]
  where
    firstEntry@(firstKey, firstValue) = head substitutedOutputEntries
    secondEntry@(secondKey, secondValue) = substitutedOutputEntries !! 1

substitutedOutputStep :: PD.Data
substitutedOutputStep =
  stepWith 0 l2EventKey 2 l2PreRoot (ledgerRoot 0xcd)

wrongAuthenticatedTxId :: BS.ByteString
wrongAuthenticatedTxId = BS.replicate 32 0xcc

wrongAuthenticatedLeaf :: BS.ByteString
wrongAuthenticatedLeaf =
  serialise (PD.Constr 0 [PD.B wrongAuthenticatedTxId, l2Triple])

wrongAuthenticatedStep :: PD.Data
wrongAuthenticatedStep =
  stepWith
    0
    (l2EventKeyFor wrongAuthenticatedTxId)
    2
    l2PreRoot
    (ledgerRoot 0xcd)

wrongAuthenticatedOutputWitnesses :: [PD.Data]
wrongAuthenticatedOutputWitnesses =
  outputWitnessesFor wrongAuthenticatedTxId [outputItem 0, outputItem 1]

miscountedSpendInputsPreimage :: BS.ByteString
miscountedSpendInputsPreimage =
  arrayHeader 2 <> BS.drop 1 (spendInputsPreimage tx1)

{- | The compact body starts at byte 2 (@84 01@) and its first field is
@58 20 spend_inputs_hash@, so the hash payload occupies bytes 5 through 36.
Replacing only that payload retains the canonical compact framing while making
the source commit the deliberately malformed field-0 bytes.
-}
compactCommittingSpendPreimage :: BS.ByteString -> BS.ByteString
compactCommittingSpendPreimage preimage =
  BS.take 5 l2Compact <> blake2b256 preimage <> BS.drop 37 l2Compact

compactBodyCbor :: BS.ByteString -> BS.ByteString
compactBodyCbor compact =
  BS.take (BS.length compact - 37) (BS.drop 2 compact)

txIdForCompact :: BS.ByteString -> BS.ByteString
txIdForCompact compact =
  blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> compactBodyCbor compact)

miscountedCompact :: BS.ByteString
miscountedCompact = compactCommittingSpendPreimage miscountedSpendInputsPreimage

miscountedTxId :: BS.ByteString
miscountedTxId = txIdForCompact miscountedCompact

miscountedTriple :: PD.Data
miscountedTriple =
  PD.Constr 0 [PD.B miscountedCompact, PD.B l2WitnessSet, PD.B l2Lengths]

miscountedLeaf :: BS.ByteString
miscountedLeaf =
  serialise (PD.Constr 0 [PD.B miscountedTxId, miscountedTriple])

miscountedStep :: PD.Data
miscountedStep =
  stepWith 0 (l2EventKeyFor miscountedTxId) 2 l2PreRoot (ledgerRoot 0xcd)

miscountedOutputWitnesses :: [PD.Data]
miscountedOutputWitnesses =
  outputWitnessesFor miscountedTxId [outputItem 0, outputItem 1]

-- | A witness list as the builtin list of data-encoded values a walker expects.
witnessList :: forall (a :: S -> Type) s. [PD.Data] -> Term s (PBuiltinList (PAsData a))
witnessList ws = punsafeCoerce (pconstant @(PBuiltinList PData) ws)

--------------------------------------------------------------------------------
-- The deposit projection
--------------------------------------------------------------------------------

{- | The pieces of an L1 deposit UTxO.

The asset names and hashes are arbitrary; what is not arbitrary is that the
deposit policy holds its NFT /alongside/ other assets, because that is the case
where projecting the value has something to do.
-}
dpPolicy, dpAssetName, dpTokenPolicy, dpPkh, dpStakeHash :: BS.ByteString
dpPolicy = BS.replicate 28 0xde
dpAssetName = "deposit-nft"
dpTokenPolicy = BS.replicate 28 0x7a
dpPkh = BS.replicate 28 0xb1
dpStakeHash = BS.replicate 28 0xb2

pubKeyCredD, scriptCredD :: BS.ByteString -> PD.Data
pubKeyCredD h = PD.Constr 0 [PD.B h]
scriptCredD h = PD.Constr 1 [PD.B h]

justD :: PD.Data -> PD.Data
justD x = PD.Constr 0 [x]

nothingD :: PD.Data
nothingD = PD.Constr 1 []

addressD :: PD.Data -> PD.Data -> PD.Data
addressD credential stake = PD.Constr 0 [credential, stake]

stakingHashD :: PD.Data -> PD.Data
stakingHashD c = PD.Constr 0 [c]

{- | A Cardano value, in the order its maps carry it.

Written out rather than built from a library so the entry order is visible: the
projection walks the map as it finds it, and the encoder groups by runs of equal
policy, so order is part of what these tests pin.
-}
valueD :: [(BS.ByteString, [(BS.ByteString, Integer)])] -> PD.Data
valueD policies =
  PD.Map [(PD.B p, PD.Map [(PD.B n, PD.I q) | (n, q) <- ts]) | (p, ts) <- policies]

dpL2Address :: PD.Data
dpL2Address =
  addressD (pubKeyCredD dpPkh) (justD (stakingHashD (pubKeyCredD dpStakeHash)))

dpL1Address :: PD.Data
dpL1Address = addressD (scriptCredD (BS.replicate 28 0xc1)) nothingD

-- | @ledger_state.DepositInfo@ — L2 address, L2 network id, optional L2 datum.
dpInfoWith :: PD.Data -> Integer -> PD.Data -> PD.Data
dpInfoWith address networkId datum = PD.Constr 0 [address, PD.I networkId, datum]

dpInfo :: PD.Data
dpInfo = dpInfoWith dpL2Address 1 (justD (PD.I 42))

dpValue :: PD.Data
dpValue =
  valueD
    [ ("", [("", 5_000_000)])
    , (dpTokenPolicy, [("TOK", 7)])
    , (dpPolicy, [(dpAssetName, 1)])
    ]

--------------------------------------------------------------------------------
-- The reference encoding of the projected output
--------------------------------------------------------------------------------

{- | The Midgard address payload: one header byte and the two hashes.

Address type 0 — both credentials are key credentials — network 1, and the
protected bit clear, so the header is @0 * 16 + 1 + 0@.
-}
dpAddressBytes :: BS.ByteString
dpAddressBytes = BS.pack [0x01] <> dpPkh <> dpStakeHash

-- | @[lovelace, {policy: {name: quantity}}]@, with Ada and the NFT both gone.
dpValueBytes :: BS.ByteString
dpValueBytes =
  BS.concat
    [ "\x82"
    , cborInt 5_000_000
    , "\xa1"
    , wrapItem dpTokenPolicy
    , "\xa1"
    , wrapItem "TOK"
    , cborInt 7
    ]

-- | The output map: address, value and datum, with no script reference.
dpOutputCbor :: BS.ByteString
dpOutputCbor =
  BS.concat
    [ "\xa3"
    , "\x00"
    , wrapItem dpAddressBytes
    , "\x01"
    , dpValueBytes
    , "\x02"
    , wrapItem (serialise (PD.I 42))
    ]

--------------------------------------------------------------------------------
-- The deposit's place in the block
--------------------------------------------------------------------------------

dpId :: PD.Data
dpId = outRef 0x93

dpLedgerKey :: BS.ByteString
dpLedgerKey = ledgerKey dpId

{- | The ledger before the deposit lands, and after.

Empty before, so the insertion's exclusion proof is the empty path and the
resulting trie is the single projected UTxO — the smallest shape in which the
post-root is determined by the projection alone.
-}
dpPreRoot, dpPostRoot :: BS.ByteString
dpPreRoot = emptyMerkleRoot
dpPostRoot = singleEntryRoot dpLedgerKey dpOutputCbor

dpEventKey :: PD.Data
dpEventKey = PD.Constr 3 [dpId]

dpStep :: BS.ByteString -> PD.Data
dpStep post = stepWith 0 dpEventKey 3 dpPreRoot post

dpMapValue :: PD.Data
dpMapValue = PD.Constr 0 [PD.I 0, PD.Constr 3 []]

dpMapMembership :: PD.Data
dpMapMembership =
  singleMembership
    eventToStepDomain
    4
    (serialise dpEventKey)
    (serialise dpMapValue)
    dpEventKey
    dpMapValue

dpMembership :: PD.Data -> PD.Data
dpMembership info =
  singleMembership depositsDomain 1 (serialise dpId) (serialise info) dpId info

-- | The block, with its deposit tree and event-to-step map built around a leaf.
dpBlock :: PD.Data -> PD.Data -> Block
dpBlock s0 info =
  transitionBlock
    { bDepositsRoot = singleRoot depositsDomain 1 (serialise dpId) (serialise info)
    , bEventToStepRoot =
        singleRoot eventToStepDomain 4 (serialise dpEventKey) (serialise dpMapValue)
    , bTraceRoot = commitCountedRoot traceDomain (traceRawRootOf s0 step1) 2
    }

{- | The L1 UTxO the deposit NFT authenticates.

Its datum is an optimistic event datum: the event first, its inclusion time
second, and a third field nothing here reads — which is the point of reading the
first two positionally.
-}
dpRefInput :: PD.Data -> PD.Data -> PD.Data
dpRefInput value datum = PD.Constr 0 [outRef 0x94, dpRefOutput value datum]

-- | The resolved half of that reference input: the deposit UTxO itself.
dpRefOutput :: PD.Data -> PD.Data -> PD.Data
dpRefOutput value datum = PD.Constr 0 [dpL1Address, value, PD.Constr 2 [datum], nothingD]

dpDatum :: PD.Data -> PD.Data -> PD.Data
dpDatum = dpDatumAt 1234

dpDatumAt :: Integer -> PD.Data -> PD.Data -> PD.Data
dpDatumAt inclusionTime idD info =
  PD.Constr 0 [PD.Constr 0 [idD, info], PD.I inclusionTime, PD.B "unread"]

dpHonestRefInput :: PD.Data
dpHonestRefInput = dpRefInput dpValue (dpDatum dpId dpInfo)

{- | A hub oracle datum whose deposit policy is the one under test.

Twelve policy ids, thirteen addresses and the reserve observer's script hash —
the shape matters because the field is read positionally.
-}
dpHubDatum :: PD.Data
dpHubDatum = PD.Constr 0 (policies <> addresses <> [PD.B (BS.replicate 28 0xee)])
  where
    policies =
      [ PD.B (if i == 7 then dpPolicy else BS.replicate 28 (fromIntegral i))
      | i <- [0 .. 11 :: Integer]
      ]
    addresses = replicate 13 dpL1Address

--------------------------------------------------------------------------------

depositTests :: [TestTree]
depositTests =
  [ testCase "a key credential projects to a Midgard key credential" $
      passertEval $
        pdata (pcardanoCredentialToMidgard (fromData (pubKeyCredD dpPkh)))
          #== asDataTerm (PD.Constr 0 [PD.B dpPkh])
  , testCase "a script credential projects to a Midgard script credential" $
      passertEval $
        pdata (pcardanoCredentialToMidgard (fromData (scriptCredD dpPkh)))
          #== asDataTerm (PD.Constr 1 [PD.B dpPkh])
  , testCase "an inline stake credential projects to its credential" $
      passertEval $
        pdata
          ( pcardanoStakeCredentialToMidgard
              (fromData (stakingHashD (pubKeyCredD dpStakeHash)))
          )
          #== asDataTerm (PD.Constr 0 [PD.B dpStakeHash])
  , testCase "a pointer stake credential aborts" $
      pfails $
        pdata
          ( pcardanoStakeCredentialToMidgard
              (fromData (PD.Constr 1 [PD.I 1, PD.I 2, PD.I 3]))
          )
  , testCase "an address projects unprotected, keeping both credentials" $
      passertEval $
        pdata (pdepositAddressToMidgard (fromData dpL2Address) 1)
          #== asDataTerm
            ( PD.Constr
                0
                [ PD.Constr 0 []
                , PD.I 1
                , pubKeyCredD dpPkh
                , justD (pubKeyCredD dpStakeHash)
                ]
            )
  , testCase "…and an address without a stake credential keeps none" $
      passertEval $
        pdata
          ( pdepositAddressToMidgard
              (fromData (addressD (pubKeyCredD dpPkh) nothingD))
              0
          )
          #== asDataTerm
            (PD.Constr 0 [PD.Constr 0 [], PD.I 0, pubKeyCredD dpPkh, nothingD])
  , testCase "aborts on a network id that is neither mainnet nor testnet" $
      pfails (pdata (pdepositAddressToMidgard (fromData dpL2Address) 2))
  , testCase "the asset list drops Ada and keys by policy ‖ name" $
      passertEval $
        pdata (pcardanoAssetPairsToMidgard # fromData dpValue)
          #== asDataTerm
            ( PD.Map
                [ (PD.B (dpTokenPolicy <> "TOK"), PD.I 7)
                , (PD.B (dpPolicy <> dpAssetName), PD.I 1)
                ]
            )
  , testCase "…and a value of nothing but Ada projects to no assets" $
      passertEval $
        pdata (pcardanoAssetPairsToMidgard # fromData (valueD [("", [("", 1)])]))
          #== asDataTerm (PD.Map [])
  , testCase "aborts on a zero quantity" $
      pfails $
        pdata (pcardanoAssetPairsToMidgard # fromData (valueD [(dpTokenPolicy, [("TOK", 0)])]))
  , testCase "aborts on a negative quantity" $
      pfails $
        pdata (pcardanoAssetPairsToMidgard # fromData (valueD [(dpTokenPolicy, [("TOK", -1)])]))
  , testCase "the projected value is the L1 value less the deposit NFT" $
      passertEval $
        pdata (pprojectedDepositValue (fromData dpValue) dpPolicyT dpAssetNameT)
          #== asDataTerm
            (PD.Constr 0 [PD.I 5_000_000, PD.Map [(PD.B (dpTokenPolicy <> "TOK"), PD.I 7)]])
  , testCase "…and a policy the NFT emptied disappears entirely" $
      passertEval $
        pdata
          ( pprojectedDepositValue
              (fromData (valueD [("", [("", 2)]), (dpPolicy, [(dpAssetName, 1)])]))
              dpPolicyT
              dpAssetNameT
          )
          #== asDataTerm (PD.Constr 0 [PD.I 2, PD.Map []])
  , testCase "…while another name under the same policy survives it" $
      passertEval $
        pdata
          ( pprojectedDepositValue
              ( fromData
                  ( valueD
                      [ ("", [("", 2)])
                      , (dpPolicy, [("OTHER", 3), (dpAssetName, 1)])
                      ]
                  )
              )
              dpPolicyT
              dpAssetNameT
          )
          #== asDataTerm
            (PD.Constr 0 [PD.I 2, PD.Map [(PD.B (dpPolicy <> "OTHER"), PD.I 3)]])
  , testCase "aborts when the deposit NFT is not there" $
      pfails $
        pdata
          ( pprojectedDepositValue
              (fromData (valueD [("", [("", 2)])]))
              dpPolicyT
              dpAssetNameT
          )
  , testCase "aborts when the value holds two of it" $
      pfails $
        pdata
          ( pprojectedDepositValue
              (fromData (valueD [("", [("", 2)]), (dpPolicy, [(dpAssetName, 2)])]))
              dpPolicyT
              dpAssetNameT
          )
  , testCase "a datum projects to its serialisation" $
      passertEval $
        pdata (pdepositDatumCbor (pconstant (justD (PD.I 42))))
          #== asDataTerm (justD (PD.B (serialise (PD.I 42))))
  , testCase "…and no datum projects to none" $
      passertEval $
        pdata (pdepositDatumCbor (pconstant nothingD)) #== asDataTerm nothingD
  , testCase "aborts on an optional datum that is neither" $
      pfails (pdata (pdepositDatumCbor (pconstant (PD.Constr 2 [PD.I 42]))))
  , testCase "the projected output is the encoding of all three" $
      passertEval $
        pprojectedDepositOutputCbor
          (fromData dpInfo)
          (fromData dpValue)
          dpPolicyT
          dpAssetNameT
          #== pconstant dpOutputCbor
  , testCase "…and a deposit with no datum encodes two entries, not three" $
      passertEval $
        pprojectedDepositOutputCbor
          (fromData (dpInfoWith dpL2Address 1 nothingD))
          (fromData dpValue)
          dpPolicyT
          dpAssetNameT
          #== pconstant
            (BS.concat ["\xa2", "\x00", wrapItem dpAddressBytes, "\x01", dpValueBytes])
  , testCase "the authenticated reference is the event the datum carries" $
      passertEval $
        pmatch (dpReference dpHonestRefInput) $ \r ->
          pauthDeposit'id r
            #== pconstant dpId
            #&& pauthDeposit'info r
            #== pconstant dpInfo
            #&& pauthDeposit'inclusionTime r
            #== 1234
  , testCase "aborts on a reference input that does not hold the NFT" $
      pfails $
        pmatch
          (dpReference (dpRefInput (valueD [("", [("", 2)])]) (dpDatum dpId dpInfo)))
          (\r -> pauthDeposit'inclusionTime r #== 1234)
  , testCase "aborts on a reference input holding two of it" $
      pfails $
        pmatch
          ( dpReference
              ( dpRefInput
                  (valueD [("", [("", 2)]), (dpPolicy, [(dpAssetName, 2)])])
                  (dpDatum dpId dpInfo)
              )
          )
          (\r -> pauthDeposit'inclusionTime r #== 1234)
  , testCase "aborts on a reference input whose datum is not inline" $
      pfails $
        pmatch
          ( dpReference
              (PD.Constr 0 [outRef 0x94, PD.Constr 0 [dpL1Address, dpValue, PD.Constr 0 [], nothingD]])
          )
          (\r -> pauthDeposit'inclusionTime r #== 1234)
  , testCase "aborts on a reference-input index the transaction does not have" $
      pfails $
        pmatch
          ( pgetAuthenticatedDepositReference
              # witnessList [dpHonestRefInput]
              # dpPolicyT
              # dpAssetNameT
              # 1
          )
          (\r -> pauthDeposit'inclusionTime r #== 1234)
  , testCase "a step that landed the projected UTxO is no fault" $
      prefuses (dpTransition (dpStep dpPostRoot) dpHonestRefInput dpWitness)
  , testCase "a step that published another root is" $
      passertEval (dpTransition (dpStep (ledgerRoot 0xce)) dpHonestRefInput dpWitness)
  , testCase "…including one that left the ledger untouched" $
      passertEval (dpTransition (dpStep dpPreRoot) dpHonestRefInput dpWitness)
  , testCase "refuses a reference input filed under another deposit id" $
      prefuses $
        dpTransition
          (dpStep dpPostRoot)
          (dpRefInput dpValue (dpDatum (outRef 0x95) dpInfo))
          dpWitness
  , testCase "refuses a reference input carrying another deposit's info" $
      prefuses $
        dpTransition
          (dpStep dpPostRoot)
          (dpRefInput dpValue (dpDatum dpId (dpInfoWith dpL2Address 0 nothingD)))
          dpWitness
  , testCase "refuses a witness that names another ledger key" $
      prefuses $
        dpTransition
          (dpStep dpPostRoot)
          dpHonestRefInput
          (insertWitness (ledgerKey (outRef 0x96)) dpOutputCbor)
  , testCase "refuses a witness that names a value other than the projection" $
      prefuses $
        dpTransition (dpStep dpPostRoot) dpHonestRefInput (insertWitness dpLedgerKey "junk")
  , testCase "a deposit landing something other than its projection is a fault" $
      passertEval $
        dpTransitionWith
          (dpStep (singleEntryRoot dpLedgerKey "junk"))
          dpInfo
          dpHonestRefInput
          (insertWitness dpLedgerKey dpOutputCbor)
  , testCase "aborts when the L1 value does not hold the deposit NFT" $
      pfails $
        dpTransition
          (dpStep dpPostRoot)
          (dpRefInput (valueD [("", [("", 2)])]) (dpDatum dpId dpInfo))
          dpWitness
  , testCase "the step, the map and the deposits tree agree" $
      passertEval $
        pvalidateDepositOneStepBinding
          (headerT (dpBlock (dpStep dpPostRoot) dpInfo))
          (fromData (traceProofOf (dpStep dpPostRoot) step1 0))
          (fromData dpMapMembership)
          (fromData (dpMembership dpInfo))
  ]
  where
    dpWitness = insertWitness dpLedgerKey dpOutputCbor
    dpReference :: forall s. PD.Data -> Term s PAuthenticatedDepositReference
    dpReference refInput =
      pgetAuthenticatedDepositReference
        # witnessList [refInput]
        # dpPolicyT
        # dpAssetNameT
        # 0
    dpTransition s0 refInput witness = dpTransitionWith s0 dpInfo refInput witness
    dpTransitionWith :: forall s. PD.Data -> PD.Data -> PD.Data -> PD.Data -> Term s PBool
    dpTransitionWith s0 info refInput witness =
      pvalidateValidDepositTransition
        (headerT (dpBlock s0 info))
        (fromData dpHubDatum)
        (witnessList [refInput])
        (fromData (traceProofOf s0 step1 0))
        (fromData dpMapMembership)
        (fromData (dpMembership info))
        0
        dpAssetNameT
        (fromData witness)

dpPolicyT :: forall s. Term s (PAsData PCurrencySymbol)
dpPolicyT = asDataTerm (PD.B dpPolicy)

dpAssetNameT :: forall s. Term s (PAsData PTokenName)
dpAssetNameT = asDataTerm (PD.B dpAssetName)

--------------------------------------------------------------------------------
-- The L1-event faults
--------------------------------------------------------------------------------

{- | The three event policies the hub oracle names.

The deposit one is 'dpPolicy'; the other two are the filler hashes
'dpHubDatum' puts at their positions, spelled out here so the fixtures below and
the datum agree by construction rather than by counting fields twice.
-}
hubWithdrawalPolicy, hubTxOrderPolicy :: BS.ByteString
hubWithdrawalPolicy = BS.replicate 28 8
hubTxOrderPolicy = BS.replicate 28 9

{- | The block's inclusion window is @(100, 200]@ — see 'headerData'.

Half-open at the bottom, so 100 is not due and 101 is. Every inclusion time
below is chosen against those two boundaries.
-}
dueTime, lateTime, earlyTime :: Integer
dueTime = 150
lateTime = 1234
earlyTime = 50

wdAssetName, toAssetName :: BS.ByteString
wdAssetName = "withdrawal-nft"
toAssetName = "tx-order-nft"

wdEventId, toEventId :: PD.Data
wdEventId = outRef 0x97
toEventId = outRef 0x98

{- | An optimistic event datum: the event, its inclusion time, and three fields
this family never reads.

The trailing three are what makes reading the first two positionally worth
saying out loud — the port takes them off any event datum without knowing which
one it holds.
-}
optimisticDatum :: PD.Data -> Integer -> PD.Data
optimisticDatum event inclusionTime =
  PD.Constr
    0
    [event, PD.I inclusionTime, PD.B (BS.replicate 28 0xc2), dpL1Address, PD.Constr 0 []]

-- | A reference input holding exactly one non-Ada asset, as a state UTxO does.
nftRefInput :: BS.ByteString -> BS.ByteString -> PD.Data -> PD.Data
nftRefInput policy name datum =
  dpRefInput (valueD [("", [("", 2_000_000)]), (policy, [(name, 1)])]) datum

wdInfo :: PD.Data
wdInfo = withdrawalInfoWith utxoRef 0

-- | The same withdrawal with a different verdict recorded against it.
wdInfoWithValidity :: Integer -> PD.Data
wdInfoWithValidity validity = withdrawalInfoWith utxoRef validity

wdRefInput :: Integer -> PD.Data
wdRefInput inclusionTime =
  nftRefInput
    hubWithdrawalPolicy
    wdAssetName
    (optimisticDatum (PD.Constr 0 [wdEventId, wdInfo]) inclusionTime)

{- | A forced transaction order, carrying the compact bytes of a real transaction.

The compact bytes are re-verified against the payload's own transaction id, so
they cannot be a placeholder: the validity interval this family reads comes out
of them and nowhere else.
-}
toSourceOf :: Tx -> PD.Data
toSourceOf tx =
  PD.Constr
    0
    [ PD.B (compactWithValidity tx (blake2b256 (witnessSetCborOf tx)) 0)
    , PD.B (witnessSetCborOf tx)
    , PD.B (BS.concat ("\x89" : replicate 9 "\x00"))
    ]

toPayloadOf :: Tx -> PD.Data
toPayloadOf tx =
  PD.Constr
    0
    [PD.B (txIdOf tx), PD.B (BS.replicate 32 0x99), toSourceOf tx, PD.Constr 1 []]

toRefInput :: Tx -> Integer -> PD.Data
toRefInput tx inclusionTime =
  nftRefInput
    hubTxOrderPolicy
    toAssetName
    (optimisticDatum (PD.Constr 0 [toEventId, toPayloadOf tx]) inclusionTime)

-- | @ledger_state.ForcedInclusionTxV1@ over the same order, under a verdict.
toSourceValue :: Tx -> Integer -> PD.Data
toSourceValue tx validity =
  PD.Constr 0 [PD.B (txIdOf tx), toSourceOf tx, PD.Constr validity []]

-- | A transaction whose validity interval closes before the block opens.
staleTx :: Tx
staleTx = tx1 {tValidityStart = 10, tValidityEnd = 50}

-- | The Aiken boundary vectors use an authenticated transaction with no own window.
forcedTimingTx :: Tx
forcedTimingTx = tx1 {tValidityStart = -1, tValidityEnd = -1}

{- | A block with all four source trees empty, so an absence can be proven
against any of them.
-}
omittedBlock :: Block
omittedBlock =
  defaultBlock
    { bWithdrawalsRoot = emptyRoot
    , bWithdrawalCount = 0
    , bForcedRoot = emptyRoot
    , bForcedCount = 0
    , bTransactionsRoot = emptyRoot
    , bL2Count = 0
    , bDepositsRoot = emptyRoot
    , bDepositCount = 0
    , bTotalEventCount = 0
    }

-- | A block whose named source tree holds exactly the one entry given.
sourceBlock :: Integer -> PD.Data -> PD.Data -> Block
sourceBlock domain key value
  | domain == withdrawalsDomain =
      defaultBlock {bWithdrawalsRoot = root, bWithdrawalCount = 1, bTotalEventCount = 4}
  | domain == forcedDomain =
      defaultBlock {bForcedRoot = root, bForcedCount = 1, bTotalEventCount = 5}
  | otherwise =
      defaultBlock {bDepositsRoot = root, bDepositCount = 1}
  where
    root = singleRoot domain 1 (serialise key) (serialise value)

omittedWitness :: Integer -> BS.ByteString -> PD.Data -> PD.Data
omittedWitness tag assetName nonMembership
  | tag == 2 = PD.Constr tag [PD.I 0, PD.B assetName, PD.Constr 0 [], nonMembership]
  | otherwise = PD.Constr tag [PD.I 0, PD.B assetName, nonMembership]

outOfWindowWitness :: Integer -> BS.ByteString -> Maybe Integer -> PD.Data -> PD.Data
outOfWindowWitness tag assetName override membership =
  case override of
    Nothing -> PD.Constr tag [PD.I 0, PD.B assetName, membership]
    Just v -> PD.Constr tag [PD.I 0, PD.B assetName, PD.Constr v [], membership]

--------------------------------------------------------------------------------

l1EventTests :: [TestTree]
l1EventTests =
  [ testGroup
      "transition-trace/proof Aiken forced-order timing parity"
      [ testCase "accepts_omitted_forced_order_at_authenticated_end_time_boundary" $
          passertEval $
            omitted
              omittedBlock
              (toRefInput forcedTimingTx 200)
              (omittedWitness 2 toAssetName (emptyNonMembership forcedDomain toEventId))
      , testCase "rejects_omitted_forced_order_at_excluded_start_time_boundary" $
          prefuses $
            omitted
              omittedBlock
              (toRefInput forcedTimingTx 100)
              (omittedWitness 2 toAssetName (emptyNonMembership forcedDomain toEventId))
      , testCase "rejects_omitted_forced_order_published_after_challenged_block" $
          prefuses $
            omitted
              omittedBlock
              (toRefInput forcedTimingTx 201)
              (omittedWitness 2 toAssetName (emptyNonMembership forcedDomain toEventId))
      , testCase "accepts_late_forced_order_as_out_of_window_source_event" $
          passertEval $
            outOfWindow
              (sourceBlock forcedDomain toEventId (toSourceValue forcedTimingTx 0))
              (toRefInput forcedTimingTx 201)
              (outOfWindowWitness 2 toAssetName (Just 0) (forcedLeaf forcedTimingTx 0))
      ]
  , testCase "an event at the block's start time is not yet due" $
      prefuses (ptimedL1EventIsDue (headerT defaultBlock) 100)
  , testCase "…one a millisecond later is" $
      passertEval (ptimedL1EventIsDue (headerT defaultBlock) 101)
  , testCase "…one at the block's end time still is" $
      passertEval (ptimedL1EventIsDue (headerT defaultBlock) 200)
  , testCase "…and one after it is not" $
      prefuses (ptimedL1EventIsDue (headerT defaultBlock) 201)
  , testCase "a transaction with no validity interval is always due" $
      passertEval (forcedDue (-1) (-1))
  , testCase "an open start is due while its end is still ahead of the block" $
      passertEval (forcedDue (-1) 150)
  , testCase "…and not once its end is behind the block" $
      prefuses (forcedDue (-1) 50)
  , testCase "an open end is due once its start is inside the block" $
      passertEval (forcedDue 150 (-1))
  , testCase "…and not while its start is still ahead" $
      prefuses (forcedDue 300 (-1))
  , testCase "a closed interval overlapping the block is due" $
      passertEval (forcedDue 150 180)
  , testCase "…one entirely before it is not" $ prefuses (forcedDue 10 50)
  , testCase "…one entirely after it is not" $ prefuses (forcedDue 300 400)
  , testCase "…and an inverted interval is due in no block at all" $
      prefuses (forcedDue 180 150)
  , testCase "a due deposit missing from the deposits tree is a fault" $
      passertEval $
        omitted
          omittedBlock
          (dpRefInput dpValue (dpDatumAt dueTime dpId dpInfo))
          (omittedWitness 0 dpAssetName (emptyNonMembership depositsDomain dpId))
  , testCase "…and one that arrived after the block closed is not" $
      prefuses $
        omitted
          omittedBlock
          (dpRefInput dpValue (dpDatumAt lateTime dpId dpInfo))
          (omittedWitness 0 dpAssetName (emptyNonMembership depositsDomain dpId))
  , testCase "…nor is an absence proven for another deposit id" $
      prefuses $
        omitted
          omittedBlock
          (dpRefInput dpValue (dpDatumAt dueTime dpId dpInfo))
          (omittedWitness 0 dpAssetName (emptyNonMembership depositsDomain (outRef 0x9a)))
  , testCase "a due withdrawal missing from the withdrawals tree is a fault" $
      passertEval $
        omitted
          omittedBlock
          (wdRefInput dueTime)
          (omittedWitness 1 wdAssetName (emptyNonMembership withdrawalsDomain wdEventId))
  , testCase "…and one that arrived before the block opened is not" $
      prefuses $
        omitted
          omittedBlock
          (wdRefInput earlyTime)
          (omittedWitness 1 wdAssetName (emptyNonMembership withdrawalsDomain wdEventId))
  , testCase "aborts when the withdrawal UTxO holds more than its own NFT" $
      pfails $
        omitted
          omittedBlock
          ( dpRefInput
              dpValue
              (optimisticDatum (PD.Constr 0 [wdEventId, wdInfo]) dueTime)
          )
          (omittedWitness 1 wdAssetName (emptyNonMembership withdrawalsDomain wdEventId))
  , testCase "a due forced transaction missing from the forced tree is a fault" $
      passertEval $
        omitted
          omittedBlock
          (toRefInput tx1 dueTime)
          (omittedWitness 2 toAssetName (emptyNonMembership forcedDomain toEventId))
  , testCase "…and one whose own window closed before the block is not" $
      prefuses $
        omitted
          omittedBlock
          (toRefInput staleTx dueTime)
          (omittedWitness 2 toAssetName (emptyNonMembership forcedDomain toEventId))
  , testCase "aborts on an order whose compact bytes are not its transaction's" $
      pfails $
        omitted
          omittedBlock
          ( nftRefInput
              hubTxOrderPolicy
              toAssetName
              ( optimisticDatum
                  ( PD.Constr
                      0
                      [ toEventId
                      , PD.Constr
                          0
                          [ PD.B (BS.replicate 32 0x9b)
                          , PD.B (BS.replicate 32 0x99)
                          , toSourceOf tx1
                          , PD.Constr 1 []
                          ]
                      ]
                  )
                  dueTime
              )
          )
          (omittedWitness 2 toAssetName (emptyNonMembership forcedDomain toEventId))
  , testCase "a deposit taken after the block closed is out of window" $
      passertEval $
        outOfWindow
          (sourceBlock depositsDomain dpId dpInfo)
          (dpRefInput dpValue (dpDatumAt lateTime dpId dpInfo))
          (outOfWindowWitness 0 dpAssetName Nothing (dpMembership dpInfo))
  , testCase "…and one taken inside the block is not" $
      prefuses $
        outOfWindow
          (sourceBlock depositsDomain dpId dpInfo)
          (dpRefInput dpValue (dpDatumAt dueTime dpId dpInfo))
          (outOfWindowWitness 0 dpAssetName Nothing (dpMembership dpInfo))
  , testCase "…nor is a leaf whose info is not the one the L1 UTxO carries" $
      prefuses $
        outOfWindow
          (sourceBlock depositsDomain dpId otherInfo)
          (dpRefInput dpValue (dpDatumAt lateTime dpId dpInfo))
          (outOfWindowWitness 0 dpAssetName Nothing (dpMembership otherInfo))
  , testCase "a withdrawal taken before the block opened is out of window" $
      passertEval $
        outOfWindow
          (sourceBlock withdrawalsDomain wdEventId wdInfo)
          (wdRefInput earlyTime)
          ( outOfWindowWitness
              1
              wdAssetName
              (Just 0)
              (withdrawalLeaf wdInfo)
          )
  , testCase "…and the fault holds under whichever verdict the block recorded" $
      passertEval $
        outOfWindow
          (sourceBlock withdrawalsDomain wdEventId (wdInfoWithValidity 2))
          (wdRefInput earlyTime)
          ( outOfWindowWitness
              1
              wdAssetName
              (Just 2)
              (withdrawalLeaf (wdInfoWithValidity 2))
          )
  , testCase "…but refuses an override the committed leaf does not carry" $
      prefuses $
        outOfWindow
          (sourceBlock withdrawalsDomain wdEventId wdInfo)
          (wdRefInput earlyTime)
          (outOfWindowWitness 1 wdAssetName (Just 2) (withdrawalLeaf wdInfo))
  , testCase "…and refuses one taken inside the block" $
      prefuses $
        outOfWindow
          (sourceBlock withdrawalsDomain wdEventId wdInfo)
          (wdRefInput dueTime)
          (outOfWindowWitness 1 wdAssetName (Just 0) (withdrawalLeaf wdInfo))
  , testCase "a forced transaction whose window closed is out of window" $
      passertEval $
        outOfWindow
          (sourceBlock forcedDomain toEventId (toSourceValue staleTx 0))
          (toRefInput staleTx dueTime)
          (outOfWindowWitness 2 toAssetName (Just 0) (forcedLeaf staleTx 0))
  , testCase "…and one still inside both windows is not" $
      prefuses $
        outOfWindow
          (sourceBlock forcedDomain toEventId (toSourceValue tx1 0))
          (toRefInput tx1 dueTime)
          (outOfWindowWitness 2 toAssetName (Just 0) (forcedLeaf tx1 0))
  ]
  where
    forcedDue start end = pforcedTxIsDue (headerT defaultBlock) (pconstant start) (pconstant end)
    omitted block refInput witness =
      pvalidateOmittedDueL1Event
        (headerT block)
        (fromData dpHubDatum)
        (witnessList [refInput])
        (asDataTerm witness)
    outOfWindow block refInput witness =
      pvalidateOutOfWindowSourceEvent
        (headerT block)
        (fromData dpHubDatum)
        (witnessList [refInput])
        (asDataTerm witness)
    otherInfo = dpInfoWith dpL2Address 0 nothingD
    withdrawalLeaf info =
      singleMembership
        withdrawalsDomain
        1
        (serialise wdEventId)
        (serialise info)
        wdEventId
        info
    forcedLeaf tx validity =
      singleMembership
        forcedDomain
        1
        (serialise toEventId)
        (serialise (toSourceValue tx validity))
        toEventId
        (toSourceValue tx validity)

--------------------------------------------------------------------------------
-- The one-step dispatcher
--------------------------------------------------------------------------------

{- | The dispatcher's job is routing, so each case checks two things at once: the
arm reaches its rule, and it reaches /that/ rule rather than a neighbour. The
negative cases are the ones carrying the second half — a witness that would be
refused by the rule it names but accepted by another would pass a routing test
that only ever fed it guilty blocks.
-}
oneStepDispatchTests :: [TestTree]
oneStepDispatchTests =
  [ testCase "the withdrawal arm convicts a step that published another root" $
      passertEval $
        dispatch
          (blockWithStep guiltyStep)
          ( PD.Constr
              0
              [ traceProofOf guiltyStep step1 0
              , mapMembership4
              , withdrawalMembership validWithdrawal
              , deleteWitness utxoKey utxoValue
              ]
          )
  , testCase "…and refuses the step that spent what it should" $
      prefuses $
        dispatch
          (blockWithStep honestStep)
          ( PD.Constr
              0
              [ traceProofOf honestStep step1 0
              , mapMembership4
              , withdrawalMembership validWithdrawal
              , deleteWitness utxoKey utxoValue
              ]
          )
  , testCase "the withdrawal no-op arm convicts a rejected one that moved the ledger" $
      passertEval $
        dispatch
          (invalidBlockWithStep honestStep)
          ( PD.Constr
              1
              [ traceProofOf honestStep step1 0
              , mapMembership4
              , withdrawalMembership invalidWithdrawal
              ]
          )
  , testCase "…and aborts on a withdrawal the operator called valid, as its rule does" $
      pfails $
        dispatch
          (blockWithStep honestStep)
          ( PD.Constr
              1
              [ traceProofOf honestStep step1 0
              , mapMembership4
              , withdrawalMembership validWithdrawal
              ]
          )
  , testCase "the forced no-op arm convicts a rejected order that moved the ledger" $
      passertEval $
        dispatch
          (forcedBlock forcedMovingStep)
          ( PD.Constr
              2
              [ traceProofOf forcedMovingStep step1 0
              , forcedMapMembership
              , forcedMembership rejectedForced
              ]
          )
  , testCase "…and refuses the one that changed nothing" $
      prefuses $
        dispatch
          (forcedBlock forcedNoOpStep)
          ( PD.Constr
              2
              [ traceProofOf forcedNoOpStep step1 0
              , forcedMapMembership
              , forcedMembership rejectedForced
              ]
          )
  , testCase "the deposit arm convicts a step that landed another root" $
      passertEval $
        dispatch
          (dpBlock (dpStep (ledgerRoot 0xce)) dpInfo)
          (depositArm (dpStep (ledgerRoot 0xce)))
  , testCase "…and refuses the one that landed the projection" $
      prefuses $
        dispatch (dpBlock (dpStep dpPostRoot) dpInfo) (depositArm (dpStep dpPostRoot))
  , testCase "the L2 arm convicts a transaction the ledger did not follow" $
      passertEval $
        dispatch (l2FaultBlock (l2Step (ledgerRoot 0xcd)) l2Leaf) (l2Arm (l2Step (ledgerRoot 0xcd)))
  , testCase "…and refuses the step that applied it" $
      prefuses $
        dispatch (l2FaultBlock (l2Step l2PostRoot) l2Leaf) (l2Arm (l2Step l2PostRoot))
  ]
  where
    dispatch block witness =
      pvalidateInvalidOneStepTransition
        (headerT block)
        (fromData dpHubDatum)
        (witnessList [dpHonestRefInput])
        (asDataTerm witness)
    depositArm s0 =
      PD.Constr
        3
        [ traceProofOf s0 step1 0
        , dpMapMembership
        , dpMembership dpInfo
        , PD.I 0
        , PD.B dpAssetName
        , insertWitness dpLedgerKey dpOutputCbor
        ]
    l2Arm s0 =
      PD.Constr
        4
        [ traceProofOf s0 step1 0
        , l2MapMembership
        , l2SourceMembership l2Leaf
        , PD.B (spendInputsPreimage tx1)
        , PD.B (outputsPreimage tx1)
        , PD.List l2SpendWitnesses
        , PD.List l2OutputWitnesses
        , l2Triple
        ]
    forcedEventKey = PD.Constr 1 [forcedOrderId]
    forcedNoOpStep = stepWith 0 forcedEventKey 1 ledgerBefore ledgerBefore
    forcedMovingStep = stepWith 0 forcedEventKey 1 ledgerBefore ledgerAfter
    forcedMap = PD.Constr 0 [PD.I 0, PD.Constr 1 []]
    forcedMapMembership =
      singleMembership
        eventToStepDomain
        4
        (serialise forcedEventKey)
        (serialise forcedMap)
        forcedEventKey
        forcedMap
    forcedBlock s0 =
      transitionBlock
        { bTraceRoot = commitCountedRoot traceDomain (traceRawRootOf s0 step1) 2
        , bEventToStepRoot =
            singleRoot eventToStepDomain 4 (serialise forcedEventKey) (serialise forcedMap)
        }

--------------------------------------------------------------------------------
-- The fault proof and its nine entry points
--------------------------------------------------------------------------------

-- | @blake2b_224(cbor.serialise(header))@ — what the envelope checks against.
headerHashOf :: Block -> BS.ByteString
headerHashOf = blake2b224 . serialise . headerData

{- | The computation thread's token name: this family's four-byte catalogue id
followed by the header hash of the block being convicted.
-}
threadNameFor :: Block -> BS.ByteString
threadNameFor block = "\x00\x00\x00\x04" <> headerHashOf block

-- | @proof.TransitionFaultProof@ — the hash, the header, and the fault.
faultProof :: Block -> PD.Data -> PD.Data
faultProof block fault = PD.Constr 0 [PD.B (headerHashOf block), headerData block, fault]

-- | A block whose trace does not start where its own prior ledger root says.
boundaryFaultBlock :: Block
boundaryFaultBlock = defaultBlock {bPrevUtxosRoot = ledgerRoot 0xb0}

boundaryFault :: PD.Data
boundaryFault = PD.Constr 0 [PD.Constr 0 [], traceProofOf step0 step1 0]

{- | The default fixture's two steps apply the same withdrawal, so the block is
guilty of the duplicate fault and innocent of every other — see the duplicate
group above.
-}
duplicateFault :: PD.Data
duplicateFault =
  PD.Constr 6 [traceProofOf step0 step1 0, traceProofOf step0 step1 1]

-- | A malformed accepted-mismatch fault for the typed-decoding boundary.
acceptedMismatchFault :: PD.Data
acceptedMismatchFault = PD.Constr 9 [PD.Constr 0 [PD.I 0, PD.B "witness"]]

oneStepFault :: PD.Data -> PD.Data
oneStepFault witness = PD.Constr 4 [witness]

-- | Two steps that do not meet, and the block committing them.
linkFaultStep1 :: PD.Data
linkFaultStep1 = stepWith 1 withdrawalKey 0 (ledgerRoot 0xb1) (ledgerRoot 0xa2)

linkFaultBlock :: Block
linkFaultBlock =
  defaultBlock
    {bTraceRoot = commitCountedRoot traceDomain (traceRawRootOf step0 linkFaultStep1) 2}

linkFault :: PD.Data
linkFault = PD.Constr 1 [adjacentOf step0 linkFaultStep1]

-- | An event the trace names that no source tree holds.
sourceMismatchFault :: PD.Data
sourceMismatchFault =
  PD.Constr
    3
    [ PD.Constr
        0
        [ traceProofOf step0 step1 0
        , mapMembershipAt 3 eventToStep0
        , PD.Constr 0 [emptyNonMembership withdrawalsDomain withdrawalKeyId]
        ]
    ]

--------------------------------------------------------------------------------
-- Convictions, for the dispatch validators above this library
--------------------------------------------------------------------------------

{- | A fault proof that genuinely holds, with the thread token name it has to be
filed under.

"Testing.FraudProofsTransitionTrace" drives the eight dispatch validators, each
of which convicts only if its entry point answers — so it needs proofs this
module has already shown to hold rather than a second set assembled beside them.
The eight below are exactly the cases 'faultProofTests' asserts, which is why
that group consumes them too instead of inlining its own.
-}
data ConvictingProof = ConvictingProof
  { cpThreadName :: BS.ByteString
  , cpProof :: PD.Data
  }

convictionOf :: Block -> PD.Data -> ConvictingProof
convictionOf block fault =
  ConvictingProof {cpThreadName = threadNameFor block, cpProof = faultProof block fault}

-- | A trace that does not start where the block's own prior ledger root says.
controlConviction :: ConvictingProof
controlConviction = convictionOf boundaryFaultBlock boundaryFault

-- | An event the trace names that no source tree holds.
sourceConviction :: ConvictingProof
sourceConviction = convictionOf noWithdrawalsBlock sourceMismatchFault

-- | The default fixture's two steps, which apply the same withdrawal twice.
duplicateConviction :: ConvictingProof
duplicateConviction = convictionOf defaultBlock duplicateFault

-- | A withdrawal the block should have applied and moved the ledger for.
withdrawalConviction :: ConvictingProof
withdrawalConviction =
  convictionOf (blockWithStep guiltyStep) (oneStepFault (withdrawalArm guiltyStep))

withdrawalArm :: PD.Data -> PD.Data
withdrawalArm s0 =
  PD.Constr
    0
    [ traceProofOf s0 step1 0
    , mapMembership4
    , withdrawalMembership validWithdrawal
    , deleteWitness utxoKey utxoValue
    ]

-- | A rejected forced transaction the block moved the ledger for anyway.
forcedConviction :: ConvictingProof
forcedConviction =
  convictionOf (forcedFaultBlock forcedMovingStep) (oneStepFault (forcedArm forcedMovingStep))

forcedArm :: PD.Data -> PD.Data
forcedArm s0 =
  PD.Constr
    2
    [traceProofOf s0 step1 0, forcedFaultMapMembership, forcedMembership rejectedForced]

forcedEventKey :: PD.Data
forcedEventKey = PD.Constr 1 [forcedOrderId]

forcedMovingStep :: PD.Data
forcedMovingStep = stepWith 0 forcedEventKey 1 ledgerBefore ledgerAfter

forcedMap :: PD.Data
forcedMap = PD.Constr 0 [PD.I 0, PD.Constr 1 []]

forcedFaultMapMembership :: PD.Data
forcedFaultMapMembership =
  singleMembership
    eventToStepDomain
    4
    (serialise forcedEventKey)
    (serialise forcedMap)
    forcedEventKey
    forcedMap

forcedFaultBlock :: PD.Data -> Block
forcedFaultBlock s0 =
  transitionBlock
    { bTraceRoot = commitCountedRoot traceDomain (traceRawRootOf s0 step1) 2
    , bEventToStepRoot =
        singleRoot eventToStepDomain 4 (serialise forcedEventKey) (serialise forcedMap)
    }

-- | An L2 transaction whose step does not apply the spends and outputs it names.
acceptedTransactionConviction :: ConvictingProof
acceptedTransactionConviction =
  convictionOf (l2FaultBlock l2ConvictionStep l2Leaf) (oneStepFault (l2Arm l2ConvictionStep))

l2ConvictionStep :: PD.Data
l2ConvictionStep = l2Step (ledgerRoot 0xcd)

l2Arm :: PD.Data -> PD.Data
l2Arm s0 =
  PD.Constr
    4
    [ traceProofOf s0 step1 0
    , l2MapMembership
    , l2SourceMembership l2Leaf
    , PD.B (spendInputsPreimage tx1)
    , PD.B (outputsPreimage tx1)
    , PD.List l2SpendWitnesses
    , PD.List l2OutputWitnesses
    , l2Triple
    ]

-- | A deposit the block should have projected into the ledger and did not.
depositConviction :: ConvictingProof
depositConviction =
  convictionOf (dpBlock dpConvictionStep dpInfo) (oneStepFault (depositArm dpConvictionStep))

dpConvictionStep :: PD.Data
dpConvictionStep = dpStep (ledgerRoot 0xce)

depositArm :: PD.Data -> PD.Data
depositArm s0 =
  PD.Constr
    3
    [ traceProofOf s0 step1 0
    , dpMapMembership
    , dpMembership dpInfo
    , PD.I 0
    , PD.B dpAssetName
    , insertWitness dpLedgerKey dpOutputCbor
    ]

{- | A deposit the block included though its inclusion time is outside the
block's own window.

Its reference input is 'depositReferenceInput', the same one the deposit
conviction is read off — the deposit fixture's inclusion time is already late.
-}
l1EventConviction :: ConvictingProof
l1EventConviction =
  convictionOf
    (sourceBlock depositsDomain dpId dpInfo)
    (PD.Constr 7 [outOfWindowWitness 0 dpAssetName Nothing (dpMembership dpInfo)])

-- | The hub oracle datum naming the deposit policy the two above authenticate against.
depositHubOracleDatum :: PD.Data
depositHubOracleDatum = dpHubDatum

-- | The deposit UTxO both of those convictions read, as a reference input.
depositReferenceInput :: PD.Data
depositReferenceInput = dpHonestRefInput

{- | Its resolved half on its own.

The dispatch tests put this deposit UTxO into a real @ScriptContext@ and so need
a @TxInInfo@ that decodes, which 'depositReferenceInput' is not: the fixture's
output references carry a @TxId@ wrapped in a constructor, as V1 and V2 encode
it, and nothing in this module ever reads one. The output is the half that is
read, so that is the half the dispatch tests take.
-}
depositReferenceOutput :: PD.Data
depositReferenceOutput = dpRefOutput dpValue (dpDatum dpId dpInfo)

faultProofTests :: [TestTree]
faultProofTests =
  [ testCase "the envelope admits a header that hashes to the challenged hash" $
      passertEval $
        pvalidateTransitionFaultProofEnvelope
          (fromData (faultProof boundaryFaultBlock boundaryFault))
          (asDataTerm (PD.B (threadNameFor boundaryFaultBlock)))
  , testCase "…refuses a challenged hash that is not this header's" $
      prefuses $
        pvalidateTransitionFaultProofEnvelope
          ( fromData
              ( PD.Constr
                  0
                  [ PD.B (headerHashOf defaultBlock)
                  , headerData boundaryFaultBlock
                  , boundaryFault
                  ]
              )
          )
          (asDataTerm (PD.B (threadNameFor boundaryFaultBlock)))
  , testCase "…refuses a thread token that is not 32 bytes" $
      prefuses $
        pvalidateTransitionFaultProofEnvelope
          (fromData (faultProof boundaryFaultBlock boundaryFault))
          (asDataTerm (PD.B (threadNameFor boundaryFaultBlock <> "\x00")))
  , testCase "…refuses one filed under another fraud category" $
      prefuses $
        pvalidateTransitionFaultProofEnvelope
          (fromData (faultProof boundaryFaultBlock boundaryFault))
          (asDataTerm (PD.B ("\x00\x00\x00\x05" <> headerHashOf boundaryFaultBlock)))
  , testCase "…and refuses one opened against another block" $
      prefuses $
        pvalidateTransitionFaultProofEnvelope
          (fromData (faultProof boundaryFaultBlock boundaryFault))
          (asDataTerm (PD.B ("\x00\x00\x00\x04" <> headerHashOf defaultBlock)))
  , testCase "the control entry point answers a boundary fault" $
      passertEval (control boundaryFaultBlock boundaryFault)
  , testCase "…and refuses a duplicate fault, which is not its family" $
      prefuses (control defaultBlock duplicateFault)
  , testCase "…and refuses even a boundary fault on a mismatched thread token" $
      prefuses $
        pvalidateControlFaultProof
          (fromData (faultProof boundaryFaultBlock boundaryFault))
          (asDataTerm (PD.B (threadNameFor defaultBlock)))
  , testCase "…a link fault" $
      passertEval (control linkFaultBlock linkFault)
  , testCase "…an event-to-step mismatch" $
      passertEval (control (blockMapping otherIndexMapping) eventToStepMismatchFault)
  , testCase "…and a count fault" $
      passertEval (control defaultBlock {bTotalEventCount = 9} (PD.Constr 8 [PD.Constr 0 []]))
  , testCase "the source entry point answers a source-membership mismatch" $
      passertEval (entry pvalidateSourceFaultProof noWithdrawalsBlock sourceMismatchFault)
  , testCase "…and refuses a boundary fault" $
      prefuses (entry pvalidateSourceFaultProof boundaryFaultBlock boundaryFault)
  , testCase "the catch-all entry point answers a source-membership mismatch" $
      passertEval (catchAll noWithdrawalsBlock sourceMismatchFault)
  , testCase "the duplicate entry point answers a duplicate fault" $
      passertEval $
        entry pvalidateDuplicateFaultProof defaultBlock duplicateFault
  , testCase "the withdrawal entry point answers the valid-withdrawal arm" $
      passertEval $ entryFor pvalidateWithdrawalFaultProof withdrawalConviction
  , testCase "…and refuses the deposit arm, which is another validator's" $
      prefuses $ entryFor pvalidateWithdrawalFaultProof depositConviction
  , testCase "the forced entry point answers the forced no-op arm" $
      passertEval $ entryFor pvalidateForcedFaultProof forcedConviction
  , testCase "the deposit entry point answers the deposit arm" $
      passertEval $ depositEntryFor depositConviction
  , testCase "…and refuses the L2 arm" $
      prefuses $ depositEntryFor acceptedTransactionConviction
  , testCase "the accepted-transaction entry point answers the L2 arm" $
      passertEval $ entryFor pvalidateAcceptedTransactionFaultProof acceptedTransactionConviction
  , testCase "…and aborts on a malformed accepted-mismatch witness" $
      pfails $
        entry pvalidateAcceptedTransactionFaultProof defaultBlock acceptedMismatchFault
  , testCase "the L1-event entry point answers an omitted-due fault" $
      passertEval $
        l1Entry
          omittedBlock
          ( PD.Constr
              5
              [omittedWitness 0 dpAssetName (emptyNonMembership depositsDomain dpId)]
          )
  , testCase "…and an out-of-window fault, off the same reference input" $
      passertEval $ l1EntryFor l1EventConviction
  , testCase "…and refuses a boundary fault" $
      prefuses (l1Entry boundaryFaultBlock boundaryFault)
  , testCase "the catch-all entry point answers a boundary fault" $
      passertEval (catchAll boundaryFaultBlock boundaryFault)
  , testCase "…and aborts on a malformed accepted-mismatch witness" $
      pfails (catchAll defaultBlock acceptedMismatchFault)
  ]
  where
    control block fault =
      pvalidateControlFaultProof
        (fromData (faultProof block fault))
        (asDataTerm (PD.B (threadNameFor block)))
    entry f block fault =
      f (fromData (faultProof block fault)) (asDataTerm (PD.B (threadNameFor block)))
    entryFor f c = f (fromData (cpProof c)) (asDataTerm (PD.B (cpThreadName c)))
    -- The deposit and L1-event entry points take the hub datum and the
    -- reference inputs on top of the proof, and the two convictions below are
    -- served by the same reference input — which is what lets the dispatch
    -- validators drive both off one exported fixture.
    hubEntryFor f c =
      f
        (fromData (cpProof c))
        (asDataTerm (PD.B (cpThreadName c)))
        (fromData depositHubOracleDatum)
        (witnessList [depositReferenceInput])
    depositEntryFor = hubEntryFor pvalidateDepositFaultProof
    l1EntryFor = hubEntryFor pvalidateL1EventFaultProof
    l1Entry = l1EntryAt dueTime
    l1EntryAt inclusionTime block fault =
      pvalidateL1EventFaultProof
        (fromData (faultProof block fault))
        (asDataTerm (PD.B (threadNameFor block)))
        (fromData dpHubDatum)
        (witnessList [dpRefInput dpValue (dpDatumAt inclusionTime dpId dpInfo)])
    catchAll block fault =
      pvalidateTransitionFaultProof
        (fromData (faultProof block fault))
        (asDataTerm (PD.B (threadNameFor block)))
        (fromData dpHubDatum)
        (witnessList [dpHonestRefInput])
    otherIndexMapping = PD.Constr 0 [PD.I 1, PD.Constr 0 []]
    eventToStepMismatchFault =
      PD.Constr 2 [traceProofOf step0 step1 0, membershipArm (mapMembership otherIndexMapping)]
