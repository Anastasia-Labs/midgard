{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.HeaderValidity
Description : Tests for the header-validity layer of
              @lib/midgard/ledger-state.ak@.

@header_v1_is_valid@ is what a block header has to satisfy on its own, before
anything is checked about what it is appended to. The state queue runs it on
commit and again on merge, and fraud proofs are written assuming it held, so a
gap here is a gap everywhere downstream.

Three of its conditions are accounting identities rather than bounds, and they
carry the weight: the event total must equal the sum of the four per-kind
counts, there must be exactly one transition step per event, and the validation
traces must count exactly the script-running events. Those get a test each in
both directions.
-}
module Testing.HeaderValidity (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.Builtin.Crypto (pblake2b_224)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.LedgerState (
  PConfirmedState,
  PHeaderV1,
  pconfirmedStateNextHeaderProtocolVersionV1,
  pgenesisConfirmedStateV1,
  pheaderTransitionCommitmentsV1,
  pheaderTransitionCommitmentsV1AreValid,
  pheaderV1IsValid,
  pheaderValidationContextScalarsV1AreValid,
  prootMatchesCountV1,
 )
import Midgard.StateQueue qualified as StateQueue
import Testing.Eval (passertEval)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Header Validity Tests"
    [ rootCountTests
    , scalarTests
    , commitmentTests
    , headerTests
    , confirmedStateTests
    , ledgerStateVectorTests
    ]

--------------------------------------------------------------------------------
-- root_matches_count_v1
--------------------------------------------------------------------------------

{- | The counted-root invariant: a count and its root must agree about whether
anything was committed.
-}
rootCountTests :: TestTree
rootCountTests =
  testGroup
    "rootMatchesCountV1"
    [ testCase "accepts the empty root at count zero" $
        holds $ rootCount emptyRoot 0
    , -- A non-empty root at count zero would commit to events the header does
      -- not admit to having.
      testCase "rejects a non-empty root at count zero" $
        fails $ rootCount fullRoot 0
    , testCase "accepts a non-empty 32-byte root at a positive count" $
        holds $ rootCount fullRoot 1
    , -- And the converse: claiming events under the empty root.
      testCase "rejects the empty root at a positive count" $
        fails $ rootCount emptyRoot 1
    , testCase "rejects a root that is not 32 bytes" $
        fails $ rootCount (BS.replicate 31 0x07) 1
    , testCase "rejects a negative count" $
        fails $ rootCount fullRoot (-1)
    ]
  where
    rootCount root count = prootMatchesCountV1 # pconstant root # pconstant count

--------------------------------------------------------------------------------
-- header_validation_context_scalars_v1_are_valid
--------------------------------------------------------------------------------

scalarTests :: TestTree
scalarTests =
  testGroup
    "headerValidationContextScalarsV1AreValid"
    [ testCase "accepts network id 0" $
        holds $ scalars header {hNetworkId = 0}
    , testCase "accepts network id 1" $
        holds $ scalars header {hNetworkId = 1}
    , testCase "rejects any other network id" $
        fails $ scalars header {hNetworkId = 2}
    , -- A negative min_fee_b would let fee validation admit free transactions.
      testCase "rejects a negative min_fee_b" $
        fails $ scalars header {hMinFeeB = -1}
    , testCase "rejects a negative min_fee_a" $
        fails $ scalars header {hMinFeeA = -1}
    , testCase "rejects a negative block slot" $
        fails $ scalars header {hBlockSlot = -1}
    ]
  where
    scalars h = pheaderValidationContextScalarsV1AreValid # headerTerm h

--------------------------------------------------------------------------------
-- header_transition_commitments_v1_are_valid
--------------------------------------------------------------------------------

commitmentTests :: TestTree
commitmentTests =
  testGroup
    "headerTransitionCommitmentsV1AreValid"
    [ testCase "accepts an empty block" $
        holds $ commitments header
    , testCase "accepts a block carrying one withdrawal" $
        holds $ commitments oneWithdrawal
    , testCase "accepts a block carrying one L2 transaction" $
        holds $ commitments oneL2Tx
    , -- The event total is the sum of the four per-kind counts, so a block
      -- cannot claim more events than it itemised.
      testCase "rejects an event total above the sum of its parts" $
        fails $ commitments oneWithdrawal {hTotalEventCount = 2, hTransitionStepCount = 2}
    , testCase "rejects an event total below the sum of its parts" $
        fails $ commitments oneWithdrawal {hTotalEventCount = 0, hTransitionStepCount = 0}
    , -- One transition step per event, exactly.
      testCase "rejects a transition-step count that is not the event total" $
        fails $ commitments oneWithdrawal {hTransitionStepCount = 2}
    , -- Validation traces count exactly the script-running events: the forced
      -- transactions and the L2 transactions, not withdrawals or deposits.
      testCase "rejects a validation-trace count that includes a withdrawal" $
        fails $ commitments oneWithdrawal {hValidationTraceCount = 1}
    , testCase "rejects an L2 transaction with no validation trace" $
        fails $ commitments oneL2Tx {hValidationTraceCount = 0, hValidationTracesRoot = emptyRoot}
    , testCase "rejects a count above its ceiling" $
        fails $
          commitments
            header
              { hWithdrawalCount = 10_001
              , hWithdrawalsRoot = fullRoot
              , hTotalEventCount = 10_001
              , hTransitionStepCount = 10_001
              , hTransitionTraceRoot = fullRoot
              , hEventToStepRoot = fullRoot
              }
    , testCase "rejects a negative count" $
        fails $ commitments header {hWithdrawalCount = -1, hTotalEventCount = -1, hTransitionStepCount = -1}
    ]
  where
    commitments h =
      pheaderTransitionCommitmentsV1AreValid
        # (pheaderTransitionCommitmentsV1 # headerTerm h)

--------------------------------------------------------------------------------
-- header_v1_is_valid
--------------------------------------------------------------------------------

headerTests :: TestTree
headerTests =
  testGroup
    "headerV1IsValid"
    [ testCase "accepts a well-formed empty header" $
        holds $ valid header
    , testCase "accepts a well-formed header carrying events" $
        holds $ valid oneL2Tx
    , -- The genesis sentinel's version zero must never reach a header.
      testCase "rejects protocol version 0" $
        fails $ valid header {hProtocolVersion = 0}
    , testCase "rejects a future protocol version" $
        fails $ valid header {hProtocolVersion = 2}
    , -- Composition: each of the three sub-predicates can veto.
      testCase "rejects a header failing the scalar checks" $
        fails $ valid header {hNetworkId = 2}
    , testCase "rejects a header failing the commitment identities" $
        fails $ valid oneWithdrawal {hTransitionStepCount = 2}
    , testCase "rejects a withdrawals root that does not match its count" $
        fails $ valid header {hWithdrawalsRoot = fullRoot}
    , testCase "rejects a deposits root that does not match its count" $
        fails $ valid header {hDepositsRoot = fullRoot}
    , -- The relational fields are deliberately not checked here; they only mean
      -- something against a predecessor, and the state queue checks them.
      testCase "accepts a header whose end time precedes its start time" $
        holds $ valid header {hStartTime = 200, hEndTime = 100}
    ]
  where
    valid h = pheaderV1IsValid # headerTerm h

--------------------------------------------------------------------------------
-- The confirmed-state pair
--------------------------------------------------------------------------------

confirmedStateTests :: TestTree
confirmedStateTests =
  testGroup
    "confirmed state"
    [ testCase "genesisConfirmedStateV1 builds the sentinel at a valid time" $
        passertEval $
          pmatch (pgenesisConfirmedStateV1 # 500) $ \case
            PNothing -> pconstant @PBool False
            PJust st -> st #== confirmedTerm (genesis 500)
    , testCase "genesisConfirmedStateV1 refuses a negative genesis time" $
        passertEval $
          pmatch (pgenesisConfirmedStateV1 # (-1)) $ \case
            PNothing -> pconstant @PBool True
            PJust _ -> pconstant @PBool False
    , -- The sentinel is one specific value, so every field is pinned.
      testCase "accepts the genesis sentinel and answers v1" $
        answersV1 (genesis 500)
    , testCase "rejects a sentinel whose times differ" $
        answersNothing (genesis 500) {cEndTime = 600}
    , testCase "rejects a sentinel with a non-genesis utxo root" $
        answersNothing (genesis 500) {cUtxoRoot = fullRoot}
    , testCase "rejects a sentinel at a negative time" $
        answersNothing (genesis 500) {cStartTime = -1, cEndTime = -1}
    , -- The version answered is the *next* header's, not the state's own, which
      -- is how the sentinel's zero is kept out of committed headers.
      testCase "answers v1 for the sentinel, whose own version is 0" $
        answersV1 (genesis 500)
    , testCase "accepts an ordinary confirmed state" $
        answersV1 ordinary
    , -- An ordinary state may not claim the genesis hash.
      testCase "rejects an ordinary state carrying the genesis header hash" $
        answersNothing ordinary {cHeaderHash = BS.replicate 28 0x00}
    , testCase "rejects an ordinary state whose end time precedes its start" $
        answersNothing ordinary {cStartTime = 600, cEndTime = 500}
    , testCase "accepts an ordinary state covering a single instant" $
        answersV1 ordinary {cStartTime = 500, cEndTime = 500}
    , testCase "rejects a state at an unknown protocol version" $
        answersNothing ordinary {cProtocolVersion = 2}
    ]
  where
    answersV1 c =
      passertEval $
        pmatch (pconfirmedStateNextHeaderProtocolVersionV1 # confirmedTerm c) $ \case
          PNothing -> pconstant @PBool False
          PJust v -> v #== 1
    answersNothing c =
      passertEval $
        pmatch (pconfirmedStateNextHeaderProtocolVersionV1 # confirmedTerm c) $ \case
            PNothing -> pconstant @PBool True
            PJust _ -> pconstant @PBool False

--------------------------------------------------------------------------------
-- Exact Aiken ledger-state vectors
--------------------------------------------------------------------------------

ledgerStateVectorTests :: TestTree
ledgerStateVectorTests =
  testGroup
    "Aiken ledger-state V1 vectors"
    [ testCase "l01_header_v1_aiken_cbor_and_hash_vector_is_exact" $
        passertEval exactHeaderVector
    , testCase "l02_header_transition_commitments_v1_are_complete_and_exact" $
        passertEval exactCommitmentsVector
    , testCase "l03_state_queue_node_v1_aiken_topology_and_datum_vector_is_exact" $
        passertEval exactStateQueueNodeVector
    , testCase "l04_init_v1_and_merge_to_confirmed_state_v1_aiken_vectors_are_exact" $
        passertEval exactStateQueueRedeemerVectors
    , testCase "genesis_protocol_sentinel_and_first_ordinary_v1_are_distinct" $
        passertEval exactGenesisSentinelCases
    , testCase "l05_header_v1_counted_roots_require_canonical_hash_length" $
        passertEval exactCountedRootCases
    ]

exactHeaderVector :: forall s. Term s PBool
exactHeaderVector =
  plet (pserialiseData # pforgetData (pdata (headerTerm vectorHeader))) $ \encoded ->
    pand'List
      [ encoded #== pconstant vectorHeaderCbor
      , pblake2b_224 # encoded #== pconstant (hex "68e507eaad2278934d696204c01ffa64ca7381e989823e5aed19afbc")
      , pheaderV1IsValid # headerTerm vectorHeader
      ]

exactCommitmentsVector :: forall s. Term s PBool
exactCommitmentsVector =
  plet (pheaderTransitionCommitmentsV1 # headerTerm vectorHeader) $ \commitments ->
    pand'List
      [ pserialiseData # pforgetData (pdata commitments) #== pconstant vectorCommitmentsCbor
      , pheaderTransitionCommitmentsV1AreValid # commitments
      , pnot
          # ( pheaderTransitionCommitmentsV1AreValid
                # (pheaderTransitionCommitmentsV1 # headerTerm vectorHeader {hForcedTransactionCount = 1})
            )
      , pnot
          # ( pheaderTransitionCommitmentsV1AreValid
                # (pheaderTransitionCommitmentsV1 # headerTerm vectorHeader {hTransitionStepCount = 0})
            )
      , pnot # (pheaderV1IsValid # headerTerm vectorHeader {hProtocolVersion = 2})
      ]

exactStateQueueNodeVector :: forall s. Term s PBool
exactStateQueueNodeVector =
  let node =
        pcon $
          StateQueue.PStateQueueNode
            (pdata (headerTerm vectorHeader))
            (pdata (pconstant "\xaa"))
   in pserialiseData # pforgetData (pdata node) #== pconstant vectorStateQueueNodeCbor

exactStateQueueRedeemerVectors :: forall s. Term s PBool
exactStateQueueRedeemerVectors =
  let initRedeemer = pcon $ StateQueue.PInitV1 (pdata 2)
      mergeRedeemer =
        pcon $
          StateQueue.PMergeToConfirmedStateV1
            (pconstant $ PD.B (BS.replicate 28 0x11))
            (pconstant $ PD.Constr 0 [PD.B (BS.replicate 32 0x44), PD.I 0])
            (pconstant $ PD.I 0)
            (pconstant $ PD.Constr 0 [PD.I 1])
            (pconstant $ PD.B (BS.replicate 32 0x21))
            (pconstant $ PD.B (BS.replicate 32 0x22))
            (pconstant $ PD.B (BS.replicate 32 0x23))
            (pconstant $ PD.B (BS.replicate 32 0x24))
            (pconstant $ PD.B (BS.replicate 32 0x25))
            (pconstant $ PD.B (BS.replicate 32 0x26))
            (pconstant $ PD.B (BS.replicate 32 0x27))
            (pconstant $ PD.I 1)
            (pconstant $ PD.I 2)
            (pconstant $ PD.I 3)
            (pconstant $ PD.I 4)
            (pconstant $ PD.I 10)
            (pconstant $ PD.I 10)
            (pconstant $ PD.I 5)
   in pand'List
        [ pserialiseData # pforgetData (pdata initRedeemer) #== pconstant (hex "d8799f02ff")
        , pserialiseData # pforgetData (pdata mergeRedeemer) #== pconstant vectorMergeRedeemerCbor
        ]

exactGenesisSentinelCases :: forall s. Term s PBool
exactGenesisSentinelCases =
  let exactGenesis = genesis 10
      ordinaryV1 =
        Confirmed
          { cHeaderHash = BS.replicate 28 0xaa
          , cPrevHeaderHash = BS.replicate 28 0x00
          , cUtxoRoot = BS.replicate 32 0xbb
          , cStartTime = 10
          , cEndTime = 11
          , cProtocolVersion = 1
          }
      nextIsV1 state =
        pmatch (pconfirmedStateNextHeaderProtocolVersionV1 # confirmedTerm state) $ \case
          PNothing -> pconstant False
          PJust version -> version #== 1
      nextIsNothing state =
        pmatch (pconfirmedStateNextHeaderProtocolVersionV1 # confirmedTerm state) $ \case
          PNothing -> pconstant True
          PJust _ -> pconstant False
   in pand'List
        [ pmatch (pgenesisConfirmedStateV1 # 10) $ \case
            PNothing -> pconstant False
            PJust state -> state #== confirmedTerm exactGenesis
        , pmatch (pgenesisConfirmedStateV1 # (-1)) $ \case
            PNothing -> pconstant True
            PJust _ -> pconstant False
        , nextIsV1 exactGenesis
        , nextIsV1 ordinaryV1
        , nextIsNothing exactGenesis {cProtocolVersion = 1}
        , nextIsNothing exactGenesis {cHeaderHash = BS.replicate 28 0xaa, cUtxoRoot = BS.replicate 32 0xbb}
        , nextIsNothing exactGenesis {cPrevHeaderHash = BS.replicate 28 0xaa}
        , nextIsNothing exactGenesis {cUtxoRoot = BS.replicate 32 0xbb}
        , nextIsNothing exactGenesis {cEndTime = 11}
        , nextIsNothing ordinaryV1 {cStartTime = 11, cEndTime = 10}
        , nextIsNothing ordinaryV1 {cProtocolVersion = 2}
        ]

exactCountedRootCases :: forall s. Term s PBool
exactCountedRootCases =
  let zeroHeader =
        vectorHeader
          { hTransactionsRoot = emptyRoot
          , hTransitionTraceRoot = emptyRoot
          , hEventToStepRoot = emptyRoot
          , hValidationTracesRoot = emptyRoot
          , hL2TransactionCount = 0
          , hTotalEventCount = 0
          , hTransitionStepCount = 0
          , hValidationTraceCount = 0
          }
   in pand'List
        [ pnot # (pheaderV1IsValid # headerTerm vectorHeader {hValidationTracesRoot = "\xaa"})
        , pnot # (pheaderV1IsValid # headerTerm vectorHeader {hValidationTracesRoot = BS.replicate 33 0x08})
        , pnot # (pheaderV1IsValid # headerTerm vectorHeader {hValidationTracesRoot = emptyRoot})
        , pheaderV1IsValid # headerTerm vectorHeader
        , pheaderV1IsValid # headerTerm zeroHeader
        , pnot # (pheaderV1IsValid # headerTerm zeroHeader {hValidationTracesRoot = BS.replicate 32 0x08})
        ]

vectorHeader :: Header
vectorHeader =
  Header
    { hPrevUtxosRoot = BS.replicate 32 0x01
    , hUtxosRoot = BS.replicate 32 0x02
    , hWithdrawalsRoot = emptyRoot
    , hForcedTransactionsRoot = emptyRoot
    , hTransactionsRoot = BS.replicate 32 0x03
    , hDepositsRoot = emptyRoot
    , hTransitionTraceRoot = BS.replicate 32 0x04
    , hEventToStepRoot = BS.replicate 32 0x05
    , hValidationTracesRoot = BS.replicate 32 0x08
    , hWithdrawalCount = 0
    , hForcedTransactionCount = 0
    , hL2TransactionCount = 1
    , hDepositCount = 0
    , hTotalEventCount = 1
    , hTransitionStepCount = 1
    , hValidationTraceCount = 1
    , hStartTime = 1
    , hEndTime = 2
    , hBlockSlot = 0
    , hNetworkId = 0
    , hMinFeeA = 0
    , hMinFeeB = 0
    , hPrevHeaderHash = BS.replicate 28 0x06
    , hOperatorVkey = BS.replicate 28 0x07
    , hProtocolVersion = 1
    }

vectorHeaderCbor :: BS.ByteString
vectorHeaderCbor = hex $
  "d8799f58200101010101010101010101010101010101010101010101010101010101010101"
    <> "58200202020202020202020202020202020202020202020202020202020202020202"
    <> "58200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
    <> "58200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
    <> "58200303030303030303030303030303030303030303030303030303030303030303"
    <> "58200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
    <> "58200404040404040404040404040404040404040404040404040404040404040404"
    <> "58200505050505050505050505050505050505050505050505050505050505050505"
    <> "58200808080808080808080808080808080808080808080808080808080808080808"
    <> "00000100010101010200000000581c06060606060606060606060606060606060606060606060606060606"
    <> "581c0707070707070707070707070707070707070707070707070707070701ff"

vectorCommitmentsCbor :: BS.ByteString
vectorCommitmentsCbor = hex $
  "d8799f58200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
    <> "58200404040404040404040404040404040404040404040404040404040404040404"
    <> "58200505050505050505050505050505050505050505050505050505050505050505"
    <> "58200808080808080808080808080808080808080808080808080808080808080808"
    <> "00000100010101ff"

vectorStateQueueNodeCbor :: BS.ByteString
vectorStateQueueNodeCbor = hex "d8799f" <> vectorHeaderCbor <> hex "41aaff"

vectorMergeRedeemerCbor :: BS.ByteString
vectorMergeRedeemerCbor = hex $
  "d87d9f581c11111111111111111111111111111111111111111111111111111111"
    <> "d8799f5820444444444444444444444444444444444444444444444444444444444444444400ff"
    <> "00d8799f01ff"
    <> "58202121212121212121212121212121212121212121212121212121212121212121"
    <> "58202222222222222222222222222222222222222222222222222222222222222222"
    <> "58202323232323232323232323232323232323232323232323232323232323232323"
    <> "58202424242424242424242424242424242424242424242424242424242424242424"
    <> "58202525252525252525252525252525252525252525252525252525252525252525"
    <> "58202626262626262626262626262626262626262626262626262626262626262626"
    <> "58202727272727272727272727272727272727272727272727272727272727272727"
    <> "010203040a0a05ff"

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient

--------------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

fails :: (forall s. Term s PBool) -> Assertion
fails b = passertEval (pnot # b)

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

-- | Aiken @env.empty_merkle_tree_root@ — blake2b_256 of the empty list.
emptyRoot :: BS.ByteString
emptyRoot =
  BS.pack
    [ 0x0e, 0x57, 0x51, 0xc0, 0x26, 0xe5, 0x43, 0xb2
    , 0xe8, 0xab, 0x2e, 0xb0, 0x60, 0x99, 0xda, 0xa1
    , 0xd1, 0xe5, 0xdf, 0x47, 0x77, 0x8f, 0x77, 0x87
    , 0xfa, 0xab, 0x45, 0xcd, 0xf1, 0x2f, 0xe3, 0xa8
    ]

-- | Any 32-byte root that is not the empty one.
fullRoot :: BS.ByteString
fullRoot = BS.replicate 32 0x33

{- | A @HeaderV1@ as twenty-five named fields.

A record so each test names the single field it changed rather than restating a
whole header; 'headerData' puts them back in the order the on-chain encoding
requires.
-}
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

-- | A valid header committing nothing: every root empty, every count zero.
header :: Header
header =
  Header
    { hPrevUtxosRoot = emptyRoot
    , hUtxosRoot = emptyRoot
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
    , hPrevHeaderHash = BS.replicate 28 0x02
    , hOperatorVkey = BS.replicate 28 0x03
    , hProtocolVersion = 1
    }

{- | One withdrawal: an event that moves value but runs no script, so it counts
towards the event total but not towards the validation traces.
-}
oneWithdrawal :: Header
oneWithdrawal =
  header
    { hWithdrawalsRoot = fullRoot
    , hWithdrawalCount = 1
    , hTotalEventCount = 1
    , hTransitionStepCount = 1
    , hTransitionTraceRoot = fullRoot
    , hEventToStepRoot = fullRoot
    }

-- | One L2 transaction: an event that does run scripts, so it does count.
oneL2Tx :: Header
oneL2Tx =
  header
    { hTransactionsRoot = fullRoot
    , hL2TransactionCount = 1
    , hTotalEventCount = 1
    , hTransitionStepCount = 1
    , hValidationTraceCount = 1
    , hTransitionTraceRoot = fullRoot
    , hEventToStepRoot = fullRoot
    , hValidationTracesRoot = fullRoot
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

-- | A @ConfirmedState@ as six named fields.
data Confirmed = Confirmed
  { cHeaderHash :: BS.ByteString
  , cPrevHeaderHash :: BS.ByteString
  , cUtxoRoot :: BS.ByteString
  , cStartTime :: Integer
  , cEndTime :: Integer
  , cProtocolVersion :: Integer
  }

-- | The genesis sentinel at a chosen instant: every field pinned, times equal.
genesis :: Integer -> Confirmed
genesis t =
  Confirmed
    { cHeaderHash = BS.replicate 28 0x00
    , cPrevHeaderHash = BS.replicate 28 0x00
    , cUtxoRoot = emptyRoot
    , cStartTime = t
    , cEndTime = t
    , cProtocolVersion = 0
    }

-- | An ordinary confirmed state: a real header hash, and a covered interval.
ordinary :: Confirmed
ordinary =
  Confirmed
    { cHeaderHash = BS.replicate 28 0xaa
    , cPrevHeaderHash = BS.replicate 28 0xbb
    , cUtxoRoot = fullRoot
    , cStartTime = 500
    , cEndTime = 600
    , cProtocolVersion = 1
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
