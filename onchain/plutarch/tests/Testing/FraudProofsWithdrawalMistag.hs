{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsWithdrawalMistag (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.V3 (PTokenName (..))
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.NativeTx.Types (PMidgardTxOutput)
import Midgard.FraudProofs.WithdrawalMistag
import Midgard.LedgerOutputCommitment (pencodeLedgerOutputCommitment)
import Midgard.LedgerOutputDescriptor (pbuildV1)
import Midgard.LedgerState (PWithdrawalBody, PWithdrawalInfo)
import Midgard.TransitionTrace (PIndexedTraceProof, PRootMembershipProof)
import Testing.Eval (pfailsNoTraceWithoutHoistChecks, psucceedsNoTraceWithoutHoistChecks)
import Testing.FraudProofsFixture (
    blake2b256,
    commitCountedRoot,
    emptyProof,
    membershipProof,
    midgardOutputCbor,
    pubKeyAddressBytes,
    serialise,
    singleEntryPhasRoot,
 )

tests :: TestTree
tests =
    testGroup
        "Withdrawal Mistag Fraud Proof Tests"
        [ testGroup "step 01" step01Tests
        , testGroup "step 02" step02Tests
        , testGroup "step 03" step03Tests
        , testGroup "step 04" step04Tests
        , testGroup "step 05" step05Tests
        ]

psucceeds :: (forall s. Term s a) -> Assertion
psucceeds = psucceedsNoTraceWithoutHoistChecks

pfails :: (forall s. Term s a) -> Assertion
pfails = pfailsNoTraceWithoutHoistChecks

assertTerm :: forall s. Term s PBool -> Term s PUnit
assertTerm predicate = pif predicate (pconstant ()) perror

step01Tests :: [TestTree]
step01Tests =
    [ testCase "reserved_test_category_is_exact" $
        psucceeds $
            assertTerm $
                pwithdrawalMistagTestCategoryIdV1 #== phexByteStr "00000014"
    , testCase "accepts_own_category_and_header_suffix" $
        psucceeds $
            assertTerm $
                plet headerHash $ \hash ->
                    plet (pcomputationThreadAssetNameV1 # hash) $ \name ->
                        pcategoryIsWithdrawalMistagV1
                            # pdata name
                            #&& pchallengedHeaderHashOfV1
                            # pdata name
                            #== hash
    , testCase "rejects_wrong_category" $
        psucceeds $
            assertTerm $
                pnot #$ pcategoryIsWithdrawalMistagV1 # pdata (pcon $ PTokenName $ phexByteStr "0000000414141414141414141414141414141414141414141414141414141414")
    , testCase "rejects_wrong_category_length" $
        psucceeds $
            assertTerm $
                pnot #$ pcategoryIsWithdrawalMistagV1 # pdata (pcon $ PTokenName $ phexByteStr "00000014")
    ]

step02Tests :: [TestTree]
step02Tests =
    [ testCase "accepts_exact_counted_withdrawal_coordinate" $
        psucceeds $
            assertTerm $
                coordinate withdrawalPhase 1 eventRoot transitionRoot
    , testCase "rejects_non_withdrawal_phase" $
        psucceeds $
            assertTerm $
                pnot #$ coordinate depositPhase 1 depositEventRoot depositTransitionRoot
    , testCase "rejects_mutated_transition_count" $
        psucceeds $
            assertTerm $
                pnot #$ coordinate withdrawalPhase 2 eventRoot transitionRoot
    , testCase "rejects_mutated_event_root" $
        psucceeds $
            assertTerm $
                pnot #$ coordinate withdrawalPhase 1 (BS.replicate 32 0xff) transitionRoot
    ]

step03Tests :: [TestTree]
step03Tests =
    [ testCase "accepts_exact_owner_value_and_asset_count" $
        psucceeds $
            assertTerm $
                poutputCoreMatchesWithSignatureV1 # infoTerm 1_000_000 # outputTerm 1_000_000 owner # pconstant True
    , testCase "rejects_wrong_owner" $
        psucceeds $
            assertTerm $
                pnot
                    #$ poutputCoreMatchesWithSignatureV1
                    # infoTerm 1_000_000
                    # outputTerm 1_000_000 (BS.replicate 28 0x22)
                    # pconstant True
    , testCase "rejects_wrong_value" $
        psucceeds $
            assertTerm $
                pnot
                    #$ poutputCoreMatchesWithSignatureV1
                    # infoTerm 1_000_000
                    # outputTerm 999_999 owner
                    # pconstant True
    , testCase "classifies_authenticated_output_membership" $
        psucceeds $
            assertTerm presentClassification
    , testCase "accepts_authenticated_empty_ledger_nonmembership" $
        psucceeds $
            assertTerm absentClassification
    , testCase "rejects_substituted_withdrawal_opening" $
        pfails $
            pmatch
                (pclassifyLedgerEvidenceV1 (classificationState authenticInfo emptyLedgerRoot) (infoTerm 999_999) absentEvidence)
                (\_ -> pconstant @PUnit ())
    , testCase "rejects_wrong_outref_key" $
        psucceeds $
            assertTerm $
                ( pwithdrawalLedgerOutrefKeyV1
                    # pconstant @PData outref
                    #/= pwithdrawalLedgerOutrefKeyV1
                    # pconstant @PData (PD.Constr 0 [PD.B outrefTxId, PD.I 1])
                )
    ]

step04Tests :: [TestTree]
step04Tests =
    [ testCase "binds_parameterized_payability" $
        psucceeds $
            assertTerm $
                pnot #$ ppayoutIsExactlyPayableWithRateV1 # bodyTerm 1_000_000 defaultAddress noDatum # 5 # 1_000_000
    , testCase "accepts_unpayable_mistag" $
        psucceeds $
            assertTerm $
                pnot #$ ppayoutIsExactlyPayableV1 # bodyTerm 1 defaultAddress noDatum # 5
    , testCase "accepts_exact_minimum" $
        psucceeds $
            assertTerm $
                ppayoutIsExactlyPayableV1 # bodyTerm exactMinimum defaultAddress noDatum # 5
    , testCase "rejects_one_lovelace_below_exact_minimum" $
        psucceeds $
            assertTerm $
                pnot #$ ppayoutIsExactlyPayableV1 # bodyTerm (exactMinimum - 1) defaultAddress noDatum # 5
    , testCase "rejects_parameter_mutation" $
        psucceeds $
            assertTerm $
                pnot
                    # ( ppayoutIsExactlyPayableWithRateV1
                            # bodyTerm 1_000_000 defaultAddress noDatum
                            # 5
                            # 1
                            #== ppayoutIsExactlyPayableWithRateV1
                            # bodyTerm 1_000_000 defaultAddress noDatum
                            # 5
                            # 1_000_000
                      )
    , testCase "counts_inline_stake_and_datum_hash_bytes_exactly" $
        psucceeds $
            assertTerm $
                pexactPayoutOutputBytesV1 # bodyTerm 1_000_000 inlineStakeAddress datumHash # 5 #== 104
    , testCase "counts_pointer_address_words_exactly" $
        psucceeds $
            assertTerm $
                pexactPayoutOutputBytesV1 # bodyTerm 1_000_000 pointerAddress noDatum # 5 #== 45
    , testCase "counts_inline_datum_tag_and_bytes_exactly" $
        psucceeds $
            assertTerm $
                pexactPayoutOutputBytesV1 # bodyTerm 1_000_000 defaultAddress inlineIntegerDatum # 5 #== 47
    , testCase "counts_canonical_inline_map_datum_exactly" $
        psucceeds $
            assertTerm $
                pexactPayoutOutputBytesV1 # bodyTerm 1_000_000 defaultAddress inlineMapDatum # 5 #== 48
    , testCase "accepts_valid_withdrawal_marked_invalid" $
        psucceeds $
            assertTerm $
                pmatch (pestablishMistagV1 (step03State authenticBody False True True 5) (bodyTerm 1_000_000 defaultAddress noDatum)) $ \state ->
                    pnot
                        # pfromData (pstep04State'claimedValid state)
                        #&& pfromData (pstep04State'actualValid state)
    , testCase "accepts_unpayable_withdrawal_marked_valid" $
        psucceeds $
            assertTerm $
                pmatch (pestablishMistagV1 (step03State poorBody True True True 5) (bodyTerm 1 defaultAddress noDatum)) $ \state ->
                    pfromData (pstep04State'claimedValid state)
                        #&& pnot
                        # pfromData (pstep04State'actualValid state)
    , testCase "refuses_honestly_tagged_valid_withdrawal" $
        pfails $
            forceEstablished $
                pestablishMistagV1 (step03State authenticBody True True True 5) (bodyTerm 1_000_000 defaultAddress noDatum)
    , testCase "refuses_honestly_tagged_unpayable_withdrawal" $
        pfails $
            forceEstablished $
                pestablishMistagV1 (step03State poorBody False True True 5) (bodyTerm 1 defaultAddress noDatum)
    , testCase "refuses_substituted_body_opening" $
        pfails $
            forceEstablished $
                pestablishMistagV1 (step03State authenticBody False True True 5) (bodyTerm 999_999 defaultAddress noDatum)
    ]

step05Tests :: [TestTree]
step05Tests =
    [ testCase "accepts_exact_mistag_fault" $
        psucceeds $
            assertTerm $
                pmistagFaultIsEstablishedV1 # terminalState True False 42 1
    , testCase "rejects_valid_block" $
        psucceeds $
            assertTerm $
                pnot #$ pmistagFaultIsEstablishedV1 # terminalState True True 42 1
    , testCase "rejects_malformed_handoff" $
        psucceeds $
            assertTerm $
                pnot #$ pmistagFaultIsEstablishedV1 # terminalState False True 0 1
    , testCase "rejects_wrong_terminal" $
        psucceeds $
            assertTerm $
                pnot #$ pmistagFaultIsEstablishedV1 # terminalState False False 42 1
    ]

headerHash :: forall s. Term s PByteString
headerHash = phexByteStr "14141414141414141414141414141414141414141414141414141414"

withdrawalId :: PD.Data
withdrawalId = PD.Constr 0 [PD.B $ BS.replicate 32 0x01, PD.I 0]

withdrawalPhase, depositPhase :: PD.Data
withdrawalPhase = PD.Constr 0 []
depositPhase = PD.Constr 3 []

eventKey :: PD.Data
eventKey = PD.Constr 0 [withdrawalId]

eventValue :: PD.Data -> PD.Data
eventValue phase = PD.Constr 0 [PD.I 0, phase]

transitionValue :: PD.Data -> PD.Data
transitionValue phase =
    PD.Constr
        0
        [ PD.I 1
        , PD.I 0
        , eventKey
        , phase
        , PD.B $ BS.replicate 32 0x11
        , PD.B $ BS.replicate 32 0x22
        ]

eventProof :: PD.Data -> PD.Data
eventProof phase = membershipProof 5 root rawRoot 1 eventKey (eventValue phase)
  where
    rawRoot = singleEntryPhasRoot (serialise eventKey) (serialise $ eventValue phase)
    root = commitCountedRoot 5 rawRoot 1

transitionProof :: PD.Data -> PD.Data
transitionProof phase = membershipProof 4 root rawRoot 1 (PD.I 0) (transitionValue phase)
  where
    rawRoot = singleEntryPhasRoot (serialise $ PD.I 0) (serialise $ transitionValue phase)
    root = commitCountedRoot 4 rawRoot 1

eventRoot, transitionRoot, depositEventRoot, depositTransitionRoot :: BS.ByteString
eventRoot = rootOf $ eventProof withdrawalPhase
transitionRoot = rootOf $ transitionProof withdrawalPhase
depositEventRoot = rootOf $ eventProof depositPhase
depositTransitionRoot = rootOf $ transitionProof depositPhase

rootOf :: PD.Data -> BS.ByteString
rootOf (PD.Constr 0 (_ : PD.B root : _)) = root
rootOf _ = error "malformed root membership fixture"

coordinate :: forall s. PD.Data -> Integer -> BS.ByteString -> BS.ByteString -> Term s PBool
coordinate phase transitionCount expectedEventRoot expectedTransitionRoot =
    ptraceCoordinateIsExactV1 state eventMembership transitionMembership
  where
    state =
        pcon $
            PStep01State
                (pdata headerHash)
                (punsafeCoerce $ pconstant @PData withdrawalId)
                (pdata $ pconstant $ BS.replicate 32 0x33)
                (pdata $ pconstant True)
                (pdata $ pconstant expectedEventRoot)
                (pdata 1)
                (pdata $ pconstant expectedTransitionRoot)
                (pdata $ pconstant transitionCount)
    eventMembership =
        pfromData $ punsafeCoerce @(PAsData PRootMembershipProof) $ pconstant @PData $ eventProof phase
    transitionMembership =
        pfromData $ punsafeCoerce @(PAsData PIndexedTraceProof) $ pconstant @PData $ transitionProof phase

owner, outrefTxId :: BS.ByteString
owner = BS.replicate 28 0x11
outrefTxId = BS.replicate 32 0x01

outref :: PD.Data
outref = PD.Constr 0 [PD.B outrefTxId, PD.I 0]

withdrawalInfo :: Integer -> PD.Data
withdrawalInfo lovelace =
    PD.Constr
        0
        [ bodyData lovelace defaultAddress noDatum
        , PD.List [PD.B "", PD.B ""]
        , PD.Constr 0 []
        ]

authenticInfo :: PD.Data
authenticInfo = withdrawalInfo 1_000_000

infoTerm :: forall s. Integer -> Term s PWithdrawalInfo
infoTerm lovelace = pfromData $ punsafeCoerce $ pconstant @PData $ withdrawalInfo lovelace

outputData :: Integer -> BS.ByteString -> PD.Data
outputData lovelace paymentOwner =
    PD.Constr
        0
        [ PD.Constr
            0
            [ PD.Constr 0 []
            , PD.I 0
            , PD.Constr 0 [PD.B paymentOwner]
            , PD.Constr 1 []
            ]
        , PD.Constr 0 [PD.I lovelace, PD.Map []]
        , PD.Constr 1 []
        , PD.Constr 1 []
        ]

outputTerm :: forall s. Integer -> BS.ByteString -> Term s PMidgardTxOutput
outputTerm lovelace paymentOwner =
    pfromData $ punsafeCoerce $ pconstant @PData $ outputData lovelace paymentOwner

canonicalOutput :: BS.ByteString
canonicalOutput = midgardOutputCbor (pubKeyAddressBytes owner) 1_000_000 Nothing

descriptorBytes :: BS.ByteString
descriptorBytes =
    plift @PByteString $
        pmatch (pbuildV1 # 0 # pconstant canonicalOutput) $ \case
            PNothing -> perror
            PJust descriptor -> pencodeLedgerOutputCommitment # descriptor

ledgerKey :: BS.ByteString
ledgerKey = "\x82\x58\x20" <> outrefTxId <> "\x19\x00\x00"

ledgerRawRoot, ledgerRoot, emptyLedgerRoot :: BS.ByteString
ledgerRawRoot = singleEntryPhasRoot ledgerKey descriptorBytes
ledgerRoot = ledgerRawRoot
emptyLedgerRoot = blake2b256 ""

classificationState :: forall s. PD.Data -> BS.ByteString -> Term s PStep02State
classificationState info root =
    pcon $
        PStep02State
            (pdata headerHash)
            (punsafeCoerce $ pconstant @PData withdrawalId)
            (pdata $ pconstant $ blake2b256 $ serialise info)
            (pdata $ pconstant True)
            (pdata $ pconstant root)

presentEvidence :: forall s. Term s PWithdrawalLedgerEvidenceV1
presentEvidence =
    pcon $
        PPresentLedgerOutput
            (pdata $ pconstant canonicalOutput)
            (punsafeCoerce $ pconstant @PData emptyProof)

absentEvidence :: forall s. Term s PWithdrawalLedgerEvidenceV1
absentEvidence = pcon $ PAbsentLedgerOutput (punsafeCoerce $ pconstant @PData emptyProof)

presentClassification :: forall s. Term s PBool
presentClassification =
    pmatch (pclassifyLedgerEvidenceV1 (classificationState authenticInfo ledgerRoot) (infoTerm 1_000_000) presentEvidence) $ \state ->
        pfromData (pstep03State'outputPresent state)
            #&& pnot
            # pfromData (pstep03State'coreValid state)
            #&& pfromData (pstep03State'cardanoValueSize state)
            #> 0
            #&& pfromData (pstep03State'withdrawalBodyHash state)
            #== pconstant (blake2b256 $ serialise $ case authenticInfo of PD.Constr 0 (body : _) -> body; _ -> error "fixture")

absentClassification :: forall s. Term s PBool
absentClassification =
    pmatch (pclassifyLedgerEvidenceV1 (classificationState authenticInfo emptyLedgerRoot) (infoTerm 1_000_000) absentEvidence) $ \state ->
        pnot
            # pfromData (pstep03State'outputPresent state)
            #&& pnot
            # pfromData (pstep03State'coreValid state)
            #&& pfromData (pstep03State'cardanoValueSize state)
            #== 0

defaultAddress, inlineStakeAddress, pointerAddress, noDatum, datumHash, inlineIntegerDatum, inlineMapDatum :: PD.Data
defaultAddress = PD.Constr 0 [PD.Constr 0 [PD.B owner], PD.Constr 1 []]
inlineStakeAddress =
    PD.Constr
        0
        [ PD.Constr 0 [PD.B owner]
        , PD.Constr 0 [PD.Constr 0 [PD.Constr 0 [PD.B $ BS.replicate 28 0x22]]]
        ]
pointerAddress =
    PD.Constr
        0
        [ PD.Constr 0 [PD.B owner]
        , PD.Constr 0 [PD.Constr 1 [PD.I 127, PD.I 128, PD.I 16_384]]
        ]
noDatum = PD.Constr 0 []
datumHash = PD.Constr 1 [PD.B $ BS.replicate 32 0x33]
inlineIntegerDatum = PD.Constr 2 [PD.I 42]
inlineMapDatum = PD.Constr 2 [PD.Map [(PD.I 1, PD.I 2)]]

bodyData :: Integer -> PD.Data -> PD.Data -> PD.Data
bodyData lovelace address datum =
    PD.Constr
        0
        [ outref
        , PD.B owner
        , PD.Map [(PD.B "", PD.Map [(PD.B "", PD.I lovelace)])]
        , address
        , datum
        ]

bodyTerm :: forall s. Integer -> PD.Data -> PD.Data -> Term s PWithdrawalBody
bodyTerm lovelace address datum =
    pfromData $ punsafeCoerce $ pconstant @PData $ bodyData lovelace address datum

authenticBody, poorBody :: PD.Data
authenticBody = bodyData 1_000_000 defaultAddress noDatum
poorBody = bodyData 1 defaultAddress noDatum

exactMinimum :: Integer
exactMinimum = 4_310 * (160 + 39)

step03State :: forall s. PD.Data -> Bool -> Bool -> Bool -> Integer -> Term s PStep03State
step03State body claimed present core valueSize =
    pcon $
        PStep03State
            (pdata headerHash)
            (punsafeCoerce $ pconstant @PData withdrawalId)
            (pdata $ pconstant $ blake2b256 $ serialise body)
            (pdata $ pconstant claimed)
            (pdata $ pconstant present)
            (pdata $ pconstant core)
            (pdata $ pconstant valueSize)

forceEstablished :: forall s. Term s PStep04State -> Term s PUnit
forceEstablished state = pmatch state (\_ -> pconstant @PUnit ())

terminalState :: forall s. Bool -> Bool -> Integer -> Integer -> Term s PStep04State
terminalState claimed actual bytes required =
    pcon $
        PStep04State
            (pdata headerHash)
            (punsafeCoerce $ pconstant @PData withdrawalId)
            (pdata $ pconstant claimed)
            (pdata $ pconstant actual)
            (pdata $ pconstant bytes)
            (pdata $ pconstant required)
