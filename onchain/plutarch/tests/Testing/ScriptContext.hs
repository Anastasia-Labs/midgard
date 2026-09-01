{-# LANGUAGE OverloadedStrings #-}

module Testing.ScriptContext (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Core.Utils ((#/=), pand'List)
import Plutarch.Prelude
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins qualified as Builtins
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.BoundedItem qualified as Bounded
import Aiken.Cbor (pdeserialise)
import Midgard.CanonicalPlutusData (pisCanonicalPlutusDataV1)
import Midgard.CekData (
  PDataSequenceSummaryV1 (..), PDataSummaryV1 (..), pemptyDataListSummaryV1,
  pemptyDataPairSummaryV1, plistDataSummaryV1, pmapDataSummaryV1, psemanticDataSummaryV1,
 )
import Midgard.LedgerOutput (pdecodeCanonicalOutput)
import Midgard.LedgerOutputCommitment (
  PLedgerOutputCommitmentV1 (..), pencodeLedgerOutputCommitment, pledgerOutputCommitmentVersion,
 )
import Midgard.ScriptContext
import Midgard.ScriptProof qualified as Proof
import Midgard.ValidationMerkle (PFrontierPeak (..), pfrontierCommitment, phashBranch)
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests = testGroup "Midgard.ScriptContext"
  [ canonicalOutputCase "offchain_simple_output_vector_is_canonical" simpleOutputCbor
  , canonicalOutputCase "offchain_datum_output_vector_is_canonical" datumOutputCbor
  , canonicalOutputCase "offchain_reference_output_vector_is_canonical" referenceOutputCbor
  , canonicalOutputCase "offchain_output_vector_is_canonical" outputCbor
  , testCase "offchain_tx_out_semantic_roots_match" $ passertEvalNoTrace txOutSemanticRootsMatch
  , testCase "malformed_tx_out_fails_closed" $ passertEvalNoTrace malformedTxOutFailsClosed
  , testCase "offchain_tx_in_info_semantic_roots_match" $ passertEvalNoTrace txInInfoSemanticRootsMatch
  , testCase "authenticated_item_is_prepended_in_reverse_index_order" $ passertEvalNoTrace authenticatedItemPrepends
  , testCase "raw_resolved_input_is_converted_and_prepended_in_one_bounded_step" $ passertEvalNoTrace resolvedInputPrepends
  , testCase "authenticated_output_is_converted_and_prepended_in_one_bounded_step" $ passertEvalNoTrace authenticatedOutputPrepends
  , testCase "observer_context_collections_are_not_capped_at_16" $ passertEvalNoTrace observerCollectionsExceed16
  , testCase "composed_transaction_info_and_context_match_direct_plutus_data" $ passertEvalNoTrace composedContextMatchesData
  , testCase "authenticated_descriptor_drives_spend_and_reference_context_items" $ passertEvalNoTrace descriptorDrivesInputs
  , testCase "complete_output_cbor_cannot_substitute_for_a_resolved_descriptor" $ pfails rawOutputIsNotDescriptor
  , testCase "output_descriptor_membership_selects_exact_maximum_semantic_summary" $ passertEvalNoTrace maximumDescriptorSummary
  , testCase "maximum_signer_frontier_item_fits_one_step" $ passertEvalNoTrace maximumSignerFits
  , testGroup "datum_predicates_agree_on_ordinary_datums" $
      map ordinaryDatumCase ordinaryDatums
  , testGroup "datum_predicates_agree_on_every_non_canonical_form" $
      map noncanonicalDatumCase noncanonicalDatums
  , testCase "unknown_tags_are_refused_without_materialising_them" $ passertEvalNoTrace unknownTagRefused
  , testGroup "datum_predicates_diverge_on_high_constructor_alternatives" $
      map highAlternativeDiverges highAlternativeDatums
  , testGroup "high_alternative_datums_are_still_declined_by_this_gate" $
      map highAlternativeGateDeclines [head highAlternativeDatums, last highAlternativeDatums]
  , testCase "bignum_datums_are_canonical_and_reach_the_output_decoder" $ passertEvalNoTrace bignumsReachOutputDecoder
  , testCase "outputs_carrying_l1_acceptable_datums_now_decode" $
      passertEvalNoTrace outputsCarryingL1AcceptableDatumsNowDecode
  , testCase "outputs_carrying_non_canonical_datums_still_reject" $
      passertEvalNoTrace outputsCarryingNonCanonicalDatumsStillReject
  , testGroup "bignum_datums_are_declined_by_the_materialisation_path" $
      map bignumMaterialisationDeclines bignumDatums
  ]

canonicalOutputCase :: String -> BS.ByteString -> TestTree
canonicalOutputCase name output = testCase name $ passertEvalNoTrace $
  pisJust $ pdecodeCanonicalOutput # pconstant output

txOutSemanticRootsMatch :: forall s. Term s PBool
txOutSemanticRootsMatch =
  pmatch (ptxOutSummaryV1 # bytes outputCbor # pconstant False) $ \case
    PNothing -> pconstant False
    PJust cardano -> pmatch (ptxOutSummaryV1 # bytes outputCbor # pconstant True) $ \case
      PNothing -> pconstant False
      PJust midgard -> pmatch cardano $ \c -> pmatch midgard $ \m ->
        pfromData (psummary'root c) #== bytes (hex "e1649a619efea4d8319405dae3455e5add966785c25f269e83fd0f15352693a7")
          #&& pfromData (psummary'cborLength c) #== 133
          #&& pfromData (psummary'memory c) #== 169
          #&& pfromData (psummary'root m) #== bytes (hex "65951e5653516caf350b2cd750bab349fbd1a48811a4c278859dd825868c2d32")
          #&& pfromData (psummary'cborLength m) #== 133
          #&& pfromData (psummary'memory m) #== 169

malformedTxOutFailsClosed :: forall s. Term s PBool
malformedTxOutFailsClosed = pisNothing $ ptxOutSummaryV1 # bytes (hex "a0") # pconstant False

txInInfoSemanticRootsMatch :: forall s. Term s PBool
txInInfoSemanticRootsMatch =
  pmatch (ptxInInfoSummaryV1 # bytes inputCbor # bytes outputCbor # pconstant False) $ \case
    PNothing -> pconstant False
    PJust cardano -> pmatch (ptxInInfoSummaryV1 # bytes inputCbor # bytes outputCbor # pconstant True) $ \case
      PNothing -> pconstant False
      PJust midgard -> pmatch cardano $ \c -> pmatch midgard $ \m ->
        pfromData (psummary'root c) #== bytes (hex "66b39a3f329165f6ab15f249df38d5e8bc99230853ce5c16976b4780af1ed029")
          #&& pfromData (psummary'cborLength c) #== 176
          #&& pfromData (psummary'memory c) #== 218
          #&& pfromData (psummary'root m) #== bytes (hex "097c6693964ba9a7448e88f4e73fcaff44b7e41663d4cd3ba2b52ff3afbc3f8f")
          #&& pfromData (psummary'cborLength m) #== 176
          #&& pfromData (psummary'memory m) #== 218

authenticatedItemPrepends :: forall s. Term s PBool
authenticatedItemPrepends = plet pcardanoInputSummary $ \item -> pmatch item $ \i ->
  plet (Proof.pcontextItemLeafHash # 0 # 0 # pfromData (psummary'root i)
      # pfromData (psummary'cborLength i) # pfromData (psummary'memory i)) $ \leaf ->
  plet (pexpectJust $ pprependAuthenticatedListItemV1 # 0 # 1 # psinglePeak leaf # 0 # item
      # pnil # pemptyDataListSummaryV1) $ \sequence ->
  plet (plistDataSummaryV1 # sequence) $ \listSummary -> pmatch sequence $ \s -> pmatch listSummary $ \l ->
    leaf #== bytes (hex "d2b8985450e215893013635f7403ff16d8ee18a991747ca8428bfcf2ddb22546")
      #&& pfromData (pseq'root s) #== bytes (hex "f5df83aaebfb31fabe0c3758026af9b87655bdfe0b07fe4755a47e8dd345cbca")
      #&& pfromData (pseq'length s) #== 1
      #&& pfromData (pseq'payloadCborLength s) #== 176
      #&& pfromData (pseq'memory s) #== 218
      #&& pfromData (psummary'root l) #== bytes (hex "7f0329cc3ab6a71c6452a353ca7328124ef675be5ec1f035856943453baa732e")
      #&& pfromData (psummary'cborLength l) #== 178
      #&& pfromData (psummary'memory l) #== 222

resolvedInputPrepends :: forall s. Term s PBool
resolvedInputPrepends =
  plet (Proof.presolvedContextItemLeafHash # 0 # 0 # bytes inputCbor # bytes outputCbor) $ \leaf ->
  plet (pexpectJust $ pprependResolvedTxInInfoV1 # 1 # psinglePeak leaf # 1 # 0 # 0
      # bytes inputCbor # bytes outputCbor # pnil # pconstant False # pemptyDataListSummaryV1) $ \sequence ->
  plet (plistDataSummaryV1 # sequence) $ \listSummary -> pmatch sequence $ \s -> pmatch listSummary $ \l ->
    pfromData (pseq'root s) #== bytes (hex "f5df83aaebfb31fabe0c3758026af9b87655bdfe0b07fe4755a47e8dd345cbca")
      #&& pfromData (psummary'root l) #== bytes (hex "7f0329cc3ab6a71c6452a353ca7328124ef675be5ec1f035856943453baa732e")
      #&& pisNothing (pprependResolvedTxInInfoV1 # 1 # psinglePeak leaf # 1 # 1 # 0
        # bytes inputCbor # bytes outputCbor # pnil # pconstant False # pemptyDataListSummaryV1)

authenticatedOutputPrepends :: forall s. Term s PBool
authenticatedOutputPrepends = plet (Proof.poutputLeafHash # 0 # bytes outputCbor) $ \leaf ->
  plet (pexpectJust $ pprependOutputV1 # 1 # psinglePeak leaf # 0 # bytes outputCbor
      # pnil # pconstant True # pemptyDataListSummaryV1) $ \sequence ->
  plet (plistDataSummaryV1 # sequence) $ \item -> pmatch sequence $ \s -> pmatch item $ \i ->
    pfromData (pseq'length s) #== 1
      #&& pfromData (pseq'payloadCborLength s) #== 133
      #&& pfromData (pseq'memory s) #== 169
      #&& pfromData (psummary'cborLength i) #== 135
      #&& pfromData (psummary'memory i) #== 173

observerCollectionsExceed16 :: forall s. Term s PBool
observerCollectionsExceed16 =
  pmatch (pobserverCollectionSummaryV1 # pconstant observerHashes # pconstant False) $ \case
    PNothing -> pconstant False
    PJust cardano -> pmatch (pobserverCollectionSummaryV1 # pconstant observerHashes # pconstant True) $ \case
      PNothing -> pconstant False
      PJust midgard -> pmatch cardano $ \c -> pmatch midgard $ \m ->
        pfromData (psummary'cborLength c) #> pfromData (psummary'cborLength m)
          #&& pfromData (psummary'cborLength m) #> 400

composedContextMatchesData :: forall s. Term s PBool
composedContextMatchesData =
  plet pemptyDataListSummaryV1 $ \emptyItems ->
  plet pemptyDataPairSummaryV1 $ \emptyPairs ->
  plet (pmapDataSummaryV1 # emptyPairs) $ \emptyMap ->
  plet (plistDataSummaryV1 # emptyItems) $ \emptyList ->
  plet (ptxInfoSummaryV1 # pconstant False # emptyItems # emptyItems # emptyItems # 0 # (-1) # (-1)
      # emptyMap # emptyItems # emptyMap # emptyPairs # bytes txId) $ \cardano ->
  plet (ptxInfoSummaryV1 # pconstant True # emptyItems # emptyItems # emptyItems # 0 # (-1) # (-1)
      # emptyList # emptyItems # emptyMap # emptyPairs # bytes txId) $ \midgard ->
  plet (psemanticDataSummaryV1 # pconstant @PData (PD.I 0)) $ \redeemer ->
  pmatch (pscriptPurposeSummaryV1 # 1 # bytes scriptHash # pconstant "" # pconstant False) $ \case
    PNothing -> pconstant False
    PJust scriptInfo ->
      cardano #== psemanticDataSummaryV1 # pconstant @PData directCardanoTxInfo
        #&& midgard #== psemanticDataSummaryV1 # pconstant @PData directMidgardTxInfo
        #&& pscriptContextSummaryV1 # cardano # redeemer # scriptInfo
          #== psemanticDataSummaryV1 # pconstant @PData directScriptContext

descriptorDrivesInputs :: forall s. Term s PBool
descriptorDrivesInputs = plet (pexactDescriptor 2 outputCbor) $ \descriptorCbor ->
  plet (Proof.presolvedContextItemLeafHash # 0 # 0 # bytes inputCbor # descriptorCbor) $ \spendLeaf ->
  plet (Proof.presolvedContextItemLeafHash # 1 # 0 # bytes inputCbor # descriptorCbor) $ \referenceLeaf ->
  plet (pexpectJust $ pprependResolvedDescriptorTxInInfoV1 # 1 # psinglePeak spendLeaf # 1 # 0 # 0
      # bytes inputCbor # descriptorCbor # pnil # pconstant False # pemptyDataListSummaryV1) $ \spend ->
  plet (pexpectJust $ pprependResolvedDescriptorTxInInfoV1 # 1 # psinglePeak referenceLeaf # 0 # 1 # 0
      # bytes inputCbor # descriptorCbor # pnil # pconstant True # pemptyDataListSummaryV1) $ \reference ->
  pmatch spend $ \s -> pmatch reference $ \r ->
    pfromData (pseq'length s) #== 1
      #&& pfromData (pseq'length r) #== 1
      #&& pfromData (pseq'root s) #/= pfromData (pseq'root r)
      #&& pisNothing (pprependResolvedDescriptorTxInInfoV1 # 1 # psinglePeak spendLeaf # 1 # 0 # 0
        # bytes inputCbor # pexactDescriptor 2 simpleOutputCbor # pnil # pconstant False # pemptyDataListSummaryV1)

rawOutputIsNotDescriptor :: forall s. Term s PBool
rawOutputIsNotDescriptor = plet (Proof.presolvedContextItemLeafHash # 0 # 0 # bytes inputCbor # bytes outputCbor) $ \leaf ->
  pisJust $ pprependResolvedDescriptorTxInInfoV1 # 1 # psinglePeak leaf # 1 # 0 # 0
    # bytes inputCbor # bytes outputCbor # pnil # pconstant False # pemptyDataListSummaryV1

maximumDescriptorSummary :: forall s. Term s PBool
maximumDescriptorSummary = plet pmaximumDescriptor $ \descriptorCbor ->
  plet (Proof.poutputDescriptorLeafHash # 0 # descriptorCbor) $ \leaf ->
  plet (pexpectJust $ pprependOutputDescriptorV1 # 1 # psinglePeak leaf # 0 # descriptorCbor
      # pnil # pconstant False # pemptyDataListSummaryV1) $ \output ->
  plet (pexpectJust $ pcardanoSpendScriptInfoFromDescriptorV1 # bytes maximumInputCbor # descriptorCbor) $ \spend ->
  pmatch output $ \o -> pmatch spend $ \s ->
    pfromData (pseq'length o) #== 1
      #&& pfromData (pseq'payloadCborLength o) #== 16_384
      #&& pfromData (pseq'memory o) #== 16_384
      #&& plengthBS # pfromData (psummary'root s) #== 32
      #&& pfromData (psummary'root s) #/= bytes (BS.replicate 32 0xaa)

maximumSignerFits :: forall s. Term s PBool
maximumSignerFits = plet (Proof.psignerLeafHash # bytes signerHash) $ \leaf ->
  plet (prightmostPeakRoot leaf 6) $ \root ->
  plet (pcons # pdata (pcon $ PFrontierPeak (pdata 6) (pdata root)) # pnil) $ \peaks ->
  plet (pexpectJust $ pprependSignerV1 # 64 # (pfrontierCommitment # 64 # peaks) # peaks # 63
      # bytes signerHash # psixSiblings # pemptyDataListSummaryV1) $ \sequence -> pmatch sequence $ \s ->
    pfromData (pseq'length s) #== 1

ordinaryDatumCase :: BS.ByteString -> TestTree
ordinaryDatumCase datum = testCase (show $ Base16.encode datum) $ passertEvalNoTrace $
  pisCanonicalPlutusDataV1 # bytes datum
    #&& pretiredRoundTripPin (bytes datum)
    #&& pdatumGateAdmits datum

noncanonicalDatumCase :: BS.ByteString -> TestTree
noncanonicalDatumCase datum = testCase (show $ Base16.encode datum) $ passertEvalNoTrace $
  pnot # (pisCanonicalPlutusDataV1 # bytes datum)
    #&& pnot # pretiredRoundTripPin (bytes datum)
    #&& pnot # pdatumGateAdmits datum

unknownTagRefused :: forall s. Term s PBool
unknownTagRefused = pnot # (pisCanonicalPlutusDataV1 # bytes unknownTagDatum)
  #&& pnot # pdatumGateAdmits unknownTagDatum

highAlternativeDiverges :: BS.ByteString -> TestTree
highAlternativeDiverges datum = testCase (show $ Base16.encode datum) $ passertEvalNoTrace $
  pisCanonicalPlutusDataV1 # bytes datum #&& pnot # pretiredRoundTripPin (bytes datum)

highAlternativeGateDeclines :: BS.ByteString -> TestTree
highAlternativeGateDeclines datum = testCase (show $ Base16.encode datum) $ passertEvalNoTrace $
  pisCanonicalPlutusDataV1 # bytes datum #&& pnot # pdatumGateAdmits datum

bignumsReachOutputDecoder :: forall s. Term s PBool
bignumsReachOutputDecoder = pisCanonicalPlutusDataV1 # bytes (head bignumDatums)
  #&& pisCanonicalPlutusDataV1 # bytes (last bignumDatums)
  #&& pisJust (pdecodeCanonicalOutput # bytes (outputWithDatum $ head bignumDatums))
  #&& pisJust (pdecodeCanonicalOutput # bytes (outputWithDatum $ last bignumDatums))

outputsCarryingL1AcceptableDatumsNowDecode :: forall s. Term s PBool
outputsCarryingL1AcceptableDatumsNowDecode = pand'List
  [ pisJust $ pdecodeCanonicalOutput # bytes (outputWithDatum datum)
  | datum <- l1AcceptableOutputDatums
  ]

outputsCarryingNonCanonicalDatumsStillReject :: forall s. Term s PBool
outputsCarryingNonCanonicalDatumsStillReject = pand'List
  [ pisNothing $ pdecodeCanonicalOutput # bytes (outputWithDatum datum)
  | datum <- rejectedOutputDatums
  ]

bignumMaterialisationDeclines :: BS.ByteString -> TestTree
bignumMaterialisationDeclines datum = testCase (show $ Base16.encode datum) $ passertEvalNoTrace $
  pisCanonicalPlutusDataV1 # bytes datum
    #&& pnot # pdatumGateAdmits datum
    #&& pisNothing (ptxOutDataV1 # bytes (outputWithDatum datum) # pconstant False)
    #&& pisNothing (ptxOutDataV1 # bytes (outputWithDatum datum) # pconstant True)

pretiredRoundTripPin :: forall s. Term s PByteString -> Term s PBool
pretiredRoundTripPin datumCbor = pmatch (pdeserialise # datumCbor) $ \case
  PNothing -> pconstant False
  PJust datum -> pserialiseData # datum #== datumCbor

pdatumGateAdmits :: forall s. BS.ByteString -> Term s PBool
pdatumGateAdmits datum = pisJust $ pspendDatumSummaryV1 # bytes (outputWithDatum datum)

pexactDescriptor :: forall s. Integer -> BS.ByteString -> Term s PByteString
pexactDescriptor outputIndex rawOutput =
  plet (pexpectJust $ ptxOutSummaryV1 # bytes rawOutput # pconstant False) $ \cardano ->
  plet (pexpectJust $ ptxOutSummaryV1 # bytes rawOutput # pconstant True) $ \midgard ->
  plet (pexpectJust $ pspendDatumSummaryV1 # bytes rawOutput) $ \spend ->
    pencodeLedgerOutputCommitment # pcon (PLedgerOutputCommitmentV1
      (pdata pledgerOutputCommitmentVersion) (pdata $ pconstant outputIndex) (pdata $ plengthBS # bytes rawOutput)
      (pdata $ Bounded.pfromBytes # 2 # pconstant outputIndex # bytes rawOutput)
      (pdata $ bytes $ hex "68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      (pdata 1_234_567) (pdata 0) (pdata $ pfrontierCommitment # 0 # pnil) (pdata 0)
      (pdata $ -1) (pdata $ pconstant "") (pdata 0) (pdata $ pconstant "")
      (pdata cardano) (pdata midgard) (pdata spend))

pmaximumDescriptor :: forall s. Term s PByteString
pmaximumDescriptor = plet (pcon $ PDataSummaryV1 (pdata $ bytes $ BS.replicate 32 0xaa)
    (pdata 16_384) (pdata 16_384)) $ \summary ->
  pencodeLedgerOutputCommitment # pcon (PLedgerOutputCommitmentV1
    (pdata pledgerOutputCommitmentVersion) (pdata 0) (pdata 16_384)
    (pdata $ bytes $ BS.replicate 32 0xbb)
    (pdata $ bytes $ hex "68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    (pdata 18_446_744_073_709_551_615) (pdata 0) (pdata $ pfrontierCommitment # 0 # pnil) (pdata 5_000)
    (pdata $ -1) (pdata $ pconstant "") (pdata 0) (pdata $ pconstant "")
    (pdata summary)
    (pdata $ pcon $ PDataSummaryV1 (pdata $ bytes $ BS.replicate 32 0xbb) (pdata 16_384) (pdata 16_384))
    (pdata $ pcon $ PDataSummaryV1 (pdata $ bytes $ BS.replicate 32 0xcc) (pdata 16_384) (pdata 16_384)))

prightmostPeakRoot :: forall s. Term s PByteString -> Int -> Term s PByteString
prightmostPeakRoot current 0 = current
prightmostPeakRoot current height = prightmostPeakRoot (phashBranch # bytes siblingHash # current) (height - 1)

psixSiblings :: forall s. Term s (PBuiltinList (PAsData PByteString))
psixSiblings = foldr (\_ rest -> pcons # pdata (bytes siblingHash) # rest) pnil [1 .. (6 :: Int)]

pcardanoInputSummary :: forall s. Term s PDataSummaryV1
pcardanoInputSummary = pcon $ PDataSummaryV1
  (pdata $ bytes $ hex "66b39a3f329165f6ab15f249df38d5e8bc99230853ce5c16976b4780af1ed029")
  (pdata 176) (pdata 218)

psinglePeak :: forall s. Term s PByteString -> Term s (PBuiltinList (PAsData PFrontierPeak))
psinglePeak root = pcons # pdata (pcon $ PFrontierPeak (pdata 0) (pdata root)) # pnil

pexpectJust :: forall s a. Term s (PMaybe a) -> Term s a
pexpectJust value = pmatch value $ \case PNothing -> perror; PJust result -> result

pisJust :: forall s a. Term s (PMaybe a) -> Term s PBool
pisJust value = pmatch value $ \case PNothing -> pconstant False; PJust _ -> pconstant True

pisNothing :: forall s a. Term s (PMaybe a) -> Term s PBool
pisNothing value = pmatch value $ \case PNothing -> pconstant True; PJust _ -> pconstant False

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient

outputCbor, simpleOutputCbor, datumOutputCbor, referenceOutputCbor, inputCbor :: BS.ByteString
outputCbor = hex "a400581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a0012d687a1581c11111111111111111111111111111111111111111111111111111111a1422233070246d87b9f182aff03820343010203"
simpleOutputCbor = hex "a200581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a0012d687a1581c11111111111111111111111111111111111111111111111111111111a142223307"
datumOutputCbor = hex "a300581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a0012d687a1581c11111111111111111111111111111111111111111111111111111111a1422233070246d87b9f182aff"
referenceOutputCbor = hex "a300581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a0012d687a1581c11111111111111111111111111111111111111111111111111111111a14222330703820343010203"
inputCbor = hex "825820bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb190002"

observerHashes :: [BS.ByteString]
observerHashes = [BS.replicate 27 0 <> BS.singleton n | n <- [0 .. 16]]

txId, scriptHash :: BS.ByteString
txId = BS.replicate 32 0xaa
scriptHash = BS.replicate 28 0x11

signerHash, siblingHash, maximumInputCbor :: BS.ByteString
signerHash = BS.replicate 28 0x11
siblingHash = BS.replicate 32 0xaa
maximumInputCbor = hex "825820bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb190000"

emptyListData, emptyMapData, noneData, validityData :: PD.Data
emptyListData = PD.List []
emptyMapData = PD.Map []
noneData = PD.Constr 1 []
validityData = PD.Constr 0
  [ PD.Constr 0 [PD.Constr 0 [], PD.Constr 1 []]
  , PD.Constr 0 [PD.Constr 0 [], PD.Constr 0 []]
  ]

directCardanoTxInfo, directMidgardTxInfo, directScriptContext :: PD.Data
directCardanoTxInfo = PD.Constr 0
  [ emptyListData, emptyListData, emptyListData, PD.I 0, emptyMapData, emptyListData,
    emptyMapData, validityData, emptyListData, emptyMapData, emptyMapData, PD.B txId,
    emptyMapData, emptyListData, noneData, noneData
  ]
directMidgardTxInfo = PD.Constr 0
  [ emptyListData, emptyListData, emptyListData, PD.I 0, validityData, emptyListData,
    emptyListData, emptyMapData, emptyMapData, PD.B txId
  ]
directScriptContext = PD.Constr 0
  [ directCardanoTxInfo, PD.I 0, PD.Constr 0 [PD.B scriptHash] ]

ordinaryDatums, noncanonicalDatums, highAlternativeDatums, bignumDatums :: [BS.ByteString]
ordinaryDatums = map hex
  [ "00", "1818", "20", "4401020304", "80", "9f0001ff", "a0", "a10102",
    "d87980", "d87f9f00ff"
  ] <> [serialisedOf $ PD.Constr 127 [PD.B $ hex "ff"]]
noncanonicalDatums = map hex
  [ "1817", "8100", "9fff", "5fff", "d8798100", "bf0000ff", "6161", "f5",
    "0000", "18", ""
  ]
highAlternativeDatums = map hex ["d86682188080", "d8668218809f00ff", "d8668219ffff80"]
bignumDatums = map hex ["c249010000000000000000", "c349010000000000000000"]

l1AcceptableOutputDatums, rejectedOutputDatums :: [BS.ByteString]
l1AcceptableOutputDatums =
  [ hex "c249010000000000000000"
  , hex "c349010000000000000000"
  , serialisedOf $ PD.Constr 128 []
  , serialisedOf $ PD.I 18_446_744_073_709_551_616
  , hex "d87980"
  , hex "00"
  ]
rejectedOutputDatums = map hex ["1817", "8100", "c24101", "d866820080"]

unknownTagDatum :: BS.ByteString
unknownTagDatum = hex "c11a514b67b0"

outputWithDatum :: BS.ByteString -> BS.ByteString
outputWithDatum datum = BS.singleton 0xa3 <> BS.drop 1 simpleOutputCbor <> BS.singleton 0x02
  <> encodeBytes datum

encodeBytes :: BS.ByteString -> BS.ByteString
encodeBytes value
  | BS.length value <= 23 = BS.singleton (fromIntegral $ 0x40 + BS.length value) <> value
  | BS.length value <= 255 = BS.pack [0x58, fromIntegral $ BS.length value] <> value
  | otherwise = error "test datum fixture exceeds one-byte CBOR length"

serialisedOf :: PD.Data -> BS.ByteString
serialisedOf = Builtins.fromBuiltin . Builtins.serialiseData . Builtins.dataToBuiltinData
