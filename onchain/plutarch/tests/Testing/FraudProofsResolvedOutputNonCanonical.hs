{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsResolvedOutputNonCanonical (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import PlutusLedgerApi.V3 (TxId (..), TxOutRef (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.BoundedItem qualified as Bounded
import Midgard.CekData (PDataSummaryV1 (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.ResolvedOutputNonCanonical
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), pledgerOutputCommitmentVersion, poutputFieldIndex, poutputItemCommitment)
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Midgard.ValidationMerkle qualified as Merkle
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests =
  testGroup
    "Resolved-output non-canonical rule"
    [ testCase "resolved_output_non_canonical_convicts_wrongful_acceptance" $ passertEvalNoTrace $ pterminalContradictionV1 # verdict acceptedSubject (pconstant True)
    , testCase "resolved_output_non_canonical_convicts_wrongful_rejection" $ passertEvalNoTrace $ pterminalContradictionV1 # verdict (rejectedSubject 1 4) (pconstant False)
    , testCase "resolved_output_non_canonical_refuses_honest_acceptance" $ pfails $ requireTrue $ pterminalContradictionV1 # verdict acceptedSubject (pconstant False)
    , testCase "resolved_output_non_canonical_refuses_honest_rejection" $ pfails $ requireTrue $ pterminalContradictionV1 # verdict (rejectedSubject 0 2) (pconstant True)
    , testCase "resolved_output_non_canonical_binds_exact_reason_coordinate" $ passertEvalNoTrace exactReasonCoordinate
    , testCase "resolved_output_non_canonical_binds_spend_source_reason" $ passertEvalNoTrace $ boundSourceKind (pbindInputV1 # rejectedSubject 0 0 # 0 # 0 # priorRoot) #== 0
    , testCase "resolved_output_non_canonical_refuses_source_substitution" $ pfails $ boundSourceKind (pbindInputV1 # rejectedSubject 1 4 # 0 # 4 # priorRoot) #== 0
    , testCase "resolved_output_non_canonical_refuses_index_substitution" $ pfails $ boundInputIndex (pbindInputV1 # rejectedSubject 1 4 # 1 # 3 # priorRoot) #== 3
    , testCase "resolved_output_non_canonical_refuses_out_of_range_source" $ pfails $ boundSourceKind (pbindInputV1 # acceptedSubject # 2 # 0 # priorRoot) #== 2
    , testCase "resolved_output_non_canonical_refuses_negative_index" $ pfails $ boundInputIndex (pbindInputV1 # acceptedSubject # 0 # (-1) # priorRoot) #== (-1)
    , testCase "resolved_output_non_canonical_refuses_other_reason" $ pfails $ boundInputIndex (pbindInputV1 # otherReasonSubject # 1 # 4 # priorRoot) #== 4
    , testCase "resolved_output_non_canonical_refuses_prior_root_substitution" $ pfails $ boundInputIndex (pbindInputV1 # acceptedSubject # 0 # 0 # phexByteStr "00") #== 0
    , testCase "resolved_output_non_canonical_authenticates_exact_out_ref" $ passertEvalNoTrace authenticatesExactOutRef
    , testCase "resolved_output_non_canonical_reconstructs_canonical_output_to_exact_end" $ passertEvalNoTrace reconstructsCanonicalOutput
    , testCase "resolved_output_non_canonical_refuses_premature_finalize" $ passertEvalNoTrace refusesPrematureFinalize
    , testCase "resolved_output_non_canonical_refuses_advance_at_finishable_control" $ pfails $ isNothing $ advance canonicalOutput (walkValue canonicalOutput)
    , testCase "resolved_output_non_canonical_trailing_byte_is_non_canonical" $ passertEvalNoTrace trailingByteIsNonCanonical
    , testCase "resolved_output_non_canonical_malformed_address_is_non_canonical" $ passertEvalNoTrace $ isNothing $ advance malformedAddress Scan.pinitialControlV1
    , testCase "resolved_output_non_canonical_refuses_substituted_chunk_bytes" $ pfails $ isNothing $ padvanceReconstructionV1 # descriptor 0 canonicalOutput # Scan.pinitialControlV1 # singleChunkProof 0 substitutedOutput # pcon PDNothing
    , testCase "resolved_output_non_canonical_refuses_substituted_descriptor" $ pfails $ isNothing $ padvanceReconstructionV1 # descriptor 1 canonicalOutput # Scan.pinitialControlV1 # singleChunkProof 0 canonicalOutput # pcon PDNothing
    , testCase "resolved_output_non_canonical_refuses_chunk_outside_checkpoint" $ pfails $ isNothing $ advance canonicalOutput cursorAtChunkBoundary
    , testCase "resolved_output_non_canonical_refuses_unexpected_next_chunk" $ pfails $ isNothing $ padvanceReconstructionV1 # descriptor 0 canonicalOutput # Scan.pinitialControlV1 # singleChunkProof 0 canonicalOutput # (pcon $ PDJust $ pdata $ singleChunkProof 0 canonicalOutput)
    , testCase "resolved_output_non_canonical_advances_across_chunk_boundary" $ passertEvalNoTrace advancesAcrossChunkBoundary
    , testCase "resolved_output_non_canonical_multi_chunk_requires_next_chunk" $ pfails $ isNothing $ padvanceReconstructionV1 # descriptor 0 longOutput # Scan.pinitialControlV1 # firstLongChunkProof # pcon PDNothing
    , testCase "resolved_output_non_canonical_refuses_substituted_next_chunk" $ pfails $ isNothing $ padvanceReconstructionV1 # descriptor 0 longOutput # Scan.pinitialControlV1 # firstLongChunkProof # (pcon $ PDJust $ pdata substitutedSecondLongChunkProof)
    , testCase "resolved_output_non_canonical_successor_is_decided_by_the_engine" $ passertEvalNoTrace successorIsDecidedByEngine
    ]

canonicalBytes, malformedAddressBytes, longBytes :: BS.ByteString
canonicalBytes = Base16.decodeLenient "a200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"
malformedAddressBytes = Base16.decodeLenient "a200581c111111111111111111111111111111111111111111111111111101821a004c4b40a0"
longBytes = canonicalBytes <> BS.replicate 5000 0

canonicalOutput, malformedAddress, substitutedOutput, longOutput :: forall s. Term s PByteString
canonicalOutput = pconstant canonicalBytes
malformedAddress = pconstant malformedAddressBytes
substitutedOutput = pconstant $ BS.cons 0xa3 $ BS.drop 1 canonicalBytes
longOutput = pconstant longBytes

priorRoot :: forall s. Term s PByteString
priorRoot = phexByteStr "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

acceptedSubject :: forall s. Term s Subject.PVerdictSubject
acceptedSubject = subject 0 0 (pconstant "") (pcon PDNothing)

rejectedSubject :: forall s. Integer -> Integer -> Term s Subject.PVerdictSubject
rejectedSubject sourceKind inputIndex =
  subject
    1
    1
    (phexByteStr "01")
    (pcon $ PDJust $ pdata $ pcon $ PInputSpentOutputNonCanonical (pdata $ pconstant sourceKind) (pdata $ pconstant inputIndex))

otherReasonSubject :: forall s. Term s Subject.PVerdictSubject
otherReasonSubject =
  subject
    1
    1
    (phexByteStr "01")
    (pcon $ PDJust $ pdata $ pcon $ PInputNotFound (pdata 1) (pdata 4))

subject :: forall s. Integer -> Integer -> Term s PByteString -> Term s (PMaybeData PRejectionReasonV1) -> Term s Subject.PVerdictSubject
subject direction sourceKind sourceKey reason =
  pcon $
    Subject.PVerdictSubject
      (pdata 1)
      (pdata $ pconstant direction)
      (pdata $ pconstant sourceKind)
      (pdata $ phexByteStr "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
      (pdata sourceKey)
      (pdata reason)

verdict :: forall s. Term s Subject.PVerdictSubject -> Term s PBool -> Term s PCanonicalVerdictV1
verdict ruleSubject nonCanonical = pcon $ PCanonicalVerdictV1 (pdata ruleSubject) (pdata nonCanonical)

exactReasonCoordinate :: forall s. Term s PBool
exactReasonCoordinate =
  pmatch (pbindInputV1 # rejectedSubject 1 4 # 1 # 4 # priorRoot) $ \PBoundInputV1{..} ->
    pfromData pboundInput'sourceKind
      #== 1
      #&& pfromData pboundInput'inputIndex
      #== 4
      #&& pfromData pboundInput'priorRoot
      #== priorRoot

boundSourceKind :: forall s. Term s PBoundInputV1 -> Term s PInteger
boundSourceKind bound = pmatch bound $ \PBoundInputV1{pboundInput'sourceKind} -> pfromData pboundInput'sourceKind

boundInputIndex :: forall s. Term s PBoundInputV1 -> Term s PInteger
boundInputIndex bound = pmatch bound $ \PBoundInputV1{pboundInput'inputIndex} -> pfromData pboundInput'inputIndex

authenticatesExactOutRef :: forall s. Term s PBool
authenticatesExactOutRef =
  plet (pconstant $ TxOutRef (TxId $ toBuiltin $ BS.pack [0 .. 31]) 7) $ \outRef ->
    pmatch (pauthenticateOutRefV1 # (pbindInputV1 # acceptedSubject # 0 # 0 # priorRoot) # outRef) $ \PAuthenticatedOutRefV1{pauthenticatedOutRef'outRef} ->
      pfromData pauthenticatedOutRef'outRef #== outRef

summary :: forall s. Integer -> Integer -> Term s PDataSummaryV1
summary cborLength memory =
  pcon $
    PDataSummaryV1
      (pdata $ phexByteStr "2222222222222222222222222222222222222222222222222222222222222222")
      (pdata $ pconstant cborLength)
      (pdata $ pconstant memory)

descriptor :: forall s. Term s PInteger -> Term s PByteString -> Term s PLedgerOutputCommitmentV1
descriptor outputIndex output =
  pcon $
    PLedgerOutputCommitmentV1
      (pdata pledgerOutputCommitmentVersion)
      (pdata outputIndex)
      (pdata $ plengthBS # output)
      (pdata $ poutputItemCommitment # outputIndex # output)
      (pdata $ phexByteStr "6011111111111111111111111111111111111111111111111111111111")
      (pdata 5_000_000)
      (pdata 0)
      (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
      (pdata 5)
      (pdata $ -1)
      (pdata $ pconstant "")
      (pdata 0)
      (pdata $ pconstant "")
      (pdata $ summary 101 202)
      (pdata $ summary 103 204)
      (pdata $ summary 3 4)

singleChunkProof :: forall s. Term s PInteger -> Term s PByteString -> Term s Bounded.PChunkProofV1
singleChunkProof outputIndex output =
  plet (Bounded.phashChunk # poutputFieldIndex # outputIndex # 0 # output) $ \leaf ->
    pcon $
      Bounded.PChunkProofV1
        (pdata Bounded.pversion)
        (pdata poutputFieldIndex)
        (pdata outputIndex)
        (pdata $ plengthBS # output)
        (pdata 0)
        (pdata output)
        (pdata $ Merkle.pappendLeaf # 0 # pnil # leaf)
        (pdata pnil)

advance :: forall s. Term s PByteString -> Term s Scan.PLedgerOutputScanControlV1 -> Term s (PMaybe Scan.PLedgerOutputScanControlV1)
advance output control =
  padvanceReconstructionV1
    # descriptor 0 output
    # control
    # singleChunkProof 0 output
    # pcon PDNothing

expectJust :: forall s a. Term s (PMaybe a) -> Term s a
expectJust value = pmatch value $ \case PNothing -> perror; PJust result -> result

isNothing :: forall s a. Term s (PMaybe a) -> Term s PBool
isNothing value = pmatch value $ \case PNothing -> pconstant True; PJust _ -> pconstant False

walkValue :: forall s. Term s PByteString -> Term s Scan.PLedgerOutputScanControlV1
walkValue output =
  plet (expectJust $ advance output Scan.pinitialControlV1) $ \afterAddress ->
    expectJust $ advance output afterAddress

reconstructsCanonicalOutput :: forall s. Term s PBool
reconstructsCanonicalOutput =
  plet (walkValue canonicalOutput) $ \atEnd ->
    pmatch atEnd $ \control ->
      pfromData (Scan.pscan'cursor control)
        #== plengthBS
        # canonicalOutput
        #&& pfinalizeCanonicalV1
        # descriptor 0 canonicalOutput
        # atEnd

refusesPrematureFinalize :: forall s. Term s PBool
refusesPrematureFinalize =
  plet (expectJust $ advance canonicalOutput Scan.pinitialControlV1) $ \afterAddress ->
    pnot
      # (pfinalizeCanonicalV1 # descriptor 0 canonicalOutput # Scan.pinitialControlV1)
      #&& pnot
      # (pfinalizeCanonicalV1 # descriptor 0 canonicalOutput # afterAddress)

trailingByteIsNonCanonical :: forall s. Term s PBool
trailingByteIsNonCanonical =
  plet (canonicalOutput <> phexByteStr "00") $ \trailing ->
    plet (walkValue trailing) $ \atEnd ->
      plet (expectJust $ advance trailing atEnd) $ \terminal ->
        pmatch atEnd $ \atEndControl ->
          pmatch terminal $ \terminalControl ->
            pfromData (Scan.pscan'cursor atEndControl)
              #== plengthBS
              # canonicalOutput
              #&& pnot
              # (pfinalizeCanonicalV1 # descriptor 0 trailing # atEnd)
              #&& pfromData (Scan.pscan'stage terminalControl)
              #== Scan.pstageTerminal
              #&& pnot
              # (pfinalizeCanonicalV1 # descriptor 0 trailing # terminal)
              #&& isNothing (advance trailing terminal)

cursorAtChunkBoundary :: forall s. Term s Scan.PLedgerOutputScanControlV1
cursorAtChunkBoundary =
  pmatch Scan.pinitialControlV1 $ \control ->
    pcon control{Scan.pscan'cursor = pdata Bounded.pchunkBytes}

firstLongChunk, secondLongChunk :: BS.ByteString
firstLongChunk = BS.take 4095 longBytes
secondLongChunk = BS.drop 4095 longBytes

longFrontier :: forall s. Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
longFrontier =
  plet (Bounded.phashChunk # poutputFieldIndex # 0 # 0 # pconstant firstLongChunk) $ \firstHash ->
    plet (Bounded.phashChunk # poutputFieldIndex # 0 # 1 # pconstant secondLongChunk) $ \secondHash ->
      Merkle.pappendLeaf # 1 # (Merkle.pappendLeaf # 0 # pnil # firstHash) # secondHash

longChunkProof :: forall s. Integer -> BS.ByteString -> Term s PByteString -> Term s Bounded.PChunkProofV1
longChunkProof chunkIndex chunk sibling =
  pcon $
    Bounded.PChunkProofV1
      (pdata Bounded.pversion)
      (pdata poutputFieldIndex)
      (pdata 0)
      (pdata $ pconstant $ fromIntegral $ BS.length longBytes)
      (pdata $ pconstant chunkIndex)
      (pdata $ pconstant chunk)
      (pdata longFrontier)
      (pdata $ pcons # pdata sibling # pnil)

firstLongChunkProof, secondLongChunkProof, substitutedSecondLongChunkProof :: forall s. Term s Bounded.PChunkProofV1
firstLongChunkProof =
  longChunkProof 0 firstLongChunk $ Bounded.phashChunk # poutputFieldIndex # 0 # 1 # pconstant secondLongChunk
secondLongChunkProof =
  longChunkProof 1 secondLongChunk $ Bounded.phashChunk # poutputFieldIndex # 0 # 0 # pconstant firstLongChunk
substitutedSecondLongChunkProof =
  pmatch secondLongChunkProof $ \proof ->
    pcon proof{Bounded.pchunkProof'chunk = pdata $ pconstant $ BS.cons 1 $ BS.drop 1 secondLongChunk}

advancesAcrossChunkBoundary :: forall s. Term s PBool
advancesAcrossChunkBoundary =
  pmatch
    ( padvanceReconstructionV1
        # descriptor 0 longOutput
        # Scan.pinitialControlV1
        # firstLongChunkProof
        # (pcon $ PDJust $ pdata secondLongChunkProof)
    )
    $ \case
      PNothing -> pconstant False
      PJust afterAddress -> pmatch afterAddress $ \control -> pfromData (Scan.pscan'stage control) #== Scan.pstageValueHeader

successorIsDecidedByEngine :: forall s. Term s PBool
successorIsDecidedByEngine =
  plet (walkValue canonicalOutput) $ \canonicalEnd ->
    plet (canonicalOutput <> phexByteStr "00") $ \trailing ->
      plet (walkValue trailing) $ \trailingEnd ->
        plet (expectJust $ advance trailing trailingEnd) $ \terminal ->
          pnot
            # isNothing (advance canonicalOutput Scan.pinitialControlV1)
            #&& pfinalizeCanonicalV1
            # descriptor 0 canonicalOutput
            # canonicalEnd
            #&& pnot
            # isNothing (advance trailing trailingEnd)
            #&& isNothing (advance trailing terminal)

requireTrue :: forall s. Term s PBool -> Term s PBool
requireTrue condition = pif condition (pconstant True) perror
