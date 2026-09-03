{-# LANGUAGE OverloadedStrings #-}

module Testing.ValidationInstructionEvidenceBounds (tests) where

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Midgard.BoundedCollection qualified as Collection
import Midgard.BoundedItem qualified as Bounded
import Midgard.FraudProofs.NativeTx.Codec qualified as Codec
import Midgard.FraudProofs.NativeTx.Compact qualified as Compact
import Midgard.FraudProofs.NativeTx.Types
import Midgard.MpfProof.Types (PProofStep (..))
import Midgard.NativeTxFieldAccess qualified as NativeField
import Midgard.ValidationMachine
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationTrace
import Testing.Eval (passertEvalNoTrace, passertEvalNoTraceWithoutHoistChecks)

tests :: TestTree
tests =
  testGroup
    "Midgard.ValidationInstructionEvidenceBounds"
    [ testCase "maximum_general_field_bounded_chunk_instruction_evidence_is_bounded" $
        passertEvalNoTrace maximumGeneralFieldBoundedChunkInstructionEvidenceIsBounded
    , testCase "maximum_script_program_instruction_evidence_is_bounded" $
        passertEvalNoTraceWithoutHoistChecks maximumScriptProgramInstructionEvidenceIsBounded
    , testCase "value_asset_output_instruction_evidence_is_bounded" $
        passertEvalNoTraceWithoutHoistChecks valueAssetOutputInstructionEvidenceIsBounded
    ]

maximumGeneralFieldBoundedChunkInstructionEvidenceIsBounded :: forall s. Term s PBool
maximumGeneralFieldBoundedChunkInstructionEvidenceIsBounded =
  withEmptyProofSource $ \compactCbor witnessSetCbor fieldLengthsCbor transactionId transactionCommitment ->
  plet validationContextCbor $ \contextCbor ->
  plet
    ( pencodeTransactionFieldScanWitness
        # compactCbor # witnessSetCbor # fieldLengthsCbor # contextCbor
        # 0 # 0 # 0 # (-1) # 0
    )
    $ \workCbor ->
  plet
    ( pencodeTransactionFieldScanWitness
        # compactCbor # witnessSetCbor # fieldLengthsCbor # contextCbor
        # 1 # 0 # 0 # (-1) # 0
    )
    $ \nextWorkCbor ->
  plet
    ( machineState
        transactionId transactionCommitment (phashValidationContext # contextCbor)
        (pcon PCanonicalDecode) 1
        (phashWorkWitness # pcon PCanonicalDecode # 1 # nextWorkCbor) hA
    )
    $ \post ->
  plet maximumCollectionFrontier $ \collectionFrontier ->
  plet maximumChunkFrontier $ \chunkFrontier ->
  plet (preplicateBS # Bounded.pchunkBytes # (pintegerToByte # 0)) $ \maximumChunk ->
  plet
    ( pcon $ Collection.PItemProofV1
        (pdata Collection.pboundedCollectionVersion)
        (pdata 8) (pdata 16_383) (pdata 16_382) (pdata 16_384)
        (pdata hA) (pdata collectionFrontier)
        (pdata $ repeatedByteStrings 14 zero32)
    )
    $ \collectionProof ->
  plet
    ( pcon $ Bounded.PChunkProofV1
        (pdata Bounded.pversion) (pdata 8) (pdata 16_382) (pdata 16_384)
        (pdata 3) (pdata maximumChunk) (pdata chunkFrontier)
        (pdata $ repeatedByteStrings 3 zero32)
    )
    $ \chunkProof ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \transition ->
  plet
    ( pcon $ PValidationOneStepEvidenceV1
        (pdata transition)
        (pdata $ pcon $ PTransactionFieldChunkWitness (pdata collectionProof) (pdata chunkProof))
    )
    $ \evidence ->
    pand'List
      [ plengthBS # maximumChunk #== Bounded.pchunkBytes
      , plength # collectionFrontier #== 14
      , plength # chunkFrontier #== 2
      , plengthBS # (pserialiseData # pforgetData (pdata evidence)) #< 16_384
      ]

maximumScriptProgramInstructionEvidenceIsBounded :: forall s. Term s PBool
maximumScriptProgramInstructionEvidenceIsBounded =
  withEmptyProofSource $ \compactCbor witnessSetCbor fieldLengthsCbor transactionId transactionCommitment ->
  pmatch (Merkle.pbuildFrontier # repeatedHashes 15 hA) $ \(Merkle.PBuiltFrontier _ denseFour) ->
  pmatch (Merkle.pbuildFrontier # repeatedHashes 63 hB) $ \(Merkle.PBuiltFrontier _ denseSix) ->
  pmatch (Merkle.pbuildFrontier # repeatedHashes 127 hC) $ \(Merkle.PBuiltFrontier _ denseSeven) ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata 15) (pdata 15) (pdata 15) (pdata 3) (pdata 15)
        (pdata h28A) (pdata $ preplicateBS # 36 # (pintegerToByte # 0))
        (pdata 15) (pdata 128) (pdata hA)
        (pdata 65_535) (pdata 65_535) (pdata $ pconstant "")
        (pdata 15) (pdata denseFour)
    )
    $ \discovery ->
  plet
    ( pcon $ PScriptSourcesControlV1
        (pdata compactCbor) (pdata witnessSetCbor) (pdata fieldLengthsCbor)
        (pdata validationContextCbor)
        (pdata 127) (pdata hA) (pdata 63) (pdata hB) (pdata denseSeven)
        (pdata 8) (pdata 15) (pdata denseFour) (pdata 15) (pdata denseFour)
        (pdata 127) (pdata hB) (pdata hC) (pdata 63)
        (pdata 15) (pdata denseFour)
        (pdata 63) (pdata 63) (pdata denseSix) (pdata 63)
        (pdata pemptyReceivePurposeScanControl)
        (pdata 15) (pdata 15) (pdata pemptyObserverPurposeScanControl)
        (pdata discovery) (pdata $ pcon PDNothing) (pdata $ pconstant "")
        (pdata pemptyMintFoldControl) (pdata hC)
    )
    $ \control ->
  plet (pencodeScriptSourcesDiscoveryWitness # control # 8 # discovery) $ \workCbor ->
  plet
    ( machineState
        transactionId transactionCommitment (phashValidationContext # validationContextCbor)
        (pcon PScriptSources) 53 hB (phashLedgerDelta # phexByteStr "80")
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \transition ->
  plet
    ( pcon $ PScriptSourceScanWitness
        (pdata 15) (pdata 1) (pdata $ preplicateBS # 36 # (pintegerToByte # 0))
        (pdata 3) (pdata h28A) (pdata 50) (pdata hA)
        (pdata $ repeatedByteStrings 4 zero32)
    )
    $ \auxiliary ->
  plet (pcon $ PValidationOneStepEvidenceV1 (pdata transition) (pdata auxiliary)) $ \evidence ->
    plengthBS # (pserialiseData # pforgetData (pdata evidence)) #< 16_384

valueAssetOutputInstructionEvidenceIsBounded :: forall s. Term s PBool
valueAssetOutputInstructionEvidenceIsBounded =
  withEmptyProofSource $ \compactCbor witnessSetCbor fieldLengthsCbor transactionId transactionCommitment ->
  plet denseFrontierFour $ \denseFour ->
  plet denseFrontierSix $ \denseSix ->
  plet denseFrontierSeven $ \denseSeven ->
  plet
    ( pcon $ PNativeScriptsControlV1
        (pdata compactCbor) (pdata witnessSetCbor) (pdata fieldLengthsCbor)
        (pdata validationContextCbor)
        (pdata 127) (pdata pinitialResolutionAccumulator) (pdata 63) (pdata denseSeven)
        (pdata 63) (pdata $ Merkle.pfrontierCommitment # 0 # pnil)
        (pdata 15) (pdata denseFour) (pdata 15) (pdata denseFour)
        (pdata 15) (pdata denseFour)
        (pdata 63) (pdata denseSix) (pdata denseSix)
        (pdata 0) (pdata pnil)
        (pdata 15) (pdata denseFour) (pdata 15) (pdata 3)
        (pdata pemptyResolutionScheduleHash)
    )
    $ \nativeControl ->
  plet
    ( pcon $ PValueAndMintControlV1
        (pdata nativeControl) (pdata 3) (pdata pemptyResolutionScheduleHash)
        (pdata 0) (pdata 0) (pdata zero32)
        (pdata pinitialResolutionAccumulator) (pdata pemptyResolutionScheduleHash)
        (pdata 0) (pdata 1) (pdata 0)
        (pdata $ pcon $ PValueAccumulatorV1 (pdata 0) (pdata hA) (pdata 127) (pdata 127))
    )
    $ \control ->
  plet (pencodeValueAndMintControlV1 # control) $ \workCbor ->
  plet
    ( machineState
        transactionId transactionCommitment (phashValidationContext # validationContextCbor)
        (pcon PValueAndMint) 51 hC hB
    )
    $ \post ->
  plet (pcon $ PValidationOneStepWitnessV1 (pdata workCbor) (pdata post)) $ \transition ->
  plet
    ( pcon $ PValueAssetMutationWitnessV1
        (pdata $ pconstant True) (pdata $ pconstant (-9_223_372_036_854_775_808))
        (pdata maximumMapProof)
    )
    $ \mutation ->
  plet
    ( pcon $ PValueOutputAssetWitness
        (pdata 63) (pdata $ preplicateBS # 512 # (pintegerToByte # 0)) (pdata 127)
        (pdata h28A) (pdata $ preplicateBS # 32 # (pintegerToByte # 0))
        (pdata 9_223_372_036_854_775_807)
        (pdata denseSeven) (pdata $ repeatedByteStrings 7 zero32) (pdata mutation)
    )
    $ \auxiliary ->
  plet (pcon $ PValidationOneStepEvidenceV1 (pdata transition) (pdata auxiliary)) $ \evidence ->
    plengthBS # (pserialiseData # pforgetData (pdata evidence)) #< 12_288

withEmptyProofSource :: forall s.
  ( Term s PByteString -> Term s PByteString -> Term s PByteString ->
    Term s PByteString -> Term s PByteString -> Term s PBool
  ) -> Term s PBool
withEmptyProofSource continuation =
  plet
    ( pcon $ PNativeTxWitnessSetCompact
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
        (pdata NativeField.pemptyFieldCommitment)
    )
    $ \witnessSet ->
  plet (Compact.pencodeNativeTxWitnessSetCompact # witnessSet) $ \witnessSetCbor ->
  plet (pcon $ PNativeTxFieldPreimageLengthsV1 0 0 0 0 0 0 0 0 0) $ \fieldLengths ->
  plet (Compact.pencodeNativeTxFieldPreimageLengthsV1 # fieldLengths) $ \fieldLengthsCbor ->
  plet
    ( pcon $ PNativeTxBodyCompact
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment 0 (-1) (-1)
        NativeField.pemptyFieldCommitment NativeField.pemptyFieldCommitment
        NativeField.pemptyFieldCommitment zero32 zero32 255
    )
    $ \body ->
  plet (Compact.pencodeNativeTxBodyCompact # body) $ \bodyCbor ->
  plet (Compact.pnativeTxIdForVersion # 1 # bodyCbor) $ \transactionId ->
  plet (pcon $ PNativeTxCompact body (pblake2b_256 # witnessSetCbor) 0) $ \compact ->
  plet (Compact.pencodeNativeTxCompactV1 # compact) $ \compactCbor ->
  plet
    (Compact.pnativeTxProofCommitmentV1 # compactCbor # witnessSetCbor # fieldLengthsCbor)
    $ \transactionCommitment ->
      continuation compactCbor witnessSetCbor fieldLengthsCbor transactionId transactionCommitment

machineState :: forall s.
  Term s PByteString -> Term s PByteString -> Term s PByteString ->
  Term s PValidationPhase -> Term s PInteger -> Term s PByteString ->
  Term s PByteString -> Term s PValidationMachineStateV1
machineState transactionId transactionCommitment contextHash phase counter workRoot ledgerDeltaRoot =
  pcon $ PValidationMachineStateV1
    (pdata pmachineVersion) (pdata hA) (pdata transactionId)
    (pdata transactionCommitment) (pdata contextHash) (pdata $ pcon PForced)
    (pdata hC) (pdata phase) (pdata counter) (pdata workRoot)
    (pdata 0) (pdata 0) (pdata $ pcon PPending) (pdata zero32)
    (pdata ledgerDeltaRoot)

validationContextCbor :: forall s. Term s PByteString
validationContextCbor =
  pconstant "\x87"
    <> Codec.pcborInt 1
    <> (Codec.pencodeDefiniteBytes # pconstant "midgard-consensus-v1")
    <> Codec.pcborInt 100 <> Codec.pcborInt 0 <> Codec.pcborInt 0
    <> Codec.pcborInt 0 <> Codec.pcborInt 100

maximumCollectionFrontier :: forall s. Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
maximumCollectionFrontier = peakList
  [ (0, zero32), (1, hA), (2, hB), (3, hC), (4, zero32), (5, hA), (6, hB)
  , (7, hC), (8, zero32), (9, hA), (10, hB), (11, hC), (12, zero32), (13, hA)
  ]

maximumChunkFrontier :: forall s. Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
maximumChunkFrontier = peakList [(0, hB), (2, hC)]

denseFrontierFour, denseFrontierSix, denseFrontierSeven :: forall s.
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
denseFrontierFour = peakList [(0, zero32), (1, hA), (2, hB), (3, hC)]
denseFrontierSix = denseFrontierFour `appendPeaks` [(4, hA), (5, hB)]
denseFrontierSeven = denseFrontierSix `appendPeaks` [(6, hC)]

appendPeaks :: forall s.
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak)) ->
  [(Integer, Term s PByteString)] ->
  Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
appendPeaks prefix suffix = foldr add prefix suffix
  where add (height, hashValue) rest =
          pcons # pdata (pcon $ Merkle.PFrontierPeak (pdata $ pconstant height) (pdata hashValue)) # rest

peakList :: forall s. [(Integer, Term s PByteString)] -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
peakList = foldr add pnil
  where add (height, hashValue) rest =
          pcons # pdata (pcon $ Merkle.PFrontierPeak (pdata $ pconstant height) (pdata hashValue)) # rest

repeatedByteStrings :: forall s. Int -> Term s PByteString -> Term s (PBuiltinList (PAsData PByteString))
repeatedByteStrings count value = foldr (\_ rest -> pcons # pdata value # rest) pnil [1 .. count]

repeatedHashes :: forall s. Int -> Term s PByteString -> Term s (PBuiltinList (PAsData PByteString))
repeatedHashes = repeatedByteStrings

maximumMapProof :: forall s. Term s (PBuiltinList (PAsData PProofStep))
maximumMapProof = foldr (\_ rest -> pcons # pdata maximumBranch # rest) pnil [1 .. (16 :: Int)]
  where
    maximumBranch = pcon $ PBranch (pdata 0) (pdata $ preplicateBS # 128 # (pintegerToByte # 0))

hA, hB, hC, zero32 :: forall s. Term s PByteString
hA = preplicateBS # 32 # (pintegerToByte # 0xaa)
hB = preplicateBS # 32 # (pintegerToByte # 0xbb)
hC = preplicateBS # 32 # (pintegerToByte # 0xcc)
zero32 = preplicateBS # 32 # (pintegerToByte # 0)

h28A :: forall s. Term s PByteString
h28A = preplicateBS # 28 # (pintegerToByte # 0xaa)
