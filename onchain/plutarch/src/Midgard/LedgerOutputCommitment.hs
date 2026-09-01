{-# LANGUAGE OverloadedStrings #-}

module Midgard.LedgerOutputCommitment (
  PLedgerOutputCommitmentV1 (..),
  pledgerOutputCommitmentVersion,
  poutputFieldIndex,
  pmaxCardanoValueCborBytes,
  pencodeLedgerOutputCommitment,
  pdecodeLedgerOutputCommitment,
  poutputItemCommitment,
  passetLeafHash,
  pverifyOutputAssetMembership,
  pverifyOutputChunk,
  pverifyReferenceScriptChunk,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Builtin.Data (pasByteStr, pasInt, pasList)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.BoundedItem (PChunkProofV1 (..), pfromBytes, pverifyChunk)
import Midgard.CekData (PDataSummaryV1 (..))
import Midgard.FraudProofs.NativeTx.Codec (pencodeDefiniteArrayHeader, pencodeDefiniteBytes)
import Midgard.LedgerOutput (pdecodeCanonicalAddressBytes)
import Midgard.ValidationMerkle (
  PFrontierPeak,
  pfrontierCommitment,
  pfrontierIsWellFormed,
  pmaximumLeafCount,
  pverifyMembership,
 )
import Midgard.ValidationTrace (pcborInt)

data PLedgerOutputCommitmentV1 (s :: S) = PLedgerOutputCommitmentV1
  { poutputCommitment'version :: Term s (PAsData PInteger)
  , poutputCommitment'outputIndex :: Term s (PAsData PInteger)
  , poutputCommitment'totalLength :: Term s (PAsData PInteger)
  , poutputCommitment'itemCommitment :: Term s (PAsData PByteString)
  , poutputCommitment'address :: Term s (PAsData PByteString)
  , poutputCommitment'lovelace :: Term s (PAsData PInteger)
  , poutputCommitment'assetCount :: Term s (PAsData PInteger)
  , poutputCommitment'assetFrontierCommitment :: Term s (PAsData PByteString)
  , poutputCommitment'cardanoValueSize :: Term s (PAsData PInteger)
  , poutputCommitment'referenceScriptLanguage :: Term s (PAsData PInteger)
  , poutputCommitment'referenceScriptHash :: Term s (PAsData PByteString)
  , poutputCommitment'referenceScriptTotalLength :: Term s (PAsData PInteger)
  , poutputCommitment'referenceScriptItemCommitment :: Term s (PAsData PByteString)
  , poutputCommitment'cardanoTxOut :: Term s (PAsData PDataSummaryV1)
  , poutputCommitment'midgardTxOut :: Term s (PAsData PDataSummaryV1)
  , poutputCommitment'cardanoSpendDatum :: Term s (PAsData PDataSummaryV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerOutputCommitmentV1)

pledgerOutputCommitmentVersion, poutputFieldIndex, pmaxCardanoValueCborBytes :: forall (s :: S). Term s PInteger
pledgerOutputCommitmentVersion = 1
poutputFieldIndex = 2
pmaxCardanoValueCborBytes = 5_000

puint64Max :: forall (s :: S). Term s PInteger
puint64Max = 18_446_744_073_709_551_615

puint64IsValid :: forall (s :: S). Term s PInteger -> Term s PBool
puint64IsValid value = value #>= 0 #&& value #<= puint64Max

psummaryIsWellFormed :: forall (s :: S). Term s PDataSummaryV1 -> Term s PBool
psummaryIsWellFormed summary = pmatch summary $ \s -> pand'List
  [ plengthBS # pfromData (psummary'root s) #== 32
  , puint64IsValid (pfromData $ psummary'cborLength s)
  , puint64IsValid (pfromData $ psummary'memory s)
  ]

pencodeSummary :: forall (s :: S). Term s PDataSummaryV1 -> Term s PByteString
pencodeSummary summary = pmatch summary $ \s ->
  pif (psummaryIsWellFormed summary)
    ( (pencodeDefiniteArrayHeader # 3)
        <> (pencodeDefiniteBytes # pfromData (psummary'root s))
        <> pcborInt (pfromData $ psummary'cborLength s)
        <> pcborInt (pfromData $ psummary'memory s)
    )
    perror

psummaryFromData :: forall (s :: S). Term s PData -> Term s PDataSummaryV1
psummaryFromData dat =
  plet (pasList # dat) $ \items ->
  pif (plength # items #== 3)
    (plet (pcon $ PDataSummaryV1
      (pdata $ pasByteStr # (pelemAt # 0 # items))
      (pdata $ pasInt # (pelemAt # 1 # items))
      (pdata $ pasInt # (pelemAt # 2 # items))) $ \summary ->
        pif (psummaryIsWellFormed summary) summary perror)
    perror

preferenceScriptIsWellFormed :: forall (s :: S). Term s PLedgerOutputCommitmentV1 -> Term s PBool
preferenceScriptIsWellFormed descriptor = pmatch descriptor $ \d ->
  plet (pfromData $ poutputCommitment'referenceScriptLanguage d) $ \language ->
  pif (language #== -1)
    ( pfromData (poutputCommitment'referenceScriptHash d) #== pconstant ""
        #&& pfromData (poutputCommitment'referenceScriptTotalLength d) #== 0
        #&& pfromData (poutputCommitment'referenceScriptItemCommitment d) #== pconstant ""
    )
    ( (language #== 0 #|| language #== 3 #|| language #== 128)
        #&& plengthBS # pfromData (poutputCommitment'referenceScriptHash d) #== 28
        #&& pfromData (poutputCommitment'referenceScriptTotalLength d) #> 0
        #&& plengthBS # pfromData (poutputCommitment'referenceScriptItemCommitment d) #== 32
    )

pdescriptorIsWellFormed :: forall (s :: S). Term s PLedgerOutputCommitmentV1 -> Term s PBool
pdescriptorIsWellFormed descriptor = pmatch descriptor $ \d ->
  plet (pfromData $ poutputCommitment'referenceScriptLanguage d) $ \language -> pand'List
    [ pfromData (poutputCommitment'version d) #== pledgerOutputCommitmentVersion
    , pfromData (poutputCommitment'outputIndex d) #>= 0
    , pfromData (poutputCommitment'outputIndex d) #<= 65_535
    , pfromData (poutputCommitment'totalLength d) #>= 0
    , plengthBS # pfromData (poutputCommitment'itemCommitment d) #== 32
    , pmatch (pdecodeCanonicalAddressBytes # pfromData (poutputCommitment'address d)) $ \case
        PNothing -> pconstant False
        PJust _ -> pconstant True
    , puint64IsValid (pfromData $ poutputCommitment'lovelace d)
    , pfromData (poutputCommitment'assetCount d) #>= 0
    , pfromData (poutputCommitment'assetCount d) #<= pmaximumLeafCount
    , plengthBS # pfromData (poutputCommitment'assetFrontierCommitment d) #== 32
    , pfromData (poutputCommitment'cardanoValueSize d) #>= 0
    , pfromData (poutputCommitment'cardanoValueSize d) #<= pmaxCardanoValueCborBytes
    , language #== -1 #|| language #== 0 #|| language #== 3 #|| language #== 128
    , preferenceScriptIsWellFormed descriptor
    , psummaryIsWellFormed (pfromData $ poutputCommitment'cardanoTxOut d)
    , psummaryIsWellFormed (pfromData $ poutputCommitment'midgardTxOut d)
    , psummaryIsWellFormed (pfromData $ poutputCommitment'cardanoSpendDatum d)
    ]

pencodeLedgerOutputCommitment :: forall (s :: S). Term s (PLedgerOutputCommitmentV1 :--> PByteString)
pencodeLedgerOutputCommitment = phoistAcyclic $ plam $ \descriptor -> pmatch descriptor $ \d ->
  pif (pdescriptorIsWellFormed descriptor)
    ( (pencodeDefiniteArrayHeader # 16)
        <> pcborInt pledgerOutputCommitmentVersion
        <> pcborInt (pfromData $ poutputCommitment'outputIndex d)
        <> pcborInt (pfromData $ poutputCommitment'totalLength d)
        <> (pencodeDefiniteBytes # pfromData (poutputCommitment'itemCommitment d))
        <> (pencodeDefiniteBytes # pfromData (poutputCommitment'address d))
        <> pcborInt (pfromData $ poutputCommitment'lovelace d)
        <> pcborInt (pfromData $ poutputCommitment'assetCount d)
        <> (pencodeDefiniteBytes # pfromData (poutputCommitment'assetFrontierCommitment d))
        <> pcborInt (pfromData $ poutputCommitment'cardanoValueSize d)
        <> pcborInt (pfromData $ poutputCommitment'referenceScriptLanguage d)
        <> (pencodeDefiniteBytes # pfromData (poutputCommitment'referenceScriptHash d))
        <> pcborInt (pfromData $ poutputCommitment'referenceScriptTotalLength d)
        <> (pencodeDefiniteBytes # pfromData (poutputCommitment'referenceScriptItemCommitment d))
        <> pencodeSummary (pfromData $ poutputCommitment'cardanoTxOut d)
        <> pencodeSummary (pfromData $ poutputCommitment'midgardTxOut d)
        <> pencodeSummary (pfromData $ poutputCommitment'cardanoSpendDatum d)
    )
    perror

pdecodeLedgerOutputCommitment :: forall (s :: S). Term s (PByteString :--> PLedgerOutputCommitmentV1)
pdecodeLedgerOutputCommitment = phoistAcyclic $ plam $ \descriptorCbor ->
  pmatch (pdeserialise # descriptorCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 16)
        (plet (pcon $ PLedgerOutputCommitmentV1
          (pdata $ pasInt # (pelemAt # 0 # items))
          (pdata $ pasInt # (pelemAt # 1 # items))
          (pdata $ pasInt # (pelemAt # 2 # items))
          (pdata $ pasByteStr # (pelemAt # 3 # items))
          (pdata $ pasByteStr # (pelemAt # 4 # items))
          (pdata $ pasInt # (pelemAt # 5 # items))
          (pdata $ pasInt # (pelemAt # 6 # items))
          (pdata $ pasByteStr # (pelemAt # 7 # items))
          (pdata $ pasInt # (pelemAt # 8 # items))
          (pdata $ pasInt # (pelemAt # 9 # items))
          (pdata $ pasByteStr # (pelemAt # 10 # items))
          (pdata $ pasInt # (pelemAt # 11 # items))
          (pdata $ pasByteStr # (pelemAt # 12 # items))
          (pdata $ psummaryFromData $ pelemAt # 13 # items)
          (pdata $ psummaryFromData $ pelemAt # 14 # items)
          (pdata $ psummaryFromData $ pelemAt # 15 # items)) $ \descriptor ->
          pif (pdescriptorIsWellFormed descriptor
            #&& pencodeLedgerOutputCommitment # descriptor #== descriptorCbor)
            descriptor perror)
        perror

poutputItemCommitment :: forall (s :: S). Term s (PInteger :--> PByteString :--> PByteString)
poutputItemCommitment = phoistAcyclic $ plam $ \outputIndex outputCbor ->
  pfromBytes # poutputFieldIndex # outputIndex # outputCbor

passetLeafDomain :: forall (s :: S). Term s PByteString
passetLeafDomain = pconstant "MidgardLedgerOutputAssetLeafV1"

passetLeafHash :: forall (s :: S). Term s (PByteString :--> PByteString :--> PInteger :--> PByteString)
passetLeafHash = phoistAcyclic $ plam $ \policyId assetName quantity ->
  pif (plengthBS # policyId #== 28 #&& plengthBS # assetName #<= 32 #&& quantity #> 0)
    (pblake2b_256 #$ passetLeafDomain <> (pencodeDefiniteArrayHeader # 3)
      <> (pencodeDefiniteBytes # policyId) <> (pencodeDefiniteBytes # assetName) <> pcborInt quantity)
    perror

pverifyOutputAssetMembership ::
  forall (s :: S).
  Term s (PLedgerOutputCommitmentV1 :--> PInteger :--> PByteString :--> PByteString :--> PInteger
    :--> PBuiltinList (PAsData PFrontierPeak) :--> PBuiltinList (PAsData PByteString) :--> PBool)
pverifyOutputAssetMembership = phoistAcyclic $ plam $
  \descriptor assetIndex policyId assetName quantity peaks siblings -> pmatch descriptor $ \d -> pand'List
    [ pdescriptorIsWellFormed descriptor
    , pfrontierIsWellFormed # pfromData (poutputCommitment'assetCount d) # peaks
    , pfrontierCommitment # pfromData (poutputCommitment'assetCount d) # peaks
        #== pfromData (poutputCommitment'assetFrontierCommitment d)
    , pverifyMembership # pfromData (poutputCommitment'assetCount d) # peaks # assetIndex
        # (passetLeafHash # policyId # assetName # quantity) # siblings
    ]

pverifyOutputChunk :: forall (s :: S). Term s (PLedgerOutputCommitmentV1 :--> PChunkProofV1 :--> PBool)
pverifyOutputChunk = phoistAcyclic $ plam $ \descriptor proof -> pmatch descriptor $ \d -> pmatch proof $ \p -> pand'List
  [ pdescriptorIsWellFormed descriptor
  , pfromData (pchunkProof'fieldIndex p) #== poutputFieldIndex
  , pchunkProof'itemIndex p #== poutputCommitment'outputIndex d
  , pchunkProof'totalLength p #== poutputCommitment'totalLength d
  , pverifyChunk # pfromData (poutputCommitment'itemCommitment d) # proof
  ]

pverifyReferenceScriptChunk ::
  forall (s :: S). Term s (PLedgerOutputCommitmentV1 :--> PChunkProofV1 :--> PBool)
pverifyReferenceScriptChunk = phoistAcyclic $ plam $ \descriptor proof -> pmatch descriptor $ \d -> pmatch proof $ \p -> pand'List
  [ pdescriptorIsWellFormed descriptor
  , pfromData (poutputCommitment'referenceScriptLanguage d) #/= -1
  , pfromData (pchunkProof'fieldIndex p) #== poutputFieldIndex
  , pchunkProof'itemIndex p #== poutputCommitment'outputIndex d
  , pchunkProof'totalLength p #== poutputCommitment'referenceScriptTotalLength d
  , pverifyChunk # pfromData (poutputCommitment'referenceScriptItemCommitment d) # proof
  ]
