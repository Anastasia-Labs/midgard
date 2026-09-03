module Midgard.ValidationResolver (
  PCekMaterialRouteV1 (..),
  PCekSinglePublicationDatumV1 (..),
  pphaseAScriptPreconditionsSemanticResolverCount,
  pscriptSourcesSemanticResolverCount,
  pcekReferenceIndicesUniqueNonnegativeV1,
  pverifyCekMaterialRouteForSelectedEnvelopeV1,
  pverifyCekRouteV1,
  pselectSemanticResolver,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  POutputDatum (..),
  PScriptHash,
  PTxInInfo (..),
  PTxOut (..),
 )
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.BoundedItem qualified as BoundedItem
import Midgard.CekProof qualified as CekProof
import Midgard.FraudProofs.NativeTx.Codec (psliceLen)
import Midgard.LedgerState (PCekProgramMaterialDatumV1)
import Midgard.NativeScriptScan qualified as NativeScriptScan
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepEvidenceV1 (..),
  pvalidationAuxiliaryWitnessFromData,
 )

pphaseAScriptPreconditionsSemanticResolverCount :: forall (s :: S). Term s PInteger
pphaseAScriptPreconditionsSemanticResolverCount = 2

pscriptSourcesSemanticResolverCount :: forall (s :: S). Term s PInteger
pscriptSourcesSemanticResolverCount = 29

data PCekMaterialRouteV1 (s :: S)
  = PNoCekMaterial
  | PDirectCekMaterial
      { pdirectCekMaterial'envelopeCbor :: Term s (PAsData PByteString)
      , pdirectCekMaterial'sidecarCbor :: Term s (PAsData PByteString)
      }
  | PSinglePublicationCekMaterial
      { psinglePublication'envelopeCbor :: Term s (PAsData PByteString)
      , psinglePublication'referenceInputIndex :: Term s (PAsData PInteger)
      }
  | PMinimumMultiOutputCekMaterial
      { pmultiOutput'envelopeCbor :: Term s (PAsData PByteString)
      , pmultiOutput'referenceInputIndices :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
      }
  | PIncrementalCekMaterial
      { pincrementalCekMaterial'programEnvelopeHash :: Term s (PAsData PByteString) }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekMaterialRouteV1)

data PCekSinglePublicationDatumV1 (s :: S) = PCekSinglePublicationDatumV1
  { psinglePublicationDatum'version :: Term s (PAsData PInteger)
  , psinglePublicationDatum'programEnvelopeHash :: Term s (PAsData PByteString)
  , psinglePublicationDatum'sidecarCbor :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekSinglePublicationDatumV1)

pcekReferenceIndicesUniqueNonnegativeV1 ::
  forall (s :: S). Term s (PBuiltinList PInteger :--> PBool)
pcekReferenceIndicesUniqueNonnegativeV1 = phoistAcyclic $ pfix $ \self -> plam $ \indices ->
  pelimList
    (\first rest -> first #>= 0 #&& pnot # (phas # rest # first) #&& self # rest)
    (pconstant True)
    indices
  where
    phas :: forall t. Term t (PBuiltinList PInteger :--> PInteger :--> PBool)
    phas = phoistAcyclic $ pfix $ \self -> plam $ \xs target ->
      pelimList (\x rest -> x #== target #|| self # rest # target) (pconstant False) xs

-- | Aiken @cek_selection_envelope_cbor_v1@.
pcekSelectionEnvelopeCborV1 ::
  forall (s :: S). Term s (PValidationOneStepEvidenceV1 :--> PMaybe PByteString)
pcekSelectionEnvelopeCborV1 = phoistAcyclic $ plam $ \evidence ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 _ auxiliaryD) ->
    pmatch (pvalidationAuxiliaryWitnessFromData # pforgetData auxiliaryD) $ \case
      PNativeExecutionScanWitness
        _ languageTagD _ _ _ _ _ _ _ _ scriptTotalLengthD _ _ _ _ firstChunkProofD ->
          plet (pfromData languageTagD) $ \languageTag ->
          plet (pfromData scriptTotalLengthD) $ \scriptTotalLength ->
          plet (pfromData firstChunkProofD) $ \firstChunkProof ->
          pmatch firstChunkProof $ \proof ->
          plet (pfromData $ BoundedItem.pchunkProof'chunk proof) $ \chunk ->
          pif
            ( (languageTag #== 3 #|| languageTag #== 128)
                #&& pfromData (BoundedItem.pchunkProof'chunkIndex proof) #== 0
                #&& pfromData (BoundedItem.pchunkProof'totalLength proof) #== scriptTotalLength
            )
            ( pmatch (NativeScriptScan.pversionedScriptHeaderV1 # chunk # scriptTotalLength) $ \case
                PNothing -> pcon PNothing
                PJust header -> pmatch header $ \headerFields ->
                  plet (pfromData $ NativeScriptScan.pheader'payloadOffset headerFields) $ \payloadOffset ->
                  plet (pfromData $ NativeScriptScan.pheader'payloadLength headerFields) $ \payloadLength ->
                    pif
                      ( pfromData (NativeScriptScan.pheader'languageTag headerFields) #== languageTag
                          #&& payloadOffset #>= 0
                          #&& payloadLength #> 0
                          #&& payloadOffset + payloadLength #<= plengthBS # chunk
                      )
                      (pcon $ PJust $ psliceLen # chunk # payloadOffset # payloadLength)
                      (pcon PNothing)
            )
            (pcon PNothing)
      _ -> pcon PNothing

-- | Aiken @cek_envelope_hash_v1@.
pcekEnvelopeHashV1 :: forall (s :: S). Term s (PByteString :--> PMaybe PByteString)
pcekEnvelopeHashV1 = phoistAcyclic $ plam $ \envelopeCbor ->
  pmatch (CekProof.pinspectProgramEnvelopeV1 # envelopeCbor) $ \case
    PNothing -> pcon PNothing
    PJust envelope -> pmatch envelope $ \fields ->
      pcon $ PJust $
        CekProof.phashProgramEnvelopeV1
          # 1
          # 1
          # 0
          # pfromData (CekProof.penvelope'termRoot fields)
          # pfromData (CekProof.penvelope'nodeCount fields)
          # pfromData (CekProof.penvelope'materialByteLength fields)

-- | Aiken's total @list.at@, including its rejection of negative indices.
pelemAtMaybe ::
  forall (s :: S) (a :: S -> Type).
  PIsListLike PBuiltinList a =>
  Term s (PInteger :--> PBuiltinList a :--> PMaybe a)
pelemAtMaybe = phoistAcyclic $ pfix $ \self -> plam $ \index items ->
  pif (index #< 0) (pcon PNothing) $
    pelimList
      (\item rest -> pif (index #== 0) (pcon $ PJust item) (self # (index - 1) # rest))
      (pcon PNothing)
      items

-- | Aiken @material_reference_input_at@.
pmaterialReferenceInputAt ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PInteger
        :--> PAsData PScriptHash
        :--> PMaybe PData
    )
pmaterialReferenceInputAt = phoistAcyclic $ plam $ \referenceInputs index materialScriptHash ->
  pmatch (pelemAtMaybe # index # referenceInputs) $ \case
    PNothing -> pcon PNothing
    PJust inputD ->
      pmatch (pfromData inputD) $ \PTxInInfo {ptxInInfo'resolved} ->
      pmatch ptxInInfo'resolved $ \PTxOut {ptxOut'address, ptxOut'datum} ->
      pmatch ptxOut'address $ \PAddress {paddress'credential} ->
      pmatch paddress'credential $ \case
        PPubKeyCredential _ -> pcon PNothing
        PScriptCredential actualScriptHash ->
          pif
            (pfromData actualScriptHash #== pfromData materialScriptHash)
            ( pmatch ptxOut'datum $ \case
                POutputDatum {poutputDatum'outputDatum} -> pcon $ PJust $ pto poutputDatum'outputDatum
                _ -> pcon PNothing
            )
            (pcon PNothing)

-- | Aiken @verify_single_publication_route_v1@.
pverifySinglePublicationRouteV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PInteger
        :--> PAsData PScriptHash
        :--> PBool
    )
pverifySinglePublicationRouteV1 = phoistAcyclic $ plam $ \envelopeCbor referenceInputs referenceInputIndex materialScriptHash ->
  pmatch (pmaterialReferenceInputAt # referenceInputs # referenceInputIndex # materialScriptHash) $ \case
    PNothing -> pconstant False
    PJust datumData ->
      plet
        (pfromData $ punsafeCoerce @(PAsData PCekSinglePublicationDatumV1) datumData)
        $ \single -> pmatch single $ \fields ->
          pmatch (pcekEnvelopeHashV1 # envelopeCbor) $ \case
            PNothing -> pconstant False
            PJust expectedHash ->
              pand'List
                [ pfromData (psinglePublicationDatum'version fields) #== 1
                , pfromData (psinglePublicationDatum'programEnvelopeHash fields) #== expectedHash
                , CekProof.pverifyCompleteProgramMaterialV1
                    # envelopeCbor
                    # pfromData (psinglePublicationDatum'sidecarCbor fields)
                ]

-- | Aiken @material_entries_from_references_v1@.
pmaterialEntriesFromReferencesV1 ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PBuiltinList (PAsData PInteger)
        :--> PAsData PScriptHash
        :--> PMaybe (PBuiltinList (PAsData PCekProgramMaterialDatumV1))
    )
pmaterialEntriesFromReferencesV1 = phoistAcyclic $ pfix $ \self ->
  plam $ \referenceInputs indices materialScriptHash ->
    pelimList
      ( \indexD rest ->
          pmatch
            (pmaterialReferenceInputAt # referenceInputs # pfromData indexD # materialScriptHash)
            $ \case
              PNothing -> pcon PNothing
              PJust datumData ->
                pmatch (self # referenceInputs # rest # materialScriptHash) $ \case
                  PNothing -> pcon PNothing
                  PJust entries ->
                    pcon $ PJust $
                      pcons
                        # punsafeCoerce @(PAsData PCekProgramMaterialDatumV1) datumData
                        # entries
      )
      (pcon $ PJust pnil)
      indices

-- | Aiken @verify_cek_material_route_for_selected_envelope_v1@.
pverifyCekMaterialRouteForSelectedEnvelopeV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PCekMaterialRouteV1
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PScriptHash
        :--> PBool
    )
pverifyCekMaterialRouteForSelectedEnvelopeV1 = phoistAcyclic $
  plam $ \selectedEnvelope materialRoute referenceInputs materialScriptHash ->
    pmatch materialRoute $ \case
      PDirectCekMaterial envelopeCborD sidecarCborD ->
        plet (pfromData envelopeCborD) $ \envelopeCbor ->
          envelopeCbor #== selectedEnvelope
            #&& CekProof.pverifyCompleteProgramMaterialV1
              # envelopeCbor
              # pfromData sidecarCborD
      PSinglePublicationCekMaterial envelopeCborD referenceInputIndexD ->
        plet (pfromData envelopeCborD) $ \envelopeCbor ->
          envelopeCbor #== selectedEnvelope
            #&& pverifySinglePublicationRouteV1
              # envelopeCbor
              # referenceInputs
              # pfromData referenceInputIndexD
              # materialScriptHash
      PMinimumMultiOutputCekMaterial envelopeCborD referenceInputIndicesD ->
        plet (pfromData envelopeCborD) $ \envelopeCbor ->
        plet (pfromData referenceInputIndicesD) $ \referenceInputIndices ->
        plet (pmap # plam pfromData # referenceInputIndices) $ \plainIndices ->
          pif
            ( envelopeCbor #== selectedEnvelope
                #&& plength # referenceInputIndices #> 0
                #&& pcekReferenceIndicesUniqueNonnegativeV1 # plainIndices
            )
            ( pmatch
                (pmaterialEntriesFromReferencesV1 # referenceInputs # referenceInputIndices # materialScriptHash)
                $ \case
                  PNothing -> pconstant False
                  PJust entries ->
                    CekProof.pverifyCompleteProgramMaterialEntriesV1 # envelopeCbor # entries
            )
            (pconstant False)
      PIncrementalCekMaterial _ -> pconstant False
      PNoCekMaterial -> pconstant False

-- | Aiken @verify_cek_route_v1@.
pverifyCekRouteV1 ::
  forall (s :: S).
  Term
    s
    ( PValidationOneStepEvidenceV1
        :--> PCekMaterialRouteV1
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PScriptHash
        :--> PBool
    )
pverifyCekRouteV1 = phoistAcyclic $
  plam $ \evidence materialRoute referenceInputs materialScriptHash ->
    pmatch (pcekSelectionEnvelopeCborV1 # evidence) $ \case
      PNothing -> materialRoute #== pcon PNoCekMaterial
      PJust selectedEnvelope ->
        pverifyCekMaterialRouteForSelectedEnvelopeV1
          # selectedEnvelope
          # materialRoute
          # referenceInputs
          # materialScriptHash

pselectSemanticResolver ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PScriptHash) :--> PInteger :--> PInteger :--> PMaybe PScriptHash)
pselectSemanticResolver = phoistAcyclic $ plam $ \scriptHashes expectedCount selectedIndex ->
  pif
    ( expectedCount #> 0
        #&& plength # scriptHashes #== expectedCount
        #&& selectedIndex #>= 0
        #&& selectedIndex #< expectedCount
    )
    (pindex # scriptHashes # selectedIndex)
    (pcon PNothing)
  where
    pindex :: forall t. Term t (PBuiltinList (PAsData PScriptHash) :--> PInteger :--> PMaybe PScriptHash)
    pindex = phoistAcyclic $ pfix $ \self -> plam $ \xs index ->
      pelimList
        (\x rest -> pif (index #== 0) (pcon $ PJust $ pfromData x) (self # rest # (index - 1)))
        (pcon PNothing)
        xs
