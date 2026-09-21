{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.ScriptSourcesRedeemerNormalization
Description : Plutarch port of @script-sources-redeemer-normalization-v1.ak@.
-}
module Midgard.ScriptSourcesRedeemerNormalization (
  PScriptSourcesRedeemerEnvelopeFactsV1 (..),
  PPreparedScriptSourcesRedeemerEnvelopeV1 (..),
  PUnvalidatedRedeemerItemOuterFieldsV1 (..),
  PTraversalNormalizedScriptSourcesRedeemerActionV1 (..),
  PFamilyTraversalSerializationTemplateV1 (..),
  POuterNormalizedScriptSourcesRedeemerActionV1 (..),
  PScriptSourcesRedeemerExecutionAttestedStateV1 (..),
  pversion,
  pfoldMapFamily,
  pfinalizeFrameFamily,
  penvelopeDomain,
  ptraversalNormalizedDomain,
  pouterNormalizedDomain,
  pexecutionAttestedDomain,
  pnarrowPreimageHashV1,
  pcanonicalAuxiliaryHashV1,
  pcanonicalActionHashV1,
  pactionMatchesFamilyV1,
  pnarrowActionIsBoundV1,
  ptraversalActionIdentityV1,
  ptraversalActionIdentityIsBoundV1,
  pverifyRawEnvelopeV1,
  prawCommonControlIsInitial,
  ptraversalNormalizedStateIsBoundV1,
  pouterNormalizerRouteIsExactV1,
  ptraversalSerializationTemplateV1,
  pouterNormalizedStateIsBoundV1,
  psemanticExecutorRouteIsExactV1,
  pexecutionAttestedStateV1,
  pexecutionAttestationIsBoundToEnvelopeV1,
  pexecutionAttestationSettlementIsExactV1,
  pcarrierIdentity,
  pcanonicalItemActionHash,
  pscriptSourcesCarrier, pcekContextCarrier,
  popenHeaderFamily, popenTailFamily, pheadFamily, pattachScalarFamily,
  pfoldListFamily, padvanceFamily, pfinishDataFamily, pinvalidFamily,
  penvelopeCommitmentV1,
  penvelopeStateIsBoundV1,
  pbaseProvenanceIdentityV1,
  ptraversalNormalizerRouteIsExactV1,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.CekDataTraverse (
  PDataTraverseActionV1 (..),
  PDataTraverseControlV1 (..),
  pstageFold,
 )
import Midgard.FraudProofs.NativeTx.Codec (
  pcborInt,
  pencodeDefiniteArrayHeader,
  pencodeDefiniteBytes,
 )
import Midgard.FraudProofs.NativeTx.Compact (pnativeTxProofCommitmentV1)
import Midgard.CekContextItemWire qualified as ItemWire
import Midgard.RedeemerItemProof qualified as Item
import Midgard.ScriptSourcesRawFrame qualified as Frame
import Midgard.ScriptProof qualified as Script
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationResolutionData (decodePrepared, recordFields)
import Midgard.RejectionReason (prejectInvalidFieldType)
import Midgard.ValidationMachine (
  prejectedSuccessorIsExact,
  PValidationOneStepWitnessV1 (..),
 )
import Midgard.ValidationResolution (
  PPreparedValidationResolutionStateV1 (..),
  PValidationResolutionStateV1 (..),
  ppreparedResolutionIsWellFormed,
  ppreparedResolutionVersion,
  pwinningResolution,
 )
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (..),
  phashValidationContext,
  phashWorkWitness,
 )

pversion, pfoldMapFamily, pfinalizeFrameFamily :: forall s. Term s PInteger
pversion = 1
pfoldMapFamily = 0
pfinalizeFrameFamily = 1

penvelopeDomain, ptraversalNormalizedDomain, pouterNormalizedDomain,
  pexecutionAttestedDomain :: forall s. Term s PByteString
penvelopeDomain = pconstant "MidgardScriptSourcesRedeemerEnvelopeV1"
ptraversalNormalizedDomain = pconstant "MidgardScriptSourcesTraversalNormalizedV1"
pouterNormalizedDomain = pconstant "MidgardScriptSourcesOuterNormalizedV1"
pexecutionAttestedDomain = pconstant "MidgardScriptSourcesRedeemerExecutionAttestedV1"

pauxiliaryIdentityDomain, pactionIdentityDomain,
  ptraversalActionIdentityDomain :: forall s. Term s PByteString
pauxiliaryIdentityDomain = pconstant "MidgardScriptSourcesAuxiliaryIdentityV1"
pactionIdentityDomain = pconstant "MidgardScriptSourcesNarrowActionIdentityV1"
ptraversalActionIdentityDomain = pconstant "MidgardScriptSourcesTraversalActionIdentityV1"

pscriptSourcesCarrier, pcekContextCarrier, popenHeaderFamily, popenTailFamily,
  pheadFamily, pattachScalarFamily, pfoldListFamily, padvanceFamily, pfinishDataFamily, pinvalidFamily :: forall s. Term s PInteger
pscriptSourcesCarrier = 0
pcekContextCarrier = 1
popenHeaderFamily = 2
popenTailFamily = 3
pheadFamily = 4
pattachScalarFamily = 5
pfoldListFamily = 6
padvanceFamily = 7
pfinishDataFamily = 8
pinvalidFamily = 9
presolutionIdentityDomain, penvelopeCommitmentDomain,
  pbaseProvenanceIdentityDomain :: forall s. Term s PByteString
presolutionIdentityDomain = pconstant "MidgardScriptSourcesResolutionIdentityV1"
penvelopeCommitmentDomain = pconstant "MidgardScriptSourcesRedeemerEnvelopeCommitmentV1"
pbaseProvenanceIdentityDomain = pconstant "MidgardScriptSourcesBaseProvenanceIdentityV1"

data PScriptSourcesRedeemerEnvelopeFactsV1 (s :: S) = PScriptSourcesRedeemerEnvelopeFactsV1
  { penvelopeFacts'currentPendingItemControlHash :: Term s (PAsData PByteString)
  , penvelopeFacts'redeemerCount :: Term s (PAsData PInteger)
  , penvelopeFacts'redeemerTotalCount :: Term s (PAsData PInteger)
  , penvelopeFacts'canonicalAuxiliaryHash :: Term s (PAsData PByteString)
  , penvelopeFacts'canonicalActionHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesRedeemerEnvelopeFactsV1)

data PPreparedScriptSourcesRedeemerEnvelopeV1 (s :: S) = PPreparedScriptSourcesRedeemerEnvelopeV1
  { penvelope'version :: Term s (PAsData PInteger)
  , penvelope'domain :: Term s (PAsData PByteString)
  , penvelope'deploymentId :: Term s (PAsData PByteString)
  , penvelope'carrier :: Term s (PAsData PInteger)
  , penvelope'base :: Term s PData
  , penvelope'carrierIdentity :: Term s (PAsData PByteString)
  , penvelope'expectedNextControlDataHash :: Term s (PAsData PByteString)
  , penvelope'actionFamily :: Term s (PAsData PInteger)
  , penvelope'canonicalAuxiliaryHash :: Term s (PAsData PByteString)
  , penvelope'canonicalActionHash :: Term s (PAsData PByteString)
  , penvelope'currentPendingItemControlHash :: Term s (PAsData PByteString)
  , penvelope'expectedNextItemControlHash :: Term s (PAsData PByteString)
  , penvelope'redeemerCount :: Term s (PAsData PInteger)
  , penvelope'redeemerTotalCount :: Term s (PAsData PInteger)
  , penvelope'envelopeBinderScriptHash :: Term s (PAsData PByteString)
  , penvelope'traversalNormalizerScriptHash :: Term s (PAsData PByteString)
  , penvelope'outerNormalizerScriptHash :: Term s (PAsData PByteString)
  , penvelope'semanticExecutorScriptHash :: Term s (PAsData PByteString)
  , penvelope'settlementScriptHash :: Term s (PAsData PByteString)
  , penvelope'envelopeCommitment :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPreparedScriptSourcesRedeemerEnvelopeV1)

data PUnvalidatedRedeemerItemOuterFieldsV1 (s :: S) = PUnvalidatedRedeemerItemOuterFieldsV1
  { pouterFields'version :: Term s (PAsData PInteger)
  , pouterFields'mode :: Term s (PAsData PInteger)
  , pouterFields'stage :: Term s (PAsData PInteger)
  , pouterFields'itemIndex :: Term s (PAsData PInteger)
  , pouterFields'itemCount :: Term s (PAsData PInteger)
  , pouterFields'totalLength :: Term s (PAsData PInteger)
  , pouterFields'itemCommitment :: Term s (PAsData PByteString)
  , pouterFields'expectedPurposeTag :: Term s (PAsData PInteger)
  , pouterFields'expectedPointerIndex :: Term s (PAsData PInteger)
  , pouterFields'purposeTag :: Term s (PAsData PInteger)
  , pouterFields'pointerIndex :: Term s (PAsData PInteger)
  , pouterFields'dataOffset :: Term s (PAsData PInteger)
  , pouterFields'dataLength :: Term s (PAsData PInteger)
  , pouterFields'executionMemory :: Term s (PAsData PInteger)
  , pouterFields'executionSteps :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PUnvalidatedRedeemerItemOuterFieldsV1)

data PTraversalNormalizedScriptSourcesRedeemerActionV1 (s :: S) =
  PTraversalNormalizedScriptSourcesRedeemerActionV1
    { ptraversalNormalized'version :: Term s (PAsData PInteger)
    , ptraversalNormalized'domain :: Term s (PAsData PByteString)
    , ptraversalNormalized'deploymentId :: Term s (PAsData PByteString)
    , ptraversalNormalized'baseProvenanceIdentity :: Term s (PAsData PByteString)
    , ptraversalNormalized'envelopeBinderScriptHash :: Term s (PAsData PByteString)
    , ptraversalNormalized'traversalNormalizerScriptHash :: Term s (PAsData PByteString)
    , ptraversalNormalized'outerNormalizerScriptHash :: Term s (PAsData PByteString)
    , ptraversalNormalized'semanticExecutorScriptHash :: Term s (PAsData PByteString)
    , ptraversalNormalized'settlementScriptHash :: Term s (PAsData PByteString)
    , ptraversalNormalized'actionFamily :: Term s (PAsData PInteger)
    , ptraversalNormalized'canonicalActionHash :: Term s (PAsData PByteString)
    , ptraversalNormalized'authenticatedTraversalActionIdentity :: Term s (PAsData PByteString)
    , ptraversalNormalized'currentPendingItemControlHash :: Term s (PAsData PByteString)
    , ptraversalNormalized'expectedNextItemControlHash :: Term s (PAsData PByteString)
    , ptraversalNormalized'redeemerCount :: Term s (PAsData PInteger)
    , ptraversalNormalized'redeemerTotalCount :: Term s (PAsData PInteger)
    , ptraversalNormalized'unvalidatedOuterFields :: Term s (PAsData PUnvalidatedRedeemerItemOuterFieldsV1)
    , ptraversalNormalized'validatedTraversalControl :: Term s (PAsData PDataTraverseControlV1)
    , ptraversalNormalized'checkedTraversalControlCbor :: Term s (PAsData PByteString)
    }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTraversalNormalizedScriptSourcesRedeemerActionV1)

data PFamilyTraversalSerializationTemplateV1 (s :: S)
  = PFoldMapFrameRootTemplate
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
  | PFinalizeFrameTemplate
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFamilyTraversalSerializationTemplateV1)

data POuterNormalizedScriptSourcesRedeemerActionV1 (s :: S) =
  POuterNormalizedScriptSourcesRedeemerActionV1
    { pouterNormalized'version :: Term s (PAsData PInteger)
    , pouterNormalized'domain :: Term s (PAsData PByteString)
    , pouterNormalized'deploymentId :: Term s (PAsData PByteString)
    , pouterNormalized'baseProvenanceIdentity :: Term s (PAsData PByteString)
    , pouterNormalized'envelopeBinderScriptHash :: Term s (PAsData PByteString)
    , pouterNormalized'traversalNormalizerScriptHash :: Term s (PAsData PByteString)
    , pouterNormalized'outerNormalizerScriptHash :: Term s (PAsData PByteString)
    , pouterNormalized'semanticExecutorScriptHash :: Term s (PAsData PByteString)
    , pouterNormalized'settlementScriptHash :: Term s (PAsData PByteString)
    , pouterNormalized'actionFamily :: Term s (PAsData PInteger)
    , pouterNormalized'canonicalActionHash :: Term s (PAsData PByteString)
    , pouterNormalized'authenticatedTraversalActionIdentity :: Term s (PAsData PByteString)
    , pouterNormalized'currentPendingItemControlHash :: Term s (PAsData PByteString)
    , pouterNormalized'expectedNextItemControlHash :: Term s (PAsData PByteString)
    , pouterNormalized'redeemerCount :: Term s (PAsData PInteger)
    , pouterNormalized'redeemerTotalCount :: Term s (PAsData PInteger)
    , pouterNormalized'nextItemControlHashPrefixCbor :: Term s (PAsData PByteString)
    , pouterNormalized'validatedTraversalControl :: Term s (PAsData PDataTraverseControlV1)
    , pouterNormalized'traversalSerializationTemplate :: Term s (PAsData PFamilyTraversalSerializationTemplateV1)
    }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POuterNormalizedScriptSourcesRedeemerActionV1)

data PScriptSourcesRedeemerExecutionAttestedStateV1 (s :: S) =
  PScriptSourcesRedeemerExecutionAttestedStateV1
    { pattested'version :: Term s (PAsData PInteger)
    , pattested'domain :: Term s (PAsData PByteString)
    , pattested'deploymentId :: Term s (PAsData PByteString)
    , pattested'baseProvenanceIdentity :: Term s (PAsData PByteString)
    , pattested'envelopeBinderScriptHash :: Term s (PAsData PByteString)
    , pattested'traversalNormalizerScriptHash :: Term s (PAsData PByteString)
    , pattested'outerNormalizerScriptHash :: Term s (PAsData PByteString)
    , pattested'semanticExecutorScriptHash :: Term s (PAsData PByteString)
    , pattested'settlementScriptHash :: Term s (PAsData PByteString)
    , pattested'actionFamily :: Term s (PAsData PInteger)
    , pattested'canonicalActionHash :: Term s (PAsData PByteString)
    , pattested'authenticatedTraversalActionIdentity :: Term s (PAsData PByteString)
    , pattested'currentPendingItemControlHash :: Term s (PAsData PByteString)
    , pattested'expectedNextItemControlHash :: Term s (PAsData PByteString)
    , pattested'actualNextItemControlHash :: Term s (PAsData PByteString)
    , pattested'redeemerCount :: Term s (PAsData PInteger)
    , pattested'redeemerTotalCount :: Term s (PAsData PInteger)
    }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesRedeemerExecutionAttestedStateV1)

pnarrowPreimageHashV1 :: forall s. Term s (PData :--> PData :--> PByteString)
pnarrowPreimageHashV1 = phoistAcyclic $ plam $ \current action ->
  pblake2b_256 #$ pactionIdentityDomain <>
    (pserialiseData #$ plistData #$ pcons # current #$ pcons # action # pnil)

pcanonicalAuxiliaryHashV1 :: forall s. Term s (PData :--> PByteString)
pcanonicalAuxiliaryHashV1 = phoistAcyclic $ plam $ \auxiliary ->
  pblake2b_256 #$ pauxiliaryIdentityDomain <> (pserialiseData # auxiliary)


pcanonicalItemActionHash :: forall s. Term s (PData :--> PData :--> PInteger :--> PByteString)
pcanonicalItemActionHash = phoistAcyclic $ plam $ \current witness family ->
  plet (recordFields 3 witness) $ \f -> plet (phead # f) $ \action ->
  plet (pelemAt # 1 # f) $ \chunk -> plet (pelemAt # 2 # f) $ \nextChunk ->
  pmatch (pasConstr # action) $ \(PBuiltinPair tag fields) ->
  pif (family #>= pfoldMapFamily #&& family #<= pfinalizeFrameFamily)
    (pexpectLength fields 1 $ plet (phead # fields) $ \traversal -> pmatch (pasConstr # traversal) $ \(PBuiltinPair t items) ->
      pexpecting (tag #== 2 #&& pdataIsNone chunk #&& pdataIsNone nextChunk
        #&& pif (family #== pfoldMapFamily) (t #== 7 #&& plength # items #== 6) (t #== 8 #&& plength # items #== 2))
        (pnarrowPreimageHashV1 # current # traversal))
    (plet (pif (family #== popenHeaderFamily) (tag #== 0 #&& pnull # fields) $
      pif (family #== popenTailFamily) (tag #== 1 #&& pnull # fields) $
      pif (family #== pfinishDataFamily) (tag #== 3 #&& pnull # fields #&& pdataIsNone chunk #&& pdataIsNone nextChunk) $
      pif (family #== pinvalidFamily) ((tag #== 0 #|| tag #== 1) #&& pnull # fields) $
      pexpectLength fields 1 $ pmatch (pasConstr # (phead # fields)) $ \(PBuiltinPair t items) ->
        tag #== 2 #&& pif (family #== pheadFamily)
          ((t #== 1 #&& plength # items #== 1) #|| (t #== 2 #&& plength # items #== 1) #|| (t #== 3 #&& pnull # items) #|| (t #== 4 #&& plength # items #== 2))
          (pif (family #== pattachScalarFamily) (t #== 5 #&& plength # items #== 1 #&& pdataIsNone chunk #&& pdataIsNone nextChunk) $
           pif (family #== pfoldListFamily) (t #== 6 #&& plength # items #== 4 #&& pdataIsNone chunk #&& pdataIsNone nextChunk) $
           family #== padvanceFamily #&& t #== 0 #&& pnull # items)) $ \valid ->
      pexpecting valid (pblake2b_256 # (pactionIdentityDomain <> (pserialiseData # (plistData # (pcons # current # (pcons # action # (pcons # chunk # (pcons # nextChunk # pnil)))))))))

pcanonicalActionHashV1 :: forall s. Term s (PData :--> PInteger :--> PByteString)
pcanonicalActionHashV1 = phoistAcyclic $ plam $ \auxiliary family -> pmatch (pasConstr # auxiliary) $ \(PBuiltinPair tag f) ->
  pexpectLength f 3 $ pexpecting (tag #== 18 #&& pdataIsNone (phead # f))
    (pcanonicalItemActionHash # (pelemAt # 1 # f) # (pelemAt # 2 # f) # family)


pactionMatchesFamilyV1 :: forall s.
  Term s (PDataTraverseActionV1 :--> PInteger :--> PBool)
pactionMatchesFamilyV1 = phoistAcyclic $ plam $ \action family -> pmatch action $ \case
  PFoldMap _ _ _ _ _ _ -> family #== pfoldMapFamily
  PFinalizeFrame _ _ -> family #== pfinalizeFrameFamily
  PHeadScalar _ -> family #== pheadFamily
  PHeadSequence _ -> family #== pheadFamily
  PHeadMap -> family #== pheadFamily
  PHeadLargeConstructor _ _ -> family #== pheadFamily
  PAttachScalar _ -> family #== pattachScalarFamily
  PFoldList _ _ _ _ -> family #== pfoldListFamily
  PNoAction -> family #== padvanceFamily

pnarrowActionIsBoundV1 :: forall s.
  Term s (PData :--> PDataTraverseActionV1 :--> PInteger :--> PByteString :--> PBool)
pnarrowActionIsBoundV1 = phoistAcyclic $ plam $ \current action family expected ->
  pactionMatchesFamilyV1 # action # family
    #&& pnarrowPreimageHashV1 # current # pforgetData (pdata action) #== expected

ptraversalActionIdentityV1 :: forall s.
  Term s (PDataTraverseActionV1 :--> PByteString)
ptraversalActionIdentityV1 = phoistAcyclic $ plam $ \action ->
  pblake2b_256 #$ ptraversalActionIdentityDomain <>
    (pserialiseData # pforgetData (pdata action))

ptraversalActionIdentityIsBoundV1 :: forall s.
  Term s (PDataTraverseActionV1 :--> PInteger :--> PByteString :--> PBool)
ptraversalActionIdentityIsBoundV1 = phoistAcyclic $ plam $ \action family expected ->
  pactionMatchesFamilyV1 # action # family
    #&& plengthBS # expected #== 32
    #&& ptraversalActionIdentityV1 # action #== expected

pverifyRawEnvelopeV1 :: forall s.
  Term s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PData
        :--> PByteString
        :--> PInteger
        :--> PMaybe PScriptSourcesRedeemerEnvelopeFactsV1
    )
pverifyRawEnvelopeV1 = phoistAcyclic $
  plam $ \pre transition auxiliary expectedNextHash family ->
    pmatch transition $ \step ->
    plet (pfromData $ poneStep'workWitnessCbor step) $ \workWitnessCbor ->
    pmatch (pdeserialise # workWitnessCbor) $ \case
      PNothing -> perror
      PJust controlData ->
        plet (pasList # controlData) $ \fields ->
        pexpectLength fields 31 $
          plet (pasByteStr # (pelemAt # 0 # fields)) $ \compactCbor ->
          plet (pasByteStr # (pelemAt # 1 # fields)) $ \witnessSetCompactCbor ->
          plet (pasByteStr # (pelemAt # 2 # fields)) $ \fieldPreimageLengthsCbor ->
          plet (pasByteStr # (pelemAt # 3 # fields)) $ \contextCbor ->
          plet (pasInt # (pelemAt # 4 # fields)) $ \resolvedInputCount ->
          plet (pasByteStr # (pelemAt # 5 # fields)) $ \resolvedInputsAccumulator ->
          plet (pasInt # (pelemAt # 6 # fields)) $ \signerCount ->
          plet (pasByteStr # (pelemAt # 7 # fields)) $ \signerFrontierCommitment ->
          plet (pasInt # (pelemAt # 9 # fields)) $ \stage ->
          plet (pasInt # (pelemAt # 12 # fields)) $ \redeemerCount ->
          plet (pasByteStr # (pelemAt # 15 # fields)) $ \replayAccumulator ->
          plet (pasByteStr # (pelemAt # 16 # fields)) $ \replayScheduleHash ->
          plet (pasInt # (pelemAt # 23 # fields)) $ \outputTotalCount ->
          plet (pasInt # (pelemAt # 26 # fields)) $ \redeemerTotalCount ->
          plet (pasByteStr # (pelemAt # 29 # fields)) $ \resolutionScheduleHash ->
          plet (pasByteStr # (pelemAt # 30 # fields)) $ \currentPendingHash ->
          plet (plengthBS # workWitnessCbor) $ \workWitnessLength ->
          plet (psliceBS # 0 # (workWitnessLength - 34) # workWitnessCbor) $ \pendingPrefix ->
          plet
            ( pencodeDefiniteArrayHeader # 31
                <> (pencodeDefiniteBytes # compactCbor)
                <> (pencodeDefiniteBytes # witnessSetCompactCbor)
                <> (pencodeDefiniteBytes # fieldPreimageLengthsCbor)
                <> (pencodeDefiniteBytes # contextCbor)
                <> pcborInt resolvedInputCount
                <> (pencodeDefiniteBytes # resolvedInputsAccumulator)
                <> pcborInt signerCount
                <> (pencodeDefiniteBytes # signerFrontierCommitment)
            )
            $ \canonicalPrefixHead ->
          plet (pendingPrefix <> (pencodeDefiniteBytes # currentPendingHash)) $ \expectedWorkWitnessCbor ->
          plet (pif (family #== pfinishDataFamily)
            (pmatch (pasConstr # auxiliary) $ \(PBuiltinPair _ auxiliaryFields) -> pexpectLength auxiliaryFields 3 $
              pmatch (ItemWire.pdecodeRedeemerItemProofControl # (pelemAt # 1 # auxiliaryFields)) $ \current ->
              plet (Frame.popenFrameV1 # pre # transition # 31 # 1) $ \frame ->
              plet (Merkle.pappendLeaf # redeemerCount # (Frame.pitemFrontierV1 # frame # 13) # (Script.predeemerItemLeafHash # redeemerCount # pfromData (Item.predeemerControl'itemCommitment current))) $ \peaks ->
              Frame.pdropExtensionV1 # (Frame.preplaceItemsV1 # frame # workWitnessCbor # 12 # 2 # (pcborInt (redeemerCount + 1) <> (Merkle.pencodeFrontier # peaks))) # currentPendingHash)
            (pendingPrefix <> (pencodeDefiniteBytes # expectedNextHash))) $ \nextWorkWitnessCbor ->
          plet (pcanonicalAuxiliaryHashV1 # auxiliary) $ \auxiliaryHash ->
          plet (pcanonicalActionHashV1 # auxiliary # family) $ \actionHash ->
          pmatch pre $ \preState ->
          pmatch (pfromData $ poneStep'claimedSuccessor step) $ \successor ->
          plet
            ( pand'List
                [ plengthBS # currentPendingHash #== 32
                , plengthBS # expectedNextHash #== 32
                , psliceBS # 0 # (plengthBS # canonicalPrefixHead) # pendingPrefix #== canonicalPrefixHead
                , workWitnessCbor #== expectedWorkWitnessCbor
                , family #>= pfoldMapFamily #&& family #<= pinvalidFamily
                , stage #== 1
                , pnativeTxProofCommitmentV1
                    # compactCbor # witnessSetCompactCbor # fieldPreimageLengthsCbor
                    #== pfromData (pmachineState'transactionCommitment preState)
                , phashValidationContext # contextCbor
                    #== pfromData (pmachineState'validationContextHash preState)
                , resolvedInputCount #>= 0
                , plengthBS # resolvedInputsAccumulator #== 32
                , signerCount #>= 0
                , plengthBS # signerFrontierCommitment #== 32
                , plengthBS # replayAccumulator #== 32
                , plengthBS # replayScheduleHash #== 32
                , pnull # (pasList # (pelemAt # 19 # fields))
                , pnull # (pasList # (pelemAt # 22 # fields))
                , outputTotalCount #== 0
                , plengthBS # resolutionScheduleHash #== 32
                , prawCommonControlIsInitial
                    (pelemAt # 8 # fields)
                    (pelemAt # 10 # fields)
                    (pelemAt # 14 # fields)
                    (pelemAt # 17 # fields)
                    (pelemAt # 18 # fields)
                    (pelemAt # 20 # fields)
                    (pelemAt # 21 # fields)
                    (pelemAt # 24 # fields)
                    (pelemAt # 25 # fields)
                , redeemerCount #>= 0
                , redeemerTotalCount #> redeemerCount
                , redeemerTotalCount #<= 16384
                , pif (family #== pinvalidFamily)
                    (expectedNextHash #== pconstant "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
                      #&& prejectedSuccessorIsExact # pre # pfromData (poneStep'claimedSuccessor step) # prejectInvalidFieldType)
                    (pfromData (pmachineState'phase successor) #== pcon PScriptSources
                      #&& pfromData (pmachineState'workRoot successor) #== phashWorkWitness # pcon PScriptSources # (pfromData (pmachineState'programCounter preState) + 1) # nextWorkWitnessCbor)
                ]
            )
            $ \valid ->
              pif
                valid
                ( pcon $ PJust $ pcon $ PScriptSourcesRedeemerEnvelopeFactsV1
                    { penvelopeFacts'currentPendingItemControlHash = pdata currentPendingHash
                    , penvelopeFacts'redeemerCount = pdata redeemerCount
                    , penvelopeFacts'redeemerTotalCount = pdata redeemerTotalCount
                    , penvelopeFacts'canonicalAuxiliaryHash = pdata auxiliaryHash
                    , penvelopeFacts'canonicalActionHash = pdata actionHash
                    }
                )
                (pcon PNothing)

prawCommonControlIsInitial :: forall s.
  Term s PData -> Term s PData -> Term s PData -> Term s PData -> Term s PData ->
  Term s PData -> Term s PData -> Term s PData -> Term s PData -> Term s PBool
prawCommonControlIsInitial resolvedPeaks sourceCount replayCursor spendIndex purposeCount
  outputCursor outputCount receiveScan sourceTotalCount =
    plet (pasList # receiveScan) $ \receiveFields ->
    pexpectLength receiveFields 6 $
      pand'List
        [ pasInt # sourceTotalCount #== pasInt # sourceCount
        , pnull # (pasList # resolvedPeaks)
        , pasInt # replayCursor #== 0
        , pasInt # spendIndex #== 0
        , pasInt # purposeCount #== 0
        , pasInt # outputCursor #== 0
        , pasInt # outputCount #== 0
        , pasInt # (pelemAt # 0 # receiveFields) #== 0
        , pnull # (pasList # (pelemAt # 1 # receiveFields))
        , pasInt # (pelemAt # 2 # receiveFields) #== 0
        , pasByteStr # (pelemAt # 3 # receiveFields) #== pconstant ""
        , pasByteStr # (pelemAt # 4 # receiveFields) #== pconstant ""
        , pnull # (pasList # (pelemAt # 5 # receiveFields))
        ]


pcarrierIdentity :: forall s. Term s (PData :--> PByteString)
pcarrierIdentity = phoistAcyclic $ plam $ \base -> pblake2b_256 # (presolutionIdentityDomain <> (pserialiseData # base))



penvelopeCommitmentV1 :: forall s. Term s (PByteString :--> PInteger :--> PByteString :--> PInteger :--> PByteString :--> PByteString :--> PByteString :--> PByteString :--> PInteger :--> PInteger :--> PByteString :--> PByteString :--> PByteString :--> PByteString :--> PByteString :--> PByteString :--> PByteString)
penvelopeCommitmentV1 = phoistAcyclic $ plam $ \deployment carrier identity family auxiliaryHash actionHash currentHash nextHash count total binder traversal outer executor settlement nextDataHash ->
  pblake2b_256 # (penvelopeCommitmentDomain <> (pencodeDefiniteArrayHeader # 17)
    <> pcborInt pversion <> (pencodeDefiniteBytes # deployment) <> pcborInt carrier <> (pencodeDefiniteBytes # identity)
    <> pcborInt family <> (pencodeDefiniteBytes # auxiliaryHash) <> (pencodeDefiniteBytes # actionHash)
    <> (pencodeDefiniteBytes # currentHash) <> (pencodeDefiniteBytes # nextHash) <> pcborInt count <> pcborInt total
    <> (pencodeDefiniteBytes # binder) <> (pencodeDefiniteBytes # traversal) <> (pencodeDefiniteBytes # outer)
    <> (pencodeDefiniteBytes # executor) <> (pencodeDefiniteBytes # settlement) <> (pencodeDefiniteBytes # nextDataHash))


penvelopeStateIsBoundV1 :: forall s.
  Term s
    ( PPreparedScriptSourcesRedeemerEnvelopeV1
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
penvelopeStateIsBoundV1 = phoistAcyclic $ plam $ \state deployment binder ->
  pmatch state $ \st ->
    pfromData (penvelope'version st) #== pversion
      #&& pfromData (penvelope'domain st) #== penvelopeDomain
      #&& plengthBS # pfromData (penvelope'deploymentId st) #== 32
      #&& pfromData (penvelope'deploymentId st) #== deployment
      #&& pfromData (penvelope'carrierIdentity st) #== pcarrierIdentity # penvelope'base st
      #&& pif (pfromData (penvelope'carrier st) #== pscriptSourcesCarrier)
        (pfromData (penvelope'expectedNextControlDataHash st) #== pconstant "")
        (pfromData (penvelope'carrier st) #== pcekContextCarrier #&& plengthBS # pfromData (penvelope'expectedNextControlDataHash st) #== 32)
      #&& pfromData (penvelope'actionFamily st) #>= pfoldMapFamily #&& pfromData (penvelope'actionFamily st) #<= pinvalidFamily
      #&& plengthBS # pfromData (penvelope'canonicalAuxiliaryHash st) #== 32
      #&& plengthBS # pfromData (penvelope'canonicalActionHash st) #== 32
      #&& plengthBS # pfromData (penvelope'currentPendingItemControlHash st) #== 32
      #&& plengthBS # pfromData (penvelope'expectedNextItemControlHash st) #== 32
      #&& pfromData (penvelope'redeemerCount st) #>= 0
      #&& pfromData (penvelope'redeemerTotalCount st) #> pfromData (penvelope'redeemerCount st)
      #&& pfromData (penvelope'redeemerTotalCount st) #<= 16384
      #&& plengthBS # pfromData (penvelope'envelopeBinderScriptHash st) #== 28
      #&& pfromData (penvelope'envelopeBinderScriptHash st) #== binder
      #&& plengthBS # pfromData (penvelope'traversalNormalizerScriptHash st) #== 28
      #&& plengthBS # pfromData (penvelope'outerNormalizerScriptHash st) #== 28
      #&& plengthBS # pfromData (penvelope'semanticExecutorScriptHash st) #== 28
      #&& plengthBS # pfromData (penvelope'settlementScriptHash st) #== 28
      #&& plengthBS # pfromData (penvelope'envelopeCommitment st) #== 32
      #&& pfromData (penvelope'envelopeCommitment st)
        #== penvelopeCommitmentV1
          # pfromData (penvelope'deploymentId st)
          # pfromData (penvelope'carrier st)
          # pfromData (penvelope'carrierIdentity st)
          # pfromData (penvelope'actionFamily st)
          # pfromData (penvelope'canonicalAuxiliaryHash st)
          # pfromData (penvelope'canonicalActionHash st)
          # pfromData (penvelope'currentPendingItemControlHash st)
          # pfromData (penvelope'expectedNextItemControlHash st)
          # pfromData (penvelope'redeemerCount st)
          # pfromData (penvelope'redeemerTotalCount st)
          # pfromData (penvelope'envelopeBinderScriptHash st)
          # pfromData (penvelope'traversalNormalizerScriptHash st)
          # pfromData (penvelope'outerNormalizerScriptHash st)
          # pfromData (penvelope'semanticExecutorScriptHash st)
          # pfromData (penvelope'settlementScriptHash st)
          # pfromData (penvelope'expectedNextControlDataHash st)

pbaseProvenanceIdentityV1 :: forall s.
  Term s (PPreparedScriptSourcesRedeemerEnvelopeV1 :--> PByteString)
pbaseProvenanceIdentityV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \st ->
    pblake2b_256 #$
      pbaseProvenanceIdentityDomain
        <> (pencodeDefiniteArrayHeader # 3)
        <> pcborInt (pfromData $ penvelope'carrier st)
        <> (pencodeDefiniteBytes # pfromData (penvelope'carrierIdentity st))
        <> (pencodeDefiniteBytes # pfromData (penvelope'envelopeCommitment st))

ptraversalNormalizerRouteIsExactV1 :: forall s.
  Term s
    ( PPreparedScriptSourcesRedeemerEnvelopeV1
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
ptraversalNormalizerRouteIsExactV1 = phoistAcyclic $ plam $ \state inputScript outputScript ->
  pmatch state $ \st ->
    inputScript #== pfromData (penvelope'traversalNormalizerScriptHash st)
      #&& outputScript #== pfromData (penvelope'outerNormalizerScriptHash st)

ptraversalNormalizedStateIsBoundV1 :: forall s.
  Term s
    ( PTraversalNormalizedScriptSourcesRedeemerActionV1
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
ptraversalNormalizedStateIsBoundV1 = phoistAcyclic $ plam $ \state deployment binder traversalNormalizer ->
  pmatch state $ \st ->
    pfromData (ptraversalNormalized'version st) #== pversion
      #&& pfromData (ptraversalNormalized'domain st) #== ptraversalNormalizedDomain
      #&& plengthBS # pfromData (ptraversalNormalized'deploymentId st) #== 32
      #&& pfromData (ptraversalNormalized'deploymentId st) #== deployment
      #&& plengthBS # pfromData (ptraversalNormalized'baseProvenanceIdentity st) #== 32
      #&& pfromData (ptraversalNormalized'envelopeBinderScriptHash st) #== binder
      #&& pfromData (ptraversalNormalized'traversalNormalizerScriptHash st) #== traversalNormalizer
      #&& plengthBS # pfromData (ptraversalNormalized'outerNormalizerScriptHash st) #== 28
      #&& plengthBS # pfromData (ptraversalNormalized'semanticExecutorScriptHash st) #== 28
      #&& plengthBS # pfromData (ptraversalNormalized'settlementScriptHash st) #== 28
      #&& ( pfromData (ptraversalNormalized'actionFamily st) #== pfoldMapFamily
              #|| pfromData (ptraversalNormalized'actionFamily st) #== pfinalizeFrameFamily
          )
      #&& plengthBS # pfromData (ptraversalNormalized'canonicalActionHash st) #== 32
      #&& plengthBS # pfromData (ptraversalNormalized'authenticatedTraversalActionIdentity st) #== 32
      #&& plengthBS # pfromData (ptraversalNormalized'currentPendingItemControlHash st) #== 32
      #&& plengthBS # pfromData (ptraversalNormalized'expectedNextItemControlHash st) #== 32
      #&& pfromData (ptraversalNormalized'redeemerCount st) #>= 0
      #&& pfromData (ptraversalNormalized'redeemerTotalCount st)
        #> pfromData (ptraversalNormalized'redeemerCount st)
      #&& pfromData (ptraversalNormalized'redeemerTotalCount st) #<= 16384
      #&& plengthBS # pfromData (ptraversalNormalized'checkedTraversalControlCbor st) #> 0

pouterNormalizerRouteIsExactV1 :: forall s.
  Term s
    ( PTraversalNormalizedScriptSourcesRedeemerActionV1
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
pouterNormalizerRouteIsExactV1 = phoistAcyclic $ plam $ \state inputScript outputScript ->
  pmatch state $ \st ->
    inputScript #== pfromData (ptraversalNormalized'outerNormalizerScriptHash st)
      #&& outputScript #== pfromData (ptraversalNormalized'semanticExecutorScriptHash st)

ptraversalSerializationTemplateV1 :: forall s.
  Term s
    ( PDataTraverseControlV1
        :--> PInteger
        :--> PFamilyTraversalSerializationTemplateV1
    )
ptraversalSerializationTemplateV1 = phoistAcyclic $ plam $ \traversal family ->
  pmatch traversal $ \control ->
    pif
      (family #== pfoldMapFamily)
      ( pcon $ PFoldMapFrameRootTemplate
          ( pdata $
              pencodeDefiniteArrayHeader # 10
                <> pcborInt (pfromData $ ptraverse'version control)
                <> pcborInt (pfromData $ ptraverse'stage control)
                <> pcborInt (pfromData $ ptraverse'sourceStart control)
                <> pcborInt (pfromData $ ptraverse'sourceLength control)
                <> pcborInt (pfromData $ ptraverse'offset control)
          )
          (pdata $ pconstant "\xd8\x7a\x80\xd8\x7a\x80\xd8\x7a\x80\xd8\x7a\x80")
      ) $
      pexpecting (family #== pfinalizeFrameFamily) $
        pcon $ PFinalizeFrameTemplate
          ( pdata $
              pencodeDefiniteArrayHeader # 10
                <> pcborInt (pfromData $ ptraverse'version control)
          )
          ( pdata $
              pcborInt (pfromData $ ptraverse'sourceStart control)
                <> pcborInt (pfromData $ ptraverse'sourceLength control)
                <> pcborInt (pfromData $ ptraverse'offset control)
          )
          (pdata $ pconstant "\xd8\x7a\x80\xd8\x7a\x80\xd8\x7a\x80")

pouterNormalizedStateIsBoundV1 :: forall s.
  Term s
    ( POuterNormalizedScriptSourcesRedeemerActionV1
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
pouterNormalizedStateIsBoundV1 = phoistAcyclic $
  plam $ \state family deployment binder traversalNormalizer outerNormalizer semanticExecutor settlement ->
    pmatch state $ \st ->
      pfromData (pouterNormalized'version st) #== pversion
        #&& pfromData (pouterNormalized'domain st) #== pouterNormalizedDomain
        #&& plengthBS # pfromData (pouterNormalized'deploymentId st) #== 32
        #&& pfromData (pouterNormalized'deploymentId st) #== deployment
        #&& plengthBS # pfromData (pouterNormalized'baseProvenanceIdentity st) #== 32
        #&& plengthBS # pfromData (pouterNormalized'envelopeBinderScriptHash st) #== 28
        #&& pfromData (pouterNormalized'envelopeBinderScriptHash st) #== binder
        #&& plengthBS # pfromData (pouterNormalized'traversalNormalizerScriptHash st) #== 28
        #&& pfromData (pouterNormalized'traversalNormalizerScriptHash st) #== traversalNormalizer
        #&& plengthBS # pfromData (pouterNormalized'outerNormalizerScriptHash st) #== 28
        #&& pfromData (pouterNormalized'outerNormalizerScriptHash st) #== outerNormalizer
        #&& plengthBS # pfromData (pouterNormalized'semanticExecutorScriptHash st) #== 28
        #&& pfromData (pouterNormalized'semanticExecutorScriptHash st) #== semanticExecutor
        #&& plengthBS # pfromData (pouterNormalized'settlementScriptHash st) #== 28
        #&& pfromData (pouterNormalized'settlementScriptHash st) #== settlement
        #&& (family #== pfoldMapFamily #|| family #== pfinalizeFrameFamily)
        #&& pfromData (pouterNormalized'actionFamily st) #== family
        #&& plengthBS # pfromData (pouterNormalized'canonicalActionHash st) #== 32
        #&& plengthBS # pfromData (pouterNormalized'authenticatedTraversalActionIdentity st) #== 32
        #&& plengthBS # pfromData (pouterNormalized'currentPendingItemControlHash st) #== 32
        #&& plengthBS # pfromData (pouterNormalized'expectedNextItemControlHash st) #== 32
        #&& pfromData (pouterNormalized'redeemerCount st) #>= 0
        #&& pfromData (pouterNormalized'redeemerTotalCount st)
          #> pfromData (pouterNormalized'redeemerCount st)
        #&& pfromData (pouterNormalized'redeemerTotalCount st) #<= 16384
        #&& plengthBS # pfromData (pouterNormalized'nextItemControlHashPrefixCbor st) #> 0
        #&& pmatch (pfromData $ pouterNormalized'validatedTraversalControl st) (\control ->
          pfromData (ptraverse'stage control) #== pstageFold)
        #&& pfromData (pouterNormalized'traversalSerializationTemplate st)
          #== ptraversalSerializationTemplateV1
            # pfromData (pouterNormalized'validatedTraversalControl st)
            # family

psemanticExecutorRouteIsExactV1 :: forall s.
  Term s
    ( POuterNormalizedScriptSourcesRedeemerActionV1
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PBool
    )
psemanticExecutorRouteIsExactV1 = phoistAcyclic $
  plam $ \state family inputScript outputScript settlement -> pmatch state $ \st ->
    pfromData (pouterNormalized'actionFamily st) #== family
      #&& inputScript #== pfromData (pouterNormalized'semanticExecutorScriptHash st)
      #&& pfromData (pouterNormalized'settlementScriptHash st) #== settlement
      #&& outputScript #== pfromData (pouterNormalized'settlementScriptHash st)

pexecutionAttestedStateV1 :: forall s.
  Term s
    ( POuterNormalizedScriptSourcesRedeemerActionV1
        :--> PByteString
        :--> PScriptSourcesRedeemerExecutionAttestedStateV1
    )
pexecutionAttestedStateV1 = phoistAcyclic $ plam $ \state actualNext -> pmatch state $ \st ->
  pcon $ PScriptSourcesRedeemerExecutionAttestedStateV1
    { pattested'version = pdata pversion
    , pattested'domain = pdata pexecutionAttestedDomain
    , pattested'deploymentId = pouterNormalized'deploymentId st
    , pattested'baseProvenanceIdentity = pouterNormalized'baseProvenanceIdentity st
    , pattested'envelopeBinderScriptHash = pouterNormalized'envelopeBinderScriptHash st
    , pattested'traversalNormalizerScriptHash = pouterNormalized'traversalNormalizerScriptHash st
    , pattested'outerNormalizerScriptHash = pouterNormalized'outerNormalizerScriptHash st
    , pattested'semanticExecutorScriptHash = pouterNormalized'semanticExecutorScriptHash st
    , pattested'settlementScriptHash = pouterNormalized'settlementScriptHash st
    , pattested'actionFamily = pouterNormalized'actionFamily st
    , pattested'canonicalActionHash = pouterNormalized'canonicalActionHash st
    , pattested'authenticatedTraversalActionIdentity = pouterNormalized'authenticatedTraversalActionIdentity st
    , pattested'currentPendingItemControlHash = pouterNormalized'currentPendingItemControlHash st
    , pattested'expectedNextItemControlHash = pouterNormalized'expectedNextItemControlHash st
    , pattested'actualNextItemControlHash = pdata actualNext
    , pattested'redeemerCount = pouterNormalized'redeemerCount st
    , pattested'redeemerTotalCount = pouterNormalized'redeemerTotalCount st
    }

pexecutionAttestationIsBoundToEnvelopeV1 :: forall s.
  Term s
    ( PScriptSourcesRedeemerExecutionAttestedStateV1
        :--> PPreparedScriptSourcesRedeemerEnvelopeV1
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PByteString
        :--> PBool
    )
pexecutionAttestationIsBoundToEnvelopeV1 = phoistAcyclic $
  plam $ \state envelope deployment binder traversalNormalizer outerNormalizer executors settlement ->
    pmatch state $ \st -> pmatch envelope $ \env ->
      plet (pfromData $ pattested'actionFamily st) $ \family ->
      plet (pif (family #>= 0 #&& family #<= 9)
        (pelemAt # family # (pconstant @(PBuiltinList PInteger) [0,1,2,3,4,8,10,11,16,17])) perror) $ \first ->
      plet (pelemAt # family # (pconstant @(PBuiltinList PInteger) [0,1,2,3,7,9,10,15,16,18])) $ \last ->
      plet (pattested'semanticExecutorScriptHash st #== penvelope'semanticExecutorScriptHash env
        #&& (pfix $ \self -> plam $ \index hashes -> pmatch hashes $ \case
          PNil -> pconstant False
          PCons hash rest -> (index #>= first #&& index #<= last #&& hash #== pattested'semanticExecutorScriptHash st) #|| self # (index + 1) # rest) # 0 # executors)
        $ \selectedExecutorIsExact ->
          pand'List
            [ pfromData (pattested'version st) #== pversion
            , pfromData (pattested'domain st) #== pexecutionAttestedDomain
            , plengthBS # pfromData (pattested'deploymentId st) #== 32
            , pfromData (pattested'deploymentId st) #== deployment
            , pfromData (penvelope'deploymentId env) #== deployment
            , penvelopeStateIsBoundV1 # envelope # deployment # binder
            , plengthBS # pfromData (pattested'baseProvenanceIdentity st) #== 32
            , pfromData (pattested'baseProvenanceIdentity st) #== pbaseProvenanceIdentityV1 # envelope
            , plengthBS # pfromData (pattested'envelopeBinderScriptHash st) #== 28
            , pfromData (pattested'envelopeBinderScriptHash st) #== binder
            , pfromData (penvelope'envelopeBinderScriptHash env) #== binder
            , plengthBS # pfromData (pattested'traversalNormalizerScriptHash st) #== 28
            , pfromData (pattested'traversalNormalizerScriptHash st) #== traversalNormalizer
            , pfromData (penvelope'traversalNormalizerScriptHash env) #== traversalNormalizer
            , plengthBS # pfromData (pattested'outerNormalizerScriptHash st) #== 28
            , pfromData (pattested'outerNormalizerScriptHash st) #== outerNormalizer
            , pfromData (penvelope'outerNormalizerScriptHash env) #== outerNormalizer
            , plengthBS # pfromData (pattested'semanticExecutorScriptHash st) #== 28
            , selectedExecutorIsExact
            , plengthBS # pfromData (pattested'settlementScriptHash st) #== 28
            , pfromData (pattested'settlementScriptHash st) #== settlement
            , pfromData (penvelope'settlementScriptHash env) #== settlement
            , pattested'actionFamily st #== penvelope'actionFamily env
            , plengthBS # pfromData (pattested'canonicalActionHash st) #== 32
            , pattested'canonicalActionHash st #== penvelope'canonicalActionHash env
            , plengthBS # pfromData (pattested'authenticatedTraversalActionIdentity st) #== 32
            , plengthBS # pfromData (pattested'currentPendingItemControlHash st) #== 32
            , pattested'currentPendingItemControlHash st #== penvelope'currentPendingItemControlHash env
            , plengthBS # pfromData (pattested'expectedNextItemControlHash st) #== 32
            , pattested'expectedNextItemControlHash st #== penvelope'expectedNextItemControlHash env
            , plengthBS # pfromData (pattested'actualNextItemControlHash st) #== 32
            , pattested'actualNextItemControlHash st #== pattested'expectedNextItemControlHash st
            , pattested'redeemerCount st #== penvelope'redeemerCount env
            , pattested'redeemerTotalCount st #== penvelope'redeemerTotalCount env
            ]

pexecutionAttestationSettlementIsExactV1 :: forall s.
  Term s
    ( PScriptSourcesRedeemerExecutionAttestedStateV1
        :--> PPreparedScriptSourcesRedeemerEnvelopeV1
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PData
        :--> PBool
    )
pexecutionAttestationSettlementIsExactV1 = phoistAcyclic $
  plam $ \state envelope deployment binder traversalNormalizer outerNormalizer executors settlement award outputScript outputState ->
    pmatch envelope $ \env -> plet (decodePrepared $ penvelope'base env) $ \base ->
    pand'List
      [ pfromData (penvelope'carrier env) #== pscriptSourcesCarrier
      , ppreparedResolutionIsWellFormed # base
      , pexecutionAttestationIsBoundToEnvelopeV1
          # state
          # envelope
          # deployment
          # binder
          # traversalNormalizer
          # outerNormalizer
          # executors
          # settlement
      , outputScript #== award
      , outputState #== pforgetData (pdata pwinningResolution)
      ]

pdataIsNone :: forall s. Term s PData -> Term s PBool
pdataIsNone dat = pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
  tag #== 1 #&& pnull # fields

pexpectLength :: forall (b :: S -> Type) s.
  Term s (PBuiltinList PData) -> Integer -> Term s b -> Term s b
pexpectLength values expected result =
  pif (plength # values #== pconstant expected) result perror

pexpecting :: forall (a :: S -> Type) s. Term s PBool -> Term s a -> Term s a
pexpecting condition value = pif condition value perror
