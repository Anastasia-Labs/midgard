{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeSignatures
Description : Phase-A native-script signer payload validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeSignatures (
  PPhaseANativeSignatureActionV1 (..),
  phaseANativeSignatureMembershipPayloadSemanticV1Validator,
  phaseANativeSignatureEmptyPayloadSemanticV1Validator,
  phaseANativeSignatureBelowFirstPayloadSemanticV1Validator,
  phaseANativeSignatureAboveLastPayloadSemanticV1Validator,
  phaseANativeSignatureBetweenPayloadSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.BoundedItem (PChunkProofV1)
import Midgard.ValidationMachine (
  PSignerSetProofV1 (..),
  PValidationAuxiliaryWitnessV1 (PNativeScriptTokenWitness),
  PValidationOneStepWitnessV1,
  pverifyPhaseANativeSignatureAboveLastPayloadSemanticsV1,
  pverifyPhaseANativeSignatureBelowFirstPayloadSemanticsV1,
  pverifyPhaseANativeSignatureBetweenPayloadSemanticsV1,
  pverifyPhaseANativeSignatureEmptyPayloadSemanticsV1,
  pverifyPhaseANativeSignatureMembershipPayloadSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (
  PValidationMachineStateV1,
  PValidationPhase (PPhaseANativeScripts),
 )
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PPhaseANativeSignatureActionV1 (s :: S)
  = PVerifyToken
      { ptoken'inputIndex :: Term s (PAsData PInteger)
      , ptoken'outputIndex :: Term s (PAsData PInteger)
      , ptoken'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , ptoken'chunkProof :: Term s (PAsData PChunkProofV1)
      , ptoken'nextChunkProof :: Term s (PAsData (PMaybeData PChunkProofV1))
      , ptoken'signerProof :: Term s (PAsData PSignerSetProofV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseANativeSignatureActionV1)

psignerValidator :: forall s.
  ( Term s PValidationMachineStateV1 ->
    Term s PValidationOneStepWitnessV1 ->
    Term s PChunkProofV1 ->
    Term s (PMaybeData PChunkProofV1) ->
    Term s PSignerSetProofV1 ->
    Term s PBool
  ) ->
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
psignerValidator verifyProof = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseANativeSignatureActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyToken inputIndex outputIndex transitionD chunkProofD nextChunkProofD signerProofD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pfromData chunkProofD) $ \chunkProof ->
      plet (pfromData nextChunkProofD) $ \nextChunkProof ->
      plet (pfromData signerProofD) $ \signerProof ->
      plet
        (pcon $ PNativeScriptTokenWitness chunkProofD nextChunkProofD signerProofD)
        $ \auxiliary ->
          pcontinueWinning
            (pcon PPhaseANativeScripts)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( verifyProof
                (pvalidationSemanticPreState datum)
                transition chunkProof nextChunkProof signerProof
            )
            ownOutRef txInfo

phaseANativeSignatureMembershipPayloadSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeSignatureMembershipPayloadSemanticV1Validator =
  psignerValidator $ \pre transition chunkProof nextChunkProof signerProof ->
    pmatch signerProof $ \case
      PSignerMembershipProof peaks signerIndex siblings ->
        pverifyPhaseANativeSignatureMembershipPayloadSemanticsV1
          # pre # transition # chunkProof # nextChunkProof
          # pfromData peaks # pfromData signerIndex # pfromData siblings
      _ -> perror

phaseANativeSignatureEmptyPayloadSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeSignatureEmptyPayloadSemanticV1Validator =
  psignerValidator $ \pre transition chunkProof nextChunkProof signerProof ->
    pmatch signerProof $ \case
      PEmptySignerSetProof peaks ->
        pverifyPhaseANativeSignatureEmptyPayloadSemanticsV1
          # pre # transition # chunkProof # nextChunkProof # pfromData peaks
      _ -> perror

phaseANativeSignatureBelowFirstPayloadSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeSignatureBelowFirstPayloadSemanticV1Validator =
  psignerValidator $ \pre transition chunkProof nextChunkProof signerProof ->
    pmatch signerProof $ \case
      PSignerBelowFirstProof peaks firstSignerHash siblings ->
        pverifyPhaseANativeSignatureBelowFirstPayloadSemanticsV1
          # pre # transition # chunkProof # nextChunkProof
          # pfromData peaks # pfromData firstSignerHash # pfromData siblings
      _ -> perror

phaseANativeSignatureAboveLastPayloadSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeSignatureAboveLastPayloadSemanticV1Validator =
  psignerValidator $ \pre transition chunkProof nextChunkProof signerProof ->
    pmatch signerProof $ \case
      PSignerAboveLastProof peaks lastSignerHash siblings ->
        pverifyPhaseANativeSignatureAboveLastPayloadSemanticsV1
          # pre # transition # chunkProof # nextChunkProof
          # pfromData peaks # pfromData lastSignerHash # pfromData siblings
      _ -> perror

phaseANativeSignatureBetweenPayloadSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeSignatureBetweenPayloadSemanticV1Validator =
  psignerValidator $ \pre transition chunkProof nextChunkProof signerProof ->
    pmatch signerProof $ \case
      PSignerBetweenProof peaks lowerIndex lowerSignerHash lowerSiblings upperSignerHash upperSiblings ->
        pverifyPhaseANativeSignatureBetweenPayloadSemanticsV1
          # pre # transition # chunkProof # nextChunkProof
          # pfromData peaks # pfromData lowerIndex # pfromData lowerSignerHash
          # pfromData lowerSiblings # pfromData upperSignerHash # pfromData upperSiblings
      _ -> perror
