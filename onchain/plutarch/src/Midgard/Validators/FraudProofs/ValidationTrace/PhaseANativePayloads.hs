{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativePayloads
Description : Phase-A native-script token payload validators.

Ports token-head, timelock, and container payload semantic scripts.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativePayloads (
  PPhaseANativeTokenActionV1 (..),
  PPhaseANativePayloadActionV1 (..),
  phaseANativeTokenHeadSemanticV1Validator,
  phaseANativeTimelockPayloadSemanticV1Validator,
  phaseANativeAllOrAnyContainerFramePayloadSemanticV1Validator,
  phaseANativeAllOrAnyEmptyContainerPayloadSemanticV1Validator,
  phaseANativeAtLeastContainerFramePayloadSemanticV1Validator,
  phaseANativeAtLeastEmptyContainerPayloadSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo,
  PTxOutRef,
 )
import Plutarch.Prelude

import Midgard.BoundedItem (PChunkProofV1)
import Midgard.ComputationThread (PStepDatum)
import Midgard.ValidationMachine (
  PSignerSetProofV1 (PNoSignerSetProof),
  PValidationAuxiliaryWitnessV1 (PNativeScriptTokenWitness),
  PValidationOneStepWitnessV1,
  pverifyPhaseANativeAllOrAnyContainerFramePayloadSemanticsV1,
  pverifyPhaseANativeAllOrAnyEmptyContainerPayloadSemanticsV1,
  pverifyPhaseANativeAtLeastContainerFramePayloadSemanticsV1,
  pverifyPhaseANativeAtLeastEmptyContainerPayloadSemanticsV1,
  pverifyPhaseANativeTimelockPayloadSemanticsV1,
  pverifyPhaseANativeTokenHeadSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (
  PValidationMachineStateV1,
  PValidationPhase (PPhaseANativeScripts),
 )
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PPhaseANativeTokenActionV1 (s :: S)
  = PVerifyToken
      { ptoken'inputIndex :: Term s (PAsData PInteger)
      , ptoken'outputIndex :: Term s (PAsData PInteger)
      , ptoken'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , ptoken'chunkProof :: Term s (PAsData PChunkProofV1)
      , ptoken'nextChunkProof :: Term s (PAsData (PMaybeData PChunkProofV1))
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseANativeTokenActionV1)

data PPhaseANativePayloadActionV1 (s :: S)
  = PVerifyPayload
      { ppayload'inputIndex :: Term s (PAsData PInteger)
      , ppayload'outputIndex :: Term s (PAsData PInteger)
      , ppayload'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , ppayload'chunkProof :: Term s (PAsData PChunkProofV1)
      , ppayload'nextChunkProof :: Term s (PAsData (PMaybeData PChunkProofV1))
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseANativePayloadActionV1)

pcontinueTokenPayload :: forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationOneStepWitnessV1 ->
  Term s (PAsData PChunkProofV1) ->
  Term s (PAsData (PMaybeData PChunkProofV1)) ->
  Term s PBool ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
pcontinueTokenPayload awardScriptHash policyId datum inputIndex outputIndex transition chunkProofD nextChunkProofD isValid ownOutRef txInfo =
  plet (pdata $ pcon PNoSignerSetProof) $ \noSignerProofD ->
  plet
    (pcon $ PNativeScriptTokenWitness chunkProofD nextChunkProofD noSignerProofD)
    $ \auxiliary ->
      pcontinueWinning
        (pcon PPhaseANativeScripts)
        awardScriptHash policyId datum inputIndex outputIndex transition
        (pforgetData $ pdata auxiliary)
        isValid ownOutRef txInfo

phaseANativeTokenHeadSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeTokenHeadSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseANativeTokenActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyToken inputIndex outputIndex transitionD chunkProofD nextChunkProofD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pfromData chunkProofD) $ \chunkProof ->
      plet (pfromData nextChunkProofD) $ \nextChunkProof ->
        pcontinueTokenPayload
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex)
          transition chunkProofD nextChunkProofD
          ( pverifyPhaseANativeTokenHeadSemanticsV1
              # pvalidationSemanticPreState datum
              # transition # chunkProof # nextChunkProof
          )
          ownOutRef txInfo

phaseANativeTimelockPayloadSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeTimelockPayloadSemanticV1Validator =
  ptokenValidator pverifyPhaseANativeTimelockPayloadSemanticsV1

phaseANativeAllOrAnyContainerFramePayloadSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeAllOrAnyContainerFramePayloadSemanticV1Validator =
  ptokenValidator pverifyPhaseANativeAllOrAnyContainerFramePayloadSemanticsV1

phaseANativeAllOrAnyEmptyContainerPayloadSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeAllOrAnyEmptyContainerPayloadSemanticV1Validator =
  ptokenValidator pverifyPhaseANativeAllOrAnyEmptyContainerPayloadSemanticsV1

phaseANativeAtLeastContainerFramePayloadSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeAtLeastContainerFramePayloadSemanticV1Validator =
  ppayloadValidator pverifyPhaseANativeAtLeastContainerFramePayloadSemanticsV1

phaseANativeAtLeastEmptyContainerPayloadSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeAtLeastEmptyContainerPayloadSemanticV1Validator =
  ppayloadValidator pverifyPhaseANativeAtLeastEmptyContainerPayloadSemanticsV1

ptokenValidator :: forall s.
  Term s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PChunkProofV1
        :--> PMaybeData PChunkProofV1
        :--> PBool
    ) ->
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
ptokenValidator verifier = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseANativeTokenActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyToken inputIndex outputIndex transitionD chunkProofD nextChunkProofD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pfromData chunkProofD) $ \chunkProof ->
      plet (pfromData nextChunkProofD) $ \nextChunkProof ->
        pcontinueTokenPayload
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex)
          transition chunkProofD nextChunkProofD
          ( verifier
              # pvalidationSemanticPreState datum
              # transition # chunkProof # nextChunkProof
          )
          ownOutRef txInfo

ppayloadValidator :: forall s.
  Term s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PChunkProofV1
        :--> PMaybeData PChunkProofV1
        :--> PBool
    ) ->
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
ppayloadValidator verifier = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseANativePayloadActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyPayload inputIndex outputIndex transitionD chunkProofD nextChunkProofD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pfromData chunkProofD) $ \chunkProof ->
      plet (pfromData nextChunkProofD) $ \nextChunkProof ->
        pcontinueTokenPayload
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex)
          transition chunkProofD nextChunkProofD
          ( verifier
              # pvalidationSemanticPreState datum
              # transition # chunkProof # nextChunkProof
          )
          ownOutRef txInfo
