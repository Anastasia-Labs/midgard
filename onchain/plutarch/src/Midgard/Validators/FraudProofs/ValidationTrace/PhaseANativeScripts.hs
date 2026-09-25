{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeScripts
Description : Basic Phase-A native-script validation-trace validators.

Ports the phase router plus the advance, item, and frame semantic scripts.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeScripts (
  PPhaseANativeAdvanceActionV1 (..),
  PPhaseANativeItemActionV1 (..),
  PPhaseANativeFrameActionV1 (..),
  phaseANativeScriptsV1Validator,
  phaseANativeAdvanceSemanticV1Validator,
  phaseANativeItemSemanticV1Validator,
  phaseANativeFrameSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.PhaseANativeItem qualified as Item
import Midgard.PhaseANativeItemYield (PPhaseANativeItemActionV1 (..), nativeRole, foreignRole)
import Midgard.StateQueueYield qualified as Yield
import Midgard.NativeScriptScan (PNativeScriptFrameV1)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyPhaseANativeAdvanceSemanticsV1,
  pverifyPhaseANativeFrameSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PPhaseANativeScripts))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

data PPhaseANativeAdvanceActionV1 (s :: S)
  = PVerifyAdvance
      { padvance'inputIndex :: Term s (PAsData PInteger)
      , padvance'outputIndex :: Term s (PAsData PInteger)
      , padvance'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseANativeAdvanceActionV1)

data PPhaseANativeFrameActionV1 (s :: S)
  = PVerifyFrame
      { pframe'inputIndex :: Term s (PAsData PInteger)
      , pframe'outputIndex :: Term s (PAsData PInteger)
      , pframe'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , pframe'frame :: Term s (PAsData PNativeScriptFrameV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseANativeFrameActionV1)

phaseANativeScriptsV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
phaseANativeScriptsV1Validator =
  pprepareSelectedValidator (pcon PPhaseANativeScripts) 14

phaseANativeAdvanceSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeAdvanceSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseANativeAdvanceActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyAdvance inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PPhaseANativeScripts)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyPhaseANativeAdvanceSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

phaseANativeItemSemanticV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit
    )
phaseANativeItemSemanticV1Validator = plam $ \awardScriptHash policyId referenceScriptAuthPolicyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseANativeItemActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyItem inputIndex outputIndex transitionD fieldIndexD itemIndexD carriageD yieldIndexD yieldKindD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon $ PTransactionFieldChunkWitness fieldIndexD itemIndexD carriageD) $ \auxiliary ->
      plet (pfromData yieldKindD) $ \kind ->
      pif (kind #== 0 #|| kind #== 1)
        (plet (Yield.prequireAuthenticatedZeroYield # txInfo # pfromData referenceScriptAuthPolicyId
          # pif (kind #== 0) nativeRole foreignRole # pfromData yieldIndexD) $ \_ ->
        pcontinueWinning
          (pcon PPhaseANativeScripts)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          ( Item.pdispatch
              # pvalidationSemanticPreState datum
              # transition # pfromData fieldIndexD # pfromData itemIndexD
          )
          ownOutRef txInfo)
        perror

phaseANativeFrameSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeFrameSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseANativeFrameActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyFrame inputIndex outputIndex transitionD frameD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pfromData frameD) $ \frame ->
      plet (pcon $ PNativeScriptFrameWitness frameD) $ \auxiliary ->
        pcontinueWinning
          (pcon PPhaseANativeScripts)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          ( pverifyPhaseANativeFrameSemanticsV1
              # pvalidationSemanticPreState datum
              # transition # frame
          )
          ownOutRef txInfo
