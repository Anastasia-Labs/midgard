{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageTwelve
Description : ScriptSources stage-twelve semantic validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageTwelve (
  PScriptSourcesStageTwelveFinishActionV1 (..),
  PScriptSourcesStageTwelveRedeemerActionV1 (..),
  scriptSourcesStageTwelveFinishSemanticV1Validator,
  scriptSourcesStageTwelveRedeemerSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pvalidationAuxiliaryWitnessFromData,
  pverifyScriptSourcesStageTwelveFinishSemanticsV1,
  pverifyScriptSourcesStageTwelveRedeemerSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PScriptSources))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PScriptSourcesStageTwelveFinishActionV1 (s :: S)
  = PVerifyFinish
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageTwelveFinishActionV1)

data PScriptSourcesStageTwelveRedeemerActionV1 (s :: S)
  = PVerifyRedeemer
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PValidationAuxiliaryWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageTwelveRedeemerActionV1)

scriptSourcesStageTwelveFinishSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageTwelveFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageTwelveFinishActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyFinish inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyScriptSourcesStageTwelveFinishSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

scriptSourcesStageTwelveRedeemerSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageTwelveRedeemerSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageTwelveRedeemerActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyRedeemer inputIndex outputIndex transitionD auxiliaryD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pvalidationAuxiliaryWitnessFromData # pforgetData auxiliaryD) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData auxiliaryD)
          ( pverifyScriptSourcesStageTwelveRedeemerSemanticsV1
              # pvalidationSemanticPreState datum # transition # auxiliary
          )
          ownOutRef txInfo
