{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageTen
Description : ScriptSources stage-ten semantic validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageTen (
  PScriptSourcesStageTenMissingActionV1 (..),
  PScriptSourcesStageTenMatchActionV1 (..),
  PScriptSourcesStageTenMismatchActionV1 (..),
  scriptSourcesStageTenMissingSemanticV1Validator,
  scriptSourcesStageTenMatchSemanticV1Validator,
  scriptSourcesStageTenMismatchSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pvalidationAuxiliaryWitnessFromData,
  pverifyScriptSourcesStageTenMatchSemanticsV1,
  pverifyScriptSourcesStageTenMismatchSemanticsV1,
  pverifyScriptSourcesStageTenMissingSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PScriptSources))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PScriptSourcesStageTenMissingActionV1 (s :: S)
  = PVerifyMissing
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageTenMissingActionV1)

data PScriptSourcesStageTenMatchActionV1 (s :: S)
  = PVerifyMatch
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PValidationAuxiliaryWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageTenMatchActionV1)

data PScriptSourcesStageTenMismatchActionV1 (s :: S)
  = PVerifyMismatch
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PValidationAuxiliaryWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageTenMismatchActionV1)

scriptSourcesStageTenMissingSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageTenMissingSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageTenMissingActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyMissing inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyScriptSourcesStageTenMissingSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

scriptSourcesStageTenMatchSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageTenMatchSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageTenMatchActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyMatch inputIndex outputIndex transitionD auxiliaryD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pvalidationAuxiliaryWitnessFromData # pforgetData auxiliaryD) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData auxiliaryD)
          ( pverifyScriptSourcesStageTenMatchSemanticsV1
              # pvalidationSemanticPreState datum # transition # auxiliary
          )
          ownOutRef txInfo

scriptSourcesStageTenMismatchSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageTenMismatchSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageTenMismatchActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyMismatch inputIndex outputIndex transitionD auxiliaryD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pvalidationAuxiliaryWitnessFromData # pforgetData auxiliaryD) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData auxiliaryD)
          ( pverifyScriptSourcesStageTenMismatchSemanticsV1
              # pvalidationSemanticPreState datum # transition # auxiliary
          )
          ownOutRef txInfo
