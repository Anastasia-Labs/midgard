{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageOne
Description : ScriptSources stage-one semantic validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageOne (
  PScriptSourcesStageOneFinishActionV1 (..),
  PScriptSourcesStageOneRedeemerActionV1 (..),
  scriptSourcesStageOneFinishSemanticV1Validator,
  scriptSourcesStageOneRedeemerSemanticV1Validator,
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

import Midgard.ComputationThread (PStepDatum)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (PNoAuxiliaryWitness),
  PValidationOneStepWitnessV1,
  pvalidationAuxiliaryWitnessFromData,
  pverifyScriptSourcesStageOneFinishRawSemanticsV1,
  pverifyScriptSourcesStageOneRedeemerSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PScriptSources))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PScriptSourcesStageOneFinishActionV1 (s :: S)
  = PVerifyFinish
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageOneFinishActionV1)

data PScriptSourcesStageOneRedeemerActionV1 (s :: S)
  = PVerifyRedeemer
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PValidationAuxiliaryWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageOneRedeemerActionV1)

pcontinueScriptSources :: forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationOneStepWitnessV1 ->
  Term s PData ->
  Term s PBool ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
pcontinueScriptSources awardScriptHash policyId datum inputIndex outputIndex transition auxiliaryData isValid ownOutRef txInfo =
  pcontinueWinning
    (pcon PScriptSources)
    awardScriptHash policyId datum inputIndex outputIndex transition
    auxiliaryData isValid ownOutRef txInfo

scriptSourcesStageOneFinishSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageOneFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageOneFinishActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyFinish inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueScriptSources
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyScriptSourcesStageOneFinishRawSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

scriptSourcesStageOneRedeemerSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageOneRedeemerSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageOneRedeemerActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyRedeemer inputIndex outputIndex transitionD auxiliaryD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pvalidationAuxiliaryWitnessFromData # pforgetData auxiliaryD) $ \auxiliary ->
        pcontinueScriptSources
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData auxiliaryD)
          ( pverifyScriptSourcesStageOneRedeemerSemanticsV1
              # pvalidationSemanticPreState datum # transition # auxiliary
          )
          ownOutRef txInfo
