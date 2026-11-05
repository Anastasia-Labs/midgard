{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageEight
Description : ScriptSources stage-eight semantic validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageEight (
  PScriptSourcesStageEightFinishActionV1 (..),
  PScriptSourcesStageEightPurposeActionV1 (..),
  scriptSourcesStageEightFinishSemanticV1Validator,
  scriptSourcesStageEightPurposeSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyScriptSourcesStageEightFinishSemanticsV1,
  pverifyScriptSourcesStageEightPurposeSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PScriptSources))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PScriptSourcesStageEightFinishActionV1 (s :: S)
  = PVerifyFinish
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageEightFinishActionV1)

data PScriptSourcesStageEightPurposeActionV1 (s :: S)
  = PVerifyPurpose
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageEightPurposeActionV1)

scriptSourcesStageEightFinishSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageEightFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageEightFinishActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyFinish inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyScriptSourcesStageEightFinishSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

scriptSourcesStageEightPurposeSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageEightPurposeSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageEightPurposeActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyPurpose inputIndex outputIndex transitionD purposeKindD purposeIndexD scriptHashD subjectD siblingsD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        (pcon $ PScriptPurposeScanWitness purposeKindD purposeIndexD scriptHashD subjectD siblingsD)
        $ \auxiliary ->
          pcontinueWinning
            (pcon PScriptSources)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyScriptSourcesStageEightPurposeSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData purposeKindD # pfromData purposeIndexD
                # pfromData scriptHashD # pfromData subjectD # pfromData siblingsD
            )
            ownOutRef txInfo
