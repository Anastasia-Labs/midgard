{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageEleven
Description : ScriptSources stage-eleven semantic validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageEleven (
  PScriptSourcesStageElevenFinishActionV1 (..),
  PScriptSourcesStageElevenSourceActionV1 (..),
  scriptSourcesStageElevenFinishSemanticV1Validator,
  scriptSourcesStageElevenSourceSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyScriptSourcesStageElevenFinishSemanticsV1,
  pverifyScriptSourcesStageElevenSourceSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PScriptSources))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PScriptSourcesStageElevenFinishActionV1 (s :: S)
  = PVerifyFinish
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageElevenFinishActionV1)

data PScriptSourcesStageElevenSourceActionV1 (s :: S)
  = PVerifySource
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageElevenSourceActionV1)

scriptSourcesStageElevenFinishSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageElevenFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageElevenFinishActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyFinish inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyScriptSourcesStageElevenFinishSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

scriptSourcesStageElevenSourceSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageElevenSourceSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageElevenSourceActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifySource inputIndex outputIndex transitionD sourceIndexD originKindD sourceKeyD languageTagD scriptHashD totalLengthD itemCommitmentD siblingsD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        (pcon $ PScriptSourceScanWitness sourceIndexD originKindD sourceKeyD languageTagD scriptHashD totalLengthD itemCommitmentD siblingsD)
        $ \auxiliary ->
          pcontinueWinning
            (pcon PScriptSources)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyScriptSourcesStageElevenSourceSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData sourceIndexD # pfromData originKindD
                # pfromData sourceKeyD # pfromData languageTagD
                # pfromData scriptHashD # pfromData totalLengthD
                # pfromData itemCommitmentD # pfromData siblingsD
            )
            ownOutRef txInfo
