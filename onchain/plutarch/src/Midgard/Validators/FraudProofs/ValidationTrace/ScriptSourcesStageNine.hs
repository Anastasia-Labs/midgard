{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageNine
Description : ScriptSources stage-nine semantic validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageNine (
  PScriptSourcesStageNineMissingActionV1 (..),
  PScriptSourcesStageNineMismatchActionV1 (..),
  PScriptSourcesStageNineNativeMatchActionV1 (..),
  PScriptSourcesStageNineEffectfulMatchActionV1 (..),
  scriptSourcesStageNineMissingSemanticV1Validator,
  scriptSourcesStageNineMismatchSemanticV1Validator,
  scriptSourcesStageNineNativeMatchSemanticV1Validator,
  scriptSourcesStageNineEffectfulMatchSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyScriptSourcesStageNineEffectfulMatchSemanticsV1,
  pverifyScriptSourcesStageNineMismatchSemanticsV1,
  pverifyScriptSourcesStageNineMissingSemanticsV1,
  pverifyScriptSourcesStageNineNativeMatchSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PScriptSources))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PScriptSourcesStageNineMissingActionV1 (s :: S)
  = PVerifyMissing
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageNineMissingActionV1)

data PScriptSourcesStageNineMismatchActionV1 (s :: S)
  = PVerifyMismatch
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
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageNineMismatchActionV1)

data PScriptSourcesStageNineNativeMatchActionV1 (s :: S)
  = PVerifyNativeMatch
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageNineNativeMatchActionV1)

data PScriptSourcesStageNineEffectfulMatchActionV1 (s :: S)
  = PVerifyEffectfulMatch
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
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageNineEffectfulMatchActionV1)

scriptSourcesStageNineMissingSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageNineMissingSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageNineMissingActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyMissing inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyScriptSourcesStageNineMissingSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

scriptSourcesStageNineMismatchSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageNineMismatchSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageNineMismatchActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyMismatch inputIndex outputIndex transitionD sourceIndexD originKindD sourceKeyD languageTagD scriptHashD totalLengthD itemCommitmentD siblingsD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        (pcon $ PScriptSourceScanWitness sourceIndexD originKindD sourceKeyD languageTagD scriptHashD totalLengthD itemCommitmentD siblingsD)
        $ \auxiliary ->
          pcontinueWinning
            (pcon PScriptSources)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyScriptSourcesStageNineMismatchSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData sourceIndexD # pfromData originKindD
                # pfromData sourceKeyD # pfromData languageTagD
                # pfromData scriptHashD # pfromData totalLengthD
                # pfromData itemCommitmentD # pfromData siblingsD
            )
            ownOutRef txInfo

scriptSourcesStageNineNativeMatchSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageNineNativeMatchSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageNineNativeMatchActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyNativeMatch inputIndex outputIndex transitionD sourceIndexD originKindD sourceKeyD scriptHashD totalLengthD itemCommitmentD siblingsD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        (pcon $ PScriptSourceScanWitness sourceIndexD originKindD sourceKeyD (pdata 0) scriptHashD totalLengthD itemCommitmentD siblingsD)
        $ \auxiliary ->
          pcontinueWinning
            (pcon PScriptSources)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyScriptSourcesStageNineNativeMatchSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData sourceIndexD # pfromData originKindD
                # pfromData sourceKeyD # pfromData scriptHashD
                # pfromData totalLengthD # pfromData itemCommitmentD
                # pfromData siblingsD
            )
            ownOutRef txInfo

scriptSourcesStageNineEffectfulMatchSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageNineEffectfulMatchSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageNineEffectfulMatchActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyEffectfulMatch inputIndex outputIndex transitionD sourceIndexD originKindD sourceKeyD languageTagD scriptHashD totalLengthD itemCommitmentD siblingsD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        (pcon $ PScriptSourceScanWitness sourceIndexD originKindD sourceKeyD languageTagD scriptHashD totalLengthD itemCommitmentD siblingsD)
        $ \auxiliary ->
          pcontinueWinning
            (pcon PScriptSources)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyScriptSourcesStageNineEffectfulMatchSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData sourceIndexD # pfromData originKindD
                # pfromData sourceKeyD # pfromData languageTagD
                # pfromData scriptHashD # pfromData totalLengthD
                # pfromData itemCommitmentD # pfromData siblingsD
            )
            ownOutRef txInfo
