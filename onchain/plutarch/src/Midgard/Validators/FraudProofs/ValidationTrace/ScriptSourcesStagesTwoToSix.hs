{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStagesTwoToSix
Description : ScriptSources non-output and output-proof semantic validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStagesTwoToSix (
  PScriptSourcesNonOutputActionV1 (..),
  PScriptSourcesOutputProofBeginActionV1 (..),
  PScriptSourcesOutputProofStepActionV1 (..),
  PScriptSourcesOutputProofFinalizeActionV1 (..),
  PScriptSourcesOutputProofFinishActionV1 (..),
  scriptSourcesNonOutputSemanticV1Validator,
  scriptSourcesOutputProofBeginSemanticV1Validator,
  scriptSourcesOutputProofStepSemanticV1Validator,
  scriptSourcesOutputProofFinalizeSemanticV1Validator,
  scriptSourcesOutputProofFinishSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.LedgerOutputProof (PLedgerOutputProofWitnessV1)
import Midgard.ValidationMachine (
  PSignerSetProofV1,
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepEvidenceV1 (..),
  PValidationOneStepWitnessV1,
  pverifyScriptSourcesNonOutputSemanticsV1,
  pverifyScriptSourcesOutputProofBeginSemanticsV1,
  pverifyScriptSourcesOutputProofFinalizeSemanticsV1,
  pverifyScriptSourcesOutputProofStepSemanticsV1,
  pverifyScriptSourcesStageFiveFinishSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PScriptSources))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PScriptSourcesNonOutputActionV1 (s :: S)
  = PVerifyNonOutput
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PValidationAuxiliaryWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesNonOutputActionV1)

data PScriptSourcesOutputProofBeginActionV1 (s :: S)
  = PVerifyOutputProofBegin
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesOutputProofBeginActionV1)

data PScriptSourcesOutputProofStepActionV1 (s :: S)
  = PVerifyOutputProofStep
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PLedgerOutputProofWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesOutputProofStepActionV1)

data PScriptSourcesOutputProofFinalizeActionV1 (s :: S)
  = PVerifyOutputProofFinalize
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PByteString))
      (Term s (PAsData PSignerSetProofV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesOutputProofFinalizeActionV1)

data PScriptSourcesOutputProofFinishActionV1 (s :: S)
  = PVerifyOutputProofFinish
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesOutputProofFinishActionV1)

scriptSourcesNonOutputSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesNonOutputSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesNonOutputActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyNonOutput inputIndex outputIndex transitionD auxiliaryD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon $ PValidationOneStepEvidenceV1 transitionD auxiliaryD) $ \evidence ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData auxiliaryD)
          (pverifyScriptSourcesNonOutputSemanticsV1 # pvalidationSemanticPreState datum # evidence)
          ownOutRef txInfo

scriptSourcesOutputProofBeginSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesOutputProofBeginSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesOutputProofBeginActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyOutputProofBegin inputIndex outputIndex transitionD ledgerOutputIndexD totalLengthD itemCommitmentD siblingsD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        (pcon $ PLedgerOutputProofBeginWitness ledgerOutputIndexD totalLengthD itemCommitmentD siblingsD)
        $ \auxiliary ->
          pcontinueWinning
            (pcon PScriptSources)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyScriptSourcesOutputProofBeginSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData ledgerOutputIndexD # pfromData totalLengthD
                # pfromData itemCommitmentD # pfromData siblingsD
            )
            ownOutRef txInfo

scriptSourcesOutputProofStepSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesOutputProofStepSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesOutputProofStepActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyOutputProofStep inputIndex outputIndex transitionD proofWitnessD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon $ PLedgerOutputProofStepWitness proofWitnessD) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          ( pverifyScriptSourcesOutputProofStepSemanticsV1
              # pvalidationSemanticPreState datum # transition # pfromData proofWitnessD
          )
          ownOutRef txInfo

scriptSourcesOutputProofFinalizeSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesOutputProofFinalizeSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesOutputProofFinalizeActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyOutputProofFinalize inputIndex outputIndex transitionD descriptorCborD signerProofD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon $ PLedgerOutputProofFinalizeWitness descriptorCborD signerProofD) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          ( pverifyScriptSourcesOutputProofFinalizeSemanticsV1
              # pvalidationSemanticPreState datum # transition
              # pfromData descriptorCborD # pfromData signerProofD
          )
          ownOutRef txInfo

scriptSourcesOutputProofFinishSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesOutputProofFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesOutputProofFinishActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyOutputProofFinish inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyScriptSourcesStageFiveFinishSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo
