{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.CompactBinding
Description : CompactBinding validation-trace validators.

Ports @compact-binding-v1.ak@ and @compact-binding-semantic-v1.ak@.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.CompactBinding (
  PVerifyCompactBindingActionV1 (..),
  compactBindingV1Validator,
  compactBindingSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (PNoAuxiliaryWitness),
  PValidationOneStepEvidenceV1 (PValidationOneStepEvidenceV1),
  PValidationOneStepWitnessV1,
  pverifyCompactBindingSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (
  PValidationPhase (PCompactBinding),
 )
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

data PVerifyCompactBindingActionV1 (s :: S)
  = PVerifyCompactBinding
      { pverifyCompactBinding'inputIndex :: Term s (PAsData PInteger)
      , pverifyCompactBinding'outputIndex :: Term s (PAsData PInteger)
      , pverifyCompactBinding'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVerifyCompactBindingActionV1)

compactBindingV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
compactBindingV1Validator = pprepareSelectedValidator (pcon PCompactBinding) 1

compactBindingSemanticV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
compactBindingSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PVerifyCompactBindingActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyCompactBinding inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
      plet
        ( pcon $ PValidationOneStepEvidenceV1
            (pdata transition)
            (pdata auxiliary)
        )
        $ \evidence ->
          pcontinueWinning
            (pcon PCompactBinding)
            awardScriptHash
            policyId
            datum
            (pfromData inputIndex)
            (pfromData outputIndex)
            transition
            (pforgetData $ pdata auxiliary)
            (pverifyCompactBindingSemanticsV1 # pvalidationSemanticPreState datum # evidence)
            ownOutRef
            txInfo
