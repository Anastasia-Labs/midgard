{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptIntegrity
Description : ScriptIntegrity validation-trace validators.

Ports the phase router and the authentication, compact, witness-set, and
finalize semantic scripts.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptIntegrity (
  PScriptIntegrityActionV1 (..),
  scriptIntegrityV1Validator,
  scriptIntegrityAuthenticationSemanticV1Validator,
  scriptIntegrityCompactSemanticV1Validator,
  scriptIntegrityWitnessSetSemanticV1Validator,
  scriptIntegrityFinalizeSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyScriptIntegrityAuthenticationSemanticsV1,
  pverifyScriptIntegrityCompactSemanticsV1,
  pverifyScriptIntegrityFinalizeSemanticsV1,
  pverifyScriptIntegrityWitnessSetSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PScriptIntegrity))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

data PScriptIntegrityActionV1 (s :: S)
  = PVerify
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptIntegrityActionV1)

scriptIntegrityV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
scriptIntegrityV1Validator = pprepareSelectedValidator (pcon PScriptIntegrity) 4

scriptIntegrityAuthenticationSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptIntegrityAuthenticationSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptIntegrityActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerify inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptIntegrity)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyScriptIntegrityAuthenticationSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

scriptIntegrityCompactSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptIntegrityCompactSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptIntegrityActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerify inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptIntegrity)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyScriptIntegrityCompactSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

scriptIntegrityWitnessSetSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptIntegrityWitnessSetSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptIntegrityActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerify inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptIntegrity)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyScriptIntegrityWitnessSetSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

scriptIntegrityFinalizeSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptIntegrityFinalizeSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptIntegrityActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerify inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptIntegrity)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyScriptIntegrityFinalizeSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo
