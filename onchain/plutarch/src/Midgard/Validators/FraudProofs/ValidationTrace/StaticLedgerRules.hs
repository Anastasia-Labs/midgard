{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.StaticLedgerRules
Description : StaticLedgerRules validation-trace validators.

Ports @static-ledger-rules-v1.ak@ and
@static-ledger-rules-semantic-v1.ak@.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.StaticLedgerRules (
  PVerifyStaticLedgerRulesActionV1 (..),
  staticLedgerRulesV1Validator,
  staticLedgerRulesSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (PNoAuxiliaryWitness),
  PValidationOneStepEvidenceV1 (PValidationOneStepEvidenceV1),
  PValidationOneStepWitnessV1,
  pverifyStaticLedgerRulesSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PStaticLedgerRules))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

data PVerifyStaticLedgerRulesActionV1 (s :: S)
  = PVerify
      { pverifyStatic'inputIndex :: Term s (PAsData PInteger)
      , pverifyStatic'outputIndex :: Term s (PAsData PInteger)
      , pverifyStatic'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVerifyStaticLedgerRulesActionV1)

staticLedgerRulesV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
staticLedgerRulesV1Validator = pprepareSelectedValidator (pcon PStaticLedgerRules) 1

staticLedgerRulesSemanticV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
staticLedgerRulesSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PVerifyStaticLedgerRulesActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerify inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
      plet
        ( pcon $ PValidationOneStepEvidenceV1
            (pdata transition)
            (pdata auxiliary)
        )
        $ \evidence ->
          pcontinueWinning
            (pcon PStaticLedgerRules)
            awardScriptHash
            policyId
            datum
            (pfromData inputIndex)
            (pfromData outputIndex)
            transition
            (pforgetData $ pdata auxiliary)
            (pverifyStaticLedgerRulesSemanticsV1 # pvalidationSemanticPreState datum # evidence)
            ownOutRef
            txInfo
