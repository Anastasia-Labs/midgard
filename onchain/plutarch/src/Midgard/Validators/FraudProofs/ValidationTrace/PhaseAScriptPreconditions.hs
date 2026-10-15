{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.PhaseAScriptPreconditions
Description : Phase-A script-precondition validation-trace validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.PhaseAScriptPreconditions (
  PPhaseAScriptPreconditionsActionV1 (..),
  PPhaseAScriptPreconditionsItemActionV1 (..),
  phaseAScriptPreconditionsV1Validator,
  phaseAScriptPreconditionsSemanticV1Validator,
  phaseAScriptPreconditionsItemSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
 )
import Plutarch.Prelude

import Midgard.NativeTxFieldAccess (PFieldCarriageV1)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyPhaseAScriptPreconditionsFinalizeSemanticsV1,
  pverifyPhaseAScriptPreconditionsItemSemanticsV1,
 )
import Midgard.ValidationResolver (pphaseAScriptPreconditionsSemanticResolverCount)
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PPhaseAScriptPreconditions))
import Midgard.ValidationMachineFieldDoor (PMachineFieldDoorV1 (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

data PPhaseAScriptPreconditionsActionV1 (s :: S)
  = PVerify
      { pverify'inputIndex :: Term s (PAsData PInteger)
      , pverify'outputIndex :: Term s (PAsData PInteger)
      , pverify'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseAScriptPreconditionsActionV1)

data PPhaseAScriptPreconditionsItemActionV1 (s :: S)
  = PVerifyItem
      { pverifyItem'inputIndex :: Term s (PAsData PInteger)
      , pverifyItem'outputIndex :: Term s (PAsData PInteger)
      , pverifyItem'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , pverifyItem'fieldIndex :: Term s (PAsData PInteger)
      , pverifyItem'itemIndex :: Term s (PAsData PInteger)
      , pverifyItem'carriage :: Term s (PAsData PFieldCarriageV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseAScriptPreconditionsItemActionV1)

phaseAScriptPreconditionsV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
phaseAScriptPreconditionsV1Validator =
  pprepareSelectedValidator
    (pcon PPhaseAScriptPreconditions)
    pphaseAScriptPreconditionsSemanticResolverCount

phaseAScriptPreconditionsSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseAScriptPreconditionsSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseAScriptPreconditionsActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerify inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      pcontinueWinning (pcon PPhaseAScriptPreconditions) awardScriptHash policyId datum
        (pfromData inputIndex) (pfromData outputIndex) transition
        (pforgetData $ pdata $ pcon PNoAuxiliaryWitness)
        (pverifyPhaseAScriptPreconditionsFinalizeSemanticsV1 # pvalidationSemanticPreState datum # transition)
        ownOutRef txInfo

phaseAScriptPreconditionsItemSemanticV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit
    )
phaseAScriptPreconditionsItemSemanticV1Validator = plam $ \awardScriptHash policyId certificatePolicyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseAScriptPreconditionsItemActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyItem inputIndex outputIndex transitionD fieldIndex itemIndex carriage) ->
      plet (pfromData transitionD) $ \transition ->
      pmatch txInfo $ \PTxInfo {ptxInfo'referenceInputs} ->
      plet (pcon $ PMachineFieldDoorV1 (pfromData ptxInfo'referenceInputs) certificatePolicyId) $ \door ->
      pcontinueWinning (pcon PPhaseAScriptPreconditions) awardScriptHash policyId datum
        (pfromData inputIndex) (pfromData outputIndex) transition
        (pforgetData $ pdata $ pcon $ PTransactionFieldChunkWitness fieldIndex itemIndex carriage)
        (pverifyPhaseAScriptPreconditionsItemSemanticsV1 # pvalidationSemanticPreState datum # transition # door
          # pfromData fieldIndex # pfromData itemIndex # pfromData carriage)
        ownOutRef txInfo
