{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeScripts
Description : Basic Phase-A native-script validation-trace validators.

Ports the phase router plus the advance, item, and frame semantic scripts.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeScripts (
  PPhaseANativeAdvanceActionV1 (..),
  PPhaseANativeItemActionV1 (..),
  PPhaseANativeFrameActionV1 (..),
  phaseANativeScriptsV1Validator,
  phaseANativeAdvanceSemanticV1Validator,
  phaseANativeItemSemanticV1Validator,
  phaseANativeFrameSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Prelude

import Midgard.NativeTxFieldAccess (PFieldCarriageV1)
import Midgard.NativeScriptScan (PNativeScriptFrameV1)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyPhaseANativeAdvanceSemanticsV1,
  pverifyPhaseANativeFrameSemanticsV1,
  pverifyPhaseANativeItemSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PPhaseANativeScripts))
import Midgard.ValidationMachineFieldDoor (PMachineFieldDoorV1 (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

data PPhaseANativeAdvanceActionV1 (s :: S)
  = PVerifyAdvance
      { padvance'inputIndex :: Term s (PAsData PInteger)
      , padvance'outputIndex :: Term s (PAsData PInteger)
      , padvance'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseANativeAdvanceActionV1)

data PPhaseANativeItemActionV1 (s :: S)
  = PVerifyItem
      { pitem'inputIndex :: Term s (PAsData PInteger)
      , pitem'outputIndex :: Term s (PAsData PInteger)
      , pitem'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , pitem'fieldIndex :: Term s (PAsData PInteger)
      , pitem'itemIndex :: Term s (PAsData PInteger)
      , pitem'carriage :: Term s (PAsData PFieldCarriageV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseANativeItemActionV1)

data PPhaseANativeFrameActionV1 (s :: S)
  = PVerifyFrame
      { pframe'inputIndex :: Term s (PAsData PInteger)
      , pframe'outputIndex :: Term s (PAsData PInteger)
      , pframe'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , pframe'frame :: Term s (PAsData PNativeScriptFrameV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseANativeFrameActionV1)

phaseANativeScriptsV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
phaseANativeScriptsV1Validator =
  pprepareSelectedValidator (pcon PPhaseANativeScripts) 14

phaseANativeAdvanceSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeAdvanceSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseANativeAdvanceActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyAdvance inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PPhaseANativeScripts)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyPhaseANativeAdvanceSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

phaseANativeItemSemanticV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit
    )
phaseANativeItemSemanticV1Validator = plam $ \awardScriptHash policyId certificatePolicyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseANativeItemActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyItem inputIndex outputIndex transitionD fieldIndexD itemIndexD carriageD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon $ PTransactionFieldChunkWitness fieldIndexD itemIndexD carriageD) $ \auxiliary ->
      pmatch txInfo $ \PTxInfo {ptxInfo'referenceInputs} ->
      plet (pcon $ PMachineFieldDoorV1 (pfromData ptxInfo'referenceInputs) certificatePolicyId) $ \door ->
        pcontinueWinning
          (pcon PPhaseANativeScripts)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          ( pverifyPhaseANativeItemSemanticsV1
              # pvalidationSemanticPreState datum
              # transition # door # pfromData fieldIndexD # pfromData itemIndexD
              # pfromData carriageD
          )
          ownOutRef txInfo

phaseANativeFrameSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseANativeFrameSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseANativeFrameActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyFrame inputIndex outputIndex transitionD frameD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pfromData frameD) $ \frame ->
      plet (pcon $ PNativeScriptFrameWitness frameD) $ \auxiliary ->
        pcontinueWinning
          (pcon PPhaseANativeScripts)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          ( pverifyPhaseANativeFrameSemanticsV1
              # pvalidationSemanticPreState datum
              # transition # frame
          )
          ownOutRef txInfo
