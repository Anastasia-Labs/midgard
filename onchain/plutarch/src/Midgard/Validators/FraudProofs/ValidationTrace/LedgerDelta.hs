{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.LedgerDelta
Description : LedgerDelta validation-trace validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.LedgerDelta (
  PLedgerDeltaNoAuxiliaryActionV1 (..),
  PLedgerDeltaReplayActionV1 (..),
  PLedgerDeltaOutputActionV1 (..),
  PLedgerDeltaProofFrameActionV1 (..),
  PLedgerDeltaOperationActionV1 (..),
  ledgerDeltaV1Validator,
  ledgerDeltaReplaySemanticV1Validator,
  ledgerDeltaReplayFinishSemanticV1Validator,
  ledgerDeltaOutputFinishSemanticV1Validator,
  ledgerDeltaOutputSemanticV1Validator,
  ledgerDeltaProofFrameSemanticV1Validator,
  ledgerDeltaOperationSemanticV1Validator,
  ledgerDeltaFinalizeSemanticV1Validator,
  ledgerDeltaTerminalSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.MpfProofFold (PProofFrameV1)
import Midgard.ValidationMachine (
  PLedgerDeltaOperationProofV1,
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyLedgerDeltaFinalizeSemanticsV1,
  pverifyLedgerDeltaOperationSemanticsV1,
  pverifyLedgerDeltaOutputSemanticsV1,
  pverifyLedgerDeltaOutputFinishSemanticsV1,
  pverifyLedgerDeltaProofFrameSemanticsV1,
  pverifyLedgerDeltaReplayFinishSemanticsV1,
  pverifyLedgerDeltaReplaySemanticsV1,
  pverifyLedgerDeltaTerminalSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PLedgerDelta))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

data PLedgerDeltaNoAuxiliaryActionV1 (s :: S)
  = PVerifyNoAuxiliary
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerDeltaNoAuxiliaryActionV1)

data PLedgerDeltaReplayActionV1 (s :: S)
  = PVerifyReplay
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerDeltaReplayActionV1)

data PLedgerDeltaOutputActionV1 (s :: S)
  = PVerifyOutput
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerDeltaOutputActionV1)

data PLedgerDeltaProofFrameActionV1 (s :: S)
  = PVerifyProofFrame
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PProofFrameV1))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerDeltaProofFrameActionV1)

data PLedgerDeltaOperationActionV1 (s :: S)
  = PVerifyOperation
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PLedgerDeltaOperationProofV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerDeltaOperationActionV1)

ledgerDeltaV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
ledgerDeltaV1Validator = pprepareSelectedValidator (pcon PLedgerDelta) 8

ledgerDeltaReplaySemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
ledgerDeltaReplaySemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PLedgerDeltaReplayActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyReplay inputIndex outputIndex transitionD sourceKindD keyD nextScheduleHashD valueD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        (pcon $ PLedgerDeltaReplayWitness sourceKindD keyD nextScheduleHashD valueD)
        $ \auxiliary ->
          pcontinueWinning
            (pcon PLedgerDelta)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyLedgerDeltaReplaySemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData sourceKindD # pfromData keyD
                # pfromData nextScheduleHashD # pfromData valueD
            )
            ownOutRef txInfo

ledgerDeltaReplayFinishSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
ledgerDeltaReplayFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PLedgerDeltaNoAuxiliaryActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyNoAuxiliary inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PLedgerDelta)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyLedgerDeltaReplayFinishSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

ledgerDeltaOutputFinishSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
ledgerDeltaOutputFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PLedgerDeltaNoAuxiliaryActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyNoAuxiliary inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PLedgerDelta)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyLedgerDeltaOutputFinishSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

ledgerDeltaOutputSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
ledgerDeltaOutputSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PLedgerDeltaOutputActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyOutput inputIndex outputIndex transitionD ledgerOutputIndexD descriptorCborD siblingsD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        (pcon $ PLedgerDeltaOutputWitness ledgerOutputIndexD descriptorCborD siblingsD)
        $ \auxiliary ->
          pcontinueWinning
            (pcon PLedgerDelta)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyLedgerDeltaOutputSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData ledgerOutputIndexD # pfromData descriptorCborD
                # pfromData siblingsD
            )
            ownOutRef txInfo

ledgerDeltaProofFrameSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
ledgerDeltaProofFrameSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PLedgerDeltaProofFrameActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyProofFrame inputIndex outputIndex transitionD frameD siblingsD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon $ PLedgerDeltaProofFrameWitness frameD siblingsD) $ \auxiliary ->
        pcontinueWinning
          (pcon PLedgerDelta)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          ( pverifyLedgerDeltaProofFrameSemanticsV1
              # pvalidationSemanticPreState datum # transition
              # pfromData frameD # pfromData siblingsD
          )
          ownOutRef txInfo

ledgerDeltaOperationSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
ledgerDeltaOperationSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PLedgerDeltaOperationActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyOperation inputIndex outputIndex transitionD operationKindD keyD valueD operationProofD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        (pcon $ PLedgerDeltaOperationWitness operationKindD keyD valueD operationProofD)
        $ \auxiliary ->
          pcontinueWinning
            (pcon PLedgerDelta)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyLedgerDeltaOperationSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData operationKindD # pfromData keyD # pfromData valueD
                # pfromData operationProofD
            )
            ownOutRef txInfo

ledgerDeltaFinalizeSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
ledgerDeltaFinalizeSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PLedgerDeltaNoAuxiliaryActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyNoAuxiliary inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PLedgerDelta)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyLedgerDeltaFinalizeSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

ledgerDeltaTerminalSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
ledgerDeltaTerminalSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PLedgerDeltaNoAuxiliaryActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyNoAuxiliary inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PLedgerDelta)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyLedgerDeltaTerminalSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo
