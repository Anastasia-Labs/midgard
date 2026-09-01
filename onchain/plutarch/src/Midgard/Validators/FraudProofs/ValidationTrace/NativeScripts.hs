{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.NativeScripts
Description : NativeScripts validation-trace validators.

Ports the phase router and the terminal, effectful, and native semantic scripts.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.NativeScripts (
  PNativeScriptsTerminalActionV1 (..),
  PNativeScriptsEffectfulActionV1 (..),
  PNativeScriptsNativeActionV1 (..),
  nativeScriptsV1Validator,
  nativeScriptsTerminalSemanticV1Validator,
  nativeScriptsEffectfulSemanticV1Validator,
  nativeScriptsNativeSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.BoundedItem (PChunkProofV1)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyNativeScriptsEffectfulSemanticsV1,
  pverifyNativeScriptsNativeSemanticsV1,
  pverifyNativeScriptsTerminalSemanticsV1,
 )
import Midgard.ValidationMerkle (PFrontierPeak)
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PNativeScripts))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

data PNativeScriptsTerminalActionV1 (s :: S)
  = PVerifyTerminal
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptsTerminalActionV1)

data PNativeScriptsEffectfulActionV1 (s :: S)
  = PVerifyEffectful
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptsEffectfulActionV1)

data PNativeScriptsNativeActionV1 (s :: S)
  = PVerifyNative
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PChunkProofV1))
      (Term s (PAsData (PBuiltinList (PAsData PFrontierPeak))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptsNativeActionV1)

nativeScriptsV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
nativeScriptsV1Validator = pprepareSelectedValidator (pcon PNativeScripts) 3

nativeScriptsTerminalSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
nativeScriptsTerminalSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PNativeScriptsTerminalActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyTerminal inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PNativeScripts)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyNativeScriptsTerminalSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

nativeScriptsEffectfulSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
nativeScriptsEffectfulSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PNativeScriptsEffectfulActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyEffectful inputIndex outputIndex transitionD executionIndexD languageTagD purposeKindD purposeIndexD scriptHashD subjectD purposeSiblingsD sourceIndexD originKindD sourceKeyD totalLengthD itemCommitmentD sourceSiblingsD redeemerLeafD executionSiblingsD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        ( pcon $ PNativeExecutionDescriptorWitness
            executionIndexD languageTagD purposeKindD purposeIndexD
            scriptHashD subjectD purposeSiblingsD sourceIndexD originKindD
            sourceKeyD totalLengthD itemCommitmentD sourceSiblingsD
            redeemerLeafD executionSiblingsD
            (pdata $ pcon PDNothing) (pdata pnil)
        )
        $ \auxiliary ->
          pcontinueWinning
            (pcon PNativeScripts)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyNativeScriptsEffectfulSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData executionIndexD # pfromData languageTagD
                # pfromData purposeKindD # pfromData purposeIndexD
                # pfromData scriptHashD # pfromData subjectD # pfromData purposeSiblingsD
                # pfromData sourceIndexD # pfromData originKindD # pfromData sourceKeyD
                # pfromData totalLengthD # pfromData itemCommitmentD
                # pfromData sourceSiblingsD # pfromData redeemerLeafD
                # pfromData executionSiblingsD
            )
            ownOutRef txInfo

nativeScriptsNativeSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
nativeScriptsNativeSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PNativeScriptsNativeActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyNative inputIndex outputIndex transitionD executionIndexD purposeKindD purposeIndexD scriptHashD subjectD purposeSiblingsD sourceIndexD originKindD sourceKeyD totalLengthD itemCommitmentD sourceSiblingsD redeemerLeafD executionSiblingsD firstChunkProofD signerPeaksD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        ( pcon $ PNativeExecutionDescriptorWitness
            executionIndexD (pdata 0) purposeKindD purposeIndexD
            scriptHashD subjectD purposeSiblingsD sourceIndexD originKindD
            sourceKeyD totalLengthD itemCommitmentD sourceSiblingsD
            redeemerLeafD executionSiblingsD
            (pdata $ pcon $ PDJust firstChunkProofD) signerPeaksD
        )
        $ \auxiliary ->
          pcontinueWinning
            (pcon PNativeScripts)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyNativeScriptsNativeSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData executionIndexD # pfromData purposeKindD
                # pfromData purposeIndexD # pfromData scriptHashD
                # pfromData subjectD # pfromData purposeSiblingsD
                # pfromData sourceIndexD # pfromData originKindD # pfromData sourceKeyD
                # pfromData totalLengthD # pfromData itemCommitmentD
                # pfromData sourceSiblingsD # pfromData redeemerLeafD
                # pfromData executionSiblingsD # pfromData firstChunkProofD
                # pfromData signerPeaksD
            )
            ownOutRef txInfo
