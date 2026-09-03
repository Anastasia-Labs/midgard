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

import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo,
  PTxOutRef,
 )
import Plutarch.Prelude

import Midgard.BoundedCollection (PItemProofV1)
import Midgard.BoundedItem (PChunkProofV1)
import Midgard.ComputationThread (PStepDatum)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepEvidenceV1 (PValidationOneStepEvidenceV1),
  PValidationOneStepWitnessV1,
  pverifyPhaseAScriptPreconditionsSemanticsV1,
 )
import Midgard.ValidationResolver (pphaseAScriptPreconditionsSemanticResolverCount)
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PPhaseAScriptPreconditions))
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
      , pverifyItem'collectionProof :: Term s (PAsData PItemProofV1)
      , pverifyItem'chunkProof :: Term s (PAsData PChunkProofV1)
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

pcontinuePreconditions :: forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationOneStepWitnessV1 ->
  Term s PValidationAuxiliaryWitnessV1 ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
pcontinuePreconditions awardScriptHash policyId datum inputIndex outputIndex transition auxiliary ownOutRef txInfo =
  plet transition $ \boundTransition ->
    plet auxiliary $ \boundAuxiliary ->
      plet
        (pcon $ PValidationOneStepEvidenceV1 (pdata boundTransition) (pdata boundAuxiliary))
        $ \evidence ->
          pcontinueWinning
            (pcon PPhaseAScriptPreconditions)
            awardScriptHash policyId datum inputIndex outputIndex boundTransition
            (pforgetData $ pdata boundAuxiliary)
            ( pverifyPhaseAScriptPreconditionsSemanticsV1
                # pvalidationSemanticPreState datum
                # evidence
            )
            ownOutRef txInfo

phaseAScriptPreconditionsSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseAScriptPreconditionsSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseAScriptPreconditionsActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerify inputIndex outputIndex transitionD) ->
      pcontinuePreconditions
        awardScriptHash policyId datum
        (pfromData inputIndex) (pfromData outputIndex)
        (pfromData transitionD) (pcon PNoAuxiliaryWitness)
        ownOutRef txInfo

phaseAScriptPreconditionsItemSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
phaseAScriptPreconditionsItemSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PPhaseAScriptPreconditionsItemActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyItem inputIndex outputIndex transitionD collectionProof chunkProof) ->
      pcontinuePreconditions
        awardScriptHash policyId datum
        (pfromData inputIndex) (pfromData outputIndex)
        (pfromData transitionD)
        (pcon $ PTransactionFieldChunkWitness collectionProof chunkProof)
        ownOutRef txInfo
