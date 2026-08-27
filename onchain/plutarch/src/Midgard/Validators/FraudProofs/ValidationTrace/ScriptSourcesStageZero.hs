{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageZero
Description : ScriptSources router and stage-zero semantic validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageZero (
  PScriptSourcesStageZeroFinishActionV1 (..),
  PScriptSourcesStageZeroBeginActionV1 (..),
  PScriptSourcesStageZeroHashBlockActionV1 (..),
  PScriptSourcesStageZeroHashAdvanceActionV1 (..),
  PScriptSourcesStageZeroHashTerminalActionV1 (..),
  scriptSourcesV1Validator,
  scriptSourcesStageZeroFinishSemanticV1Validator,
  scriptSourcesStageZeroBeginSemanticV1Validator,
  scriptSourcesStageZeroHashBlockSemanticV1Validator,
  scriptSourcesStageZeroHashAdvanceSemanticV1Validator,
  scriptSourcesStageZeroHashTerminalSemanticV1Validator,
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
  PValidationOneStepWitnessV1,
  pverifyScriptSourcesStageZeroBeginSemanticsV1,
  pverifyScriptSourcesStageZeroFinishSemanticsV1,
  pverifyScriptSourcesStageZeroHashAdvanceSemanticsV1,
  pverifyScriptSourcesStageZeroHashBlockSemanticsV1,
  pverifyScriptSourcesStageZeroHashTerminalSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PScriptSources))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

data PScriptSourcesStageZeroFinishActionV1 (s :: S)
  = PVerifyFinish
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageZeroFinishActionV1)

data PScriptSourcesStageZeroBeginActionV1 (s :: S)
  = PVerifyBegin
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PItemProofV1))
      (Term s (PAsData PChunkProofV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageZeroBeginActionV1)

data PScriptSourcesStageZeroHashBlockActionV1 (s :: S)
  = PVerifyHashBlock
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PChunkProofV1))
      (Term s (PAsData (PMaybeData PChunkProofV1)))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageZeroHashBlockActionV1)

data PScriptSourcesStageZeroHashAdvanceActionV1 (s :: S)
  = PVerifyHashAdvance
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageZeroHashAdvanceActionV1)

data PScriptSourcesStageZeroHashTerminalActionV1 (s :: S)
  = PVerifyHashTerminal
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageZeroHashTerminalActionV1)

scriptSourcesV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
scriptSourcesV1Validator = pprepareSelectedValidator (pcon PScriptSources) 29

pcontinueScriptSources :: forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationOneStepWitnessV1 ->
  Term s PValidationAuxiliaryWitnessV1 ->
  Term s PBool ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
pcontinueScriptSources awardScriptHash policyId datum inputIndex outputIndex transition auxiliary isValid ownOutRef txInfo =
  pcontinueWinning
    (pcon PScriptSources)
    awardScriptHash policyId datum inputIndex outputIndex transition
    (pforgetData $ pdata auxiliary)
    isValid ownOutRef txInfo

scriptSourcesStageZeroFinishSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageZeroFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageZeroFinishActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyFinish inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
        pcontinueScriptSources
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pcon PNoAuxiliaryWitness)
          (pverifyScriptSourcesStageZeroFinishSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

scriptSourcesStageZeroBeginSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageZeroBeginSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageZeroBeginActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyBegin inputIndex outputIndex transitionD collectionProofD chunkProofD) ->
      plet (pfromData transitionD) $ \transition ->
        pcontinueScriptSources
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pcon $ PTransactionFieldChunkWitness collectionProofD chunkProofD)
          ( pverifyScriptSourcesStageZeroBeginSemanticsV1
              # pvalidationSemanticPreState datum # transition
              # pfromData collectionProofD # pfromData chunkProofD
          )
          ownOutRef txInfo

scriptSourcesStageZeroHashBlockSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageZeroHashBlockSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageZeroHashBlockActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyHashBlock inputIndex outputIndex transitionD chunkProofD nextChunkProofD) ->
      plet (pfromData transitionD) $ \transition ->
        pcontinueScriptSources
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pcon $ PScriptSourceHashBlockWitness chunkProofD nextChunkProofD)
          ( pverifyScriptSourcesStageZeroHashBlockSemanticsV1
              # pvalidationSemanticPreState datum # transition
              # pfromData chunkProofD # pfromData nextChunkProofD
          )
          ownOutRef txInfo

scriptSourcesStageZeroHashAdvanceSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageZeroHashAdvanceSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageZeroHashAdvanceActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyHashAdvance inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
        pcontinueScriptSources
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pcon PNoAuxiliaryWitness)
          (pverifyScriptSourcesStageZeroHashAdvanceSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

scriptSourcesStageZeroHashTerminalSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageZeroHashTerminalSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageZeroHashTerminalActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyHashTerminal inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
        pcontinueScriptSources
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pcon PNoAuxiliaryWitness)
          (pverifyScriptSourcesStageZeroHashTerminalSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo
