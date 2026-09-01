{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageSeven
Description : ScriptSources stage-seven semantic validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageSeven (
  PScriptSourcesStageSevenObserverActionV1 (..),
  PScriptSourcesStageSevenReceiveActionV1 (..),
  PScriptSourcesStageSevenFinishActionV1 (..),
  scriptSourcesStageSevenObserverSemanticV1Validator,
  scriptSourcesStageSevenReceiveSemanticV1Validator,
  scriptSourcesStageSevenFinishSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.BoundedCollection (PItemProofV1)
import Midgard.BoundedItem (PChunkProofV1)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyScriptSourcesStageSevenFinishSemanticsV1,
  pverifyScriptSourcesStageSevenObserverSemanticsV1,
  pverifyScriptSourcesStageSevenReceiveSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PScriptSources))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PScriptSourcesStageSevenObserverActionV1 (s :: S)
  = PVerifyObserver
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PItemProofV1))
      (Term s (PAsData PChunkProofV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageSevenObserverActionV1)

data PScriptSourcesStageSevenReceiveActionV1 (s :: S)
  = PVerifyReceive
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageSevenReceiveActionV1)

data PScriptSourcesStageSevenFinishActionV1 (s :: S)
  = PVerifyFinish
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageSevenFinishActionV1)

scriptSourcesStageSevenObserverSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageSevenObserverSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageSevenObserverActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyObserver inputIndex outputIndex transitionD collectionProofD chunkProofD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon $ PTransactionFieldChunkWitness collectionProofD chunkProofD) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          ( pverifyScriptSourcesStageSevenObserverSemanticsV1
              # pvalidationSemanticPreState datum # transition
              # pfromData collectionProofD # pfromData chunkProofD
          )
          ownOutRef txInfo

scriptSourcesStageSevenReceiveSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageSevenReceiveSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageSevenReceiveActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyReceive inputIndex outputIndex transitionD purposeKindD purposeIndexD scriptHashD subjectD siblingsD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        (pcon $ PScriptPurposeScanWitness purposeKindD purposeIndexD scriptHashD subjectD siblingsD)
        $ \auxiliary ->
          pcontinueWinning
            (pcon PScriptSources)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyScriptSourcesStageSevenReceiveSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData purposeKindD # pfromData purposeIndexD
                # pfromData scriptHashD # pfromData subjectD # pfromData siblingsD
            )
            ownOutRef txInfo

scriptSourcesStageSevenFinishSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageSevenFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesStageSevenFinishActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyFinish inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PScriptSources)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          (pverifyScriptSourcesStageSevenFinishSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo
