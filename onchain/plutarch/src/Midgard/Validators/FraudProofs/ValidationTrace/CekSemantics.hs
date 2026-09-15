{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.CekSemantics
Description : Split CEK validation-trace semantic resolvers.

Ports the four @cek-*-semantic-v1.ak@ validators. The preparation validator in
"Midgard.Validators.FraudProofs.ValidationTrace.Cek" routes these resolvers in
finish, execution-selection, context-step, and core-step order.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.CekSemantics (
  PCekFinishActionV1 (..),
  PCekExecutionSelectionActionV1 (..),
  PCekContextStepActionV1 (..),
  PCekCoreStepActionV1 (..),
  cekFinishSemanticV1Validator,
  cekExecutionSelectionSemanticV1Validator,
  cekContextStepSemanticV1Validator,
  cekCoreStepSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Prelude

import Midgard.CekMachine (PCoreStepEvidenceV1)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepEvidenceV1 (..),
  PValidationOneStepWitnessV1,
 )
import Midgard.ValidationMachine.CekSemantics (
  pverifyCekContextStepSemanticsV1,
  pverifyCekCoreStepSemanticsV1,
  pverifyCekExecutionSelectionSemanticsV1,
  pverifyCekFinishSemanticsV1,
 )
import Midgard.ValidationMachineFieldDoor (PMachineFieldDoorV1 (..))
import Midgard.ValidationResolver (PCekMaterialRouteV1, pverifyCekRouteV1)
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PCek))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PCekFinishActionV1 (s :: S)
  = PVerifyFinish
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekFinishActionV1)

data PCekExecutionSelectionActionV1 (s :: S)
  = PVerifyExecutionSelection
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PValidationAuxiliaryWitnessV1))
      (Term s (PAsData PCekMaterialRouteV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekExecutionSelectionActionV1)

data PCekContextStepActionV1 (s :: S)
  = PVerifyContextStep
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PValidationAuxiliaryWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekContextStepActionV1)

data PCekCoreStepActionV1 (s :: S)
  = PVerifyCoreStep
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PCoreStepEvidenceV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekCoreStepActionV1)

cekFinishSemanticV1Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
cekFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PCekFinishActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PVerifyFinish inputIndex outputIndex transitionD) ->
        plet (pfromData transitionD) $ \transition ->
          plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
            pcontinueWinning
              (pcon PCek)
              awardScriptHash
              policyId
              datum
              (pfromData inputIndex)
              (pfromData outputIndex)
              transition
              (pforgetData $ pdata auxiliary)
              (pverifyCekFinishSemanticsV1 # pvalidationSemanticPreState datum # transition)
              ownOutRef
              txInfo

cekExecutionSelectionSemanticV1Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
cekExecutionSelectionSemanticV1Validator = plam $ \awardScriptHash policyId materialScriptHash ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PCekExecutionSelectionActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PVerifyExecutionSelection inputIndex outputIndex transitionD auxiliaryD materialRouteD) ->
        plet (pfromData transitionD) $ \transition ->
          plet (pfromData auxiliaryD) $ \auxiliary ->
            plet
              (pcon $ PValidationOneStepEvidenceV1 transitionD auxiliaryD)
              $ \evidence ->
                pmatch txInfo $ \PTxInfo {ptxInfo'referenceInputs} ->
                  pcontinueWinning
                    (pcon PCek)
                    awardScriptHash
                    policyId
                    datum
                    (pfromData inputIndex)
                    (pfromData outputIndex)
                    transition
                    (pforgetData auxiliaryD)
                    ( pverifyCekRouteV1
                        # evidence
                        # pfromData materialRouteD
                        # pfromData ptxInfo'referenceInputs
                        # materialScriptHash
                        #&& pverifyCekExecutionSelectionSemanticsV1
                        # pvalidationSemanticPreState datum
                        # transition
                        # auxiliary
                    )
                    ownOutRef
                    txInfo

cekContextStepSemanticV1Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
cekContextStepSemanticV1Validator = plam $ \awardScriptHash policyId certificatePolicyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PCekContextStepActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PVerifyContextStep inputIndex outputIndex transitionD auxiliaryD) ->
        plet (pfromData transitionD) $ \transition ->
          plet (pfromData auxiliaryD) $ \auxiliary ->
            pmatch txInfo $ \PTxInfo {ptxInfo'referenceInputs} ->
              plet
                ( pcon $
                    PMachineFieldDoorV1
                      (pfromData ptxInfo'referenceInputs)
                      certificatePolicyId
                )
                $ \door ->
                  pcontinueWinning
                    (pcon PCek)
                    awardScriptHash
                    policyId
                    datum
                    (pfromData inputIndex)
                    (pfromData outputIndex)
                    transition
                    (pforgetData auxiliaryD)
                    ( pverifyCekContextStepSemanticsV1
                        # pvalidationSemanticPreState datum
                        # transition
                        # auxiliary
                        # door
                    )
                    ownOutRef
                    txInfo

cekCoreStepSemanticV1Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
cekCoreStepSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PCekCoreStepActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PVerifyCoreStep inputIndex outputIndex transitionD stepD) ->
        plet (pfromData transitionD) $ \transition ->
          plet (pfromData stepD) $ \coreStep ->
            plet (pcon $ PCekCoreStepWitness stepD) $ \auxiliary ->
              pcontinueWinning
                (pcon PCek)
                awardScriptHash
                policyId
                datum
                (pfromData inputIndex)
                (pfromData outputIndex)
                transition
                (pforgetData $ pdata auxiliary)
                (pverifyCekCoreStepSemanticsV1 # pvalidationSemanticPreState datum # transition # coreStep)
                ownOutRef
                txInfo
