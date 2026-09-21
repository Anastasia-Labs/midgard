{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.CekSemantics
Description : Split CEK validation-trace semantic resolvers.

Ports the four @cek-*-semantic-v1.ak@ validators. The preparation validator in
"Midgard.Validators.FraudProofs.ValidationTrace.Cek" routes these resolvers in
finish, execution-selection, context-step, and core-step order.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.CekSemantics (
    PCekFinishActionV1 (..),
    cekFinishSemanticV1Validator,
    cekExecutionSelectionSemanticV1Validator,
    cekContextStepSemanticV1Validator,
    cekCoreStepSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.ValidationMachine (
    PValidationAuxiliaryWitnessV1 (..),
    PValidationOneStepWitnessV1,
 )
import Midgard.ValidationMachine.CekSemantics (
    pverifyCekFinishSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PCek))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.CekContext qualified as Context
import Midgard.Validators.FraudProofs.ValidationTrace.CekCore qualified as Core
import Midgard.Validators.FraudProofs.ValidationTrace.CekSelection qualified as Selection

data PCekFinishActionV1 (s :: S)
    = PVerifyFinish
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PValidationOneStepWitnessV1))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PCekFinishActionV1)

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

cekExecutionSelectionSemanticV1Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
cekExecutionSelectionSemanticV1Validator = Selection.selectionValidator

cekContextStepSemanticV1Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
cekContextStepSemanticV1Validator = Context.bindValidator

cekCoreStepSemanticV1Validator ::
    forall s.
    Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
cekCoreStepSemanticV1Validator = Core.bindValidator
