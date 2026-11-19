{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStagesTwoToSix
Description : ScriptSources non-output and output-proof semantic validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStagesTwoToSix (
    PScriptSourcesNonOutputActionV1 (..),
    PScriptSourcesOutputProofBeginActionV1 (..),
    PScriptSourcesOutputProofStepActionV1 (..),
    PScriptSourcesOutputProofFinalizeActionV1 (..),
    PScriptSourcesOutputProofFinishActionV1 (..),
    scriptSourcesNonOutputSemanticV1Validator,
    scriptSourcesOutputProofBeginSemanticV1Validator,
    scriptSourcesOutputProofStepSemanticV1Validator,
    scriptSourcesOutputProofFinalizeSemanticV1Validator,
    scriptSourcesOutputProofFinishSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.LedgerOutputProofDispatch qualified as OutputDispatch
import Midgard.ScriptSourcesMiddleYield qualified as Middle
import Midgard.ScriptSourcesRawFrame qualified as Raw
import Midgard.ScriptSourcesOutputSemantics qualified as Output
import Midgard.ValidationSemanticYield qualified as Yield
import Midgard.ValidationMachine (
    PSignerSetProofV1,
    PValidationAuxiliaryWitnessV1 (..),
    PValidationOneStepWitnessV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PScriptSources))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PScriptSourcesNonOutputActionV1 (s :: S)
    = PVerifyNonOutput
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PValidationOneStepWitnessV1))
        (Term s (PAsData PData))
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesNonOutputActionV1)

data PScriptSourcesOutputProofBeginActionV1 (s :: S)
    = PVerifyOutputProofBegin
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PValidationOneStepWitnessV1))
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PByteString))
        (Term s (PAsData (PBuiltinList (PAsData PByteString))))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesOutputProofBeginActionV1)

data PScriptSourcesOutputProofStepActionV1 (s :: S)
    = PVerifyOutputProofStep
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PValidationOneStepWitnessV1))
        (Term s (PAsData PData))
        (Term s (PAsData PByteString))
        (Term s (PAsData PByteString))
        (Term s (PAsData PData))
        (Term s (PAsData PInteger))
        (Term s (PAsData (PBuiltinList (PAsData PInteger))))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesOutputProofStepActionV1)

data PScriptSourcesOutputProofFinalizeActionV1 (s :: S)
    = PVerifyOutputProofFinalize
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PValidationOneStepWitnessV1))
        (Term s (PAsData PByteString))
        (Term s (PAsData PSignerSetProofV1))
        (Term s (PAsData PByteString))
        (Term s (PAsData PData))
        (Term s (PAsData PData))
        (Term s (PAsData (PBuiltinList (PAsData PInteger))))
        (Term s (PAsData (PBuiltinList (PAsData PInteger))))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesOutputProofFinalizeActionV1)

data PScriptSourcesOutputProofFinishActionV1 (s :: S)
    = PVerifyOutputProofFinish
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PValidationOneStepWitnessV1))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesOutputProofFinishActionV1)

scriptSourcesNonOutputSemanticV1Validator ::
    forall s.
    Term
        s
        ( PAsData PScriptHash
            :--> PAsData PCurrencySymbol
            :--> PAsData PCurrencySymbol
            :--> PScriptContext
            :--> PUnit
        )
scriptSourcesNonOutputSemanticV1Validator = plam $ \awardScriptHash policyId authPolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PScriptSourcesNonOutputActionV1 policyId datum redeemer ownOutRef txInfo $
            \action -> pmatch action $ \(PVerifyNonOutput inputIndex outputIndex transitionD auxiliaryD roleD refIndexD) ->
                plet (pfromData transitionD) $ \transition ->
                plet (Raw.popenFrameV1 # pvalidationSemanticPreState datum # transition # 30 # (Middle.pstage # pfromData roleD)) $ \frame ->
                plet (Yield.prequireSemanticYieldV1 # txInfo # pfromData authPolicy # (Middle.prole # pfromData roleD) # pfromData refIndexD) $ \yieldHash ->
                pmatch frame $ \f ->
                    pcontinueWinning
                        (pcon PScriptSources) awardScriptHash policyId datum
                        (pfromData inputIndex) (pfromData outputIndex) transition (pfromData auxiliaryD)
                        (Raw.pframe'itemCount f #== 30 #&& plengthBS # pto yieldHash #== 28)
                        ownOutRef txInfo

scriptSourcesOutputProofBeginSemanticV1Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesOutputProofBeginSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PScriptSourcesOutputProofBeginActionV1 policyId datum redeemer ownOutRef txInfo $
            \action -> pmatch action $ \(PVerifyOutputProofBegin inputIndex outputIndex transitionD ledgerOutputIndexD totalLengthD itemCommitmentD siblingsD) ->
                plet (pfromData transitionD) $ \transition ->
                    plet
                        (pcon $ PLedgerOutputProofBeginWitness ledgerOutputIndexD totalLengthD itemCommitmentD siblingsD)
                        $ \auxiliary ->
                            pcontinueWinning
                                (pcon PScriptSources)
                                awardScriptHash
                                policyId
                                datum
                                (pfromData inputIndex)
                                (pfromData outputIndex)
                                transition
                                (pforgetData $ pdata auxiliary)
                                ( Output.pbegin
                                    # pvalidationSemanticPreState datum
                                    # transition
                                    # pfromData ledgerOutputIndexD
                                    # pfromData totalLengthD
                                    # pfromData itemCommitmentD
                                    # pfromData siblingsD
                                )
                                ownOutRef
                                txInfo

scriptSourcesOutputProofStepSemanticV1Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesOutputProofStepSemanticV1Validator = plam $ \awardScriptHash policyId authPolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PScriptSourcesOutputProofStepActionV1 policyId datum redeemer ownOutRef txInfo $
            \action -> pmatch action $ \(PVerifyOutputProofStep inputIndex outputIndex transitionD proofWitnessD controlD nextD _claimedScalar roleD indicesD) ->
                plet (pfromData transitionD) $ \transition ->
                    pcontinueWinning
                        (pcon PScriptSources)
                        awardScriptHash
                        policyId
                        datum
                        (pfromData inputIndex)
                        (pfromData outputIndex)
                        transition
                        (pforgetData $ pconstrBuiltin # 32 # (pcons # pfromData proofWitnessD # pnil))
                        ( OutputDispatch.poutputStep
                            # pvalidationSemanticPreState datum
                            # transition
                            # pfromData controlD
                            # pfromData nextD
                            # pfromData roleD
                            # pfromData indicesD
                            # pfromData authPolicy
                            # txInfo
                        )
                        ownOutRef
                        txInfo

scriptSourcesOutputProofFinalizeSemanticV1Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesOutputProofFinalizeSemanticV1Validator = plam $ \awardScriptHash policyId authPolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PScriptSourcesOutputProofFinalizeActionV1 policyId datum redeemer ownOutRef txInfo $
            \action -> pmatch action $ \(PVerifyOutputProofFinalize inputIndex outputIndex transitionD descriptorD signerD controlD valueD datumD rolesD indicesD) ->
                plet (pfromData transitionD) $ \transition ->
                    plet (pcon $ PLedgerOutputProofFinalizeWitness descriptorD signerD) $ \auxiliary ->
                        pcontinueWinning
                            (pcon PScriptSources)
                            awardScriptHash
                            policyId
                            datum
                            (pfromData inputIndex)
                            (pfromData outputIndex)
                            transition
                            (pforgetData $ pdata auxiliary)
                            ( OutputDispatch.poutputFinalize
                                # pvalidationSemanticPreState datum
                                # transition
                                # pfromData descriptorD
                                # pfromData signerD
                                # pfromData controlD
                                # pfromData valueD
                                # pfromData datumD
                                # pfromData rolesD
                                # pfromData indicesD
                                # pfromData authPolicy
                                # txInfo
                            )
                            ownOutRef
                            txInfo

scriptSourcesOutputProofFinishSemanticV1Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesOutputProofFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PScriptSourcesOutputProofFinishActionV1 policyId datum redeemer ownOutRef txInfo $
            \action -> pmatch action $ \(PVerifyOutputProofFinish inputIndex outputIndex transitionD) ->
                plet (pfromData transitionD) $ \transition ->
                    plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
                        pcontinueWinning
                            (pcon PScriptSources)
                            awardScriptHash
                            policyId
                            datum
                            (pfromData inputIndex)
                            (pfromData outputIndex)
                            transition
                            (pforgetData $ pdata auxiliary)
                            (Output.pfinish # pvalidationSemanticPreState datum # transition)
                            ownOutRef
                            txInfo
