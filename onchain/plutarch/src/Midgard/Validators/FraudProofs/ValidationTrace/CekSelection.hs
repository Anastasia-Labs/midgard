-- | Execution-selection dispatcher and its four authenticated rewarding arms.
module Midgard.Validators.FraudProofs.ValidationTrace.CekSelection (selectionValidator, authenticateValidator, successorValidator, programValidator, dataValidator) where

import Midgard.CekMaterialTraversal qualified as Traversal
import Midgard.CekProof qualified as Proof
import Midgard.CekSelection qualified as Selection
import Midgard.CekSelectionSemantics qualified as Semantics
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.FraudProofs.Common qualified as Common
import Midgard.StateQueueYield qualified as Yield
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationMachine.CekSemantics qualified as Cek
import Midgard.ValidationResolution qualified as Resolution
import Midgard.ValidationResolutionData (decodePrepared)
import Midgard.ValidationResolver qualified as Resolver
import Midgard.ValidationTrace (PValidationMachineStateV1 (..), PValidationPhase (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectState, pstep)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

selectionValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
selectionValidator = plam $ \award policy _material auth traversal ctx -> pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PData policy datum redeemer ownRef tx $ \raw ->
        plet (Selection.pdecodeAction # raw) $ \action -> pmatch action $ \a ->
            plet (pexpectDatum datum) $ \step -> pmatch step $ \d ->
                plet (decodePrepared $ pexpectState $ pstep'data d) $ \prepared -> pmatch prepared $ \p ->
                    pmatch (pfromData $ Resolution.pprepared'resolution p) $ \r ->
                        plet (pfromData $ Resolution.presolution'preState r) $ \pre -> pmatch pre $ \before ->
                            plet (pfromData $ Selection.paction'transition a) $ \transition ->
                                plet (Cek.pcekWitnessControlV1 # transition) $ \cek ->
                                    pmatch cek $ \(Cek.PCekWitnessControlV1 _ _ _ _ _ _ envelope cpu memory) -> pmatch tx $ \t ->
                                        Common.pcontinue
                                            policy
                                            step
                                            (pfromData $ Selection.paction'inputIndex a)
                                            (pfromData $ Selection.paction'outputIndex a)
                                            ownRef
                                            (pfromData $ ptxInfo'inputs t)
                                            (pfromData $ ptxInfo'outputs t)
                                            ( \_ _ _ _ outputHash outputState ->
                                                pand'List
                                                    [ Resolution.ppreparedResolutionIsWellFormed # prepared
                                                    , pfromData (pmachineState'phase before) #== pcon PCek
                                                    , Resolution.phashOneStepEvidence # pforgetData (Selection.paction'transition a) # Selection.paction'auxiliary a #== pfromData (Resolution.pprepared'evidenceHash p)
                                                    , Cek.pcekWitnessIsWellFormedV1 # pre # transition # cek
                                                    , Cek.pcekControlIsExecutionSelectionV1 # cek
                                                    , cpu #== 0
                                                    , memory #== 0
                                                    , envelope #== pconstant ""
                                                    , requiredYields action auth tx
                                                    , pif
                                                        (pfromData $ Selection.paction'beginTraversal a)
                                                        (pmatch (pfromData $ Selection.paction'envelope a) $ \e -> outputHash #== traversal #&& outputState #== pforgetData (pdata $ Traversal.pinitial # pfromData (Proof.penvelope'termRoot e) # pfromData (Proof.penvelope'nodeCount e) # pfromData (Proof.penvelope'materialByteLength e)))
                                                        (outputHash #== award #&& outputState #== pforgetData (pdata Resolution.pwinningResolution))
                                                    ]
                                            )

requiredYields :: forall s. Term s Selection.PAction -> Term s (PAsData PCurrencySymbol) -> Term s PTxInfo -> Term s PBool
requiredYields action auth tx = pmatch action $ \a ->
    plet (pfromData $ Selection.paction'yields a) $ \indices ->
        plet (Selection.pselectionLanguage # Selection.paction'auxiliary a) $ \language ->
            let invoke role i = plet (Yield.prequireAuthenticatedZeroYield # tx # pfromData auth # (pcon $ PTokenName role) # pfromData (pelemAt # i # indices)) $ \_ -> pconstant True
                common = invoke Selection.pauthenticateRole 0 #&& invoke Selection.psuccessorRole 1
             in pif
                    (language #== 0)
                    ( pmatch (pfromData $ Selection.paction'envelope a) $ \e -> pmatch (pfromData $ Selection.paction'material a) $ \(Proof.PCekMaterialPartitionFacts pn pb dn db roots blobs) ->
                        pand'List
                            [ plength # indices #== 2
                            , pnot # pfromData (Selection.paction'beginTraversal a)
                            , pfromData (Selection.paction'route a) #== pcon Resolver.PNoCekMaterial
                            , pfromData (Proof.penvelope'termRoot e) #== pconstant ""
                            , pfromData (Proof.penvelope'nodeCount e) #== 0
                            , pfromData (Proof.penvelope'materialByteLength e) #== 0
                            , pfromData pn #== 0
                            , pfromData pb #== 0
                            , pfromData dn #== 0
                            , pfromData db #== 0
                            , pnull # pfromData roots
                            , pnull # pfromData blobs
                            , common
                            ]
                    )
                    ( (language #== 3 #|| language #== 128)
                        #&& pif
                            (pfromData $ Selection.paction'beginTraversal a)
                            (plength # indices #== 2 #&& common)
                            (plength # indices #== 4 #&& common #&& invoke Selection.pprogramRole 2 #&& invoke Selection.pdataRole 3)
                    )

-- Rewarding arms recover one exact dispatcher spend, including its typed datum
-- and Continue action. Their redeemer and reward credential are not claims.
yielding :: forall s. Term s (PAsData PScriptHash) -> Term s PScriptContext -> (Term s PStepDatum -> Term s Selection.PAction -> Term s PTxInfo -> Term s PBool) -> Term s PUnit
yielding dispatcher ctx predicate = pmatch ctx $ \c -> pmatch (pscriptContext'scriptInfo c) $ \case
    PRewardingScript _ -> plet (pscriptContext'txInfo c) $ \tx -> pmatch tx $ \t ->
        pmatch (Selection.puniqueDispatch # pfromData dispatcher # pfromData (ptxInfo'inputs t) # (pto $ pto $ pfromData $ ptxInfo'redeemers t)) $ \(PPair datum action) ->
            pif (predicate datum action tx) (pconstant ()) perror
    _ -> perror

authenticateValidator, successorValidator :: forall s. Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
authenticateValidator = plam $ \dispatcher ctx -> yielding dispatcher ctx $ \_ action _ -> pmatch action $ \a ->
    plet (Selection.pdecodeAuxiliary # Selection.paction'auxiliary a) $ \aux ->
        pmatch (Cek.pcekWitnessControlV1 # pfromData (Selection.paction'transition a)) $ \(Cek.PCekWitnessControlV1 control _ cursor _ _ _ _ _ _) ->
            Semantics.pauthenticate
                # aux
                # control
                # cursor
                #&& pif
                    (Selection.pselectionLanguage # Selection.paction'auxiliary a #== 0)
                    (pfromData (Selection.paction'route a) #== pcon Resolver.PNoCekMaterial)
                    (Resolver.pcekSelectionEnvelopeCborV1 # (pcon $ VM.PValidationOneStepEvidenceV1 (Selection.paction'transition a) (pdata aux)) #== pcon (PJust $ Selection.prouteEnvelope # pfromData (Selection.paction'route a)))
successorValidator = plam $ \dispatcher ctx -> yielding dispatcher ctx $ \datum action _ -> pmatch action $ \a -> pmatch datum $ \d ->
    pmatch (decodePrepared $ pexpectState $ pstep'data d) $ \p -> pmatch (pfromData $ Resolution.pprepared'resolution p) $ \r ->
        plet (Selection.pdecodeAuxiliary # Selection.paction'auxiliary a) $ \aux ->
            pmatch (Cek.pcekWitnessControlV1 # pfromData (Selection.paction'transition a)) $ \(Cek.PCekWitnessControlV1 control _ cursor cpu memory _ _ _ _) ->
                pif
                    (Selection.pselectionLanguage # Selection.paction'auxiliary a #== 0)
                    (pconstant True)
                    ( pmatch (Proof.pinspectProgramEnvelopeV1 # (Selection.prouteEnvelope # pfromData (Selection.paction'route a))) $ \case
                        PNothing -> perror
                        PJust envelope -> pdata envelope #== Selection.paction'envelope a
                    )
                    #&& Semantics.pverifySuccessor
                    # pfromData (Resolution.presolution'preState r)
                    # pfromData (Selection.paction'transition a)
                    # aux
                    # control
                    # cursor
                    # cpu
                    # memory

programValidator, dataValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
programValidator = plam $ \dispatcher material ctx -> materialYield True dispatcher material ctx
dataValidator = plam $ \dispatcher material ctx -> materialYield False dispatcher material ctx

materialYield :: forall s. Bool -> Term s (PAsData PScriptHash) -> Term s (PAsData PScriptHash) -> Term s PScriptContext -> Term s PUnit
materialYield program dispatcher material ctx = yielding dispatcher ctx $ \_ action tx -> pmatch action $ \a -> pmatch tx $ \t ->
    pmatch (Resolver.pmaterialEntriesForRoute # pfromData (Selection.paction'route a) # pfromData (ptxInfo'referenceInputs t) # material) $ \case
        PNothing -> perror
        PJust entries -> pmatch (pfromData $ Selection.paction'envelope a) $ \e ->
            if program
                then Proof.pverifyProgramMaterialPartition # pfromData (Proof.penvelope'termRoot e) # entries # pfromData (Selection.paction'material a)
                else Proof.pverifyDataMaterialPartition # entries # pfromData (Selection.paction'material a) # pfromData (Proof.penvelope'nodeCount e) # pfromData (Proof.penvelope'materialByteLength e)
