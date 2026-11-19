-- | Physical CEK core authentication, budget and semantic spending hops.
module Midgard.Validators.FraudProofs.ValidationTrace.CekCoreArms (
    arm,
    computeValidator,
    builtinRootsValidator,
    semanticResultValidator,
    builtinBudgetValidator,
    directScalarValidator,
    directStructuredValidator,
) where

import Midgard.CekBuiltin qualified as Builtin
import Midgard.CekCoreWitness qualified as Witness
import Midgard.CekCost qualified as Cost
import Midgard.CekMachine qualified as Machine
import Midgard.CekProof qualified as Proof
import Midgard.CekSemanticChain qualified as Chain
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectState, pstep)
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

-- Cancellation/custody is the same for every physical hop. The callback returns
-- the next pinned script, expected progress, and exact facts for continue_core.
arm ::
    forall s.
    Term s (PAsData PCurrencySymbol) ->
    Term s PScriptContext ->
    (Term s Chain.PCoreBound -> Term s Chain.PArmAction -> (Term s (PAsData PScriptHash) -> Term s PInteger -> Term s PData -> Term s PBool) -> Term s PBool) ->
    Term s PUnit
arm policy ctx verify = pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PData policy datum redeemer ownRef tx $ \raw ->
        plet (Chain.pdecodeAction # raw) $ \action ->
            plet (pexpectDatum datum) $ \step -> pmatch step $ \d ->
                plet (Chain.pdecodeBound # pexpectState (pstep'data d)) $ \bound ->
                    verify bound action $ \next progress facts -> Chain.pcontinueCore next policy step action progress facts ownRef tx

computeValidator, builtinRootsValidator, semanticResultValidator, directScalarValidator, directStructuredValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
computeValidator = plam $ \next policy ctx -> arm policy ctx $ \bound action continue -> pmatch bound $ \b -> pmatch action $ \a ->
    pmatch (pfromData $ Chain.pbound'pre b) $ \pre ->
        pif
            ( pfromData (Chain.pbound'group b)
                #== 0
                #&& pfromData (Machine.pstate'mode pre)
                #== Machine.pmodeCompute
                #&& Machine.pverifyComputeStep
                # pfromData (Chain.pbound'pre b)
                # pfromData (Chain.pbound'post b)
                # (Witness.pdecodeCompute # Chain.parm'witness a)
            )
            (continue next 0 $ Chain.pbound'facts b)
            perror
builtinRootsValidator = plam $ \next policy ctx -> arm policy ctx $ \bound action continue -> pmatch bound $ \b -> pmatch action $ \a ->
    pmatch (pfromData $ Chain.pbound'pre b) $ \pre ->
        plet (Chain.pdecodeBuiltinSuccess # pfromData (Chain.pbound'arm b) # Chain.parm'witness a) $ \raw -> pmatch raw $ \r ->
            plet
                ( pif
                    (pfromData (Chain.pbound'arm b) #== 31)
                    (pcon r{Chain.psuccess'arguments = pdata $ pmap # plam (\value -> pdata $ Builtin.pcompactSemanticValue # pfromData value) # pfromData (Chain.psuccess'arguments r)})
                    raw
                )
                $ \item -> pmatch item $ \i ->
                    pmatch (Builtin.pargumentsRootV1 # pfromData (Chain.psuccess'arguments i)) $ \(PPair root count) ->
                        plet (Proof.phashBuiltinValueV1 # pfromData (Chain.psuccess'tag i) # 0 # count # root) $ \builtinRoot ->
                            pif
                                ( (pif (pfromData (Chain.pbound'arm b) #== 30) (pfromData (Chain.pbound'group b) #== 3 #|| pfromData (Chain.pbound'group b) #== 4) (pfromData (Chain.pbound'group b) #== 5))
                                    #&& pfromData (Machine.pstate'mode pre)
                                    #== Machine.pmodeBuiltin
                                    #&& count
                                    #== Cost.pbuiltinArgumentCountV1
                                    # pfromData (Chain.psuccess'tag i)
                                    #&& pfromData (Machine.pstate'focusRoot pre)
                                    #== builtinRoot
                                )
                                ( continue next 0 $
                                    pif
                                        (pfromData (Chain.pbound'arm b) #== 31)
                                        (pforgetData $ pdata $ pcon $ Chain.PSemanticArguments (pdata root) (pdata count) (pdata builtinRoot) (Chain.psuccess'arguments i))
                                        (pforgetData $ pdata $ pcon $ Chain.PBuiltinRoots (pdata root) (pdata count) (pdata $ Builtin.presultRootV1 # pfromData (Chain.psuccess'result i)) (pdata builtinRoot))
                                )
                                perror
semanticResultValidator = plam $ \next policy ctx -> arm policy ctx $ \bound action continue -> pmatch bound $ \b -> pmatch action $ \a ->
    pmatch (pfromData $ Chain.pbound'pre b) $ \pre ->
        plet (Chain.pdecodeBuiltinSuccess # pfromData (Chain.pbound'arm b) # Chain.parm'witness a) $ \raw -> pmatch raw $ \r ->
            pmatch (Chain.pdecodeSemanticArguments # Chain.pbound'facts b) $ \prior ->
                plet (Builtin.pcompactSemanticValue # pfromData (Chain.psuccess'result r)) $ \result ->
                    pif
                        (pfromData (Chain.pbound'group b) #== 5 #&& pfromData (Chain.pbound'arm b) #== 31 #&& pfromData (Machine.pstate'mode pre) #== 3)
                        ( continue next 1 $
                            pforgetData $
                                pdata $
                                    pcon $
                                        Chain.PSemanticRoots
                                            (pdata $ pcon $ Chain.PBuiltinRoots (Chain.parguments'root prior) (Chain.parguments'count prior) (pdata $ Builtin.presultRootV1 # result) (Chain.parguments'builtinRoot prior))
                                            (Chain.parguments'arguments prior)
                                            (pdata result)
                        )
                        perror

builtinBudgetValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
builtinBudgetValidator = plam $ \nextHashes policy ctx -> arm policy ctx $ \bound action continue -> pmatch bound $ \b -> pmatch action $ \a ->
    pmatch (pfromData $ Chain.pbound'pre b) $ \pre ->
        plet (pfromData (Chain.pbound'arm b) #== 31) $ \semantic ->
            plet (pif semantic (Chain.psemanticItem # bound # Chain.parm'witness a) (Chain.pdecodeBuiltinSuccess # pfromData (Chain.pbound'arm b) # Chain.parm'witness a)) $ \item -> pmatch item $ \i ->
                plet (pif semantic (pmatch (Chain.pdecodeSemanticRoots # Chain.pbound'facts b) $ \facts -> pfromData $ Chain.psemantic'roots facts) (Chain.pdecodeBuiltinRoots # Chain.pbound'facts b)) $ \facts -> pmatch facts $ \f ->
                    pmatch (Builtin.pdirectBuiltinBudgetV1 # pfromData (Chain.psuccess'tag i) # pfromData (Chain.psuccess'arguments i)) $ \budget ->
                        pif
                            ( (pif (pfromData (Chain.pbound'arm b) #== 30) (pfromData (Chain.pbound'group b) #== 3 #|| pfromData (Chain.pbound'group b) #== 4) (pfromData (Chain.pbound'group b) #== 5))
                                #&& pfromData (Machine.pstate'mode pre)
                                #== Machine.pmodeBuiltin
                                #&& pfromData (Chain.pbound'post b)
                                #== Machine.pexactState
                                    (pfromData $ Chain.pbound'pre b)
                                    Machine.pmodeReturn
                                    (pfromData $ Chain.proots'resultRoot f)
                                    Proof.pemptyEnvironmentRootV1
                                    (pfromData $ Machine.pstate'continuationRoot pre)
                                    0
                                    (pfromData $ Cost.pbudget'cpu budget)
                                    (pfromData $ Cost.pbudget'memory budget)
                            )
                            (continue (pelemAt # pif semantic (Chain.psemanticGroup # pfromData (Chain.psuccess'tag i)) 0 # pfromData nextHashes) (pif semantic 2 1) $ Chain.pbound'facts b)
                            perror

directScalarValidator = directValidator True
directStructuredValidator = directValidator False

directValidator :: forall s. Bool -> Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
directValidator scalar = plam $ \next policy ctx -> arm policy ctx $ \bound action continue -> pmatch bound $ \b -> pmatch action $ \a ->
    pmatch (pfromData $ Chain.pbound'pre b) $ \pre ->
        pmatch (Chain.pdecodeBuiltinSuccess # pfromData (Chain.pbound'arm b) # Chain.parm'witness a) $ \i ->
            pif
                ( pfromData (Chain.pbound'group b)
                    #== (if scalar then 3 else 4)
                    #&& pfromData (Machine.pstate'mode pre)
                    #== Machine.pmodeBuiltin
                    #&& pfromData (Chain.pbound'arm b)
                    #== 30
                    #&& (if scalar then Builtin.pverifyDirectScalarSemantics else Builtin.pverifyDirectStructuredSemantics)
                    # pfromData (Chain.psuccess'tag i)
                    # pfromData (Chain.psuccess'arguments i)
                    # pfromData (Chain.psuccess'result i)
                )
                (continue next 2 $ Chain.pbound'facts b)
                perror
