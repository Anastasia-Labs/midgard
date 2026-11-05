-- | Spending traversal plus the program and Data task rewarding arms.
module Midgard.Validators.FraudProofs.ValidationTrace.CekMaterialTraversal (
    traversalValidator,
    programYieldValidator,
    dataYieldValidator,
) where

import Midgard.CekMaterialTraversal qualified as Material
import Midgard.CekProof qualified as Proof
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.FraudProofs.Common qualified as Common
import Midgard.StateQueueYield qualified as Yield
import Midgard.ValidationResolution (pwinningResolution)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectState, pstep)
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

traversalValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
traversalValidator = plam $ \award policy auth ctx -> pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PData policy datum redeemer ownRef tx $ \raw ->
        plet (Material.pdecodeAction # raw) $ \action -> pmatch action $ \a ->
            plet (pexpectDatum datum) $ \step -> pmatch step $ \d -> pmatch tx $ \t ->
                plet (Material.padvance # (Material.pdecodeState # pexpectState (pstep'data d)) # action) $ \next -> pmatch next $ \n ->
                    pmatch (Material.pdecodeTask # Material.pvisit'task a) $ \task ->
                        plet
                            ( Yield.prequireAuthenticatedZeroYield
                                # tx
                                # pfromData auth
                                # pif (Proof.ptask'kind task #>= 0 #&& Proof.ptask'kind task #<= 3) Material.pprogramRole Material.pdataRole
                                # pfromData (Material.pvisit'yieldIndex a)
                            )
                            $ \_ ->
                                Common.pcontinue
                                    policy
                                    step
                                    (pfromData $ Material.pvisit'inputIndex a)
                                    (pfromData $ Material.pvisit'outputIndex a)
                                    ownRef
                                    (pfromData $ ptxInfo'inputs t)
                                    (pfromData $ ptxInfo'outputs t)
                                    ( \inputHash _ _ _ outputHash outputState ->
                                        pif
                                            (pfromData (Material.pstate'pendingRoot n) #== Material.pemptyStack)
                                            ( pfromData (Material.pstate'nodeCount n)
                                                #== pfromData (Material.pstate'expectedNodeCount n)
                                                #&& pfromData (Material.pstate'byteLength n)
                                                #== pfromData (Material.pstate'expectedByteLength n)
                                                #&& outputHash
                                                #== award
                                                #&& outputState
                                                #== pforgetData (pdata pwinningResolution)
                                            )
                                            (outputHash #== inputHash #&& outputState #== pforgetData (pdata next))
                                    )

programYieldValidator, dataYieldValidator :: forall s. Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
programYieldValidator = plam $ \dispatcher ctx -> taskYield True dispatcher ctx
dataYieldValidator = plam $ \dispatcher ctx -> taskYield False dispatcher ctx

taskYield :: forall s. Bool -> Term s (PAsData PScriptHash) -> Term s PScriptContext -> Term s PUnit
taskYield program dispatcher ctx = P.do
    PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
    pmatch pscriptContext'scriptInfo $ \case
        PRewardingScript _ -> P.do
            PTxInfo{ptxInfo'inputs, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
            PPair _ action <- pmatch $ Material.puniqueDispatch # pfromData dispatcher # pfromData ptxInfo'inputs # (pto $ pto $ pfromData ptxInfo'redeemers)
            a <- pmatch action
            let matches = if program then Proof.pprogramPartitionRootMatches else Proof.pdataPartitionRootMatches
                childrenOf = if program then Proof.pprogramPartitionChildren else Proof.pdataPartitionChildren
            pif
                (matches # pfromData (Material.pvisit'entry a))
                ( pmatch (childrenOf # (Material.pdecodeTask # Material.pvisit'task a) # pfromData (Material.pvisit'entry a)) $ \case
                    PNothing -> perror
                    PJust children ->
                        pif
                            ( pfromData (Material.pvisit'nextPendingRoot a)
                                #== pif
                                    (pfromData $ Material.pvisit'alreadySeen a)
                                    (pfromData $ Material.pvisit'tailRoot a)
                                    (Material.ppushChildren # children # pfromData (Material.pvisit'tailRoot a))
                            )
                            (pconstant ())
                            perror
                )
                perror
        _ -> perror
