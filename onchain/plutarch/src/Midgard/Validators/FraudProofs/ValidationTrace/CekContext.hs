-- | Context binder, control router, pure context stages and final settlement.
module Midgard.Validators.FraudProofs.ValidationTrace.CekContext where

import Midgard.CekContextChain qualified as Chain
import Midgard.CekContextStep qualified as Context
import Midgard.CekSelection (bytesList)
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.FraudProofs.Common qualified as Common
import Midgard.RedeemerItemProof qualified as Item
import Midgard.ScriptProof qualified as Script
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationMachine.CekSemantics qualified as Cek
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationResolution qualified as Resolution
import Midgard.ValidationResolutionData (bytesField, decodePrepared, integerField, recordFields)
import Midgard.ValidationTrace qualified as Trace
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectState, pstep)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

type Hop s = Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)

-- A stage owns one raw state opening and produces the exact next carrier.
hop :: forall s. (Term s PData -> Term s Chain.PAction -> Term s PData) -> Hop s
hop next = plam $ \nextHash policy ctx -> pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PData policy datum redeemer ownRef tx $ \raw ->
        plet (Chain.pdecodeAction # raw) $ \action ->
            plet (pexpectDatum datum) $ \step -> pmatch step $ \d ->
                plet (next (pexpectState $ pstep'data d) action) $ \state -> continue policy step action ownRef tx nextHash state

continue :: forall s. Term s (PAsData PCurrencySymbol) -> Term s PStepDatum -> Term s Chain.PAction -> Term s PTxOutRef -> Term s PTxInfo -> Term s (PAsData PScriptHash) -> Term s PData -> Term s PBool
continue policy step action ownRef tx next state = pmatch action $ \a -> pmatch tx $ \t ->
    Common.pcontinue
        policy
        step
        (pfromData $ Chain.paction'inputIndex a)
        (pfromData $ Chain.paction'outputIndex a)
        ownRef
        (pfromData $ ptxInfo'inputs t)
        (pfromData $ ptxInfo'outputs t)
        (\_ _ _ _ outputHash outputState -> outputHash #== next #&& outputState #== state)

bindValidator :: forall s. Hop s
bindValidator = hop $ \raw action -> pmatch action $ \a -> plet (decodePrepared raw) $ \prepared -> pmatch prepared $ \p ->
    pmatch (pfromData $ Resolution.pprepared'resolution p) $ \r ->
        plet (pfromData $ Resolution.presolution'preState r) $ \pre -> pmatch pre $ \before ->
            plet (pfromData $ Chain.paction'transition a) $ \transition -> plet (Cek.pcekWitnessControlV1 # transition) $ \control ->
                pif
                    ( pand'List
                        [ Resolution.ppreparedResolutionIsWellFormed # prepared
                        , pfromData (Trace.pmachineState'phase before) #== pcon Trace.PCek
                        , Resolution.phashOneStepEvidence # pforgetData (Chain.paction'transition a) # Chain.paction'auxiliary a #== pfromData (Resolution.pprepared'evidenceHash p)
                        , Cek.pcekWitnessIsWellFormedV1 # pre # transition # control
                        , Cek.pcekControlIsContextStepV1 # control
                        ]
                    )
                    ( pforgetData $
                        pdata $
                            pcon $
                                Chain.PBound
                                    (pforgetData $ pdata prepared)
                                    (Chain.pcontrolData # control)
                                    (pdata $ pblake2b_256 # (pserialiseData # Chain.paction'auxiliary a))
                                    (Trace.pmachineState'transactionId before)
                    )
                    perror

controlValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
controlValidator = plam $ \stages policy ctx -> pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PData policy datum redeemer ownRef tx $ \raw ->
        plet (Chain.pdecodeAction # raw) $ \action -> pmatch action $ \a ->
            plet (pexpectDatum datum) $ \step -> pmatch step $ \d ->
                plet (Chain.pdecodeBound # pexpectState (pstep'data d)) $ \bound -> pmatch bound $ \b ->
                    pmatch (Chain.pdecodeControl # Chain.pbound'control b) $ \(Cek.PCekWitnessControlV1 native contextCbor cursor _ _ _ envelope cpu memory) -> pmatch native $ \n ->
                        plet (VM.pcekContextControlFromCbor # contextCbor) $ \context -> pmatch context $ \c ->
                            plet (pfromData $ VM.pcekContext'stage c) $ \stage ->
                                pif
                                    ( pand'List
                                        [ Chain.pauxiliaryIsBound # bound # Chain.paction'auxiliary a
                                        , cursor #< pfromData (VM.pnativeControl'executionCount n)
                                        , cpu #== 0
                                        , memory #== 0
                                        , envelope #== pfromData (VM.pcekContext'programEnvelopeHash c)
                                        , contextCbor #== VM.pencodeCekContextControlV1 # context
                                        , VM.pcekContextControlIsWellFormed # native # context
                                        , pnot # (stage #== 7)
                                        ]
                                    )
                                    ( plet
                                        ( pif (stage #== 0) (pif (pfromData (VM.pcekContext'redeemerContextControlHash c) #== pconstant "") 0 1) $
                                            pif (stage #>= 1 #&& stage #<= 6) (stage + 1) $
                                                pif (stage #== 8) 8 $
                                                    pif (stage #== 9) (pmatch (pasConstr # Chain.paction'auxiliary a) $ \(PBuiltinPair tag _) -> pif (tag #== 18) 1 9) stage
                                        )
                                        $ \route ->
                                            continue
                                                policy
                                                step
                                                action
                                                ownRef
                                                tx
                                                (pelemAt # route # pfromData stages)
                                                (pforgetData $ pdata $ pcon $ Chain.PStaged (pdata bound) (pdata context) (pdata $ Context.pnativeFacts # native) (pdata cursor))
                                    )
                                    perror

stageHop :: forall s. Integer -> (Term s Chain.PStaged -> Term s PData -> Term s Chain.PSuccessor) -> Hop s
stageHop stage next = hop $ \raw action -> pmatch action $ \a -> plet (Chain.pdecodeStaged # raw) $ \staged -> pmatch staged $ \s ->
    pmatch (pfromData $ Chain.pstaged'context s) $ \c ->
        pif
            (pfromData (VM.pcekContext'stage c) #== pconstant stage #&& Chain.pauxiliaryIsBound # pfromData (Chain.pstaged'bound s) # Chain.paction'auxiliary a)
            (pforgetData $ pdata $ pcon $ Chain.PVerified (pforgetData $ pdata staged) (pdata $ next staged $ Chain.paction'auxiliary a))
            perror

referenceValidator, spendValidator, outputValidator, signerValidator, mintInitValidator, mintItemValidator, assembleValidator, txInfoValidator, seedValidator :: forall s. Hop s
referenceValidator = stageHop 1 $ \staged auxiliary -> pmatch staged $ \s ->
    pcon $
        Chain.PContinueContext $
            pdata $
                Context.pnextResolved # pfromData (Chain.pstaged'context s) # pfromData (Chain.pstaged'native s) # auxiliary # 1
spendValidator = stageHop 2 $ \staged auxiliary -> pmatch staged $ \s ->
    pcon $
        Chain.PContinueContext $
            pdata $
                Context.pnextResolved # pfromData (Chain.pstaged'context s) # pfromData (Chain.pstaged'native s) # auxiliary # 0
outputValidator = stageHop 3 $ \staged auxiliary -> pmatch staged $ \s ->
    pcon $
        Chain.PContinueContext $
            pdata $
                Context.pnextOutput # pfromData (Chain.pstaged'context s) # pfromData (Chain.pstaged'native s) # auxiliary
signerValidator = stageHop 4 $ \staged auxiliary -> pmatch staged $ \s ->
    pcon $
        Chain.PContinueContext $
            pdata $
                Context.pnextSigner # pfromData (Chain.pstaged'context s) # pfromData (Chain.pstaged'native s) # auxiliary
mintInitValidator = stageHop 6 $ \staged auxiliary -> pmatch staged $ \s ->
    pcon $
        Chain.PContinueContext $
            pdata $
                Context.pnextMintInit # pfromData (Chain.pstaged'context s) # pfromData (Chain.pstaged'native s) # auxiliary
mintItemValidator = stageHop 8 $ \staged auxiliary -> pmatch staged $ \s ->
    pcon $
        Chain.PContinueContext $
            pdata $
                Context.pnextMintItem # pfromData (Chain.pstaged'context s) # pfromData (Chain.pstaged'native s) # auxiliary
assembleValidator = stageHop 11 $ \staged auxiliary -> pmatch staged $ \s -> pmatch (pfromData $ Chain.pstaged'bound s) $ \b ->
    pcon $
        Chain.PContinueContext $
            pdata $
                Context.pnextAssemble # pfromData (Chain.pstaged'context s) # pfromData (Chain.pbound'transactionId b) # auxiliary
txInfoValidator = stageHop 12 $ \staged auxiliary -> pmatch staged $ \s ->
    pcon $
        Chain.PContinueContext $
            pdata $
                Context.pnextTxInfo # pfromData (Chain.pstaged'context s) # pfromData (Chain.pstaged'native s) # auxiliary
seedValidator = stageHop 13 $ \staged auxiliary -> pmatch staged $ \s -> pmatch (pfromData $ Chain.pstaged'context s) $ \c ->
    pcon $
        Chain.PStartExecution
            (pdata $ Context.pseedHash # pfromData (Chain.pstaged'context s) # pfromData (Chain.pstaged'cursor s) # auxiliary)
            (VM.pcekContext'executionCpuLimit c)
            (VM.pcekContext'executionMemoryLimit c)

settleValidator :: forall s. Hop s
settleValidator = hop $ \raw action -> pmatch action $ \a -> pmatch (Chain.pdecodeVerified # raw) $ \v ->
    plet (recordFields 4 $ Chain.pverified'staged v) $ \fields -> pmatch (Chain.pdecodeBound # (phead # fields)) $ \b ->
        pmatch (decodePrepared $ Chain.pbound'prepared b) $ \p -> pmatch (pfromData $ Resolution.pprepared'resolution p) $ \r ->
            pmatch (Chain.pdecodeControl # Chain.pbound'control b) $ \(Cek.PCekWitnessControlV1 native _ cursor cpu memory _ envelope _ _) ->
                plet
                    ( pmatch (pfromData $ Chain.pverified'successor v) $ \case
                        Chain.PContinueContext next -> pmatch (pfromData next) $ \c -> VM.pencodeCekWitnessV1 # native # (VM.pencodeCekContextControlV1 # pfromData next) # cursor # cpu # memory # pconstant "" # 0 # 0 # pfromData (VM.pcekContext'programEnvelopeHash c)
                        Chain.PStartExecution initial cpuLimit memoryLimit -> VM.pencodeCekWitnessV1 # native # pconstant "" # cursor # cpu # memory # pfromData initial # pfromData cpuLimit # pfromData memoryLimit # envelope
                    )
                    $ \work ->
                        pmatch (pfromData $ Resolution.presolution'preState r) $ \pre -> pmatch (pfromData $ Chain.paction'transition a) $ \w -> pmatch (pfromData $ VM.poneStep'claimedSuccessor w) $ \post ->
                            pif
                                ( pand'List
                                    [ Chain.pauxiliaryIsBound # pcon b # Chain.paction'auxiliary a
                                    , Resolution.phashOneStepEvidence # pforgetData (Chain.paction'transition a) # Chain.paction'auxiliary a #== pfromData (Resolution.pprepared'evidenceHash p)
                                    , pfromData (Trace.pmachineState'phase post) #== pcon Trace.PCek
                                    , Trace.pmachineState'executionCpu post #== Trace.pmachineState'executionCpu pre
                                    , Trace.pmachineState'executionMemory post #== Trace.pmachineState'executionMemory pre
                                    , pfromData (Trace.pmachineState'workRoot post) #== Trace.phashWorkWitness # pcon Trace.PCek # (pfromData (Trace.pmachineState'programCounter pre) + 1) # work
                                    ]
                                )
                                (pforgetData $ pdata Resolution.pwinningResolution)
                                perror

redeemerBeginValidator :: forall s. Hop s
redeemerBeginValidator = stageHop 0 $ \staged auxiliary -> pmatch staged $ \s ->
    pmatch (pfromData $ Chain.pstaged'context s) $ \c -> pmatch (pfromData $ Chain.pstaged'native s) $ \n ->
        plet (Context.pauxiliaryFields 10 5 auxiliary) $ \f -> plet (pfromData $ integerField f 0) $ \index ->
            plet (pfromData $ integerField f 1) $ \count -> plet (pfromData $ bytesField f 3) $ \commitment ->
                plet (Script.predeemerItemLeafHash # index # commitment) $ \leaf ->
                    pif
                        ( pand'List
                            [ pfromData (VM.pcekContext'redeemerContextControlHash c) #== pconstant ""
                            , count #== pfromData (Context.pnative'redeemerCount n)
                            , leaf #== pfromData (VM.pcekContext'redeemerLeaf c)
                            , Merkle.pverifyMembership # pfromData (Context.pnative'redeemerCount n) # pfromData (Context.pnative'redeemerPeaks n) # index # leaf # pfromData (bytesList $ pelemAt # 4 # f)
                            ]
                        )
                        ( pmatch (VM.predeemerTagForPurposeKindV1 # pfromData (VM.pcekContext'purposeKind c)) $ \case
                            PNothing -> perror
                            PJust purpose ->
                                pcon $
                                    Chain.PContinueContext $
                                        pdata $
                                            pcon
                                                c
                                                    { VM.pcekContext'redeemerContextControlHash =
                                                        pdata $
                                                            Item.phashControlV1
                                                                # (Item.pinitialControlV1 # Item.pmodeDescriptor # index # count # pfromData (integerField f 2) # commitment # purpose # pfromData (VM.pcekContext'purposeIndex c))
                                                    }
                        )
                        perror
