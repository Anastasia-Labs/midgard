-- | Bind a CEK core witness, and settle its fully authenticated physical chain.
module Midgard.Validators.FraudProofs.ValidationTrace.CekCore (
    PCoreAction (..),
    bindValidator,
    settleValidator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.CekCoreWitness qualified as Witness
import Midgard.CekMachine qualified as Machine
import Midgard.CekSemanticChain qualified as Chain
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.FraudProofs.Common qualified as Common
import Midgard.ValidationMachine (PValidationOneStepWitnessV1 (..))
import Midgard.ValidationMachine.CekSemantics qualified as Cek
import Midgard.ValidationResolution qualified as Resolution
import Midgard.ValidationResolutionData (bytesField, decodePrepared, decodeState, integerField, recordFields)
import Midgard.ValidationTrace (PValidationMachineStateV1 (..), PValidationPhase (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectState, pstep)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

-- Both entry and settlement actions have four fields. Entry carries raw core
-- evidence; settlement carries its exact arm witness. Neither re-encodes it.
data PCoreAction s = PCoreAction
    { paction'inputIndex :: Term s (PAsData PInteger)
    , paction'outputIndex :: Term s (PAsData PInteger)
    , paction'transition :: Term s (PAsData PValidationOneStepWitnessV1)
    , paction'witness :: Term s PData
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PCoreAction)

bindValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
bindValidator = plam $ \arms policy ctx -> pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PData policy datum redeemer ownRef tx $ \rawAction ->
        plet (decodeAction rawAction) $ \action -> pmatch action $ \a ->
            plet (pexpectDatum datum) $ \step -> pmatch step $ \d ->
                plet (decodePrepared $ pexpectState $ pstep'data d) $ \prepared -> pmatch prepared $ \p ->
                    pmatch (pfromData $ Resolution.pprepared'resolution p) $ \resolution ->
                        plet (pfromData $ Resolution.presolution'preState resolution) $ \pre -> pmatch pre $ \preState ->
                            pmatch (pasConstr # paction'witness a) $ \(PBuiltinPair tag fields) ->
                                pif
                                    (tag #== 0 #&& plength # fields #== 3)
                                    ( plet (Witness.pdecodeMachine # (pelemAt # 0 # fields)) $ \machinePre ->
                                        plet (Witness.pdecodeMachine # (pelemAt # 1 # fields)) $ \machinePost ->
                                            plet (pelemAt # 2 # fields) $ \armWitness ->
                                                plet (Cek.pcekWitnessControlV1 # pfromData (paction'transition a)) $ \control ->
                                                    pmatch control $ \(Cek.PCekWitnessControlV1 _ context _ _ _ _ _ _ _) ->
                                                        pif
                                                            ( Resolution.ppreparedResolutionIsWellFormed
                                                                # prepared
                                                                #&& pfromData (pmachineState'phase preState)
                                                                #== pcon PCek
                                                                #&& Resolution.phashOneStepEvidence
                                                                # pforgetData (paction'transition a)
                                                                # auxiliary (paction'witness a)
                                                                #== pfromData (Resolution.pprepared'evidenceHash p)
                                                                #&& Cek.pcekWitnessIsWellFormedV1
                                                                # pre
                                                                # pfromData (paction'transition a)
                                                                # control
                                                                #&& Cek.pcekControlIsCoreStepV1
                                                                # control
                                                                #&& context
                                                                #== pconstant ""
                                                                #&& Cek.pverifyCekCoreFrame
                                                                # pre
                                                                # machinePre
                                                                # machinePost
                                                                # control
                                                            )
                                                            ( pmatch machinePre $ \before ->
                                                                plet (Chain.pcoreGroup # pfromData (Machine.pstate'mode before) # armWitness) $ \group ->
                                                                    let next =
                                                                            pcon $
                                                                                Chain.PCoreBound
                                                                                    (pforgetData $ pdata prepared)
                                                                                    (pdata machinePre)
                                                                                    (pdata machinePost)
                                                                                    (pdata $ pblake2b_256 # (pserialiseData # armWitness))
                                                                                    (pdata $ pmatch (pasConstr # armWitness) $ \(PBuiltinPair armTag _) -> armTag)
                                                                                    (pdata group)
                                                                                    (pdata 0)
                                                                                    (pforgetData $ pconstrBuiltin # 0 # pnil)
                                                                     in continue policy step action ownRef tx (pelemAt # group # pfromData arms) (pforgetData $ pdata next)
                                                            )
                                                            perror
                                    )
                                    perror

settleValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData (PBuiltinList (PAsData PInteger)) :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
settleValidator = plam $ \award hops policy ctx -> pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PData policy datum redeemer ownRef tx $ \rawAction ->
        plet (decodeAction rawAction) $ \action -> pmatch action $ \a ->
            plet (pexpectDatum datum) $ \step -> pmatch step $ \d ->
                plet (Chain.pdecodeBound # pexpectState (pstep'data d)) $ \bound -> pmatch bound $ \b ->
                    pmatch (decodePrepared $ Chain.pbound'prepared b) $ \prepared ->
                        pmatch (pfromData $ Resolution.pprepared'resolution prepared) $ \resolution ->
                            let coreStep =
                                    pforgetData $
                                        pconstrBuiltin
                                            # 0
                                            # (pcons # pforgetData (Chain.pbound'pre b) # (pcons # pforgetData (Chain.pbound'post b) # (pcons # paction'witness a # pnil)))
                             in pif
                                    ( Chain.pbound'progress b
                                        #== pelemAt
                                        # pfromData (Chain.pbound'group b)
                                        # pfromData hops
                                        #&& Chain.pwitnessIsBound
                                        # bound
                                        # paction'witness a
                                        #&& Resolution.phashOneStepEvidence
                                        # pforgetData (paction'transition a)
                                        # auxiliary coreStep
                                        #== pfromData (Resolution.pprepared'evidenceHash prepared)
                                        #&& Cek.pverifyCekCoreSuccessor
                                        # pfromData (Resolution.presolution'preState resolution)
                                        # pfromData (paction'transition a)
                                        # pfromData (Chain.pbound'post b)
                                        # (Cek.pcekWitnessControlV1 # pfromData (paction'transition a))
                                    )
                                    (continue policy step action ownRef tx award $ pforgetData $ pdata Resolution.pwinningResolution)
                                    perror

auxiliary :: forall s. Term s PData -> Term s PData
auxiliary coreStep = pforgetData $ pconstrBuiltin # 12 # (pcons # coreStep # pnil)

continue :: forall s. Term s (PAsData PCurrencySymbol) -> Term s PStepDatum -> Term s PCoreAction -> Term s PTxOutRef -> Term s PTxInfo -> Term s (PAsData PScriptHash) -> Term s PData -> Term s PBool
continue policy step action ownRef tx next state = pmatch action $ \a -> pmatch tx $ \t ->
    Common.pcontinue
        policy
        step
        (pfromData $ paction'inputIndex a)
        (pfromData $ paction'outputIndex a)
        ownRef
        (pfromData $ ptxInfo'inputs t)
        (pfromData $ ptxInfo'outputs t)
        (\_ _ _ _ outputHash outputState -> outputHash #== next #&& outputState #== state)

decodeAction :: forall s. Term s PData -> Term s PCoreAction
decodeAction raw = plet (recordFields 4 raw) $ \f -> plet (recordFields 2 $ pelemAt # 2 # f) $ \transition ->
    pcon $
        PCoreAction
            (integerField f 0)
            (integerField f 1)
            (pdata $ pcon $ PValidationOneStepWitnessV1 (bytesField transition 0) (pdata $ decodeState $ pelemAt # 1 # transition))
            (pelemAt # 3 # f)
