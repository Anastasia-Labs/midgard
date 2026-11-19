-- | The fixed-target CEK core's authenticated physical semantic chain.
module Midgard.CekSemanticChain (
    PCoreBound (..),
    PArmAction (..),
    PBuiltinSuccess (..),
    PBuiltinRoots (..),
    PSemanticRoots (..),
    PSemanticArguments (..),
    PMapProjection (..),
    pdecodeBound,
    pdecodeAction,
    pdecodeBuiltinRoots,
    pdecodeSemanticRoots,
    pdecodeSemanticArguments,
    pdecodeMapProjection,
    pwitnessIsBound,
    pcontinueCore,
    pcoreGroup,
    pdecodeBuiltinSuccess,
    psemanticGroup,
    psemanticItem,
    pmapItem,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.CekBuiltin qualified as Builtin
import Midgard.CekCoreWitness qualified as Witness
import Midgard.CekMachine qualified as Machine
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.FraudProofs.Common qualified as Common
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

-- Prepared validation evidence remains opaque between bind and settle.
data PCoreBound s = PCoreBound
    { pbound'prepared :: Term s PData
    , pbound'pre :: Term s (PAsData Machine.PMachineStateV1)
    , pbound'post :: Term s (PAsData Machine.PMachineStateV1)
    , pbound'witnessHash :: Term s (PAsData PByteString)
    , pbound'arm :: Term s (PAsData PInteger)
    , pbound'group :: Term s (PAsData PInteger)
    , pbound'progress :: Term s (PAsData PInteger)
    , pbound'facts :: Term s PData
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PCoreBound)

data PArmAction s = PVerifyArm
    { parm'inputIndex :: Term s (PAsData PInteger)
    , parm'outputIndex :: Term s (PAsData PInteger)
    , parm'witness :: Term s PData
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PArmAction)

data PBuiltinSuccess s = PBuiltinSuccess
    { psuccess'tag :: Term s (PAsData PInteger)
    , psuccess'arguments :: Term s (PAsData (PBuiltinList (PAsData Builtin.PValueWitnessV1)))
    , psuccess'result :: Term s (PAsData Builtin.PValueWitnessV1)
    , psuccess'material :: Term s PData
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PBuiltinSuccess)

data PBuiltinRoots s = PBuiltinRoots
    { proots'argumentsRoot :: Term s (PAsData PByteString)
    , proots'argumentsCount :: Term s (PAsData PInteger)
    , proots'resultRoot :: Term s (PAsData PByteString)
    , proots'builtinRoot :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PBuiltinRoots)

data PSemanticRoots s = PSemanticRoots
    { psemantic'roots :: Term s (PAsData PBuiltinRoots)
    , psemantic'arguments :: Term s (PAsData (PBuiltinList (PAsData Builtin.PValueWitnessV1)))
    , psemantic'result :: Term s (PAsData Builtin.PValueWitnessV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PSemanticRoots)

data PSemanticArguments s = PSemanticArguments
    { parguments'root :: Term s (PAsData PByteString)
    , parguments'count :: Term s (PAsData PInteger)
    , parguments'builtinRoot :: Term s (PAsData PByteString)
    , parguments'arguments :: Term s (PAsData (PBuiltinList (PAsData Builtin.PValueWitnessV1)))
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PSemanticArguments)

data PMapProjection s = PMapProjection
    { pmap'arguments :: Term s (PAsData (PBuiltinList (PAsData Builtin.PValueWitnessV1)))
    , pmap'result :: Term s (PAsData Builtin.PValueWitnessV1)
    , pmap'budget :: Term s PData
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PMapProjection)

pfields :: forall s. Term s PInteger -> Term s PData -> Term s (PBuiltinList PData)
pfields count raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0 #&& plength # fields #== count) fields perror
pint :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s (PAsData PInteger)
pint fields index = pdata $ pasInt # (pelemAt # index # fields)
pbytes :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s (PAsData PByteString)
pbytes fields index = pdata $ pasByteStr # (pelemAt # index # fields)
pvalues :: forall s. Term s PData -> Term s (PAsData (PBuiltinList (PAsData Builtin.PValueWitnessV1)))
pvalues raw = pdata $ pmap # plam (\v -> pdata $ Witness.pdecodeValue # v) # (pasList # raw)

pdecodeBound :: forall s. Term s (PData :--> PCoreBound)
pdecodeBound = phoistAcyclic $ plam $ \raw -> plet (pfields 8 raw) $ \f ->
    pcon $
        PCoreBound
            (pelemAt # 0 # f)
            (pdata $ Witness.pdecodeMachine # (pelemAt # 1 # f))
            (pdata $ Witness.pdecodeMachine # (pelemAt # 2 # f))
            (pbytes f 3)
            (pint f 4)
            (pint f 5)
            (pint f 6)
            (pelemAt # 7 # f)
pdecodeAction :: forall s. Term s (PData :--> PArmAction)
pdecodeAction = phoistAcyclic $ plam $ \raw -> plet (pfields 3 raw) $ \f -> pcon $ PVerifyArm (pint f 0) (pint f 1) (pelemAt # 2 # f)
pdecodeBuiltinRoots :: forall s. Term s (PData :--> PBuiltinRoots)
pdecodeBuiltinRoots = phoistAcyclic $ plam $ \raw -> plet (pfields 4 raw) $ \f ->
    pcon $ PBuiltinRoots (pbytes f 0) (pint f 1) (pbytes f 2) (pbytes f 3)
pdecodeSemanticRoots :: forall s. Term s (PData :--> PSemanticRoots)
pdecodeSemanticRoots = phoistAcyclic $ plam $ \raw -> plet (pfields 3 raw) $ \f ->
    pcon $ PSemanticRoots (pdata $ pdecodeBuiltinRoots # (pelemAt # 0 # f)) (pvalues $ pelemAt # 1 # f) (pdata $ Witness.pdecodeValue # (pelemAt # 2 # f))
pdecodeSemanticArguments :: forall s. Term s (PData :--> PSemanticArguments)
pdecodeSemanticArguments = phoistAcyclic $ plam $ \raw -> plet (pfields 4 raw) $ \f ->
    pcon $ PSemanticArguments (pbytes f 0) (pint f 1) (pbytes f 2) (pvalues $ pelemAt # 3 # f)
pdecodeMapProjection :: forall s. Term s (PData :--> PMapProjection)
pdecodeMapProjection = phoistAcyclic $ plam $ \raw -> plet (pfields 3 raw) $ \f ->
    pcon $ PMapProjection (pvalues $ pelemAt # 0 # f) (pdata $ Witness.pdecodeValue # (pelemAt # 1 # f)) (pelemAt # 2 # f)

pwitnessIsBound :: forall s. Term s (PCoreBound :--> PData :--> PBool)
pwitnessIsBound = phoistAcyclic $ plam $ \bound witness -> pmatch bound $ \b ->
    pblake2b_256
        # (pserialiseData # witness)
        #== pfromData (pbound'witnessHash b)
        #&& (pmatch (pasConstr # witness) $ \(PBuiltinPair arm _) -> arm #== pfromData (pbound'arm b))

pcontinueCore :: forall s. Term s (PAsData PScriptHash) -> Term s (PAsData PCurrencySymbol) -> Term s PStepDatum -> Term s PArmAction -> Term s PInteger -> Term s PData -> Term s PTxOutRef -> Term s PTxInfo -> Term s PBool
pcontinueCore next policy step action expected facts ownRef tx = pmatch step $ \d -> pmatch action $ \a -> pmatch tx $ \t ->
    pmatch (pstep'data d) $ \case
        PDNothing -> perror
        PDJust raw -> plet (pdecodeBound # pfromData raw) $ \bound -> pmatch bound $ \b ->
            pif
                (pfromData (pbound'progress b) #== expected #&& pwitnessIsBound # bound # parm'witness a)
                ( Common.pcontinue
                    policy
                    step
                    (pfromData $ parm'inputIndex a)
                    (pfromData $ parm'outputIndex a)
                    ownRef
                    (pfromData $ ptxInfo'inputs t)
                    (pfromData $ ptxInfo'outputs t)
                    (\_ _ _ _ outputHash outputState -> outputHash #== next #&& outputState #== pforgetData (pdata $ pcon b{pbound'progress = pdata $ pfromData (pbound'progress b) + 1, pbound'facts = facts}))
                )
                perror

pcoreGroup :: forall s. Term s (PInteger :--> PData :--> PInteger)
pcoreGroup = phoistAcyclic $ plam $ \mode witness -> pmatch (pasConstr # witness) $ \(PBuiltinPair arm fields) ->
    pif (mode #== Machine.pmodeCompute) (pif ((arm #>= 0 #&& arm #<= 10) #|| arm #== 40) 0 perror) $
        pif (mode #== Machine.pmodeLookup) (pif (arm #== 11 #|| arm #== 12) 1 perror) $
            pif (mode #== Machine.pmodeReturn) (pif (arm #>= 13 #&& arm #<= 27) 1 perror) $
                pif (mode #== Machine.pmodeCaseSelect) (pif (arm #== 28) 1 perror) $
                    pif (mode #== Machine.pmodeCaseApply) (pif (arm #== 29) 1 perror) $
                        pif (mode #== Machine.pmodeSemanticBuiltin) (pif (arm #>= 33 #&& arm #<= 35) 2 perror) $
                            pif
                                (mode #== Machine.pmodeBuiltin)
                                ( pif (arm #== 30) (pif (plength # fields #== 3) (plet (pasInt # (phead # fields)) $ \tag -> pif (tag #>= 29 #&& tag #<= 69 #&& pnot # (tag #== 52 #|| tag #== 53)) 4 3) perror) $
                                    pif (arm #== 31) 5 $
                                        pif (arm #== 32) 6 $
                                            pif (arm #== 36) 7 $
                                                pif (arm #== 37) 8 $
                                                    pif (arm #== 38) 9 $
                                                        pif (arm #== 39) 10 perror
                                )
                                perror

pdecodeBuiltinSuccess :: forall s. Term s (PInteger :--> PData :--> PBuiltinSuccess)
pdecodeBuiltinSuccess = phoistAcyclic $ plam $ \arm raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif
        (tag #== arm #&& pif (arm #== 30) (plength # fields #== 3) ((arm #== 31 #|| arm #== 32) #&& plength # fields #== 4))
        ( pcon $
            PBuiltinSuccess
                (pint fields 0)
                (pvalues $ pelemAt # 1 # fields)
                (pdata $ Witness.pdecodeValue # (pelemAt # 2 # fields))
                (pif (arm #== 30) (pforgetData $ pconstrBuiltin # 0 # pnil) (pelemAt # 3 # fields))
        )
        perror

psemanticGroup :: forall s. Term s (PInteger :--> PInteger)
psemanticGroup = phoistAcyclic $ plam $ \tag ->
    pif (tag #== 29 #|| tag #== 30) 0 $
        pif (tag #== 31 #|| tag #== 32 #|| tag #== 35) 1 $
            pif (tag #== 33 #|| tag #== 34) 2 $
                pif (tag #== 36) 3 $
                    pif (tag #== 37 #|| tag #== 42) 4 $
                        pif (tag #>= 47 #&& tag #<= 51) 6 $
                            pif (tag #== 39 #|| tag #== 40 #|| tag #== 41 #|| tag #== 44 #|| tag #== 45 #|| tag #== 46) 5 perror

psemanticItem, pmapItem :: forall s. Term s (PCoreBound :--> PData :--> PBuiltinSuccess)
psemanticItem = phoistAcyclic $ plam $ \bound witness -> pmatch bound $ \b ->
    pmatch (pdecodeBuiltinSuccess # pfromData (pbound'arm b) # witness) $ \raw ->
        pmatch (pdecodeSemanticRoots # pbound'facts b) $ \facts -> pcon raw{psuccess'arguments = psemantic'arguments facts, psuccess'result = psemantic'result facts}
pmapItem = phoistAcyclic $ plam $ \bound witness -> pmatch bound $ \b ->
    pmatch (pdecodeBuiltinSuccess # pfromData (pbound'arm b) # witness) $ \raw ->
        pmatch (pdecodeMapProjection # pbound'facts b) $ \facts -> pcon raw{psuccess'arguments = pmap'arguments facts, psuccess'result = pmap'result facts}
