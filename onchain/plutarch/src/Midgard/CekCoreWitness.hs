-- | Narrow checked openings used by the physical CEK semantic hops.
module Midgard.CekCoreWitness (
    pdecodeCompute,
    pdecodeConstant,
    pdecodeValue,
    pdecodeMachine,
) where

import Midgard.CekBuiltin qualified as Builtin
import Midgard.CekConstant qualified as Constant
import Midgard.CekData qualified as Data
import Midgard.CekMachine qualified as Machine
import Plutarch.Prelude

pfields :: forall s. Term s PInteger -> Term s PInteger -> Term s PData -> Term s (PBuiltinList PData)
pfields tag count raw = pmatch (pasConstr # raw) $ \(PBuiltinPair actual fields) ->
    pif (actual #== tag #&& plength # fields #== count) fields perror

pint :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s (PAsData PInteger)
pint fields index = pdata $ pasInt # (pelemAt # index # fields)
pbytes :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s (PAsData PByteString)
pbytes fields index = pdata $ pasByteStr # (pelemAt # index # fields)

pdecodeConstant :: forall s. Term s (PData :--> Constant.PConstantWitnessV1)
pdecodeConstant = phoistAcyclic $ plam $ \raw -> plet (pfields 0 2 raw) $ \f ->
    pcon $ Constant.PConstantWitnessV1 (pbytes f 0) (pbytes f 1)

pdecodeValue :: forall s. Term s (PData :--> Builtin.PValueWitnessV1)
pdecodeValue = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif
        (tag #== 0)
        (pif (plength # fields #== 1) (pcon $ Builtin.PConstantValue $ pdata $ pdecodeConstant # (phead # fields)) perror)
        $ pif
            (tag #== 1)
            ( pif
                (plength # fields #== 3)
                ( plet (pfields 0 3 $ pelemAt # 1 # fields) $ \summary ->
                    pcon $
                        Builtin.PSemanticConstantValue
                            (pbytes fields 0)
                            (pdata $ pcon $ Data.PDataSummaryV1 (pbytes summary 0) (pint summary 1) (pint summary 2))
                            (pint fields 2)
                )
                perror
            )
        $ pif
            (tag #== 2)
            (pif (plength # fields #== 1) (pcon $ Builtin.POpaqueValue $ pbytes fields 0) perror)
        $ pif
            (tag #== 3)
            (pif (plength # fields #== 1) (pcon $ Builtin.PBlsMillerLoopValue $ pbytes fields 0) perror)
            perror

pdecodeMachine :: forall s. Term s (PData :--> Machine.PMachineStateV1)
pdecodeMachine = phoistAcyclic $ plam $ \raw -> plet (pfields 0 8 raw) $ \f ->
    pcon $ Machine.PMachineStateV1 (pint f 0) (pint f 1) (pbytes f 2) (pbytes f 3) (pbytes f 4) (pint f 5) (pint f 6) (pint f 7)

-- Unlike the aggregate witness opening, the compute hop accepts exactly the
-- twelve compute constructors and checks every field's Data type.
pdecodeCompute :: forall s. Term s (PData :--> Machine.PCoreStepWitnessV1)
pdecodeCompute = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
    pif (tag #== 0) (pif (plength # f #== 1) (pcon $ Machine.PComputeVariable $ pint f 0) perror) $
        pif (tag #== 1) (pif (plength # f #== 1) (pcon $ Machine.PComputeConstant $ pdata $ pdecodeConstant # (phead # f)) perror) $
            pif (tag #== 2) (pif (plength # f #== 1) (pcon $ Machine.PComputeLambda $ pbytes f 0) perror) $
                pif (tag #== 3) (pif (plength # f #== 1) (pcon $ Machine.PComputeDelay $ pbytes f 0) perror) $
                    pif (tag #== 4) (pif (plength # f #== 2) (pcon $ Machine.PComputeApplication (pbytes f 0) (pbytes f 1)) perror) $
                        pif (tag #== 5) (pif (plength # f #== 1) (pcon $ Machine.PComputeForce $ pbytes f 0) perror) $
                            pif (tag #== 6) (pif (pnull # f) (pcon Machine.PComputeError) perror) $
                                pif (tag #== 7) (pif (plength # f #== 1) (pcon $ Machine.PComputeBuiltin $ pint f 0) perror) $
                                    pif (tag #== 8) (pif (plength # f #== 1) (pcon $ Machine.PComputeConstrEmpty $ pint f 0) perror) $
                                        pif (tag #== 9) (pif (plength # f #== 4) (pcon $ Machine.PComputeConstrNonEmpty (pint f 0) (pint f 1) (pbytes f 2) (pbytes f 3)) perror) $
                                            pif (tag #== 10) (pif (plength # f #== 3) (pcon $ Machine.PComputeCase (pbytes f 0) (pint f 1) (pbytes f 2)) perror) $
                                                pif (tag #== 40) (pif (plength # f #== 1) (pcon $ Machine.PComputeContextConstant $ pbytes f 0) perror) perror
