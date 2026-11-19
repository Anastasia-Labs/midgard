{- | Checked Data openings for material carried by the physical CEK hops.
Constructor tags, field counts, and nested Data types match the Aiken types.
-}
module Midgard.CekMaterialWitness (
    pdecodeDataSummary,
    pdecodeDataNode,
    pdecodeDataListNode,
    pdecodeDataPairNode,
    pdecodeRuntimeValueWitness,
    pdecodeBlsExpressionWitness,
    pdecodeSemanticBuiltinWitness,
    pdecodeEnvironmentSummary,
    pdecodeMachineValueWitness,
    pdecodeMapConversionControl,
    pdecodeMapConversionStartWitness,
    pdecodeMachineArm,
    pdecodeMapControlArm,
    pdecodeCoreStepWitness,
) where

import Midgard.CekBuiltin qualified as Builtin
import Midgard.CekCoreWitness qualified as Core
import Midgard.CekData qualified as Data
import Midgard.CekMachine qualified as Machine
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

pdecodeDataSummary :: forall s. Term s (PData :--> Data.PDataSummaryV1)
pdecodeDataSummary = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (plength # fields #== 3) (pcon $ Data.PDataSummaryV1 (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields)))) perror) $
        perror

pdecodeDataNode :: forall s. Term s (PData :--> Data.PDataNodeV1)
pdecodeDataNode = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (plength # fields #== 5) (pcon $ Data.PConstrSmallData (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pasInt # (pelemAt # 3 # fields))) (pdata $ (pasInt # (pelemAt # 4 # fields)))) perror) $
        pif (tag #== 1) (pif (plength # fields #== 7) (pcon $ Data.PConstrLargeData (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasInt # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields))) (pdata $ (pasInt # (pelemAt # 5 # fields))) (pdata $ (pasInt # (pelemAt # 6 # fields)))) perror) $
            pif (tag #== 2) (pif (plength # fields #== 4) (pcon $ Data.PMapDataNode (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasInt # (pelemAt # 3 # fields)))) perror) $
                pif (tag #== 3) (pif (plength # fields #== 4) (pcon $ Data.PListDataNode (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasInt # (pelemAt # 3 # fields)))) perror) $
                    pif (tag #== 4) (pif (plength # fields #== 3) (pcon $ Data.PIntegerDataNode (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields)))) perror) $
                        pif (tag #== 5) (pif (plength # fields #== 4) (pcon $ Data.PBytesDataNode (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasInt # (pelemAt # 3 # fields)))) perror) $
                            perror

pdecodeDataListNode :: forall s. Term s (PData :--> Data.PDataListNodeV1)
pdecodeDataListNode = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (plength # fields #== 7) (pcon $ Data.PDataListNodeV1 (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasInt # (pelemAt # 4 # fields))) (pdata $ (pasInt # (pelemAt # 5 # fields))) (pdata $ (pasInt # (pelemAt # 6 # fields)))) perror) $
        perror

pdecodeDataPairNode :: forall s. Term s (PData :--> Data.PDataPairNodeV1)
pdecodeDataPairNode = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (plength # fields #== 10) (pcon $ Data.PDataPairNodeV1 (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasInt # (pelemAt # 4 # fields))) (pdata $ (pasInt # (pelemAt # 5 # fields))) (pdata $ (pasByteStr # (pelemAt # 6 # fields))) (pdata $ (pasInt # (pelemAt # 7 # fields))) (pdata $ (pasInt # (pelemAt # 8 # fields))) (pdata $ (pasInt # (pelemAt # 9 # fields)))) perror) $
        perror

pdecodeRuntimeValueWitness :: forall s. Term s (PData :--> Builtin.PRuntimeValueWitnessV1)
pdecodeRuntimeValueWitness = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (plength # fields #== 1) (pcon $ Builtin.PRuntimeConstantValue (pdata $ (Core.pdecodeConstant # (pelemAt # 0 # fields)))) perror) $
        pif (tag #== 1) (pif (plength # fields #== 3) (pcon $ Builtin.PRuntimeSemanticConstantValue (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pdecodeDataSummary # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields)))) perror) $
            pif (tag #== 2) (pif (plength # fields #== 2) (pcon $ Builtin.PRuntimeLambdaValue (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields)))) perror) $
                pif (tag #== 3) (pif (plength # fields #== 2) (pcon $ Builtin.PRuntimeDelayValue (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields)))) perror) $
                    pif (tag #== 4) (pif (plength # fields #== 3) (pcon $ Builtin.PRuntimeConstrValue (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields)))) perror) $
                        pif (tag #== 5) (pif (plength # fields #== 4) (pcon $ Builtin.PRuntimeBuiltinValue (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields)))) perror) $
                            pif (tag #== 6) (pif (plength # fields #== 1) (pcon $ Builtin.PRuntimeBlsMillerLoopValue (pdata $ (pasByteStr # (pelemAt # 0 # fields)))) perror) $
                                perror

pdecodeBlsExpressionWitness :: forall s. Term s (PData :--> Builtin.PBlsExpressionWitnessV1)
pdecodeBlsExpressionWitness = phoistAcyclic $ pfix $ \self -> plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (plength # fields #== 2) (pcon $ Builtin.PBlsMillerLoopExpression (pdata $ (Core.pdecodeConstant # (pelemAt # 0 # fields))) (pdata $ (Core.pdecodeConstant # (pelemAt # 1 # fields)))) perror) $
        pif (tag #== 1) (pif (plength # fields #== 2) (pcon $ Builtin.PBlsMultiplyExpression (pdata $ (self # (pelemAt # 0 # fields))) (pdata $ (self # (pelemAt # 1 # fields)))) perror) $
            perror

pdecodeSemanticBuiltinWitness :: forall s. Term s (PData :--> Builtin.PSemanticBuiltinWitnessV1)
pdecodeSemanticBuiltinWitness = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (plength # fields #== 4) (pcon $ Builtin.PSemanticBuiltinWitnessV1 (pdata $ (pmap # plam (\item -> (pdata $ (pdecodeDataNode # item))) # (pasList # (pelemAt # 0 # fields)))) (pdata $ (pmap # plam (\item -> (pdata $ (pdecodeDataListNode # item))) # (pasList # (pelemAt # 1 # fields)))) (pdata $ (pmap # plam (\item -> (pdata $ (pdecodeDataPairNode # item))) # (pasList # (pelemAt # 2 # fields)))) (pdata $ (pmap # plam (\item -> (pdata $ (pasByteStr # item))) # (pasList # (pelemAt # 3 # fields))))) perror) $
        perror

pdecodeEnvironmentSummary :: forall s. Term s (PData :--> Machine.PEnvironmentSummaryV1)
pdecodeEnvironmentSummary = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (plength # fields #== 0) (pcon $ Machine.PEmptyEnvironmentSummary) perror) $
        pif (tag #== 1) (pif (plength # fields #== 3) (pcon $ Machine.PNonEmptyEnvironmentSummary (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields)))) perror) $
            perror

pdecodeMachineValueWitness :: forall s. Term s (PData :--> Machine.PMachineValueWitnessV1)
pdecodeMachineValueWitness = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (plength # fields #== 5) (pcon $ Machine.PMachineConstantValue (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasInt # (pelemAt # 4 # fields)))) perror) $
        pif (tag #== 1) (pif (plength # fields #== 2) (pcon $ Machine.PMachineLambdaValue (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields)))) perror) $
            pif (tag #== 2) (pif (plength # fields #== 2) (pcon $ Machine.PMachineDelayValue (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields)))) perror) $
                pif (tag #== 3) (pif (plength # fields #== 3) (pcon $ Machine.PMachineConstrValue (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields)))) perror) $
                    pif (tag #== 4) (pif (plength # fields #== 4) (pcon $ Machine.PMachineBuiltinValue (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields)))) perror) $
                        pif (tag #== 5) (pif (plength # fields #== 1) (pcon $ Machine.PMachineBlsMillerLoopValue (pdata $ (pasByteStr # (pelemAt # 0 # fields)))) perror) $
                            perror

pdecodeMapConversionControl :: forall s. Term s (PData :--> Machine.PMapConversionControlV1)
pdecodeMapConversionControl = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (plength # fields #== 12) (pcon $ Machine.PMapConversionControlV1 (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pasInt # (pelemAt # 3 # fields))) (pdata $ (pasInt # (pelemAt # 4 # fields))) (pdata $ (pasInt # (pelemAt # 5 # fields))) (pdata $ (pasByteStr # (pelemAt # 6 # fields))) (pdata $ (pasInt # (pelemAt # 7 # fields))) (pdata $ (pasInt # (pelemAt # 8 # fields))) (pdata $ (pasInt # (pelemAt # 9 # fields))) (pdata $ (pasInt # (pelemAt # 10 # fields))) (pdata $ (pasInt # (pelemAt # 11 # fields)))) perror) $
        perror

pdecodeMapConversionStartWitness :: forall s. Term s (PData :--> Machine.PMapConversionStartWitnessV1)
pdecodeMapConversionStartWitness = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (plength # fields #== 6) (pcon $ Machine.PMapConversionStartWitnessV1 (pdata $ (pdecodeDataNode # (pelemAt # 0 # fields))) (pdata $ (pmatch (pasConstr # (pelemAt # 1 # fields)) $ \(PBuiltinPair tag values) -> pif (tag #== 1 #&& pnull # values) (pcon PDNothing) $ pif (tag #== 0 #&& plength # values #== 1) (pcon $ PDJust $ pdata $ (pdecodeDataListNode # (phead # values))) perror)) (pdata $ (pmatch (pasConstr # (pelemAt # 2 # fields)) $ \(PBuiltinPair tag values) -> pif (tag #== 1 #&& pnull # values) (pcon PDNothing) $ pif (tag #== 0 #&& plength # values #== 1) (pcon $ PDJust $ pdata $ (pdecodeDataPairNode # (phead # values))) perror)) (pdata $ (pdecodeDataNode # (pelemAt # 3 # fields))) (pdata $ (pmatch (pasConstr # (pelemAt # 4 # fields)) $ \(PBuiltinPair tag values) -> pif (tag #== 1 #&& pnull # values) (pcon PDNothing) $ pif (tag #== 0 #&& plength # values #== 1) (pcon $ PDJust $ pdata $ (pdecodeDataListNode # (phead # values))) perror)) (pdata $ (pmatch (pasConstr # (pelemAt # 5 # fields)) $ \(PBuiltinPair tag values) -> pif (tag #== 1 #&& pnull # values) (pcon PDNothing) $ pif (tag #== 0 #&& plength # values #== 1) (pcon $ PDJust $ pdata $ (pdecodeDataPairNode # (phead # values))) perror))) perror) $
        perror

pdecodeCoreStepWitness :: forall s. Term s (PData :--> Machine.PCoreStepWitnessV1)
pdecodeCoreStepWitness = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (plength # fields #== 1) (pcon $ Machine.PComputeVariable (pdata $ (pasInt # (pelemAt # 0 # fields)))) perror) $
        pif (tag #== 1) (pif (plength # fields #== 1) (pcon $ Machine.PComputeConstant (pdata $ (Core.pdecodeConstant # (pelemAt # 0 # fields)))) perror) $
            pif (tag #== 2) (pif (plength # fields #== 1) (pcon $ Machine.PComputeLambda (pdata $ (pasByteStr # (pelemAt # 0 # fields)))) perror) $
                pif (tag #== 3) (pif (plength # fields #== 1) (pcon $ Machine.PComputeDelay (pdata $ (pasByteStr # (pelemAt # 0 # fields)))) perror) $
                    pif (tag #== 4) (pif (plength # fields #== 2) (pcon $ Machine.PComputeApplication (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields)))) perror) $
                        pif (tag #== 5) (pif (plength # fields #== 1) (pcon $ Machine.PComputeForce (pdata $ (pasByteStr # (pelemAt # 0 # fields)))) perror) $
                            pif (tag #== 6) (pif (plength # fields #== 0) (pcon $ Machine.PComputeError) perror) $
                                pif (tag #== 7) (pif (plength # fields #== 1) (pcon $ Machine.PComputeBuiltin (pdata $ (pasInt # (pelemAt # 0 # fields)))) perror) $
                                    pif (tag #== 8) (pif (plength # fields #== 1) (pcon $ Machine.PComputeConstrEmpty (pdata $ (pasInt # (pelemAt # 0 # fields)))) perror) $
                                        pif (tag #== 9) (pif (plength # fields #== 4) (pcon $ Machine.PComputeConstrNonEmpty (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields)))) perror) $
                                            pif (tag #== 10) (pif (plength # fields #== 3) (pcon $ Machine.PComputeCase (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields)))) perror) $
                                                pif (tag #== 11) (pif (plength # fields #== 3) (pcon $ Machine.PLookupEnvironment (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields)))) perror) $
                                                    pif (tag #== 12) (pif (plength # fields #== 0) (pcon $ Machine.PLookupEmptyEnvironment) perror) $
                                                        pif (tag #== 13) (pif (plength # fields #== 1) (pcon $ Machine.PReturnEmptyContinuation (pdata $ (pdecodeMachineValueWitness # (pelemAt # 0 # fields)))) perror) $
                                                            pif (tag #== 14) (pif (plength # fields #== 3) (pcon $ Machine.PReturnApplyArgument (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields)))) perror) $
                                                                pif (tag #== 15) (pif (plength # fields #== 4) (pcon $ Machine.PReturnApplyLambda (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pdecodeEnvironmentSummary # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields)))) perror) $
                                                                    pif (tag #== 16) (pif (plength # fields #== 5) (pcon $ Machine.PReturnApplyBuiltin (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields)))) perror) $
                                                                        pif (tag #== 17) (pif (plength # fields #== 2) (pcon $ Machine.PReturnApplyInvalid (pdata $ (pdecodeMachineValueWitness # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields)))) perror) $
                                                                            pif (tag #== 18) (pif (plength # fields #== 5) (pcon $ Machine.PReturnApplyValueLambda (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pdecodeEnvironmentSummary # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields)))) perror) $
                                                                                pif (tag #== 19) (pif (plength # fields #== 6) (pcon $ Machine.PReturnApplyValueBuiltin (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasInt # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields))) (pdata $ (pasByteStr # (pelemAt # 5 # fields)))) perror) $
                                                                                    pif (tag #== 20) (pif (plength # fields #== 3) (pcon $ Machine.PReturnApplyValueInvalid (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pdecodeMachineValueWitness # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields)))) perror) $
                                                                                        pif (tag #== 21) (pif (plength # fields #== 3) (pcon $ Machine.PReturnForceDelay (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields)))) perror) $
                                                                                            pif (tag #== 22) (pif (plength # fields #== 5) (pcon $ Machine.PReturnForceBuiltin (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields)))) perror) $
                                                                                                pif (tag #== 23) (pif (plength # fields #== 2) (pcon $ Machine.PReturnForceInvalid (pdata $ (pdecodeMachineValueWitness # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields)))) perror) $
                                                                                                    pif (tag #== 24) (pif (plength # fields #== 8) (pcon $ Machine.PReturnConstrNext (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasInt # (pelemAt # 4 # fields))) (pdata $ (pasByteStr # (pelemAt # 5 # fields))) (pdata $ (pasByteStr # (pelemAt # 6 # fields))) (pdata $ (pasByteStr # (pelemAt # 7 # fields)))) perror) $
                                                                                                        pif (tag #== 25) (pif (plength # fields #== 5) (pcon $ Machine.PReturnConstrDone (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields)))) perror) $
                                                                                                            pif (tag #== 26) (pif (plength # fields #== 7) (pcon $ Machine.PReturnCaseConstr (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pasInt # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields))) (pdata $ (pasByteStr # (pelemAt # 5 # fields))) (pdata $ (pasByteStr # (pelemAt # 6 # fields)))) perror) $
                                                                                                                pif (tag #== 27) (pif (plength # fields #== 5) (pcon $ Machine.PReturnCaseInvalid (pdata $ (pdecodeMachineValueWitness # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields)))) perror) $
                                                                                                                    pif (tag #== 28) (pif (plength # fields #== 6) (pcon $ Machine.PSelectCaseBranch (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields))) (pdata $ (pasInt # (pelemAt # 5 # fields)))) perror) $
                                                                                                                        pif (tag #== 29) (pif (plength # fields #== 5) (pcon $ Machine.PApplyCaseValue (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields)))) perror) $
                                                                                                                            pif (tag #== 30) (pif (plength # fields #== 3) (pcon $ Machine.PExecuteBuiltinDirect (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pmap # plam (\item -> (pdata $ (Core.pdecodeValue # item))) # (pasList # (pelemAt # 1 # fields)))) (pdata $ (Core.pdecodeValue # (pelemAt # 2 # fields)))) perror) $
                                                                                                                                pif (tag #== 31) (pif (plength # fields #== 4) (pcon $ Machine.PExecuteBuiltinSemantic (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pmap # plam (\item -> (pdata $ (Core.pdecodeValue # item))) # (pasList # (pelemAt # 1 # fields)))) (pdata $ (Core.pdecodeValue # (pelemAt # 2 # fields))) (pdata $ (pdecodeSemanticBuiltinWitness # (pelemAt # 3 # fields)))) perror) $
                                                                                                                                    pif (tag #== 32) (pif (plength # fields #== 4) (pcon $ Machine.PStartBuiltinMapConversion (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pmap # plam (\item -> (pdata $ (Core.pdecodeValue # item))) # (pasList # (pelemAt # 1 # fields)))) (pdata $ (Core.pdecodeValue # (pelemAt # 2 # fields))) (pdata $ (pdecodeMapConversionStartWitness # (pelemAt # 3 # fields)))) perror) $
                                                                                                                                        pif (tag #== 33) (pif (plength # fields #== 8) (pcon $ Machine.PStepBuiltinListToMap (pdata $ (pdecodeMapConversionControl # (pelemAt # 0 # fields))) (pdata $ (pdecodeDataListNode # (pelemAt # 1 # fields))) (pdata $ (pdecodeDataNode # (pelemAt # 2 # fields))) (pdata $ (pdecodeDataListNode # (pelemAt # 3 # fields))) (pdata $ (pdecodeDataListNode # (pelemAt # 4 # fields))) (pdata $ (pdecodeDataNode # (pelemAt # 5 # fields))) (pdata $ (pdecodeDataNode # (pelemAt # 6 # fields))) (pdata $ (pdecodeDataPairNode # (pelemAt # 7 # fields)))) perror) $
                                                                                                                                            pif (tag #== 34) (pif (plength # fields #== 8) (pcon $ Machine.PStepBuiltinMapToList (pdata $ (pdecodeMapConversionControl # (pelemAt # 0 # fields))) (pdata $ (pdecodeDataPairNode # (pelemAt # 1 # fields))) (pdata $ (pdecodeDataListNode # (pelemAt # 2 # fields))) (pdata $ (pdecodeDataNode # (pelemAt # 3 # fields))) (pdata $ (pdecodeDataListNode # (pelemAt # 4 # fields))) (pdata $ (pdecodeDataListNode # (pelemAt # 5 # fields))) (pdata $ (pdecodeDataNode # (pelemAt # 6 # fields))) (pdata $ (pdecodeDataNode # (pelemAt # 7 # fields)))) perror) $
                                                                                                                                                pif (tag #== 35) (pif (plength # fields #== 1) (pcon $ Machine.PFinishBuiltinMapConversion (pdata $ (pdecodeMapConversionControl # (pelemAt # 0 # fields)))) perror) $
                                                                                                                                                    pif (tag #== 36) (pif (plength # fields #== 3) (pcon $ Machine.PExecuteBuiltinSemanticFailure (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pmap # plam (\item -> (pdata $ (Core.pdecodeValue # item))) # (pasList # (pelemAt # 1 # fields)))) (pdata $ (pdecodeSemanticBuiltinWitness # (pelemAt # 2 # fields)))) perror) $
                                                                                                                                                        pif (tag #== 37) (pif (plength # fields #== 5) (pcon $ Machine.PExecuteBuiltinBlsFinal (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pdecodeBlsExpressionWitness # (pelemAt # 2 # fields))) (pdata $ (pdecodeBlsExpressionWitness # (pelemAt # 3 # fields))) (pdata $ (Core.pdecodeValue # (pelemAt # 4 # fields)))) perror) $
                                                                                                                                                            pif (tag #== 38) (pif (plength # fields #== 2) (pcon $ Machine.PExecuteBuiltinFailure (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pmap # plam (\item -> (pdata $ (Core.pdecodeValue # item))) # (pasList # (pelemAt # 1 # fields))))) perror) $
                                                                                                                                                                pif (tag #== 39) (pif (plength # fields #== 2) (pcon $ Machine.PExecuteBuiltinTypeFailure (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pmap # plam (\item -> (pdata $ (pdecodeRuntimeValueWitness # item))) # (pasList # (pelemAt # 1 # fields))))) perror) $
                                                                                                                                                                    pif (tag #== 40) (pif (plength # fields #== 1) (pcon $ Machine.PComputeContextConstant (pdata $ (pasByteStr # (pelemAt # 0 # fields)))) perror) $
                                                                                                                                                                        perror

-- The caller rejects all other arms; retain only the reachable typed openings.
pdecodeMachineArm :: forall s. Term s (PData :--> Machine.PCoreStepWitnessV1)
pdecodeMachineArm = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 11) (pif (plength # fields #== 3) (pcon $ Machine.PLookupEnvironment (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields)))) perror) $
    pif (tag #== 12) (pif (plength # fields #== 0) (pcon $ Machine.PLookupEmptyEnvironment) perror) $
    pif (tag #== 13) (pif (plength # fields #== 1) (pcon $ Machine.PReturnEmptyContinuation (pdata $ (pdecodeMachineValueWitness # (pelemAt # 0 # fields)))) perror) $
    pif (tag #== 14) (pif (plength # fields #== 3) (pcon $ Machine.PReturnApplyArgument (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields)))) perror) $
    pif (tag #== 15) (pif (plength # fields #== 4) (pcon $ Machine.PReturnApplyLambda (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pdecodeEnvironmentSummary # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields)))) perror) $
    pif (tag #== 16) (pif (plength # fields #== 5) (pcon $ Machine.PReturnApplyBuiltin (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields)))) perror) $
    pif (tag #== 17) (pif (plength # fields #== 2) (pcon $ Machine.PReturnApplyInvalid (pdata $ (pdecodeMachineValueWitness # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields)))) perror) $
    pif (tag #== 18) (pif (plength # fields #== 5) (pcon $ Machine.PReturnApplyValueLambda (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pdecodeEnvironmentSummary # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields)))) perror) $
    pif (tag #== 19) (pif (plength # fields #== 6) (pcon $ Machine.PReturnApplyValueBuiltin (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasInt # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields))) (pdata $ (pasByteStr # (pelemAt # 5 # fields)))) perror) $
    pif (tag #== 20) (pif (plength # fields #== 3) (pcon $ Machine.PReturnApplyValueInvalid (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pdecodeMachineValueWitness # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields)))) perror) $
    pif (tag #== 21) (pif (plength # fields #== 3) (pcon $ Machine.PReturnForceDelay (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields)))) perror) $
    pif (tag #== 22) (pif (plength # fields #== 5) (pcon $ Machine.PReturnForceBuiltin (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields)))) perror) $
    pif (tag #== 23) (pif (plength # fields #== 2) (pcon $ Machine.PReturnForceInvalid (pdata $ (pdecodeMachineValueWitness # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields)))) perror) $
    pif (tag #== 24) (pif (plength # fields #== 8) (pcon $ Machine.PReturnConstrNext (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasInt # (pelemAt # 4 # fields))) (pdata $ (pasByteStr # (pelemAt # 5 # fields))) (pdata $ (pasByteStr # (pelemAt # 6 # fields))) (pdata $ (pasByteStr # (pelemAt # 7 # fields)))) perror) $
    pif (tag #== 25) (pif (plength # fields #== 5) (pcon $ Machine.PReturnConstrDone (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields)))) perror) $
    pif (tag #== 26) (pif (plength # fields #== 7) (pcon $ Machine.PReturnCaseConstr (pdata $ (pasInt # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pasInt # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields))) (pdata $ (pasByteStr # (pelemAt # 5 # fields))) (pdata $ (pasByteStr # (pelemAt # 6 # fields)))) perror) $
    pif (tag #== 27) (pif (plength # fields #== 5) (pcon $ Machine.PReturnCaseInvalid (pdata $ (pdecodeMachineValueWitness # (pelemAt # 0 # fields))) (pdata $ (pasInt # (pelemAt # 1 # fields))) (pdata $ (pasByteStr # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields)))) perror) $
    pif (tag #== 28) (pif (plength # fields #== 6) (pcon $ Machine.PSelectCaseBranch (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields))) (pdata $ (pasInt # (pelemAt # 5 # fields)))) perror) $
    pif (tag #== 29) (pif (plength # fields #== 5) (pcon $ Machine.PApplyCaseValue (pdata $ (pasByteStr # (pelemAt # 0 # fields))) (pdata $ (pasByteStr # (pelemAt # 1 # fields))) (pdata $ (pasInt # (pelemAt # 2 # fields))) (pdata $ (pasByteStr # (pelemAt # 3 # fields))) (pdata $ (pasByteStr # (pelemAt # 4 # fields)))) perror) $
    perror

-- The caller rejects all other arms; retain only the reachable typed openings.
pdecodeMapControlArm :: forall s. Term s (PData :--> Machine.PCoreStepWitnessV1)
pdecodeMapControlArm = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 33) (pif (plength # fields #== 8) (pcon $ Machine.PStepBuiltinListToMap (pdata $ (pdecodeMapConversionControl # (pelemAt # 0 # fields))) (pdata $ (pdecodeDataListNode # (pelemAt # 1 # fields))) (pdata $ (pdecodeDataNode # (pelemAt # 2 # fields))) (pdata $ (pdecodeDataListNode # (pelemAt # 3 # fields))) (pdata $ (pdecodeDataListNode # (pelemAt # 4 # fields))) (pdata $ (pdecodeDataNode # (pelemAt # 5 # fields))) (pdata $ (pdecodeDataNode # (pelemAt # 6 # fields))) (pdata $ (pdecodeDataPairNode # (pelemAt # 7 # fields)))) perror) $
    pif (tag #== 34) (pif (plength # fields #== 8) (pcon $ Machine.PStepBuiltinMapToList (pdata $ (pdecodeMapConversionControl # (pelemAt # 0 # fields))) (pdata $ (pdecodeDataPairNode # (pelemAt # 1 # fields))) (pdata $ (pdecodeDataListNode # (pelemAt # 2 # fields))) (pdata $ (pdecodeDataNode # (pelemAt # 3 # fields))) (pdata $ (pdecodeDataListNode # (pelemAt # 4 # fields))) (pdata $ (pdecodeDataListNode # (pelemAt # 5 # fields))) (pdata $ (pdecodeDataNode # (pelemAt # 6 # fields))) (pdata $ (pdecodeDataNode # (pelemAt # 7 # fields)))) perror) $
    pif (tag #== 35) (pif (plength # fields #== 1) (pcon $ Machine.PFinishBuiltinMapConversion (pdata $ (pdecodeMapConversionControl # (pelemAt # 0 # fields)))) perror) $
    perror
