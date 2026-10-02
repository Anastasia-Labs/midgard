import type { MidgardCekMachineState } from "@al-ft/midgard-core";
import { lucidDataToCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  blsExpressionData,
  bytesData,
  type CekData,
  constantWitnessData,
  dataListNodeData,
  dataNodeData,
  dataPairNodeData,
  directValueData,
  environmentSummaryData,
  machineValueData,
  mapConversionControlData,
  mapConversionStartWitnessData,
  midgardCekMachineStateData,
  runtimeValueData,
  semanticBuiltinWitnessData,
  unreachable,
} from "./cek-data.data-node-data.js";
import type { MidgardCekCoreStepWitness } from "./cek-machine.js";

export const midgardCekCoreStepWitnessData = (
  witness: MidgardCekCoreStepWitness,
): CekData => {
  switch (witness.kind) {
    case "computeVariable":
      return new Constr(0, [witness.index]);
    case "computeConstant":
      return new Constr(1, [constantWitnessData(witness.value)]);
    case "computeLambda":
      return new Constr(2, [bytesData(witness.body)]);
    case "computeDelay":
      return new Constr(3, [bytesData(witness.body)]);
    case "computeApplication":
      return new Constr(4, [
        bytesData(witness.function),
        bytesData(witness.argument),
      ]);
    case "computeForce":
      return new Constr(5, [bytesData(witness.term)]);
    case "computeError":
      return new Constr(6, []);
    case "computeBuiltin":
      return new Constr(7, [witness.tag]);
    case "computeConstrEmpty":
      return new Constr(8, [witness.tag]);
    case "computeConstrNonempty":
      return new Constr(9, [
        witness.tag,
        witness.termsCount,
        bytesData(witness.firstTerm),
        bytesData(witness.remainingTermsRoot),
      ]);
    case "computeCase":
      return new Constr(10, [
        bytesData(witness.scrutinee),
        witness.branchesCount,
        bytesData(witness.branchesRoot),
      ]);
    case "lookupEnvironment":
      return new Constr(11, [
        bytesData(witness.value),
        bytesData(witness.tail),
        witness.length,
      ]);
    case "lookupEmptyEnvironment":
      return new Constr(12, []);
    case "returnEmptyContinuation":
      return new Constr(13, [machineValueData(witness.value)]);
    case "returnApplyArgument":
      return new Constr(14, [
        bytesData(witness.argument),
        bytesData(witness.capturedEnvironment),
        bytesData(witness.tail),
      ]);
    case "returnApplyLambda":
      return new Constr(15, [
        bytesData(witness.body),
        bytesData(witness.closureEnvironment),
        environmentSummaryData(witness.closureSummary),
        bytesData(witness.tail),
      ]);
    case "returnApplyBuiltin":
      return new Constr(16, [
        witness.tag,
        witness.forcesRemaining,
        witness.argumentsCount,
        bytesData(witness.argumentsRoot),
        bytesData(witness.tail),
      ]);
    case "returnApplyInvalid":
      return new Constr(17, [
        machineValueData(witness.function),
        bytesData(witness.tail),
      ]);
    case "returnApplyValueLambda":
      return new Constr(18, [
        bytesData(witness.argument),
        bytesData(witness.body),
        bytesData(witness.closureEnvironment),
        environmentSummaryData(witness.closureSummary),
        bytesData(witness.tail),
      ]);
    case "returnApplyValueBuiltin":
      return new Constr(19, [
        bytesData(witness.argument),
        witness.tag,
        witness.forcesRemaining,
        witness.argumentsCount,
        bytesData(witness.argumentsRoot),
        bytesData(witness.tail),
      ]);
    case "returnApplyValueInvalid":
      return new Constr(20, [
        bytesData(witness.argument),
        machineValueData(witness.function),
        bytesData(witness.tail),
      ]);
    case "returnForceDelay":
      return new Constr(21, [
        bytesData(witness.body),
        bytesData(witness.closureEnvironment),
        bytesData(witness.tail),
      ]);
    case "returnForceBuiltin":
      return new Constr(22, [
        witness.tag,
        witness.forcesRemaining,
        witness.argumentsCount,
        bytesData(witness.argumentsRoot),
        bytesData(witness.tail),
      ]);
    case "returnForceInvalid":
      return new Constr(23, [
        machineValueData(witness.value),
        bytesData(witness.tail),
      ]);
    case "returnConstrNext":
      return new Constr(24, [
        witness.tag,
        witness.remainingTermsCount,
        bytesData(witness.nextTerm),
        bytesData(witness.remainingTermsTail),
        witness.valuesCount,
        bytesData(witness.valuesRoot),
        bytesData(witness.capturedEnvironment),
        bytesData(witness.tail),
      ]);
    case "returnConstrDone":
      return new Constr(25, [
        witness.tag,
        witness.valuesCount,
        bytesData(witness.valuesRoot),
        bytesData(witness.capturedEnvironment),
        bytesData(witness.tail),
      ]);
    case "returnCaseConstr":
      return new Constr(26, [
        witness.tag,
        witness.valuesCount,
        bytesData(witness.valuesRoot),
        witness.branchesCount,
        bytesData(witness.branchesRoot),
        bytesData(witness.capturedEnvironment),
        bytesData(witness.tail),
      ]);
    case "returnCaseInvalid":
      return new Constr(27, [
        machineValueData(witness.value),
        witness.branchesCount,
        bytesData(witness.branchesRoot),
        bytesData(witness.capturedEnvironment),
        bytesData(witness.tail),
      ]);
    case "selectCaseBranch":
      return new Constr(28, [
        bytesData(witness.branch),
        bytesData(witness.remainingBranchesRoot),
        witness.length,
        bytesData(witness.capturedEnvironment),
        bytesData(witness.tail),
        witness.valuesCount,
      ]);
    case "applyCaseValue":
      return new Constr(29, [
        bytesData(witness.value),
        bytesData(witness.remainingValuesRoot),
        witness.length,
        bytesData(witness.capturedEnvironment),
        bytesData(witness.builtContinuation),
      ]);
    case "executeBuiltinDirect":
      return new Constr(30, [
        witness.tag,
        witness.arguments.map(directValueData),
        directValueData(witness.result),
      ]);
    case "executeBuiltinSemantic":
      return new Constr(31, [
        witness.tag,
        witness.arguments.map(directValueData),
        directValueData(witness.result),
        semanticBuiltinWitnessData(witness.material),
      ]);
    case "startBuiltinMapConversion":
      return new Constr(32, [
        witness.tag,
        witness.arguments.map(directValueData),
        directValueData(witness.result),
        mapConversionStartWitnessData(witness.material),
      ]);
    case "stepBuiltinListToMap":
      return new Constr(33, [
        mapConversionControlData(witness.control),
        dataListNodeData(witness.source),
        dataNodeData(witness.pair),
        dataListNodeData(witness.first),
        dataListNodeData(witness.second),
        dataNodeData(witness.key),
        dataNodeData(witness.value),
        dataPairNodeData(witness.destination),
      ]);
    case "stepBuiltinMapToList":
      return new Constr(34, [
        mapConversionControlData(witness.control),
        dataPairNodeData(witness.source),
        dataListNodeData(witness.destination),
        dataNodeData(witness.pair),
        dataListNodeData(witness.first),
        dataListNodeData(witness.second),
        dataNodeData(witness.key),
        dataNodeData(witness.value),
      ]);
    case "finishBuiltinMapConversion":
      return new Constr(35, [mapConversionControlData(witness.control)]);
    case "executeBuiltinSemanticFailure":
      return new Constr(36, [
        witness.tag,
        witness.arguments.map(directValueData),
        semanticBuiltinWitnessData(witness.material),
      ]);
    case "executeBuiltinBlsFinal":
      return new Constr(37, [
        bytesData(witness.leftRoot),
        bytesData(witness.rightRoot),
        blsExpressionData(witness.leftExpression),
        blsExpressionData(witness.rightExpression),
        directValueData(witness.result),
      ]);
    case "executeBuiltinFailure":
      return new Constr(38, [
        witness.tag,
        witness.arguments.map(directValueData),
      ]);
    case "executeBuiltinTypeFailure":
      return new Constr(39, [
        witness.tag,
        witness.arguments.map(runtimeValueData),
      ]);
    case "computeContextConstant":
      return new Constr(40, [bytesData(witness.valueRoot)]);
    default:
      return unreachable(witness);
  }
};

export const midgardCekCoreStepData = (step: {
  readonly pre: MidgardCekMachineState;
  readonly post: MidgardCekMachineState;
  readonly witness: MidgardCekCoreStepWitness;
}): CekData =>
  new Constr(0, [
    midgardCekMachineStateData(step.pre),
    midgardCekMachineStateData(step.post),
    midgardCekCoreStepWitnessData(step.witness),
  ]);

export const encodeMidgardCekCoreStepDataCbor = (step: {
  readonly pre: MidgardCekMachineState;
  readonly post: MidgardCekMachineState;
  readonly witness: MidgardCekCoreStepWitness;
}): Buffer =>
  lucidDataToCborIterative(midgardCekCoreStepData(step) as unknown as Data);
