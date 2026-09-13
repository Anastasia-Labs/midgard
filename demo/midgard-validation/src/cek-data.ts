import type {
  MidgardCekDataListNode,
  MidgardCekDataNode,
  MidgardCekDataPairNode,
  MidgardCekMachineState,
  MidgardCekValueNode,
} from "@al-ft/midgard-core";
import { Constr, Data } from "@lucid-evolution/lucid";

import type {
  MidgardCekBlsExpressionWitness,
  MidgardCekDirectValueWitness,
  MidgardCekRuntimeValueWitness,
} from "./cek-builtin.js";
import type { MidgardCekConstantWitness } from "./cek-constant.js";
import type {
  MidgardCekCoreStepWitness,
  MidgardCekEnvironmentSummary,
  MidgardCekMapConversionControl,
  MidgardCekMapConversionStartWitness,
  MidgardCekSemanticBuiltinWitness,
} from "./cek-machine.js";

type CekData = Constr<unknown>;

const bytesData = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

const unreachable = (value: never): never => {
  throw new Error(`unknown V1 CEK data variant ${String(value)}`);
};

export const midgardCekMachineStateData = (
  state: MidgardCekMachineState,
): CekData =>
  new Constr(0, [
    BigInt(
      {
        compute: 0,
        return: 1,
        lookup: 2,
        builtin: 3,
        haltSuccess: 4,
        haltError: 5,
        caseSelect: 6,
        caseApply: 7,
        semanticBuiltin: 8,
      }[state.mode],
    ),
    state.executionIndex,
    bytesData(state.focusRoot),
    bytesData(state.environmentRoot),
    bytesData(state.continuationRoot),
    state.auxiliary,
    state.cpu,
    state.memory,
  ]);

const constantWitnessData = (witness: MidgardCekConstantWitness): CekData =>
  new Constr(0, [bytesData(witness.typeCbor), bytesData(witness.payloadCbor)]);

const environmentSummaryData = (
  summary: MidgardCekEnvironmentSummary,
): CekData =>
  summary.kind === "empty"
    ? new Constr(0, [])
    : new Constr(1, [
        bytesData(summary.value),
        bytesData(summary.tail),
        summary.length,
      ]);

const machineValueData = (value: MidgardCekValueNode): CekData => {
  switch (value.kind) {
    case "constant":
      return new Constr(0, [
        bytesData(value.typeRoot),
        bytesData(value.payloadRoot),
        value.payloadLength,
        bytesData(value.semanticRoot),
        value.memory,
      ]);
    case "lambda":
      return new Constr(1, [
        bytesData(value.body),
        bytesData(value.environment),
      ]);
    case "delay":
      return new Constr(2, [
        bytesData(value.body),
        bytesData(value.environment),
      ]);
    case "constr":
      return new Constr(3, [
        value.tag,
        value.valuesCount,
        bytesData(value.valuesRoot),
      ]);
    case "builtin":
      return new Constr(4, [
        value.tag,
        value.forcesRemaining,
        value.argumentsCount,
        bytesData(value.argumentsRoot),
      ]);
    case "blsMillerLoop":
      return new Constr(5, [bytesData(value.expressionRoot)]);
    default:
      return unreachable(value);
  }
};

const directValueData = (value: MidgardCekDirectValueWitness): CekData => {
  switch (value.kind) {
    case "constant":
      return new Constr(0, [constantWitnessData(value.witness)]);
    case "semanticConstant":
      return new Constr(1, [
        bytesData(value.witness.typeCbor),
        new Constr(0, [
          bytesData(value.witness.payload.root),
          value.witness.payload.cborLength,
          value.witness.payload.memory,
        ]),
        value.witness.memory,
      ]);
    case "opaque":
      return new Constr(2, [bytesData(value.root)]);
    case "blsMillerLoop":
      return new Constr(3, [bytesData(value.expressionRoot)]);
    default:
      return unreachable(value);
  }
};

const runtimeValueData = (value: MidgardCekRuntimeValueWitness): CekData => {
  switch (value.kind) {
    case "constant":
      return new Constr(0, [constantWitnessData(value.witness)]);
    case "semanticConstant":
      return new Constr(1, [
        bytesData(value.witness.typeCbor),
        new Constr(0, [
          bytesData(value.witness.payload.root),
          value.witness.payload.cborLength,
          value.witness.payload.memory,
        ]),
        value.witness.memory,
      ]);
    case "lambda":
      return new Constr(2, [
        bytesData(value.body),
        bytesData(value.environment),
      ]);
    case "delay":
      return new Constr(3, [
        bytesData(value.body),
        bytesData(value.environment),
      ]);
    case "constr":
      return new Constr(4, [
        value.tag,
        value.valuesCount,
        bytesData(value.valuesRoot),
      ]);
    case "builtin":
      return new Constr(5, [
        value.tag,
        value.forcesRemaining,
        value.argumentsCount,
        bytesData(value.argumentsRoot),
      ]);
    case "blsMillerLoop":
      return new Constr(6, [bytesData(value.expressionRoot)]);
    default:
      return unreachable(value);
  }
};

const blsExpressionData = (
  expression: MidgardCekBlsExpressionWitness,
): CekData =>
  expression.kind === "millerLoop"
    ? new Constr(0, [
        constantWitnessData(expression.g1),
        constantWitnessData(expression.g2),
      ])
    : new Constr(1, [
        blsExpressionData(expression.left),
        blsExpressionData(expression.right),
      ]);

const dataNodeData = (node: MidgardCekDataNode): CekData => {
  switch (node.kind) {
    case "constrSmall":
      return new Constr(0, [
        node.constructor,
        node.fieldsCount,
        bytesData(node.fieldsRoot),
        node.cborLength,
        node.memory,
      ]);
    case "constrLarge":
      return new Constr(1, [
        bytesData(node.constructorCborRoot),
        node.constructorCborLength,
        node.constructorMemory,
        node.fieldsCount,
        bytesData(node.fieldsRoot),
        node.cborLength,
        node.memory,
      ]);
    case "map":
      return new Constr(2, [
        node.entriesCount,
        bytesData(node.entriesRoot),
        node.cborLength,
        node.memory,
      ]);
    case "list":
      return new Constr(3, [
        node.itemsCount,
        bytesData(node.itemsRoot),
        node.cborLength,
        node.memory,
      ]);
    case "integer":
      return new Constr(4, [
        bytesData(node.cborRoot),
        node.cborLength,
        node.memory,
      ]);
    case "bytes":
      return new Constr(5, [
        bytesData(node.bytesRoot),
        node.bytesLength,
        node.cborLength,
        node.memory,
      ]);
    default:
      return unreachable(node);
  }
};

const dataListNodeData = (node: MidgardCekDataListNode): CekData =>
  new Constr(0, [
    bytesData(node.head),
    node.headCborLength,
    node.headMemory,
    bytesData(node.tail),
    node.length,
    node.payloadCborLength,
    node.memory,
  ]);

const dataPairNodeData = (node: MidgardCekDataPairNode): CekData =>
  new Constr(0, [
    bytesData(node.key),
    node.keyCborLength,
    node.keyMemory,
    bytesData(node.value),
    node.valueCborLength,
    node.valueMemory,
    bytesData(node.tail),
    node.length,
    node.payloadCborLength,
    node.memory,
  ]);

const optionData = <T>(
  value: T | null,
  encode: (exact: T) => CekData,
): CekData =>
  value === null ? new Constr(1, []) : new Constr(0, [encode(value)]);

const semanticBuiltinWitnessData = (
  witness: MidgardCekSemanticBuiltinWitness,
): CekData =>
  new Constr(0, [
    witness.dataNodes.map(dataNodeData),
    witness.listNodes.map(dataListNodeData),
    witness.pairNodes.map(dataPairNodeData),
    witness.scalarPreimages.map(bytesData),
  ]);

const mapConversionControlData = (
  control: MidgardCekMapConversionControl,
): CekData =>
  new Constr(0, [
    control.tag,
    bytesData(control.resultRoot),
    bytesData(control.sourceRoot),
    control.sourceRemaining,
    control.sourcePayloadCborLength,
    control.sourceMemory,
    bytesData(control.destinationRoot),
    control.destinationRemaining,
    control.destinationPayloadCborLength,
    control.destinationMemory,
    control.budgetCpu,
    control.budgetMemory,
  ]);

const mapConversionStartWitnessData = (
  witness: MidgardCekMapConversionStartWitness,
): CekData =>
  new Constr(0, [
    dataNodeData(witness.sourceNode),
    optionData(witness.sourceList, dataListNodeData),
    optionData(witness.sourcePairs, dataPairNodeData),
    dataNodeData(witness.resultNode),
    optionData(witness.resultList, dataListNodeData),
    optionData(witness.resultPairs, dataPairNodeData),
  ]);

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
  Buffer.from(Data.to(midgardCekCoreStepData(step) as unknown as Data), "hex");
