import type {
  MidgardCekDataListNode,
  MidgardCekDataNode,
  MidgardCekDataPairNode,
  MidgardCekMachineState,
  MidgardCekValueNode,
} from "@al-ft/midgard-core";
import { Constr } from "@lucid-evolution/lucid";

import type {
  MidgardCekBlsExpressionWitness,
  MidgardCekDirectValueWitness,
  MidgardCekRuntimeValueWitness,
} from "./cek-builtin.js";
import type { MidgardCekConstantWitness } from "./cek-constant.js";
import type {
  MidgardCekEnvironmentSummary,
  MidgardCekMapConversionControl,
  MidgardCekMapConversionStartWitness,
  MidgardCekSemanticBuiltinWitness,
} from "./cek-machine.js";

export type CekData = Constr<unknown>;

export const bytesData = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

export const unreachable = (value: never): never => {
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

export const constantWitnessData = (
  witness: MidgardCekConstantWitness,
): CekData =>
  new Constr(0, [bytesData(witness.typeCbor), bytesData(witness.payloadCbor)]);

export const environmentSummaryData = (
  summary: MidgardCekEnvironmentSummary,
): CekData =>
  summary.kind === "empty"
    ? new Constr(0, [])
    : new Constr(1, [
        bytesData(summary.value),
        bytesData(summary.tail),
        summary.length,
      ]);

export const machineValueData = (value: MidgardCekValueNode): CekData => {
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

export const directValueData = (
  value: MidgardCekDirectValueWitness,
): CekData => {
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

export const runtimeValueData = (
  value: MidgardCekRuntimeValueWitness,
): CekData => {
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

export const blsExpressionData = (
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

export const dataNodeData = (node: MidgardCekDataNode): CekData => {
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

export const dataListNodeData = (node: MidgardCekDataListNode): CekData =>
  new Constr(0, [
    bytesData(node.head),
    node.headCborLength,
    node.headMemory,
    bytesData(node.tail),
    node.length,
    node.payloadCborLength,
    node.memory,
  ]);

export const dataPairNodeData = (node: MidgardCekDataPairNode): CekData =>
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

export const semanticBuiltinWitnessData = (
  witness: MidgardCekSemanticBuiltinWitness,
): CekData =>
  new Constr(0, [
    witness.dataNodes.map(dataNodeData),
    witness.listNodes.map(dataListNodeData),
    witness.pairNodes.map(dataPairNodeData),
    witness.scalarPreimages.map(bytesData),
  ]);

export const mapConversionControlData = (
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

export const mapConversionStartWitnessData = (
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
