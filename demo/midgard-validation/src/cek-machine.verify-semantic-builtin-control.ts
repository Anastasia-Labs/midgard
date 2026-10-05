import {
  commitMidgardCekBlob,
  hashMidgardCekDataListNode,
  hashMidgardCekDataPairNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  midgardCekDataConstrCborLength,
  midgardCekDataListCborLength,
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";
import { type Data } from "@harmoniclabs/plutus-data";

import { type MidgardCekDirectValueWitness } from "./cek-builtin.js";
import {
  type MidgardCekConstantType,
  midgardCekIntegerMemorySize,
} from "./cek-constant.js";
import {
  commitMidgardCekDataTree,
  encodeMidgardCekDataTreeInteger,
} from "./cek-data-tree.js";
import {
  exactState,
  hashMidgardCekMapConversionControl,
  mapConversionControlIsWellFormed,
  sameState,
} from "./cek-machine.midgard-cek-builtin-argument-count.js";
import {
  type MidgardCekCoreStepWitness,
  sameBytes,
} from "./cek-machine.midgard-cek-core-step-witness.js";
import {
  constantParts,
  dataNodeSummary,
  type DataSequenceSummary,
  type DataSummary,
} from "./cek-machine.verify-case-select.js";
import {
  nextMapControl,
  pairWrapperMatches,
} from "./cek-machine.verify-map-conversion-start.js";

export const verifySemanticBuiltinControl = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
  witness: MidgardCekCoreStepWitness,
): boolean => {
  if (witness.kind === "finishBuiltinMapConversion") {
    return (
      mapConversionControlIsWellFormed(witness.control) &&
      witness.control.sourceRemaining === 0n &&
      sameBytes(
        pre.focusRoot,
        hashMidgardCekMapConversionControl(witness.control),
      ) &&
      sameState(
        post,
        exactState(pre, {
          mode: "return",
          focusRoot: witness.control.resultRoot,
          environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
          cpuDelta: witness.control.budgetCpu,
          memoryDelta: witness.control.budgetMemory,
        }),
      )
    );
  }
  if (witness.kind === "stepBuiltinListToMap") {
    const control = witness.control;
    const pairSummary = dataNodeSummary(witness.pair);
    const keySummary = dataNodeSummary(witness.key);
    const valueSummary = dataNodeSummary(witness.value);
    const next = nextMapControl(
      control,
      witness.source.headCborLength,
      witness.source.headMemory,
      witness.source.tail,
      witness.destination.keyCborLength + witness.destination.valueCborLength,
      witness.destination.keyMemory + witness.destination.valueMemory,
      witness.destination.tail,
    );
    return (
      control.tag === 38n &&
      control.sourceRemaining > 0n &&
      sameBytes(pre.focusRoot, hashMidgardCekMapConversionControl(control)) &&
      sameBytes(
        hashMidgardCekDataListNode(witness.source),
        control.sourceRoot,
      ) &&
      witness.source.length === control.sourceRemaining &&
      witness.source.payloadCborLength === control.sourcePayloadCborLength &&
      witness.source.memory === control.sourceMemory &&
      sameBytes(witness.source.head, pairSummary.root) &&
      witness.source.headCborLength === pairSummary.cborLength &&
      witness.source.headMemory === pairSummary.memory &&
      pairWrapperMatches(
        witness.pair,
        witness.first,
        witness.second,
        witness.key,
        witness.value,
      ) &&
      sameBytes(
        hashMidgardCekDataPairNode(witness.destination),
        control.destinationRoot,
      ) &&
      witness.destination.length === control.destinationRemaining &&
      witness.destination.payloadCborLength ===
        control.destinationPayloadCborLength &&
      witness.destination.memory === control.destinationMemory &&
      sameBytes(witness.destination.key, keySummary.root) &&
      witness.destination.keyCborLength === keySummary.cborLength &&
      witness.destination.keyMemory === keySummary.memory &&
      sameBytes(witness.destination.value, valueSummary.root) &&
      witness.destination.valueCborLength === valueSummary.cborLength &&
      witness.destination.valueMemory === valueSummary.memory &&
      sameState(
        post,
        exactState(pre, {
          mode: "semanticBuiltin",
          focusRoot: hashMidgardCekMapConversionControl(next),
          environmentRoot: pre.environmentRoot,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
        }),
      )
    );
  }
  if (witness.kind === "stepBuiltinMapToList") {
    const control = witness.control;
    const pairSummary = dataNodeSummary(witness.pair);
    const keySummary = dataNodeSummary(witness.key);
    const valueSummary = dataNodeSummary(witness.value);
    const next = nextMapControl(
      control,
      witness.source.keyCborLength + witness.source.valueCborLength,
      witness.source.keyMemory + witness.source.valueMemory,
      witness.source.tail,
      witness.destination.headCborLength,
      witness.destination.headMemory,
      witness.destination.tail,
    );
    return (
      control.tag === 43n &&
      control.sourceRemaining > 0n &&
      sameBytes(pre.focusRoot, hashMidgardCekMapConversionControl(control)) &&
      sameBytes(
        hashMidgardCekDataPairNode(witness.source),
        control.sourceRoot,
      ) &&
      witness.source.length === control.sourceRemaining &&
      witness.source.payloadCborLength === control.sourcePayloadCborLength &&
      witness.source.memory === control.sourceMemory &&
      sameBytes(witness.source.key, keySummary.root) &&
      witness.source.keyCborLength === keySummary.cborLength &&
      witness.source.keyMemory === keySummary.memory &&
      sameBytes(witness.source.value, valueSummary.root) &&
      witness.source.valueCborLength === valueSummary.cborLength &&
      witness.source.valueMemory === valueSummary.memory &&
      pairWrapperMatches(
        witness.pair,
        witness.first,
        witness.second,
        witness.key,
        witness.value,
      ) &&
      sameBytes(
        hashMidgardCekDataListNode(witness.destination),
        control.destinationRoot,
      ) &&
      witness.destination.length === control.destinationRemaining &&
      witness.destination.payloadCborLength ===
        control.destinationPayloadCborLength &&
      witness.destination.memory === control.destinationMemory &&
      sameBytes(witness.destination.head, pairSummary.root) &&
      witness.destination.headCborLength === pairSummary.cborLength &&
      witness.destination.headMemory === pairSummary.memory &&
      sameState(
        post,
        exactState(pre, {
          mode: "semanticBuiltin",
          focusRoot: hashMidgardCekMapConversionControl(next),
          environmentRoot: pre.environmentRoot,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
        }),
      )
    );
  }
  return false;
};

export const sameConstantType = (
  left: MidgardCekConstantType,
  right: MidgardCekConstantType,
): boolean => {
  if (left.kind !== right.kind) return false;
  if (left.kind === "list" && right.kind === "list") {
    return sameConstantType(left.element, right.element);
  }
  if (left.kind === "pair" && right.kind === "pair") {
    return (
      sameConstantType(left.first, right.first) &&
      sameConstantType(left.second, right.second)
    );
  }
  return true;
};

export const sameDataSummary = (
  left: DataSummary,
  right: DataSummary,
): boolean =>
  sameBytes(left.root, right.root) &&
  left.cborLength === right.cborLength &&
  left.memory === right.memory;

export const resultMatchesParts = (
  result: MidgardCekDirectValueWitness,
  type: MidgardCekConstantType,
  payload: DataSummary,
  memory: bigint,
): boolean => {
  const actual = constantParts(result);
  return (
    actual !== null &&
    sameConstantType(actual.type, type) &&
    sameDataSummary(actual.payload, payload) &&
    actual.memory === memory
  );
};

export const semanticSummary = (value: Data): DataSummary => {
  const tree = commitMidgardCekDataTree(value);
  return {
    root: tree.root,
    cborLength: tree.cborLength,
    memory: tree.memory,
  };
};

export const emptyListSequence = (): DataSequenceSummary => ({
  root: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  length: 0n,
  payloadCborLength: 0n,
  memory: 0n,
});

export const prependListSequence = (
  head: DataSummary,
  tail: DataSequenceSummary,
): DataSequenceSummary => {
  const node: MidgardCekDataListNode = {
    head: head.root,
    headCborLength: head.cborLength,
    headMemory: head.memory,
    tail: tail.root,
    length: tail.length + 1n,
    payloadCborLength: head.cborLength + tail.payloadCborLength,
    memory: head.memory + tail.memory,
  };
  return {
    root: hashMidgardCekDataListNode(node),
    length: node.length,
    payloadCborLength: node.payloadCborLength,
    memory: node.memory,
  };
};

export const listDataSummary = (sequence: DataSequenceSummary): DataSummary => {
  const node: MidgardCekDataNode = {
    kind: "list",
    itemsCount: sequence.length,
    itemsRoot: sequence.root,
    cborLength: midgardCekDataListCborLength(
      sequence.length,
      sequence.payloadCborLength,
    ),
    memory: 4n + sequence.memory,
  };
  return dataNodeSummary(node);
};

export const constrDataSummary = (
  constructor: bigint,
  fields: DataSequenceSummary,
): DataSummary => {
  if (constructor < 0n) {
    throw new Error("CEK Data constructor cannot be negative");
  }
  const cborLength = midgardCekDataConstrCborLength(
    constructor,
    fields.length,
    fields.payloadCborLength,
  );
  const memory = 4n + fields.memory;
  const node: MidgardCekDataNode =
    constructor <= 127n
      ? {
          kind: "constrSmall",
          constructor,
          fieldsCount: fields.length,
          fieldsRoot: fields.root,
          cborLength,
          memory,
        }
      : {
          kind: "constrLarge",
          constructorCborRoot: commitMidgardCekBlob(
            encodeMidgardCekDataTreeInteger(constructor),
          ).root,
          constructorCborLength: BigInt(
            encodeMidgardCekDataTreeInteger(constructor).length,
          ),
          constructorMemory: 4n + midgardCekIntegerMemorySize(constructor),
          fieldsCount: fields.length,
          fieldsRoot: fields.root,
          cborLength,
          memory,
        };
  return dataNodeSummary(node);
};

export const pairDataSummary = (
  first: DataSummary,
  second: DataSummary,
): DataSummary =>
  constrDataSummary(
    0n,
    prependListSequence(
      first,
      prependListSequence(second, emptyListSequence()),
    ),
  );
