import {
  hashMidgardCekContinuationFrame,
  hashMidgardCekDataListNode,
  hashMidgardCekDataNode,
  hashMidgardCekDataPairNode,
  hashMidgardCekSequenceNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";

import {
  hashMidgardCekDirectArguments,
  type MidgardCekDirectValueWitness,
} from "./cek-builtin.js";
import {
  decodeMidgardCekConstantTypeCbor,
  decodeMidgardCekConstantWitness,
  midgardCekConstantMemorySize,
  type MidgardCekConstantType,
} from "./cek-constant.js";
import { commitMidgardCekDataTree } from "./cek-data-tree.js";
import {
  exactState,
  linkedSequenceRootIsWellFormed,
  linkedSequenceTailIsWellFormed,
  sameState,
} from "./cek-machine.midgard-cek-builtin-argument-count.js";
import {
  type Bytes,
  type MidgardCekCoreStepWitness,
  sameBytes,
} from "./cek-machine.midgard-cek-core-step-witness.js";

export const verifyCaseSelect = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
  witness: MidgardCekCoreStepWitness,
): boolean => {
  if (witness.kind !== "selectCaseBranch") return false;
  if (
    witness.length <= 0n ||
    pre.auxiliary < 0n ||
    pre.auxiliary >= witness.length ||
    !linkedSequenceTailIsWellFormed(
      witness.remainingBranchesRoot,
      witness.length,
    ) ||
    !linkedSequenceRootIsWellFormed(pre.environmentRoot, witness.valuesCount)
  ) {
    return false;
  }
  const work = hashMidgardCekContinuationFrame({
    kind: "caseSelect",
    environment: witness.capturedEnvironment,
    tail: witness.tail,
    valuesCount: witness.valuesCount,
  });
  if (
    !sameBytes(
      pre.focusRoot,
      hashMidgardCekSequenceNode({
        head: witness.branch,
        tail: witness.remainingBranchesRoot,
        length: witness.length,
      }),
    ) ||
    !sameBytes(pre.continuationRoot, work)
  ) {
    return false;
  }
  const expected =
    pre.auxiliary > 0n
      ? exactState(pre, {
          mode: "caseSelect",
          focusRoot: witness.remainingBranchesRoot,
          environmentRoot: pre.environmentRoot,
          continuationRoot: work,
          auxiliary: pre.auxiliary - 1n,
        })
      : witness.valuesCount === 0n
        ? exactState(pre, {
            mode: "compute",
            focusRoot: witness.branch,
            environmentRoot: witness.capturedEnvironment,
            continuationRoot: witness.tail,
            auxiliary: 0n,
          })
        : exactState(pre, {
            mode: "caseApply",
            focusRoot: pre.environmentRoot,
            environmentRoot: witness.branch,
            continuationRoot: hashMidgardCekContinuationFrame({
              kind: "caseApply",
              environment: witness.capturedEnvironment,
              builtContinuation: witness.tail,
            }),
            auxiliary: witness.valuesCount,
          });
  return sameState(post, expected);
};

export const verifyCaseApply = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
  witness: MidgardCekCoreStepWitness,
): boolean => {
  if (
    witness.kind !== "applyCaseValue" ||
    witness.length <= 0n ||
    pre.auxiliary !== witness.length ||
    !linkedSequenceTailIsWellFormed(witness.remainingValuesRoot, witness.length)
  ) {
    return false;
  }
  if (
    !sameBytes(
      pre.focusRoot,
      hashMidgardCekSequenceNode({
        head: witness.value,
        tail: witness.remainingValuesRoot,
        length: witness.length,
      }),
    ) ||
    !sameBytes(
      pre.continuationRoot,
      hashMidgardCekContinuationFrame({
        kind: "caseApply",
        environment: witness.capturedEnvironment,
        builtContinuation: witness.builtContinuation,
      }),
    )
  ) {
    return false;
  }
  const nextContinuation = hashMidgardCekContinuationFrame({
    kind: "applyValue",
    value: witness.value,
    tail: witness.builtContinuation,
  });
  return sameState(
    post,
    witness.length === 1n
      ? exactState(pre, {
          mode: "compute",
          focusRoot: pre.environmentRoot,
          environmentRoot: witness.capturedEnvironment,
          continuationRoot: nextContinuation,
          auxiliary: 0n,
        })
      : exactState(pre, {
          mode: "caseApply",
          focusRoot: witness.remainingValuesRoot,
          environmentRoot: pre.environmentRoot,
          continuationRoot: hashMidgardCekContinuationFrame({
            kind: "caseApply",
            environment: witness.capturedEnvironment,
            builtContinuation: nextContinuation,
          }),
          auxiliary: witness.length - 1n,
        }),
  );
};

export type DataSummary = {
  readonly root: Bytes;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

export type DataSequenceSummary = {
  readonly root: Bytes;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

export const dataNodeSummary = (node: MidgardCekDataNode): DataSummary => ({
  root: hashMidgardCekDataNode(node),
  cborLength: node.cborLength,
  memory: node.memory,
});

export const listSequenceFromNode = (
  node: MidgardCekDataNode,
): DataSequenceSummary | null => {
  if (node.kind !== "list") return null;
  return {
    root: node.itemsRoot,
    length: node.itemsCount,
    payloadCborLength: node.cborLength - (node.itemsCount === 0n ? 1n : 2n),
    memory: node.memory - 4n,
  };
};

export const mapSequenceFromNode = (
  node: MidgardCekDataNode,
): DataSequenceSummary | null => {
  if (node.kind !== "map") return null;
  const header =
    node.entriesCount < 24n
      ? 1n
      : node.entriesCount <= 0xffn
        ? 2n
        : node.entriesCount <= 0xffffn
          ? 3n
          : 5n;
  return {
    root: node.entriesRoot,
    length: node.entriesCount,
    payloadCborLength: node.cborLength - header,
    memory: node.memory - 4n,
  };
};

export const dataListSummaryMatches = (
  sequence: DataSequenceSummary,
  node: MidgardCekDataListNode | null,
): boolean =>
  sequence.length === 0n
    ? node === null &&
      sameBytes(sequence.root, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT) &&
      sequence.payloadCborLength === 0n &&
      sequence.memory === 0n
    : node !== null &&
      sameBytes(sequence.root, hashMidgardCekDataListNode(node)) &&
      node.length === sequence.length &&
      node.payloadCborLength === sequence.payloadCborLength &&
      node.memory === sequence.memory;

export const dataPairSummaryMatches = (
  sequence: DataSequenceSummary,
  node: MidgardCekDataPairNode | null,
): boolean =>
  sequence.length === 0n
    ? node === null &&
      sameBytes(sequence.root, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT) &&
      sequence.payloadCborLength === 0n &&
      sequence.memory === 0n
    : node !== null &&
      sameBytes(sequence.root, hashMidgardCekDataPairNode(node)) &&
      node.length === sequence.length &&
      node.payloadCborLength === sequence.payloadCborLength &&
      node.memory === sequence.memory;

/**
 * The payload summary a map-conversion endpoint commits to must be the exact
 * summary of the top Data node the witness reveals. Inline and semantic
 * constants both reach here through `constantParts`, mirroring the Aiken
 * `semantic_constant_parts_v1` acceptance of `ConstantValue`.
 */
export const payloadSummaryMatchesNode = (
  summary: DataSummary,
  node: MidgardCekDataNode,
): boolean =>
  sameBytes(summary.root, hashMidgardCekDataNode(node)) &&
  summary.cborLength === node.cborLength &&
  summary.memory === node.memory;

export const isDataType = (type: MidgardCekConstantType): boolean =>
  type.kind === "data";

export const isListDataPairType = (type: MidgardCekConstantType): boolean =>
  type.kind === "list" &&
  type.element.kind === "pair" &&
  type.element.first.kind === "data" &&
  type.element.second.kind === "data";

export const builtinRootMatches = (
  pre: MidgardCekMachineState,
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  const committed = hashMidgardCekDirectArguments(arguments_);
  return sameBytes(
    pre.focusRoot,
    hashMidgardCekValueNode({
      kind: "builtin",
      tag,
      forcesRemaining: 0n,
      argumentsCount: committed.count,
      argumentsRoot: committed.root,
    }),
  );
};

type ConstantParts = {
  readonly type: MidgardCekConstantType;
  readonly payload: DataSummary;
  readonly memory: bigint;
};

export const constantParts = (
  value: MidgardCekDirectValueWitness,
): ConstantParts | null => {
  if (value.kind === "constant") {
    const decoded = decodeMidgardCekConstantWitness(value.witness);
    const tree = commitMidgardCekDataTree(decoded.payload);
    return {
      type: decoded.type,
      payload: {
        root: tree.root,
        cborLength: tree.cborLength,
        memory: tree.memory,
      },
      memory: midgardCekConstantMemorySize(decoded.type, decoded.payload),
    };
  }
  if (value.kind === "semanticConstant") {
    return {
      type: decodeMidgardCekConstantTypeCbor(value.witness.typeCbor),
      payload: value.witness.payload,
      memory: value.witness.memory,
    };
  }
  return null;
};
