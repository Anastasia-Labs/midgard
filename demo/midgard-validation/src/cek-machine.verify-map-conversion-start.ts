import {
  hashMidgardCekDataListNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";

import {
  hashMidgardCekDirectValueWitness,
  midgardCekDirectBuiltinBudget,
} from "./cek-builtin.js";
import {
  exactState,
  hashMidgardCekMapConversionControl,
  sameState,
} from "./cek-machine.midgard-cek-builtin-argument-count.js";
import {
  type Bytes,
  type MidgardCekCoreStepWitness,
  type MidgardCekMapConversionControl,
  sameBytes,
} from "./cek-machine.midgard-cek-core-step-witness.js";
import {
  builtinRootMatches,
  constantParts,
  dataListSummaryMatches,
  dataNodeSummary,
  dataPairSummaryMatches,
  isDataType,
  isListDataPairType,
  listSequenceFromNode,
  mapSequenceFromNode,
  payloadSummaryMatchesNode,
} from "./cek-machine.verify-case-select.js";

export const verifyMapConversionStart = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
  witness: Extract<
    MidgardCekCoreStepWitness,
    { readonly kind: "startBuiltinMapConversion" }
  >,
): boolean => {
  if (
    (witness.tag !== 38n && witness.tag !== 43n) ||
    witness.arguments.length !== 1 ||
    !builtinRootMatches(pre, witness.tag, witness.arguments)
  ) {
    return false;
  }
  const source = constantParts(witness.arguments[0]!);
  const result = constantParts(witness.result);
  if (
    source === null ||
    result === null ||
    !payloadSummaryMatchesNode(source.payload, witness.material.sourceNode) ||
    !payloadSummaryMatchesNode(result.payload, witness.material.resultNode)
  ) {
    return false;
  }
  const sourceSequence =
    witness.tag === 38n
      ? listSequenceFromNode(witness.material.sourceNode)
      : mapSequenceFromNode(witness.material.sourceNode);
  const destinationSequence =
    witness.tag === 38n
      ? mapSequenceFromNode(witness.material.resultNode)
      : listSequenceFromNode(witness.material.resultNode);
  if (
    sourceSequence === null ||
    destinationSequence === null ||
    sourceSequence.length !== destinationSequence.length
  ) {
    return false;
  }
  const topologyMatches =
    witness.tag === 38n
      ? dataListSummaryMatches(sourceSequence, witness.material.sourceList) &&
        witness.material.sourcePairs === null &&
        dataPairSummaryMatches(
          destinationSequence,
          witness.material.resultPairs,
        ) &&
        witness.material.resultList === null
      : dataPairSummaryMatches(sourceSequence, witness.material.sourcePairs) &&
        witness.material.sourceList === null &&
        dataListSummaryMatches(
          destinationSequence,
          witness.material.resultList,
        ) &&
        witness.material.resultPairs === null;
  if (!topologyMatches) return false;
  if (
    witness.tag === 38n
      ? !isListDataPairType(source.type) ||
        !isDataType(result.type) ||
        source.memory !== sourceSequence.memory - sourceSequence.length * 4n ||
        result.memory !== destinationSequence.memory + 4n
      : !isDataType(source.type) ||
        !isListDataPairType(result.type) ||
        source.memory !== sourceSequence.memory + 4n ||
        result.memory !==
          destinationSequence.memory - destinationSequence.length * 4n
  ) {
    return false;
  }
  const budget = midgardCekDirectBuiltinBudget(witness.tag, witness.arguments);
  const control: MidgardCekMapConversionControl = {
    tag: witness.tag,
    resultRoot: hashMidgardCekDirectValueWitness(witness.result),
    sourceRoot: sourceSequence.root,
    sourceRemaining: sourceSequence.length,
    sourcePayloadCborLength: sourceSequence.payloadCborLength,
    sourceMemory: sourceSequence.memory,
    destinationRoot: destinationSequence.root,
    destinationRemaining: destinationSequence.length,
    destinationPayloadCborLength: destinationSequence.payloadCborLength,
    destinationMemory: destinationSequence.memory,
    budgetCpu: budget.cpu,
    budgetMemory: budget.memory,
  };
  return sameState(
    post,
    exactState(pre, {
      mode: "semanticBuiltin",
      focusRoot: hashMidgardCekMapConversionControl(control),
      environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      continuationRoot: pre.continuationRoot,
      auxiliary: 0n,
    }),
  );
};

export const dataListLinkMatches = (
  link: MidgardCekDataListNode,
  head: MidgardCekDataNode,
  tail: MidgardCekDataListNode | null,
): boolean => {
  const headSummary = dataNodeSummary(head);
  const tailRoot =
    tail === null
      ? MIDGARD_CEK_EMPTY_DATA_LIST_ROOT
      : hashMidgardCekDataListNode(tail);
  const tailLength = tail?.length ?? 0n;
  const tailPayload = tail?.payloadCborLength ?? 0n;
  const tailMemory = tail?.memory ?? 0n;
  return (
    sameBytes(link.head, headSummary.root) &&
    link.headCborLength === headSummary.cborLength &&
    link.headMemory === headSummary.memory &&
    sameBytes(link.tail, tailRoot) &&
    link.length === tailLength + 1n &&
    link.payloadCborLength === headSummary.cborLength + tailPayload &&
    link.memory === headSummary.memory + tailMemory
  );
};

export const pairWrapperMatches = (
  pair: MidgardCekDataNode,
  first: MidgardCekDataListNode,
  second: MidgardCekDataListNode,
  key: MidgardCekDataNode,
  value: MidgardCekDataNode,
): boolean =>
  pair.kind === "constrSmall" &&
  pair.constructor === 0n &&
  pair.fieldsCount === 2n &&
  sameBytes(pair.fieldsRoot, hashMidgardCekDataListNode(first)) &&
  pair.memory === 4n + first.memory &&
  dataListLinkMatches(first, key, second) &&
  dataListLinkMatches(second, value, null);

export const nextMapControl = (
  control: MidgardCekMapConversionControl,
  sourcePayload: bigint,
  sourceMemory: bigint,
  sourceTail: Bytes,
  destinationPayload: bigint,
  destinationMemory: bigint,
  destinationTail: Bytes,
): MidgardCekMapConversionControl => ({
  ...control,
  sourceRoot: sourceTail,
  sourceRemaining: control.sourceRemaining - 1n,
  sourcePayloadCborLength: control.sourcePayloadCborLength - sourcePayload,
  sourceMemory: control.sourceMemory - sourceMemory,
  destinationRoot: destinationTail,
  destinationRemaining: control.destinationRemaining - 1n,
  destinationPayloadCborLength:
    control.destinationPayloadCborLength - destinationPayload,
  destinationMemory: control.destinationMemory - destinationMemory,
});
