import {
  commitMidgardCekBlob,
  midgardCekDataBytesCborLength,
  midgardCekDataBytesMemory,
  midgardCekDataConstrCborLength,
  midgardCekDataListCborLength,
  type MidgardCekDataListNode,
  midgardCekDataMapCborLength,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
  midgardCekStringMemoryWords,
} from "@al-ft/midgard-core";
import { DataConstr, DataI } from "@harmoniclabs/plutus-data";

import { type MidgardCekDirectValueWitness } from "./cek-builtin.js";
import {
  decodeMidgardCekConstantWitness,
  midgardCekConstantMemorySize,
  type MidgardCekConstantType,
} from "./cek-constant.js";
import { encodeMidgardCekDataTreeInteger } from "./cek-data-tree.js";
import {
  type Bytes,
  type MidgardCekSemanticBuiltinWitness,
  sameBytes,
} from "./cek-machine.midgard-cek-core-step-witness.js";
import {
  constantParts,
  dataListSummaryMatches,
  dataNodeSummary,
  dataPairSummaryMatches,
  type DataSequenceSummary,
  type DataSummary,
  listSequenceFromNode,
  mapSequenceFromNode,
} from "./cek-machine.verify-case-select.js";
import { dataListLinkMatches } from "./cek-machine.verify-map-conversion-start.js";
import {
  resultMatchesParts,
  sameDataSummary,
  semanticSummary,
} from "./cek-machine.verify-semantic-builtin-control.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";

const dataNodeTopologyMatches = (
  node: MidgardCekDataNode,
  listNode: MidgardCekDataListNode | null,
  pairNode: MidgardCekDataPairNode | null,
): boolean => {
  if (
    node.cborLength < 0n ||
    node.memory < 0n ||
    (listNode !== null && pairNode !== null)
  ) {
    return false;
  }
  if (node.kind === "constrSmall" || node.kind === "constrLarge") {
    const sequence: DataSequenceSummary = {
      root: node.fieldsRoot,
      length: node.fieldsCount,
      payloadCborLength: listNode?.payloadCborLength ?? 0n,
      memory: listNode?.memory ?? 0n,
    };
    return (
      pairNode === null &&
      dataListSummaryMatches(sequence, listNode) &&
      (node.kind === "constrSmall"
        ? node.constructor >= 0n &&
          node.constructor <= 127n &&
          node.cborLength ===
            midgardCekDataConstrCborLength(
              node.constructor,
              sequence.length,
              sequence.payloadCborLength,
            )
        : node.constructorCborRoot.length === 32 &&
          node.constructorCborLength > 0n &&
          node.constructorMemory >= 5n &&
          node.cborLength ===
            3n +
              node.constructorCborLength +
              (sequence.length === 0n
                ? 1n
                : 2n + sequence.payloadCborLength)) &&
      node.memory === 4n + sequence.memory
    );
  }
  if (node.kind === "list") {
    const sequence = listSequenceFromNode(node);
    return (
      sequence !== null &&
      pairNode === null &&
      dataListSummaryMatches(sequence, listNode) &&
      node.cborLength ===
        midgardCekDataListCborLength(
          sequence.length,
          sequence.payloadCborLength,
        ) &&
      node.memory === 4n + sequence.memory
    );
  }
  if (node.kind === "map") {
    const sequence = mapSequenceFromNode(node);
    return (
      sequence !== null &&
      listNode === null &&
      dataPairSummaryMatches(sequence, pairNode) &&
      node.cborLength ===
        midgardCekDataMapCborLength(
          sequence.length,
          sequence.payloadCborLength,
        ) &&
      node.memory === 4n + sequence.memory
    );
  }
  if (listNode !== null || pairNode !== null) return false;
  if (node.kind === "bytes") {
    return (
      node.bytesRoot.length === 32 &&
      node.bytesLength >= 0n &&
      node.cborLength === midgardCekDataBytesCborLength(node.bytesLength) &&
      node.memory === midgardCekDataBytesMemory(node.bytesLength)
    );
  }
  return (
    node.kind === "integer" &&
    node.cborRoot.length === 32 &&
    node.cborLength > 0n &&
    node.memory >= 5n
  );
};

export const exactTopMaterial = (
  summary: DataSummary,
  node: MidgardCekDataNode,
  lists: readonly MidgardCekDataListNode[],
  pairs: readonly MidgardCekDataPairNode[],
): boolean => {
  const needsList =
    node.kind === "constrSmall" || node.kind === "constrLarge"
      ? node.fieldsCount > 0n
      : node.kind === "list"
        ? node.itemsCount > 0n
        : false;
  const needsPair = node.kind === "map" && node.entriesCount > 0n;
  if (
    lists.length !== (needsList ? 1 : 0) ||
    pairs.length !== (needsPair ? 1 : 0)
  ) {
    return false;
  }
  return (
    sameDataSummary(summary, dataNodeSummary(node)) &&
    dataNodeTopologyMatches(node, lists[0] ?? null, pairs[0] ?? null)
  );
};

/**
 * Whether the memory of a list constant of this element type is read from its
 * list node by `constantMemoryFromPayloadNode` (on chain,
 * `constant_memory_from_payload_node_v1`): a Data, integer, bytestring, unit,
 * boolean or `pair<data, data>` element.
 */
export const listMemoryIsReadFromNode = (
  element: MidgardCekConstantType,
): boolean =>
  element.kind === "data" ||
  element.kind === "integer" ||
  element.kind === "bytes" ||
  element.kind === "unit" ||
  element.kind === "boolean" ||
  (element.kind === "pair" &&
    element.first.kind === "data" &&
    element.second.kind === "data");

export const constantMemoryFromPayloadNode = (
  type: MidgardCekConstantType,
  node: MidgardCekDataNode,
): bigint | null => {
  if (type.kind === "integer" || type.kind === "bytes") {
    return node.memory - 4n;
  }
  if (type.kind === "unit" || type.kind === "boolean") return 1n;
  if (type.kind === "string") {
    return node.kind === "bytes"
      ? midgardCekStringMemoryWords(node.bytesLength)
      : null;
  }
  if (type.kind === "list" && node.kind === "list") {
    if (!listMemoryIsReadFromNode(type.element)) return null;
    // An integer, bytestring or data-pair item costs its data node less the
    // node's four words; a unit or boolean item costs one word.
    if (type.element.kind === "data") return node.memory - 4n;
    if (type.element.kind === "unit" || type.element.kind === "boolean") {
      return node.itemsCount;
    }
    return node.memory - 4n - node.itemsCount * 4n;
  }
  if (type.kind === "pair") {
    if (type.first.kind === "data" && type.second.kind === "data") {
      return node.memory - 4n;
    }
    if (
      type.first.kind === "integer" &&
      type.second.kind === "list" &&
      type.second.element.kind === "data"
    ) {
      return node.memory - 12n;
    }
    return null;
  }
  return type.kind === "data" ? node.memory : null;
};

export const canonicalIntegerLeaf = (
  summary: DataSummary,
  node: MidgardCekDataNode,
  raw: Bytes,
): bigint | null => {
  if (
    node.kind !== "integer" ||
    raw.length === 0 ||
    raw.length > 9_215 ||
    !sameBytes(node.cborRoot, commitMidgardCekBlob(raw).root) ||
    node.cborLength !== BigInt(raw.length) ||
    !sameDataSummary(summary, dataNodeSummary(node))
  ) {
    return null;
  }
  const decoded = plutusDataFromCborIterative(raw);
  return decoded instanceof DataI &&
    sameBytes(encodeMidgardCekDataTreeInteger(decoded.int), raw)
    ? decoded.int
    : null;
};

export const canonicalBytesLeaf = (
  summary: DataSummary,
  node: MidgardCekDataNode,
  raw: Bytes,
): boolean =>
  node.kind === "bytes" &&
  raw.length <= 9_215 &&
  sameBytes(node.bytesRoot, commitMidgardCekBlob(raw).root) &&
  node.bytesLength === BigInt(raw.length) &&
  sameDataSummary(summary, dataNodeSummary(node));

export const directUnit = (value: MidgardCekDirectValueWitness): boolean => {
  if (value.kind !== "constant") return false;
  const decoded = decodeMidgardCekConstantWitness(value.witness);
  return (
    decoded.type.kind === "unit" &&
    decoded.payload instanceof DataConstr &&
    decoded.payload.constr === 0n &&
    decoded.payload.fields.length === 0
  );
};

export type SemanticComponent = {
  readonly type: MidgardCekConstantType;
  readonly summary: DataSummary;
  readonly memory: bigint;
};

/**
 * Opens a pair constant into its two components from the pair material, as
 * `fstPair`/`sndPair` and `case` on a pair read it.
 */
export const semanticPairComponents = (
  source: MidgardCekDirectValueWitness,
  material: MidgardCekSemanticBuiltinWitness,
): {
  readonly first: SemanticComponent;
  readonly second: SemanticComponent;
} | null => {
  if (
    material.dataNodes.length !== 3 ||
    material.listNodes.length !== 2 ||
    material.pairNodes.length !== 0 ||
    material.scalarPreimages.length !== 0
  ) {
    return null;
  }
  const parts = constantParts(source);
  const [payload, firstNode, secondNode] = material.dataNodes;
  const [firstLink, secondLink] = material.listNodes;
  if (
    parts === null ||
    parts.type.kind !== "pair" ||
    payload?.kind !== "constrSmall" ||
    payload.constructor !== 0n ||
    payload.fieldsCount !== 2n ||
    firstNode === undefined ||
    secondNode === undefined ||
    firstLink === undefined ||
    secondLink === undefined ||
    !exactTopMaterial(parts.payload, payload, [firstLink], []) ||
    !dataListLinkMatches(firstLink, firstNode, secondLink) ||
    !dataListLinkMatches(secondLink, secondNode, null)
  ) {
    return null;
  }
  let firstMemory: bigint | null;
  let secondMemory: bigint | null;
  if (source.kind === "constant") {
    const decoded = decodeMidgardCekConstantWitness(source.witness);
    if (
      !(decoded.payload instanceof DataConstr) ||
      decoded.payload.constr !== 0n ||
      decoded.payload.fields.length !== 2 ||
      !sameDataSummary(
        semanticSummary(decoded.payload.fields[0]!),
        dataNodeSummary(firstNode),
      ) ||
      !sameDataSummary(
        semanticSummary(decoded.payload.fields[1]!),
        dataNodeSummary(secondNode),
      )
    ) {
      return null;
    }
    firstMemory = midgardCekConstantMemorySize(
      parts.type.first,
      decoded.payload.fields[0]!,
    );
    secondMemory = midgardCekConstantMemorySize(
      parts.type.second,
      decoded.payload.fields[1]!,
    );
  } else {
    firstMemory = constantMemoryFromPayloadNode(parts.type.first, firstNode);
    secondMemory = constantMemoryFromPayloadNode(parts.type.second, secondNode);
  }
  if (
    firstMemory === null ||
    secondMemory === null ||
    parts.memory !== firstMemory + secondMemory
  ) {
    return null;
  }
  return {
    first: {
      type: parts.type.first,
      summary: dataNodeSummary(firstNode),
      memory: firstMemory,
    },
    second: {
      type: parts.type.second,
      summary: dataNodeSummary(secondNode),
      memory: secondMemory,
    },
  };
};

export const verifySemanticPair = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  result: MidgardCekDirectValueWitness,
  material: MidgardCekSemanticBuiltinWitness,
): boolean => {
  if (arguments_.length !== 1) return false;
  const components = semanticPairComponents(arguments_[0]!, material);
  if (components === null || (tag !== 29n && tag !== 30n)) return false;
  const selected = tag === 29n ? components.first : components.second;
  return resultMatchesParts(
    result,
    selected.type,
    selected.summary,
    selected.memory,
  );
};
