import {
  commitMidgardCekBlob,
  midgardCekDataBytesCborLength,
  midgardCekDataConstrCborLength,
  midgardCekDataListCborLength,
  type MidgardCekDataListNode,
  midgardCekDataMapCborLength,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
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
      node.memory === 4n + (node.bytesLength === 0n ? 1n : node.bytesLength)
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

export const constantMemoryFromPayloadNode = (
  type: MidgardCekConstantType,
  node: MidgardCekDataNode,
): bigint | null => {
  if (type.kind === "integer" || type.kind === "bytes") {
    return node.memory - 4n;
  }
  if (type.kind === "list" && node.kind === "list") {
    if (type.element.kind === "data") return node.memory - 4n;
    if (type.element.kind === "integer") {
      return node.memory - 4n - node.itemsCount * 4n;
    }
    if (
      type.element.kind === "pair" &&
      type.element.first.kind === "data" &&
      type.element.second.kind === "data"
    ) {
      return node.memory - 4n - node.itemsCount * 4n;
    }
    return null;
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

export const verifySemanticPair = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  result: MidgardCekDirectValueWitness,
  material: MidgardCekSemanticBuiltinWitness,
): boolean => {
  if (
    arguments_.length !== 1 ||
    material.dataNodes.length !== 3 ||
    material.listNodes.length !== 2 ||
    material.pairNodes.length !== 0 ||
    material.scalarPreimages.length !== 0
  ) {
    return false;
  }
  const parts = constantParts(arguments_[0]!);
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
    return false;
  }
  let firstMemory: bigint | null;
  let secondMemory: bigint | null;
  if (arguments_[0]?.kind === "constant") {
    const decoded = decodeMidgardCekConstantWitness(arguments_[0].witness);
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
      return false;
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
    return false;
  }
  return tag === 29n
    ? resultMatchesParts(
        result,
        parts.type.first,
        dataNodeSummary(firstNode),
        firstMemory,
      )
    : tag === 30n &&
        resultMatchesParts(
          result,
          parts.type.second,
          dataNodeSummary(secondNode),
          secondMemory,
        );
};
