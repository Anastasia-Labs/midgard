/**
 * The recursive semantic-Data walks as they stood before they were made
 * iterative, vendored (imports repointed, the reconstruct closures wrapped in
 * a factory over the same source) so the differential test can compare old
 * and new. Test-only. The one change to the bodies: a map is the ordered
 * entry list `SemanticDataMap` rather than a JS `Map`, so duplicate keys stay
 * separate entries here as they do in the code under test.
 */
import { Data as LucidData } from "@lucid-evolution/lucid";

import { semanticIntegerMemory } from "../src/cek-proof.commit-semantic-data.js";
import { commitMidgardCekBlob } from "../src/cek-proof.encode-midgard-cek-continuation-frame.js";
import { MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES } from "../src/cek-proof.encode-midgard-cek-term-node.js";
import {
  encodeSemanticBytes,
  isSemanticConstr,
  isSemanticList,
  isSemanticMap,
  semanticCborHeader,
  type SemanticDataEntry,
  type SemanticDataMap,
  type SemanticDataValue,
} from "../src/cek-proof.program-material-task.js";
import { type SemanticDataReconstructionSource } from "../src/cek-proof.reconstruct-semantic-data.js";
import {
  hashMidgardCekDataListNode,
  hashMidgardCekDataNode,
  hashMidgardCekDataPairNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  midgardCekDataBytesCborLength,
  midgardCekDataConstrCborLength,
  midgardCekDataListCborLength,
  type MidgardCekDataListNode,
  midgardCekDataMapCborLength,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
} from "../src/cek-semantic.js";
import { type Hash32 } from "../src/codec/hash.js";

const rootKey = (root: Uint8Array): string => Buffer.from(root).toString("hex");

const encodeSemanticList = (values: readonly SemanticDataValue[]): Buffer =>
  values.length === 0
    ? Buffer.from([0x80])
    : Buffer.concat([
        Buffer.from([0x9f]),
        ...values.map(legacyEncodeSemanticData),
        Buffer.from([0xff]),
      ]);

export const legacyEncodeSemanticData = (value: SemanticDataValue): Buffer => {
  if (typeof value === "bigint") {
    return Buffer.from(LucidData.to(value), "hex");
  }
  if (typeof value === "string") {
    return encodeSemanticBytes(Buffer.from(value, "hex"));
  }
  if (isSemanticList(value)) {
    return encodeSemanticList(value);
  }
  if (isSemanticMap(value)) {
    return Buffer.concat([
      semanticCborHeader(5, BigInt(value.entries.length)),
      ...value.entries.flatMap(([key, mapped]) => [
        legacyEncodeSemanticData(key),
        legacyEncodeSemanticData(mapped),
      ]),
    ]);
  }
  if (isSemanticConstr(value)) {
    const fields = encodeSemanticList(value.fields);
    if (value.constructor <= 6n) {
      return Buffer.concat([
        semanticCborHeader(6, 121n + value.constructor),
        fields,
      ]);
    }
    if (value.constructor <= 127n) {
      return Buffer.concat([
        semanticCborHeader(6, 1280n + value.constructor - 7n),
        fields,
      ]);
    }
    return Buffer.concat([
      semanticCborHeader(6, 102n),
      Buffer.from([0x82]),
      Buffer.from(LucidData.to(value.constructor), "hex"),
      fields,
    ]);
  }
  throw new Error("CEK constant contains unknown semantic Data");
};

type SemanticDataSummary = {
  readonly root: Hash32;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

type SemanticListSummary = {
  readonly root: Hash32;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

export const legacyCommitSemanticData = (
  value: SemanticDataValue,
): SemanticDataSummary => {
  const canonicalCbor = legacyEncodeSemanticData(value);
  const commitList = (
    items: readonly SemanticDataValue[],
  ): SemanticListSummary => {
    let summary: SemanticListSummary = {
      root: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
      length: 0n,
      payloadCborLength: 0n,
      memory: 0n,
    };
    for (let index = items.length - 1; index >= 0; index -= 1) {
      const head = legacyCommitSemanticData(items[index]!);
      const node: MidgardCekDataListNode = {
        head: head.root,
        headCborLength: head.cborLength,
        headMemory: head.memory,
        tail: summary.root,
        length: summary.length + 1n,
        payloadCborLength: head.cborLength + summary.payloadCborLength,
        memory: head.memory + summary.memory,
      };
      summary = {
        root: hashMidgardCekDataListNode(node),
        length: node.length,
        payloadCborLength: node.payloadCborLength,
        memory: node.memory,
      };
    }
    return summary;
  };
  const commitPairs = (
    entries: readonly (readonly [SemanticDataValue, SemanticDataValue])[],
  ): SemanticListSummary => {
    let summary: SemanticListSummary = {
      root: MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
      length: 0n,
      payloadCborLength: 0n,
      memory: 0n,
    };
    for (let index = entries.length - 1; index >= 0; index -= 1) {
      const [keyValue, mappedValue] = entries[index]!;
      const key = legacyCommitSemanticData(keyValue);
      const mapped = legacyCommitSemanticData(mappedValue);
      const node: MidgardCekDataPairNode = {
        key: key.root,
        keyCborLength: key.cborLength,
        keyMemory: key.memory,
        value: mapped.root,
        valueCborLength: mapped.cborLength,
        valueMemory: mapped.memory,
        tail: summary.root,
        length: summary.length + 1n,
        payloadCborLength:
          key.cborLength + mapped.cborLength + summary.payloadCborLength,
        memory: key.memory + mapped.memory + summary.memory,
      };
      summary = {
        root: hashMidgardCekDataPairNode(node),
        length: node.length,
        payloadCborLength: node.payloadCborLength,
        memory: node.memory,
      };
    }
    return summary;
  };

  let node: MidgardCekDataNode;
  if (typeof value === "bigint") {
    node = {
      kind: "integer",
      cborRoot: commitMidgardCekBlob(canonicalCbor).root,
      cborLength: BigInt(canonicalCbor.length),
      memory: 4n + semanticIntegerMemory(value),
    };
  } else if (typeof value === "string") {
    const bytes = Buffer.from(value, "hex");
    node = {
      kind: "bytes",
      bytesRoot: commitMidgardCekBlob(bytes).root,
      bytesLength: BigInt(bytes.length),
      cborLength: midgardCekDataBytesCborLength(BigInt(bytes.length)),
      memory: 4n + BigInt(Math.max(1, bytes.length)),
    };
  } else if (Array.isArray(value)) {
    const items = commitList(value);
    node = {
      kind: "list",
      itemsCount: items.length,
      itemsRoot: items.root,
      cborLength: midgardCekDataListCborLength(
        items.length,
        items.payloadCborLength,
      ),
      memory: 4n + items.memory,
    };
  } else if (isSemanticMap(value)) {
    const entries = commitPairs(value.entries);
    node = {
      kind: "map",
      entriesCount: entries.length,
      entriesRoot: entries.root,
      cborLength: midgardCekDataMapCborLength(
        entries.length,
        entries.payloadCborLength,
      ),
      memory: 4n + entries.memory,
    };
  } else if (isSemanticConstr(value)) {
    const constructor = value.constructor;
    const fields = commitList(value.fields);
    if (constructor <= 127n) {
      node = {
        kind: "constrSmall",
        constructor,
        fieldsCount: fields.length,
        fieldsRoot: fields.root,
        cborLength: midgardCekDataConstrCborLength(
          constructor,
          fields.length,
          fields.payloadCborLength,
        ),
        memory: 4n + fields.memory,
      };
    } else {
      const constructorCbor = Buffer.from(LucidData.to(constructor), "hex");
      node = {
        kind: "constrLarge",
        constructorCborRoot: commitMidgardCekBlob(constructorCbor).root,
        constructorCborLength: BigInt(constructorCbor.length),
        constructorMemory: 4n + semanticIntegerMemory(constructor),
        fieldsCount: fields.length,
        fieldsRoot: fields.root,
        cborLength: midgardCekDataConstrCborLength(
          constructor,
          fields.length,
          fields.payloadCborLength,
        ),
        memory: 4n + fields.memory,
      };
    }
  } else {
    throw new Error("CEK constant contains unknown Plutus Data");
  }
  if (node.cborLength !== BigInt(canonicalCbor.length)) {
    throw new Error("CEK semantic Data CBOR summary is not exact");
  }
  return {
    root: hashMidgardCekDataNode(node),
    cborLength: node.cborLength,
    memory: node.memory,
  };
};

export const makeLegacySemanticDataReconstructor = ({
  dataNodes: decodedDataNodes,
  dataLists: decodedDataLists,
  dataPairs: decodedDataPairs,
  materializeBlob,
}: SemanticDataReconstructionSource): ((
  root: Uint8Array,
) => SemanticDataValue) => {
  const reconstructedData = new Map<string, SemanticDataValue>();
  const reconstructDataList = (
    root: Uint8Array,
    length: bigint,
  ): readonly SemanticDataValue[] => {
    const items: SemanticDataValue[] = [];
    let cursor = rootKey(root);
    let remaining = length;
    while (remaining > 0n) {
      const link = decodedDataLists.get(cursor);
      if (link === undefined || link.length !== remaining) {
        throw new Error("CEK semantic Data list cannot be reconstructed");
      }
      items.push(reconstructData(link.head));
      cursor = rootKey(link.tail);
      remaining -= 1n;
    }
    if (cursor !== rootKey(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT)) {
      throw new Error("CEK semantic Data list has a non-empty tail");
    }
    return items;
  };
  const reconstructDataPairs = (
    root: Uint8Array,
    length: bigint,
  ): SemanticDataMap => {
    const entries: SemanticDataEntry[] = [];
    let cursor = rootKey(root);
    let remaining = length;
    while (remaining > 0n) {
      const link = decodedDataPairs.get(cursor);
      if (link === undefined || link.length !== remaining) {
        throw new Error("CEK semantic Data map cannot be reconstructed");
      }
      entries.push([reconstructData(link.key), reconstructData(link.value)]);
      cursor = rootKey(link.tail);
      remaining -= 1n;
    }
    if (cursor !== rootKey(MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT)) {
      throw new Error("CEK semantic Data map has a non-empty tail");
    }
    return { kind: "map", entries };
  };
  function reconstructData(root: Uint8Array): SemanticDataValue {
    const key = rootKey(root);
    const cached = reconstructedData.get(key);
    if (cached !== undefined) return cached;
    const node = decodedDataNodes.get(key);
    if (node === undefined) {
      throw new Error("CEK semantic Data node is missing");
    }
    let value: SemanticDataValue;
    if (node.kind === "integer") {
      const bytes = materializeBlob(
        node.cborRoot,
        BigInt(MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES),
        "CEK semantic integer",
      );
      const decoded = LucidData.from(bytes.toString("hex"));
      if (typeof decoded !== "bigint") {
        throw new Error("CEK semantic integer leaf is invalid");
      }
      value = decoded;
    } else if (node.kind === "bytes") {
      const bytes = materializeBlob(
        node.bytesRoot,
        BigInt(MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES),
        "CEK semantic bytes",
      );
      value = bytes.toString("hex");
    } else if (node.kind === "list") {
      value = reconstructDataList(node.itemsRoot, node.itemsCount);
    } else if (node.kind === "map") {
      value = reconstructDataPairs(node.entriesRoot, node.entriesCount);
    } else {
      let constructor: bigint;
      if (node.kind === "constrLarge") {
        const bytes = materializeBlob(
          node.constructorCborRoot,
          BigInt(MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES),
          "CEK semantic constructor",
        );
        const decoded = LucidData.from(bytes.toString("hex"));
        if (typeof decoded !== "bigint") {
          throw new Error("CEK semantic constructor index is invalid");
        }
        constructor = decoded;
      } else {
        constructor = node.constructor;
      }
      if (constructor < 0n) {
        throw new Error("CEK semantic constructor index must be non-negative");
      }
      value = {
        kind: "constr",
        constructor,
        fields: [...reconstructDataList(node.fieldsRoot, node.fieldsCount)],
      };
    }
    reconstructedData.set(key, value);
    return value;
  }
  return reconstructData;
};
