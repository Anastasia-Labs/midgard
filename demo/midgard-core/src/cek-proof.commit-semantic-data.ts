import { Data as LucidData } from "@lucid-evolution/lucid";

import { commitMidgardCekBlob } from "./cek-proof.encode-midgard-cek-continuation-frame.js";
import { MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES } from "./cek-proof.encode-midgard-cek-term-node.js";
import {
  encodeSemanticBytes,
  isSemanticConstr,
  isSemanticList,
  isSemanticMap,
  semanticCborHeader,
  type SemanticConstantType,
  type SemanticDataValue,
} from "./cek-proof.program-material-task.js";
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
} from "./cek-semantic.js";
import { type Hash32 } from "./codec/hash.js";

export const decodeSemanticConstantType = (
  typeCbor: Uint8Array,
): SemanticConstantType => {
  if (typeCbor.length > MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES) {
    throw new Error(
      `CEK constant type exceeds the ${MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES.toString()}-byte L1 bound`,
    );
  }
  const decoded = LucidData.from(Buffer.from(typeCbor).toString("hex"));
  const canonical = encodeSemanticData(decoded as SemanticDataValue);
  if (!canonical.equals(Buffer.from(typeCbor))) {
    throw new Error("CEK constant type CBOR is not canonical");
  }
  if (
    !Array.isArray(decoded) ||
    !decoded.every((tag) => typeof tag === "bigint")
  ) {
    throw new Error("CEK constant type payload is not an integer list");
  }
  const stack: SemanticConstantType[] = [];
  for (let offset = decoded.length - 1; offset >= 0; offset -= 1) {
    const tag = decoded[offset];
    if (tag === 0n) stack.push({ kind: "integer" });
    else if (tag === 1n) stack.push({ kind: "bytes" });
    else if (tag === 2n) stack.push({ kind: "string" });
    else if (tag === 3n) stack.push({ kind: "unit" });
    else if (tag === 4n) stack.push({ kind: "boolean" });
    else if (tag === 8n) stack.push({ kind: "data" });
    else if (tag === 9n) stack.push({ kind: "blsG1" });
    else if (tag === 10n) stack.push({ kind: "blsG2" });
    else if (tag === 11n) stack.push({ kind: "blsMillerLoop" });
    else if (tag === 5n) {
      const element = stack.pop();
      if (element === undefined) {
        throw new Error("CEK constant list type is missing its element type");
      }
      stack.push({ kind: "list", element });
    } else if (tag === 6n) {
      const first = stack.pop();
      const second = stack.pop();
      if (first === undefined || second === undefined) {
        throw new Error("CEK constant pair type is missing a child type");
      }
      stack.push({ kind: "pair", first, second });
    } else {
      throw new Error("CEK constant has an unknown semantic type tag");
    }
  }
  if (stack.length !== 1) {
    throw new Error("CEK constant type payload has trailing tags");
  }
  return stack[0]!;
};

export const semanticIntegerMemory = (value: bigint): bigint => {
  const doubled = value < 0n ? (-value - 1n) * 2n : value * 2n;
  if (doubled === 0n) return 1n;
  return BigInt(Math.ceil(doubled.toString(2).length / 8));
};

const encodeSemanticList = (values: readonly SemanticDataValue[]): Buffer =>
  values.length === 0
    ? Buffer.from([0x80])
    : Buffer.concat([
        Buffer.from([0x9f]),
        ...values.map(encodeSemanticData),
        Buffer.from([0xff]),
      ]);

export const encodeSemanticData = (value: SemanticDataValue): Buffer => {
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
      semanticCborHeader(5, BigInt(value.size)),
      ...[...value.entries()].flatMap(([key, mapped]) => [
        encodeSemanticData(key),
        encodeSemanticData(mapped),
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

export const commitSemanticData = (
  value: SemanticDataValue,
): SemanticDataSummary => {
  const canonicalCbor = encodeSemanticData(value);
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
      const head = commitSemanticData(items[index]!);
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
      const key = commitSemanticData(keyValue);
      const mapped = commitSemanticData(mappedValue);
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
    const entries = commitPairs([...value.entries()]);
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
