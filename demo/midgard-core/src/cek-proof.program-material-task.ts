import {
  decodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialEntry,
  type MidgardCekProgramMaterialEntry,
} from "./cek-proof.decode-midgard-cek-program-envelope.js";
import { decodeMidgardCekProgramMaterialEntry } from "./cek-proof.decode-midgard-cek-program-material-da-entry.js";
import {
  encodeMidgardCekProgramEnvelope,
  type MidgardCekProgramEnvelope,
} from "./cek-proof.encode-midgard-cek-continuation-frame.js";
import {
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT,
} from "./cek-proof.encode-midgard-cek-term-node.js";
import { type Hash32 } from "./codec/hash.js";

export type ProgramMaterialTask =
  | {
      readonly kind: "term";
      readonly root: Hash32;
    }
  | {
      readonly kind: "value";
      readonly root: Hash32;
    }
  | {
      readonly kind: "sequence";
      readonly root: Hash32;
      readonly length: bigint;
    }
  | {
      readonly kind: "blob";
      readonly root: Hash32;
      readonly byteLength?: bigint;
      readonly maxByteLength?: bigint;
    }
  | {
      readonly kind: "dataNode";
      readonly root: Hash32;
    }
  | {
      readonly kind: "dataList";
      readonly root: Hash32;
      readonly length: bigint;
    }
  | {
      readonly kind: "dataPair";
      readonly root: Hash32;
      readonly length: bigint;
    };

/**
 * A content-addressed CEK node was not present in the supplied material
 * snapshot. Callers may classify this as publication lag only when the source
 * snapshot itself was authenticated and clean; all other verifier failures
 * remain ordinary errors.
 */
export class MidgardCekProgramMaterialMissingRootError extends Error {
  readonly rootHex: string;

  constructor(root: Uint8Array) {
    const rootHex = Buffer.from(root).toString("hex");
    super(`CEK program material is missing root ${rootHex}`);
    this.name = "MidgardCekProgramMaterialMissingRootError";
    this.rootHex = rootHex;
  }
}

export type MidgardCekProgramConstantMaterial = {
  readonly valueRoot: Hash32;
  readonly typeRoot: Hash32;
  readonly payloadRoot: Hash32;
  readonly semanticRoot: Hash32;
  readonly memory: bigint;
  readonly typeCbor: Buffer;
  readonly payloadCbor: Buffer;
};

export type MidgardCekProgramMaterialVerification = {
  readonly reachableRoots: ReadonlySet<string>;
  readonly nodeCount: bigint;
  readonly materialByteLength: bigint;
  readonly constants: readonly MidgardCekProgramConstantMaterial[];
};

export type MidgardCekProgramMaterialVerificationOptions = {
  readonly allowUnreachable?: boolean;
  /**
   * Allocation observability for resource-bound tests and metrics. It fires
   * only when a final content root is assembled, never for branch validation
   * or a bundle-cache hit. Callback failures are ignored so observability
   * cannot change verifier acceptance.
   */
  readonly onBlobMaterialized?: (rootHex: string, byteLength: bigint) => void;
  /**
   * Fires on the first validated constant-result materialization for a value
   * content root in a bundle. Callback failures are ignored.
   */
  readonly onConstantMaterialized?: (
    valueRootHex: string,
    payloadByteLength: bigint,
  ) => void;
};

export type NormalizedProgramMaterial = ReadonlyMap<
  string,
  MidgardCekProgramMaterialEntry
>;

export type ProgramMaterialBundleCache = {
  /**
   * Internal-only buffers keyed by authenticated content root. Callers receive
   * copies, so cached bytes are immutable for the lifetime of verification.
   */
  readonly materializedBlobs: Map<string, Buffer>;
  readonly validatedConstants: Map<
    string,
    {
      readonly typeCbor: Buffer;
      readonly payloadCbor: Buffer;
    }
  >;
};

export const normalizeProgramMaterial = (
  entries: Iterable<MidgardCekProgramMaterialEntry>,
): NormalizedProgramMaterial => {
  const normalized = new Map<string, MidgardCekProgramMaterialEntry>();
  let materialByteLength = 0n;
  for (const entry of entries) {
    const exact = decodeMidgardCekProgramMaterialEntry(
      encodeMidgardCekProgramMaterialEntry(entry),
    );
    const key = Buffer.from(exact.root).toString("hex");
    if (normalized.has(key)) {
      throw new Error(`duplicate CEK program material root ${key}`);
    }
    normalized.set(key, exact);
    if (BigInt(normalized.size) > MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT) {
      throw new Error(
        `CEK program material contains more than ${MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT.toString()} nodes`,
      );
    }
    materialByteLength += BigInt(exact.preimage.length);
    if (materialByteLength > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES) {
      throw new Error(
        `CEK program material exceeds ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES.toString()} bytes`,
      );
    }
  }
  return normalized;
};

export const canonicalProgramEnvelope = (
  envelope: MidgardCekProgramEnvelope,
): {
  readonly envelope: MidgardCekProgramEnvelope;
  readonly identity: string;
} => {
  const encoded = encodeMidgardCekProgramEnvelope(envelope);
  return {
    envelope: decodeMidgardCekProgramEnvelope(encoded),
    identity: encoded.toString("hex"),
  };
};

export const greatestPowerOfTwoBelow = (value: bigint): bigint => {
  let power = 1n;
  while (power * 2n < value) {
    power *= 2n;
  }
  return power;
};

export type SemanticConstantType =
  | { readonly kind: "integer" }
  | { readonly kind: "bytes" }
  | { readonly kind: "string" }
  | { readonly kind: "unit" }
  | { readonly kind: "boolean" }
  | {
      readonly kind: "list";
      readonly element: SemanticConstantType;
    }
  | {
      readonly kind: "pair";
      readonly first: SemanticConstantType;
      readonly second: SemanticConstantType;
    }
  | { readonly kind: "data" }
  | { readonly kind: "blsG1" }
  | { readonly kind: "blsG2" }
  | { readonly kind: "blsMillerLoop" };

type SemanticConstr = {
  readonly kind: "constr";
  readonly constructor: bigint;
  readonly fields: readonly SemanticDataValue[];
};

/** One map entry: a key and its value. */
export type SemanticDataEntry = readonly [SemanticDataValue, SemanticDataValue];

/**
 * A Plutus Data map as the ordered list of its entries. Data maps are lists
 * of pairs: they keep every entry, in order, duplicate keys included.
 */
export type SemanticDataMap = {
  readonly kind: "map";
  readonly entries: readonly SemanticDataEntry[];
};

export type SemanticDataValue =
  | bigint
  | string
  | readonly SemanticDataValue[]
  | SemanticDataMap
  | SemanticConstr;

/**
 * `Array.isArray` is declared `value is any[]`, so using it to pick the list
 * member out of {@link SemanticDataValue} discards the element type. This
 * narrows to the union member instead.
 */
export const isSemanticList = (
  value: SemanticDataValue,
): value is readonly SemanticDataValue[] => Array.isArray(value);

export const isSemanticMap = (
  value: SemanticDataValue,
): value is SemanticDataMap =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  "kind" in value &&
  value.kind === "map";

export const isSemanticConstr = (
  value: SemanticDataValue,
): value is SemanticConstr =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  "kind" in value &&
  value.kind === "constr";

/**
 * A map's entries flattened key, value, key, value, in order. An entry that
 * is not a two-item array is refused, as unknown Data is.
 */
export const semanticMapChildren = (
  value: SemanticDataMap,
): readonly SemanticDataValue[] => {
  const unknown = (): Error =>
    new Error("CEK constant contains unknown semantic Data");
  const entries: unknown = value.entries;
  if (!Array.isArray(entries)) throw unknown();
  const children: SemanticDataValue[] = [];
  for (const entry of entries as readonly unknown[]) {
    if (!Array.isArray(entry) || entry.length !== 2) throw unknown();
    children.push(entry[0] as SemanticDataValue, entry[1] as SemanticDataValue);
  }
  return children;
};

export const semanticCborHeader = (major: number, value: bigint): Buffer => {
  if (value < 0n) {
    throw new Error("CEK semantic CBOR length must be non-negative");
  }
  const prefix = major << 5;
  if (value < 24n) return Buffer.from([prefix | Number(value)]);
  if (value <= 0xffn) {
    return Buffer.from([prefix | 24, Number(value)]);
  }
  if (value <= 0xffffn) {
    const result = Buffer.alloc(3);
    result[0] = prefix | 25;
    result.writeUInt16BE(Number(value), 1);
    return result;
  }
  if (value <= 0xffff_ffffn) {
    const result = Buffer.alloc(5);
    result[0] = prefix | 26;
    result.writeUInt32BE(Number(value), 1);
    return result;
  }
  if (value <= 0xffff_ffff_ffff_ffffn) {
    const result = Buffer.alloc(9);
    result[0] = prefix | 27;
    result.writeBigUInt64BE(value, 1);
    return result;
  }
  throw new Error("CEK semantic CBOR length exceeds uint64");
};

export const encodeSemanticBytes = (value: Buffer): Buffer => {
  if (value.length <= 64) {
    return Buffer.concat([semanticCborHeader(2, BigInt(value.length)), value]);
  }
  const chunks: Buffer[] = [Buffer.from([0x5f])];
  for (let offset = 0; offset < value.length; offset += 64) {
    const chunk = value.subarray(offset, offset + 64);
    chunks.push(semanticCborHeader(2, BigInt(chunk.length)), chunk);
  }
  chunks.push(Buffer.from([0xff]));
  return Buffer.concat(chunks);
};
