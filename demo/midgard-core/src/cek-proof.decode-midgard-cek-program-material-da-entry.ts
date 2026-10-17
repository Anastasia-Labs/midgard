import {
  encodeMidgardCekProgramMaterialEntry,
  exactMaterialPreimage,
  hashMidgardCekProgramMaterialPreimage,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES,
  MIDGARD_CEK_PROGRAM_MATERIAL_VERSION,
  type MidgardCekProgramMaterialEntry,
  midgardCekProgramMaterialKindFromTag,
  midgardCekProgramMaterialKindTag,
  type MidgardCekProgramMaterialValue,
} from "./cek-proof.decode-midgard-cek-program-envelope.js";
import { exactHash } from "./cek-proof.encode-midgard-cek-term-node.js";
import {
  encodeCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./codec/cbor.js";
import { type Hash32 } from "./codec/hash.js";

export const decodeMidgardCekProgramMaterialEntry = (
  bytes: Uint8Array,
): MidgardCekProgramMaterialEntry => {
  const source = Buffer.from(bytes);
  if (source.length > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES) {
    throw new Error(
      `CEK program material entry exceeds ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES.toString()} bytes`,
    );
  }
  const header = readCborArrayHeader(source, 0, "cek_program_material_entry");
  if (header.length !== 3) {
    throw new Error(
      "CEK program material entry must contain exactly three fields",
    );
  }
  const tag = readCborUnsigned(
    source,
    header.nextOffset,
    "cek_program_material_entry.kind",
  );
  const kind = midgardCekProgramMaterialKindFromTag(tag.value);
  const root = readCborBytes(
    source,
    tag.nextOffset,
    "cek_program_material_entry.root",
  );
  const exactRoot = exactHash(root.value, "cek_program_material_entry.root");
  const preimage = readCborBytes(
    source,
    root.nextOffset,
    "cek_program_material_entry.preimage",
  );
  if (preimage.nextOffset !== source.length) {
    throw new Error("CEK program material entry has trailing bytes");
  }
  const exactPreimage = exactMaterialPreimage(preimage.value);
  const decoded = Object.freeze({
    kind,
    root: exactRoot as Hash32,
    preimage: exactPreimage,
  });
  if (!encodeMidgardCekProgramMaterialEntry(decoded).equals(source)) {
    throw new Error("CEK program material entry CBOR is not canonical");
  }
  const computed = hashMidgardCekProgramMaterialPreimage(kind, exactPreimage);
  if (!Buffer.from(computed).equals(exactRoot)) {
    throw new Error("CEK program material root does not match its preimage");
  }
  return decoded;
};

/**
 * Compact DA/submission sidecar value. The content root is the containing
 * sorted entry key, while the versioned value carries the domain kind and
 * exact node preimage.
 */
export const encodeMidgardCekProgramMaterialDaValue = (
  entry: MidgardCekProgramMaterialValue,
): Buffer => {
  const encoded = encodeCbor([
    MIDGARD_CEK_PROGRAM_MATERIAL_VERSION,
    midgardCekProgramMaterialKindTag(entry.kind),
    exactMaterialPreimage(entry.preimage),
  ]);
  if (encoded.length > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES) {
    throw new Error(
      `CEK program material DA value exceeds ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES.toString()} bytes`,
    );
  }
  return encoded;
};

export const decodeMidgardCekProgramMaterialDaEntry = (
  root: Uint8Array,
  value: Uint8Array,
): MidgardCekProgramMaterialEntry => {
  const exactRoot = exactHash(root, "cek_program_material_da.root") as Hash32;
  const source = Buffer.from(value);
  if (source.length > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES) {
    throw new Error(
      `CEK program material DA value exceeds ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES.toString()} bytes`,
    );
  }
  const header = readCborArrayHeader(
    source,
    0,
    "cek_program_material_da.value",
  );
  if (header.length !== 3) {
    throw new Error(
      "CEK program material DA value must contain exactly three fields",
    );
  }
  const version = readCborUnsigned(
    source,
    header.nextOffset,
    "cek_program_material_da.version",
  );
  if (version.value !== MIDGARD_CEK_PROGRAM_MATERIAL_VERSION) {
    throw new Error(
      `unsupported CEK program material DA value version ${version.value.toString()}`,
    );
  }
  const tag = readCborUnsigned(
    source,
    version.nextOffset,
    "cek_program_material_da.kind",
  );
  const kind = midgardCekProgramMaterialKindFromTag(tag.value);
  const preimage = readCborBytes(
    source,
    tag.nextOffset,
    "cek_program_material_da.preimage",
  );
  if (preimage.nextOffset !== source.length) {
    throw new Error("CEK program material DA value has trailing bytes");
  }
  const decoded = Object.freeze({
    kind,
    root: exactRoot,
    preimage: exactMaterialPreimage(preimage.value),
  });
  if (!encodeMidgardCekProgramMaterialDaValue(decoded).equals(source)) {
    throw new Error("CEK program material DA value CBOR is not canonical");
  }
  if (
    !Buffer.from(
      hashMidgardCekProgramMaterialPreimage(kind, decoded.preimage),
    ).equals(exactRoot)
  ) {
    throw new Error(
      "CEK program material DA key does not match its typed preimage",
    );
  }
  return decoded;
};

export type MidgardCekDecodedProgramTerm =
  | { readonly kind: "variable"; readonly index: bigint }
  | { readonly kind: "error" }
  | { readonly kind: "builtin"; readonly tag: bigint }
  | {
      readonly kind: "unaryTerm";
      readonly termKind: "delay" | "lambda" | "force";
      readonly child: Hash32;
    }
  | {
      readonly kind: "application";
      readonly function: Hash32;
      readonly argument: Hash32;
    }
  | { readonly kind: "constant"; readonly value: Hash32 }
  | { readonly kind: "contextConstant"; readonly value: Hash32 }
  | {
      readonly kind: "constr";
      readonly tag: bigint;
      readonly count: bigint;
      readonly sequence: Hash32;
    }
  | {
      readonly kind: "case";
      readonly scrutinee: Hash32;
      readonly count: bigint;
      readonly sequence: Hash32;
    };

export type MidgardCekDecodedProgramValue = {
  readonly typeRoot: Hash32;
  readonly payloadRoot: Hash32;
  readonly payloadLength: bigint;
  readonly semanticRoot: Hash32;
  readonly memory: bigint;
};

export type MidgardCekDecodedProgramSequence = {
  readonly head: Hash32;
  readonly tail: Hash32;
  readonly length: bigint;
};

export type MidgardCekDecodedProgramBlob =
  | { readonly kind: "chunk"; readonly bytes: Buffer }
  | {
      readonly kind: "branch";
      readonly left: Hash32;
      readonly right: Hash32;
      readonly byteLength: bigint;
    };

export const readExactHashAt = (
  bytes: Buffer,
  offset: number,
  fieldName: string,
): { readonly value: Hash32; readonly nextOffset: number } => {
  const decoded = readCborBytes(bytes, offset, fieldName);
  return {
    value: exactHash(decoded.value, fieldName) as Hash32,
    nextOffset: decoded.nextOffset,
  };
};

export const assertPreimageConsumed = (
  bytes: Buffer,
  nextOffset: number,
  fieldName: string,
): void => {
  if (nextOffset !== bytes.length) {
    throw new Error(`${fieldName} has trailing bytes`);
  }
};
