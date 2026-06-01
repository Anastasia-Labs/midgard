import { blake2b } from "@noble/hashes/blake2.js";

import {
  BinaryReader,
  BinaryWriter,
  readU64,
  readVarBytesDynamic,
  readVarBytesLen,
  writeU64,
  writeVarBytesDynamic,
  writeVarBytesStatic,
} from "./binary.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import {
  decodeMidgardNativeScript,
  encodeMidgardNativeScript,
  type MidgardNativeScript,
} from "./native-script.js";

export const MidgardVersionedScriptTags = {
  NativeCardano: 0n,
  PlutusV3: 3n,
  MidgardV1: 128n,
} as const;

export const MidgardScriptHashPrefixes = {
  NativeCardano: 0x00,
  PlutusV3: 0x03,
  MidgardV1: 0x80,
} as const;

export type MidgardScriptLanguage = keyof typeof MidgardVersionedScriptTags;

export type MidgardVersionedScript =
  | {
      readonly language: "NativeCardano";
      readonly scriptBytes: Buffer;
      readonly nativeScript: MidgardNativeScript;
    }
  | { readonly language: "PlutusV3"; readonly scriptBytes: Buffer }
  | { readonly language: "MidgardV1"; readonly scriptBytes: Buffer };

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
    detail,
  );
};

const languageToTag = (language: MidgardScriptLanguage): bigint =>
  MidgardVersionedScriptTags[language];

const tagToLanguage = (tag: bigint): MidgardScriptLanguage => {
  if (tag === MidgardVersionedScriptTags.NativeCardano) return "NativeCardano";
  if (tag === MidgardVersionedScriptTags.PlutusV3) return "PlutusV3";
  if (tag === MidgardVersionedScriptTags.MidgardV1) return "MidgardV1";
  return fail("Unsupported Midgard versioned script tag", tag.toString());
};

const innerScriptBytes = (script: MidgardVersionedScript): Buffer =>
  script.language === "NativeCardano"
    ? encodeMidgardNativeScript(script.nativeScript)
    : Buffer.from(script.scriptBytes);

/**
 * Binary encoding of MidgardVersionedScript:
 *   Static:  language_tag (u64), script_bytes_len (u64)
 *   Dynamic: script_bytes + pad
 *
 * Inner script payloads (Plutus / Native Cardano CBOR) remain opaque byte
 * blobs; only the container is binary.
 */
export const writeMidgardVersionedScriptStatic = (
  w: BinaryWriter,
  script: MidgardVersionedScript,
): void => {
  writeU64(w, Number(languageToTag(script.language)));
  writeVarBytesStatic(w, innerScriptBytes(script));
};

export const writeMidgardVersionedScriptDynamic = (
  w: BinaryWriter,
  script: MidgardVersionedScript,
): void => {
  writeVarBytesDynamic(w, innerScriptBytes(script));
};

type VersionedScriptPartial = {
  readonly language: MidgardScriptLanguage;
  readonly scriptLen: number;
};

export const readMidgardVersionedScriptStatic = (
  r: BinaryReader,
): VersionedScriptPartial => {
  const tag = BigInt(readU64(r));
  const language = tagToLanguage(tag);
  const scriptLen = readVarBytesLen(r);
  return { language, scriptLen };
};

export const readMidgardVersionedScriptDynamic = (
  r: BinaryReader,
  partial: VersionedScriptPartial,
): MidgardVersionedScript => {
  const bytes = readVarBytesDynamic(r, partial.scriptLen);
  return materializeVersionedScript(partial.language, bytes);
};

const materializeVersionedScript = (
  language: MidgardScriptLanguage,
  scriptBytes: Buffer,
): MidgardVersionedScript => {
  if (language === "NativeCardano") {
    const decoded = decodeMidgardNativeScript(scriptBytes);
    return {
      language: "NativeCardano",
      scriptBytes: decoded.cbor,
      nativeScript: decoded.script,
    };
  }
  return { language, scriptBytes };
};

export const encodeMidgardVersionedScript = (
  script: MidgardVersionedScript,
): Buffer => {
  const sw = new BinaryWriter();
  writeMidgardVersionedScriptStatic(sw, script);
  const dw = new BinaryWriter();
  writeMidgardVersionedScriptDynamic(dw, script);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

export const decodeMidgardVersionedScript = (
  bytes: Uint8Array,
): MidgardVersionedScript => {
  const r = new BinaryReader(bytes);
  const partial = readMidgardVersionedScriptStatic(r);
  const decoded = readMidgardVersionedScriptDynamic(r, partial);
  r.expectEnd("versioned_script");
  return decoded;
};

export const hashMidgardVersionedScript = (
  script: MidgardVersionedScript,
): string =>
  Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from([MidgardScriptHashPrefixes[script.language]]),
        innerScriptBytes(script),
      ]),
      { dkLen: 28 },
    ),
  ).toString("hex");

/**
 * Binary list-of-versioned-scripts (script_tx_wits preimage).
 *
 *   Static:  count (u64) + per-script static (tag u64 + len u64)
 *   Dynamic: per-script scriptBytes + pad
 */
export const writeMidgardVersionedScriptListStatic = (
  w: BinaryWriter,
  scripts: readonly MidgardVersionedScript[],
): void => {
  writeU64(w, scripts.length);
  for (const s of scripts) writeMidgardVersionedScriptStatic(w, s);
};

export const writeMidgardVersionedScriptListDynamic = (
  w: BinaryWriter,
  scripts: readonly MidgardVersionedScript[],
): void => {
  for (const s of scripts) writeMidgardVersionedScriptDynamic(w, s);
};

export const readMidgardVersionedScriptListStatic = (
  r: BinaryReader,
): VersionedScriptPartial[] => {
  const count = readU64(r);
  const partials: VersionedScriptPartial[] = [];
  for (let i = 0; i < count; i += 1) {
    partials.push(readMidgardVersionedScriptStatic(r));
  }
  return partials;
};

export const readMidgardVersionedScriptListDynamic = (
  r: BinaryReader,
  partials: readonly VersionedScriptPartial[],
): MidgardVersionedScript[] => {
  const out: MidgardVersionedScript[] = [];
  for (const p of partials) {
    out.push(readMidgardVersionedScriptDynamic(r, p));
  }
  return out;
};

export const encodeMidgardVersionedScriptList = (
  scripts: readonly MidgardVersionedScript[],
): Buffer => {
  const sw = new BinaryWriter();
  writeMidgardVersionedScriptListStatic(sw, scripts);
  const dw = new BinaryWriter();
  writeMidgardVersionedScriptListDynamic(dw, scripts);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

export const decodeMidgardVersionedScriptList = (
  bytes: Uint8Array,
  fieldName = "versioned_script_list",
): MidgardVersionedScript[] => {
  const r = new BinaryReader(bytes);
  const partials = readMidgardVersionedScriptListStatic(r);
  const scripts = readMidgardVersionedScriptListDynamic(r, partials);
  r.expectEnd(fieldName);
  return scripts;
};

/**
 * Legacy name preserved for downstream callers; functionally equivalent to
 * decodeMidgardVersionedScriptList but accepts the same `fieldName`.
 */
export const decodeMidgardVersionedScriptListPreimage =
  decodeMidgardVersionedScriptList;

export const encodeMidgardVersionedScriptListPreimage =
  encodeMidgardVersionedScriptList;
