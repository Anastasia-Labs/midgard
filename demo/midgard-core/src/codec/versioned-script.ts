import { blake2b } from "@noble/hashes/blake2.js";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborUnsigned,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./cbor.js";
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

export const encodeMidgardVersionedScript = (
  script: MidgardVersionedScript,
): Buffer => {
  const scriptBytes =
    script.language === "NativeCardano"
      ? encodeMidgardNativeScript(script.nativeScript)
      : Buffer.from(script.scriptBytes);
  return encodeCborArrayRaw([
    encodeCborUnsigned(MidgardVersionedScriptTags[script.language]),
    encodeCborBytes(scriptBytes),
  ]);
};

export const decodeMidgardVersionedScript = (
  bytes: Uint8Array,
): MidgardVersionedScript => {
  const header = readCborArrayHeader(bytes, 0, "versioned_script");
  if (header.length !== 2) {
    fail("MidgardVersionedScript must be [language_tag, script_bytes]");
  }
  const tag = readCborUnsigned(bytes, header.nextOffset, "versioned_script.tag");
  const payload = readCborBytes(bytes, tag.nextOffset, "versioned_script.bytes");
  if (payload.nextOffset !== bytes.length) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Trailing bytes after MidgardVersionedScript",
      `offset=${payload.nextOffset}`,
    );
  }

  const decoded: MidgardVersionedScript = (() => {
    if (tag.value === MidgardVersionedScriptTags.NativeCardano) {
      const native = decodeMidgardNativeScript(payload.value);
      return {
        language: "NativeCardano",
        scriptBytes: native.cbor,
        nativeScript: native.script,
      };
    }
    if (tag.value === MidgardVersionedScriptTags.PlutusV3) {
      return { language: "PlutusV3", scriptBytes: payload.value };
    }
    if (tag.value === MidgardVersionedScriptTags.MidgardV1) {
      return { language: "MidgardV1", scriptBytes: payload.value };
    }
    return fail("Unsupported Midgard versioned script tag", tag.value.toString());
  })();

  const encoded = encodeMidgardVersionedScript(decoded);
  if (!encoded.equals(Buffer.from(bytes))) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "MidgardVersionedScript CBOR is not canonical",
    );
  }
  return decoded;
};

export const hashMidgardVersionedScript = (
  script: MidgardVersionedScript,
): string =>
  Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from([MidgardScriptHashPrefixes[script.language]]),
        script.language === "NativeCardano"
          ? encodeMidgardNativeScript(script.nativeScript)
          : Buffer.from(script.scriptBytes),
      ]),
      { dkLen: 28 },
    ),
  ).toString("hex");

export const encodeMidgardVersionedScriptListPreimage = (
  scripts: readonly MidgardVersionedScript[],
): Buffer => encodeCborArrayRaw(scripts.map(encodeMidgardVersionedScript));

export const decodeMidgardVersionedScriptListPreimage = (
  bytes: Uint8Array,
  fieldName = "script_tx_wits",
): readonly MidgardVersionedScript[] => {
  const header = readCborArrayHeader(bytes, 0, fieldName);
  let cursor = header.nextOffset;
  const scripts: MidgardVersionedScript[] = [];
  for (let i = 0; i < header.length; i += 1) {
    const start = cursor;
    const itemHeader = readCborArrayHeader(bytes, cursor, `${fieldName}[${i}]`);
    cursor = itemHeader.nextOffset;
    for (let j = 0; j < itemHeader.length; j += 1) {
      const spanStart = cursor;
      if (j === 0) {
        cursor = readCborUnsigned(bytes, cursor, `${fieldName}[${i}].tag`).nextOffset;
      } else if (j === 1) {
        cursor = readCborBytes(bytes, cursor, `${fieldName}[${i}].bytes`).nextOffset;
      } else {
        fail("MidgardVersionedScript must have exactly 2 fields", `${fieldName}[${i}]`);
      }
      if (cursor <= spanStart) {
        fail("Invalid versioned script cursor progress", `${fieldName}[${i}]`);
      }
    }
    const script = decodeMidgardVersionedScript(bytes.subarray(start, cursor));
    scripts.push(script);
  }
  if (cursor !== bytes.length) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      `${fieldName} has trailing bytes`,
      `offset=${cursor}`,
    );
  }
  const encoded = encodeMidgardVersionedScriptListPreimage(scripts);
  if (!encoded.equals(Buffer.from(bytes))) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      `${fieldName} is not canonical`,
    );
  }
  return scripts;
};
