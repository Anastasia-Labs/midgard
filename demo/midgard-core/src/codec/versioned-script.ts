import { blake2b } from "@noble/hashes/blake2.js";

import {
  assertCanonicalCborRoundTrip,
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
import {
  decodeMidgardFieldPreimage,
  encodeMidgardFieldPreimage,
} from "./native-tx-field-access-v1.js";

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
  const tag = readCborUnsigned(
    bytes,
    header.nextOffset,
    "versioned_script.tag",
  );
  const payload = readCborBytes(
    bytes,
    tag.nextOffset,
    "versioned_script.bytes",
  );
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
    return fail(
      "Unsupported Midgard versioned script tag",
      tag.value.toString(),
    );
  })();

  assertCanonicalCborRoundTrip(
    bytes,
    decoded,
    encodeMidgardVersionedScript,
    "MidgardVersionedScript CBOR is not canonical",
  );
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

/**
 * The §5.1 preimage of field 6 (`script_tx_wits`).
 *
 * Each item carries the per-item byte-string envelope, like all nine fields —
 * under the retired counted scheme this field concatenated raw item CBOR, and
 * `docs/spec/midgard-tx.md` §5.1 prohibits that form. The envelope is what buys
 * O(1) top-level skips: one head decode plus a byte jump per item, instead of a
 * structural CBOR walk into each script.
 */
export const encodeMidgardVersionedScriptListPreimage = (
  scripts: readonly MidgardVersionedScript[],
): Buffer =>
  encodeMidgardFieldPreimage(scripts.map(encodeMidgardVersionedScript));

export const decodeMidgardVersionedScriptListPreimage = (
  bytes: Uint8Array,
  fieldName = "script_tx_wits",
): readonly MidgardVersionedScript[] => {
  // §5.1's one uniform byte-list decode; it already fails closed on a
  // non-minimal header, a count that disagrees with the walked content, and
  // trailing bytes, so this function only has to read each item back.
  const scripts = decodeMidgardFieldPreimage(bytes).map((item, index) => {
    try {
      return decodeMidgardVersionedScript(item);
    } catch (error) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.CborDecode,
        `${fieldName}[${index}] is not a canonical versioned script`,
        String(error),
      );
    }
  });
  assertCanonicalCborRoundTrip(
    bytes,
    scripts,
    encodeMidgardVersionedScriptListPreimage,
    `${fieldName} is not canonical`,
  );
  return scripts;
};
