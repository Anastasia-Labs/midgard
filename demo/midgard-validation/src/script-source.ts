import { blake2b } from "@noble/hashes/blake2.js";
import {
  decodeMidgardNativeScript,
  decodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  MidgardScriptHashPrefixes,
  type MidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import type { VersionedScript } from "@al-ft/midgard-ts";

export const MIDGARD_V1_SCRIPT_TAG = 0x80;

export type LocalScriptVersion = "NativeCardano" | "PlutusV3" | "MidgardV1";

export type ScriptSource = {
  readonly origin: "inline" | "reference";
  readonly sourceId: string;
  readonly version: LocalScriptVersion;
  readonly scriptBytes: Buffer;
  readonly scriptHash: string;
  readonly nativeScript?: MidgardNativeScript;
};

export type ResolvedScriptSource = {
  readonly version: LocalScriptVersion;
  readonly scriptHash: string;
  readonly source: ScriptSource;
};

export const decodeScriptSource = (
  bytes: Uint8Array,
  origin: ScriptSource["origin"],
  sourceId: string,
): ScriptSource => {
  const script = decodeMidgardVersionedScript(bytes);
  const scriptHash = hashMidgardVersionedScript(script);
  if (script.language === "NativeCardano") {
    return {
      origin,
      sourceId,
      version: "NativeCardano",
      scriptBytes: Buffer.from(script.scriptBytes),
      scriptHash,
      nativeScript: script.nativeScript,
    };
  }
  return {
    origin,
    sourceId,
    version: script.language,
    scriptBytes: Buffer.from(script.scriptBytes),
    scriptHash,
  };
};

// midgard-ts VersionedScript → ScriptSource (no CBOR round-trip).
// Mirrors the inline shape of `hashMidgardVersionedScript`
// (blake2b-224 over prefix_byte || bytes) and surfaces the parsed
// `nativeScript` for the NativeCardano case so phase-b can pass it to
// `verifyMidgardNativeScript` without re-decoding.
export const scriptSourceFromMidgardTsScript = (
  script: VersionedScript,
  origin: ScriptSource["origin"],
  sourceId: string,
): ScriptSource => {
  const scriptHash = Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from([MidgardScriptHashPrefixes[script.language]]),
        Buffer.from(script.bytes),
      ]),
      { dkLen: 28 },
    ),
  ).toString("hex");
  if (script.language === "NativeCardano") {
    const decoded = decodeMidgardNativeScript(script.bytes);
    return {
      origin,
      sourceId,
      version: "NativeCardano",
      scriptBytes: Buffer.from(decoded.cbor),
      scriptHash,
      nativeScript: decoded.script,
    };
  }
  return {
    origin,
    sourceId,
    version: script.language,
    scriptBytes: Buffer.from(script.bytes),
    scriptHash,
  };
};

export const resolveScriptSource = (
  scriptHash: string,
  sources: readonly ScriptSource[],
): ResolvedScriptSource | undefined => {
  for (const source of sources) {
    if (source.scriptHash === scriptHash) {
      return { version: source.version, scriptHash, source };
    }
  }
  return undefined;
};
