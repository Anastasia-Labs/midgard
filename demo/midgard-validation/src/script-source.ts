import {
  decodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  type MidgardNativeScript,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core/codec";

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

export const scriptSourceFromVersionedScript = (
  script: MidgardVersionedScript,
  origin: ScriptSource["origin"],
  sourceId: string,
): ScriptSource => {
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

export const decodeScriptSource = (
  bytes: Uint8Array,
  origin: ScriptSource["origin"],
  sourceId: string,
): ScriptSource =>
  scriptSourceFromVersionedScript(
    decodeMidgardVersionedScript(bytes),
    origin,
    sourceId,
  );

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
