/**
 * Versioned (language-tagged) script representation.
 *
 * Midgard supports three script "languages" for reference scripts and witness
 * scripts:
 *   0 = NativeCardano  — `bytes` is the CBOR of a Cardano `NativeScript`
 *   1 = PlutusV3       — `bytes` is the raw flat-encoded Plutus V3 script
 *   2 = MidgardV1      — `bytes` is the raw flat-encoded Midgard V1 script
 *
 * Encoding (canonical codec, static/dynamic split):
 *   Static:  language(u64) + bytes_len(u64)
 *   Dynamic: bytes + alignment padding
 */

import {
  Writer,
  Reader,
  writeU64,
  readU64,
  writeVarBytesStatic,
  writeVarBytesDynamic,
  readVarBytesLen,
  readVarBytesDynamic,
} from "../codec";

export type ScriptLanguage = "NativeCardano" | "PlutusV3" | "MidgardV1";

export interface VersionedScript {
  language: ScriptLanguage;
  bytes: Uint8Array;
}

const LANGUAGE_TO_DISC: Record<ScriptLanguage, number> = {
  NativeCardano: 0,
  PlutusV3: 1,
  MidgardV1: 2,
};

const DISC_TO_LANGUAGE: Record<number, ScriptLanguage> = {
  0: "NativeCardano",
  1: "PlutusV3",
  2: "MidgardV1",
};

export interface VersionedScriptPartial {
  language: ScriptLanguage;
  bytesLen: number;
}

export function writeVersionedScriptStatic(w: Writer, vs: VersionedScript): void {
  writeU64(w, LANGUAGE_TO_DISC[vs.language]);
  writeVarBytesStatic(w, vs.bytes); // len u64
}

export function writeVersionedScriptDynamic(
  w: Writer,
  vs: VersionedScript,
): void {
  writeVarBytesDynamic(w, vs.bytes); // bytes + pad
}

export function readVersionedScriptStatic(r: Reader): VersionedScriptPartial {
  const disc = readU64(r);
  const language = DISC_TO_LANGUAGE[disc];
  if (language === undefined) {
    throw new Error(`UnknownDiscriminant for VersionedScript language: ${disc}`);
  }
  const bytesLen = readVarBytesLen(r);
  return { language, bytesLen };
}

export function readVersionedScriptDynamic(
  r: Reader,
  p: VersionedScriptPartial,
): VersionedScript {
  return { language: p.language, bytes: readVarBytesDynamic(r, p.bytesLen) };
}

// ---------------------------------------------------------------------------
// Vec<VersionedScript>  — len(u64) + each.static, then each.dynamic
// ---------------------------------------------------------------------------

export function writeVersionedScriptVecStatic(
  w: Writer,
  scripts: VersionedScript[],
): void {
  writeU64(w, scripts.length);
  for (const s of scripts) writeVersionedScriptStatic(w, s);
}

export function writeVersionedScriptVecDynamic(
  w: Writer,
  scripts: VersionedScript[],
): void {
  for (const s of scripts) writeVersionedScriptDynamic(w, s);
}

export function readVersionedScriptVecStatic(
  r: Reader,
): VersionedScriptPartial[] {
  const len = readU64(r);
  const partials: VersionedScriptPartial[] = [];
  for (let i = 0; i < len; i++) partials.push(readVersionedScriptStatic(r));
  return partials;
}

export function readVersionedScriptVecDynamic(
  r: Reader,
  partials: VersionedScriptPartial[],
): VersionedScript[] {
  return partials.map((p) => readVersionedScriptDynamic(r, p));
}

/** Encode a `Vec<VersionedScript>` as a standalone blob (static then dynamic). */
export function encodeVersionedScriptVec(
  scripts: VersionedScript[],
): Uint8Array {
  const sw = new Writer();
  writeVersionedScriptVecStatic(sw, scripts);
  const dw = new Writer();
  writeVersionedScriptVecDynamic(dw, scripts);
  const s = sw.toBytes();
  const d = dw.toBytes();
  const out = new Uint8Array(s.length + d.length);
  out.set(s);
  out.set(d, s.length);
  return out;
}
