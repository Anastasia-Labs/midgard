import {
  decodeMidgardNativeScript,
  isProtectedMidgardAddress,
  midgardAddressFromText,
  type MidgardDatum as CoreMidgardDatum,
  type MidgardTxOutput as CoreMidgardTxOutput,
  type MidgardValue as CoreMidgardValue,
  type MidgardVersionedScript,
  protectMidgardAddress,
  validateCanonicalPlutusDataCbor,
} from "@al-ft/midgard-core/codec";
import { hexToBytes } from "@al-ft/midgard-core/hex";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { CML } from "@lucid-evolution/lucid";

import {
  assertNonNegativeAssets,
  type Assets,
  normalizeAssets,
  normalizeValueLike,
  type ValueLike,
} from "./assets.js";
import { BuilderInvariantError } from "./errors.js";
import type {
  Address,
  MidgardDatum,
  MidgardScript,
  MidgardTxOutput,
} from "./types.js";

export type PlutusDataLike = CML.PlutusData | Uint8Array | string;

export type ScriptRefLike = CML.Script | Uint8Array | string | MidgardScript;

export type OutputDatum =
  | { readonly kind: "none" }
  | { readonly kind: "inline"; readonly data: PlutusDataLike }
  | { readonly kind: "hash"; readonly hash: string };

export type OutputKind = "ordinary" | "protected";

export type OutputOptions = {
  readonly kind?: OutputKind;
  readonly datum?: OutputDatum | PlutusDataLike;
  readonly scriptRef?: ScriptRefLike;
};

export type AuthoredOutput = {
  readonly kind: OutputKind;
  readonly address: Address;
  readonly assets: Assets;
  readonly datum?: OutputDatum;
  readonly scriptRef?: ScriptRefLike;
};

export type DecodedMidgardOutput = {
  readonly outputCbor: Buffer;
  readonly address: Address;
  readonly assets: Assets;
  readonly txOutput: MidgardTxOutput;
};

const isCmlPlutusDataLike = (
  data: unknown,
): data is { readonly to_cbor_bytes: () => Uint8Array } =>
  typeof data === "object" &&
  data !== null &&
  typeof (data as { readonly as_integer?: unknown }).as_integer ===
    "function" &&
  typeof (data as { readonly to_cbor_bytes?: unknown }).to_cbor_bytes ===
    "function";

const isCmlScriptLike = (script: unknown): script is CML.Script =>
  typeof script === "object" &&
  script !== null &&
  typeof (script as { readonly as_native?: unknown }).as_native ===
    "function" &&
  typeof (script as { readonly as_plutus_v3?: unknown }).as_plutus_v3 ===
    "function" &&
  typeof (script as { readonly to_cbor_bytes?: unknown }).to_cbor_bytes ===
    "function";

const isOutputDatumOption = (datum: unknown): datum is OutputDatum => {
  if (
    typeof datum !== "object" ||
    datum === null ||
    datum instanceof Uint8Array
  ) {
    return false;
  }
  const kind = (datum as { readonly kind?: unknown }).kind;
  return kind === "none" || kind === "inline" || kind === "hash";
};

const fromHex = (hex: string, fieldName: string): Buffer => {
  try {
    return hexToBytes(hex, { fieldName, allowEmpty: true });
  } catch {
    throw new BuilderInvariantError(`${fieldName} must be hex`, hex);
  }
};

export const addressBytesForOutput = (
  address: Address | CML.Address,
  kind: OutputKind = "ordinary",
): Buffer => {
  const bytes =
    typeof address === "string"
      ? midgardAddressFromText(address)
      : Buffer.from(address.to_raw_bytes());
  return kind === "protected" ? protectMidgardAddress(bytes) : bytes;
};

export const outputKindFromAddress = (address: Address): OutputKind =>
  isProtectedMidgardAddress(midgardAddressFromText(address))
    ? "protected"
    : "ordinary";

export const normalizePlutusData = (data: PlutusDataLike): CML.PlutusData => {
  if (data instanceof CML.PlutusData) {
    return data;
  }
  if (isCmlPlutusDataLike(data)) {
    return CML.PlutusData.from_cbor_bytes(data.to_cbor_bytes());
  }
  const bytes = typeof data === "string" ? fromHex(data, "datum") : data;
  return CML.PlutusData.from_cbor_bytes(bytes);
};

/**
 * Redeemer data exactly as committed: the bytes `serialiseData` emits for its
 * value (§6.2), the only spelling phase A admits. A `CML.PlutusData` is a
 * value, so it is serialised in that form here — CML's own encoder spells
 * lists and constructor fields definite-length, which is not it. Bytes or hex
 * are the caller's own encoding: they pass through unchanged when already in
 * that form and are refused, never rewritten, when they are not.
 */
export const redeemerDataCbor = (data: PlutusDataLike): Buffer => {
  if (typeof data !== "string" && !(data instanceof Uint8Array)) {
    try {
      return Buffer.from(
        aikenSerialisedPlutusDataCborPreservingMapOrder(
          Buffer.from(data.to_cbor_bytes()).toString("hex"),
        ),
        "hex",
      );
    } catch (error) {
      throw new BuilderInvariantError(
        "Redeemer data must be supported Plutus Data",
        String(error),
      );
    }
  }
  const bytes =
    typeof data === "string" ? fromHex(data, "redeemer.data") : data;
  try {
    return validateCanonicalPlutusDataCbor(bytes, "redeemer.data");
  } catch (error) {
    throw new BuilderInvariantError(
      "Redeemer data bytes must be the serialiseData encoding of their value; pass a CML.PlutusData to have the builder encode it",
      error instanceof Error ? error.message : String(error),
    );
  }
};

const isMidgardScript = (script: ScriptRefLike): script is MidgardScript =>
  typeof script === "object" &&
  script !== null &&
  !(script instanceof Uint8Array) &&
  !(script instanceof CML.Script) &&
  "type" in script &&
  "script" in script;

const cmlScriptToMidgardVersionedScript = (
  script: CML.Script,
): MidgardVersionedScript => {
  const native = script.as_native();
  if (native !== undefined) {
    const decoded = decodeMidgardNativeScript(native.to_cbor_bytes());
    return {
      language: "NativeCardano",
      scriptBytes: decoded.cbor,
      nativeScript: decoded.script,
    };
  }
  if (
    script.as_plutus_v1() !== undefined ||
    script.as_plutus_v2() !== undefined
  ) {
    throw new BuilderInvariantError(
      "Midgard script references do not support PlutusV1 or PlutusV2",
    );
  }
  const plutusV3 = script.as_plutus_v3();
  if (plutusV3 === undefined) {
    throw new BuilderInvariantError("Unsupported Cardano script reference");
  }
  return {
    language: "PlutusV3",
    scriptBytes: Buffer.from(plutusV3.to_raw_bytes()),
  };
};

const midgardScriptToVersionedScript = (
  scriptRef: MidgardScript,
): MidgardVersionedScript => {
  const bytes = fromHex(scriptRef.script, "scriptRef.script");
  switch (scriptRef.type) {
    case "Native": {
      const decoded = decodeMidgardNativeScript(bytes);
      return {
        language: "NativeCardano",
        scriptBytes: decoded.cbor,
        nativeScript: decoded.script,
      };
    }
    case "PlutusV3":
    case "MidgardV1":
      return { language: scriptRef.type, scriptBytes: bytes };
  }
};

export const normalizeScriptRef = (
  scriptRef: ScriptRefLike,
): MidgardVersionedScript => {
  if (scriptRef instanceof CML.Script || isCmlScriptLike(scriptRef)) {
    return cmlScriptToMidgardVersionedScript(scriptRef);
  }
  if (isMidgardScript(scriptRef)) {
    return midgardScriptToVersionedScript(scriptRef);
  }
  const bytes =
    typeof scriptRef === "string" ? fromHex(scriptRef, "scriptRef") : scriptRef;
  return cmlScriptToMidgardVersionedScript(CML.Script.from_cbor_bytes(bytes));
};

export const midgardScriptFromCore = (
  script: MidgardVersionedScript,
): MidgardScript => {
  switch (script.language) {
    case "NativeCardano":
      return {
        type: "Native",
        script: Buffer.from(script.scriptBytes).toString("hex"),
      };
    case "PlutusV3":
    case "MidgardV1":
      return {
        type: script.language,
        script: Buffer.from(script.scriptBytes).toString("hex"),
      };
  }
};

export const normalizeOutputDatum = (
  datum: OutputOptions["datum"],
): OutputDatum | undefined => {
  if (datum === undefined) {
    return undefined;
  }
  if (isOutputDatumOption(datum)) {
    return datum;
  }
  return { kind: "inline", data: datum };
};

export const authoredDatumToCore = (
  datum: OutputOptions["datum"],
): CoreMidgardDatum | undefined => {
  const normalized = normalizeOutputDatum(datum);
  if (normalized === undefined || normalized.kind === "none") {
    return undefined;
  }
  if (normalized.kind === "hash") {
    throw new BuilderInvariantError(
      "Midgard outputs must not use datum hashes",
    );
  }
  return {
    kind: "inline",
    cbor: Buffer.from(normalizePlutusData(normalized.data).to_cbor_bytes()),
  };
};

const publicDatumToCore = (
  datum: MidgardTxOutput["datum"],
): CoreMidgardDatum | undefined => {
  if (datum === undefined || datum === null) {
    return undefined;
  }
  return {
    kind: "inline",
    cbor: fromHex(datum.cbor, "output.datum.cbor"),
  };
};

export const publicDatumFromCore = (
  datum: CoreMidgardDatum | undefined,
): MidgardDatum | undefined =>
  datum === undefined
    ? undefined
    : { kind: "inline", cbor: Buffer.from(datum.cbor).toString("hex") };

export const assetsToMidgardValue = (value: ValueLike): CoreMidgardValue => {
  const normalized = assertNonNegativeAssets(
    normalizeValueLike(value),
    "output.assets",
  );
  const assets = new Map<string, Map<string, bigint>>();
  const lovelace = normalized.lovelace ?? 0n;
  for (const [unit, quantity] of Object.entries(normalized)) {
    if (unit === "lovelace") {
      continue;
    }
    const unitBytes = fromHex(unit, "asset unit");
    if (unitBytes.length < 28) {
      throw new BuilderInvariantError(
        "Asset unit must include a 28-byte policy id",
        unit,
      );
    }
    const policyId = unit.slice(0, 56);
    const assetName = unit.slice(56);
    if (unitBytes.length - 28 > 32) {
      throw new BuilderInvariantError(
        "Asset name must be at most 32 bytes",
        unit,
      );
    }
    const policyAssets = assets.get(policyId) ?? new Map<string, bigint>();
    policyAssets.set(assetName, quantity);
    assets.set(policyId, policyAssets);
  }
  return { lovelace, assets };
};

export const midgardValueToAssets = (value: CoreMidgardValue): Assets => {
  const result: Record<string, bigint> = {};
  if (value.lovelace !== 0n) {
    result.lovelace = value.lovelace;
  }
  for (const [policyId, assets] of value.assets.entries()) {
    for (const [assetName, quantity] of assets.entries()) {
      result[`${policyId}${assetName}`] = quantity;
    }
  }
  return normalizeAssets(result);
};

export const publicOutputToCore = (
  output: MidgardTxOutput,
): CoreMidgardTxOutput => ({
  address: midgardAddressFromText(output.address),
  value: assetsToMidgardValue(output.assets),
  ...(output.datum === undefined || output.datum === null
    ? {}
    : { datum: publicDatumToCore(output.datum) }),
  ...(output.scriptRef === undefined || output.scriptRef === null
    ? {}
    : { script_ref: normalizeScriptRef(output.scriptRef) }),
});
