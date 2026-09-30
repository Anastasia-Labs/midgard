import {
  type MidgardCekProgramMaterialEntry,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardNativeByteListPreimage,
  hashMidgardVersionedScript,
  MIDGARD_REDEEMER_PURPOSE_TAGS,
  type MidgardNativeTxFull,
  type MidgardVersionedScript,
  type ScriptLanguageName,
} from "@al-ft/midgard-core/codec";
import { hexToBytes, normalizeHex } from "@al-ft/midgard-core/hex";
import {
  collectMidgardAttachedProgramEnvelopes,
  collectMidgardReferencedProgramEnvelopes,
} from "@al-ft/midgard-core/script-proof";
import { CML } from "@lucid-evolution/lucid";

import { type Assets, normalizeAssets } from "../core/assets.js";
import { BuilderInvariantError } from "../core/errors.js";
import { normalizePlutusData, normalizeScriptRef } from "../core/output.js";
import type {
  Redeemer,
  ScriptLanguage,
  ScriptSource,
} from "../core/scripts.js";
import type { BuilderState } from "./context.js";
import { normalizeHashHex, normalizeNonNegativeBigInt } from "./normalizers.js";

export type KnownScriptSource = {
  readonly sourceId: string;
  readonly witnessScript?: MidgardVersionedScript;
  readonly hashes: ReadonlyMap<"NativeCardano" | ScriptLanguageName, string>;
  readonly inline: boolean;
};

export type EffectiveMint = {
  readonly policyId: string;
  readonly assets: Assets;
  readonly redeemer?: Redeemer;
};

export type RedeemerPointer = {
  readonly tag: number;
  readonly index: bigint;
};

export type DerivedRedeemer = {
  readonly pointer: RedeemerPointer;
  readonly redeemer: Redeemer;
};

/**
 * The four §5.3 `purpose_tag` values the Midgard builder emits, taken from the
 * spec's own table rather than re-derived from `CML.RedeemerTag`. §5.3 reuses
 * Cardano's numbering for 0–5, so the values are the same either way — but there
 * is one place the value set lives, and `Receive` (6) is Midgard-only and has no
 * CML spelling at all.
 *
 * The format's bound is the full seven-value set; this is deliberately the
 * narrower builder subset (§5.3 names both).
 */
export const RedeemerTags = {
  Spend: MIDGARD_REDEEMER_PURPOSE_TAGS.Spend,
  Mint: MIDGARD_REDEEMER_PURPOSE_TAGS.Mint,
  Reward: MIDGARD_REDEEMER_PURPOSE_TAGS.Reward,
  Receive: MIDGARD_REDEEMER_PURPOSE_TAGS.Receive,
} as const;

export const compareCanonicalStrings = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

const bytesFromBytesLike = (
  value: Uint8Array | string,
  fieldName: string,
): Buffer => {
  if (typeof value !== "string") {
    return Buffer.from(value);
  }
  try {
    return hexToBytes(value, { fieldName, allowEmpty: true });
  } catch {
    throw new BuilderInvariantError(`${fieldName} must be hex`, value);
  }
};

export const normalizeScriptHash = (
  hash: string,
  fieldName = "script hash",
): string => normalizeHashHex(hash, fieldName, 28);

export const normalizePolicyId = (policyId: string): string =>
  normalizeHashHex(policyId, "policy id", 28);

export const normalizeScriptLanguage = (
  language: unknown,
  fieldName: string,
): ScriptLanguage => {
  if (
    language === "NativeCardano" ||
    language === "PlutusV3" ||
    language === "MidgardV1"
  ) {
    return language;
  }
  throw new BuilderInvariantError(
    `${fieldName} must be NativeCardano, PlutusV3, or MidgardV1`,
    String(language),
  );
};

const normalizeMintAssetName = (policyId: string, unit: string): string => {
  const normalized = unit.trim().toLowerCase();
  if (normalized === "lovelace") {
    throw new BuilderInvariantError("Mint assets cannot include lovelace");
  }
  const assetName =
    normalized.length >= 56 && normalized.startsWith(policyId)
      ? normalized.slice(56)
      : normalized;
  try {
    return normalizeHex(assetName, {
      fieldName: "mint asset name",
      allowEmpty: true,
      trim: false,
    });
  } catch {
    throw new BuilderInvariantError("Mint asset names must be hex", unit);
  }
};

export const normalizeMintAssetsForNormalizedPolicy = (
  normalizedPolicyId: string,
  assets: Assets,
): Assets => {
  const normalized: Record<string, bigint> = {};
  for (const [unit, quantity] of Object.entries(normalizeAssets(assets))) {
    normalized[normalizeMintAssetName(normalizedPolicyId, unit)] = quantity;
  }
  if (Object.keys(normalized).length === 0) {
    throw new BuilderInvariantError("Mint assets must not be empty");
  }
  return normalized;
};

export const normalizeExUnits = (
  redeemer: Redeemer,
): { mem: bigint; steps: bigint } => {
  if (redeemer.exUnits === undefined) {
    throw new BuilderInvariantError("Redeemer exUnits are required");
  }
  return {
    mem: normalizeNonNegativeBigInt(
      redeemer.exUnits.mem,
      "redeemer.exUnits.mem",
    ),
    steps: normalizeNonNegativeBigInt(
      redeemer.exUnits.steps,
      "redeemer.exUnits.steps",
    ),
  };
};

export const redeemerDataBytes = (redeemer: Redeemer): Buffer =>
  Buffer.from(normalizePlutusData(redeemer.data).to_cbor_bytes());

const nativeScriptFromLike = (
  script: CML.NativeScript | Uint8Array | string,
): CML.NativeScript =>
  script instanceof CML.NativeScript
    ? script
    : CML.NativeScript.from_cbor_bytes(
        bytesFromBytesLike(script, "native script"),
      );

const knownNativeScriptSource = (
  script: CML.NativeScript | Uint8Array | string,
  sourceId: string,
  inline: boolean,
): KnownScriptSource => {
  const native = nativeScriptFromLike(script);
  const versioned = normalizeScriptRef(CML.Script.new_native(native));
  return {
    sourceId,
    inline,
    witnessScript: versioned,
    hashes: new Map([["NativeCardano", hashMidgardVersionedScript(versioned)]]),
  };
};

const knownPlutusScriptFromCml = (
  script: CML.Script,
  sourceId: string,
  inline: boolean,
): KnownScriptSource => {
  const native = script.as_native();
  if (native !== undefined) {
    return knownNativeScriptSource(native, sourceId, inline);
  }
  const plutusV3 = script.as_plutus_v3();
  if (plutusV3 === undefined) {
    throw new BuilderInvariantError(
      "Only native and PlutusV3 scripts are supported",
    );
  }
  const versioned = normalizeScriptRef(script);
  return {
    sourceId,
    inline,
    witnessScript: versioned,
    hashes: new Map([["PlutusV3", hashMidgardVersionedScript(versioned)]]),
  };
};

export const knownScriptSource = (
  source: ScriptSource,
  sourceId: string,
  inline: boolean,
): KnownScriptSource => {
  switch (source.kind) {
    case "native":
      return knownNativeScriptSource(source.script, sourceId, inline);
    case "plutus-v3": {
      if (source.script instanceof CML.Script) {
        const known = knownPlutusScriptFromCml(source.script, sourceId, inline);
        const plutusHash = known.hashes.get("PlutusV3");
        if (plutusHash === undefined) {
          throw new BuilderInvariantError(
            "PlutusV3 script source did not hash as PlutusV3",
          );
        }
        return {
          ...known,
          hashes: new Map([["PlutusV3", plutusHash]]),
        };
      }
      if (
        !(
          typeof source.script === "string" ||
          source.script instanceof Uint8Array
        )
      ) {
        throw new BuilderInvariantError(
          "PlutusV3 script source must be script bytes",
        );
      }
      const raw = bytesFromBytesLike(source.script, "PlutusV3 script");
      const versioned = {
        language: "PlutusV3" as const,
        scriptBytes: raw,
      };
      return {
        sourceId,
        inline,
        witnessScript: versioned,
        hashes: new Map([["PlutusV3", hashMidgardVersionedScript(versioned)]]),
      };
    }
    case "midgard-v1": {
      const raw = bytesFromBytesLike(source.script, "MidgardV1 script");
      const versioned = {
        language: "MidgardV1" as const,
        scriptBytes: raw,
      };
      return {
        sourceId,
        inline,
        witnessScript: versioned,
        hashes: new Map([["MidgardV1", hashMidgardVersionedScript(versioned)]]),
      };
    }
    case "dual-plutus-v3-midgard-v1":
      throw new BuilderInvariantError(
        "Dual PlutusV3/MidgardV1 script witnesses are not supported; attach explicit versioned scripts",
        sourceId,
      );
  }
};

export type PreparedProofBuilderState = {
  readonly state: BuilderState;
  readonly programMaterial: readonly MidgardCekProgramMaterialEntry[];
};

export const assertCompleteTxProgramMaterial = (
  tx: MidgardNativeTxFull,
  resolvedOutputsByOutRef: ReadonlyMap<string, Uint8Array> | undefined,
  programMaterial: readonly MidgardCekProgramMaterialEntry[],
): void => {
  try {
    const resolved = resolvedOutputsByOutRef ?? new Map<string, Uint8Array>();
    const referenceInputs = decodeMidgardNativeByteListPreimage(
      tx.body.referenceInputsPreimageCbor,
      "reference_inputs_preimage",
    );
    const expected = new Set(
      referenceInputs.map((outRef) => Buffer.from(outRef).toString("hex")),
    );
    for (const key of resolved.keys()) {
      if (!expected.has(key)) {
        throw new Error(
          `resolved reference output map contains unexpected outref ${key}`,
        );
      }
    }
    const envelopes = [
      ...collectMidgardAttachedProgramEnvelopes(tx),
      ...collectMidgardReferencedProgramEnvelopes(tx, resolved),
    ];
    verifyMidgardCekProgramMaterialBundle(envelopes, programMaterial);
  } catch (cause) {
    throw new BuilderInvariantError(
      "Incomplete or mismatched CEK program material",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};
