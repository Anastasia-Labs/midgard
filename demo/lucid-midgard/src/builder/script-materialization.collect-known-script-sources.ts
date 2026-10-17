import {
  EMPTY_CBOR_LIST,
  encodeMidgardFieldPreimageForField,
  encodeMidgardHash28Item,
  type ScriptLanguageName,
  sortMidgardMintItems,
} from "@al-ft/midgard-core/codec";

import { BuilderInvariantError } from "../core/errors.js";
import { outRefLabel } from "../core/out-ref.js";
import {
  decodeMidgardTxOutput,
  outputAddressPaymentScriptHash,
  utxoAddress,
  utxoOutputCbor,
} from "../core/output.js";
import type { MintIntent, ObserverIntent, Redeemer } from "../core/scripts.js";
import type { MidgardUtxo } from "../core/types.js";
import type { BuilderState } from "./context.js";
import {
  compareCanonicalStrings,
  type EffectiveMint,
  type KnownScriptSource,
  knownScriptSource,
  type RedeemerPointer,
} from "./script-materialization.known-script-source.js";
import {
  assertMetadataOnlyReferenceScriptMaterial,
  knownReferenceScriptSource,
  knownTrustedReferenceScriptMetadataSource,
} from "./script-materialization.prepare-proof-builder-state.js";
import { cloneRedeemer } from "./state.js";
import { encodeByteListPreimage } from "./unsigned-tx.js";

export const collectKnownScriptSources = (
  state: BuilderState,
): KnownScriptSource[] => {
  const inline = state.scripts.scripts.map((source, index) =>
    knownScriptSource(source, `inline:${index.toString()}`, true),
  );
  const metadataByOutRef = new Map(
    state.scripts.referenceScriptMetadata.map((metadata) => [
      outRefLabel(metadata),
      metadata,
    ]),
  );
  if (metadataByOutRef.size !== state.scripts.referenceScriptMetadata.length) {
    throw new BuilderInvariantError(
      "Duplicate trusted reference script metadata",
    );
  }
  const consumedMetadata = new Set<string>();
  const reference = state.referenceInputs.flatMap((input) => {
    const label = outRefLabel(input);
    const metadata = metadataByOutRef.get(label);
    const scriptRef = decodeMidgardTxOutput(utxoOutputCbor(input)).txOutput
      .scriptRef;
    if (metadata !== undefined) {
      consumedMetadata.add(label);
    }
    if (scriptRef === undefined || scriptRef === null) {
      assertMetadataOnlyReferenceScriptMaterial(metadata, `reference:${label}`);
      return metadata === undefined
        ? []
        : [
            knownTrustedReferenceScriptMetadataSource(
              metadata,
              `reference:${label}`,
            ),
          ];
    }
    return [
      knownReferenceScriptSource(scriptRef, `reference:${label}`, metadata),
    ];
  });
  for (const label of metadataByOutRef.keys()) {
    if (!consumedMetadata.has(label)) {
      throw new BuilderInvariantError(
        "Trusted reference script metadata has no matching reference input",
        label,
      );
    }
  }
  return [...inline, ...reference];
};

export const resolveKnownScript = (
  scriptHash: string,
  sources: readonly KnownScriptSource[],
):
  | {
      readonly language: "NativeCardano" | ScriptLanguageName;
      readonly source: KnownScriptSource;
    }
  | undefined => {
  let resolved:
    | {
        readonly language: "NativeCardano" | ScriptLanguageName;
        readonly source: KnownScriptSource;
      }
    | undefined;
  for (const source of sources) {
    for (const [language, hash] of source.hashes.entries()) {
      if (hash !== scriptHash) {
        continue;
      }
      if (resolved !== undefined) {
        throw new BuilderInvariantError(
          "Ambiguous script source resolution",
          scriptHash,
        );
      }
      resolved = { language, source };
    }
  }
  return resolved;
};

export const effectiveMints = (
  mints: readonly MintIntent[],
): readonly EffectiveMint[] => {
  const byPolicy = new Map<string, Map<string, bigint>>();
  const redeemers = new Map<string, Redeemer>();
  for (const mint of mints) {
    const policyId = mint.policyId;
    if (mint.redeemer !== undefined) {
      if (redeemers.has(policyId)) {
        throw new BuilderInvariantError(
          "Duplicate mint redeemer for policy",
          policyId,
        );
      }
      redeemers.set(policyId, cloneRedeemer(mint.redeemer));
    }
    const policyAssets = byPolicy.get(policyId) ?? new Map<string, bigint>();
    for (const [assetName, quantity] of Object.entries(mint.assets)) {
      const next = (policyAssets.get(assetName) ?? 0n) + quantity;
      if (next === 0n) {
        policyAssets.delete(assetName);
      } else {
        policyAssets.set(assetName, next);
      }
    }
    if (policyAssets.size === 0) {
      byPolicy.delete(policyId);
    } else {
      byPolicy.set(policyId, policyAssets);
    }
  }

  for (const policyId of redeemers.keys()) {
    if (!byPolicy.has(policyId)) {
      throw new BuilderInvariantError(
        "Mint redeemer has no effective mint policy",
        policyId,
      );
    }
  }

  return [...byPolicy.entries()]
    .sort(([a], [b]) => compareCanonicalStrings(a, b))
    .map(([policyId, assets]) => ({
      policyId,
      assets: Object.fromEntries(
        [...assets.entries()].sort(([a], [b]) => compareCanonicalStrings(a, b)),
      ),
      redeemer: redeemers.get(policyId),
    }));
};

/**
 * §5.6: field 5 is the **enveloped list of per-policy items** under the §5.1
 * grammar — `82 ‖ 58 1C policy_id ‖ map(k) ‖ asset entries` per item, and an
 * empty mint is exactly `80` like every other field. The retired raw-map
 * `encode_mint_preimage` form (`a0` when empty) is prohibited.
 *
 * `sortMidgardMintItems` puts the items into §5.6's canonical key order at both
 * levels; `encodeMidgardFieldPreimageForField` then *enforces* that order rather
 * than trusting it, so a builder cannot emit a preimage no decoder accepts.
 */
export const mintPreimageCbor = (mints: readonly EffectiveMint[]): Buffer =>
  encodeMidgardFieldPreimageForField({
    fieldIndex: 5,
    items: sortMidgardMintItems(
      mints.map(({ policyId, assets }) => ({
        policyId: Buffer.from(policyId, "hex"),
        assets: Object.entries(assets).map(([assetName, quantity]) => ({
          assetName: Buffer.from(assetName, "hex"),
          quantity,
        })),
      })),
    ),
  });

/**
 * §5.3 field 3 items: the raw 28-byte observer script hash, no interior CBOR.
 * Built with the §5.3 encoder so the width the on-chain stride-30 arithmetic
 * assumes is asserted by the producer rather than inherited from whatever
 * `normalizeScriptHash` happened to admit.
 */
export const requiredObserversPreimageCbor = (
  observers: readonly ObserverIntent[],
): Buffer =>
  observers.length === 0
    ? Buffer.from(EMPTY_CBOR_LIST)
    : encodeByteListPreimage(
        [...new Set(observers.map(({ scriptHash }) => scriptHash))]
          .sort()
          .map((hash) => encodeMidgardHash28Item(Buffer.from(hash, "hex"))),
      );

export const pointerKey = (pointer: RedeemerPointer): string =>
  `${pointer.tag.toString()}:${pointer.index.toString(10)}`;

export const redeemerIntentKey = (
  purpose: "spend" | "mint" | "observe" | "receive",
  id: string,
): string => `${purpose}:${id}`;

export const recordConsumedRedeemer = (
  consumed: Set<string>,
  key: string,
): void => {
  if (consumed.has(key)) {
    throw new BuilderInvariantError("Duplicate consumed redeemer intent", key);
  }
  consumed.add(key);
};

export const assertAllRedeemerIntentsConsumed = (
  state: BuilderState,
  effective: readonly EffectiveMint[],
  consumed: ReadonlySet<string>,
): void => {
  for (const intent of state.scripts.spendRedeemers) {
    if (intent.redeemer === undefined) {
      continue;
    }
    const key = redeemerIntentKey("spend", outRefLabel(intent));
    if (!consumed.has(key)) {
      throw new BuilderInvariantError("Unconsumed spend redeemer", key);
    }
  }
  for (const mint of effective) {
    if (mint.redeemer === undefined) {
      continue;
    }
    const key = redeemerIntentKey("mint", mint.policyId);
    if (!consumed.has(key)) {
      throw new BuilderInvariantError("Unconsumed mint redeemer", key);
    }
  }
  for (const observer of state.scripts.observers) {
    if (observer.redeemer === undefined) {
      continue;
    }
    const key = redeemerIntentKey("observe", observer.scriptHash);
    if (!consumed.has(key)) {
      throw new BuilderInvariantError("Unconsumed observer redeemer", key);
    }
  }
  for (const receive of state.scripts.receiveRedeemers) {
    const key = redeemerIntentKey("receive", receive.scriptHash);
    if (!consumed.has(key)) {
      throw new BuilderInvariantError("Unconsumed receive redeemer", key);
    }
  }
};

export const findSpendRedeemer = (
  state: BuilderState,
  input: MidgardUtxo,
): Redeemer | undefined => {
  const inputLabel = outRefLabel(input);
  return state.scripts.spendRedeemers.find(
    (intent) => outRefLabel(intent) === inputLabel,
  )?.redeemer;
};

export const findMintRedeemer = (
  mints: readonly EffectiveMint[],
  policyId: string,
): Redeemer | undefined =>
  mints.find((mint) => mint.policyId === policyId)?.redeemer;

export const findObserverRedeemer = (
  state: BuilderState,
  scriptHash: string,
): Redeemer | undefined =>
  state.scripts.observers.find(
    ({ scriptHash: candidate }) => candidate === scriptHash,
  )?.redeemer;

export const findReceiveRedeemer = (
  state: BuilderState,
  scriptHash: string,
): Redeemer | undefined =>
  state.scripts.receiveRedeemers.find(
    ({ scriptHash: candidate }) => candidate === scriptHash,
  )?.redeemer;

export const paymentScriptHashFromUtxo = (
  utxo: MidgardUtxo,
): string | undefined => outputAddressPaymentScriptHash(utxoAddress(utxo));
