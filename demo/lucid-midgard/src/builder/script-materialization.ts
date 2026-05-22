import {
  computeHash32,
  computeScriptIntegrityHashForLanguages,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardVersionedScript,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
  type MidgardVersionedScript,
  type ScriptLanguageName,
} from "@al-ft/midgard-core/codec";
import { hexToBytes, normalizeHex } from "@al-ft/midgard-core/hex";
import { CML } from "@lucid-evolution/lucid";

import { type Assets, normalizeAssets } from "../core/assets.js";
import { BuilderInvariantError } from "../core/errors.js";
import { compareOutRefs, outRefLabel } from "../core/out-ref.js";
import {
  decodeMidgardTxOutput,
  normalizePlutusData,
  normalizeScriptRef,
  outputAddressPaymentScriptHash,
  outputAddressProtected,
  utxoAddress,
  utxoOutputCbor,
} from "../core/output.js";
import type {
  MintIntent,
  ObserverIntent,
  Redeemer,
  ScriptLanguage,
  ScriptSource,
  TrustedReferenceScriptMetadata,
} from "../core/scripts.js";
import type { MidgardScript, MidgardUtxo } from "../core/types.js";
import { mintDeltaAssets } from "./balancing.js";
import type { BuilderState } from "./context.js";
import { normalizeHashHex, normalizeNonNegativeBigInt } from "./normalizers.js";
import { cloneRedeemer } from "./state.js";
import {
  encodeByteListPreimage,
  type ScriptMaterialization,
} from "./unsigned-tx.js";

type KnownScriptSource = {
  readonly sourceId: string;
  readonly witnessScript?: MidgardVersionedScript;
  readonly hashes: ReadonlyMap<"NativeCardano" | ScriptLanguageName, string>;
  readonly inline: boolean;
};

type EffectiveMint = {
  readonly policyId: string;
  readonly assets: Assets;
  readonly redeemer?: Redeemer;
};

type RedeemerPointer = {
  readonly tag: number;
  readonly index: bigint;
};

type DerivedRedeemer = {
  readonly pointer: RedeemerPointer;
  readonly redeemer: Redeemer;
};

const RedeemerTags = {
  Spend: Number(CML.RedeemerTag.Spend),
  Mint: Number(CML.RedeemerTag.Mint),
  Reward: Number(CML.RedeemerTag.Reward),
  Receive: 6,
} as const;

const compareCanonicalStrings = (left: string, right: string): number =>
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

const normalizeExUnits = (
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

const redeemerDataBytes = (redeemer: Redeemer): Buffer =>
  Buffer.from(normalizePlutusData(redeemer.data).to_cbor_bytes());

const nativeScriptFromLike = (
  script: InstanceType<typeof CML.NativeScript> | Uint8Array | string,
): InstanceType<typeof CML.NativeScript> =>
  script instanceof CML.NativeScript
    ? script
    : CML.NativeScript.from_cbor_bytes(
        bytesFromBytesLike(script, "native script"),
      );

const knownNativeScriptSource = (
  script: InstanceType<typeof CML.NativeScript> | Uint8Array | string,
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
  script: InstanceType<typeof CML.Script>,
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

const knownScriptSource = (
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

const knownReferenceScriptSource = (
  script: MidgardScript,
  sourceId: string,
  metadata?: TrustedReferenceScriptMetadata,
): KnownScriptSource => {
  const versioned = normalizeScriptRef(script);
  const localLanguage = versioned.language;
  const localHash = hashMidgardVersionedScript(versioned);
  if (metadata !== undefined && metadata.language !== localLanguage) {
    throw new BuilderInvariantError(
      "Reference script metadata language does not match local script reference",
      `${sourceId} ${metadata.language}`,
    );
  }
  if (metadata !== undefined && localHash !== metadata.scriptHash) {
    throw new BuilderInvariantError(
      "Reference script metadata hash does not match local script reference",
      `${sourceId} ${metadata.scriptHash}`,
    );
  }
  if (metadata?.scriptCborHash !== undefined) {
    const localScriptCborHash = computeHash32(
      encodeMidgardVersionedScript(versioned),
    ).toString("hex");
    if (localScriptCborHash !== metadata.scriptCborHash) {
      throw new BuilderInvariantError(
        "Reference script metadata scriptCborHash does not match local script reference",
        `${sourceId} ${metadata.scriptCborHash}`,
      );
    }
  }
  return {
    sourceId,
    inline: false,
    witnessScript: undefined,
    hashes: new Map([[localLanguage, localHash]]),
  };
};

const knownTrustedReferenceScriptMetadataSource = (
  metadata: TrustedReferenceScriptMetadata,
  sourceId: string,
): KnownScriptSource => ({
  sourceId,
  inline: false,
  witnessScript: undefined,
  hashes: new Map([[metadata.language, metadata.scriptHash]]),
});

const collectKnownScriptSources = (
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

const resolveKnownScript = (
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

const effectiveMints = (
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

const mintPreimageCbor = (mints: readonly EffectiveMint[]): Buffer => {
  if (mints.length === 0) {
    return Buffer.from(EMPTY_CBOR_LIST);
  }
  const cborMap = new Map<Buffer, Map<Buffer, bigint>>();
  for (const { policyId, assets } of mints) {
    const assetMap = new Map<Buffer, bigint>();
    for (const [assetName, quantity] of Object.entries(assets)) {
      assetMap.set(Buffer.from(assetName, "hex"), quantity);
    }
    cborMap.set(Buffer.from(policyId, "hex"), assetMap);
  }
  return encodeCbor(cborMap);
};

const requiredObserversPreimageCbor = (
  observers: readonly ObserverIntent[],
): Buffer =>
  observers.length === 0
    ? Buffer.from(EMPTY_CBOR_LIST)
    : encodeByteListPreimage(
        [...new Set(observers.map(({ scriptHash }) => scriptHash))]
          .sort()
          .map((hash) => Buffer.from(hash, "hex")),
      );

const pointerKey = (pointer: RedeemerPointer): string =>
  `${pointer.tag.toString()}:${pointer.index.toString(10)}`;

const redeemerIntentKey = (
  purpose: "spend" | "mint" | "observe" | "receive",
  id: string,
): string => `${purpose}:${id}`;

const recordConsumedRedeemer = (consumed: Set<string>, key: string): void => {
  if (consumed.has(key)) {
    throw new BuilderInvariantError("Duplicate consumed redeemer intent", key);
  }
  consumed.add(key);
};

const assertAllRedeemerIntentsConsumed = (
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

const findSpendRedeemer = (
  state: BuilderState,
  input: MidgardUtxo,
): Redeemer | undefined => {
  const inputLabel = outRefLabel(input);
  return state.scripts.spendRedeemers.find(
    (intent) => outRefLabel(intent) === inputLabel,
  )?.redeemer;
};

const findMintRedeemer = (
  mints: readonly EffectiveMint[],
  policyId: string,
): Redeemer | undefined =>
  mints.find((mint) => mint.policyId === policyId)?.redeemer;

const findObserverRedeemer = (
  state: BuilderState,
  scriptHash: string,
): Redeemer | undefined =>
  state.scripts.observers.find(
    ({ scriptHash: candidate }) => candidate === scriptHash,
  )?.redeemer;

const findReceiveRedeemer = (
  state: BuilderState,
  scriptHash: string,
): Redeemer | undefined =>
  state.scripts.receiveRedeemers.find(
    ({ scriptHash: candidate }) => candidate === scriptHash,
  )?.redeemer;

const paymentScriptHashFromUtxo = (utxo: MidgardUtxo): string | undefined =>
  outputAddressPaymentScriptHash(utxoAddress(utxo));

const addRequiredExecution = ({
  scriptHash,
  purpose,
  pointer,
  redeemer,
  sources,
  redeemers,
  usedSources,
  languages,
}: {
  readonly scriptHash: string;
  readonly purpose: string;
  readonly pointer: RedeemerPointer;
  readonly redeemer: Redeemer | undefined;
  readonly sources: readonly KnownScriptSource[];
  readonly redeemers: DerivedRedeemer[];
  readonly usedSources: Set<string>;
  readonly languages: Set<ScriptLanguageName>;
}): void => {
  const resolved = resolveKnownScript(scriptHash, sources);
  if (resolved === undefined) {
    throw new BuilderInvariantError(
      `Missing script source for ${purpose}`,
      scriptHash,
    );
  }
  usedSources.add(resolved.source.sourceId);
  if (resolved.language === "NativeCardano") {
    if (redeemer !== undefined) {
      throw new BuilderInvariantError(
        `Native script ${purpose} cannot have a redeemer`,
        scriptHash,
      );
    }
    return;
  }
  if (purpose === "receive" && resolved.language === "PlutusV3") {
    throw new BuilderInvariantError(
      "PlutusV3 receive scripts are not supported",
    );
  }
  if (redeemer === undefined) {
    throw new BuilderInvariantError(
      `Missing redeemer for ${purpose}`,
      scriptHash,
    );
  }
  languages.add(resolved.language);
  redeemers.push({ pointer, redeemer });
};

const encodeRedeemers = (redeemers: readonly DerivedRedeemer[]): Buffer => {
  if (redeemers.length === 0) {
    return Buffer.from(EMPTY_CBOR_LIST);
  }
  const seen = new Set<string>();
  const entries = [...redeemers].sort((left, right) => {
    if (left.pointer.tag !== right.pointer.tag) {
      return left.pointer.tag - right.pointer.tag;
    }
    return left.pointer.index < right.pointer.index
      ? -1
      : left.pointer.index > right.pointer.index
        ? 1
        : 0;
  });
  const encoded: [bigint, bigint, Buffer, readonly [bigint, bigint]][] = [];
  for (const entry of entries) {
    const key = pointerKey(entry.pointer);
    if (seen.has(key)) {
      throw new BuilderInvariantError("Duplicate redeemer pointer", key);
    }
    seen.add(key);
    const exUnits = normalizeExUnits(entry.redeemer);
    encoded.push([
      BigInt(entry.pointer.tag),
      entry.pointer.index,
      redeemerDataBytes(entry.redeemer),
      [exUnits.mem, exUnits.steps],
    ]);
  }
  return encodeCbor(encoded);
};

export const deriveScriptMaterialization = (
  state: BuilderState,
): ScriptMaterialization => {
  if (state.scripts.datumWitnesses.length > 0) {
    throw new BuilderInvariantError(
      "Datum witnesses are not supported by Midgard native transactions; use inline datums",
    );
  }
  const sources = collectKnownScriptSources(state);
  const usedSources = new Set<string>();
  const languages = new Set<ScriptLanguageName>();
  const redeemers: DerivedRedeemer[] = [];
  const consumedRedeemers = new Set<string>();
  const effective = effectiveMints(state.scripts.mints);

  const spent = [...state.spendInputs].sort(compareOutRefs);
  for (let index = 0; index < spent.length; index += 1) {
    const input = spent[index]!;
    const scriptHash = paymentScriptHashFromUtxo(input);
    if (scriptHash === undefined) {
      continue;
    }
    const redeemer = findSpendRedeemer(state, input);
    addRequiredExecution({
      scriptHash,
      purpose: "spend",
      pointer: { tag: RedeemerTags.Spend, index: BigInt(index) },
      redeemer,
      sources,
      redeemers,
      usedSources,
      languages,
    });
    if (redeemer !== undefined) {
      recordConsumedRedeemer(
        consumedRedeemers,
        redeemerIntentKey("spend", outRefLabel(input)),
      );
    }
  }

  const policyIds = effective.map((mint) => mint.policyId);
  for (let index = 0; index < policyIds.length; index += 1) {
    const policyId = policyIds[index]!;
    const redeemer = findMintRedeemer(effective, policyId);
    addRequiredExecution({
      scriptHash: policyId,
      purpose: "mint",
      pointer: { tag: RedeemerTags.Mint, index: BigInt(index) },
      redeemer,
      sources,
      redeemers,
      usedSources,
      languages,
    });
    if (redeemer !== undefined) {
      recordConsumedRedeemer(
        consumedRedeemers,
        redeemerIntentKey("mint", policyId),
      );
    }
  }

  const observers = [
    ...new Set(state.scripts.observers.map(({ scriptHash }) => scriptHash)),
  ].sort();
  for (let index = 0; index < observers.length; index += 1) {
    const observer = observers[index]!;
    const redeemer = findObserverRedeemer(state, observer);
    addRequiredExecution({
      scriptHash: observer,
      purpose: "observe",
      pointer: { tag: RedeemerTags.Reward, index: BigInt(index) },
      redeemer,
      sources,
      redeemers,
      usedSources,
      languages,
    });
    if (redeemer !== undefined) {
      recordConsumedRedeemer(
        consumedRedeemers,
        redeemerIntentKey("observe", observer),
      );
    }
  }

  const receivingHashes = [
    ...new Set(
      state.outputs.flatMap((output) => {
        if (!outputAddressProtected(output.address)) {
          return [];
        }
        const scriptHash = outputAddressPaymentScriptHash(output.address);
        return scriptHash === undefined ? [] : [scriptHash];
      }),
    ),
  ].sort();
  for (let index = 0; index < receivingHashes.length; index += 1) {
    const scriptHash = receivingHashes[index]!;
    const redeemer = findReceiveRedeemer(state, scriptHash);
    addRequiredExecution({
      scriptHash,
      purpose: "receive",
      pointer: { tag: RedeemerTags.Receive, index: BigInt(index) },
      redeemer,
      sources,
      redeemers,
      usedSources,
      languages,
    });
    if (redeemer !== undefined) {
      recordConsumedRedeemer(
        consumedRedeemers,
        redeemerIntentKey("receive", scriptHash),
      );
    }
  }

  assertAllRedeemerIntentsConsumed(state, effective, consumedRedeemers);

  for (const source of sources) {
    if (source.inline && !usedSources.has(source.sourceId)) {
      throw new BuilderInvariantError(
        "Extraneous script witness",
        source.sourceId,
      );
    }
  }

  const redeemerTxWitsPreimageCbor = encodeRedeemers(redeemers);
  const redeemerTxWitsHash = computeHash32(redeemerTxWitsPreimageCbor);
  const requiredLanguages = [...languages].sort();
  return {
    requiredObserversPreimageCbor: requiredObserversPreimageCbor(
      state.scripts.observers,
    ),
    mintPreimageCbor: mintPreimageCbor(effective),
    scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage(
      sources
        .filter((source) => source.inline)
        .map((known) => {
          if (known.witnessScript === undefined) {
            throw new BuilderInvariantError(
              "Inline script source missing witness bytes",
            );
          }
          return known.witnessScript;
        }),
    ),
    redeemerTxWitsPreimageCbor,
    scriptIntegrityHash:
      requiredLanguages.length === 0
        ? Buffer.from(EMPTY_NULL_ROOT)
        : computeScriptIntegrityHashForLanguages(
            redeemerTxWitsHash,
            requiredLanguages,
          ),
    mintDelta: mintDeltaAssets(effective),
  };
};
