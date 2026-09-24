import { encodeCbor } from "@al-ft/midgard-core";
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
  type MidgardValue,
} from "@al-ft/midgard-core/codec";
import {
  type Credential,
  credentialToAddress,
  type Network,
} from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";

import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "./ledger-output-descriptor.js";
import { decodeMidgardOutRefBytes } from "./ledger-tx/codec.js";

export const MIDGARD_CANONICAL_TRANSITION_EFFECT_SCHEMA_VERSION =
  "midgard-canonical-transition-effect-v1" as const;

export type CanonicalTransitionEffectRawOperation =
  | Readonly<{ type: "delete"; outRefCbor: Buffer }>
  | Readonly<{
      type: "insert";
      outRefCbor: Buffer;
      outputCbor: Buffer;
    }>;

export type CanonicalTransitionEffect = Readonly<{
  schemaVersion: typeof MIDGARD_CANONICAL_TRANSITION_EFFECT_SCHEMA_VERSION;
  operations: readonly CanonicalTransitionEffectRawOperation[];
  canonicalCbor: Buffer;
  digest: string;
}>;

export type CanonicalDepositAddressCredential =
  | Readonly<{ PublicKeyCredential: readonly [string] }>
  | Readonly<{ ScriptCredential: readonly [string] }>;

export type CanonicalDepositAddressData = Readonly<{
  paymentCredential: CanonicalDepositAddressCredential;
  stakeCredential:
    | Readonly<{
        Inline: readonly [CanonicalDepositAddressCredential];
      }>
    | Readonly<{
        Pointer: readonly [
          Readonly<{
            slotNumber: bigint;
            transactionIndex: bigint;
            certificateIndex: bigint;
          }>,
        ];
      }>
    | null;
}>;

const lucidCredential = (
  value: CanonicalDepositAddressCredential,
): Credential =>
  "PublicKeyCredential" in value
    ? { type: "Key", hash: value.PublicKeyCredential[0] }
    : { type: "Script", hash: value.ScriptCredential[0] };

const depositNetwork = (
  configuredNetwork: Network,
  l2NetworkId: bigint,
): Network => {
  if (l2NetworkId === 1n) {
    return "Mainnet";
  }
  if (l2NetworkId === 0n) {
    return configuredNetwork === "Mainnet" ? "Preprod" : configuredNetwork;
  }
  throw new Error("unsupported committed deposit L2 network id");
};

const projectedOriginalDepositValue = (
  originalAssets: Readonly<Record<string, bigint>>,
): MidgardValue => {
  const policies = new Map<string, Map<string, bigint>>();
  for (const [unit, quantity] of Object.entries(originalAssets)) {
    if (unit === "lovelace") continue;
    if (!/^[0-9a-f]{56}([0-9a-f]{2}){0,32}$/u.test(unit) || quantity <= 0n)
      throw new Error("deposit contains an invalid projected asset");
    const policyId = unit.slice(0, 56);
    const assetName = unit.slice(56);
    const policy = policies.get(policyId) ?? new Map<string, bigint>();
    policy.set(assetName, quantity);
    policies.set(policyId, policy);
  }
  const lovelace = originalAssets.lovelace ?? 0n;
  if (lovelace < 0n) throw new Error("deposit contains negative lovelace");
  return Object.freeze({ lovelace, assets: policies });
};

/**
 * The out-ref in a transition effect is the ledger trie key, so its one valid
 * byte form is §5.3's fixed-index item (`82 ‖ 58 20 tx_id ‖ 19 index_be16`,
 * 38 bytes) — the same bytes on-chain `ledger_outref_key` derives. Re-encoding
 * and requiring byte equality is what makes that "exact", so the encoder here
 * must be the §5.3 one; `encodeCbor([txId, index])` would spell indices 0–23
 * minimally and reject every key the trie actually holds.
 */
const canonicalOutRefCbor = (value: Uint8Array): Buffer => {
  const source = Buffer.from(value);
  const decoded = decodeMidgardOutRefBytes(source);
  const canonical = encodeMidgardSpendInputItem({
    txId: decoded.txId,
    outputIndex: Number(decoded.index),
  });
  if (!canonical.equals(source)) {
    throw new Error(
      "transition effect out-ref must use the exact §5.3 fixed-index CBOR",
    );
  }
  return canonical;
};

const canonicalOperationCbor = (
  operation: CanonicalTransitionEffectRawOperation,
): readonly unknown[] =>
  operation.type === "delete"
    ? [0n, operation.outRefCbor]
    : [1n, operation.outRefCbor, operation.outputCbor];

/**
 * Constructs the byte-exact transition effect shared by the node producer and
 * independent replay consumers. The operation order is significant: it is the
 * producer's state-transition order and is committed by both CBOR and digest.
 */
export const buildCanonicalTransitionEffect = (
  operations: readonly CanonicalTransitionEffectRawOperation[],
): CanonicalTransitionEffect => {
  const seenOutRefs = new Set<string>();
  const canonicalOperations = operations.map((operation) => {
    const outRefCbor = canonicalOutRefCbor(operation.outRefCbor);
    const outRefHex = outRefCbor.toString("hex");
    if (seenOutRefs.has(outRefHex)) {
      throw new Error("transition effect contains a duplicate out-ref");
    }
    seenOutRefs.add(outRefHex);
    if (operation.type === "delete") {
      return Object.freeze({
        type: "delete" as const,
        outRefCbor,
      });
    }
    const outputCbor = Buffer.from(operation.outputCbor);
    buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: outRefCbor,
      outputCbor,
    });
    return Object.freeze({
      type: "insert" as const,
      outRefCbor,
      outputCbor,
    });
  });
  const canonicalCbor = encodeCbor([
    1n,
    canonicalOperations.map(canonicalOperationCbor),
  ]);
  return Object.freeze({
    schemaVersion: MIDGARD_CANONICAL_TRANSITION_EFFECT_SCHEMA_VERSION,
    operations: Object.freeze(canonicalOperations),
    canonicalCbor,
    digest: Buffer.from(sha256(canonicalCbor)).toString("hex"),
  });
};

export const canonicalTransitionEffectFromStatePatch = (patch: {
  readonly deletedOutRefs: readonly string[];
  readonly upsertedOutRefs: readonly (readonly [string, Buffer])[];
}): CanonicalTransitionEffect =>
  buildCanonicalTransitionEffect([
    ...patch.deletedOutRefs.map((outRefHex) => ({
      type: "delete" as const,
      outRefCbor: Buffer.from(outRefHex, "hex"),
    })),
    ...patch.upsertedOutRefs.map(([outRefHex, outputCbor]) => ({
      type: "insert" as const,
      outRefCbor: Buffer.from(outRefHex, "hex"),
      outputCbor: Buffer.from(outputCbor),
    })),
  ]);

export const canonicalDepositTransitionEffect = (entry: {
  readonly outRefCbor: Uint8Array;
  readonly outputCbor: Uint8Array;
}): CanonicalTransitionEffect =>
  buildCanonicalTransitionEffect([
    {
      type: "insert",
      outRefCbor: Buffer.from(entry.outRefCbor),
      outputCbor: Buffer.from(entry.outputCbor),
    },
  ]);

/** Projection of authenticated original funds, excluding list and storage funds. */
export const deriveCanonicalOriginalDepositTransitionEffect = (input: {
  readonly configuredNetwork: Network;
  readonly eventId: Readonly<{
    transactionId: string;
    outputIndex: bigint;
  }>;
  readonly l2NetworkId: bigint;
  readonly l2Address: CanonicalDepositAddressData;
  readonly l2DatumCbor: Uint8Array | null;
  readonly originalAssets: Readonly<Record<string, bigint>>;
}): CanonicalTransitionEffect => {
  const network = depositNetwork(input.configuredNetwork, input.l2NetworkId);
  const stakeCredential =
    input.l2Address.stakeCredential === null
      ? undefined
      : "Inline" in input.l2Address.stakeCredential
        ? lucidCredential(input.l2Address.stakeCredential.Inline[0])
        : undefined;
  const addressText = credentialToAddress(
    network,
    lucidCredential(input.l2Address.paymentCredential),
    stakeCredential,
  );
  const outputCbor = encodeMidgardTxOutput({
    address: midgardAddressFromText(addressText),
    value: projectedOriginalDepositValue(input.originalAssets),
    ...(input.l2DatumCbor === null
      ? {}
      : {
          datum: {
            kind: "inline" as const,
            cbor: Buffer.from(input.l2DatumCbor),
          },
        }),
  });
  const outRefCbor = encodeMidgardSpendInputItem({
    txId: Buffer.from(input.eventId.transactionId, "hex"),
    outputIndex: Number(input.eventId.outputIndex),
  });
  return canonicalDepositTransitionEffect({ outRefCbor, outputCbor });
};

/** Legacy ingestion projection; migrate callers to authenticated original funds. */
export const deriveCanonicalDepositTransitionEffect = (
  input: Omit<
    Parameters<typeof deriveCanonicalOriginalDepositTransitionEffect>[0],
    "originalAssets"
  > & {
    readonly l1Assets: Readonly<Record<string, bigint>>;
    readonly depositPolicyId: string;
    readonly depositAssetNameHex: string;
  },
): CanonicalTransitionEffect => {
  const unit = input.depositPolicyId + input.depositAssetNameHex;
  if (input.l1Assets[unit] !== 1n)
    throw new Error("deposit authentication NFT quantity must equal one");
  const originalAssets = { ...input.l1Assets };
  delete originalAssets[unit];
  if (originalAssets.lovelace === undefined && originalAssets[""] === undefined)
    throw new Error("deposit is missing a valid lovelace quantity");
  if (originalAssets[""] !== undefined) {
    originalAssets.lovelace ??= originalAssets[""];
    delete originalAssets[""];
  }
  return deriveCanonicalOriginalDepositTransitionEffect({
    ...input,
    originalAssets,
  });
};

export const canonicalCommittedWithdrawalTransitionEffect = (input: {
  readonly committedValid: boolean;
  readonly outRefCbor: Uint8Array;
}): CanonicalTransitionEffect =>
  buildCanonicalTransitionEffect(
    input.committedValid
      ? [
          {
            type: "delete",
            outRefCbor: Buffer.from(input.outRefCbor),
          },
        ]
      : [],
  );
