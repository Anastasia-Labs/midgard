import { encodeCbor } from "@al-ft/midgard-core";
import {
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

import { buildCanonicalMidgardLedgerEntryOutputMaterialV1 } from "./ledger-output-descriptor.js";
import { decodeMidgardOutRefBytes } from "./ledger-tx/codec.js";

export const MIDGARD_CANONICAL_TRANSITION_EFFECT_V1_SCHEMA_VERSION =
  "midgard-canonical-transition-effect-v1" as const;

export type CanonicalTransitionEffectRawOperationV1 =
  | Readonly<{ type: "delete"; outRefCbor: Buffer }>
  | Readonly<{
      type: "insert";
      outRefCbor: Buffer;
      outputCbor: Buffer;
    }>;

export type CanonicalTransitionEffectV1 = Readonly<{
  schemaVersion: typeof MIDGARD_CANONICAL_TRANSITION_EFFECT_V1_SCHEMA_VERSION;
  operations: readonly CanonicalTransitionEffectRawOperationV1[];
  canonicalCbor: Buffer;
  digest: string;
}>;

export type CanonicalDepositAddressCredentialV1 =
  | Readonly<{ PublicKeyCredential: readonly [string] }>
  | Readonly<{ ScriptCredential: readonly [string] }>;

export type CanonicalDepositAddressDataV1 = Readonly<{
  paymentCredential: CanonicalDepositAddressCredentialV1;
  stakeCredential:
    | Readonly<{
        Inline: readonly [CanonicalDepositAddressCredentialV1];
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
  value: CanonicalDepositAddressCredentialV1,
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

const projectedDepositValue = (input: {
  readonly l1Assets: Readonly<Record<string, bigint>>;
  readonly depositPolicyId: string;
  readonly depositAssetNameHex: string;
}): MidgardValue => {
  const authenticationUnit = `${input.depositPolicyId}${input.depositAssetNameHex}`;
  if (input.l1Assets[authenticationUnit] !== 1n) {
    throw new Error("deposit authentication NFT quantity must equal one");
  }
  const policies = new Map<string, Map<string, bigint>>();
  for (const [unit, quantity] of Object.entries(input.l1Assets)) {
    if (unit === "lovelace" || unit === "" || unit === authenticationUnit) {
      continue;
    }
    if (unit.length < 56 || quantity <= 0n) {
      throw new Error("deposit contains an invalid projected asset");
    }
    const policyId = unit.slice(0, 56);
    const assetName = unit.slice(56);
    const policy = policies.get(policyId) ?? new Map<string, bigint>();
    if (policy.has(assetName)) {
      throw new Error("deposit contains a duplicate projected asset");
    }
    policy.set(assetName, quantity);
    policies.set(policyId, policy);
  }
  const lovelace = input.l1Assets.lovelace ?? input.l1Assets[""];
  if (lovelace === undefined || lovelace < 0n) {
    throw new Error("deposit is missing a valid lovelace quantity");
  }
  return Object.freeze({ lovelace, assets: policies });
};

const canonicalOutRefCbor = (value: Uint8Array): Buffer => {
  const source = Buffer.from(value);
  const decoded = decodeMidgardOutRefBytes(source);
  const canonical = encodeCbor([decoded.txId, decoded.index]);
  if (!canonical.equals(source)) {
    throw new Error("transition effect out-ref must use exact canonical CBOR");
  }
  return canonical;
};

const canonicalOperationCbor = (
  operation: CanonicalTransitionEffectRawOperationV1,
): readonly unknown[] =>
  operation.type === "delete"
    ? [0n, operation.outRefCbor]
    : [1n, operation.outRefCbor, operation.outputCbor];

/**
 * Constructs the byte-exact transition effect shared by the node producer and
 * independent replay consumers. The operation order is significant: it is the
 * producer's state-transition order and is committed by both CBOR and digest.
 */
export const buildCanonicalTransitionEffectV1 = (
  operations: readonly CanonicalTransitionEffectRawOperationV1[],
): CanonicalTransitionEffectV1 => {
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
    buildCanonicalMidgardLedgerEntryOutputMaterialV1({
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
    schemaVersion: MIDGARD_CANONICAL_TRANSITION_EFFECT_V1_SCHEMA_VERSION,
    operations: Object.freeze(canonicalOperations),
    canonicalCbor,
    digest: Buffer.from(sha256(canonicalCbor)).toString("hex"),
  });
};

export const canonicalTransitionEffectFromStatePatchV1 = (patch: {
  readonly deletedOutRefs: readonly string[];
  readonly upsertedOutRefs: readonly (readonly [string, Buffer])[];
}): CanonicalTransitionEffectV1 =>
  buildCanonicalTransitionEffectV1([
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

export const canonicalDepositTransitionEffectV1 = (entry: {
  readonly outRefCbor: Uint8Array;
  readonly outputCbor: Uint8Array;
}): CanonicalTransitionEffectV1 =>
  buildCanonicalTransitionEffectV1([
    {
      type: "insert",
      outRefCbor: Buffer.from(entry.outRefCbor),
      outputCbor: Buffer.from(entry.outputCbor),
    },
  ]);

/** Exact deposit producer projection used by both ingestion and replay. */
export const deriveCanonicalDepositTransitionEffectV1 = (input: {
  readonly configuredNetwork: Network;
  readonly eventId: Readonly<{
    transactionId: string;
    outputIndex: bigint;
  }>;
  readonly l2NetworkId: bigint;
  readonly l2Address: CanonicalDepositAddressDataV1;
  readonly l2DatumCbor: Uint8Array | null;
  readonly l1Assets: Readonly<Record<string, bigint>>;
  readonly depositPolicyId: string;
  readonly depositAssetNameHex: string;
}): CanonicalTransitionEffectV1 => {
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
    value: projectedDepositValue(input),
    ...(input.l2DatumCbor === null
      ? {}
      : {
          datum: {
            kind: "inline" as const,
            cbor: Buffer.from(input.l2DatumCbor),
          },
        }),
  });
  const outRefCbor = encodeCbor([
    Buffer.from(input.eventId.transactionId, "hex"),
    input.eventId.outputIndex,
  ]);
  return canonicalDepositTransitionEffectV1({ outRefCbor, outputCbor });
};

export const canonicalCommittedWithdrawalTransitionEffectV1 = (input: {
  readonly committedValid: boolean;
  readonly outRefCbor: Uint8Array;
}): CanonicalTransitionEffectV1 =>
  buildCanonicalTransitionEffectV1(
    input.committedValid
      ? [
          {
            type: "delete",
            outRefCbor: Buffer.from(input.outRefCbor),
          },
        ]
      : [],
  );
