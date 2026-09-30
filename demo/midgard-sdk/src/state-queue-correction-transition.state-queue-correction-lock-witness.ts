import { compareCanonicalJsonKeys } from "@al-ft/midgard-core/canonical-json";
import { toHex } from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";

import type {
  CorrectionIdentity,
  CorrectionLockDatum,
} from "./correction-lock.js";

export const STATE_QUEUE_CORRECTION_TRANSITION_SCHEMA_VERSION =
  "midgard-state-queue-correction-transition-v1" as const;

export const STATE_QUEUE_AUTHENTICATED_TRANSITION_SCHEMA_VERSION =
  "midgard-state-queue-authenticated-transition-v1" as const;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export type StateQueueTransitionNode = Readonly<{
  headerHash: string | null;
  outRef: string;
}>;

export type StateQueueTransitionRedeemer = Readonly<{
  purpose: string;
  index: string;
  cborHex: string;
}>;

/**
 * Exact CorrectionLock evidence carried by an authenticated state-queue
 * checkpoint.  The variants mirror the only legal relationships with the
 * singleton: genesis creates it, deinit burns it, append/merge reference Idle,
 * and a correction consumes and continues it.
 */
export type StateQueueCorrectionLockWitness =
  | Readonly<{
      kind: "none";
    }>
  | Readonly<{
      kind: "genesis";
      producedOutRef: string;
      nextDatum: CorrectionLockDatum;
    }>
  | Readonly<{
      kind: "deinit";
      consumedOutRef: string;
      previousDatum: CorrectionLockDatum;
    }>
  | Readonly<{
      kind: "idle_reference";
      referenceOutRef: string;
      datum: CorrectionLockDatum;
    }>
  | Readonly<{
      kind: "correction_transition";
      consumedOutRef: string;
      continuedOutRef: string;
      targetHeaderHash: string;
      correctionIdentity: CorrectionIdentity;
      previousDatum: CorrectionLockDatum;
      nextDatum: CorrectionLockDatum;
    }>;

export type StateQueueCorrectionTransition = Readonly<{
  schemaVersion: typeof STATE_QUEUE_CORRECTION_TRANSITION_SCHEMA_VERSION;
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  transactionHash: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  chainPointId: string;
  finalityDepth: string;
  timedOutHeaderHash: string;
  removalApproach:
    | "PruneUnattestedBlockDescendant"
    | "RemoveLastUnattestedBlock"
    | "PruneTimedOutBlockDescendant"
    | "RemoveTimedOutHead";
  consumedQueueOutRefs: readonly string[];
  continuedQueueOutRefs: readonly Readonly<{
    headerHash: string | null;
    consumedOutRef: string;
    producedOutRef: string;
  }>[];
  removedHeaderHashes: readonly string[];
  transitionDigest: string;
}>;

export type DeriveStateQueueCorrectionTransitionInput = Readonly<{
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  transactionHash: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  chainPointId: string;
  finalityDepth: string;
  mintPolicyIds: readonly string[];
  redeemers: readonly StateQueueTransitionRedeemer[];
  spentInputOutRefs: readonly string[];
  previousQueue: readonly StateQueueTransitionNode[];
  nextQueue: readonly StateQueueTransitionNode[];
}>;

export type StateQueueAuthenticatedTransitionKind =
  | "timeout_correction"
  | "merge"
  | "fraud_removal";

/**
 * Pure, service-independent provenance for an accepted state-queue removal.
 * Admission/authentication remains the responsibility of each service's own
 * Kupo/Ogmios chain follower; this exact, digest-bound shape prevents the node,
 * watcher and committee scanner from giving the same L1 transition different
 * meanings after admission.
 */
export type StateQueueAuthenticatedTransition = Readonly<{
  schemaVersion: typeof STATE_QUEUE_AUTHENTICATED_TRANSITION_SCHEMA_VERSION;
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  transactionHash: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  transactionIndex: string;
  chainPointId: string;
  finalityDepth: string;
  transitionKind: StateQueueAuthenticatedTransitionKind;
  stateQueueMintRedeemer: StateQueueTransitionRedeemer;
  previousQueue: readonly StateQueueTransitionNode[];
  nextQueue: readonly StateQueueTransitionNode[];
  consumedQueueOutRefs: readonly string[];
  continuedQueueOutRefs: readonly Readonly<{
    headerHash: string | null;
    consumedOutRef: string;
    producedOutRef: string;
  }>[];
  removedHeaderHashes: readonly string[];
  correctionLockWitness: StateQueueCorrectionLockWitness;
  correctionTransition: StateQueueCorrectionTransition | null;
  transitionDigest: string;
}>;

export type DeriveStateQueueAuthenticatedTransitionInput =
  DeriveStateQueueCorrectionTransitionInput &
    Readonly<{
      transactionIndex: string;
      referenceInputOutRefs: readonly string[];
      correctionLockWitness: StateQueueCorrectionLockWitness;
    }>;

export type Json =
  | null
  | boolean
  | number
  | string
  | readonly Json[]
  | { readonly [key: string]: Json };

export const stableJson = (value: Json): string => {
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map(stableJson).join(",")}]`;
  }
  return `{${Object.entries(value)
    .sort(([left], [right]) => compareCanonicalJsonKeys(left, right))
    .map(([key, member]) => `${JSON.stringify(key)}:${stableJson(member)}`)
    .join(",")}}`;
};

export const digest = (value: Json): string =>
  toHex(sha256(new TextEncoder().encode(stableJson(value))));

export const withoutDigest = (
  value: Omit<StateQueueCorrectionTransition, "transitionDigest">,
): Json => value as Json;

export const exactRecord = (
  value: unknown,
  keys: readonly string[],
): Record<string, unknown> | null => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return null;
  }
  const actual = Reflect.ownKeys(value);
  const expected = new Set(keys);
  if (
    Object.getPrototypeOf(value) !== Object.prototype ||
    actual.length !== keys.length ||
    actual.some((key) => typeof key !== "string" || !expected.has(key))
  ) {
    return null;
  }
  return value as Record<string, unknown>;
};

export const parseCorrectionIdentity = (
  value: unknown,
): CorrectionIdentity | null => {
  if (value === "AttestationTimeout") return value;
  const fraud = exactRecord(value, ["FraudProof"]);
  if (fraud !== null) {
    const fields = exactRecord(fraud.FraudProof, ["fraud_proof_asset_name"]);
    return fields !== null &&
      typeof fields.fraud_proof_asset_name === "string" &&
      HEX_32.test(fields.fraud_proof_asset_name)
      ? {
          FraudProof: {
            fraud_proof_asset_name: fields.fraud_proof_asset_name,
          },
        }
      : null;
  }
  const availability = exactRecord(value, ["AvailabilityChallenge"]);
  if (availability !== null) {
    const fields = exactRecord(availability.AvailabilityChallenge, [
      "challenge_asset_name",
    ]);
    return fields !== null &&
      typeof fields.challenge_asset_name === "string" &&
      /^44414348[0-9a-f]{56}$/u.test(fields.challenge_asset_name)
      ? {
          AvailabilityChallenge: {
            challenge_asset_name: fields.challenge_asset_name,
          },
        }
      : null;
  }
  return null;
};

export const parseStateQueueCorrectionLockDatum = (
  value: unknown,
): CorrectionLockDatum | null => {
  if (value === "Idle") return value;
  const locked = exactRecord(value, ["Locked"]);
  const fields = exactRecord(locked?.Locked, [
    "target_header_hash",
    "correction_identity",
  ]);
  const identity = parseCorrectionIdentity(fields?.correction_identity);
  return fields !== null &&
    typeof fields.target_header_hash === "string" &&
    HEX_28.test(fields.target_header_hash) &&
    identity !== null
    ? ({
        Locked: {
          target_header_hash: fields.target_header_hash,
          correction_identity: identity,
        },
      } as CorrectionLockDatum)
    : null;
};
