import { compareCanonicalJsonKeys } from "@al-ft/midgard-core/canonical-json";
import { CML, Data, toHex } from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";

import {
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
} from "./state-queue.js";
import {
  type StateQueueAuthenticatedTransition,
  type StateQueueCorrectionLockWitness,
  type StateQueueTransitionNode,
  type StateQueueTransitionRedeemer,
} from "./state-queue-correction-transition.js";

export const STATE_QUEUE_AUTHENTICATED_REPLAY_CHECKPOINT_SCHEMA_VERSION =
  "midgard-state-queue-authenticated-replay-checkpoint-v1" as const;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export type StateQueueAuthenticatedReplayCheckpointKind =
  | "init"
  | "deinit"
  | "append"
  | "datum_update"
  | "merge"
  | "fraud_removal"
  | "timeout_correction";

export type StateQueueAuthenticatedReplayCheckpoint = Readonly<{
  schemaVersion: typeof STATE_QUEUE_AUTHENTICATED_REPLAY_CHECKPOINT_SCHEMA_VERSION;
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  transactionHash: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  transactionIndex: string;
  chainPointId: string;
  finalityDepth: string;
  checkpointKind: StateQueueAuthenticatedReplayCheckpointKind;
  mintPolicyIds: readonly string[];
  stateQueueMintRedeemer: StateQueueTransitionRedeemer | null;
  spentInputOutRefs: readonly string[];
  referenceInputOutRefs: readonly string[];
  correctionLockWitness: StateQueueCorrectionLockWitness;
  previousQueue: readonly StateQueueTransitionNode[];
  nextQueue: readonly StateQueueTransitionNode[];
  terminalTransition: StateQueueAuthenticatedTransition | null;
  checkpointDigest: string;
}>;

export type DeriveStateQueueAuthenticatedReplayCheckpointInput = Readonly<{
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  transactionHash: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  transactionIndex: string;
  chainPointId: string;
  finalityDepth: string;
  mintPolicyIds: readonly string[];
  redeemers: readonly StateQueueTransitionRedeemer[];
  spentInputOutRefs: readonly string[];
  referenceInputOutRefs: readonly string[];
  correctionLockWitness: StateQueueCorrectionLockWitness;
  previousQueue: readonly StateQueueTransitionNode[];
  nextQueue: readonly StateQueueTransitionNode[];
}>;

export type Json =
  | null
  | boolean
  | number
  | string
  | readonly Json[]
  | { readonly [key: string]: Json };

export const stableJson = (value: Json): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(stableJson).join(",")}]`;
  return `{${Object.entries(value)
    .sort(([left], [right]) => compareCanonicalJsonKeys(left, right))
    .map(([key, member]) => `${JSON.stringify(key)}:${stableJson(member)}`)
    .join(",")}}`;
};

export const digest = (value: unknown): string =>
  toHex(sha256(new TextEncoder().encode(stableJson(value as Json))));

export const exactRecord = (
  value: unknown,
  keys: readonly string[],
): Record<string, unknown> | null => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return null;
  }
  const actual = Reflect.ownKeys(value);
  const expected = new Set(keys);
  return Object.getPrototypeOf(value) === Object.prototype &&
    actual.length === keys.length &&
    actual.every((key) => typeof key === "string" && expected.has(key))
    ? (value as Record<string, unknown>)
    : null;
};

export const canonicalQueue = (
  queue: readonly StateQueueTransitionNode[],
  allowEmpty: boolean,
): boolean =>
  (allowEmpty || queue.length > 0) &&
  (queue.length === 0 || queue[0]?.headerHash === null) &&
  queue.every(
    (node, index) =>
      Object.getPrototypeOf(node) === Object.prototype &&
      Reflect.ownKeys(node).length === 2 &&
      Object.prototype.hasOwnProperty.call(node, "headerHash") &&
      Object.prototype.hasOwnProperty.call(node, "outRef") &&
      OUT_REF.test(node.outRef) &&
      (index === 0 ? node.headerHash === null : HEX_28.test(node.headerHash!)),
  ) &&
  new Set(queue.map(({ headerHash }) => headerHash)).size === queue.length &&
  new Set(queue.map(({ outRef }) => outRef)).size === queue.length;

export const outputIndex = (outRef: string): bigint =>
  BigInt(outRef.split("#")[1]!);

export const sameIdentities = (
  left: readonly StateQueueTransitionNode[],
  right: readonly StateQueueTransitionNode[],
): boolean =>
  left.length === right.length &&
  left.every((node, index) => node.headerHash === right[index]?.headerHash);

export const canonicalRedeemer = (
  input: DeriveStateQueueAuthenticatedReplayCheckpointInput,
): {
  redeemer: StateQueueTransitionRedeemer;
  decoded: StateQueueRedeemerType;
} | null => {
  const policyIndex = input.mintPolicyIds.indexOf(input.stateQueuePolicyId);
  const matches = input.redeemers.filter(
    ({ purpose, index }) =>
      purpose === "mint" && index === policyIndex.toString(),
  );
  if (policyIndex < 0 || matches.length !== 1) return null;
  const redeemer = matches[0]!;
  try {
    const decoded = Data.from(
      redeemer.cborHex,
      StateQueueRedeemer,
    ) as StateQueueRedeemerType;
    if (
      Data.to(decoded, StateQueueRedeemer) !== redeemer.cborHex &&
      CML.PlutusData.from_cbor_hex(redeemer.cborHex).to_canonical_cbor_hex() !==
        redeemer.cborHex
    ) {
      return null;
    }
    return { redeemer, decoded };
  } catch {
    return null;
  }
};
