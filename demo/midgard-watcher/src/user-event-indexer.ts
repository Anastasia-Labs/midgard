import { createHash } from "node:crypto";

import {
  DepositDatumSchema,
  DepositEventSchema,
  DepositSpendRedeemerSchema,
  EVENT_WAIT_DURATION_MS,
  ForcedInclusionTxV1Schema,
  HubOracleDatumSchema,
  MerkleRoot,
  outputReferenceToPlutusDataCbor,
  PayoutDatumSchema,
  PayoutMintRedeemerSchema,
  Proof,
  resolveEventInclusionTime,
  RootDomainSchema,
  SettlementDatumSchema,
  TxFieldReceiptV1Schema,
  TxOrderDatumV1Schema,
  TxOrderEventV1Schema,
  TxOrderSpendRedeemerV1Schema,
  UserEventMintRedeemer,
  UserEventWitnessPublishRedeemer,
  userEventWitnessScriptHash,
  WithdrawalEventSchema,
  WithdrawalOrderDatumSchema,
  WithdrawalSpendRedeemerSchema,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import { blake2b } from "../../midgard-core/node_modules/@noble/hashes/blake2.js";
import { verifyMidgardBoundedCollectionItemProofV1 } from "../../midgard-core/src/bounded-collection-v1.js";
import { midgardBoundedItemChunkCountV1 } from "../../midgard-core/src/bounded-item-v1.js";
import {
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxCompactV1,
  decodeMidgardNativeTxProofFieldLengthsV1,
  decodeMidgardNativeTxWitnessSetCompactV1,
  verifyMidgardNativeTxProofSourceV1,
} from "../../midgard-core/src/codec/native.js";
import { deriveMidgardNativeFieldCollectionV1 } from "../../midgard-core/src/codec/native-field-items.js";
import { deriveMidgardTxFieldReceiptAssetNameV1 } from "../../midgard-core/src/consensus-validation-v1.js";
import {
  type DeploymentMarkerV1,
  MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
} from "../../midgard-core/src/deployment-manifest-identity-v1.js";
import {
  type VerifiedWatcherDeploymentIdentityV1,
  verifyWatcherDeploymentIdentityV1,
  type WatcherDeploymentIdentityPolicyV1,
  type WatcherDeploymentTrustRootV1,
} from "./deployment-identity.js";
import {
  encodeWatcherDurableStoreV1,
  journalWatcherProtocolUtxoTransitionV1,
  parseWatcherDurableStoreV1,
  watcherDurableStoreBytesSha256,
  type WatcherDurableStoreV1,
} from "./durable-store.js";
import {
  evaluateWatcherFinalityV1,
  parseWatcherFinalityPolicyV1,
  watcherFinalityConfiguredSourceV1,
  type WatcherFinalityPolicyV1,
  type WatcherFinalityResultV1,
} from "./finality-engine.js";
import {
  encodeWatcherNormalizedL1BlockV1,
  normalizeWatcherL1BlockV1,
  type WatcherL1TransportAttestationContextV1,
  watcherL1TransportAttestationDetailsV1,
  type WatcherNormalizedL1BlockV1,
} from "./l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 } from "./multi-provider-consistency.js";
import {
  parseWatcherPostFinalityRecoveryResultV1,
  parseWatcherRollbackResultV1,
  type WatcherPostFinalityRecoveryInputV1,
  type WatcherPostFinalityRecoveryResultV1,
  type WatcherRollbackResultV1,
  type WatcherRollbackVerificationContextV1,
} from "./rollback-engine.js";

export const WATCHER_USER_EVENT_INDEXER_POLICY_V1_SCHEMA_VERSION =
  "midgard-watcher-user-event-indexer-policy-v1" as const;
export const WATCHER_USER_EVENT_SNAPSHOT_V1_SCHEMA_VERSION =
  "midgard-watcher-user-event-snapshot-v1" as const;
export const WATCHER_USER_EVENT_OBSERVATION_V1_SCHEMA_VERSION =
  "midgard-watcher-user-event-observation-v1" as const;
export const WATCHER_USER_EVENT_HISTORY_ENTRY_V1_SCHEMA_VERSION =
  "midgard-watcher-user-event-history-entry-v1" as const;
export const WATCHER_USER_EVENT_INDEXER_STATE_V1_SCHEMA_VERSION =
  "midgard-watcher-user-event-indexer-state-v1" as const;
export const WATCHER_USER_EVENT_INDEXER_RESULT_V1_SCHEMA_VERSION =
  "midgard-watcher-user-event-indexer-result-v1" as const;
export const WATCHER_USER_EVENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION =
  "midgard-watcher-user-event-public-context-v1" as const;

export const WATCHER_USER_EVENT_INDEXER_V1_BOUNDS = Object.freeze({
  activeEvents: 4_096,
  terminalEvents: 8_192,
  activeHistoryEntries: 128,
  auditHistoryEntries: 1_024,
  evidenceGraphNodes: 2_000_000,
  evidenceGraphBytes: 134_217_728,
  maximumDepositNonNftAssets: 10,
  requiredFinalityDepthMaximum: 2_160,
  finalityLineageSteps: 2_160,
  observationsPerFinalityStep: 16,
  evidenceContainerEntries: 16_384,
  cumulativeEvidenceBytes: 134_217_728,
  cumulativeEvidenceNodes: 2_000_000,
});

export const WATCHER_USER_EVENT_INDEXER_REASON_CODES_V1 = [
  "block_authenticated",
  "rollback_authenticated",
  "duplicate_observation",
  "malformed_policy",
  "malformed_state",
  "malformed_observation",
  "malformed_public_context",
  "binding_mismatch",
  "public_evidence_mismatch",
  "durable_evidence_mismatch",
  "event_output_mismatch",
  "event_datum_mismatch",
  "event_nft_mismatch",
  "event_witness_mismatch",
  "event_inclusion_time_mismatch",
  "event_redeemer_mismatch",
  "event_content_mismatch",
  "event_topology_mismatch",
  "identity_collision",
  "stale_chain_point",
  "history_limit_exceeded",
  "rollback_authority_mismatch",
  "unknown_rollback_target",
  "post_finality_quarantine",
] as const;

export const WATCHER_USER_EVENT_INDEXER_ALERT_CODES_V1 = [
  "watcher_user_event_input_rejected",
  "watcher_user_event_binding_rejected",
  "watcher_user_event_transition_rejected",
  "watcher_user_event_rollback_quarantined",
] as const;

export type WatcherUserEventIndexerReasonCodeV1 =
  (typeof WATCHER_USER_EVENT_INDEXER_REASON_CODES_V1)[number];
export type WatcherUserEventIndexerAlertCodeV1 =
  (typeof WATCHER_USER_EVENT_INDEXER_ALERT_CODES_V1)[number];
export type WatcherUserEventNetworkV1 = "Mainnet" | "Preprod" | "Preview";
export type WatcherUserEventKindV1 = "deposit" | "withdrawal" | "forced_order";
export type WatcherUserEventFinalityStatusV1 = "pending" | "final";
export type WatcherUserEventTerminalStatusV1 =
  | "absorbed"
  | "payout_initialized"
  | "refunded"
  | "processed";

type EventPolicyFields = Readonly<{
  policyId: string;
  spendScriptHash: string;
  addressHex: string;
}>;

export type WatcherUserEventDeploymentAuthorityV1 = Readonly<{
  signedIdentity: unknown;
  policy: WatcherDeploymentIdentityPolicyV1;
  trustRoots: readonly WatcherDeploymentTrustRootV1[];
  result: VerifiedWatcherDeploymentIdentityV1;
}>;

export type WatcherUserEventIndexerPolicyV1 = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_INDEXER_POLICY_V1_SCHEMA_VERSION;
  network: WatcherUserEventNetworkV1;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  deposit: EventPolicyFields;
  withdrawal: EventPolicyFields;
  forcedOrder: EventPolicyFields;
  bootstrapStoreDigest: string;
  deploymentTrustRootId: string;
  eventWaitDurationMs: string;
  requiredFinalityDepth: string;
  maximumActiveHistoryEntries: string;
  maximumAuditHistoryEntries: string;
  policyDigest: string;
}>;

export type WatcherIndexedUserEventV1 = Readonly<{
  kind: WatcherUserEventKindV1;
  eventId: string;
  outRef: string;
  transactionHash: string;
  outputIndex: string;
  nonceOutRef: string;
  policyId: string;
  spendScriptHash: string;
  addressHex: string;
  assetNameHex: string;
  witnessScriptHash: string;
  inclusionTime: string;
  eventCborHex: string;
  datumCborHex: string;
  outputCborHex: string;
  eventContentDigest: string;
  datumDigest: string;
  outputDigest: string;
  originPointDigest: string;
  originChainPointId: string;
  originBlockHash: string;
  originSlot: string;
  originBlockNo: string;
  finalityStatus: WatcherUserEventFinalityStatusV1;
}>;

export type WatcherTerminalUserEventV1 = WatcherIndexedUserEventV1 &
  Readonly<{
    terminalStatus: WatcherUserEventTerminalStatusV1;
    terminalTransactionHash: string;
    terminalPointDigest: string;
    terminalBlockHash: string;
    terminalSlot: string;
    terminalBlockNo: string;
    terminalFinalityStatus: WatcherUserEventFinalityStatusV1;
  }>;

export type WatcherUserEventSnapshotV1 = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_SNAPSHOT_V1_SCHEMA_VERSION;
  activeEvents: readonly WatcherIndexedUserEventV1[];
  terminalEvents: readonly WatcherTerminalUserEventV1[];
  quarantined: boolean;
  snapshotDigest: string;
}>;

export type WatcherUserEventObservationV1 = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_OBSERVATION_V1_SCHEMA_VERSION;
  policyDigest: string;
  network: WatcherUserEventNetworkV1;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  transitionKind: "apply_block" | "rollback";
  pointDigest: string | null;
  blockHash: string | null;
  slot: string | null;
  blockNo: string | null;
  sourceObservationDigest: string | null;
  chainPointId: string | null;
  sourceDurableStoreDigest: string;
  sourceDurableStoreRevision: string;
  durableStoreDigest: string;
  durableStoreRevision: string;
  rollbackTargetEntryDigest: string | null;
  snapshot: WatcherUserEventSnapshotV1;
  observationDigest: string;
}>;

export type WatcherUserEventRollbackAuthorityV1 = Readonly<{
  result: unknown;
  context:
    | WatcherRollbackVerificationContextV1
    | WatcherPostFinalityRecoveryInputV1;
}>;

export type WatcherUserEventFinalityAuthorityV1 = Readonly<{
  policy: unknown;
  lineage: readonly Readonly<{
    observations: readonly Readonly<{
      authenticatedProvider: unknown;
      l1Observation: unknown;
    }>[];
    consistency: unknown;
    result: unknown;
  }>[];
  previousState: unknown;
  observations: readonly Readonly<{
    authenticatedProvider: unknown;
    l1Observation: unknown;
  }>[];
  consistency: unknown;
  result: unknown;
}>;

export type WatcherUserEventPublicContextV1 = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION;
  authenticatedProvider: unknown | null;
  l1Observation: unknown | null;
  sourceDurableStore: unknown;
  durableStore: unknown;
  deploymentAuthority: WatcherUserEventDeploymentAuthorityV1;
  rollbackRestoredEventUtxos: readonly unknown[];
  finalityAuthority: WatcherUserEventFinalityAuthorityV1 | null;
  rollbackAuthority: WatcherUserEventRollbackAuthorityV1 | null;
}>;

export type WatcherUserEventHistoryEntryV1 = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_HISTORY_ENTRY_V1_SCHEMA_VERSION;
  predecessorStateDigest: string | null;
  observation: WatcherUserEventObservationV1;
  publicContext: WatcherUserEventPublicContextV1;
  entryDigest: string;
}>;

export type WatcherUserEventIndexerStateV1 = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_INDEXER_STATE_V1_SCHEMA_VERSION;
  policyDigest: string;
  network: WatcherUserEventNetworkV1;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  durableStoreDigest: string;
  durableStoreRevision: string;
  snapshot: WatcherUserEventSnapshotV1;
  history: readonly WatcherUserEventHistoryEntryV1[];
  activeEntryDigests: readonly string[];
  stateDigest: string;
}>;

export type WatcherUserEventIndexerResultV1 = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_INDEXER_RESULT_V1_SCHEMA_VERSION;
  action: "accept" | "duplicate" | "reject" | "quarantine";
  protocolDecision: "indexed" | "hold" | "quarantined";
  reasonCodes: readonly WatcherUserEventIndexerReasonCodeV1[];
  alertCodes: readonly WatcherUserEventIndexerAlertCodeV1[];
  state: WatcherUserEventIndexerStateV1 | null;
  resultDigest: string;
}>;

type PlainRecord = Record<string, unknown>;
type EventSchema = Parameters<typeof Data.from>[1];
type EvidenceGraphBudget = {
  nodes: number;
  bytes: number;
};

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_BYTES = /^(?:[0-9a-f]{2})*$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const NETWORKS = ["Mainnet", "Preprod", "Preview"] as const;

const immutableWireValue = <T>(value: T): T => {
  const clone = JSON.parse(JSON.stringify(value)) as T;
  const pending: object[] =
    typeof clone === "object" && clone !== null ? [clone] : [];
  while (pending.length > 0) {
    const candidate = pending.pop()!;
    for (const member of Object.values(candidate)) {
      if (typeof member === "object" && member !== null) {
        pending.push(member);
      }
    }
    Object.freeze(candidate);
  }
  return clone;
};

const evidenceWithinBounds = (
  value: unknown,
  budget: EvidenceGraphBudget = { nodes: 0, bytes: 0 },
): boolean => {
  const seen = new WeakSet<object>();
  const path = new WeakSet<object>();
  const visit = (candidate: unknown): boolean => {
    budget.nodes += 1;
    if (
      budget.nodes > WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.evidenceGraphNodes
    ) {
      return false;
    }
    if (typeof candidate === "string") {
      budget.bytes += Buffer.byteLength(candidate, "utf8");
    } else if (
      typeof candidate === "number" ||
      typeof candidate === "bigint" ||
      typeof candidate === "boolean"
    ) {
      budget.bytes += 8;
    } else if (
      typeof candidate === "symbol" ||
      typeof candidate === "function"
    ) {
      return false;
    }
    if (
      budget.bytes > WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.evidenceGraphBytes
    ) {
      return false;
    }
    if (typeof candidate !== "object" || candidate === null) {
      return true;
    }
    if (path.has(candidate)) {
      // A node reachable from itself is a true cycle; recursive parsers must
      // never see one.
      return false;
    }
    if (seen.has(candidate)) {
      // Shared acyclic evidence is walked and budgeted once.
      return true;
    }
    seen.add(candidate);
    path.add(candidate);
    const array = Array.isArray(candidate);
    if (
      Object.getPrototypeOf(candidate) !==
      (array ? Array.prototype : Object.prototype)
    ) {
      return false;
    }
    if (
      array &&
      candidate.length >
        WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.evidenceContainerEntries
    ) {
      return false;
    }
    const keys = Reflect.ownKeys(candidate);
    if (
      keys.some((key) => typeof key !== "string") ||
      (array &&
        (keys.length !== candidate.length + 1 ||
          keys.some(
            (key) =>
              key !== "length" &&
              (!NATURAL.test(key as string) ||
                BigInt(key as string) >= BigInt(candidate.length)),
          )))
    ) {
      return false;
    }
    if (
      !array &&
      keys.length > WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.evidenceContainerEntries
    ) {
      return false;
    }
    for (const key of keys) {
      const descriptor = Object.getOwnPropertyDescriptor(candidate, key);
      if (
        descriptor === undefined ||
        descriptor.get !== undefined ||
        descriptor.set !== undefined ||
        (key !== "length" && !descriptor.enumerable)
      ) {
        return false;
      }
      if (key === "length") {
        continue;
      }
      budget.bytes += Buffer.byteLength(key as string, "utf8");
      if (
        budget.bytes > WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.evidenceGraphBytes
      ) {
        return false;
      }
      if (!visit(descriptor.value)) {
        return false;
      }
    }
    path.delete(candidate);
    return true;
  };
  try {
    return visit(value);
  } catch {
    return false;
  }
};

const sha256Bytes = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");
const sha256Canonical = (value: unknown): string =>
  createHash("sha256").update(JSON.stringify(value), "utf8").digest("hex");
const same = (left: unknown, right: unknown): boolean =>
  JSON.stringify(left) === JSON.stringify(right);

const exactRecord = (
  value: unknown,
  keys: readonly string[],
): PlainRecord | null => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== keys.length
  ) {
    return null;
  }
  const expected = new Set(keys);
  for (const key of Reflect.ownKeys(value)) {
    if (typeof key !== "string" || !expected.has(key)) {
      return null;
    }
    const descriptor = Object.getOwnPropertyDescriptor(value, key);
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      return null;
    }
  }
  return value as PlainRecord;
};


const isHex28 = (value: unknown): value is string =>
  typeof value === "string" && HEX_28.test(value);
const isHex32 = (value: unknown): value is string =>
  typeof value === "string" && HEX_32.test(value);
const isHexBytes = (value: unknown): value is string =>
  typeof value === "string" && HEX_BYTES.test(value);
const isNatural = (value: unknown): value is string =>
  typeof value === "string" && NATURAL.test(value);
const isNetwork = (value: unknown): value is WatcherUserEventNetworkV1 =>
  typeof value === "string" &&
  NETWORKS.includes(value as WatcherUserEventNetworkV1);

const cloneMarker = (value: unknown): DeploymentMarkerV1 | null => {
  const record = exactRecord(value, ["schemaVersion", "manifestId"]);
  if (
    record === null ||
    record.schemaVersion !== MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION ||
    !isHex32(record.manifestId)
  ) {
    return null;
  }
  return Object.freeze({
    schemaVersion: MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
    manifestId: record.manifestId,
  });
};

const policyFields = (value: unknown): EventPolicyFields | null => {
  const record = exactRecord(value, [
    "policyId",
    "spendScriptHash",
    "addressHex",
  ]);
  if (
    record === null ||
    !isHex28(record.policyId) ||
    !isHex28(record.spendScriptHash) ||
    !isHexBytes(record.addressHex) ||
    record.addressHex.length === 0
  ) {
    return null;
  }
  try {
    const address = CML.Address.from_hex(record.addressHex);
    if (
      address.payment_cred()?.as_script()?.to_hex() !== record.spendScriptHash
    ) {
      return null;
    }
  } catch {
    return null;
  }
  return Object.freeze({
    policyId: record.policyId,
    spendScriptHash: record.spendScriptHash,
    addressHex: record.addressHex,
  });
};

const policyWithoutDigest = (
  value: Omit<WatcherUserEventIndexerPolicyV1, "policyDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_INDEXER_POLICY_V1_SCHEMA_VERSION,
  network: value.network,
  releaseEvidenceDigest: value.releaseEvidenceDigest,
  deploymentMarker: value.deploymentMarker,
  deposit: value.deposit,
  withdrawal: value.withdrawal,
  forcedOrder: value.forcedOrder,
  bootstrapStoreDigest: value.bootstrapStoreDigest,
  deploymentTrustRootId: value.deploymentTrustRootId,
  eventWaitDurationMs: value.eventWaitDurationMs,
  requiredFinalityDepth: value.requiredFinalityDepth,
  maximumActiveHistoryEntries: value.maximumActiveHistoryEntries,
  maximumAuditHistoryEntries: value.maximumAuditHistoryEntries,
});

export const makeWatcherUserEventIndexerPolicyV1 = (
  value: Omit<
    WatcherUserEventIndexerPolicyV1,
    "schemaVersion" | "policyDigest" | "eventWaitDurationMs"
  >,
): WatcherUserEventIndexerPolicyV1 | null => {
  const canonical = policyWithoutDigest({
    schemaVersion: WATCHER_USER_EVENT_INDEXER_POLICY_V1_SCHEMA_VERSION,
    ...value,
    eventWaitDurationMs: EVENT_WAIT_DURATION_MS.toString(),
  });
  return parseWatcherUserEventIndexerPolicyV1({
    ...canonical,
    policyDigest: sha256Canonical(canonical),
  });
};

export const parseWatcherUserEventIndexerPolicyV1 = (
  value: unknown,
): WatcherUserEventIndexerPolicyV1 | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "network",
    "releaseEvidenceDigest",
    "deploymentMarker",
    "deposit",
    "withdrawal",
    "forcedOrder",
    "bootstrapStoreDigest",
    "deploymentTrustRootId",
    "eventWaitDurationMs",
    "requiredFinalityDepth",
    "maximumActiveHistoryEntries",
    "maximumAuditHistoryEntries",
    "policyDigest",
  ]);
  const marker = record === null ? null : cloneMarker(record.deploymentMarker);
  const deposit = record === null ? null : policyFields(record.deposit);
  const withdrawal = record === null ? null : policyFields(record.withdrawal);
  const forcedOrder = record === null ? null : policyFields(record.forcedOrder);
  if (
    record === null ||
    marker === null ||
    deposit === null ||
    withdrawal === null ||
    forcedOrder === null ||
    record.schemaVersion !==
      WATCHER_USER_EVENT_INDEXER_POLICY_V1_SCHEMA_VERSION ||
    !isNetwork(record.network) ||
    !isHex32(record.releaseEvidenceDigest) ||
    !isHex32(record.bootstrapStoreDigest) ||
    !isHex32(record.deploymentTrustRootId) ||
    !isNatural(record.eventWaitDurationMs) ||
    record.eventWaitDurationMs !== EVENT_WAIT_DURATION_MS.toString() ||
    !isNatural(record.requiredFinalityDepth) ||
    BigInt(record.requiredFinalityDepth) === 0n ||
    BigInt(record.requiredFinalityDepth) >
      BigInt(
        WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.requiredFinalityDepthMaximum,
      ) ||
    !isNatural(record.maximumActiveHistoryEntries) ||
    BigInt(record.maximumActiveHistoryEntries) === 0n ||
    BigInt(record.maximumActiveHistoryEntries) >
      BigInt(WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.activeHistoryEntries) ||
    !isNatural(record.maximumAuditHistoryEntries) ||
    BigInt(record.maximumAuditHistoryEntries) === 0n ||
    BigInt(record.maximumAuditHistoryEntries) >
      BigInt(WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.auditHistoryEntries) ||
    BigInt(record.maximumAuditHistoryEntries) <
      BigInt(record.maximumActiveHistoryEntries) ||
    !isHex32(record.policyDigest) ||
    new Set([deposit.policyId, withdrawal.policyId, forcedOrder.policyId])
      .size !== 3
  ) {
    return null;
  }
  const expectedNetworkId = record.network === "Mainnet" ? 1 : 0;
  for (const fields of [deposit, withdrawal, forcedOrder]) {
    try {
      if (
        CML.Address.from_hex(fields.addressHex).network_id() !==
        expectedNetworkId
      ) {
        return null;
      }
    } catch {
      return null;
    }
  }
  const canonical = policyWithoutDigest({
    schemaVersion: WATCHER_USER_EVENT_INDEXER_POLICY_V1_SCHEMA_VERSION,
    network: record.network,
    releaseEvidenceDigest: record.releaseEvidenceDigest,
    deploymentMarker: marker,
    deposit,
    withdrawal,
    forcedOrder,
    bootstrapStoreDigest: record.bootstrapStoreDigest,
    deploymentTrustRootId: record.deploymentTrustRootId,
    eventWaitDurationMs: record.eventWaitDurationMs,
    requiredFinalityDepth: record.requiredFinalityDepth,
    maximumActiveHistoryEntries: record.maximumActiveHistoryEntries,
    maximumAuditHistoryEntries: record.maximumAuditHistoryEntries,
  });
  if (sha256Canonical(canonical) !== record.policyDigest) {
    return null;
  }
  return Object.freeze({ ...canonical, policyDigest: record.policyDigest });
};

const eventPolicy = (
  policy: WatcherUserEventIndexerPolicyV1,
  kind: WatcherUserEventKindV1,
): EventPolicyFields =>
  kind === "deposit"
    ? policy.deposit
    : kind === "withdrawal"
      ? policy.withdrawal
      : policy.forcedOrder;

const kindForPolicy = (
  policy: WatcherUserEventIndexerPolicyV1,
  policyId: string,
): WatcherUserEventKindV1 | null =>
  policy.deposit.policyId === policyId
    ? "deposit"
    : policy.withdrawal.policyId === policyId
      ? "withdrawal"
      : policy.forcedOrder.policyId === policyId
        ? "forced_order"
        : null;

const dataRoundTrip = <T>(cborHex: string, schema: EventSchema): T | null => {
  try {
    const value = Data.from(cborHex, schema) as T;
    const canonicalInput =
      CML.PlutusData.from_cbor_hex(cborHex).to_canonical_cbor_hex();
    const canonicalRoundTrip = CML.PlutusData.from_cbor_hex(
      Data.to(value as never, schema),
    ).to_canonical_cbor_hex();
    return canonicalRoundTrip === canonicalInput ? value : null;
  } catch {
    return null;
  }
};

const outputReference = (input: CML.TransactionInput): string =>
  `${input.transaction_id().to_hex()}#${input.index().toString()}`;

const mintPolicyIndex = (mint: CML.Mint, policyId: string): number => {
  const keys = mint.keys();
  for (let index = 0; index < keys.len(); index += 1) {
    if (keys.get(index).to_hex() === policyId) {
      return index;
    }
  }
  return -1;
};

const matchingRedeemer = (
  transaction: WatcherNormalizedL1BlockV1["transactions"][number],
  purpose: "spend" | "mint" | "certificate",
  index: number,
) => {
  const matches = transaction.redeemers.filter(
    (redeemer) =>
      redeemer.purpose === purpose && redeemer.index === index.toString(),
  );
  return matches.length === 1 ? matches[0]! : null;
};

const redeemerAtGlobalIndex = (
  transaction: WatcherNormalizedL1BlockV1["transactions"][number],
  index: bigint,
) =>
  index >= 0n && index <= BigInt(Number.MAX_SAFE_INTEGER)
    ? (transaction.redeemers[Number(index)] ?? null)
    : null;

const decodeMintRedeemer = (
  bytesHex: string,
):
  | Readonly<{
      AuthenticateEvent: Readonly<{
        nonce_input_index: bigint;
        event_output_index: bigint;
        hub_ref_input_index: bigint;
        witness_registration_redeemer_index: bigint;
      }>;
    }>
  | Readonly<{
      BurnEventNFT: Readonly<{
        nonce_asset_name: string;
        witness_unregistration_redeemer_index: bigint;
      }>;
    }>
  | null =>
  dataRoundTrip(bytesHex, UserEventMintRedeemer as unknown as EventSchema);

const decodeWitnessRedeemer = (
  bytesHex: string,
):
  | Readonly<{ MintOrBurn: Readonly<{ targetPolicy: string }> }>
  | Readonly<{
      RegisterToProveNotRegistered: Readonly<{
        registrationCertificateIndex: bigint;
      }>;
    }>
  | Readonly<{
      UnregisterToProveNotRegistered: Readonly<{
        registrationCertificateIndex: bigint;
      }>;
    }>
  | null =>
  dataRoundTrip(
    bytesHex,
    UserEventWitnessPublishRedeemer as unknown as EventSchema,
  );

const registeredScriptHashAt = (
  body: CML.TransactionBody,
  index: number,
  registration: boolean,
): string | null => {
  const certificates = body.certs();
  if (certificates === undefined || index < 0 || index >= certificates.len()) {
    return null;
  }
  const certificate = certificates.get(index);
  const credential = registration
    ? (certificate.as_stake_registration()?.stake_credential() ??
      certificate.as_reg_cert()?.stake_credential())
    : (certificate.as_stake_deregistration()?.stake_credential() ??
      certificate.as_unreg_cert()?.stake_credential());
  return credential?.as_script()?.to_hex() ?? null;
};

const referencedOutRefAt = (
  body: CML.TransactionBody,
  index: bigint,
): string | null => {
  const inputs = body.reference_inputs();
  if (
    inputs === undefined ||
    index < 0n ||
    index >= BigInt(inputs.len()) ||
    index > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    return null;
  }
  return outputReference(inputs.get(Number(index)));
};

const inlineDatumCbor = (output: CML.TransactionOutput): string | null =>
  output.datum()?.as_datum()?.to_cbor_hex() ?? null;

const durableOutputAt = (
  store: WatcherDurableStoreV1,
  outRef: string | null,
  role: WatcherDurableStoreV1["protocolUtxos"][number]["role"],
): CML.TransactionOutput | null => {
  if (outRef === null) {
    return null;
  }
  const matches = store.protocolUtxos.filter(
    (candidate) => candidate.outRef === outRef && candidate.role === role,
  );
  if (matches.length !== 1) {
    return null;
  }
  try {
    const output = CML.TransactionOutput.from_cbor_hex(
      matches[0]!.output.cborHex,
    );
    return output.to_cbor_hex() === matches[0]!.output.cborHex ? output : null;
  } catch {
    return null;
  }
};

const decodeHubAt = (
  store: WatcherDurableStoreV1,
  body: CML.TransactionBody,
  index: bigint,
  deployment: WatcherDeploymentIdentityPolicyV1,
) => {
  const outRef = referencedOutRefAt(body, index);
  const output = durableOutputAt(store, outRef, "hub_oracle");
  const datumHex = output === null ? null : inlineDatumCbor(output);
  return datumHex === null ||
    output!.script_ref() !== undefined ||
    exactlyOneAsset(output!, deployment.appliedScriptHashes.hubOracleMint ?? "")
      ?.quantity !== 1n ||
    output!.address().payment_cred()?.as_script()?.to_hex() !==
      deployment.appliedScriptHashes.hubOracleMint
    ? null
    : dataRoundTrip<Record<string, unknown>>(
        datumHex,
        HubOracleDatumSchema as unknown as EventSchema,
      );
};

const canonicalBody = (bytesHex: string): CML.TransactionBody | null => {
  try {
    const body = CML.TransactionBody.from_cbor_hex(bytesHex);
    return body.to_cbor_hex() === bytesHex ? body : null;
  } catch {
    return null;
  }
};

const outputPolicies = (output: CML.TransactionOutput): readonly string[] => {
  const value = output.amount();
  if (!value.has_multiassets()) {
    return [];
  }
  const keys = value.multi_asset().keys();
  const result: string[] = [];
  for (let index = 0; index < keys.len(); index += 1) {
    result.push(keys.get(index).to_hex());
  }
  return result;
};

const exactlyOneAsset = (
  output: CML.TransactionOutput,
  policyId: string,
): Readonly<{ assetNameHex: string; quantity: bigint }> | null => {
  const assets = output
    .amount()
    .multi_asset()
    .get_assets(CML.ScriptHash.from_hex(policyId));
  if (assets === undefined || assets.len() !== 1) {
    return null;
  }
  const keys = assets.keys();
  const asset = keys.get(0);
  const quantity = assets.get(asset);
  return quantity === undefined
    ? null
    : Object.freeze({ assetNameHex: asset.to_hex(), quantity });
};

const canonicalDatumForOutput = (
  transaction: WatcherNormalizedL1BlockV1["transactions"][number],
  outputIndex: number,
  output: CML.TransactionOutput,
): Readonly<{ cborHex: string; digest: string }> | null => {
  const datum = output.datum()?.as_datum();
  if (datum === undefined || output.script_ref() !== undefined) {
    return null;
  }
  const cborHex = datum.to_cbor_hex();
  const l1Utxo = transaction.utxos.find(
    (candidate) => candidate.outputIndex === outputIndex.toString(),
  );
  if (
    l1Utxo === undefined ||
    l1Utxo.output.bytesHex !== output.to_cbor_hex() ||
    l1Utxo.datum === null ||
    l1Utxo.datum.bytes.bytesHex !== cborHex ||
    l1Utxo.datum.datumHash !== CML.hash_plutus_data(datum).to_hex()
  ) {
    return null;
  }
  return Object.freeze({ cborHex, digest: l1Utxo.datum.bytes.sha256 });
};

const nonceAssetName = (input: CML.TransactionInput): string => {
  const cbor = outputReferenceToPlutusDataCbor({
    txHash: input.transaction_id().to_hex(),
    outputIndex: Number(input.index()),
  });
  return Buffer.from(blake2b(Buffer.from(cbor, "hex"), { dkLen: 32 })).toString(
    "hex",
  );
};

const eventSchemas = (
  kind: WatcherUserEventKindV1,
): Readonly<{ datum: EventSchema; event: EventSchema }> =>
  kind === "deposit"
    ? { datum: DepositDatumSchema, event: DepositEventSchema }
    : kind === "withdrawal"
      ? { datum: WithdrawalOrderDatumSchema, event: WithdrawalEventSchema }
      : { datum: TxOrderDatumV1Schema, event: TxOrderEventV1Schema };

const parseEventDatum = (
  kind: WatcherUserEventKindV1,
  cborHex: string,
): Readonly<{
  event: unknown;
  eventCborHex: string;
  inclusionTime: bigint;
  witness: string;
}> | null => {
  const schemas = eventSchemas(kind);
  const datum = dataRoundTrip<{
    event: unknown;
    inclusion_time: bigint;
    witness: string;
  }>(cborHex, schemas.datum);
  if (
    datum === null ||
    typeof datum.inclusion_time !== "bigint" ||
    !isHex28(datum.witness)
  ) {
    return null;
  }
  try {
    return Object.freeze({
      event: datum.event,
      eventCborHex: Data.to(datum.event as never, schemas.event),
      inclusionTime: datum.inclusion_time,
      witness: datum.witness,
    });
  } catch {
    return null;
  }
};

const eventIdMatchesNonce = (
  kind: WatcherUserEventKindV1,
  event: unknown,
  input: CML.TransactionInput,
): boolean => {
  const record = event as {
    id?: { transactionId?: unknown; outputIndex?: unknown };
  };
  return (
    (kind === "forced_order" || kind === "deposit" || kind === "withdrawal") &&
    record.id?.transactionId === input.transaction_id().to_hex() &&
    record.id.outputIndex === input.index()
  );
};

const scanCreatedEvents = (
  policy: WatcherUserEventIndexerPolicyV1,
  block: WatcherNormalizedL1BlockV1,
  sourceStore: WatcherDurableStoreV1,
  deployment: WatcherDeploymentIdentityPolicyV1,
): readonly WatcherIndexedUserEventV1[] | null => {
  const failCreated = (_label: string): null => null;
  const events: WatcherIndexedUserEventV1[] = [];
  for (const transaction of block.transactions) {
    if (!transaction.isValid) {
      continue;
    }
    const body = canonicalBody(transaction.body.bytesHex);
    if (body === null) {
      return failCreated("body");
    }
    const outputs = body.outputs();
    const inputs = body.inputs();
    const mint = body.mint();
    for (let outputIndex = 0; outputIndex < outputs.len(); outputIndex += 1) {
      const output = outputs.get(outputIndex);
      const knownPolicies = outputPolicies(output)
        .map((policyId) => [policyId, kindForPolicy(policy, policyId)] as const)
        .filter(
          (entry): entry is readonly [string, WatcherUserEventKindV1] =>
            entry[1] !== null,
        );
      if (knownPolicies.length === 0) {
        continue;
      }
      if (knownPolicies.length !== 1 || mint === undefined) {
        return failCreated("known-policy");
      }
      const [policyId, kind] = knownPolicies[0]!;
      const fields = eventPolicy(policy, kind);
      const nft = exactlyOneAsset(output, policyId);
      const policyIndex = mintPolicyIndex(mint, policyId);
      const redeemer =
        policyIndex < 0
          ? null
          : matchingRedeemer(transaction, "mint", policyIndex);
      const decoded =
        redeemer === null ? null : decodeMintRedeemer(redeemer.bytes.bytesHex);
      if (
        nft === null ||
        nft.quantity !== 1n ||
        policyIndex < 0 ||
        mint.get(
          CML.ScriptHash.from_hex(policyId),
          CML.AssetName.from_hex(nft.assetNameHex),
        ) !== 1n ||
        mint.get_assets(CML.ScriptHash.from_hex(policyId))?.len() !== 1 ||
        decoded === null ||
        !("AuthenticateEvent" in decoded)
      ) {
        return failCreated("nft-or-mint");
      }
      const auth = decoded.AuthenticateEvent;
      if (
        auth.event_output_index !== BigInt(outputIndex) ||
        auth.nonce_input_index < 0n ||
        auth.nonce_input_index >= BigInt(inputs.len()) ||
        auth.hub_ref_input_index < 0n ||
        auth.witness_registration_redeemer_index < 0n
      ) {
        return failCreated("auth-indices");
      }
      const nonceInput = inputs.get(Number(auth.nonce_input_index));
      const expectedAssetName = nonceAssetName(nonceInput);
      const expectedWitness = userEventWitnessScriptHash(expectedAssetName);
      const certificateRedeemer = redeemerAtGlobalIndex(
        transaction,
        auth.witness_registration_redeemer_index,
      );
      const certificateIndex =
        certificateRedeemer?.purpose === "certificate" &&
        isNatural(certificateRedeemer.index) &&
        BigInt(certificateRedeemer.index) <= BigInt(Number.MAX_SAFE_INTEGER)
          ? Number(certificateRedeemer.index)
          : -1;
      const witnessRedeemer =
        certificateRedeemer === null
          ? null
          : decodeWitnessRedeemer(certificateRedeemer.bytes.bytesHex);
      const datum = canonicalDatumForOutput(transaction, outputIndex, output);
      const hubDatum = decodeHubAt(
        sourceStore,
        body,
        auth.hub_ref_input_index,
        deployment,
      );
      const expectedHubPolicy =
        kind === "deposit"
          ? hubDatum?.deposit
          : kind === "withdrawal"
            ? hubDatum?.withdrawal
            : hubDatum?.tx_order;
      const expectedHubAddress =
        kind === "deposit"
          ? hubDatum?.deposit_addr
          : kind === "withdrawal"
            ? hubDatum?.withdrawal_addr
            : hubDatum?.tx_order_addr;
      if (
        nft.assetNameHex !== expectedAssetName ||
        output.address().to_hex() !== fields.addressHex ||
        output.address().payment_cred()?.as_script()?.to_hex() !==
          fields.spendScriptHash ||
        datum === null ||
        hubDatum === null ||
        expectedHubPolicy !== policyId ||
        !addressMatchesData(output.address(), expectedHubAddress) ||
        registeredScriptHashAt(body, certificateIndex, true) !==
          expectedWitness ||
        witnessRedeemer === null ||
        !("MintOrBurn" in witnessRedeemer) ||
        witnessRedeemer.MintOrBurn.targetPolicy !== policyId
      ) {
        return failCreated(
          `output-witness datum=${String(datum === null)} cert=${String(
            registeredScriptHashAt(body, certificateIndex, true),
          )} expected=${expectedWitness}`,
        );
      }
      const parsedDatum = parseEventDatum(kind, datum.cborHex);
      const ttl = body.ttl();
      const forcedEvent = parsedDatum?.event as
        | {
            id?: { transactionId?: unknown; outputIndex?: unknown };
            tx?: unknown;
          }
        | undefined;
      if (
        parsedDatum === null ||
        ttl === undefined ||
        ttl > BigInt(Number.MAX_SAFE_INTEGER) ||
        parsedDatum.inclusionTime !==
          BigInt(resolveEventInclusionTime(Number(ttl), policy.network)) ||
        parsedDatum.witness !== expectedWitness ||
        !eventIdMatchesNonce(kind, parsedDatum.event, nonceInput) ||
        (kind === "forced_order" &&
          (!isHex32(forcedEvent?.id?.transactionId) ||
            typeof forcedEvent.id.outputIndex !== "bigint" ||
            !forcedPayloadMatchesNativeSource(forcedEvent.tx, {
              sourceStore,
              body,
              deployment,
              txOrderId: {
                transactionId: forcedEvent.id.transactionId,
                outputIndex: forcedEvent.id.outputIndex,
              },
            })))
      ) {
        return failCreated(
          `datum-time parsed=${String(parsedDatum === null)} ttl=${String(ttl)}`,
        );
      }
      const policies = outputPolicies(output);
      const nonNftAssetCount = policies.reduce((count, candidatePolicy) => {
        if (candidatePolicy === policyId) {
          return count;
        }
        return (
          count +
          (output
            .amount()
            .multi_asset()
            .get_assets(CML.ScriptHash.from_hex(candidatePolicy))
            ?.len() ?? 0)
        );
      }, 1);
      if (
        (kind === "deposit" &&
          nonNftAssetCount >
            WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.maximumDepositNonNftAssets) ||
        (kind !== "deposit" &&
          (policies.length !== 1 || nonNftAssetCount !== 1))
      ) {
        return failCreated("asset-count");
      }
      const outRef = `${transaction.txHash}#${outputIndex.toString()}`;
      const outputCborHex = output.to_cbor_hex();
      const eventId = outputReferenceToPlutusDataCbor({
        txHash: nonceInput.transaction_id().to_hex(),
        outputIndex: Number(nonceInput.index()),
      });
      events.push(
        Object.freeze({
          kind,
          eventId,
          outRef,
          transactionHash: transaction.txHash,
          outputIndex: outputIndex.toString(),
          nonceOutRef: outputReference(nonceInput),
          policyId,
          spendScriptHash: fields.spendScriptHash,
          addressHex: fields.addressHex,
          assetNameHex: expectedAssetName,
          witnessScriptHash: expectedWitness,
          inclusionTime: parsedDatum.inclusionTime.toString(),
          eventCborHex: parsedDatum.eventCborHex,
          datumCborHex: datum.cborHex,
          outputCborHex,
          eventContentDigest: sha256Bytes(
            Buffer.from(parsedDatum.eventCborHex, "hex"),
          ),
          datumDigest: datum.digest,
          outputDigest: sha256Bytes(Buffer.from(outputCborHex, "hex")),
          originPointDigest: block.chainPoint.pointDigest,
          originChainPointId: block.chainPoint.chainPointId,
          originBlockHash: block.chainPoint.blockHash,
          originSlot: block.chainPoint.slot,
          originBlockNo: block.chainPoint.blockNo,
          finalityStatus: "pending",
        }),
      );
    }
  }
  const sorted = events.sort((left, right) =>
    left.outRef.localeCompare(right.outRef),
  );
  if (
    sorted.length > WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.activeEvents ||
    new Set(sorted.map(({ outRef }) => outRef)).size !== sorted.length ||
    new Set(sorted.map(({ eventId }) => eventId)).size !== sorted.length
  ) {
    return failCreated("identity");
  }
  return Object.freeze(sorted);
};

type DecodedTerminalSpend = Readonly<{
  terminalStatus: WatcherUserEventTerminalStatusV1;
  outputIndex: bigint;
  hubRefInputIndex: bigint;
  settlementRefInputIndex: bigint;
  mintRedeemerIndex: bigint;
  payoutMintRedeemerIndex: bigint | null;
  membershipProof: Readonly<{
    domain: string;
    root: string;
    phas_root: string;
    count: bigint;
    key: string;
    value: string;
    proof: unknown;
  }>;
  inclusionProofRedeemerIndex: bigint;
  purpose: unknown;
}>;

const decodeTerminalSpend = (
  kind: WatcherUserEventKindV1,
  bytesHex: string,
  inputIndex: number,
): DecodedTerminalSpend | null => {
  if (kind === "deposit") {
    const decoded = dataRoundTrip<{
      input_index: bigint;
      output_index: bigint;
      hub_ref_input_index: bigint;
      settlement_ref_input_index: bigint;
      mint_redeemer_index: bigint;
      membership_proof: DecodedTerminalSpend["membershipProof"];
      inclusion_proof_script_withdraw_redeemer_index: bigint;
    }>(bytesHex, DepositSpendRedeemerSchema);
    return decoded?.input_index === BigInt(inputIndex)
      ? {
          terminalStatus: "absorbed",
          outputIndex: decoded.output_index,
          hubRefInputIndex: decoded.hub_ref_input_index,
          settlementRefInputIndex: decoded.settlement_ref_input_index,
          mintRedeemerIndex: decoded.mint_redeemer_index,
          payoutMintRedeemerIndex: null,
          membershipProof: decoded.membership_proof,
          inclusionProofRedeemerIndex:
            decoded.inclusion_proof_script_withdraw_redeemer_index,
          purpose: null,
        }
      : null;
  }
  if (kind === "forced_order") {
    const decoded = dataRoundTrip<{
      input_index: bigint;
      output_index: bigint;
      hub_ref_input_index: bigint;
      settlement_ref_input_index: bigint;
      burn_redeemer_index: bigint;
      membership_proof: DecodedTerminalSpend["membershipProof"];
      inclusion_proof_script_withdraw_redeemer_index: bigint;
      validity_override: unknown;
    }>(bytesHex, TxOrderSpendRedeemerV1Schema);
    return decoded?.input_index === BigInt(inputIndex)
      ? {
          terminalStatus: "processed",
          outputIndex: decoded.output_index,
          hubRefInputIndex: decoded.hub_ref_input_index,
          settlementRefInputIndex: decoded.settlement_ref_input_index,
          mintRedeemerIndex: decoded.burn_redeemer_index,
          payoutMintRedeemerIndex: null,
          membershipProof: decoded.membership_proof,
          inclusionProofRedeemerIndex:
            decoded.inclusion_proof_script_withdraw_redeemer_index,
          purpose: decoded.validity_override,
        }
      : null;
  }
  const decoded = dataRoundTrip<{
    input_index: bigint;
    output_index: bigint;
    hub_ref_input_index: bigint;
    settlement_ref_input_index: bigint;
    burn_redeemer_index: bigint;
    payout_mint_redeemer_index: bigint;
    membership_proof: DecodedTerminalSpend["membershipProof"];
    inclusion_proof_script_withdraw_redeemer_index: bigint;
    purpose: "InitializePayout" | { Refund: { validity_override: unknown } };
  }>(bytesHex, WithdrawalSpendRedeemerSchema);
  if (decoded === null) {
    return null;
  }
  if (decoded.input_index !== BigInt(inputIndex)) {
    return null;
  }
  const terminalStatus =
    decoded.purpose === "InitializePayout"
      ? "payout_initialized"
      : "Refund" in decoded.purpose
        ? "refunded"
        : null;
  return terminalStatus === null
    ? null
    : {
        terminalStatus,
        outputIndex: decoded.output_index,
        hubRefInputIndex: decoded.hub_ref_input_index,
        settlementRefInputIndex: decoded.settlement_ref_input_index,
        mintRedeemerIndex: decoded.burn_redeemer_index,
        payoutMintRedeemerIndex: decoded.payout_mint_redeemer_index,
        membershipProof: decoded.membership_proof,
        inclusionProofRedeemerIndex:
          decoded.inclusion_proof_script_withdraw_redeemer_index,
        purpose: decoded.purpose,
      };
};

const outputValue = (
  output: CML.TransactionOutput,
): ReadonlyMap<string, bigint> => {
  const result = new Map<string, bigint>([
    ["lovelace", output.amount().coin()],
  ]);
  const multiAsset = output.amount().multi_asset();
  const policies = multiAsset.keys();
  for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
    const policyId = policies.get(policyIndex);
    const assets = multiAsset.get_assets(policyId);
    if (assets === undefined) {
      continue;
    }
    for (let assetIndex = 0; assetIndex < assets.len(); assetIndex += 1) {
      const assetName = assets.keys().get(assetIndex);
      result.set(
        `${policyId.to_hex()}${assetName.to_hex()}`,
        assets.get(assetName) ?? 0n,
      );
    }
  }
  return result;
};

const expectedTerminalValue = (
  event: WatcherIndexedUserEventV1,
  input: CML.TransactionOutput,
  hubDatum: Record<string, unknown>,
  status: WatcherUserEventTerminalStatusV1,
): ReadonlyMap<string, bigint> => {
  const expected = new Map(outputValue(input));
  expected.delete(`${event.policyId}${event.assetNameHex}`);
  if (status === "payout_initialized" && isHex28(hubDatum.payout)) {
    expected.set(`${hubDatum.payout}${event.assetNameHex}`, 1n);
  }
  return expected;
};

const sameValue = (
  left: ReadonlyMap<string, bigint>,
  right: ReadonlyMap<string, bigint>,
): boolean =>
  left.size === right.size &&
  [...left].every(([unit, quantity]) => right.get(unit) === quantity);

const addressMatchesData = (address: CML.Address, value: unknown): boolean => {
  const candidate = value as {
    paymentCredential?:
      | { ScriptCredential?: [unknown] }
      | { PublicKeyCredential?: [unknown] };
    stakeCredential?: unknown;
  };
  const payment = address.payment_cred();
  const expectedScript =
    "ScriptCredential" in (candidate.paymentCredential ?? {})
      ? (
          candidate.paymentCredential as {
            ScriptCredential: [unknown];
          }
        ).ScriptCredential[0]
      : null;
  const expectedKey =
    "PublicKeyCredential" in (candidate.paymentCredential ?? {})
      ? (
          candidate.paymentCredential as {
            PublicKeyCredential: [unknown];
          }
        ).PublicKeyCredential[0]
      : null;
  return (
    candidate.stakeCredential === null &&
    ((isHex28(expectedScript) &&
      payment?.as_script()?.to_hex() === expectedScript) ||
      (isHex28(expectedKey) && payment?.as_pub_key()?.to_hex() === expectedKey))
  );
};

const eventKeyValueCbor = (
  event: WatcherIndexedUserEventV1,
): Readonly<{ key: string; value: string; payload: unknown }> | null => {
  try {
    const parsed = Data.from(
      event.eventCborHex,
      eventSchemas(event.kind).event,
    ) as { id: unknown; info?: unknown; tx?: unknown };
    const plutus = CML.PlutusData.from_cbor_hex(event.eventCborHex)
      .as_constr_plutus_data()
      ?.fields();
    if (plutus === undefined || plutus.len() !== 2) {
      return null;
    }
    return {
      key: plutus.get(0).to_cbor_hex(),
      value: plutus.get(1).to_cbor_hex(),
      payload: parsed.info ?? parsed.tx,
    };
  } catch {
    return null;
  }
};

const countedRootMatches = (
  proof: DecodedTerminalSpend["membershipProof"],
  expectedDomain:
    | "DepositsRootDomain"
    | "WithdrawalsRootDomain"
    | "ForcedTransactionsV1RootDomain",
  expectedRoot: unknown,
): boolean => {
  if (
    proof.domain !== expectedDomain ||
    proof.root !== expectedRoot ||
    proof.count <= 0n ||
    !isHex32(proof.root) ||
    !isHex32(proof.phas_root)
  ) {
    return false;
  }
  const tag = Buffer.from("MidgardRootCountV1", "utf8");
  const domain = Buffer.from(
    Data.to(expectedDomain as never, RootDomainSchema as never),
    "hex",
  );
  const count = Buffer.from(
    Data.to(proof.count as never, Data.Integer() as never),
    "hex",
  );
  return (
    Buffer.from(
      blake2b(
        Buffer.concat([
          tag,
          domain,
          Buffer.from(proof.phas_root, "hex"),
          count,
        ]),
        { dkLen: 32 },
      ),
    ).toString("hex") === proof.root
  );
};

const membershipWithdrawalCbor = (
  proof: DecodedTerminalSpend["membershipProof"],
): string | null => {
  try {
    const values = CML.PlutusDataList.new();
    values.add(
      CML.PlutusData.from_cbor_hex(
        Data.to(proof.phas_root as never, MerkleRoot as never),
      ),
    );
    values.add(CML.PlutusData.new_bytes(Buffer.from(proof.key, "hex")));
    values.add(CML.PlutusData.new_bytes(Buffer.from(proof.value, "hex")));
    values.add(
      CML.PlutusData.from_cbor_hex(
        Data.to(proof.proof as never, Proof as never),
      ),
    );
    return CML.PlutusData.new_list(values).to_canonical_cbor_hex();
  } catch {
    return null;
  }
};

const authenticReferenceDatum = (
  store: WatcherDurableStoreV1,
  body: CML.TransactionBody,
  index: bigint,
  role: "settlement" | "hub_oracle",
  policyId: string,
  schema: EventSchema,
): Readonly<{
  output: CML.TransactionOutput;
  datum: Record<string, unknown>;
}> | null => {
  const output = durableOutputAt(store, referencedOutRefAt(body, index), role);
  const datumCbor = output === null ? null : inlineDatumCbor(output);
  if (
    output === null ||
    datumCbor === null ||
    exactlyOneAsset(output, policyId)?.quantity !== 1n
  ) {
    return null;
  }
  const datum = dataRoundTrip<Record<string, unknown>>(datumCbor, schema);
  return datum === null ? null : { output, datum };
};

const naturalNumber = (value: unknown): number | null =>
  typeof value === "bigint" &&
  value >= 0n &&
  value <= BigInt(Number.MAX_SAFE_INTEGER)
    ? Number(value)
    : null;

const forcedPayloadMatchesNativeSource = (
  payload: unknown,
  verification: Readonly<{
    sourceStore: WatcherDurableStoreV1;
    body: CML.TransactionBody;
    deployment: WatcherDeploymentIdentityPolicyV1;
    txOrderId: Readonly<{ transactionId: string; outputIndex: bigint }>;
  }>,
): boolean => {
  const candidate = payload as {
    tx_id?: unknown;
    transaction_commitment?: unknown;
    source?: {
      compact_cbor?: unknown;
      witness_set_compact_cbor?: unknown;
      field_preimage_lengths_cbor?: unknown;
    };
    terminal_receipt_reference?: unknown;
  };
  if (
    !isHex32(candidate.tx_id) ||
    !isHex32(candidate.transaction_commitment) ||
    !isHexBytes(candidate.source?.compact_cbor) ||
    !isHexBytes(candidate.source.witness_set_compact_cbor) ||
    !isHexBytes(candidate.source.field_preimage_lengths_cbor)
  ) {
    return false;
  }
  try {
    const source = {
      compactCbor: Buffer.from(candidate.source.compact_cbor, "hex"),
      witnessSetCompactCbor: Buffer.from(
        candidate.source.witness_set_compact_cbor,
        "hex",
      ),
      fieldPreimageLengthsCbor: Buffer.from(
        candidate.source.field_preimage_lengths_cbor,
        "hex",
      ),
    };
    verifyMidgardNativeTxProofSourceV1({
      transactionId: Buffer.from(candidate.tx_id, "hex"),
      source,
    });
    if (
      computeMidgardNativeTxProofCommitmentV1(source).toString("hex") !==
      candidate.transaction_commitment
    ) {
      return false;
    }
    const compact = decodeMidgardNativeTxCompactV1(source.compactCbor);
    const witness = decodeMidgardNativeTxWitnessSetCompactV1(
      source.witnessSetCompactCbor,
    );
    const lengths = decodeMidgardNativeTxProofFieldLengthsV1(
      source.fieldPreimageLengthsCbor,
    );
    const commitments = [
      compact.transactionBody.spendInputsHash,
      compact.transactionBody.referenceInputsHash,
      compact.transactionBody.outputsHash,
      compact.transactionBody.requiredObserversHash,
      compact.transactionBody.requiredSignersHash,
      compact.transactionBody.mintHash,
      witness.scriptTxWitsHash,
      witness.addrTxWitsHash,
      witness.redeemerTxWitsHash,
    ];
    const emptyAt = (fieldIndex: number): boolean =>
      lengths[fieldIndex] === 1 &&
      deriveMidgardNativeFieldCollectionV1({
        fieldIndex,
        preimageCbor: Buffer.from([0x80]),
      }).commitment.equals(commitments[fieldIndex]!);
    if (candidate.terminal_receipt_reference === null) {
      return commitments.every((_commitment, fieldIndex) =>
        emptyAt(fieldIndex),
      );
    }
    const receiptReference = candidate.terminal_receipt_reference as {
      transactionId?: unknown;
      outputIndex?: unknown;
    };
    if (
      !isHex32(receiptReference.transactionId) ||
      typeof receiptReference.outputIndex !== "bigint" ||
      receiptReference.outputIndex < 0n
    ) {
      return false;
    }
    const receiptOutRef = `${receiptReference.transactionId}#${receiptReference.outputIndex.toString()}`;
    const referenceInputs = verification.body.reference_inputs();
    let referenceMatches = 0;
    if (referenceInputs !== undefined) {
      for (let index = 0; index < referenceInputs.len(); index += 1) {
        if (outputReference(referenceInputs.get(index)) === receiptOutRef) {
          referenceMatches += 1;
        }
      }
    }
    const receiptOutput = durableOutputAt(
      verification.sourceStore,
      receiptOutRef,
      "proof_thread",
    );
    const receiptDatumHex =
      receiptOutput === null ? null : inlineDatumCbor(receiptOutput);
    const receipt =
      receiptDatumHex === null
        ? null
        : dataRoundTrip<{
            field_receipt_policy_id: string;
            tx_order_policy_id: string;
            tx_order_id: {
              transactionId: string;
              outputIndex: bigint;
            };
            transaction_commitment: string;
            collection_proof: {
              version: bigint;
              field_index: bigint;
              item_count: bigint;
              item_index: bigint;
              item_length: bigint;
              item_commitment: string;
              frontier: readonly { height: bigint; hash: string }[];
              siblings: readonly string[];
            };
            chunk_index: bigint;
            field_encoded_size: bigint;
          }>(receiptDatumHex, TxFieldReceiptV1Schema as unknown as EventSchema);
    const receiptPolicy =
      verification.deployment.appliedScriptHashes.txOrderFieldReceiptMint;
    const receiptSpend =
      verification.deployment.appliedScriptHashes.txOrderFieldReceiptSpend;
    if (
      referenceMatches !== 1 ||
      receiptOutput === null ||
      receiptOutput.script_ref() !== undefined ||
      receiptOutput.address().payment_cred()?.as_script()?.to_hex() !==
        receiptSpend ||
      receipt === null ||
      receipt.field_receipt_policy_id !== receiptPolicy ||
      receipt.tx_order_policy_id !==
        verification.deployment.appliedScriptHashes.txOrderMint ||
      receipt.tx_order_id.transactionId !==
        verification.txOrderId.transactionId ||
      receipt.tx_order_id.outputIndex !== verification.txOrderId.outputIndex ||
      receipt.transaction_commitment !== candidate.transaction_commitment
    ) {
      return false;
    }
    const proof = receipt.collection_proof;
    const fieldIndex = naturalNumber(proof.field_index);
    const itemCount = naturalNumber(proof.item_count);
    const itemIndex = naturalNumber(proof.item_index);
    const itemLength = naturalNumber(proof.item_length);
    const version = naturalNumber(proof.version);
    const chunkIndex = naturalNumber(receipt.chunk_index);
    const fieldEncodedSize = naturalNumber(receipt.field_encoded_size);
    const frontier = proof.frontier.map((peak) => ({
      height: naturalNumber(peak.height),
      hash: isHex32(peak.hash) ? Buffer.from(peak.hash, "hex") : null,
    }));
    if (
      fieldIndex === null ||
      fieldIndex >= commitments.length ||
      itemCount === null ||
      itemIndex === null ||
      itemLength === null ||
      version !== 1 ||
      chunkIndex === null ||
      fieldEncodedSize === null ||
      !isHex32(proof.item_commitment) ||
      !proof.siblings.every(isHex32) ||
      frontier.some((peak) => peak.height === null || peak.hash === null) ||
      receiptPolicy === undefined
    ) {
      return false;
    }
    const collectionProof = {
      version: 1 as const,
      fieldIndex,
      itemCount,
      itemIndex,
      itemLength,
      itemCommitment: Buffer.from(proof.item_commitment, "hex"),
      frontier: {
        count: itemCount,
        peaks: frontier.map((peak) => ({
          height: peak.height!,
          hash: peak.hash!,
        })),
      },
      siblings: proof.siblings.map((hash) => Buffer.from(hash, "hex")),
    };
    const expectedAssetName = deriveMidgardTxFieldReceiptAssetNameV1({
      txOrderPolicyId: Buffer.from(
        verification.deployment.appliedScriptHashes.txOrderMint!,
        "hex",
      ),
      txOrderTransactionId: Buffer.from(
        verification.txOrderId.transactionId,
        "hex",
      ),
      txOrderOutputIndex: verification.txOrderId.outputIndex,
      transactionCommitment: Buffer.from(
        candidate.transaction_commitment,
        "hex",
      ),
      fieldIndex,
      itemIndex,
      chunkIndex,
    }).toString("hex");
    const receiptAssets = receiptOutput
      .amount()
      .multi_asset()
      .get_assets(CML.ScriptHash.from_hex(receiptPolicy));
    return (
      verifyMidgardBoundedCollectionItemProofV1({
        expectedCommitment: commitments[fieldIndex]!,
        proof: collectionProof,
      }) &&
      itemIndex + 1 === itemCount &&
      chunkIndex + 1 === midgardBoundedItemChunkCountV1(itemLength) &&
      fieldEncodedSize === lengths[fieldIndex] &&
      commitments
        .slice(fieldIndex + 1)
        .every((_commitment, index) => emptyAt(fieldIndex + index + 1)) &&
      receiptAssets?.get(CML.AssetName.from_hex(expectedAssetName)) === 1n
    );
  } catch {
    return false;
  }
};

const cardanoDatumMatches = (
  output: CML.TransactionOutput,
  expected: unknown,
): boolean => {
  if (expected === "NoDatum") {
    return output.datum() === undefined;
  }
  const candidate = expected as {
    DatumHash?: { hash?: unknown };
    InlineDatum?: { data?: unknown };
  };
  if (candidate.DatumHash !== undefined) {
    return (
      (
        output.datum() as
          | { as_hash?: () => { to_hex(): string } | undefined }
          | undefined
      )
        ?.as_hash?.()
        ?.to_hex() === candidate.DatumHash.hash
    );
  }
  if (candidate.InlineDatum !== undefined) {
    try {
      return (
        output.datum()?.as_datum()?.to_cbor_hex() ===
        Data.to(candidate.InlineDatum.data as never)
      );
    } catch {
      return false;
    }
  }
  return false;
};

const verifyTerminalSemantics = (
  event: WatcherIndexedUserEventV1,
  sourceStore: WatcherDurableStoreV1,
  transaction: WatcherNormalizedL1BlockV1["transactions"][number],
  body: CML.TransactionBody,
  inputIndex: number,
  spend: DecodedTerminalSpend,
  deployment: WatcherDeploymentIdentityPolicyV1,
): boolean => {
  const outputs = body.outputs();
  if (
    spend.outputIndex < 0n ||
    spend.outputIndex >= BigInt(outputs.len()) ||
    spend.outputIndex > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    return false;
  }
  const produced = outputs.get(Number(spend.outputIndex));
  const input = CML.TransactionOutput.from_cbor_hex(event.outputCborHex);
  const hubDatum = decodeHubAt(
    sourceStore,
    body,
    spend.hubRefInputIndex,
    deployment,
  );
  const settlementPolicy = hubDatum?.settlement;
  const settlement = isHex28(settlementPolicy)
    ? authenticReferenceDatum(
        sourceStore,
        body,
        spend.settlementRefInputIndex,
        "settlement",
        settlementPolicy,
        SettlementDatumSchema as unknown as EventSchema,
      )
    : null;
  const eventPair = eventKeyValueCbor(event);
  const forcedProofValue =
    event.kind === "forced_order" && eventPair !== null
      ? (() => {
          const tx = eventPair.payload as {
            tx_id?: unknown;
            transaction_commitment?: unknown;
            source?: unknown;
          };
          try {
            return Data.to(
              {
                tx_id: tx.tx_id,
                transaction_commitment: tx.transaction_commitment,
                source: tx.source,
                operator_validity: spend.purpose,
              } as never,
              ForcedInclusionTxV1Schema as never,
            );
          } catch {
            return null;
          }
        })()
      : eventPair?.value;
  if (
    hubDatum === null ||
    settlement === null ||
    eventPair === null ||
    settlement.output.script_ref() !== undefined ||
    !addressMatchesData(
      settlement.output.address(),
      hubDatum.settlement_addr,
    ) ||
    produced.script_ref() !== undefined ||
    spend.membershipProof.key !== eventPair.key ||
    forcedProofValue === null ||
    spend.membershipProof.value !== forcedProofValue
  ) {
    return false;
  }
  const eventPolicy =
    event.kind === "deposit"
      ? hubDatum.deposit
      : event.kind === "withdrawal"
        ? hubDatum.withdrawal
        : hubDatum.tx_order;
  const domain =
    event.kind === "deposit"
      ? "DepositsRootDomain"
      : event.kind === "withdrawal"
        ? "WithdrawalsRootDomain"
        : "ForcedTransactionsV1RootDomain";
  const root =
    event.kind === "deposit"
      ? settlement.datum.deposits_root
      : event.kind === "withdrawal"
        ? settlement.datum.withdrawals_root
        : settlement.datum.forced_transactions_root;
  const membershipRedeemer = redeemerAtGlobalIndex(
    transaction,
    spend.inclusionProofRedeemerIndex,
  );
  const mintRedeemer = redeemerAtGlobalIndex(
    transaction,
    spend.mintRedeemerIndex,
  );
  const policyIndex =
    body.mint() === undefined
      ? -1
      : mintPolicyIndex(body.mint()!, event.policyId);
  if (
    eventPolicy !== event.policyId ||
    !countedRootMatches(spend.membershipProof, domain, root) ||
    membershipRedeemer?.purpose !== "withdrawal" ||
    membershipRedeemer.bytes.bytesHex !==
      membershipWithdrawalCbor(spend.membershipProof) ||
    mintRedeemer?.purpose !== "mint" ||
    mintRedeemer.index !== policyIndex.toString() ||
    !sameValue(
      outputValue(produced),
      expectedTerminalValue(event, input, hubDatum, spend.terminalStatus),
    )
  ) {
    return false;
  }
  const datum = Data.from(
    event.datumCborHex,
    eventSchemas(event.kind).datum,
  ) as {
    event: {
      id?: { transactionId?: unknown; outputIndex?: unknown };
      info?: unknown;
      tx?: unknown;
    };
    refund_address?: unknown;
    refund_datum?: unknown;
  };
  if (event.kind === "deposit") {
    return (
      addressMatchesData(produced.address(), hubDatum.reserve_addr) &&
      produced.datum() === undefined
    );
  }
  if (event.kind === "forced_order") {
    return (
      isHex32(datum.event.id?.transactionId) &&
      typeof datum.event.id.outputIndex === "bigint" &&
      forcedPayloadMatchesNativeSource(datum.event.tx, {
        sourceStore,
        body,
        deployment,
        txOrderId: {
          transactionId: datum.event.id.transactionId,
          outputIndex: datum.event.id.outputIndex,
        },
      }) &&
      addressMatchesData(produced.address(), datum.refund_address) &&
      cardanoDatumMatches(produced, datum.refund_datum)
    );
  }
  if (spend.terminalStatus === "refunded") {
    return (
      addressMatchesData(produced.address(), datum.refund_address) &&
      cardanoDatumMatches(produced, datum.refund_datum)
    );
  }
  const withdrawalInfo = datum.event.info as {
    body?: {
      l2_value?: unknown;
      l1_address?: unknown;
      l1_datum?: unknown;
    };
    validity?: unknown;
  };
  const payoutPolicy = hubDatum.payout;
  const payoutMint =
    spend.payoutMintRedeemerIndex === null
      ? null
      : redeemerAtGlobalIndex(transaction, spend.payoutMintRedeemerIndex);
  const decodedPayout =
    payoutMint === null
      ? null
      : dataRoundTrip<{
          MintPayout: {
            withdrawal_utxo_out_ref: {
              transactionId: string;
              outputIndex: bigint;
            };
            withdrawal_input_index: bigint;
            withdrawal_spend_redeemer_index: bigint;
            hub_ref_input_index: bigint;
          };
        }>(
          payoutMint.bytes.bytesHex,
          PayoutMintRedeemerSchema as unknown as EventSchema,
        );
  const payoutDatumHex = inlineDatumCbor(produced);
  const payoutDatum =
    payoutDatumHex === null
      ? null
      : dataRoundTrip<Record<string, unknown>>(
          payoutDatumHex,
          PayoutDatumSchema as unknown as EventSchema,
        );
  const payoutPolicyIndex =
    isHex28(payoutPolicy) && body.mint() !== undefined
      ? mintPolicyIndex(body.mint()!, payoutPolicy)
      : -1;
  const withdrawalSpendRedeemerIndex = transaction.redeemers.findIndex(
    (redeemer) =>
      redeemer.purpose === "spend" && redeemer.index === inputIndex.toString(),
  );
  const payoutAssets =
    isHex28(payoutPolicy) && body.mint() !== undefined
      ? body.mint()!.get_assets(CML.ScriptHash.from_hex(payoutPolicy))
      : undefined;
  return (
    isHex28(payoutPolicy) &&
    payoutMint?.purpose === "mint" &&
    payoutMint.index === payoutPolicyIndex.toString() &&
    decodedPayout !== null &&
    decodedPayout.MintPayout.withdrawal_utxo_out_ref.transactionId ===
      event.transactionHash &&
    decodedPayout.MintPayout.withdrawal_utxo_out_ref.outputIndex ===
      BigInt(event.outputIndex) &&
    decodedPayout.MintPayout.withdrawal_input_index === BigInt(inputIndex) &&
    decodedPayout.MintPayout.withdrawal_spend_redeemer_index ===
      BigInt(withdrawalSpendRedeemerIndex) &&
    decodedPayout.MintPayout.hub_ref_input_index === spend.hubRefInputIndex &&
    payoutAssets?.len() === 1 &&
    payoutAssets.get(CML.AssetName.from_hex(event.assetNameHex)) === 1n &&
    withdrawalInfo.validity === "WithdrawalIsValid" &&
    addressMatchesData(produced.address(), hubDatum.payout_addr) &&
    payoutDatum !== null &&
    same(payoutDatum.l2_value, withdrawalInfo.body?.l2_value) &&
    same(payoutDatum.l1_address, withdrawalInfo.body?.l1_address) &&
    same(payoutDatum.l1_datum, withdrawalInfo.body?.l1_datum)
  );
};

const scanConsumedEvents = (
  block: WatcherNormalizedL1BlockV1,
  sourceStore: WatcherDurableStoreV1,
  activeEvents: readonly WatcherIndexedUserEventV1[],
  deployment: WatcherDeploymentIdentityPolicyV1,
): Readonly<{
  remaining: readonly WatcherIndexedUserEventV1[];
  terminal: readonly WatcherTerminalUserEventV1[];
}> | null => {
  const active = new Map(activeEvents.map((event) => [event.outRef, event]));
  const terminal: WatcherTerminalUserEventV1[] = [];
  for (const transaction of block.transactions) {
    if (!transaction.isValid) {
      continue;
    }
    const body = canonicalBody(transaction.body.bytesHex);
    if (body === null) {
      return null;
    }
    const inputs = body.inputs();
    const mint = body.mint();
    for (let inputIndex = 0; inputIndex < inputs.len(); inputIndex += 1) {
      const event = active.get(outputReference(inputs.get(inputIndex)));
      if (event === undefined) {
        continue;
      }
      const spendRedeemer = matchingRedeemer(transaction, "spend", inputIndex);
      if (spendRedeemer === null || mint === undefined) {
        return null;
      }
      const policyIndex = mintPolicyIndex(mint, event.policyId);
      const terminalSpend =
        policyIndex < 0
          ? null
          : decodeTerminalSpend(
              event.kind,
              spendRedeemer.bytes.bytesHex,
              inputIndex,
            );
      const mintRedeemer =
        terminalSpend === null
          ? null
          : redeemerAtGlobalIndex(transaction, terminalSpend.mintRedeemerIndex);
      const decodedMint =
        mintRedeemer === null
          ? null
          : decodeMintRedeemer(mintRedeemer.bytes.bytesHex);
      if (terminalSpend === null) {
        return null;
      }
      if (
        !verifyTerminalSemantics(
          event,
          sourceStore,
          transaction,
          body,
          inputIndex,
          terminalSpend,
          deployment,
        )
      ) {
        return null;
      }
      if (
        policyIndex < 0 ||
        mint.get(
          CML.ScriptHash.from_hex(event.policyId),
          CML.AssetName.from_hex(event.assetNameHex),
        ) !== -1n ||
        mint.get_assets(CML.ScriptHash.from_hex(event.policyId))?.len() !== 1 ||
        decodedMint === null ||
        !("BurnEventNFT" in decodedMint) ||
        decodedMint.BurnEventNFT.nonce_asset_name !== event.assetNameHex ||
        decodedMint.BurnEventNFT.witness_unregistration_redeemer_index < 0n
      ) {
        return null;
      }
      const certificateRedeemer = redeemerAtGlobalIndex(
        transaction,
        decodedMint.BurnEventNFT.witness_unregistration_redeemer_index,
      );
      const certificateIndex =
        certificateRedeemer?.purpose === "certificate" &&
        isNatural(certificateRedeemer.index) &&
        BigInt(certificateRedeemer.index) <= BigInt(Number.MAX_SAFE_INTEGER)
          ? Number(certificateRedeemer.index)
          : -1;
      const witnessRedeemer =
        certificateRedeemer === null
          ? null
          : decodeWitnessRedeemer(certificateRedeemer.bytes.bytesHex);
      if (
        registeredScriptHashAt(body, certificateIndex, false) !==
          event.witnessScriptHash ||
        witnessRedeemer === null ||
        !("MintOrBurn" in witnessRedeemer) ||
        witnessRedeemer.MintOrBurn.targetPolicy !== event.policyId
      ) {
        return null;
      }
      active.delete(event.outRef);
      terminal.push(
        Object.freeze({
          ...event,
          terminalStatus: terminalSpend.terminalStatus,
          terminalTransactionHash: transaction.txHash,
          terminalPointDigest: block.chainPoint.pointDigest,
          terminalBlockHash: block.chainPoint.blockHash,
          terminalSlot: block.chainPoint.slot,
          terminalBlockNo: block.chainPoint.blockNo,
          terminalFinalityStatus: "pending",
        }),
      );
    }
  }
  return Object.freeze({
    remaining: Object.freeze(
      [...active.values()].sort((left, right) =>
        left.outRef.localeCompare(right.outRef),
      ),
    ),
    terminal: Object.freeze(
      terminal.sort((left, right) => left.outRef.localeCompare(right.outRef)),
    ),
  });
};

const snapshotWithoutDigest = (
  value: Omit<WatcherUserEventSnapshotV1, "snapshotDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_SNAPSHOT_V1_SCHEMA_VERSION,
  activeEvents: value.activeEvents,
  terminalEvents: value.terminalEvents,
  quarantined: value.quarantined,
});

const makeSnapshot = (
  activeEvents: readonly WatcherIndexedUserEventV1[],
  terminalEvents: readonly WatcherTerminalUserEventV1[],
  quarantined = false,
): WatcherUserEventSnapshotV1 | null => {
  if (
    activeEvents.length > WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.activeEvents ||
    terminalEvents.length > WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.terminalEvents
  ) {
    return null;
  }
  const canonical = snapshotWithoutDigest({
    schemaVersion: WATCHER_USER_EVENT_SNAPSHOT_V1_SCHEMA_VERSION,
    activeEvents: Object.freeze([...activeEvents]),
    terminalEvents: Object.freeze([...terminalEvents]),
    quarantined,
  });
  return Object.freeze({
    ...canonical,
    snapshotDigest: sha256Canonical(canonical),
  });
};

const protocolRole = (
  kind: WatcherUserEventKindV1,
): "deposit" | "withdrawal" | "forced_transaction" =>
  kind === "forced_order" ? "forced_transaction" : kind;

const topologyMatches = (
  store: WatcherDurableStoreV1,
  snapshot: WatcherUserEventSnapshotV1,
): boolean => {
  const durable = store.protocolUtxos
    .filter(({ role }) =>
      ["deposit", "withdrawal", "forced_transaction"].includes(role),
    )
    .sort((left, right) => left.outRef.localeCompare(right.outRef));
  const active = [...snapshot.activeEvents].sort((left, right) =>
    left.outRef.localeCompare(right.outRef),
  );
  return (
    durable.length === active.length &&
    durable.every((utxo, index) => {
      const event = active[index];
      return (
        event !== undefined &&
        utxo.outRef === event.outRef &&
        utxo.role === protocolRole(event.kind) &&
        utxo.chainPointId === event.originChainPointId &&
        utxo.output.cborHex === event.outputCborHex &&
        utxo.output.sha256 === event.outputDigest
      );
    })
  );
};

type VerifiedBlockContext = Readonly<{
  block: WatcherNormalizedL1BlockV1;
  lineageBlocks: readonly WatcherNormalizedL1BlockV1[];
  sourceStore: WatcherDurableStoreV1;
  store: WatcherDurableStoreV1;
  finalityPolicy: WatcherFinalityPolicyV1;
  finalityResult: WatcherFinalityResultV1;
  context: WatcherUserEventPublicContextV1;
}>;

const transportAttestationForProvider = (
  provider: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): WatcherL1TransportAttestationContextV1 | null => {
  if (
    !Array.isArray(transportAttestations) ||
    transportAttestations.length >
      WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.observationsPerFinalityStep
  ) {
    return null;
  }
  const matches = transportAttestations.filter((candidate) => {
    const details = watcherL1TransportAttestationDetailsV1(candidate);
    return details !== null && same(details.provider, provider);
  });
  return matches.length === 1 ? matches[0]! : null;
};

const normalizeTransportAttestedBlock = (
  provider: unknown,
  observation: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): Readonly<{
  block: WatcherNormalizedL1BlockV1;
  transportAttestation: WatcherL1TransportAttestationContextV1;
}> | null => {
  const transportAttestation = transportAttestationForProvider(
    provider,
    transportAttestations,
  );
  if (transportAttestation === null) {
    return null;
  }
  return Object.freeze({
    block: normalizeWatcherL1BlockV1(transportAttestation, observation),
    transportAttestation,
  });
};

const storeDigest = (store: WatcherDurableStoreV1): string =>
  watcherDurableStoreBytesSha256(encodeWatcherDurableStoreV1(store));

const canonicalSuccessor = (
  prior: WatcherUserEventObservationV1,
  next: WatcherNormalizedL1BlockV1,
  lineage: readonly WatcherNormalizedL1BlockV1[],
): boolean => {
  if (
    prior.transitionKind !== "apply_block" ||
    prior.pointDigest === null ||
    prior.blockHash === null ||
    prior.slot === null ||
    prior.blockNo === null
  ) {
    return false;
  }
  if (next.chainPoint.pointDigest === prior.pointDigest) {
    return (
      next.chainPoint.blockHash === prior.blockHash &&
      next.chainPoint.slot === prior.slot &&
      next.chainPoint.blockNo === prior.blockNo
    );
  }
  if (
    BigInt(next.chainPoint.blockNo) <= BigInt(prior.blockNo) ||
    BigInt(next.chainPoint.slot) <= BigInt(prior.slot)
  ) {
    return false;
  }
  const ancestors = new Map<string, WatcherNormalizedL1BlockV1>();
  for (const candidate of lineage) {
    const existing = ancestors.get(candidate.chainPoint.blockHash);
    if (
      existing !== undefined &&
      (existing.chainPoint.pointDigest !== candidate.chainPoint.pointDigest ||
        existing.chainPoint.parentBlockHash !==
          candidate.chainPoint.parentBlockHash)
    ) {
      return false;
    }
    ancestors.set(candidate.chainPoint.blockHash, candidate);
  }
  let cursor = next;
  const visited = new Set<string>();
  while (!visited.has(cursor.chainPoint.blockHash)) {
    visited.add(cursor.chainPoint.blockHash);
    const parentHash = cursor.chainPoint.parentBlockHash;
    if (parentHash === prior.blockHash) {
      return (
        BigInt(cursor.chainPoint.blockNo) === BigInt(prior.blockNo) + 1n &&
        BigInt(cursor.chainPoint.slot) > BigInt(prior.slot)
      );
    }
    if (parentHash === null) {
      return false;
    }
    const parent = ancestors.get(parentHash);
    if (
      parent === undefined ||
      BigInt(cursor.chainPoint.blockNo) !==
        BigInt(parent.chainPoint.blockNo) + 1n ||
      BigInt(cursor.chainPoint.slot) <= BigInt(parent.chainPoint.slot)
    ) {
      return false;
    }
    cursor = parent;
  }
  return false;
};

const canonicalSuccessorAfterRollback = (
  source: WatcherDurableStoreV1,
  next: WatcherNormalizedL1BlockV1,
): boolean => {
  const prior = [...source.chainPoints]
    .filter(
      ({ blockNo, slot }) =>
        BigInt(blockNo) < BigInt(next.chainPoint.blockNo) &&
        BigInt(slot) < BigInt(next.chainPoint.slot),
    )
    .sort((left, right) =>
      BigInt(left.blockNo) < BigInt(right.blockNo) ? 1 : -1,
    )
    .at(0);
  if (
    prior === undefined ||
    BigInt(next.chainPoint.blockNo) !== BigInt(prior.blockNo) + 1n
  ) {
    return false;
  }
  return next.chainPoint.parentBlockHash === prior.blockHash;
};

const verifyDeploymentAuthority = (
  policy: WatcherUserEventIndexerPolicyV1,
  authority: WatcherUserEventDeploymentAuthorityV1,
): VerifiedWatcherDeploymentIdentityV1 | null => {
  try {
    const verified = verifyWatcherDeploymentIdentityV1({
      signedIdentity: authority.signedIdentity,
      policy: authority.policy,
      trustRoots: authority.trustRoots,
      durableMarker: policy.deploymentMarker,
    });
    const applied = authority.policy.appliedScriptHashes;
    return same(verified, authority.result) &&
      verified.network === policy.network &&
      verified.releaseEvidenceDigest === policy.releaseEvidenceDigest &&
      verified.trustRootId === policy.deploymentTrustRootId &&
      same(verified.durableMarker, policy.deploymentMarker) &&
      applied.depositMint === policy.deposit.policyId &&
      applied.depositSpend === policy.deposit.spendScriptHash &&
      applied.withdrawalMint === policy.withdrawal.policyId &&
      applied.withdrawalSpend === policy.withdrawal.spendScriptHash &&
      applied.txOrderMint === policy.forcedOrder.policyId &&
      applied.txOrderSpend === policy.forcedOrder.spendScriptHash
      ? verified
      : null;
  } catch {
    return null;
  }
};

const sameRecordSet = <T>(left: readonly T[], right: readonly T[]): boolean =>
  same(left, right);

const storeTransitionMatches = (
  source: WatcherDurableStoreV1,
  next: WatcherDurableStoreV1,
  block: WatcherNormalizedL1BlockV1,
  snapshot: WatcherUserEventSnapshotV1,
): boolean => {
  if (
    BigInt(next.revision) !== BigInt(source.revision) + 1n ||
    !same(source.deploymentMarker, next.deploymentMarker) ||
    !sameRecordSet(source.daProofInputs, next.daProofInputs) ||
    !sameRecordSet(source.reconstructedStates, next.reconstructedStates) ||
    !sameRecordSet(source.decisions, next.decisions) ||
    !sameRecordSet(source.faults, next.faults) ||
    !sameRecordSet(source.submissions, next.submissions) ||
    !sameRecordSet(source.confirmations, next.confirmations) ||
    !sameRecordSet(source.retries, next.retries) ||
    !sameRecordSet(source.deadlines, next.deadlines) ||
    !sameRecordSet(source.correctionResults, next.correctionResults)
  ) {
    return false;
  }
  const nextObservations = new Map(
    next.l1Observations.map((entry) => [entry.observationId, entry]),
  );
  const alreadyObserved = source.l1Observations.some(
    ({ observationId }) => observationId === block.observationDigest,
  );
  if (
    !source.l1Observations.every((entry) =>
      same(nextObservations.get(entry.observationId), entry),
    ) ||
    next.l1Observations.length !==
      source.l1Observations.length + (alreadyObserved ? 0 : 1)
  ) {
    return false;
  }
  const nextPoints = new Map(
    next.chainPoints.map((entry) => [entry.chainPointId, entry]),
  );
  for (const entry of source.chainPoints) {
    const candidate = nextPoints.get(entry.chainPointId);
    if (
      candidate === undefined ||
      (!same(candidate, entry) &&
        entry.chainPointId !== block.chainPoint.chainPointId)
    ) {
      return false;
    }
  }
  if (
    next.chainPoints.length !==
    source.chainPoints.length +
      (source.chainPoints.some(
        ({ chainPointId }) => chainPointId === block.chainPoint.chainPointId,
      )
        ? 0
        : 1)
  ) {
    return false;
  }
  const eventRoles = new Set(["deposit", "withdrawal", "forced_transaction"]);
  const sourceUnrelated = source.protocolUtxos.filter(
    ({ role }) => !eventRoles.has(role),
  );
  const nextUnrelated = next.protocolUtxos.filter(
    ({ role }) => !eventRoles.has(role),
  );
  try {
    const journal = journalWatcherProtocolUtxoTransitionV1({
      sourceStore: source,
      nextChainPoints: next.chainPoints,
      nextProtocolUtxos: next.protocolUtxos,
      spentAtChainPointId: block.chainPoint.chainPointId,
    });
    return (
      same(sourceUnrelated, nextUnrelated) &&
      same(journal.spentProtocolUtxos, next.spentProtocolUtxos) &&
      topologyMatches(next, snapshot)
    );
  } catch {
    return false;
  }
};

const rollbackSourceExtends = (
  prior: WatcherDurableStoreV1,
  source: WatcherDurableStoreV1,
  recoverableEventOutRefs: ReadonlySet<string> | null = null,
): boolean => {
  const retainsExactRecords = <T>(
    priorRecords: readonly T[],
    sourceRecords: readonly T[],
    keyOf: (record: T) => string,
  ): boolean => {
    const sourceByKey = new Map(
      sourceRecords.map((entry) => [keyOf(entry), entry]),
    );
    return priorRecords.every((entry) =>
      same(sourceByKey.get(keyOf(entry)), entry),
    );
  };
  const eventRoles = new Set(["deposit", "withdrawal", "forced_transaction"]);
  const priorEventUtxos = prior.protocolUtxos.filter(({ role }) =>
    eventRoles.has(role),
  );
  const sourceEventUtxos = source.protocolUtxos.filter(({ role }) =>
    eventRoles.has(role),
  );
  const priorUnrelatedUtxos = prior.protocolUtxos.filter(
    ({ role }) => !eventRoles.has(role),
  );
  const sourceUnrelatedUtxos = source.protocolUtxos.filter(
    ({ role }) => !eventRoles.has(role),
  );
  const priorSpentEventUtxos = prior.spentProtocolUtxos.filter(({ role }) =>
    eventRoles.has(role),
  );
  const sourceSpentEventUtxos = source.spentProtocolUtxos.filter(({ role }) =>
    eventRoles.has(role),
  );
  const priorUnrelatedSpentUtxos = prior.spentProtocolUtxos.filter(
    ({ role }) => !eventRoles.has(role),
  );
  const sourceUnrelatedSpentUtxos = source.spentProtocolUtxos.filter(
    ({ role }) => !eventRoles.has(role),
  );
  const retainsRecoverableEventRecords = <
    T extends { readonly outRef: string },
  >(
    priorRecords: readonly T[],
    sourceRecords: readonly T[],
  ): boolean =>
    retainsExactRecords(priorRecords, sourceRecords, (entry) => entry.outRef) &&
    sourceRecords.every(
      (entry) =>
        priorRecords.some(
          (priorEntry) =>
            priorEntry.outRef === entry.outRef && same(priorEntry, entry),
        ) || recoverableEventOutRefs?.has(entry.outRef) === true,
    );
  return (
    (BigInt(source.revision) > BigInt(prior.revision) || same(source, prior)) &&
    same(source.deploymentMarker, prior.deploymentMarker) &&
    (recoverableEventOutRefs === null
      ? same(sourceEventUtxos, priorEventUtxos) &&
        same(sourceSpentEventUtxos, priorSpentEventUtxos)
      : retainsRecoverableEventRecords(priorEventUtxos, sourceEventUtxos) &&
        retainsRecoverableEventRecords(
          priorSpentEventUtxos,
          sourceSpentEventUtxos,
        )) &&
    retainsExactRecords(
      priorUnrelatedUtxos,
      sourceUnrelatedUtxos,
      (entry) => entry.outRef,
    ) &&
    retainsExactRecords(
      priorUnrelatedSpentUtxos,
      sourceUnrelatedSpentUtxos,
      (entry) => entry.outRef,
    ) &&
    retainsExactRecords(
      prior.daProofInputs,
      source.daProofInputs,
      (entry) => entry.inputId,
    ) &&
    retainsExactRecords(
      prior.reconstructedStates,
      source.reconstructedStates,
      (entry) => entry.blockHash,
    ) &&
    retainsExactRecords(
      prior.decisions,
      source.decisions,
      (entry) => entry.blockHash,
    ) &&
    retainsExactRecords(
      prior.faults,
      source.faults,
      (entry) => entry.faultId,
    ) &&
    retainsExactRecords(
      prior.submissions,
      source.submissions,
      (entry) => entry.submissionId,
    ) &&
    retainsExactRecords(
      prior.confirmations,
      source.confirmations,
      (entry) => entry.confirmationId,
    ) &&
    retainsExactRecords(
      prior.retries,
      source.retries,
      (entry) => entry.retryId,
    ) &&
    retainsExactRecords(
      prior.deadlines,
      source.deadlines,
      (entry) => entry.deadlineId,
    ) &&
    retainsExactRecords(
      prior.correctionResults,
      source.correctionResults,
      (entry) => entry.correctionId,
    ) &&
    retainsExactRecords(
      prior.l1Observations,
      source.l1Observations,
      (entry) => entry.observationId,
    ) &&
    retainsExactRecords(
      prior.chainPoints,
      source.chainPoints,
      (entry) => entry.chainPointId,
    )
  );
};

const parsePublicContext = (
  value: unknown,
): WatcherUserEventPublicContextV1 | null => {
  if (!evidenceWithinBounds(value)) {
    return null;
  }
  const record = exactRecord(value, [
    "schemaVersion",
    "authenticatedProvider",
    "l1Observation",
    "sourceDurableStore",
    "durableStore",
    "deploymentAuthority",
    "rollbackRestoredEventUtxos",
    "finalityAuthority",
    "rollbackAuthority",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_USER_EVENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION
  ) {
    return null;
  }
  const rollbackAuthority =
    record.rollbackAuthority === null
      ? null
      : exactRecord(record.rollbackAuthority, ["result", "context"]);
  if (record.rollbackAuthority !== null && rollbackAuthority === null) {
    return null;
  }
  const finalityAuthority =
    record.finalityAuthority === null
      ? null
      : exactRecord(record.finalityAuthority, [
          "policy",
          "lineage",
          "previousState",
          "observations",
          "consistency",
          "result",
        ]);
  if (
    record.finalityAuthority !== null &&
    (finalityAuthority === null ||
      !Array.isArray(finalityAuthority.lineage) ||
      finalityAuthority.lineage.length >
        WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.finalityLineageSteps ||
      !Array.isArray(finalityAuthority.observations) ||
      finalityAuthority.observations.length >
        WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.observationsPerFinalityStep)
  ) {
    return null;
  }
  const finalityObservations: {
    authenticatedProvider: unknown;
    l1Observation: unknown;
  }[] = [];
  const finalityLineage: {
    observations: readonly {
      authenticatedProvider: unknown;
      l1Observation: unknown;
    }[];
    consistency: unknown;
    result: unknown;
  }[] = [];
  if (finalityAuthority !== null) {
    for (const candidate of finalityAuthority.lineage as readonly unknown[]) {
      const step = exactRecord(candidate, [
        "observations",
        "consistency",
        "result",
      ]);
      if (
        step === null ||
        !Array.isArray(step.observations) ||
        step.observations.length >
          WATCHER_USER_EVENT_INDEXER_V1_BOUNDS.observationsPerFinalityStep
      ) {
        return null;
      }
      const observations: {
        authenticatedProvider: unknown;
        l1Observation: unknown;
      }[] = [];
      for (const stepCandidate of step.observations) {
        const observation = exactRecord(stepCandidate, [
          "authenticatedProvider",
          "l1Observation",
        ]);
        if (observation === null) {
          return null;
        }
        observations.push({
          authenticatedProvider: observation.authenticatedProvider,
          l1Observation: observation.l1Observation,
        });
      }
      finalityLineage.push({
        observations: Object.freeze(observations),
        consistency: step.consistency,
        result: step.result,
      });
    }
    for (const candidate of finalityAuthority.observations as readonly unknown[]) {
      const observation = exactRecord(candidate, [
        "authenticatedProvider",
        "l1Observation",
      ]);
      if (observation === null) {
        return null;
      }
      finalityObservations.push({
        authenticatedProvider: observation.authenticatedProvider,
        l1Observation: observation.l1Observation,
      });
    }
  }
  const deploymentAuthority = exactRecord(record.deploymentAuthority, [
    "signedIdentity",
    "policy",
    "trustRoots",
    "result",
  ]);
  if (
    deploymentAuthority === null ||
    !Array.isArray(deploymentAuthority.trustRoots) ||
    !Array.isArray(record.rollbackRestoredEventUtxos)
  ) {
    return null;
  }
  return Object.freeze({
    schemaVersion: WATCHER_USER_EVENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
    authenticatedProvider: record.authenticatedProvider,
    l1Observation: record.l1Observation,
    sourceDurableStore: record.sourceDurableStore,
    durableStore: record.durableStore,
    deploymentAuthority: {
      signedIdentity: deploymentAuthority.signedIdentity,
      policy: deploymentAuthority.policy as WatcherDeploymentIdentityPolicyV1,
      trustRoots:
        deploymentAuthority.trustRoots as readonly WatcherDeploymentTrustRootV1[],
      result: deploymentAuthority.result as VerifiedWatcherDeploymentIdentityV1,
    },
    rollbackRestoredEventUtxos:
      record.rollbackRestoredEventUtxos as readonly unknown[],
    finalityAuthority:
      finalityAuthority === null
        ? null
        : {
            policy: finalityAuthority.policy,
            lineage: Object.freeze(finalityLineage),
            previousState: finalityAuthority.previousState,
            observations: Object.freeze(finalityObservations),
            consistency: finalityAuthority.consistency,
            result: finalityAuthority.result,
          },
    rollbackAuthority:
      rollbackAuthority === null
        ? null
        : {
            result: rollbackAuthority.result,
            context:
              rollbackAuthority.context as WatcherRollbackVerificationContextV1,
          },
  });
};

const verifyBlockContext = (
  policy: WatcherUserEventIndexerPolicyV1,
  rawContext: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): VerifiedBlockContext | null => {
  const context = parsePublicContext(rawContext);
  if (
    context === null ||
    context.authenticatedProvider === null ||
    context.l1Observation === null ||
    context.finalityAuthority === null ||
    context.rollbackAuthority !== null ||
    context.rollbackRestoredEventUtxos.length !== 0
  ) {
    return null;
  }
  try {
    const normalizedBlock = normalizeTransportAttestedBlock(
      context.authenticatedProvider,
      context.l1Observation,
      transportAttestations,
    );
    if (normalizedBlock === null) {
      return null;
    }
    const block = normalizedBlock.block;
    const sourceStore = parseWatcherDurableStoreV1(context.sourceDurableStore);
    const store = parseWatcherDurableStoreV1(context.durableStore);
    const finalityPolicy = parseWatcherFinalityPolicyV1(
      context.finalityAuthority.policy,
    );
    if (
      finalityPolicy === null ||
      verifyDeploymentAuthority(policy, context.deploymentAuthority) === null ||
      finalityPolicy.network !== policy.network ||
      finalityPolicy.releaseEvidenceDigest !== policy.releaseEvidenceDigest ||
      !same(finalityPolicy.deploymentMarker, policy.deploymentMarker) ||
      finalityPolicy.confirmationDepth !== policy.requiredFinalityDepth
    ) {
      return null;
    }
    if (
      finalityPolicy.sourceMode === "local_node" &&
      (finalityPolicy.authorityNodeId === null ||
        finalityPolicy.authorityGenesisIdentitySha256 === null)
    ) {
      return null;
    }
    let replayedState: unknown = null;
    const lineageBlocks: WatcherNormalizedL1BlockV1[] = [];
    for (const step of context.finalityAuthority.lineage) {
      const normalizedStepWithAttestations = step.observations.map(
        ({ authenticatedProvider, l1Observation }) =>
          normalizeTransportAttestedBlock(
            authenticatedProvider,
            l1Observation,
            transportAttestations,
          ),
      );
      if (normalizedStepWithAttestations.some((entry) => entry === null)) {
        return null;
      }
      const normalizedStep = normalizedStepWithAttestations.map(
        (entry) => entry!.block,
      );
      lineageBlocks.push(...normalizedStep);
      const stepConsistency = evaluateWatcherMultiProviderConsistencyV1(
        watcherFinalityConfiguredSourceV1(finalityPolicy),
        normalizedStep,
        normalizedStepWithAttestations.map(
          (entry) => entry!.transportAttestation,
        ),
      );
      const stepResult = evaluateWatcherFinalityV1(
        finalityPolicy,
        replayedState,
        stepConsistency,
      );
      if (
        !same(stepConsistency, step.consistency) ||
        !same(stepResult, step.result) ||
        stepResult.state === null
      ) {
        return null;
      }
      replayedState = stepResult.state;
    }
    if (!same(replayedState, context.finalityAuthority.previousState)) {
      return null;
    }
    const normalizedWithAttestations =
      context.finalityAuthority.observations.map(
        ({ authenticatedProvider, l1Observation }) =>
          normalizeTransportAttestedBlock(
            authenticatedProvider,
            l1Observation,
            transportAttestations,
          ),
      );
    if (normalizedWithAttestations.some((entry) => entry === null)) {
      return null;
    }
    const normalized = normalizedWithAttestations.map((entry) => entry!.block);
    const consistency = evaluateWatcherMultiProviderConsistencyV1(
      watcherFinalityConfiguredSourceV1(finalityPolicy),
      normalized,
      normalizedWithAttestations.map((entry) => entry!.transportAttestation),
    );
    const finalityResult = evaluateWatcherFinalityV1(
      finalityPolicy,
      context.finalityAuthority.previousState,
      consistency,
    );
    const bound =
      finalityResult.state?.finalized ?? finalityResult.state?.pending;
    const source = block.provider.source;
    const sourceMatchesFinality =
      source.sourceMode === finalityPolicy.sourceMode &&
      (source.sourceMode === "local_node"
        ? source.surface === "chain_sync" &&
          source.authorityNodeId === finalityPolicy.authorityNodeId &&
          block.provider.authentication.kind === "cardano_node_genesis_v1" &&
          block.provider.authentication.publicIdentitySha256 ===
            finalityPolicy.authorityGenesisIdentitySha256 &&
          consistency.chainAuthorityObservationDigest ===
            block.observationDigest
        : consistency.observationEvidenceDigests.includes(
            block.observationDigest,
          ));
    if (
      !same(consistency, context.finalityAuthority.consistency) ||
      !same(finalityResult, context.finalityAuthority.result) ||
      !["hold", "finality_granted", "rewind_required"].includes(
        finalityResult.protocolDecision,
      ) ||
      ![
        "observe_pending",
        "advance_pending",
        "finalize",
        "duplicate",
        "rewind_pending",
      ].includes(finalityResult.action) ||
      !sourceMatchesFinality ||
      !normalized.some(
        (candidate) => candidate.observationDigest === block.observationDigest,
      ) ||
      bound?.pointDigest !== block.chainPoint.pointDigest ||
      bound.blockContentDigest !== block.blockContentDigest
    ) {
      return null;
    }
    const persistedBytes =
      encodeWatcherNormalizedL1BlockV1(block).toString("hex");
    if (
      block.network !== policy.network ||
      !same(sourceStore.deploymentMarker, policy.deploymentMarker) ||
      !same(store.deploymentMarker, policy.deploymentMarker) ||
      store.l1Observations.filter(
        ({ providerId, chainPointId, payload }) =>
          providerId === block.provider.providerId &&
          chainPointId === block.chainPoint.chainPointId &&
          payload.cborHex === persistedBytes &&
          payload.sha256 === sha256Bytes(Buffer.from(persistedBytes, "hex")),
      ).length !== 1 ||
      store.chainPoints.filter(
        (point) =>
          point.providerId === block.provider.providerId &&
          point.chainPointId === block.chainPoint.chainPointId &&
          point.blockHash === block.chainPoint.blockHash &&
          point.slot === block.chainPoint.slot &&
          point.blockNo === block.chainPoint.blockNo &&
          point.depth === block.chainPoint.depth,
      ).length !== 1
    ) {
      return null;
    }
    return Object.freeze({
      block,
      lineageBlocks: Object.freeze(lineageBlocks),
      sourceStore,
      store,
      finalityPolicy,
      finalityResult,
      context,
    });
  } catch {
    return null;
  }
};

const withCurrentFinality = (
  currentPointDigest: string,
  finalityGranted: boolean,
  events: readonly WatcherIndexedUserEventV1[],
): readonly WatcherIndexedUserEventV1[] =>
  Object.freeze(
    events.map((event) => ({
      ...event,
      finalityStatus:
        finalityGranted && event.originPointDigest === currentPointDigest
          ? "final"
          : event.finalityStatus,
    })),
  );

const withCurrentTerminalFinality = (
  currentPointDigest: string,
  finalityGranted: boolean,
  events: readonly WatcherTerminalUserEventV1[],
): readonly WatcherTerminalUserEventV1[] =>
  Object.freeze(
    events.map((event) => ({
      ...event,
      terminalFinalityStatus:
        finalityGranted && event.terminalPointDigest === currentPointDigest
          ? "final"
          : event.terminalFinalityStatus,
    })),
  );

const observationWithoutDigest = (
  value: Omit<WatcherUserEventObservationV1, "observationDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_OBSERVATION_V1_SCHEMA_VERSION,
  policyDigest: value.policyDigest,
  network: value.network,
  releaseEvidenceDigest: value.releaseEvidenceDigest,
  deploymentMarker: value.deploymentMarker,
  transitionKind: value.transitionKind,
  pointDigest: value.pointDigest,
  blockHash: value.blockHash,
  slot: value.slot,
  blockNo: value.blockNo,
  sourceObservationDigest: value.sourceObservationDigest,
  chainPointId: value.chainPointId,
  sourceDurableStoreDigest: value.sourceDurableStoreDigest,
  sourceDurableStoreRevision: value.sourceDurableStoreRevision,
  durableStoreDigest: value.durableStoreDigest,
  durableStoreRevision: value.durableStoreRevision,
  rollbackTargetEntryDigest: value.rollbackTargetEntryDigest,
  snapshot: value.snapshot,
});

const makeObservation = (
  value: Omit<WatcherUserEventObservationV1, "observationDigest">,
): WatcherUserEventObservationV1 => {
  const canonical = observationWithoutDigest(value);
  return Object.freeze({
    ...canonical,
    observationDigest: sha256Canonical(canonical),
  });
};

const deriveBlockObservation = (
  policy: WatcherUserEventIndexerPolicyV1,
  previous: WatcherUserEventIndexerStateV1 | null,
  rawContext: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): WatcherUserEventObservationV1 | null => {
  const verified = verifyBlockContext(
    policy,
    rawContext,
    transportAttestations,
  );
  if (verified === null) {
    return null;
  }
  if (previous?.snapshot.quarantined === true) {
    return null;
  }
  const sourceDigest = storeDigest(verified.sourceStore);
  const priorEntry =
    previous === null
      ? null
      : ([...previous.activeEntryDigests]
          .reverse()
          .map((digest) =>
            previous.history.find(({ entryDigest }) => entryDigest === digest),
          )
          .find(
            (entry) => entry?.observation.transitionKind === "apply_block",
          ) ?? null);
  const followsRollback =
    previous?.history.at(-1)?.observation.transitionKind === "rollback";
  let priorStore: WatcherDurableStoreV1 | null = null;
  try {
    priorStore =
      previous === null
        ? null
        : parseWatcherDurableStoreV1(
            previous.history.at(-1)?.publicContext.durableStore,
          );
  } catch {
    return null;
  }
  if (
    sourceDigest !==
      (previous?.durableStoreDigest ?? policy.bootstrapStoreDigest) ||
    verified.sourceStore.revision !== (previous?.durableStoreRevision ?? "0") ||
    (priorStore !== null && !same(priorStore, verified.sourceStore)) ||
    (followsRollback &&
      !canonicalSuccessorAfterRollback(verified.sourceStore, verified.block)) ||
    (!followsRollback &&
      priorEntry !== null &&
      !canonicalSuccessor(
        priorEntry.observation,
        verified.block,
        verified.lineageBlocks,
      ))
  ) {
    return null;
  }
  const priorActive = previous?.snapshot.activeEvents ?? [];
  const consumed = scanConsumedEvents(
    verified.block,
    verified.sourceStore,
    priorActive,
    verified.context.deploymentAuthority.policy,
  );
  const created = scanCreatedEvents(
    policy,
    verified.block,
    verified.sourceStore,
    verified.context.deploymentAuthority.policy,
  );
  if (consumed === null || created === null) {
    return null;
  }
  const activeByOutRef = new Map(
    consumed.remaining.map((event) => [event.outRef, event]),
  );
  const activeEventIds = new Set(
    consumed.remaining.map((event) => event.eventId),
  );
  for (const event of created) {
    const existing = activeByOutRef.get(event.outRef);
    if (existing !== undefined) {
      if (
        same(
          {
            ...existing,
            originChainPointId: "",
            finalityStatus: "pending",
          },
          {
            ...event,
            originChainPointId: "",
            finalityStatus: "pending",
          },
        )
      ) {
        continue;
      }
      return null;
    }
    if (
      activeEventIds.has(event.eventId) ||
      (previous?.snapshot.terminalEvents ?? []).some(
        (prior) => prior.eventId === event.eventId,
      )
    ) {
      return null;
    }
    activeByOutRef.set(event.outRef, event);
    activeEventIds.add(event.eventId);
  }
  const active = withCurrentFinality(
    verified.block.chainPoint.pointDigest,
    verified.finalityResult.protocolDecision === "finality_granted",
    [...activeByOutRef.values()].sort((left, right) =>
      left.outRef.localeCompare(right.outRef),
    ),
  );
  const terminal = withCurrentTerminalFinality(
    verified.block.chainPoint.pointDigest,
    verified.finalityResult.protocolDecision === "finality_granted",
    [...(previous?.snapshot.terminalEvents ?? []), ...consumed.terminal].sort(
      (left, right) =>
        `${left.terminalPointDigest}:${left.outRef}`.localeCompare(
          `${right.terminalPointDigest}:${right.outRef}`,
        ),
    ),
  );
  const snapshot = makeSnapshot(active, terminal);
  if (
    snapshot === null ||
    !storeTransitionMatches(
      verified.sourceStore,
      verified.store,
      verified.block,
      snapshot,
    )
  ) {
    return null;
  }
  return makeObservation({
    schemaVersion: WATCHER_USER_EVENT_OBSERVATION_V1_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    transitionKind: "apply_block",
    pointDigest: verified.block.chainPoint.pointDigest,
    blockHash: verified.block.chainPoint.blockHash,
    slot: verified.block.chainPoint.slot,
    blockNo: verified.block.chainPoint.blockNo,
    sourceObservationDigest: verified.block.observationDigest,
    chainPointId: verified.block.chainPoint.chainPointId,
    sourceDurableStoreDigest: sourceDigest,
    sourceDurableStoreRevision: verified.sourceStore.revision,
    durableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(verified.store),
    ),
    durableStoreRevision: verified.store.revision,
    rollbackTargetEntryDigest: null,
    snapshot,
  });
};

type VerifiedRollbackContextBase = Readonly<{
  sourceStore: WatcherDurableStoreV1;
  store: WatcherDurableStoreV1;
  context: WatcherUserEventPublicContextV1;
}>;
type VerifiedRollbackContext =
  | (VerifiedRollbackContextBase &
      Readonly<{
        kind: "pre_finality_rollback";
        result: WatcherRollbackResultV1;
      }>)
  | (VerifiedRollbackContextBase &
      Readonly<{
        kind: "post_finality_recovery";
        result: WatcherPostFinalityRecoveryResultV1;
      }>);

const verifyRollbackContext = (
  policy: WatcherUserEventIndexerPolicyV1,
  rawContext: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): VerifiedRollbackContext | null => {
  const context = parsePublicContext(rawContext);
  if (
    context === null ||
    context.authenticatedProvider !== null ||
    context.l1Observation !== null ||
    context.finalityAuthority !== null ||
    context.rollbackAuthority === null ||
    verifyDeploymentAuthority(policy, context.deploymentAuthority) === null
  ) {
    return null;
  }
  try {
    const rollbackResult = parseWatcherRollbackResultV1(
      context.rollbackAuthority.result,
      {
        ...(context.rollbackAuthority
          .context as WatcherRollbackVerificationContextV1),
        transportAttestations,
      },
    );
    const recoveryResult =
      rollbackResult === null
        ? parseWatcherPostFinalityRecoveryResultV1(
            context.rollbackAuthority.result,
            {
              ...(context.rollbackAuthority
                .context as WatcherPostFinalityRecoveryInputV1),
              transportAttestations,
            },
          )
        : null;
    const result = rollbackResult ?? recoveryResult;
    if (
      result === null ||
      (rollbackResult !== null
        ? !["apply_rewind", "quarantine_incident"].includes(result.action)
        : result.action !== "rewind_and_replay") ||
      result.nextStore === null
    ) {
      return null;
    }
    const sourceStore = parseWatcherDurableStoreV1(context.sourceDurableStore);
    const store = parseWatcherDurableStoreV1(context.durableStore);
    if (
      !same(sourceStore, context.rollbackAuthority.context.sourceStore) ||
      storeDigest(sourceStore) !== result.sourceStoreDigest ||
      !same(store, result.nextStore) ||
      !same(store.deploymentMarker, policy.deploymentMarker) ||
      (rollbackResult === null &&
        (recoveryResult === null ||
          recoveryResult.recoveryState === null ||
          recoveryResult.resumableFinalityState === null ||
          recoveryResult.recoveryState.network !== policy.network ||
          recoveryResult.recoveryState.releaseEvidenceDigest !==
            policy.releaseEvidenceDigest ||
          !same(
            recoveryResult.recoveryState.deploymentMarker,
            policy.deploymentMarker,
          )))
    ) {
      return null;
    }
    return rollbackResult !== null
      ? Object.freeze({
          kind: "pre_finality_rollback" as const,
          result: rollbackResult,
          sourceStore,
          store,
          context,
        })
      : Object.freeze({
          kind: "post_finality_recovery" as const,
          result: recoveryResult!,
          sourceStore,
          store,
          context,
        });
  } catch {
    return null;
  }
};

const historyEntryForDigest = (
  state: WatcherUserEventIndexerStateV1,
  digest: string,
): WatcherUserEventHistoryEntryV1 | null =>
  state.history.find(({ entryDigest }) => entryDigest === digest) ?? null;

const deriveRollbackObservation = (
  policy: WatcherUserEventIndexerPolicyV1,
  previous: WatcherUserEventIndexerStateV1,
  rawContext: unknown,
  requestedRollbackTargetEntryDigest: string | null,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): WatcherUserEventObservationV1 | null => {
  const verified = verifyRollbackContext(
    policy,
    rawContext,
    transportAttestations,
  );
  if (verified === null) {
    return null;
  }
  let previousStore: WatcherDurableStoreV1;
  try {
    previousStore = parseWatcherDurableStoreV1(
      previous.history.at(-1)?.publicContext.durableStore,
    );
  } catch {
    return null;
  }
  if (
    storeDigest(previousStore) !== previous.durableStoreDigest ||
    !rollbackSourceExtends(
      previousStore,
      verified.sourceStore,
      verified.kind === "post_finality_recovery"
        ? new Set(verified.result.removedRecords.protocolUtxoOutRefs)
        : null,
    )
  ) {
    return null;
  }
  if (
    verified.kind === "pre_finality_rollback" &&
    verified.result.action === "quarantine_incident"
  ) {
    const snapshot = makeSnapshot(
      previous.snapshot.activeEvents,
      previous.snapshot.terminalEvents,
      true,
    );
    return snapshot === null
      ? null
      : makeObservation({
          schemaVersion: WATCHER_USER_EVENT_OBSERVATION_V1_SCHEMA_VERSION,
          policyDigest: policy.policyDigest,
          network: policy.network,
          releaseEvidenceDigest: policy.releaseEvidenceDigest,
          deploymentMarker: policy.deploymentMarker,
          transitionKind: "rollback",
          pointDigest: null,
          blockHash: null,
          slot: null,
          blockNo: null,
          sourceObservationDigest: null,
          chainPointId: null,
          sourceDurableStoreDigest: storeDigest(verified.sourceStore),
          sourceDurableStoreRevision: verified.sourceStore.revision,
          durableStoreDigest: watcherDurableStoreBytesSha256(
            encodeWatcherDurableStoreV1(verified.store),
          ),
          durableStoreRevision: verified.store.revision,
          rollbackTargetEntryDigest: null,
          snapshot,
        });
  }
  const activeEntries = previous.activeEntryDigests
    .map((digest) => historyEntryForDigest(previous, digest))
    .filter(
      (entry): entry is WatcherUserEventHistoryEntryV1 =>
        entry !== null && entry.observation.transitionKind === "apply_block",
    );
  const commonAncestorBlockNo =
    verified.kind === "post_finality_recovery"
      ? (verified.result.recoveryState?.path.commonAncestorBlockNo ?? null)
      : null;
  const target =
    verified.kind === "post_finality_recovery"
      ? commonAncestorBlockNo === null
        ? null
        : (activeEntries
            .filter(
              (entry) =>
                entry.observation.blockNo !== null &&
                BigInt(entry.observation.blockNo) <=
                  BigInt(commonAncestorBlockNo),
            )
            .at(-1) ?? null)
      : requestedRollbackTargetEntryDigest === null
        ? null
        : historyEntryForDigest(previous, requestedRollbackTargetEntryDigest);
  const rollbackTargetEntryDigest = target?.entryDigest ?? null;
  if (
    verified.kind === "pre_finality_rollback" &&
    (rollbackTargetEntryDigest === null ||
      !previous.activeEntryDigests.includes(rollbackTargetEntryDigest))
  ) {
    return null;
  }
  const removedPoints = new Set(verified.result.removedRecords.chainPointIds);
  const removedObservations = new Set(
    verified.result.removedRecords.l1ObservationIds,
  );
  const removed = (entry: WatcherUserEventHistoryEntryV1): boolean =>
    (entry.observation.chainPointId !== null &&
      removedPoints.has(entry.observation.chainPointId)) ||
    (entry.observation.sourceObservationDigest !== null &&
      removedObservations.has(entry.observation.sourceObservationDigest));
  const retained =
    verified.kind === "post_finality_recovery"
      ? activeEntries.filter(
          (entry) =>
            entry.observation.blockNo !== null &&
            commonAncestorBlockNo !== null &&
            BigInt(entry.observation.blockNo) <= BigInt(commonAncestorBlockNo),
        )
      : activeEntries.filter((entry) => !removed(entry));
  const orphaned =
    verified.kind === "post_finality_recovery"
      ? activeEntries.slice(retained.length)
      : activeEntries.filter((entry) => removed(entry));
  if (
    (target !== null && target.observation.transitionKind !== "apply_block") ||
    (target === null && retained.length !== 0) ||
    retained.at(-1)?.entryDigest !== rollbackTargetEntryDigest ||
    (verified.kind === "post_finality_recovery" &&
      (retained.some((entry) => removed(entry)) ||
        orphaned.some((entry) => !removed(entry))))
  ) {
    return null;
  }
  let targetStore: WatcherDurableStoreV1 | null = null;
  if (target !== null) {
    try {
      targetStore = parseWatcherDurableStoreV1(
        target.publicContext.durableStore,
      );
    } catch {
      return null;
    }
  }
  const sourceOutRefs = new Set(
    verified.sourceStore.protocolUtxos.map(({ outRef }) => outRef),
  );
  const restored = (targetStore?.protocolUtxos ?? []).filter(
    ({ outRef, role }) =>
      ["deposit", "withdrawal", "forced_transaction"].includes(role) &&
      !sourceOutRefs.has(outRef) &&
      verified.store.protocolUtxos.some(
        (candidate) => candidate.outRef === outRef,
      ) &&
      target?.observation.snapshot.activeEvents.some(
        (event) => event.outRef === outRef,
      ) === true,
  );
  const targetSnapshot =
    target?.observation.snapshot ?? makeSnapshot([], [], false);
  if (
    targetSnapshot === null ||
    !same(restored, verified.context.rollbackRestoredEventUtxos) ||
    !topologyMatches(verified.store, targetSnapshot)
  ) {
    return null;
  }
  const removedOutRefs = new Set(
    verified.result.removedRecords.protocolUtxoOutRefs,
  );
  const targetActive = new Set(
    targetSnapshot.activeEvents.map(({ outRef }) => outRef),
  );
  for (const event of previous.snapshot.activeEvents) {
    if (!targetActive.has(event.outRef) && !removedOutRefs.has(event.outRef)) {
      return null;
    }
  }
  return makeObservation({
    schemaVersion: WATCHER_USER_EVENT_OBSERVATION_V1_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    transitionKind: "rollback",
    pointDigest: null,
    blockHash: null,
    slot: null,
    blockNo: null,
    sourceObservationDigest: null,
    chainPointId: null,
    sourceDurableStoreDigest: storeDigest(verified.sourceStore),
    sourceDurableStoreRevision: verified.sourceStore.revision,
    durableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(verified.store),
    ),
    durableStoreRevision: verified.store.revision,
    rollbackTargetEntryDigest,
    snapshot: targetSnapshot,
  });
};

export const deriveWatcherUserEventObservationV1 = (
  rawPolicy: unknown,
  rawPreviousState: unknown,
  rawPublicContext: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
  rollbackTargetEntryDigest: string | null = null,
): WatcherUserEventObservationV1 | null => {
  const evidenceBudget: EvidenceGraphBudget = { nodes: 0, bytes: 0 };
  if (
    !evidenceWithinBounds(rawPolicy, evidenceBudget) ||
    (rawPreviousState !== null &&
      !evidenceWithinBounds(rawPreviousState, evidenceBudget)) ||
    !evidenceWithinBounds(rawPublicContext, evidenceBudget)
  ) {
    return null;
  }
  const policy = parseWatcherUserEventIndexerPolicyV1(rawPolicy);
  if (policy === null) {
    return null;
  }
  const previous =
    rawPreviousState === null
      ? null
      : parseWatcherUserEventIndexerStateV1(
          rawPreviousState,
          policy,
          transportAttestations,
        );
  if (rawPreviousState !== null && previous === null) {
    return null;
  }
  const context = parsePublicContext(rawPublicContext);
  if (context === null) {
    return null;
  }
  return context.rollbackAuthority === null
    ? deriveBlockObservation(policy, previous, context, transportAttestations)
    : previous === null
      ? null
      : deriveRollbackObservation(
          policy,
          previous,
          context,
          rollbackTargetEntryDigest,
          transportAttestations,
        );
};

const historyEntryWithoutDigest = (
  value: Omit<WatcherUserEventHistoryEntryV1, "entryDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_HISTORY_ENTRY_V1_SCHEMA_VERSION,
  predecessorStateDigest: value.predecessorStateDigest,
  observation: value.observation,
  publicContext: value.publicContext,
});

const stateWithoutDigest = (
  value: Omit<WatcherUserEventIndexerStateV1, "stateDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_INDEXER_STATE_V1_SCHEMA_VERSION,
  policyDigest: value.policyDigest,
  network: value.network,
  releaseEvidenceDigest: value.releaseEvidenceDigest,
  deploymentMarker: value.deploymentMarker,
  durableStoreDigest: value.durableStoreDigest,
  durableStoreRevision: value.durableStoreRevision,
  snapshot: value.snapshot,
  history: value.history,
  activeEntryDigests: value.activeEntryDigests,
});

const applyObservation = (
  policy: WatcherUserEventIndexerPolicyV1,
  previous: WatcherUserEventIndexerStateV1 | null,
  observation: WatcherUserEventObservationV1,
  publicContext: WatcherUserEventPublicContextV1,
): WatcherUserEventIndexerStateV1 | null => {
  const entryCanonical = historyEntryWithoutDigest({
    schemaVersion: WATCHER_USER_EVENT_HISTORY_ENTRY_V1_SCHEMA_VERSION,
    predecessorStateDigest: previous?.stateDigest ?? null,
    observation,
    publicContext,
  });
  const entry = Object.freeze({
    ...entryCanonical,
    entryDigest: sha256Canonical(entryCanonical),
  });
  const history = Object.freeze([...(previous?.history ?? []), entry]);
  let activeEntryDigests =
    observation.transitionKind === "rollback"
      ? observation.rollbackTargetEntryDigest === null
        ? observation.snapshot.quarantined
          ? [...(previous?.activeEntryDigests ?? [])]
          : []
        : (previous?.activeEntryDigests.slice(
            0,
            previous.activeEntryDigests.indexOf(
              observation.rollbackTargetEntryDigest,
            ) + 1,
          ) ?? [])
      : [...(previous?.activeEntryDigests ?? [])];
  activeEntryDigests = [...activeEntryDigests, entry.entryDigest];
  if (
    history.length > BigInt(policy.maximumAuditHistoryEntries) ||
    activeEntryDigests.length > BigInt(policy.maximumActiveHistoryEntries)
  ) {
    return null;
  }
  const canonical = stateWithoutDigest({
    schemaVersion: WATCHER_USER_EVENT_INDEXER_STATE_V1_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    durableStoreDigest: observation.durableStoreDigest,
    durableStoreRevision: observation.durableStoreRevision,
    snapshot: observation.snapshot,
    history,
    activeEntryDigests: Object.freeze(activeEntryDigests),
  });
  return immutableWireValue({
    ...canonical,
    stateDigest: sha256Canonical(canonical),
  });
};

const parseObservationStructural = (
  value: unknown,
): WatcherUserEventObservationV1 | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "policyDigest",
    "network",
    "releaseEvidenceDigest",
    "deploymentMarker",
    "transitionKind",
    "pointDigest",
    "blockHash",
    "slot",
    "blockNo",
    "sourceObservationDigest",
    "chainPointId",
    "sourceDurableStoreDigest",
    "sourceDurableStoreRevision",
    "durableStoreDigest",
    "durableStoreRevision",
    "rollbackTargetEntryDigest",
    "snapshot",
    "observationDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_USER_EVENT_OBSERVATION_V1_SCHEMA_VERSION ||
    !isHex32(record.policyDigest) ||
    !isNetwork(record.network) ||
    !isHex32(record.releaseEvidenceDigest) ||
    cloneMarker(record.deploymentMarker) === null ||
    !["apply_block", "rollback"].includes(String(record.transitionKind)) ||
    !isHex32(record.sourceDurableStoreDigest) ||
    !isNatural(record.sourceDurableStoreRevision) ||
    !isHex32(record.durableStoreDigest) ||
    !isNatural(record.durableStoreRevision) ||
    !isHex32(record.observationDigest)
  ) {
    return null;
  }
  const candidate = value as WatcherUserEventObservationV1;
  const canonical = observationWithoutDigest(candidate);
  return sha256Canonical(canonical) === candidate.observationDigest
    ? candidate
    : null;
};

export const parseWatcherUserEventIndexerStateV1 = (
  value: unknown,
  rawPolicy: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): WatcherUserEventIndexerStateV1 | null => {
  const evidenceBudget: EvidenceGraphBudget = { nodes: 0, bytes: 0 };
  if (
    !evidenceWithinBounds(rawPolicy, evidenceBudget) ||
    !evidenceWithinBounds(value, evidenceBudget)
  ) {
    return null;
  }
  const policy = parseWatcherUserEventIndexerPolicyV1(rawPolicy);
  const record = exactRecord(value, [
    "schemaVersion",
    "policyDigest",
    "network",
    "releaseEvidenceDigest",
    "deploymentMarker",
    "durableStoreDigest",
    "durableStoreRevision",
    "snapshot",
    "history",
    "activeEntryDigests",
    "stateDigest",
  ]);
  if (
    policy === null ||
    record === null ||
    record.schemaVersion !==
      WATCHER_USER_EVENT_INDEXER_STATE_V1_SCHEMA_VERSION ||
    !Array.isArray(record.history) ||
    !Array.isArray(record.activeEntryDigests) ||
    !isHex32(record.stateDigest) ||
    !isHex32(record.durableStoreDigest) ||
    !isNatural(record.durableStoreRevision) ||
    record.policyDigest !== policy.policyDigest ||
    record.network !== policy.network ||
    record.releaseEvidenceDigest !== policy.releaseEvidenceDigest ||
    !same(record.deploymentMarker, policy.deploymentMarker)
  ) {
    return null;
  }
  let replay: WatcherUserEventIndexerStateV1 | null = null;
  for (const rawEntry of record.history) {
    const entryRecord = exactRecord(rawEntry, [
      "schemaVersion",
      "predecessorStateDigest",
      "observation",
      "publicContext",
      "entryDigest",
    ]);
    const observation =
      entryRecord === null
        ? null
        : parseObservationStructural(entryRecord.observation);
    const publicContext =
      entryRecord === null
        ? null
        : parsePublicContext(entryRecord.publicContext);
    if (
      entryRecord === null ||
      observation === null ||
      publicContext === null ||
      entryRecord.schemaVersion !==
        WATCHER_USER_EVENT_HISTORY_ENTRY_V1_SCHEMA_VERSION ||
      entryRecord.predecessorStateDigest !== (replay?.stateDigest ?? null) ||
      !isHex32(entryRecord.entryDigest)
    ) {
      return null;
    }
    const expectedObservation =
      publicContext.rollbackAuthority === null
        ? deriveBlockObservation(
            policy,
            replay,
            publicContext,
            transportAttestations,
          )
        : replay === null
          ? null
          : deriveRollbackObservation(
              policy,
              replay,
              publicContext,
              observation.rollbackTargetEntryDigest,
              transportAttestations,
            );
    if (
      expectedObservation === null ||
      !same(expectedObservation, observation)
    ) {
      return null;
    }
    replay = applyObservation(policy, replay, observation, publicContext);
    if (
      replay === null ||
      replay.history.at(-1)?.entryDigest !== entryRecord.entryDigest
    ) {
      return null;
    }
  }
  return replay !== null && same(replay, value) ? replay : null;
};

const result = (
  action: WatcherUserEventIndexerResultV1["action"],
  protocolDecision: WatcherUserEventIndexerResultV1["protocolDecision"],
  reasonCodes: readonly WatcherUserEventIndexerReasonCodeV1[],
  alertCodes: readonly WatcherUserEventIndexerAlertCodeV1[],
  state: WatcherUserEventIndexerStateV1 | null,
): WatcherUserEventIndexerResultV1 => {
  const canonical = {
    schemaVersion: WATCHER_USER_EVENT_INDEXER_RESULT_V1_SCHEMA_VERSION,
    action,
    protocolDecision,
    reasonCodes,
    alertCodes,
    state,
  };
  return immutableWireValue({
    ...canonical,
    resultDigest: sha256Canonical(canonical),
  });
};

const reject = (
  reason: WatcherUserEventIndexerReasonCodeV1,
  alert: WatcherUserEventIndexerAlertCodeV1 = "watcher_user_event_input_rejected",
): WatcherUserEventIndexerResultV1 =>
  result("reject", "hold", [reason], [alert], null);

export const evaluateWatcherUserEventIndexerV1 = (
  rawPolicy: unknown,
  rawState: unknown,
  rawObservation: unknown,
  rawPublicContext: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): WatcherUserEventIndexerResultV1 => {
  const evidenceBudget: EvidenceGraphBudget = { nodes: 0, bytes: 0 };
  if (!evidenceWithinBounds(rawPolicy, evidenceBudget)) {
    return reject("malformed_policy");
  }
  if (rawState !== null && !evidenceWithinBounds(rawState, evidenceBudget)) {
    return reject("malformed_state");
  }
  if (!evidenceWithinBounds(rawObservation, evidenceBudget)) {
    return reject("malformed_observation");
  }
  if (!evidenceWithinBounds(rawPublicContext, evidenceBudget)) {
    return reject("malformed_public_context");
  }
  const policy = parseWatcherUserEventIndexerPolicyV1(rawPolicy);
  if (policy === null) {
    return reject("malformed_policy");
  }
  const previous =
    rawState === null
      ? null
      : parseWatcherUserEventIndexerStateV1(
          rawState,
          policy,
          transportAttestations,
        );
  if (rawState !== null && previous === null) {
    return reject("malformed_state");
  }
  const observation = parseObservationStructural(rawObservation);
  if (observation === null) {
    return reject("malformed_observation");
  }
  const publicContext = parsePublicContext(rawPublicContext);
  if (publicContext === null) {
    return reject("malformed_public_context");
  }
  if (
    observation.policyDigest !== policy.policyDigest ||
    observation.network !== policy.network ||
    observation.releaseEvidenceDigest !== policy.releaseEvidenceDigest ||
    !same(observation.deploymentMarker, policy.deploymentMarker)
  ) {
    return reject("binding_mismatch", "watcher_user_event_binding_rejected");
  }
  const duplicateEntry = previous?.activeEntryDigests
    .map((digest) => historyEntryForDigest(previous, digest))
    .find(
      (entry) =>
        entry?.observation.observationDigest === observation.observationDigest,
    );
  if (duplicateEntry !== undefined && duplicateEntry !== null) {
    return same(duplicateEntry.publicContext, publicContext)
      ? result(
          "duplicate",
          previous?.snapshot.quarantined === true ? "quarantined" : "indexed",
          ["duplicate_observation"],
          [],
          previous ?? null,
        )
      : reject("identity_collision", "watcher_user_event_binding_rejected");
  }
  const expected =
    publicContext.rollbackAuthority === null
      ? deriveBlockObservation(
          policy,
          previous,
          publicContext,
          transportAttestations,
        )
      : previous === null
        ? null
        : deriveRollbackObservation(
            policy,
            previous,
            publicContext,
            observation.rollbackTargetEntryDigest,
            transportAttestations,
          );
  if (expected === null || !same(expected, observation)) {
    return reject(
      publicContext.rollbackAuthority === null
        ? "public_evidence_mismatch"
        : "rollback_authority_mismatch",
      "watcher_user_event_transition_rejected",
    );
  }
  const next = applyObservation(policy, previous, observation, publicContext);
  if (next === null) {
    return reject("history_limit_exceeded");
  }
  if (observation.snapshot.quarantined) {
    return result(
      "quarantine",
      "quarantined",
      ["post_finality_quarantine"],
      ["watcher_user_event_rollback_quarantined"],
      next,
    );
  }
  return result(
    "accept",
    "indexed",
    [
      observation.transitionKind === "rollback"
        ? "rollback_authenticated"
        : "block_authenticated",
    ],
    [],
    next,
  );
};

export const parseWatcherUserEventIndexerResultV1 = (
  value: unknown,
  context: Readonly<{
    policy: unknown;
    previousState: unknown;
    observation: unknown;
    publicContext: unknown;
    transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
  }>,
): WatcherUserEventIndexerResultV1 | null => {
  if (!evidenceWithinBounds(value)) {
    return null;
  }
  const expected = evaluateWatcherUserEventIndexerV1(
    context.policy,
    context.previousState,
    context.observation,
    context.publicContext,
    context.transportAttestations,
  );
  return same(expected, value) ? expected : null;
};
