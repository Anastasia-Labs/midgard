import { createHash } from "node:crypto";
import { isProxy } from "node:util/types";

import {
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxProofFieldLengths,
  verifyMidgardNativeTxProofSource,
} from "@al-ft/midgard-core/codec/native";
import { MIDGARD_EMPTY_FIELD_COMMITMENT } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { midgardTxFieldCommitmentsFromSource } from "@al-ft/midgard-core/consensus-validation";
import {
  type DeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import { admitFraudProofRawL1Point } from "@al-ft/midgard-fault-proofs";
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
  rejectionCodeOf,
  type RejectionReason,
  rejectionReasonArmOf,
  resolveEventInclusionTime,
  RootDomainSchema,
  SettlementDatumSchema,
  TxOrderDatumSchema,
  TxOrderEventSchema,
  TxOrderMintRedeemer,
  TxOrderSpendRedeemerSchema,
  UserEventMintRedeemer,
  UserEventWitnessPublishRedeemer,
  userEventWitnessScriptHash,
  WithdrawalEventSchema,
  WithdrawalOrderDatumSchema,
  WithdrawalSpendRedeemerSchema,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  SLOT_CONFIG_NETWORK,
  slotToBeginUnixTime,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  evaluateWatcherFinality,
  parseWatcherFinalityPolicy,
  readWatcherLocalBackfillFinalityObservation,
  readWatcherLocalBackfillFinalityOriginalWitness,
  watcherFinalityConfiguredSource,
  type WatcherFinalityPolicy,
  type WatcherFinalityResult,
  type WatcherLocalBackfillFinalityReceipt,
} from "../l1/finality-engine.js";
import {
  encodeWatcherNormalizedL1Block,
  normalizeWatcherL1Block,
  type WatcherL1TransportAttestationContext,
  watcherL1TransportAttestationDetails,
  type WatcherLocalBackfillObservationReceipt,
  type WatcherNormalizedL1Block,
} from "../l1/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistency } from "../l1/multi-provider-consistency.js";
import {
  parseWatcherPostFinalityRecoveryResult,
  parseWatcherRollbackResult,
  type WatcherPostFinalityRecoveryInput,
  type WatcherPostFinalityRecoveryResult,
  type WatcherRollbackResult,
  type WatcherRollbackVerificationContext,
} from "../l1/rollback-engine.js";
import {
  readWatcherUserEventScriptBinding,
  type VerifiedWatcherDeploymentIdentity,
  verifyWatcherDeploymentIdentity,
  type WatcherDeploymentIdentityPolicy,
  type WatcherDeploymentTrustRoot,
  type WatcherUserEventScriptBinding,
} from "../runtime/deployment-identity.js";
import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
  type WatcherDurableRuntime,
  type WatcherProtectedUserEventCheckpoint,
} from "../storage/durable-runtime.js";
import {
  encodeWatcherDurableStore,
  journalWatcherProtocolUtxoTransition,
  makeEmptyWatcherDurableStore,
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
  parseWatcherDurableStore,
  watcherCanonicalJson,
  type WatcherDurableStore,
  watcherDurableStoreBytesSha256,
  watcherSameCanonicalJson,
  watcherSha256CanonicalJson,
} from "../storage/durable-store.js";
import {
  makeWatcherUserEventCheckpoint,
  WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
  WATCHER_USER_EVENT_VALIDATION_SCHEMA_VERSION,
  type WatcherUserEventArchive,
  type WatcherUserEventCheckpoint,
  type WatcherUserEventValidation,
} from "../storage/user-event-checkpoint.js";
import {
  assertWatcherStateQueueHeaderObservation,
  type WatcherStateQueueHeaderObservation,
} from "./authenticated-state-queue-observation.js";
import {
  findWatcherUserEventArchiveIndex,
  findWatcherUserEventArchiveIndexForEntry,
  makeWatcherUserEventArchiveIndex,
  readWatcherUserEventArchiveIndex,
  type WatcherUserEventArchiveIndexRead,
} from "./user-event-history-archive.js";
import {
  readWatcherUserEventOrigin,
  type WatcherUserEventOriginFacts,
  type WatcherUserEventOriginReceipt,
} from "./user-event-origin.js";
import {
  admitWatcherLocalBackfillUserEventReferenceEvidence,
  admitWatcherUserEventReferenceEvidence,
  readWatcherUserEventReferenceEvidence,
  type WatcherUserEventReferenceAuthority,
  type WatcherUserEventReferenceEvidence,
  watcherUserEventReferenceOutput,
} from "./user-event-reference-authority.js";

export const WATCHER_USER_EVENT_INDEXER_POLICY_SCHEMA_VERSION =
  "midgard-watcher-user-event-indexer-policy-v1" as const;
export const WATCHER_USER_EVENT_SNAPSHOT_SCHEMA_VERSION =
  "midgard-watcher-user-event-snapshot-v1" as const;
export const WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION =
  "midgard-watcher-user-event-observation-v1" as const;
export const WATCHER_USER_EVENT_HISTORY_ENTRY_SCHEMA_VERSION =
  "midgard-watcher-user-event-history-entry-v1" as const;
export const WATCHER_USER_EVENT_INDEXER_STATE_SCHEMA_VERSION =
  "midgard-watcher-user-event-indexer-state-v1" as const;
export const WATCHER_USER_EVENT_INDEXER_RESULT_SCHEMA_VERSION =
  "midgard-watcher-user-event-indexer-result-v1" as const;
export const WATCHER_USER_EVENT_PUBLIC_CONTEXT_SCHEMA_VERSION =
  "midgard-watcher-user-event-public-context-v1" as const;
export const WATCHER_FORCED_TERMINAL_CLASSIFICATION_SCHEMA_VERSION =
  "midgard-watcher-forced-terminal-classification-v1" as const;

export const WATCHER_USER_EVENT_INDEXER_BOUNDS = Object.freeze({
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

export const WATCHER_USER_EVENT_INDEXER_REASON_CODES = [
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

export const WATCHER_USER_EVENT_INDEXER_ALERT_CODES = [
  "watcher_user_event_input_rejected",
  "watcher_user_event_binding_rejected",
  "watcher_user_event_transition_rejected",
  "watcher_user_event_rollback_quarantined",
] as const;

export type WatcherUserEventIndexerReasonCode =
  (typeof WATCHER_USER_EVENT_INDEXER_REASON_CODES)[number];
export type WatcherUserEventIndexerAlertCode =
  (typeof WATCHER_USER_EVENT_INDEXER_ALERT_CODES)[number];
export type WatcherUserEventNetwork = "Mainnet" | "Preprod" | "Preview";
export type WatcherUserEventKind = "deposit" | "withdrawal" | "forced_order";
export type WatcherUserEventFinalityStatus = "pending" | "final";
export type WatcherUserEventTerminalStatus =
  | "absorbed"
  | "payout_initialized"
  | "refunded"
  | "processed";

type EventPolicyFields = Readonly<{
  policyId: string;
  spendScriptHash: string;
  addressHex: string;
}>;

export type WatcherUserEventDeploymentAuthority = Readonly<{
  signedIdentity: unknown;
  policy: WatcherDeploymentIdentityPolicy;
  trustRoots: readonly WatcherDeploymentTrustRoot[];
  result: VerifiedWatcherDeploymentIdentity;
}>;

export type WatcherUserEventIndexerPolicy = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_INDEXER_POLICY_SCHEMA_VERSION;
  network: WatcherUserEventNetwork;
  blueprintHash: string;
  deploymentMarker: DeploymentMarker;
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

export type WatcherIndexedUserEvent = Readonly<{
  kind: WatcherUserEventKind;
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
  finalityStatus: WatcherUserEventFinalityStatus;
}>;

export type WatcherTerminalUserEvent = WatcherIndexedUserEvent &
  Readonly<{
    terminalStatus: WatcherUserEventTerminalStatus;
    terminalTransactionHash: string;
    terminalPointDigest: string;
    terminalBlockHash: string;
    terminalSlot: string;
    terminalBlockNo: string;
    terminalFinalityStatus: WatcherUserEventFinalityStatus;
    terminalClassification?: WatcherForcedTerminalClassification;
  }>;

/**
 * The watcher's JSON-safe spelling of a forced-inclusion operator verdict
 * (`ForcedInclusionTxV1.verdict`, #640): the literal `ForcedTxValid`, or the
 * constructor tag of the `RejectionReason` an invalid verdict carries.
 *
 * The reason's subject coordinates are deliberately dropped. Watcher
 * classification records are canonical-JSON digested and round-trip through
 * the durable store, and that encoding admits no `bigint`; the constructor tag
 * is also exactly as much as the retired six-member `MidgardTxValidity`
 * classification ever discriminated, so nothing this comparison consumed is
 * lost.
 */
export type WatcherForcedOperatorVerdict = string;

export type WatcherForcedTerminalClassification = Readonly<{
  schemaVersion: typeof WATCHER_FORCED_TERMINAL_CLASSIFICATION_SCHEMA_VERSION;
  operatorValidity: WatcherForcedOperatorVerdict;
  terminalTransactionHash: string;
  terminalPointDigest: string;
}>;

export type WatcherUserEventSnapshot = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_SNAPSHOT_SCHEMA_VERSION;
  activeEvents: readonly WatcherIndexedUserEvent[];
  terminalEvents: readonly WatcherTerminalUserEvent[];
  quarantined: boolean;
  snapshotDigest: string;
}>;

export type WatcherUserEventObservation = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION;
  policyDigest: string;
  network: WatcherUserEventNetwork;
  blueprintHash: string;
  deploymentMarker: DeploymentMarker;
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
  snapshot: WatcherUserEventSnapshot;
  observationDigest: string;
}>;

export type WatcherUserEventRollbackAuthority = Readonly<{
  result: unknown;
  context:
    | WatcherRollbackVerificationContext
    | WatcherPostFinalityRecoveryInput;
}>;

export type WatcherUserEventFinalityAuthority = Readonly<{
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

export type WatcherUserEventPublicContext = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_PUBLIC_CONTEXT_SCHEMA_VERSION;
  authenticatedProvider: unknown | null;
  l1Observation: unknown | null;
  referenceEvidence: WatcherUserEventReferenceEvidence | null;
  sourceDurableStore: unknown;
  durableStore: unknown;
  deploymentAuthority: WatcherUserEventDeploymentAuthority;
  rollbackRestoredEventUtxos: readonly unknown[];
  finalityAuthority: WatcherUserEventFinalityAuthority | null;
  rollbackAuthority: WatcherUserEventRollbackAuthority | null;
}>;

export type WatcherUserEventHistoryEntry = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_HISTORY_ENTRY_SCHEMA_VERSION;
  predecessorStateDigest: string | null;
  observation: WatcherUserEventObservation;
  publicContext: WatcherUserEventPublicContext;
  entryDigest: string;
}>;

export type WatcherUserEventIndexerState = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_INDEXER_STATE_SCHEMA_VERSION;
  policyDigest: string;
  network: WatcherUserEventNetwork;
  blueprintHash: string;
  deploymentMarker: DeploymentMarker;
  durableStoreDigest: string;
  durableStoreRevision: string;
  snapshot: WatcherUserEventSnapshot;
  history: readonly WatcherUserEventHistoryEntry[];
  activeEntryDigests: readonly string[];
  stateDigest: string;
}>;

export type WatcherUserEventIndexerResult = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_INDEXER_RESULT_SCHEMA_VERSION;
  action: "accept" | "duplicate" | "reject" | "quarantine";
  protocolDecision: "indexed" | "hold" | "quarantined";
  reasonCodes: readonly WatcherUserEventIndexerReasonCode[];
  alertCodes: readonly WatcherUserEventIndexerAlertCode[];
  state: WatcherUserEventIndexerState | null;
  resultDigest: string;
}>;

export type WatcherUserEventViewTransitionInput = Readonly<{
  policy: unknown;
  previousState: unknown;
  sourceDurableStore: unknown;
  authenticatedProvider: unknown;
  l1Observation: unknown;
  referenceEvidence: WatcherUserEventReferenceEvidence;
  deploymentAuthority: WatcherUserEventDeploymentAuthority;
  finalityAuthority: WatcherUserEventFinalityAuthority;
  transportAttestations: readonly WatcherL1TransportAttestationContext[];
  referenceAuthorities: readonly WatcherUserEventReferenceAuthority[];
}>;

export type WatcherUserEventViewTransitionResult =
  | Readonly<{
      status: "derived";
      sourceStore: WatcherDurableStore;
      nextStore: WatcherDurableStore;
      publicContext: WatcherUserEventPublicContext;
      observation: WatcherUserEventObservation;
      result: WatcherUserEventIndexerResult;
    }>
  | Readonly<{
      status: "refused";
      reason: WatcherUserEventIndexerReasonCode;
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
    if (budget.nodes > WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceGraphNodes) {
      return false;
    }
    if (typeof candidate === "string") {
      budget.bytes += Buffer.byteLength(candidate, "utf8");
    } else if (typeof candidate === "number") {
      if (!Number.isSafeInteger(candidate)) {
        return false;
      }
      budget.bytes += 8;
    } else if (
      typeof candidate === "bigint" ||
      typeof candidate === "symbol" ||
      typeof candidate === "function" ||
      typeof candidate === "undefined"
    ) {
      return false;
    } else if (typeof candidate === "boolean") {
      budget.bytes += 8;
    }
    if (budget.bytes > WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceGraphBytes) {
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
        WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries
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
      keys.length > WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries
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
      if (budget.bytes > WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceGraphBytes) {
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
const sha256Canonical = watcherSha256CanonicalJson;
const same = watcherSameCanonicalJson;
const samePlutusData = (left: unknown, right: unknown): boolean => {
  if (left === right) {
    return true;
  }
  try {
    return Data.to(left as never) === Data.to(right as never);
  } catch {
    return watcherSameCanonicalJson(left, right);
  }
};

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
const isNetwork = (value: unknown): value is WatcherUserEventNetwork =>
  typeof value === "string" &&
  NETWORKS.includes(value as WatcherUserEventNetwork);

/**
 * `ForcedTxValid` — the watcher spelling of an accepting operator verdict.
 */
export const WATCHER_FORCED_TX_VALID = "ForcedTxValid" as const;

/**
 * Membership test for {@link WatcherForcedOperatorVerdict}: the accepting
 * literal, or any constructor tag the canonical 47-arm `RejectionReason`
 * bridge knows. Delegating to `rejectionCodeOf` (rather than restating the
 * arm list here) keeps the watcher vocabulary in lockstep with the SDK twin.
 */
export const isWatcherForcedOperatorVerdict = (
  value: unknown,
): value is WatcherForcedOperatorVerdict => {
  if (typeof value !== "string") {
    return false;
  }
  if (value === WATCHER_FORCED_TX_VALID) {
    return true;
  }
  try {
    rejectionCodeOf(value as RejectionReason);
    return true;
  } catch {
    return false;
  }
};

/**
 * Projects a decoded `OperatorVerdictV1` (`ForcedInclusionTxV1.verdict`) onto
 * its watcher spelling, or `null` when the value is not a verdict at all.
 */
export const watcherForcedOperatorVerdict = (
  verdict: unknown,
): WatcherForcedOperatorVerdict | null => {
  if (verdict === WATCHER_FORCED_TX_VALID) {
    return WATCHER_FORCED_TX_VALID;
  }
  if (typeof verdict !== "object" || verdict === null) {
    return null;
  }
  const invalid = (verdict as { ForcedTxInvalid?: unknown }).ForcedTxInvalid;
  if (typeof invalid !== "object" || invalid === null) {
    return null;
  }
  const reason = (invalid as { reason?: unknown }).reason;
  if (
    typeof reason !== "string" &&
    (typeof reason !== "object" || reason === null)
  ) {
    return null;
  }
  let arm: string;
  try {
    arm = rejectionReasonArmOf(reason as RejectionReason);
  } catch {
    return null;
  }
  return isWatcherForcedOperatorVerdict(arm) ? arm : null;
};

const parseForcedTerminalClassification = (
  value: unknown,
): WatcherForcedTerminalClassification | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "operatorValidity",
    "terminalTransactionHash",
    "terminalPointDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !==
      WATCHER_FORCED_TERMINAL_CLASSIFICATION_SCHEMA_VERSION ||
    !isWatcherForcedOperatorVerdict(record.operatorValidity) ||
    !isHex32(record.terminalTransactionHash) ||
    !isHex32(record.terminalPointDigest)
  ) {
    return null;
  }
  return Object.freeze({
    schemaVersion: WATCHER_FORCED_TERMINAL_CLASSIFICATION_SCHEMA_VERSION,
    operatorValidity: record.operatorValidity,
    terminalTransactionHash: record.terminalTransactionHash,
    terminalPointDigest: record.terminalPointDigest,
  });
};

const snapshotTerminalClassificationsAreExact = (value: unknown): boolean => {
  if (typeof value !== "object" || value === null) {
    return false;
  }
  const snapshot = value as PlainRecord;
  if (
    !Array.isArray(snapshot.activeEvents) ||
    !Array.isArray(snapshot.terminalEvents)
  ) {
    return false;
  }
  for (const event of snapshot.activeEvents) {
    if (
      typeof event !== "object" ||
      event === null ||
      Object.hasOwn(event, "terminalClassification")
    ) {
      return false;
    }
  }
  for (const event of snapshot.terminalEvents) {
    if (typeof event !== "object" || event === null) {
      return false;
    }
    const terminal = event as PlainRecord;
    const mustHaveClassification =
      terminal.kind === "forced_order" &&
      terminal.terminalStatus === "processed";
    const hasClassification = Object.hasOwn(terminal, "terminalClassification");
    if (mustHaveClassification !== hasClassification) {
      return false;
    }
    if (!hasClassification) {
      continue;
    }
    const classification = parseForcedTerminalClassification(
      terminal.terminalClassification,
    );
    if (
      classification === null ||
      classification.terminalTransactionHash !==
        terminal.terminalTransactionHash ||
      classification.terminalPointDigest !== terminal.terminalPointDigest
    ) {
      return false;
    }
  }
  return true;
};

const cloneMarker = (value: unknown): DeploymentMarker | null => {
  const record = exactRecord(value, ["schemaVersion", "manifestId"]);
  if (
    record === null ||
    record.schemaVersion !== MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION ||
    !isHex32(record.manifestId)
  ) {
    return null;
  }
  return Object.freeze({
    schemaVersion: MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
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
  value: Omit<WatcherUserEventIndexerPolicy, "policyDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_INDEXER_POLICY_SCHEMA_VERSION,
  network: value.network,
  blueprintHash: value.blueprintHash,
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

export const makeWatcherUserEventIndexerPolicy = (
  value: Omit<
    WatcherUserEventIndexerPolicy,
    "schemaVersion" | "policyDigest" | "eventWaitDurationMs"
  >,
): WatcherUserEventIndexerPolicy | null => {
  const canonical = policyWithoutDigest({
    schemaVersion: WATCHER_USER_EVENT_INDEXER_POLICY_SCHEMA_VERSION,
    ...value,
    eventWaitDurationMs: EVENT_WAIT_DURATION_MS.toString(),
  });
  return parseWatcherUserEventIndexerPolicy({
    ...canonical,
    policyDigest: sha256Canonical(canonical),
  });
};

export const parseWatcherUserEventIndexerPolicy = (
  value: unknown,
): WatcherUserEventIndexerPolicy | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "network",
    "blueprintHash",
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
    record.schemaVersion !== WATCHER_USER_EVENT_INDEXER_POLICY_SCHEMA_VERSION ||
    !isNetwork(record.network) ||
    !isHex32(record.blueprintHash) ||
    !isHex32(record.bootstrapStoreDigest) ||
    !isHex32(record.deploymentTrustRootId) ||
    !isNatural(record.eventWaitDurationMs) ||
    record.eventWaitDurationMs !== EVENT_WAIT_DURATION_MS.toString() ||
    !isNatural(record.requiredFinalityDepth) ||
    BigInt(record.requiredFinalityDepth) === 0n ||
    BigInt(record.requiredFinalityDepth) >
      BigInt(WATCHER_USER_EVENT_INDEXER_BOUNDS.requiredFinalityDepthMaximum) ||
    !isNatural(record.maximumActiveHistoryEntries) ||
    BigInt(record.maximumActiveHistoryEntries) === 0n ||
    BigInt(record.maximumActiveHistoryEntries) >
      BigInt(WATCHER_USER_EVENT_INDEXER_BOUNDS.activeHistoryEntries) ||
    !isNatural(record.maximumAuditHistoryEntries) ||
    BigInt(record.maximumAuditHistoryEntries) === 0n ||
    BigInt(record.maximumAuditHistoryEntries) >
      BigInt(WATCHER_USER_EVENT_INDEXER_BOUNDS.auditHistoryEntries) ||
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
    schemaVersion: WATCHER_USER_EVENT_INDEXER_POLICY_SCHEMA_VERSION,
    network: record.network,
    blueprintHash: record.blueprintHash,
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
  policy: WatcherUserEventIndexerPolicy,
  kind: WatcherUserEventKind,
): EventPolicyFields =>
  kind === "deposit"
    ? policy.deposit
    : kind === "withdrawal"
      ? policy.withdrawal
      : policy.forcedOrder;

const kindForPolicy = (
  policy: WatcherUserEventIndexerPolicy,
  policyId: string,
): WatcherUserEventKind | null =>
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
  transaction: WatcherNormalizedL1Block["transactions"][number],
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
  transaction: WatcherNormalizedL1Block["transactions"][number],
  index: bigint,
) =>
  index >= 0n && index <= BigInt(Number.MAX_SAFE_INTEGER)
    ? (transaction.redeemers[Number(index)] ?? null)
    : null;

type UserEventMintRedeemerBody =
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
    }>;

type DecodedUserEventMintRedeemer = Readonly<{
  event: UserEventMintRedeemerBody;
  /**
   * The §8 carriage vector, present only at the tx-order policy. `null` at the
   * three policies whose redeemer is the bare enum — a distinction the type keeps
   * so a later reader cannot mistake "this policy carries no material" for "this
   * order declared no carriage".
   */
  materialCarriage: readonly unknown[] | null;
}>;

/**
 * Decodes a user-event mint redeemer at the spelling the policy in question
 * actually uses.
 *
 * Three of the four user-event policies take `user_events.MintRedeemer`
 * unchanged. **The tx-order policy does not**: #594's owner ruling gave it its own
 * `MintRedeemer`, which wraps that enum beside the §8 `FieldCarriageV1` vector for
 * the order's material. Its wire form is therefore `Constr 0 [<enum>, <list>]`,
 * and a bare-enum decode of it fails — which is why this takes `kind` and is not
 * one schema for all four.
 *
 * Discriminating on the policy rather than trying both spellings is deliberate.
 * The two forms are structurally distinguishable, so a permissive decoder is
 * writable, but it would index a forced order out of a redeemer shape the tx-order
 * mint cannot accept, and a verifier that accepts more than the validator does is
 * worse than no verifier. An old-shape redeemer at the tx-order policy is
 * therefore a decode failure, and its consequence is this module's ordinary one:
 * the containing block yields no observation at all (see `scanCreatedEvents`).
 */
const decodeMintRedeemer = (
  bytesHex: string,
  kind: WatcherUserEventKind,
): DecodedUserEventMintRedeemer | null => {
  if (kind !== "forced_order") {
    const event = dataRoundTrip<UserEventMintRedeemerBody>(
      bytesHex,
      UserEventMintRedeemer as unknown as EventSchema,
    );
    return event === null ? null : { event, materialCarriage: null };
  }
  const wrapped = dataRoundTrip<
    Readonly<{
      event: UserEventMintRedeemerBody;
      material_carriage: readonly unknown[];
    }>
  >(bytesHex, TxOrderMintRedeemer as unknown as EventSchema);
  return wrapped === null
    ? null
    : { event: wrapped.event, materialCarriage: wrapped.material_carriage };
};

/**
 * How many of §2.5's nine slots a forced order's payload commits material to.
 *
 * This is the whole input the mint's **exhaustion** rule needs beyond the
 * redeemer itself: the vector is positional over the non-empty slots, so its
 * length must equal this count exactly. The nine commitments come out of the
 * payload's own compact structures positionally (§4 has no field-index domain
 * separation, so the slot has to come from the structure), and
 * `forcedPayloadMatchesNativeSource` has already bound those structures to the
 * carried `tx_id` and commitment by the time this is consulted.
 *
 * Returns `null` when the payload is not a decodable §4 binding, so a caller
 * cannot read a count off a payload nothing authenticated.
 */
const forcedOrderMaterialFieldCount = (payload: unknown): number | null => {
  const candidate = payload as {
    source?: {
      compact_cbor?: unknown;
      witness_set_compact_cbor?: unknown;
      field_preimage_lengths_cbor?: unknown;
    };
  };
  if (
    !isHexBytes(candidate.source?.compact_cbor) ||
    !isHexBytes(candidate.source.witness_set_compact_cbor) ||
    !isHexBytes(candidate.source.field_preimage_lengths_cbor)
  ) {
    return null;
  }
  try {
    return midgardTxFieldCommitmentsFromSource({
      compactCbor: Buffer.from(candidate.source.compact_cbor, "hex"),
      witnessSetCompactCbor: Buffer.from(
        candidate.source.witness_set_compact_cbor,
        "hex",
      ),
      fieldPreimageLengthsCbor: Buffer.from(
        candidate.source.field_preimage_lengths_cbor,
        "hex",
      ),
    }).filter(
      (commitment) => !commitment.equals(MIDGARD_EMPTY_FIELD_COMMITMENT),
    ).length;
  } catch {
    return null;
  }
};

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
  // Plutus reference-input indices follow ledger ordering, independent of the
  // order in the transaction body's CBOR set.
  const ordered = Array.from({ length: inputs.len() }, (_, position) => {
    const input = inputs.get(position);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  }).sort(compareOutRefs);
  const input = ordered[Number(index)]!;
  return `${input.txHash}#${input.outputIndex.toString()}`;
};

const inlineDatumCbor = (output: CML.TransactionOutput): string | null =>
  output.datum()?.as_datum()?.to_cbor_hex() ?? null;

const decodeHubAt = (
  referenceEvidence: WatcherUserEventReferenceEvidence,
  transactionHash: string,
  body: CML.TransactionBody,
  index: bigint,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
) => {
  const outRef = referencedOutRefAt(body, index);
  const output = watcherUserEventReferenceOutput(
    referenceEvidence,
    transactionHash,
    outRef,
  );
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
        asDataType<EventSchema>(HubOracleDatumSchema),
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
  transaction: WatcherNormalizedL1Block["transactions"][number],
  outputIndex: number,
  output: CML.TransactionOutput,
): Readonly<{ cborHex: string; digest: string }> | null => {
  const datum = output.datum()?.as_datum();
  if (datum === undefined || output.script_ref() !== undefined) {
    return null;
  }
  const cborHex = datum.to_cbor_hex();
  const normalizedDatum = datum.to_canonical_cbor_hex();
  const l1Utxo = transaction.utxos.find(
    (candidate) => candidate.outputIndex === outputIndex.toString(),
  );
  if (
    l1Utxo === undefined ||
    l1Utxo.output.bytesHex !== output.to_canonical_cbor_hex() ||
    l1Utxo.datum === null ||
    l1Utxo.datum.bytes.bytesHex !== normalizedDatum ||
    l1Utxo.datum.datumHash !==
      CML.hash_plutus_data(
        CML.PlutusData.from_cbor_hex(normalizedDatum),
      ).to_hex()
  ) {
    return null;
  }
  // The adapter descriptors are normalized; the event retains the original
  // datum from the authenticated transaction body, together with its own digest.
  return Object.freeze({
    cborHex,
    digest: sha256Bytes(Buffer.from(cborHex, "hex")),
  });
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
  kind: WatcherUserEventKind,
): Readonly<{ datum: EventSchema; event: EventSchema }> =>
  kind === "deposit"
    ? { datum: DepositDatumSchema, event: DepositEventSchema }
    : kind === "withdrawal"
      ? { datum: WithdrawalOrderDatumSchema, event: WithdrawalEventSchema }
      : { datum: TxOrderDatumSchema, event: TxOrderEventSchema };

const parseEventDatum = (
  kind: WatcherUserEventKind,
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
  kind: WatcherUserEventKind,
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

const scanCreatedTransactionEvents = (
  policy: WatcherUserEventIndexerPolicy,
  block: WatcherNormalizedL1Block,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  referenceEvidence: WatcherUserEventReferenceEvidence,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
  events: WatcherIndexedUserEvent[],
): true | null => {
  const failCreated = (_label: string): null => null;
  if (!transaction.isValid) {
    return true;
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
        (entry): entry is readonly [string, WatcherUserEventKind] =>
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
      redeemer === null
        ? null
        : decodeMintRedeemer(redeemer.bytes.bytesHex, kind);
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
      !("AuthenticateEvent" in decoded.event)
    ) {
      return failCreated("nft-or-mint");
    }
    const auth = decoded.event.AuthenticateEvent;
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
      referenceEvidence,
      transaction.txHash,
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
        BigInt(
          resolveEventInclusionTime(
            slotToBeginUnixTime(
              Number(ttl),
              SLOT_CONFIG_NETWORK[policy.network],
            ),
            policy.network,
          ),
        ) ||
      parsedDatum.witness !== expectedWitness ||
      !eventIdMatchesNonce(kind, parsedDatum.event, nonceInput) ||
      (kind === "forced_order" &&
        (!isHex32(forcedEvent?.id?.transactionId) ||
          typeof forcedEvent.id.outputIndex !== "bigint" ||
          !forcedPayloadMatchesNativeSource(forcedEvent.tx) ||
          // #594's exhaustion rule, re-derived. The redeemer's carriage vector
          // is positional over the payload's non-empty slots, so its length
          // must equal their count exactly — a short vector leaves a field's
          // material uncarried, a spare entry lets two distinct redeemers spell
          // one order (§8.11). Both inputs are in hand here: the vector came
          // out of the mint redeemer above and the count out of the payload
          // whose binding the previous clause just verified. The per-field
          // *hash* half is not reachable from this module — see
          // `forcedPayloadMatchesNativeSource` — but this half is, so it is
          // checked rather than deferred with it.
          decoded.materialCarriage === null ||
          decoded.materialCarriage.length !==
            forcedOrderMaterialFieldCount(forcedEvent.tx)))
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
          WATCHER_USER_EVENT_INDEXER_BOUNDS.maximumDepositNonNftAssets) ||
      (kind !== "deposit" && (policies.length !== 1 || nonNftAssetCount !== 1))
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
  return true;
};

const scanCreatedEvents = (
  policy: WatcherUserEventIndexerPolicy,
  block: WatcherNormalizedL1Block,
  referenceEvidence: WatcherUserEventReferenceEvidence,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
): readonly WatcherIndexedUserEvent[] | null => {
  const failCreated = (_label: string): null => null;
  const events: WatcherIndexedUserEvent[] = [];
  for (const transaction of block.transactions) {
    if (
      scanCreatedTransactionEvents(
        policy,
        block,
        transaction,
        referenceEvidence,
        deployment,
        events,
      ) === null
    ) {
      return null;
    }
  }
  const sorted = events.sort((left, right) =>
    left.outRef.localeCompare(right.outRef),
  );
  if (
    sorted.length > WATCHER_USER_EVENT_INDEXER_BOUNDS.activeEvents ||
    new Set(sorted.map(({ outRef }) => outRef)).size !== sorted.length ||
    new Set(sorted.map(({ eventId }) => eventId)).size !== sorted.length
  ) {
    return failCreated("identity");
  }
  return Object.freeze(sorted);
};

type DecodedTerminalSpend = Readonly<{
  terminalStatus: WatcherUserEventTerminalStatus;
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
  kind: WatcherUserEventKind,
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
    }>(bytesHex, TxOrderSpendRedeemerSchema);
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
  event: WatcherIndexedUserEvent,
  input: CML.TransactionOutput,
  hubDatum: Record<string, unknown>,
  status: WatcherUserEventTerminalStatus,
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
  event: WatcherIndexedUserEvent,
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
  referenceEvidence: WatcherUserEventReferenceEvidence,
  transactionHash: string,
  body: CML.TransactionBody,
  index: bigint,
  policyId: string,
  schema: EventSchema,
): Readonly<{
  output: CML.TransactionOutput;
  datum: Record<string, unknown>;
}> | null => {
  const output = watcherUserEventReferenceOutput(
    referenceEvidence,
    transactionHash,
    referencedOutRefAt(body, index),
  );
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

/**
 * Whether a forced order's payload is a well-formed §4 binding to its own native
 * source.
 *
 * It used to take a `verification` bundle — the durable store, the transaction
 * body, the deployment's policy identities and the tx-order id — because it had to
 * resolve `terminal_receipt_reference` to a receipt UTxO in the store, match that
 * UTxO's script address and minted asset name against the deployment, and count
 * the transaction's reference inputs to it. #587 retired the receipt chain and
 * with it every one of those lookups, so the predicate is now a pure function of
 * the payload.
 *
 * ### What this deliberately does not re-derive, and why
 *
 * The tx-order mint runs `tx_order_v1.verify_order_material`, which is
 * `material_directory` — re-derived below in full — followed by a walk of §2.5's
 * nine slots that opens the §8.8 field-access door at every slot carrying
 * material, against a `FieldCarriageV1` vector the **mint redeemer** supplies
 * (#594's owner ruling). That walk is not re-derived here.
 *
 * The omission used to be about deployability: while §8's availability
 * re-expression was unwired the clause admitted only the canonically-empty
 * transaction, and mirroring a producer-side stopgap would have cost the watcher
 * the ability to observe any forced order that moves anything. #594 wired the
 * mechanism, so that reasoning is spent and this is the reason that replaces it.
 *
 * **The carriage is not in the payload, by design.** §8.7's mandatory content
 * addressing prohibits identifying carriage by UTxO identity, so
 * `TxOrderPayloadV1` deliberately carries no carriage reference — the nine
 * commitments *are* the material directory, and this predicate re-derives them in
 * full. There is therefore nothing in a payload for a payload-shaped predicate to
 * check the carriage against, and adding a field to the datum to give it one is
 * exactly what the ruling refused. (The ruling's own text cites §8.5 for the
 * content-addressing rule; §8.5 is _Custody_ and the rule is §8.7's. Corrected
 * here and in §8.11.)
 *
 * **The exhaustion half is reachable and is not omitted.** The walk's rule that
 * the redeemer's vector be exhausted exactly needs two things: the vector, which
 * `scanCreatedEvents` decodes out of the tx-order mint redeemer, and the count of
 * non-empty slots, which comes out of this payload's own compact structures. Both
 * are in this module, so that clause is re-derived — at the redeemer site rather
 * than here, because this predicate never sees a redeemer. See
 * `forcedOrderMaterialFieldCount` and its caller. The burn's empty-vector rule is
 * likewise re-derived, in `scanConsumedEvents`.
 *
 * Per-field material hashes are not re-derived by this payload predicate.
 * `Inline` preimages ride the mint redeemer, while `RawUtxo`/`Certified` bytes
 * live in reference-input datums. The indexer now admits resolved reference
 * evidence for its hub/settlement checks, but connecting material carriage to
 * those bytes and validating every field remains a separate verification step.
 * Admitting reference bytes does not itself establish those material hashes.
 *
 * **What this predicate is not.** It is not the first line of defence, but the
 * reason is narrower than "the mint already checked". The mint in *this tree*
 * hashes every non-empty field's preimage against its committed hash before the
 * NFT exists. The mint currently **deployed** is the receipt-era one behind the
 * frozen blueprint (#579 owns the regeneration): its per-item opening is
 * unsatisfiable for a payload whose commitments are §4 flat hashes of real
 * material, but a payload *declaring* counted roots in place of flat commitments
 * could satisfy it, so it can authenticate a material-bearing order. That
 * residual is real and is not covered here — what stood here before #587 was the
 * same receipt walk that mint gates on, accepting exactly the payloads it
 * accepted, so no version of this predicate ever closed it. It closes when the
 * blueprint is regenerated, not by anything written in this module.
 */
const forcedPayloadMatchesNativeSource = (payload: unknown): boolean => {
  const candidate = payload as {
    tx_id?: unknown;
    transaction_commitment?: unknown;
    source?: {
      compact_cbor?: unknown;
      witness_set_compact_cbor?: unknown;
      field_preimage_lengths_cbor?: unknown;
    };
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
    verifyMidgardNativeTxProofSource({
      transactionId: Buffer.from(candidate.tx_id, "hex"),
      source,
    });
    if (
      computeMidgardNativeTxProofCommitment(source).toString("hex") !==
      candidate.transaction_commitment
    ) {
      return false;
    }
    // The committed field lengths are decoded, not merely present: a payload whose
    // length vector is malformed or is not nine entries is not a §4 binding, and
    // `decodeMidgardNativeTxProofFieldLengths` is the thing that says so.
    decodeMidgardNativeTxProofFieldLengths(source.fieldPreimageLengthsCbor);
    // What stood here walked the counted publication receipt chain: it resolved
    // `terminal_receipt_reference` out of the durable store, checked the receipt
    // datum's identity and its minted `deriveMidgardTxFieldReceiptAssetNameV1`
    // name, verified the `collection_proof` with
    // `verifyMidgardBoundedCollectionItemProofV1`, and re-derived the terminal
    // chunk and encoded-size arithmetic — decoding both compact structures to get
    // the nine commitments it needed for that. All of it retired in #587 with the
    // chain itself: under `docs/spec/midgard-tx.md` §4 a field commitment is one
    // flat hash over the whole preimage, so no per-item Merkle opening can be
    // checked against it and the receipt mint policy was unsatisfiable for any
    // payload whose commitments were the §4 flat hashes of real material — a
    // narrowing this walk inherited, not a closed door (see the docstring above
    // for the declaring-payload residual the mint left open and this walk shared).
    //
    // **The payload no longer carries availability evidence at all.**
    // `TxOrderPayloadV1` shed `terminal_receipt_reference` in the same change, so
    // what is left to verify from a forced order's datum is exactly what is
    // checked above: the proof source authenticates against the carried `tx_id`,
    // and the carried commitment is the one derived from that source. Availability
    // is enforced where the evidence for it lives — the tx-order mint's
    // `verify_order_material` — and the docstring above says why this predicate
    // does not mirror that function's temporary all-empty clause.
    return true;
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
  event: WatcherIndexedUserEvent,
  referenceEvidence: WatcherUserEventReferenceEvidence,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  body: CML.TransactionBody,
  inputIndex: number,
  spend: DecodedTerminalSpend,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
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
    referenceEvidence,
    transaction.txHash,
    body,
    spend.hubRefInputIndex,
    deployment,
  );
  const settlementPolicy = hubDatum?.settlement;
  const settlement = isHex28(settlementPolicy)
    ? authenticReferenceDatum(
        referenceEvidence,
        transaction.txHash,
        body,
        spend.settlementRefInputIndex,
        settlementPolicy,
        asDataType<EventSchema>(SettlementDatumSchema),
      )
    : null;
  const eventPair = eventKeyValueCbor(event);
  const forcedProofValue =
    event.kind === "forced_order" && eventPair !== null
      ? (() => {
          const tx = eventPair.payload as {
            tx_id?: unknown;
            source?: unknown;
          };
          try {
            return Data.to(
              {
                tx_id: tx.tx_id,
                source: tx.source,
                verdict: spend.purpose,
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
      forcedPayloadMatchesNativeSource(datum.event.tx) &&
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
          asDataType<EventSchema>(PayoutMintRedeemerSchema),
        );
  const payoutDatumHex = inlineDatumCbor(produced);
  const payoutDatum =
    payoutDatumHex === null
      ? null
      : dataRoundTrip<Record<string, unknown>>(
          payoutDatumHex,
          asDataType<EventSchema>(PayoutDatumSchema),
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
    samePlutusData(payoutDatum.l2_value, withdrawalInfo.body?.l2_value) &&
    samePlutusData(payoutDatum.l1_address, withdrawalInfo.body?.l1_address) &&
    samePlutusData(payoutDatum.l1_datum, withdrawalInfo.body?.l1_datum)
  );
};

const scanConsumedTransactionEvents = (
  block: WatcherNormalizedL1Block,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  referenceEvidence: WatcherUserEventReferenceEvidence,
  active: Map<string, WatcherIndexedUserEvent>,
  terminal: WatcherTerminalUserEvent[],
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
): true | null => {
  if (!transaction.isValid) {
    return true;
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
        : decodeMintRedeemer(mintRedeemer.bytes.bytesHex, event.kind);
    if (terminalSpend === null) {
      return null;
    }
    if (
      !verifyTerminalSemantics(
        event,
        referenceEvidence,
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
      !("BurnEventNFT" in decodedMint.event) ||
      decodedMint.event.BurnEventNFT.nonce_asset_name !== event.assetNameHex ||
      decodedMint.event.BurnEventNFT.witness_unregistration_redeemer_index <
        0n ||
      // #594: the tx-order policy requires a burn's carriage vector to be
      // empty, because a burn reads no material and an unread wire field is a
      // second spelling of the same transaction (§8.11, §6.1). `null` here is
      // the three unwrapped policies, which have no vector to constrain.
      (decodedMint.materialCarriage !== null &&
        decodedMint.materialCarriage.length !== 0)
    ) {
      return null;
    }
    const certificateRedeemer = redeemerAtGlobalIndex(
      transaction,
      decodedMint.event.BurnEventNFT.witness_unregistration_redeemer_index,
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
    const forcedOperatorValidity =
      event.kind === "forced_order"
        ? watcherForcedOperatorVerdict(terminalSpend.purpose)
        : null;
    if (event.kind === "forced_order" && forcedOperatorValidity === null) {
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
        ...(forcedOperatorValidity === null
          ? {}
          : {
              terminalClassification: Object.freeze({
                schemaVersion:
                  WATCHER_FORCED_TERMINAL_CLASSIFICATION_SCHEMA_VERSION,
                operatorValidity: forcedOperatorValidity,
                terminalTransactionHash: transaction.txHash,
                terminalPointDigest: block.chainPoint.pointDigest,
              }),
            }),
      }),
    );
  }
  return true;
};

const scanConsumedEvents = (
  block: WatcherNormalizedL1Block,
  referenceEvidence: WatcherUserEventReferenceEvidence,
  activeEvents: readonly WatcherIndexedUserEvent[],
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
): Readonly<{
  remaining: readonly WatcherIndexedUserEvent[];
  terminal: readonly WatcherTerminalUserEvent[];
}> | null => {
  const active = new Map(activeEvents.map((event) => [event.outRef, event]));
  const terminal: WatcherTerminalUserEvent[] = [];
  for (const transaction of block.transactions) {
    if (
      scanConsumedTransactionEvents(
        block,
        transaction,
        referenceEvidence,
        active,
        terminal,
        deployment,
      ) === null
    ) {
      return null;
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
  value: Omit<WatcherUserEventSnapshot, "snapshotDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_SNAPSHOT_SCHEMA_VERSION,
  activeEvents: value.activeEvents,
  terminalEvents: value.terminalEvents,
  quarantined: value.quarantined,
});

const makeSnapshot = (
  activeEvents: readonly WatcherIndexedUserEvent[],
  terminalEvents: readonly WatcherTerminalUserEvent[],
  quarantined = false,
): WatcherUserEventSnapshot | null => {
  if (
    activeEvents.length > WATCHER_USER_EVENT_INDEXER_BOUNDS.activeEvents ||
    terminalEvents.length > WATCHER_USER_EVENT_INDEXER_BOUNDS.terminalEvents
  ) {
    return null;
  }
  const canonical = snapshotWithoutDigest({
    schemaVersion: WATCHER_USER_EVENT_SNAPSHOT_SCHEMA_VERSION,
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
  kind: WatcherUserEventKind,
): "deposit" | "withdrawal" | "forced_transaction" =>
  kind === "forced_order" ? "forced_transaction" : kind;

const topologyMatches = (
  store: WatcherDurableStore,
  snapshot: WatcherUserEventSnapshot,
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

type VerifiedBlockInputs = Readonly<{
  block: WatcherNormalizedL1Block;
  lineageBlocks: readonly WatcherNormalizedL1Block[];
  referenceEvidence: WatcherUserEventReferenceEvidence;
  sourceStore: WatcherDurableStore;
  finalityPolicy: WatcherFinalityPolicy;
  finalityResult: WatcherFinalityResult;
  context: Omit<WatcherUserEventPublicContext, "durableStore">;
}>;

type VerifiedBlockContext = VerifiedBlockInputs &
  Readonly<{
    store: WatcherDurableStore;
    context: WatcherUserEventPublicContext;
  }>;

const transportAttestationForProvider = (
  provider: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
): WatcherL1TransportAttestationContext | null => {
  if (
    !Array.isArray(transportAttestations) ||
    transportAttestations.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.observationsPerFinalityStep
  ) {
    return null;
  }
  const matches = transportAttestations.filter((candidate) => {
    const details = watcherL1TransportAttestationDetails(candidate);
    return details !== null && same(details.provider, provider);
  });
  return matches.length === 1 ? matches[0]! : null;
};

const normalizeTransportAttestedBlock = (
  provider: unknown,
  observation: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
): Readonly<{
  block: WatcherNormalizedL1Block;
  transportAttestation: WatcherL1TransportAttestationContext;
}> | null => {
  const transportAttestation = transportAttestationForProvider(
    provider,
    transportAttestations,
  );
  if (transportAttestation === null) {
    return null;
  }
  return Object.freeze({
    block: normalizeWatcherL1Block(transportAttestation, observation),
    transportAttestation,
  });
};

const storeDigest = (store: WatcherDurableStore): string =>
  watcherDurableStoreBytesSha256(encodeWatcherDurableStore(store));

const canonicalSuccessor = (
  prior: WatcherUserEventObservation,
  next: WatcherNormalizedL1Block,
  lineage: readonly WatcherNormalizedL1Block[],
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
  const ancestors = new Map<string, WatcherNormalizedL1Block>();
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
  source: WatcherDurableStore,
  next: WatcherNormalizedL1Block,
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
  policy: WatcherUserEventIndexerPolicy,
  authority: WatcherUserEventDeploymentAuthority,
): VerifiedWatcherDeploymentIdentity | null => {
  try {
    const verified = verifyWatcherDeploymentIdentity({
      signedIdentity: authority.signedIdentity,
      policy: authority.policy,
      trustRoots: authority.trustRoots,
      durableMarker: policy.deploymentMarker,
    });
    const applied = authority.policy.appliedScriptHashes;
    return same(verified, authority.result) &&
      verified.network === policy.network &&
      verified.blueprintHash === policy.blueprintHash &&
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
  source: WatcherDurableStore,
  next: WatcherDurableStore,
  block: WatcherNormalizedL1Block,
  snapshot: WatcherUserEventSnapshot,
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
    const journal = journalWatcherProtocolUtxoTransition({
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
  prior: WatcherDurableStore,
  source: WatcherDurableStore,
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
): WatcherUserEventPublicContext | null => {
  if (!evidenceWithinBounds(value)) {
    return null;
  }
  const record = exactRecord(value, [
    "schemaVersion",
    "authenticatedProvider",
    "l1Observation",
    "referenceEvidence",
    "sourceDurableStore",
    "durableStore",
    "deploymentAuthority",
    "rollbackRestoredEventUtxos",
    "finalityAuthority",
    "rollbackAuthority",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_USER_EVENT_PUBLIC_CONTEXT_SCHEMA_VERSION
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
        WATCHER_USER_EVENT_INDEXER_BOUNDS.finalityLineageSteps ||
      !Array.isArray(finalityAuthority.observations) ||
      finalityAuthority.observations.length >
        WATCHER_USER_EVENT_INDEXER_BOUNDS.observationsPerFinalityStep)
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
          WATCHER_USER_EVENT_INDEXER_BOUNDS.observationsPerFinalityStep
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
    schemaVersion: WATCHER_USER_EVENT_PUBLIC_CONTEXT_SCHEMA_VERSION,
    authenticatedProvider: record.authenticatedProvider,
    l1Observation: record.l1Observation,
    referenceEvidence:
      record.referenceEvidence as WatcherUserEventReferenceEvidence | null,
    sourceDurableStore: record.sourceDurableStore,
    durableStore: record.durableStore,
    deploymentAuthority: {
      signedIdentity: deploymentAuthority.signedIdentity,
      policy: deploymentAuthority.policy as WatcherDeploymentIdentityPolicy,
      trustRoots:
        deploymentAuthority.trustRoots as readonly WatcherDeploymentTrustRoot[],
      result: deploymentAuthority.result as VerifiedWatcherDeploymentIdentity,
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
              rollbackAuthority.context as WatcherRollbackVerificationContext,
          },
  });
};

const verifyBlockInputs = (
  policy: WatcherUserEventIndexerPolicy,
  context: Omit<WatcherUserEventPublicContext, "durableStore">,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
  referenceAuthorities: readonly WatcherUserEventReferenceAuthority[],
): VerifiedBlockInputs | null => {
  if (
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
    const sourceStore = parseWatcherDurableStore(context.sourceDurableStore);
    const finalityPolicy = parseWatcherFinalityPolicy(
      context.finalityAuthority.policy,
    );
    const deploymentIdentity = verifyDeploymentAuthority(
      policy,
      context.deploymentAuthority,
    );
    if (
      finalityPolicy === null ||
      deploymentIdentity === null ||
      finalityPolicy.network !== policy.network ||
      finalityPolicy.blueprintHash !== policy.blueprintHash ||
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
    const lineageBlocks: WatcherNormalizedL1Block[] = [];
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
      const stepConsistency = evaluateWatcherMultiProviderConsistency(
        watcherFinalityConfiguredSource(finalityPolicy),
        normalizedStep,
        normalizedStepWithAttestations.map(
          (entry) => entry!.transportAttestation,
        ),
      );
      const stepResult = evaluateWatcherFinality(
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
    const consistency = evaluateWatcherMultiProviderConsistency(
      watcherFinalityConfiguredSource(finalityPolicy),
      normalized,
      normalizedWithAttestations.map((entry) => entry!.transportAttestation),
    );
    const finalityResult = evaluateWatcherFinality(
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
    const normalFinalityDecision =
      (finalityResult.protocolDecision === "hold" &&
        ["observe_pending", "advance_pending", "duplicate"].includes(
          finalityResult.action,
        )) ||
      (finalityResult.protocolDecision === "finality_granted" &&
        finalityResult.action === "finalize") ||
      // A point-only rewind can be the existing skipped-block ancestry path;
      // same-point content and depth rewinds remain rollback-only.
      (finalityResult.protocolDecision === "rewind_required" &&
        finalityResult.action === "rewind_pending" &&
        finalityResult.rewindInstruction?.kind === "pending_point_changed");
    if (
      !same(consistency, context.finalityAuthority.consistency) ||
      !same(finalityResult, context.finalityAuthority.result) ||
      !normalFinalityDecision ||
      !sourceMatchesFinality ||
      !normalized.some(
        (candidate) => candidate.observationDigest === block.observationDigest,
      ) ||
      bound?.pointDigest !== block.chainPoint.pointDigest ||
      bound.blockContentDigest !== block.blockContentDigest
    ) {
      return null;
    }
    if (
      block.network !== policy.network ||
      !same(sourceStore.deploymentMarker, policy.deploymentMarker)
    ) {
      return null;
    }
    const referenceEvidence = admitWatcherUserEventReferenceEvidence({
      evidence: context.referenceEvidence,
      targetBlock: block,
      deploymentIdentity,
      referenceAuthorities,
    });
    if (
      referenceEvidence === null ||
      (referenceEvidence.evidenceKind === "resolved_block" &&
        referenceEvidence.confirmationDepth !== policy.requiredFinalityDepth)
    )
      return null;
    return Object.freeze({
      block,
      referenceEvidence,
      lineageBlocks: Object.freeze(lineageBlocks),
      sourceStore,
      finalityPolicy,
      finalityResult,
      context,
    });
  } catch {
    return null;
  }
};

const verifyBlockContext = (
  policy: WatcherUserEventIndexerPolicy,
  rawContext: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
  referenceAuthorities: readonly WatcherUserEventReferenceAuthority[],
): VerifiedBlockContext | null => {
  const context = parsePublicContext(rawContext);
  if (context === null) return null;
  const verified = verifyBlockInputs(
    policy,
    context,
    transportAttestations,
    referenceAuthorities,
  );
  if (verified === null) return null;
  try {
    const store = parseWatcherDurableStore(context.durableStore);
    const { block, sourceStore } = verified;
    const persistedBytes =
      encodeWatcherNormalizedL1Block(block).toString("hex");
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
    return Object.freeze({ ...verified, store, context });
  } catch {
    return null;
  }
};

const withCurrentFinality = (
  currentPointDigest: string,
  finalityGranted: boolean,
  events: readonly WatcherIndexedUserEvent[],
): readonly WatcherIndexedUserEvent[] =>
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
  events: readonly WatcherTerminalUserEvent[],
): readonly WatcherTerminalUserEvent[] =>
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
  value: Omit<WatcherUserEventObservation, "observationDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION,
  policyDigest: value.policyDigest,
  network: value.network,
  blueprintHash: value.blueprintHash,
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
  value: Omit<WatcherUserEventObservation, "observationDigest">,
): WatcherUserEventObservation => {
  const canonical = observationWithoutDigest(value);
  return Object.freeze({
    ...canonical,
    observationDigest: sha256Canonical(canonical),
  });
};

/** Fold an admitted whole block in native order; no partial block is constructed. */
const deriveLocalBlockEventSnapshot = (
  policy: WatcherUserEventIndexerPolicy,
  previous: WatcherUserEventSnapshot,
  block: WatcherNormalizedL1Block,
  referenceEvidence: WatcherUserEventReferenceEvidence,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
): WatcherUserEventSnapshot | null => {
  if (previous.quarantined) return null;
  const active = new Map(
    previous.activeEvents.map((event) => [event.outRef, event]),
  );
  const terminal = [...previous.terminalEvents];
  const eventIds = new Set(
    [...previous.activeEvents, ...previous.terminalEvents].map(
      (event) => event.eventId,
    ),
  );
  for (const transaction of block.transactions) {
    if (
      scanConsumedTransactionEvents(
        block,
        transaction,
        referenceEvidence,
        active,
        terminal,
        deployment,
      ) === null
    )
      return null;
    const created: WatcherIndexedUserEvent[] = [];
    if (
      scanCreatedTransactionEvents(
        policy,
        block,
        transaction,
        referenceEvidence,
        deployment,
        created,
      ) === null
    )
      return null;
    for (const event of created) {
      if (active.has(event.outRef) || eventIds.has(event.eventId)) return null;
      active.set(
        event.outRef,
        Object.freeze({ ...event, finalityStatus: "final" }),
      );
      eventIds.add(event.eventId);
    }
    if (
      active.size > WATCHER_USER_EVENT_INDEXER_BOUNDS.activeEvents ||
      terminal.length > WATCHER_USER_EVENT_INDEXER_BOUNDS.terminalEvents
    )
      return null;
  }
  return makeSnapshot(
    [...active.values()].sort((left, right) =>
      left.outRef.localeCompare(right.outRef),
    ),
    withCurrentTerminalFinality(
      block.chainPoint.pointDigest,
      true,
      terminal.sort((left, right) =>
        `${left.terminalPointDigest}:${left.outRef}`.localeCompare(
          `${right.terminalPointDigest}:${right.outRef}`,
        ),
      ),
    ),
  );
};

const deriveBlockSnapshot = (
  policy: WatcherUserEventIndexerPolicy,
  previous: WatcherUserEventIndexerState | null,
  verified: VerifiedBlockInputs,
): WatcherUserEventSnapshot | null => {
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
  let priorStore: WatcherDurableStore | null = null;
  try {
    priorStore =
      previous === null
        ? null
        : parseWatcherDurableStore(
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
    verified.referenceEvidence,
    priorActive,
    verified.context.deploymentAuthority.policy,
  );
  const created = scanCreatedEvents(
    policy,
    verified.block,
    verified.referenceEvidence,
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
  return snapshot;
};

const deriveBlockObservation = (
  policy: WatcherUserEventIndexerPolicy,
  previous: WatcherUserEventIndexerState | null,
  rawContext: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
  referenceAuthorities: readonly WatcherUserEventReferenceAuthority[],
): WatcherUserEventObservation | null => {
  const verified = verifyBlockContext(
    policy,
    rawContext,
    transportAttestations,
    referenceAuthorities,
  );
  if (verified === null) {
    return null;
  }
  const snapshot = deriveBlockSnapshot(policy, previous, verified);
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
    schemaVersion: WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    blueprintHash: policy.blueprintHash,
    deploymentMarker: policy.deploymentMarker,
    transitionKind: "apply_block",
    pointDigest: verified.block.chainPoint.pointDigest,
    blockHash: verified.block.chainPoint.blockHash,
    slot: verified.block.chainPoint.slot,
    blockNo: verified.block.chainPoint.blockNo,
    sourceObservationDigest: verified.block.observationDigest,
    chainPointId: verified.block.chainPoint.chainPointId,
    sourceDurableStoreDigest: storeDigest(verified.sourceStore),
    sourceDurableStoreRevision: verified.sourceStore.revision,
    durableStoreDigest: watcherDurableStoreBytesSha256(
      encodeWatcherDurableStore(verified.store),
    ),
    durableStoreRevision: verified.store.revision,
    rollbackTargetEntryDigest: null,
    snapshot,
  });
};

type VerifiedRollbackContextBase = Readonly<{
  sourceStore: WatcherDurableStore;
  store: WatcherDurableStore;
  context: WatcherUserEventPublicContext;
}>;
type VerifiedRollbackContext =
  | (VerifiedRollbackContextBase &
      Readonly<{
        kind: "pre_finality_rollback";
        result: WatcherRollbackResult;
      }>)
  | (VerifiedRollbackContextBase &
      Readonly<{
        kind: "post_finality_recovery";
        result: WatcherPostFinalityRecoveryResult;
      }>);

const verifyRollbackContext = (
  policy: WatcherUserEventIndexerPolicy,
  rawContext: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
): VerifiedRollbackContext | null => {
  const context = parsePublicContext(rawContext);
  if (
    context === null ||
    context.authenticatedProvider !== null ||
    context.l1Observation !== null ||
    context.referenceEvidence !== null ||
    context.finalityAuthority !== null ||
    context.rollbackAuthority === null ||
    verifyDeploymentAuthority(policy, context.deploymentAuthority) === null
  ) {
    return null;
  }
  try {
    const rollbackResult = parseWatcherRollbackResult(
      context.rollbackAuthority.result,
      {
        ...(context.rollbackAuthority
          .context as WatcherRollbackVerificationContext),
        transportAttestations,
      },
    );
    const recoveryResult =
      rollbackResult === null
        ? parseWatcherPostFinalityRecoveryResult(
            context.rollbackAuthority.result,
            {
              ...(context.rollbackAuthority
                .context as WatcherPostFinalityRecoveryInput),
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
    const sourceStore = parseWatcherDurableStore(context.sourceDurableStore);
    const store = parseWatcherDurableStore(context.durableStore);
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
          recoveryResult.recoveryState.blueprintHash !== policy.blueprintHash ||
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
  state: WatcherUserEventIndexerState,
  digest: string,
): WatcherUserEventHistoryEntry | null =>
  state.history.find(({ entryDigest }) => entryDigest === digest) ?? null;

const deriveRollbackObservation = (
  policy: WatcherUserEventIndexerPolicy,
  previous: WatcherUserEventIndexerState,
  rawContext: unknown,
  requestedRollbackTargetEntryDigest: string | null,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
): WatcherUserEventObservation | null => {
  const verified = verifyRollbackContext(
    policy,
    rawContext,
    transportAttestations,
  );
  if (verified === null) {
    return null;
  }
  let previousStore: WatcherDurableStore;
  try {
    previousStore = parseWatcherDurableStore(
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
          schemaVersion: WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION,
          policyDigest: policy.policyDigest,
          network: policy.network,
          blueprintHash: policy.blueprintHash,
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
            encodeWatcherDurableStore(verified.store),
          ),
          durableStoreRevision: verified.store.revision,
          rollbackTargetEntryDigest: null,
          snapshot,
        });
  }
  const activeEntries = previous.activeEntryDigests
    .map((digest) => historyEntryForDigest(previous, digest))
    .filter(
      (entry): entry is WatcherUserEventHistoryEntry =>
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
  const removed = (entry: WatcherUserEventHistoryEntry): boolean =>
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
  let targetStore: WatcherDurableStore | null = null;
  if (target !== null) {
    try {
      targetStore = parseWatcherDurableStore(target.publicContext.durableStore);
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
    schemaVersion: WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    blueprintHash: policy.blueprintHash,
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
      encodeWatcherDurableStore(verified.store),
    ),
    durableStoreRevision: verified.store.revision,
    rollbackTargetEntryDigest,
    snapshot: targetSnapshot,
  });
};

export const deriveWatcherUserEventObservation = (
  rawPolicy: unknown,
  rawPreviousState: unknown,
  rawPublicContext: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
  referenceAuthorities: readonly WatcherUserEventReferenceAuthority[],
  rollbackTargetEntryDigest: string | null = null,
): WatcherUserEventObservation | null => {
  const evidenceBudget: EvidenceGraphBudget = { nodes: 0, bytes: 0 };
  if (
    !evidenceWithinBounds(rawPolicy, evidenceBudget) ||
    (rawPreviousState !== null &&
      !evidenceWithinBounds(rawPreviousState, evidenceBudget)) ||
    !evidenceWithinBounds(rawPublicContext, evidenceBudget)
  ) {
    return null;
  }
  const policy = parseWatcherUserEventIndexerPolicy(rawPolicy);
  if (policy === null) {
    return null;
  }
  const previous =
    rawPreviousState === null
      ? null
      : parseWatcherUserEventIndexerState(
          rawPreviousState,
          policy,
          transportAttestations,
          referenceAuthorities,
        );
  if (rawPreviousState !== null && previous === null) {
    return null;
  }
  const context = parsePublicContext(rawPublicContext);
  if (context === null) {
    return null;
  }
  return context.rollbackAuthority === null
    ? deriveBlockObservation(
        policy,
        previous,
        context,
        transportAttestations,
        referenceAuthorities,
      )
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

const authorityCollectionWithinBounds = (
  value: unknown,
): value is readonly unknown[] => {
  if (
    !Array.isArray(value) ||
    isProxy(value) ||
    Object.getPrototypeOf(value) !== Array.prototype ||
    value.length > WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries
  )
    return false;
  const keys = Reflect.ownKeys(value);
  return (
    keys.length === value.length + 1 &&
    keys.every((key) => {
      if (key === "length") return true;
      if (
        typeof key !== "string" ||
        !NATURAL.test(key) ||
        BigInt(key) >= BigInt(value.length)
      )
        return false;
      const descriptor = Object.getOwnPropertyDescriptor(value, key);
      return (
        descriptor !== undefined &&
        descriptor.enumerable === true &&
        descriptor.get === undefined &&
        descriptor.set === undefined
      );
    })
  );
};

/**
 * Constructs one canonical apply-block candidate from admitted evidence.
 * Existing verification still admits every representation allowed by its store
 * predicates. This operation does not publish a store or mint durable authority.
 */
export const deriveWatcherUserEventViewTransition = (
  input: WatcherUserEventViewTransitionInput,
): WatcherUserEventViewTransitionResult => {
  const refuse = (
    reason: WatcherUserEventIndexerReasonCode,
  ): WatcherUserEventViewTransitionResult =>
    Object.freeze({ status: "refused", reason });
  try {
    if (typeof input === "object" && input !== null && isProxy(input)) {
      return refuse("malformed_public_context");
    }
    const record = exactRecord(input, [
      "policy",
      "previousState",
      "sourceDurableStore",
      "authenticatedProvider",
      "l1Observation",
      "referenceEvidence",
      "deploymentAuthority",
      "finalityAuthority",
      "transportAttestations",
      "referenceAuthorities",
    ]);
    if (
      record === null ||
      !authorityCollectionWithinBounds(record.transportAttestations) ||
      !authorityCollectionWithinBounds(record.referenceAuthorities)
    ) {
      return refuse("malformed_public_context");
    }
    // Parse the existing public context shape before a destination exists. The
    // structural parser does not grant authority to this null placeholder.
    const rawContext = {
      schemaVersion: WATCHER_USER_EVENT_PUBLIC_CONTEXT_SCHEMA_VERSION,
      authenticatedProvider: record.authenticatedProvider,
      l1Observation: record.l1Observation,
      referenceEvidence: record.referenceEvidence,
      sourceDurableStore: record.sourceDurableStore,
      durableStore: null,
      deploymentAuthority: record.deploymentAuthority,
      rollbackRestoredEventUtxos: [],
      finalityAuthority: record.finalityAuthority,
      rollbackAuthority: null,
    };
    const budget: EvidenceGraphBudget = { nodes: 0, bytes: 0 };
    if (!evidenceWithinBounds(record.policy, budget))
      return refuse("malformed_policy");
    if (
      record.previousState !== null &&
      !evidenceWithinBounds(record.previousState, budget)
    )
      return refuse("malformed_state");
    if (!evidenceWithinBounds(rawContext, budget))
      return refuse("malformed_public_context");
    const policy = parseWatcherUserEventIndexerPolicy(record.policy);
    if (policy === null) return refuse("malformed_policy");
    const transportAttestations =
      record.transportAttestations as readonly WatcherL1TransportAttestationContext[];
    const referenceAuthorities =
      record.referenceAuthorities as readonly WatcherUserEventReferenceAuthority[];
    const previous =
      record.previousState === null
        ? null
        : parseWatcherUserEventIndexerState(
            record.previousState,
            policy,
            transportAttestations,
            referenceAuthorities,
          );
    if (record.previousState !== null && previous === null)
      return refuse("malformed_state");
    const context = parsePublicContext(rawContext);
    if (context === null) return refuse("malformed_public_context");
    const verified = verifyBlockInputs(
      policy,
      context,
      transportAttestations,
      referenceAuthorities,
    );
    if (verified === null) return refuse("public_evidence_mismatch");
    const snapshot = deriveBlockSnapshot(policy, previous, verified);
    if (snapshot === null) return refuse("public_evidence_mismatch");
    const { sourceStore, block } = verified;
    const targetObservation = {
      observationId: block.observationDigest,
      providerId: block.provider.providerId,
      chainPointId: block.chainPoint.chainPointId,
      payload: makeWatcherDurablePayload(
        encodeWatcherNormalizedL1Block(block).toString("hex"),
      ),
    };
    const existingObservation = sourceStore.l1Observations.find(
      ({ observationId }) => observationId === targetObservation.observationId,
    );
    if (
      existingObservation !== undefined &&
      !same(existingObservation, targetObservation)
    ) {
      return refuse("durable_evidence_mismatch");
    }
    const chainPoints = [
      ...sourceStore.chainPoints.filter(
        ({ chainPointId }) => chainPointId !== block.chainPoint.chainPointId,
      ),
      {
        chainPointId: block.chainPoint.chainPointId,
        providerId: block.provider.providerId,
        blockHash: block.chainPoint.blockHash,
        slot: block.chainPoint.slot,
        blockNo: block.chainPoint.blockNo,
        depth: block.chainPoint.depth,
      },
    ];
    const protocolUtxos = [
      ...sourceStore.protocolUtxos.filter(
        ({ role }) =>
          !["deposit", "withdrawal", "forced_transaction"].includes(role),
      ),
      ...snapshot.activeEvents.map((event) => ({
        outRef: event.outRef,
        role: protocolRole(event.kind),
        chainPointId: event.originChainPointId,
        output: makeWatcherDurablePayload(event.outputCborHex),
      })),
    ];
    const journal = journalWatcherProtocolUtxoTransition({
      sourceStore,
      nextChainPoints: chainPoints,
      nextProtocolUtxos: protocolUtxos,
      spentAtChainPointId: block.chainPoint.chainPointId,
    });
    const nextStore = makeWatcherDurableStore({
      deploymentMarker: sourceStore.deploymentMarker,
      revision: (BigInt(sourceStore.revision) + 1n).toString(),
      records: {
        l1Observations:
          existingObservation === undefined
            ? [...sourceStore.l1Observations, targetObservation]
            : sourceStore.l1Observations,
        chainPoints,
        ...journal,
        daProofInputs: sourceStore.daProofInputs,
        reconstructedStates: sourceStore.reconstructedStates,
        decisions: sourceStore.decisions,
        faults: sourceStore.faults,
        submissions: sourceStore.submissions,
        confirmations: sourceStore.confirmations,
        retries: sourceStore.retries,
        deadlines: sourceStore.deadlines,
        correctionResults: sourceStore.correctionResults,
      },
    });
    const candidateContext = { ...context, durableStore: nextStore };
    // The generated destination and observation add wire evidence. Apply the
    // existing cumulative budget before cloning the complete verification input.
    const candidateBudget: EvidenceGraphBudget = { nodes: 0, bytes: 0 };
    if (
      !evidenceWithinBounds(policy, candidateBudget) ||
      (previous !== null && !evidenceWithinBounds(previous, candidateBudget)) ||
      !evidenceWithinBounds(candidateContext, candidateBudget)
    ) {
      return refuse("malformed_public_context");
    }
    const publicContext = immutableWireValue(candidateContext);
    const observation = deriveWatcherUserEventObservation(
      policy,
      previous,
      publicContext,
      transportAttestations,
      referenceAuthorities,
    );
    if (observation === null) return refuse("public_evidence_mismatch");
    const indexed = evaluateWatcherUserEventIndexer(
      policy,
      previous,
      observation,
      publicContext,
      transportAttestations,
      referenceAuthorities,
    );
    const parsed = parseWatcherUserEventIndexerResult(indexed, {
      policy,
      previousState: previous,
      observation,
      publicContext,
      transportAttestations,
      referenceAuthorities,
    });
    if (
      parsed === null ||
      parsed.action !== "accept" ||
      parsed.protocolDecision !== "indexed" ||
      parsed.state === null
    ) {
      return refuse(indexed.reasonCodes[0] ?? "public_evidence_mismatch");
    }
    return Object.freeze({
      status: "derived",
      sourceStore: parseWatcherDurableStore(publicContext.sourceDurableStore),
      nextStore: parseWatcherDurableStore(publicContext.durableStore),
      publicContext,
      observation,
      result: parsed,
    });
  } catch {
    return refuse("durable_evidence_mismatch");
  }
};

const historyEntryWithoutDigest = (
  value: Omit<WatcherUserEventHistoryEntry, "entryDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_HISTORY_ENTRY_SCHEMA_VERSION,
  predecessorStateDigest: value.predecessorStateDigest,
  observation: value.observation,
  publicContext: value.publicContext,
});

const stateWithoutDigest = (
  value: Omit<WatcherUserEventIndexerState, "stateDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_INDEXER_STATE_SCHEMA_VERSION,
  policyDigest: value.policyDigest,
  network: value.network,
  blueprintHash: value.blueprintHash,
  deploymentMarker: value.deploymentMarker,
  durableStoreDigest: value.durableStoreDigest,
  durableStoreRevision: value.durableStoreRevision,
  snapshot: value.snapshot,
  history: value.history,
  activeEntryDigests: value.activeEntryDigests,
});

const applyObservation = (
  policy: WatcherUserEventIndexerPolicy,
  previous: WatcherUserEventIndexerState | null,
  observation: WatcherUserEventObservation,
  publicContext: WatcherUserEventPublicContext,
): WatcherUserEventIndexerState | null => {
  const entryCanonical = historyEntryWithoutDigest({
    schemaVersion: WATCHER_USER_EVENT_HISTORY_ENTRY_SCHEMA_VERSION,
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
    schemaVersion: WATCHER_USER_EVENT_INDEXER_STATE_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    blueprintHash: policy.blueprintHash,
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
): WatcherUserEventObservation | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "policyDigest",
    "network",
    "blueprintHash",
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
    record.schemaVersion !== WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION ||
    !isHex32(record.policyDigest) ||
    !isNetwork(record.network) ||
    !isHex32(record.blueprintHash) ||
    cloneMarker(record.deploymentMarker) === null ||
    !["apply_block", "rollback"].includes(String(record.transitionKind)) ||
    !isHex32(record.sourceDurableStoreDigest) ||
    !isNatural(record.sourceDurableStoreRevision) ||
    !isHex32(record.durableStoreDigest) ||
    !isNatural(record.durableStoreRevision) ||
    !isHex32(record.observationDigest) ||
    !snapshotTerminalClassificationsAreExact(record.snapshot)
  ) {
    return null;
  }
  const candidate = value as WatcherUserEventObservation;
  const canonical = observationWithoutDigest(candidate);
  return sha256Canonical(canonical) === candidate.observationDigest
    ? candidate
    : null;
};

export const parseWatcherUserEventIndexerState = (
  value: unknown,
  rawPolicy: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
  referenceAuthorities: readonly WatcherUserEventReferenceAuthority[],
): WatcherUserEventIndexerState | null => {
  const evidenceBudget: EvidenceGraphBudget = { nodes: 0, bytes: 0 };
  if (
    !evidenceWithinBounds(rawPolicy, evidenceBudget) ||
    !evidenceWithinBounds(value, evidenceBudget)
  ) {
    return null;
  }
  const policy = parseWatcherUserEventIndexerPolicy(rawPolicy);
  const record = exactRecord(value, [
    "schemaVersion",
    "policyDigest",
    "network",
    "blueprintHash",
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
    record.schemaVersion !== WATCHER_USER_EVENT_INDEXER_STATE_SCHEMA_VERSION ||
    !Array.isArray(record.history) ||
    !Array.isArray(record.activeEntryDigests) ||
    !isHex32(record.stateDigest) ||
    !isHex32(record.durableStoreDigest) ||
    !isNatural(record.durableStoreRevision) ||
    record.policyDigest !== policy.policyDigest ||
    record.network !== policy.network ||
    record.blueprintHash !== policy.blueprintHash ||
    !same(record.deploymentMarker, policy.deploymentMarker) ||
    !snapshotTerminalClassificationsAreExact(record.snapshot)
  ) {
    return null;
  }
  let replay: WatcherUserEventIndexerState | null = null;
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
        WATCHER_USER_EVENT_HISTORY_ENTRY_SCHEMA_VERSION ||
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
            referenceAuthorities,
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
  action: WatcherUserEventIndexerResult["action"],
  protocolDecision: WatcherUserEventIndexerResult["protocolDecision"],
  reasonCodes: readonly WatcherUserEventIndexerReasonCode[],
  alertCodes: readonly WatcherUserEventIndexerAlertCode[],
  state: WatcherUserEventIndexerState | null,
): WatcherUserEventIndexerResult => {
  const canonical = {
    schemaVersion: WATCHER_USER_EVENT_INDEXER_RESULT_SCHEMA_VERSION,
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
  reason: WatcherUserEventIndexerReasonCode,
  alert: WatcherUserEventIndexerAlertCode = "watcher_user_event_input_rejected",
): WatcherUserEventIndexerResult =>
  result("reject", "hold", [reason], [alert], null);

export const evaluateWatcherUserEventIndexer = (
  rawPolicy: unknown,
  rawState: unknown,
  rawObservation: unknown,
  rawPublicContext: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
  referenceAuthorities: readonly WatcherUserEventReferenceAuthority[],
): WatcherUserEventIndexerResult => {
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
  const policy = parseWatcherUserEventIndexerPolicy(rawPolicy);
  if (policy === null) {
    return reject("malformed_policy");
  }
  const previous =
    rawState === null
      ? null
      : parseWatcherUserEventIndexerState(
          rawState,
          policy,
          transportAttestations,
          referenceAuthorities,
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
    observation.blueprintHash !== policy.blueprintHash ||
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
          referenceAuthorities,
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

export const parseWatcherUserEventIndexerResult = (
  value: unknown,
  context: Readonly<{
    policy: unknown;
    previousState: unknown;
    observation: unknown;
    publicContext: unknown;
    transportAttestations: readonly WatcherL1TransportAttestationContext[];
    referenceAuthorities: readonly WatcherUserEventReferenceAuthority[];
  }>,
): WatcherUserEventIndexerResult | null => {
  if (typeof value === "object" && value !== null && isProxy(value)) {
    return null;
  }
  if (!evidenceWithinBounds(value)) {
    return null;
  }
  const expected = evaluateWatcherUserEventIndexer(
    context.policy,
    context.previousState,
    context.observation,
    context.publicContext,
    context.transportAttestations,
    context.referenceAuthorities,
  );
  return same(expected, value) ? expected : null;
};

const localHistoryBrand = Symbol("watcher-local-user-event-history");
const localTransitionBrand = Symbol("watcher-local-user-event-transition");
export type WatcherLocalUserEventHistory = Readonly<{
  [localHistoryBrand]: true;
}>;
export type WatcherLocalUserEventTransition = Readonly<{
  [localTransitionBrand]: true;
}>;
type LocalPair = Readonly<{
  finality: WatcherLocalBackfillFinalityReceipt;
  observation: WatcherLocalBackfillObservationReceipt;
  referenceAuthority: WatcherUserEventReferenceAuthority;
}>;
type LocalWitness = ReturnType<
  typeof readWatcherLocalBackfillFinalityOriginalWitness
>;
type LocalArchiveObject = Readonly<{ digest: string; bytesHex: string }>;
export type WatcherLocalUserEventEntry = Readonly<{
  schemaVersion: "midgard-watcher-local-user-event-entry-v1";
  sequence: string;
  originDigest: string;
  policyDigest: string;
  predecessorEntryDigest: string | null;
  predecessorStateDigest: string | null;
  cursor: WatcherUserEventOriginFacts["parentPoint"];
  parent: WatcherUserEventOriginFacts["parentPoint"];
  sourceStoreDigest: string;
  nextStoreDigest: string;
  sourceStoreRevision: string;
  nextStoreRevision: string;
  observationDigest: string;
  snapshotDigest: string;
  evidenceDigest: string;
  entryDigest: string;
}>;
type LocalPreparedRead = Readonly<{
  sourceStore: WatcherDurableStore;
  nextStore: WatcherDurableStore;
  observation: WatcherUserEventObservation;
  snapshot: WatcherUserEventSnapshot;
  entry: WatcherLocalUserEventEntry;
  archiveObjects: readonly LocalArchiveObject[];
  nextCheckpoint: WatcherUserEventCheckpoint;
  expectedCheckpointDigest: string | null;
  expectedCheckpointSequence: string | null;
}>;
type LocalRetainedEvidence = Readonly<{
  entry: WatcherLocalUserEventEntry;
  entryArchiveDigest: string;
  rawBlockCbor: string;
  pointDigest: string;
  chainPointId: string;
}>;
type LocalHistoryOwner = {
  readonly origin: WatcherUserEventOriginFacts;
  readonly originDigest: string;
  readonly activationPair: Readonly<{
    finality: WatcherLocalBackfillFinalityReceipt;
    observation: WatcherLocalBackfillObservationReceipt;
  }>;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly scriptBinding: WatcherUserEventScriptBinding;
  readonly policy: WatcherUserEventIndexerPolicy;
  readonly finalityPolicy: WatcherFinalityPolicy;
  readonly originArchive: LocalArchiveObject;
  store: WatcherDurableStore;
  snapshot: WatcherUserEventSnapshot;
  entries: readonly WatcherLocalUserEventEntry[];
  acceptedEvidence: readonly LocalRetainedEvidence[];
  pinnedEvidence: readonly LocalRetainedEvidence[];
  archiveIndex: WatcherUserEventArchiveIndexRead | null;
  anchorCandidate: WatcherLocalUserEventAnchor | null;
  lastAccepted: WatcherLocalUserEventTransition | null;
  archiveObjects: readonly LocalArchiveObject[];
  checkpoint: WatcherUserEventCheckpoint | null;
  candidate: WatcherLocalUserEventTransition | null;
  generation: number;
  acceptedAtMonotonicMs: number | null;
  closed: boolean;
  suspendedAt: number | null;
  semanticReplay: boolean;
};
type LocalTransitionOwner = {
  readonly history: WatcherLocalUserEventHistory;
  readonly generation: number;
  readonly pair: LocalPair;
  readonly witness: LocalWitness;
  readonly referenceEvidence: WatcherUserEventReferenceEvidence;
  readonly value: LocalPreparedRead;
  accepted: boolean;
};
const localHistories = new WeakMap<
  WatcherLocalUserEventHistory,
  LocalHistoryOwner
>();
const localTransitions = new WeakMap<
  WatcherLocalUserEventTransition,
  LocalTransitionOwner
>();
const localRefuse = (reason: string): never => {
  throw new Error(`Local user-event history refused: ${reason}`);
};
const localOwner = (
  history: WatcherLocalUserEventHistory,
): LocalHistoryOwner => {
  const owner =
    localHistories.get(history) ??
    localRefuse("history is not privately admitted");
  if (owner.closed) return localRefuse("history is closed");
  if (owner.suspendedAt !== null) return localRefuse("history is suspended");
  return owner;
};

/** Archive schema encodes every number as its exact finite decimal string.
 * Numeric field types belong to the evidence schema, never to a revived clock.
 * These bytes are descriptive past-process facts, not reissued receipt authority.
 */
const localArchiveEvidence = (value: unknown): unknown => {
  if (typeof value === "bigint") return value.toString();
  if (typeof value === "number") {
    if (!Number.isFinite(value))
      return localRefuse("non-finite archive number");
    const decimal = Object.is(value, -0) ? "-0" : value.toString();
    if (!Object.is(Number(decimal), value))
      return localRefuse("inexact archive number");
    return decimal;
  }
  if (Array.isArray(value)) return value.map(localArchiveEvidence);
  if (typeof value === "object" && value !== null) {
    return Object.fromEntries(
      Object.entries(value).map(([key, child]) => [
        key,
        localArchiveEvidence(child),
      ]),
    );
  }
  return value;
};
const localArchiveBudgets = new WeakMap<
  LocalArchiveObject,
  EvidenceGraphBudget
>();
const localArchiveObject = (value: unknown): LocalArchiveObject => {
  const budget = { nodes: 0, bytes: 0 };
  if (!evidenceWithinBounds(value, budget))
    return localRefuse("archive evidence bound exceeded");
  const bytes = Buffer.from(watcherCanonicalJson(value), "utf8");
  if (
    bytes.byteLength > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes
  )
    return localRefuse("archive byte bound exceeded");
  const object = Object.freeze({
    digest: sha256Bytes(bytes),
    bytesHex: bytes.toString("hex"),
  });
  localArchiveBudgets.set(object, Object.freeze(budget));
  return object;
};
const localHistoryAnchorDescriptor = (owner: LocalHistoryOwner) =>
  owner.archiveIndex === null
    ? {
        kind: "activation_origin",
        parent: owner.origin.parentPoint,
        bootstrapStoreDigest: owner.policy.bootstrapStoreDigest,
      }
    : {
        kind: "materialized_history",
        indexDigest: owner.archiveIndex.digest,
        indexSequence: owner.archiveIndex.index.indexSequence,
        retainedSuffixEntries: "64",
      };
const localRetainedEvidence = (
  owner: LocalHistoryOwner,
): readonly LocalRetainedEvidence[] =>
  [...owner.pinnedEvidence, ...owner.acceptedEvidence].sort((left, right) =>
    BigInt(left.entry.sequence) < BigInt(right.entry.sequence)
      ? -1
      : BigInt(left.entry.sequence) > BigInt(right.entry.sequence)
        ? 1
        : 0,
  );

const localLivePair = (owner: LocalHistoryOwner, pair: LocalPair) => {
  const scripts = readWatcherUserEventScriptBinding({
    binding: owner.scriptBinding,
    deploymentIdentity: owner.deploymentIdentity,
  });
  const current = readWatcherLocalBackfillFinalityObservation(pair);
  const witness = readWatcherLocalBackfillFinalityOriginalWitness(pair);
  const evidence = readWatcherUserEventReferenceEvidence(
    pair.referenceAuthority,
  );
  const referenceEvidence = admitWatcherLocalBackfillUserEventReferenceEvidence(
    {
      ...pair,
      evidence,
      deploymentIdentity: owner.deploymentIdentity,
    },
  );
  if (
    scripts !== owner.origin.scripts ||
    referenceEvidence !== evidence ||
    witness.current.finality !== current.finality ||
    witness.current.observation !== current.observation ||
    !same(current.finality.policy, owner.finalityPolicy) ||
    !same(
      current.observation.capture.sourceBinding,
      owner.origin.originalWitness.current.observation.capture.sourceBinding,
    ) ||
    current.observation.sourceIdentityDigest !==
      owner.origin.originalWitness.current.observation.sourceIdentityDigest ||
    witness.first.observation.capture.nativeBlock.rawBlockCbor !==
      current.observation.capture.nativeBlock.rawBlockCbor ||
    !same(
      witness.first.observation.capture.predecessorPoint,
      current.observation.capture.predecessorPoint,
    )
  ) {
    return localRefuse("live finality/reference/source binding differs");
  }
  return { witness, referenceEvidence };
};

/** Empty initialization is available only at the authenticated whole activation block. */
const createLocalUserEventHistory = (
  input: Readonly<{
    origin: WatcherUserEventOriginReceipt;
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    scriptBinding: WatcherUserEventScriptBinding;
    finality: WatcherLocalBackfillFinalityReceipt;
    observation: WatcherLocalBackfillObservationReceipt;
    publication: WatcherProtectedUserEventCheckpoint;
    semanticReplay: boolean;
  }>,
): WatcherLocalUserEventHistory => {
  const {
    origin: originReceipt,
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
    publication: publicationReceipt,
  } = input;
  const origin = readWatcherUserEventOrigin({
    origin: originReceipt,
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
  });
  const publication =
    readWatcherProtectedUserEventCheckpointReceipt(publicationReceipt);
  const finalityPolicy = origin.originalWitness.current.finality.policy;
  if (
    (!input.semanticReplay &&
      (publication.checkpoint !== null || publication.payload !== null)) ||
    !same(
      publication.trustedHead.deploymentMarker,
      deploymentIdentity.durableMarker,
    ) ||
    !same(finalityPolicy.deploymentMarker, deploymentIdentity.durableMarker) ||
    finalityPolicy.blueprintHash !== origin.blueprintHash ||
    finalityPolicy.network !== origin.network
  ) {
    return localRefuse(
      "empty origin requires an absent matching protected checkpoint",
    );
  }
  const store = immutableWireValue(
    makeEmptyWatcherDurableStore(deploymentIdentity.durableMarker),
  );
  const parsedPolicy = makeWatcherUserEventIndexerPolicy({
    network: origin.network,
    blueprintHash: origin.blueprintHash,
    deploymentMarker: deploymentIdentity.durableMarker,
    deposit: origin.scripts.deposit,
    withdrawal: origin.scripts.withdrawal,
    forcedOrder: origin.scripts.forcedOrder,
    bootstrapStoreDigest: storeDigest(store),
    deploymentTrustRootId: deploymentIdentity.trustRootId,
    requiredFinalityDepth: finalityPolicy.confirmationDepth,
    maximumActiveHistoryEntries:
      WATCHER_USER_EVENT_INDEXER_BOUNDS.activeHistoryEntries.toString(),
    maximumAuditHistoryEntries:
      WATCHER_USER_EVENT_INDEXER_BOUNDS.auditHistoryEntries.toString(),
  });
  if (parsedPolicy === null)
    return localRefuse("origin cannot establish the strict event policy");
  const policy = immutableWireValue(parsedPolicy);
  const snapshot = makeSnapshot([], []);
  if (snapshot === null)
    return localRefuse("empty snapshot construction failed");
  const originArchive = localArchiveObject({
    schemaVersion: "midgard-watcher-local-user-event-origin-archive-v1",
    numericEncoding: "exact-decimal-strings",
    facts: localArchiveEvidence(origin),
    policy,
    bootstrapStore: store,
  });
  const history = Object.freeze({ [localHistoryBrand]: true as const });
  localHistories.set(history, {
    origin,
    originDigest: origin.originDigest,
    activationPair: Object.freeze({
      finality: finality,
      observation: observation,
    }),
    deploymentIdentity: deploymentIdentity,
    scriptBinding: scriptBinding,
    policy,
    finalityPolicy,
    originArchive,
    store,
    snapshot: immutableWireValue(snapshot),
    entries: Object.freeze([]),
    acceptedEvidence: Object.freeze([]),
    pinnedEvidence: Object.freeze([]),
    archiveIndex: null,
    anchorCandidate: null,
    lastAccepted: null,
    archiveObjects: Object.freeze([originArchive]),
    checkpoint: null,
    candidate: null,
    generation: 0,
    acceptedAtMonotonicMs: null,
    closed: false,
    suspendedAt: null,
    semanticReplay: input.semanticReplay,
  });
  return history;
};

/** Empty initialization remains unavailable over a published checkpoint. */
export const createWatcherLocalUserEventHistory = (
  input: Omit<
    Parameters<typeof createLocalUserEventHistory>[0],
    "semanticReplay"
  >,
): WatcherLocalUserEventHistory =>
  createLocalUserEventHistory({ ...input, semanticReplay: false });

export const readWatcherLocalUserEventHistory = (
  history: WatcherLocalUserEventHistory,
) => {
  const owner = localOwner(history);
  return Object.freeze({
    policy: owner.policy,
    store: owner.store,
    snapshot: owner.snapshot,
    cursor: owner.entries.at(-1)?.cursor ?? null,
    entryDigest: owner.entries.at(-1)?.entryDigest ?? null,
    checkpoint: owner.checkpoint,
    retainedEntries: owner.entries.length,
    status:
      owner.candidate !== null || owner.anchorCandidate !== null
        ? ("publication_pending" as const)
        : owner.entries.length >=
            Number(owner.policy.maximumActiveHistoryEntries)
          ? ("history_bound_hold" as const)
          : ("ready" as const),
  });
};

export const prepareWatcherLocalUserEventTransition = (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      publication: WatcherProtectedUserEventCheckpoint;
    }>,
): WatcherLocalUserEventTransition => {
  const {
    history,
    publication: receipt,
    finality,
    observation,
    referenceAuthority,
  } = input;
  const owner = localOwner(history);
  if (owner.semanticReplay)
    return localRefuse("semantic replay has not been published");
  const publication = readWatcherProtectedUserEventCheckpointReceipt(receipt);
  if (!same(publication.checkpoint, owner.checkpoint))
    return localRefuse(
      "protected predecessor differs; semantic reconciliation required",
    );
  const transition = prepareLocalUserEventTransition(
    history,
    Object.freeze({ finality, observation, referenceAuthority }),
  );
  readWatcherProtectedUserEventCheckpointReceipt(receipt);
  return transition;
};

const prepareLocalUserEventTransition = (
  history: WatcherLocalUserEventHistory,
  pair: LocalPair,
): WatcherLocalUserEventTransition => {
  const owner = localOwner(history);
  if (owner.anchorCandidate !== null)
    return localRefuse("anchor publication is unresolved");
  const live = localLivePair(owner, pair);
  if (owner.candidate !== null) {
    const candidate = localTransitions.get(owner.candidate)!;
    if (
      candidate.pair.finality === pair.finality &&
      candidate.pair.observation === pair.observation &&
      candidate.pair.referenceAuthority === pair.referenceAuthority
    )
      return owner.candidate;
    return localRefuse("a different publication is unresolved");
  }
  if (owner.lastAccepted !== null) {
    const accepted = localTransitions.get(owner.lastAccepted)!;
    if (
      accepted.pair.finality === pair.finality &&
      accepted.pair.observation === pair.observation &&
      accepted.pair.referenceAuthority === pair.referenceAuthority
    )
      return owner.lastAccepted;
  }
  if (
    owner.entries.length >= Number(owner.policy.maximumActiveHistoryEntries) ||
    owner.entries.length >= Number(owner.policy.maximumAuditHistoryEntries)
  ) {
    return localRefuse(
      "history bound reached; semantic anchor rotation required",
    );
  }
  const { witness, referenceEvidence } = live;
  const { native: block, capture } = witness.current.observation;
  const predecessor = owner.entries.at(-1);
  if (predecessor === undefined) {
    if (
      pair.finality !== owner.activationPair.finality ||
      pair.observation !== owner.activationPair.observation ||
      block !== owner.origin.block
    )
      return localRefuse("first block is not the exact activation pair");
  } else if (
    !same(capture.predecessorPoint, predecessor.cursor) ||
    block.chainPoint.parentBlockHash !== predecessor.cursor.blockHash ||
    BigInt(capture.point.blockNo) !== BigInt(predecessor.cursor.blockNo) + 1n ||
    BigInt(capture.point.slot) <= BigInt(predecessor.cursor.slot)
  ) {
    return localRefuse("block is not the strict full-point successor");
  }
  const derivedSnapshot = deriveLocalBlockEventSnapshot(
    owner.policy,
    owner.snapshot,
    block,
    referenceEvidence,
    {
      appliedScriptHashes: { hubOracleMint: owner.origin.scripts.hub.policyId },
    },
  );
  if (derivedSnapshot === null)
    return localRefuse("whole-block event semantics differ");
  const snapshot = immutableWireValue(derivedSnapshot);
  const sourceStore = owner.store;
  const chainPoints = [
    ...sourceStore.chainPoints,
    {
      chainPointId: block.chainPoint.chainPointId,
      providerId: block.provider.providerId,
      blockHash: block.chainPoint.blockHash,
      slot: block.chainPoint.slot,
      blockNo: block.chainPoint.blockNo,
      depth: block.chainPoint.depth,
    },
  ];
  const journal = journalWatcherProtocolUtxoTransition({
    sourceStore,
    nextChainPoints: chainPoints,
    spentAtChainPointId: block.chainPoint.chainPointId,
    nextProtocolUtxos: [
      ...sourceStore.protocolUtxos.filter(
        ({ role }) =>
          !["deposit", "withdrawal", "forced_transaction"].includes(role),
      ),
      ...snapshot.activeEvents.map((event) => ({
        outRef: event.outRef,
        role: protocolRole(event.kind),
        chainPointId: event.originChainPointId,
        output: makeWatcherDurablePayload(event.outputCborHex),
      })),
    ],
  });
  const nextStore = immutableWireValue(
    makeWatcherDurableStore({
      deploymentMarker: sourceStore.deploymentMarker,
      revision: (BigInt(sourceStore.revision) + 1n).toString(),
      records: {
        ...sourceStore,
        chainPoints,
        ...journal,
        l1Observations: [
          ...sourceStore.l1Observations,
          {
            observationId: block.observationDigest,
            providerId: block.provider.providerId,
            chainPointId: block.chainPoint.chainPointId,
            payload: makeWatcherDurablePayload(
              encodeWatcherNormalizedL1Block(block).toString("hex"),
            ),
          },
        ],
      },
    }),
  );
  if (!storeTransitionMatches(sourceStore, nextStore, block, snapshot))
    return localRefuse("event view journal differs");
  const observation = makeObservation({
    schemaVersion: WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION,
    policyDigest: owner.policy.policyDigest,
    network: owner.policy.network,
    blueprintHash: owner.policy.blueprintHash,
    deploymentMarker: owner.policy.deploymentMarker,
    transitionKind: "apply_block",
    pointDigest: block.chainPoint.pointDigest,
    blockHash: block.chainPoint.blockHash,
    slot: block.chainPoint.slot,
    blockNo: block.chainPoint.blockNo,
    sourceObservationDigest: block.observationDigest,
    chainPointId: block.chainPoint.chainPointId,
    sourceDurableStoreDigest: storeDigest(sourceStore),
    sourceDurableStoreRevision: sourceStore.revision,
    durableStoreDigest: storeDigest(nextStore),
    durableStoreRevision: nextStore.revision,
    rollbackTargetEntryDigest: null,
    snapshot,
  });
  const evidence = localArchiveObject({
    schemaVersion: "midgard-watcher-local-user-event-block-evidence-v1",
    numericEncoding: "exact-decimal-strings",
    witnesses: localArchiveEvidence(witness),
    referenceEvidence,
  });
  const entryFields = {
    schemaVersion: "midgard-watcher-local-user-event-entry-v1" as const,
    sequence:
      predecessor === undefined
        ? "0"
        : (BigInt(predecessor.sequence) + 1n).toString(),
    originDigest: owner.originDigest,
    policyDigest: owner.policy.policyDigest,
    predecessorEntryDigest: predecessor?.entryDigest ?? null,
    predecessorStateDigest: owner.checkpoint?.payloadDigest ?? null,
    cursor: capture.point,
    parent: capture.predecessorPoint,
    sourceStoreDigest: observation.sourceDurableStoreDigest,
    nextStoreDigest: observation.durableStoreDigest,
    sourceStoreRevision: sourceStore.revision,
    nextStoreRevision: nextStore.revision,
    observationDigest: observation.observationDigest,
    snapshotDigest: snapshot.snapshotDigest,
    evidenceDigest: evidence.digest,
  };
  const entry = Object.freeze({
    ...entryFields,
    entryDigest: sha256Canonical(entryFields),
  });
  const entryArchive = localArchiveObject({ entry, observation });
  const storeArchive = localArchiveObject(nextStore);
  const payload = localArchiveObject({
    schemaVersion: "midgard-watcher-local-user-event-checkpoint-payload-v1",
    originArchiveDigest: owner.originArchive.digest,
    originDigest: owner.originDigest,
    policy: owner.policy,
    anchor: localHistoryAnchorDescriptor(owner),
    head: entry,
    storeArchiveDigest: storeArchive.digest,
    snapshot,
    retainedEntries: [...owner.entries, entry],
    requiredSemanticResume:
      "authenticated_origin_replay_or_semantic_publication_receipt",
  });
  const archiveObjects = Object.freeze([
    ...owner.archiveObjects,
    evidence,
    entryArchive,
    storeArchive,
    payload,
  ]);
  if (
    archiveObjects.reduce(
      (nodes, object) => nodes + localArchiveBudgets.get(object)!.nodes,
      0,
    ) > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes ||
    archiveObjects.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries ||
    archiveObjects.reduce(
      (bytes, object) => bytes + object.bytesHex.length / 2,
      0,
    ) > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes
  )
    return localRefuse("retained archive bound reached");
  const nextCheckpoint = makeWatcherUserEventCheckpoint({
    schemaVersion: WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
    deploymentMarker: owner.policy.deploymentMarker,
    network: owner.policy.network,
    blueprintHash: owner.policy.blueprintHash,
    finalityPolicyDigest: owner.finalityPolicy.policyDigest,
    userEventPolicyDigest: owner.policy.policyDigest,
    checkpointSequence:
      owner.checkpoint === null
        ? "0"
        : (BigInt(owner.checkpoint.checkpointSequence) + 1n).toString(),
    predecessorCheckpointDigest: owner.checkpoint?.checkpointDigest ?? null,
    rollbackGeneration: owner.checkpoint?.rollbackGeneration ?? "0",
    payloadDigest: payload.digest,
    requiredArchiveDigests: [
      ...new Set(archiveObjects.map(({ digest }) => digest)),
    ].sort(),
  });
  const rechecked = localLivePair(owner, pair);
  if (
    rechecked.witness.first !== witness.first ||
    rechecked.witness.current !== witness.current ||
    rechecked.referenceEvidence !== referenceEvidence
  )
    return localRefuse("candidate evidence changed");
  const transition = Object.freeze({ [localTransitionBrand]: true as const });
  const value = Object.freeze({
    sourceStore,
    nextStore,
    observation,
    snapshot,
    entry,
    archiveObjects,
    nextCheckpoint,
    expectedCheckpointDigest: owner.checkpoint?.checkpointDigest ?? null,
    expectedCheckpointSequence: owner.checkpoint?.checkpointSequence ?? null,
  });
  localTransitions.set(transition, {
    history: history,
    generation: owner.generation,
    pair,
    witness,
    referenceEvidence,
    value,
    accepted: false,
  });
  owner.candidate = transition;
  return transition;
};

export const readWatcherLocalUserEventTransition = (
  transition: WatcherLocalUserEventTransition,
): LocalPreparedRead => {
  const prepared =
    localTransitions.get(transition) ??
    localRefuse("transition is not privately admitted");
  const owner = localOwner(prepared.history);
  if (prepared.accepted) {
    if (
      owner.lastAccepted !== transition ||
      owner.checkpoint?.checkpointDigest !==
        prepared.value.nextCheckpoint.checkpointDigest
    )
      return localRefuse("accepted transition is no longer the head");
  } else if (
    owner.generation !== prepared.generation ||
    owner.candidate !== transition
  )
    return localRefuse("transition is no longer pending");
  const live = localLivePair(owner, prepared.pair);
  if (
    live.witness.first !== prepared.witness.first ||
    live.witness.current !== prepared.witness.current ||
    live.referenceEvidence !== prepared.referenceEvidence
  )
    return localRefuse("prepared evidence differs");
  return prepared.value;
};

/** Exact protected publication advances the private cursor once, never preparation. */
export const acceptWatcherLocalUserEventPublication = (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    transition: WatcherLocalUserEventTransition;
    publication: WatcherProtectedUserEventCheckpoint;
  }>,
) => {
  const { history, transition, publication: publicationReceipt } = input;
  const owner = localOwner(history);
  const prepared =
    localTransitions.get(transition) ??
    localRefuse("transition is not privately admitted");
  const publication =
    readWatcherProtectedUserEventCheckpointReceipt(publicationReceipt);
  if (
    prepared.history !== history ||
    !same(publication.checkpoint, prepared.value.nextCheckpoint) ||
    publication.payload === null ||
    sha256Bytes(publication.payload) !==
      prepared.value.nextCheckpoint.payloadDigest
  )
    return localRefuse(
      "publication does not match the exact prepared frame and payload",
    );
  if (prepared.accepted) {
    if (
      owner.checkpoint?.checkpointDigest !==
      prepared.value.nextCheckpoint.checkpointDigest
    )
      return localRefuse("accepted publication is no longer the head");
    return Object.freeze({
      entryDigest: prepared.value.entry.entryDigest,
      cursor: prepared.value.entry.cursor,
    });
  }
  if (owner.semanticReplay)
    return localRefuse("semantic replay requires readmission publication");
  return commitLocalUserEventTransition(history, transition);
};

const commitLocalUserEventTransition = (
  history: WatcherLocalUserEventHistory,
  transition: WatcherLocalUserEventTransition,
) => {
  const owner = localOwner(history);
  const prepared =
    localTransitions.get(transition) ??
    localRefuse("transition is not privately admitted");
  if (prepared.history !== history || prepared.accepted)
    return localRefuse("transition is not a pending step of this owner");
  readWatcherLocalUserEventTransition(transition);
  owner.store = prepared.value.nextStore;
  owner.snapshot = prepared.value.snapshot;
  owner.entries = Object.freeze([...owner.entries, prepared.value.entry]);
  owner.acceptedEvidence = Object.freeze([
    ...owner.acceptedEvidence,
    Object.freeze({
      entry: prepared.value.entry,
      entryArchiveDigest: localArchiveObject({
        entry: prepared.value.entry,
        observation: prepared.value.observation,
      }).digest,
      rawBlockCbor:
        prepared.witness.current.observation.capture.nativeBlock.rawBlockCbor,
      pointDigest:
        prepared.witness.current.observation.native.chainPoint.pointDigest,
      chainPointId:
        prepared.witness.current.observation.native.chainPoint.chainPointId,
    }),
  ]);
  owner.lastAccepted = transition;
  owner.archiveObjects = prepared.value.archiveObjects;
  owner.checkpoint = prepared.value.nextCheckpoint;
  owner.generation += 1;
  owner.acceptedAtMonotonicMs = performance.now();
  owner.candidate = null;
  prepared.accepted = true;
  return Object.freeze({
    entryDigest: prepared.value.entry.entryDigest,
    cursor: prepared.value.entry.cursor,
  });
};

/** Closing an owner revokes every transition and event capability it issued. */
export const closeWatcherLocalUserEventHistory = (
  history: WatcherLocalUserEventHistory,
): void => {
  const owner =
    localHistories.get(history) ??
    localRefuse("history is not privately admitted");
  owner.closed = true;
};

/** Retire every issued capability synchronously before rollback recovery awaits. */
export const suspendWatcherLocalUserEventHistory = (
  history: WatcherLocalUserEventHistory,
): void => {
  const owner = localHistories.get(history) ?? localRefuse("unknown history");
  if (owner.closed) return localRefuse("history is closed");
  owner.generation += 1;
  owner.suspendedAt = performance.now();
};

/** Same-process recovery preserves the accepted fold only when new native W12
 * evidence and a genuine protected read still identify that exact publication. */
export const resumeWatcherLocalUserEventHistory = (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      publication: WatcherProtectedUserEventCheckpoint;
    }>,
): void => {
  const owner =
    localHistories.get(input.history) ?? localRefuse("unknown history");
  const head = owner.entries.at(-1);
  const accepted = owner.acceptedEvidence.at(-1);
  const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
    input.publication,
  );
  if (
    owner.closed ||
    owner.suspendedAt === null ||
    owner.semanticReplay ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    head === undefined ||
    accepted === undefined ||
    owner.checkpoint === null ||
    owner.snapshot.quarantined ||
    !same(protectedHead.checkpoint, owner.checkpoint) ||
    protectedHead.payload === null ||
    sha256Bytes(protectedHead.payload) !== owner.checkpoint.payloadDigest
  )
    return localRefuse("suspended history requires restart reconciliation");
  const { witness } = localLivePair(owner, input);
  if (
    witness.first.observation.capture.startedAtMonotonicMs <
      owner.suspendedAt ||
    !same(witness.current.observation.capture.point, head.cursor) ||
    witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      accepted.rawBlockCbor
  )
    return localRefuse(
      "rollback recovery does not freshly corroborate the accepted head",
    );
  readWatcherProtectedUserEventCheckpointReceipt(input.publication);
  owner.generation += 1;
  owner.acceptedAtMonotonicMs = performance.now();
  owner.suspendedAt = null;
};

const localUnavailableErrors = new WeakSet<Error>();
const localAuthorityUnavailable = (reason: string): never => {
  const error = new Error(`Local user-event authority unavailable: ${reason}`);
  localUnavailableErrors.add(error);
  throw error;
};
/** Only an ordinary candidate's unavailable event/header membership is recoverable.
 * Callers must still freshly fence protected-head, native lease and generation. */
export const isWatcherLocalUserEventAuthorityUnavailable = (
  error: unknown,
): error is Error =>
  error instanceof Error && localUnavailableErrors.has(error);

/** Refresh the protected checkpoint without requiring any particular event. */
export const assertWatcherLocalUserEventHeadCurrent = async (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      runtime: WatcherDurableRuntime;
    }>,
): Promise<void> => {
  const owner = localOwner(input.history);
  const generation = owner.generation;
  const checkpoint = owner.checkpoint;
  const assertCurrent = () => {
    const head = owner.entries.at(-1);
    const accepted = owner.acceptedEvidence.at(-1);
    const finality = input.runtime.read().currentFinalityState;
    if (
      localOwner(input.history) !== owner ||
      owner.generation !== generation ||
      owner.checkpoint !== checkpoint ||
      checkpoint === null ||
      owner.semanticReplay ||
      owner.candidate !== null ||
      owner.anchorCandidate !== null ||
      owner.snapshot.quarantined ||
      owner.acceptedAtMonotonicMs === null ||
      head === undefined ||
      accepted === undefined ||
      finality.phase === "quarantined" ||
      finality.incident !== null
    )
      return localRefuse("user-event protected head is no longer current");
    const { witness } = localLivePair(owner, input);
    if (
      witness.first.observation.capture.startedAtMonotonicMs <
        owner.acceptedAtMonotonicMs ||
      !same(witness.current.observation.capture.point, head.cursor) ||
      witness.current.observation.capture.nativeBlock.rawBlockCbor !==
        accepted.rawBlockCbor
    )
      return localRefuse(
        "user-event head lease is not fresh exact W12 evidence",
      );
  };
  assertCurrent();
  const receipt = await readWatcherProtectedUserEventCheckpoint(input.runtime);
  assertCurrent();
  const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(receipt);
  if (
    checkpoint === null ||
    !same(protectedHead.checkpoint, checkpoint) ||
    protectedHead.payload === null ||
    sha256Bytes(protectedHead.payload) !== checkpoint.payloadDigest
  )
    return localRefuse(
      "user-event protected head differs after candidate refusal",
    );
};

const localEventAuthorityBrand = Symbol("watcher-local-user-event-authority");
export type WatcherLocalUserEventAuthority = Readonly<{
  [localEventAuthorityBrand]: true;
}>;
export type WatcherLocalUserEventAuthorityRead = Readonly<{
  deploymentManifestId: string;
  blueprintHash: string;
  network: WatcherUserEventIndexerPolicy["network"];
  event: WatcherIndexedUserEvent | WatcherTerminalUserEvent;
  throughHeader: WatcherLocalUserEventHeaderCutoff | null;
  checkpointDigest: string;
  checkpointPayloadDigest: string;
  snapshotDigest: string;
  headEntryDigest: string;
  historyEntryDigests: readonly string[];
}>;
type LocalEventAuthorityOwner = Readonly<{
  history: WatcherLocalUserEventHistory;
  header: WatcherStateQueueHeaderObservation | null;
  runtime: WatcherDurableRuntime;
  pair: LocalPair;
  generation: number;
  value: WatcherLocalUserEventAuthorityRead;
  protectedRead: { receipt: WatcherProtectedUserEventCheckpoint | null };
}>;
const localEventAuthorities = new WeakMap<
  WatcherLocalUserEventAuthority,
  LocalEventAuthorityOwner
>();

export type WatcherLocalUserEventHeaderCutoff = Readonly<{
  headerHash: string;
  headerCborHex: string;
  queueOutRef: string;
  observedTransactionHash: string;
  observedBlockHash: string;
  observedSlot: string;
  observedBlockNo: string;
  transactionIndex: string;
  historyEntryDigest: string;
}>;

const localHeaderFields = (header: WatcherStateQueueHeaderObservation) => {
  assertWatcherStateQueueHeaderObservation(header);
  return Object.freeze({
    headerHash: header.headerHash,
    headerCborHex: header.headerCborHex,
    queueOutRef: header.queueOutRef,
    observedTransactionHash: header.observedTransactionHash,
    observedBlockHash: header.observedBlockHash,
    observedSlot: header.observedSlot,
    observedBlockNo: header.observedBlockNo,
  });
};

const localReadArchivedValue = async (
  archive: WatcherUserEventArchive,
  digest: string,
): Promise<unknown> => {
  if (!isHex32(digest))
    return localRefuse("historical cutoff archive digest differs");
  const bytes = await archive.read(digest);
  if (
    bytes === null ||
    bytes.byteLength >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes ||
    sha256Bytes(bytes) !== digest
  )
    return localRefuse("historical cutoff archive is absent or corrupt");
  let value: unknown;
  try {
    value = JSON.parse(new TextDecoder("utf-8", { fatal: true }).decode(bytes));
  } catch {
    return localRefuse("historical cutoff archive is not JSON");
  }
  if (
    !evidenceWithinBounds(value, { nodes: 0, bytes: 0 }) ||
    localArchiveObject(value).digest !== digest
  )
    return localRefuse("historical cutoff archive encoding or bounds differ");
  return value;
};

const localHeaderBlock = async (
  owner: LocalHistoryOwner,
  header: Pick<
    ReturnType<typeof localHeaderFields>,
    "observedBlockHash" | "observedBlockNo" | "observedSlot"
  >,
  archive: WatcherUserEventArchive,
): Promise<
  Readonly<{ entry: WatcherLocalUserEventEntry; rawBlockCbor: string }>
> => {
  const sequence =
    BigInt(header.observedBlockNo) -
    BigInt(owner.origin.block.chainPoint.blockNo);
  const head = owner.entries.at(-1)!;
  if (sequence < 0n || sequence > BigInt(head.sequence))
    return localRefuse(
      "header cutoff lies outside the published event history",
    );
  const root = owner.archiveIndex;
  const originDigest = owner.originDigest;
  const policyDigest = owner.policy.policyDigest;
  const retained = localRetainedEvidence(owner).find(
    ({ entry }) => BigInt(entry.sequence) === sequence,
  );
  let entry: WatcherLocalUserEventEntry;
  let rawBlockCbor: unknown;
  if (retained !== undefined) {
    entry = retained.entry;
    rawBlockCbor = retained.rawBlockCbor;
  } else {
    if (root === null)
      return localRefuse("header cutoff entry is not retained or sealed");
    const segment = await findWatcherUserEventArchiveIndexForEntry(
      archive,
      root,
      sequence.toString(),
    );
    const payload = await localReadArchivedValue(
      archive,
      segment.index.sourcePayloadDigest,
    );
    const entriesValue = localArchiveField(payload, ["retainedEntries"]);
    if (
      !Array.isArray(entriesValue) ||
      entriesValue.length > Number(owner.policy.maximumActiveHistoryEntries) ||
      localArchiveField(payload, ["originDigest"]) !== originDigest ||
      !same(localArchiveField(payload, ["policy"]), owner.policy)
    )
      return localRefuse("historical cutoff segment payload differs");
    const entries = entriesValue.map(localArchivedEntry);
    const matches = entries.filter(
      (candidate) => BigInt(candidate.sequence) === sequence,
    );
    if (
      matches.length !== 1 ||
      !same(localArchiveField(payload, ["head"]), entries.at(-1))
    )
      return localRefuse("historical cutoff entry is not uniquely archived");
    entry = matches[0]!;
    if (!segment.index.sourceArchiveDigests.includes(entry.evidenceDigest))
      return localRefuse(
        "historical cutoff evidence is not in the sealed closure",
      );
    const evidence = await localReadArchivedValue(
      archive,
      entry.evidenceDigest,
    );
    if (
      localArchiveField(evidence, ["schemaVersion"]) !==
        "midgard-watcher-local-user-event-block-evidence-v1" ||
      localArchiveField(evidence, ["numericEncoding"]) !==
        "exact-decimal-strings"
    )
      return localRefuse("historical cutoff evidence framing differs");
    rawBlockCbor = localArchiveField(evidence, [
      "witnesses",
      "current",
      "observation",
      "capture",
      "nativeBlock",
      "rawBlockCbor",
    ]);
    for (const step of ["first", "current"] as const) {
      if (
        localArchiveField(evidence, [
          "witnesses",
          step,
          "observation",
          "capture",
          "nativeBlock",
          "rawBlockCbor",
        ]) !== rawBlockCbor ||
        !same(
          localArchiveField(evidence, [
            "witnesses",
            step,
            "observation",
            "capture",
            "point",
          ]),
          entry.cursor,
        ) ||
        !same(
          localArchiveField(evidence, [
            "witnesses",
            step,
            "observation",
            "capture",
            "predecessorPoint",
          ]),
          entry.parent,
        )
      )
        return localRefuse(
          "historical cutoff original witness binding differs",
        );
    }
  }
  if (
    entry.originDigest !== originDigest ||
    entry.policyDigest !== policyDigest ||
    entry.cursor.blockHash !== header.observedBlockHash ||
    entry.cursor.slot !== header.observedSlot ||
    entry.cursor.blockNo !== header.observedBlockNo ||
    typeof rawBlockCbor !== "string" ||
    !isHexBytes(rawBlockCbor)
  )
    return localRefuse("header cutoff is not the exact accepted block");
  return Object.freeze({ entry, rawBlockCbor });
};

/** Pure decoding of the already admitted lineage's original bytes. This creates
 * neither a native acquisition receipt nor fresh W12 finality authority.
 */
const localCutoffTransactionOrder = (
  block: Readonly<{ entry: WatcherLocalUserEventEntry; rawBlockCbor: string }>,
) => {
  const decoded = CML.Block.from_cbor_hex(block.rawBlockCbor);
  const header = decoded.header();
  const body = header.header_body();
  if (
    decoded.to_cbor_hex() !== block.rawBlockCbor ||
    Buffer.from(
      blake2b(Buffer.from(header.to_cbor_hex(), "hex"), { dkLen: 32 }),
    ).toString("hex") !== block.entry.cursor.blockHash ||
    body.slot().toString() !== block.entry.cursor.slot ||
    body.block_number().toString() !== block.entry.cursor.blockNo ||
    body.prev_hash()?.to_hex() !== block.entry.parent.blockHash
  )
    return localRefuse("historical cutoff raw header differs");
  const bodies = decoded.transaction_bodies();
  const transactionIds = Array.from({ length: bodies.len() }, (_, index) =>
    CML.hash_transaction(bodies.get(index)).to_hex(),
  );
  if (new Set(transactionIds).size !== transactionIds.length)
    return localRefuse("historical cutoff transaction order is ambiguous");
  return {
    bodies,
    transactionIds,
    invalidTransactions: new Set(decoded.invalid_transactions()),
  };
};

const localEventAtHeaderCutoff = async (
  owner: LocalHistoryOwner,
  event: WatcherIndexedUserEvent,
  terminal: WatcherTerminalUserEvent | null,
  originEvidence: LocalRetainedEvidence,
  terminalEvidence: LocalRetainedEvidence,
  header: WatcherStateQueueHeaderObservation,
  archive: WatcherUserEventArchive,
) => {
  const fields = localHeaderFields(header);
  const block = await localHeaderBlock(owner, fields, archive);
  const ordered = localCutoffTransactionOrder(block);
  const transactionIndex = ordered.transactionIds.indexOf(
    fields.observedTransactionHash,
  );
  if (transactionIndex < 0 || ordered.invalidTransactions.has(transactionIndex))
    return localRefuse("header cutoff transaction is not validly included");
  const outRef = fields.queueOutRef.split("#");
  if (
    outRef.length !== 2 ||
    outRef[0] !== fields.observedTransactionHash ||
    !isNatural(outRef[1]) ||
    BigInt(outRef[1]) >=
      BigInt(ordered.bodies.get(transactionIndex).outputs().len())
  )
    return localRefuse("header cutoff output reference differs");
  const output = ordered.bodies
    .get(transactionIndex)
    .outputs()
    .get(Number(outRef[1]));
  if (
    output.datum()?.as_datum()?.to_canonical_cbor_hex() !==
      header.linkedListDatumCborHex ||
    Buffer.from(
      blake2b(Buffer.from(fields.headerCborHex, "hex"), { dkLen: 28 }),
    ).toString("hex") !== fields.headerHash
  )
    return localRefuse("header cutoff output or header bytes differ");
  const occursThroughHeader = (
    evidence: LocalRetainedEvidence,
    transactionHash: string,
  ): boolean => {
    const entrySequence = BigInt(evidence.entry.sequence);
    if (entrySequence !== BigInt(block.entry.sequence))
      return entrySequence < BigInt(block.entry.sequence);
    if (!same(evidence.entry, block.entry))
      return localRefuse("event cutoff entry membership differs");
    const index = ordered.transactionIds.indexOf(transactionHash);
    if (index < 0 || ordered.invalidTransactions.has(index))
      return localRefuse("event cutoff transaction is not validly included");
    return index <= transactionIndex;
  };
  if (!occursThroughHeader(originEvidence, event.transactionHash))
    return localAuthorityUnavailable(
      "event origin occurs after the challenged header",
    );
  let selected: WatcherIndexedUserEvent | WatcherTerminalUserEvent = event;
  let includeTerminal = false;
  if (terminal !== null) {
    includeTerminal = occursThroughHeader(
      terminalEvidence,
      terminal.terminalTransactionHash,
    );
    if (includeTerminal) selected = terminal;
    else {
      const {
        terminalStatus: _status,
        terminalTransactionHash: _tx,
        terminalPointDigest: _point,
        terminalBlockHash: _hash,
        terminalSlot: _slot,
        terminalBlockNo: _number,
        terminalFinalityStatus: _finality,
        terminalClassification: _classification,
        ...origin
      } = terminal;
      selected = Object.freeze(origin);
    }
  }
  if (!same(localHeaderFields(header), fields))
    return localRefuse("header cutoff changed during its archive read");
  return Object.freeze({
    event: selected,
    includeTerminal,
    throughHeader: Object.freeze({
      ...fields,
      transactionIndex: transactionIndex.toString(),
      historyEntryDigest: block.entry.entryDigest,
    }),
  });
};

const localEventAuthorityCurrent = (
  authority: LocalEventAuthorityOwner,
): LocalHistoryOwner => {
  const owner = localOwner(authority.history);
  if (authority.header !== null) {
    const cutoff = authority.value.throughHeader;
    if (cutoff === null) return localRefuse("event header cutoff is absent");
    const {
      transactionIndex: _index,
      historyEntryDigest: _entry,
      ...fields
    } = cutoff;
    if (!same(localHeaderFields(authority.header), fields))
      return localRefuse("event header cutoff is no longer identical");
  }
  const head = owner.entries.at(-1);
  const accepted = owner.acceptedEvidence.at(-1);
  if (
    owner.semanticReplay ||
    owner.generation !== authority.generation ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    owner.checkpoint === null ||
    owner.acceptedAtMonotonicMs === null ||
    owner.snapshot.quarantined ||
    head === undefined ||
    accepted === undefined ||
    owner.checkpoint.checkpointDigest !== authority.value.checkpointDigest ||
    head.entryDigest !== authority.value.headEntryDigest
  )
    return localRefuse(
      "event authority no longer matches the published semantic head",
    );
  // This is a new corroboration, acquired after publication. Original archived
  // W12 observations and depths remain unchanged and are never revived from JSON.
  const { witness } = localLivePair(owner, authority.pair);
  if (
    witness.first.observation.capture.startedAtMonotonicMs <
      owner.acceptedAtMonotonicMs ||
    !same(witness.current.observation.capture.point, head.cursor) ||
    witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      accepted.rawBlockCbor
  )
    return localRefuse(
      "event authority requires a fresh post-publication capture of the exact head",
    );
  return owner;
};

/** Final synchronous fence after all asynchronous authority reads. This checks
 * same-runtime protected-head changes as well as closure, source liveness and
 * private owner generation. The async reader remains necessary for disk freshness.
 */
export const assertWatcherLocalUserEventAuthorityCurrent = (
  receipt: WatcherLocalUserEventAuthority,
): void => {
  const authority =
    localEventAuthorities.get(receipt) ??
    localRefuse("event authority is not privately admitted");
  const owner = localEventAuthorityCurrent(authority);
  const publication = authority.protectedRead.receipt;
  const finality = authority.runtime.read().currentFinalityState;
  if (
    publication === null ||
    finality.phase === "quarantined" ||
    finality.incident !== null ||
    !same(
      readWatcherProtectedUserEventCheckpointReceipt(publication).checkpoint,
      owner.checkpoint,
    )
  )
    return localRefuse(
      "event authority protected checkpoint is no longer current",
    );
};

/** Descriptive output is never accepted as authority. Each read refreshes the
 * protected head and checks the still-live post-publication capture after await.
 * The runtime owner must close this history on a source rollback or shutdown.
 */
export const readWatcherLocalUserEventAuthority = async (
  receipt: WatcherLocalUserEventAuthority,
): Promise<WatcherLocalUserEventAuthorityRead> => {
  const authority =
    localEventAuthorities.get(receipt) ??
    localRefuse("event authority is not privately admitted");
  localEventAuthorityCurrent(authority);
  const publication = await readWatcherProtectedUserEventCheckpoint(
    authority.runtime,
  );
  const owner = localEventAuthorityCurrent(authority);
  const protectedHead =
    readWatcherProtectedUserEventCheckpointReceipt(publication);
  const finalityState = authority.runtime.read().currentFinalityState;
  if (
    finalityState.phase === "quarantined" ||
    finalityState.incident !== null ||
    !same(protectedHead.checkpoint, owner.checkpoint) ||
    protectedHead.payload === null ||
    sha256Bytes(protectedHead.payload) !==
      authority.value.checkpointPayloadDigest
  )
    return localRefuse(
      "event authority protected checkpoint is no longer current",
    );
  authority.protectedRead.receipt = publication;
  return authority.value;
};

/** Verify an older stream intersection against the accepted private/archive
 * lineage. A matching height alone never permits skipping native blocks. */
export const assertWatcherLocalUserEventPointCovered = async (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    point: WatcherUserEventOriginFacts["parentPoint"];
    runtime: WatcherDurableRuntime;
    archive: WatcherUserEventArchive;
  }>,
): Promise<void> => {
  const owner = localOwner(input.history);
  const generation = owner.generation;
  const checkpoint = owner.checkpoint;
  if (
    checkpoint === null ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null
  )
    return localRefuse("coverage requires a settled publication");
  const point = admitFraudProofRawL1Point(input.point);
  const block = await localHeaderBlock(
    owner,
    {
      observedBlockHash: point.blockHash,
      observedBlockNo: point.blockNo,
      observedSlot: point.slot,
    },
    input.archive,
  );
  localCutoffTransactionOrder(block);
  const publication = readWatcherProtectedUserEventCheckpointReceipt(
    await readWatcherProtectedUserEventCheckpoint(input.runtime),
  );
  const finality = input.runtime.read().currentFinalityState;
  if (
    localOwner(input.history) !== owner ||
    owner.generation !== generation ||
    owner.checkpoint !== checkpoint ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    finality.phase === "quarantined" ||
    finality.incident !== null ||
    !same(publication.checkpoint, checkpoint) ||
    publication.payload === null ||
    sha256Bytes(publication.payload) !== checkpoint.payloadDigest
  )
    return localRefuse("coverage changed during protected archive read");
};

/** Issues one retained event from the privately published whole-block fold. */
export const admitWatcherLocalUserEventAuthority = async (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      runtime: WatcherDurableRuntime;
      eventId: string;
      kind: WatcherUserEventKind;
      throughHeader?: WatcherStateQueueHeaderObservation;
      archive?: WatcherUserEventArchive;
    }>,
): Promise<WatcherLocalUserEventAuthority> => {
  const {
    history,
    runtime,
    eventId,
    kind,
    throughHeader,
    archive,
    finality,
    observation,
    referenceAuthority,
  } = input;
  const owner = localOwner(history);
  const generation = owner.generation;
  const checkpoint = owner.checkpoint;
  const head = owner.entries.at(-1);
  if (checkpoint === null || head === undefined)
    return localRefuse("event authority requires a published history");
  const matches = [
    ...owner.snapshot.activeEvents,
    ...owner.snapshot.terminalEvents,
  ].filter((event) => event.eventId === eventId && event.kind === kind);
  if (matches.length === 0)
    return localAuthorityUnavailable("event is not retained");
  if (matches.length !== 1)
    return localRefuse("event is not uniquely retained");
  const event = matches[0]!;
  const retainedEvidence = localRetainedEvidence(owner);
  const originIndex = retainedEvidence.findIndex(
    ({ pointDigest }) => pointDigest === event.originPointDigest,
  );
  const terminalIndex =
    "terminalPointDigest" in event
      ? retainedEvidence.findIndex(
          ({ pointDigest }) => pointDigest === event.terminalPointDigest,
        )
      : originIndex;
  if (
    originIndex < 0 ||
    terminalIndex < originIndex ||
    event.finalityStatus !== "final" ||
    ("terminalFinalityStatus" in event &&
      event.terminalFinalityStatus !== "final")
  )
    return localRefuse(
      "event origin or terminal membership is absent from finalized history",
    );
  const terminal =
    owner.snapshot.terminalEvents.find(
      (candidate) => candidate.eventId === eventId && candidate.kind === kind,
    ) ?? null;
  const scoped =
    throughHeader === undefined
      ? null
      : await localEventAtHeaderCutoff(
          owner,
          event,
          terminal,
          retainedEvidence[originIndex]!,
          retainedEvidence[terminalIndex]!,
          throughHeader,
          archive ?? localRefuse("header cutoff requires the history archive"),
        );
  if (
    localOwner(history) !== owner ||
    owner.generation !== generation ||
    owner.checkpoint !== checkpoint ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null
  )
    return localRefuse("history changed during header cutoff acquisition");
  const receipt = Object.freeze({ [localEventAuthorityBrand]: true as const });
  localEventAuthorities.set(
    receipt,
    Object.freeze({
      history,
      header: throughHeader ?? null,
      runtime,
      pair: Object.freeze({ finality, observation, referenceAuthority }),
      generation,
      protectedRead: { receipt: null },
      value: Object.freeze({
        deploymentManifestId: owner.origin.deploymentFingerprint,
        blueprintHash: owner.policy.blueprintHash,
        network: owner.policy.network,
        event: scoped?.event ?? event,
        throughHeader: scoped?.throughHeader ?? null,
        checkpointDigest: checkpoint.checkpointDigest,
        checkpointPayloadDigest: checkpoint.payloadDigest,
        snapshotDigest: owner.snapshot.snapshotDigest,
        headEntryDigest: head.entryDigest,
        historyEntryDigests: Object.freeze([
          ...new Set([
            retainedEvidence[originIndex]!.entry.entryDigest,
            ...(scoped === null || scoped.includeTerminal
              ? [retainedEvidence[terminalIndex]!.entry.entryDigest]
              : []),
            ...(scoped === null
              ? []
              : [scoped.throughHeader.historyEntryDigest]),
          ]),
        ]),
      }),
    }),
  );
  await readWatcherLocalUserEventAuthority(receipt);
  return receipt;
};

const localReadmissionBrand = Symbol("watcher-local-user-event-readmission");
export type WatcherLocalUserEventReadmission = Readonly<{
  [localReadmissionBrand]: true;
}>;
export type WatcherLocalUserEventReplaySource = (
  point: WatcherUserEventOriginFacts["parentPoint"],
) => Promise<LocalPair & Readonly<{ close(): Promise<void> }>>;
type LocalReadmissionOwner = {
  readonly runtime: WatcherDurableRuntime;
  readonly history: WatcherLocalUserEventHistory;
  readonly pair: LocalPair;
  readonly generation: number;
  readonly previousCheckpoint: WatcherUserEventCheckpoint;
  readonly nextCheckpoint: WatcherUserEventCheckpoint;
  readonly archiveObjects: readonly LocalArchiveObject[];
  readonly release: () => Promise<void>;
  accepted: boolean;
};
const localReadmissions = new WeakMap<
  WatcherLocalUserEventReadmission,
  LocalReadmissionOwner
>();

const localArchiveField = (
  value: unknown,
  keys: readonly string[],
): unknown => {
  let current = value;
  for (const key of keys) {
    if (
      typeof current !== "object" ||
      current === null ||
      Array.isArray(current)
    )
      return localRefuse("archive field is absent");
    const descriptor = Object.getOwnPropertyDescriptor(current, key);
    if (descriptor === undefined || !("value" in descriptor))
      return localRefuse("archive field is absent");
    current = descriptor.value;
  }
  return current;
};

const localArchivedEntry = (value: unknown): WatcherLocalUserEventEntry => {
  const record = exactRecord(value, [
    "schemaVersion",
    "sequence",
    "originDigest",
    "policyDigest",
    "predecessorEntryDigest",
    "predecessorStateDigest",
    "cursor",
    "parent",
    "sourceStoreDigest",
    "nextStoreDigest",
    "sourceStoreRevision",
    "nextStoreRevision",
    "observationDigest",
    "snapshotDigest",
    "evidenceDigest",
    "entryDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !== "midgard-watcher-local-user-event-entry-v1" ||
    !isNatural(record.sequence) ||
    record.sequence.length > 20 ||
    !isNatural(record.sourceStoreRevision) ||
    record.sourceStoreRevision.length > 20 ||
    !isNatural(record.nextStoreRevision) ||
    record.nextStoreRevision.length > 20 ||
    !isHex32(record.originDigest) ||
    !isHex32(record.policyDigest) ||
    !(
      record.predecessorEntryDigest === null ||
      isHex32(record.predecessorEntryDigest)
    ) ||
    !(
      record.predecessorStateDigest === null ||
      isHex32(record.predecessorStateDigest)
    ) ||
    !isHex32(record.sourceStoreDigest) ||
    !isHex32(record.nextStoreDigest) ||
    !isHex32(record.observationDigest) ||
    !isHex32(record.snapshotDigest) ||
    !isHex32(record.evidenceDigest) ||
    !isHex32(record.entryDigest)
  )
    return localRefuse("archive entry framing differs");
  const entry = Object.freeze({
    schemaVersion: record.schemaVersion,
    sequence: record.sequence,
    originDigest: record.originDigest,
    policyDigest: record.policyDigest,
    predecessorEntryDigest: record.predecessorEntryDigest,
    predecessorStateDigest: record.predecessorStateDigest,
    cursor: Object.freeze(admitFraudProofRawL1Point(record.cursor)),
    parent: Object.freeze(admitFraudProofRawL1Point(record.parent)),
    sourceStoreDigest: record.sourceStoreDigest,
    nextStoreDigest: record.nextStoreDigest,
    sourceStoreRevision: record.sourceStoreRevision,
    nextStoreRevision: record.nextStoreRevision,
    observationDigest: record.observationDigest,
    snapshotDigest: record.snapshotDigest,
    evidenceDigest: record.evidenceDigest,
    entryDigest: record.entryDigest,
  });
  const { entryDigest, ...fields } = entry;
  if (sha256Canonical(fields) !== entryDigest)
    return localRefuse("archive entry digest differs");
  return entry;
};

/** Compare only replay-stable semantics. The three original acquisition-derived
 * point commitments remain checked/retained as archived values, and are never
 * relabelled as commitments produced by the fresh W12 observations.
 */
const localStableSnapshot = (value: unknown): unknown => {
  const snapshot = exactRecord(value, [
    "schemaVersion",
    "activeEvents",
    "terminalEvents",
    "quarantined",
    "snapshotDigest",
  ]);
  if (
    snapshot === null ||
    snapshot.schemaVersion !== WATCHER_USER_EVENT_SNAPSHOT_SCHEMA_VERSION ||
    snapshot.quarantined !== false ||
    !Array.isArray(snapshot.activeEvents) ||
    !Array.isArray(snapshot.terminalEvents) ||
    snapshot.activeEvents.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.activeEvents ||
    snapshot.terminalEvents.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.terminalEvents ||
    !isHex32(snapshot.snapshotDigest)
  )
    return localRefuse("archive snapshot framing differs");
  const { snapshotDigest, ...fields } = snapshot;
  if (sha256Canonical(fields) !== snapshotDigest)
    return localRefuse("archive snapshot digest differs");
  const stableEvent = (value: unknown, terminal: boolean) => {
    const baseKeys = [
      "kind",
      "eventId",
      "outRef",
      "transactionHash",
      "outputIndex",
      "nonceOutRef",
      "policyId",
      "spendScriptHash",
      "addressHex",
      "assetNameHex",
      "witnessScriptHash",
      "inclusionTime",
      "eventCborHex",
      "datumCborHex",
      "outputCborHex",
      "eventContentDigest",
      "datumDigest",
      "outputDigest",
      "originPointDigest",
      "originChainPointId",
      "originBlockHash",
      "originSlot",
      "originBlockNo",
      "finalityStatus",
    ];
    const classification =
      typeof value === "object" &&
      value !== null &&
      Object.hasOwn(value, "terminalClassification");
    const event = exactRecord(value, [
      ...baseKeys,
      ...(terminal
        ? [
            "terminalStatus",
            "terminalTransactionHash",
            "terminalPointDigest",
            "terminalBlockHash",
            "terminalSlot",
            "terminalBlockNo",
            "terminalFinalityStatus",
            ...(classification ? ["terminalClassification"] : []),
          ]
        : []),
    ]);
    if (
      event === null ||
      !isHex32(event.originPointDigest) ||
      !isHex32(event.originChainPointId) ||
      event.finalityStatus !== "final" ||
      (terminal &&
        (!isHex32(event.terminalPointDigest) ||
          event.terminalFinalityStatus !== "final"))
    )
      return localRefuse("archive event framing differs");
    const {
      originPointDigest: _originPointDigest,
      originChainPointId: _originChainPointId,
      terminalPointDigest: _terminalPointDigest,
      terminalClassification,
      ...stable
    } = event;
    if (classification) {
      const decoded = exactRecord(terminalClassification, [
        "schemaVersion",
        "operatorValidity",
        "terminalTransactionHash",
        "terminalPointDigest",
      ]);
      if (
        decoded === null ||
        decoded.terminalPointDigest !== event.terminalPointDigest
      )
        return localRefuse("archive terminal classification differs");
      const {
        terminalPointDigest: _classificationPoint,
        ...stableClassification
      } = decoded;
      return { ...stable, terminalClassification: stableClassification };
    }
    return stable;
  };
  return {
    schemaVersion: snapshot.schemaVersion,
    quarantined: false,
    activeEvents: snapshot.activeEvents.map((event) =>
      stableEvent(event, false),
    ),
    terminalEvents: snapshot.terminalEvents.map((event) =>
      stableEvent(event, true),
    ),
  };
};

const localStableEventStore = (store: WatcherDurableStore): unknown => {
  const points = new Map(
    store.chainPoints.map((point) => [point.chainPointId, point]),
  );
  const stablePoint = (id: string) => {
    const point =
      points.get(id) ?? localRefuse("event store point dependency is absent");
    return {
      providerId: point.providerId,
      blockHash: point.blockHash,
      slot: point.slot,
      blockNo: point.blockNo,
    };
  };
  const {
    chainPoints,
    l1Observations,
    protocolUtxos,
    spentProtocolUtxos,
    caches: _caches,
    ...rest
  } = store;
  const order = (values: readonly unknown[]) =>
    [...values].sort((a, b) =>
      watcherCanonicalJson(a).localeCompare(watcherCanonicalJson(b)),
    );
  return {
    ...rest,
    chainPoints: order(
      chainPoints.map((point) => stablePoint(point.chainPointId)),
    ),
    l1Observations: order(
      l1Observations.map((row) => ({
        providerId: row.providerId,
        point: stablePoint(row.chainPointId),
      })),
    ),
    protocolUtxos: protocolUtxos.map(({ chainPointId, ...utxo }) => ({
      ...utxo,
      point: stablePoint(chainPointId),
    })),
    spentProtocolUtxos: spentProtocolUtxos.map(
      ({ chainPointId, spentAtChainPointId, ...utxo }) => ({
        ...utxo,
        point: stablePoint(chainPointId),
        spentAt: stablePoint(spentAtChainPointId),
      }),
    ),
  };
};

const localStableOrigin = (value: unknown): unknown => ({
  schemaVersion: localArchiveField(value, ["schemaVersion"]),
  deploymentFingerprint: localArchiveField(value, ["deploymentFingerprint"]),
  blueprintHash: localArchiveField(value, ["blueprintHash"]),
  blueprintSha256: localArchiveField(value, ["blueprintSha256"]),
  network: localArchiveField(value, ["network"]),
  canonicalOneShotOutRef: localArchiveField(value, ["canonicalOneShotOutRef"]),
  scripts: localArchiveField(value, ["scripts"]),
  parentPoint: localArchiveField(value, ["parentPoint"]),
  activation: localArchiveField(value, ["activation"]),
});

/** A protected archive is descriptive input. Only the actual fresh W12 pairs
 * drive this private fold; archived observations never become W12 receipts.
 * Indexed sealed segments are replayed chronologically with bounded live state.
 */
export const prepareWatcherLocalUserEventReadmission = async (
  input: Omit<
    Parameters<typeof createWatcherLocalUserEventHistory>[0],
    "publication"
  > &
    Readonly<{
      referenceAuthority: WatcherUserEventReferenceAuthority;
      runtime: WatcherDurableRuntime;
      archive: WatcherUserEventArchive;
      replayBlock: WatcherLocalUserEventReplaySource;
    }>,
): Promise<WatcherLocalUserEventReadmission> => {
  const {
    origin,
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
    referenceAuthority,
    runtime,
    archive,
    replayBlock,
  } = input;
  const firstPair = Object.freeze({
    finality,
    observation,
    referenceAuthority,
  });
  const publication = await readWatcherProtectedUserEventCheckpoint(runtime);
  const protectedHead =
    readWatcherProtectedUserEventCheckpointReceipt(publication);
  const previousCheckpoint = protectedHead.checkpoint;
  const runtimeFinality = runtime.read().currentFinalityState;
  if (
    runtimeFinality.phase === "quarantined" ||
    runtimeFinality.incident !== null
  )
    return localRefuse("semantic readmission runtime is quarantined");
  if (previousCheckpoint === null || protectedHead.payload === null)
    return localRefuse("semantic readmission requires a published checkpoint");
  const history = createLocalUserEventHistory({
    origin,
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
    publication,
    semanticReplay: true,
  });
  const owner = localOwner(history);
  const replayState: {
    retainedSource: Awaited<
      ReturnType<WatcherLocalUserEventReplaySource>
    > | null;
    priorIndex: WatcherUserEventArchiveIndexRead | null;
  } = { retainedSource: null, priorIndex: null };
  try {
    if (
      previousCheckpoint.userEventPolicyDigest !== owner.policy.policyDigest ||
      previousCheckpoint.finalityPolicyDigest !==
        owner.finalityPolicy.policyDigest ||
      previousCheckpoint.blueprintHash !== owner.policy.blueprintHash ||
      previousCheckpoint.network !== owner.policy.network
    )
      return localRefuse("archived policy differs from the fresh deployment");
    const bootstrapStore = owner.store;
    const readClosure = async (requiredDigests: readonly string[]) => {
      const objects = new Map<
        string,
        Readonly<{ object: LocalArchiveObject; value: unknown }>
      >();
      const budget: EvidenceGraphBudget = { nodes: 0, bytes: 0 };
      let bytesRead = 0;
      for (const digest of requiredDigests) {
        const bytes = await archive.read(digest);
        if (bytes === null || sha256Bytes(bytes) !== digest)
          return localRefuse("archived closure is absent or corrupt");
        bytesRead += bytes.byteLength;
        if (
          bytesRead > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes
        )
          return localRefuse("archived closure byte bound exceeded");
        let value: unknown;
        try {
          value = JSON.parse(
            new TextDecoder("utf-8", { fatal: true }).decode(bytes),
          );
        } catch {
          return localRefuse("archived closure is not canonical JSON");
        }
        if (!evidenceWithinBounds(value, budget))
          return localRefuse("archived closure evidence bound exceeded");
        const object = localArchiveObject(value);
        if (object.digest !== digest)
          return localRefuse("archived closure JSON encoding differs");
        objects.set(digest, Object.freeze({ object, value }));
      }
      return objects;
    };
    const readValue = async (digest: string): Promise<unknown> => {
      const objects = await readClosure([digest]);
      return objects.get(digest)!.value;
    };
    const parsePayload = (payloadValue: unknown) => {
      const hasReadmission =
        typeof payloadValue === "object" &&
        payloadValue !== null &&
        Object.hasOwn(payloadValue, "readmission");
      const payload = exactRecord(payloadValue, [
        "schemaVersion",
        "originArchiveDigest",
        "originDigest",
        "policy",
        "anchor",
        "head",
        "storeArchiveDigest",
        "snapshot",
        "retainedEntries",
        "requiredSemanticResume",
        ...(hasReadmission ? ["readmission"] : []),
      ]);
      if (
        payload === null ||
        payload.schemaVersion !==
          "midgard-watcher-local-user-event-checkpoint-payload-v1" ||
        payload.requiredSemanticResume !==
          "authenticated_origin_replay_or_semantic_publication_receipt" ||
        !isHex32(payload.originArchiveDigest) ||
        !isHex32(payload.originDigest) ||
        !isHex32(payload.storeArchiveDigest) ||
        !same(payload.policy, owner.policy) ||
        !Array.isArray(payload.retainedEntries) ||
        payload.retainedEntries.length === 0 ||
        payload.retainedEntries.length >
          Number(owner.policy.maximumActiveHistoryEntries)
      )
        return localRefuse("archived semantic payload framing differs");
      return {
        payload: {
          schemaVersion: payload.schemaVersion,
          originArchiveDigest: payload.originArchiveDigest,
          originDigest: payload.originDigest,
          policy: payload.policy,
          anchor: payload.anchor,
          head: payload.head,
          storeArchiveDigest: payload.storeArchiveDigest,
          snapshot: payload.snapshot,
          retainedEntries: payload.retainedEntries,
          requiredSemanticResume: payload.requiredSemanticResume,
          readmission: payload.readmission,
        },
        hasReadmission,
      };
    };
    const currentObjects = await readClosure(
      previousCheckpoint.requiredArchiveDigests,
    );
    const currentPayloadValue =
      currentObjects.get(previousCheckpoint.payloadDigest)?.value ??
      localRefuse("current payload is absent");
    const { payload } = parsePayload(currentPayloadValue);
    const anchorDescriptor = exactRecord(payload.anchor, [
      "kind",
      "indexDigest",
      "indexSequence",
      "retainedSuffixEntries",
    ]);
    let rootIndex: WatcherUserEventArchiveIndexRead | null = null;
    if (anchorDescriptor !== null) {
      if (
        anchorDescriptor.kind !== "materialized_history" ||
        !isHex32(anchorDescriptor.indexDigest) ||
        anchorDescriptor.retainedSuffixEntries !== "64"
      )
        return localRefuse("archive anchor descriptor differs");
      rootIndex = await readWatcherUserEventArchiveIndex(
        archive,
        anchorDescriptor.indexDigest,
      );
      if (rootIndex.index.indexSequence !== anchorDescriptor.indexSequence)
        return localRefuse("archive root sequence differs");
    }
    let lastProcessedSequence = -1n;
    let lastProcessedEntry: WatcherLocalUserEventEntry | null = null;
    let oldRetainedEntries: readonly WatcherLocalUserEventEntry[] = [];
    let archivedSourceStore = bootstrapStore;
    const writePrivateObjects = async (
      objects: readonly LocalArchiveObject[],
    ) => {
      for (const object of objects) {
        const digest = await archive.put(Buffer.from(object.bytesHex, "hex"));
        if (digest !== object.digest)
          return localRefuse("semantic replay archive write differs");
      }
    };
    const replaySegment = async (
      objects: Awaited<ReturnType<typeof readClosure>>,
      payloadDigest: string,
      sealedIndex: WatcherUserEventArchiveIndexRead | null,
    ) => {
      const archived = (digest: string): unknown =>
        objects.get(digest)?.value ??
        localRefuse("archive dependency is absent");
      const { payload, hasReadmission } = parsePayload(archived(payloadDigest));
      const expectedAnchor =
        replayState.priorIndex === null
          ? {
              kind: "activation_origin",
              parent: owner.origin.parentPoint,
              bootstrapStoreDigest: owner.policy.bootstrapStoreDigest,
            }
          : {
              kind: "materialized_history",
              indexDigest: replayState.priorIndex.digest,
              indexSequence: replayState.priorIndex.index.indexSequence,
              retainedSuffixEntries: "64",
            };
      const oldOrigin = exactRecord(archived(payload.originArchiveDigest), [
        "schemaVersion",
        "numericEncoding",
        "facts",
        "policy",
        "bootstrapStore",
      ]);
      if (
        payload.originDigest !==
          localArchiveField(currentPayloadValue, ["originDigest"]) ||
        oldOrigin === null ||
        oldOrigin.schemaVersion !==
          "midgard-watcher-local-user-event-origin-archive-v1" ||
        oldOrigin.numericEncoding !== "exact-decimal-strings" ||
        !same(oldOrigin.policy, owner.policy) ||
        !same(
          localStableOrigin(oldOrigin.facts),
          localArchiveEvidence(localStableOrigin(owner.origin)),
        ) ||
        localArchiveField(oldOrigin.facts, ["originDigest"]) !==
          payload.originDigest ||
        !same(oldOrigin.bootstrapStore, bootstrapStore) ||
        !same(payload.anchor, expectedAnchor)
      )
        return localRefuse(
          "archived activation origin differs from the fresh authenticated activation",
        );
      if (hasReadmission) {
        const priorReadmission = exactRecord(payload.readmission, [
          "kind",
          "previousCheckpointDigest",
          "previousPayloadDigest",
          "archivedOriginDigest",
          "freshOriginDigest",
          "stableSnapshotDigest",
        ]);
        if (
          priorReadmission === null ||
          priorReadmission.kind !== "fresh_authenticated_origin_replay" ||
          !isHex32(priorReadmission.previousCheckpointDigest) ||
          !isHex32(priorReadmission.previousPayloadDigest) ||
          !isHex32(priorReadmission.archivedOriginDigest) ||
          priorReadmission.freshOriginDigest !== payload.originDigest ||
          priorReadmission.stableSnapshotDigest !==
            sha256Canonical(localStableSnapshot(payload.snapshot)) ||
          localArchiveField(
            await readValue(priorReadmission.previousPayloadDigest),
            ["originDigest"],
          ) !== priorReadmission.archivedOriginDigest
        )
          return localRefuse("archived semantic readmission linkage differs");
      }
      const entries = payload.retainedEntries.map(localArchivedEntry);
      if (
        entries.some(
          (entry, index) =>
            BigInt(entry.sequence) !==
            BigInt(entries[0]!.sequence) + BigInt(index),
        ) ||
        entries[0]!.sequence !== (oldRetainedEntries[0]?.sequence ?? "0")
      )
        return localRefuse(
          "archived retained entries are not the exact consecutive suffix",
        );
      if (
        !same(payload.head, entries.at(-1)) ||
        (replayState.priorIndex === null &&
          payload.storeArchiveDigest !== entries.at(-1)!.nextStoreDigest)
      )
        return localRefuse("archived head differs");
      const entryObservations = new Map<string, WatcherUserEventObservation>();
      for (const { value } of objects.values()) {
        const entryArchive = exactRecord(value, ["entry", "observation"]);
        if (entryArchive === null) continue;
        const entry = localArchivedEntry(entryArchive.entry);
        localStableSnapshot(
          localArchiveField(entryArchive.observation, ["snapshot"]),
        );
        const observed = parseObservationStructural(entryArchive.observation);
        if (
          observed === null ||
          observed.observationDigest !== entry.observationDigest ||
          entryObservations.has(entry.entryDigest)
        )
          return localRefuse("archived entry observation differs");
        entryObservations.set(entry.entryDigest, observed);
      }
      const headStore = parseWatcherDurableStore(
        archived(payload.storeArchiveDigest),
      );
      if (storeDigest(headStore) !== payload.storeArchiveDigest)
        return localRefuse("archived head store digest differs");
      for (const entry of entries) {
        if (BigInt(entry.sequence) <= lastProcessedSequence) {
          if (!oldRetainedEntries.some((retained) => same(retained, entry)))
            return localRefuse(
              "archived retained suffix differs from its materialization",
            );
          continue;
        }
        const previous: WatcherLocalUserEventEntry | null = lastProcessedEntry;

        const oldObservation = entryObservations.get(entry.entryDigest);
        if (
          BigInt(entry.sequence) !== lastProcessedSequence + 1n ||
          entry.originDigest !== payload.originDigest ||
          entry.policyDigest !== owner.policy.policyDigest ||
          entry.predecessorEntryDigest !== (previous?.entryDigest ?? null) ||
          entry.sourceStoreDigest !== storeDigest(archivedSourceStore) ||
          BigInt(entry.nextStoreRevision) !==
            BigInt(entry.sourceStoreRevision) + 1n ||
          entry.sourceStoreRevision !== archivedSourceStore.revision ||
          oldObservation === undefined ||
          oldObservation.transitionKind !== "apply_block" ||
          oldObservation.policyDigest !== owner.policy.policyDigest ||
          oldObservation.network !== owner.policy.network ||
          oldObservation.blueprintHash !== owner.policy.blueprintHash ||
          !same(
            oldObservation.deploymentMarker,
            owner.policy.deploymentMarker,
          ) ||
          oldObservation.snapshot.snapshotDigest !== entry.snapshotDigest ||
          oldObservation.sourceDurableStoreDigest !== entry.sourceStoreDigest ||
          oldObservation.durableStoreDigest !== entry.nextStoreDigest ||
          oldObservation.sourceDurableStoreRevision !==
            entry.sourceStoreRevision ||
          oldObservation.durableStoreRevision !== entry.nextStoreRevision ||
          oldObservation.blockHash !== entry.cursor.blockHash ||
          oldObservation.blockNo !== entry.cursor.blockNo ||
          oldObservation.slot !== entry.cursor.slot ||
          (lastProcessedSequence === -1n
            ? entry.predecessorStateDigest !== null
            : entry.predecessorStateDigest === null)
        )
          return localRefuse("archived semantic entry chain differs");
        if (
          previous !== null &&
          (entry.predecessorStateDigest === null ||
            !same(
              localArchiveField(archived(entry.predecessorStateDigest), [
                "head",
              ]),
              previous,
            ))
        )
          return localRefuse("archived predecessor state differs");
        if (replayState.retainedSource !== null) {
          await replayState.retainedSource.close();
          replayState.retainedSource = null;
        }
        const source =
          lastProcessedSequence === -1n
            ? null
            : await replayBlock(entry.cursor);
        if (source !== null) replayState.retainedSource = source;
        const pair =
          source === null
            ? firstPair
            : Object.freeze({
                finality: source.finality,
                observation: source.observation,
                referenceAuthority: source.referenceAuthority,
              });
        const live = localLivePair(owner, pair);
        const oldEvidence = exactRecord(archived(entry.evidenceDigest), [
          "schemaVersion",
          "numericEncoding",
          "witnesses",
          "referenceEvidence",
        ]);
        if (
          oldEvidence === null ||
          oldEvidence.schemaVersion !==
            "midgard-watcher-local-user-event-block-evidence-v1" ||
          oldEvidence.numericEncoding !== "exact-decimal-strings" ||
          !same(live.witness.current.observation.capture.point, entry.cursor) ||
          !same(
            live.witness.current.observation.capture.predecessorPoint,
            entry.parent,
          )
        )
          return localRefuse(
            "fresh replay does not match the archived whole block",
          );
        for (const step of ["first", "current"] as const) {
          if (
            localArchiveField(oldEvidence.witnesses, [
              step,
              "observation",
              "capture",
              "nativeBlock",
              "rawBlockCbor",
            ]) !==
              live.witness.current.observation.capture.nativeBlock
                .rawBlockCbor ||
            !same(
              localArchiveField(oldEvidence.witnesses, [
                step,
                "observation",
                "capture",
                "point",
              ]),
              entry.cursor,
            ) ||
            !same(
              localArchiveField(oldEvidence.witnesses, [
                step,
                "observation",
                "capture",
                "predecessorPoint",
              ]),
              entry.parent,
            )
          )
            return localRefuse(
              "archived original block bytes differ from fresh canonical replay",
            );
        }
        const transition = prepareLocalUserEventTransition(history, pair);
        const fresh = readWatcherLocalUserEventTransition(transition);
        if (
          !same(
            localStableSnapshot(oldObservation.snapshot),
            localStableSnapshot(fresh.snapshot),
          )
        )
          return localRefuse(
            "fresh semantic replay differs from the archived event fold",
          );
        const oldStore = parseWatcherDurableStore(
          archived(entry.nextStoreDigest),
        );
        if (
          storeDigest(oldStore) !== entry.nextStoreDigest ||
          !topologyMatches(oldStore, oldObservation.snapshot)
        )
          return localRefuse("archived event store topology differs");
        const oldNative = localArchiveField(oldEvidence.witnesses, [
          "current",
          "observation",
          "native",
        ]);
        const oldPoint = oldStore.chainPoints.find(
          (point) => point.chainPointId === oldObservation.chainPointId,
        );
        const oldRow = oldStore.l1Observations.find(
          (row) => row.observationId === oldObservation.sourceObservationDigest,
        );
        if (
          oldPoint === undefined ||
          oldRow === undefined ||
          oldRow.chainPointId !== oldPoint.chainPointId ||
          oldPoint.blockHash !== entry.cursor.blockHash ||
          oldPoint.blockNo !== entry.cursor.blockNo ||
          oldPoint.slot !== entry.cursor.slot ||
          oldPoint.chainPointId !==
            localArchiveField(oldNative, ["chainPoint", "chainPointId"]) ||
          oldPoint.depth !==
            localArchiveField(oldNative, ["chainPoint", "depth"]) ||
          oldPoint.providerId !==
            localArchiveField(oldNative, ["provider", "providerId"]) ||
          oldObservation.pointDigest !==
            localArchiveField(oldNative, ["chainPoint", "pointDigest"]) ||
          oldRow.observationId !==
            localArchiveField(oldNative, ["observationDigest"]) ||
          !same(
            localArchiveEvidence(
              JSON.parse(
                Buffer.from(oldRow.payload.cborHex, "hex").toString("utf8"),
              ),
            ),
            oldNative,
          )
        )
          return localRefuse(
            "archived original observation/store binding differs",
          );
        const oldPoints = [...archivedSourceStore.chainPoints, oldPoint];
        const oldJournal = journalWatcherProtocolUtxoTransition({
          sourceStore: archivedSourceStore,
          nextChainPoints: oldPoints,
          spentAtChainPointId: oldPoint.chainPointId,
          nextProtocolUtxos: oldObservation.snapshot.activeEvents.map(
            (event) => ({
              outRef: event.outRef,
              role: protocolRole(event.kind),
              chainPointId: event.originChainPointId,
              output: makeWatcherDurablePayload(event.outputCborHex),
            }),
          ),
        });
        const rebuiltOldStore = makeWatcherDurableStore({
          deploymentMarker: owner.policy.deploymentMarker,
          revision: entry.nextStoreRevision,
          records: {
            ...archivedSourceStore,
            chainPoints: oldPoints,
            ...oldJournal,
            l1Observations: [...archivedSourceStore.l1Observations, oldRow],
          },
        });
        if (
          !same(oldStore, rebuiltOldStore) ||
          !same(
            localStableEventStore(oldStore),
            localStableEventStore(fresh.nextStore),
          )
        )
          return localRefuse(
            "archived event journal differs from fresh semantic replay",
          );
        archivedSourceStore = oldStore;
        lastProcessedSequence = BigInt(entry.sequence);
        lastProcessedEntry = entry;
        oldRetainedEntries = [...oldRetainedEntries, entry];
        commitLocalUserEventTransition(history, transition);
      }
      if (
        !same(
          payload.snapshot,
          entryObservations.get(entries.at(-1)!.entryDigest)!.snapshot,
        ) ||
        !same(
          localStableSnapshot(payload.snapshot),
          localStableSnapshot(owner.snapshot),
        )
      )
        return localRefuse(
          "fresh semantic head differs from archived snapshot",
        );
      if (!same(archivedSourceStore, headStore))
        return localRefuse(
          "archived payload materialization differs from the replayed head",
        );
      if (sealedIndex !== null) {
        if (
          sealedIndex.index.lastEntrySequence !==
            lastProcessedEntry!.sequence ||
          !same(
            sealedIndex.index.retainedEntryDigests,
            oldRetainedEntries.slice(-64).map((entry) => entry.entryDigest),
          )
        )
          return localRefuse(
            "archive segment does not seal the exact retained suffix",
          );
        const requiredPoints = new Set(
          oldRetainedEntries.slice(-64).map((entry) => {
            const observation = entryObservations.get(entry.entryDigest);
            if (observation === undefined || observation.chainPointId === null)
              return localRefuse("materialized suffix observation is absent");
            return observation.chainPointId;
          }),
        );
        const retainPoint = (
          blockHash: string,
          slot: string,
          blockNo: string,
        ) => {
          const points = archivedSourceStore.chainPoints.filter(
            (point) =>
              point.blockHash === blockHash &&
              point.slot === slot &&
              point.blockNo === blockNo,
          );
          if (points.length !== 1)
            return localRefuse(
              "materialized event point is not uniquely retained",
            );
          requiredPoints.add(points[0]!.chainPointId);
        };
        for (const event of owner.snapshot.activeEvents)
          retainPoint(
            event.originBlockHash,
            event.originSlot,
            event.originBlockNo,
          );
        for (const event of owner.snapshot.terminalEvents) {
          retainPoint(
            event.originBlockHash,
            event.originSlot,
            event.originBlockNo,
          );
          retainPoint(
            event.terminalBlockHash,
            event.terminalSlot,
            event.terminalBlockNo,
          );
        }
        const materialized = localMaterializedStoreFromPoints(
          archivedSourceStore,
          requiredPoints,
        );
        const archivedMaterialized = parseWatcherDurableStore(
          await readValue(sealedIndex.index.materializedStoreDigest),
        );
        if (!same(materialized, archivedMaterialized))
          return localRefuse(
            "archived materialization is not the exact dependency-preserving projection",
          );
        const pair =
          replayState.retainedSource === null
            ? firstPair
            : replayState.retainedSource;
        await writePrivateObjects(owner.archiveObjects);
        const anchor = await prepareLocalUserEventAnchor(
          history,
          pair,
          archive,
        );
        const freshMaterialized = localAnchors.get(anchor)!.value.nextStore;
        if (
          !same(
            localStableEventStore(materialized),
            localStableEventStore(freshMaterialized),
          )
        )
          return localRefuse(
            "fresh materialization differs from archived semantics",
          );
        await writePrivateObjects(
          readWatcherLocalUserEventAnchor(anchor).archiveObjects,
        );
        commitLocalUserEventAnchor(anchor);
        archivedSourceStore = materialized;
        oldRetainedEntries = oldRetainedEntries.slice(-64);
        replayState.priorIndex = sealedIndex;
      }
    };
    if (rootIndex !== null) {
      for (
        let sequence = 0n;
        sequence <= BigInt(rootIndex.index.indexSequence);
        sequence += 1n
      ) {
        const segment = await findWatcherUserEventArchiveIndex(
          archive,
          rootIndex,
          sequence.toString(),
        );
        if (
          segment.index.previousIndexDigest !==
            replayState.priorIndex?.digest &&
          !(
            replayState.priorIndex === null &&
            segment.index.previousIndexDigest === null
          )
        )
          return localRefuse("archive segment immediate predecessor differs");
        if (
          BigInt(segment.index.firstEntrySequence) !==
          lastProcessedSequence + 1n
        )
          return localRefuse("archive segment entry boundary differs");
        await replaySegment(
          await readClosure(segment.index.sourceArchiveDigests),
          segment.index.sourcePayloadDigest,
          segment,
        );
      }
      if (replayState.priorIndex?.digest !== rootIndex.digest)
        return localRefuse("archive root is not the replayed segment head");
    }
    await replaySegment(currentObjects, previousCheckpoint.payloadDigest, null);
    const replayHead = owner.entries.at(-1)!;
    const replayPayload = owner.archiveObjects.find(
      (object) => object.digest === owner.checkpoint!.payloadDigest,
    )!;
    const replayPayloadFields = exactRecord(
      JSON.parse(Buffer.from(replayPayload.bytesHex, "hex").toString("utf8")),
      [
        "schemaVersion",
        "originArchiveDigest",
        "originDigest",
        "policy",
        "anchor",
        "head",
        "storeArchiveDigest",
        "snapshot",
        "retainedEntries",
        "requiredSemanticResume",
      ],
    );
    if (replayPayloadFields === null)
      return localRefuse("private replay payload differs");
    const readmissionPayload = localArchiveObject({
      ...replayPayloadFields,
      readmission: {
        kind: "fresh_authenticated_origin_replay",
        previousCheckpointDigest: previousCheckpoint.checkpointDigest,
        previousPayloadDigest: previousCheckpoint.payloadDigest,
        archivedOriginDigest: payload.originDigest,
        freshOriginDigest: owner.originDigest,
        stableSnapshotDigest: sha256Canonical(
          localStableSnapshot(owner.snapshot),
        ),
      },
    });
    const archiveObjects = Object.freeze([
      ...new Map([
        ...[...currentObjects.values()].map(
          ({ object }) => [object.digest, object] as const,
        ),
        ...owner.archiveObjects.map(
          (object) => [object.digest, object] as const,
        ),
        [readmissionPayload.digest, readmissionPayload] as const,
      ]).values(),
    ]);
    if (
      archiveObjects.length >
        WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries ||
      archiveObjects.reduce(
        (total, object) => total + object.bytesHex.length / 2,
        0,
      ) > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes ||
      archiveObjects.reduce(
        (total, object) => total + localArchiveBudgets.get(object)!.nodes,
        0,
      ) > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes
    )
      return localRefuse("semantic readmission archive bound exceeded");
    const nextCheckpoint = makeWatcherUserEventCheckpoint({
      ...previousCheckpoint,
      checkpointSequence: (
        BigInt(previousCheckpoint.checkpointSequence) + 1n
      ).toString(),
      predecessorCheckpointDigest: previousCheckpoint.checkpointDigest,
      payloadDigest: readmissionPayload.digest,
      requiredArchiveDigests: archiveObjects.map(({ digest }) => digest).sort(),
    });
    const refreshed = readWatcherProtectedUserEventCheckpointReceipt(
      await readWatcherProtectedUserEventCheckpoint(runtime),
    );
    if (!same(refreshed.checkpoint, previousCheckpoint))
      return localRefuse("protected head changed during semantic replay");
    const pair =
      replayState.retainedSource === null
        ? firstPair
        : Object.freeze({
            finality: replayState.retainedSource.finality,
            observation: replayState.retainedSource.observation,
            referenceAuthority: replayState.retainedSource.referenceAuthority,
          });
    if (
      !same(
        localLivePair(owner, pair).witness.current.observation.capture.point,
        replayHead.cursor,
      )
    )
      return localRefuse("semantic replay head is no longer live");
    const retained = replayState.retainedSource;
    const readmission = Object.freeze({
      [localReadmissionBrand]: true as const,
    });
    localReadmissions.set(readmission, {
      runtime,
      history,
      pair,
      generation: owner.generation,
      previousCheckpoint,
      nextCheckpoint,
      archiveObjects,
      release: async () => {
        await retained?.close();
      },
      accepted: false,
    });
    replayState.retainedSource = null;
    return readmission;
  } catch (error) {
    closeWatcherLocalUserEventHistory(history);
    await replayState.retainedSource?.close();
    throw error;
  }
};

export const readWatcherLocalUserEventReadmission = (
  receipt: WatcherLocalUserEventReadmission,
) => {
  const readmission =
    localReadmissions.get(receipt) ??
    localRefuse("readmission is not privately admitted");
  const owner = localOwner(readmission.history);
  if (
    readmission.accepted ||
    !owner.semanticReplay ||
    owner.generation !== readmission.generation ||
    owner.candidate !== null
  )
    return localRefuse("readmission is no longer pending");
  const live = localLivePair(owner, readmission.pair);
  const finality = readmission.runtime.read().currentFinalityState;
  if (
    finality.phase === "quarantined" ||
    finality.incident !== null ||
    !same(
      live.witness.current.observation.capture.point,
      owner.entries.at(-1)!.cursor,
    ) ||
    live.witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      owner.acceptedEvidence.at(-1)!.rawBlockCbor
  )
    return localRefuse("semantic readmission head is no longer current");
  return Object.freeze({
    archiveObjects: readmission.archiveObjects,
    nextCheckpoint: readmission.nextCheckpoint,
    expectedCheckpointDigest: readmission.previousCheckpoint.checkpointDigest,
    expectedCheckpointSequence:
      readmission.previousCheckpoint.checkpointSequence,
  });
};

export const acceptWatcherLocalUserEventReadmission = (
  receipt: WatcherLocalUserEventReadmission,
  publication: WatcherProtectedUserEventCheckpoint,
): WatcherLocalUserEventHistory => {
  const prepared = readWatcherLocalUserEventReadmission(receipt);
  const observed = readWatcherProtectedUserEventCheckpointReceipt(publication);
  if (
    !same(observed.checkpoint, prepared.nextCheckpoint) ||
    observed.payload === null ||
    sha256Bytes(observed.payload) !== prepared.nextCheckpoint.payloadDigest
  )
    return localRefuse("semantic readmission publication differs");
  const readmission = localReadmissions.get(receipt)!;
  const owner = localOwner(readmission.history);
  owner.checkpoint = readmission.nextCheckpoint;
  owner.archiveObjects = readmission.archiveObjects;
  owner.lastAccepted = null;
  owner.semanticReplay = false;
  owner.generation += 1;
  owner.acceptedAtMonotonicMs = performance.now();
  readmission.accepted = true;
  return readmission.history;
};

export const closeWatcherLocalUserEventReadmission = async (
  receipt: WatcherLocalUserEventReadmission,
): Promise<void> => {
  const readmission =
    localReadmissions.get(receipt) ??
    localRefuse("readmission is not privately admitted");
  if (!readmission.accepted)
    closeWatcherLocalUserEventHistory(readmission.history);
  await readmission.release();
};

const localAnchorBrand = Symbol("watcher-local-user-event-anchor");
export type WatcherLocalUserEventAnchor = Readonly<{
  [localAnchorBrand]: true;
}>;
type LocalAnchorValue = Readonly<{
  sourceStore: WatcherDurableStore;
  nextStore: WatcherDurableStore;
  archiveIndex: WatcherUserEventArchiveIndexRead;
  archiveObjects: readonly LocalArchiveObject[];
  retainedEntries: readonly WatcherLocalUserEventEntry[];
  retainedEvidence: readonly LocalRetainedEvidence[];
  pinnedEvidence: readonly LocalRetainedEvidence[];
  nextCheckpoint: WatcherUserEventCheckpoint;
  expectedCheckpointDigest: string;
  expectedCheckpointSequence: string;
}>;
type LocalAnchorOwner = {
  readonly history: WatcherLocalUserEventHistory;
  readonly pair: LocalPair;
  readonly generation: number;
  readonly value: LocalAnchorValue;
  accepted: boolean;
};
const localAnchors = new WeakMap<
  WatcherLocalUserEventAnchor,
  LocalAnchorOwner
>();

const localMaterializedStore = (
  source: WatcherDurableStore,
  retainedEvidence: readonly LocalRetainedEvidence[],
): WatcherDurableStore => {
  return localMaterializedStoreFromPoints(
    source,
    new Set(retainedEvidence.map(({ chainPointId }) => chainPointId)),
  );
};

const localMaterializedStoreFromPoints = (
  source: WatcherDurableStore,
  requiredPoints: Set<string>,
): WatcherDurableStore => {
  for (const utxo of source.protocolUtxos)
    requiredPoints.add(utxo.chainPointId);
  for (const utxo of source.spentProtocolUtxos) {
    requiredPoints.add(utxo.chainPointId);
    requiredPoints.add(utxo.spentAtChainPointId);
  }
  const chainPoints = source.chainPoints.filter(({ chainPointId }) =>
    requiredPoints.has(chainPointId),
  );
  if (chainPoints.length !== requiredPoints.size)
    return localRefuse("materialization point dependency is absent");
  return immutableWireValue(
    makeWatcherDurableStore({
      deploymentMarker: source.deploymentMarker,
      revision: (BigInt(source.revision) + 1n).toString(),
      records: {
        ...source,
        chainPoints,
        l1Observations: source.l1Observations.filter(({ chainPointId }) =>
          requiredPoints.has(chainPointId),
        ),
      },
    }),
  );
};

const localArchiveClosure = (
  objects: readonly LocalArchiveObject[],
): readonly LocalArchiveObject[] => {
  const unique = Object.freeze([
    ...new Map(objects.map((object) => [object.digest, object])).values(),
  ]);
  if (
    unique.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries ||
    unique.reduce((total, object) => total + object.bytesHex.length / 2, 0) >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes ||
    unique.reduce(
      (total, object) => total + localArchiveBudgets.get(object)!.nodes,
      0,
    ) > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes
  )
    return localRefuse("materialized archive bound exceeded");
  return unique;
};

const prepareLocalUserEventAnchor = async (
  history: WatcherLocalUserEventHistory,
  pair: LocalPair,
  archive: WatcherUserEventArchive,
  publication?: WatcherProtectedUserEventCheckpoint,
): Promise<WatcherLocalUserEventAnchor> => {
  const owner = localOwner(history);
  const generation = owner.generation;
  const sourceStore = owner.store;
  const sourceCheckpoint = owner.checkpoint;
  const sourceObjects = owner.archiveObjects;
  const priorIndex = owner.archiveIndex;
  const snapshot = owner.snapshot;
  if (
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    sourceCheckpoint === null ||
    owner.entries.length <= 64
  )
    return localRefuse(
      "anchor requires a published head with more than 64 retained blocks and no pending operation",
    );
  const current = localLivePair(owner, pair);
  const head = owner.entries.at(-1)!;
  if (
    !same(current.witness.current.observation.capture.point, head.cursor) ||
    current.witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      owner.acceptedEvidence.at(-1)!.rawBlockCbor
  )
    return localRefuse("anchor capture differs from the exact semantic head");
  const retainedEntries = Object.freeze(owner.entries.slice(-64));
  const retainedEvidence = Object.freeze(owner.acceptedEvidence.slice(-64));
  const retainedIds = new Set(
    retainedEntries.map(({ entryDigest }) => entryDigest),
  );
  const eventPoints = new Set(
    [...snapshot.activeEvents, ...snapshot.terminalEvents].flatMap((event) =>
      "terminalPointDigest" in event
        ? [event.originPointDigest, event.terminalPointDigest]
        : [event.originPointDigest],
    ),
  );
  const allEvidence = localRetainedEvidence(owner);
  if (
    [...eventPoints].some(
      (point) => !allEvidence.some(({ pointDigest }) => pointDigest === point),
    )
  )
    return localRefuse("anchor event provenance is absent");
  const pinnedEvidence = Object.freeze(
    allEvidence.filter(
      ({ entry, pointDigest }) =>
        !retainedIds.has(entry.entryDigest) && eventPoints.has(pointDigest),
    ),
  );
  const nextStore = localMaterializedStore(sourceStore, [
    ...pinnedEvidence,
    ...retainedEvidence,
  ]);
  if (!topologyMatches(nextStore, snapshot))
    return localRefuse("anchor changes event topology");
  const storeArchive = localArchiveObject(nextStore);
  const index = await makeWatcherUserEventArchiveIndex(archive, {
    previous: priorIndex,
    firstEntrySequence:
      priorIndex === null
        ? "0"
        : (BigInt(priorIndex.index.lastEntrySequence) + 1n).toString(),
    lastEntrySequence: head.sequence,
    sourcePayloadDigest: sourceCheckpoint.payloadDigest,
    sourceArchiveDigests: sourceCheckpoint.requiredArchiveDigests,
    materializedStoreDigest: storeArchive.digest,
    retainedEntryDigests: retainedEntries.map(({ entryDigest }) => entryDigest),
  });
  const indexArchive = localArchiveObject(index);
  const archiveIndex = Object.freeze({ digest: indexArchive.digest, index });
  const payload = localArchiveObject({
    schemaVersion: "midgard-watcher-local-user-event-checkpoint-payload-v1",
    originArchiveDigest: owner.originArchive.digest,
    originDigest: owner.originDigest,
    policy: owner.policy,
    anchor: {
      kind: "materialized_history",
      indexDigest: archiveIndex.digest,
      indexSequence: index.indexSequence,
      retainedSuffixEntries: "64",
    },
    head,
    storeArchiveDigest: storeArchive.digest,
    snapshot,
    retainedEntries,
    requiredSemanticResume:
      "authenticated_origin_replay_or_semantic_publication_receipt",
  });
  const requiredEvidence = new Set(
    [...pinnedEvidence, ...retainedEvidence].flatMap((record) => [
      record.entry.evidenceDigest,
      record.entryArchiveDigest,
    ]),
  );
  const evidenceObjects = sourceObjects.filter(({ digest }) =>
    requiredEvidence.has(digest),
  );
  if (evidenceObjects.length !== requiredEvidence.size)
    return localRefuse("anchor retained archive dependency is absent");
  const archiveObjects = localArchiveClosure([
    owner.originArchive,
    ...evidenceObjects,
    storeArchive,
    indexArchive,
    payload,
  ]);
  const nextCheckpoint = makeWatcherUserEventCheckpoint({
    ...sourceCheckpoint,
    checkpointSequence: (
      BigInt(sourceCheckpoint.checkpointSequence) + 1n
    ).toString(),
    predecessorCheckpointDigest: sourceCheckpoint.checkpointDigest,
    payloadDigest: payload.digest,
    requiredArchiveDigests: archiveObjects.map(({ digest }) => digest).sort(),
  });
  if (
    owner.generation !== generation ||
    owner.store !== sourceStore ||
    owner.checkpoint !== sourceCheckpoint ||
    owner.archiveObjects !== sourceObjects ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null
  )
    return localRefuse("history changed during anchor preparation");
  if (publication !== undefined)
    readWatcherProtectedUserEventCheckpointReceipt(publication);
  localLivePair(owner, pair);
  const receipt = Object.freeze({ [localAnchorBrand]: true as const });
  const value = Object.freeze({
    sourceStore,
    nextStore,
    archiveIndex,
    archiveObjects,
    retainedEntries,
    retainedEvidence,
    pinnedEvidence,
    nextCheckpoint,
    expectedCheckpointDigest: sourceCheckpoint.checkpointDigest,
    expectedCheckpointSequence: sourceCheckpoint.checkpointSequence,
  });
  localAnchors.set(receipt, {
    history,
    pair,
    generation,
    value,
    accepted: false,
  });
  owner.anchorCandidate = receipt;
  return receipt;
};

export const prepareWatcherLocalUserEventAnchor = async (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      archive: WatcherUserEventArchive;
      publication: WatcherProtectedUserEventCheckpoint;
    }>,
): Promise<WatcherLocalUserEventAnchor> => {
  const {
    history,
    archive,
    publication,
    finality,
    observation,
    referenceAuthority,
  } = input;
  const owner = localOwner(history);
  if (
    owner.semanticReplay ||
    !same(
      readWatcherProtectedUserEventCheckpointReceipt(publication).checkpoint,
      owner.checkpoint,
    )
  )
    return localRefuse("anchor protected predecessor differs");
  return await prepareLocalUserEventAnchor(
    history,
    Object.freeze({ finality, observation, referenceAuthority }),
    archive,
    publication,
  );
};

export const readWatcherLocalUserEventAnchor = (
  receipt: WatcherLocalUserEventAnchor,
) => {
  const anchor =
    localAnchors.get(receipt) ??
    localRefuse("anchor is not privately admitted");
  const owner = localOwner(anchor.history);
  if (
    anchor.accepted ||
    owner.anchorCandidate !== receipt ||
    owner.generation !== anchor.generation ||
    owner.store !== anchor.value.sourceStore ||
    owner.candidate !== null
  )
    return localRefuse("anchor is no longer pending");
  const current = localLivePair(owner, anchor.pair);
  if (
    !same(
      current.witness.current.observation.capture.point,
      owner.entries.at(-1)!.cursor,
    ) ||
    current.witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      owner.acceptedEvidence.at(-1)!.rawBlockCbor
  )
    return localRefuse("anchor head is no longer current");
  return Object.freeze({
    archiveObjects: anchor.value.archiveObjects,
    nextCheckpoint: anchor.value.nextCheckpoint,
    expectedCheckpointDigest: anchor.value.expectedCheckpointDigest,
    expectedCheckpointSequence: anchor.value.expectedCheckpointSequence,
  });
};

const commitLocalUserEventAnchor = (
  receipt: WatcherLocalUserEventAnchor,
): WatcherLocalUserEventHistory => {
  readWatcherLocalUserEventAnchor(receipt);
  const anchor = localAnchors.get(receipt)!;
  const owner = localOwner(anchor.history);
  owner.store = anchor.value.nextStore;
  owner.entries = anchor.value.retainedEntries;
  owner.acceptedEvidence = anchor.value.retainedEvidence;
  owner.pinnedEvidence = anchor.value.pinnedEvidence;
  owner.archiveIndex = anchor.value.archiveIndex;
  owner.archiveObjects = anchor.value.archiveObjects;
  owner.checkpoint = anchor.value.nextCheckpoint;
  owner.anchorCandidate = null;
  owner.lastAccepted = null;
  owner.generation += 1;
  owner.acceptedAtMonotonicMs = performance.now();
  anchor.accepted = true;
  return anchor.history;
};

export const acceptWatcherLocalUserEventAnchor = (
  receipt: WatcherLocalUserEventAnchor,
  publication: WatcherProtectedUserEventCheckpoint,
): WatcherLocalUserEventHistory => {
  const prepared = readWatcherLocalUserEventAnchor(receipt);
  const anchor = localAnchors.get(receipt)!;
  if (localOwner(anchor.history).semanticReplay)
    return localRefuse("provisional anchor requires semantic readmission");
  const observed = readWatcherProtectedUserEventCheckpointReceipt(publication);
  if (
    !same(observed.checkpoint, prepared.nextCheckpoint) ||
    observed.payload === null ||
    sha256Bytes(observed.payload) !== prepared.nextCheckpoint.payloadDigest
  )
    return localRefuse(
      "anchor publication differs from the exact materialization",
    );
  return commitLocalUserEventAnchor(receipt);
};

/** The durable owner records semantic completion only for a live candidate
 * admitted by this module. Descriptive JSON and copied handles cannot mint it. */
export const readWatcherLocalUserEventValidation = (
  candidate: unknown,
  checkpoint: WatcherUserEventCheckpoint,
): WatcherUserEventValidation => {
  if (typeof candidate !== "object" || candidate === null)
    return localRefuse("semantic validation candidate is absent");
  const next = localTransitions.has(
    candidate as WatcherLocalUserEventTransition,
  )
    ? readWatcherLocalUserEventTransition(
        candidate as WatcherLocalUserEventTransition,
      ).nextCheckpoint
    : localAnchors.has(candidate as WatcherLocalUserEventAnchor)
      ? readWatcherLocalUserEventAnchor(
          candidate as WatcherLocalUserEventAnchor,
        ).nextCheckpoint
      : localReadmissions.has(candidate as WatcherLocalUserEventReadmission)
        ? readWatcherLocalUserEventReadmission(
            candidate as WatcherLocalUserEventReadmission,
          ).nextCheckpoint
        : localRefuse("semantic validation candidate was not admitted");
  if (!same(next, checkpoint))
    return localRefuse("semantic validation candidate checkpoint differs");
  return Object.freeze({
    schemaVersion: WATCHER_USER_EVENT_VALIDATION_SCHEMA_VERSION,
    checkpointDigest: next.checkpointDigest,
    payloadDigest: next.payloadDigest,
    policyDigest: next.userEventPolicyDigest,
  });
};

/** Restore a completed fold. Original archive facts remain historical data;
 * only the newly acquired head pair supplies current source/finality authority. */
export const restoreWatcherLocalUserEventHistory = async (
  input: Omit<
    Parameters<typeof createWatcherLocalUserEventHistory>[0],
    "publication"
  > &
    Readonly<{
      runtime: WatcherDurableRuntime;
      archive: WatcherUserEventArchive;
      readHead: WatcherLocalUserEventReplaySource;
      referenceAuthority: WatcherUserEventReferenceAuthority;
    }>,
): Promise<WatcherLocalUserEventHistory> => {
  const publication = await readWatcherProtectedUserEventCheckpoint(
    input.runtime,
  );
  const published = readWatcherProtectedUserEventCheckpointReceipt(publication);
  const checkpoint = published.checkpoint;
  if (
    checkpoint === null ||
    published.payload === null ||
    published.validation?.schemaVersion !==
      WATCHER_USER_EVENT_VALIDATION_SCHEMA_VERSION ||
    published.validation.checkpointDigest !== checkpoint.checkpointDigest ||
    published.validation.payloadDigest !== checkpoint.payloadDigest ||
    published.validation.policyDigest !== checkpoint.userEventPolicyDigest
  )
    return localRefuse(
      "restart requires durable semantic validation; explicit recovery is required",
    );
  const runtimeFinality = input.runtime.readFinality();
  if (
    runtimeFinality.phase === "quarantined" ||
    runtimeFinality.incident !== null
  )
    return localRefuse("restart runtime is quarantined");
  const history = createLocalUserEventHistory({
    ...input,
    publication,
    semanticReplay: true,
  });
  const provisional = localOwner(history);
  let headPair:
    | Awaited<ReturnType<WatcherLocalUserEventReplaySource>>
    | undefined;
  try {
    const payload = objectForLocalRestart(published.payload);
    if (
      payload.schemaVersion !==
        "midgard-watcher-local-user-event-checkpoint-payload-v1" ||
      !same(payload.policy, provisional.policy) ||
      checkpoint.userEventPolicyDigest !== provisional.policy.policyDigest ||
      !isHex32(payload.originArchiveDigest) ||
      !isHex32(payload.originDigest) ||
      !isHex32(payload.storeArchiveDigest) ||
      !Array.isArray(payload.retainedEntries) ||
      payload.retainedEntries.length === 0 ||
      payload.retainedEntries.length >
        Number(provisional.policy.maximumActiveHistoryEntries)
    )
      return localRefuse("saved semantic state dependencies differ");
    const objects = new Map<
      string,
      Readonly<{ object: LocalArchiveObject; value: unknown }>
    >();
    let retainedBytes = 0;
    let retainedNodes = 0;
    // Only the bounded current closure is loaded. Sealed historical segments
    // are left in the archive; no block is replayed or semantically revalidated.
    for (const key of checkpoint.requiredArchiveDigests) {
      const bytes = await input.archive.read(key);
      if (bytes === null || sha256Bytes(bytes) !== key)
        return localRefuse("saved semantic state archive is absent or corrupt");
      retainedBytes += bytes.length;
      if (
        retainedBytes >
        WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes
      )
        return localRefuse("saved semantic state archive exceeds its bound");
      const value: unknown = JSON.parse(
        new TextDecoder("utf-8", { fatal: true }).decode(bytes),
      );
      const archived = localArchiveObject(value);
      retainedNodes += localArchiveBudgets.get(archived)!.nodes;
      if (
        archived.digest !== key ||
        retainedNodes >
          WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes
      )
        return localRefuse("saved semantic state archive framing differs");
      objects.set(key, { object: archived, value });
    }
    const originArchive = objects.get(payload.originArchiveDigest);
    const savedOrigin = originArchive?.value;
    if (
      originArchive === undefined ||
      localArchiveField(savedOrigin, ["schemaVersion"]) !==
        "midgard-watcher-local-user-event-origin-archive-v1" ||
      !same(
        localStableOrigin(localArchiveField(savedOrigin, ["facts"])),
        localArchiveEvidence(localStableOrigin(provisional.origin)),
      ) ||
      localArchiveField(savedOrigin, ["facts", "originDigest"]) !==
        payload.originDigest
    )
      return localRefuse("saved origin differs from the admitted deployment");
    const stored = objects.get(payload.storeArchiveDigest)?.value;
    if (stored === undefined)
      return localRefuse("saved materialized event state is absent");
    const entries = Object.freeze(
      payload.retainedEntries.map(localArchivedEntry),
    );
    const head = entries.at(-1)!;
    if (
      !same(payload.head, head) ||
      entries.some(
        (entry, index) =>
          entry.originDigest !== payload.originDigest ||
          entry.policyDigest !== provisional.policy.policyDigest ||
          (index > 0 &&
            entry.predecessorEntryDigest !== entries[index - 1]!.entryDigest),
      )
    )
      return localRefuse("saved semantic progress marker differs");
    const snapshot = immutableWireValue(
      payload.snapshot,
    ) as WatcherUserEventSnapshot;
    const retainedIds = new Set(entries.map((entry) => entry.entryDigest));
    const pinnedPoints = new Set(
      [...snapshot.activeEvents, ...snapshot.terminalEvents].flatMap((event) =>
        "terminalPointDigest" in event
          ? [event.originPointDigest, event.terminalPointDigest]
          : [event.originPointDigest],
      ),
    );
    const evidence: LocalRetainedEvidence[] = [];
    for (const [entryArchiveDigest, archived] of objects) {
      const candidate = exactRecord(archived.value, ["entry", "observation"]);
      if (candidate === null) continue;
      const entry = localArchivedEntry(candidate.entry);
      if (
        entry.originDigest !== payload.originDigest ||
        entry.policyDigest !== provisional.policy.policyDigest
      )
        continue;
      const original = objects.get(entry.evidenceDigest)?.value;
      if (original === undefined) continue;
      const rawBlockCbor = localArchiveField(original, [
        "witnesses",
        "current",
        "observation",
        "capture",
        "nativeBlock",
        "rawBlockCbor",
      ]);
      const pointDigest = localArchiveField(original, [
        "witnesses",
        "current",
        "observation",
        "native",
        "chainPoint",
        "pointDigest",
      ]);
      const chainPointId = localArchiveField(original, [
        "witnesses",
        "current",
        "observation",
        "native",
        "chainPoint",
        "chainPointId",
      ]);
      if (
        !isHex32(pointDigest) ||
        !isHex32(chainPointId) ||
        !isHexBytes(rawBlockCbor)
      )
        return localRefuse("saved historical block facts are malformed");
      if (retainedIds.has(entry.entryDigest) || pinnedPoints.has(pointDigest))
        evidence.push(
          Object.freeze({
            entry,
            entryArchiveDigest,
            rawBlockCbor,
            pointDigest,
            chainPointId,
          }),
        );
    }
    const acceptedEvidence = Object.freeze(
      entries.map((entry) => {
        const matches = evidence.filter(
          (record) => record.entry.entryDigest === entry.entryDigest,
        );
        if (matches.length !== 1)
          return localRefuse("saved progress evidence is absent or ambiguous");
        return matches[0]!;
      }),
    );
    const pinnedEvidence = Object.freeze(
      evidence.filter(({ entry }) => !retainedIds.has(entry.entryDigest)),
    );
    if (
      [...pinnedPoints].some(
        (point) => !evidence.some((record) => record.pointDigest === point),
      )
    )
      return localRefuse("saved event provenance is absent");
    const anchor = objectForLocalRestart(
      Buffer.from(watcherCanonicalJson(payload.anchor), "utf8"),
    );
    const archiveIndex =
      anchor.kind === "activation_origin"
        ? null
        : anchor.kind === "materialized_history" && isHex32(anchor.indexDigest)
          ? await readWatcherUserEventArchiveIndex(
              input.archive,
              anchor.indexDigest,
            )
          : localRefuse("saved history anchor is invalid");
    if (
      archiveIndex !== null &&
      archiveIndex.index.indexSequence !== anchor.indexSequence
    )
      return localRefuse("saved history anchor sequence differs");
    headPair = await input.readHead(head.cursor);
    const live = localLivePair(provisional, headPair);
    if (
      !same(live.witness.current.observation.capture.point, head.cursor) ||
      live.witness.current.observation.capture.nativeBlock.rawBlockCbor !==
        acceptedEvidence.at(-1)!.rawBlockCbor
    )
      return localRefuse(
        "saved head is no longer canonical; explicit recovery is required",
      );
    const fresh = readWatcherProtectedUserEventCheckpointReceipt(
      await readWatcherProtectedUserEventCheckpoint(input.runtime),
    );
    localLivePair(provisional, headPair);
    if (
      !same(fresh.checkpoint, checkpoint) ||
      !same(fresh.validation, published.validation)
    )
      return localRefuse("saved semantic progress changed during restart");
    localHistories.set(history, {
      ...provisional,
      originDigest: payload.originDigest,
      originArchive: originArchive.object,
      store: immutableWireValue(stored) as WatcherDurableStore,
      snapshot,
      entries,
      acceptedEvidence,
      pinnedEvidence,
      archiveIndex,
      archiveObjects: Object.freeze(
        [...objects.values()].map(({ object }) => object),
      ),
      checkpoint,
      semanticReplay: false,
      acceptedAtMonotonicMs: performance.now(),
    });
    return history;
  } catch (cause) {
    closeWatcherLocalUserEventHistory(history);
    throw cause;
  } finally {
    await headPair?.close();
  }
};

const objectForLocalRestart = (bytes: Uint8Array): PlainRecord => {
  const value: unknown = JSON.parse(
    new TextDecoder("utf-8", { fatal: true }).decode(bytes),
  );
  if (typeof value !== "object" || value === null || Array.isArray(value))
    return localRefuse("saved semantic state is not an object");
  return value as PlainRecord;
};
