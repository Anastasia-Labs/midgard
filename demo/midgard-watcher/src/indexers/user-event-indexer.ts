import { createHash } from "node:crypto";

import {
  computeMidgardForcedTxProofCommitment,
  decodeMidgardNativeTxProofFieldLengths,
  verifyMidgardForcedTxProofSource,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_EMPTY_FIELD_COMMITMENT } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { midgardTxFieldCommitmentsFromSource } from "@al-ft/midgard-core/consensus-validation";
import {
  type DeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import {
  admitFraudProofRawL1Point,
  computeFraudProofRawL1PointId,
} from "@al-ft/midgard-fault-proofs";
import {
  ConfirmedState,
  DepositDatumSchema,
  DepositEventSchema,
  EVENT_WAIT_DURATION_MS,
  EventHistoryOperation,
  eventHistoryRetirementOperation,
  ForcedInclusionTxV1Schema,
  HubOracleDatumSchema,
  LinkedListDatum,
  MerkleRoot,
  outputReferenceToPlutusDataCbor,
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
} from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  SLOT_CONFIG_NETWORK,
  slotToBeginUnixTime,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  readWatcherLocalBackfillFinalityObservation,
  readWatcherLocalBackfillFinalityOriginalWitness,
  type WatcherFinalityPolicy,
  type WatcherLocalBackfillFinalityReceipt,
} from "../l1/finality-engine.js";
import {
  encodeWatcherNormalizedL1Block,
  type WatcherLocalBackfillObservationReceipt,
  type WatcherNormalizedL1Block,
} from "../l1/l1-adapter.js";
import {
  parseWatcherCustomNetwork,
  type WatcherCustomNetwork,
} from "../runtime/custom-network.js";
import {
  readWatcherUserEventScriptBinding,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentAppliedScriptHashes,
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
  historyCardanoDatumMatches,
  historyListObservation,
  historyNodeFromOutput,
  historyOrderContinuationMatches,
  historyPayloadFromNode,
  historyRawField,
  historyRetirementObservation,
  historyWithdrawalPayoutDatum,
} from "./authenticated-event-history.js";
import {
  assertWatcherStateQueueHeaderObservation,
  type WatcherStateQueueHeaderObservation,
} from "./authenticated-state-queue-observation.js";
import {
  findWatcherUserEventArchiveIndex,
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
export type WatcherUserEventNetwork =
  | "Mainnet"
  | "Preprod"
  | "Preview"
  | "Custom";
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
  customNetwork?: WatcherCustomNetwork;
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
  witnessScriptHash?: string;
  /** Original payload retained from actual authenticated L1 admission. */
  historyPayloadCborHex?: string;
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
const NETWORKS = ["Mainnet", "Preprod", "Preview", "Custom"] as const;

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
  ...(value.customNetwork === undefined
    ? {}
    : { customNetwork: value.customNetwork }),
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
  const custom =
    typeof value === "object" &&
    value !== null &&
    Object.getOwnPropertyDescriptor(value, "network")?.value === "Custom";
  const record = exactRecord(value, [
    "schemaVersion",
    "network",
    ...(custom ? ["customNetwork"] : []),
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
  let customNetwork: WatcherCustomNetwork | undefined;
  if (custom) {
    try {
      customNetwork = parseWatcherCustomNetwork(record.customNetwork);
    } catch {
      return null;
    }
  }
  const canonical = policyWithoutDigest({
    schemaVersion: WATCHER_USER_EVENT_INDEXER_POLICY_SCHEMA_VERSION,
    network: record.network,
    ...(customNetwork === undefined ? {} : { customNetwork }),
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
 * `forcedPayloadMatchesSubmittedSource` has already bound those structures to the
 * carried `tx_id` and commitment by the time this is consulted.
 *
 * Returns `null` when the payload is not a decodable §4 binding, so a caller
 * cannot read a count off a payload nothing authenticated.
 */
const forcedOrderMaterialFieldCount = (payload: unknown): number | null => {
  const candidate = payload as {
    submitted_source?: {
      compact_cbor?: unknown;
      witness_set_compact_cbor?: unknown;
      field_preimage_lengths_cbor?: unknown;
    };
  };
  if (
    !isHexBytes(candidate.submitted_source?.compact_cbor) ||
    !isHexBytes(candidate.submitted_source.witness_set_compact_cbor) ||
    !isHexBytes(candidate.submitted_source.field_preimage_lengths_cbor)
  ) {
    return null;
  }
  try {
    return midgardTxFieldCommitmentsFromSource(
      {
        compactCbor: Buffer.from(
          candidate.submitted_source.compact_cbor,
          "hex",
        ),
        witnessSetCompactCbor: Buffer.from(
          candidate.submitted_source.witness_set_compact_cbor,
          "hex",
        ),
        fieldPreimageLengthsCbor: Buffer.from(
          candidate.submitted_source.field_preimage_lengths_cbor,
          "hex",
        ),
      },
      "forced",
    ).filter((commitment) => !commitment.equals(MIDGARD_EMPTY_FIELD_COMMITMENT))
      .length;
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

const scanHistoryOutput = (
  policy: WatcherUserEventIndexerPolicy,
  block: WatcherNormalizedL1Block,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  references: WatcherUserEventReferenceEvidence,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
  kind: "deposit" | "withdrawal",
  outputIndex: number,
): { event?: WatcherIndexedUserEvent } | null => {
  const fields = eventPolicy(policy, kind);
  const body = canonicalBody(transaction.body.bytesHex)!;
  const output = body.outputs().get(outputIndex);
  const authenticated = historyNodeFromOutput(output, fields.policyId);
  const observed = historyListObservation(transaction, fields.policyId);
  if (
    authenticated === null ||
    observed === null ||
    output.address().to_hex() !== fields.addressHex
  )
    return null;
  const { node, key } = authenticated;
  const observe = observed.observe;
  if ("Initialize" in observe)
    return node.payload === "RootContent" &&
      observe.Initialize.root_output_index === BigInt(outputIndex)
      ? {}
      : null;
  const hub = decodeHubAt(
    references,
    transaction.txHash,
    body,
    observe.Apply.hub_reference_index,
    deployment,
  );
  if (
    hub === null ||
    hub[kind] !== fields.policyId ||
    !addressMatchesData(output.address(), hub[kind + "_addr"])
  )
    return null;
  const operation = observe.Apply.operation;
  if (node.payload === "RootContent" || !("Order" in node.payload)) return {};
  const admission =
    "InsertOrder" in operation
      ? operation.InsertOrder
      : "PromoteFiller" in operation
        ? operation.PromoteFiller
        : null;
  if (
    admission === null ||
    admission.order_output_index !== BigInt(outputIndex)
  ) {
    return "predecessor_output_index" in Object.values(operation)[0]! &&
      (Object.values(operation)[0] as { predecessor_output_index: bigint })
        .predecessor_output_index === BigInt(outputIndex)
      ? {}
      : null;
  }
  const nonceIndex = admission.nonce_input_index;
  if (nonceIndex < 0n || nonceIndex >= BigInt(body.inputs().len())) return null;
  const nonce = body.inputs().get(Number(nonceIndex));
  const facts = node.payload.Order.facts;
  const external =
    admission.external_reference_index === null
      ? null
      : watcherUserEventReferenceOutput(
          references,
          transaction.txHash,
          referencedOutRefAt(body, admission.external_reference_index),
        );
  const opened = historyPayloadFromNode(
    authenticated,
    kind,
    key,
    external,
    deployment.appliedScriptHashes[kind + "HistoryRetentionSpend"],
  );
  const ttl = body.ttl();
  const mint = body
    .mint()
    ?.get_assets(CML.ScriptHash.from_hex(fields.policyId));
  const expectedMint = "PromoteFiller" in operation ? 0n : 1n;
  if (expectedMint === 1n) {
    const index =
      body.mint() === undefined
        ? -1
        : mintPolicyIndex(body.mint()!, fields.policyId);
    if (index < 0 || matchingRedeemer(transaction, "mint", index) === null)
      return null;
  }
  if (
    opened === null ||
    key !== nonceAssetName(nonce) ||
    ttl === undefined ||
    ttl > BigInt(Number.MAX_SAFE_INTEGER) ||
    facts.inclusion_time !==
      BigInt(
        resolveEventInclusionTime(
          slotToBeginUnixTime(
            Number(ttl),
            policy.customNetwork?.slotConfig ??
              SLOT_CONFIG_NETWORK[policy.network],
          ),
          policy.network,
        ),
      ) ||
    (expectedMint === 0n
      ? mint !== undefined && mint.len() !== 0
      : mint?.len() !== 1 || mint.get(CML.AssetName.from_hex(key)) !== 1n)
  )
    return null;
  const { payload, payloadCbor, eventCbor: eventCborHex } = opened;
  const event =
    "DepositPayload" in payload
      ? payload.DepositPayload.event
      : payload.WithdrawalPayload.event;
  if (
    !eventIdMatchesNonce(kind, event, nonce) ||
    output.amount().coin() < facts.structural_lovelace
  )
    return null;
  const datum = canonicalDatumForOutput(transaction, outputIndex, output);
  if (datum === null) return null;
  const outputCborHex = output.to_cbor_hex();
  return {
    event: Object.freeze({
      kind,
      eventId: outputReferenceToPlutusDataCbor({
        txHash: nonce.transaction_id().to_hex(),
        outputIndex: Number(nonce.index()),
      }),
      outRef: `${transaction.txHash}#${outputIndex}`,
      transactionHash: transaction.txHash,
      outputIndex: String(outputIndex),
      nonceOutRef: outputReference(nonce),
      policyId: fields.policyId,
      spendScriptHash: fields.spendScriptHash,
      addressHex: fields.addressHex,
      assetNameHex: key,
      inclusionTime: String(facts.inclusion_time),
      eventCborHex,
      historyPayloadCborHex: payloadCbor,
      datumCborHex: datum.cborHex,
      outputCborHex,
      eventContentDigest: sha256Bytes(Buffer.from(eventCborHex, "hex")),
      datumDigest: datum.digest,
      outputDigest: sha256Bytes(Buffer.from(outputCborHex, "hex")),
      originPointDigest: block.chainPoint.pointDigest,
      originChainPointId: block.chainPoint.chainPointId,
      originBlockHash: block.chainPoint.blockHash,
      originSlot: block.chainPoint.slot,
      originBlockNo: block.chainPoint.blockNo,
      finalityStatus: "pending",
    }),
  };
};

const scanCreatedTransactionEvents = (
  policy: WatcherUserEventIndexerPolicy,
  block: WatcherNormalizedL1Block,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  referenceEvidence: WatcherUserEventReferenceEvidence,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
  events: WatcherIndexedUserEvent[],
): true | null => {
  if (!transaction.isValid) {
    return true;
  }
  const body = canonicalBody(transaction.body.bytesHex);
  if (body === null) {
    return null;
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
    if (knownPolicies.length !== 1) {
      return null;
    }
    const [policyId, kind] = knownPolicies[0]!;
    const fields = eventPolicy(policy, kind);
    if (kind !== "forced_order") {
      const admitted = scanHistoryOutput(
        policy,
        block,
        transaction,
        referenceEvidence,
        deployment,
        kind,
        outputIndex,
      );
      if (admitted === null) return null;
      if (admitted.event !== undefined) events.push(admitted.event);
      continue;
    }
    if (mint === undefined) return null;
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
      return null;
    }
    const auth = decoded.event.AuthenticateEvent;
    if (
      auth.event_output_index !== BigInt(outputIndex) ||
      auth.nonce_input_index < 0n ||
      auth.nonce_input_index >= BigInt(inputs.len()) ||
      auth.hub_ref_input_index < 0n ||
      auth.witness_registration_redeemer_index < 0n
    ) {
      return null;
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
    const expectedHubPolicy = hubDatum?.tx_order;
    const expectedHubAddress = hubDatum?.tx_order_addr;
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
      return null;
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
              policy.customNetwork?.slotConfig ??
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
          !forcedPayloadMatchesSubmittedSource(forcedEvent.tx) ||
          // #594's exhaustion rule, re-derived. The redeemer's carriage vector
          // is positional over the payload's non-empty slots, so its length
          // must equal their count exactly — a short vector leaves a field's
          // material uncarried, a spare entry lets two distinct redeemers spell
          // one order (§8.11). Both inputs are in hand here: the vector came
          // out of the mint redeemer above and the count out of the payload
          // whose binding the previous clause just verified. The per-field
          // *hash* half is not reachable from this module — see
          // `forcedPayloadMatchesSubmittedSource` — but this half is, so it is
          // checked rather than deferred with it.
          decoded.materialCarriage === null ||
          decoded.materialCarriage.length !==
            forcedOrderMaterialFieldCount(forcedEvent.tx)))
    ) {
      return null;
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
    if (policies.length !== 1 || nonNftAssetCount !== 1) {
      return null;
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
  return null;
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
const forcedPayloadMatchesSubmittedSource = (payload: unknown): boolean => {
  const candidate = payload as {
    tx_id?: unknown;
    transaction_commitment?: unknown;
    submitted_source?: {
      compact_cbor?: unknown;
      witness_set_compact_cbor?: unknown;
      field_preimage_lengths_cbor?: unknown;
    };
  };
  if (
    !isHex32(candidate.tx_id) ||
    !isHex32(candidate.transaction_commitment) ||
    !isHexBytes(candidate.submitted_source?.compact_cbor) ||
    !isHexBytes(candidate.submitted_source.witness_set_compact_cbor) ||
    !isHexBytes(candidate.submitted_source.field_preimage_lengths_cbor)
  ) {
    return false;
  }
  try {
    const source = {
      compactCbor: Buffer.from(candidate.submitted_source.compact_cbor, "hex"),
      witnessSetCompactCbor: Buffer.from(
        candidate.submitted_source.witness_set_compact_cbor,
        "hex",
      ),
      fieldPreimageLengthsCbor: Buffer.from(
        candidate.submitted_source.field_preimage_lengths_cbor,
        "hex",
      ),
    };
    verifyMidgardForcedTxProofSource({
      transactionId: Buffer.from(candidate.tx_id, "hex"),
      source,
    });
    if (
      computeMidgardForcedTxProofCommitment(source).toString("hex") !==
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
            submitted_source?: unknown;
          };
          try {
            return Data.to(
              {
                tx_id: tx.tx_id,
                submitted_source: tx.submitted_source,
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
  return (
    event.kind === "forced_order" &&
    isHex32(datum.event.id?.transactionId) &&
    typeof datum.event.id.outputIndex === "bigint" &&
    forcedPayloadMatchesSubmittedSource(datum.event.tx) &&
    addressMatchesData(produced.address(), datum.refund_address) &&
    cardanoDatumMatches(produced, datum.refund_datum)
  );
};

/** Classify a consumed history order using the deployed observer. A consumed
 * predecessor continues the same event; it is never a settlement by itself. */
const consumeHistoryOrder = (
  event: WatcherIndexedUserEvent,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  references: WatcherUserEventReferenceEvidence,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
  inputIndex: number,
):
  | { continuation: WatcherIndexedUserEvent }
  | { terminalStatus: WatcherUserEventTerminalStatus }
  | null => {
  if (event.kind === "forced_order") return null;
  const body = canonicalBody(transaction.body.bytesHex)!;
  const observed = historyListObservation(transaction, event.policyId);
  const input = CML.TransactionOutput.from_cbor_hex(event.outputCborHex);
  const node = historyNodeFromOutput(input, event.policyId);
  if (
    observed === null ||
    !("Apply" in observed.observe) ||
    node === null ||
    node.node.payload === "RootContent" ||
    !("Order" in node.node.payload)
  ) {
    return null;
  }
  const spend = matchingRedeemer(transaction, "spend", inputIndex);
  if (
    spend === null ||
    dataRoundTrip<bigint>(
      spend.bytes.bytesHex,
      Data.Integer() as EventSchema,
    ) !== BigInt(inputIndex)
  )
    return null;
  const operation = observed.observe.Apply.operation;
  const operationFields = Object.values(operation)[0]!;
  const continuingIndex =
    "predecessor_output_index" in operationFields
      ? operationFields.predecessor_output_index
      : null;
  if (
    continuingIndex !== null &&
    continuingIndex >= 0n &&
    continuingIndex < BigInt(body.outputs().len())
  ) {
    const output = body.outputs().get(Number(continuingIndex));
    if (historyOrderContinuationMatches(input, output, event.policyId)) {
      const datum = canonicalDatumForOutput(
        transaction,
        Number(continuingIndex),
        output,
      );
      if (datum === null) return null;
      const outputCborHex = output.to_cbor_hex();
      return {
        continuation: Object.freeze({
          ...event,
          outRef: `${transaction.txHash}#${continuingIndex}`,
          transactionHash: transaction.txHash,
          outputIndex: String(continuingIndex),
          datumCborHex: datum.cborHex,
          datumDigest: datum.digest,
          outputCborHex,
          outputDigest: sha256Bytes(Buffer.from(outputCborHex, "hex")),
        }),
      };
    }
  }
  if (!("RetireOrder" in operation)) return null;
  const retirementHash =
    deployment.appliedScriptHashes[event.kind + "HistoryRetirementWithdraw"];
  if (!isHex28(retirementHash)) return null;
  const retired = historyRetirementObservation(transaction, retirementHash);
  if (retired === null) return null;
  const { witness, hub_reference_index } = retired.args;
  if (
    witness.order_input_index !== BigInt(inputIndex) ||
    hub_reference_index !== observed.observe.Apply.hub_reference_index ||
    Data.to(eventHistoryRetirementOperation(witness), EventHistoryOperation) !==
      Data.to(operation, EventHistoryOperation)
  )
    return null;
  const hub = decodeHubAt(
    references,
    transaction.txHash,
    body,
    hub_reference_index,
    deployment,
  );
  if (
    hub === null ||
    hub[event.kind] !== event.policyId ||
    !addressMatchesData(input.address(), hub[event.kind + "_addr"])
  )
    return null;
  const facts = node.node.payload.Order.facts;
  const external =
    witness.external_reference_index === null
      ? null
      : watcherUserEventReferenceOutput(
          references,
          transaction.txHash,
          referencedOutRefAt(body, witness.external_reference_index),
        );
  const opened = historyPayloadFromNode(
    node,
    event.kind,
    node.key,
    external,
    deployment.appliedScriptHashes[event.kind + "HistoryRetentionSpend"],
  );
  if (opened === null || event.historyPayloadCborHex !== opened.payloadCbor)
    return null;
  const { payload, payloadCbor } = opened;
  const confirmedOutput = watcherUserEventReferenceOutput(
    references,
    transaction.txHash,
    referencedOutRefAt(body, witness.confirmed_reference_index),
  );
  try {
    if (
      confirmedOutput === null ||
      !isHex28(hub.state_queue) ||
      !addressMatchesData(confirmedOutput.address(), hub.state_queue_addr) ||
      confirmedOutput.script_ref() !== undefined ||
      exactlyOneAsset(confirmedOutput, hub.state_queue)?.assetNameHex !==
        Buffer.from("MIDGARD_CONFIRMED_STATE").toString("hex") ||
      exactlyOneAsset(confirmedOutput, hub.state_queue)?.quantity !== 1n
    )
      return null;
    const confirmedNode = Data.from(
      inlineDatumCbor(confirmedOutput)!,
      LinkedListDatum,
    );
    if (!("Root" in confirmedNode.data)) return null;
    const confirmed = Data.castFrom(
      confirmedNode.data.Root.data,
      ConfirmedState,
    );
    if (facts.inclusion_time <= 0n || facts.inclusion_time > confirmed.endTime)
      return null;
  } catch {
    return null;
  }
  const settlement = isHex28(hub.settlement)
    ? authenticReferenceDatum(
        references,
        transaction.txHash,
        body,
        witness.settlement_reference_index,
        hub.settlement,
        asDataType<EventSchema>(SettlementDatumSchema),
      )
    : null;
  const root =
    settlement?.datum[
      event.kind === "deposit" ? "deposits_root" : "withdrawals_root"
    ];
  if (
    settlement === null ||
    !addressMatchesData(settlement.output.address(), hub.settlement_addr) ||
    settlement.output.script_ref() !== undefined ||
    !countedRootMatches(
      {
        ...witness.membership,
        domain:
          event.kind === "deposit"
            ? "DepositsRootDomain"
            : "WithdrawalsRootDomain",
        root: root as string,
        key: "",
        value: "",
      },
      event.kind === "deposit" ? "DepositsRootDomain" : "WithdrawalsRootDomain",
      root,
    )
  )
    return null;
  const mint = body.mint()?.get_assets(CML.ScriptHash.from_hex(event.policyId));
  if (
    mint?.len() !== 1 ||
    mint.get(CML.AssetName.from_hex(event.assetNameHex)) !== -1n ||
    witness.funds_output_index < 0n ||
    witness.funds_output_index >= BigInt(body.outputs().len()) ||
    witness.funds_output_index === witness.predecessor_output_index
  )
    return null;
  const output = body.outputs().get(Number(witness.funds_output_index));
  const original = new Map(outputValue(input));
  original.delete(event.policyId + event.assetNameHex);
  original.set(
    "lovelace",
    (original.get("lovelace") ?? 0n) - facts.structural_lovelace,
  );
  if (
    (original.get("lovelace") ?? -1n) < 0n ||
    output.script_ref() !== undefined
  )
    return null;
  if (facts.structural_lovelace === 0n) {
    if (witness.structural_refund_output_index !== null) return null;
  } else {
    const index = witness.structural_refund_output_index;
    if (
      index === null ||
      index < 0n ||
      index >= BigInt(body.outputs().len()) ||
      index === witness.funds_output_index ||
      index === witness.predecessor_output_index
    )
      return null;
    const refund = body.outputs().get(Number(index));
    if (
      CML.EnterpriseAddress.from_address(refund.address()) === undefined ||
      refund.address().payment_cred()?.as_pub_key()?.to_hex() !==
        facts.structural_refund_key ||
      refund.datum() !== undefined ||
      refund.script_ref() !== undefined ||
      refund.amount().has_multiassets() ||
      refund.amount().coin() < facts.structural_lovelace
    )
      return null;
  }
  if (witness.purpose === "AbsorbDeposit")
    return event.kind === "deposit" &&
      "DepositPayload" in payload &&
      addressMatchesData(output.address(), hub.reserve_addr) &&
      output.datum() === undefined &&
      sameValue(original, outputValue(output))
      ? { terminalStatus: "absorbed" }
      : null;
  if (event.kind !== "withdrawal" || !("WithdrawalPayload" in payload))
    return null;
  const withdrawal = payload.WithdrawalPayload;
  if (typeof witness.purpose === "object")
    return witness.purpose.RefundInvalidWithdrawal.validity !==
      "WithdrawalIsValid" &&
      addressMatchesData(output.address(), withdrawal.refund_address) &&
      historyCardanoDatumMatches(output, historyRawField(payloadCbor, [2])) &&
      sameValue(original, outputValue(output))
      ? { terminalStatus: "refunded" }
      : null;
  if (witness.purpose !== "InitializeWithdrawalPayout" || !isHex28(hub.payout))
    return null;
  const payoutIndex =
    body.mint() === undefined ? -1 : mintPolicyIndex(body.mint()!, hub.payout);
  const payoutRedeemer =
    payoutIndex < 0 ? null : matchingRedeemer(transaction, "mint", payoutIndex);
  const payout =
    payoutRedeemer === null
      ? null
      : dataRoundTrip<{
          MintPayout: {
            withdrawal_utxo_out_ref: {
              transactionId: string;
              outputIndex: bigint;
            };
            withdrawal_input_index: bigint;
            retirement_withdraw_redeemer_index: bigint;
            hub_ref_input_index: bigint;
          };
        }>(
          payoutRedeemer.bytes.bytesHex,
          asDataType<EventSchema>(PayoutMintRedeemerSchema),
        );
  if (
    payout === null ||
    !("MintPayout" in payout) ||
    payout.MintPayout.retirement_withdraw_redeemer_index !==
      BigInt(retired.globalIndex) ||
    payout.MintPayout.withdrawal_input_index !== BigInt(inputIndex) ||
    payout.MintPayout.hub_ref_input_index !== hub_reference_index ||
    payout.MintPayout.withdrawal_utxo_out_ref.transactionId !==
      event.transactionHash ||
    payout.MintPayout.withdrawal_utxo_out_ref.outputIndex !==
      BigInt(event.outputIndex) ||
    withdrawal.event.info.validity !== "WithdrawalIsValid" ||
    !addressMatchesData(output.address(), hub.payout_addr)
  )
    return null;
  original.set(hub.payout + event.assetNameHex, 1n);
  const payoutAssets = body
    .mint()
    ?.get_assets(CML.ScriptHash.from_hex(hub.payout));
  const datum = inlineDatumCbor(output);
  return payoutAssets?.len() === 1 &&
    payoutAssets.get(CML.AssetName.from_hex(event.assetNameHex)) === 1n &&
    sameValue(original, outputValue(output)) &&
    datum !== null &&
    historyRawField(datum, []) === historyWithdrawalPayoutDatum(payloadCbor)
    ? { terminalStatus: "payout_initialized" }
    : null;
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
    if (event.kind !== "forced_order") {
      const disposition = consumeHistoryOrder(
        event,
        transaction,
        referenceEvidence,
        deployment,
        inputIndex,
      );
      if (disposition === null) return null;
      active.delete(event.outRef);
      if ("continuation" in disposition)
        active.set(disposition.continuation.outRef, disposition.continuation);
      else
        terminal.push(
          Object.freeze({
            ...event,
            terminalStatus: disposition.terminalStatus,
            terminalTransactionHash: transaction.txHash,
            terminalPointDigest: block.chainPoint.pointDigest,
            terminalBlockHash: block.chainPoint.blockHash,
            terminalSlot: block.chainPoint.slot,
            terminalBlockNo: block.chainPoint.blockNo,
            terminalFinalityStatus: "pending",
          }),
        );
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
        store.chainPoints.some(
          ({ chainPointId }) => chainPointId === utxo.chainPointId,
        ) &&
        utxo.output.cborHex === event.outputCborHex &&
        utxo.output.sha256 === event.outputDigest
      );
    })
  );
};

const storeDigest = (store: WatcherDurableStore): string =>
  watcherDurableStoreBytesSha256(encodeWatcherDurableStore(store));

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
  /** Coverage sits on the head entry and extends over quiet blocks. */
  coverage: WatcherLocalUserEventCoverage | null;
  generation: number;
  acceptedAtMonotonicMs: number | null;
  closed: boolean;
  suspendedAt: number | null;
  semanticReplay: boolean;
};
/**
 * The moving coverage checkpoint: every block from the head entry's cursor
 * through `point` has been admitted from the native stream with its parent
 * link checked, and none of them carried anything the event fold tracks.
 * Point coverage is a lookup against it, never a walk.
 */
export type WatcherLocalUserEventCoverage = Readonly<{
  point: WatcherUserEventOriginFacts["parentPoint"];
  headEntryDigest: string;
}>;
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

const localCoverageOnHead = (
  head: WatcherLocalUserEventEntry,
): WatcherLocalUserEventCoverage =>
  Object.freeze({
    point: Object.freeze({ ...head.cursor }),
    headEntryDigest: head.entryDigest,
  });

/** The covered head: the coverage point when it references the head entry,
 * otherwise the head entry's own cursor. The activation block has no entry
 * yet, so before it the covered head is the origin's parent. */
const localCoverageHead = (
  owner: LocalHistoryOwner,
): WatcherUserEventOriginFacts["parentPoint"] => {
  const head = owner.entries.at(-1);
  if (head === undefined) return owner.origin.parentPoint;
  const coverage = owner.coverage;
  if (coverage === null || coverage.headEntryDigest !== head.entryDigest)
    return localRefuse("coverage does not sit on the head entry");
  return coverage.point;
};

export const readWatcherLocalUserEventCoverage = (
  history: WatcherLocalUserEventHistory,
): WatcherLocalUserEventCoverage | null => {
  const owner = localOwner(history);
  const head = owner.entries.at(-1);
  if (head === undefined) return null;
  return Object.freeze({
    point: Object.freeze({ ...localCoverageHead(owner) }),
    headEntryDigest: head.entryDigest,
  });
};

/**
 * Admits one quiet native block above the covered head. The link is checked
 * locally from the header the native stream delivered: parent hash, block
 * number and slot. No request, no observation, no digest-chain change.
 */
export const advanceWatcherLocalUserEventCoverage = (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    header: Readonly<{
      blockHash: string;
      parentBlockHash: string;
      blockNo: string;
      slot: string;
    }>;
  }>,
): WatcherLocalUserEventCoverage => {
  const owner = localOwner(input.history);
  const head = owner.entries.at(-1);
  if (
    head === undefined ||
    owner.checkpoint === null ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    owner.semanticReplay
  )
    return localRefuse("coverage requires a settled publication");
  const covered = localCoverageHead(owner);
  const { header } = input;
  if (
    !isHex32(header.blockHash) ||
    !isHex32(header.parentBlockHash) ||
    !isNatural(header.blockNo) ||
    !isNatural(header.slot) ||
    header.parentBlockHash !== covered.blockHash ||
    BigInt(header.blockNo) !== BigInt(covered.blockNo) + 1n ||
    BigInt(header.slot) <= BigInt(covered.slot)
  )
    return localRefuse(
      "quiet block is not the direct child of the covered head",
    );
  const point = admitFraudProofRawL1Point({
    blockHash: header.blockHash,
    blockNo: header.blockNo,
    slot: header.slot,
    pointId: computeFraudProofRawL1PointId({
      blockHash: header.blockHash,
      blockNo: header.blockNo,
      slot: header.slot,
    }),
  });
  owner.coverage = Object.freeze({
    point: Object.freeze({ ...point }),
    headEntryDigest: head.entryDigest,
  });
  return owner.coverage;
};

/**
 * Moves coverage back to a point at or above the head entry after a native
 * rollback whose fork lies inside the quiet stretch. The caller resolved the
 * point's block number and hash from the headers it admitted; a fork below
 * the head entry is not a coverage matter and goes through rollback recovery.
 */
export const rewindWatcherLocalUserEventCoverage = (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    point: WatcherUserEventOriginFacts["parentPoint"];
  }>,
): WatcherLocalUserEventCoverage => {
  const owner = localOwner(input.history);
  const head = owner.entries.at(-1);
  if (
    head === undefined ||
    owner.checkpoint === null ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    owner.semanticReplay
  )
    return localRefuse("coverage requires a settled publication");
  const covered = localCoverageHead(owner);
  const point = admitFraudProofRawL1Point(input.point);
  if (
    BigInt(point.blockNo) > BigInt(covered.blockNo) ||
    BigInt(point.blockNo) < BigInt(head.cursor.blockNo) ||
    (point.blockNo === head.cursor.blockNo && !same(point, head.cursor))
  )
    return localRefuse("coverage rewind target is outside the covered stretch");
  owner.coverage = Object.freeze({
    point: Object.freeze({ ...point }),
    headEntryDigest: head.entryDigest,
  });
  return owner.coverage;
};

/**
 * Restores saved coverage over a restored head. A record that references an
 * older entry is a torn write between the head publication and the coverage
 * update; it is discarded and coverage restarts at the head. A record on the
 * current head that lies below it is a bug and fails loudly.
 */
export const restoreWatcherLocalUserEventCoverage = (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    saved: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      headEntryDigest: string;
      checkpointDigest: string;
    }> | null;
  }>,
): Readonly<{
  coverage: WatcherLocalUserEventCoverage;
  disposition: "restored" | "head" | "discarded_stale";
}> => {
  const owner = localOwner(input.history);
  const head = owner.entries.at(-1);
  if (head === undefined || owner.checkpoint === null)
    return localRefuse("coverage requires a settled publication");
  const onHead = localCoverageOnHead(head);
  const saved = input.saved;
  if (saved === null) {
    owner.coverage = onHead;
    return Object.freeze({ coverage: onHead, disposition: "head" });
  }
  if (
    saved.headEntryDigest !== head.entryDigest ||
    saved.checkpointDigest !== owner.checkpoint.checkpointDigest
  ) {
    owner.coverage = onHead;
    return Object.freeze({ coverage: onHead, disposition: "discarded_stale" });
  }
  if (
    !isHex32(saved.blockHash) ||
    !isNatural(saved.blockNo) ||
    !isNatural(saved.slot) ||
    BigInt(saved.blockNo) < BigInt(head.cursor.blockNo) ||
    BigInt(saved.slot) < BigInt(head.cursor.slot) ||
    (saved.blockNo === head.cursor.blockNo &&
      saved.blockHash !== head.cursor.blockHash)
  )
    return localRefuse(
      "saved coverage lies below the head entry it references",
    );
  const point = admitFraudProofRawL1Point({
    blockHash: saved.blockHash,
    blockNo: saved.blockNo,
    slot: saved.slot,
    pointId: computeFraudProofRawL1PointId({
      blockHash: saved.blockHash,
      blockNo: saved.blockNo,
      slot: saved.slot,
    }),
  });
  owner.coverage = Object.freeze({
    point: Object.freeze({ ...point }),
    headEntryDigest: head.entryDigest,
  });
  return Object.freeze({ coverage: owner.coverage, disposition: "restored" });
};

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
    ...(finalityPolicy.customNetwork === undefined
      ? {}
      : { customNetwork: finalityPolicy.customNetwork }),
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
    coverage: null,
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

/** Retained checkpoint closure measured the way the publish bound measures
 * it: unique objects by digest, canonical bytes, evidence nodes. */
const localRetainedArchive = (owner: LocalHistoryOwner) =>
  Object.freeze({
    objects: owner.archiveObjects.length,
    bytes: owner.archiveObjects.reduce(
      (total, object) => total + object.bytesHex.length / 2,
      0,
    ),
    nodes: owner.archiveObjects.reduce(
      (total, object) => total + localArchiveBudgets.get(object)!.nodes,
      0,
    ),
  });
/** Every retained entry pins its own durable-store snapshot, so the closure
 * grows with the store as well as with the entry count. An anchor is due once
 * more than the 64-entry suffix is retained and either the entry count reaches
 * the active bound or any closure dimension has used half its bound; waiting
 * for the entry count alone lets the publish bound refuse first. */
const localAnchorDue = (owner: LocalHistoryOwner): boolean => {
  if (owner.entries.length <= 64) return false;
  if (
    owner.entries.length >=
    WATCHER_USER_EVENT_INDEXER_BOUNDS.activeHistoryEntries
  )
    return true;
  const retained = localRetainedArchive(owner);
  return (
    retained.objects * 2 >=
      WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries ||
    retained.bytes * 2 >=
      WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes ||
    retained.nodes * 2 >=
      WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes
  );
};

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
    retainedArchive: localRetainedArchive(owner),
    anchorDue: localAnchorDue(owner),
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
  } else {
    // The block must be the direct child of the covered head: the last entry
    // itself, or the quiet stretch admitted above it block by block.
    const covered = localCoverageHead(owner);
    if (
      !same(capture.predecessorPoint, covered) ||
      block.chainPoint.parentBlockHash !== covered.blockHash ||
      BigInt(capture.point.blockNo) !== BigInt(covered.blockNo) + 1n ||
      BigInt(capture.point.slot) <= BigInt(covered.slot)
    ) {
      return localRefuse("block is not the strict full-point successor");
    }
  }
  const derivedSnapshot = deriveLocalBlockEventSnapshot(
    owner.policy,
    owner.snapshot,
    block,
    referenceEvidence,
    {
      appliedScriptHashes: watcherDeploymentAppliedScriptHashes(
        owner.deploymentIdentity,
      ),
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
        chainPointId:
          sourceStore.protocolUtxos.find(
            ({ outRef }) => outRef === event.outRef,
          )?.chainPointId ?? block.chainPoint.chainPointId,
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
  // Retained objects are identified by digest: an entry whose evidence or
  // store archive repeats an earlier object must not be budgeted twice.
  const archiveObjects = Object.freeze([
    ...new Map(
      [
        ...owner.archiveObjects,
        evidence,
        entryArchive,
        storeArchive,
        payload,
      ].map((object) => [object.digest, object] as const),
    ).values(),
  ]);
  const retainedNodes = archiveObjects.reduce(
    (nodes, object) => nodes + localArchiveBudgets.get(object)!.nodes,
    0,
  );
  const retainedBytes = archiveObjects.reduce(
    (bytes, object) => bytes + object.bytesHex.length / 2,
    0,
  );
  if (
    retainedNodes > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes ||
    archiveObjects.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries ||
    retainedBytes > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes
  )
    return localRefuse(
      `retained archive bound reached (objects ${archiveObjects.length.toString()}/${WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries.toString()}, bytes ${retainedBytes.toString()}/${WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes.toString()}, nodes ${retainedNodes.toString()}/${WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes.toString()}; new evidence ${localArchiveBudgets.get(evidence)!.nodes.toString()} nodes ${(evidence.bytesHex.length / 2).toString()} bytes)`,
    );
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
  owner.coverage = localCoverageOnHead(prepared.value.entry);
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

/** Parses and validates one sealed segment's retained-entry payload. */
const localArchivedSegmentEntries = async (
  owner: LocalHistoryOwner,
  archive: WatcherUserEventArchive,
  segment: WatcherUserEventArchiveIndexRead,
): Promise<readonly WatcherLocalUserEventEntry[]> => {
  const payload = await localReadArchivedValue(
    archive,
    segment.index.sourcePayloadDigest,
  );
  const entriesValue = localArchiveField(payload, ["retainedEntries"]);
  if (
    !Array.isArray(entriesValue) ||
    entriesValue.length === 0 ||
    entriesValue.length > Number(owner.policy.maximumActiveHistoryEntries) ||
    localArchiveField(payload, ["originDigest"]) !== owner.originDigest ||
    !same(localArchiveField(payload, ["policy"]), owner.policy)
  )
    return localRefuse("historical cutoff segment payload differs");
  const entries = entriesValue.map(localArchivedEntry);
  if (!same(localArchiveField(payload, ["head"]), entries.at(-1)))
    return localRefuse("historical cutoff segment head differs");
  return entries;
};

/**
 * Finds the sealed entry observed at `blockNo`, or null when no event block
 * was observed there. Entries are no longer dense in block numbers (quiet
 * blocks publish nothing), so sealed segments are bisected by the block
 * numbers of their first and last retained entries.
 */
const localArchivedEntryAtBlock = async (
  owner: LocalHistoryOwner,
  archive: WatcherUserEventArchive,
  root: WatcherUserEventArchiveIndexRead,
  blockNo: bigint,
): Promise<Readonly<{
  segment: WatcherUserEventArchiveIndexRead;
  entry: WatcherLocalUserEventEntry;
}> | null> => {
  let first = 0n;
  let last = BigInt(root.index.indexSequence);
  for (let iteration = 0; first <= last && iteration < 65; iteration += 1) {
    const middle = (first + last) / 2n;
    const segment = await findWatcherUserEventArchiveIndex(
      archive,
      root,
      middle.toString(),
    );
    // The sealed payload retains the suffix carried over from the previous
    // segment as well; only this segment's own entry range orders the search.
    const entries = (
      await localArchivedSegmentEntries(owner, archive, segment)
    ).filter(
      (candidate) =>
        BigInt(candidate.sequence) >=
          BigInt(segment.index.firstEntrySequence) &&
        BigInt(candidate.sequence) <= BigInt(segment.index.lastEntrySequence),
    );
    if (entries.length === 0)
      return localRefuse("historical cutoff segment range is absent");
    if (blockNo < BigInt(entries[0]!.cursor.blockNo)) last = middle - 1n;
    else if (blockNo > BigInt(entries.at(-1)!.cursor.blockNo))
      first = middle + 1n;
    else {
      const matches = entries.filter(
        (candidate) => BigInt(candidate.cursor.blockNo) === blockNo,
      );
      if (matches.length > 1)
        return localRefuse("historical cutoff entry is not uniquely archived");
      return matches.length === 0
        ? null
        : Object.freeze({ segment, entry: matches[0]! });
    }
  }
  return null;
};

/**
 * Looks up the accepted event block at a block number: the retained suffix
 * first, then the sealed archive. Null means the block lies inside the
 * published range but was quiet, so nothing was observed there.
 */
const localEntryAtBlock = async (
  owner: LocalHistoryOwner,
  blockNo: bigint,
  archive: WatcherUserEventArchive,
): Promise<Readonly<{
  entry: WatcherLocalUserEventEntry;
  rawBlockCbor: unknown;
}> | null> => {
  const head = owner.entries.at(-1)!;
  if (
    blockNo < BigInt(owner.origin.block.chainPoint.blockNo) ||
    blockNo > BigInt(head.cursor.blockNo)
  )
    return localRefuse(
      "header cutoff lies outside the published event history",
    );
  const retained = localRetainedEvidence(owner).find(
    ({ entry }) => BigInt(entry.cursor.blockNo) === blockNo,
  );
  if (retained !== undefined)
    return Object.freeze({
      entry: retained.entry,
      rawBlockCbor: retained.rawBlockCbor,
    });
  // Pinned evidence reaches below the retained suffix, so only the suffix's
  // own oldest entry bounds the range where an absent entry means quiet.
  if (blockNo > BigInt(owner.entries[0]!.cursor.blockNo)) return null;
  const root = owner.archiveIndex;
  if (root === null) return null;
  const found = await localArchivedEntryAtBlock(owner, archive, root, blockNo);
  if (found === null) return null;
  const { segment, entry } = found;
  if (!segment.index.sourceArchiveDigests.includes(entry.evidenceDigest))
    return localRefuse(
      "historical cutoff evidence is not in the sealed closure",
    );
  const evidence = await localReadArchivedValue(archive, entry.evidenceDigest);
  if (
    localArchiveField(evidence, ["schemaVersion"]) !==
      "midgard-watcher-local-user-event-block-evidence-v1" ||
    localArchiveField(evidence, ["numericEncoding"]) !== "exact-decimal-strings"
  )
    return localRefuse("historical cutoff evidence framing differs");
  const rawBlockCbor = localArchiveField(evidence, [
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
      return localRefuse("historical cutoff original witness binding differs");
  }
  return Object.freeze({ entry, rawBlockCbor });
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
  const originDigest = owner.originDigest;
  const policyDigest = owner.policy.policyDigest;
  const found = await localEntryAtBlock(
    owner,
    BigInt(header.observedBlockNo),
    archive,
  );
  if (found === null)
    return localRefuse("header cutoff block carried no observed event");
  const { entry, rawBlockCbor } = found;
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
  // Pointer continuations replace transactionHash/outRef, but admission remains
  // the unique valid transaction that consumed the immutable event nonce in
  // the authenticated origin block. Archived bytes describe this live owner's
  // accepted lineage; they do not establish a new origin or fresh authority.
  const originOrder = localCutoffTransactionOrder(originEvidence);
  const admissions = originOrder.transactionIds.filter((_, index) => {
    if (originOrder.invalidTransactions.has(index)) return false;
    const inputs = originOrder.bodies.get(index).inputs();
    for (let inputIndex = 0; inputIndex < inputs.len(); inputIndex += 1) {
      if (outputReference(inputs.get(inputIndex)) === event.nonceOutRef)
        return true;
    }
    return false;
  });
  if (admissions.length !== 1)
    return localRefuse("event origin nonce is not uniquely consumed");
  if (!occursThroughHeader(originEvidence, admissions[0]!))
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
): Promise<WatcherLocalUserEventPointCoverage> => {
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
  const head = owner.entries.at(-1)!;
  if (BigInt(point.blockNo) > BigInt(head.cursor.blockNo))
    return localRefuse(
      "point lies above the head entry; coverage of the quiet stretch is a runtime lookup",
    );
  const found = await localEntryAtBlock(
    owner,
    BigInt(point.blockNo),
    input.archive,
  );
  if (found !== null) {
    // An event block: the point must be exactly the accepted block.
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
  }
  // A quiet block at or below the head entry lies inside the strict successor
  // chain the head's lineage established. Callers below the release-final
  // boundary need no hash check; the head entry's canonical corroboration
  // already fixes every ancestor by construction.
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
  return found === null ? "quiet" : "event";
};

/**
 * How a point at or below the head entry is covered: "event" when the exact
 * accepted block was observed there, "quiet" when the block lies inside the
 * linked stretch between observations. A quiet point above the release-final
 * boundary still needs its hash confirmed against the canonical chain, which
 * the runtime resolves with one node lookup on demand.
 */
export type WatcherLocalUserEventPointCoverage = "event" | "quiet";

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
      ...(typeof value === "object" &&
      value !== null &&
      Object.hasOwn(value, "witnessScriptHash")
        ? ["witnessScriptHash"]
        : []),
      ...(typeof value === "object" &&
      value !== null &&
      Object.hasOwn(value, "historyPayloadCborHex")
        ? ["historyPayloadCborHex"]
        : []),
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
      (event.kind === "forced_order"
        ? !isHex28(event.witnessScriptHash) ||
          Object.hasOwn(event, "historyPayloadCborHex")
        : (event.kind !== "deposit" && event.kind !== "withdrawal") ||
          !isHexBytes(event.historyPayloadCborHex) ||
          Object.hasOwn(event, "witnessScriptHash")) ||
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
        // The fresh W12 capture just corroborated `entry.parent` as this
        // block's canonical predecessor; the archived quiet stretch between
        // the previous entry and that parent lies on the same linear chain.
        if (previous !== null)
          owner.coverage = Object.freeze({
            point: Object.freeze({ ...entry.parent }),
            headEntryDigest: owner.entries.at(-1)!.entryDigest,
          });
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
              chainPointId:
                archivedSourceStore.protocolUtxos.find(
                  ({ outRef }) => outRef === event.outRef,
                )?.chainPointId ?? oldPoint.chainPointId,
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

/** Rebuild a replacement branch from fresh native evidence. Unlike archived
 * readmission, this requires a positive conflicting block at the saved head's
 * height; an unavailable Order, missing block, or transport error proves nothing. */
export const prepareWatcherLocalUserEventCanonicalReplay = async (
  input: Omit<
    Parameters<typeof prepareWatcherLocalUserEventReadmission>[0],
    "replayBlock"
  > &
    Readonly<{
      replayCanonical: (
        savedHead: WatcherUserEventOriginFacts["parentPoint"],
      ) => AsyncIterable<
        Awaited<ReturnType<WatcherLocalUserEventReplaySource>>
      >;
    }>,
): Promise<WatcherLocalUserEventReadmission> => {
  const publication = await readWatcherProtectedUserEventCheckpoint(
    input.runtime,
  );
  const published = readWatcherProtectedUserEventCheckpointReceipt(publication);
  const previous = published.checkpoint;
  if (
    previous === null ||
    published.payload === null ||
    published.validation?.checkpointDigest !== previous.checkpointDigest ||
    published.validation.payloadDigest !== previous.payloadDigest
  )
    return localRefuse(
      "canonical replay requires a validated protected checkpoint",
    );
  const payload = objectForLocalRestart(published.payload);
  const savedHead = localArchivedEntry(payload.head);
  const history = createLocalUserEventHistory({
    ...input,
    publication,
    semanticReplay: true,
  });
  const owner = localOwner(history);
  let retained: Awaited<ReturnType<WatcherLocalUserEventReplaySource>> | null =
    null;
  try {
    if (
      !same(payload.policy, owner.policy) ||
      previous.userEventPolicyDigest !== owner.policy.policyDigest ||
      previous.finalityPolicyDigest !== owner.finalityPolicy.policyDigest ||
      !isHex32(payload.originArchiveDigest)
    )
      return localRefuse("canonical replay deployment or policy differs");
    const oldOriginBytes = await input.archive.read(
      payload.originArchiveDigest,
    );
    if (
      oldOriginBytes === null ||
      sha256Bytes(oldOriginBytes) !== payload.originArchiveDigest
    )
      return localRefuse("canonical replay original provenance is absent");
    const oldOrigin = objectForLocalRestart(oldOriginBytes);
    if (
      !same(
        localStableOrigin(oldOrigin.facts),
        localArchiveEvidence(localStableOrigin(owner.origin)),
      )
    )
      return localRefuse("canonical replay activation differs");
    const firstPair = {
      finality: input.finality,
      observation: input.observation,
      referenceAuthority: input.referenceAuthority,
    };
    commitLocalUserEventTransition(
      history,
      prepareLocalUserEventTransition(history, firstPair),
    );
    let replacement: WatcherUserEventOriginFacts["parentPoint"] | null = null;
    const writeObjects = async () => {
      for (const object of owner.archiveObjects) {
        if (
          (await input.archive.put(Buffer.from(object.bytesHex, "hex"))) !==
          object.digest
        )
          return localRefuse("canonical replay archive write differs");
      }
    };
    for await (const pair of input.replayCanonical(savedHead.cursor)) {
      try {
        const current = localLivePair(owner, pair).witness.current.observation
          .capture.point;
        if (BigInt(current.blockNo) > BigInt(savedHead.cursor.blockNo))
          return localRefuse("canonical replay skipped the saved head height");
        commitLocalUserEventTransition(
          history,
          prepareLocalUserEventTransition(history, pair),
        );
        await retained?.close();
        retained = pair;
        if (localAnchorDue(owner)) {
          await writeObjects();
          const anchor = await prepareLocalUserEventAnchor(
            history,
            pair,
            input.archive,
          );
          commitLocalUserEventAnchor(anchor);
        }
        if (current.blockNo === savedHead.cursor.blockNo) {
          if (current.blockHash === savedHead.cursor.blockHash)
            return localRefuse(
              "canonical replay has no conflicting head block",
            );
          replacement = current;
          break;
        }
      } finally {
        if (retained !== pair) await pair.close();
      }
    }
    if (replacement === null || retained === null)
      return localRefuse(
        "canonical replacement has not reached the saved head height",
      );
    // Historical predecessor evidence remains inspectable, but only this fresh
    // contiguous fold and its live final head authorize the replacement.
    const provenance = localArchiveObject({
      kind: "canonical_branch_replacement",
      previousCheckpoint: previous,
      previousHead: savedHead,
      replacementHead: replacement,
    });
    const archiveObjects = localArchiveClosure([
      ...owner.archiveObjects,
      provenance,
    ]);
    const nextCheckpoint = makeWatcherUserEventCheckpoint({
      ...previous,
      checkpointSequence: (BigInt(previous.checkpointSequence) + 1n).toString(),
      predecessorCheckpointDigest: previous.checkpointDigest,
      rollbackGeneration: (BigInt(previous.rollbackGeneration) + 1n).toString(),
      payloadDigest: owner.checkpoint!.payloadDigest,
      requiredArchiveDigests: archiveObjects.map(({ digest }) => digest).sort(),
    });
    const refreshed = readWatcherProtectedUserEventCheckpointReceipt(
      await readWatcherProtectedUserEventCheckpoint(input.runtime),
    );
    if (!same(refreshed.checkpoint, previous))
      return localRefuse("protected head changed during canonical replay");
    const receipt = Object.freeze({ [localReadmissionBrand]: true as const });
    const headPair = retained;
    localReadmissions.set(receipt, {
      runtime: input.runtime,
      history,
      pair: headPair,
      generation: owner.generation,
      previousCheckpoint: previous,
      nextCheckpoint,
      archiveObjects,
      release: () => headPair.close(),
      accepted: false,
    });
    readWatcherLocalUserEventReadmission(receipt);
    retained = null;
    return receipt;
  } catch (error) {
    closeWatcherLocalUserEventHistory(history);
    throw error;
  } finally {
    await retained?.close();
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
  owner.coverage = localCoverageOnHead(owner.entries.at(-1)!);
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
      coverage: localCoverageOnHead(head),
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
