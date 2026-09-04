import { createHash } from "node:crypto";

import {
  type DeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorDatum,
  AddressSchema,
  ConfirmedState,
  DA_ATTESTATION_ASSET_NAME_PREFIX,
  DaAttestationDatum,
  deriveStateQueueCorrectionTransition,
  FraudProofTokenDatum,
  Header,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  LinkedListDatum,
  parseStateQueueCorrectionTransition,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  RetiredOperatorDatum,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  type StateQueueCorrectionTransition,
  StateQueueNode,
  type StateQueueTransitionNode,
} from "@al-ft/midgard-sdk";
import { CML, Data, valueToAssets } from "@lucid-evolution/lucid";

import { blake2b } from "../../../midgard-core/node_modules/@noble/hashes/blake2.js";
import {
  evaluateWatcherFinality,
  parseWatcherFinalityPolicy,
  watcherFinalityConfiguredSource,
  type WatcherFinalityResult,
} from "../l1/finality-engine.js";
import {
  encodeWatcherNormalizedL1Block,
  makeWatcherL1NormalizationSession,
  normalizeWatcherL1Block,
  type WatcherL1NormalizationSession,
  type WatcherL1Transaction,
  type WatcherL1TransportAttestationContext,
  watcherL1TransportAttestationDetails,
  type WatcherNormalizedL1Block,
} from "../l1/l1-adapter.js";
import {
  evaluateWatcherMultiProviderConsistency,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS,
} from "../l1/multi-provider-consistency.js";
import {
  parseWatcherPostFinalityRecoveryResult,
  parseWatcherRollbackResult,
  type WatcherPostFinalityRecoveryInput,
  type WatcherPostFinalityRecoveryResult,
  type WatcherRollbackResult,
  type WatcherRollbackVerificationContext,
} from "../l1/rollback-engine.js";
import {
  type VerifiedWatcherDeploymentIdentity,
  verifyWatcherDeploymentIdentity,
  type WatcherDeploymentIdentityPolicy,
  type WatcherDeploymentTrustRoot,
} from "../runtime/deployment-identity.js";
import {
  encodeWatcherDurableStore,
  journalWatcherProtocolUtxoTransition,
  parseWatcherDurableStore,
  type WatcherDurableStore,
  watcherDurableStoreBytesSha256,
  type WatcherProtocolUtxo,
  watcherSameCanonicalJson,
  watcherSha256CanonicalJson,
} from "../storage/durable-store.js";

export const WATCHER_STATE_QUEUE_INDEXER_POLICY_SCHEMA_VERSION =
  "midgard-watcher-state-queue-indexer-policy-v1" as const;
export const WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION =
  "midgard-watcher-state-queue-snapshot-v1" as const;
export const WATCHER_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION =
  "midgard-watcher-state-queue-observation-v1" as const;
export const WATCHER_STATE_QUEUE_INDEXER_STATE_SCHEMA_VERSION =
  "midgard-watcher-state-queue-indexer-state-v1" as const;
export const WATCHER_STATE_QUEUE_INDEXER_RESULT_SCHEMA_VERSION =
  "midgard-watcher-state-queue-indexer-result-v1" as const;
export const WATCHER_STATE_QUEUE_PUBLIC_CONTEXT_SCHEMA_VERSION =
  "midgard-watcher-state-queue-public-context-v1" as const;

export const WATCHER_STATE_QUEUE_INDEXER_BOUNDS = Object.freeze({
  queueNodes: 4_096,
  activeOperators: 4_096,
  historyEntries: 256,
  auditEntries: 256,
  evidenceGraphNodes: 2_000_000,
  evidenceGraphBytes: 134_217_728,
  deploymentTrustRoots: 16,
  originAuthorities: 256,
  finalityLineageSteps: 2_160,
  evidenceContainerEntries: 16_384,
  cumulativeEvidenceBytes: 134_217_728,
  cumulativeEvidenceNodes: 2_000_000,
  cumulativeFinalitySteps: 2_162,
  maturityDurationMs: 604_800_000n,
  uint64Maximum: 18_446_744_073_709_551_615n,
  withdrawalCount: 10_000n,
  forcedTransactionCount: 10_000n,
  l2TransactionCount: 10_000n,
  depositCount: 10_000n,
  totalEventCount: 40_000n,
  transitionStepCount: 40_000n,
  validationTraceCount: 20_000n,
});

export const WATCHER_STATE_QUEUE_INDEXER_REASON_CODES = [
  "bootstrap_authenticated",
  "append_authenticated",
  "da_attestation_authenticated",
  "merge_authenticated",
  "removal_authenticated",
  "timeout_correction_authenticated",
  "rollback_authenticated",
  "duplicate_observation",
  "malformed_policy",
  "malformed_state",
  "malformed_observation",
  "malformed_public_context",
  "public_evidence_mismatch",
  "binding_mismatch",
  "stale_state",
  "stale_chain_point",
  "identity_collision",
  "linked_queue_mismatch",
  "append_mismatch",
  "merge_mismatch",
  "removal_mismatch",
  "rollback_mismatch",
  "rollback_authority_mismatch",
  "history_limit_exceeded",
] as const;

export const WATCHER_STATE_QUEUE_INDEXER_ALERT_CODES = [
  "watcher_state_queue_input_rejected",
  "watcher_state_queue_binding_rejected",
  "watcher_state_queue_state_rejected",
  "watcher_state_queue_transition_rejected",
] as const;

export type WatcherStateQueueIndexerReasonCode =
  (typeof WATCHER_STATE_QUEUE_INDEXER_REASON_CODES)[number];
export type WatcherStateQueueIndexerAlertCode =
  (typeof WATCHER_STATE_QUEUE_INDEXER_ALERT_CODES)[number];
export type WatcherStateQueueNetwork = "Mainnet" | "Preprod" | "Preview";
export type WatcherStateQueueTransitionKind =
  | "bootstrap"
  | "append"
  | "attach_da"
  | "merge"
  | "remove_fraudulent"
  | "remove_unattested_timeout"
  | "rollback";

export type WatcherStateQueueIndexerPolicy = Readonly<{
  schemaVersion: typeof WATCHER_STATE_QUEUE_INDEXER_POLICY_SCHEMA_VERSION;
  network: WatcherStateQueueNetwork;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarker;
  deploymentTrustRootId: string;
  requiredFinalityDepth: string;
  stateQueuePolicyId: string;
  stateQueueSpendScriptHash: string;
  schedulerPolicyId: string;
  schedulerSpendScriptHash: string;
  activeOperatorsPolicyId: string;
  activeOperatorsSpendScriptHash: string;
  retiredOperatorsPolicyId: string;
  retiredOperatorsSpendScriptHash: string;
  fraudProofPolicyId: string;
  fraudProofSpendScriptHash: string;
  daAttestationPolicyId: string;
  daAttestationSpendScriptHash: string;
  hubOraclePolicyId: string;
  stateQueueAddressHex: string;
  schedulerAddressHex: string;
  activeOperatorsAddressHex: string;
  retiredOperatorsAddressHex: string;
  fraudProofAddressHex: string;
  daAttestationAddressHex: string;
  hubOracleAddressHex: string;
  stateQueueRootAssetNameHex: string;
  stateQueueNodeAssetPrefixHex: string;
  schedulerAssetNameHex: string;
  activeOperatorAssetPrefixHex: string;
  retiredOperatorAssetPrefixHex: string;
  fraudProofCategoryIdsHex: readonly string[];
  daAttestationAssetPrefixHex: string;
  hubOracleAssetNameHex: string;
  maturityDurationMs: string;
  maximumHistoryEntries: string;
  policyDigest: string;
}>;

export type WatcherConfirmedState = Readonly<{
  headerHash: string;
  prevHeaderHash: string;
  utxosRoot: string;
  startTime: string;
  endTime: string;
  protocolVersion: string;
  datumSha256: string;
}>;

export type WatcherStateQueueHeader = Readonly<{
  headerHash: string;
  headerCborHex: string;
  nextHeaderHash: string | null;
  datumSha256: string;
  prevUtxosRoot: string;
  utxosRoot: string;
  withdrawalsRoot: string;
  forcedTransactionsRoot: string;
  transactionsRoot: string;
  depositsRoot: string;
  transitionTraceRoot: string;
  eventToStepRoot: string;
  validationTracesRoot: string;
  withdrawalCount: string;
  forcedTransactionCount: string;
  l2TransactionCount: string;
  depositCount: string;
  totalEventCount: string;
  transitionStepCount: string;
  validationTraceCount: string;
  startTime: string;
  endTime: string;
  blockSlot: string;
  expectedNetworkId: string;
  minFeeA: string;
  minFeeB: string;
  prevHeaderHash: string;
  operatorVkey: string;
  protocolVersion: string;
  daAttestationPolicyId: string | null;
}>;

export type WatcherIndexedActiveOperator = Readonly<{
  operatorVkey: string;
  nextOperatorVkey: string | null;
  bondUnlockTime: string | null;
  inactivityStrikes: string;
  datumSha256: string;
}>;

export type WatcherIndexedRetiredOperator = Readonly<{
  operatorVkey: string;
  nextOperatorVkey: string | null;
  bondUnlockTime: string | null;
  datumSha256: string;
}>;

export type WatcherIndexedScheduler = Readonly<{
  operatorVkey: string | null;
  shiftStartTime: string | null;
  datumSha256: string;
}>;

export type WatcherStateQueueSnapshot = Readonly<{
  schemaVersion: typeof WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION;
  confirmedState: WatcherConfirmedState;
  queue: readonly WatcherStateQueueHeader[];
  scheduler: WatcherIndexedScheduler;
  activeOperators: readonly WatcherIndexedActiveOperator[];
  retiredOperators: readonly WatcherIndexedRetiredOperator[];
  quarantinedFromHeaderHash: string | null;
  snapshotDigest: string;
}>;

export type WatcherStateQueueObservation = Readonly<{
  schemaVersion: typeof WATCHER_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION;
  policyDigest: string;
  network: WatcherStateQueueNetwork;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarker;
  transitionKind: WatcherStateQueueTransitionKind;
  pointDigest: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  transactionHash: string | null;
  transactionIndex: string | null;
  publicInputDigest: string;
  sourceObservationDigest: string;
  chainPointId: string;
  sourceDurableStoreDigest: string;
  sourceDurableStoreRevision: string;
  durableStoreDigest: string;
  durableStoreRevision: string;
  predecessorStateDigest: string | null;
  observationDigest: string;
}>;

export type WatcherStateQueueFinalityLineageStep = Readonly<{
  observations: readonly unknown[];
  consistency: unknown;
  result: unknown;
}>;

export type WatcherStateQueueFinalityAuthority = Readonly<{
  policy: unknown;
  lineage: readonly WatcherStateQueueFinalityLineageStep[];
  previousState: unknown;
  observations: readonly unknown[];
  consistency: unknown;
  result: unknown;
}>;

export type WatcherStateQueueOriginAuthority = Readonly<{
  authenticatedProvider: unknown;
  l1Observation: unknown;
  finalityAuthority: WatcherStateQueueFinalityAuthority;
}>;

export type WatcherStateQueuePublicContext = Readonly<{
  schemaVersion: typeof WATCHER_STATE_QUEUE_PUBLIC_CONTEXT_SCHEMA_VERSION;
  authenticatedProvider: unknown | null;
  l1Observation: unknown | null;
  sourceDurableStore: unknown;
  durableStore: unknown;
  deploymentAuthority: Readonly<{
    signedIdentity: unknown;
    policy: WatcherDeploymentIdentityPolicy;
    trustRoots: readonly WatcherDeploymentTrustRoot[];
    result: VerifiedWatcherDeploymentIdentity;
  }>;
  finalityAuthority: WatcherStateQueueFinalityAuthority | null;
  originAuthorities: readonly WatcherStateQueueOriginAuthority[];
  rollbackAuthority: Readonly<{
    result: unknown;
    context:
      | WatcherRollbackVerificationContext
      | WatcherPostFinalityRecoveryInput;
  }> | null;
}>;

export type WatcherStateQueueRollbackResult =
  | WatcherRollbackResult
  | WatcherPostFinalityRecoveryResult;

export type WatcherStateQueueHistoryEntry = Readonly<{
  predecessorStateDigest: string | null;
  priorActiveEntryDigest: string | null;
  chainPointId: string;
  pointDigest: string;
  transactionHash: string | null;
  transactionIndex: string | null;
  publicInputDigest: string;
  transitionKind: WatcherStateQueueTransitionKind;
  correctionTransition: StateQueueCorrectionTransition | null;
  snapshot: WatcherStateQueueSnapshot;
  observation: WatcherStateQueueObservation;
  publicContext: WatcherStateQueuePublicContext;
  rollbackResult: WatcherStateQueueRollbackResult | null;
  entryDigest: string;
}>;

export type WatcherStateQueueAuditEntry = Readonly<{
  status: "orphaned" | "rollback";
  entry: WatcherStateQueueHistoryEntry;
  auditDigest: string;
}>;

export type WatcherStateQueueIndexerState = Readonly<{
  schemaVersion: typeof WATCHER_STATE_QUEUE_INDEXER_STATE_SCHEMA_VERSION;
  policyDigest: string;
  network: WatcherStateQueueNetwork;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarker;
  pointDigest: string;
  transactionHash: string | null;
  transactionIndex: string | null;
  publicInputDigest: string;
  durableStoreDigest: string;
  snapshot: WatcherStateQueueSnapshot;
  history: readonly WatcherStateQueueHistoryEntry[];
  auditHistory: readonly WatcherStateQueueAuditEntry[];
  stateDigest: string;
}>;

export type WatcherStateQueueIndexerResult = Readonly<{
  schemaVersion: typeof WATCHER_STATE_QUEUE_INDEXER_RESULT_SCHEMA_VERSION;
  action: "accept" | "duplicate" | "reject";
  protocolDecision: "indexed" | "hold" | "quarantined";
  reasonCodes: readonly WatcherStateQueueIndexerReasonCode[];
  alertCodes: readonly WatcherStateQueueIndexerAlertCode[];
  state: WatcherStateQueueIndexerState | null;
  resultDigest: string;
}>;

type PlainRecord = Record<string, unknown>;
type EvidenceGraphBudget = {
  nodes: number;
  bytes: number;
};

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_4 = /^[0-9a-f]{8}$/u;
const HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const NETWORKS = ["Mainnet", "Preprod", "Preview"] as const;
const EMPTY_MERKLE_ROOT =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";

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
    if (budget.nodes > WATCHER_STATE_QUEUE_INDEXER_BOUNDS.evidenceGraphNodes) {
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
    if (budget.bytes > WATCHER_STATE_QUEUE_INDEXER_BOUNDS.evidenceGraphBytes) {
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
      // Shared acyclic evidence (for example finality lineage steps that
      // reference the preceding step's result) is walked and budgeted once.
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
        budget.bytes > WATCHER_STATE_QUEUE_INDEXER_BOUNDS.evidenceGraphBytes
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

const sha256Bytes = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");
const sha256Canonical = watcherSha256CanonicalJson;
const same = watcherSameCanonicalJson;
const headerHashFromCbor = (cborHex: string): string =>
  Buffer.from(blake2b(Buffer.from(cborHex, "hex"), { dkLen: 28 })).toString(
    "hex",
  );

const exactRecord = (
  value: unknown,
  keys: readonly string[],
): PlainRecord | null => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    return null;
  }
  const actual = Reflect.ownKeys(value);
  if (
    actual.length !== keys.length ||
    actual.some((key) => typeof key !== "string" || !keys.includes(key))
  ) {
    return null;
  }
  for (const key of keys) {
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

const exactArray = (
  value: unknown,
  maximum: number,
): readonly unknown[] | null => {
  if (
    !Array.isArray(value) ||
    value.length > maximum ||
    Object.getPrototypeOf(value) !== Array.prototype ||
    Reflect.ownKeys(value).length !== value.length + 1
  ) {
    return null;
  }
  for (let index = 0; index < value.length; index += 1) {
    const descriptor = Object.getOwnPropertyDescriptor(value, index.toString());
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      return null;
    }
  }
  return value;
};

const isHex28 = (value: unknown): value is string =>
  typeof value === "string" && HEX_28.test(value);
const isHex32 = (value: unknown): value is string =>
  typeof value === "string" && HEX_32.test(value);
const isNatural = (value: unknown): value is string =>
  typeof value === "string" &&
  NATURAL.test(value) &&
  value.length <= 20 &&
  BigInt(value) <= WATCHER_STATE_QUEUE_INDEXER_BOUNDS.uint64Maximum;
const isNullableNatural = (value: unknown): value is string | null =>
  value === null || isNatural(value);
const isNullableHex28 = (value: unknown): value is string | null =>
  value === null || isHex28(value);
const isNetwork = (value: unknown): value is WatcherStateQueueNetwork =>
  typeof value === "string" &&
  NETWORKS.includes(value as WatcherStateQueueNetwork);

const parseMarker = (value: unknown): DeploymentMarker | null => {
  const marker = exactRecord(value, ["schemaVersion", "manifestId"]);
  return marker !== null &&
    marker.schemaVersion === MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION &&
    isHex32(marker.manifestId)
    ? Object.freeze({
        schemaVersion: MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
        manifestId: marker.manifestId,
      })
    : null;
};

const sameMarker = (left: DeploymentMarker, right: DeploymentMarker): boolean =>
  left.schemaVersion === right.schemaVersion &&
  left.manifestId === right.manifestId;

const expectedNetworkId = (network: WatcherStateQueueNetwork): number =>
  network === "Mainnet" ? 1 : 0;

const addressMatches = (
  addressHex: string,
  scriptHash: string,
  network: WatcherStateQueueNetwork,
): boolean => {
  try {
    const address = CML.Address.from_hex(addressHex);
    return (
      address.to_hex() === addressHex &&
      address.network_id() === expectedNetworkId(network) &&
      address.payment_cred()?.as_script()?.to_hex() === scriptHash
    );
  } catch {
    return false;
  }
};

const policyWithoutDigest = (
  value: Omit<WatcherStateQueueIndexerPolicy, "policyDigest">,
) => ({ ...value });

export const makeWatcherStateQueueIndexerPolicy = (
  value: Omit<WatcherStateQueueIndexerPolicy, "schemaVersion" | "policyDigest">,
): WatcherStateQueueIndexerPolicy | null => {
  const canonical = {
    schemaVersion: WATCHER_STATE_QUEUE_INDEXER_POLICY_SCHEMA_VERSION,
    ...value,
  };
  return parseWatcherStateQueueIndexerPolicy({
    ...canonical,
    policyDigest: sha256Canonical(canonical),
  });
};

export const parseWatcherStateQueueIndexerPolicy = (
  value: unknown,
): WatcherStateQueueIndexerPolicy | null => {
  const keys = [
    "schemaVersion",
    "network",
    "releaseEvidenceDigest",
    "deploymentMarker",
    "deploymentTrustRootId",
    "requiredFinalityDepth",
    "stateQueuePolicyId",
    "stateQueueSpendScriptHash",
    "schedulerPolicyId",
    "schedulerSpendScriptHash",
    "activeOperatorsPolicyId",
    "activeOperatorsSpendScriptHash",
    "retiredOperatorsPolicyId",
    "retiredOperatorsSpendScriptHash",
    "fraudProofPolicyId",
    "fraudProofSpendScriptHash",
    "daAttestationPolicyId",
    "daAttestationSpendScriptHash",
    "hubOraclePolicyId",
    "stateQueueAddressHex",
    "schedulerAddressHex",
    "activeOperatorsAddressHex",
    "retiredOperatorsAddressHex",
    "fraudProofAddressHex",
    "daAttestationAddressHex",
    "hubOracleAddressHex",
    "stateQueueRootAssetNameHex",
    "stateQueueNodeAssetPrefixHex",
    "schedulerAssetNameHex",
    "activeOperatorAssetPrefixHex",
    "retiredOperatorAssetPrefixHex",
    "fraudProofCategoryIdsHex",
    "daAttestationAssetPrefixHex",
    "hubOracleAssetNameHex",
    "maturityDurationMs",
    "maximumHistoryEntries",
    "policyDigest",
  ] as const;
  const record = exactRecord(value, keys);
  const marker = record === null ? null : parseMarker(record.deploymentMarker);
  if (
    record === null ||
    marker === null ||
    record.schemaVersion !==
      WATCHER_STATE_QUEUE_INDEXER_POLICY_SCHEMA_VERSION ||
    !isNetwork(record.network) ||
    !isHex32(record.releaseEvidenceDigest) ||
    !isHex32(record.deploymentTrustRootId) ||
    !isNatural(record.requiredFinalityDepth) ||
    BigInt(record.requiredFinalityDepth) === 0n ||
    !isHex28(record.stateQueuePolicyId) ||
    !isHex28(record.stateQueueSpendScriptHash) ||
    !isHex28(record.schedulerPolicyId) ||
    !isHex28(record.schedulerSpendScriptHash) ||
    !isHex28(record.activeOperatorsPolicyId) ||
    !isHex28(record.activeOperatorsSpendScriptHash) ||
    !isHex28(record.retiredOperatorsPolicyId) ||
    !isHex28(record.retiredOperatorsSpendScriptHash) ||
    !isHex28(record.fraudProofPolicyId) ||
    !isHex28(record.fraudProofSpendScriptHash) ||
    !isHex28(record.daAttestationPolicyId) ||
    !isHex28(record.daAttestationSpendScriptHash) ||
    !isHex28(record.hubOraclePolicyId) ||
    typeof record.stateQueueAddressHex !== "string" ||
    typeof record.schedulerAddressHex !== "string" ||
    typeof record.activeOperatorsAddressHex !== "string" ||
    typeof record.retiredOperatorsAddressHex !== "string" ||
    typeof record.fraudProofAddressHex !== "string" ||
    typeof record.daAttestationAddressHex !== "string" ||
    typeof record.hubOracleAddressHex !== "string" ||
    record.stateQueueRootAssetNameHex !== STATE_QUEUE_ROOT_ASSET_NAME ||
    record.stateQueueNodeAssetPrefixHex !==
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX ||
    record.schedulerAssetNameHex !== SCHEDULER_ASSET_NAME ||
    record.activeOperatorAssetPrefixHex !==
      ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX ||
    record.retiredOperatorAssetPrefixHex !==
      RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX ||
    !Array.isArray(record.fraudProofCategoryIdsHex) ||
    record.fraudProofCategoryIdsHex.length === 0 ||
    record.fraudProofCategoryIdsHex.some(
      (category) => typeof category !== "string" || !HEX_4.test(category),
    ) ||
    new Set(record.fraudProofCategoryIdsHex).size !==
      record.fraudProofCategoryIdsHex.length ||
    (record.fraudProofCategoryIdsHex as readonly string[]).some(
      (category, index, categories) =>
        index > 0 && categories[index - 1]! >= category,
    ) ||
    record.daAttestationAssetPrefixHex !== DA_ATTESTATION_ASSET_NAME_PREFIX ||
    record.hubOracleAssetNameHex !== HUB_ORACLE_ASSET_NAME ||
    record.maturityDurationMs !==
      WATCHER_STATE_QUEUE_INDEXER_BOUNDS.maturityDurationMs.toString() ||
    !isNatural(record.maximumHistoryEntries) ||
    BigInt(record.maximumHistoryEntries) === 0n ||
    BigInt(record.maximumHistoryEntries) >
      BigInt(WATCHER_STATE_QUEUE_INDEXER_BOUNDS.historyEntries) ||
    !isHex32(record.policyDigest)
  ) {
    return null;
  }
  const canonical = Object.freeze({
    schemaVersion: WATCHER_STATE_QUEUE_INDEXER_POLICY_SCHEMA_VERSION,
    network: record.network,
    releaseEvidenceDigest: record.releaseEvidenceDigest,
    deploymentMarker: marker,
    deploymentTrustRootId: record.deploymentTrustRootId,
    requiredFinalityDepth: record.requiredFinalityDepth,
    stateQueuePolicyId: record.stateQueuePolicyId,
    stateQueueSpendScriptHash: record.stateQueueSpendScriptHash,
    schedulerPolicyId: record.schedulerPolicyId,
    schedulerSpendScriptHash: record.schedulerSpendScriptHash,
    activeOperatorsPolicyId: record.activeOperatorsPolicyId,
    activeOperatorsSpendScriptHash: record.activeOperatorsSpendScriptHash,
    retiredOperatorsPolicyId: record.retiredOperatorsPolicyId,
    retiredOperatorsSpendScriptHash: record.retiredOperatorsSpendScriptHash,
    fraudProofPolicyId: record.fraudProofPolicyId,
    fraudProofSpendScriptHash: record.fraudProofSpendScriptHash,
    daAttestationPolicyId: record.daAttestationPolicyId,
    daAttestationSpendScriptHash: record.daAttestationSpendScriptHash,
    hubOraclePolicyId: record.hubOraclePolicyId,
    stateQueueAddressHex: record.stateQueueAddressHex,
    schedulerAddressHex: record.schedulerAddressHex,
    activeOperatorsAddressHex: record.activeOperatorsAddressHex,
    retiredOperatorsAddressHex: record.retiredOperatorsAddressHex,
    fraudProofAddressHex: record.fraudProofAddressHex,
    daAttestationAddressHex: record.daAttestationAddressHex,
    hubOracleAddressHex: record.hubOracleAddressHex,
    stateQueueRootAssetNameHex: record.stateQueueRootAssetNameHex,
    stateQueueNodeAssetPrefixHex: record.stateQueueNodeAssetPrefixHex,
    schedulerAssetNameHex: record.schedulerAssetNameHex,
    activeOperatorAssetPrefixHex: record.activeOperatorAssetPrefixHex,
    retiredOperatorAssetPrefixHex: record.retiredOperatorAssetPrefixHex,
    fraudProofCategoryIdsHex: Object.freeze([
      ...record.fraudProofCategoryIdsHex,
    ]) as readonly string[],
    daAttestationAssetPrefixHex: record.daAttestationAssetPrefixHex,
    hubOracleAssetNameHex: record.hubOracleAssetNameHex,
    maturityDurationMs: record.maturityDurationMs,
    maximumHistoryEntries: record.maximumHistoryEntries,
  });
  return sha256Canonical(policyWithoutDigest(canonical)) ===
    record.policyDigest &&
    addressMatches(
      canonical.stateQueueAddressHex,
      canonical.stateQueueSpendScriptHash,
      canonical.network,
    ) &&
    addressMatches(
      canonical.schedulerAddressHex,
      canonical.schedulerSpendScriptHash,
      canonical.network,
    ) &&
    addressMatches(
      canonical.activeOperatorsAddressHex,
      canonical.activeOperatorsSpendScriptHash,
      canonical.network,
    ) &&
    addressMatches(
      canonical.retiredOperatorsAddressHex,
      canonical.retiredOperatorsSpendScriptHash,
      canonical.network,
    ) &&
    addressMatches(
      canonical.fraudProofAddressHex,
      canonical.fraudProofSpendScriptHash,
      canonical.network,
    ) &&
    addressMatches(
      canonical.daAttestationAddressHex,
      canonical.daAttestationSpendScriptHash,
      canonical.network,
    ) &&
    addressMatches(
      canonical.hubOracleAddressHex,
      canonical.hubOraclePolicyId,
      canonical.network,
    )
    ? Object.freeze({ ...canonical, policyDigest: record.policyDigest })
    : null;
};

const dataRoundTrip = <T>(cborHex: string, schema: unknown): T | null => {
  try {
    const decoded = Data.from(cborHex, schema as never) as T;
    const lucidHex = Data.to(decoded as never, schema as never);
    const cardanoHex =
      CML.PlutusData.from_cbor_hex(cborHex).to_canonical_cbor_hex();
    return lucidHex === cborHex || cardanoHex === cborHex ? decoded : null;
  } catch {
    return null;
  }
};

const headerData = (
  value: Omit<
    WatcherStateQueueHeader,
    | "headerHash"
    | "headerCborHex"
    | "nextHeaderHash"
    | "datumSha256"
    | "daAttestationPolicyId"
  >,
): Header => ({
  prevUtxosRoot: value.prevUtxosRoot,
  utxosRoot: value.utxosRoot,
  withdrawalsRoot: value.withdrawalsRoot,
  forcedTransactionsRoot: value.forcedTransactionsRoot,
  transactionsRoot: value.transactionsRoot,
  depositsRoot: value.depositsRoot,
  transitionTraceRoot: value.transitionTraceRoot,
  eventToStepRoot: value.eventToStepRoot,
  validationTracesRoot: value.validationTracesRoot,
  withdrawalCount: BigInt(value.withdrawalCount),
  forcedTransactionCount: BigInt(value.forcedTransactionCount),
  l2TransactionCount: BigInt(value.l2TransactionCount),
  depositCount: BigInt(value.depositCount),
  totalEventCount: BigInt(value.totalEventCount),
  transitionStepCount: BigInt(value.transitionStepCount),
  validationTraceCount: BigInt(value.validationTraceCount),
  startTime: BigInt(value.startTime),
  endTime: BigInt(value.endTime),
  blockSlot: BigInt(value.blockSlot),
  expectedNetworkId: BigInt(value.expectedNetworkId),
  minFeeA: BigInt(value.minFeeA),
  minFeeB: BigInt(value.minFeeB),
  prevHeaderHash: value.prevHeaderHash,
  operatorVkey: value.operatorVkey,
  protocolVersion: BigInt(value.protocolVersion),
});

const headerView = (
  header: Header,
  nextHeaderHash: string | null,
  datumSha256: string,
): WatcherStateQueueHeader => {
  const headerCborHex = Data.to(header, Header);
  return Object.freeze({
    headerHash: headerHashFromCbor(headerCborHex),
    headerCborHex,
    nextHeaderHash,
    datumSha256,
    prevUtxosRoot: header.prevUtxosRoot,
    utxosRoot: header.utxosRoot,
    withdrawalsRoot: header.withdrawalsRoot,
    forcedTransactionsRoot: header.forcedTransactionsRoot,
    transactionsRoot: header.transactionsRoot,
    depositsRoot: header.depositsRoot,
    transitionTraceRoot: header.transitionTraceRoot,
    eventToStepRoot: header.eventToStepRoot,
    validationTracesRoot: header.validationTracesRoot,
    withdrawalCount: header.withdrawalCount.toString(),
    forcedTransactionCount: header.forcedTransactionCount.toString(),
    l2TransactionCount: header.l2TransactionCount.toString(),
    depositCount: header.depositCount.toString(),
    totalEventCount: header.totalEventCount.toString(),
    transitionStepCount: header.transitionStepCount.toString(),
    validationTraceCount: header.validationTraceCount.toString(),
    startTime: header.startTime.toString(),
    endTime: header.endTime.toString(),
    blockSlot: header.blockSlot.toString(),
    expectedNetworkId: header.expectedNetworkId.toString(),
    minFeeA: header.minFeeA.toString(),
    minFeeB: header.minFeeB.toString(),
    prevHeaderHash: header.prevHeaderHash,
    operatorVkey: header.operatorVkey,
    protocolVersion: header.protocolVersion.toString(),
    daAttestationPolicyId: null,
  });
};

const HEADER_KEYS = [
  "headerHash",
  "headerCborHex",
  "nextHeaderHash",
  "datumSha256",
  "prevUtxosRoot",
  "utxosRoot",
  "withdrawalsRoot",
  "forcedTransactionsRoot",
  "transactionsRoot",
  "depositsRoot",
  "transitionTraceRoot",
  "eventToStepRoot",
  "validationTracesRoot",
  "withdrawalCount",
  "forcedTransactionCount",
  "l2TransactionCount",
  "depositCount",
  "totalEventCount",
  "transitionStepCount",
  "validationTraceCount",
  "startTime",
  "endTime",
  "blockSlot",
  "expectedNetworkId",
  "minFeeA",
  "minFeeB",
  "prevHeaderHash",
  "operatorVkey",
  "protocolVersion",
  "daAttestationPolicyId",
] as const;

export const makeWatcherStateQueueHeader = (
  value: Omit<WatcherStateQueueHeader, "headerHash" | "headerCborHex">,
): WatcherStateQueueHeader | null => {
  try {
    const { nextHeaderHash, datumSha256, daAttestationPolicyId, ...fields } =
      value;
    const header = headerData(fields);
    const view = {
      ...headerView(header, nextHeaderHash, datumSha256),
      daAttestationPolicyId,
    };
    return parseWatcherStateQueueHeader(view);
  } catch {
    return null;
  }
};

export const parseWatcherStateQueueHeader = (
  value: unknown,
): WatcherStateQueueHeader | null => {
  const record = exactRecord(value, HEADER_KEYS);
  if (
    record === null ||
    !isHex28(record.headerHash) ||
    typeof record.headerCborHex !== "string" ||
    !HEX_BYTES.test(record.headerCborHex) ||
    !isNullableHex28(record.nextHeaderHash) ||
    !isHex32(record.datumSha256) ||
    !isHex32(record.prevUtxosRoot) ||
    !isHex32(record.utxosRoot) ||
    !isHex32(record.withdrawalsRoot) ||
    !isHex32(record.forcedTransactionsRoot) ||
    !isHex32(record.transactionsRoot) ||
    !isHex32(record.depositsRoot) ||
    !isHex32(record.transitionTraceRoot) ||
    !isHex32(record.eventToStepRoot) ||
    !isHex32(record.validationTracesRoot) ||
    !isNatural(record.withdrawalCount) ||
    !isNatural(record.forcedTransactionCount) ||
    !isNatural(record.l2TransactionCount) ||
    !isNatural(record.depositCount) ||
    !isNatural(record.totalEventCount) ||
    !isNatural(record.transitionStepCount) ||
    !isNatural(record.validationTraceCount) ||
    !isNatural(record.startTime) ||
    !isNatural(record.endTime) ||
    !isNatural(record.blockSlot) ||
    !isNatural(record.expectedNetworkId) ||
    !isNatural(record.minFeeA) ||
    !isNatural(record.minFeeB) ||
    !isHex28(record.prevHeaderHash) ||
    !isHex28(record.operatorVkey) ||
    !isNatural(record.protocolVersion) ||
    !isNullableHex28(record.daAttestationPolicyId)
  ) {
    return null;
  }
  const decoded = dataRoundTrip<Header>(record.headerCborHex, Header);
  if (decoded === null) {
    return null;
  }
  const expected = headerView(
    decoded,
    record.nextHeaderHash,
    record.datumSha256,
  );
  const comparable = {
    ...expected,
    daAttestationPolicyId: record.daAttestationPolicyId,
  };
  if (
    !same(value, comparable) ||
    record.headerHash !== headerHashFromCbor(record.headerCborHex) ||
    BigInt(record.endTime) <= BigInt(record.startTime) ||
    BigInt(record.withdrawalCount) >
      WATCHER_STATE_QUEUE_INDEXER_BOUNDS.withdrawalCount ||
    BigInt(record.forcedTransactionCount) >
      WATCHER_STATE_QUEUE_INDEXER_BOUNDS.forcedTransactionCount ||
    BigInt(record.l2TransactionCount) >
      WATCHER_STATE_QUEUE_INDEXER_BOUNDS.l2TransactionCount ||
    BigInt(record.depositCount) >
      WATCHER_STATE_QUEUE_INDEXER_BOUNDS.depositCount ||
    BigInt(record.totalEventCount) !==
      BigInt(record.withdrawalCount) +
        BigInt(record.forcedTransactionCount) +
        BigInt(record.l2TransactionCount) +
        BigInt(record.depositCount) ||
    BigInt(record.transitionStepCount) !== BigInt(record.totalEventCount) ||
    BigInt(record.validationTraceCount) !==
      BigInt(record.forcedTransactionCount) +
        BigInt(record.l2TransactionCount) ||
    (BigInt(record.withdrawalCount) === 0n) !==
      (record.withdrawalsRoot === EMPTY_MERKLE_ROOT) ||
    (BigInt(record.forcedTransactionCount) === 0n) !==
      (record.forcedTransactionsRoot === EMPTY_MERKLE_ROOT) ||
    (BigInt(record.l2TransactionCount) === 0n) !==
      (record.transactionsRoot === EMPTY_MERKLE_ROOT) ||
    (BigInt(record.depositCount) === 0n) !==
      (record.depositsRoot === EMPTY_MERKLE_ROOT) ||
    (BigInt(record.totalEventCount) === 0n) !==
      (record.transitionTraceRoot === EMPTY_MERKLE_ROOT) ||
    (BigInt(record.totalEventCount) === 0n) !==
      (record.eventToStepRoot === EMPTY_MERKLE_ROOT) ||
    (BigInt(record.validationTraceCount) === 0n) !==
      (record.validationTracesRoot === EMPTY_MERKLE_ROOT)
  ) {
    return null;
  }
  return Object.freeze(comparable);
};

const parseConfirmed = (value: unknown): WatcherConfirmedState | null => {
  const record = exactRecord(value, [
    "headerHash",
    "prevHeaderHash",
    "utxosRoot",
    "startTime",
    "endTime",
    "protocolVersion",
    "datumSha256",
  ]);
  return record !== null &&
    isHex28(record.headerHash) &&
    isHex28(record.prevHeaderHash) &&
    isHex32(record.utxosRoot) &&
    isNatural(record.startTime) &&
    isNatural(record.endTime) &&
    isNatural(record.protocolVersion) &&
    isHex32(record.datumSha256) &&
    BigInt(record.endTime) >= BigInt(record.startTime)
    ? Object.freeze(record as unknown as WatcherConfirmedState)
    : null;
};

const parseScheduler = (value: unknown): WatcherIndexedScheduler | null => {
  const record = exactRecord(value, [
    "operatorVkey",
    "shiftStartTime",
    "datumSha256",
  ]);
  if (
    record === null ||
    !isNullableHex28(record.operatorVkey) ||
    !isNullableNatural(record.shiftStartTime) ||
    !isHex32(record.datumSha256) ||
    (record.operatorVkey === null) !== (record.shiftStartTime === null)
  ) {
    return null;
  }
  return Object.freeze(record as unknown as WatcherIndexedScheduler);
};

const parseActive = (value: unknown): WatcherIndexedActiveOperator | null => {
  const record = exactRecord(value, [
    "operatorVkey",
    "nextOperatorVkey",
    "bondUnlockTime",
    "inactivityStrikes",
    "datumSha256",
  ]);
  return record !== null &&
    isHex28(record.operatorVkey) &&
    isNullableHex28(record.nextOperatorVkey) &&
    isNullableNatural(record.bondUnlockTime) &&
    isNatural(record.inactivityStrikes) &&
    isHex32(record.datumSha256)
    ? Object.freeze(record as unknown as WatcherIndexedActiveOperator)
    : null;
};

const parseRetired = (value: unknown): WatcherIndexedRetiredOperator | null => {
  const record = exactRecord(value, [
    "operatorVkey",
    "nextOperatorVkey",
    "bondUnlockTime",
    "datumSha256",
  ]);
  return record !== null &&
    isHex28(record.operatorVkey) &&
    isNullableHex28(record.nextOperatorVkey) &&
    isNullableNatural(record.bondUnlockTime) &&
    isHex32(record.datumSha256)
    ? Object.freeze(record as unknown as WatcherIndexedRetiredOperator)
    : null;
};

const snapshotWithoutDigest = (
  value: Omit<WatcherStateQueueSnapshot, "snapshotDigest">,
) => ({ ...value });

export const makeWatcherStateQueueSnapshot = (
  value: Omit<WatcherStateQueueSnapshot, "schemaVersion" | "snapshotDigest">,
): WatcherStateQueueSnapshot | null => {
  const canonical = {
    schemaVersion: WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION,
    ...value,
  };
  return parseWatcherStateQueueSnapshot({
    ...canonical,
    snapshotDigest: sha256Canonical(canonical),
  });
};

/**
 * Parses the indexer's derived projection for durable restart/replay.
 * Snapshots are outputs of node-derived topology reconstruction, never an
 * accepted observation or security-boundary input.
 */
export const parseWatcherStateQueueSnapshot = (
  value: unknown,
): WatcherStateQueueSnapshot | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "confirmedState",
    "queue",
    "scheduler",
    "activeOperators",
    "retiredOperators",
    "quarantinedFromHeaderHash",
    "snapshotDigest",
  ]);
  const confirmed =
    record === null ? null : parseConfirmed(record.confirmedState);
  const scheduler = record === null ? null : parseScheduler(record.scheduler);
  const queueValues =
    record === null
      ? null
      : exactArray(record.queue, WATCHER_STATE_QUEUE_INDEXER_BOUNDS.queueNodes);
  const activeValues =
    record === null
      ? null
      : exactArray(
          record.activeOperators,
          WATCHER_STATE_QUEUE_INDEXER_BOUNDS.activeOperators,
        );
  const retiredValues =
    record === null
      ? null
      : exactArray(
          record.retiredOperators,
          WATCHER_STATE_QUEUE_INDEXER_BOUNDS.activeOperators,
        );
  const queue = queueValues?.map(parseWatcherStateQueueHeader) ?? null;
  const active = activeValues?.map(parseActive) ?? null;
  const retired = retiredValues?.map(parseRetired) ?? null;
  if (
    record === null ||
    confirmed === null ||
    scheduler === null ||
    queue === null ||
    active === null ||
    retired === null ||
    queue.some((entry) => entry === null) ||
    active.some((entry) => entry === null) ||
    retired.some((entry) => entry === null) ||
    !isNullableHex28(record.quarantinedFromHeaderHash) ||
    !isHex32(record.snapshotDigest)
  ) {
    return null;
  }
  const canonical = Object.freeze({
    schemaVersion: WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION,
    confirmedState: confirmed,
    queue: Object.freeze(queue as WatcherStateQueueHeader[]),
    scheduler,
    activeOperators: Object.freeze(active as WatcherIndexedActiveOperator[]),
    retiredOperators: Object.freeze(retired as WatcherIndexedRetiredOperator[]),
    quarantinedFromHeaderHash: record.quarantinedFromHeaderHash,
  });
  const allOperators = [
    ...canonical.activeOperators.map(({ operatorVkey }) => operatorVkey),
    ...canonical.retiredOperators.map(({ operatorVkey }) => operatorVkey),
  ];
  const queueLinks = canonical.queue.every(
    (header, index) =>
      header.nextHeaderHash ===
      (canonical.queue[index + 1]?.headerHash ?? null),
  );
  const chainBreaks = canonical.queue
    .map((header, index) => {
      const previous = canonical.queue[index - 1];
      return index > 0 &&
        (header.prevHeaderHash !== previous?.headerHash ||
          header.prevUtxosRoot !== previous.utxosRoot ||
          BigInt(header.startTime) !== BigInt(previous.endTime))
        ? (previous?.headerHash ?? null)
        : null;
    })
    .filter((entry): entry is string => entry !== null);
  const queueHead = canonical.queue[0];
  if (
    sha256Canonical(snapshotWithoutDigest(canonical)) !==
      record.snapshotDigest ||
    !queueLinks ||
    chainBreaks.length > 1 ||
    (chainBreaks[0] ?? null) !== canonical.quarantinedFromHeaderHash ||
    (queueHead !== undefined &&
      (queueHead.prevHeaderHash !== canonical.confirmedState.headerHash ||
        queueHead.prevUtxosRoot !== canonical.confirmedState.utxosRoot ||
        BigInt(queueHead.startTime) !==
          BigInt(canonical.confirmedState.endTime))) ||
    new Set(canonical.queue.map(({ headerHash }) => headerHash)).size !==
      canonical.queue.length ||
    new Set(allOperators).size !== allOperators.length ||
    !linkedKeys(
      canonical.activeOperators.map((entry) => [
        entry.operatorVkey,
        entry.nextOperatorVkey,
      ]),
    ) ||
    !linkedKeys(
      canonical.retiredOperators.map((entry) => [
        entry.operatorVkey,
        entry.nextOperatorVkey,
      ]),
    ) ||
    (canonical.scheduler.operatorVkey !== null &&
      !canonical.activeOperators.some(
        ({ operatorVkey }) => operatorVkey === canonical.scheduler.operatorVkey,
      ))
  ) {
    return null;
  }
  return Object.freeze({
    ...canonical,
    snapshotDigest: record.snapshotDigest,
  });
};

const linkedKeys = (
  entries: readonly (readonly [string, string | null])[],
): boolean =>
  entries.every(
    ([key, next], index) =>
      key === entries[index]?.[0] &&
      next === (entries[index + 1]?.[0] ?? null) &&
      (next === null || key < next),
  );

const observationWithoutDigest = (
  value: Omit<WatcherStateQueueObservation, "observationDigest">,
) => ({ ...value });

export const makeWatcherStateQueueObservation = (
  value: Omit<
    WatcherStateQueueObservation,
    "schemaVersion" | "observationDigest"
  >,
): WatcherStateQueueObservation | null => {
  const canonical = {
    schemaVersion: WATCHER_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION,
    ...value,
  };
  return parseWatcherStateQueueObservation({
    ...canonical,
    observationDigest: sha256Canonical(canonical),
  });
};

export const parseWatcherStateQueueObservation = (
  value: unknown,
): WatcherStateQueueObservation | null => {
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
    "transactionHash",
    "transactionIndex",
    "publicInputDigest",
    "sourceObservationDigest",
    "chainPointId",
    "sourceDurableStoreDigest",
    "sourceDurableStoreRevision",
    "durableStoreDigest",
    "durableStoreRevision",
    "predecessorStateDigest",
    "observationDigest",
  ]);
  const marker = record === null ? null : parseMarker(record.deploymentMarker);
  const kinds: readonly WatcherStateQueueTransitionKind[] = [
    "bootstrap",
    "append",
    "attach_da",
    "merge",
    "remove_fraudulent",
    "remove_unattested_timeout",
    "rollback",
  ];
  if (
    record === null ||
    marker === null ||
    record.schemaVersion !== WATCHER_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION ||
    !isHex32(record.policyDigest) ||
    !isNetwork(record.network) ||
    !isHex32(record.releaseEvidenceDigest) ||
    typeof record.transitionKind !== "string" ||
    !kinds.includes(record.transitionKind as WatcherStateQueueTransitionKind) ||
    !isHex32(record.pointDigest) ||
    !isHex32(record.blockHash) ||
    !isNatural(record.slot) ||
    !isNatural(record.blockNo) ||
    !(record.transactionHash === null || isHex32(record.transactionHash)) ||
    !(record.transactionIndex === null || isNatural(record.transactionIndex)) ||
    !isHex32(record.publicInputDigest) ||
    !isHex32(record.sourceObservationDigest) ||
    !isHex32(record.chainPointId) ||
    !isHex32(record.sourceDurableStoreDigest) ||
    !isNatural(record.sourceDurableStoreRevision) ||
    !isHex32(record.durableStoreDigest) ||
    !isNatural(record.durableStoreRevision) ||
    !(
      record.predecessorStateDigest === null ||
      isHex32(record.predecessorStateDigest)
    ) ||
    !isHex32(record.observationDigest)
  ) {
    return null;
  }
  const kind = record.transitionKind as WatcherStateQueueTransitionKind;
  if (
    (kind === "rollback") !== (record.transactionHash === null) ||
    (kind === "rollback") !== (record.transactionIndex === null) ||
    (kind === "bootstrap") !== (record.predecessorStateDigest === null)
  ) {
    return null;
  }
  const canonical = Object.freeze({
    schemaVersion: WATCHER_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION,
    policyDigest: record.policyDigest,
    network: record.network,
    releaseEvidenceDigest: record.releaseEvidenceDigest,
    deploymentMarker: marker,
    transitionKind: kind,
    pointDigest: record.pointDigest,
    blockHash: record.blockHash,
    slot: record.slot,
    blockNo: record.blockNo,
    transactionHash: record.transactionHash,
    transactionIndex: record.transactionIndex,
    publicInputDigest: record.publicInputDigest,
    sourceObservationDigest: record.sourceObservationDigest,
    chainPointId: record.chainPointId,
    sourceDurableStoreDigest: record.sourceDurableStoreDigest,
    sourceDurableStoreRevision: record.sourceDurableStoreRevision,
    durableStoreDigest: record.durableStoreDigest,
    durableStoreRevision: record.durableStoreRevision,
    predecessorStateDigest: record.predecessorStateDigest,
  }) as Omit<WatcherStateQueueObservation, "observationDigest">;
  return sha256Canonical(observationWithoutDigest(canonical)) ===
    record.observationDigest
    ? Object.freeze({
        ...canonical,
        observationDigest: record.observationDigest,
      })
    : null;
};

type VerifiedContext = Readonly<{
  context: WatcherStateQueuePublicContext;
  block: WatcherNormalizedL1Block;
  sourceStore: WatcherDurableStore;
  store: WatcherDurableStore;
  transaction: WatcherL1Transaction | null;
  deploymentPolicy: WatcherDeploymentIdentityPolicy;
  finalityResult: WatcherFinalityResult | null;
  originBlocks: readonly WatcherNormalizedL1Block[];
}>;

type EvidenceBudget = {
  bytes: number;
  nodes: number;
  finalitySteps: number;
  normalizationSession: WatcherL1NormalizationSession;
};

const newEvidenceBudget = (): EvidenceBudget => ({
  bytes: 0,
  nodes: 0,
  finalitySteps: 0,
  normalizationSession: makeWatcherL1NormalizationSession(),
});

const consumeRawEvidence = (
  budget: EvidenceBudget,
  value: unknown,
  rejectAliases: boolean,
): void => {
  const pending: unknown[] = [value];
  const visited = new Set<object>();
  const rootBytes =
    typeof value === "string" ? Buffer.byteLength(value, "utf8") : 0;
  if (
    budget.nodes >=
      WATCHER_STATE_QUEUE_INDEXER_BOUNDS.cumulativeEvidenceNodes ||
    rootBytes >
      WATCHER_STATE_QUEUE_INDEXER_BOUNDS.cumulativeEvidenceBytes - budget.bytes
  ) {
    throw new Error("cumulative W10 evidence budget exceeded");
  }
  budget.nodes += 1;
  budget.bytes += rootBytes;
  while (pending.length > 0) {
    const candidate = pending.pop();
    if (typeof candidate === "object" && candidate !== null) {
      if (visited.has(candidate)) {
        if (rejectAliases) {
          throw new Error("aliased W10 evidence rejected");
        }
        continue;
      }
      visited.add(candidate);
      const prototype = Object.getPrototypeOf(candidate);
      if (
        prototype !== Object.prototype &&
        prototype !== Array.prototype &&
        prototype !== null
      ) {
        throw new Error("unsafe W10 evidence rejected");
      }
      if (
        Array.isArray(candidate) &&
        candidate.length >
          WATCHER_STATE_QUEUE_INDEXER_BOUNDS.evidenceContainerEntries
      ) {
        throw new Error("cumulative W10 evidence budget exceeded");
      }
      let childCount = 0;
      for (const key in candidate) {
        if (!Object.hasOwn(candidate, key)) {
          continue;
        }
        childCount += 1;
        if (
          childCount >
          WATCHER_STATE_QUEUE_INDEXER_BOUNDS.evidenceContainerEntries
        ) {
          throw new Error("cumulative W10 evidence budget exceeded");
        }
        const descriptor = Object.getOwnPropertyDescriptor(candidate, key);
        if (
          descriptor === undefined ||
          descriptor.get !== undefined ||
          descriptor.set !== undefined
        ) {
          throw new Error("unsafe W10 evidence descriptor rejected");
        }
        const keyBytes = Buffer.byteLength(key, "utf8");
        if (
          keyBytes >
          WATCHER_STATE_QUEUE_INDEXER_BOUNDS.cumulativeEvidenceBytes -
            budget.bytes
        ) {
          throw new Error("cumulative W10 evidence budget exceeded");
        }
        budget.bytes += keyBytes;
        if (
          budget.nodes >=
          WATCHER_STATE_QUEUE_INDEXER_BOUNDS.cumulativeEvidenceNodes
        ) {
          throw new Error("cumulative W10 evidence budget exceeded");
        }
        budget.nodes += 1;
        if (typeof descriptor.value === "string") {
          const valueBytes = Buffer.byteLength(descriptor.value, "utf8");
          if (
            valueBytes >
            WATCHER_STATE_QUEUE_INDEXER_BOUNDS.cumulativeEvidenceBytes -
              budget.bytes
          ) {
            throw new Error("cumulative W10 evidence budget exceeded");
          }
          budget.bytes += valueBytes;
        }
        pending.push(descriptor.value);
      }
    }
  }
};

const normalizeBudgetedL1Block = (
  budget: EvidenceBudget,
  authenticatedProvider: unknown,
  observation: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
): WatcherNormalizedL1Block => {
  consumeRawEvidence(budget, authenticatedProvider, true);
  consumeRawEvidence(budget, observation, true);
  const matchingAttestations = transportAttestations.filter((attestation) => {
    const details = watcherL1TransportAttestationDetails(attestation);
    return details !== null && same(details.provider, authenticatedProvider);
  });
  if (matchingAttestations.length !== 1) {
    throw new Error("missing or ambiguous live W10 transport attestation");
  }
  return normalizeWatcherL1Block(
    matchingAttestations[0]!,
    observation,
    budget.normalizationSession,
  );
};

const consumeFinalityStep = (budget: EvidenceBudget): void => {
  budget.finalitySteps += 1;
  if (
    budget.finalitySteps >
    WATCHER_STATE_QUEUE_INDEXER_BOUNDS.cumulativeFinalitySteps
  ) {
    throw new Error("cumulative W12 evidence budget exceeded");
  }
};

const hasAuthenticatedBlockSequence = (
  block: WatcherNormalizedL1Block,
): boolean =>
  block.transactions.every(
    (transaction, index) => transaction.transactionIndex === index.toString(),
  );

const blockParentHash = (block: WatcherNormalizedL1Block): string | null =>
  block.chainPoint.parentBlockHash;

const storeDigest = (store: WatcherDurableStore): string =>
  watcherDurableStoreBytesSha256(encodeWatcherDurableStore(store));

const sameRecordSet = <T>(left: readonly T[], right: readonly T[]): boolean =>
  same(left, right);

const nonProtocolRecordsMatch = (
  source: WatcherDurableStore,
  next: WatcherDurableStore,
): boolean =>
  sameRecordSet(source.daProofInputs, next.daProofInputs) &&
  sameRecordSet(source.reconstructedStates, next.reconstructedStates) &&
  sameRecordSet(source.decisions, next.decisions) &&
  sameRecordSet(source.faults, next.faults) &&
  sameRecordSet(source.submissions, next.submissions) &&
  sameRecordSet(source.confirmations, next.confirmations) &&
  sameRecordSet(source.retries, next.retries) &&
  sameRecordSet(source.deadlines, next.deadlines) &&
  sameRecordSet(source.correctionResults, next.correctionResults);

type StateQueueOwnedRole =
  | "state_queue"
  | "operator_directory"
  | "hub_oracle"
  | "proof_thread";

type StateQueueRoleClassification = "owned" | "foreign" | "invalid";

const stateQueueOwnedRole = (
  policy: WatcherStateQueueIndexerPolicy,
  durable: WatcherProtocolUtxo,
): StateQueueOwnedRole | null | undefined => {
  try {
    const output = CML.TransactionOutput.from_cbor_hex(durable.output.cborHex);
    if (output.to_canonical_cbor_hex() !== durable.output.cborHex) {
      return undefined;
    }
    const identities = outputAssets(output).filter(
      ({ policyId, quantity }) =>
        quantity === 1n &&
        [
          policy.stateQueuePolicyId,
          policy.schedulerPolicyId,
          policy.activeOperatorsPolicyId,
          policy.retiredOperatorsPolicyId,
          policy.fraudProofPolicyId,
          policy.daAttestationPolicyId,
          policy.hubOraclePolicyId,
        ].includes(policyId),
    );
    if (identities.length === 0) {
      return null;
    }
    if (identities.length !== 1) {
      return undefined;
    }
    const identity = identities[0]!;
    if (identity.policyId === policy.stateQueuePolicyId) {
      return "state_queue";
    }
    if (
      identity.policyId === policy.schedulerPolicyId ||
      identity.policyId === policy.activeOperatorsPolicyId ||
      identity.policyId === policy.retiredOperatorsPolicyId
    ) {
      return "operator_directory";
    }
    if (
      identity.policyId === policy.fraudProofPolicyId ||
      identity.policyId === policy.daAttestationPolicyId
    ) {
      return "proof_thread";
    }
    return "hub_oracle";
  } catch {
    return undefined;
  }
};

const classifyStateQueueRole = (
  policy: WatcherStateQueueIndexerPolicy,
  durable: WatcherProtocolUtxo,
): StateQueueRoleClassification => {
  const derived = stateQueueOwnedRole(policy, durable);
  return derived === undefined
    ? "invalid"
    : derived === null
      ? "foreign"
      : durable.role === derived
        ? "owned"
        : "invalid";
};

const stateQueueRolesMatchOutputs = (
  policy: WatcherStateQueueIndexerPolicy,
  store: WatcherDurableStore,
): boolean =>
  [...store.protocolUtxos, ...store.spentProtocolUtxos].every(
    (durable) => classifyStateQueueRole(policy, durable) !== "invalid",
  );

const stateQueueForeignRecordsPreserved = (
  policy: WatcherStateQueueIndexerPolicy,
  source: WatcherDurableStore,
  next: WatcherDurableStore,
): boolean => {
  const foreign = (records: readonly WatcherProtocolUtxo[]) =>
    records.filter(
      (durable) => classifyStateQueueRole(policy, durable) === "foreign",
    );
  return (
    same(foreign(source.protocolUtxos), foreign(next.protocolUtxos)) &&
    same(foreign(source.spentProtocolUtxos), foreign(next.spentProtocolUtxos))
  );
};

const stateQueueOwnedRecords = (
  policy: WatcherStateQueueIndexerPolicy,
  store: WatcherDurableStore,
  spent: boolean,
): readonly WatcherProtocolUtxo[] =>
  (spent ? store.spentProtocolUtxos : store.protocolUtxos).filter(
    (durable) => classifyStateQueueRole(policy, durable) === "owned",
  );

const storeTransitionMatches = (
  source: WatcherDurableStore,
  next: WatcherDurableStore,
  block: WatcherNormalizedL1Block,
  transaction: WatcherL1Transaction | null,
): boolean => {
  if (
    BigInt(next.revision) !== BigInt(source.revision) + 1n ||
    !sameMarker(source.deploymentMarker, next.deploymentMarker) ||
    !nonProtocolRecordsMatch(source, next) ||
    !source.l1Observations.every((entry) =>
      next.l1Observations.some((candidate) => same(candidate, entry)),
    ) ||
    !source.chainPoints.every((entry) =>
      next.chainPoints.some((candidate) => same(candidate, entry)),
    )
  ) {
    return false;
  }
  const encodedBlock = encodeWatcherNormalizedL1Block(block).toString("hex");
  const sourceHasObservation = source.l1Observations.some(
    (entry) =>
      entry.observationId === block.observationDigest &&
      entry.providerId === block.provider.providerId &&
      entry.chainPointId === block.chainPoint.chainPointId &&
      entry.payload.cborHex === encodedBlock,
  );
  const sourceHasPoint = source.chainPoints.some(
    (entry) =>
      entry.chainPointId === block.chainPoint.chainPointId &&
      entry.providerId === block.provider.providerId &&
      entry.blockHash === block.chainPoint.blockHash &&
      entry.slot === block.chainPoint.slot &&
      entry.blockNo === block.chainPoint.blockNo &&
      entry.depth === block.chainPoint.depth,
  );
  if (
    sourceHasObservation !== sourceHasPoint ||
    next.l1Observations.length !==
      source.l1Observations.length + (sourceHasObservation ? 0 : 1) ||
    next.chainPoints.length !==
      source.chainPoints.length + (sourceHasPoint ? 0 : 1)
  ) {
    return false;
  }
  const newObservations = next.l1Observations.filter(
    (entry) =>
      !source.l1Observations.some((candidate) => same(candidate, entry)),
  );
  const newPoints = next.chainPoints.filter(
    (entry) => !source.chainPoints.some((candidate) => same(candidate, entry)),
  );
  if (
    newObservations.length !== (sourceHasObservation ? 0 : 1) ||
    newPoints.length !== (sourceHasPoint ? 0 : 1) ||
    (!sourceHasObservation &&
      (newObservations[0]!.observationId !== block.observationDigest ||
        newObservations[0]!.providerId !== block.provider.providerId ||
        newObservations[0]!.chainPointId !== block.chainPoint.chainPointId ||
        newObservations[0]!.payload.cborHex !== encodedBlock)) ||
    (!sourceHasPoint &&
      (newPoints[0]!.chainPointId !== block.chainPoint.chainPointId ||
        newPoints[0]!.providerId !== block.provider.providerId ||
        newPoints[0]!.blockHash !== block.chainPoint.blockHash ||
        newPoints[0]!.slot !== block.chainPoint.slot ||
        newPoints[0]!.blockNo !== block.chainPoint.blockNo ||
        newPoints[0]!.depth !== block.chainPoint.depth))
  ) {
    return false;
  }
  if (transaction === null) {
    return (
      same(source.protocolUtxos, next.protocolUtxos) &&
      same(source.spentProtocolUtxos, next.spentProtocolUtxos)
    );
  }
  let expectedJournal: ReturnType<typeof journalWatcherProtocolUtxoTransition>;
  try {
    expectedJournal = journalWatcherProtocolUtxoTransition({
      sourceStore: source,
      nextChainPoints: next.chainPoints,
      nextProtocolUtxos: next.protocolUtxos,
      spentAtChainPointId: block.chainPoint.chainPointId,
    });
  } catch {
    return false;
  }
  if (
    !same(expectedJournal.protocolUtxos, next.protocolUtxos) ||
    !same(expectedJournal.spentProtocolUtxos, next.spentProtocolUtxos)
  ) {
    return false;
  }
  let body: CML.TransactionBody;
  try {
    body = CML.TransactionBody.from_cbor_hex(transaction.body.bytesHex);
  } catch {
    return false;
  }
  const inputs = new Set(bodyInputs(body));
  const retained = source.protocolUtxos.filter(
    ({ outRef }) => !inputs.has(outRef),
  );
  const removed = source.protocolUtxos.filter(({ outRef }) =>
    inputs.has(outRef),
  );
  const nextByOutRef = new Map(
    next.protocolUtxos.map((entry) => [entry.outRef, entry]),
  );
  const sourceOutRefs = new Set(
    source.protocolUtxos.map(({ outRef }) => outRef),
  );
  const created = next.protocolUtxos.filter(
    ({ outRef }) => !sourceOutRefs.has(outRef),
  );
  return (
    removed.length ===
      source.protocolUtxos.filter(({ outRef }) => !nextByOutRef.has(outRef))
        .length &&
    retained.every((entry) => same(nextByOutRef.get(entry.outRef), entry)) &&
    created.every((entry) => {
      const output = transaction.utxos.find(
        ({ outRef }) => outRef === entry.outRef,
      );
      return (
        output !== undefined &&
        entry.chainPointId === block.chainPoint.chainPointId &&
        entry.output.cborHex === output.output.bytesHex &&
        entry.output.sha256 ===
          sha256Bytes(Buffer.from(output.output.bytesHex, "hex"))
      );
    }) &&
    next.protocolUtxos.length === retained.length + created.length
  );
};

const verifyDeploymentAuthority = (
  policy: WatcherStateQueueIndexerPolicy,
  authority: WatcherStateQueuePublicContext["deploymentAuthority"],
): WatcherDeploymentIdentityPolicy | null => {
  try {
    const verified = verifyWatcherDeploymentIdentity({
      signedIdentity: authority.signedIdentity,
      policy: authority.policy,
      trustRoots: authority.trustRoots,
      durableMarker: policy.deploymentMarker,
    });
    const applied = authority.policy.appliedScriptHashes;
    const categories = Object.values(
      authority.policy.fraudProofCatalogue.categories,
    )
      .map(({ categoryId }) => categoryId)
      .sort();
    return same(verified, authority.result) &&
      verified.network === policy.network &&
      verified.releaseEvidenceDigest === policy.releaseEvidenceDigest &&
      verified.trustRootId === policy.deploymentTrustRootId &&
      sameMarker(verified.durableMarker, policy.deploymentMarker) &&
      applied.stateQueueMint === policy.stateQueuePolicyId &&
      applied.stateQueueSpend === policy.stateQueueSpendScriptHash &&
      applied.schedulerMint === policy.schedulerPolicyId &&
      applied.schedulerSpend === policy.schedulerSpendScriptHash &&
      applied.activeOperatorsMint === policy.activeOperatorsPolicyId &&
      applied.activeOperatorsSpend === policy.activeOperatorsSpendScriptHash &&
      applied.retiredOperatorsMint === policy.retiredOperatorsPolicyId &&
      applied.retiredOperatorsSpend ===
        policy.retiredOperatorsSpendScriptHash &&
      applied.fraudProofMint === policy.fraudProofPolicyId &&
      applied.fraudProofSpend === policy.fraudProofSpendScriptHash &&
      applied.daAttestationMint === policy.daAttestationPolicyId &&
      applied.daAttestationSpend === policy.daAttestationSpendScriptHash &&
      applied.hubOracleMint === policy.hubOraclePolicyId &&
      same(categories, policy.fraudProofCategoryIdsHex)
      ? authority.policy
      : null;
  } catch {
    return null;
  }
};

const verifyFinalityAuthority = (
  policy: WatcherStateQueueIndexerPolicy,
  block: WatcherNormalizedL1Block,
  value: unknown,
  budget: EvidenceBudget,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
): Readonly<{
  authority: WatcherStateQueueFinalityAuthority;
  result: WatcherFinalityResult;
}> | null => {
  try {
    consumeRawEvidence(budget, value, false);
  } catch {
    return null;
  }
  const record = exactRecord(value, [
    "policy",
    "lineage",
    "previousState",
    "observations",
    "consistency",
    "result",
  ]);
  const lineage =
    record === null
      ? null
      : exactArray(
          record.lineage,
          WATCHER_STATE_QUEUE_INDEXER_BOUNDS.finalityLineageSteps,
        );
  const currentObservations =
    record === null
      ? null
      : exactArray(
          record.observations,
          WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS.observations,
        );
  if (record === null || lineage === null || currentObservations === null) {
    return null;
  }
  try {
    const finalityPolicy = parseWatcherFinalityPolicy(record.policy);
    if (
      finalityPolicy === null ||
      finalityPolicy.network !== policy.network ||
      finalityPolicy.releaseEvidenceDigest !== policy.releaseEvidenceDigest ||
      !sameMarker(finalityPolicy.deploymentMarker, policy.deploymentMarker) ||
      finalityPolicy.confirmationDepth !== policy.requiredFinalityDepth
    ) {
      return null;
    }
    const replayedLineage: WatcherStateQueueFinalityLineageStep[] = [];
    let replayedState: unknown = null;
    for (const candidate of lineage) {
      consumeFinalityStep(budget);
      const step = exactRecord(candidate, [
        "observations",
        "consistency",
        "result",
      ]);
      const stepObservations =
        step === null
          ? null
          : exactArray(
              step.observations,
              WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS.observations,
            );
      if (step === null || stepObservations === null) {
        return null;
      }
      const normalized = stepObservations.map((observation) => {
        const authority = exactRecord(observation, [
          "authenticatedProvider",
          "l1Observation",
        ]);
        if (authority === null) {
          throw new Error("malformed W10 lineage authority");
        }
        return normalizeBudgetedL1Block(
          budget,
          authority.authenticatedProvider,
          authority.l1Observation,
          transportAttestations,
        );
      });
      const consistency = evaluateWatcherMultiProviderConsistency(
        watcherFinalityConfiguredSource(finalityPolicy),
        normalized,
        transportAttestations,
      );
      const result = evaluateWatcherFinality(
        finalityPolicy,
        replayedState,
        consistency,
      );
      if (
        !same(consistency, step.consistency) ||
        !same(result, step.result) ||
        result.state === null
      ) {
        return null;
      }
      replayedState = result.state;
      replayedLineage.push(
        Object.freeze({
          observations: stepObservations,
          consistency: step.consistency,
          result: step.result,
        }),
      );
    }
    if (!same(replayedState, record.previousState)) {
      return null;
    }
    consumeFinalityStep(budget);
    const authorityObservations = currentObservations.map((candidate) => {
      const authority = exactRecord(candidate, [
        "authenticatedProvider",
        "l1Observation",
      ]);
      if (authority === null) {
        throw new Error("malformed W10 authority");
      }
      return normalizeBudgetedL1Block(
        budget,
        authority.authenticatedProvider,
        authority.l1Observation,
        transportAttestations,
      );
    });
    const consistency = evaluateWatcherMultiProviderConsistency(
      watcherFinalityConfiguredSource(finalityPolicy),
      authorityObservations,
      transportAttestations,
    );
    const result = evaluateWatcherFinality(
      finalityPolicy,
      record.previousState,
      consistency,
    );
    const finalized = result.state?.finalized;
    const source = block.provider.source;
    const sourceMatchesFinality =
      source.sourceMode === finalityPolicy.sourceMode &&
      (source.sourceMode === "local_node"
        ? source.surface === "chain_sync" &&
          source.authorityNodeId === finalityPolicy.authorityNodeId &&
          block.provider.authentication.publicIdentitySha256 ===
            finalityPolicy.authorityGenesisIdentitySha256 &&
          consistency.chainAuthorityObservationDigest ===
            block.observationDigest
        : consistency.observationEvidenceDigests.includes(
            block.observationDigest,
          ));
    if (
      !same(consistency, record.consistency) ||
      !same(result, record.result) ||
      result.protocolDecision !== "finality_granted" ||
      !authorityObservations.some(
        (candidate) => candidate.observationDigest === block.observationDigest,
      ) ||
      !sourceMatchesFinality ||
      finalized?.pointDigest !== block.chainPoint.pointDigest ||
      finalized.blockContentDigest !== block.blockContentDigest ||
      BigInt(finalized.currentDepth) < BigInt(policy.requiredFinalityDepth)
    ) {
      return null;
    }
    return Object.freeze({
      authority: Object.freeze({
        policy: record.policy,
        lineage: Object.freeze(replayedLineage),
        previousState: record.previousState,
        observations: currentObservations,
        consistency: record.consistency,
        result: record.result,
      }),
      result,
    });
  } catch (_error) {
    return null;
  }
};

const parsePublicContext = (
  policy: WatcherStateQueueIndexerPolicy,
  value: unknown,
  observation: WatcherStateQueueObservation,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
  budget: EvidenceBudget = newEvidenceBudget(),
): VerifiedContext | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "authenticatedProvider",
    "l1Observation",
    "sourceDurableStore",
    "durableStore",
    "deploymentAuthority",
    "finalityAuthority",
    "originAuthorities",
    "rollbackAuthority",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_STATE_QUEUE_PUBLIC_CONTEXT_SCHEMA_VERSION
  ) {
    return null;
  }
  let block: WatcherNormalizedL1Block;
  let sourceStore: WatcherDurableStore;
  let store: WatcherDurableStore;
  const deploymentRecord =
    record === null
      ? null
      : exactRecord(record.deploymentAuthority, [
          "signedIdentity",
          "policy",
          "trustRoots",
          "result",
        ]);
  const deploymentTrustRoots =
    deploymentRecord === null
      ? null
      : exactArray(
          deploymentRecord.trustRoots,
          WATCHER_STATE_QUEUE_INDEXER_BOUNDS.deploymentTrustRoots,
        );
  const originAuthorityInputs =
    record === null
      ? null
      : exactArray(
          record.originAuthorities,
          WATCHER_STATE_QUEUE_INDEXER_BOUNDS.originAuthorities,
        );
  if (
    record === null ||
    record.authenticatedProvider === null ||
    record.l1Observation === null ||
    deploymentRecord === null ||
    deploymentTrustRoots === null ||
    originAuthorityInputs === null
  ) {
    return null;
  }
  try {
    block = normalizeBudgetedL1Block(
      budget,
      record.authenticatedProvider,
      record.l1Observation,
      transportAttestations,
    );
    sourceStore = parseWatcherDurableStore(record.sourceDurableStore);
    store = parseWatcherDurableStore(record.durableStore);
  } catch {
    return null;
  }
  const rollbackRecord =
    record.rollbackAuthority === null
      ? null
      : exactRecord(record.rollbackAuthority, ["result", "context"]);
  const rollbackAuthority =
    record.rollbackAuthority === null
      ? null
      : rollbackRecord === null
        ? undefined
        : (record.rollbackAuthority as NonNullable<
            WatcherStateQueuePublicContext["rollbackAuthority"]
          >);
  if (rollbackAuthority === undefined) {
    return null;
  }
  const deploymentAuthority = Object.freeze({
    signedIdentity: deploymentRecord.signedIdentity,
    policy: deploymentRecord.policy as WatcherDeploymentIdentityPolicy,
    trustRoots: deploymentTrustRoots as readonly WatcherDeploymentTrustRoot[],
    result: deploymentRecord.result as VerifiedWatcherDeploymentIdentity,
  });
  const deploymentPolicy = verifyDeploymentAuthority(
    policy,
    deploymentAuthority,
  );
  if (deploymentPolicy === null) {
    return null;
  }
  let finalityResult: WatcherFinalityResult | null = null;
  let finalityAuthority: WatcherStateQueueFinalityAuthority | null = null;
  if (observation.transitionKind === "rollback") {
    if (record.finalityAuthority !== null) {
      return null;
    }
  } else {
    const verifiedFinality = verifyFinalityAuthority(
      policy,
      block,
      record.finalityAuthority,
      budget,
      transportAttestations,
    );
    if (verifiedFinality === null) {
      return null;
    }
    finalityAuthority = verifiedFinality.authority;
    finalityResult = verifiedFinality.result;
  }
  const originAuthorities: WatcherStateQueueOriginAuthority[] = [];
  const originBlocks: WatcherNormalizedL1Block[] = [];
  for (const candidate of originAuthorityInputs) {
    const authority = exactRecord(candidate, [
      "authenticatedProvider",
      "l1Observation",
      "finalityAuthority",
    ]);
    if (authority === null) {
      return null;
    }
    let originBlock: WatcherNormalizedL1Block;
    try {
      originBlock = normalizeBudgetedL1Block(
        budget,
        authority.authenticatedProvider,
        authority.l1Observation,
        transportAttestations,
      );
    } catch {
      return null;
    }
    const verifiedOriginFinality = verifyFinalityAuthority(
      policy,
      originBlock,
      authority.finalityAuthority,
      budget,
      transportAttestations,
    );
    if (
      verifiedOriginFinality === null ||
      originBlock.observationDigest === block.observationDigest ||
      originBlocks.some(
        (prior) => prior.observationDigest === originBlock.observationDigest,
      )
    ) {
      return null;
    }
    originBlocks.push(originBlock);
    originAuthorities.push(
      Object.freeze({
        authenticatedProvider: authority.authenticatedProvider,
        l1Observation: authority.l1Observation,
        finalityAuthority: verifiedOriginFinality.authority,
      }),
    );
  }
  const encodedBlock = encodeWatcherNormalizedL1Block(block);
  const encodedStore = encodeWatcherDurableStore(store);
  const durableObservation = store.l1Observations.find(
    ({ observationId }) => observationId === block.observationDigest,
  );
  const durablePoint = store.chainPoints.find(
    ({ chainPointId }) => chainPointId === block.chainPoint.chainPointId,
  );
  const transactions = block.transactions
    .map((candidate, index) => ({ candidate, index }))
    .filter(
      ({ candidate }) =>
        candidate.isValid && candidate.txHash === observation.transactionHash,
    );
  const transaction =
    observation.transactionHash === null
      ? null
      : transactions.length === 1 &&
          transactions[0]!.index.toString() === observation.transactionIndex
        ? transactions[0]!.candidate
        : undefined;
  if (
    transaction === undefined ||
    !hasAuthenticatedBlockSequence(block) ||
    block.network !== observation.network ||
    block.chainPoint.chainPointId !== observation.chainPointId ||
    block.chainPoint.pointDigest !== observation.pointDigest ||
    block.chainPoint.blockHash !== observation.blockHash ||
    block.chainPoint.slot !== observation.slot ||
    block.chainPoint.blockNo !== observation.blockNo ||
    block.observationDigest !== observation.sourceObservationDigest ||
    sha256Bytes(encodedBlock) !== observation.publicInputDigest ||
    storeDigest(sourceStore) !== observation.sourceDurableStoreDigest ||
    sourceStore.revision !== observation.sourceDurableStoreRevision ||
    watcherDurableStoreBytesSha256(encodedStore) !==
      observation.durableStoreDigest ||
    store.revision !== observation.durableStoreRevision ||
    !sameMarker(sourceStore.deploymentMarker, policy.deploymentMarker) ||
    !sameMarker(store.deploymentMarker, policy.deploymentMarker) ||
    !stateQueueRolesMatchOutputs(policy, sourceStore) ||
    !stateQueueRolesMatchOutputs(policy, store) ||
    !stateQueueForeignRecordsPreserved(policy, sourceStore, store) ||
    durableObservation === undefined ||
    durableObservation.providerId !== block.provider.providerId ||
    durableObservation.chainPointId !== block.chainPoint.chainPointId ||
    durableObservation.payload.cborHex !== encodedBlock.toString("hex") ||
    durablePoint === undefined ||
    durablePoint.providerId !== block.provider.providerId ||
    durablePoint.blockHash !== block.chainPoint.blockHash ||
    durablePoint.slot !== block.chainPoint.slot ||
    durablePoint.blockNo !== block.chainPoint.blockNo ||
    durablePoint.depth !== block.chainPoint.depth ||
    (observation.transitionKind === "rollback") !==
      (rollbackAuthority !== null) ||
    (observation.transitionKind === "bootstrap"
      ? !storeTransitionMatches(sourceStore, store, block, transaction)
      : observation.transitionKind === "rollback"
        ? !same(sourceStore, rollbackAuthority?.context.sourceStore)
        : !storeTransitionMatches(sourceStore, store, block, transaction))
  ) {
    return null;
  }
  return Object.freeze({
    context: Object.freeze({
      schemaVersion: WATCHER_STATE_QUEUE_PUBLIC_CONTEXT_SCHEMA_VERSION,
      authenticatedProvider: record.authenticatedProvider,
      l1Observation: record.l1Observation,
      sourceDurableStore: sourceStore,
      durableStore: store,
      deploymentAuthority,
      finalityAuthority,
      originAuthorities: Object.freeze(originAuthorities),
      rollbackAuthority,
    }),
    block,
    sourceStore,
    store,
    transaction,
    deploymentPolicy,
    finalityResult,
    originBlocks: Object.freeze(originBlocks),
  });
};

type AuthenticatedOutputs = ReadonlyMap<
  string,
  Readonly<{
    outputHex: string;
    datumHex: string | null;
    chainPointId: string;
    pointDigest: string;
    blockNo: string;
    transactionHash: string;
    transactionIndex: number;
  }>
>;

const blockIsNotLaterThan = (
  candidate: WatcherNormalizedL1Block,
  cutoff: WatcherNormalizedL1Block,
): boolean => {
  const candidateBlock = BigInt(candidate.chainPoint.blockNo);
  const cutoffBlock = BigInt(cutoff.chainPoint.blockNo);
  return (
    candidateBlock < cutoffBlock ||
    (candidateBlock === cutoffBlock &&
      candidate.chainPoint.pointDigest === cutoff.chainPoint.pointDigest)
  );
};

const exactOriginObservations = (
  contexts: readonly VerifiedContext[],
): readonly WatcherNormalizedL1Block[] | null => {
  const authorized = new Map<string, WatcherNormalizedL1Block>();
  for (const context of contexts) {
    if (
      context.originBlocks.some(
        (origin) => !blockIsNotLaterThan(origin, context.block),
      )
    ) {
      return null;
    }
    for (const block of [context.block, ...context.originBlocks]) {
      const prior = authorized.get(block.observationDigest);
      if (
        prior !== undefined &&
        !encodeWatcherNormalizedL1Block(prior).equals(
          encodeWatcherNormalizedL1Block(block),
        )
      ) {
        return null;
      }
      authorized.set(block.observationDigest, block);
    }
  }
  for (const block of authorized.values()) {
    const encoded = encodeWatcherNormalizedL1Block(block).toString("hex");
    const durableMatch = contexts.some(({ store }) => {
      const durable = store.l1Observations.find(
        ({ observationId }) => observationId === block.observationDigest,
      );
      const point = store.chainPoints.find(
        ({ chainPointId }) => chainPointId === block.chainPoint.chainPointId,
      );
      return (
        durable !== undefined &&
        point !== undefined &&
        durable.payload.cborHex === encoded &&
        durable.providerId === block.provider.providerId &&
        durable.chainPointId === block.chainPoint.chainPointId &&
        point.providerId === block.provider.providerId &&
        point.blockHash === block.chainPoint.blockHash &&
        point.slot === block.chainPoint.slot &&
        point.blockNo === block.chainPoint.blockNo &&
        point.depth === block.chainPoint.depth
      );
    });
    if (!durableMatch) {
      return null;
    }
  }
  return Object.freeze([...authorized.values()]);
};

const authenticatedOutputs = (
  contexts: readonly VerifiedContext[],
): AuthenticatedOutputs | null => {
  const blocks = exactOriginObservations(contexts);
  if (blocks === null) {
    return null;
  }
  const outputs = new Map<
    string,
    Readonly<{
      outputHex: string;
      datumHex: string | null;
      chainPointId: string;
      pointDigest: string;
      blockNo: string;
      transactionHash: string;
      transactionIndex: number;
    }>
  >();
  for (const block of blocks) {
    for (const [
      transactionIndex,
      transaction,
    ] of block.transactions.entries()) {
      if (!transaction.isValid) {
        continue;
      }
      let body: CML.TransactionBody;
      try {
        body = CML.TransactionBody.from_cbor_hex(transaction.body.bytesHex);
      } catch {
        return null;
      }
      if (
        body.to_canonical_cbor_hex() !== transaction.body.bytesHex ||
        body.outputs().len() !== transaction.utxos.length
      ) {
        return null;
      }
      for (let index = 0; index < body.outputs().len(); index += 1) {
        const output = body.outputs().get(index);
        const evidence = transaction.utxos.find(
          ({ outputIndex }) => outputIndex === index.toString(),
        );
        const outputHex = output.to_canonical_cbor_hex();
        const datumHex =
          output.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null;
        if (
          evidence === undefined ||
          evidence.outRef !== `${transaction.txHash}#${index.toString()}` ||
          evidence.output.bytesHex !== outputHex ||
          (evidence.datum?.bytes.bytesHex ?? null) !== datumHex ||
          (datumHex !== null &&
            evidence.datum?.datumHash !==
              CML.hash_plutus_data(
                CML.PlutusData.from_cbor_hex(datumHex),
              ).to_hex())
        ) {
          return null;
        }
        const prior = outputs.get(evidence.outRef);
        if (
          prior !== undefined &&
          (prior.outputHex !== outputHex ||
            prior.datumHex !== datumHex ||
            prior.chainPointId !== block.chainPoint.chainPointId ||
            prior.transactionHash !== transaction.txHash ||
            prior.transactionIndex !== transactionIndex)
        ) {
          return null;
        }
        outputs.set(evidence.outRef, {
          outputHex,
          datumHex,
          chainPointId: block.chainPoint.chainPointId,
          pointDigest: block.chainPoint.pointDigest,
          blockNo: block.chainPoint.blockNo,
          transactionHash: transaction.txHash,
          transactionIndex,
        });
      }
    }
  }
  for (const context of contexts) {
    const selectedIndex =
      context.transaction === null
        ? null
        : context.block.transactions.indexOf(context.transaction);
    if (
      context.transaction !== null &&
      selectedIndex !== null &&
      selectedIndex < 0
    ) {
      return null;
    }
    for (const [store, sourceStore] of [
      [context.sourceStore, true],
      [context.store, false],
    ] as const) {
      for (const durable of [
        ...store.protocolUtxos,
        ...store.spentProtocolUtxos,
      ]) {
        const source = outputs.get(durable.outRef);
        const sourceBlock = source === undefined ? -1n : BigInt(source.blockNo);
        const cutoffBlock = BigInt(context.block.chainPoint.blockNo);
        const maximumTransactionIndex =
          selectedIndex === null
            ? null
            : sourceStore
              ? selectedIndex - 1
              : selectedIndex;
        if (
          source === undefined ||
          source.outputHex !== durable.output.cborHex ||
          source.chainPointId !== durable.chainPointId ||
          source.transactionHash !== durable.outRef.split("#")[0] ||
          (selectedIndex !== null &&
            (sourceBlock > cutoffBlock ||
              (sourceBlock === cutoffBlock &&
                (source.pointDigest !== context.block.chainPoint.pointDigest ||
                  (maximumTransactionIndex !== null &&
                    source.transactionIndex > maximumTransactionIndex)))))
        ) {
          return null;
        }
      }
    }
  }
  return outputs;
};

type DecodedTopology = Readonly<{
  snapshot: WatcherStateQueueSnapshot;
  outRefs: ReadonlyMap<string, string>;
  hubDatum: HubOracleDatum;
}>;

const outputAssets = (
  output: CML.TransactionOutput,
): readonly Readonly<{
  policyId: string;
  assetName: string;
  quantity: bigint;
}>[] => {
  const result: Array<{
    policyId: string;
    assetName: string;
    quantity: bigint;
  }> = [];
  const assets = valueToAssets(output.amount());
  for (const [unit, quantity] of Object.entries(assets)) {
    if (unit === "lovelace") {
      continue;
    }
    result.push({
      policyId: unit.slice(0, 56),
      assetName: unit.slice(56),
      quantity,
    });
  }
  return result;
};

const traverse = <
  T extends Readonly<{ key: string | null; next: string | null }>,
>(
  root: T,
  nodes: readonly T[],
): readonly T[] | null => {
  const byKey = new Map(nodes.map((node) => [node.key, node]));
  const ordered: T[] = [];
  const seen = new Set<string>();
  let next = root.next;
  while (next !== null) {
    const node = byKey.get(next);
    if (node === undefined || seen.has(next)) {
      return null;
    }
    seen.add(next);
    ordered.push(node);
    next = node.next;
  }
  return seen.size === nodes.length ? Object.freeze(ordered) : null;
};

type RawLinked = Readonly<{
  key: string | null;
  next: string | null;
  raw: unknown;
  datumSha256: string;
  outRef: string;
}>;

const rawLinked = (
  datumHex: string,
  assetName: string,
  rootAssetName: string,
  prefix: string,
  datumSha256: string,
  outRef: string,
): RawLinked | null => {
  const linked = dataRoundTrip<LinkedListDatum>(datumHex, LinkedListDatum);
  const root = assetName === rootAssetName;
  if (
    linked === null ||
    root !== "Root" in linked.data ||
    (!root &&
      (!assetName.startsWith(prefix) ||
        !isHex28(assetName.slice(prefix.length)))) ||
    (linked.link !== null && !isHex28(linked.link))
  ) {
    return null;
  }
  const raw =
    "Root" in linked.data ? linked.data.Root.data : linked.data.Node.data;
  return Object.freeze({
    key: root ? null : assetName.slice(prefix.length),
    next: linked.link,
    raw,
    datumSha256,
    outRef,
  });
};

const castRaw = <T>(value: unknown, schema: unknown): T | null => {
  try {
    const decoded = Data.castFrom(value as never, schema as never) as T;
    Data.to(decoded as never, schema as never);
    return decoded;
  } catch {
    return null;
  }
};

const plutusCredential = (
  value: unknown,
): Readonly<
  | { PublicKeyCredential: readonly [string] }
  | { ScriptCredential: readonly [string] }
> | null => {
  const record =
    typeof value === "object" && value !== null && !Array.isArray(value)
      ? (value as Record<string, unknown>)
      : null;
  const pubKey =
    record?.PubKey !== undefined
      ? (record.PubKey as { hash?: unknown }).hash
      : null;
  const script =
    record?.Script !== undefined
      ? (record.Script as { hash?: unknown }).hash
      : null;
  return isHex28(pubKey)
    ? { PublicKeyCredential: [pubKey] }
    : isHex28(script)
      ? { ScriptCredential: [script] }
      : null;
};

const hubAddressMatches = (value: unknown, addressHex: string): boolean => {
  try {
    const address = CML.Address.from_hex(addressHex);
    const payment = plutusCredential(address.payment_cred()?.to_js_value());
    const stakeValue = address.staking_cred()?.to_js_value();
    const stake =
      stakeValue === undefined ? null : plutusCredential(stakeValue);
    if (payment === null || (stakeValue !== undefined && stake === null)) {
      return false;
    }
    return (
      Data.to(value as never, AddressSchema as never) ===
      Data.to(
        {
          paymentCredential: payment,
          stakeCredential: stake === null ? null : { Inline: [stake] },
        } as never,
        AddressSchema as never,
      )
    );
  } catch {
    return false;
  }
};

const hubDatumMatchesPolicy = (
  datum: HubOracleDatum,
  policy: WatcherStateQueueIndexerPolicy,
): boolean =>
  datum.active_operators === policy.activeOperatorsPolicyId &&
  datum.retired_operators === policy.retiredOperatorsPolicyId &&
  datum.scheduler === policy.schedulerPolicyId &&
  datum.state_queue === policy.stateQueuePolicyId &&
  datum.fraud_proof === policy.fraudProofPolicyId &&
  hubAddressMatches(
    datum.active_operators_addr,
    policy.activeOperatorsAddressHex,
  ) &&
  hubAddressMatches(
    datum.retired_operators_addr,
    policy.retiredOperatorsAddressHex,
  ) &&
  hubAddressMatches(datum.scheduler_addr, policy.schedulerAddressHex) &&
  hubAddressMatches(datum.state_queue_addr, policy.stateQueueAddressHex) &&
  hubAddressMatches(datum.fraud_proof_addr, policy.fraudProofAddressHex);

const reconstructTopology = (
  policy: WatcherStateQueueIndexerPolicy,
  store: WatcherDurableStore,
  sources: AuthenticatedOutputs,
): DecodedTopology | null => {
  const queue: RawLinked[] = [];
  const active: RawLinked[] = [];
  const retired: RawLinked[] = [];
  let scheduler: Readonly<{
    datum: SchedulerDatum;
    digest: string;
    outRef: string;
  }> | null = null;
  let hubOracle: Readonly<{ datum: HubOracleDatum; outRef: string }> | null =
    null;
  const outRefs = new Map<string, string>();
  const relevantPolicies = new Set([
    policy.stateQueuePolicyId,
    policy.schedulerPolicyId,
    policy.activeOperatorsPolicyId,
    policy.retiredOperatorsPolicyId,
    policy.fraudProofPolicyId,
    policy.daAttestationPolicyId,
    policy.hubOraclePolicyId,
    policy.hubOraclePolicyId,
  ]);
  for (const durable of store.protocolUtxos) {
    let output: CML.TransactionOutput;
    try {
      output = CML.TransactionOutput.from_cbor_hex(durable.output.cborHex);
    } catch {
      return null;
    }
    if (output.to_canonical_cbor_hex() !== durable.output.cborHex) {
      return null;
    }
    const assets = outputAssets(output);
    const identities = assets.filter(({ policyId }) =>
      relevantPolicies.has(policyId),
    );
    if (identities.length === 0) {
      continue;
    }
    if (
      identities.length !== 1 ||
      identities[0]!.quantity !== 1n ||
      output.script_ref() !== undefined
    ) {
      return null;
    }
    const source = sources.get(durable.outRef);
    const datum = output.datum()?.as_datum();
    const datumHex = datum?.to_canonical_cbor_hex() ?? null;
    if (
      source === undefined ||
      source.outputHex !== durable.output.cborHex ||
      source.datumHex !== datumHex ||
      source.chainPointId !== durable.chainPointId ||
      durable.outRef.split("#")[0] !== source.transactionHash ||
      datum === undefined ||
      datum.to_cbor_hex() !== datumHex
    ) {
      return null;
    }
    const digest = sha256Bytes(Buffer.from(datumHex, "hex"));
    const identity = identities[0]!;
    const addressHex = output.address().to_hex();
    if (identity.policyId === policy.stateQueuePolicyId) {
      if (
        durable.role !== "state_queue" ||
        addressHex !== policy.stateQueueAddressHex ||
        output.address().payment_cred()?.as_script()?.to_hex() !==
          policy.stateQueueSpendScriptHash
      ) {
        return null;
      }
      const decoded = rawLinked(
        datumHex,
        identity.assetName,
        policy.stateQueueRootAssetNameHex,
        policy.stateQueueNodeAssetPrefixHex,
        digest,
        durable.outRef,
      );
      if (decoded === null) {
        return null;
      }
      queue.push(decoded);
      outRefs.set(
        decoded.key === null ? "queue:root" : `queue:${decoded.key}`,
        durable.outRef,
      );
    } else if (identity.policyId === policy.schedulerPolicyId) {
      if (
        durable.role !== "operator_directory" ||
        identity.assetName !== policy.schedulerAssetNameHex ||
        addressHex !== policy.schedulerAddressHex ||
        output.address().payment_cred()?.as_script()?.to_hex() !==
          policy.schedulerSpendScriptHash ||
        scheduler !== null
      ) {
        return null;
      }
      const decoded = dataRoundTrip<SchedulerDatum>(datumHex, SchedulerDatum);
      if (decoded === null) {
        return null;
      }
      scheduler = Object.freeze({
        datum: decoded,
        digest,
        outRef: durable.outRef,
      });
      outRefs.set("scheduler", durable.outRef);
    } else if (identity.policyId === policy.activeOperatorsPolicyId) {
      if (
        durable.role !== "operator_directory" ||
        addressHex !== policy.activeOperatorsAddressHex ||
        output.address().payment_cred()?.as_script()?.to_hex() !==
          policy.activeOperatorsSpendScriptHash
      ) {
        return null;
      }
      const decoded = rawLinked(
        datumHex,
        identity.assetName,
        ACTIVE_OPERATORS_ROOT_ASSET_NAME,
        policy.activeOperatorAssetPrefixHex,
        digest,
        durable.outRef,
      );
      if (decoded === null) {
        return null;
      }
      active.push(decoded);
      outRefs.set(
        decoded.key === null ? "active:root" : `active:${decoded.key}`,
        durable.outRef,
      );
    } else if (identity.policyId === policy.retiredOperatorsPolicyId) {
      if (
        durable.role !== "operator_directory" ||
        addressHex !== policy.retiredOperatorsAddressHex ||
        output.address().payment_cred()?.as_script()?.to_hex() !==
          policy.retiredOperatorsSpendScriptHash
      ) {
        return null;
      }
      const decoded = rawLinked(
        datumHex,
        identity.assetName,
        RETIRED_OPERATORS_ROOT_ASSET_NAME,
        policy.retiredOperatorAssetPrefixHex,
        digest,
        durable.outRef,
      );
      if (decoded === null) {
        return null;
      }
      retired.push(decoded);
      outRefs.set(
        decoded.key === null ? "retired:root" : `retired:${decoded.key}`,
        durable.outRef,
      );
    } else if (identity.policyId === policy.daAttestationPolicyId) {
      const headerHash = identity.assetName.slice(
        policy.daAttestationAssetPrefixHex.length,
      );
      const decoded = dataRoundTrip<{
        header_hash: string;
      }>(datumHex, DaAttestationDatum);
      if (
        durable.role !== "proof_thread" ||
        !identity.assetName.startsWith(policy.daAttestationAssetPrefixHex) ||
        !isHex28(headerHash) ||
        addressHex !== policy.daAttestationAddressHex ||
        output.address().payment_cred()?.as_script()?.to_hex() !==
          policy.daAttestationSpendScriptHash ||
        decoded === null ||
        decoded.header_hash !== headerHash ||
        outRefs.has(`da_attestation:${headerHash}`)
      ) {
        return null;
      }
      outRefs.set(`da_attestation:${headerHash}`, durable.outRef);
    } else if (identity.policyId === policy.fraudProofPolicyId) {
      const categoryId = identity.assetName.slice(0, 8);
      const headerHash = identity.assetName.slice(8);
      const proofKey = `proof:${headerHash}`;
      if (
        durable.role !== "proof_thread" ||
        identity.assetName.length !== 64 ||
        !policy.fraudProofCategoryIdsHex.includes(categoryId) ||
        !isHex28(headerHash) ||
        addressHex !== policy.fraudProofAddressHex ||
        output.address().payment_cred()?.as_script()?.to_hex() !==
          policy.fraudProofSpendScriptHash ||
        dataRoundTrip<unknown>(datumHex, FraudProofTokenDatum) === null ||
        outRefs.has(proofKey)
      ) {
        return null;
      }
      outRefs.set(proofKey, durable.outRef);
    } else {
      if (
        durable.role !== "hub_oracle" ||
        identity.assetName !== policy.hubOracleAssetNameHex ||
        addressHex !== policy.hubOracleAddressHex ||
        output.address().payment_cred()?.as_script()?.to_hex() !==
          policy.hubOraclePolicyId ||
        hubOracle !== null
      ) {
        return null;
      }
      const decoded = dataRoundTrip<HubOracleDatum>(datumHex, HubOracleDatum);
      if (decoded === null || !hubDatumMatchesPolicy(decoded, policy)) {
        return null;
      }
      hubOracle = Object.freeze({ datum: decoded, outRef: durable.outRef });
      outRefs.set("hub_oracle", durable.outRef);
    }
  }
  const queueRoot = queue.filter(({ key }) => key === null);
  const activeRoot = active.filter(({ key }) => key === null);
  const retiredRoot = retired.filter(({ key }) => key === null);
  if (
    queueRoot.length !== 1 ||
    activeRoot.length !== 1 ||
    retiredRoot.length !== 1 ||
    scheduler === null ||
    hubOracle === null
  ) {
    return null;
  }
  const queueOrdered = traverse(
    queueRoot[0]!,
    queue.filter(({ key }) => key !== null),
  );
  const activeOrdered = traverse(
    activeRoot[0]!,
    active.filter(({ key }) => key !== null),
  );
  const retiredOrdered = traverse(
    retiredRoot[0]!,
    retired.filter(({ key }) => key !== null),
  );
  if (
    queueOrdered === null ||
    activeOrdered === null ||
    retiredOrdered === null
  ) {
    return null;
  }
  const confirmed = castRaw<ConfirmedState>(queueRoot[0]!.raw, ConfirmedState);
  if (confirmed === null) {
    return null;
  }
  const queueViews: WatcherStateQueueHeader[] = [];
  for (const node of queueOrdered) {
    const decoded = castRaw<StateQueueNode>(node.raw, StateQueueNode);
    if (
      decoded === null ||
      node.key !== headerHashFromCbor(Data.to(decoded.header, Header))
    ) {
      return null;
    }
    queueViews.push(
      Object.freeze({
        ...headerView(decoded.header, node.next, node.datumSha256),
        daAttestationPolicyId:
          decoded.da_attestation === "Unattested"
            ? null
            : policy.daAttestationPolicyId,
      }),
    );
  }
  const activeViews: WatcherIndexedActiveOperator[] = [];
  for (const node of activeOrdered) {
    const decoded = castRaw<ActiveOperatorDatum>(node.raw, ActiveOperatorDatum);
    if (decoded === null || node.key === null) {
      return null;
    }
    activeViews.push(
      Object.freeze({
        operatorVkey: node.key,
        nextOperatorVkey: node.next,
        bondUnlockTime: decoded.bond_unlock_time?.toString() ?? null,
        inactivityStrikes: decoded.inactivity_strikes.toString(),
        datumSha256: node.datumSha256,
      }),
    );
  }
  const retiredViews: WatcherIndexedRetiredOperator[] = [];
  for (const node of retiredOrdered) {
    const decoded = castRaw<RetiredOperatorDatum>(
      node.raw,
      RetiredOperatorDatum,
    );
    if (decoded === null || node.key === null) {
      return null;
    }
    retiredViews.push(
      Object.freeze({
        operatorVkey: node.key,
        nextOperatorVkey: node.next,
        bondUnlockTime: decoded.bond_unlock_time?.toString() ?? null,
        datumSha256: node.datumSha256,
      }),
    );
  }
  const schedulerView: WatcherIndexedScheduler =
    scheduler.datum === "NoActiveOperators"
      ? Object.freeze({
          operatorVkey: null,
          shiftStartTime: null,
          datumSha256: scheduler.digest,
        })
      : Object.freeze({
          operatorVkey: scheduler.datum.ActiveOperator.operator,
          shiftStartTime: scheduler.datum.ActiveOperator.start_time.toString(),
          datumSha256: scheduler.digest,
        });
  const provisional = {
    confirmedState: Object.freeze({
      headerHash: confirmed.headerHash,
      prevHeaderHash: confirmed.prevHeaderHash,
      utxosRoot: confirmed.utxoRoot,
      startTime: confirmed.startTime.toString(),
      endTime: confirmed.endTime.toString(),
      protocolVersion: confirmed.protocolVersion.toString(),
      datumSha256: queueRoot[0]!.datumSha256,
    }),
    queue: Object.freeze(queueViews),
    scheduler: schedulerView,
    activeOperators: Object.freeze(activeViews),
    retiredOperators: Object.freeze(retiredViews),
    quarantinedFromHeaderHash: null as string | null,
  };
  const breaks = queueViews
    .map((header, index) => {
      const previous = queueViews[index - 1];
      return index > 0 &&
        (header.prevHeaderHash !== previous?.headerHash ||
          header.prevUtxosRoot !== previous.utxosRoot ||
          BigInt(header.startTime) !== BigInt(previous.endTime))
        ? (previous?.headerHash ?? null)
        : null;
    })
    .filter((entry): entry is string => entry !== null);
  if (breaks.length > 1) {
    return null;
  }
  const snapshot = makeWatcherStateQueueSnapshot({
    ...provisional,
    quarantinedFromHeaderHash: breaks[0] ?? null,
  });
  return snapshot === null
    ? null
    : Object.freeze({ snapshot, outRefs, hubDatum: hubOracle.datum });
};

/**
 * Classifies a state change from decoded, node-derived output/datum bytes.
 *
 * This is deliberately topology-only. Cardano consensus and the deployed
 * validators have already established transaction validity; W14 must not
 * replay validator redeemers or independently prove those rules.
 */
const classifyObservedTransition = (
  previous: WatcherStateQueueSnapshot,
  next: WatcherStateQueueSnapshot,
): Exclude<
  WatcherStateQueueTransitionKind,
  "bootstrap" | "rollback"
> | null => {
  const queueHashes = (snapshot: WatcherStateQueueSnapshot) =>
    snapshot.queue.map(({ headerHash }) => headerHash);
  const operatorKeys = (
    values: readonly Readonly<{ operatorVkey: string }>[],
  ) => values.map(({ operatorVkey }) => operatorVkey);
  const stableDirectories =
    same(previous.scheduler, next.scheduler) &&
    same(
      operatorKeys(previous.activeOperators),
      operatorKeys(next.activeOperators),
    ) &&
    same(
      operatorKeys(previous.retiredOperators),
      operatorKeys(next.retiredOperators),
    );
  const confirmedIdentityStable =
    previous.confirmedState.headerHash === next.confirmedState.headerHash &&
    previous.confirmedState.prevHeaderHash ===
      next.confirmedState.prevHeaderHash &&
    previous.confirmedState.utxosRoot === next.confirmedState.utxosRoot &&
    previous.confirmedState.startTime === next.confirmedState.startTime &&
    previous.confirmedState.endTime === next.confirmedState.endTime &&
    previous.confirmedState.protocolVersion ===
      next.confirmedState.protocolVersion;
  const previousHashes = queueHashes(previous);
  const nextHashes = queueHashes(next);

  if (
    confirmedIdentityStable &&
    stableDirectories &&
    nextHashes.length === previousHashes.length + 1 &&
    same(nextHashes.slice(0, -1), previousHashes)
  ) {
    return "append";
  }
  if (
    confirmedIdentityStable &&
    stableDirectories &&
    same(nextHashes, previousHashes)
  ) {
    const changedDa = previous.queue.filter(
      (header, index) =>
        header.daAttestationPolicyId !==
        next.queue[index]?.daAttestationPolicyId,
    );
    return changedDa.length === 1 ? "attach_da" : null;
  }
  if (
    stableDirectories &&
    previous.queue[0]?.headerHash === next.confirmedState.headerHash &&
    same(nextHashes, previousHashes.slice(1))
  ) {
    return "merge";
  }
  if (
    confirmedIdentityStable &&
    nextHashes.length < previousHashes.length &&
    nextHashes.every((hash) => previousHashes.includes(hash))
  ) {
    return "remove_fraudulent";
  }
  return null;
};

const bodyInputs = (body: CML.TransactionBody): readonly string[] => {
  const values: string[] = [];
  const inputs = body.inputs();
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    values.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return Object.freeze(values);
};

const bodyMintPolicyIds = (body: CML.TransactionBody): readonly string[] => {
  const mint = body.mint();
  if (mint === undefined) {
    return Object.freeze([]);
  }
  const keys = mint.keys();
  const policyIds: string[] = [];
  for (let index = 0; index < keys.len(); index += 1) {
    policyIds.push(keys.get(index).to_hex());
  }
  return Object.freeze(policyIds.sort());
};

const queueTransitionNodes = (
  topology: DecodedTopology,
): readonly StateQueueTransitionNode[] | null => {
  const rootOutRef = topology.outRefs.get("queue:root");
  if (rootOutRef === undefined) {
    return null;
  }
  const nodes: StateQueueTransitionNode[] = [
    { headerHash: null, outRef: rootOutRef },
  ];
  for (const header of topology.snapshot.queue) {
    const outRef = topology.outRefs.get(`queue:${header.headerHash}`);
    if (outRef === undefined) {
      return null;
    }
    nodes.push({ headerHash: header.headerHash, outRef });
  }
  return Object.freeze(nodes);
};

const deriveAuthenticatedTimeoutCorrectionTransition = (
  policy: WatcherStateQueueIndexerPolicy,
  verified: VerifiedContext,
  previous: DecodedTopology,
  next: DecodedTopology,
): StateQueueCorrectionTransition | null => {
  if (verified.transaction === null || verified.finalityResult === null) {
    return null;
  }
  const previousQueue = queueTransitionNodes(previous);
  const nextQueue = queueTransitionNodes(next);
  const finalized = verified.finalityResult.state?.finalized;
  if (
    previousQueue === null ||
    nextQueue === null ||
    finalized === null ||
    finalized === undefined
  ) {
    return null;
  }
  let body: CML.TransactionBody;
  try {
    body = CML.TransactionBody.from_cbor_hex(
      verified.transaction.body.bytesHex,
    );
  } catch {
    return null;
  }
  return deriveStateQueueCorrectionTransition({
    deploymentIdentityDigest: policy.deploymentMarker.manifestId,
    stateQueuePolicyId: policy.stateQueuePolicyId,
    transactionHash: verified.transaction.txHash,
    blockHash: verified.block.chainPoint.blockHash,
    slot: verified.block.chainPoint.slot,
    blockNo: verified.block.chainPoint.blockNo,
    chainPointId: verified.block.chainPoint.chainPointId,
    finalityDepth: finalized.currentDepth,
    mintPolicyIds: bodyMintPolicyIds(body),
    redeemers: verified.transaction.redeemers.map((redeemer) => ({
      purpose: redeemer.purpose,
      index: redeemer.index,
      cborHex: redeemer.bytes.bytesHex,
    })),
    spentInputOutRefs: bodyInputs(body),
    previousQueue,
    nextQueue,
  });
};

const entryWithoutDigest = (
  value: Omit<WatcherStateQueueHistoryEntry, "entryDigest">,
) => ({ ...value });

const auditWithoutDigest = (
  value: Omit<WatcherStateQueueAuditEntry, "auditDigest">,
) => ({ ...value });

const stateWithoutDigest = (
  value: Omit<WatcherStateQueueIndexerState, "stateDigest">,
) => ({ ...value });

const resultWithoutDigest = (
  value: Omit<WatcherStateQueueIndexerResult, "resultDigest">,
) => ({ ...value });

const makeEntry = (
  observation: WatcherStateQueueObservation,
  verified: VerifiedContext,
  snapshot: WatcherStateQueueSnapshot,
  correctionTransition: StateQueueCorrectionTransition | null,
  rollbackResult: WatcherStateQueueRollbackResult | null,
  priorActiveEntryDigest: string | null,
): WatcherStateQueueHistoryEntry => {
  const canonical = Object.freeze({
    predecessorStateDigest: observation.predecessorStateDigest,
    priorActiveEntryDigest,
    chainPointId: verified.block.chainPoint.chainPointId,
    pointDigest: observation.pointDigest,
    transactionHash: observation.transactionHash,
    transactionIndex: observation.transactionIndex,
    publicInputDigest: observation.publicInputDigest,
    transitionKind: observation.transitionKind,
    correctionTransition,
    snapshot,
    observation,
    publicContext: verified.context,
    rollbackResult,
  });
  return Object.freeze({
    ...canonical,
    entryDigest: sha256Canonical(entryWithoutDigest(canonical)),
  });
};

const makeAudit = (
  status: WatcherStateQueueAuditEntry["status"],
  entry: WatcherStateQueueHistoryEntry,
): WatcherStateQueueAuditEntry => {
  const canonical = Object.freeze({ status, entry });
  return Object.freeze({
    ...canonical,
    auditDigest: sha256Canonical(auditWithoutDigest(canonical)),
  });
};

const auditGroups = (
  entries: readonly WatcherStateQueueAuditEntry[],
): readonly (readonly WatcherStateQueueAuditEntry[])[] | null => {
  const groups: WatcherStateQueueAuditEntry[][] = [];
  let current: WatcherStateQueueAuditEntry[] = [];
  for (const entry of entries) {
    if (
      (entry.status === "orphaned" &&
        current.some(({ status }) => status === "rollback")) ||
      (entry.status === "rollback" &&
        current.some(({ status }) => status === "rollback"))
    ) {
      return null;
    }
    current.push(entry);
    if (entry.status === "rollback") {
      groups.push(current);
      current = [];
    }
  }
  return current.length === 0 ? Object.freeze(groups) : null;
};

const pruneAuditGroups = (
  entries: readonly WatcherStateQueueAuditEntry[],
): readonly WatcherStateQueueAuditEntry[] | null => {
  const groups = auditGroups(entries);
  if (groups === null) {
    return null;
  }
  const retained: WatcherStateQueueAuditEntry[][] = [];
  let count = 0;
  for (const group of [...groups].reverse()) {
    if (
      group.length > WATCHER_STATE_QUEUE_INDEXER_BOUNDS.auditEntries ||
      count + group.length > WATCHER_STATE_QUEUE_INDEXER_BOUNDS.auditEntries
    ) {
      break;
    }
    retained.unshift([...group]);
    count += group.length;
  }
  return Object.freeze(retained.flat());
};

const makeState = (
  policy: WatcherStateQueueIndexerPolicy,
  observation: WatcherStateQueueObservation,
  history: readonly WatcherStateQueueHistoryEntry[],
  auditHistory: readonly WatcherStateQueueAuditEntry[],
): WatcherStateQueueIndexerState => {
  const snapshot = history.at(-1)?.snapshot;
  if (snapshot === undefined) {
    throw new Error("state queue indexer state requires history");
  }
  const canonical = Object.freeze({
    schemaVersion: WATCHER_STATE_QUEUE_INDEXER_STATE_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    pointDigest: observation.pointDigest,
    transactionHash: observation.transactionHash,
    transactionIndex: observation.transactionIndex,
    publicInputDigest: observation.publicInputDigest,
    durableStoreDigest: observation.durableStoreDigest,
    snapshot,
    history: Object.freeze(history),
    auditHistory: Object.freeze(auditHistory),
  });
  return immutableWireValue({
    ...canonical,
    stateDigest: sha256Canonical(stateWithoutDigest(canonical)),
  });
};

const makeResult = (
  value: Omit<WatcherStateQueueIndexerResult, "schemaVersion" | "resultDigest">,
): WatcherStateQueueIndexerResult => {
  const canonical = Object.freeze({
    schemaVersion: WATCHER_STATE_QUEUE_INDEXER_RESULT_SCHEMA_VERSION,
    ...value,
  });
  return immutableWireValue({
    ...canonical,
    resultDigest: sha256Canonical(resultWithoutDigest(canonical)),
  });
};

const currentStateEntry = (
  state: WatcherStateQueueIndexerState,
): WatcherStateQueueHistoryEntry => {
  const rollback = state.auditHistory.at(-1);
  return rollback?.status === "rollback" &&
    rollback.entry.pointDigest === state.pointDigest &&
    rollback.entry.transactionHash === state.transactionHash &&
    rollback.entry.publicInputDigest === state.publicInputDigest &&
    rollback.entry.observation.durableStoreDigest === state.durableStoreDigest
    ? rollback.entry
    : state.history.at(-1)!;
};

const rejected = (
  reasonCode: WatcherStateQueueIndexerReasonCode,
  alertCode: WatcherStateQueueIndexerAlertCode = "watcher_state_queue_input_rejected",
): WatcherStateQueueIndexerResult =>
  makeResult({
    action: "reject",
    protocolDecision: "hold",
    reasonCodes: Object.freeze([reasonCode]),
    alertCodes: Object.freeze([alertCode]),
    state: null,
  });

const rollbackSourceExtends = (
  policy: WatcherStateQueueIndexerPolicy,
  prior: WatcherDurableStore,
  sourceInput: unknown,
  recoverableOwnedOutRefs: ReadonlySet<string> | null = null,
): WatcherDurableStore | null => {
  let source: WatcherDurableStore;
  try {
    source = parseWatcherDurableStore(sourceInput);
  } catch {
    return null;
  }
  const observations = new Map(
    source.l1Observations.map((entry) => [entry.observationId, entry]),
  );
  const points = new Map(
    source.chainPoints.map((entry) => [entry.chainPointId, entry]),
  );
  const retainsExactRecords = <T>(
    priorRecords: readonly T[],
    sourceRecords: readonly T[],
    keyOf: (record: T) => string,
  ): boolean => {
    const byKey = new Map(sourceRecords.map((entry) => [keyOf(entry), entry]));
    return priorRecords.every((entry) => same(byKey.get(keyOf(entry)), entry));
  };
  const classified = (
    store: WatcherDurableStore,
    classification: Exclude<StateQueueRoleClassification, "invalid">,
    spent: boolean,
  ) =>
    (spent ? store.spentProtocolUtxos : store.protocolUtxos).filter(
      (entry) => classifyStateQueueRole(policy, entry) === classification,
    );
  const sourceOwned = (spent: boolean) =>
    classified(source, "owned", spent).filter(
      ({ outRef }) => !recoverableOwnedOutRefs?.has(outRef),
    );
  const ownedMatches = (spent: boolean) =>
    same(
      classified(source, "owned", spent),
      classified(prior, "owned", spent),
    ) || same(sourceOwned(spent), classified(prior, "owned", spent));
  return sameMarker(source.deploymentMarker, prior.deploymentMarker) &&
    ownedMatches(false) &&
    ownedMatches(true) &&
    retainsExactRecords(
      classified(prior, "foreign", false),
      classified(source, "foreign", false),
      (entry) => entry.outRef,
    ) &&
    retainsExactRecords(
      classified(prior, "foreign", true),
      classified(source, "foreign", true),
      (entry) => entry.outRef,
    ) &&
    same(source.daProofInputs, prior.daProofInputs) &&
    same(source.reconstructedStates, prior.reconstructedStates) &&
    same(source.decisions, prior.decisions) &&
    same(source.faults, prior.faults) &&
    same(source.submissions, prior.submissions) &&
    same(source.confirmations, prior.confirmations) &&
    same(source.retries, prior.retries) &&
    same(source.deadlines, prior.deadlines) &&
    same(source.correctionResults, prior.correctionResults) &&
    prior.l1Observations.every((entry) =>
      same(observations.get(entry.observationId), entry),
    ) &&
    prior.chainPoints.every((entry) =>
      same(points.get(entry.chainPointId), entry),
    )
    ? source
    : null;
};

const isPostFinalityRecoveryResult = (
  result: WatcherStateQueueRollbackResult,
): result is WatcherPostFinalityRecoveryResult =>
  result.action === "rewind_and_replay" ||
  result.action === "duplicate_recovery";

const verifiedRollbackBinding = (
  policy: WatcherStateQueueIndexerPolicy,
  priorStore: WatcherDurableStore,
  verified: VerifiedContext,
  observation: WatcherStateQueueObservation,
  persistedResult: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
): WatcherStateQueueRollbackResult | null => {
  const authority = verified.context.rollbackAuthority;
  if (authority === null) {
    return null;
  }
  const rollbackResult = parseWatcherRollbackResult(authority.result, {
    ...(authority.context as WatcherRollbackVerificationContext),
    transportAttestations,
  });
  const recoveryResult =
    rollbackResult === null
      ? parseWatcherPostFinalityRecoveryResult(authority.result, {
          ...(authority.context as WatcherPostFinalityRecoveryInput),
          transportAttestations,
        })
      : null;
  const result = rollbackResult ?? recoveryResult;
  const recoverableOwnedOutRefs =
    recoveryResult === null
      ? null
      : new Set(recoveryResult.removedRecords.protocolUtxoOutRefs);
  if (
    result === null ||
    !same(result, persistedResult) ||
    result.nextStore === null ||
    !same(result.nextStore, verified.store) ||
    result.nextStoreDigest !== observation.durableStoreDigest ||
    rollbackSourceExtends(
      policy,
      priorStore,
      authority.context.sourceStore,
      recoverableOwnedOutRefs,
    ) === null
  ) {
    return null;
  }
  if (rollbackResult !== null) {
    return rollbackResult.action === "apply_rewind" ||
      rollbackResult.action === "duplicate_rewind"
      ? rollbackResult
      : null;
  }
  const finalityPolicy = parseWatcherFinalityPolicy(
    (authority.context as WatcherPostFinalityRecoveryInput).policy,
  );
  const recoveryState = recoveryResult?.recoveryState ?? null;
  const commonAncestor = recoveryState?.path;
  return recoveryResult !== null &&
    recoveryResult.action === "rewind_and_replay" &&
    recoveryResult.protocolDecision === "resume_replay" &&
    recoveryState !== null &&
    recoveryResult.resumableFinalityState !== null &&
    finalityPolicy !== null &&
    finalityPolicy.network === policy.network &&
    finalityPolicy.releaseEvidenceDigest === policy.releaseEvidenceDigest &&
    sameMarker(finalityPolicy.deploymentMarker, policy.deploymentMarker) &&
    recoveryState.network === finalityPolicy.network &&
    recoveryState.releaseEvidenceDigest === policy.releaseEvidenceDigest &&
    sameMarker(
      recoveryState.deploymentMarker,
      verified.store.deploymentMarker,
    ) &&
    commonAncestor !== undefined &&
    observation.pointDigest === commonAncestor.commonAncestorPointDigest &&
    verified.block.chainPoint.pointDigest ===
      commonAncestor.commonAncestorPointDigest &&
    verified.block.chainPoint.blockHash ===
      commonAncestor.commonAncestorBlockHash
    ? recoveryResult
    : null;
};

const protocolStoreExtends = (
  prior: WatcherDurableStore,
  next: WatcherDurableStore,
): boolean => {
  if (
    BigInt(next.revision) < BigInt(prior.revision) ||
    !sameMarker(next.deploymentMarker, prior.deploymentMarker)
  ) {
    return false;
  }
  const nextObservations = new Map(
    next.l1Observations.map((entry) => [entry.observationId, entry]),
  );
  const nextPoints = new Map(
    next.chainPoints.map((entry) => [entry.chainPointId, entry]),
  );
  const nextActive = new Map(
    next.protocolUtxos.map((entry) => [entry.outRef, entry]),
  );
  const nextSpent = new Map(
    next.spentProtocolUtxos.map((entry) => [entry.outRef, entry]),
  );
  if (
    !prior.l1Observations.every((entry) =>
      same(nextObservations.get(entry.observationId), entry),
    ) ||
    !prior.chainPoints.every((entry) =>
      same(nextPoints.get(entry.chainPointId), entry),
    ) ||
    !prior.spentProtocolUtxos.every((entry) =>
      same(nextSpent.get(entry.outRef), entry),
    )
  ) {
    return false;
  }
  return prior.protocolUtxos.every((entry) => {
    const active = nextActive.get(entry.outRef);
    if (active !== undefined) {
      return same(active, entry);
    }
    const spent = nextSpent.get(entry.outRef);
    return (
      spent !== undefined &&
      spent.outRef === entry.outRef &&
      spent.role === entry.role &&
      spent.chainPointId === entry.chainPointId &&
      same(spent.output, entry.output) &&
      nextPoints.has(spent.spentAtChainPointId)
    );
  });
};

const nonRollbackObservationFollows = (
  prior: WatcherStateQueueObservation,
  next: WatcherStateQueueObservation,
  priorContext: VerifiedContext,
  nextContext: VerifiedContext,
): boolean => {
  if (
    next.transitionKind === "rollback" ||
    next.transactionHash === null ||
    next.transactionIndex === null ||
    prior.transactionHash === next.transactionHash
  ) {
    return false;
  }
  const nextBlock = BigInt(next.blockNo);
  const priorBlock = BigInt(prior.blockNo);
  const samePoint = next.pointDigest === prior.pointDigest;
  const orderedWithinPoint =
    samePoint &&
    next.blockHash === prior.blockHash &&
    next.slot === prior.slot &&
    prior.transactionIndex !== null &&
    BigInt(next.transactionIndex) > BigInt(prior.transactionIndex);
  if (nextBlock === priorBlock) {
    return orderedWithinPoint;
  }
  if (nextBlock < priorBlock || samePoint) {
    return false;
  }
  const ancestors = new Map<string, WatcherNormalizedL1Block>();
  for (const candidate of nextContext.originBlocks) {
    const existing = ancestors.get(candidate.chainPoint.blockHash);
    if (
      existing !== undefined &&
      (existing.chainPoint.blockNo !== candidate.chainPoint.blockNo ||
        existing.chainPoint.slot !== candidate.chainPoint.slot ||
        blockParentHash(existing) !== blockParentHash(candidate))
    ) {
      return false;
    }
    ancestors.set(candidate.chainPoint.blockHash, candidate);
  }
  let cursor = nextContext.block;
  const visited = new Set<string>();
  while (!visited.has(cursor.chainPoint.blockHash)) {
    visited.add(cursor.chainPoint.blockHash);
    const parentHash = blockParentHash(cursor);
    if (parentHash === priorContext.block.chainPoint.blockHash) {
      return (
        BigInt(cursor.chainPoint.blockNo) ===
          BigInt(priorContext.block.chainPoint.blockNo) + 1n &&
        BigInt(cursor.chainPoint.slot) >
          BigInt(priorContext.block.chainPoint.slot)
      );
    }
    if (parentHash === null || parentHash === undefined) {
      return false;
    }
    const parent = ancestors.get(parentHash);
    if (
      parent === undefined ||
      !hasAuthenticatedBlockSequence(parent) ||
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

const parseEntryStructural = (
  value: unknown,
): WatcherStateQueueHistoryEntry | null => {
  const record = exactRecord(value, [
    "predecessorStateDigest",
    "priorActiveEntryDigest",
    "chainPointId",
    "pointDigest",
    "transactionHash",
    "transactionIndex",
    "publicInputDigest",
    "transitionKind",
    "correctionTransition",
    "snapshot",
    "observation",
    "publicContext",
    "rollbackResult",
    "entryDigest",
  ]);
  const observation =
    record === null
      ? null
      : parseWatcherStateQueueObservation(record.observation);
  const correctionTransition =
    record?.correctionTransition === null
      ? null
      : parseStateQueueCorrectionTransition(record?.correctionTransition);
  if (
    record === null ||
    observation === null ||
    !(
      record.predecessorStateDigest === null ||
      isHex32(record.predecessorStateDigest)
    ) ||
    !(
      record.priorActiveEntryDigest === null ||
      isHex32(record.priorActiveEntryDigest)
    ) ||
    !isHex32(record.chainPointId) ||
    !isHex32(record.pointDigest) ||
    !(record.transactionHash === null || isHex32(record.transactionHash)) ||
    !(record.transactionIndex === null || isNatural(record.transactionIndex)) ||
    !isHex32(record.publicInputDigest) ||
    record.transitionKind !== observation.transitionKind ||
    (record.correctionTransition !== null && correctionTransition === null) ||
    (observation.transitionKind === "remove_unattested_timeout") !==
      (correctionTransition !== null) ||
    !isHex32(record.entryDigest)
  ) {
    return null;
  }
  const snapshot = parseWatcherStateQueueSnapshot(record.snapshot);
  if (
    snapshot === null ||
    record.predecessorStateDigest !== observation.predecessorStateDigest ||
    record.pointDigest !== observation.pointDigest ||
    record.transactionHash !== observation.transactionHash ||
    record.transactionIndex !== observation.transactionIndex ||
    record.publicInputDigest !== observation.publicInputDigest
  ) {
    return null;
  }
  const canonical = Object.freeze({
    predecessorStateDigest: record.predecessorStateDigest,
    priorActiveEntryDigest: record.priorActiveEntryDigest,
    chainPointId: record.chainPointId,
    pointDigest: record.pointDigest,
    transactionHash: record.transactionHash,
    transactionIndex: record.transactionIndex,
    publicInputDigest: record.publicInputDigest,
    transitionKind: observation.transitionKind,
    correctionTransition,
    snapshot,
    observation,
    publicContext: record.publicContext as WatcherStateQueuePublicContext,
    rollbackResult:
      record.rollbackResult as WatcherStateQueueRollbackResult | null,
  });
  return sha256Canonical(entryWithoutDigest(canonical)) === record.entryDigest
    ? Object.freeze({ ...canonical, entryDigest: record.entryDigest })
    : null;
};

const verifyEntries = (
  policy: WatcherStateQueueIndexerPolicy,
  history: readonly WatcherStateQueueHistoryEntry[],
  audit: readonly WatcherStateQueueAuditEntry[],
  extraContexts: readonly WatcherStateQueuePublicContext[],
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
): ReadonlyMap<string, DecodedTopology> | null => {
  const allEntries = [...history, ...audit.map(({ entry }) => entry)];
  const groupedAudit = auditGroups(audit);
  if (groupedAudit === null) {
    return null;
  }
  const rollbackTargets = new Map<string, string>();
  for (const group of groupedAudit) {
    const rollback = group.at(-1);
    const firstOrphan = group[0]?.status === "orphaned" ? group[0].entry : null;
    const targetDigest =
      firstOrphan?.priorActiveEntryDigest ??
      rollback?.entry.priorActiveEntryDigest;
    if (
      rollback === undefined ||
      rollback.status !== "rollback" ||
      targetDigest === null ||
      targetDigest === undefined
    ) {
      return null;
    }
    rollbackTargets.set(rollback.entry.entryDigest, targetDigest);
  }
  const verified: VerifiedContext[] = [];
  const evidenceBudget = newEvidenceBudget();
  for (let index = 0; index < allEntries.length; index += 1) {
    const entry = allEntries[index]!;
    const context = parsePublicContext(
      policy,
      entry.publicContext,
      entry.observation,
      transportAttestations,
      evidenceBudget,
    );
    if (
      context === null ||
      context.block.chainPoint.chainPointId !== entry.chainPointId
    ) {
      return null;
    }
    if (entry.transitionKind === "rollback") {
      const priorActiveIndex = allEntries.findIndex(
        (candidate) => candidate.entryDigest === entry.priorActiveEntryDigest,
      );
      if (
        index === 0 ||
        priorActiveIndex < 0 ||
        priorActiveIndex >= index ||
        verifiedRollbackBinding(
          policy,
          verified[priorActiveIndex]!.store,
          context,
          entry.observation,
          entry.rollbackResult,
          transportAttestations,
        ) === null
      ) {
        return null;
      }
    } else if (
      context.context.rollbackAuthority !== null ||
      entry.rollbackResult !== null
    ) {
      return null;
    }
    verified.push(context);
  }
  if (extraContexts.length !== 0) {
    return null;
  }
  const sources = authenticatedOutputs(verified);
  if (sources === null) {
    return null;
  }
  const topologies = new Map<string, DecodedTopology>();
  for (let index = 0; index < allEntries.length; index += 1) {
    const entry = allEntries[index]!;
    const context = verified[index]!;
    if (entry.transitionKind === "rollback") {
      const targetDigest = rollbackTargets.get(entry.entryDigest);
      const targetIndex = allEntries.findIndex(
        (candidate) =>
          candidate.transitionKind !== "rollback" &&
          candidate.entryDigest === targetDigest,
      );
      const targetContext = targetIndex < 0 ? undefined : verified[targetIndex];
      const target =
        targetContext === undefined
          ? null
          : reconstructTopology(policy, targetContext.store, sources);
      const restored = reconstructTopology(policy, context.store, sources);
      if (
        target === null ||
        targetContext === undefined ||
        restored === null ||
        !same(restored.snapshot, target.snapshot) ||
        !same(
          [...restored.outRefs.entries()].sort(),
          [...target.outRefs.entries()].sort(),
        ) ||
        !same(
          stateQueueOwnedRecords(policy, context.store, false),
          stateQueueOwnedRecords(policy, targetContext.store, false),
        ) ||
        !same(
          stateQueueOwnedRecords(policy, context.store, true),
          stateQueueOwnedRecords(policy, targetContext.store, true),
        )
      ) {
        return null;
      }
      topologies.set(entry.entryDigest, target);
      continue;
    }
    const topology = reconstructTopology(policy, context.store, sources);
    if (topology === null || !same(topology.snapshot, entry.snapshot)) {
      return null;
    }
    topologies.set(entry.entryDigest, topology);
  }
  for (let index = 0; index < history.length; index += 1) {
    const entry = history[index]!;
    if (index === 0) {
      const topology = topologies.get(entry.entryDigest);
      const context = verified[index]!;
      if (
        entry.transitionKind !== "bootstrap" ||
        entry.predecessorStateDigest !== null ||
        entry.priorActiveEntryDigest !== null ||
        topology === undefined ||
        context.transaction === null
      ) {
        return null;
      }
      continue;
    }
    const prior = history[index - 1]!;
    const priorContext = verified[index - 1]!;
    const priorTopology = topologies.get(prior.entryDigest);
    const topology = topologies.get(entry.entryDigest);
    const context = verified[index]!;
    const topologyTransition =
      priorTopology === undefined || topology === undefined
        ? null
        : classifyObservedTransition(prior.snapshot, topology.snapshot);
    const correctionTransition =
      topologyTransition === "remove_fraudulent" &&
      priorTopology !== undefined &&
      topology !== undefined
        ? deriveAuthenticatedTimeoutCorrectionTransition(
            policy,
            context,
            priorTopology,
            topology,
          )
        : null;
    const derivedTransition =
      correctionTransition === null
        ? topologyTransition
        : "remove_unattested_timeout";
    if (
      entry.priorActiveEntryDigest !== prior.entryDigest ||
      priorTopology === undefined ||
      topology === undefined ||
      context.transaction === null ||
      (entry.transitionKind !== "rollback" &&
        (!protocolStoreExtends(priorContext.store, context.sourceStore) ||
          !nonRollbackObservationFollows(
            prior.observation,
            entry.observation,
            priorContext,
            context,
          ) ||
          history
            .slice(0, index)
            .some(
              ({ transactionHash }) =>
                transactionHash !== null &&
                transactionHash === entry.transactionHash,
            ))) ||
      derivedTransition !== entry.transitionKind ||
      !same(correctionTransition, entry.correctionTransition)
    ) {
      return null;
    }
  }
  const nonRollbackByDigest = new Map(
    [
      ...history,
      ...audit
        .filter(({ status }) => status === "orphaned")
        .map(({ entry }) => entry),
    ].map((entry) => [entry.entryDigest, entry] as const),
  );
  for (const group of groupedAudit) {
    const rollback = group.at(-1);
    const orphaned = group.slice(0, -1);
    if (
      rollback === undefined ||
      rollback.status !== "rollback" ||
      rollback.entry.transitionKind !== "rollback"
    ) {
      return null;
    }
    const priorActive =
      orphaned.at(-1)?.entry ??
      nonRollbackByDigest.get(rollback.entry.priorActiveEntryDigest ?? "");
    if (
      priorActive === undefined ||
      rollback.entry.priorActiveEntryDigest !== priorActive.entryDigest ||
      (orphaned.length === 0 &&
        !same(priorActive.snapshot, rollback.entry.snapshot))
    ) {
      return null;
    }
    let retainedTarget = nonRollbackByDigest.get(
      orphaned[0]?.entry.priorActiveEntryDigest ??
        rollback.entry.priorActiveEntryDigest ??
        "",
    );
    if (retainedTarget === undefined) {
      return null;
    }
    for (const orphan of orphaned) {
      if (
        orphan.status !== "orphaned" ||
        orphan.entry.transitionKind === "rollback" ||
        orphan.entry.priorActiveEntryDigest !== retainedTarget.entryDigest
      ) {
        return null;
      }
      retainedTarget = orphan.entry;
    }
    const rollbackTarget = nonRollbackByDigest.get(
      orphaned[0]?.entry.priorActiveEntryDigest ??
        rollback.entry.priorActiveEntryDigest ??
        "",
    );
    if (
      rollbackTarget === undefined ||
      !same(rollbackTarget.snapshot, rollback.entry.snapshot)
    ) {
      return null;
    }
  }
  return topologies;
};

const replayStateLineage = (
  policy: WatcherStateQueueIndexerPolicy,
  expected: WatcherStateQueueIndexerState,
): WatcherStateQueueIndexerState | null => {
  const bootstrap = expected.history[0];
  if (
    bootstrap === undefined ||
    bootstrap.transitionKind !== "bootstrap" ||
    bootstrap.predecessorStateDigest !== null
  ) {
    return null;
  }
  let replayed = makeState(policy, bootstrap.observation, [bootstrap], []);
  let active: WatcherStateQueueHistoryEntry[] = [bootstrap];
  let audit: WatcherStateQueueAuditEntry[] = [];
  const pending = [
    ...expected.history.slice(1),
    ...expected.auditHistory
      .filter(({ status }) => status === "orphaned")
      .map(({ entry }) => entry),
  ];
  let auditIndex = 0;
  let steps = 0;
  while (
    (pending.length > 0 || auditIndex < expected.auditHistory.length) &&
    steps <
      WATCHER_STATE_QUEUE_INDEXER_BOUNDS.historyEntries +
        WATCHER_STATE_QUEUE_INDEXER_BOUNDS.auditEntries * 2
  ) {
    steps += 1;
    const candidates = pending.filter(
      ({ predecessorStateDigest }) =>
        predecessorStateDigest === replayed.stateDigest,
    );
    if (candidates.length === 1) {
      const next = candidates[0]!;
      pending.splice(pending.indexOf(next), 1);
      active = [...active, next];
      replayed = makeState(policy, next.observation, active, audit);
      continue;
    }
    if (candidates.length > 1) {
      return null;
    }
    const group: WatcherStateQueueAuditEntry[] = [];
    while (expected.auditHistory[auditIndex]?.status === "orphaned") {
      group.push(expected.auditHistory[auditIndex]!);
      auditIndex += 1;
    }
    const rollbackAudit = expected.auditHistory[auditIndex];
    if (
      rollbackAudit === undefined ||
      rollbackAudit.status !== "rollback" ||
      rollbackAudit.entry.predecessorStateDigest !== replayed.stateDigest
    ) {
      return null;
    }
    const orphanDigests = group.map(({ entry }) => entry.entryDigest);
    if (
      orphanDigests.length > active.length ||
      !same(
        active
          .slice(active.length - orphanDigests.length)
          .map(({ entryDigest }) => entryDigest),
        orphanDigests,
      )
    ) {
      return null;
    }
    active = active.slice(0, active.length - orphanDigests.length);
    const prunedAudit = pruneAuditGroups([...audit, ...group, rollbackAudit]);
    if (prunedAudit === null) {
      return null;
    }
    audit = [...prunedAudit];
    replayed = makeState(
      policy,
      rollbackAudit.entry.observation,
      active,
      audit,
    );
    auditIndex += 1;
  }
  return pending.length === 0 &&
    auditIndex === expected.auditHistory.length &&
    same(replayed, expected)
    ? replayed
    : null;
};

const replayFromRetainedAuditCheckpoint = (
  policy: WatcherStateQueueIndexerPolicy,
  expected: WatcherStateQueueIndexerState,
): WatcherStateQueueIndexerState | null => {
  const groups = auditGroups(expected.auditHistory);
  const latestGroup = groups?.at(-1);
  const rollbackAudit = latestGroup?.at(-1);
  if (
    groups === null ||
    latestGroup === undefined ||
    rollbackAudit === undefined ||
    rollbackAudit.status !== "rollback" ||
    rollbackAudit.entry.transitionKind !== "rollback" ||
    rollbackAudit.entry.rollbackResult === null ||
    !["apply_rewind", "rewind_and_replay"].includes(
      rollbackAudit.entry.rollbackResult.action,
    ) ||
    latestGroup.slice(0, -1).some(({ status }) => status !== "orphaned")
  ) {
    return null;
  }
  const auditEntryDigests = expected.auditHistory.map(
    ({ entry }) => entry.entryDigest,
  );
  if (
    new Set(auditEntryDigests).size !== auditEntryDigests.length ||
    expected.history.some(({ entryDigest }) =>
      auditEntryDigests.includes(entryDigest),
    )
  ) {
    return null;
  }
  const nonRollbackByDigest = new Map(
    [
      ...expected.history,
      ...expected.auditHistory
        .filter(({ status }) => status === "orphaned")
        .map(({ entry }) => entry),
    ].map((entry) => [entry.entryDigest, entry] as const),
  );
  if (
    groups.some((group) => {
      const rollback = group.at(-1);
      const firstOrphan =
        group[0]?.status === "orphaned" ? group[0].entry : null;
      const target = nonRollbackByDigest.get(
        firstOrphan?.priorActiveEntryDigest ??
          rollback?.entry.priorActiveEntryDigest ??
          "",
      );
      return (
        rollback?.status !== "rollback" ||
        rollback.entry.transitionKind !== "rollback" ||
        rollback.entry.rollbackResult === null ||
        !["apply_rewind", "rewind_and_replay"].includes(
          rollback.entry.rollbackResult.action,
        ) ||
        target === undefined ||
        !same(target.snapshot, rollback.entry.snapshot) ||
        !expected.history.some(({ snapshot }) =>
          same(snapshot, rollback.entry.snapshot),
        )
      );
    })
  ) {
    return null;
  }
  const firstOrphan =
    latestGroup[0]?.status === "orphaned" ? latestGroup[0].entry : null;
  const targetDigest =
    firstOrphan?.priorActiveEntryDigest ??
    rollbackAudit.entry.priorActiveEntryDigest;
  const targetIndex = expected.history.findIndex(
    ({ entryDigest }) => entryDigest === targetDigest,
  );
  if (targetIndex < 0) {
    return null;
  }
  let active = expected.history.slice(0, targetIndex + 1);
  let replayed = makeState(
    policy,
    rollbackAudit.entry.observation,
    active,
    expected.auditHistory,
  );
  for (const entry of expected.history.slice(targetIndex + 1)) {
    if (entry.predecessorStateDigest !== replayed.stateDigest) {
      return null;
    }
    active = [...active, entry];
    replayed = makeState(
      policy,
      entry.observation,
      active,
      expected.auditHistory,
    );
  }
  return same(replayed, expected) ? replayed : null;
};

export const parseWatcherStateQueueIndexerState = (
  value: unknown,
  policyInput: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
  restartContexts: readonly WatcherStateQueuePublicContext[] = [],
): WatcherStateQueueIndexerState | null => {
  const evidenceBudget: EvidenceGraphBudget = { nodes: 0, bytes: 0 };
  if (
    !evidenceWithinBounds(policyInput, evidenceBudget) ||
    !evidenceWithinBounds(value, evidenceBudget) ||
    !evidenceWithinBounds(restartContexts, evidenceBudget)
  ) {
    return null;
  }
  const policy = parseWatcherStateQueueIndexerPolicy(policyInput);
  const record = exactRecord(value, [
    "schemaVersion",
    "policyDigest",
    "network",
    "releaseEvidenceDigest",
    "deploymentMarker",
    "pointDigest",
    "transactionHash",
    "transactionIndex",
    "publicInputDigest",
    "durableStoreDigest",
    "snapshot",
    "history",
    "auditHistory",
    "stateDigest",
  ]);
  const marker = record === null ? null : parseMarker(record.deploymentMarker);
  const snapshot =
    record === null ? null : parseWatcherStateQueueSnapshot(record.snapshot);
  const historyValues =
    record === null
      ? null
      : exactArray(
          record.history,
          WATCHER_STATE_QUEUE_INDEXER_BOUNDS.historyEntries,
        );
  const auditValues =
    record === null
      ? null
      : exactArray(
          record.auditHistory,
          WATCHER_STATE_QUEUE_INDEXER_BOUNDS.auditEntries,
        );
  if (
    policy === null ||
    record === null ||
    marker === null ||
    snapshot === null ||
    historyValues === null ||
    auditValues === null ||
    record.schemaVersion !== WATCHER_STATE_QUEUE_INDEXER_STATE_SCHEMA_VERSION ||
    record.policyDigest !== policy.policyDigest ||
    record.network !== policy.network ||
    record.releaseEvidenceDigest !== policy.releaseEvidenceDigest ||
    !sameMarker(marker, policy.deploymentMarker) ||
    !isHex32(record.pointDigest) ||
    !(record.transactionHash === null || isHex32(record.transactionHash)) ||
    !(record.transactionIndex === null || isNatural(record.transactionIndex)) ||
    !isHex32(record.publicInputDigest) ||
    !isHex32(record.durableStoreDigest) ||
    !isHex32(record.stateDigest) ||
    historyValues.length === 0 ||
    historyValues.length > Number(policy.maximumHistoryEntries)
  ) {
    return null;
  }
  const history = historyValues.map(parseEntryStructural);
  const audit = auditValues.map((candidate) => {
    const auditRecord = exactRecord(candidate, [
      "status",
      "entry",
      "auditDigest",
    ]);
    const entry =
      auditRecord === null ? null : parseEntryStructural(auditRecord.entry);
    if (
      auditRecord === null ||
      entry === null ||
      (auditRecord.status !== "orphaned" &&
        auditRecord.status !== "rollback") ||
      !isHex32(auditRecord.auditDigest)
    ) {
      return null;
    }
    const canonical = Object.freeze({
      status: auditRecord.status,
      entry,
    });
    return sha256Canonical(auditWithoutDigest(canonical)) ===
      auditRecord.auditDigest
      ? Object.freeze({
          ...canonical,
          auditDigest: auditRecord.auditDigest,
        })
      : null;
  });
  if (
    history.some((entry) => entry === null) ||
    audit.some((entry) => entry === null)
  ) {
    return null;
  }
  const canonicalHistory = history as WatcherStateQueueHistoryEntry[];
  const canonicalAudit = audit as WatcherStateQueueAuditEntry[];
  const last = canonicalHistory.at(-1)!;
  const canonical = Object.freeze({
    schemaVersion: WATCHER_STATE_QUEUE_INDEXER_STATE_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: marker,
    pointDigest: record.pointDigest,
    transactionHash: record.transactionHash,
    transactionIndex: record.transactionIndex,
    publicInputDigest: record.publicInputDigest,
    durableStoreDigest: record.durableStoreDigest,
    snapshot,
    history: Object.freeze(canonicalHistory),
    auditHistory: Object.freeze(canonicalAudit),
  });
  if (
    !same(snapshot, last.snapshot) ||
    new Set(canonicalHistory.map(({ entryDigest }) => entryDigest)).size !==
      canonicalHistory.length ||
    verifyEntries(
      policy,
      canonicalHistory,
      canonicalAudit,
      restartContexts,
      transportAttestations,
    ) === null ||
    sha256Canonical(stateWithoutDigest(canonical)) !== record.stateDigest
  ) {
    return null;
  }
  const parsed = Object.freeze({
    ...canonical,
    stateDigest: record.stateDigest,
  });
  return (
    replayStateLineage(policy, parsed) ??
    replayFromRetainedAuditCheckpoint(policy, parsed)
  );
};

export const evaluateWatcherStateQueueIndexer = (
  policyInput: unknown,
  previousStateInput: unknown,
  observationInput: unknown,
  publicContextInput: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
): WatcherStateQueueIndexerResult => {
  const evidenceBudget: EvidenceGraphBudget = { nodes: 0, bytes: 0 };
  if (!evidenceWithinBounds(policyInput, evidenceBudget)) {
    return rejected("malformed_policy");
  }
  if (
    previousStateInput !== null &&
    !evidenceWithinBounds(previousStateInput, evidenceBudget)
  ) {
    return rejected("malformed_state", "watcher_state_queue_state_rejected");
  }
  if (!evidenceWithinBounds(observationInput, evidenceBudget)) {
    return rejected("malformed_observation");
  }
  if (!evidenceWithinBounds(publicContextInput, evidenceBudget)) {
    return rejected("malformed_public_context");
  }
  const policy = parseWatcherStateQueueIndexerPolicy(policyInput);
  if (policy === null) {
    return rejected("malformed_policy");
  }
  const observation = parseWatcherStateQueueObservation(observationInput);
  if (observation === null) {
    return rejected("malformed_observation");
  }
  if (
    observation.policyDigest !== policy.policyDigest ||
    observation.network !== policy.network ||
    observation.releaseEvidenceDigest !== policy.releaseEvidenceDigest ||
    !sameMarker(observation.deploymentMarker, policy.deploymentMarker)
  ) {
    return rejected("binding_mismatch");
  }
  const previousState =
    previousStateInput === null
      ? null
      : parseWatcherStateQueueIndexerState(
          previousStateInput,
          policy,
          transportAttestations,
        );
  if (previousStateInput !== null && previousState === null) {
    return rejected("malformed_state", "watcher_state_queue_state_rejected");
  }
  if (
    (previousState === null) !== (observation.transitionKind === "bootstrap") ||
    observation.predecessorStateDigest !== (previousState?.stateDigest ?? null)
  ) {
    return rejected("stale_state");
  }
  const verified = parsePublicContext(
    policy,
    publicContextInput,
    observation,
    transportAttestations,
  );
  if (verified === null) {
    return rejected("malformed_public_context");
  }
  const priorEntries =
    previousState === null
      ? []
      : [
          ...previousState.history,
          ...previousState.auditHistory.map(({ entry }) => entry),
        ];
  const priorVerified: VerifiedContext[] = [];
  for (const entry of priorEntries) {
    const context = parsePublicContext(
      policy,
      entry.publicContext,
      entry.observation,
      transportAttestations,
    );
    if (context === null) {
      return rejected("malformed_state");
    }
    priorVerified.push(context);
  }
  const sources = authenticatedOutputs([...priorVerified, verified]);
  if (sources === null) {
    return rejected("public_evidence_mismatch");
  }
  const nextTopology = reconstructTopology(policy, verified.store, sources);
  if (nextTopology === null) {
    return rejected(
      "linked_queue_mismatch",
      "watcher_state_queue_binding_rejected",
    );
  }
  if (previousState === null) {
    if (
      verified.transaction === null ||
      observation.transitionKind !== "bootstrap"
    ) {
      return rejected("public_evidence_mismatch");
    }
    const entry = makeEntry(
      observation,
      verified,
      nextTopology.snapshot,
      null,
      null,
      null,
    );
    const state = makeState(policy, observation, [entry], []);
    return makeResult({
      action: "accept",
      protocolDecision: "indexed",
      reasonCodes: Object.freeze(["bootstrap_authenticated"]),
      alertCodes: Object.freeze([]),
      state,
    });
  }
  const duplicate = previousState.history.find(
    ({ observation: prior }) =>
      prior.observationDigest === observation.observationDigest,
  );
  if (duplicate !== undefined) {
    return makeResult({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: Object.freeze(["duplicate_observation"]),
      alertCodes: Object.freeze([]),
      state: previousState,
    });
  }
  if (
    observation.transitionKind !== "rollback" &&
    previousState.history.some(
      (entry) =>
        observation.transactionHash !== null &&
        entry.transactionHash === observation.transactionHash,
    )
  ) {
    return rejected("identity_collision");
  }
  const priorCurrentEntry = currentStateEntry(previousState);
  const priorCurrentVerified = parsePublicContext(
    policy,
    priorCurrentEntry.publicContext,
    priorCurrentEntry.observation,
    transportAttestations,
  );
  const previousTopology =
    priorCurrentVerified === null
      ? null
      : reconstructTopology(policy, priorCurrentVerified.store, sources);
  if (priorCurrentVerified === null || previousTopology === null) {
    return rejected("malformed_state");
  }
  if (
    observation.transitionKind !== "rollback" &&
    !protocolStoreExtends(priorCurrentVerified.store, verified.sourceStore)
  ) {
    return rejected("public_evidence_mismatch");
  }
  if (observation.transitionKind !== "rollback") {
    if (
      !nonRollbackObservationFollows(
        priorCurrentEntry.observation,
        observation,
        priorCurrentVerified,
        verified,
      )
    ) {
      return rejected("stale_chain_point");
    }
  }
  if (observation.transitionKind === "rollback") {
    const rollbackResult = verifiedRollbackBinding(
      policy,
      priorCurrentVerified.store,
      verified,
      observation,
      verified.context.rollbackAuthority?.result,
      transportAttestations,
    );
    if (rollbackResult === null) {
      return rejected(
        "rollback_authority_mismatch",
        "watcher_state_queue_binding_rejected",
      );
    }
    const postFinalityRecovery = isPostFinalityRecoveryResult(rollbackResult);
    const postFinalityRecoveryState = postFinalityRecovery
      ? rollbackResult.recoveryState
      : null;
    if (!postFinalityRecovery && rollbackResult.action === "duplicate_rewind") {
      if (
        rollbackResult.protocolDecision !== "hold" ||
        !same(verified.store, verified.sourceStore) ||
        !same(nextTopology.snapshot, previousState.snapshot)
      ) {
        return rejected(
          "rollback_authority_mismatch",
          "watcher_state_queue_binding_rejected",
        );
      }
      return makeResult({
        action: "duplicate",
        protocolDecision: "hold",
        reasonCodes: Object.freeze(["duplicate_observation"]),
        alertCodes: Object.freeze([]),
        state: previousState,
      });
    }
    if (
      (postFinalityRecovery &&
        rollbackResult.protocolDecision !== "resume_replay") ||
      (!postFinalityRecovery &&
        rollbackResult.protocolDecision !== "resume_pending")
    ) {
      return rejected(
        "rollback_authority_mismatch",
        "watcher_state_queue_binding_rejected",
      );
    }
    const removedPoints = new Set(rollbackResult.removedRecords.chainPointIds);
    const removedObservations = new Set(
      rollbackResult.removedRecords.l1ObservationIds,
    );
    const removedOutRefs = new Set(
      rollbackResult.removedRecords.protocolUtxoOutRefs,
    );
    const previousTopologies = verifyEntries(
      policy,
      previousState.history,
      previousState.auditHistory,
      [],
      transportAttestations,
    );
    if (previousTopologies === null) {
      return rejected("malformed_state");
    }
    const retained: WatcherStateQueueHistoryEntry[] = [];
    const orphaned: WatcherStateQueueHistoryEntry[] = [];
    const commonAncestorBlockNo =
      postFinalityRecoveryState?.path.commonAncestorBlockNo ?? null;
    for (const entry of previousState.history) {
      const topology = previousTopologies.get(entry.entryDigest);
      const removed =
        removedPoints.has(entry.chainPointId) ||
        removedObservations.has(entry.observation.sourceObservationDigest) ||
        (topology !== undefined &&
          [...topology.outRefs.values()].some((outRef) =>
            removedOutRefs.has(outRef),
          ));
      if (postFinalityRecovery) {
        if (commonAncestorBlockNo === null) {
          return rejected("rollback_mismatch");
        }
        const beyondCommonAncestor =
          BigInt(entry.observation.blockNo) > BigInt(commonAncestorBlockNo);
        if (removed !== beyondCommonAncestor) {
          return rejected("rollback_mismatch");
        }
        (beyondCommonAncestor ? orphaned : retained).push(entry);
      } else {
        (removed ? orphaned : retained).push(entry);
      }
    }
    if (
      retained.length === 0 ||
      !same(retained.at(-1)!.snapshot, nextTopology.snapshot)
    ) {
      return rejected("rollback_mismatch");
    }
    const targetTopology = previousTopologies.get(retained.at(-1)!.entryDigest);
    const targetHistoryIndex = previousState.history.indexOf(retained.at(-1)!);
    const targetVerified = priorVerified[targetHistoryIndex];
    if (
      targetTopology === undefined ||
      targetVerified === undefined ||
      !same(nextTopology.snapshot, targetTopology.snapshot) ||
      !same(
        [...nextTopology.outRefs.entries()].sort(),
        [...targetTopology.outRefs.entries()].sort(),
      ) ||
      !same(
        stateQueueOwnedRecords(policy, verified.store, false),
        stateQueueOwnedRecords(policy, targetVerified.store, false),
      ) ||
      !same(
        stateQueueOwnedRecords(policy, verified.store, true),
        stateQueueOwnedRecords(policy, targetVerified.store, true),
      ) ||
      (postFinalityRecovery &&
        (postFinalityRecoveryState === null ||
          BigInt(targetVerified.block.chainPoint.blockNo) >
            BigInt(postFinalityRecoveryState.path.commonAncestorBlockNo) ||
          !verified.store.chainPoints.some(
            ({ blockHash, blockNo }) =>
              blockHash ===
                postFinalityRecoveryState.path.replacementTipBlockHash &&
              blockNo === postFinalityRecoveryState.path.replacementTipBlockNo,
          ))) ||
      verified.store.protocolUtxos.some(({ outRef }) =>
        removedOutRefs.has(outRef),
      )
    ) {
      return rejected("rollback_mismatch");
    }
    const rollbackEntry = makeEntry(
      observation,
      verified,
      nextTopology.snapshot,
      null,
      rollbackResult,
      previousState.history.at(-1)!.entryDigest,
    );
    const appendedAudit = [
      ...previousState.auditHistory,
      ...orphaned.map((entry) => makeAudit("orphaned", entry)),
      makeAudit("rollback", rollbackEntry),
    ];
    const boundedAudit = pruneAuditGroups(appendedAudit);
    if (boundedAudit === null || boundedAudit.length === 0) {
      return rejected("history_limit_exceeded");
    }
    const state = makeState(policy, observation, retained, boundedAudit);
    return makeResult({
      action: "accept",
      protocolDecision: "indexed",
      reasonCodes: Object.freeze(["rollback_authenticated"]),
      alertCodes: Object.freeze([]),
      state,
    });
  }
  if (verified.transaction === null) {
    return rejected("public_evidence_mismatch");
  }
  const topologyTransition = classifyObservedTransition(
    previousTopology.snapshot,
    nextTopology.snapshot,
  );
  const correctionTransition =
    topologyTransition === "remove_fraudulent"
      ? deriveAuthenticatedTimeoutCorrectionTransition(
          policy,
          verified,
          previousTopology,
          nextTopology,
        )
      : null;
  const observedTransition =
    correctionTransition === null
      ? topologyTransition
      : "remove_unattested_timeout";
  if (observedTransition !== observation.transitionKind) {
    return rejected(
      observedTransition === null
        ? "linked_queue_mismatch"
        : observation.transitionKind === "append" ||
            observation.transitionKind === "attach_da"
          ? "append_mismatch"
          : observation.transitionKind === "merge"
            ? "merge_mismatch"
            : "removal_mismatch",
      "watcher_state_queue_transition_rejected",
    );
  }
  if (previousState.history.length >= Number(policy.maximumHistoryEntries)) {
    return rejected("history_limit_exceeded");
  }
  const entry = makeEntry(
    observation,
    verified,
    nextTopology.snapshot,
    correctionTransition,
    null,
    previousState.history.at(-1)!.entryDigest,
  );
  const state = makeState(
    policy,
    observation,
    [...previousState.history, entry],
    previousState.auditHistory,
  );
  const reason =
    observation.transitionKind === "append"
      ? "append_authenticated"
      : observation.transitionKind === "attach_da"
        ? "da_attestation_authenticated"
        : observation.transitionKind === "merge"
          ? "merge_authenticated"
          : observation.transitionKind === "remove_unattested_timeout"
            ? "timeout_correction_authenticated"
            : "removal_authenticated";
  return makeResult({
    action: "accept",
    protocolDecision: "indexed",
    reasonCodes: Object.freeze([reason]),
    alertCodes: Object.freeze([]),
    state,
  });
};

export type WatcherStateQueueIndexerResultVerificationContext = Readonly<{
  policy: unknown;
  previousState: unknown;
  observation: unknown;
  publicContext: unknown;
  transportAttestations: readonly WatcherL1TransportAttestationContext[];
}>;

export const parseWatcherStateQueueIndexerResult = (
  value: unknown,
  context: WatcherStateQueueIndexerResultVerificationContext,
): WatcherStateQueueIndexerResult | null => {
  if (!evidenceWithinBounds(value)) {
    return null;
  }
  const expected = evaluateWatcherStateQueueIndexer(
    context.policy,
    context.previousState,
    context.observation,
    context.publicContext,
    context.transportAttestations,
  );
  return same(value, expected) ? expected : null;
};
