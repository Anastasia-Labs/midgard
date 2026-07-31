import { createHash } from "node:crypto";

import {
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  DepositSpendRedeemer,
  MerkleRootSchema,
  PayoutDatum,
  PayoutMintRedeemer,
  PayoutSpendRedeemer,
  ProofSchema,
  ReserveSpendRedeemer,
  RetiredOperatorMintRedeemer,
  SettlementDatum,
  SettlementMintRedeemer,
  SettlementSpendRedeemer,
  StateQueueRedeemer,
  UserEventMintRedeemer,
  UserEventWitnessPublishRedeemer,
  WithdrawalOrderDatum,
  WithdrawalSpendRedeemer,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

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
  makeWatcherDurableStoreV1,
  parseWatcherDurableStoreV1,
  watcherDurableStoreBytesSha256,
  type WatcherDurableStoreV1,
  type WatcherProtocolUtxoV1,
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
  type WatcherL1RedeemerV1,
  type WatcherL1TransactionV1,
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

export const WATCHER_SETTLEMENT_INDEXER_POLICY_V1_SCHEMA_VERSION =
  "midgard-watcher-settlement-indexer-policy-v1" as const;
export const WATCHER_SETTLEMENT_SNAPSHOT_V1_SCHEMA_VERSION =
  "midgard-watcher-settlement-snapshot-v1" as const;
export const WATCHER_SETTLEMENT_OBSERVATION_V1_SCHEMA_VERSION =
  "midgard-watcher-settlement-observation-v1" as const;
export const WATCHER_SETTLEMENT_INDEXER_STATE_V1_SCHEMA_VERSION =
  "midgard-watcher-settlement-indexer-state-v1" as const;
export const WATCHER_SETTLEMENT_INDEXER_RESULT_V1_SCHEMA_VERSION =
  "midgard-watcher-settlement-indexer-result-v1" as const;
export const WATCHER_SETTLEMENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION =
  "midgard-watcher-settlement-public-context-v1" as const;

export const WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS = Object.freeze({
  resources: 4_096,
  subjects: 4_096,
  activeHistoryEntries: 128,
  rollbackHistoryEntries: 128,
  transitionHistoryEntries: 16_384,
  orphanAuditDigests: 256,
  retryAttempts: 32,
  requiredFinalityDepthMaximum: 2_160,
  finalityLineageSteps: 2_160,
  observationsPerFinalityStep: 16,
  evidenceContainerEntries: 16_384,
  cumulativeEvidenceBytes: 134_217_728,
  cumulativeEvidenceNodes: 2_000_000,
});

export const WATCHER_SETTLEMENT_TRANSITION_KINDS_V1 = [
  "bootstrap",
  "spawn_settlement",
  "attach_resolution_claim",
  "disprove_resolution_claim",
  "resolve_settlement",
  "absorb_to_reserve",
  "initialize_payout",
  "fund_payout",
  "conclude_payout",
  "refund_withdrawal",
  "retry",
  "mark_stuck",
  "mark_invalid",
  "rollback",
] as const;

export const WATCHER_SETTLEMENT_REASON_CODES_V1 = [
  "bootstrap_authenticated",
  "transition_authenticated",
  "retry_authenticated",
  "stuck_authenticated",
  "invalid_authenticated",
  "rollback_authenticated",
  "duplicate_observation",
  "malformed_policy",
  "malformed_state",
  "malformed_observation",
  "malformed_public_context",
  "binding_mismatch",
  "public_evidence_mismatch",
  "durable_evidence_mismatch",
  "stale_state",
  "stale_chain_point",
  "identity_collision",
  "topology_mismatch",
  "transaction_mismatch",
  "output_mismatch",
  "datum_mismatch",
  "redeemer_mismatch",
  "mint_mismatch",
  "value_mismatch",
  "status_mismatch",
  "retry_mismatch",
  "rollback_mismatch",
  "rollback_authority_mismatch",
  "post_finality_quarantine",
  "history_limit_exceeded",
] as const;

export const WATCHER_SETTLEMENT_ALERT_CODES_V1 = [
  "watcher_settlement_input_rejected",
  "watcher_settlement_binding_rejected",
  "watcher_settlement_transition_rejected",
  "watcher_settlement_post_finality_incident",
] as const;

export type WatcherSettlementNetworkV1 = "Mainnet" | "Preprod" | "Preview";
export type WatcherSettlementTransitionKindV1 =
  (typeof WATCHER_SETTLEMENT_TRANSITION_KINDS_V1)[number];
export type WatcherSettlementReasonCodeV1 =
  (typeof WATCHER_SETTLEMENT_REASON_CODES_V1)[number];
export type WatcherSettlementAlertCodeV1 =
  (typeof WATCHER_SETTLEMENT_ALERT_CODES_V1)[number];

export type WatcherSettlementIndexerPolicyV1 = Readonly<{
  schemaVersion: typeof WATCHER_SETTLEMENT_INDEXER_POLICY_V1_SCHEMA_VERSION;
  network: WatcherSettlementNetworkV1;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  hubOraclePolicyId: string;
  depositPolicyId: string;
  settlementPolicyId: string;
  settlementSpendScriptHash: string;
  reserveSpendScriptHash: string;
  payoutPolicyId: string;
  payoutSpendScriptHash: string;
  withdrawalPolicyId: string;
  withdrawalSpendScriptHash: string;
  activeOperatorPolicyId: string;
  retiredOperatorPolicyId: string;
  stateQueuePolicyId: string;
  settlementAddressHex: string;
  reserveAddressHex: string;
  payoutAddressHex: string;
  withdrawalAddressHex: string;
  bootstrapStoreDigest: string;
  deploymentTrustRootId: string;
  requiredFinalityDepth: string;
  maximumHistoryEntries: string;
  maximumRollbackEntries: string;
  maximumRetryAttempts: string;
  policyDigest: string;
}>;

export type WatcherSettlementResourceRoleV1 =
  | "settlement"
  | "reserve"
  | "payout"
  | "withdrawal";

export type WatcherSettlementAssetV1 = Readonly<{
  policyId: string;
  assetName: string;
  quantity: string;
}>;

export type WatcherSettlementMintAssetV1 = Readonly<{
  policyId: string;
  assetName: string;
  quantity: string;
}>;

export type WatcherSettlementResourceV1 = Readonly<{
  outRef: string;
  role: WatcherSettlementResourceRoleV1;
  addressHex: string;
  paymentScriptHash: string;
  outputSha256: string;
  datumSha256: string | null;
  datumCborHex: string | null;
  lovelace: string;
  assets: readonly WatcherSettlementAssetV1[];
  identityPolicyId: string | null;
  identityAssetName: string | null;
  datumKind: "settlement" | "payout" | "withdrawal" | "no_datum";
  semanticDigest: string;
}>;

export type WatcherSettlementSubjectStatusV1 =
  | "open"
  | "claimed"
  | "disproved"
  | "resolved"
  | "reserve_active"
  | "reserve_depleted"
  | "withdrawal_pending"
  | "payout_initializing"
  | "payout_funding"
  | "payout_funded"
  | "retrying"
  | "paid"
  | "refunded"
  | "stuck"
  | "invalid";

export type WatcherSettlementSubjectV1 = Readonly<{
  subjectId: string;
  subjectKind:
    | "settlement"
    | "resolution_claim"
    | "reserve"
    | "withdrawal"
    | "payout";
  status: WatcherSettlementSubjectStatusV1;
  resourceOutRef: string | null;
  relatedSubjectId: string | null;
  resolutionTime: string | null;
  operatorVkey: string | null;
  attempt: string;
  terminalTransactionHash: string | null;
  failureCode: string | null;
  subjectDigest: string;
}>;

export type WatcherSettlementSnapshotV1 = Readonly<{
  schemaVersion: typeof WATCHER_SETTLEMENT_SNAPSHOT_V1_SCHEMA_VERSION;
  resources: readonly WatcherSettlementResourceV1[];
  subjects: readonly WatcherSettlementSubjectV1[];
  snapshotDigest: string;
}>;

export type WatcherSettlementTransitionV1 = Readonly<{
  kind: WatcherSettlementTransitionKindV1;
  subjectId: string | null;
  relatedSubjectId: string | null;
  consumedOutRefs: readonly string[];
  producedOutRefs: readonly string[];
  requiredRedeemers: readonly Readonly<{
    purpose: "spend" | "mint" | "certificate" | "withdrawal";
    index: string;
    schema:
      | "settlement_spend"
      | "settlement_mint"
      | "reserve_spend"
      | "payout_spend"
      | "payout_mint"
      | "withdrawal_spend"
      | "deposit_spend"
      | "user_event_mint"
      | "event_witness"
      | "membership_withdrawal"
      | "active_operator_spend"
      | "active_operator_mint"
      | "retired_operator_mint"
      | "state_queue_mint";
    constructor:
      | "AttachResolutionClaim"
      | "DisproveResolutionClaim"
      | "Resolve"
      | "Spawn"
      | "Remove"
      | "Spend"
      | "AddFunds"
      | "ConcludeWithdrawal"
      | "MintPayout"
      | "BurnPayout"
      | "InitializePayout"
      | "Refund"
      | "DepositSpend"
      | "AuthenticateEvent"
      | "BurnEventNFT"
      | "MintOrBurn"
      | "RegisterToProveNotRegistered"
      | "UnregisterToProveNotRegistered"
      | "MembershipProof"
      | "ListStateTransition"
      | "UpdateBondHoldNewSettlement"
      | "SlashOperator"
      | "MergeToConfirmedStateV1";
  }>[];
  exactMint: readonly WatcherSettlementMintAssetV1[];
  failureCode: string | null;
  retryAttempt: string | null;
}>;

export type WatcherSettlementObservationV1 = Readonly<{
  schemaVersion: typeof WATCHER_SETTLEMENT_OBSERVATION_V1_SCHEMA_VERSION;
  policyDigest: string;
  network: WatcherSettlementNetworkV1;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  pointDigest: string;
  chainPointId: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  transactionHash: string | null;
  sourceObservationDigest: string;
  durableStoreDigest: string;
  predecessorStateDigest: string | null;
  transition: WatcherSettlementTransitionV1;
  snapshot: WatcherSettlementSnapshotV1;
  observationDigest: string;
}>;

export type WatcherSettlementHistoryEntryV1 = Readonly<{
  observation: WatcherSettlementObservationV1;
  publicContextDigest: string;
  stateDigest: string;
}>;

export type WatcherSettlementRollbackEntryV1 = Readonly<{
  observation: WatcherSettlementObservationV1;
  publicContextDigest: string;
  rollbackResult: WatcherRollbackResultV1 | WatcherPostFinalityRecoveryResultV1;
  rollbackDigest: string;
}>;

export type WatcherSettlementTransitionAuditEntryV1 =
  | Readonly<{
      kind: "transition";
      entry: WatcherSettlementHistoryEntryV1;
    }>
  | Readonly<{
      kind: "rollback";
      entry: WatcherSettlementRollbackEntryV1;
    }>;

export type WatcherSettlementIndexerStateV1 = Readonly<{
  schemaVersion: typeof WATCHER_SETTLEMENT_INDEXER_STATE_V1_SCHEMA_VERSION;
  policyDigest: string;
  network: WatcherSettlementNetworkV1;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  pointDigest: string;
  chainPointId: string;
  durableStoreDigest: string;
  snapshot: WatcherSettlementSnapshotV1;
  activeHistory: readonly WatcherSettlementHistoryEntryV1[];
  rollbackHistory: readonly WatcherSettlementRollbackEntryV1[];
  transitionHistory: readonly WatcherSettlementTransitionAuditEntryV1[];
  orphanAuditDigests: readonly string[];
  orphanLineageDigest: string;
  stateDigest: string;
}>;

export type WatcherSettlementIndexerResultV1 = Readonly<{
  schemaVersion: typeof WATCHER_SETTLEMENT_INDEXER_RESULT_V1_SCHEMA_VERSION;
  action: "accept" | "duplicate" | "reject";
  protocolDecision: "indexed" | "hold" | "quarantined";
  reasonCodes: readonly WatcherSettlementReasonCodeV1[];
  alertCodes: readonly WatcherSettlementAlertCodeV1[];
  state: WatcherSettlementIndexerStateV1 | null;
  resultDigest: string;
}>;

export type WatcherSettlementRollbackContextBindingV1 = Readonly<{
  resultDigest: string;
  context:
    | WatcherRollbackVerificationContextV1
    | WatcherPostFinalityRecoveryInputV1;
}>;

export type WatcherSettlementDeploymentAuthorityV1 = Readonly<{
  signedIdentity: unknown;
  policy: WatcherDeploymentIdentityPolicyV1;
  trustRoots: readonly WatcherDeploymentTrustRootV1[];
  result: VerifiedWatcherDeploymentIdentityV1;
}>;

export type WatcherSettlementFinalityAuthorityV1 = Readonly<{
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

export type WatcherSettlementRestartContextV1 = Readonly<{
  publicContextDigest: string;
  authenticatedProvider: unknown;
  l1Observation: unknown;
  sourceDurableStore: unknown;
  durableStore: unknown;
  deploymentAuthority: WatcherSettlementDeploymentAuthorityV1;
  finalityAuthority: WatcherSettlementFinalityAuthorityV1 | null;
}>;

export type WatcherSettlementPublicContextV1 = Readonly<{
  schemaVersion: typeof WATCHER_SETTLEMENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION;
  authenticatedProvider: unknown;
  l1Observation: unknown;
  sourceDurableStore: unknown;
  durableStore: unknown;
  deploymentAuthority: WatcherSettlementDeploymentAuthorityV1;
  finalityAuthority: WatcherSettlementFinalityAuthorityV1 | null;
  rollbackAuthority: Readonly<{
    result: unknown;
    context:
      | WatcherRollbackVerificationContextV1
      | WatcherPostFinalityRecoveryInputV1;
  }> | null;
  restartContexts: readonly WatcherSettlementRestartContextV1[];
  restartRollbackContexts: readonly WatcherSettlementRollbackContextBindingV1[];
}>;

export type WatcherSettlementResultVerificationContextV1 = Readonly<{
  policy: unknown;
  previousState: unknown;
  observation: unknown;
  publicContext: unknown;
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
}>;

type PlainRecord = Record<string, unknown>;
type CanonicalJson =
  | null
  | boolean
  | string
  | readonly CanonicalJson[]
  | { readonly [key: string]: CanonicalJson };

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;
const HEX_BYTES_OR_EMPTY = /^(?:[0-9a-f]{2})*$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const NON_ZERO_INTEGER = /^-?(?:[1-9][0-9]*)$/u;
const FAILURE_CODE = /^[a-z][a-z0-9_]{0,63}$/u;
const NETWORKS = ["Mainnet", "Preprod", "Preview"] as const;
const TARGET_ROLES = new Set<WatcherSettlementResourceRoleV1>([
  "settlement",
  "reserve",
  "payout",
  "withdrawal",
]);

const canonicalJson = (value: CanonicalJson): string => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map((member) => canonicalJson(member)).join(",")}]`;
  }
  const record = value as { readonly [key: string]: CanonicalJson };
  return `{${Object.keys(record)
    .sort()
    .map(
      (key) =>
        `${JSON.stringify(key)}:${canonicalJson(record[key] as CanonicalJson)}`,
    )
    .join(",")}}`;
};

const sha256Canonical = (value: unknown): string =>
  createHash("sha256")
    .update(canonicalJson(value as CanonicalJson), "utf8")
    .digest("hex");

const sha256Bytes = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

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

const evidenceWithinBounds = (value: unknown): boolean => {
  const pending: { readonly value: unknown; readonly exiting: boolean }[] = [
    { value, exiting: false },
  ];
  const active = new Set<object>();
  let bytes = typeof value === "string" ? Buffer.byteLength(value, "utf8") : 0;
  let nodes = 1;
  if (
    bytes > WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.cumulativeEvidenceBytes ||
    nodes > WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.cumulativeEvidenceNodes
  ) {
    return false;
  }
  while (pending.length > 0) {
    const frame = pending.pop()!;
    const candidate = frame.value;
    if (frame.exiting) {
      active.delete(candidate as object);
      continue;
    }
    if (typeof candidate === "object" && candidate !== null) {
      if (active.has(candidate)) {
        return false;
      }
      active.add(candidate);
      const prototype = Object.getPrototypeOf(candidate);
      if (
        prototype !== Object.prototype &&
        prototype !== Array.prototype &&
        prototype !== null
      ) {
        return false;
      }
      if (
        Array.isArray(candidate) &&
        candidate.length >
          WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.evidenceContainerEntries
      ) {
        return false;
      }
      pending.push({ value: candidate, exiting: true });
      let childCount = 0;
      for (const key in candidate) {
        if (!Object.hasOwn(candidate, key)) {
          continue;
        }
        childCount += 1;
        if (
          childCount >
          WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.evidenceContainerEntries
        ) {
          return false;
        }
        const descriptor = Object.getOwnPropertyDescriptor(candidate, key);
        if (
          descriptor === undefined ||
          descriptor.get !== undefined ||
          descriptor.set !== undefined
        ) {
          return false;
        }
        const keyBytes = Buffer.byteLength(key, "utf8");
        if (
          keyBytes >
          WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.cumulativeEvidenceBytes - bytes
        ) {
          return false;
        }
        bytes += keyBytes;
        if (
          nodes >= WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.cumulativeEvidenceNodes
        ) {
          return false;
        }
        nodes += 1;
        if (typeof descriptor.value === "string") {
          const valueBytes = Buffer.byteLength(descriptor.value, "utf8");
          if (
            valueBytes >
            WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.cumulativeEvidenceBytes - bytes
          ) {
            return false;
          }
          bytes += valueBytes;
        }
        pending.push({ value: descriptor.value, exiting: false });
      }
    }
  }
  return true;
};

const exactArray = (
  value: unknown,
  maximum: number,
): readonly unknown[] | null => {
  if (
    !Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Array.prototype ||
    value.length > maximum ||
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
  return value as readonly unknown[];
};

const isNatural = (value: unknown): value is string =>
  typeof value === "string" && NATURAL.test(value);
const isHex28 = (value: unknown): value is string =>
  typeof value === "string" && HEX_28.test(value);
const isHex32 = (value: unknown): value is string =>
  typeof value === "string" && HEX_32.test(value);
const isBytes = (value: unknown): value is string =>
  typeof value === "string" && HEX_BYTES.test(value);
const isBytesOrEmpty = (value: unknown): value is string =>
  typeof value === "string" && HEX_BYTES_OR_EMPTY.test(value);
const isNullable = <T>(
  value: unknown,
  predicate: (candidate: unknown) => candidate is T,
): value is T | null => value === null || predicate(value);

const same = (left: unknown, right: unknown): boolean =>
  canonicalJson(left as CanonicalJson) ===
  canonicalJson(right as CanonicalJson);

const sameMarker = (
  left: DeploymentMarkerV1,
  right: DeploymentMarkerV1,
): boolean =>
  left.schemaVersion === right.schemaVersion &&
  left.manifestId === right.manifestId;

const parseMarker = (value: unknown): DeploymentMarkerV1 | null => {
  const record = exactRecord(value, ["schemaVersion", "manifestId"]);
  return record !== null &&
    record.schemaVersion === MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION &&
    isHex32(record.manifestId)
    ? Object.freeze({
        schemaVersion: MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
        manifestId: record.manifestId,
      })
    : null;
};

const sortedUniqueStrings = (
  value: unknown,
  maximum: number,
  predicate: (candidate: unknown) => candidate is string,
): readonly string[] | null => {
  const values = exactArray(value, maximum);
  if (values === null || values.some((entry) => !predicate(entry))) {
    return null;
  }
  const strings = values as readonly string[];
  if (
    strings.some((entry, index) => index > 0 && strings[index - 1]! >= entry)
  ) {
    return null;
  }
  return Object.freeze([...strings]);
};

const compareText = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

const policyWithoutDigest = (
  value: Omit<WatcherSettlementIndexerPolicyV1, "policyDigest">,
): Omit<WatcherSettlementIndexerPolicyV1, "policyDigest"> => value;

export const makeWatcherSettlementIndexerPolicyV1 = (input: {
  network: WatcherSettlementNetworkV1;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  hubOraclePolicyId: string;
  depositPolicyId: string;
  settlementPolicyId: string;
  settlementSpendScriptHash: string;
  reserveSpendScriptHash: string;
  payoutPolicyId: string;
  payoutSpendScriptHash: string;
  withdrawalPolicyId: string;
  withdrawalSpendScriptHash: string;
  activeOperatorPolicyId: string;
  retiredOperatorPolicyId: string;
  stateQueuePolicyId: string;
  settlementAddressHex: string;
  reserveAddressHex: string;
  payoutAddressHex: string;
  withdrawalAddressHex: string;
  bootstrapStoreDigest: string;
  deploymentTrustRootId: string;
  requiredFinalityDepth: string;
  maximumHistoryEntries?: string;
  maximumRollbackEntries?: string;
  maximumRetryAttempts?: string;
}): WatcherSettlementIndexerPolicyV1 | null => {
  const marker = parseMarker(input.deploymentMarker);
  const maximumHistoryEntries =
    input.maximumHistoryEntries ??
    WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.activeHistoryEntries.toString();
  const maximumRollbackEntries =
    input.maximumRollbackEntries ??
    WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.rollbackHistoryEntries.toString();
  const maximumRetryAttempts =
    input.maximumRetryAttempts ??
    WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.retryAttempts.toString();
  if (
    !NETWORKS.includes(input.network) ||
    !isHex32(input.releaseEvidenceDigest) ||
    !isHex32(input.bootstrapStoreDigest) ||
    !isHex32(input.deploymentTrustRootId) ||
    !isNatural(input.requiredFinalityDepth) ||
    BigInt(input.requiredFinalityDepth) < 1n ||
    BigInt(input.requiredFinalityDepth) >
      BigInt(
        WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.requiredFinalityDepthMaximum,
      ) ||
    marker === null ||
    ![
      input.hubOraclePolicyId,
      input.settlementPolicyId,
      input.depositPolicyId,
      input.settlementSpendScriptHash,
      input.reserveSpendScriptHash,
      input.payoutPolicyId,
      input.payoutSpendScriptHash,
      input.withdrawalPolicyId,
      input.withdrawalSpendScriptHash,
      input.activeOperatorPolicyId,
      input.retiredOperatorPolicyId,
      input.stateQueuePolicyId,
    ].every(isHex28) ||
    ![
      input.settlementAddressHex,
      input.reserveAddressHex,
      input.payoutAddressHex,
      input.withdrawalAddressHex,
    ].every(isBytes) ||
    ![
      maximumHistoryEntries,
      maximumRollbackEntries,
      maximumRetryAttempts,
    ].every(isNatural) ||
    BigInt(maximumHistoryEntries) < 1n ||
    BigInt(maximumHistoryEntries) >
      BigInt(WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.activeHistoryEntries) ||
    BigInt(maximumRollbackEntries) < 1n ||
    BigInt(maximumRollbackEntries) >
      BigInt(WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.rollbackHistoryEntries) ||
    BigInt(maximumRetryAttempts) < 1n ||
    BigInt(maximumRetryAttempts) >
      BigInt(WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.retryAttempts)
  ) {
    return null;
  }
  const canonical = Object.freeze({
    schemaVersion: WATCHER_SETTLEMENT_INDEXER_POLICY_V1_SCHEMA_VERSION,
    network: input.network,
    releaseEvidenceDigest: input.releaseEvidenceDigest,
    deploymentMarker: marker,
    hubOraclePolicyId: input.hubOraclePolicyId,
    depositPolicyId: input.depositPolicyId,
    settlementPolicyId: input.settlementPolicyId,
    settlementSpendScriptHash: input.settlementSpendScriptHash,
    reserveSpendScriptHash: input.reserveSpendScriptHash,
    payoutPolicyId: input.payoutPolicyId,
    payoutSpendScriptHash: input.payoutSpendScriptHash,
    withdrawalPolicyId: input.withdrawalPolicyId,
    withdrawalSpendScriptHash: input.withdrawalSpendScriptHash,
    activeOperatorPolicyId: input.activeOperatorPolicyId,
    retiredOperatorPolicyId: input.retiredOperatorPolicyId,
    stateQueuePolicyId: input.stateQueuePolicyId,
    settlementAddressHex: input.settlementAddressHex,
    reserveAddressHex: input.reserveAddressHex,
    payoutAddressHex: input.payoutAddressHex,
    withdrawalAddressHex: input.withdrawalAddressHex,
    bootstrapStoreDigest: input.bootstrapStoreDigest,
    deploymentTrustRootId: input.deploymentTrustRootId,
    requiredFinalityDepth: input.requiredFinalityDepth,
    maximumHistoryEntries,
    maximumRollbackEntries,
    maximumRetryAttempts,
  });
  return Object.freeze({
    ...canonical,
    policyDigest: sha256Canonical(policyWithoutDigest(canonical)),
  });
};

export const parseWatcherSettlementIndexerPolicyV1 = (
  value: unknown,
): WatcherSettlementIndexerPolicyV1 | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "network",
    "releaseEvidenceDigest",
    "deploymentMarker",
    "hubOraclePolicyId",
    "depositPolicyId",
    "settlementPolicyId",
    "settlementSpendScriptHash",
    "reserveSpendScriptHash",
    "payoutPolicyId",
    "payoutSpendScriptHash",
    "withdrawalPolicyId",
    "withdrawalSpendScriptHash",
    "activeOperatorPolicyId",
    "retiredOperatorPolicyId",
    "stateQueuePolicyId",
    "settlementAddressHex",
    "reserveAddressHex",
    "payoutAddressHex",
    "withdrawalAddressHex",
    "bootstrapStoreDigest",
    "deploymentTrustRootId",
    "requiredFinalityDepth",
    "maximumHistoryEntries",
    "maximumRollbackEntries",
    "maximumRetryAttempts",
    "policyDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_SETTLEMENT_INDEXER_POLICY_V1_SCHEMA_VERSION
  ) {
    return null;
  }
  const parsed = makeWatcherSettlementIndexerPolicyV1({
    network: record.network as WatcherSettlementNetworkV1,
    releaseEvidenceDigest: record.releaseEvidenceDigest as string,
    deploymentMarker: record.deploymentMarker as DeploymentMarkerV1,
    hubOraclePolicyId: record.hubOraclePolicyId as string,
    depositPolicyId: record.depositPolicyId as string,
    settlementPolicyId: record.settlementPolicyId as string,
    settlementSpendScriptHash: record.settlementSpendScriptHash as string,
    reserveSpendScriptHash: record.reserveSpendScriptHash as string,
    payoutPolicyId: record.payoutPolicyId as string,
    payoutSpendScriptHash: record.payoutSpendScriptHash as string,
    withdrawalPolicyId: record.withdrawalPolicyId as string,
    withdrawalSpendScriptHash: record.withdrawalSpendScriptHash as string,
    activeOperatorPolicyId: record.activeOperatorPolicyId as string,
    retiredOperatorPolicyId: record.retiredOperatorPolicyId as string,
    stateQueuePolicyId: record.stateQueuePolicyId as string,
    settlementAddressHex: record.settlementAddressHex as string,
    reserveAddressHex: record.reserveAddressHex as string,
    payoutAddressHex: record.payoutAddressHex as string,
    withdrawalAddressHex: record.withdrawalAddressHex as string,
    bootstrapStoreDigest: record.bootstrapStoreDigest as string,
    deploymentTrustRootId: record.deploymentTrustRootId as string,
    requiredFinalityDepth: record.requiredFinalityDepth as string,
    maximumHistoryEntries: record.maximumHistoryEntries as string,
    maximumRollbackEntries: record.maximumRollbackEntries as string,
    maximumRetryAttempts: record.maximumRetryAttempts as string,
  });
  return parsed !== null && parsed.policyDigest === record.policyDigest
    ? parsed
    : null;
};

type DecodedSettlementDatum = Readonly<{
  resolutionClaim: Readonly<{
    resolutionTime: string;
    operatorVkey: string;
  }> | null;
}>;

const canonicalPlutusData = <T>(cborHex: string, schema: unknown): T | null => {
  try {
    const decoded = Data.from(cborHex, schema as never) as T;
    const encoded = Data.to(decoded as never, schema as never);
    return encoded === cborHex ||
      CML.PlutusData.from_cbor_hex(encoded).to_canonical_cbor_hex() === cborHex
      ? decoded
      : null;
  } catch {
    return null;
  }
};

const decodeSettlementDatum = (
  cborHex: string,
): DecodedSettlementDatum | null => {
  const decoded = canonicalPlutusData<{
    resolution_claim: {
      resolution_time: bigint;
      operator: string;
    } | null;
  }>(cborHex, SettlementDatum);
  if (decoded === null) {
    return null;
  }
  const claim = decoded.resolution_claim;
  if (
    claim !== null &&
    (typeof claim.resolution_time !== "bigint" ||
      claim.resolution_time < 0n ||
      !isHex28(claim.operator))
  ) {
    return null;
  }
  return Object.freeze({
    resolutionClaim:
      claim === null
        ? null
        : Object.freeze({
            resolutionTime: claim.resolution_time.toString(),
            operatorVkey: claim.operator,
          }),
  });
};

const canonicalDatumKind = (
  role: WatcherSettlementResourceRoleV1,
  datumCborHex: string | null,
): WatcherSettlementResourceV1["datumKind"] | null => {
  if (role === "reserve") {
    return datumCborHex === null ? "no_datum" : null;
  }
  if (datumCborHex === null) {
    return null;
  }
  if (role === "settlement") {
    return decodeSettlementDatum(datumCborHex) === null ? null : "settlement";
  }
  if (role === "payout") {
    return canonicalPlutusData(datumCborHex, PayoutDatum) === null
      ? null
      : "payout";
  }
  return canonicalPlutusData(datumCborHex, WithdrawalOrderDatum) === null
    ? null
    : "withdrawal";
};

const canonicalAssets = (
  value: ReturnType<CML.TransactionOutput["amount"]>,
): readonly WatcherSettlementAssetV1[] => {
  const assets: WatcherSettlementAssetV1[] = [];
  const multiAsset = value.multi_asset();
  const policies = multiAsset.keys();
  for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
    const policy = policies.get(policyIndex);
    const policyId = policy.to_hex();
    const policyAssets = multiAsset.get_assets(policy);
    if (policyAssets === undefined) {
      throw new Error("Cardano value policy disappeared during decoding");
    }
    const names = policyAssets.keys();
    for (let assetIndex = 0; assetIndex < names.len(); assetIndex += 1) {
      const name = names.get(assetIndex);
      const quantity = policyAssets.get(name);
      if (quantity === undefined || quantity <= 0n) {
        throw new Error("Cardano output contains a non-positive asset");
      }
      assets.push({
        policyId,
        assetName: name.to_hex(),
        quantity: quantity.toString(),
      });
    }
  }
  return Object.freeze(
    assets.sort((left, right) =>
      compareText(
        `${left.policyId}.${left.assetName}`,
        `${right.policyId}.${right.assetName}`,
      ),
    ),
  );
};

const identityForRole = (
  policy: WatcherSettlementIndexerPolicyV1,
  role: WatcherSettlementResourceRoleV1,
  assets: readonly WatcherSettlementAssetV1[],
): Readonly<{ policyId: string; assetName: string }> | null => {
  const expectedPolicy =
    role === "settlement"
      ? policy.settlementPolicyId
      : role === "payout"
        ? policy.payoutPolicyId
        : role === "withdrawal"
          ? policy.withdrawalPolicyId
          : null;
  if (expectedPolicy === null) {
    return assets.some(({ policyId }) =>
      [
        policy.settlementPolicyId,
        policy.payoutPolicyId,
        policy.withdrawalPolicyId,
      ].includes(policyId),
    )
      ? null
      : Object.freeze({ policyId: "", assetName: "" });
  }
  const matches = assets.filter(({ policyId }) => policyId === expectedPolicy);
  return matches.length === 1 && matches[0]?.quantity === "1"
    ? Object.freeze({
        policyId: expectedPolicy,
        assetName: matches[0].assetName,
      })
    : null;
};

const roleAddress = (
  policy: WatcherSettlementIndexerPolicyV1,
  role: WatcherSettlementResourceRoleV1,
): Readonly<{ addressHex: string; scriptHash: string }> => {
  switch (role) {
    case "settlement":
      return {
        addressHex: policy.settlementAddressHex,
        scriptHash: policy.settlementSpendScriptHash,
      };
    case "reserve":
      return {
        addressHex: policy.reserveAddressHex,
        scriptHash: policy.reserveSpendScriptHash,
      };
    case "payout":
      return {
        addressHex: policy.payoutAddressHex,
        scriptHash: policy.payoutSpendScriptHash,
      };
    case "withdrawal":
      return {
        addressHex: policy.withdrawalAddressHex,
        scriptHash: policy.withdrawalSpendScriptHash,
      };
  }
};

const decodeResource = (
  policy: WatcherSettlementIndexerPolicyV1,
  durable: WatcherProtocolUtxoV1,
): WatcherSettlementResourceV1 | null => {
  if (!TARGET_ROLES.has(durable.role as WatcherSettlementResourceRoleV1)) {
    return null;
  }
  const role = durable.role as WatcherSettlementResourceRoleV1;
  try {
    const output = CML.TransactionOutput.from_cbor_hex(durable.output.cborHex);
    if (output.to_canonical_cbor_hex() !== durable.output.cborHex) {
      return null;
    }
    const addressHex = output.address().to_hex();
    const paymentScriptHash = output
      .address()
      .payment_cred()
      ?.as_script()
      ?.to_hex();
    const expected = roleAddress(policy, role);
    if (
      paymentScriptHash === undefined ||
      addressHex !== expected.addressHex ||
      paymentScriptHash !== expected.scriptHash ||
      output.script_ref() !== undefined ||
      output.address().network_id() !== (policy.network === "Mainnet" ? 1 : 0)
    ) {
      return null;
    }
    const amount = output.amount();
    const assets = canonicalAssets(amount);
    const identity = identityForRole(policy, role, assets);
    if (identity === null) {
      return null;
    }
    const datumOption = output.datum();
    const inlineDatum = datumOption?.as_datum();
    if (
      datumOption !== undefined &&
      (inlineDatum === undefined || datumOption.as_hash() !== undefined)
    ) {
      return null;
    }
    const datumCborHex = inlineDatum?.to_canonical_cbor_hex() ?? null;
    const datumKind = canonicalDatumKind(role, datumCborHex);
    if (datumKind === null) {
      return null;
    }
    const datumSha256 =
      datumCborHex === null
        ? null
        : sha256Bytes(Buffer.from(datumCborHex, "hex"));
    const canonical = Object.freeze({
      outRef: durable.outRef,
      role,
      addressHex,
      paymentScriptHash,
      outputSha256: durable.output.sha256,
      datumSha256,
      datumCborHex,
      lovelace: amount.coin().toString(),
      assets,
      identityPolicyId:
        identity.policyId.length === 0 ? null : identity.policyId,
      identityAssetName:
        identity.assetName.length === 0 ? null : identity.assetName,
      datumKind,
    });
    return Object.freeze({
      ...canonical,
      semanticDigest: sha256Canonical(canonical),
    });
  } catch {
    return null;
  }
};

export const makeWatcherSettlementResourceFromProtocolUtxoV1 = (
  policyValue: unknown,
  durable: WatcherProtocolUtxoV1,
): WatcherSettlementResourceV1 | null => {
  const policy = parseWatcherSettlementIndexerPolicyV1(policyValue);
  return policy === null ? null : decodeResource(policy, durable);
};

const parseAsset = (value: unknown): WatcherSettlementAssetV1 | null => {
  const record = exactRecord(value, ["policyId", "assetName", "quantity"]);
  return record !== null &&
    isHex28(record.policyId) &&
    isBytesOrEmpty(record.assetName) &&
    isNatural(record.quantity) &&
    BigInt(record.quantity) > 0n
    ? Object.freeze({
        policyId: record.policyId,
        assetName: record.assetName,
        quantity: record.quantity,
      })
    : null;
};

const parseMintAsset = (
  value: unknown,
): WatcherSettlementMintAssetV1 | null => {
  const record = exactRecord(value, ["policyId", "assetName", "quantity"]);
  return record !== null &&
    isHex28(record.policyId) &&
    isBytesOrEmpty(record.assetName) &&
    typeof record.quantity === "string" &&
    NON_ZERO_INTEGER.test(record.quantity)
    ? Object.freeze({
        policyId: record.policyId,
        assetName: record.assetName,
        quantity: record.quantity,
      })
    : null;
};

const parseResource = (value: unknown): WatcherSettlementResourceV1 | null => {
  const record = exactRecord(value, [
    "outRef",
    "role",
    "addressHex",
    "paymentScriptHash",
    "outputSha256",
    "datumSha256",
    "datumCborHex",
    "lovelace",
    "assets",
    "identityPolicyId",
    "identityAssetName",
    "datumKind",
    "semanticDigest",
  ]);
  const rawAssets =
    record === null
      ? null
      : exactArray(
          record.assets,
          WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.resources,
        );
  if (record === null || rawAssets === null) {
    return null;
  }
  const assets = rawAssets.map(parseAsset);
  const roles: readonly WatcherSettlementResourceRoleV1[] = [
    "settlement",
    "reserve",
    "payout",
    "withdrawal",
  ];
  const datumKinds: readonly WatcherSettlementResourceV1["datumKind"][] = [
    "settlement",
    "payout",
    "withdrawal",
    "no_datum",
  ];
  if (
    typeof record.outRef !== "string" ||
    !/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(record.outRef) ||
    !roles.includes(record.role as WatcherSettlementResourceRoleV1) ||
    !isBytes(record.addressHex) ||
    !isHex28(record.paymentScriptHash) ||
    !isHex32(record.outputSha256) ||
    !isNullable(record.datumSha256, isHex32) ||
    !isNullable(record.datumCborHex, isBytes) ||
    !isNatural(record.lovelace) ||
    assets.some((asset) => asset === null) ||
    !isNullable(record.identityPolicyId, isHex28) ||
    !isNullable(record.identityAssetName, isBytesOrEmpty) ||
    !datumKinds.includes(
      record.datumKind as WatcherSettlementResourceV1["datumKind"],
    ) ||
    !isHex32(record.semanticDigest)
  ) {
    return null;
  }
  const canonical = Object.freeze({
    outRef: record.outRef,
    role: record.role as WatcherSettlementResourceRoleV1,
    addressHex: record.addressHex,
    paymentScriptHash: record.paymentScriptHash,
    outputSha256: record.outputSha256,
    datumSha256: record.datumSha256,
    datumCborHex: record.datumCborHex,
    lovelace: record.lovelace,
    assets: Object.freeze(
      (assets as WatcherSettlementAssetV1[]).sort((left, right) =>
        compareText(
          `${left.policyId}.${left.assetName}`,
          `${right.policyId}.${right.assetName}`,
        ),
      ),
    ),
    identityPolicyId: record.identityPolicyId,
    identityAssetName: record.identityAssetName,
    datumKind: record.datumKind as WatcherSettlementResourceV1["datumKind"],
  });
  if (
    (canonical.datumCborHex === null) !== (canonical.datumSha256 === null) ||
    (canonical.identityPolicyId === null) !==
      (canonical.identityAssetName === null) ||
    canonical.assets.some(
      (asset, index) =>
        index > 0 &&
        `${canonical.assets[index - 1]!.policyId}.${canonical.assets[index - 1]!.assetName}` >=
          `${asset.policyId}.${asset.assetName}`,
    ) ||
    sha256Canonical(canonical) !== record.semanticDigest
  ) {
    return null;
  }
  return Object.freeze({
    ...canonical,
    semanticDigest: record.semanticDigest,
  });
};

const subjectWithoutDigest = (
  value: Omit<WatcherSettlementSubjectV1, "subjectDigest">,
): Omit<WatcherSettlementSubjectV1, "subjectDigest"> => value;

export const makeWatcherSettlementSubjectV1 = (
  input: Omit<WatcherSettlementSubjectV1, "subjectDigest">,
): WatcherSettlementSubjectV1 | null => {
  const kinds: readonly WatcherSettlementSubjectV1["subjectKind"][] = [
    "settlement",
    "resolution_claim",
    "reserve",
    "withdrawal",
    "payout",
  ];
  const statuses: readonly WatcherSettlementSubjectStatusV1[] = [
    "open",
    "claimed",
    "disproved",
    "resolved",
    "reserve_active",
    "reserve_depleted",
    "withdrawal_pending",
    "payout_initializing",
    "payout_funding",
    "payout_funded",
    "retrying",
    "paid",
    "refunded",
    "stuck",
    "invalid",
  ];
  if (
    typeof input.subjectId !== "string" ||
    input.subjectId.length < 1 ||
    input.subjectId.length > 256 ||
    !kinds.includes(input.subjectKind) ||
    !statuses.includes(input.status) ||
    !isNullable(
      input.resourceOutRef,
      (candidate): candidate is string =>
        typeof candidate === "string" &&
        /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(candidate),
    ) ||
    !isNullable(
      input.relatedSubjectId,
      (candidate): candidate is string =>
        typeof candidate === "string" &&
        candidate.length >= 1 &&
        candidate.length <= 256,
    ) ||
    !isNullable(input.resolutionTime, isNatural) ||
    !isNullable(input.operatorVkey, isHex28) ||
    !isNatural(input.attempt) ||
    BigInt(input.attempt) >
      BigInt(WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.retryAttempts) ||
    !isNullable(input.terminalTransactionHash, isHex32) ||
    !isNullable(
      input.failureCode,
      (candidate): candidate is string =>
        typeof candidate === "string" && FAILURE_CODE.test(candidate),
    ) ||
    (input.resolutionTime === null) !== (input.operatorVkey === null) ||
    (["paid", "refunded", "resolved"].includes(input.status) &&
      input.terminalTransactionHash === null) ||
    (!["retrying", "stuck", "invalid"].includes(input.status) &&
      input.failureCode !== null)
  ) {
    return null;
  }
  const canonical = Object.freeze({ ...input });
  return Object.freeze({
    ...canonical,
    subjectDigest: sha256Canonical(subjectWithoutDigest(canonical)),
  });
};

const parseSubject = (value: unknown): WatcherSettlementSubjectV1 | null => {
  const record = exactRecord(value, [
    "subjectId",
    "subjectKind",
    "status",
    "resourceOutRef",
    "relatedSubjectId",
    "resolutionTime",
    "operatorVkey",
    "attempt",
    "terminalTransactionHash",
    "failureCode",
    "subjectDigest",
  ]);
  if (record === null || !isHex32(record.subjectDigest)) {
    return null;
  }
  const parsed = makeWatcherSettlementSubjectV1({
    subjectId: record.subjectId as string,
    subjectKind:
      record.subjectKind as WatcherSettlementSubjectV1["subjectKind"],
    status: record.status as WatcherSettlementSubjectStatusV1,
    resourceOutRef: record.resourceOutRef as string | null,
    relatedSubjectId: record.relatedSubjectId as string | null,
    resolutionTime: record.resolutionTime as string | null,
    operatorVkey: record.operatorVkey as string | null,
    attempt: record.attempt as string,
    terminalTransactionHash: record.terminalTransactionHash as string | null,
    failureCode: record.failureCode as string | null,
  });
  return parsed !== null && parsed.subjectDigest === record.subjectDigest
    ? parsed
    : null;
};

const snapshotWithoutDigest = (
  value: Omit<WatcherSettlementSnapshotV1, "snapshotDigest">,
): Omit<WatcherSettlementSnapshotV1, "snapshotDigest"> => value;

export const makeWatcherSettlementSnapshotV1 = (input: {
  resources: readonly WatcherSettlementResourceV1[];
  subjects: readonly WatcherSettlementSubjectV1[];
}): WatcherSettlementSnapshotV1 | null => {
  if (
    input.resources.length > WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.resources ||
    input.subjects.length > WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.subjects
  ) {
    return null;
  }
  const resources = input.resources.map(parseResource);
  const subjects = input.subjects.map(parseSubject);
  if (
    resources.some((entry) => entry === null) ||
    subjects.some((entry) => entry === null)
  ) {
    return null;
  }
  const sortedResources = (resources as WatcherSettlementResourceV1[]).sort(
    (left, right) => compareText(left.outRef, right.outRef),
  );
  const sortedSubjects = (subjects as WatcherSettlementSubjectV1[]).sort(
    (left, right) =>
      compareText(
        `${left.subjectKind}:${left.subjectId}`,
        `${right.subjectKind}:${right.subjectId}`,
      ),
  );
  if (
    sortedResources.some(
      (entry, index) =>
        index > 0 && sortedResources[index - 1]!.outRef === entry.outRef,
    ) ||
    sortedSubjects.some(
      (entry, index) =>
        index > 0 &&
        sortedSubjects[index - 1]!.subjectKind === entry.subjectKind &&
        sortedSubjects[index - 1]!.subjectId === entry.subjectId,
    ) ||
    sortedSubjects.some(
      ({ resourceOutRef }) =>
        resourceOutRef !== null &&
        !sortedResources.some(({ outRef }) => outRef === resourceOutRef),
    )
  ) {
    return null;
  }
  const canonical = Object.freeze({
    schemaVersion: WATCHER_SETTLEMENT_SNAPSHOT_V1_SCHEMA_VERSION,
    resources: Object.freeze(sortedResources),
    subjects: Object.freeze(sortedSubjects),
  });
  return Object.freeze({
    ...canonical,
    snapshotDigest: sha256Canonical(snapshotWithoutDigest(canonical)),
  });
};

export const parseWatcherSettlementSnapshotV1 = (
  value: unknown,
): WatcherSettlementSnapshotV1 | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "resources",
    "subjects",
    "snapshotDigest",
  ]);
  const resources =
    record === null
      ? null
      : exactArray(
          record.resources,
          WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.resources,
        );
  const subjects =
    record === null
      ? null
      : exactArray(
          record.subjects,
          WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.subjects,
        );
  if (
    record === null ||
    record.schemaVersion !== WATCHER_SETTLEMENT_SNAPSHOT_V1_SCHEMA_VERSION ||
    resources === null ||
    subjects === null ||
    !isHex32(record.snapshotDigest)
  ) {
    return null;
  }
  const parsed = makeWatcherSettlementSnapshotV1({
    resources: resources as WatcherSettlementResourceV1[],
    subjects: subjects as WatcherSettlementSubjectV1[],
  });
  return parsed !== null && parsed.snapshotDigest === record.snapshotDigest
    ? parsed
    : null;
};

const resourcesFromStore = (
  policy: WatcherSettlementIndexerPolicyV1,
  store: WatcherDurableStoreV1,
): readonly WatcherSettlementResourceV1[] | null => {
  const resources: WatcherSettlementResourceV1[] = [];
  for (const durable of store.protocolUtxos) {
    if (!TARGET_ROLES.has(durable.role as WatcherSettlementResourceRoleV1)) {
      continue;
    }
    const decoded = decodeResource(policy, durable);
    if (decoded === null) {
      return null;
    }
    resources.push(decoded);
  }
  resources.sort((left, right) => compareText(left.outRef, right.outRef));
  return Object.freeze(resources);
};

const resourcesMatchStore = (
  policy: WatcherSettlementIndexerPolicyV1,
  snapshot: WatcherSettlementSnapshotV1,
  store: WatcherDurableStoreV1,
): boolean => {
  const authoritative = resourcesFromStore(policy, store);
  return authoritative !== null && same(authoritative, snapshot.resources);
};

const REDEEMER_SCHEMAS = [
  "settlement_spend",
  "settlement_mint",
  "reserve_spend",
  "payout_spend",
  "payout_mint",
  "withdrawal_spend",
  "deposit_spend",
  "user_event_mint",
  "event_witness",
  "membership_withdrawal",
  "active_operator_spend",
  "active_operator_mint",
  "retired_operator_mint",
  "state_queue_mint",
] as const;
const REDEEMER_CONSTRUCTORS = [
  "AttachResolutionClaim",
  "DisproveResolutionClaim",
  "Resolve",
  "Spawn",
  "Remove",
  "Spend",
  "AddFunds",
  "ConcludeWithdrawal",
  "MintPayout",
  "BurnPayout",
  "InitializePayout",
  "Refund",
  "DepositSpend",
  "AuthenticateEvent",
  "BurnEventNFT",
  "MintOrBurn",
  "RegisterToProveNotRegistered",
  "UnregisterToProveNotRegistered",
  "MembershipProof",
  "ListStateTransition",
  "UpdateBondHoldNewSettlement",
  "SlashOperator",
  "MergeToConfirmedStateV1",
] as const;

const parseTransition = (
  value: unknown,
): WatcherSettlementTransitionV1 | null => {
  const record = exactRecord(value, [
    "kind",
    "subjectId",
    "relatedSubjectId",
    "consumedOutRefs",
    "producedOutRefs",
    "requiredRedeemers",
    "exactMint",
    "failureCode",
    "retryAttempt",
  ]);
  if (
    record === null ||
    !WATCHER_SETTLEMENT_TRANSITION_KINDS_V1.includes(
      record.kind as WatcherSettlementTransitionKindV1,
    ) ||
    !isNullable(
      record.subjectId,
      (candidate): candidate is string =>
        typeof candidate === "string" &&
        candidate.length >= 1 &&
        candidate.length <= 256,
    ) ||
    !isNullable(
      record.relatedSubjectId,
      (candidate): candidate is string =>
        typeof candidate === "string" &&
        candidate.length >= 1 &&
        candidate.length <= 256,
    ) ||
    !isNullable(
      record.failureCode,
      (candidate): candidate is string =>
        typeof candidate === "string" && FAILURE_CODE.test(candidate),
    ) ||
    !isNullable(record.retryAttempt, isNatural)
  ) {
    return null;
  }
  const consumed = sortedUniqueStrings(
    record.consumedOutRefs,
    WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.resources,
    (candidate): candidate is string =>
      typeof candidate === "string" &&
      /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(candidate),
  );
  const produced = sortedUniqueStrings(
    record.producedOutRefs,
    WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.resources,
    (candidate): candidate is string =>
      typeof candidate === "string" &&
      /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(candidate),
  );
  const rawRedeemers = exactArray(record.requiredRedeemers, 16);
  const rawMint = exactArray(record.exactMint, 64);
  if (
    consumed === null ||
    produced === null ||
    rawRedeemers === null ||
    rawMint === null
  ) {
    return null;
  }
  const redeemers: WatcherSettlementTransitionV1["requiredRedeemers"][number][] =
    [];
  for (const raw of rawRedeemers) {
    const redeemer = exactRecord(raw, [
      "purpose",
      "index",
      "schema",
      "constructor",
    ]);
    if (
      redeemer === null ||
      !["spend", "mint", "certificate", "withdrawal"].includes(
        redeemer.purpose as string,
      ) ||
      !isNatural(redeemer.index) ||
      !REDEEMER_SCHEMAS.includes(
        redeemer.schema as (typeof REDEEMER_SCHEMAS)[number],
      ) ||
      !REDEEMER_CONSTRUCTORS.includes(
        redeemer[
          "constructor"
        ] as unknown as (typeof REDEEMER_CONSTRUCTORS)[number],
      )
    ) {
      return null;
    }
    redeemers.push({
      purpose: redeemer.purpose as
        | "spend"
        | "mint"
        | "certificate"
        | "withdrawal",
      index: redeemer.index,
      schema: redeemer.schema as (typeof REDEEMER_SCHEMAS)[number],
      constructor: redeemer[
        "constructor"
      ] as unknown as (typeof REDEEMER_CONSTRUCTORS)[number],
    });
  }
  redeemers.sort((left, right) =>
    compareText(
      `${left.purpose}:${left.index}`,
      `${right.purpose}:${right.index}`,
    ),
  );
  if (
    redeemers.some(
      (entry, index) =>
        index > 0 &&
        entry.purpose === redeemers[index - 1]!.purpose &&
        entry.index === redeemers[index - 1]!.index,
    )
  ) {
    return null;
  }
  const exactMint = rawMint.map(parseMintAsset);
  if (exactMint.some((entry) => entry === null)) {
    return null;
  }
  const sortedMint = (exactMint as WatcherSettlementMintAssetV1[]).sort(
    (left, right) =>
      compareText(
        `${left.policyId}.${left.assetName}`,
        `${right.policyId}.${right.assetName}`,
      ),
  );
  if (
    sortedMint.some(
      (entry, index) =>
        index > 0 &&
        entry.policyId === sortedMint[index - 1]!.policyId &&
        entry.assetName === sortedMint[index - 1]!.assetName,
    )
  ) {
    return null;
  }
  return Object.freeze({
    kind: record.kind as WatcherSettlementTransitionKindV1,
    subjectId: record.subjectId as string | null,
    relatedSubjectId: record.relatedSubjectId as string | null,
    consumedOutRefs: consumed,
    producedOutRefs: produced,
    requiredRedeemers: Object.freeze(redeemers),
    exactMint: Object.freeze(sortedMint),
    failureCode: record.failureCode as string | null,
    retryAttempt: record.retryAttempt as string | null,
  });
};

const observationWithoutDigest = (
  value: Omit<WatcherSettlementObservationV1, "observationDigest">,
): Omit<WatcherSettlementObservationV1, "observationDigest"> => value;

export const makeWatcherSettlementObservationV1 = (
  input: Omit<
    WatcherSettlementObservationV1,
    "schemaVersion" | "observationDigest"
  >,
): WatcherSettlementObservationV1 | null => {
  const marker = parseMarker(input.deploymentMarker);
  const transition = parseTransition(input.transition);
  const snapshot = parseWatcherSettlementSnapshotV1(input.snapshot);
  if (
    !isHex32(input.policyDigest) ||
    !NETWORKS.includes(input.network) ||
    !isHex32(input.releaseEvidenceDigest) ||
    marker === null ||
    !isHex32(input.pointDigest) ||
    !isHex32(input.chainPointId) ||
    !isHex32(input.blockHash) ||
    !isNatural(input.slot) ||
    !isNatural(input.blockNo) ||
    !isNullable(input.transactionHash, isHex32) ||
    !isHex32(input.sourceObservationDigest) ||
    !isHex32(input.durableStoreDigest) ||
    !isNullable(input.predecessorStateDigest, isHex32) ||
    transition === null ||
    snapshot === null ||
    (transition.kind === "rollback" ||
      transition.kind === "retry" ||
      transition.kind === "mark_stuck" ||
      transition.kind === "mark_invalid") !==
      (input.transactionHash === null)
  ) {
    return null;
  }
  const canonical = Object.freeze({
    schemaVersion: WATCHER_SETTLEMENT_OBSERVATION_V1_SCHEMA_VERSION,
    policyDigest: input.policyDigest,
    network: input.network,
    releaseEvidenceDigest: input.releaseEvidenceDigest,
    deploymentMarker: marker,
    pointDigest: input.pointDigest,
    chainPointId: input.chainPointId,
    blockHash: input.blockHash,
    slot: input.slot,
    blockNo: input.blockNo,
    transactionHash: input.transactionHash,
    sourceObservationDigest: input.sourceObservationDigest,
    durableStoreDigest: input.durableStoreDigest,
    predecessorStateDigest: input.predecessorStateDigest,
    transition,
    snapshot,
  });
  return Object.freeze({
    ...canonical,
    observationDigest: sha256Canonical(observationWithoutDigest(canonical)),
  });
};

export const parseWatcherSettlementObservationV1 = (
  value: unknown,
): WatcherSettlementObservationV1 | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "policyDigest",
    "network",
    "releaseEvidenceDigest",
    "deploymentMarker",
    "pointDigest",
    "chainPointId",
    "blockHash",
    "slot",
    "blockNo",
    "transactionHash",
    "sourceObservationDigest",
    "durableStoreDigest",
    "predecessorStateDigest",
    "transition",
    "snapshot",
    "observationDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_SETTLEMENT_OBSERVATION_V1_SCHEMA_VERSION ||
    !isHex32(record.observationDigest)
  ) {
    return null;
  }
  const parsed = makeWatcherSettlementObservationV1({
    policyDigest: record.policyDigest as string,
    network: record.network as WatcherSettlementNetworkV1,
    releaseEvidenceDigest: record.releaseEvidenceDigest as string,
    deploymentMarker: record.deploymentMarker as DeploymentMarkerV1,
    pointDigest: record.pointDigest as string,
    chainPointId: record.chainPointId as string,
    blockHash: record.blockHash as string,
    slot: record.slot as string,
    blockNo: record.blockNo as string,
    transactionHash: record.transactionHash as string | null,
    sourceObservationDigest: record.sourceObservationDigest as string,
    durableStoreDigest: record.durableStoreDigest as string,
    predecessorStateDigest: record.predecessorStateDigest as string | null,
    transition: record.transition as WatcherSettlementTransitionV1,
    snapshot: record.snapshot as WatcherSettlementSnapshotV1,
  });
  return parsed !== null &&
    parsed.observationDigest === record.observationDigest
    ? parsed
    : null;
};

type VerifiedPublicContext = Readonly<{
  block: WatcherNormalizedL1BlockV1;
  lineageBlocks: readonly WatcherNormalizedL1BlockV1[];
  sourceStore: WatcherDurableStoreV1;
  store: WatcherDurableStoreV1;
  transaction: WatcherL1TransactionV1 | null;
  deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
  finalityPolicy: WatcherFinalityPolicyV1 | null;
  finalityResult: WatcherFinalityResultV1 | null;
  publicContextDigest: string;
  rollbackAuthority: WatcherSettlementPublicContextV1["rollbackAuthority"];
  restartContexts: WatcherSettlementPublicContextV1["restartContexts"];
  restartRollbackContexts: WatcherSettlementPublicContextV1["restartRollbackContexts"];
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
}>;

const parseDeploymentAuthority = (
  value: unknown,
): WatcherSettlementDeploymentAuthorityV1 | null => {
  const record = exactRecord(value, [
    "signedIdentity",
    "policy",
    "trustRoots",
    "result",
  ]);
  return record !== null && Array.isArray(record.trustRoots)
    ? Object.freeze({
        signedIdentity: record.signedIdentity,
        policy: record.policy as WatcherDeploymentIdentityPolicyV1,
        trustRoots:
          record.trustRoots as readonly WatcherDeploymentTrustRootV1[],
        result: record.result as VerifiedWatcherDeploymentIdentityV1,
      })
    : null;
};

const parseFinalityAuthority = (
  value: unknown,
): WatcherSettlementFinalityAuthorityV1 | null => {
  const record = exactRecord(value, [
    "policy",
    "lineage",
    "previousState",
    "observations",
    "consistency",
    "result",
  ]);
  if (
    record === null ||
    !Array.isArray(record.lineage) ||
    record.lineage.length >
      WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.finalityLineageSteps ||
    !Array.isArray(record.observations) ||
    record.observations.length >
      WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.observationsPerFinalityStep
  ) {
    return null;
  }
  const lineage: {
    observations: readonly {
      authenticatedProvider: unknown;
      l1Observation: unknown;
    }[];
    consistency: unknown;
    result: unknown;
  }[] = [];
  for (const candidate of record.lineage) {
    const step = exactRecord(candidate, [
      "observations",
      "consistency",
      "result",
    ]);
    if (
      step === null ||
      !Array.isArray(step.observations) ||
      step.observations.length >
        WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.observationsPerFinalityStep
    ) {
      return null;
    }
    const stepObservations: {
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
      stepObservations.push({
        authenticatedProvider: observation.authenticatedProvider,
        l1Observation: observation.l1Observation,
      });
    }
    lineage.push({
      observations: Object.freeze(stepObservations),
      consistency: step.consistency,
      result: step.result,
    });
  }
  const observations: {
    authenticatedProvider: unknown;
    l1Observation: unknown;
  }[] = [];
  for (const candidate of record.observations) {
    const observation = exactRecord(candidate, [
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
  return Object.freeze({
    policy: record.policy,
    lineage: Object.freeze(lineage),
    previousState: record.previousState,
    observations: Object.freeze(observations),
    consistency: record.consistency,
    result: record.result,
  });
};

const policyAddressesMatchDeployment = (
  policy: WatcherSettlementIndexerPolicyV1,
): boolean => {
  const expected = [
    [policy.settlementAddressHex, policy.settlementSpendScriptHash],
    [policy.reserveAddressHex, policy.reserveSpendScriptHash],
    [policy.payoutAddressHex, policy.payoutSpendScriptHash],
    [policy.withdrawalAddressHex, policy.withdrawalSpendScriptHash],
  ] as const;
  try {
    return expected.every(([addressHex, scriptHash]) => {
      const address = CML.Address.from_hex(addressHex);
      return (
        address.network_id() === (policy.network === "Mainnet" ? 1 : 0) &&
        address.payment_cred()?.as_script()?.to_hex() === scriptHash
      );
    });
  } catch {
    return false;
  }
};

const verifyDeploymentAuthority = (
  policy: WatcherSettlementIndexerPolicyV1,
  authority: WatcherSettlementDeploymentAuthorityV1,
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
      sameMarker(verified.durableMarker, policy.deploymentMarker) &&
      applied.hubOracleMint === policy.hubOraclePolicyId &&
      applied.depositMint === policy.depositPolicyId &&
      applied.settlementMint === policy.settlementPolicyId &&
      applied.settlementSpend === policy.settlementSpendScriptHash &&
      applied.reserveSpend === policy.reserveSpendScriptHash &&
      applied.payoutMint === policy.payoutPolicyId &&
      applied.payoutSpend === policy.payoutSpendScriptHash &&
      applied.withdrawalMint === policy.withdrawalPolicyId &&
      applied.withdrawalSpend === policy.withdrawalSpendScriptHash &&
      applied.activeOperatorsMint === policy.activeOperatorPolicyId &&
      applied.retiredOperatorsMint === policy.retiredOperatorPolicyId &&
      applied.stateQueueMint === policy.stateQueuePolicyId &&
      policyAddressesMatchDeployment(policy)
      ? verified
      : null;
  } catch {
    return null;
  }
};

const canonicalSuccessor = (
  prior: WatcherSettlementObservationV1,
  current: WatcherSettlementObservationV1,
  next: WatcherNormalizedL1BlockV1,
  lineage: readonly WatcherNormalizedL1BlockV1[],
): boolean => {
  if (next.chainPoint.pointDigest === prior.pointDigest) {
    if (
      next.chainPoint.blockHash === prior.blockHash &&
      next.chainPoint.slot === prior.slot &&
      next.chainPoint.blockNo === prior.blockNo
    ) {
      if (current.transactionHash === null) {
        return false;
      }
      if (prior.transactionHash === null) {
        const currentIndex = next.transactions.findIndex(
          ({ txHash }) => txHash === current.transactionHash,
        );
        return (
          prior.transition.kind === "rollback" &&
          currentIndex >= 0 &&
          next.transactions[currentIndex]?.transactionIndex ===
            currentIndex.toString()
        );
      }
      const priorIndex = next.transactions.findIndex(
        ({ txHash }) => txHash === prior.transactionHash,
      );
      const currentIndex = next.transactions.findIndex(
        ({ txHash }) => txHash === current.transactionHash,
      );
      return (
        priorIndex >= 0 &&
        currentIndex > priorIndex &&
        next.transactions[priorIndex]?.transactionIndex ===
          priorIndex.toString() &&
        next.transactions[currentIndex]?.transactionIndex ===
          currentIndex.toString()
      );
    }
    return false;
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

const verifyFinalityAuthority = (
  policy: WatcherSettlementIndexerPolicyV1,
  block: WatcherNormalizedL1BlockV1,
  authority: WatcherSettlementFinalityAuthorityV1,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): Readonly<{
  policy: WatcherFinalityPolicyV1;
  result: WatcherFinalityResultV1;
  lineageBlocks: readonly WatcherNormalizedL1BlockV1[];
}> | null => {
  const finalityPolicy = parseWatcherFinalityPolicyV1(authority.policy);
  if (
    finalityPolicy === null ||
    finalityPolicy.network !== policy.network ||
    finalityPolicy.releaseEvidenceDigest !== policy.releaseEvidenceDigest ||
    !sameMarker(finalityPolicy.deploymentMarker, policy.deploymentMarker) ||
    finalityPolicy.confirmationDepth !== policy.requiredFinalityDepth
  ) {
    return null;
  }
  try {
    if (
      finalityPolicy.sourceMode === "local_node" &&
      (finalityPolicy.authorityNodeId === null ||
        finalityPolicy.authorityGenesisIdentitySha256 === null)
    ) {
      return null;
    }
    let replayedState: unknown = null;
    const lineageBlocks: WatcherNormalizedL1BlockV1[] = [];
    for (const step of authority.lineage) {
      const normalizedStep = step.observations.map(
        ({ authenticatedProvider, l1Observation }) => {
          const matching = transportAttestations.filter((attestation) => {
            const details = watcherL1TransportAttestationDetailsV1(attestation);
            return (
              details !== null && same(details.provider, authenticatedProvider)
            );
          });
          if (matching.length !== 1) {
            throw new Error(
              "missing or ambiguous live W10 transport attestation",
            );
          }
          return normalizeWatcherL1BlockV1(matching[0]!, l1Observation);
        },
      );
      lineageBlocks.push(...normalizedStep);
      const stepConsistency = evaluateWatcherMultiProviderConsistencyV1(
        watcherFinalityConfiguredSourceV1(finalityPolicy),
        normalizedStep,
        transportAttestations,
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
    if (!same(replayedState, authority.previousState)) {
      return null;
    }
    const normalized = authority.observations.map(
      ({ authenticatedProvider, l1Observation }) => {
        const matching = transportAttestations.filter((attestation) => {
          const details = watcherL1TransportAttestationDetailsV1(attestation);
          return (
            details !== null && same(details.provider, authenticatedProvider)
          );
        });
        if (matching.length !== 1) {
          throw new Error(
            "missing or ambiguous live W10 transport attestation",
          );
        }
        return normalizeWatcherL1BlockV1(matching[0]!, l1Observation);
      },
    );
    const consistency = evaluateWatcherMultiProviderConsistencyV1(
      watcherFinalityConfiguredSourceV1(finalityPolicy),
      normalized,
      transportAttestations,
    );
    const result = evaluateWatcherFinalityV1(
      finalityPolicy,
      authority.previousState,
      consistency,
    );
    const bound =
      result.state?.phase === "finalized"
        ? result.state.finalized
        : result.state?.pending;
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
    return same(consistency, authority.consistency) &&
      same(result, authority.result) &&
      [
        "observe_pending",
        "advance_pending",
        "finalize",
        "duplicate",
        "rewind_pending",
      ].includes(result.action) &&
      result.protocolDecision !== "quarantined" &&
      sourceMatchesFinality &&
      bound?.pointDigest === block.chainPoint.pointDigest &&
      bound.blockContentDigest === block.blockContentDigest &&
      normalized.some(
        (candidate) => candidate.observationDigest === block.observationDigest,
      )
      ? Object.freeze({
          policy: finalityPolicy,
          result,
          lineageBlocks: Object.freeze(lineageBlocks),
        })
      : null;
  } catch {
    return null;
  }
};

const verifyBaseContext = (
  policy: WatcherSettlementIndexerPolicyV1,
  observation: WatcherSettlementObservationV1,
  rawProvider: unknown,
  rawL1Observation: unknown,
  rawSourceStore: unknown,
  rawStore: unknown,
  deploymentAuthority: WatcherSettlementDeploymentAuthorityV1,
  finalityAuthority: WatcherSettlementFinalityAuthorityV1 | null,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): Omit<
  VerifiedPublicContext,
  | "rollbackAuthority"
  | "restartContexts"
  | "restartRollbackContexts"
  | "transportAttestations"
> | null => {
  let block: WatcherNormalizedL1BlockV1;
  let sourceStore: WatcherDurableStoreV1;
  let store: WatcherDurableStoreV1;
  try {
    const matching = transportAttestations.filter((attestation) => {
      const details = watcherL1TransportAttestationDetailsV1(attestation);
      return details !== null && same(details.provider, rawProvider);
    });
    if (matching.length !== 1) {
      return null;
    }
    block = normalizeWatcherL1BlockV1(matching[0]!, rawL1Observation);
    sourceStore = parseWatcherDurableStoreV1(rawSourceStore);
    store = parseWatcherDurableStoreV1(rawStore);
  } catch {
    return null;
  }
  const deploymentIdentity = verifyDeploymentAuthority(
    policy,
    deploymentAuthority,
  );
  const isRollback = observation.transition.kind === "rollback";
  const verifiedFinality =
    finalityAuthority === null
      ? null
      : verifyFinalityAuthority(
          policy,
          block,
          finalityAuthority,
          transportAttestations,
        );
  if (
    deploymentIdentity === null ||
    !sameMarker(sourceStore.deploymentMarker, policy.deploymentMarker) ||
    (isRollback ? finalityAuthority !== null : verifiedFinality === null)
  ) {
    return null;
  }
  const storeDigest = watcherDurableStoreBytesSha256(
    encodeWatcherDurableStoreV1(store),
  );
  const sourceStoreDigest = watcherDurableStoreBytesSha256(
    encodeWatcherDurableStoreV1(sourceStore),
  );
  const persistedBlockHex =
    encodeWatcherNormalizedL1BlockV1(block).toString("hex");
  const persistedObservations = store.l1Observations.filter(
    ({ providerId, chainPointId, payload }) =>
      providerId === block.provider.providerId &&
      chainPointId === block.chainPoint.chainPointId &&
      payload.cborHex === persistedBlockHex &&
      payload.sha256 === sha256Bytes(Buffer.from(persistedBlockHex, "hex")),
  );
  const persistedPoints = store.chainPoints.filter(
    (point) =>
      point.chainPointId === block.chainPoint.chainPointId &&
      point.providerId === block.provider.providerId &&
      point.blockHash === block.chainPoint.blockHash &&
      point.slot === block.chainPoint.slot &&
      point.blockNo === block.chainPoint.blockNo &&
      point.depth === block.chainPoint.depth,
  );
  const transactionMatches =
    observation.transactionHash === null
      ? []
      : block.transactions.filter(
          ({ txHash, isValid }) =>
            isValid && txHash === observation.transactionHash,
        );
  const transactionIndex =
    transactionMatches.length === 1
      ? block.transactions.indexOf(transactionMatches[0]!)
      : -1;
  if (
    block.network !== policy.network ||
    observation.pointDigest !== block.chainPoint.pointDigest ||
    observation.chainPointId !== block.chainPoint.chainPointId ||
    observation.blockHash !== block.chainPoint.blockHash ||
    observation.slot !== block.chainPoint.slot ||
    observation.blockNo !== block.chainPoint.blockNo ||
    observation.sourceObservationDigest !== block.observationDigest ||
    observation.durableStoreDigest !== storeDigest ||
    !sameMarker(store.deploymentMarker, policy.deploymentMarker) ||
    !settlementRolesMatchOutputs(policy, sourceStore) ||
    !settlementRolesMatchOutputs(policy, store) ||
    !settlementForeignRecordsPreserved(policy, sourceStore, store) ||
    persistedObservations.length !== 1 ||
    persistedPoints.length !== 1 ||
    (observation.transactionHash !== null &&
      (transactionMatches.length !== 1 ||
        transactionIndex < 0 ||
        transactionMatches[0]?.transactionIndex !==
          transactionIndex.toString()))
  ) {
    return null;
  }
  const publicContextDigest = sha256Canonical({
    policyDigest: policy.policyDigest,
    observationDigest: observation.observationDigest,
    normalizedObservationDigest: block.observationDigest,
    sourceStoreDigest,
    storeDigest,
    deploymentAuthorityDigest: sha256Canonical(deploymentAuthority),
    finalityResultDigest: verifiedFinality?.result.resultDigest ?? null,
  });
  return Object.freeze({
    block,
    lineageBlocks: verifiedFinality?.lineageBlocks ?? Object.freeze([]),
    sourceStore,
    store,
    transaction:
      observation.transactionHash === null ? null : transactionMatches[0]!,
    deploymentIdentity,
    finalityPolicy: verifiedFinality?.policy ?? null,
    finalityResult: verifiedFinality?.result ?? null,
    publicContextDigest,
  });
};

const rollbackAnchorMatchesAuthority = (
  block: WatcherNormalizedL1BlockV1,
  rollback: WatcherRollbackResultV1,
): boolean => {
  const transition = rollback.rollbackState?.transitions.at(-1);
  const consistency = transition?.consistency;
  const finalityResult = transition?.finalityResult;
  const instruction = finalityResult?.rewindInstruction;
  const replacement = finalityResult?.state?.pending;
  const agreement = consistency?.agreement;
  if (
    transition === undefined ||
    consistency === undefined ||
    finalityResult?.action !== "rewind_pending" ||
    finalityResult.protocolDecision !== "rewind_required" ||
    instruction === null ||
    instruction === undefined ||
    replacement === null ||
    replacement === undefined ||
    agreement === null ||
    agreement === undefined ||
    consistency.status !== "agreed" ||
    consistency.protocolDecision !== "allowed" ||
    block.provider.source.sourceMode !== consistency.sourceMode ||
    !consistency.observationEvidenceDigests.includes(block.observationDigest) ||
    instruction.replacementPointDigest !== block.chainPoint.pointDigest ||
    instruction.replacementContentDigest !== block.blockContentDigest ||
    replacement.pointDigest !== block.chainPoint.pointDigest ||
    replacement.blockHash !== block.chainPoint.blockHash ||
    replacement.slot !== block.chainPoint.slot ||
    replacement.blockNo !== block.chainPoint.blockNo ||
    replacement.blockContentDigest !== block.blockContentDigest ||
    agreement.pointDigest !== block.chainPoint.pointDigest ||
    agreement.blockHash !== block.chainPoint.blockHash ||
    agreement.slot !== block.chainPoint.slot ||
    agreement.blockNo !== block.chainPoint.blockNo ||
    agreement.blockContentDigest !== block.blockContentDigest ||
    agreement.minimumDepth !== instruction.replacementDepth ||
    replacement.currentDepth !== instruction.replacementDepth ||
    BigInt(block.chainPoint.depth) < BigInt(instruction.replacementDepth)
  ) {
    return false;
  }
  return consistency.sourceMode === "local_node"
    ? block.provider.source.sourceMode === "local_node" &&
        block.provider.source.surface === "chain_sync" &&
        consistency.chainAuthorityObservationDigest === block.observationDigest
    : consistency.sourceMode === "external_providers" &&
        block.provider.source.sourceMode === "external_providers";
};

const postFinalityRecoveryAnchorMatchesAuthority = (
  block: WatcherNormalizedL1BlockV1,
  recovery: WatcherPostFinalityRecoveryResultV1,
  input: WatcherPostFinalityRecoveryInputV1,
): boolean => {
  const finalityPolicy = parseWatcherFinalityPolicyV1(input.policy);
  const path = recovery.recoveryState?.path;
  return (
    finalityPolicy !== null &&
    recovery.action === "rewind_and_replay" &&
    recovery.protocolDecision === "resume_replay" &&
    recovery.nextStore !== null &&
    recovery.recoveryState !== null &&
    recovery.resumableFinalityState !== null &&
    path !== undefined &&
    path.commonAncestorPointDigest === block.chainPoint.pointDigest &&
    path.commonAncestorBlockHash === block.chainPoint.blockHash &&
    path.commonAncestorBlockNo === block.chainPoint.blockNo &&
    block.provider.source.sourceMode === finalityPolicy.sourceMode &&
    (finalityPolicy.sourceMode === "local_node"
      ? block.provider.source.sourceMode === "local_node" &&
        block.provider.source.surface === "chain_sync" &&
        block.provider.source.authorityNodeId === finalityPolicy.authorityNodeId
      : block.provider.source.sourceMode === "external_providers")
  );
};

const verifyPublicContext = (
  policy: WatcherSettlementIndexerPolicyV1,
  observation: WatcherSettlementObservationV1,
  rawContext: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): VerifiedPublicContext | null => {
  if (!evidenceWithinBounds(rawContext)) {
    return null;
  }
  const record = exactRecord(rawContext, [
    "schemaVersion",
    "authenticatedProvider",
    "l1Observation",
    "sourceDurableStore",
    "durableStore",
    "deploymentAuthority",
    "finalityAuthority",
    "rollbackAuthority",
    "restartContexts",
    "restartRollbackContexts",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_SETTLEMENT_PUBLIC_CONTEXT_V1_SCHEMA_VERSION
  ) {
    return null;
  }
  const deploymentAuthority = parseDeploymentAuthority(
    record.deploymentAuthority,
  );
  const finalityAuthority =
    record.finalityAuthority === null
      ? null
      : parseFinalityAuthority(record.finalityAuthority);
  if (
    deploymentAuthority === null ||
    (record.finalityAuthority !== null && finalityAuthority === null)
  ) {
    return null;
  }
  const verified = verifyBaseContext(
    policy,
    observation,
    record.authenticatedProvider,
    record.l1Observation,
    record.sourceDurableStore,
    record.durableStore,
    deploymentAuthority,
    finalityAuthority,
    transportAttestations,
  );
  const rawRestartContexts = exactArray(
    record.restartContexts,
    WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.transitionHistoryEntries,
  );
  const rawRestartRollbacks = exactArray(
    record.restartRollbackContexts,
    WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.rollbackHistoryEntries,
  );
  if (
    verified === null ||
    rawRestartContexts === null ||
    rawRestartRollbacks === null
  ) {
    return null;
  }
  const restartContexts: WatcherSettlementPublicContextV1["restartContexts"] =
    [];
  for (const raw of rawRestartContexts) {
    const binding = exactRecord(raw, [
      "publicContextDigest",
      "authenticatedProvider",
      "l1Observation",
      "sourceDurableStore",
      "durableStore",
      "deploymentAuthority",
      "finalityAuthority",
    ]);
    const restartDeployment =
      binding === null
        ? null
        : parseDeploymentAuthority(binding.deploymentAuthority);
    const restartFinality =
      binding?.finalityAuthority === null
        ? null
        : parseFinalityAuthority(binding?.finalityAuthority);
    if (
      binding === null ||
      !isHex32(binding.publicContextDigest) ||
      restartDeployment === null ||
      (binding.finalityAuthority !== null && restartFinality === null)
    ) {
      return null;
    }
    (restartContexts as Array<(typeof restartContexts)[number]>).push({
      publicContextDigest: binding.publicContextDigest,
      authenticatedProvider: binding.authenticatedProvider,
      l1Observation: binding.l1Observation,
      sourceDurableStore: binding.sourceDurableStore,
      durableStore: binding.durableStore,
      deploymentAuthority: restartDeployment,
      finalityAuthority: restartFinality,
    });
  }
  const restartRollbackContexts: WatcherSettlementRollbackContextBindingV1[] =
    [];
  for (const raw of rawRestartRollbacks) {
    const binding = exactRecord(raw, ["resultDigest", "context"]);
    if (binding === null || !isHex32(binding.resultDigest)) {
      return null;
    }
    restartRollbackContexts.push({
      resultDigest: binding.resultDigest,
      context: binding.context as
        | WatcherRollbackVerificationContextV1
        | WatcherPostFinalityRecoveryInputV1,
    });
  }
  const rollbackAuthority =
    record.rollbackAuthority === null
      ? null
      : exactRecord(record.rollbackAuthority, ["result", "context"]) === null
        ? undefined
        : (record.rollbackAuthority as NonNullable<
            WatcherSettlementPublicContextV1["rollbackAuthority"]
          >);
  if (rollbackAuthority === undefined) {
    return null;
  }
  return Object.freeze({
    ...verified,
    rollbackAuthority,
    restartContexts: Object.freeze(restartContexts),
    restartRollbackContexts: Object.freeze(restartRollbackContexts),
    transportAttestations,
  });
};

const transactionInputs = (
  body: ReturnType<typeof CML.TransactionBody.from_cbor_hex>,
): readonly string[] => {
  const values: string[] = [];
  const inputs = body.inputs();
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    values.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return Object.freeze(values.sort(compareText));
};

const storeRetains = <T>(previous: readonly T[], next: readonly T[]): boolean =>
  previous.every((entry) => next.some((candidate) => same(entry, candidate)));

type SettlementRoleClassification = "owned" | "foreign" | "invalid";

const classifySettlementRole = (
  policy: WatcherSettlementIndexerPolicyV1,
  durable: WatcherProtocolUtxoV1,
): SettlementRoleClassification => {
  try {
    const output = CML.TransactionOutput.from_cbor_hex(durable.output.cborHex);
    if (output.to_canonical_cbor_hex() !== durable.output.cborHex) {
      return "invalid";
    }
    const addressHex = output.address().to_hex();
    const matches = [...TARGET_ROLES].filter(
      (role) => roleAddress(policy, role).addressHex === addressHex,
    );
    if (matches.length === 0) {
      return "foreign";
    }
    return matches.length === 1 &&
      matches[0] === durable.role &&
      decodeResource(policy, durable) !== null
      ? "owned"
      : "invalid";
  } catch {
    return "invalid";
  }
};

const settlementRolesMatchOutputs = (
  policy: WatcherSettlementIndexerPolicyV1,
  store: WatcherDurableStoreV1,
): boolean =>
  [...store.protocolUtxos, ...store.spentProtocolUtxos].every(
    (durable) => classifySettlementRole(policy, durable) !== "invalid",
  );

const settlementForeignRecordsPreserved = (
  policy: WatcherSettlementIndexerPolicyV1,
  source: WatcherDurableStoreV1,
  next: WatcherDurableStoreV1,
): boolean => {
  const foreign = (records: readonly WatcherProtocolUtxoV1[]) =>
    records.filter(
      (durable) => classifySettlementRole(policy, durable) === "foreign",
    );
  return (
    same(foreign(source.protocolUtxos), foreign(next.protocolUtxos)) &&
    same(foreign(source.spentProtocolUtxos), foreign(next.spentProtocolUtxos))
  );
};

const storeIsSettlementOwnershipExtension = (
  policy: WatcherSettlementIndexerPolicyV1,
  previous: WatcherDurableStoreV1,
  next: WatcherDurableStoreV1,
  recoverableOwnedOutRefs: ReadonlySet<string> | null = null,
): boolean => {
  if (
    !sameMarker(previous.deploymentMarker, next.deploymentMarker) ||
    !settlementRolesMatchOutputs(policy, previous) ||
    !settlementRolesMatchOutputs(policy, next) ||
    BigInt(next.revision) < BigInt(previous.revision) ||
    (next.revision === previous.revision && !same(previous, next))
  ) {
    return false;
  }
  const settlementOwned = (
    entries: readonly WatcherProtocolUtxoV1[],
  ): readonly WatcherProtocolUtxoV1[] =>
    entries.filter(
      (durable) => classifySettlementRole(policy, durable) === "owned",
    );
  const nextOwned = (entries: readonly WatcherProtocolUtxoV1[]) =>
    settlementOwned(entries).filter(
      ({ outRef }) => !recoverableOwnedOutRefs?.has(outRef),
    );
  const ownedMatches = (
    previousEntries: readonly WatcherProtocolUtxoV1[],
    nextEntries: readonly WatcherProtocolUtxoV1[],
  ) =>
    same(settlementOwned(previousEntries), settlementOwned(nextEntries)) ||
    same(settlementOwned(previousEntries), nextOwned(nextEntries));
  return (
    ownedMatches(previous.protocolUtxos, next.protocolUtxos) &&
    ownedMatches(previous.spentProtocolUtxos, next.spentProtocolUtxos) &&
    storeRetains(previous.l1Observations, next.l1Observations) &&
    storeRetains(previous.chainPoints, next.chainPoints) &&
    storeRetains(previous.protocolUtxos, next.protocolUtxos) &&
    storeRetains(previous.spentProtocolUtxos, next.spentProtocolUtxos) &&
    storeRetains(previous.daProofInputs, next.daProofInputs) &&
    storeRetains(previous.reconstructedStates, next.reconstructedStates) &&
    storeRetains(previous.decisions, next.decisions) &&
    storeRetains(previous.faults, next.faults) &&
    storeRetains(previous.submissions, next.submissions) &&
    storeRetains(previous.confirmations, next.confirmations) &&
    storeRetains(previous.retries, next.retries) &&
    storeRetains(previous.deadlines, next.deadlines) &&
    storeRetains(previous.correctionResults, next.correctionResults)
  );
};

const normalStoreProgressionMatches = (
  previous: WatcherDurableStoreV1,
  next: WatcherDurableStoreV1,
  transaction: WatcherL1TransactionV1 | null,
  chainPointId: string,
): boolean => {
  if (
    BigInt(next.revision) !== BigInt(previous.revision) + 1n ||
    !sameMarker(previous.deploymentMarker, next.deploymentMarker)
  ) {
    return false;
  }
  const inputs =
    transaction === null
      ? new Set<string>()
      : new Set(
          transactionInputs(
            CML.TransactionBody.from_cbor_hex(transaction.body.bytesHex),
          ),
        );
  const outputs = new Set(transaction?.utxos.map(({ outRef }) => outRef) ?? []);
  if (
    previous.protocolUtxos.some(
      (entry) =>
        !next.protocolUtxos.some(({ outRef }) => outRef === entry.outRef) &&
        !inputs.has(entry.outRef),
    ) ||
    next.protocolUtxos.some(
      (entry) =>
        !previous.protocolUtxos.some(({ outRef }) => outRef === entry.outRef) &&
        !outputs.has(entry.outRef),
    )
  ) {
    return false;
  }
  try {
    const journal = journalWatcherProtocolUtxoTransitionV1({
      sourceStore: previous,
      nextChainPoints: next.chainPoints,
      nextProtocolUtxos: next.protocolUtxos,
      spentAtChainPointId: chainPointId,
    });
    if (
      !same(journal.protocolUtxos, next.protocolUtxos) ||
      !same(journal.spentProtocolUtxos, next.spentProtocolUtxos)
    ) {
      return false;
    }
  } catch {
    return false;
  }
  return (
    storeRetains(previous.l1Observations, next.l1Observations) &&
    storeRetains(previous.chainPoints, next.chainPoints) &&
    storeRetains(previous.daProofInputs, next.daProofInputs) &&
    storeRetains(previous.reconstructedStates, next.reconstructedStates) &&
    storeRetains(previous.decisions, next.decisions) &&
    storeRetains(previous.faults, next.faults) &&
    storeRetains(previous.submissions, next.submissions) &&
    storeRetains(previous.confirmations, next.confirmations) &&
    storeRetains(previous.retries, next.retries) &&
    storeRetains(previous.deadlines, next.deadlines) &&
    storeRetains(previous.correctionResults, next.correctionResults)
  );
};

const previousStoreFromContext = (
  policy: WatcherSettlementIndexerPolicyV1,
  state: WatcherSettlementIndexerStateV1,
  context: VerifiedPublicContext,
  extensionStore: WatcherDurableStoreV1 = context.sourceStore,
  recoverableOwnedOutRefs: ReadonlySet<string> | null = null,
): WatcherDurableStoreV1 | null => {
  const priorDigest =
    state.transitionHistory.at(-1)?.entry.publicContextDigest ?? null;
  const priorBinding = context.restartContexts.find(
    ({ publicContextDigest }) => publicContextDigest === priorDigest,
  );
  if (priorBinding === undefined) {
    return null;
  }
  try {
    const previousStore = parseWatcherDurableStoreV1(priorBinding.durableStore);
    return watcherDurableStoreBytesSha256(
      encodeWatcherDurableStoreV1(previousStore),
    ) === state.durableStoreDigest &&
      storeIsSettlementOwnershipExtension(
        policy,
        previousStore,
        extensionStore,
        recoverableOwnedOutRefs,
      )
      ? previousStore
      : null;
  } catch {
    return null;
  }
};

const transactionMint = (
  body: ReturnType<typeof CML.TransactionBody.from_cbor_hex>,
): readonly WatcherSettlementMintAssetV1[] => {
  const entries: WatcherSettlementMintAssetV1[] = [];
  const mint = body.mint();
  if (mint === undefined) {
    return Object.freeze(entries);
  }
  const policies = mint.keys();
  for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
    const policy = policies.get(policyIndex);
    const policyId = policy.to_hex();
    const assets = mint.get_assets(policy);
    if (assets === undefined) {
      throw new Error("Cardano mint policy disappeared during decoding");
    }
    const names = assets.keys();
    for (let assetIndex = 0; assetIndex < names.len(); assetIndex += 1) {
      const name = names.get(assetIndex);
      const quantity = assets.get(name);
      if (quantity === undefined || quantity === 0n) {
        throw new Error("Cardano mint contains a zero quantity");
      }
      entries.push({
        policyId,
        assetName: name.to_hex(),
        quantity: quantity.toString(),
      });
    }
  }
  return Object.freeze(
    entries.sort((left, right) =>
      compareText(
        `${left.policyId}.${left.assetName}`,
        `${right.policyId}.${right.assetName}`,
      ),
    ),
  );
};

const outputBytesMatchBody = (
  transaction: WatcherL1TransactionV1,
  body: ReturnType<typeof CML.TransactionBody.from_cbor_hex>,
): boolean => {
  const outputs = body.outputs();
  if (outputs.len() !== transaction.utxos.length) {
    return false;
  }
  for (let index = 0; index < outputs.len(); index += 1) {
    const utxo = transaction.utxos[index];
    if (
      utxo === undefined ||
      utxo.outputIndex !== index.toString() ||
      utxo.output.bytesHex !== outputs.get(index).to_canonical_cbor_hex()
    ) {
      return false;
    }
    const outputDatum = outputs.get(index).datum()?.as_datum();
    const datumHex = outputDatum?.to_canonical_cbor_hex() ?? null;
    if (
      (utxo.datum?.bytes.bytesHex ?? null) !== datumHex ||
      (utxo.datum?.bytes.sha256 ?? null) !==
        (datumHex === null ? null : sha256Bytes(Buffer.from(datumHex, "hex")))
    ) {
      return false;
    }
  }
  return true;
};

const redeemerSchema = (
  name: WatcherSettlementTransitionV1["requiredRedeemers"][number]["schema"],
): unknown => {
  switch (name) {
    case "settlement_spend":
      return SettlementSpendRedeemer;
    case "settlement_mint":
      return SettlementMintRedeemer;
    case "reserve_spend":
      return ReserveSpendRedeemer;
    case "payout_spend":
      return PayoutSpendRedeemer;
    case "payout_mint":
      return PayoutMintRedeemer;
    case "withdrawal_spend":
      return WithdrawalSpendRedeemer;
    case "deposit_spend":
      return DepositSpendRedeemer;
    case "user_event_mint":
      return UserEventMintRedeemer;
    case "event_witness":
      return UserEventWitnessPublishRedeemer;
    case "membership_withdrawal":
      return Data.Tuple([
        MerkleRootSchema,
        Data.Bytes(),
        Data.Bytes(),
        ProofSchema,
      ]);
    case "active_operator_spend":
      return ActiveOperatorSpendRedeemer;
    case "active_operator_mint":
      return ActiveOperatorMintRedeemer;
    case "retired_operator_mint":
      return RetiredOperatorMintRedeemer;
    case "state_queue_mint":
      return StateQueueRedeemer;
  }
};

const decodedConstructor = (
  schema: WatcherSettlementTransitionV1["requiredRedeemers"][number]["schema"],
  decoded: unknown,
):
  | WatcherSettlementTransitionV1["requiredRedeemers"][number]["constructor"]
  | null => {
  if (schema === "reserve_spend") {
    return "Spend";
  }
  if (schema === "deposit_spend") {
    return "DepositSpend";
  }
  if (schema === "membership_withdrawal") {
    return Array.isArray(decoded) && decoded.length === 4
      ? "MembershipProof"
      : null;
  }
  if (schema === "withdrawal_spend") {
    const record = decoded as { purpose?: unknown };
    if (record.purpose === "InitializePayout") {
      return "InitializePayout";
    }
    if (
      typeof record.purpose === "object" &&
      record.purpose !== null &&
      "Refund" in record.purpose
    ) {
      return "Refund";
    }
    return null;
  }
  if (schema === "active_operator_spend" && decoded === "ListStateTransition") {
    return "ListStateTransition";
  }
  if (typeof decoded !== "object" || decoded === null) {
    return null;
  }
  const keys = Object.keys(decoded);
  return keys.length === 1 &&
    REDEEMER_CONSTRUCTORS.includes(
      keys[0] as (typeof REDEEMER_CONSTRUCTORS)[number],
    )
    ? (keys[0] as (typeof REDEEMER_CONSTRUCTORS)[number])
    : null;
};

const verifyRedeemers = (
  transaction: WatcherL1TransactionV1,
  requirements: WatcherSettlementTransitionV1["requiredRedeemers"],
  body: ReturnType<typeof CML.TransactionBody.from_cbor_hex>,
): boolean => {
  if (transaction.redeemers.length !== requirements.length) {
    return false;
  }
  for (const requirement of requirements) {
    const matches = transaction.redeemers.filter(
      ({ purpose, index }) =>
        purpose === requirement.purpose && index === requirement.index,
    );
    if (matches.length !== 1) {
      return false;
    }
    const redeemer = matches[0] as WatcherL1RedeemerV1;
    const schema = redeemerSchema(requirement.schema);
    const decoded = canonicalPlutusData<unknown>(
      redeemer.bytes.bytesHex,
      schema,
    );
    if (
      decoded === null ||
      decodedConstructor(requirement.schema, decoded) !==
        requirement.constructor ||
      !redeemerIndicesAreInBounds(decoded, body, transaction.redeemers.length)
    ) {
      return false;
    }
  }
  return true;
};

const redeemerIndicesAreInBounds = (
  value: unknown,
  body: ReturnType<typeof CML.TransactionBody.from_cbor_hex>,
  redeemerCount: number,
): boolean => {
  const visit = (candidate: unknown, field: string | null): boolean => {
    if (typeof candidate === "bigint") {
      if (candidate < 0n || candidate > BigInt(Number.MAX_SAFE_INTEGER)) {
        return false;
      }
      const index = Number(candidate);
      if (field?.endsWith("_ref_input_index") === true) {
        return index < (body.reference_inputs()?.len() ?? 0);
      }
      if (field?.endsWith("_input_index") === true) {
        return index < body.inputs().len();
      }
      if (field?.endsWith("_output_index") === true) {
        return index < body.outputs().len();
      }
      if (field?.endsWith("_redeemer_index") === true) {
        return index < redeemerCount;
      }
      return true;
    }
    if (Array.isArray(candidate)) {
      return candidate.every((entry) => visit(entry, null));
    }
    if (typeof candidate === "object" && candidate !== null) {
      return Object.entries(candidate).every(([key, entry]) =>
        visit(entry, key),
      );
    }
    return true;
  };
  return visit(value, null);
};

const verifyTransaction = (
  transition: WatcherSettlementTransitionV1,
  transaction: WatcherL1TransactionV1,
): WatcherSettlementReasonCodeV1 | null => {
  let body: ReturnType<typeof CML.TransactionBody.from_cbor_hex>;
  try {
    body = CML.TransactionBody.from_cbor_hex(transaction.body.bytesHex);
    if (
      body.to_canonical_cbor_hex() !== transaction.body.bytesHex ||
      !outputBytesMatchBody(transaction, body)
    ) {
      return "transaction_mismatch";
    }
    const inputs = transactionInputs(body);
    if (transition.consumedOutRefs.some((outRef) => !inputs.includes(outRef))) {
      return "transaction_mismatch";
    }
    if (!same(transactionMint(body), transition.exactMint)) {
      return "mint_mismatch";
    }
  } catch {
    return "transaction_mismatch";
  }
  return verifyRedeemers(transaction, transition.requiredRedeemers, body)
    ? null
    : "redeemer_mismatch";
};

type DecodedRequiredRedeemer = Readonly<{
  globalIndex: number;
  decoded: any;
}>;

const requiredRedeemer = (
  transition: WatcherSettlementTransitionV1,
  transaction: WatcherL1TransactionV1,
  schema: WatcherSettlementTransitionV1["requiredRedeemers"][number]["schema"],
  constructor: WatcherSettlementTransitionV1["requiredRedeemers"][number]["constructor"],
): DecodedRequiredRedeemer | null => {
  const requirement = transition.requiredRedeemers.find(
    (candidate) =>
      candidate.schema === schema && candidate.constructor === constructor,
  );
  if (requirement === undefined) {
    return null;
  }
  const globalIndex = transaction.redeemers.findIndex(
    ({ purpose, index }) =>
      purpose === requirement.purpose && index === requirement.index,
  );
  if (globalIndex < 0) {
    return null;
  }
  const decoded = canonicalPlutusData<any>(
    transaction.redeemers[globalIndex]!.bytes.bytesHex,
    redeemerSchema(schema),
  );
  return decoded === null ? null : Object.freeze({ globalIndex, decoded });
};

const indexedInputOutRef = (
  body: ReturnType<typeof CML.TransactionBody.from_cbor_hex>,
  index: bigint,
): string | null => {
  if (index < 0n || index >= BigInt(body.inputs().len())) {
    return null;
  }
  const input = body.inputs().get(Number(index));
  return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
};

const indexedReferenceInputOutRef = (
  body: ReturnType<typeof CML.TransactionBody.from_cbor_hex>,
  index: bigint,
): string | null => {
  const inputs = body.reference_inputs();
  if (inputs === undefined || index < 0n || index >= BigInt(inputs.len())) {
    return null;
  }
  const input = inputs.get(Number(index));
  return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
};

const hubReferenceMatches = (
  policy: WatcherSettlementIndexerPolicyV1,
  store: WatcherDurableStoreV1,
  body: ReturnType<typeof CML.TransactionBody.from_cbor_hex>,
  index: bigint,
): boolean => {
  const outRef = indexedReferenceInputOutRef(body, index);
  const durable = store.protocolUtxos.find(
    (candidate) =>
      candidate.outRef === outRef && candidate.role === "hub_oracle",
  );
  if (durable === undefined) {
    return false;
  }
  try {
    const output = CML.TransactionOutput.from_cbor_hex(durable.output.cborHex);
    const assets = canonicalAssets(output.amount()).filter(
      ({ policyId }) => policyId === policy.hubOraclePolicyId,
    );
    return assets.length === 1 && assets[0]?.quantity === "1";
  } catch {
    return false;
  }
};

const allHubReferencesMatch = (
  policy: WatcherSettlementIndexerPolicyV1,
  transition: WatcherSettlementTransitionV1,
  transaction: WatcherL1TransactionV1,
  store: WatcherDurableStoreV1,
  body: ReturnType<typeof CML.TransactionBody.from_cbor_hex>,
): boolean => {
  const visit = (candidate: unknown, field: string | null): boolean => {
    if (
      typeof candidate === "bigint" &&
      (field === "hub_ref_input_index" ||
        field === "hub_oracle_ref_input_index")
    ) {
      return hubReferenceMatches(policy, store, body, candidate);
    }
    if (Array.isArray(candidate)) {
      return candidate.every((entry) => visit(entry, null));
    }
    if (typeof candidate === "object" && candidate !== null) {
      return Object.entries(candidate).every(([key, entry]) =>
        visit(entry, key),
      );
    }
    return true;
  };
  return transition.requiredRedeemers.every((requirement) => {
    const globalIndex = transaction.redeemers.findIndex(
      ({ purpose, index }) =>
        purpose === requirement.purpose && index === requirement.index,
    );
    if (globalIndex < 0) {
      return false;
    }
    const decoded = canonicalPlutusData<unknown>(
      transaction.redeemers[globalIndex]!.bytes.bytesHex,
      redeemerSchema(requirement.schema),
    );
    return decoded !== null && visit(decoded, null);
  });
};

const indexedOutputOutRef = (
  transaction: WatcherL1TransactionV1,
  body: ReturnType<typeof CML.TransactionBody.from_cbor_hex>,
  index: bigint,
): string | null =>
  index < 0n || index >= BigInt(body.outputs().len())
    ? null
    : `${transaction.txHash}#${index.toString()}`;

const membershipRedeemerMatches = (
  transaction: WatcherL1TransactionV1,
  globalIndex: bigint,
  proof: any,
): boolean => {
  if (
    typeof proof !== "object" ||
    proof === null ||
    globalIndex < 0n ||
    globalIndex >= BigInt(transaction.redeemers.length)
  ) {
    return false;
  }
  const decoded = canonicalPlutusData<any>(
    transaction.redeemers[Number(globalIndex)]!.bytes.bytesHex,
    redeemerSchema("membership_withdrawal"),
  );
  return (
    decoded !== null &&
    same(decoded, [proof.phas_root, proof.key, proof.value, proof.proof])
  );
};

const burnEventMatches = (
  transaction: WatcherL1TransactionV1,
  globalIndex: bigint,
  assetName: string,
): boolean => {
  if (globalIndex < 0n || globalIndex >= BigInt(transaction.redeemers.length)) {
    return false;
  }
  const decoded = canonicalPlutusData<any>(
    transaction.redeemers[Number(globalIndex)]!.bytes.bytesHex,
    UserEventMintRedeemer,
  );
  return (
    decoded !== null &&
    typeof decoded === "object" &&
    decoded !== null &&
    "BurnEventNFT" in decoded &&
    decoded.BurnEventNFT.nonce_asset_name === assetName &&
    typeof decoded.BurnEventNFT.witness_unregistration_redeemer_index ===
      "bigint" &&
    decoded.BurnEventNFT.witness_unregistration_redeemer_index >= 0n &&
    decoded.BurnEventNFT.witness_unregistration_redeemer_index <
      BigInt(transaction.redeemers.length)
  );
};

const verifyRedeemerFieldBindings = (
  policy: WatcherSettlementIndexerPolicyV1,
  transition: WatcherSettlementTransitionV1,
  transaction: WatcherL1TransactionV1,
  sourceStore: WatcherDurableStoreV1,
): boolean => {
  const subjectId = transition.subjectId;
  let body: ReturnType<typeof CML.TransactionBody.from_cbor_hex>;
  try {
    body = CML.TransactionBody.from_cbor_hex(transaction.body.bytesHex);
  } catch {
    return false;
  }
  if (
    !allHubReferencesMatch(policy, transition, transaction, sourceStore, body)
  ) {
    return false;
  }
  switch (transition.kind) {
    case "bootstrap":
    case "retry":
    case "mark_stuck":
    case "mark_invalid":
    case "rollback":
      return transaction.redeemers.length === 0;
    case "spawn_settlement": {
      const settlement = requiredRedeemer(
        transition,
        transaction,
        "settlement_mint",
        "Spawn",
      );
      const stateQueue = requiredRedeemer(
        transition,
        transaction,
        "state_queue_mint",
        "MergeToConfirmedStateV1",
      );
      const value = settlement?.decoded.Spawn;
      const merge = stateQueue?.decoded.MergeToConfirmedStateV1;
      const settlementOutput =
        value === undefined ||
        value.output_index < 0n ||
        value.output_index >= BigInt(body.outputs().len())
          ? null
          : body.outputs().get(Number(value.output_index));
      const settlementDatum =
        settlementOutput?.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null;
      const datum =
        settlementDatum === null
          ? null
          : canonicalPlutusData<any>(settlementDatum, SettlementDatum);
      return (
        subjectId !== null &&
        value !== undefined &&
        merge !== undefined &&
        datum !== null &&
        value.settlement_id === subjectId &&
        indexedOutputOutRef(transaction, body, value.output_index) ===
          transition.producedOutRefs[0] &&
        value.state_queue_merge_redeemer_index ===
          BigInt(stateQueue!.globalIndex) &&
        value.hub_ref_input_index <
          BigInt(body.reference_inputs()?.len() ?? 0) &&
        merge.header_node_key === subjectId &&
        merge.m_settlement_redeemer_index === BigInt(settlement!.globalIndex) &&
        indexedInputOutRef(body, 0n) ===
          `${merge.confirmed_state_input_outref.transactionId}#${merge.confirmed_state_input_outref.outputIndex.toString()}` &&
        merge.confirmed_state_output_index >= 0n &&
        merge.confirmed_state_output_index < BigInt(body.outputs().len()) &&
        merge.merged_block_deposits_root === datum.deposits_root &&
        merge.merged_block_withdrawals_root === datum.withdrawals_root &&
        merge.merged_block_forced_transactions_root ===
          datum.forced_transactions_root &&
        merge.merged_block_transactions_root === datum.transactions_root
      );
    }
    case "attach_resolution_claim": {
      const settlement = requiredRedeemer(
        transition,
        transaction,
        "settlement_spend",
        "AttachResolutionClaim",
      );
      const active = requiredRedeemer(
        transition,
        transaction,
        "active_operator_spend",
        "UpdateBondHoldNewSettlement",
      );
      const left = settlement?.decoded.AttachResolutionClaim;
      const right = active?.decoded.UpdateBondHoldNewSettlement;
      return (
        left !== undefined &&
        right !== undefined &&
        indexedInputOutRef(body, left.settlement_input_index) ===
          transition.consumedOutRefs[0] &&
        indexedOutputOutRef(transaction, body, left.settlement_output_index) ===
          transition.producedOutRefs[0] &&
        left.active_operators_redeemer_index === BigInt(active!.globalIndex) &&
        left.operator === right.active_operator &&
        right.settlement_redeemer_index === BigInt(settlement!.globalIndex) &&
        right.settlement_input_index === left.settlement_input_index
      );
    }
    case "disprove_resolution_claim": {
      const settlement = requiredRedeemer(
        transition,
        transaction,
        "settlement_spend",
        "DisproveResolutionClaim",
      );
      const slash =
        requiredRedeemer(
          transition,
          transaction,
          "active_operator_mint",
          "SlashOperator",
        ) ??
        requiredRedeemer(
          transition,
          transaction,
          "retired_operator_mint",
          "SlashOperator",
        );
      const value = settlement?.decoded.DisproveResolutionClaim;
      const slashValue = slash?.decoded.SlashOperator;
      const slashArgs = slashValue?.slashing_arguments;
      return (
        value !== undefined &&
        slashArgs !== undefined &&
        indexedInputOutRef(body, value.settlement_input_index) ===
          transition.consumedOutRefs[0] &&
        indexedOutputOutRef(
          transaction,
          body,
          value.settlement_output_index,
        ) === transition.producedOutRefs[0] &&
        value.operators_redeemer_index === BigInt(slash!.globalIndex) &&
        value.operator === slashArgs.slashed_operator &&
        value.operator_is_active ===
          (transition.requiredRedeemers.find(
            ({ schema, constructor }) =>
              schema === "active_operator_mint" &&
              constructor === "SlashOperator",
          ) !==
            undefined) &&
        slashArgs.slashing_reason?.SlashOperatorForBadSettlement
          ?.settlement_redeemer_index === BigInt(settlement!.globalIndex) &&
        slashArgs.slashing_reason?.SlashOperatorForBadSettlement
          ?.settlement_input_index === value.settlement_input_index &&
        membershipRedeemerMatches(
          transaction,
          value.inclusion_proof_script_withdraw_redeemer_index,
          value.membership_proof.DepositMembership?.witness ??
            value.membership_proof.WithdrawalMembership?.witness ??
            value.membership_proof.TxOrderMembership?.witness,
        )
      );
    }
    case "resolve_settlement": {
      const spend = requiredRedeemer(
        transition,
        transaction,
        "settlement_spend",
        "Resolve",
      );
      const mint = requiredRedeemer(
        transition,
        transaction,
        "settlement_mint",
        "Remove",
      );
      const spendValue = spend?.decoded.Resolve;
      const mintValue = mint?.decoded.Remove;
      return (
        subjectId !== null &&
        spendValue?.settlement_id === subjectId &&
        mintValue?.settlement_id === subjectId &&
        indexedInputOutRef(body, mintValue.input_index) ===
          transition.consumedOutRefs[0] &&
        mintValue.spend_redeemer_index === BigInt(spend!.globalIndex)
      );
    }
    case "absorb_to_reserve": {
      const spend = requiredRedeemer(
        transition,
        transaction,
        "deposit_spend",
        "DepositSpend",
      );
      const value = spend?.decoded;
      const depositOutRef =
        value === undefined
          ? null
          : indexedInputOutRef(body, value.input_index);
      const deposit =
        depositOutRef === null
          ? undefined
          : sourceStore.protocolUtxos.find(
              ({ outRef, role }) =>
                outRef === depositOutRef && role === "deposit",
            );
      let exactDepositTransfer = false;
      try {
        if (deposit !== undefined && value !== undefined) {
          const input = CML.TransactionOutput.from_cbor_hex(
            deposit.output.cborHex,
          );
          const output = body.outputs().get(Number(value.output_index));
          const expected = new Map(cmlValueAssets(input.amount()));
          const nftUnit = `${policy.depositPolicyId}${subjectId ?? ""}`;
          expected.set(nftUnit, (expected.get(nftUnit) ?? 0n) - 1n);
          if (expected.get(nftUnit) === 0n) {
            expected.delete(nftUnit);
          }
          exactDepositTransfer =
            output.address().to_hex() === policy.reserveAddressHex &&
            output.datum() === undefined &&
            output.datum_hash() === undefined &&
            output.script_ref() === undefined &&
            exactValue(expected, cmlValueAssets(output.amount()));
        }
      } catch {
        exactDepositTransfer = false;
      }
      return (
        subjectId !== null &&
        value !== undefined &&
        depositOutRef === transition.consumedOutRefs[0] &&
        exactDepositTransfer &&
        indexedOutputOutRef(transaction, body, value.output_index) ===
          transition.producedOutRefs[0] &&
        burnEventMatches(transaction, value.mint_redeemer_index, subjectId) &&
        membershipRedeemerMatches(
          transaction,
          value.inclusion_proof_script_withdraw_redeemer_index,
          value.membership_proof,
        )
      );
    }
    case "initialize_payout":
    case "refund_withdrawal": {
      const spend = requiredRedeemer(
        transition,
        transaction,
        "withdrawal_spend",
        transition.kind === "initialize_payout" ? "InitializePayout" : "Refund",
      );
      const value = spend?.decoded;
      if (
        subjectId === null ||
        value === undefined ||
        indexedInputOutRef(body, value.input_index) !==
          transition.consumedOutRefs[0] ||
        !burnEventMatches(transaction, value.burn_redeemer_index, subjectId) ||
        !membershipRedeemerMatches(
          transaction,
          value.inclusion_proof_script_withdraw_redeemer_index,
          value.membership_proof,
        )
      ) {
        return false;
      }
      if (transition.kind === "refund_withdrawal") {
        return value.purpose?.Refund !== undefined;
      }
      const mint = requiredRedeemer(
        transition,
        transaction,
        "payout_mint",
        "MintPayout",
      );
      const mintValue = mint?.decoded.MintPayout;
      return (
        value.purpose === "InitializePayout" &&
        value.payout_mint_redeemer_index === BigInt(mint!.globalIndex) &&
        mintValue.withdrawal_spend_redeemer_index ===
          BigInt(spend!.globalIndex) &&
        mintValue.withdrawal_input_index === value.input_index &&
        indexedOutputOutRef(transaction, body, value.output_index) ===
          transition.producedOutRefs[0]
      );
    }
    case "fund_payout": {
      const payout = requiredRedeemer(
        transition,
        transaction,
        "payout_spend",
        "AddFunds",
      );
      const reserve = requiredRedeemer(
        transition,
        transaction,
        "reserve_spend",
        "Spend",
      );
      const left = payout?.decoded.AddFunds;
      const right = reserve?.decoded;
      const payoutInput =
        left === undefined
          ? null
          : indexedInputOutRef(body, left.payout_input_index);
      const reserveInput =
        left === undefined
          ? null
          : indexedInputOutRef(body, left.reserve_input_index);
      const payoutOutput =
        left === undefined
          ? null
          : indexedOutputOutRef(transaction, body, left.payout_output_index);
      const reserveOutput =
        left?.reserve_change_output_index === null ||
        left?.reserve_change_output_index === undefined
          ? null
          : indexedOutputOutRef(
              transaction,
              body,
              left.reserve_change_output_index,
            );
      return (
        left !== undefined &&
        right !== undefined &&
        transition.consumedOutRefs.includes(payoutInput ?? "") &&
        transition.consumedOutRefs.includes(reserveInput ?? "") &&
        sourceStore.protocolUtxos.some(
          ({ outRef, role }) => outRef === payoutInput && role === "payout",
        ) &&
        sourceStore.protocolUtxos.some(
          ({ outRef, role }) => outRef === reserveInput && role === "reserve",
        ) &&
        transition.producedOutRefs.includes(payoutOutput ?? "") &&
        (left.reserve_change_output_index === null ||
          transition.producedOutRefs.includes(reserveOutput ?? "")) &&
        left.reserve_spend_redeemer_index === BigInt(reserve!.globalIndex) &&
        left.payout_spend_redeemer_index === BigInt(payout!.globalIndex) &&
        right.reserve_input_index === left.reserve_input_index &&
        right.payout_input_index === left.payout_input_index &&
        right.payout_spend_redeemer_index === BigInt(payout!.globalIndex) &&
        right.hub_ref_input_index === left.hub_ref_input_index
      );
    }
    case "conclude_payout": {
      const payout = requiredRedeemer(
        transition,
        transaction,
        "payout_spend",
        "ConcludeWithdrawal",
      );
      const burn = requiredRedeemer(
        transition,
        transaction,
        "payout_mint",
        "BurnPayout",
      );
      const left = payout?.decoded.ConcludeWithdrawal;
      const right = burn?.decoded.BurnPayout;
      return (
        subjectId !== null &&
        left !== undefined &&
        right !== undefined &&
        indexedInputOutRef(body, left.payout_input_index) ===
          transition.consumedOutRefs[0] &&
        left.burn_redeemer_index === BigInt(burn!.globalIndex) &&
        right.payout_spend_redeemer_index === BigInt(payout!.globalIndex) &&
        right.payout_input_index === left.payout_input_index &&
        right.payout_asset_name === subjectId &&
        right.hub_ref_input_index === left.hub_ref_input_index
      );
    }
  }
};

type PayoutDatumView = Readonly<{
  targetAssets: ReadonlyMap<string, bigint>;
  addressHex: string;
  datum:
    | Readonly<{ kind: "none" }>
    | Readonly<{ kind: "hash"; hash: string }>
    | Readonly<{ kind: "inline"; cborHex: string }>;
}>;

const credentialHash = (
  value: unknown,
): Readonly<{ kind: "key" | "script"; hash: string }> | null => {
  if (typeof value !== "object" || value === null) {
    return null;
  }
  const record = value as Record<string, unknown>;
  if (
    Array.isArray(record.PublicKeyCredential) &&
    record.PublicKeyCredential.length === 1 &&
    isHex28(record.PublicKeyCredential[0])
  ) {
    return { kind: "key", hash: record.PublicKeyCredential[0] };
  }
  if (
    Array.isArray(record.ScriptCredential) &&
    record.ScriptCredential.length === 1 &&
    isHex28(record.ScriptCredential[0])
  ) {
    return { kind: "script", hash: record.ScriptCredential[0] };
  }
  return null;
};

const addressDataHex = (
  network: WatcherSettlementNetworkV1,
  value: unknown,
): string | null => {
  if (typeof value !== "object" || value === null) {
    return null;
  }
  const record = value as {
    paymentCredential?: unknown;
    stakeCredential?: unknown;
  };
  const payment = credentialHash(record.paymentCredential);
  if (payment === null) {
    return null;
  }
  const networkId = network === "Mainnet" ? 1 : 0;
  if (record.stakeCredential === null) {
    const header = (payment.kind === "key" ? 0x60 : 0x70) | networkId;
    return Buffer.concat([
      Buffer.from([header]),
      Buffer.from(payment.hash, "hex"),
    ]).toString("hex");
  }
  if (
    typeof record.stakeCredential !== "object" ||
    record.stakeCredential === null ||
    !("Inline" in record.stakeCredential)
  ) {
    return null;
  }
  const inline = (record.stakeCredential as { Inline: unknown }).Inline;
  const stake =
    Array.isArray(inline) && inline.length === 1
      ? credentialHash(inline[0])
      : null;
  if (stake === null) {
    return null;
  }
  const addressType =
    payment.kind === "key"
      ? stake.kind === "key"
        ? 0
        : 2
      : stake.kind === "key"
        ? 1
        : 3;
  return Buffer.concat([
    Buffer.from([(addressType << 4) | networkId]),
    Buffer.from(payment.hash, "hex"),
    Buffer.from(stake.hash, "hex"),
  ]).toString("hex");
};

const payoutDatumView = (
  network: WatcherSettlementNetworkV1,
  cborHex: string,
): PayoutDatumView | null => {
  const decoded = canonicalPlutusData<{
    l2_value: unknown;
    l1_address: unknown;
    l1_datum: unknown;
  }>(cborHex, PayoutDatum);
  if (decoded === null) {
    return null;
  }
  if (!(decoded.l2_value instanceof Map)) {
    return null;
  }
  const targetAssets = new Map<string, bigint>();
  for (const [policyId, inner] of decoded.l2_value) {
    if (
      typeof policyId !== "string" ||
      (policyId !== "" && !isHex28(policyId)) ||
      !(inner instanceof Map)
    ) {
      return null;
    }
    for (const [assetName, quantity] of inner) {
      if (
        typeof assetName !== "string" ||
        !/^(?:[0-9a-f]{2}){0,32}$/.test(assetName) ||
        typeof quantity !== "bigint" ||
        quantity < 0n
      ) {
        return null;
      }
      const unit =
        policyId === "" && assetName === ""
          ? "lovelace"
          : `${policyId}${assetName}`;
      if (targetAssets.has(unit) || (policyId === "" && assetName !== "")) {
        return null;
      }
      targetAssets.set(unit, quantity);
    }
  }
  const addressHex = addressDataHex(network, decoded.l1_address);
  if (addressHex === null) {
    return null;
  }
  let datum: PayoutDatumView["datum"];
  if (decoded.l1_datum === "NoDatum") {
    datum = { kind: "none" };
  } else if (
    typeof decoded.l1_datum === "object" &&
    decoded.l1_datum !== null &&
    "DatumHash" in decoded.l1_datum
  ) {
    const candidate = (decoded.l1_datum as { DatumHash: { hash?: unknown } })
      .DatumHash.hash;
    if (!isHex32(candidate)) {
      return null;
    }
    datum = { kind: "hash", hash: candidate };
  } else if (
    typeof decoded.l1_datum === "object" &&
    decoded.l1_datum !== null &&
    "InlineDatum" in decoded.l1_datum
  ) {
    try {
      const encoded = Data.to(
        (decoded.l1_datum as { InlineDatum: { data: unknown } }).InlineDatum
          .data as never,
      );
      datum = {
        kind: "inline",
        cborHex: CML.PlutusData.from_cbor_hex(encoded).to_canonical_cbor_hex(),
      };
    } catch {
      return null;
    }
  } else {
    return null;
  }
  return Object.freeze({
    targetAssets,
    addressHex,
    datum: Object.freeze(datum),
  });
};

const cmlValueAssets = (
  value: ReturnType<
    ReturnType<typeof CML.TransactionOutput.from_cbor_hex>["amount"]
  >,
): ReadonlyMap<string, bigint> =>
  new Map([
    ["lovelace", value.coin()],
    ...canonicalAssets(value).map(
      ({ policyId, assetName, quantity }) =>
        [`${policyId}${assetName}`, BigInt(quantity)] as const,
    ),
  ]);

const resourcePayoutValue = (
  resource: WatcherSettlementResourceV1,
): ReadonlyMap<string, bigint> => {
  const entries = new Map<string, bigint>([
    ["lovelace", BigInt(resource.lovelace)],
  ]);
  for (const asset of resource.assets) {
    if (
      asset.policyId === resource.identityPolicyId &&
      asset.assetName === resource.identityAssetName
    ) {
      continue;
    }
    entries.set(`${asset.policyId}${asset.assetName}`, BigInt(asset.quantity));
  }
  return entries;
};

const exactValue = (
  left: ReadonlyMap<string, bigint>,
  right: ReadonlyMap<string, bigint>,
): boolean => {
  const keys = new Set([...left.keys(), ...right.keys()]);
  return [...keys].every(
    (key) => (left.get(key) ?? 0n) === (right.get(key) ?? 0n),
  );
};

const outputDatumMatches = (
  output: ReturnType<typeof CML.TransactionOutput.from_cbor_hex>,
  expected: PayoutDatumView["datum"],
): boolean => {
  if (expected.kind === "none") {
    return output.datum() === undefined && output.datum_hash() === undefined;
  }
  if (expected.kind === "hash") {
    return (
      output.datum_hash()?.to_hex() === expected.hash &&
      output.datum()?.as_datum() === undefined
    );
  }
  return (
    output.datum()?.as_datum()?.to_canonical_cbor_hex() === expected.cborHex &&
    output.datum_hash() === undefined
  );
};

const payoutRedeemer = (
  transaction: WatcherL1TransactionV1,
  schema: unknown,
  constructor: "ConcludeWithdrawal" | "AddFunds" | "BurnPayout" | "MintPayout",
): any | null => {
  for (const redeemer of transaction.redeemers) {
    const decoded = canonicalPlutusData<any>(redeemer.bytes.bytesHex, schema);
    if (
      decoded !== null &&
      typeof decoded === "object" &&
      decoded !== null &&
      constructor in decoded
    ) {
      return decoded[constructor];
    }
  }
  return null;
};

const verifyPayoutConclusion = (
  policy: WatcherSettlementIndexerPolicyV1,
  before: WatcherSettlementSnapshotV1,
  subjectId: string,
  transaction: WatcherL1TransactionV1,
): boolean => {
  const payout = resourceByIdentity(before, "payout", subjectId);
  if (payout?.datumCborHex === null || payout?.datumCborHex === undefined) {
    return false;
  }
  const datum = payoutDatumView(policy.network, payout.datumCborHex);
  const redeemer = payoutRedeemer(
    transaction,
    PayoutSpendRedeemer,
    "ConcludeWithdrawal",
  );
  const burn = payoutRedeemer(transaction, PayoutMintRedeemer, "BurnPayout");
  if (
    datum === null ||
    redeemer === null ||
    burn === null ||
    typeof redeemer.payout_input_index !== "bigint" ||
    typeof redeemer.l1_output_index !== "bigint" ||
    typeof redeemer.burn_redeemer_index !== "bigint" ||
    typeof redeemer.hub_ref_input_index !== "bigint" ||
    typeof burn.payout_input_index !== "bigint" ||
    typeof burn.payout_asset_name !== "string" ||
    typeof burn.payout_spend_redeemer_index !== "bigint" ||
    typeof burn.hub_ref_input_index !== "bigint" ||
    redeemer.payout_input_index < 0n ||
    redeemer.l1_output_index < 0n
  ) {
    return false;
  }
  try {
    const body = CML.TransactionBody.from_cbor_hex(transaction.body.bytesHex);
    const inputs = body.inputs();
    const outputs = body.outputs();
    const inputIndex = Number(redeemer.payout_input_index);
    const outputIndex = Number(redeemer.l1_output_index);
    if (
      !Number.isSafeInteger(inputIndex) ||
      inputIndex >= inputs.len() ||
      !Number.isSafeInteger(outputIndex) ||
      outputIndex >= outputs.len() ||
      !exactValue(resourcePayoutValue(payout), datum.targetAssets) ||
      `${inputs.get(inputIndex).transaction_id().to_hex()}#${inputs
        .get(inputIndex)
        .index()
        .toString()}` !== payout.outRef ||
      burn.payout_input_index !== redeemer.payout_input_index ||
      burn.payout_asset_name !== subjectId ||
      burn.hub_ref_input_index !== redeemer.hub_ref_input_index
    ) {
      return false;
    }
    const output = outputs.get(outputIndex);
    for (let index = 0; index < outputs.len(); index += 1) {
      if (
        canonicalAssets(outputs.get(index).amount()).some(
          ({ policyId }) => policyId === policy.payoutPolicyId,
        )
      ) {
        return false;
      }
    }
    return (
      output.address().to_hex() === datum.addressHex &&
      output.script_ref() === undefined &&
      exactValue(cmlValueAssets(output.amount()), datum.targetAssets) &&
      outputDatumMatches(output, datum.datum)
    );
  } catch {
    return false;
  }
};

const verifyWithdrawalRefund = (
  policy: WatcherSettlementIndexerPolicyV1,
  before: WatcherSettlementSnapshotV1,
  subjectId: string,
  transaction: WatcherL1TransactionV1,
): boolean => {
  const withdrawal = resourceByIdentity(before, "withdrawal", subjectId);
  if (
    withdrawal?.datumCborHex === null ||
    withdrawal?.datumCborHex === undefined
  ) {
    return false;
  }
  const datum = canonicalPlutusData<{
    refund_address: unknown;
    refund_datum: unknown;
  }>(withdrawal.datumCborHex, WithdrawalOrderDatum);
  const addressHex =
    datum === null
      ? null
      : addressDataHex(policy.network, datum.refund_address);
  let expectedDatum: PayoutDatumView["datum"] | null = null;
  if (datum?.refund_datum === "NoDatum") {
    expectedDatum = { kind: "none" };
  } else if (
    typeof datum?.refund_datum === "object" &&
    datum.refund_datum !== null &&
    "DatumHash" in datum.refund_datum
  ) {
    const hash = (datum.refund_datum as { DatumHash: { hash?: unknown } })
      .DatumHash.hash;
    expectedDatum = isHex32(hash) ? { kind: "hash", hash } : null;
  } else if (
    typeof datum?.refund_datum === "object" &&
    datum.refund_datum !== null &&
    "InlineDatum" in datum.refund_datum
  ) {
    try {
      const encoded = Data.to(
        (
          datum.refund_datum as {
            InlineDatum: { data: unknown };
          }
        ).InlineDatum.data as never,
      );
      expectedDatum = {
        kind: "inline",
        cborHex: CML.PlutusData.from_cbor_hex(encoded).to_canonical_cbor_hex(),
      };
    } catch {
      return false;
    }
  }
  let spend: any | null = null;
  let burn: any | null = null;
  for (const redeemer of transaction.redeemers) {
    const decodedSpend = canonicalPlutusData<any>(
      redeemer.bytes.bytesHex,
      WithdrawalSpendRedeemer,
    );
    if (
      decodedSpend !== null &&
      typeof decodedSpend.purpose === "object" &&
      decodedSpend.purpose !== null &&
      "Refund" in decodedSpend.purpose
    ) {
      spend = decodedSpend;
    }
    const decodedBurn = canonicalPlutusData<any>(
      redeemer.bytes.bytesHex,
      UserEventMintRedeemer,
    );
    if (
      decodedBurn !== null &&
      typeof decodedBurn === "object" &&
      "BurnEventNFT" in decodedBurn
    ) {
      burn = decodedBurn.BurnEventNFT;
    }
  }
  const validityOverride = spend?.purpose?.Refund?.validity_override;
  if (
    addressHex === null ||
    expectedDatum === null ||
    spend === null ||
    burn === null ||
    typeof spend.input_index !== "bigint" ||
    typeof spend.output_index !== "bigint" ||
    spend.input_index < 0n ||
    spend.output_index < 0n ||
    validityOverride === "WithdrawalIsValid" ||
    typeof validityOverride !== "string" ||
    burn.nonce_asset_name !== subjectId
  ) {
    return false;
  }
  try {
    const body = CML.TransactionBody.from_cbor_hex(transaction.body.bytesHex);
    const inputs = body.inputs();
    const outputs = body.outputs();
    const inputIndex = Number(spend.input_index);
    const outputIndex = Number(spend.output_index);
    if (
      !Number.isSafeInteger(inputIndex) ||
      inputIndex >= inputs.len() ||
      !Number.isSafeInteger(outputIndex) ||
      outputIndex >= outputs.len() ||
      `${inputs.get(inputIndex).transaction_id().to_hex()}#${inputs
        .get(inputIndex)
        .index()
        .toString()}` !== withdrawal.outRef
    ) {
      return false;
    }
    const output = outputs.get(outputIndex);
    for (let index = 0; index < outputs.len(); index += 1) {
      if (
        canonicalAssets(outputs.get(index).amount()).some(
          ({ policyId }) => policyId === policy.withdrawalPolicyId,
        )
      ) {
        return false;
      }
    }
    return (
      output.address().to_hex() === addressHex &&
      output.script_ref() === undefined &&
      exactValue(
        cmlValueAssets(output.amount()),
        resourcePayoutValue(withdrawal),
      ) &&
      outputDatumMatches(output, expectedDatum)
    );
  } catch {
    return false;
  }
};

const valueDoesNotExceed = (
  value: ReadonlyMap<string, bigint>,
  maximum: ReadonlyMap<string, bigint>,
): boolean =>
  [...new Set([...value.keys(), ...maximum.keys()])].every(
    (unit) => (value.get(unit) ?? 0n) <= (maximum.get(unit) ?? 0n),
  );

const verifyPayoutFundingValue = (
  policy: WatcherSettlementIndexerPolicyV1,
  snapshot: WatcherSettlementSnapshotV1,
  subjectId: string,
): boolean => {
  const resource = resourceByIdentity(snapshot, "payout", subjectId);
  const subject = subjectByIdentity(snapshot, "payout", subjectId);
  if (
    resource?.datumCborHex === null ||
    resource?.datumCborHex === undefined ||
    subject === null
  ) {
    return false;
  }
  const datum = payoutDatumView(policy.network, resource.datumCborHex);
  if (datum === null) {
    return false;
  }
  const current = resourcePayoutValue(resource);
  return (
    valueDoesNotExceed(current, datum.targetAssets) &&
    (subject.status === "payout_funded"
      ? exactValue(current, datum.targetAssets)
      : !exactValue(current, datum.targetAssets))
  );
};

const resourceIdentity = (resource: WatcherSettlementResourceV1): string =>
  resource.identityAssetName ??
  (resource.role === "reserve" ? resource.outRef : "");

const subjectById = (
  snapshot: WatcherSettlementSnapshotV1,
  subjectId: string,
): WatcherSettlementSubjectV1 | null => {
  const matches = snapshot.subjects.filter(
    (subject) => subject.subjectId === subjectId,
  );
  if (matches.length === 1) {
    return matches[0]!;
  }
  const resourceBound = matches.filter(
    ({ resourceOutRef }) => resourceOutRef !== null,
  );
  return resourceBound.length === 1 ? resourceBound[0]! : null;
};

const subjectByIdentity = (
  snapshot: WatcherSettlementSnapshotV1,
  subjectKind: WatcherSettlementSubjectV1["subjectKind"],
  subjectId: string,
): WatcherSettlementSubjectV1 | null =>
  snapshot.subjects.find(
    (subject) =>
      subject.subjectKind === subjectKind && subject.subjectId === subjectId,
  ) ?? null;

const resourceByIdentity = (
  snapshot: WatcherSettlementSnapshotV1,
  role: WatcherSettlementResourceRoleV1,
  identity: string,
): WatcherSettlementResourceV1 | null =>
  snapshot.resources.find(
    (resource) =>
      resource.role === role && resourceIdentity(resource) === identity,
  ) ?? null;

const subjectMatchesResource = (
  subject: WatcherSettlementSubjectV1,
  resources: readonly WatcherSettlementResourceV1[],
): boolean => {
  const resource =
    subject.resourceOutRef === null
      ? null
      : (resources.find(({ outRef }) => outRef === subject.resourceOutRef) ??
        null);
  if (subject.resourceOutRef !== null && resource === null) {
    return false;
  }
  if (subject.subjectKind === "settlement") {
    if (
      ["resolved", "invalid", "stuck"].includes(subject.status) &&
      resource === null
    ) {
      return true;
    }
    if (
      resource === null ||
      resource.role !== "settlement" ||
      resource.identityAssetName !== subject.subjectId ||
      resource.datumCborHex === null
    ) {
      return false;
    }
    const datum = decodeSettlementDatum(resource.datumCborHex);
    if (datum === null) {
      return false;
    }
    if (
      datum.resolutionClaim !== null &&
      ["retrying", "stuck", "invalid"].includes(subject.status)
    ) {
      return (
        subject.resolutionTime === datum.resolutionClaim.resolutionTime &&
        subject.operatorVkey === datum.resolutionClaim.operatorVkey
      );
    }
    return datum.resolutionClaim === null
      ? subject.status === "open" &&
          subject.resolutionTime === null &&
          subject.operatorVkey === null
      : subject.status === "claimed" &&
          subject.resolutionTime === datum.resolutionClaim.resolutionTime &&
          subject.operatorVkey === datum.resolutionClaim.operatorVkey;
  }
  if (subject.subjectKind === "payout") {
    return ["paid", "invalid", "stuck"].includes(subject.status) &&
      resource === null
      ? true
      : resource !== null &&
          resource.role === "payout" &&
          resource.identityAssetName === subject.subjectId &&
          [
            "payout_initializing",
            "payout_funding",
            "payout_funded",
            "retrying",
            "stuck",
            "invalid",
          ].includes(subject.status);
  }
  if (subject.subjectKind === "withdrawal") {
    return ["resolved", "refunded", "invalid", "stuck"].includes(
      subject.status,
    ) && resource === null
      ? true
      : resource !== null &&
          resource.role === "withdrawal" &&
          resource.identityAssetName === subject.subjectId &&
          ["withdrawal_pending", "retrying", "stuck", "invalid"].includes(
            subject.status,
          );
  }
  if (subject.subjectKind === "reserve") {
    return (
      resource !== null &&
      resource.role === "reserve" &&
      ["reserve_active", "reserve_depleted", "stuck", "invalid"].includes(
        subject.status,
      )
    );
  }
  return (
    subject.subjectKind === "resolution_claim" &&
    ["disproved", "resolved", "stuck", "invalid"].includes(subject.status) &&
    resource === null &&
    subject.resolutionTime !== null &&
    subject.operatorVkey !== null
  );
};

const snapshotSemanticConsistency = (
  snapshot: WatcherSettlementSnapshotV1,
): boolean =>
  snapshot.subjects.every((subject) =>
    subjectMatchesResource(subject, snapshot.resources),
  ) &&
  snapshot.resources.every((resource) => {
    if (resource.role === "reserve") {
      return snapshot.subjects.some(
        (subject) =>
          subject.subjectKind === "reserve" &&
          subject.resourceOutRef === resource.outRef,
      );
    }
    return snapshot.subjects.some(
      (subject) =>
        subject.subjectKind === resource.role &&
        subject.subjectId === resource.identityAssetName &&
        subject.resourceOutRef === resource.outRef,
    );
  });

const changedResourceOutRefs = (
  before: WatcherSettlementSnapshotV1,
  after: WatcherSettlementSnapshotV1,
): Readonly<{ consumed: readonly string[]; produced: readonly string[] }> => {
  const beforeRefs = new Set(before.resources.map(({ outRef }) => outRef));
  const afterRefs = new Set(after.resources.map(({ outRef }) => outRef));
  return Object.freeze({
    consumed: Object.freeze(
      before.resources
        .map(({ outRef }) => outRef)
        .filter((outRef) => !afterRefs.has(outRef))
        .sort(compareText),
    ),
    produced: Object.freeze(
      after.resources
        .map(({ outRef }) => outRef)
        .filter((outRef) => !beforeRefs.has(outRef))
        .sort(compareText),
    ),
  });
};

const protectedMintPolicies = (
  policy: WatcherSettlementIndexerPolicyV1,
): readonly string[] => [
  policy.depositPolicyId,
  policy.settlementPolicyId,
  policy.payoutPolicyId,
  policy.withdrawalPolicyId,
  policy.activeOperatorPolicyId,
  policy.retiredOperatorPolicyId,
  policy.stateQueuePolicyId,
];

const exactProtectedMint = (
  policy: WatcherSettlementIndexerPolicyV1,
  transition: WatcherSettlementTransitionV1,
  expected: readonly WatcherSettlementMintAssetV1[],
): boolean => {
  const actual = transition.exactMint
    .filter(({ policyId }) => protectedMintPolicies(policy).includes(policyId))
    .sort((left, right) =>
      compareText(
        `${left.policyId}.${left.assetName}`,
        `${right.policyId}.${right.assetName}`,
      ),
    );
  return same(
    actual,
    [...expected].sort((left, right) =>
      compareText(
        `${left.policyId}.${left.assetName}`,
        `${right.policyId}.${right.assetName}`,
      ),
    ),
  );
};

const hasConstructors = (
  transition: WatcherSettlementTransitionV1,
  expected: readonly string[],
): boolean => {
  const actual = transition.requiredRedeemers
    .map(({ constructor }) => constructor)
    .filter((constructor) => expected.includes(constructor));
  return expected.every(
    (constructor) =>
      actual.filter((candidate) => candidate === constructor).length === 1,
  );
};

const assetQuantities = (
  resource: WatcherSettlementResourceV1,
): ReadonlyMap<string, bigint> =>
  new Map([
    ["lovelace", BigInt(resource.lovelace)],
    ...resource.assets.map(
      ({ policyId, assetName, quantity }) =>
        [`${policyId}.${assetName}`, BigInt(quantity)] as const,
    ),
  ]);

const valueMonotonicallyIncreases = (
  before: WatcherSettlementResourceV1,
  after: WatcherSettlementResourceV1,
): boolean => {
  const oldValue = assetQuantities(before);
  const newValue = assetQuantities(after);
  let increased = false;
  for (const [unit, quantity] of oldValue) {
    const next = newValue.get(unit) ?? 0n;
    if (next < quantity) {
      return false;
    }
    increased ||= next > quantity;
  }
  for (const [unit, quantity] of newValue) {
    if (!oldValue.has(unit) && quantity > 0n) {
      increased = true;
    }
  }
  return increased;
};

const payoutFundingDeltaMatches = (
  before: WatcherSettlementSnapshotV1,
  after: WatcherSettlementSnapshotV1,
  subjectId: string,
): boolean => {
  const priorPayout = resourceByIdentity(before, "payout", subjectId);
  const nextPayout = resourceByIdentity(after, "payout", subjectId);
  const priorReserve = before.resources.find(({ role }) => role === "reserve");
  const nextReserve = after.resources.find(({ role }) => role === "reserve");
  if (
    priorPayout === null ||
    nextPayout === null ||
    priorReserve === undefined
  ) {
    return false;
  }
  const oldPayout = assetQuantities(priorPayout);
  const newPayout = assetQuantities(nextPayout);
  const oldReserve = assetQuantities(priorReserve);
  const newReserve =
    nextReserve === undefined
      ? new Map<string, bigint>()
      : assetQuantities(nextReserve);
  const keys = new Set([
    ...oldPayout.keys(),
    ...newPayout.keys(),
    ...oldReserve.keys(),
    ...newReserve.keys(),
  ]);
  let moved = false;
  for (const key of keys) {
    const payoutIncrease =
      (newPayout.get(key) ?? 0n) - (oldPayout.get(key) ?? 0n);
    const reserveDecrease =
      (oldReserve.get(key) ?? 0n) - (newReserve.get(key) ?? 0n);
    if (payoutIncrease < 0n || payoutIncrease !== reserveDecrease) {
      return false;
    }
    moved ||= payoutIncrease > 0n;
  }
  return moved;
};

const verifyTransitionSemantics = (
  policy: WatcherSettlementIndexerPolicyV1,
  before: WatcherSettlementSnapshotV1 | null,
  observation: WatcherSettlementObservationV1,
): WatcherSettlementReasonCodeV1 | null => {
  const { transition, snapshot } = observation;
  if (!snapshotSemanticConsistency(snapshot)) {
    return "status_mismatch";
  }
  if (transition.kind === "bootstrap") {
    return before === null &&
      transition.consumedOutRefs.length === 0 &&
      transition.requiredRedeemers.length === 0 &&
      transition.exactMint.length === 0 &&
      transition.failureCode === null &&
      transition.retryAttempt === null
      ? null
      : "status_mismatch";
  }
  if (before === null) {
    return "stale_state";
  }
  const diff = changedResourceOutRefs(before, snapshot);
  if (
    !(
      transition.kind === "absorb_to_reserve" &&
      diff.consumed.length === 0 &&
      transition.consumedOutRefs.length === 1
    ) &&
    !same(diff.consumed, transition.consumedOutRefs)
  ) {
    return "topology_mismatch";
  }
  if (!same(diff.produced, transition.producedOutRefs)) {
    return "topology_mismatch";
  }
  const id = transition.subjectId;
  const previousSubject = id === null ? null : subjectById(before, id);
  const nextSubject = id === null ? null : subjectById(snapshot, id);
  const noFailure =
    transition.failureCode === null && transition.retryAttempt === null;
  switch (transition.kind) {
    case "spawn_settlement": {
      const resource =
        id === null ? null : resourceByIdentity(snapshot, "settlement", id);
      return id !== null &&
        previousSubject === null &&
        nextSubject?.status === "open" &&
        resource !== null &&
        transition.consumedOutRefs.length === 0 &&
        transition.producedOutRefs.length === 1 &&
        hasConstructors(transition, ["Spawn", "MergeToConfirmedStateV1"]) &&
        exactProtectedMint(policy, transition, [
          {
            policyId: policy.settlementPolicyId,
            assetName: id,
            quantity: "1",
          },
          {
            policyId: policy.stateQueuePolicyId,
            assetName: `4d424c43${id}`,
            quantity: "-1",
          },
        ]) &&
        noFailure
        ? null
        : "status_mismatch";
    }
    case "attach_resolution_claim": {
      const prior =
        id === null ? null : resourceByIdentity(before, "settlement", id);
      const next =
        id === null ? null : resourceByIdentity(snapshot, "settlement", id);
      return previousSubject?.status === "open" &&
        nextSubject?.status === "claimed" &&
        prior !== null &&
        next !== null &&
        transition.consumedOutRefs.length === 1 &&
        transition.producedOutRefs.length === 1 &&
        hasConstructors(transition, [
          "AttachResolutionClaim",
          "UpdateBondHoldNewSettlement",
        ]) &&
        exactProtectedMint(policy, transition, []) &&
        noFailure
        ? null
        : "status_mismatch";
    }
    case "disprove_resolution_claim": {
      const next =
        id === null ? null : resourceByIdentity(snapshot, "settlement", id);
      const claimId = transition.relatedSubjectId;
      const claim =
        claimId === null
          ? null
          : subjectByIdentity(snapshot, "resolution_claim", claimId);
      const activeSlash = transition.requiredRedeemers.some(
        ({ schema, constructor }) =>
          schema === "active_operator_mint" && constructor === "SlashOperator",
      );
      const retiredSlash = transition.requiredRedeemers.some(
        ({ schema, constructor }) =>
          schema === "retired_operator_mint" && constructor === "SlashOperator",
      );
      const slashPolicy =
        activeSlash === retiredSlash
          ? null
          : activeSlash
            ? policy.activeOperatorPolicyId
            : policy.retiredOperatorPolicyId;
      const slashMint =
        slashPolicy === null
          ? []
          : transition.exactMint.filter(
              ({ policyId }) => policyId === slashPolicy,
            );
      return previousSubject?.status === "claimed" &&
        nextSubject?.status === "open" &&
        next !== null &&
        claim?.subjectKind === "resolution_claim" &&
        claim.status === "disproved" &&
        hasConstructors(transition, [
          "DisproveResolutionClaim",
          "ListStateTransition",
          "SlashOperator",
          "MembershipProof",
        ]) &&
        slashMint.length === 1 &&
        slashMint[0]!.quantity === "-1" &&
        exactProtectedMint(policy, transition, slashMint) &&
        noFailure
        ? null
        : "status_mismatch";
    }
    case "resolve_settlement": {
      return id !== null &&
        previousSubject?.status === "claimed" &&
        nextSubject?.status === "resolved" &&
        nextSubject.resourceOutRef === null &&
        resourceByIdentity(snapshot, "settlement", id) === null &&
        hasConstructors(transition, ["Resolve", "Remove"]) &&
        exactProtectedMint(policy, transition, [
          {
            policyId: policy.settlementPolicyId,
            assetName: id,
            quantity: "-1",
          },
        ]) &&
        noFailure
        ? null
        : "status_mismatch";
    }
    case "absorb_to_reserve": {
      const priorReserveOutRefs = new Set(
        before.resources
          .filter(({ role }) => role === "reserve")
          .map(({ outRef }) => outRef),
      );
      const nextReserveOutRefs = new Set(
        snapshot.resources
          .filter(({ role }) => role === "reserve")
          .map(({ outRef }) => outRef),
      );
      const added = [...nextReserveOutRefs].filter(
        (outRef) => !priorReserveOutRefs.has(outRef),
      );
      const removed = [...priorReserveOutRefs].filter(
        (outRef) => !nextReserveOutRefs.has(outRef),
      );
      return id !== null &&
        added.length === 1 &&
        removed.length === 0 &&
        transition.consumedOutRefs.length === 1 &&
        same(transition.producedOutRefs, added.sort(compareText)) &&
        hasConstructors(transition, [
          "DepositSpend",
          "BurnEventNFT",
          "MintOrBurn",
          "MembershipProof",
        ]) &&
        exactProtectedMint(policy, transition, [
          {
            policyId: policy.depositPolicyId,
            assetName: id,
            quantity: "-1",
          },
        ]) &&
        noFailure
        ? null
        : "value_mismatch";
    }
    case "initialize_payout": {
      const payout =
        id === null ? null : resourceByIdentity(snapshot, "payout", id);
      const previousPayout =
        id === null ? null : subjectByIdentity(before, "payout", id);
      const nextPayout =
        id === null ? null : subjectByIdentity(snapshot, "payout", id);
      const withdrawalId = transition.relatedSubjectId;
      const withdrawal =
        withdrawalId === null
          ? null
          : subjectByIdentity(snapshot, "withdrawal", withdrawalId);
      return id !== null &&
        previousPayout === null &&
        payout !== null &&
        ["payout_initializing", "payout_funding", "payout_funded"].includes(
          nextPayout?.status ?? "",
        ) &&
        withdrawal?.status === "resolved" &&
        hasConstructors(transition, [
          "InitializePayout",
          "BurnEventNFT",
          "MintPayout",
          "MintOrBurn",
          "MembershipProof",
        ]) &&
        verifyPayoutFundingValue(policy, snapshot, id) &&
        exactProtectedMint(
          policy,
          transition,
          withdrawalId === null
            ? []
            : [
                {
                  policyId: policy.payoutPolicyId,
                  assetName: id,
                  quantity: "1",
                },
                {
                  policyId: policy.withdrawalPolicyId,
                  assetName: withdrawalId,
                  quantity: "-1",
                },
              ],
        ) &&
        noFailure
        ? null
        : "status_mismatch";
    }
    case "fund_payout": {
      const prior =
        id === null ? null : resourceByIdentity(before, "payout", id);
      const next =
        id === null ? null : resourceByIdentity(snapshot, "payout", id);
      const previousPayout =
        id === null ? null : subjectByIdentity(before, "payout", id);
      const nextPayout =
        id === null ? null : subjectByIdentity(snapshot, "payout", id);
      return id !== null &&
        prior !== null &&
        next !== null &&
        previousPayout !== null &&
        ["payout_initializing", "payout_funding"].includes(
          previousPayout.status,
        ) &&
        ["payout_funding", "payout_funded"].includes(
          nextPayout?.status ?? "",
        ) &&
        prior.datumCborHex === next.datumCborHex &&
        valueMonotonicallyIncreases(prior, next) &&
        payoutFundingDeltaMatches(before, snapshot, id) &&
        verifyPayoutFundingValue(policy, snapshot, id) &&
        hasConstructors(transition, ["AddFunds", "Spend"]) &&
        exactProtectedMint(policy, transition, []) &&
        noFailure
        ? null
        : "value_mismatch";
    }
    case "conclude_payout": {
      const previousPayout =
        id === null ? null : subjectByIdentity(before, "payout", id);
      const nextPayout =
        id === null ? null : subjectByIdentity(snapshot, "payout", id);
      return id !== null &&
        previousPayout?.status === "payout_funded" &&
        nextPayout?.status === "paid" &&
        nextPayout.resourceOutRef === null &&
        resourceByIdentity(snapshot, "payout", id) === null &&
        hasConstructors(transition, ["ConcludeWithdrawal", "BurnPayout"]) &&
        exactProtectedMint(policy, transition, [
          {
            policyId: policy.payoutPolicyId,
            assetName: id,
            quantity: "-1",
          },
        ]) &&
        noFailure
        ? null
        : "status_mismatch";
    }
    case "refund_withdrawal": {
      const previousWithdrawal =
        id === null ? null : subjectByIdentity(before, "withdrawal", id);
      const nextWithdrawal =
        id === null ? null : subjectByIdentity(snapshot, "withdrawal", id);
      return id !== null &&
        previousWithdrawal?.status === "withdrawal_pending" &&
        nextWithdrawal?.status === "refunded" &&
        nextWithdrawal.resourceOutRef === null &&
        resourceByIdentity(snapshot, "withdrawal", id) === null &&
        hasConstructors(transition, [
          "Refund",
          "BurnEventNFT",
          "MintOrBurn",
          "MembershipProof",
        ]) &&
        exactProtectedMint(policy, transition, [
          {
            policyId: policy.withdrawalPolicyId,
            assetName: id,
            quantity: "-1",
          },
        ]) &&
        noFailure
        ? null
        : "status_mismatch";
    }
    case "retry": {
      return previousSubject !== null &&
        nextSubject?.status === "retrying" &&
        transition.failureCode !== null &&
        transition.retryAttempt !== null &&
        BigInt(transition.retryAttempt) ===
          BigInt(previousSubject.attempt) + 1n &&
        nextSubject.attempt === transition.retryAttempt &&
        nextSubject.failureCode === transition.failureCode &&
        transition.consumedOutRefs.length === 0 &&
        transition.producedOutRefs.length === 0 &&
        transition.requiredRedeemers.length === 0 &&
        transition.exactMint.length === 0
        ? null
        : "retry_mismatch";
    }
    case "mark_stuck": {
      return previousSubject?.status === "retrying" &&
        nextSubject?.status === "stuck" &&
        transition.failureCode !== null &&
        transition.failureCode.startsWith("stuck_") &&
        BigInt(previousSubject.attempt) >=
          BigInt(policy.maximumRetryAttempts) &&
        nextSubject.failureCode === transition.failureCode &&
        transition.retryAttempt === previousSubject.attempt &&
        transition.consumedOutRefs.length === 0 &&
        transition.producedOutRefs.length === 0
        ? null
        : "retry_mismatch";
    }
    case "mark_invalid": {
      return previousSubject !== null &&
        nextSubject?.status === "invalid" &&
        transition.failureCode !== null &&
        transition.failureCode.startsWith("invalid_") &&
        nextSubject.failureCode === transition.failureCode &&
        transition.retryAttempt === previousSubject.attempt &&
        transition.consumedOutRefs.length === 0 &&
        transition.producedOutRefs.length === 0
        ? null
        : "status_mismatch";
    }
    case "rollback":
      return "rollback_mismatch";
  }
};

const historyEntryDigest = (
  observation: WatcherSettlementObservationV1,
  publicContextDigest: string,
): string =>
  sha256Canonical({
    observationDigest: observation.observationDigest,
    publicContextDigest,
  });

const rollbackEntryDigest = (
  observation: WatcherSettlementObservationV1,
  publicContextDigest: string,
  rollbackResult: WatcherRollbackResultV1 | WatcherPostFinalityRecoveryResultV1,
): string =>
  sha256Canonical({
    observationDigest: observation.observationDigest,
    publicContextDigest,
    rollbackResultDigest: rollbackResult.resultDigest,
  });

const stateWithoutDigest = (
  value: Omit<WatcherSettlementIndexerStateV1, "stateDigest">,
): Omit<WatcherSettlementIndexerStateV1, "stateDigest"> => value;

const makeState = (
  input: Omit<WatcherSettlementIndexerStateV1, "schemaVersion" | "stateDigest">,
): WatcherSettlementIndexerStateV1 => {
  const canonical = Object.freeze({
    schemaVersion: WATCHER_SETTLEMENT_INDEXER_STATE_V1_SCHEMA_VERSION,
    ...input,
  });
  return Object.freeze({
    ...canonical,
    stateDigest: sha256Canonical(stateWithoutDigest(canonical)),
  });
};

const emptySnapshot = (): WatcherSettlementSnapshotV1 =>
  makeWatcherSettlementSnapshotV1({ resources: [], subjects: [] })!;

const parseHistoryEntry = (
  value: unknown,
): WatcherSettlementHistoryEntryV1 | null => {
  const record = exactRecord(value, [
    "observation",
    "publicContextDigest",
    "stateDigest",
  ]);
  if (
    record === null ||
    !isHex32(record.publicContextDigest) ||
    !isHex32(record.stateDigest)
  ) {
    return null;
  }
  const observation = parseWatcherSettlementObservationV1(record.observation);
  if (
    observation === null ||
    observation.transition.kind === "rollback" ||
    historyEntryDigest(observation, record.publicContextDigest) !==
      record.stateDigest
  ) {
    return null;
  }
  return Object.freeze({
    observation,
    publicContextDigest: record.publicContextDigest,
    stateDigest: record.stateDigest,
  });
};

const parseRollbackEntry = (
  value: unknown,
): WatcherSettlementRollbackEntryV1 | null => {
  const record = exactRecord(value, [
    "observation",
    "publicContextDigest",
    "rollbackResult",
    "rollbackDigest",
  ]);
  if (
    record === null ||
    !isHex32(record.publicContextDigest) ||
    !isHex32(record.rollbackDigest)
  ) {
    return null;
  }
  const observation = parseWatcherSettlementObservationV1(record.observation);
  const rollbackResult =
    typeof record.rollbackResult === "object" && record.rollbackResult !== null
      ? (record.rollbackResult as
          | WatcherRollbackResultV1
          | WatcherPostFinalityRecoveryResultV1)
      : null;
  if (
    observation === null ||
    observation.transition.kind !== "rollback" ||
    rollbackResult === null ||
    !isHex32(rollbackResult.resultDigest) ||
    rollbackEntryDigest(
      observation,
      record.publicContextDigest,
      rollbackResult,
    ) !== record.rollbackDigest
  ) {
    return null;
  }
  return Object.freeze({
    observation,
    publicContextDigest: record.publicContextDigest,
    rollbackResult,
    rollbackDigest: record.rollbackDigest,
  });
};

const parseTransitionAuditEntry = (
  value: unknown,
): WatcherSettlementTransitionAuditEntryV1 | null => {
  const record = exactRecord(value, ["kind", "entry"]);
  if (record?.kind === "transition") {
    const entry = parseHistoryEntry(record.entry);
    return entry === null ? null : Object.freeze({ kind: "transition", entry });
  }
  if (record?.kind === "rollback") {
    const entry = parseRollbackEntry(record.entry);
    return entry === null ? null : Object.freeze({ kind: "rollback", entry });
  }
  return null;
};

export const parseWatcherSettlementIndexerStateV1 = (
  value: unknown,
  policyValue: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
  restartContextValues: readonly WatcherSettlementPublicContextV1["restartContexts"][number][] = [],
  restartRollbackContextValues: readonly WatcherSettlementRollbackContextBindingV1[] = [],
): WatcherSettlementIndexerStateV1 | null => {
  if (
    !evidenceWithinBounds({
      policy: policyValue,
      state: value,
      restartContexts: restartContextValues,
      restartRollbackContexts: restartRollbackContextValues,
    })
  ) {
    return null;
  }
  const policy = parseWatcherSettlementIndexerPolicyV1(policyValue);
  const record = exactRecord(value, [
    "schemaVersion",
    "policyDigest",
    "network",
    "releaseEvidenceDigest",
    "deploymentMarker",
    "pointDigest",
    "chainPointId",
    "durableStoreDigest",
    "snapshot",
    "activeHistory",
    "rollbackHistory",
    "transitionHistory",
    "orphanAuditDigests",
    "orphanLineageDigest",
    "stateDigest",
  ]);
  const rawHistory =
    record === null
      ? null
      : exactArray(
          record.activeHistory,
          WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.activeHistoryEntries,
        );
  const rawRollbacks =
    record === null
      ? null
      : exactArray(
          record.rollbackHistory,
          WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.rollbackHistoryEntries,
        );
  const rawTransitions =
    record === null
      ? null
      : exactArray(
          record.transitionHistory,
          WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.transitionHistoryEntries,
        );
  const rawOrphans =
    record === null
      ? null
      : sortedUniqueStrings(
          record.orphanAuditDigests,
          WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.orphanAuditDigests,
          isHex32,
        );
  if (
    policy === null ||
    record === null ||
    record.schemaVersion !==
      WATCHER_SETTLEMENT_INDEXER_STATE_V1_SCHEMA_VERSION ||
    record.policyDigest !== policy.policyDigest ||
    record.network !== policy.network ||
    record.releaseEvidenceDigest !== policy.releaseEvidenceDigest ||
    !isHex32(record.pointDigest) ||
    !isHex32(record.chainPointId) ||
    !isHex32(record.durableStoreDigest) ||
    !isHex32(record.orphanLineageDigest) ||
    !isHex32(record.stateDigest) ||
    rawHistory === null ||
    rawRollbacks === null ||
    rawTransitions === null ||
    rawOrphans === null
  ) {
    return null;
  }
  const marker = parseMarker(record.deploymentMarker);
  const snapshot = parseWatcherSettlementSnapshotV1(record.snapshot);
  const activeHistory = rawHistory.map(parseHistoryEntry);
  const rollbackHistory = rawRollbacks.map(parseRollbackEntry);
  const transitionHistory = rawTransitions.map(parseTransitionAuditEntry);
  if (
    marker === null ||
    !sameMarker(marker, policy.deploymentMarker) ||
    snapshot === null ||
    activeHistory.some((entry) => entry === null) ||
    rollbackHistory.some((entry) => entry === null) ||
    transitionHistory.some((entry) => entry === null)
  ) {
    return null;
  }
  const transitions =
    transitionHistory as WatcherSettlementTransitionAuditEntryV1[];
  const restartByDigest = new Map(
    restartContextValues.map((binding) => [
      binding.publicContextDigest,
      binding,
    ]),
  );
  const rollbackContextByDigest = new Map(
    restartRollbackContextValues.map((binding) => [
      binding.resultDigest,
      binding.context,
    ]),
  );
  if (
    restartByDigest.size !== restartContextValues.length ||
    rollbackContextByDigest.size !== restartRollbackContextValues.length
  ) {
    return null;
  }
  let replayed: WatcherSettlementIndexerStateV1 | null = null;
  const replayRestartContexts: WatcherSettlementPublicContextV1["restartContexts"][number][] =
    [];
  const replayRollbackContexts: WatcherSettlementRollbackContextBindingV1[] =
    [];
  const usedRestartDigests = new Set<string>();
  const usedRollbackDigests = new Set<string>();
  for (const auditEntry of transitions) {
    const entry = auditEntry.entry;
    const rollbackEntry =
      auditEntry.kind === "rollback" ? auditEntry.entry : null;
    const binding = restartByDigest.get(entry.publicContextDigest);
    if (binding === undefined) {
      return null;
    }
    const verified = verifyBaseContext(
      policy,
      entry.observation,
      binding.authenticatedProvider,
      binding.l1Observation,
      binding.sourceDurableStore,
      binding.durableStore,
      binding.deploymentAuthority,
      binding.finalityAuthority,
      transportAttestations,
    );
    if (
      verified === null ||
      verified.publicContextDigest !== entry.publicContextDigest
    ) {
      return null;
    }
    const rollbackContext =
      rollbackEntry !== null
        ? rollbackContextByDigest.get(rollbackEntry.rollbackResult.resultDigest)
        : undefined;
    if (
      (rollbackEntry !== null && rollbackContext === undefined) ||
      (auditEntry.kind === "transition" &&
        entry.observation.transition.kind === "rollback")
    ) {
      return null;
    }
    replayRestartContexts.push(binding);
    usedRestartDigests.add(binding.publicContextDigest);
    if (rollbackEntry !== null) {
      replayRollbackContexts.push({
        resultDigest: rollbackEntry.rollbackResult.resultDigest,
        context: rollbackContext!,
      });
      usedRollbackDigests.add(rollbackEntry.rollbackResult.resultDigest);
    }
    const context: VerifiedPublicContext = Object.freeze({
      ...verified,
      rollbackAuthority:
        rollbackEntry !== null
          ? {
              result: rollbackEntry.rollbackResult,
              context: rollbackContext!,
            }
          : null,
      restartContexts: Object.freeze([...replayRestartContexts]),
      restartRollbackContexts: Object.freeze([...replayRollbackContexts]),
      transportAttestations,
    });
    const result = applyVerifiedSettlementTransition(
      policy,
      replayed,
      entry.observation,
      context,
    );
    if (
      result.action !== "accept" ||
      result.state === null ||
      !same(result.state.transitionHistory.at(-1), auditEntry)
    ) {
      return null;
    }
    replayed = result.state;
  }
  return replayed !== null &&
    usedRestartDigests.size === restartByDigest.size &&
    usedRollbackDigests.size === rollbackContextByDigest.size &&
    same(replayed, value) &&
    same(replayed.activeHistory, activeHistory) &&
    same(replayed.rollbackHistory, rollbackHistory) &&
    same(replayed.orphanAuditDigests, rawOrphans) &&
    replayed.snapshot.snapshotDigest === snapshot.snapshotDigest &&
    sameMarker(replayed.deploymentMarker, marker)
    ? replayed
    : null;
};

const makeResult = (
  input: Omit<
    WatcherSettlementIndexerResultV1,
    "schemaVersion" | "resultDigest"
  >,
): WatcherSettlementIndexerResultV1 => {
  const canonical = Object.freeze({
    schemaVersion: WATCHER_SETTLEMENT_INDEXER_RESULT_V1_SCHEMA_VERSION,
    ...input,
  });
  return Object.freeze({
    ...canonical,
    resultDigest: sha256Canonical(canonical),
  });
};

const reject = (
  reason: WatcherSettlementReasonCodeV1,
  alert: WatcherSettlementAlertCodeV1 = "watcher_settlement_transition_rejected",
): WatcherSettlementIndexerResultV1 =>
  makeResult({
    action: "reject",
    protocolDecision: "quarantined",
    reasonCodes: [reason],
    alertCodes: [alert],
    state: null,
  });

const orphanedByRollback = (
  entry: WatcherSettlementHistoryEntryV1,
  rollback: WatcherRollbackResultV1 | WatcherPostFinalityRecoveryResultV1,
): boolean => {
  const removedPoints = new Set(rollback.removedRecords.chainPointIds);
  const removedObservations = new Set(rollback.removedRecords.l1ObservationIds);
  return (
    removedPoints.has(entry.observation.chainPointId) &&
    removedObservations.has(entry.observation.sourceObservationDigest)
  );
};

const ROLLBACK_RESTORABLE_ROLES = new Set<WatcherProtocolUtxoV1["role"]>([
  "settlement",
  "reserve",
  "payout",
  "withdrawal",
]);

const composeRollbackStore = (
  policy: WatcherSettlementIndexerPolicyV1,
  authoritativeBase: WatcherDurableStoreV1,
  snapshot: WatcherSettlementSnapshotV1,
  restartContexts: readonly WatcherSettlementPublicContextV1["restartContexts"][number][],
): WatcherDurableStoreV1 | null => {
  const targetOutRefs = new Set(snapshot.resources.map(({ outRef }) => outRef));
  const candidates = new Map<string, WatcherProtocolUtxoV1>();
  for (const binding of restartContexts) {
    let retainedStore: WatcherDurableStoreV1;
    try {
      retainedStore = parseWatcherDurableStoreV1(binding.durableStore);
    } catch {
      return null;
    }
    for (const candidate of retainedStore.protocolUtxos) {
      if (!targetOutRefs.has(candidate.outRef)) {
        continue;
      }
      const prior = candidates.get(candidate.outRef);
      if (prior !== undefined && !same(prior, candidate)) {
        return null;
      }
      candidates.set(candidate.outRef, candidate);
    }
  }
  const restored: WatcherProtocolUtxoV1[] = [];
  for (const resource of snapshot.resources) {
    const candidate = candidates.get(resource.outRef);
    if (
      candidate === undefined ||
      candidate.role !== resource.role ||
      !ROLLBACK_RESTORABLE_ROLES.has(candidate.role)
    ) {
      return null;
    }
    const decoded = decodeResource(policy, candidate);
    if (decoded === null || !same(decoded, resource)) {
      return null;
    }
    restored.push(candidate);
  }
  const baseProtocol = authoritativeBase.protocolUtxos.filter(
    ({ role }) => !ROLLBACK_RESTORABLE_ROLES.has(role),
  );
  if (
    authoritativeBase.protocolUtxos.some(
      (candidate) =>
        ROLLBACK_RESTORABLE_ROLES.has(candidate.role) &&
        !restored.some((expected) => same(expected, candidate)),
    )
  ) {
    return null;
  }
  try {
    return makeWatcherDurableStoreV1({
      deploymentMarker: authoritativeBase.deploymentMarker,
      revision: authoritativeBase.revision,
      records: {
        l1Observations: authoritativeBase.l1Observations,
        chainPoints: authoritativeBase.chainPoints,
        protocolUtxos: [...baseProtocol, ...restored],
        spentProtocolUtxos: authoritativeBase.spentProtocolUtxos,
        daProofInputs: authoritativeBase.daProofInputs,
        reconstructedStates: authoritativeBase.reconstructedStates,
        decisions: authoritativeBase.decisions,
        faults: authoritativeBase.faults,
        submissions: authoritativeBase.submissions,
        confirmations: authoritativeBase.confirmations,
        retries: authoritativeBase.retries,
        deadlines: authoritativeBase.deadlines,
        correctionResults: authoritativeBase.correctionResults,
      },
    });
  } catch {
    return null;
  }
};

const applyVerifiedSettlementTransition = (
  policy: WatcherSettlementIndexerPolicyV1,
  previousState: WatcherSettlementIndexerStateV1 | null,
  observation: WatcherSettlementObservationV1,
  context: VerifiedPublicContext,
): WatcherSettlementIndexerResultV1 => {
  const recoveryAuthority =
    observation.transition.kind === "rollback" &&
    context.rollbackAuthority !== null
      ? parseWatcherPostFinalityRecoveryResultV1(
          context.rollbackAuthority.result,
          {
            ...(context.rollbackAuthority
              .context as WatcherPostFinalityRecoveryInputV1),
            transportAttestations: context.transportAttestations,
          },
        )
      : null;
  const recoverableOwnedOutRefs =
    recoveryAuthority?.action === "rewind_and_replay"
      ? new Set(recoveryAuthority.removedRecords.protocolUtxoOutRefs)
      : null;
  if (
    observation.predecessorStateDigest !== previousState?.stateDigest &&
    !(
      previousState === null &&
      observation.predecessorStateDigest === null &&
      observation.transition.kind === "bootstrap"
    )
  ) {
    return reject("stale_state");
  }
  const anchoredPreviousStore =
    previousState === null
      ? null
      : previousStoreFromContext(
          policy,
          previousState,
          context,
          context.sourceStore,
          recoverableOwnedOutRefs,
        );
  if (previousState !== null && anchoredPreviousStore === null) {
    return reject("stale_state");
  }
  if (
    !resourcesMatchStore(policy, observation.snapshot, context.store) ||
    !snapshotSemanticConsistency(observation.snapshot)
  ) {
    return reject("topology_mismatch");
  }
  if (
    previousState?.activeHistory.some(
      ({ observation: prior }) =>
        prior.observationDigest === observation.observationDigest,
    ) === true ||
    previousState?.rollbackHistory.some(
      ({ observation: prior }) =>
        prior.observationDigest === observation.observationDigest,
    ) === true
  ) {
    return makeResult({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: ["duplicate_observation"],
      alertCodes: [],
      state: previousState,
    });
  }
  if (
    observation.transactionHash !== null &&
    previousState?.activeHistory.some(
      ({ observation: prior }) =>
        prior.transactionHash === observation.transactionHash,
    ) === true
  ) {
    return reject("identity_collision");
  }
  if (
    previousState !== null &&
    observation.transition.kind !== "rollback" &&
    previousState.transitionHistory.length > 0 &&
    !canonicalSuccessor(
      previousState.transitionHistory.at(-1)!.entry.observation,
      observation,
      context.block,
      context.lineageBlocks,
    )
  ) {
    return reject("stale_chain_point");
  }
  if (observation.transition.kind === "rollback") {
    if (
      previousState === null ||
      context.rollbackAuthority === null ||
      observation.transactionHash !== null ||
      observation.transition.subjectId !== null ||
      observation.transition.relatedSubjectId !== null ||
      observation.transition.consumedOutRefs.length !== 0 ||
      observation.transition.producedOutRefs.length !== 0 ||
      observation.transition.requiredRedeemers.length !== 0 ||
      observation.transition.exactMint.length !== 0 ||
      observation.transition.failureCode !== null ||
      observation.transition.retryAttempt !== null
    ) {
      return reject("rollback_mismatch");
    }
    const rollbackResult = parseWatcherRollbackResultV1(
      context.rollbackAuthority.result,
      {
        ...(context.rollbackAuthority
          .context as WatcherRollbackVerificationContextV1),
        transportAttestations: context.transportAttestations,
      },
    );
    const recoveryResult =
      rollbackResult === null
        ? parseWatcherPostFinalityRecoveryResultV1(
            context.rollbackAuthority.result,
            {
              ...(context.rollbackAuthority
                .context as WatcherPostFinalityRecoveryInputV1),
              transportAttestations: context.transportAttestations,
            },
          )
        : null;
    const authoritative = rollbackResult ?? recoveryResult;
    if (authoritative === null) {
      return reject("rollback_authority_mismatch");
    }
    if (
      rollbackResult?.action === "quarantine_incident" ||
      authoritative.protocolDecision === "quarantined"
    ) {
      return reject(
        "post_finality_quarantine",
        "watcher_settlement_post_finality_incident",
      );
    }
    if (
      (rollbackResult !== null
        ? rollbackResult.action !== "apply_rewind" ||
          rollbackResult.protocolDecision !== "resume_pending" ||
          !rollbackAnchorMatchesAuthority(context.block, rollbackResult)
        : recoveryResult === null ||
          !postFinalityRecoveryAnchorMatchesAuthority(
            context.block,
            recoveryResult,
            context.rollbackAuthority
              .context as WatcherPostFinalityRecoveryInputV1,
          )) ||
      authoritative.sourceStoreDigest !==
        watcherDurableStoreBytesSha256(
          encodeWatcherDurableStoreV1(context.sourceStore),
        ) ||
      authoritative.nextStore === null ||
      authoritative.nextStoreDigest !==
        watcherDurableStoreBytesSha256(
          encodeWatcherDurableStoreV1(authoritative.nextStore),
        )
    ) {
      return reject("rollback_authority_mismatch");
    }
    const commonAncestorBlockNo =
      recoveryResult?.recoveryState?.path.commonAncestorBlockNo ?? null;
    const firstOrphaned =
      recoveryResult === null
        ? previousState.activeHistory.findIndex((entry) =>
            orphanedByRollback(entry, authoritative),
          )
        : previousState.activeHistory.findIndex(
            (entry) =>
              commonAncestorBlockNo !== null &&
              BigInt(entry.observation.blockNo) > BigInt(commonAncestorBlockNo),
          );
    const retained =
      firstOrphaned < 0
        ? [...previousState.activeHistory]
        : previousState.activeHistory.slice(0, firstOrphaned);
    const orphaned =
      firstOrphaned < 0 ? [] : previousState.activeHistory.slice(firstOrphaned);
    if (
      orphaned.some((entry) => !orphanedByRollback(entry, authoritative)) ||
      (recoveryResult !== null &&
        (commonAncestorBlockNo === null ||
          retained.some(
            (entry) =>
              BigInt(entry.observation.blockNo) > BigInt(commonAncestorBlockNo),
          ) ||
          retained.some((entry) => orphanedByRollback(entry, authoritative))))
    ) {
      return reject("rollback_authority_mismatch");
    }
    const expectedSnapshot =
      retained.at(-1)?.observation.snapshot ?? emptySnapshot();
    if (!same(expectedSnapshot, observation.snapshot)) {
      return reject("rollback_mismatch");
    }
    const augmentedStore = composeRollbackStore(
      policy,
      authoritative.nextStore,
      expectedSnapshot,
      context.restartContexts,
    );
    if (augmentedStore === null || !same(augmentedStore, context.store)) {
      return reject("rollback_authority_mismatch");
    }
    const rollbackEntryWithoutDigest = {
      observation,
      publicContextDigest: context.publicContextDigest,
      rollbackResult: authoritative,
    };
    const rollbackEntry: WatcherSettlementRollbackEntryV1 = Object.freeze({
      ...rollbackEntryWithoutDigest,
      rollbackDigest: rollbackEntryDigest(
        observation,
        context.publicContextDigest,
        authoritative,
      ),
    });
    const rollbackHistory = Object.freeze([
      ...previousState.rollbackHistory,
      rollbackEntry,
    ]);
    if (rollbackHistory.length > Number(policy.maximumRollbackEntries)) {
      return reject("history_limit_exceeded");
    }
    const transitionHistory = Object.freeze([
      ...previousState.transitionHistory,
      Object.freeze({
        kind: "rollback" as const,
        entry: rollbackEntry,
      }),
    ]);
    if (
      transitionHistory.length >
      WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.transitionHistoryEntries
    ) {
      return reject("history_limit_exceeded");
    }
    const orphanAuditDigests = Object.freeze(
      [
        ...previousState.orphanAuditDigests,
        ...orphaned.map(({ stateDigest }) => stateDigest),
      ]
        .sort(compareText)
        .filter(
          (digest, index, values) =>
            index === 0 || values[index - 1] !== digest,
        )
        .slice(-WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.orphanAuditDigests),
    );
    const orphanLineageDigest = sha256Canonical({
      previous: previousState.orphanLineageDigest,
      rollbackDigest: rollbackEntry.rollbackDigest,
      orphaned: orphaned
        .map(({ stateDigest }) => stateDigest)
        .sort(compareText),
    });
    const state = makeState({
      policyDigest: policy.policyDigest,
      network: policy.network,
      releaseEvidenceDigest: policy.releaseEvidenceDigest,
      deploymentMarker: policy.deploymentMarker,
      pointDigest: observation.pointDigest,
      chainPointId: observation.chainPointId,
      durableStoreDigest: observation.durableStoreDigest,
      snapshot: expectedSnapshot,
      activeHistory: Object.freeze(retained),
      rollbackHistory,
      transitionHistory,
      orphanAuditDigests,
      orphanLineageDigest,
    });
    return makeResult({
      action: "accept",
      protocolDecision: "indexed",
      reasonCodes: ["rollback_authenticated"],
      alertCodes: [],
      state,
    });
  }
  if (context.rollbackAuthority !== null) {
    return reject("rollback_mismatch");
  }
  if (previousState === null) {
    if (
      observation.transition.kind !== "bootstrap" ||
      watcherDurableStoreBytesSha256(
        encodeWatcherDurableStoreV1(context.sourceStore),
      ) !== policy.bootstrapStoreDigest ||
      !normalStoreProgressionMatches(
        context.sourceStore,
        context.store,
        context.transaction,
        observation.chainPointId,
      )
    ) {
      return reject(
        "durable_evidence_mismatch",
        "watcher_settlement_binding_rejected",
      );
    }
  } else {
    if (
      !normalStoreProgressionMatches(
        context.sourceStore,
        context.store,
        context.transaction,
        observation.chainPointId,
      )
    ) {
      return reject(
        "durable_evidence_mismatch",
        "watcher_settlement_binding_rejected",
      );
    }
  }
  const semanticMismatch = verifyTransitionSemantics(
    policy,
    previousState?.snapshot ?? null,
    observation,
  );
  if (semanticMismatch !== null) {
    return reject(semanticMismatch);
  }
  if (context.transaction !== null) {
    const transactionMismatch = verifyTransaction(
      observation.transition,
      context.transaction,
    );
    if (transactionMismatch !== null) {
      return reject(transactionMismatch);
    }
    if (
      !verifyRedeemerFieldBindings(
        policy,
        observation.transition,
        context.transaction,
        context.sourceStore,
      )
    ) {
      return reject("redeemer_mismatch");
    }
    if (
      observation.transition.kind === "conclude_payout" &&
      (previousState === null ||
        observation.transition.subjectId === null ||
        !verifyPayoutConclusion(
          policy,
          previousState.snapshot,
          observation.transition.subjectId,
          context.transaction,
        ))
    ) {
      return reject("value_mismatch");
    }
    if (
      observation.transition.kind === "refund_withdrawal" &&
      (previousState === null ||
        observation.transition.subjectId === null ||
        !verifyWithdrawalRefund(
          policy,
          previousState.snapshot,
          observation.transition.subjectId,
          context.transaction,
        ))
    ) {
      return reject("value_mismatch");
    }
    for (const outRef of observation.transition.producedOutRefs) {
      const utxo = context.transaction.utxos.find(
        (candidate) => candidate.outRef === outRef,
      );
      const durable = context.store.protocolUtxos.find(
        (candidate) => candidate.outRef === outRef,
      );
      const resource = observation.snapshot.resources.find(
        (candidate) => candidate.outRef === outRef,
      );
      if (
        utxo === undefined ||
        durable === undefined ||
        resource === undefined ||
        utxo.output.bytesHex !== durable.output.cborHex ||
        utxo.output.sha256 !== durable.output.sha256 ||
        resource.outputSha256 !== utxo.output.sha256
      ) {
        return reject("output_mismatch");
      }
    }
  } else if (
    !same(
      previousState?.snapshot.resources ?? [],
      observation.snapshot.resources,
    )
  ) {
    return reject("topology_mismatch");
  }
  const historyEntry: WatcherSettlementHistoryEntryV1 = Object.freeze({
    observation,
    publicContextDigest: context.publicContextDigest,
    stateDigest: historyEntryDigest(observation, context.publicContextDigest),
  });
  const activeHistory = Object.freeze([
    ...(previousState?.activeHistory ?? []),
    historyEntry,
  ]);
  if (activeHistory.length > Number(policy.maximumHistoryEntries)) {
    return reject("history_limit_exceeded");
  }
  const transitionHistory = Object.freeze([
    ...(previousState?.transitionHistory ?? []),
    Object.freeze({
      kind: "transition" as const,
      entry: historyEntry,
    }),
  ]);
  if (
    transitionHistory.length >
    WATCHER_SETTLEMENT_INDEXER_V1_BOUNDS.transitionHistoryEntries
  ) {
    return reject("history_limit_exceeded");
  }
  const state = makeState({
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    pointDigest: observation.pointDigest,
    chainPointId: observation.chainPointId,
    durableStoreDigest: observation.durableStoreDigest,
    snapshot: observation.snapshot,
    activeHistory,
    rollbackHistory: previousState?.rollbackHistory ?? Object.freeze([]),
    transitionHistory,
    orphanAuditDigests: previousState?.orphanAuditDigests ?? Object.freeze([]),
    orphanLineageDigest:
      previousState?.orphanLineageDigest ?? sha256Canonical("genesis"),
  });
  const reason: WatcherSettlementReasonCodeV1 =
    observation.transition.kind === "bootstrap"
      ? "bootstrap_authenticated"
      : observation.transition.kind === "retry"
        ? "retry_authenticated"
        : observation.transition.kind === "mark_stuck"
          ? "stuck_authenticated"
          : observation.transition.kind === "mark_invalid"
            ? "invalid_authenticated"
            : "transition_authenticated";
  return makeResult({
    action: "accept",
    protocolDecision: "indexed",
    reasonCodes: [reason],
    alertCodes: [],
    state,
  });
};

export const evaluateWatcherSettlementIndexerV1 = (
  policyValue: unknown,
  previousStateValue: unknown,
  observationValue: unknown,
  publicContextValue: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): WatcherSettlementIndexerResultV1 => {
  const policy = parseWatcherSettlementIndexerPolicyV1(policyValue);
  if (policy === null) {
    return reject("malformed_policy", "watcher_settlement_input_rejected");
  }
  const observation = parseWatcherSettlementObservationV1(observationValue);
  if (observation === null) {
    return reject("malformed_observation", "watcher_settlement_input_rejected");
  }
  if (
    observation.policyDigest !== policy.policyDigest ||
    observation.network !== policy.network ||
    observation.releaseEvidenceDigest !== policy.releaseEvidenceDigest ||
    !sameMarker(observation.deploymentMarker, policy.deploymentMarker)
  ) {
    return reject("binding_mismatch", "watcher_settlement_binding_rejected");
  }
  const context = verifyPublicContext(
    policy,
    observation,
    publicContextValue,
    transportAttestations,
  );
  if (context === null) {
    return reject(
      "malformed_public_context",
      "watcher_settlement_binding_rejected",
    );
  }
  const previousState =
    previousStateValue === null
      ? null
      : parseWatcherSettlementIndexerStateV1(
          previousStateValue,
          policy,
          transportAttestations,
          context.restartContexts,
          context.restartRollbackContexts,
        );
  if (previousStateValue !== null && previousState === null) {
    return reject("malformed_state", "watcher_settlement_input_rejected");
  }
  if (
    previousState !== null &&
    observation.transition.kind === "rollback" &&
    context.rollbackAuthority !== null
  ) {
    const rollback = parseWatcherRollbackResultV1(
      context.rollbackAuthority.result,
      {
        ...(context.rollbackAuthority
          .context as WatcherRollbackVerificationContextV1),
        transportAttestations: context.transportAttestations,
      },
    );
    const recovery = parseWatcherPostFinalityRecoveryResultV1(
      context.rollbackAuthority.result,
      {
        ...(context.rollbackAuthority
          .context as WatcherPostFinalityRecoveryInputV1),
        transportAttestations: context.transportAttestations,
      },
    );
    if (rollback === null && recovery === null) {
      return reject("rollback_authority_mismatch");
    }
    if (recovery !== null) {
      if (
        recovery.action !== "rewind_and_replay" ||
        recovery.protocolDecision !== "resume_replay" ||
        recovery.recoveryState === null
      ) {
        return reject("rollback_authority_mismatch");
      }
      const prior = previousState.rollbackHistory.find(
        (entry) =>
          entry.observation.observationDigest ===
            observation.observationDigest &&
          entry.rollbackResult.resultDigest === recovery.resultDigest,
      );
      if (prior !== undefined) {
        return makeResult({
          action: "duplicate",
          protocolDecision: "hold",
          reasonCodes: ["duplicate_observation"],
          alertCodes: [],
          state: previousState,
        });
      }
    }
  }
  return applyVerifiedSettlementTransition(
    policy,
    previousState,
    observation,
    context,
  );
};

export const parseWatcherSettlementIndexerResultV1 = (
  value: unknown,
  context: WatcherSettlementResultVerificationContextV1,
): WatcherSettlementIndexerResultV1 | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "action",
    "protocolDecision",
    "reasonCodes",
    "alertCodes",
    "state",
    "resultDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !==
      WATCHER_SETTLEMENT_INDEXER_RESULT_V1_SCHEMA_VERSION ||
    !isHex32(record.resultDigest)
  ) {
    return null;
  }
  const authoritative = evaluateWatcherSettlementIndexerV1(
    context.policy,
    context.previousState,
    context.observation,
    context.publicContext,
    context.transportAttestations,
  );
  return same(authoritative, value) ? authoritative : null;
};

export const encodeWatcherSettlementIndexerStateV1 = (
  value: WatcherSettlementIndexerStateV1,
): Buffer =>
  Buffer.from(canonicalJson(value as unknown as CanonicalJson), "utf8");

export const encodeWatcherSettlementIndexerResultV1 = (
  value: WatcherSettlementIndexerResultV1,
): Buffer =>
  Buffer.from(canonicalJson(value as unknown as CanonicalJson), "utf8");
