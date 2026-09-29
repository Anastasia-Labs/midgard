import { type DeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Data } from "@lucid-evolution/lucid";

import { type WatcherCustomNetwork } from "../../runtime/custom-network.js";
import {
  type VerifiedWatcherDeploymentIdentity,
  type WatcherDeploymentIdentityPolicy,
  type WatcherDeploymentTrustRoot,
} from "../../runtime/deployment-identity.js";

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

export type EventPolicyFields = Readonly<{
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

export type PlainRecord = Record<string, unknown>;
export type EventSchema = Parameters<typeof Data.from>[1];
export type EvidenceGraphBudget = {
  nodes: number;
  bytes: number;
};

export const HEX_28 = /^[0-9a-f]{56}$/u;
export const HEX_32 = /^[0-9a-f]{64}$/u;
export const HEX_BYTES = /^(?:[0-9a-f]{2})*$/u;
export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
