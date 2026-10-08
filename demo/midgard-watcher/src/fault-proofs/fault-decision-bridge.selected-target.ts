import {
  assertWorkflowActuationPermitIdentity,
  type HeaderDecision,
  type WorkflowActuationPermitController,
} from "@al-ft/midgard-fault-proofs";
import {
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  Header,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherCorrectionLockObservation,
  type WatcherReleasedHeaderProof,
  type WatcherStateQueueHeaderObservation,
  type WatcherStateQueueRemovalKind,
} from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import { watcherSameCanonicalJson } from "../storage/durable-store.js";
import type { WatcherClassificationWarning } from "./fault-decision-bridge.classification-miss.js";
import { type WatcherPersistedFaultDecisionRecord } from "./fault-decision-journal.js";
import {
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  type WatcherFaultProofApplication,
  type WatcherInstalledWorkflowCategory,
} from "./fault-proof-application.js";
import type { WatcherFaultProofProgressRequest } from "./fault-proof-progress-authority.js";
import { type WatcherFaultProofDeadline } from "./fault-proof-supervisor.js";

export const WATCHER_FAULT_DECISION_BRIDGE_SCHEMA_VERSION =
  "midgard-watcher-production-fault-decision-bridge-v1" as const;

/** An in-flight decision lost its authority before it could be delivered. */
export class WatcherFaultDecisionRetired extends Error {}

export const ACTION_CONFIRMATION_DEPTH = 1;

export const MAXIMUM_CLASSIFICATION_CONCURRENCY = 64;

export type WatcherFaultDecisionTarget = Readonly<{
  category: WatcherInstalledWorkflowCategory;
  headerHash: string;
  decisionDigest: string;
}>;

export type WatcherFaultDecisionBridgeResult = Readonly<{
  observationDigest: string;
  decisionDigests: readonly string[];
  target: WatcherFaultDecisionTarget | null;
}>;

export type WatcherFaultDecisionBridge = Readonly<{
  schemaVersion: typeof WATCHER_FAULT_DECISION_BRIDGE_SCHEMA_VERSION;
  /**
   * Re-authenticates and journals the current queue without starting work.
   * Startup uses this before scanning existing workflow journals.
   */
  prepareForRecovery(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<WatcherFaultDecisionBridgeResult>;
  /** Forwards prepared queue authority to the supervisor during startup. */
  recoverExisting(
    input?: Readonly<{ nativeProgress: WatcherNativeBlockAdmission }>,
  ): Promise<number>;
  /** Reconciles one finalized queue cursor and schedules its one allowed fault. */
  reconcileAndDispatch(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<WatcherFaultDecisionBridgeResult>;
  /** Retries only a queue whose public DA or predecessor attachment is pending. */
  retryDeferredClassification(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<void>;
  /** Schedules the target selected by the most recent successful prepare. */
  dispatchPrepared(): Promise<unknown> | null;
  /** Invalidates all runnable authority synchronously on native rollback. */
  invalidateForRollback(): void;
  /** Revokes all runnable authority before production shutdown can await I/O. */
  invalidateForShutdown(): void;
  status(): Readonly<{
    observationDigest: string | null;
    target: WatcherFaultDecisionTarget | null;
  }>;
}>;

export type BridgeApplication = Pick<
  WatcherFaultProofApplication,
  "classifyHeader" | "deploymentFingerprint" | "installedCategories"
>;

export type WatcherUnverifiedHeaderWarning = Readonly<{
  event: "unverified_merged" | "unverified_removed";
  headerHash: string;
  /** The merge or removal transaction. */
  transactionHash: string;
  removalKind?: WatcherStateQueueRemovalKind;
  /** The header was the finalized CorrectionLock's Locked target. */
  lockedCorrectionTarget: boolean;
}>;

export type BridgeDependencies = Readonly<{
  /** Queued headers merged or removed on L1 at release finality, by hash. */
  mergedHeaders?(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<ReadonlyMap<string, WatcherReleasedHeaderProof>>;
  /**
   * Availability observations are reconciled before this bridge is invoked.
   * Merged headers are passed so their public DA is never read.
   */
  pendingAvailabilityHeaders?(
    observation: WatcherAuthenticatedStateQueueObservation,
    merged?: ReadonlySet<string>,
  ): ReadonlySet<string> | Promise<ReadonlySet<string>>;
  assertObservation(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): void;
  observationDigest(
    observation: AuthenticatedStateQueueHeaderObservation,
  ): Promise<string>;
  /** Pins classifier configuration; omitted dependencies disable decision reuse. */
  classificationContextIdentity?(): Promise<string>;
  readRecords(): Promise<readonly WatcherPersistedFaultDecisionRecord[]>;
  append(
    decision: HeaderDecision,
  ): Promise<WatcherPersistedFaultDecisionRecord>;
  createActuationController(
    decision: Extract<HeaderDecision, { readonly decision: "fault_detected" }>,
    rollbackGeneration: string,
  ): WorkflowActuationPermitController;
  assertActuationPermitIdentity: typeof assertWorkflowActuationPermitIdentity;
  deadlineForHeader(
    header: WatcherStateQueueHeaderObservation,
  ): WatcherFaultProofDeadline;
  resolvePredecessorHeader?(
    header: WatcherStateQueueHeaderObservation,
  ): Promise<WatcherStateQueueHeaderObservation | undefined>;
  operationsSink?: WatcherOperationsSink;
  /**
   * Operator warning, once per header released on L1 unverified, skipped
   * past its challengeability horizon or deferred.
   */
  warn?(
    warning: WatcherUnverifiedHeaderWarning | WatcherClassificationWarning,
  ): void;
  /**
   * Wait before the `consecutive`-th retry of a deferred suffix. Omitted, a
   * deferred suffix is retried on every wake.
   */
  deferredRetryDelayMs?(consecutive: number): number;
  nowMs?(): bigint;
  monotonicNowMs?(): number;
  requestProgress(request: WatcherFaultProofProgressRequest): Promise<void>;
  revokeAuthority(reason: string): void;
  unfinishedObjectiveCount(): number;
  retainDecisionAuthorities(decisionDigest: string | null): void;
  decisionUsesLocalEventHistory(decisionDigest: string): boolean;
}>;

export const exactInstalledScope = (
  categories: readonly string[],
): readonly WatcherInstalledWorkflowCategory[] => {
  if (
    categories.length !== WATCHER_INSTALLED_WORKFLOW_CATEGORIES.length ||
    categories.some(
      (category, index) =>
        category !== WATCHER_INSTALLED_WORKFLOW_CATEGORIES[index],
    )
  ) {
    throw new Error(
      "fault decision bridge application scope differs from the installed application",
    );
  }
  return WATCHER_INSTALLED_WORKFLOW_CATEGORIES;
};

const confirmationDepth = (value: string): number => {
  if (!/^(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error("state-queue header confirmation depth is malformed");
  }
  const parsed = BigInt(value);
  if (
    parsed < BigInt(ACTION_CONFIRMATION_DEPTH) ||
    parsed > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error("state-queue header is not included");
  }
  return Number(parsed);
};

export const authenticatedHeaderObservation = (
  source: WatcherAuthenticatedStateQueueObservation,
  header: WatcherStateQueueHeaderObservation,
): AuthenticatedStateQueueHeaderObservation => {
  const decoded = Data.from(header.headerCborHex, Header);
  if (Data.to(decoded, Header) !== header.headerCborHex) {
    throw new Error("authenticated state-queue HeaderV1 CBOR is noncanonical");
  }
  return Object.freeze({
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
    sourceMode: "local_node" as const,
    provenance: Object.freeze({
      trustClass: "authenticated_cardano_l1" as const,
      sourceId: source.sourceId,
      grade: "security" as const,
    }),
    chainPoint: Object.freeze({
      slot: BigInt(header.observedSlot),
      blockHash: header.observedBlockHash,
    }),
    confirmationDepth: confirmationDepth(header.finalityDepth),
    headerHash: header.headerHash,
    header: decoded,
  });
};

export const assertExactFinalizedHeaderOrder = (
  observation: WatcherAuthenticatedStateQueueObservation,
): void => {
  const queued = observation.finalizedQueue.filter(
    (node): node is typeof node & { readonly headerHash: string } =>
      node.headerHash !== null,
  );
  if (
    queued.length !== observation.finalizedHeaders.length ||
    new Set(observation.finalizedHeaders.map(({ headerHash }) => headerHash))
      .size !== observation.finalizedHeaders.length ||
    queued.some((node, index) => {
      const header = observation.finalizedHeaders[index];
      return (
        header === undefined ||
        header.headerHash !== node.headerHash ||
        header.queueOutRef !== node.outRef
      );
    })
  ) {
    throw new Error(
      "authenticated state-queue headers differ from the finalized queue order",
    );
  }
};

const exactLockedFraudProof = (
  lock: WatcherCorrectionLockObservation,
): Readonly<{ headerHash: string; fraudProofAssetName: string }> | null => {
  if (lock.datum === "Idle") return null;
  const identity = lock.datum.Locked.correction_identity;
  if (
    typeof identity !== "object" ||
    identity === null ||
    !("FraudProof" in identity)
  ) {
    return null;
  }
  return Object.freeze({
    headerHash: lock.datum.Locked.target_header_hash,
    fraudProofAssetName: identity.FraudProof.fraud_proof_asset_name,
  });
};

export const selectedTarget = (input: {
  readonly observation: WatcherAuthenticatedStateQueueObservation;
  readonly decisions: readonly HeaderDecision[];
  /** Headers L1 already merged or removed; none can be a runnable target. */
  readonly merged?: ReadonlySet<string>;
}): WatcherFaultDecisionTarget | null => {
  const lock = input.observation.finalizedCorrectionLock;
  if (lock === null) {
    throw new Error(
      "initialized production state has no authenticated CorrectionLock",
    );
  }
  const faults = input.decisions.filter(
    (
      decision,
    ): decision is Extract<
      HeaderDecision,
      { readonly decision: "fault_detected" }
    > => decision.decision === "fault_detected",
  );
  if (lock.datum === "Idle") {
    const first = faults[0];
    return first === undefined
      ? null
      : Object.freeze({
          category: first.category as WatcherInstalledWorkflowCategory,
          headerHash: first.headerHash,
          decisionDigest: first.decisionDigest,
        });
  }
  const locked = exactLockedFraudProof(lock);
  if (locked === null || input.merged?.has(locked.headerHash)) return null;
  const match = faults.find(
    (decision) =>
      decision.headerHash === locked.headerHash &&
      `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[decision.category]}${decision.headerHash}` ===
        locked.fraudProofAssetName,
  );
  if (match === undefined) {
    throw new Error(
      "locked fraud-proof target did not reproduce an exact runnable classification",
    );
  }
  return Object.freeze({
    category: match.category as WatcherInstalledWorkflowCategory,
    headerHash: match.headerHash,
    decisionDigest: match.decisionDigest,
  });
};

export const observationPreservesTarget = (
  observation: WatcherAuthenticatedStateQueueObservation,
  target: WatcherFaultDecisionTarget,
): boolean => {
  if (
    !observation.finalizedHeaders.some(
      ({ headerHash }) => headerHash === target.headerHash,
    )
  ) {
    return false;
  }
  const lock = observation.finalizedCorrectionLock;
  if (lock === null) return false;
  if (lock.datum === "Idle") return true;
  const locked = exactLockedFraudProof(lock);
  return (
    locked !== null &&
    locked.headerHash === target.headerHash &&
    locked.fraudProofAssetName ===
      `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[target.category]}${target.headerHash}`
  );
};

/** Native admission authenticates continuity; reuse additionally refuses a
 * regressed point, changed source authority, or a substituted same-height seal. */
export const preservesObservationAuthority = (
  previous: WatcherAuthenticatedStateQueueObservation,
  candidate: WatcherAuthenticatedStateQueueObservation,
): boolean => {
  const prior = previous.nativePoint;
  const current = candidate.nativePoint;
  const { finalityDepth: _priorDepth, ...priorSeal } = prior;
  const { finalityDepth: _currentDepth, ...currentSeal } = current;
  const samePoint = watcherSameCanonicalJson(priorSeal, currentSeal);
  const forwardPoint =
    BigInt(current.blockNo) > BigInt(prior.blockNo) &&
    BigInt(current.slot) > BigInt(prior.slot);
  return (
    (samePoint || forwardPoint) &&
    BigInt(current.finalityDepth) >= BigInt(prior.finalityDepth) &&
    previous.deploymentIdentityDigest === candidate.deploymentIdentityDigest &&
    previous.protocolScriptAuthorityDigest ===
      candidate.protocolScriptAuthorityDigest &&
    previous.sourceId === candidate.sourceId &&
    previous.stateQueuePolicyId === candidate.stateQueuePolicyId &&
    previous.hubOraclePolicyId === candidate.hubOraclePolicyId
  );
};
