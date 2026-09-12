import { createHash } from "node:crypto";
import { readFile, realpath } from "node:fs/promises";
import { join } from "node:path";

import {
  assertWorkflowActuationPermitIdentity,
  authenticatedStateQueueObservationDigest,
  computeFraudProofWorkflowId,
  createWorkflowActuationPermitController,
  createWorkflowReconciliationPermitController,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  type HeaderDecision,
  type WorkflowActuationPermit,
  type WorkflowActuationPermitController,
} from "@al-ft/midgard-fault-proofs";
import {
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  GENESIS_HEADER_HASH,
  Header,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherCorrectionLockObservation,
  type WatcherStateQueueHeaderObservation,
  type WatcherStateQueueObservationSource,
} from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import { watcherSameCanonicalJson } from "../storage/durable-store.js";
import {
  openWatcherFaultDecisionJournal,
  type WatcherPersistedFaultDecisionRecord,
} from "./fault-decision-journal.js";
import {
  assertWatcherFaultProofApplication,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  type WatcherFaultProofApplication,
  type WatcherInstalledWorkflowCategory,
} from "./fault-proof-application.js";
import {
  enqueueWatcherFaultDecision,
  type WatcherFaultProofDeadline,
  watcherFaultProofDeadline,
  type WatcherFaultProofJob,
  type WatcherFaultProofReconciliation,
  type WatcherFaultProofSupervisor,
} from "./fault-proof-supervisor.js";

export const WATCHER_FAULT_DECISION_BRIDGE_SCHEMA_VERSION =
  "midgard-watcher-production-fault-decision-bridge-v1" as const;

const RELEASE_CONFIRMATION_DEPTH = 30;
const MAXIMUM_CLASSIFICATION_CONCURRENCY = 64;

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
  /** Scans durable workflows only after a fresh opaque decision is prepared. */
  recoverExisting(): Promise<number>;
  /** Reconciles one finalized queue cursor and schedules its one allowed fault. */
  reconcileAndDispatch(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<WatcherFaultDecisionBridgeResult>;
  /** Schedules the target selected by the most recent successful prepare. */
  dispatchPrepared(): Promise<unknown> | null;
  /** Invalidates all runnable authority synchronously on native rollback. */
  invalidateForRollback(): void;
  /** Fence advancing history without revoking a target that does not consume it. */
  beforeHistoryAdvance(): void;
  /** Retire all cached decisions when local event history loses authority. */
  invalidateForHistoryChange(): void;
  /** Revokes all runnable authority before production shutdown can await I/O. */
  invalidateForShutdown(): void;
  /** Called immediately before any new or resumed workflow may execute. */
  isJobPermitted(
    job: Pick<
      WatcherFaultProofJob,
      | "mode"
      | "category"
      | "headerHash"
      | "decisionDigest"
      | "rollbackGeneration"
    >,
  ): boolean;
  status(): Readonly<{
    observationDigest: string | null;
    target: WatcherFaultDecisionTarget | null;
  }>;
}>;

type BridgeApplication = Pick<
  WatcherFaultProofApplication,
  "classifyHeader" | "deploymentFingerprint" | "installedCategories"
>;

type BridgeDependencies = Readonly<{
  /** Availability observations are reconciled before this bridge is invoked. */
  pendingAvailabilityHeaders?(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): ReadonlySet<string>;
  assertObservation(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): void;
  observationDigest(
    observation: AuthenticatedStateQueueHeaderObservation,
  ): Promise<string>;
  /** Pins classifier configuration; omitted dependencies disable decision reuse. */
  classificationContextIdentity?(): Promise<string>;
  readRecords(): Promise<readonly WatcherPersistedFaultDecisionRecord[]>;
  loadExistingExecution?(
    decision: Extract<HeaderDecision, { decision: "fault_detected" }>,
  ): Promise<readonly FraudProofWorkflowJournalEntry[]>;
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
  nowMs?(): bigint;
  monotonicNowMs?(): number;
  enqueue(
    decision: HeaderDecision,
    actuationPermit: WorkflowActuationPermit,
    deadline: WatcherFaultProofDeadline,
    rollbackGeneration: string,
  ): Promise<unknown>;
  recover(
    decision: HeaderDecision | null,
    actuationPermit: WorkflowActuationPermit | null,
    deadline: WatcherFaultProofDeadline | null,
    rollbackGeneration: string,
    reconciliations?: readonly WatcherFaultProofReconciliation[],
  ): Promise<number>;
  retainDecisionAuthorities(decisionDigest: string | null): void;
  decisionUsesLocalEventHistory(decisionDigest: string): boolean;
}>;

const exactInstalledScope = (
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
    parsed < BigInt(RELEASE_CONFIRMATION_DEPTH) ||
    parsed > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "state-queue header confirmation depth differs from the release policy",
    );
  }
  return Number(parsed);
};

const authenticatedHeaderObservation = (
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

const assertExactFinalizedHeaderOrder = (
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

const selectedTarget = (input: {
  readonly observation: WatcherAuthenticatedStateQueueObservation;
  readonly decisions: readonly HeaderDecision[];
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
  if (locked === null) return null;
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

const observationPreservesTarget = (
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
const preservesObservationAuthority = (
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

/** Availability rechecks cannot erase an already admitted immutable fault.
 * This does not admit a new decision: the exact target, inclusion seal, queue
 * output, source authority and correction lock must still be unchanged. */
const preservesPendingTargetEvidence = (
  previous: WatcherAuthenticatedStateQueueObservation,
  candidate: WatcherAuthenticatedStateQueueObservation,
  target: WatcherFaultDecisionTarget,
): boolean => {
  const priorHeader = previous.finalizedHeaders.find(
    (header) => header.headerHash === target.headerHash,
  );
  const currentHeader = candidate.finalizedHeaders.find(
    (header) => header.headerHash === target.headerHash,
  );
  if (priorHeader === undefined || currentHeader === undefined) return false;
  const { finalityDepth: priorHeaderDepth, ...priorHeaderSeal } = priorHeader;
  const { finalityDepth: currentHeaderDepth, ...currentHeaderSeal } =
    currentHeader;
  return (
    preservesObservationAuthority(previous, candidate) &&
    BigInt(currentHeaderDepth) >= BigInt(priorHeaderDepth) &&
    watcherSameCanonicalJson(priorHeaderSeal, currentHeaderSeal) &&
    watcherSameCanonicalJson(
      previous.finalizedCorrectionLock,
      candidate.finalizedCorrectionLock,
    ) &&
    observationPreservesTarget(candidate, target)
  );
};

const createBridge = (input: {
  readonly application: BridgeApplication;
  readonly runtimeConfigPath: string;
  readonly maximumClassificationConcurrency: number;
  readonly dependencies: BridgeDependencies;
}): WatcherFaultDecisionBridge => {
  const scope = exactInstalledScope(input.application.installedCategories);
  if (
    !Number.isSafeInteger(input.maximumClassificationConcurrency) ||
    input.maximumClassificationConcurrency < 1 ||
    input.maximumClassificationConcurrency > MAXIMUM_CLASSIFICATION_CONCURRENCY
  ) {
    throw new Error(
      "fault decision bridge classification concurrency is out of bounds",
    );
  }
  const deploymentFingerprint = input.application.deploymentFingerprint;
  let classificationEpoch = 0;
  let rollbackGeneration = 0;
  let observation: WatcherAuthenticatedStateQueueObservation | null = null;
  let target: WatcherFaultDecisionTarget | null = null;
  let targetDecision: HeaderDecision | null = null;
  let targetDeadline: WatcherFaultProofDeadline | null = null;
  let actuationController: WorkflowActuationPermitController | null = null;
  const reconciliationControllers =
    new Set<WorkflowActuationPermitController>();
  let preparedResult: WatcherFaultDecisionBridgeResult | null = null;
  let serial: Promise<void> = Promise.resolve();
  type ClassificationBinding = Readonly<{
    contextIdentity: string;
    predecessor: WatcherStateQueueHeaderObservation | undefined;
    deadline: WatcherFaultProofDeadline;
  }>;
  // Only the selected fault retains private replay/actuation authority. Healthy
  // decisions can depend on moving settlement/event context and are not reused.
  let targetClassification: ClassificationBinding | null = null;

  const invalidate = (reason: string): void => {
    input.dependencies.retainDecisionAuthorities(null);
    actuationController?.revoke(reason);
    for (const controller of reconciliationControllers)
      controller.revoke(reason);
    reconciliationControllers.clear();
    actuationController = null;
    classificationEpoch += 1;
    rollbackGeneration += 1;
    observation = null;
    target = null;
    targetDecision = null;
    targetDeadline = null;
    preparedResult = null;
    targetClassification = null;
  };

  const prepare = async (
    candidate: WatcherAuthenticatedStateQueueObservation,
  ): Promise<WatcherFaultDecisionBridgeResult> => {
    input.dependencies.assertObservation(candidate);
    if (candidate.deploymentIdentityDigest !== deploymentFingerprint) {
      throw new Error(
        "state-queue observation differs from the fault-proof deployment",
      );
    }
    const pendingAvailability =
      input.dependencies.pendingAvailabilityHeaders?.(candidate) ??
      new Set<string>();
    for (const headerHash of pendingAvailability) {
      const header = candidate.finalizedHeaders.find(
        (header) => header.headerHash === headerHash,
      );
      if (
        header === undefined ||
        header.daAvailability === "Unattested" ||
        "Published" in header.daAvailability
      ) {
        throw new Error(
          "pending availability classification lacks an authenticated challengeable header",
        );
      }
    }
    const token = ++classificationEpoch;
    assertExactFinalizedHeaderOrder(candidate);
    if (
      target !== null &&
      actuationController !== null &&
      !observationPreservesTarget(candidate, target)
    ) {
      actuationController.restrictToReconciliation(
        "state_queue_target_changed",
      );
      reconciliationControllers.add(actuationController);
      actuationController = null;
      rollbackGeneration += 1;
      observation = null;
      target = null;
      targetDecision = null;
      targetDeadline = null;
      preparedResult = null;
    }
    const contextIdentity =
      await input.dependencies.classificationContextIdentity?.();
    const sameQueueEvidence =
      observation !== null &&
      preservesObservationAuthority(observation, candidate) &&
      watcherSameCanonicalJson(
        observation.finalizedHeaders,
        candidate.finalizedHeaders,
      ) &&
      watcherSameCanonicalJson(
        observation.finalizedQueue,
        candidate.finalizedQueue,
      ) &&
      watcherSameCanonicalJson(
        observation.finalizedCorrectionLock,
        candidate.finalizedCorrectionLock,
      );
    const classifiedBindings = new Map<string, ClassificationBinding>();
    let reusedTargetDecision: HeaderDecision | null = null;
    const persisted = await input.dependencies.readRecords();
    const persistedByObservation = new Map<
      string,
      WatcherPersistedFaultDecisionRecord
    >();
    for (const record of persisted) {
      const key = `${record.decision.headerHash}\u0000${record.decision.authenticatedObservationDigest}`;
      if (persistedByObservation.has(key)) {
        throw new Error(
          "durable decision evidence repeats a header observation identity",
        );
      }
      persistedByObservation.set(key, record);
    }
    let retainedPendingDecision: HeaderDecision | null = null;
    const assertRetainedTargetAuthority = () => {
      if (target === null || actuationController === null)
        throw new Error("retained fault target authority changed");
      const identity = input.dependencies.assertActuationPermitIdentity({
        permit: actuationController.permit,
        category: target.category,
        rollbackGeneration: rollbackGeneration.toString(),
      });
      if (
        identity.authority !== "submission" ||
        identity.deploymentFingerprint !== deploymentFingerprint ||
        identity.headerHash !== target.headerHash ||
        identity.decisionDigest !== target.decisionDigest
      )
        throw new Error(
          "retained classification cannot replace or revive fault submission authority",
        );
    };
    const classifications = new Array<HeaderDecision | null>(
      candidate.finalizedHeaders.length,
    );
    let nextHeaderIndex = 0;
    let classificationFailed = false;
    let classificationFailure: unknown;
    const classifyNext = async (): Promise<void> => {
      while (!classificationFailed) {
        const index = nextHeaderIndex;
        nextHeaderIndex += 1;
        if (index >= candidate.finalizedHeaders.length) return;
        const header = candidate.finalizedHeaders[index]!;
        if (pendingAvailability.has(header.headerHash)) {
          try {
            if (
              observation !== null &&
              target !== null &&
              targetDecision?.decision === "fault_detected" &&
              actuationController !== null &&
              targetDeadline !== null &&
              target.headerHash === header.headerHash &&
              watcherSameCanonicalJson(
                targetDeadline,
                input.dependencies.deadlineForHeader(header),
              ) &&
              preservesPendingTargetEvidence(observation, candidate, target)
            ) {
              assertRetainedTargetAuthority();
              classifications[index] = targetDecision;
              retainedPendingDecision = targetDecision;
            } else classifications[index] = null;
          } catch (error) {
            classificationFailed = true;
            classificationFailure = error;
            return;
          }
          continue;
        }
        const nowMs = input.dependencies.nowMs ?? (() => BigInt(Date.now()));
        const monotonicNowMs =
          input.dependencies.monotonicNowMs ?? (() => performance.now());
        const queuedAtMs = nowMs().toString();
        let startedMonotonicMs = monotonicNowMs();
        let startedAtMs = queuedAtMs;
        let verificationSubjectDigest: string | null = null;
        try {
          const admitted = authenticatedHeaderObservation(candidate, header);
          const authenticatedObservationDigest =
            await input.dependencies.observationDigest(admitted);
          verificationSubjectDigest = authenticatedObservationDigest;
          startedAtMs = nowMs().toString();
          startedMonotonicMs = monotonicNowMs();
          const predecessor =
            await input.dependencies.resolvePredecessorHeader?.(header);
          if (token !== classificationEpoch) {
            throw new Error(
              "state-queue authority changed before fault classification",
            );
          }
          const deadline = input.dependencies.deadlineForHeader(header);
          if (
            pendingAvailability.size === 0 &&
            sameQueueEvidence &&
            contextIdentity !== undefined &&
            target !== null &&
            targetDecision !== null &&
            targetClassification !== null &&
            header.headerHash === target.headerHash &&
            targetDecision.authenticatedObservationDigest ===
              authenticatedObservationDigest &&
            targetClassification.contextIdentity === contextIdentity &&
            watcherSameCanonicalJson(
              targetClassification.predecessor ?? null,
              predecessor ?? null,
            ) &&
            watcherSameCanonicalJson(targetClassification.deadline, deadline) &&
            watcherSameCanonicalJson(targetDeadline, deadline) &&
            !input.dependencies.decisionUsesLocalEventHistory(
              target.decisionDigest,
            )
          ) {
            assertRetainedTargetAuthority();
            classifications[index] = targetDecision;
            classifiedBindings.set(header.headerHash, targetClassification);
            reusedTargetDecision = targetDecision;
            continue;
          }
          const decision = await input.application.classifyHeader({
            runtimeConfigPath: input.runtimeConfigPath,
            observation: admitted,
            stateQueueObservation: candidate,
            header,
            authenticatedObservationDigest,
            ...(predecessor === undefined ? {} : { predecessor }),
          });
          if (token !== classificationEpoch) {
            throw new Error(
              "state-queue authority changed during fault classification",
            );
          }
          if (
            decision.deploymentFingerprint !== deploymentFingerprint ||
            decision.headerHash !== header.headerHash ||
            decision.authenticatedObservationDigest !==
              authenticatedObservationDigest ||
            decision.launchScope.length !== scope.length ||
            decision.launchScope.some(
              (category, scopeIndex) => category !== scope[scopeIndex],
            )
          ) {
            throw new Error(
              "production classifier changed the authenticated queue identity",
            );
          }
          const prior = persistedByObservation.get(
            `${decision.headerHash}\u0000${decision.authenticatedObservationDigest}`,
          );
          if (
            prior !== undefined &&
            prior.decision.decisionDigest !== decision.decisionDigest
          ) {
            throw new Error(
              "fresh production classification differs from durable decision evidence",
            );
          }
          input.dependencies.operationsSink?.recordVerification({
            subjectDigest: authenticatedObservationDigest,
            queuedAtMs,
            startedAtMs,
            completedAtMs: nowMs().toString(),
            elapsedMs: Math.ceil(
              monotonicNowMs() - startedMonotonicMs,
            ).toString(),
            outcome:
              decision.decision === "fault_detected"
                ? "fault_detected"
                : decision.decision === "healthy"
                  ? "verified"
                  : "unprovable_gap",
          });
          if (contextIdentity !== undefined) {
            classifiedBindings.set(header.headerHash, {
              contextIdentity,
              predecessor,
              deadline,
            });
          }
          classifications[index] = decision;
        } catch (error) {
          classificationFailed = true;
          classificationFailure = error;
          if (verificationSubjectDigest !== null) {
            try {
              input.dependencies.operationsSink?.recordVerification({
                subjectDigest: verificationSubjectDigest,
                queuedAtMs,
                startedAtMs,
                completedAtMs: nowMs().toString(),
                elapsedMs: Math.ceil(
                  monotonicNowMs() - startedMonotonicMs,
                ).toString(),
                outcome: "failed",
              });
            } catch (diagnosticError) {
              classificationFailure = new AggregateError(
                [error, diagnosticError],
                "fault classification and failure diagnostics failed",
                { cause: error },
              );
            }
          }
        }
      }
    };
    await Promise.all(
      Array.from(
        {
          length: Math.min(
            input.maximumClassificationConcurrency,
            candidate.finalizedHeaders.length,
          ),
        },
        async () => await classifyNext(),
      ),
    );
    if (classificationFailed) throw classificationFailure;
    const decisions: HeaderDecision[] = [];
    for (let index = 0; index < classifications.length; index += 1) {
      const classification = classifications[index];
      if (classification === undefined)
        throw new Error("bounded production classification omitted a header");
      if (classification !== null) decisions.push(classification);
    }
    for (const decision of decisions) {
      if (decision === undefined) {
        throw new Error("bounded production classification omitted a header");
      }
      await input.dependencies.append(decision);
    }
    // Classification may finish out of order, but append and CorrectionLock
    // target selection remain in exact finalized queue order.
    if (token !== classificationEpoch) {
      throw new Error("state-queue authority changed during decision append");
    }
    input.dependencies.assertObservation(candidate);
    if (retainedPendingDecision !== null || reusedTargetDecision !== null)
      assertRetainedTargetAuthority();
    if (
      contextIdentity !== undefined &&
      (await input.dependencies.classificationContextIdentity?.()) !==
        contextIdentity
    ) {
      throw new Error(
        "classifier configuration changed during decision preparation",
      );
    }
    if (token !== classificationEpoch) {
      throw new Error(
        "state-queue authority changed during context validation",
      );
    }
    input.dependencies.assertObservation(candidate);
    if (retainedPendingDecision !== null || reusedTargetDecision !== null)
      assertRetainedTargetAuthority();
    const selected = selectedTarget({ observation: candidate, decisions });
    const selectedDecision =
      selected === null
        ? null
        : (decisions.find(
            (
              decision,
            ): decision is Extract<
              HeaderDecision,
              { readonly decision: "fault_detected" }
            > =>
              decision.decision === "fault_detected" &&
              decision.decisionDigest === selected.decisionDigest,
          ) ?? null);
    if (selected !== null && selectedDecision === null) {
      throw new Error(
        "selected fault target has no exact admitted classification decision",
      );
    }
    const selectedHeader =
      selected === null
        ? null
        : (candidate.finalizedHeaders.find(
            ({ headerHash }) => headerHash === selected.headerHash,
          ) ?? null);
    if (selected !== null && selectedHeader === null) {
      throw new Error(
        "selected fault target has no authenticated HeaderV1 observation",
      );
    }
    const selectedDeadline =
      selectedHeader === null
        ? null
        : input.dependencies.deadlineForHeader(selectedHeader);
    const preservesActuation =
      selected !== null &&
      target !== null &&
      selected.category === target.category &&
      selected.headerHash === target.headerHash &&
      selected.decisionDigest === target.decisionDigest &&
      selectedDeadline !== null &&
      targetDeadline !== null &&
      selectedDeadline.headerEndTimeMs === targetDeadline.headerEndTimeMs &&
      selectedDeadline.maturityAtMs === targetDeadline.maturityAtMs &&
      selectedDeadline.latestSafeStartAtMs ===
        targetDeadline.latestSafeStartAtMs &&
      actuationController !== null;
    if (
      !preservesActuation &&
      ((retainedPendingDecision !== null &&
        selectedDecision === retainedPendingDecision) ||
        (reusedTargetDecision !== null &&
          selectedDecision === reusedTargetDecision))
    ) {
      actuationController?.restrictToReconciliation(
        "state_queue_target_changed",
      );
      throw new Error(
        "retained classification cannot mint replacement fault submission authority",
      );
    }
    if (!preservesActuation) {
      if (actuationController !== null) {
        actuationController.restrictToReconciliation(
          "state_queue_target_changed",
        );
        reconciliationControllers.add(actuationController);
      }
      rollbackGeneration += 1;
      actuationController =
        selectedDecision === null
          ? null
          : input.dependencies.createActuationController(
              selectedDecision,
              rollbackGeneration.toString(),
            );
    }
    observation = candidate;
    target = selected;
    targetDecision = selectedDecision;
    targetDeadline = selectedDeadline;
    targetClassification =
      pendingAvailability.size === 0 && selected !== null
        ? (classifiedBindings.get(selected.headerHash) ?? null)
        : null;
    preparedResult = Object.freeze({
      observationDigest: candidate.observationDigest,
      decisionDigests: Object.freeze(
        decisions.map(({ decisionDigest }) => decisionDigest),
      ),
      target: selected,
    });
    input.dependencies.retainDecisionAuthorities(
      selected?.decisionDigest ?? null,
    );
    return preparedResult;
  };

  const serializedPrepare = (
    candidate: WatcherAuthenticatedStateQueueObservation,
    dispatch: boolean,
  ): Promise<WatcherFaultDecisionBridgeResult> => {
    const result = serial.then(async () => {
      let prepared: WatcherFaultDecisionBridgeResult;
      try {
        prepared = await prepare(candidate);
      } catch (error) {
        targetClassification = null;
        input.dependencies.retainDecisionAuthorities(
          target?.decisionDigest ?? null,
        );
        throw error;
      }
      // Keep reconciliation and its exact selected decision inside the same
      // serializer turn. A later prepare/rollback must not replace globals in
      // the gap between prepare resolution and enqueue.
      if (dispatch) {
        const exactDecision = targetDecision;
        const exactController = actuationController;
        const exactDeadline = targetDeadline;
        if (
          exactDecision !== null &&
          exactController !== null &&
          exactDeadline !== null
        ) {
          await input.dependencies.enqueue(
            exactDecision,
            exactController.permit,
            exactDeadline,
            rollbackGeneration.toString(),
          );
        }
      }
      return prepared;
    });
    serial = result.then(
      () => undefined,
      () => undefined,
    );
    return result;
  };

  const dispatchPrepared = (): Promise<unknown> | null => {
    if (
      target === null ||
      targetDecision === null ||
      observation === null ||
      actuationController === null ||
      targetDeadline === null
    ) {
      return null;
    }
    const exactDecision = targetDecision;
    const exactController = actuationController;
    const exactGeneration = rollbackGeneration.toString();
    return input.dependencies.enqueue(
      exactDecision,
      exactController.permit,
      targetDeadline,
      exactGeneration,
    );
  };

  return Object.freeze({
    schemaVersion: WATCHER_FAULT_DECISION_BRIDGE_SCHEMA_VERSION,
    prepareForRecovery: async (candidate) =>
      await serializedPrepare(candidate, false),
    recoverExisting: async () => {
      if (observation === null) {
        throw new Error(
          "fault-proof recovery requires a fresh authenticated queue reconciliation",
        );
      }
      const recoveryObservation = observation;
      const recoveryEpoch = classificationEpoch;
      const recoveryGeneration = rollbackGeneration;
      const assertRecoveryCurrent = (): void => {
        if (
          observation !== recoveryObservation ||
          classificationEpoch !== recoveryEpoch ||
          rollbackGeneration !== recoveryGeneration
        )
          throw new Error(
            "queue authority changed during existing workflow recovery",
          );
        input.dependencies.assertObservation(recoveryObservation);
      };
      const reconciliations: WatcherFaultProofReconciliation[] = [];
      if (input.dependencies.loadExistingExecution !== undefined) {
        const seen = new Set<string>();
        for (const { decision } of await input.dependencies.readRecords()) {
          assertRecoveryCurrent();
          if (
            decision.decision !== "fault_detected" ||
            seen.has(decision.decisionDigest) ||
            observation.finalizedHeaders.some(
              (header) => header.headerHash === decision.headerHash,
            )
          )
            continue;
          seen.add(decision.decisionDigest);
          const entries =
            await input.dependencies.loadExistingExecution(decision);
          assertRecoveryCurrent();
          if (
            entries.length === 0 ||
            entries.some(({ event }) => event.kind === "completed") ||
            !entries.some(({ event }) => event.kind === "submission_intent")
          )
            continue;
          const controller = createWorkflowReconciliationPermitController({
            decision,
            deploymentFingerprint,
            entries,
            rollbackGeneration: rollbackGeneration.toString(),
          });
          reconciliationControllers.add(controller);
          reconciliations.push({
            category: decision.category as WatcherInstalledWorkflowCategory,
            headerHash: decision.headerHash,
            decisionDigest: decision.decisionDigest,
            actuationPermit: controller.permit,
          });
        }
      }
      assertRecoveryCurrent();
      return await input.dependencies.recover(
        targetDecision,
        actuationController?.permit ?? null,
        targetDeadline,
        rollbackGeneration.toString(),
        reconciliations,
      );
    },
    reconcileAndDispatch: async (candidate) => {
      return await serializedPrepare(candidate, true);
    },
    dispatchPrepared,
    invalidateForRollback: () => invalidate("native_chain_rollback"),
    beforeHistoryAdvance: () => {
      // Ordinary canonical append cannot revoke an already classified historical
      // fault before the following queue observation can reconcile its removal.
      // Rollback and explicit history-generation loss use the invalidators below.
      classificationEpoch += 1;
      input.dependencies.retainDecisionAuthorities(
        targetDecision?.decisionDigest ?? null,
      );
    },
    invalidateForHistoryChange: () => invalidate("local_event_history_change"),
    invalidateForShutdown: () => invalidate("watcher_shutdown"),
    isJobPermitted: (job) => {
      if (observation === null) {
        throw new Error(
          "fault-proof runner has no current authenticated state-queue authority",
        );
      }
      input.dependencies.assertObservation(observation);
      for (const controller of reconciliationControllers) {
        try {
          const identity = assertWorkflowActuationPermitIdentity({
            permit: controller.permit,
            category: job.category,
            rollbackGeneration: job.rollbackGeneration,
          });
          if (
            job.mode === "resume" &&
            identity.authority === "reconciliation" &&
            identity.headerHash === job.headerHash &&
            identity.decisionDigest === job.decisionDigest &&
            identity.deploymentFingerprint === deploymentFingerprint
          )
            return true;
        } catch {
          // A different execution/generation never grants this job authority.
        }
      }
      return (
        target !== null &&
        target.category === job.category &&
        target.headerHash === job.headerHash &&
        target.decisionDigest === job.decisionDigest &&
        rollbackGeneration.toString() === job.rollbackGeneration &&
        observation.finalizedHeaders.some(
          ({ headerHash }) => headerHash === job.headerHash,
        )
      );
    },
    status: () =>
      Object.freeze({
        observationDigest: observation?.observationDigest ?? null,
        target,
      }),
  });
};

export const createWatcherFaultDecisionBridge = async (input: {
  readonly application: WatcherFaultProofApplication;
  readonly supervisor: WatcherFaultProofSupervisor;
  readonly stateQueueSource: WatcherStateQueueObservationSource;
  readonly journalDirectory: string;
  readonly runtimeConfigPath: string;
  readonly maximumClassificationConcurrency: number;
  readonly operationsSink?: WatcherOperationsSink;
  readonly pendingAvailabilityHeaders?: BridgeDependencies["pendingAvailabilityHeaders"];
  readonly nowMs?: () => bigint;
}): Promise<WatcherFaultDecisionBridge> => {
  assertWatcherFaultProofApplication(input.application);
  const journal = await openWatcherFaultDecisionJournal({
    directory: input.journalDirectory,
    deploymentFingerprint: input.application.deploymentFingerprint,
    launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  });
  return createBridge({
    application: input.application,
    runtimeConfigPath: input.runtimeConfigPath,
    maximumClassificationConcurrency: input.maximumClassificationConcurrency,
    dependencies: Object.freeze({
      ...(input.pendingAvailabilityHeaders === undefined
        ? {}
        : { pendingAvailabilityHeaders: input.pendingAvailabilityHeaders }),
      assertObservation: assertWatcherStateQueueObservation,
      retainDecisionAuthorities: input.application.retainDecisionAuthorities,
      decisionUsesLocalEventHistory:
        input.application.decisionUsesLocalEventHistory,
      observationDigest: async (candidate) =>
        await authenticatedStateQueueObservationDigest({
          observation: candidate,
          minimumConfirmationDepth: RELEASE_CONFIRMATION_DEPTH,
        }),
      classificationContextIdentity: async () => {
        const path = await realpath(input.runtimeConfigPath);
        const contents = await readFile(path);
        return createHash("sha256")
          .update(path)
          .update("\0")
          .update(contents)
          .digest("hex");
      },
      readRecords: journal.readAll,
      loadExistingExecution: async (decision) => {
        const workflowId = computeFraudProofWorkflowId({
          schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
          deploymentFingerprint: decision.deploymentFingerprint,
          category: decision.category,
          target: {
            kind: "state_queue_header",
            headerHash: decision.headerHash,
          },
          decisionDigest: decision.decisionDigest,
        });
        return await new DirectoryFraudProofWorkflowJournalStore(
          join(
            input.journalDirectory,
            "fault-proofs",
            decision.category,
            decision.headerHash,
          ),
        ).load(workflowId);
      },
      append: journal.appendLiveDecision,
      assertActuationPermitIdentity: assertWorkflowActuationPermitIdentity,
      createActuationController: (decision, rollbackGeneration) =>
        createWorkflowActuationPermitController({
          decision,
          rollbackGeneration,
        }),
      deadlineForHeader: watcherFaultProofDeadline,
      resolvePredecessorHeader: async (header) => {
        const decoded = Data.from(header.headerCborHex, Header);
        if (decoded.prevHeaderHash === GENESIS_HEADER_HASH) return undefined;
        return await input.stateQueueSource.resolveRetainedHeader({
          headerHash: decoded.prevHeaderHash,
        });
      },
      ...(input.operationsSink === undefined
        ? {}
        : { operationsSink: input.operationsSink }),
      ...(input.nowMs === undefined ? {} : { nowMs: input.nowMs }),
      enqueue: async (
        decision,
        actuationPermit,
        deadline,
        rollbackGeneration,
      ) =>
        await enqueueWatcherFaultDecision({
          supervisor: input.supervisor,
          decision,
          actuationPermit,
          deadline,
          rollbackGeneration,
        }),
      recover: async (
        decision,
        actuationPermit,
        deadline,
        rollbackGeneration,
        reconciliations,
      ) =>
        await input.supervisor.recoverExisting(
          decision,
          actuationPermit ?? undefined,
          deadline ?? undefined,
          rollbackGeneration,
          reconciliations,
        ),
    }),
  });
};

/** Test-only dependency seam; it never mints production application authority. */
export const unsafeCreateWatcherFaultDecisionBridgeForTest = (
  input: Readonly<{
    application: BridgeApplication;
    runtimeConfigPath: string;
    maximumClassificationConcurrency: number;
    dependencies: BridgeDependencies;
  }>,
): WatcherFaultDecisionBridge => createBridge(input);
