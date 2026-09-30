import {
  type HeaderDecision,
  LocalKupmiosCheckpointChangedError,
  type WorkflowActuationPermitController,
} from "@al-ft/midgard-fault-proofs";

import {
  type WatcherAuthenticatedStateQueueObservation,
  WatcherRetainedHeaderAttestationPendingError,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherNativeBlockAdmission } from "../l1/native-block-admission.js";
import { watcherSameCanonicalJson } from "../storage/durable-store.js";
import { preservesPendingTargetEvidence } from "./fault-decision-bridge.preserves-pending-target-evidence.js";
import {
  assertExactFinalizedHeaderOrder,
  authenticatedHeaderObservation,
  type BridgeApplication,
  type BridgeDependencies,
  exactInstalledScope,
  MAXIMUM_CLASSIFICATION_CONCURRENCY,
  observationPreservesTarget,
  preservesObservationAuthority,
  selectedTarget,
  WATCHER_FAULT_DECISION_BRIDGE_SCHEMA_VERSION,
  type WatcherFaultDecisionBridge,
  type WatcherFaultDecisionBridgeResult,
  WatcherFaultDecisionRetired,
  type WatcherFaultDecisionTarget,
} from "./fault-decision-bridge.selected-target.js";
import { type WatcherPersistedFaultDecisionRecord } from "./fault-decision-journal.js";
import { type WatcherFaultProofDeadline } from "./fault-proof-supervisor.js";

export const createBridge = (input: {
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
  let preparedResult: WatcherFaultDecisionBridgeResult | null = null;
  let serial: Promise<void> = Promise.resolve();
  let classificationDeferred = false;
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
    input.dependencies.revokeAuthority(reason);
    actuationController = null;
    classificationEpoch += 1;
    rollbackGeneration += 1;
    observation = null;
    target = null;
    targetDecision = null;
    targetDeadline = null;
    preparedResult = null;
    targetClassification = null;
    classificationDeferred = false;
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
      (await input.dependencies.pendingAvailabilityHeaders?.(candidate)) ??
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
      // A new observation replaces classification context, not an invocation's
      // permit. The supervisor retains active attempts until they return.
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
    let deferredFromIndex = candidate.finalizedHeaders.length;
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
          let predecessor: WatcherStateQueueHeaderObservation | undefined;
          try {
            predecessor =
              await input.dependencies.resolvePredecessorHeader?.(header);
          } catch (error) {
            if (
              !(error instanceof WatcherRetainedHeaderAttestationPendingError)
            )
              throw error;
            // The predecessor's public DA attachment is not included
            // yet. Classification of this header and every later one waits
            // for the next observation; earlier headers keep their decisions
            // so target selection still runs over a fully classified prefix.
            deferredFromIndex = Math.min(deferredFromIndex, index);
            classifications[index] = null;
            continue;
          }
          if (token !== classificationEpoch) {
            throw new WatcherFaultDecisionRetired(
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
            throw new WatcherFaultDecisionRetired(
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
          if (error instanceof LocalKupmiosCheckpointChangedError) {
            // Capture drift is an incomplete classification, not fault evidence.
            // Preserve only the classified prefix and retry this suffix on the
            // next canonical wake through the existing deferred path.
            deferredFromIndex = Math.min(deferredFromIndex, index);
            classifications[index] = null;
            continue;
          }
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
    for (
      let index = deferredFromIndex;
      index < classifications.length;
      index += 1
    ) {
      classifications[index] = null;
    }
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
      throw new WatcherFaultDecisionRetired(
        "state-queue authority changed during decision append",
      );
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
      throw new WatcherFaultDecisionRetired(
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
      throw new Error(
        "retained classification cannot mint replacement fault submission authority",
      );
    }
    if (!preservesActuation) {
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
    classificationDeferred =
      pendingAvailability.size > 0 ||
      deferredFromIndex < candidate.finalizedHeaders.length;
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
      if (dispatch) await dispatchPrepared();
      return prepared;
    });
    serial = result.then(
      () => undefined,
      () => undefined,
    );
    return result;
  };

  const dispatchPrepared = (
    nativeProgress?: WatcherNativeBlockAdmission,
  ): Promise<void> | null => {
    if (observation === null) return null;
    input.dependencies.assertObservation(observation);
    return input.dependencies.requestProgress({
      observation,
      ...(nativeProgress === undefined ? {} : { nativeProgress }),
      rollbackGeneration: rollbackGeneration.toString(),
      ...(targetDecision?.decision === "fault_detected" &&
      actuationController !== null &&
      targetDeadline !== null
        ? {
            fault: {
              decision: targetDecision,
              actuationPermit: actuationController.permit,
              deadline: targetDeadline,
            },
          }
        : {}),
    });
  };

  return Object.freeze({
    schemaVersion: WATCHER_FAULT_DECISION_BRIDGE_SCHEMA_VERSION,
    prepareForRecovery: async (candidate) =>
      await serializedPrepare(candidate, false),
    recoverExisting: async (progress) => {
      const request = dispatchPrepared(progress?.nativeProgress);
      if (request === null)
        throw new Error(
          "fault-proof recovery requires a fresh authenticated queue reconciliation",
        );
      await request;
      return input.dependencies.unfinishedObjectiveCount();
    },
    reconcileAndDispatch: async (candidate) => {
      return await serializedPrepare(candidate, true);
    },
    retryDeferredClassification: async (candidate) => {
      await serial;
      if (classificationDeferred) await serializedPrepare(candidate, true);
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
    status: () =>
      Object.freeze({
        observationDigest: observation?.observationDigest ?? null,
        target,
      }),
  });
};
