import {
  authenticatedStateQueueObservationDigestV1,
  createProductionWorkflowActuationPermitControllerV1,
  type ProductionHeaderDecisionV1,
  type ProductionWorkflowActuationPermitControllerV1,
  type ProductionWorkflowActuationPermitV1,
} from "@al-ft/midgard-fault-proofs";
import {
  type AuthenticatedStateQueueHeaderObservationV1,
  CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
  EMPTY_MERKLE_TREE_ROOT,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  HeaderV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  openWatcherProductionFaultDecisionJournalV1,
  type WatcherPersistedProductionFaultDecisionRecordV1,
} from "./production-fault-decision-journal-v1.js";
import {
  assertWatcherFaultProofProductionApplicationV1,
  WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
  type WatcherFaultProofProductionApplicationV1,
  type WatcherInstalledProductionWorkflowCategoryV1,
} from "./production-fault-proof-application-v1.js";
import {
  enqueueWatcherProductionFaultDecisionV1,
  type WatcherProductionFaultProofDeadlineV1,
  watcherProductionFaultProofDeadlineV1,
  type WatcherProductionFaultProofJobV1,
  type WatcherProductionFaultProofSupervisorV1,
} from "./production-fault-proof-supervisor-v1.js";
import type { WatcherProductionOperationsSinkV1 } from "./production-operations-observability-v1.js";
import {
  assertWatcherProductionStateQueueObservationV1,
  type WatcherProductionCorrectionLockObservationV1,
  type WatcherProductionStateQueueHeaderObservationV1,
  type WatcherProductionStateQueueObservationSourceV1,
  type WatcherProductionStateQueueObservationV1,
} from "./production-state-queue-observation-v1.js";

export const WATCHER_PRODUCTION_FAULT_DECISION_BRIDGE_V1_SCHEMA_VERSION =
  "midgard-watcher-production-fault-decision-bridge-v1" as const;

const RELEASE_CONFIRMATION_DEPTH = 30;
const MAXIMUM_CLASSIFICATION_CONCURRENCY = 64;

export type WatcherProductionFaultDecisionTargetV1 = Readonly<{
  category: WatcherInstalledProductionWorkflowCategoryV1;
  headerHash: string;
  decisionDigest: string;
}>;

export type WatcherProductionFaultDecisionBridgeResultV1 = Readonly<{
  observationDigest: string;
  decisionDigests: readonly string[];
  target: WatcherProductionFaultDecisionTargetV1 | null;
}>;

export type WatcherProductionFaultDecisionBridgeV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_FAULT_DECISION_BRIDGE_V1_SCHEMA_VERSION;
  /**
   * Re-authenticates and journals the current queue without starting work.
   * Startup uses this before scanning existing workflow journals.
   */
  prepareForRecovery(
    observation: WatcherProductionStateQueueObservationV1,
  ): Promise<WatcherProductionFaultDecisionBridgeResultV1>;
  /** Scans durable workflows only after a fresh opaque decision is prepared. */
  recoverExisting(): Promise<number>;
  /** Reconciles one finalized queue cursor and schedules its one allowed fault. */
  reconcileAndDispatch(
    observation: WatcherProductionStateQueueObservationV1,
  ): Promise<WatcherProductionFaultDecisionBridgeResultV1>;
  /** Schedules the target selected by the most recent successful prepare. */
  dispatchPrepared(): Promise<unknown> | null;
  /** Invalidates all runnable authority synchronously on native rollback. */
  invalidateForRollback(): void;
  /** Revokes all runnable authority before production shutdown can await I/O. */
  invalidateForShutdown(): void;
  /** Called immediately before any new or resumed workflow may execute. */
  isJobPermitted(
    job: Pick<
      WatcherProductionFaultProofJobV1,
      | "mode"
      | "category"
      | "headerHash"
      | "decisionDigest"
      | "rollbackGeneration"
    >,
  ): boolean;
  status(): Readonly<{
    observationDigest: string | null;
    target: WatcherProductionFaultDecisionTargetV1 | null;
  }>;
}>;

type BridgeApplicationV1 = Pick<
  WatcherFaultProofProductionApplicationV1,
  "classifyHeader" | "deploymentFingerprint" | "installedCategories"
>;

type BridgeDependenciesV1 = Readonly<{
  assertObservation(
    observation: WatcherProductionStateQueueObservationV1,
  ): void;
  observationDigest(
    observation: AuthenticatedStateQueueHeaderObservationV1,
  ): Promise<string>;
  readRecords(): Promise<
    readonly WatcherPersistedProductionFaultDecisionRecordV1[]
  >;
  append(
    decision: ProductionHeaderDecisionV1,
  ): Promise<WatcherPersistedProductionFaultDecisionRecordV1>;
  createActuationController(
    decision: Extract<
      ProductionHeaderDecisionV1,
      { readonly decision: "fault_detected" }
    >,
    rollbackGeneration: string,
  ): ProductionWorkflowActuationPermitControllerV1;
  deadlineForHeader(
    header: WatcherProductionStateQueueHeaderObservationV1,
  ): WatcherProductionFaultProofDeadlineV1;
  resolvePredecessorHeader?(
    header: WatcherProductionStateQueueHeaderObservationV1,
  ): Promise<WatcherProductionStateQueueHeaderObservationV1 | undefined>;
  operationsSink?: WatcherProductionOperationsSinkV1;
  nowMs?(): bigint;
  enqueue(
    decision: ProductionHeaderDecisionV1,
    actuationPermit: ProductionWorkflowActuationPermitV1,
    deadline: WatcherProductionFaultProofDeadlineV1,
    rollbackGeneration: string,
  ): Promise<unknown>;
  recover(
    decision: ProductionHeaderDecisionV1 | null,
    actuationPermit: ProductionWorkflowActuationPermitV1 | null,
    deadline: WatcherProductionFaultProofDeadlineV1 | null,
    rollbackGeneration: string,
  ): Promise<number>;
}>;

const exactInstalledScope = (
  categories: readonly string[],
): readonly WatcherInstalledProductionWorkflowCategoryV1[] => {
  if (
    categories.length !==
      WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1.length ||
    categories.some(
      (category, index) =>
        category !== WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1[index],
    )
  ) {
    throw new Error(
      "fault decision bridge application scope differs from the installed application",
    );
  }
  return WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1;
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
  source: WatcherProductionStateQueueObservationV1,
  header: WatcherProductionStateQueueHeaderObservationV1,
): AuthenticatedStateQueueHeaderObservationV1 => {
  const decoded = Data.from(header.headerCborHex, HeaderV1);
  if (Data.to(decoded, HeaderV1) !== header.headerCborHex) {
    throw new Error("authenticated state-queue HeaderV1 CBOR is noncanonical");
  }
  return Object.freeze({
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
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
  observation: WatcherProductionStateQueueObservationV1,
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
  lock: WatcherProductionCorrectionLockObservationV1,
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
  readonly observation: WatcherProductionStateQueueObservationV1;
  readonly decisions: readonly ProductionHeaderDecisionV1[];
}): WatcherProductionFaultDecisionTargetV1 | null => {
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
      ProductionHeaderDecisionV1,
      { readonly decision: "fault_detected" }
    > => decision.decision === "fault_detected",
  );
  if (lock.datum === "Idle") {
    const first = faults[0];
    return first === undefined
      ? null
      : Object.freeze({
          category:
            first.category as WatcherInstalledProductionWorkflowCategoryV1,
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
    category: match.category as WatcherInstalledProductionWorkflowCategoryV1,
    headerHash: match.headerHash,
    decisionDigest: match.decisionDigest,
  });
};

const observationPreservesTarget = (
  observation: WatcherProductionStateQueueObservationV1,
  target: WatcherProductionFaultDecisionTargetV1,
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

const createBridge = (input: {
  readonly application: BridgeApplicationV1;
  readonly runtimeConfigPath: string;
  readonly maximumClassificationConcurrency: number;
  readonly dependencies: BridgeDependenciesV1;
}): WatcherProductionFaultDecisionBridgeV1 => {
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
  let observation: WatcherProductionStateQueueObservationV1 | null = null;
  let target: WatcherProductionFaultDecisionTargetV1 | null = null;
  let targetDecision: ProductionHeaderDecisionV1 | null = null;
  let targetDeadline: WatcherProductionFaultProofDeadlineV1 | null = null;
  let actuationController: ProductionWorkflowActuationPermitControllerV1 | null =
    null;
  let preparedResult: WatcherProductionFaultDecisionBridgeResultV1 | null =
    null;
  let serial: Promise<void> = Promise.resolve();

  const invalidate = (reason: string): void => {
    actuationController?.revoke(reason);
    actuationController = null;
    classificationEpoch += 1;
    rollbackGeneration += 1;
    observation = null;
    target = null;
    targetDecision = null;
    targetDeadline = null;
    preparedResult = null;
  };

  const prepare = async (
    candidate: WatcherProductionStateQueueObservationV1,
  ): Promise<WatcherProductionFaultDecisionBridgeResultV1> => {
    input.dependencies.assertObservation(candidate);
    if (candidate.deploymentIdentityDigest !== deploymentFingerprint) {
      throw new Error(
        "state-queue observation differs from the fault-proof deployment",
      );
    }
    if (
      observation?.observationDigest === candidate.observationDigest &&
      preparedResult !== null
    ) {
      return preparedResult;
    }
    const token = ++classificationEpoch;
    assertExactFinalizedHeaderOrder(candidate);
    if (
      target !== null &&
      actuationController !== null &&
      !observationPreservesTarget(candidate, target)
    ) {
      actuationController.revoke("state_queue_target_changed");
      actuationController = null;
      rollbackGeneration += 1;
      observation = null;
      target = null;
      targetDecision = null;
      targetDeadline = null;
      preparedResult = null;
    }
    const persisted = await input.dependencies.readRecords();
    const persistedByObservation = new Map<
      string,
      WatcherPersistedProductionFaultDecisionRecordV1
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
    const decisions = new Array<ProductionHeaderDecisionV1>(
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
        const nowMs = input.dependencies.nowMs ?? (() => BigInt(Date.now()));
        const queuedAtMs = nowMs().toString();
        let startedAtMs = queuedAtMs;
        let verificationSubjectDigest: string | null = null;
        try {
          const admitted = authenticatedHeaderObservation(candidate, header);
          const authenticatedObservationDigest =
            await input.dependencies.observationDigest(admitted);
          verificationSubjectDigest = authenticatedObservationDigest;
          startedAtMs = nowMs().toString();
          const predecessor =
            await input.dependencies.resolvePredecessorHeader?.(header);
          const decision = await input.application.classifyHeader({
            runtimeConfigPath: input.runtimeConfigPath,
            observation: admitted,
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
            outcome:
              decision.decision === "fault_detected"
                ? "fault_detected"
                : decision.decision === "healthy"
                  ? "verified"
                  : "unprovable_gap",
          });
          decisions[index] = decision;
        } catch (error) {
          if (verificationSubjectDigest !== null) {
            const failedAtMs = nowMs().toString();
            input.dependencies.operationsSink?.recordVerification({
              subjectDigest: verificationSubjectDigest,
              queuedAtMs,
              startedAtMs,
              completedAtMs: failedAtMs,
              outcome: "failed",
            });
          }
          classificationFailed = true;
          classificationFailure = error;
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
    const selected = selectedTarget({ observation: candidate, decisions });
    const selectedDecision =
      selected === null
        ? null
        : (decisions.find(
            (
              decision,
            ): decision is Extract<
              ProductionHeaderDecisionV1,
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
    if (!preservesActuation) {
      actuationController?.revoke("state_queue_target_changed");
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
    preparedResult = Object.freeze({
      observationDigest: candidate.observationDigest,
      decisionDigests: Object.freeze(
        decisions.map(({ decisionDigest }) => decisionDigest),
      ),
      target: selected,
    });
    return preparedResult;
  };

  const serializedPrepare = (
    candidate: WatcherProductionStateQueueObservationV1,
    dispatch: boolean,
  ): Promise<WatcherProductionFaultDecisionBridgeResultV1> => {
    const result = serial.then(async () => {
      const prepared = await prepare(candidate);
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
    schemaVersion: WATCHER_PRODUCTION_FAULT_DECISION_BRIDGE_V1_SCHEMA_VERSION,
    prepareForRecovery: async (candidate) =>
      await serializedPrepare(candidate, false),
    recoverExisting: async () => {
      if (observation === null) {
        throw new Error(
          "fault-proof recovery requires a fresh authenticated queue reconciliation",
        );
      }
      return await input.dependencies.recover(
        targetDecision,
        actuationController?.permit ?? null,
        targetDeadline,
        rollbackGeneration.toString(),
      );
    },
    reconcileAndDispatch: async (candidate) => {
      return await serializedPrepare(candidate, true);
    },
    dispatchPrepared,
    invalidateForRollback: () => invalidate("native_chain_rollback"),
    invalidateForShutdown: () => invalidate("watcher_shutdown"),
    isJobPermitted: (job) => {
      if (observation === null) {
        throw new Error(
          "fault-proof runner has no current authenticated state-queue authority",
        );
      }
      input.dependencies.assertObservation(observation);
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

export const createWatcherProductionFaultDecisionBridgeV1 = async (input: {
  readonly application: WatcherFaultProofProductionApplicationV1;
  readonly supervisor: WatcherProductionFaultProofSupervisorV1;
  readonly stateQueueSource: WatcherProductionStateQueueObservationSourceV1;
  readonly journalDirectory: string;
  readonly runtimeConfigPath: string;
  readonly maximumClassificationConcurrency: number;
  readonly operationsSink?: WatcherProductionOperationsSinkV1;
  readonly nowMs?: () => bigint;
}): Promise<WatcherProductionFaultDecisionBridgeV1> => {
  assertWatcherFaultProofProductionApplicationV1(input.application);
  const journal = await openWatcherProductionFaultDecisionJournalV1({
    directory: input.journalDirectory,
    deploymentFingerprint: input.application.deploymentFingerprint,
    launchScope: WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
  });
  return createBridge({
    application: input.application,
    runtimeConfigPath: input.runtimeConfigPath,
    maximumClassificationConcurrency: input.maximumClassificationConcurrency,
    dependencies: Object.freeze({
      assertObservation: assertWatcherProductionStateQueueObservationV1,
      observationDigest: async (candidate) =>
        await authenticatedStateQueueObservationDigestV1({
          observation: candidate,
          minimumConfirmationDepth: RELEASE_CONFIRMATION_DEPTH,
        }),
      readRecords: journal.readAll,
      append: journal.appendLiveDecision,
      createActuationController: (decision, rollbackGeneration) =>
        createProductionWorkflowActuationPermitControllerV1({
          decision,
          rollbackGeneration,
        }),
      deadlineForHeader: watcherProductionFaultProofDeadlineV1,
      resolvePredecessorHeader: async (header) => {
        const decoded = Data.from(header.headerCborHex, HeaderV1);
        if (decoded.prevUtxosRoot === EMPTY_MERKLE_TREE_ROOT) return undefined;
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
        await enqueueWatcherProductionFaultDecisionV1({
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
      ) =>
        await input.supervisor.recoverExisting(
          decision,
          actuationPermit ?? undefined,
          deadline ?? undefined,
          rollbackGeneration,
        ),
    }),
  });
};

/** Test-only dependency seam; it never mints production application authority. */
export const unsafeCreateWatcherProductionFaultDecisionBridgeForTestV1 = (
  input: Readonly<{
    application: BridgeApplicationV1;
    runtimeConfigPath: string;
    maximumClassificationConcurrency: number;
    dependencies: BridgeDependenciesV1;
  }>,
): WatcherProductionFaultDecisionBridgeV1 => createBridge(input);
