import "./listen-router.post-tx-status-batch-handler.js";

import { HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql/SqlClient";
import { Effect, Option, Ref } from "effect";

import {
  DaPayloadPublicationsDB,
  DaPayloadTerminalOutcomesDB,
  ForeignTipReconciliationsDB,
  MempoolDB,
  MutationJobsDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
} from "../database/index.js";
import * as HistoryAuthority from "../database/eventHistoryAuthority.js";
import { foreignBaseVerificationForAuthority } from "../services/foreign-base-verification.js";
import { attestationTimeoutCorrectionReadinessBounds } from "../fibers/index.js";
import {
  localOgmiosSubmitSlotEvidence,
  readLocalOgmiosSubmitSlot,
} from "../local-ogmios-slot.js";
import {
  DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  ValidationPool,
} from "../services/index.js";
import {
  ContractDeploymentIdentity,
  Globals,
  Lucid,
  MidgardContracts,
  nextL1ProviderHealthEvidence,
  NodeConfig,
  withL1ControlPlaneIfAvailable,
} from "../services/index.js";
import * as Initialization from "../transactions/initialization.js";
import {
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  planMergePreflight,
} from "../transactions/state-queue/merge-readiness.js";
import { runL1ProviderPreflight } from "./l1-provider-preflight.js";
import {
  l1ProviderReadinessEvidenceIsFresh,
  type L1ProviderReadinessProbe,
  READINESS_L1_PROVIDER_LIVE_TIMEOUT_MS,
  runBoundedDirectL1ProviderPreflight,
} from "./listen-router.l1-provider-readiness-evidence-is-fresh.js";
import {
  resolveL1ProviderReadinessSnapshot,
  runBusyL1ProviderReadinessProbe,
  runCombinedL1ReadinessProbe,
} from "./listen-router.run-busy-l1-provider-readiness-probe.js";
import { evaluateReadiness } from "./readiness.js";

/**
 * `GET /readyz`: readiness endpoint that checks worker heartbeats, queue depth,
 * local recovery state, and database connectivity.
 */
export const getReadinessHandler = Effect.gen(function* () {
  const globals = yield* Globals;
  const nodeConfig = yield* NodeConfig;
  const sql = yield* SqlClient;
  const validationPool = yield* ValidationPool;
  const validationPoolStats = yield* validationPool.stats;

  const durableAdmissionBacklog = yield* TxAdmissionsDB.countBacklog;
  const durableAdmissionOldestAgeMs = yield* TxAdmissionsDB.oldestQueuedAgeMs;
  const unfinishedMutationJobs = yield* MutationJobsDB.countUnfinished;
  const daPublicationConflicts = yield* DaPayloadPublicationsDB.conflictCount(
    15,
    DaPayloadTerminalOutcomesDB.deploymentIdentityDigestOf(
      yield* ContractDeploymentIdentity,
    ),
  );
  const awaitingForeignTipReconciliations =
    yield* ForeignTipReconciliationsDB.countAwaiting;
  const foreignVerificationAuthority = yield* HistoryAuthority.retrieve;
  const foreignBaseVerification = foreignBaseVerificationForAuthority(
    yield* Ref.get(globals.FOREIGN_BASE_VERIFICATION),
    Option.isSome(foreignVerificationAuthority) &&
      foreignVerificationAuthority.value.state === "ready"
      ? HistoryAuthority.tokenFromRow(foreignVerificationAuthority.value)
      : undefined,
  );
  const mempoolTxCount = yield* MempoolDB.retrieveTxCount;
  const nowMillis = Date.now();
  const stateQueueBlocksInQueue = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
  const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
  const commitWorkerActive = yield* Ref.get(globals.COMMIT_WORKER_ACTIVE);
  const commitPipelinePhase = yield* Ref.get(globals.COMMIT_PIPELINE_PHASE);
  const blockCommitmentHeartbeat = yield* Ref.get(
    globals.HEARTBEAT_BLOCK_COMMITMENT,
  );
  const blockConfirmationHeartbeat = yield* Ref.get(
    globals.HEARTBEAT_BLOCK_CONFIRMATION,
  );
  const mergeHeartbeat = yield* Ref.get(globals.HEARTBEAT_MERGE);
  const txQueueProcessorHeartbeat = yield* Ref.get(
    globals.HEARTBEAT_TX_QUEUE_PROCESSOR,
  );
  const attestationTimeoutCorrection = yield* Ref.get(
    globals.ATTESTATION_TIMEOUT_CORRECTION_HEALTH,
  );
  const localFinalizationPending = yield* Ref.get(
    globals.LOCAL_FINALIZATION_PENDING,
  );
  const unconfirmedSubmittedBlockTxHash = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );
  const unconfirmedSubmittedBlockSinceMs = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
  );
  const unresolvedBlockSubmissionAgeMs =
    unconfirmedSubmittedBlockTxHash === "" ||
    unconfirmedSubmittedBlockSinceMs <= 0
      ? 0
      : nowMillis - unconfirmedSubmittedBlockSinceMs;

  const dbProbe = yield* Effect.either(sql`SELECT 1 AS ok`);
  const dbHealthy = dbProbe._tag === "Right";
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const providerHealthBefore = yield* Ref.get(globals.L1_PROVIDER_HEALTH);
  const cachedProviderEvidenceIsFresh = l1ProviderReadinessEvidenceIsFresh({
    evidence: providerHealthBefore,
    nowMs: nowMillis,
    maxAgeMs: nodeConfig.READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS,
    maxExactAgeMs: DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  });
  const liveProviderProbeTimeoutMs = Math.min(
    nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
    READINESS_L1_PROVIDER_LIVE_TIMEOUT_MS,
  );
  let readinessProbe: L1ProviderReadinessProbe = {
    mode: "cached_fresh",
    baseRevision: providerHealthBefore.evidenceRevision,
  };
  if (!cachedProviderEvidenceIsFresh) {
    const primaryProbeAttempt = yield* Effect.either(
      withL1ControlPlaneIfAvailable(
        globals,
        {
          scope: "readiness_l1_provider",
          maxHoldMs: liveProviderProbeTimeoutMs,
        },
        runCombinedL1ReadinessProbe(
          Initialization.fetchHubOracleWitness(lucid.api, contracts),
          readLocalOgmiosSubmitSlot({
            ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
            timeoutMs: liveProviderProbeTimeoutMs,
          }),
        ),
      ),
    );
    if (primaryProbeAttempt._tag === "Left") {
      const observedAtMs = Date.now();
      const error = String(primaryProbeAttempt.left);
      const published = yield* Ref.modify(
        globals.L1_PROVIDER_HEALTH,
        (current) => {
          const updated = nextL1ProviderHealthEvidence({
            current,
            healthy: false,
            error,
            observedAtMs,
            successKind: "exact",
          });
          return [updated, updated] as const;
        },
      );
      readinessProbe = {
        mode: "live",
        healthy: false,
        error,
        publishedRevision: published.evidenceRevision,
      };
    } else if (Option.isSome(primaryProbeAttempt.right)) {
      const observedAtMs = Date.now();
      const ogmiosSlot = primaryProbeAttempt.right.value;
      const published = yield* Ref.modify(
        globals.L1_PROVIDER_HEALTH,
        (current) => {
          const updated = nextL1ProviderHealthEvidence({
            current,
            healthy: true,
            observedAtMs,
            ogmiosSlot,
            successKind: "exact",
          });
          return [updated, updated] as const;
        },
      );
      readinessProbe = {
        mode: "live",
        healthy: true,
        ogmiosSlot,
        publishedRevision: published.evidenceRevision,
      };
    } else {
      const directProbe = runBoundedDirectL1ProviderPreflight({
        runPreflight: (signal) =>
          runL1ProviderPreflight({
            config: {
              L1_PROVIDER: nodeConfig.L1_PROVIDER,
              L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: liveProviderProbeTimeoutMs,
              L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS:
                nodeConfig.L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS,
              L1_OGMIOS_KEY: nodeConfig.L1_OGMIOS_KEY,
              L1_KUPO_KEY: nodeConfig.L1_KUPO_KEY,
              NETWORK: nodeConfig.NETWORK,
            },
            signal,
          }),
        timeoutMs: liveProviderProbeTimeoutMs,
      });
      readinessProbe = yield* runBusyL1ProviderReadinessProbe({
        globals,
        directProbe,
        now: Date.now,
        maxAgeMs: nodeConfig.READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS,
        maxExactAgeMs: DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
      });
    }
  }
  const providerHealthAfter = yield* Ref.get(globals.L1_PROVIDER_HEALTH);
  const providerEvidenceObservedAtMs = Date.now();
  const providerProbe = resolveL1ProviderReadinessSnapshot({
    probe: readinessProbe,
    evidence: providerHealthAfter,
    nowMs: providerEvidenceObservedAtMs,
    maxAgeMs: nodeConfig.READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS,
    maxExactAgeMs: DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  });
  const leaseInspection = yield* StateQueueMutationLeasesDB.inspect({
    recentLimit: 3,
  });
  const encodedLeaseInspection =
    StateQueueMutationLeasesDB.encodeInspectionJson(leaseInspection);
  const activeLease = leaseInspection.activeLease;
  const activeLeaseRemainingMs =
    activeLease === undefined
      ? null
      : activeLease[StateQueueMutationLeasesDB.Columns.EXPIRES_AT].getTime() -
        leaseInspection.dbNow.getTime();

  const baseReadiness = evaluateReadiness({
    nowMillis,
    maxHeartbeatAgeMs: nodeConfig.READINESS_MAX_HEARTBEAT_AGE_MS,
    maxQueueDepth: nodeConfig.READINESS_MAX_DURABLE_ADMISSION_BACKLOG,
    queueDepth: Number(durableAdmissionBacklog),
    workerHeartbeats: {
      blockCommitment: blockCommitmentHeartbeat,
      blockConfirmation: blockConfirmationHeartbeat,
      merge: mergeHeartbeat,
      txQueueProcessor: txQueueProcessorHeartbeat,
    },
    localFinalizationPending,
    unresolvedBlockSubmissionAgeMs,
    maxUnresolvedBlockSubmissionAgeMs: nodeConfig.UNCONFIRMED_BLOCK_MAX_AGE_MS,
    dbHealthy,
    awaitingForeignTipReconciliations,
    foreignBaseVerification,
    validationPool: {
      configuredWorkers: validationPool.poolSize,
      liveWorkers: validationPoolStats.liveWorkers,
      restartingWorkers: validationPoolStats.restartingWorkers,
      oldestInFlightAgeMs: validationPoolStats.oldestInFlightAgeMs,
      jobTimeoutMs: nodeConfig.VALIDATION_WORKER_JOB_TIMEOUT_MS,
    },
    stateQueueMutationLease: {
      active: activeLease !== undefined,
      stale:
        activeLeaseRemainingMs !== null &&
        activeLeaseRemainingMs <
          -nodeConfig.STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS,
      remainingMs: activeLeaseRemainingMs,
      holder: activeLease?.[StateQueueMutationLeasesDB.Columns.HOLDER] ?? null,
    },
    attestationTimeoutCorrection: {
      ...attestationTimeoutCorrection,
      ...attestationTimeoutCorrectionReadinessBounds(
        nodeConfig.WAIT_BETWEEN_MERGE_TXS,
      ),
    },
  });
  const reasons = [...baseReadiness.reasons];
  const settlement = yield* Ref.get(globals.SETTLEMENT_HEALTH);
  // Settlement health is reported separately: an L1 payout delay must not
  // take healthy L2 admission out of service.
  // Informational: pending signed-header recovery holding history retention
  // more than the rollback horizon back grows the journal until it resolves.
  const historyOwner = yield* Ref.get(globals.EVENT_HISTORY_OWNER);
  const eventHistoryRetentionHold =
    historyOwner === undefined
      ? null
      : ((yield* historyOwner.retentionHold) ?? null);
  // The history gate: closed while recovering, and named when an open gate's
  // follower falls further behind the source tip than it may.
  const eventHistoryFrontier =
    historyOwner === undefined ? null : yield* historyOwner.frontier;
  if (eventHistoryFrontier !== null) {
    if (!eventHistoryFrontier.ready) reasons.push("history_owner_not_ready");
    else if (
      eventHistoryFrontier.lagBlocks > eventHistoryFrontier.maximumLagBlocks
    )
      reasons.push(
        `history_follower_lagging:${eventHistoryFrontier.lagBlocks}:${eventHistoryFrontier.maximumLagBlocks}`,
      );
  }
  const nativeMpfOwner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
  const nativeMpfDiagnostics =
    nativeMpfOwner === undefined
      ? undefined
      : yield* Effect.either(
          Effect.tryPromise({
            try: () => nativeMpfOwner.diagnostics(),
            catch: (cause) => cause,
          }),
        );
  if (nativeMpfOwner === undefined) {
    reasons.push("native_mpf_owner_unavailable");
  } else if (nativeMpfDiagnostics?._tag === "Left") {
    reasons.push("native_mpf_owner_unhealthy");
  }
  if (!providerProbe.healthy) {
    reasons.push("provider_query_unhealthy:l1-provider");
  }
  if (
    durableAdmissionOldestAgeMs >
    nodeConfig.READINESS_MAX_DURABLE_ADMISSION_AGE_MS
  ) {
    reasons.push(
      `durable_admission_oldest_age_exceeded:${durableAdmissionOldestAgeMs}:${nodeConfig.READINESS_MAX_DURABLE_ADMISSION_AGE_MS}`,
    );
  }
  if (unfinishedMutationJobs > 0n) {
    reasons.push(
      `unfinished_local_mutation_jobs:${unfinishedMutationJobs.toString()}`,
    );
  }
  if (daPublicationConflicts > 0) {
    reasons.push(
      `da_publication_conflict:${daPublicationConflicts.toString()}`,
    );
  }
  const mergeReadiness = planMergePreflight({
    force: false,
    queueLength: stateQueueBlocksInQueue,
    minQueueLength:
      nodeConfig.MIN_QUEUE_LENGTH_FOR_MERGING ??
      DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
    unresolvedSubmittedBlockTxHash: unconfirmedSubmittedBlockTxHash,
    localFinalizationPending,
    resetInProgress,
    durableAdmissionBacklog,
    mempoolTxCount,
    unfinishedMutationJobs,
  });
  const readiness = {
    settlement,
    ready: reasons.length === 0,
    reasons,
    durableAdmissionBacklog: durableAdmissionBacklog.toString(),
    durableAdmissionOldestAgeMs,
    mempoolTxCount: mempoolTxCount.toString(),
    unfinishedLocalMutationJobs: unfinishedMutationJobs.toString(),
    daPublicationConflicts,
    awaitingForeignTipReconciliations,
    foreignBaseVerification:
      foreignBaseVerification.status === "unobserved"
        ? foreignBaseVerification
        : {
            status: foreignBaseVerification.status,
            foreignHeaderHash: foreignBaseVerification.foreignHeaderHash,
            reason: foreignBaseVerification.reason,
            generation: foreignBaseVerification.scope.generation,
          },
    unresolvedBlockSubmissionAgeMs,
    providerQueryHealthy: providerProbe.healthy,
    providerQueryMode: providerProbe.mode,
    providerQueryEvidenceAgeMs: providerProbe.evidenceAgeMs,
    providerQueryEvidenceRevision: providerHealthAfter.evidenceRevision,
    providerQueryLastObservationKind: providerHealthAfter.lastObservationKind,
    providerQueryLastExactEvidenceRevision:
      providerHealthAfter.lastExactEvidenceRevision,
    providerQueryLastExactObservationKind:
      providerHealthAfter.lastExactObservationKind,
    providerQueryEvidenceKind: providerHealthAfter.lastSuccessKind,
    providerQueryExactEvidenceAgeMs:
      providerHealthAfter.lastExactSuccessAtMs <= 0
        ? null
        : Math.max(0, Date.now() - providerHealthAfter.lastExactSuccessAtMs),
    providerQueryError: providerProbe.error,
    localOgmiosSlot:
      providerProbe.ogmiosSlot !== null
        ? {
            ...providerProbe.ogmiosSlot,
            evidence: localOgmiosSubmitSlotEvidence(providerProbe.ogmiosSlot),
            mode: providerProbe.mode,
            evidenceAgeMs: providerProbe.evidenceAgeMs,
          }
        : {
            mode: providerProbe.mode,
            evidenceAgeMs: providerProbe.evidenceAgeMs,
            error: providerProbe.error,
          },
    stateQueueMutationLease: encodedLeaseInspection,
    attestationTimeoutCorrection,
    blockCommitmentCoordination: {
      commitWorkerActive,
      commitPipelinePhase,
    },
    nativeMpfOwner:
      nativeMpfDiagnostics === undefined
        ? null
        : nativeMpfDiagnostics._tag === "Left"
          ? { healthy: false, error: String(nativeMpfDiagnostics.left) }
          : {
              healthy: true,
              ...nativeMpfDiagnostics.right,
              ownerEpoch: Buffer.from(
                nativeMpfDiagnostics.right.ownerEpoch,
              ).toString("hex"),
            },
    eventHistoryRetentionHold,
    eventHistoryFrontier,
    mergeReadiness,
  };

  return yield* HttpServerResponse.json(readiness, {
    status: readiness.ready ? 200 : 503,
  });
});
