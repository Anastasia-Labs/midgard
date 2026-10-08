import "./listen-router.post-tx-status-batch-handler.js";

import { HttpServerResponse } from "@effect/platform";
import { Effect, Option, Ref } from "effect";

import {
  DaPayloadTerminalOutcomesDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import { attestationTimeoutCorrectionReadinessBounds } from "../fibers/index.js";
import { localOgmiosSubmitSlotEvidence } from "../l1-heads.js";
import { READINESS_L1_PROVIDER_PROBE_TIMEOUT_MS } from "../l1-provider-readiness-probe.js";
import {
  DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  ValidationPool,
} from "../services/index.js";
import {
  ContractDeploymentIdentity,
  Globals,
  Lucid,
  NodeConfig,
} from "../services/index.js";
import { l1FollowerReadiness } from "../services/l1-follower.readiness.js";
import { activeLivenessReasons } from "../services/liveness-halt.js";
import { settlementReadinessReason } from "../services/settlement-readiness.js";
import {
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  planMergePreflight,
} from "../transactions/state-queue/merge-readiness.js";
import { runL1ProviderPreflight } from "./l1-provider-preflight.js";
import {
  l1ProviderReadiness,
  pendingFinalizationAgeDetail,
  readinessDatabaseError,
  readinessL1ProviderUnhealthyAfterMs,
  readReadinessDatabaseState,
} from "./listen-router.get-readiness-handler.inputs.js";
import {
  l1ProviderReadinessEvidenceIsFresh,
  type L1ProviderReadinessProbe,
  runBoundedDirectL1ProviderPreflight,
} from "./listen-router.l1-provider-readiness-evidence-is-fresh.js";
import {
  resolveL1ProviderReadinessSnapshot,
  runExactGatedDirectL1ProviderProbe,
} from "./listen-router.run-exact-gated-direct-l1-provider-probe.js";
import { evaluateReadiness } from "./readiness.js";

/**
 * `GET /readyz`: readiness endpoint that checks worker heartbeats, queue depth,
 * local recovery state, and database connectivity. A database that does not
 * answer is `db_unhealthy` (503), never a server error. `details` names
 * degradations that do not stop admission, so they leave the node ready.
 * Every raised liveness reason (a hold on block production, or a stalled
 * fiber) is a reason, and `livenessReasons` gives each one's source, age and
 * escalation. Readiness only reports: no admission route reads it.
 */
export const getReadinessHandler = Effect.gen(function* () {
  const globals = yield* Globals;
  const operatorMembership = yield* Ref.get(globals.OPERATOR_MEMBERSHIP);
  const nodeConfig = yield* NodeConfig;
  const validationPool = yield* ValidationPool;
  const validationPoolStats = yield* validationPool.stats;

  const databaseState = yield* Effect.either(
    readReadinessDatabaseState(
      DaPayloadTerminalOutcomesDB.deploymentIdentityDigestOf(
        yield* ContractDeploymentIdentity,
      ),
    ),
  );
  if (databaseState._tag === "Left") {
    return yield* HttpServerResponse.json(
      {
        settlement: yield* Ref.get(globals.SETTLEMENT_HEALTH),
        ready: false,
        reasons: ["db_unhealthy"],
        details: [],
        dbError: readinessDatabaseError(databaseState.left),
      },
      { status: 503 },
    );
  }
  const {
    durableAdmissionBacklog,
    durableAdmissionOldestAgeMs,
    unfinishedMutationJobs,
    daPublicationConflicts,
    mempoolTxCount,
    leaseInspection,
    journalAges,
  } = databaseState.right;
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

  const lucidService = yield* Effect.serviceOption(Lucid);
  const providerHealthBefore = yield* Ref.get(globals.L1_PROVIDER_HEALTH);
  const cachedProviderEvidenceIsFresh = l1ProviderReadinessEvidenceIsFresh({
    evidence: providerHealthBefore,
    nowMs: nowMillis,
    maxAgeMs: nodeConfig.READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS,
    maxExactAgeMs: DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  });
  const providerProbeTimeoutMs = Math.min(
    nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
    READINESS_L1_PROVIDER_PROBE_TIMEOUT_MS,
  );
  // Exact HubOracle evidence comes only from the background refresher, which
  // queues for the shared Lucid control plane. Between refreshes a raw
  // provider probe extends it without touching Lucid.
  const readinessProbe: L1ProviderReadinessProbe = cachedProviderEvidenceIsFresh
    ? {
        mode: "cached_fresh",
        baseRevision: providerHealthBefore.evidenceRevision,
      }
    : yield* runExactGatedDirectL1ProviderProbe({
        globals,
        directProbe: runBoundedDirectL1ProviderPreflight({
          runPreflight: (signal) =>
            runL1ProviderPreflight({
              config: {
                L1_PROVIDER: nodeConfig.L1_PROVIDER,
                L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: providerProbeTimeoutMs,
                L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS:
                  nodeConfig.L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS,
                L1_OGMIOS_KEY: nodeConfig.L1_OGMIOS_KEY,
                L1_KUPO_KEY: nodeConfig.L1_KUPO_KEY,
                NETWORK: nodeConfig.NETWORK,
                L1_OGMIOS_TIP_MAX_AGE_MS:
                  Option.getOrUndefined(lucidService)?.ogmiosTipMaxAgeMs,
              },
              signal,
            }),
          timeoutMs: providerProbeTimeoutMs,
        }),
        now: Date.now,
        maxAgeMs: nodeConfig.READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS,
        maxExactAgeMs: DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
      });
  const providerHealthAfter = yield* Ref.get(globals.L1_PROVIDER_HEALTH);
  const providerEvidenceObservedAtMs = Date.now();
  const providerProbe = resolveL1ProviderReadinessSnapshot({
    probe: readinessProbe,
    evidence: providerHealthAfter,
    nowMs: providerEvidenceObservedAtMs,
    maxAgeMs: nodeConfig.READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS,
    maxExactAgeMs: DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
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
    dbHealthy: true,
    operatorMembership,
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
  // An L1 payout delay must not take healthy L2 admission out of service;
  // only a settlement worker that keeps dying does.
  const settlementReason = settlementReadinessReason(settlement, Date.now());
  if (settlementReason !== undefined) reasons.push(settlementReason);
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
  // The L1 follower (N1): every named reason of the follow loop and of the
  // follower-change driver fails readiness; the process stays up.
  const l1Follower = l1FollowerReadiness(yield* Ref.get(globals.L1_FOLLOWER));
  for (const reason of l1Follower.reasons)
    if (!reasons.includes(reason)) reasons.push(reason);
  const details: string[] = [];
  const daFramePressure = yield* Ref.get(globals.COMMIT_DA_FRAME_PRESSURE);
  const commitDaFramePressure =
    daFramePressure === null
      ? null
      : {
          ...daFramePressure,
          ageMs: Math.max(0, nowMillis - daFramePressure.observedAtMs),
        };
  if (daFramePressure !== null) {
    for (const [kind, stage] of [
      ["candidate", daFramePressure.candidateStagePercent],
      ["initial_candidate", daFramePressure.initialCandidateStagePercent],
      ["base_ledger", daFramePressure.baseLedgerStagePercent],
      ["required_work", daFramePressure.requiredWorkStagePercent],
    ] as const) {
      if (stage !== null && stage > 0)
        details.push(`commit_da_frame_pressure:${kind}:${stage.toString()}`);
    }
  }

  // No membership check has authenticated yet (or none can): duties run, so
  // this is reported but leaves the node ready. Removal is a liveness reason.
  if (operatorMembership === "unknown")
    details.push("operator_membership_unavailable");
  const providerReadiness = l1ProviderReadiness({
    healthy: providerProbe.healthy,
    lastSuccessAtMs: providerHealthAfter.lastSuccessAtMs,
    lastExactSuccessAtMs: providerHealthAfter.lastExactSuccessAtMs,
    nowMs: providerEvidenceObservedAtMs,
    unhealthyAfterMs: readinessL1ProviderUnhealthyAfterMs(
      Option.getOrUndefined(lucidService)?.ogmiosTipMaxAgeMs,
    ),
  });
  if (providerReadiness.reason !== undefined)
    reasons.push(providerReadiness.reason);
  // A raised reason holds the fibers its source halts for as long as it is
  // raised, possibly for good, while every heartbeat stays fresh.
  const livenessReasons = yield* activeLivenessReasons(globals);
  for (const { reason } of livenessReasons)
    if (!reasons.includes(reason)) reasons.push(reason);
  if (providerReadiness.detail !== undefined)
    details.push(providerReadiness.detail);
  const pendingFinalizationDetail = pendingFinalizationAgeDetail(
    journalAges.pendingFinalizationAgeMs,
  );
  if (pendingFinalizationDetail !== undefined)
    details.push(pendingFinalizationDetail);
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
    details,
    pendingFinalizationAgeMs: journalAges.pendingFinalizationAgeMs,
    signedIntentUnresolvedAgeMs: journalAges.signedIntentUnresolvedAgeMs,
    durableAdmissionBacklog: durableAdmissionBacklog.toString(),
    durableAdmissionOldestAgeMs,
    mempoolTxCount: mempoolTxCount.toString(),
    unfinishedLocalMutationJobs: unfinishedMutationJobs.toString(),
    daPublicationConflicts,
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
    l1Follower: l1Follower.report,
    livenessReasons,
    commitDaFramePressure,
    mergeReadiness,
  };

  return yield* HttpServerResponse.json(readiness, {
    status: readiness.ready ? 200 : 503,
  });
});
