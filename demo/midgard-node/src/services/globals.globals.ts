import { TxHash } from "@lucid-evolution/lucid";
import { Effect, Queue, Ref } from "effect";

import {
  idleSpeculativeCommitState,
  type SpeculativeCommitState,
  type UserEventBarrierWatermarks,
} from "../fibers/speculative-commit-state.js";
import { SerializedStateQueueUTxO } from "../workers/utils/commit-block-header.js";
import type { CommitDaFramePressureSnapshot } from "../workers/utils/commit-block-planner.commit-da-frame-notice.js";
import type { EventHistoryOwner } from "./event-history-owner.js";
import type { IdleBackoffState } from "./globals.idle-backoff.js";
import {
  initialL1ControlPlaneActivity,
  type L1ControlPlaneActivity,
} from "./globals.l1-control-plane.js";
import {
  type AdmissionBacklogGaugeState,
  type AttestationTimeoutCorrectionHealth,
  type CommitPipelinePhase,
  type CommitSubmitWake,
  type L1ProviderHealthEvidence,
  type MempoolLedgerDeltaLog,
} from "./globals.next-l1-provider-health-evidence.js";
import type { NativeMpfOwnerService } from "./mpf-native-owner/index.js";

/**
 * Process-wide mutable references shared between long-running fibers.
 *
 * These refs hold operational state that does not belong in durable storage but
 * still needs coordination across workers, readiness checks, and recovery
 * paths.
 */
export class Globals extends Effect.Service<Globals>()("Globals", {
  effect: Effect.gen(function* () {
    const now = Date.now();

    // In-memory state queue length.
    const BLOCKS_IN_QUEUE = yield* Ref.make<number>(0);

    // Latest moment the in-memory state queue length was synchronized with
    // on-chain state.
    const LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH = yield* Ref.make<number>(0);

    // Needed for development to prevent other actions triggering while spending
    // all UTxOs at state queue.
    const RESET_IN_PROGRESS = yield* Ref.make<boolean>(false);

    // Prevents overlapping commitment workers (periodic + manual trigger).
    const COMMIT_WORKER_ACTIVE = yield* Ref.make<boolean>(false);

    // Latest measured worker candidate, separate from liveness holds. Null is
    // unmeasured/idle, not evidence that the ledger has zero frame pressure.
    const COMMIT_DA_FRAME_PRESSURE =
      yield* Ref.make<CommitDaFramePressureSnapshot | null>(null);

    // Serializes pre-worker scheduler alignment with actual mutation workers.
    // COMMIT_WORKER_ACTIVE intentionally remains true only for the worker phase.
    const COMMIT_PIPELINE_PHASE = yield* Ref.make<CommitPipelinePhase>("idle");

    // Parent fibers hold this permit across provider-using child-worker
    // lifetimes. Child workers and commit-time barriers never acquire it
    // themselves, which keeps the acquisition non-reentrant.
    const L1_CONTROL_PLANE = yield* Effect.makeSemaphore(1);
    // Raw provider liveness probes intentionally do not use L1_CONTROL_PLANE:
    // they never touch shared Lucid state. This separate permit deduplicates
    // concurrent readiness requests while a long-running Lucid action owns the
    // control plane.
    const L1_PROVIDER_DIRECT_PROBE = yield* Effect.makeSemaphore(1);
    const L1_PROVIDER_HEALTH = yield* Ref.make<L1ProviderHealthEvidence>({
      evidenceRevision: 0,
      lastObservationKind: null,
      lastExactEvidenceRevision: 0,
      lastExactObservationKind: null,
      lastSuccessAtMs: 0,
      lastExactSuccessAtMs: 0,
      lastExactFailureAtMs: 0,
      lastExactFailure: null,
      lastSuccessKind: null,
      lastFailureAtMs: 0,
      lastFailure: null,
      lastOgmiosSlot: null,
    });

    const SPECULATIVE_COMMIT_STATE = yield* Ref.make<SpeculativeCommitState>(
      idleSpeculativeCommitState(),
    );
    const SPECULATIVE_COMMIT_SESSION_ACTIVE = yield* Ref.make(false);
    const SPECULATIVE_BUILD_WAKE_QUEUE = yield* Queue.unbounded<string>();
    const COMMIT_SUBMIT_WAKE_QUEUE = yield* Queue.unbounded<CommitSubmitWake>();

    const USER_EVENT_BARRIER_WATERMARKS =
      yield* Ref.make<UserEventBarrierWatermarks>({
        depositMs: 0,
        withdrawalMs: 0,
        txOrderMs: 0,
        refreshedAtMs: 0,
      });

    // The state queue UTxO confirmed by the confirmation worker, unused for
    // block commitment.
    const AVAILABLE_CONFIRMED_BLOCK = yield* Ref.make<
      "" | SerializedStateQueueUTxO
    >("");

    // The specific confirmed state_queue block whose roots must be used for
    // local finalization recovery when a submission succeeded on-chain but the
    // node crashed or failed during local persistence.
    const AVAILABLE_LOCAL_FINALIZATION_BLOCK = yield* Ref.make<
      "" | SerializedStateQueueUTxO
    >("");

    // Accumulator for the number of processed mempool transactions (only used
    // in metrics)
    const PROCESSED_UNSUBMITTED_TXS_COUNT = yield* Ref.make<number>(0);

    // Accumulator for the total size of L2 transactions submitted in a state
    // queue block.
    const PROCESSED_UNSUBMITTED_TXS_SIZE = yield* Ref.make<number>(0);

    const UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH = yield* Ref.make<"" | TxHash>(
      "",
    );
    const UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS = yield* Ref.make<number>(0);

    // The end time of the latest block the node has locally accepted as the
    // pre-state boundary for the next block (confirmed on startup, then
    // advanced optimistically on successful submissions).
    const LATEST_LOCAL_BLOCK_END_TIME_MS = yield* Ref.make<number>(0);

    // Every direct writer of mempool_ledger must publish an exact delta or a
    // full marker. A missing/gapped delta forces the cache service to reload.
    const MEMPOOL_LEDGER_DELTA_LOG = yield* Ref.make<MempoolLedgerDeltaLog>({
      version: 0,
      entries: [],
    });

    // Coordinates event-driven tx queue wakeups without allowing overlapping
    // validation processors.
    const TX_QUEUE_PROCESSOR_ACTIVE = yield* Ref.make<number>(0);
    // Monotone generation avoids losing a wake when two drain loops finish
    // concurrently after a submit-side wakeup was coalesced.
    const TX_QUEUE_WAKE_GENERATION = yield* Ref.make<bigint>(0n);

    // Architecture G is the only component allowed to hold the ledger Level
    // lock. Workers receive opaque MessagePorts and never see this service or
    // its Level path directly.
    const SETTLEMENT_HEALTH = yield* Ref.make<
      import("./settlement.js").SettlementHealth
    >({
      observedAt: now,
      state: "starting",
      detail: "settlement worker starting",
    });
    const EVENT_HISTORY_OWNER = yield* Ref.make<EventHistoryOwner | undefined>(
      undefined,
    );

    const NATIVE_MPF_OWNER = yield* Ref.make<NativeMpfOwnerService | undefined>(
      undefined,
    );

    // Hybrid durable-admission backlog gauge. These fields share one Ref so a
    // refresh can roll the local delta into the base without exposing an
    // under-counted intermediate state to concurrent submit handlers.
    const ADMISSION_BACKLOG_GAUGE = yield* Ref.make<AdmissionBacklogGaugeState>(
      {
        ADMISSION_BACKLOG_BASE: 0n,
        ADMISSION_BACKLOG_LOCAL_DELTA: 0n,
        ADMISSION_BACKLOG_IN_FLIGHT: 0n,
        ADMISSION_BACKLOG_REFRESHED_AT: 0,
      },
    );

    // Indicates that on-chain block submission succeeded but local persistence
    // failed and must be retried against the confirmed block.
    const LOCAL_FINALIZATION_PENDING = yield* Ref.make<boolean>(false);

    // Worker liveness signals used by readiness checks.
    const HEARTBEAT_BLOCK_COMMITMENT = yield* Ref.make<number>(now);
    const HEARTBEAT_BLOCK_CONFIRMATION = yield* Ref.make<number>(now);
    const HEARTBEAT_MERGE = yield* Ref.make<number>(now);
    const HEARTBEAT_TX_QUEUE_PROCESSOR = yield* Ref.make<number>(now);
    const HEARTBEAT_SPECULATIVE_COMMIT_BUILDER = yield* Ref.make<number>(now);
    const HEARTBEAT_SPECULATIVE_COMMIT_SUBMITTER = yield* Ref.make<number>(now);

    // Who holds and who waits for L1_CONTROL_PLANE, read by readiness to tell
    // a wedged permit from a busy one.
    const L1_CONTROL_PLANE_ACTIVITY = yield* Ref.make<L1ControlPlaneActivity>(
      initialL1ControlPlaneActivity(),
    );
    // Conditions that stop the node making progress without stopping any
    // fiber, keyed by their source; each source clears its own once it
    // recovers. Readiness reports them.
    const LIVENESS_REASONS = yield* Ref.make<ReadonlyMap<string, string>>(
      new Map(),
    );
    // Set by the commitment fiber each tick: true while it found no pending
    // tx or user-event work, so the confirmation fiber may back off.
    const COMMIT_PIPELINE_IDLE = yield* Ref.make<boolean>(false);
    // The backlog that tick counted, which sizes the commitment's L1
    // control-plane hold without querying it again under the permit.
    const COMMIT_PIPELINE_BACKLOG = yield* Ref.make<{
      readonly mempoolTxCount: number;
      readonly pendingUserEventCount: number;
    }>({ mempoolTxCount: 0, pendingUserEventCount: 0 });
    const IDLE_BACKOFF = yield* Ref.make<ReadonlyMap<string, IdleBackoffState>>(
      new Map(),
    );
    // The last state each recurring status log reported, so it logs once per
    // change rather than once per tick.
    const LOGGED_STATES = yield* Ref.make<ReadonlyMap<string, string>>(
      new Map(),
    );
    const ATTESTATION_TIMEOUT_CORRECTION_HEALTH =
      yield* Ref.make<AttestationTimeoutCorrectionHealth>({
        lastProgressAtMs: now,
        lastQueueReadAtMs: now,
        correctionProgress: null,
        lastFailureAtMs: 0,
        lastError: null,
        consecutiveFailures: 0,
        oldestUnattestedHeader: null,
      });

    return {
      BLOCKS_IN_QUEUE,
      LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
      RESET_IN_PROGRESS,
      COMMIT_WORKER_ACTIVE,
      COMMIT_DA_FRAME_PRESSURE,
      COMMIT_PIPELINE_PHASE,
      L1_CONTROL_PLANE,
      SETTLEMENT_HEALTH,
      L1_PROVIDER_DIRECT_PROBE,
      L1_PROVIDER_HEALTH,
      SPECULATIVE_COMMIT_STATE,
      SPECULATIVE_COMMIT_SESSION_ACTIVE,
      SPECULATIVE_BUILD_WAKE_QUEUE,
      COMMIT_SUBMIT_WAKE_QUEUE,
      USER_EVENT_BARRIER_WATERMARKS,
      AVAILABLE_CONFIRMED_BLOCK,
      AVAILABLE_LOCAL_FINALIZATION_BLOCK,
      PROCESSED_UNSUBMITTED_TXS_COUNT,
      PROCESSED_UNSUBMITTED_TXS_SIZE,
      UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
      UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
      LATEST_LOCAL_BLOCK_END_TIME_MS,
      MEMPOOL_LEDGER_DELTA_LOG,
      TX_QUEUE_PROCESSOR_ACTIVE,
      TX_QUEUE_WAKE_GENERATION,
      NATIVE_MPF_OWNER,
      EVENT_HISTORY_OWNER,
      ADMISSION_BACKLOG_GAUGE,
      LOCAL_FINALIZATION_PENDING,
      HEARTBEAT_BLOCK_COMMITMENT,
      HEARTBEAT_BLOCK_CONFIRMATION,
      HEARTBEAT_MERGE,
      HEARTBEAT_TX_QUEUE_PROCESSOR,
      HEARTBEAT_SPECULATIVE_COMMIT_BUILDER,
      HEARTBEAT_SPECULATIVE_COMMIT_SUBMITTER,
      ATTESTATION_TIMEOUT_CORRECTION_HEALTH,
      L1_CONTROL_PLANE_ACTIVITY,
      LIVENESS_REASONS,
      COMMIT_PIPELINE_IDLE,
      COMMIT_PIPELINE_BACKLOG,
      IDLE_BACKOFF,
      LOGGED_STATES,
    };
  }),
}) {}

/**
 * Runs `effect` under the process-wide L1 control-plane permit, with its hold
 * capped; see `globals.l1-control-plane.ts`.
 */
export { withL1ControlPlane } from "./globals.l1-control-plane.js";
