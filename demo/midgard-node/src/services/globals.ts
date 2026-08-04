import { TxHash } from "@lucid-evolution/lucid";
import { Duration, Effect, Metric, Option, Queue, Ref } from "effect";

import {
  idleSpeculativeCommitState,
  type SpeculativeCommitState,
  type UserEventBarrierWatermarks,
} from "@/fibers/speculative-commit-state.js";
import type { SubmitSlotSnapshot } from "@/local-ogmios-slot.js";
import type { NativeMpfOwnerService } from "@/services/mpf-native-owner/index.js";
import { SerializedStateQueueUTxO } from "@/workers/utils/commit-block-header.js";

export type CommitPipelinePhase =
  | "idle"
  | "scheduler_alignment"
  | "speculative_build"
  | "mutation_worker";

export type CommitSubmitWake = {
  readonly confirmedHeaderHash: string;
  /** Canonical latest state-queue tip observed with confirmedHeaderHash. */
  readonly confirmedTip: SerializedStateQueueUTxO;
  readonly confirmationObservedAtMs: number;
  readonly confirmationWaitMs: number;
};

export type AdmissionBacklogGaugeState = {
  readonly ADMISSION_BACKLOG_BASE: bigint;
  readonly ADMISSION_BACKLOG_LOCAL_DELTA: bigint;
  /**
   * Slots reserved by concurrent submit handlers before their durable INSERT
   * finishes. Keeping these separate prevents an in-flight reservation from
   * being erased by a live-count refresh.
   */
  readonly ADMISSION_BACKLOG_IN_FLIGHT: bigint;
  readonly ADMISSION_BACKLOG_REFRESHED_AT: number;
};

export type MempoolLedgerDelta = {
  readonly version: number;
  readonly full: boolean;
  readonly upserts: ReadonlyArray<readonly [outRefHex: string, output: Buffer]>;
  readonly deletes: readonly string[];
};

export type MempoolLedgerDeltaLog = {
  readonly version: number;
  readonly entries: readonly MempoolLedgerDelta[];
};

export type L1ProviderHealthEvidence = {
  readonly evidenceRevision: number;
  readonly lastObservationKind:
    | "exact_success"
    | "exact_failure"
    | "direct_success"
    | "direct_failure"
    | null;
  readonly lastExactEvidenceRevision: number;
  readonly lastExactObservationKind: "exact_success" | "exact_failure" | null;
  readonly lastSuccessAtMs: number;
  /** Last success from the exact HubOracle + local-Ogmios readiness query. */
  readonly lastExactSuccessAtMs: number;
  readonly lastExactFailureAtMs: number;
  readonly lastExactFailure: string | null;
  readonly lastSuccessKind: "exact" | "direct" | null;
  readonly lastFailureAtMs: number;
  readonly lastFailure: string | null;
  readonly lastOgmiosSlot: SubmitSlotSnapshot | null;
};

export const nextL1ProviderHealthEvidence = ({
  current,
  healthy,
  error,
  observedAtMs,
  ogmiosSlot,
  successKind,
}: {
  readonly current: L1ProviderHealthEvidence;
  readonly healthy: boolean;
  readonly error?: string;
  readonly observedAtMs: number;
  readonly ogmiosSlot?: SubmitSlotSnapshot;
  readonly successKind: "exact" | "direct";
}): L1ProviderHealthEvidence => {
  const evidenceRevision = current.evidenceRevision + 1;
  const lastObservationKind =
    successKind === "exact"
      ? healthy
        ? ("exact_success" as const)
        : ("exact_failure" as const)
      : healthy
        ? ("direct_success" as const)
        : ("direct_failure" as const);

  return healthy
    ? (() => {
        const exactFailureIsUnrecovered =
          successKind === "direct" &&
          current.lastExactObservationKind === "exact_failure";
        return {
          evidenceRevision,
          lastObservationKind,
          lastExactEvidenceRevision:
            successKind === "exact"
              ? evidenceRevision
              : current.lastExactEvidenceRevision,
          lastExactObservationKind:
            successKind === "exact"
              ? "exact_success"
              : current.lastExactObservationKind,
          lastSuccessAtMs: observedAtMs,
          lastExactSuccessAtMs:
            successKind === "exact"
              ? observedAtMs
              : current.lastExactSuccessAtMs,
          lastExactFailureAtMs: current.lastExactFailureAtMs,
          lastExactFailure: current.lastExactFailure,
          lastSuccessKind: successKind,
          lastFailureAtMs: current.lastFailureAtMs,
          lastFailure: exactFailureIsUnrecovered
            ? current.lastExactFailure
            : null,
          lastOgmiosSlot: ogmiosSlot ?? current.lastOgmiosSlot,
        };
      })()
    : (() => {
        const failure = error ?? "L1 provider probe failed";
        return {
          ...current,
          evidenceRevision,
          lastObservationKind,
          lastExactEvidenceRevision:
            successKind === "exact"
              ? evidenceRevision
              : current.lastExactEvidenceRevision,
          lastExactObservationKind:
            successKind === "exact"
              ? "exact_failure"
              : current.lastExactObservationKind,
          lastExactFailureAtMs:
            successKind === "exact"
              ? observedAtMs
              : current.lastExactFailureAtMs,
          lastExactFailure:
            successKind === "exact" ? failure : current.lastExactFailure,
          lastFailureAtMs: observedAtMs,
          lastFailure: failure,
        };
      })();
};

export const DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS = 180_000;

export class L1ControlPlaneTimeoutError extends Error {
  readonly scope: string;
  readonly maxHoldMs: number;

  constructor(scope: string, maxHoldMs: number) {
    super(`L1 control-plane scope ${scope} exceeded ${maxHoldMs.toString()}ms`);
    this.name = "L1ControlPlaneTimeoutError";
    this.scope = scope;
    this.maxHoldMs = maxHoldMs;
  }
}

const l1ControlPlaneWaitTimer = Metric.timer(
  "l1_control_plane_wait_ms",
  "Time waiting for the process-wide L1 control-plane permit",
);
const l1ControlPlaneHoldTimer = Metric.timer(
  "l1_control_plane_hold_ms",
  "Time holding the process-wide L1 control-plane permit",
);
const l1ControlPlaneAcquisitionCounter = Metric.counter(
  "l1_control_plane_acquisitions_total",
  { description: "L1 control-plane permit acquisitions" },
);
const l1ControlPlaneTimeoutCounter = Metric.counter(
  "l1_control_plane_timeouts_total",
  { description: "Bounded L1 control-plane holds that timed out" },
);

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

    const LATEST_DEPOSIT_FETCH_TIME = yield* Ref.make<number>(0);

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
    const HEARTBEAT_DEPOSIT_FETCH = yield* Ref.make<number>(now);
    const HEARTBEAT_WITHDRAWAL_FETCH = yield* Ref.make<number>(now);
    const HEARTBEAT_TX_QUEUE_PROCESSOR = yield* Ref.make<number>(now);

    return {
      BLOCKS_IN_QUEUE,
      LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
      RESET_IN_PROGRESS,
      COMMIT_WORKER_ACTIVE,
      COMMIT_PIPELINE_PHASE,
      L1_CONTROL_PLANE,
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
      LATEST_DEPOSIT_FETCH_TIME,
      LATEST_LOCAL_BLOCK_END_TIME_MS,
      MEMPOOL_LEDGER_DELTA_LOG,
      TX_QUEUE_PROCESSOR_ACTIVE,
      TX_QUEUE_WAKE_GENERATION,
      NATIVE_MPF_OWNER,
      ADMISSION_BACKLOG_GAUGE,
      LOCAL_FINALIZATION_PENDING,
      HEARTBEAT_BLOCK_COMMITMENT,
      HEARTBEAT_BLOCK_CONFIRMATION,
      HEARTBEAT_MERGE,
      HEARTBEAT_DEPOSIT_FETCH,
      HEARTBEAT_WITHDRAWAL_FETCH,
      HEARTBEAT_TX_QUEUE_PROCESSOR,
    };
  }),
}) {}

export const withL1ControlPlane = <A, E, R>(
  globals: Globals,
  options: {
    readonly scope: string;
    readonly maxHoldMs?: number;
  },
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | Error, R> => {
  const maxHoldMs = options.maxHoldMs ?? DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS;
  const waitTimer = Metric.tagged(
    l1ControlPlaneWaitTimer,
    "scope",
    options.scope,
  );
  const holdTimer = Metric.tagged(
    l1ControlPlaneHoldTimer,
    "scope",
    options.scope,
  );
  const acquisitionCounter = Metric.tagged(
    l1ControlPlaneAcquisitionCounter,
    "scope",
    options.scope,
  );
  const timeoutCounter = Metric.tagged(
    l1ControlPlaneTimeoutCounter,
    "scope",
    options.scope,
  );
  return Effect.gen(function* () {
    const waitStartedAtMs = Date.now();
    return yield* globals.L1_CONTROL_PLANE.withPermits(1)(
      Effect.gen(function* () {
        yield* waitTimer(
          Effect.succeed(Duration.millis(Date.now() - waitStartedAtMs)),
        );
        yield* Metric.increment(acquisitionCounter);
        const holdStartedAtMs = Date.now();
        return yield* effect.pipe(
          Effect.timeoutFail({
            duration: Duration.millis(maxHoldMs),
            onTimeout: () =>
              new L1ControlPlaneTimeoutError(options.scope, maxHoldMs),
          }),
          Effect.tapError((error) =>
            error instanceof L1ControlPlaneTimeoutError
              ? Metric.increment(timeoutCounter)
              : Effect.void,
          ),
          Effect.ensuring(
            holdTimer(
              Effect.succeed(Duration.millis(Date.now() - holdStartedAtMs)),
            ),
          ),
        );
      }),
    );
  });
};

export const withL1ControlPlaneIfAvailable = <A, E, R>(
  globals: Globals,
  options: {
    readonly scope: string;
    readonly maxHoldMs?: number;
  },
  effect: Effect.Effect<A, E, R>,
) =>
  globals.L1_CONTROL_PLANE.withPermitsIfAvailable(1)(
    withL1ControlPlaneHeld(options, effect),
  );

const withL1ControlPlaneHeld = <A, E, R>(
  options: {
    readonly scope: string;
    readonly maxHoldMs?: number;
  },
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | Error, R> => {
  const maxHoldMs = options.maxHoldMs ?? DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS;
  const holdTimer = Metric.tagged(
    l1ControlPlaneHoldTimer,
    "scope",
    options.scope,
  );
  const acquisitionCounter = Metric.tagged(
    l1ControlPlaneAcquisitionCounter,
    "scope",
    options.scope,
  );
  const timeoutCounter = Metric.tagged(
    l1ControlPlaneTimeoutCounter,
    "scope",
    options.scope,
  );
  return Effect.gen(function* () {
    yield* Metric.increment(acquisitionCounter);
    const holdStartedAtMs = Date.now();
    return yield* effect.pipe(
      Effect.timeoutFail({
        duration: Duration.millis(maxHoldMs),
        onTimeout: () =>
          new L1ControlPlaneTimeoutError(options.scope, maxHoldMs),
      }),
      Effect.tapError((error) =>
        error instanceof L1ControlPlaneTimeoutError
          ? Metric.increment(timeoutCounter)
          : Effect.void,
      ),
      Effect.ensuring(
        holdTimer(
          Effect.succeed(Duration.millis(Date.now() - holdStartedAtMs)),
        ),
      ),
    );
  });
};

export const withL1ControlPlaneWaitTimeout = <A, E, R>(
  globals: Globals,
  options: {
    readonly scope: string;
    readonly waitTimeoutMs: number;
    readonly maxHoldMs?: number;
  },
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<Option.Option<A>, E | Error, R> =>
  Effect.uninterruptibleMask((restore) =>
    Effect.gen(function* () {
      const waitStartedAtMs = Date.now();
      const acquired = yield* restore(
        globals.L1_CONTROL_PLANE.take(1).pipe(
          Effect.timeoutOption(Duration.millis(options.waitTimeoutMs)),
        ),
      );
      if (Option.isNone(acquired)) {
        return Option.none<A>();
      }
      yield* Metric.tagged(
        l1ControlPlaneWaitTimer,
        "scope",
        options.scope,
      )(Effect.succeed(Duration.millis(Date.now() - waitStartedAtMs)));
      return yield* restore(withL1ControlPlaneHeld(options, effect)).pipe(
        Effect.map(Option.some),
        Effect.ensuring(globals.L1_CONTROL_PLANE.release(1)),
      );
    }),
  );

export const publishMempoolLedgerDelta = (
  globals: Globals,
  delta: Omit<MempoolLedgerDelta, "version">,
  maxEntries: number,
): Effect.Effect<number> =>
  Ref.modify(globals.MEMPOOL_LEDGER_DELTA_LOG, (state) => {
    const version = state.version + 1;
    const entry: MempoolLedgerDelta = {
      version,
      full: delta.full,
      upserts: delta.upserts.map(([outRefHex, output]) => [
        outRefHex,
        Buffer.from(output),
      ]),
      deletes: [...delta.deletes],
    };
    return [
      version,
      {
        version,
        entries: [...state.entries, entry].slice(-Math.max(1, maxEntries)),
      },
    ];
  });
