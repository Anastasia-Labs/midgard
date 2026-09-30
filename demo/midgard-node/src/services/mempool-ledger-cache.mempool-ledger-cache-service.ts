import { type UTxOStatePatch } from "@al-ft/midgard-validation";
import { Context, Data, Effect, Metric } from "effect";

import type { DatabaseError } from "../database/utils/common.js";

export type MempoolLedgerState = Map<string, Buffer>;

export type PhaseBSequence = {
  readonly epoch: number;
  readonly sequence: bigint;
  readonly runDecision: <A, E, R>(
    effect: Effect.Effect<A, E, R>,
  ) => Effect.Effect<A, E | ValidationPipelineEpochError, R>;
  readonly runPersistence: <A, E, R>(
    effect: Effect.Effect<A, E, R>,
  ) => Effect.Effect<A, E | ValidationPipelineEpochError, R>;
  readonly cancel: Effect.Effect<void>;
};

export class ValidationPipelineEpochError extends Data.TaggedError(
  "ValidationPipelineEpochError",
)<{
  readonly epoch: number;
  readonly sequence: bigint;
  readonly failedSequence: bigint;
  readonly message: string;
}> {}

export type MempoolLedgerCacheService = {
  readonly withClaimLock: <A, E, R>(
    effect: Effect.Effect<A, E, R>,
  ) => Effect.Effect<A, E | ValidationPipelineEpochError, R>;
  /** Register while holding withClaimLock, immediately after a non-empty claim. */
  readonly registerPhaseBSequence: Effect.Effect<PhaseBSequence>;
  readonly withPhaseBLock: <A, E, R>(
    effect: Effect.Effect<A, E, R>,
  ) => Effect.Effect<A, E, R>;
  /** Must be called while holding withPhaseBLock. */
  readonly currentState: Effect.Effect<
    MempoolLedgerState,
    DatabaseError | ValidationPipelineEpochError
  >;
  /** Must be called while holding withPhaseBLock, before ordered persistence. */
  readonly applyPatchAndSync: (
    patch: UTxOStatePatch,
  ) => Effect.Effect<void, DatabaseError | ValidationPipelineEpochError>;
  /** Must be called by its matching sequence while holding withPhaseBLock. */
  readonly applySpeculativePatch: (
    sequence: bigint,
    patch: UTxOStatePatch,
  ) => Effect.Effect<void, DatabaseError | ValidationPipelineEpochError>;
  /** Reloads durable state and advances beyond a poisoned validation epoch. */
  readonly recoverPoisonedEpoch: Effect.Effect<void, DatabaseError>;
  /** Immediately fence every old sequence, without awaiting the cache locks.
   * Commit durable authority suspension before running the returned recovery. */
  readonly retireCanonicalEpoch: Effect.Effect<CanonicalCacheRecovery>;
};

export type CanonicalCacheRecovery = {
  readonly epoch: number;
  /** Runs under claim -> Phase B -> persistence. Both callbacks may take the
   * SQL authority lock, but must not reacquire cache locks or publish deltas.
   * The owner must first drain all external delta producers/worker sessions. */
  readonly runRecovery: <A, E, R, E2, R2>(
    repairDurable: Effect.Effect<A, E, R>,
    publishReady: Effect.Effect<void, E2, R2>,
  ) => Effect.Effect<
    A,
    E | E2 | DatabaseError | ValidationPipelineEpochError,
    R | R2
  >;
};

export class MempoolLedgerCache extends Context.Tag("MempoolLedgerCache")<
  MempoolLedgerCache,
  MempoolLedgerCacheService
>() {}

export const validationLedgerCacheDeltaApplyCounter = Metric.counter(
  "validation_ledger_cache_delta_apply_count",
  {
    description: "Incremental mempool-ledger cache deltas applied",
    bigint: true,
    incremental: true,
  },
);

export const validationLedgerCacheFullReloadCounter = Metric.counter(
  "validation_ledger_cache_full_reload_count",
  {
    description:
      "Full mempool-ledger cache reloads after startup or a delta gap",
    bigint: true,
    incremental: true,
  },
);

export const validationPhaseBLockWaitTimer = Metric.timer(
  "validation_phase_b_lock_wait_duration",
  "Time validation drain loops wait for the serialized Phase B section",
);
