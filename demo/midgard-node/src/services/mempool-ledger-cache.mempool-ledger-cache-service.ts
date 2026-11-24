import type {
  UTxOLookup,
  UTxOStatePatch,
} from "@al-ft/midgard-validation/phase-b";
import { Context, Data, Effect, Metric } from "effect";

import type { DatabaseError } from "../database/utils/common.js";
import type { SpendableView } from "./admission-input-claims.js";

/**
 * The working ledger as Phase B sees it: the durable in-memory ledger with
 * the live speculative patches laid over it, answered one key at a time.
 */
export type MempoolLedgerState = UTxOLookup;

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
  /** Must be called by its matching sequence while holding withPhaseBLock. */
  readonly applySpeculativePatch: (
    sequence: bigint,
    patch: UTxOStatePatch,
  ) => Effect.Effect<void, DatabaseError | ValidationPipelineEpochError>;
  /** Reads again the keys of the patches never promoted and advances beyond
   * a poisoned validation epoch. */
  readonly recoverPoisonedEpoch: Effect.Effect<void, DatabaseError>;
  /** Immediately fence every old sequence, without awaiting the cache locks.
   * Commit durable authority suspension before running the returned recovery. */
  readonly retireCanonicalEpoch: Effect.Effect<CanonicalCacheRecovery>;
  /**
   * Which outrefs are in the mempool ledger state, for the admission input
   * check (design D6). Lock-free: it never waits for the Phase B lock, and
   * a caller must use the view synchronously. It cannot tell (answers
   * `undefined`) before the cache is first loaded, while canonical recovery
   * repairs it, at a key marked stale or being read again, and at every key
   * while all of them are marked.
   */
  readonly spendableView: Effect.Effect<SpendableView>;
};

/**
 * The working-ledger keys (hex outrefs) a repair changed, and the spendable
 * outputs among them as it left them.
 */
export type DurableChanges = Readonly<{
  keys: ReadonlySet<string>;
  spendable: ReadonlyMap<string, Buffer>;
}>;

export type CanonicalCacheRecovery = {
  readonly epoch: number;
  /** Runs under claim -> Phase B -> persistence. Both callbacks may take the
   * SQL authority lock, but must not reacquire cache locks. The cache then
   * reloads in full, or, when `durableChanges` names the keys the repair
   * changed, sets those keys from it and reads again the keys of the
   * patches it dropped. Keys writers marked stale are read at the next
   * synchronization. */
  readonly runRecovery: <A, E, R, E2, R2>(
    repairDurable: Effect.Effect<A, E, R>,
    publishReady: Effect.Effect<void, E2, R2>,
    durableChanges?: (result: A) => DurableChanges | undefined,
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

export const validationLedgerCacheFullReloadCounter = Metric.counter(
  "validation_ledger_cache_full_reload_count",
  {
    description:
      "Full mempool-ledger cache reloads at startup or when every key is marked stale",
    bigint: true,
    incremental: true,
  },
);

export const validationLedgerCacheEntryWriteCounter = Metric.counter(
  "validation_ledger_cache_entry_write_count",
  {
    description:
      "Entries written into the in-memory working ledger: full-load rows, keyed repairs and promoted patch entries",
    bigint: true,
    incremental: true,
  },
);

export const validationPhaseBLockWaitTimer = Metric.timer(
  "validation_phase_b_lock_wait_duration",
  "Time validation drain loops wait for the serialized Phase B section",
);
