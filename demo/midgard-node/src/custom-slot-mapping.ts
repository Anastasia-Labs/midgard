/**
 * The node's Lucid slot mapping, from the local node's ledger (its system
 * start and era history over local state query), and the bounded submit-time
 * retry of a submit-slot snapshot.
 *
 * Every process (the main thread, a worker thread, a CLI command) reads the
 * mapping from its own ledger query. While the node or its sidecar is not
 * reachable the read waits, logging the unready reason, and never exits. A
 * slot length other than the profile's fails at once.
 */
import {
  SUBMIT_SLOT_LENGTH_MS,
  type SubmitSlotSnapshot,
} from "@al-ft/midgard-core/ogmios-slot";
import type { SlotConfig } from "@lucid-evolution/lucid";
import { Duration, Effect, Schedule } from "effect";

import { runProviderStepWithRetry } from "./provider-retry.js";
import { transientL1ReadCause } from "./services/l1-provider.js";

export type ResolveLucidSlotMappingOptions = {
  /** One read of the ledger's slot configuration. */
  readonly read: () => Promise<SlotConfig>;
  readonly expectedSlotLengthMs?: number;
  readonly retry?: {
    readonly baseDelayMs: number;
    readonly maxDelayMs: number;
  };
};

const DEFAULT_RETRY = { baseDelayMs: 1_000, maxDelayMs: 15_000 } as const;

const asError = (cause: unknown): Error =>
  cause instanceof Error
    ? cause
    : new Error("The ledger slot configuration read failed", { cause });

/**
 * The ledger's slot mapping for Lucid. Waits out a transient L1 read failure
 * (node, sidecar or ledger unavailable) with a logged reason; any other
 * failure, or a slot length other than the profile's, fails at once.
 */
export const resolveLucidSlotMapping = (
  options: ResolveLucidSlotMappingOptions,
): Effect.Effect<SlotConfig, Error> =>
  Effect.gen(function* () {
    const retry = options.retry ?? DEFAULT_RETRY;
    const expectedSlotLengthMs =
      options.expectedSlotLengthMs ?? SUBMIT_SLOT_LENGTH_MS;
    const slotConfig = yield* Effect.tryPromise({
      try: options.read,
      catch: asError,
    }).pipe(
      Effect.tapError((error) => {
        const transient = transientL1ReadCause(error);
        return transient === undefined
          ? Effect.void
          : Effect.logWarning(
              `L1 slot mapping unready: ${transient.message}; waits and re-reads.`,
            );
      }),
      Effect.retry({
        schedule: Schedule.exponential(Duration.millis(retry.baseDelayMs)).pipe(
          Schedule.union(Schedule.spaced(Duration.millis(retry.maxDelayMs))),
        ),
        while: (error) => transientL1ReadCause(error) !== undefined,
      }),
    );
    if (slotConfig.slotLength !== expectedSlotLengthMs)
      return yield* Effect.fail(
        new Error(
          `Ledger slot length disagreement: expected=${expectedSlotLengthMs.toString()},ledger=${slotConfig.slotLength.toString()}`,
        ),
      );
    yield* Effect.logInfo(
      `L1 slot mapping resolved: zeroTime=${slotConfig.zeroTime.toString()},zeroSlot=${slotConfig.zeroSlot.toString()},slotLength=${slotConfig.slotLength.toString()}`,
    );
    return slotConfig;
  });

export type SubmitSlotSnapshotRetryOptions = {
  readonly maxAttempts: number;
  readonly baseDelayMs: number;
  readonly maxDelayMs: number;
};

/**
 * A short bounded wait for submit time: a block gap that crosses the bound
 * often clears within seconds. Past the last attempt the stale snapshot is
 * still refused; the caller never builds against an unvouched clock.
 */
export const DEFAULT_SUBMIT_SLOT_SNAPSHOT_RETRY: SubmitSlotSnapshotRetryOptions =
  { maxAttempts: 4, baseDelayMs: 1_000, maxDelayMs: 5_000 };

export const retryTransientSubmitSlotSnapshot = (
  readOnce: () => Effect.Effect<SubmitSlotSnapshot, Error>,
  retry: SubmitSlotSnapshotRetryOptions = DEFAULT_SUBMIT_SLOT_SNAPSHOT_RETRY,
): Effect.Effect<SubmitSlotSnapshot, Error> =>
  runProviderStepWithRetry(
    "L1 ledger submit-slot snapshot",
    Effect.suspend(readOnce),
    {
      ...retry,
      isRetryable: (error) => transientL1ReadCause(error) !== undefined,
    },
  );
