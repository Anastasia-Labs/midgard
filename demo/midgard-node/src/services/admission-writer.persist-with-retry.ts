import { Cause, Chunk, Duration, Effect, Exit, Metric } from "effect";

import type * as TxAdmissionsDB from "../database/txAdmissions.js";
import { isConnectionClassError } from "../provider-retry.js";
import {
  type AdmissionBatchPersistence,
  type AdmissionItemResult,
  type AdmissionWriteItem,
  admissionWritePoisonCounter,
  admissionWriteRetryCounter,
} from "./admission-writer.validate-options.js";

/** SQLSTATEs a later attempt can clear: serialization failure, deadlock,
 * lock not available, and a statement cancelled (our own attempt timeout). */
const TRANSIENT_SQLSTATES: ReadonlySet<string> = new Set([
  "40001",
  "40P01",
  "55P03",
  "57014",
]);

const hasTransientSqlState = (error: unknown): boolean => {
  let current: unknown = error;
  for (let depth = 0; depth < 8 && current !== null; depth += 1) {
    if (typeof current !== "object") return false;
    const code = (current as { readonly code?: unknown }).code;
    if (typeof code === "string" && TRANSIENT_SQLSTATES.has(code)) return true;
    current = (current as { readonly cause?: unknown }).cause;
  }
  return false;
};

/**
 * A failed batch attempt that a retry can clear: a lost or refused
 * connection, a serialization failure, deadlock or lock timeout, or an
 * attempt that ran past its timeout. Anything else (a constraint the row
 * breaks, a malformed value, a defect) fails again the same way.
 */
export const isTransientAdmissionWriteFailure = (
  cause: Cause.Cause<unknown>,
): boolean =>
  Chunk.toReadonlyArray(Cause.failures(cause)).some(
    (failure) =>
      Cause.isTimeoutException(failure) || hasTransientSqlState(failure),
  ) || isConnectionClassError(cause);

export const toReservedRequest = (
  item: AdmissionWriteItem,
): TxAdmissionsDB.ReservedAdmissionRequest => ({
  txId: item.request.txId,
  txCanonicalCbor: item.request.txCanonicalCbor,
  submitSource: item.request.submitSource,
  txFullHashV1: item.request.txFullHash,
  requestCount: item.requestCount,
});

export type PersistWithRetryOptions = {
  readonly retryBaseMs: number;
  readonly retryMaxMs: number;
  readonly attemptTimeoutMs: number;
  /** Called after every attempt that resolved its rows. */
  readonly onWritten: (
    items: readonly AdmissionWriteItem[],
    durationMs: number,
  ) => Effect.Effect<void>;
};

/**
 * Writes `items` until each one is resolved or poisoned:
 *
 * - a transient failure retries the same rows with exponential backoff; the
 *   txs stay pending, so a long outage shows as a stall, never as a loss;
 * - any other failure splits the batch and writes the rows one by one;
 * - a single row that still fails that way is poisoned: dropped with a
 *   counter and an error log, so it cannot block its shard.
 *
 * Retrying rows an earlier attempt may have committed is safe: the insert
 * resolves a stored row with the same full hash as a duplicate.
 */
export const persistWithRetry = <R>(
  persistBatch: AdmissionBatchPersistence<R>,
  options: PersistWithRetryOptions,
) => {
  const attemptOnce = (
    items: readonly AdmissionWriteItem[],
  ): Effect.Effect<
    Exit.Exit<readonly TxAdmissionsDB.ReservedAdmissionOutcome[], unknown>,
    never,
    R
  > =>
    Effect.gen(function* () {
      const startedAt = Date.now();
      const exit = yield* Effect.exit(
        persistBatch(items.map(toReservedRequest)).pipe(
          Effect.timeout(Duration.millis(options.attemptTimeoutMs)),
        ),
      );
      if (Exit.isSuccess(exit)) {
        if (exit.value.length !== items.length) {
          return Exit.die(
            new Error(
              `Admission batch outcome cardinality mismatch: expected ${items.length.toString()}, received ${exit.value.length.toString()}`,
            ),
          );
        }
        yield* options.onWritten(items, Date.now() - startedAt);
      }
      return exit;
    });

  const backoff = (attempt: number): Duration.Duration =>
    Duration.millis(
      Math.min(
        options.retryMaxMs,
        options.retryBaseMs * 2 ** Math.min(attempt, 16),
      ),
    );

  const write = (
    items: readonly AdmissionWriteItem[],
  ): Effect.Effect<readonly AdmissionItemResult[], never, R> =>
    Effect.gen(function* () {
      for (let attempt = 0; ; attempt += 1) {
        const exit = yield* attemptOnce(items);
        if (Exit.isSuccess(exit)) {
          return exit.value.map(
            (outcome) => ({ _tag: "Written", outcome }) as const,
          );
        }
        if (isTransientAdmissionWriteFailure(exit.cause)) {
          yield* Metric.increment(admissionWriteRetryCounter);
          if (attempt === 0 || attempt % 20 === 0) {
            yield* Effect.logWarning(
              `Admission write of ${items.length.toString()} txs failed transiently (attempt ${(attempt + 1).toString()}); retrying.`,
              exit.cause,
            );
          }
          yield* Effect.sleep(backoff(attempt));
          continue;
        }
        if (items.length > 1) {
          yield* Effect.logWarning(
            `Admission write of ${items.length.toString()} txs failed; writing them one by one.`,
            exit.cause,
          );
          const results: AdmissionItemResult[] = [];
          for (const item of items) results.push(...(yield* write([item])));
          return results;
        }
        yield* Metric.increment(admissionWritePoisonCounter);
        yield* Effect.logError(
          `Dropping acknowledged tx ${items[0]!.txIdHex}: its admission row cannot be written.`,
          exit.cause,
        );
        return [{ _tag: "Poisoned", cause: exit.cause }];
      }
    });

  return write;
};
