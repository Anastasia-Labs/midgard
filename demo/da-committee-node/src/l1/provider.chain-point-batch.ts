import { type UTxO } from "@lucid-evolution/lucid";

import type { ChainPoint } from "../domain.js";

/**
 * Per-UTxO inclusion lookups and confirmation-depth walks one snapshot runs
 * at a time. Each walk opens its own Ogmios session, so an unbounded fan-out
 * over a long state queue opens one session per output at once.
 */
export const CHAIN_POINT_RESOLUTION_CONCURRENCY = 4;

/**
 * Longest one snapshot's chain-point resolution may run before the pass is
 * abandoned, at most half the L1-view deadline. A pass that outlasts it
 * fails as an observation failure the next tick retakes, rather than holding
 * the tick on a view that is no longer current.
 */
export const CHAIN_POINT_BATCH_DEADLINE_MS = 60_000;

export const chainPointBatchDeadlineMs = (l1ViewFatalMs: number): number =>
  Math.max(
    1,
    Math.min(CHAIN_POINT_BATCH_DEADLINE_MS, Math.floor(l1ViewFatalMs / 2)),
  );

/** A snapshot's chain-point resolution ran past its deadline. */
export class ChainPointBatchDeadlineError extends Error {
  constructor(deadlineMs: number) {
    super(
      `state-queue chain-point resolution exceeded its ${deadlineMs.toString()} ms deadline; abandoning this pass`,
    );
    this.name = "ChainPointBatchDeadlineError";
  }
}

/**
 * Resolves one UTxO's chain point. `resolveAll`, when present, resolves a
 * whole snapshot's UTxOs against one pinned chain tip, which the caller
 * prefers to resolving them one by one.
 */
export type ChainPointResolver = ((utxo: UTxO) => Promise<ChainPoint>) & {
  readonly resolveAll?: (
    utxos: readonly UTxO[],
  ) => Promise<readonly ChainPoint[]>;
};

/**
 * Maps `items` through `run` with at most `limit` calls in flight, keeping
 * the input order. After the first failure no further call starts; the
 * calls already running settle before that failure is rethrown, so none
 * outlives the batch.
 */
export const mapWithConcurrency = async <T, R>(
  items: readonly T[],
  limit: number,
  run: (item: T) => Promise<R>,
): Promise<R[]> => {
  const results = new Array<R>(items.length);
  let next = 0;
  let failure: { readonly error: unknown } | undefined;
  const worker = async (): Promise<void> => {
    while (failure === undefined && next < items.length) {
      const index = next;
      next += 1;
      try {
        results[index] = await run(items[index] as T);
      } catch (error) {
        failure ??= { error };
      }
    }
  };
  await Promise.all(
    Array.from({ length: Math.max(1, Math.min(limit, items.length)) }, worker),
  );
  if (failure !== undefined) throw failure.error;
  return results;
};

/** Every UTxO's chain point, through `resolveAll` when the resolver has it. */
export const resolveChainPoints = (
  resolver: ChainPointResolver,
  utxos: readonly UTxO[],
): Promise<readonly ChainPoint[]> =>
  resolver.resolveAll === undefined
    ? mapWithConcurrency(utxos, CHAIN_POINT_RESOLUTION_CONCURRENCY, resolver)
    : resolver.resolveAll(utxos);
