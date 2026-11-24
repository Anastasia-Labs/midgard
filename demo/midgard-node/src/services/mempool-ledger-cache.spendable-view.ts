/**
 * The admission input check's view of the mempool ledger state (design D6),
 * read without the Phase B lock.
 */
import { Effect } from "effect";

import {
  type SpendableView,
  UNKNOWN_SPENDABLE_VIEW,
} from "./admission-input-claims.js";
import type { MempoolLedgerStale } from "./globals.js";
import type { MempoolLedgerState } from "./mempool-ledger-cache.mempool-ledger-cache-service.js";

/**
 * Membership in the cache's working ledger (`state`, the durable ledger with
 * the live patches over it). `state` changes only inside synchronous
 * sections, so a caller that uses the view synchronously never sees half a
 * patch or half a load. The view cannot tell (`undefined`) before `state` is
 * first loaded, while canonical recovery repairs it, at a key marked stale or
 * being read again, and at every key while all of them are.
 */
export const makeSpendableView = ({
  pipeline,
  stale,
  current,
}: {
  readonly pipeline: Effect.Effect<{ readonly canonicalSuspended: boolean }>;
  /** The keys writers have marked and the cache has not taken yet. */
  readonly stale: Effect.Effect<MempoolLedgerStale>;
  readonly current: () => {
    readonly state: MempoolLedgerState;
    readonly loaded: boolean;
    /** The marks the cache has taken and is reading again. */
    readonly inFlight: MempoolLedgerStale;
  };
}): Effect.Effect<SpendableView> =>
  Effect.gen(function* () {
    const { canonicalSuspended } = yield* pipeline;
    const marked = yield* stale;
    const { state, loaded, inFlight } = current();
    if (canonicalSuspended || !loaded || marked.all || inFlight.all)
      return UNKNOWN_SPENDABLE_VIEW;
    return (outRefHex) =>
      marked.keys.has(outRefHex) || inFlight.keys.has(outRefHex)
        ? undefined
        : state.get(outRefHex) !== undefined;
  });
