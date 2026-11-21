/**
 * The admission input check's view of the mempool ledger state (design D6),
 * read without the Phase B lock.
 */
import { Effect } from "effect";

import {
  type SpendableView,
  UNKNOWN_SPENDABLE_VIEW,
} from "./admission-input-claims.js";
import type { MempoolLedgerDelta, MempoolLedgerDeltaLog } from "./globals.js";

/**
 * Membership in the cache's `state`, with the journal entries published since
 * `state` was last synchronized laid over it, so a deposit projected while
 * validation is idle is already spendable. `state` changes only inside
 * synchronous sections, so a caller that uses the view synchronously never
 * sees half a patch or half a rebuild. The view cannot tell (`undefined`)
 * before `state` is first built, while canonical recovery rebuilds it, or
 * when the journal has a gap or a full-reload marker past it.
 */
export const makeSpendableView = ({
  pipeline,
  journal,
  synced,
  entriesAfter,
}: {
  readonly pipeline: Effect.Effect<{ readonly canonicalSuspended: boolean }>;
  readonly journal: Effect.Effect<MempoolLedgerDeltaLog>;
  /** `state` and the journal version it reflects (-1: not built yet). */
  readonly synced: () => {
    readonly state: ReadonlyMap<string, unknown>;
    readonly version: number;
  };
  /** The contiguous incremental entries after a version, or `undefined`. */
  readonly entriesAfter: (
    journal: MempoolLedgerDeltaLog,
    version: number,
  ) => readonly MempoolLedgerDelta[] | undefined;
}): Effect.Effect<SpendableView> =>
  Effect.gen(function* () {
    const { canonicalSuspended } = yield* pipeline;
    const log = yield* journal;
    const { state, version } = synced();
    if (canonicalSuspended || version < 0) return UNKNOWN_SPENDABLE_VIEW;
    const newer = entriesAfter(log, version);
    if (newer === undefined) return UNKNOWN_SPENDABLE_VIEW;
    if (newer.length === 0) return (outRefHex) => state.has(outRefHex);
    const overlay = new Map<string, boolean>();
    for (const entry of newer) {
      for (const outRefHex of entry.deletes) overlay.set(outRefHex, false);
      for (const [outRefHex] of entry.upserts) overlay.set(outRefHex, true);
    }
    return (outRefHex) => overlay.get(outRefHex) ?? state.has(outRefHex);
  });
