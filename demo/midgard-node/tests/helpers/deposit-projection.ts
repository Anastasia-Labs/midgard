/**
 * Bench harness (Stage B replica): `entries`, awaiting deposit rows already
 * inserted, projected as follower ingestion projects them (hidden
 * `mempool_ledger` rows, marked projected), then the empty incremental cache
 * delta the deleted deposit projection fiber published for them. Follower
 * ingestion publishes no delta for hidden rows; the bench keeps the bump as
 * load. Needs a history-ingestion permit or the unowned-history fixture.
 */
import { Effect } from "effect";

import { DepositsDB, MempoolLedgerDB } from "../../src/database/index.js";
import {
  Globals,
  NodeConfig,
  publishMempoolLedgerDelta,
} from "../../src/services/index.js";

export const projectDepositsToMempoolLedger = (
  entries: readonly DepositsDB.Entry[],
) =>
  Effect.gen(function* () {
    yield* MempoolLedgerDB.reconcileDepositEntries(
      yield* Effect.forEach(entries, DepositsDB.toMempoolLedgerEntry),
    );
    yield* DepositsDB.markAwaitingAsProjected(
      entries.map((entry) => entry[DepositsDB.Columns.ID]),
    );
    yield* publishMempoolLedgerDelta(
      yield* Globals,
      { full: false, upserts: [], deletes: [] },
      (yield* NodeConfig).VALIDATION_LEDGER_DELTA_LOG_MAX,
    );
  });
