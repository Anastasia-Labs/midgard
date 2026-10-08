/**
 * Bench harness (Stage B replica): `entries`, awaiting deposit rows already
 * inserted, projected as follower ingestion projects them: hidden
 * `mempool_ledger` rows, marked projected. Follower ingestion publishes no
 * validation-cache delta for hidden rows, so neither does this. Needs a
 * follower write capability or the fixture capability.
 */
import { Effect } from "effect";

import { DepositsDB, MempoolLedgerDB } from "../../src/database/index.js";

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
  });
