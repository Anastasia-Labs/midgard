import { Effect } from "effect";

import {
  ConfirmedLedgerDB,
  PendingBlockFinalizationsDB,
} from "../../src/database/index.js";
import { materializeConfirmedLedgerDeltaChain } from "../../src/transactions/state-queue/confirmed-ledger-snapshot.js";

/**
 * The ledger after `record`'s block: `confirmed_ledger` with the delta chain
 * of every journal from the confirmed state up to `record` applied.
 */
export const materializeJournalLedger = (
  record: PendingBlockFinalizationsDB.Record,
) =>
  Effect.gen(function* () {
    const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
    return yield* materializeConfirmedLedgerDeltaChain({
      record,
      confirmedEntries,
      retrieveParent: PendingBlockFinalizationsDB.retrieveByHeaderHash,
    });
  });
