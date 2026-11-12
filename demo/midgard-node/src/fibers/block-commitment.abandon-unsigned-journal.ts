import { Effect, Option } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  PendingBlockFinalizationsDB,
  WithdrawalsDB,
} from "../database/index.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";

const Journals = PendingBlockFinalizationsDB;

/**
 * Abandons the active journal if its commit was never signed, and clears its
 * events' projected header. The caller holds the state-queue mutation lease:
 * a journal is prepared and signed only under the lease it names
 * (`holdMutationLease`), so an unsigned journal a later holder finds belongs
 * to an attempt that stopped before signing (a crash, or a commit worker that
 * outlived its lease), was never broadcast, and can no longer be signed. Left
 * active, it would refuse every other header's preparation. A journal with a
 * signed intent is never touched here: its commit may be on L1.
 *
 * Returns the abandoned journal's header (hex), if any.
 */
export const abandonUnsignedActiveJournal = Effect.gen(function* () {
  const active = yield* Journals.retrieveActive();
  if (Option.isNone(active)) return undefined;
  const record = active.value;
  if (
    record[Journals.Columns.INTENDED_TX_HASH] != null ||
    record[Journals.Columns.SUBMITTED_TX_HASH] != null
  )
    return undefined;
  const headerHash = record[Journals.Columns.HEADER_HASH];
  const abandoned = yield* Effect.gen(function* () {
    yield* Journals.assertCanonicalEventMembers(record);
    if (!(yield* Journals.markUnsubmittedAbandoned(headerHash))) return false;
    yield* DepositsDB.clearProjectedHeaderAssignmentByEventIds(
      record.depositEventIds,
      headerHash,
    );
    yield* ForcedTransactionsDB.clearProjectedHeaderAssignmentByEventIds(
      record.forcedTransactionEventIds,
      headerHash,
    );
    yield* WithdrawalsDB.clearProjectedHeaderAssignmentByEventIds(
      record.withdrawalEventIds,
      headerHash,
    );
    return true;
  }).pipe(withFollowerWrite);
  if (!abandoned) return undefined;
  const headerHex = headerHash.toString("hex");
  yield* Effect.logWarning(
    `🔹 Abandoned unsigned pending block journal ${headerHex}: its commit attempt stopped before signing, so nothing was broadcast.`,
  );
  return headerHex;
});
