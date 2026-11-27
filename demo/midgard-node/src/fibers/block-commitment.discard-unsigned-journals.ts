import { Effect } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";

/**
 * Discards every journal whose commit was never signed. The caller holds
 * the state-queue mutation lease: a journal is prepared and signed only
 * under the lease it names (`holdMutationLease`), so an unsigned journal a
 * later holder finds belongs to an attempt that stopped before signing (a
 * crash, or a commit worker that outlived its lease), was never broadcast,
 * and can no longer be signed. Left in place, it would refuse every other
 * header's preparation while it is live. A signed journal is never touched
 * here: its commit may be on L1.
 *
 * Returns the discarded journals' headers (hex).
 */
export const discardStoppedUnsignedJournals = Effect.gen(function* () {
  const discarded = yield* PendingBlockFinalizationsDB.discardUnsignedJournals;
  for (const header of discarded)
    yield* Effect.logWarning(
      `🔹 Discarded unsigned pending block journal ${header}: its commit attempt stopped before signing, so nothing was broadcast.`,
    );
  return discarded;
});
