import { Effect } from "effect";

import type * as Pending from "../database/pendingBlockFinalizations.js";
import {
  reviveReplacedCanonicalJournal,
  signedIntentReplacementDigest,
} from "./canonical-journal-recovery.js";
import { C, failure } from "./history-expired-intent-release.table.js";
import { reincludeStateQueueCorrectedBlocks } from "./state-queue-correction-recovery.js";

/** Abandons each displaced block (sibling first, then its descendants),
 * reopened from local finalization under its own replacement digest so it
 * stays revivable should it ever land after all, then revives `winner`, in
 * the caller's transaction: only once no sibling on its base is locally
 * finalized can the revival take it. Its headers. */
export const reopenDisplaced = (displaced: readonly Pending.Record[]) =>
  Effect.gen(function* () {
    const reopened = yield* Effect.forEach(displaced, (block) => {
      const digest = signedIntentReplacementDigest(block);
      return digest === undefined
        ? Effect.fail(
            failure(
              `Displaced block ${block[C.HEADER_HASH].toString("hex")} has no signed intent to abandon it under`,
            ),
          )
        : Effect.succeed({
            headerHash: block[C.HEADER_HASH].toString("hex"),
            transitionDigest: digest,
            kind: "displaced" as const,
          });
    });
    if (reopened.length > 0) {
      const results = yield* reincludeStateQueueCorrectedBlocks(reopened);
      const missed = reopened.find(
        (_, index) =>
          results[index]?.journalFound !== true ||
          results[index]?.abandonedFromStatus === undefined,
      );
      if (results.length !== reopened.length || missed !== undefined)
        return yield* Effect.fail(
          failure(
            `Recovery did not abandon displaced block ${missed?.headerHash ?? "?"}`,
          ),
        );
    }
    return reopened.map(({ headerHash }) => headerHash);
  });

export const reviveOver = (
  winner: Pending.Record,
  displaced: readonly Pending.Record[],
) =>
  reopenDisplaced(displaced).pipe(
    Effect.tap(() => reviveReplacedCanonicalJournal(winner[C.HEADER_HASH])),
  );
