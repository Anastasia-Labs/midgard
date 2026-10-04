import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect, Either } from "effect";

import { ForeignTipReconciliationsDB } from "../database/index.js";
import { runHistoryProducer } from "../services/event-history-producer.js";
import { Database, Globals } from "../services/index.js";

/** One sweep deletes at most this many foreign-tip batches, so a large
 * backlog drains over several sweeps instead of holding one open. */
export const FOREIGN_TIP_RETENTION_MAX_BATCHES_PER_SWEEP = 64;

/** Deletes foreign-tip batches until one comes back short or the per-sweep cap
 * is reached. Each batch is its own history-owner write. A failed batch stops
 * this sweep's foreign-tip retention, logged with its cause; the next sweep
 * retries. */
export const pruneForeignTipsBeyondRetention = (
  args: Parameters<typeof ForeignTipReconciliationsDB.pruneBeyondRetention>[0],
): Effect.Effect<number, never, Database | Globals> =>
  Effect.gen(function* () {
    let pruned = 0;
    for (
      let batch = 0;
      batch < FOREIGN_TIP_RETENTION_MAX_BATCHES_PER_SWEEP;
      batch += 1
    ) {
      const deleted = yield* Effect.either(
        runHistoryProducer(
          ForeignTipReconciliationsDB.pruneBeyondRetention(args),
        ),
      );
      if (Either.isLeft(deleted)) {
        yield* Effect.logWarning(
          `Foreign-tip retention stopped after ${pruned.toString()} row(s); retrying next sweep: ${formatUnknownError(deleted.left)}`,
          deleted.left,
        );
        return pruned;
      }
      pruned += deleted.right;
      if (
        deleted.right <
        ForeignTipReconciliationsDB.FOREIGN_TIP_RETENTION_BATCH_SIZE
      )
        return pruned;
    }
    return pruned;
  });
