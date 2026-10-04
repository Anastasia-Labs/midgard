import { Cause, Effect, Exit, Option, Ref } from "effect";

import type { DatabaseError } from "../database/utils/common.js";
import {
  isHistoryProducerGateClosed,
  runHistoryProducer,
  UnownedHistoryFixture,
} from "../services/event-history-producer.js";
import { Database, Globals } from "../services/index.js";

/**
 * How long one sweep's history prunes may keep starting batches under the
 * history producer permit. No batch starts past it, and each batch is one
 * short transaction, so a recovery that drains producers waits at most this
 * plus one batch.
 */
export const RETENTION_HISTORY_PRUNE_BUDGET_MS = 10_000;

/** The hard bound on the whole permit-held history prune, should one batch
 * stall: past it the work is interrupted (its open batch rolls back), the
 * permit is returned, and the next sweep retries. */
export const RETENTION_HISTORY_PRUNE_TIMEOUT_MS =
  3 * RETENTION_HISTORY_PRUNE_BUDGET_MS;

/**
 * Runs `work` as a history producer: it takes the permit without waiting
 * (registration is refused at once while the owner recovers, lags or is not
 * up) and holds it for at most RETENTION_HISTORY_PRUNE_TIMEOUT_MS. A refused
 * or timed-out run deletes nothing more, is logged, and returns `undefined`;
 * the next sweep retries. Standalone database fixtures without an owner run
 * the work directly under their explicit fixture capability.
 */
export const withRetentionHistoryProducer = (
  work: Effect.Effect<number, DatabaseError, Database>,
): Effect.Effect<number | undefined, never, Globals | Database> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const owner = yield* Ref.get(globals.EVENT_HISTORY_OWNER);
    const fixture = yield* Effect.serviceOption(UnownedHistoryFixture);
    const run: Effect.Effect<number, DatabaseError, Globals | Database> =
      owner === undefined && Option.isSome(fixture)
        ? work
        : runHistoryProducer(work);
    const exit = yield* Effect.exit(
      run.pipe(Effect.timeout(RETENTION_HISTORY_PRUNE_TIMEOUT_MS)),
    );
    if (Exit.isSuccess(exit)) return exit.value;
    const refused = [...Cause.failures(exit.cause)].some(
      isHistoryProducerGateClosed,
    );
    yield* Effect.logWarning(
      `retention_history_prune_skipped: ${refused ? "the history owner is recovering" : "the history producer permit or a prune batch failed"}; journals are kept until the next sweep`,
      exit.cause,
    );
    return undefined;
  });
