import { Cause, Effect, Exit, Option, Ref } from "effect";

import type { DatabaseError } from "../database/utils/common.js";
import {
  FollowerWriteFixture,
  isFollowerWriteHeld,
  runAtFollowerView,
} from "../services/follower-write-gate.js";
import { Database, Globals } from "../services/index.js";

/**
 * How long one sweep's history prunes may keep starting batches under the
 * follower write permit. No batch starts past it, and each batch is one
 * short transaction, so a driver recompute that drains producers waits at most this
 * plus one batch.
 */
export const RETENTION_HISTORY_PRUNE_BUDGET_MS = 10_000;

/** The hard bound on the whole permit-held history prune, should one batch
 * stall: past it the work is interrupted (its open batch rolls back), the
 * permit is returned, and the next sweep retries. */
export const RETENTION_HISTORY_PRUNE_TIMEOUT_MS =
  3 * RETENTION_HISTORY_PRUNE_BUDGET_MS;

/**
 * Runs `work` under a follower write permit: it takes the permit without
 * waiting (registration is refused at once while the follower-change driver
 * recomputes or has not published a view) and holds it for at most
 * RETENTION_HISTORY_PRUNE_TIMEOUT_MS. A refused or timed-out run deletes
 * nothing more, is logged, and returns `undefined`; the next sweep retries.
 * Standalone database fixtures (no driver in the process) run the work
 * directly under their explicit fixture capability.
 */
export const withRetentionHistoryProducer = (
  work: Effect.Effect<number, DatabaseError, Database>,
): Effect.Effect<number | undefined, never, Globals | Database> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const gate = yield* Ref.get(globals.FOLLOWER_WRITE_GATE);
    const fixture = yield* Effect.serviceOption(FollowerWriteFixture);
    const run: Effect.Effect<number, DatabaseError, Globals | Database> =
      gate.epoch === undefined && Option.isSome(fixture)
        ? work
        : runAtFollowerView(work);
    const exit = yield* Effect.exit(
      run.pipe(Effect.timeout(RETENTION_HISTORY_PRUNE_TIMEOUT_MS)),
    );
    if (Exit.isSuccess(exit)) return exit.value;
    const refused = [...Cause.failures(exit.cause)].some(isFollowerWriteHeld);
    yield* Effect.logWarning(
      `retention_history_prune_skipped: ${refused ? "the follower write gate is held" : "the follower write permit or a prune batch failed"}; journals are kept until the next sweep`,
      exit.cause,
    );
    return undefined;
  });
