/**
 * The node's startup preparation (N1): what the node needs once, before its
 * first published follower view, run by the follower-change driver's first
 * recompute under the driver's write capability (`makeDriverRecompute`).
 *
 * It records the deployment whose settlement jobs the node enqueues, seeds
 * the commit base from the landed state queue, restores retained
 * script pins, hydrates the pending block finalization, releases the
 * previous node process's leases, checks the mutation jobs, and backfills
 * missing DA payloads (a backfill error is logged and skipped). The native
 * MPF owner starts in the recompute itself.
 *
 * A failure fails the recompute as the named hold `startup_preparation_failed`
 * (with what failed). The driver retries it on its backoff only while the
 * failure is transient (a connection-class failure, or the landed state
 * queue not ready yet: `StartupPreparationWaiting`); any other failure is
 * not retried, and startup fails on it (`awaitFollowerViewOnStartup`).
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect } from "effect";

import { restoreRetainedStatePins } from "../database/cekProgramMaterial.restore-retained-state-pins.js";
import * as NodeDeployment from "../database/node-deployment.js";
import { ContractDeploymentIdentity } from "../services/midgard-contracts.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "../workers/commit-block-header/da-payload-backfill.js";
import { landedStateQueueStartupReasons } from "./listen-startup.await-landed-state-queue.js";
import {
  assertStartupMutationJobsRecoverable,
  hydratePendingBlockFinalizationOnStartup,
  releaseStateQueueLeasesOfPreviousNodeProcess,
  seedLatestLocalBlockBoundaryOnStartup,
} from "./listen-startup.js";
import { releaseLedgerStoreLeaseOfPreviousNodeProcess } from "./listen-startup.release-ledger-store-lease-of-previous-node-process.js";

/** Names the step that failed in the failure the recompute holds on. */
const step = <A, E, R>(label: string, effect: Effect.Effect<A, E, R>) =>
  Effect.mapError(
    effect,
    (cause) => new Error(`${label}: ${formatUnknownError(cause)}`, { cause }),
  );

/**
 * The landed state queue is not ready yet: a wait on the follower and the
 * chain, which the driver retries (`retryable`).
 */
export class StartupPreparationWaiting extends Error {
  readonly retryable = true;
}

export const prepareNodeOnStartup = Effect.gen(function* () {
  const waiting = yield* landedStateQueueStartupReasons;
  if (waiting.length > 0)
    return yield* Effect.fail(
      new StartupPreparationWaiting(
        `the landed state queue is not ready: ${waiting.join(", ")}`,
      ),
    );
  const { manifestId } = yield* ContractDeploymentIdentity;
  if (manifestId !== undefined)
    yield* step("deployment record", NodeDeployment.record(manifestId));
  yield* step(
    "state-queue boundary seed",
    seedLatestLocalBlockBoundaryOnStartup,
  );
  yield* step("retained script material recovery", restoreRetainedStatePins);
  yield* step(
    "pending block finalization hydration",
    hydratePendingBlockFinalizationOnStartup,
  );
  yield* step(
    "state-queue lease release",
    releaseStateQueueLeasesOfPreviousNodeProcess,
  );
  yield* step(
    "ledger store lease release",
    releaseLedgerStoreLeaseOfPreviousNodeProcess,
  );
  yield* step(
    "mutation jobs recovery check",
    assertStartupMutationJobsRecoverable,
  );
  yield* backfillMissingDaPayloadsFromFinalizedJournals({ limit: 100 }).pipe(
    Effect.tap((summary) =>
      summary.scanned === 0
        ? Effect.void
        : Effect.logInfo(
            `Startup DA payload backfill scanned=${summary.scanned.toString()},backfilled=${summary.backfilled.length.toString()},skipped=${summary.skipped.length.toString()}`,
          ),
    ),
    Effect.catchAll((error) =>
      Effect.logWarning(
        `Startup DA payload backfill skipped after error: ${formatUnknownError(error)}`,
      ),
    ),
  );
}).pipe(
  Effect.tapError((error) =>
    Effect.logError(`Startup preparation failed: ${error.message}`),
  ),
);
