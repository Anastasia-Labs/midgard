import { Effect } from "effect";

import { StateQueueMutationLeasesDB } from "../database/index.js";
import { TIMEOUT_CORRECTION_LEASE_HOLDER } from "../fibers/attestation-timeout-correction.reconcile-state-queue-corrections.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import { NODE_PROCESS_MPF_AUDIT_LEASES } from "./mpf-audit-leases.js";

/**
 * State-queue lease holders that only a node process takes, from fibers that
 * start after startup preparation: block commitment, merge,
 * attestation-timeout removal and the node's own payload audit.
 * `mpf-payload-audit` is deliberately absent: the offline `mpf-audit` command
 * takes it without the node instance lock, so a live one may exist beside this
 * node.
 */
export const NODE_PROCESS_STATE_QUEUE_LEASE_HOLDERS: readonly string[] = [
  "block_commitment",
  "state_queue_merge",
  TIMEOUT_CORRECTION_LEASE_HOLDER,
  NODE_PROCESS_MPF_AUDIT_LEASES.stateQueueHolder,
];

const RETIRED_AT_STARTUP =
  "retired at startup: the node process holding it ended without releasing it";

/**
 * Retires the state-queue leases a previous node process left active when it
 * was killed, instead of letting commitment and merge report Busy until the
 * TTL runs out.
 *
 * Startup preparation runs while this process holds the node instance lock
 * (`acquireNodeInstanceLock`), which Postgres grants to one live session per
 * database schema, and before any fiber that takes these holders starts. So
 * no other node process is live on this database, and every active lease of
 * a node-only holder belongs to a process that ended. The update runs
 * through withFollowerWrite, like every write that derives from the L1 view.
 */
export const releaseStateQueueLeasesOfPreviousNodeProcess = Effect.gen(
  function* () {
    const retired =
      yield* StateQueueMutationLeasesDB.retireActiveLeasesOfHolders(
        NODE_PROCESS_STATE_QUEUE_LEASE_HOLDERS,
        RETIRED_AT_STARTUP,
      ).pipe(withFollowerWrite);
    for (const lease of retired)
      yield* Effect.logWarning(
        `Startup retired state-queue lease left by a previous node process: ${StateQueueMutationLeasesDB.describeActiveLease(lease)}`,
      );
    return retired;
  },
);
