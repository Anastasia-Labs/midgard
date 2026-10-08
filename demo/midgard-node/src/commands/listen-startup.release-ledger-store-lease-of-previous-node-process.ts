import { Effect } from "effect";

import { MpfEngineStateDB } from "../database/index.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";

/**
 * Retires the ledger MPF lease a previous node process's commit worker or
 * payload audit left when it was killed, instead of
 * letting every commit report the store busy until the lease's TTL runs out.
 *
 * The same proof as releaseStateQueueLeasesOfPreviousNodeProcess applies: this
 * runs under the history authority this process acquired, which is never taken
 * from a live owner, and before any fiber that takes a node-process ledger
 * lease starts. So a lease with a node-process prefix belongs to a process
 * that is dead, or whose authority lapsed; retiring it fails that process's
 * next revalidate at its mutation boundary. Leases of the offline `reconcile`
 * and `mpf-audit` commands carry other owners and are kept.
 */
export const releaseLedgerStoreLeaseOfPreviousNodeProcess = Effect.gen(
  function* () {
    const retired =
      yield* MpfEngineStateDB.retireLedgerStoreLeaseOfOwnerPrefixes(
        MpfEngineStateDB.NODE_PROCESS_LEDGER_LEASE_OWNER_PREFIXES,
      ).pipe(withFollowerWrite);
    if (retired !== undefined)
      yield* Effect.logWarning(
        `Startup retired ledger MPF lease left by a previous node process: owner=${retired.owner},expires_at=${retired.expiresAt?.toISOString() ?? "null"}`,
      );
    return retired;
  },
);
