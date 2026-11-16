/**
 * The `verifiedFrontier` a commit worker receives in a harness that runs the
 * worker program itself: what the main thread's verification of the stored
 * `confirmed_ledger` records (`verifyConfirmedLedger`), its roots derived in
 * this thread. None unless it passes, as in the node.
 */
import { Effect } from "effect";

import { deriveConfirmedLedgerRoots } from "../../src/mpf/confirmed-ledger-roots.js";
import { verifyConfirmedLedger } from "../../src/mpf/confirmed-ledger-verification.js";
import { verifiedFrontierWorkerInput } from "../../src/workers/utils/commit-block-header.js";

export const verifiedFrontierNow = verifyConfirmedLedger(
  deriveConfirmedLedgerRoots,
).pipe(
  Effect.map((outcome) =>
    verifiedFrontierWorkerInput(
      outcome.kind === "verified" ? outcome.frontier : undefined,
    ),
  ),
);
