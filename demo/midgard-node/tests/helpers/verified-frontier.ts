/**
 * The `verifiedFrontier` a commit worker receives in a harness that runs the
 * worker program itself: what the main thread's verification of the stored
 * `confirmed_ledger` records (`verifyConfirmedLedger`), at the frontier
 * generation it read, its roots derived in this thread. None unless it passes, as in the node.
 */
import { Effect, Ref } from "effect";

import type { VerifiedConfirmedLedger } from "../../src/mpf/confirmed-ledger-advance.js";
import { deriveConfirmedLedgerRoots } from "../../src/mpf/confirmed-ledger-roots.js";
import { verifyConfirmedLedger } from "../../src/mpf/confirmed-ledger-verification.js";
import { verifiedFrontierWorkerInput } from "../../src/workers/utils/commit-block-header.js";

export const verifiedFrontierNow = verifyConfirmedLedger(
  deriveConfirmedLedgerRoots,
).pipe(
  Effect.map((outcome) =>
    verifiedFrontierWorkerInput(
      outcome.kind === "verified"
        ? { ...outcome.frontier, generation: outcome.generation }
        : undefined,
    ),
  ),
);

/** A node's verified confirmed ledger with no pass recorded, for the node's
 * landed ports in a harness whose processing folds nothing it reads back. */
export const noVerification = () => ({
  VERIFIED_CONFIRMED_LEDGER: Ref.unsafeMake<
    VerifiedConfirmedLedger | undefined
  >(undefined),
});
