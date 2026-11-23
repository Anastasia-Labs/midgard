/**
 * The `verifiedFrontier` a commit worker receives in a harness that runs the
 * worker program itself: what the main thread's verification of the stored
 * `confirmed_ledger` records (`verifyConfirmedLedger`), at the frontier
 * generation it read, its roots derived in this thread. None unless it passes, as in the node.
 */
import { Effect, MutableRef, Ref } from "effect";

import {
  emptyConfirmedLedgerFoldLog,
  type VerifiedConfirmedLedger,
} from "../../src/mpf/confirmed-ledger-advance.js";
import { deriveConfirmedLedgerRoots } from "../../src/mpf/confirmed-ledger-roots.js";
import { verifyConfirmedLedger } from "../../src/mpf/confirmed-ledger-run.js";
import type { NativeMpfOwnerService } from "../../src/services/mpf-native-owner/protocol.js";
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

/** A node's verified confirmed ledger with no pass recorded, and no native
 * MPF owner, for the node's landed ports in a harness whose processing folds
 * nothing it reads back and keeps no native ledgers. */
export const noVerification = () => ({
  VERIFIED_CONFIRMED_LEDGER: Ref.unsafeMake<
    VerifiedConfirmedLedger | undefined
  >(undefined),
  CONFIRMED_LEDGER_FOLD_LOG: MutableRef.make(emptyConfirmedLedgerFoldLog),
  NATIVE_MPF_OWNER: Ref.unsafeMake<NativeMpfOwnerService | undefined>(
    undefined,
  ),
});
