import { isWorkflowActuationRevokedError } from "./actuation-permit.create-workflow-reconciliation-permit-controller.js";
import {
  activeCollateralForfeitures,
  collateralAccountingFailure,
  collateralAccountingMoot,
  readShowsForfeitureReversed,
  standingCollateralAccountingFailures,
} from "./collateral-forfeiture.js";
import { isFinalWorkflowCompletion } from "./completion-finality.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalEvent,
} from "./journal.js";
import {
  FraudProofL1CheckpointChangedError,
  isFraudProofL1WaitError,
} from "./l1-source.js";
import type { FraudProofWorkflowReconcileResult } from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import { isSqliteBusyError } from "./sqlite-busy.js";

/**
 * The collateral accounting that one read of an attempt calls for, done in
 * one place for every reader of attempts: a forfeiture when the attempt
 * landed failing phase 2 and none stands, a reversal when one stands and the
 * read shows the landing gone. A step the caller does not supply is left to
 * another reader.
 *
 * A failed step holds nothing: the journal names it once, `retryLater` runs,
 * and the failure is returned so the caller reads the attempt again later.
 * A failure the run handles the same way wherever it meets it is thrown
 * on instead: a revoked actuation, an L1 source that cannot answer or
 * refuses, a chain that moved under the read, a busy database. A read that
 * answers and leaves nothing to account clears a failure that stands.
 */
export const accountAttemptCollateral = async ({
  entries,
  append,
  transactionHash,
  read,
  forfeit,
  restore,
  retryLater,
}: {
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly append: (event: FraudProofWorkflowJournalEvent) => Promise<unknown>;
  readonly transactionHash: string;
  readonly read: FraudProofWorkflowReconcileResult;
  readonly forfeit?: () => Promise<void>;
  readonly restore?: () => Promise<void>;
  readonly retryLater?: () => void;
}): Promise<string | undefined> => {
  const step = activeCollateralForfeitures(entries).has(transactionHash)
    ? readShowsForfeitureReversed(read)
      ? { run: restore }
      : undefined
    : read.kind === "not_found" && read.landedInvalid === true
      ? { run: forfeit }
      : undefined;
  const standing = standingCollateralAccountingFailures(entries);
  // A journal past its final completion is closed to diagnostics.
  const open = !entries.some(({ event }) => isFinalWorkflowCompletion(event));
  if (step === undefined) {
    if (
      standing.has(transactionHash) &&
      (read.kind !== "unknown" || read.absent === true) &&
      open
    )
      await append(collateralAccountingMoot(transactionHash));
    return undefined;
  }
  try {
    await step.run?.();
    return undefined;
  } catch (error) {
    retryLater?.();
    if (
      isWorkflowActuationRevokedError(error) ||
      isFraudProofL1WaitError(error) ||
      error instanceof FraudProofL1CheckpointChangedError ||
      isSqliteBusyError(error)
    )
      throw error;
    const failure = collateralAccountingFailure(transactionHash, error);
    if (!standing.has(transactionHash) && open) await append(failure);
    return failure.reason;
  }
};
