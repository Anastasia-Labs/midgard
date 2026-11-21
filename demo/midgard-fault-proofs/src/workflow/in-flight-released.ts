/**
 * The in-flight parts of objectives no run drives any more.
 *
 * An objective can stop being driven (its header left the queue, it was
 * completed or held) while a transaction this process sent for it may
 * still land. No run of it comes again to follow that transaction, so the
 * process follows it here, outside any run, on each pass its owner makes:
 * one that landed final, or can no longer land (expired, invalidated, an
 * input spent by another transaction), settles
 * and releases its coins; one that may still land (pending, absent but
 * still valid, landed short of final, or not readable now) stays, holding
 * its coins and counting for every requirement read absent at an address
 * it may land at (`in-flight-absence.ts`). Once nothing of the objective is
 * left in flight its part is forgotten, with every hold it kept.
 *
 * Nothing is broadcast again: no authority drives the objective, and a
 * transaction left absent lands from the mempool or dies by its bound or
 * by a spend of its inputs. A part an owner drives again (`driven`) is left
 * to its run, which follows it as its own.
 */
import type { WorkflowInFlight, WorkflowSends } from "./in-flight.js";
import type { FraudProofWorkflowTerminalVerifier } from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import { PROOF_MATERIALS_SWEEP_ACTION_PREFIX } from "./proof-materials-sweep.action.js";
import { isFinalAtReleaseDepth } from "./signed-transaction-reconciliation.js";

/** The statuses of a transaction that can no longer land. */
const CANNOT_LAND: ReadonlySet<string> = new Set([
  "expired",
  "invalidated",
  "expired_at_tip",
  "invalidated_at_tip",
  "landed_invalid",
]);

export const followReleasedInFlight = async ({
  inFlight,
  driven,
  observeSignedTransaction,
}: {
  readonly inFlight: Pick<WorkflowInFlight, "objectives" | "forget">;
  /** Whether a run of the objective is queued, running or may come again. */
  readonly driven: (objective: WorkflowSends) => boolean;
  readonly observeSignedTransaction: FraudProofWorkflowTerminalVerifier["observeSignedTransaction"];
}): Promise<void> => {
  for (const sends of inFlight.objectives()) {
    if (driven(sends)) continue;
    for (const transaction of sends.inFlight()) {
      let observed: Awaited<ReturnType<typeof observeSignedTransaction>>;
      try {
        observed = await observeSignedTransaction({
          transactionHash: transaction.txHash,
          signedTransactionCborHex: transaction.signedTransactionCborHex,
        });
      } catch {
        continue; // read again on the next pass
      }
      if (driven(sends)) break;
      if (isFinalAtReleaseDepth(observed))
        sends.settle(transaction.txHash, true);
      else if (CANNOT_LAND.has(observed.status))
        sends.settle(transaction.txHash);
    }
    if (
      !driven(sends) &&
      sends.inFlight().length === 0 &&
      inFlight.objectives().includes(sends)
    )
      inFlight.forget(sends);
  }
};

/** Whether a proof-materials sweep this process sent, for any objective,
 * may still land. */
export const proofMaterialsSweepInFlight = (
  inFlight: Pick<WorkflowInFlight, "objectives">,
): boolean =>
  inFlight
    .objectives()
    .some((sends) =>
      sends
        .inFlight()
        .some(({ actionId }) =>
          actionId.startsWith(PROOF_MATERIALS_SWEEP_ACTION_PREFIX),
        ),
    );
