/** Where a submitted validation-trace stage stands: from its exact signed
 * bytes when the journal or the stage's recovery keeps them, otherwise from
 * the outputs of ours it claimed. */
import type { UTxO } from "@lucid-evolution/lucid";

import type { FraudProofSignedTransactionRecovery } from "../workflow/l1-source.js";
import type { FraudProofFamilyWorkflowAdapter } from "../workflow/orchestrator.js";
import { reconcileSignedWorkflowTransaction } from "../workflow/signed-transaction-reconciliation.js";
import {
  isClaimedOutput,
  isValidationTraceMove,
  parseValidationTraceAction,
  parseValidationTraceRecovery,
  recordedValidationTraceMoveBytes,
} from "./actions.js";

export const validationTraceReconcile =
  ({
    signedRecovery,
    read,
  }: {
    readonly signedRecovery: FraudProofSignedTransactionRecovery;
    /** Our outputs at `outRefs` the provider shows. */
    readonly read: (
      outRefs: readonly string[],
    ) => Promise<ReadonlyMap<string, UTxO>>;
  }): FraudProofFamilyWorkflowAdapter["reconcile"] =>
  async ({
    action,
    txHash,
    durableRecovery,
    signedTransactionCborHex,
    authorizeResubmission,
  }) => {
    if (txHash === undefined)
      return {
        kind: "conflict",
        reason: "validation-trace action omitted its transaction hash",
      };
    const { stage } = parseValidationTraceAction(action);
    let outputs: ReturnType<typeof parseValidationTraceRecovery>;
    let signed = signedTransactionCborHex;
    try {
      outputs = parseValidationTraceRecovery({
        value: durableRecovery,
        stage,
        txHash,
      });
      // A move's successor is the defender's to spend at once, so whether
      // it is live never shows the move landed: a move reconciles from its
      // exact signed bytes, which its recovery keeps when no funding
      // transition holds them.
      if (signed === undefined && isValidationTraceMove(stage))
        signed = recordedValidationTraceMoveBytes({
          value: durableRecovery,
          txHash,
        });
    } catch (cause) {
      return { kind: "conflict", reason: String(cause) };
    }
    if (signed !== undefined) {
      const reconciled = await reconcileSignedWorkflowTransaction({
        transactionHash: txHash,
        signedTransactionCborHex: signed,
        observe: signedRecovery.observeSignedTransaction,
        rebroadcast: signedRecovery.rebroadcastSignedTransaction,
        authorizeResubmission,
        reportInclusion: true,
      });
      return reconciled.kind === "pending" &&
        reconciled.retirement?.reason === "included"
        ? { kind: "confirmed", txHash }
        : reconciled;
    }
    // Without the signed bytes, any output of this exact transaction still
    // live with its claimed content shows it landed.
    const live = await read(outputs.map(({ outRef }) => outRef));
    return outputs.some((claim) =>
      isClaimedOutput(claim, live.get(claim.outRef)),
    )
      ? { kind: "confirmed", txHash }
      : { kind: "pending", txHash };
  };
