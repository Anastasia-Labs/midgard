import { type StateQueueUTxO } from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";

import {
  replaceJournalStepStatus,
  type TimeoutCorrectionJournal,
} from "./remove-unattested-block.parse-timeout-correction-journal.js";
import {
  timeoutCorrectionEffectIsCanonical,
  type TimeoutCorrectionRecovery,
} from "./remove-unattested-block.reconcile-last-timeout-correction-step.js";
import { outRefLabel } from "./runtime.js";
import { reconcileSignedWorkflowTransaction } from "./workflow/signed-transaction-reconciliation.js";
import {
  type SupersededAttemptReadSchedule,
  supersededAttemptReadSchedule,
} from "./workflow/superseded-attempt-read-schedule.js";

/**
 * Owner ruling (whichever lands wins): an abandoned timeout-correction
 * attempt that a rollback lands is adopted as confirmed once the queue shows
 * its effect; past k its retirement is bookkeeping. Reads are bounded by the
 * shared schedule, so they cost little however many attempts accumulate.
 */
export const adoptLandedTimeoutCorrectionAttempts = async ({
  journal,
  queue,
  recovery,
  nowMs,
  schedule = supersededAttemptReadSchedule,
}: {
  readonly journal: TimeoutCorrectionJournal;
  readonly queue: readonly StateQueueUTxO[];
  readonly recovery: TimeoutCorrectionRecovery | undefined;
  readonly nowMs: number;
  readonly schedule?: SupersededAttemptReadSchedule;
}): Promise<TimeoutCorrectionJournal> => {
  if (recovery === undefined) return journal;
  const abandoned = journal.steps.filter(
    ({ status }) => status === "abandoned",
  );
  const due = schedule.due(
    abandoned.map(({ txHash }) => txHash),
    nowMs,
  );
  let next = journal;
  for (const txHash of due) {
    const stepIndex = next.steps.findIndex((step) => step.txHash === txHash);
    const step = next.steps[stepIndex]!;
    const result = await reconcileSignedWorkflowTransaction({
      transactionHash: step.txHash,
      signedTransactionCborHex: step.signedCbor,
      observe: recovery.observeSignedTransaction,
      // An abandoned attempt is observed, never rebroadcast.
      reportInclusion: true,
    });
    const landed =
      result.kind === "confirmed" ||
      (result.kind === "pending" && result.retirement !== undefined);
    if (landed && timeoutCorrectionEffectIsCanonical(step, queue)) {
      schedule.forget(txHash);
      next = replaceJournalStepStatus(next, stepIndex, "confirmed");
    } else if (result.kind === "not_found" && result.retirement !== undefined) {
      schedule.forget(txHash);
      next = replaceJournalStepStatus(next, stepIndex, "retired");
    } else schedule.unresolved(txHash, nowMs);
  }
  return next;
};

/**
 * Wallet inputs a replacement must also spend: for each abandoned attempt
 * that shares none of the replacement's protocol inputs (the state-queue
 * nodes and the correction lock), one of its wallet inputs that is still
 * unspent. An attempt with neither left needs none: it cannot land without a
 * rollback, and whatever lands first wins.
 */
export const timeoutCorrectionExclusionInputs = ({
  journal,
  protocolInputOutRefs,
  walletUtxos,
}: {
  readonly journal: TimeoutCorrectionJournal;
  readonly protocolInputOutRefs: readonly string[];
  readonly walletUtxos: readonly UTxO[];
}): readonly UTxO[] => {
  const forced: UTxO[] = [];
  for (const step of journal.steps) {
    if (step.status !== "abandoned") continue;
    const spent = [
      ...protocolInputOutRefs,
      ...forced.map((utxo) => outRefLabel(utxo)),
    ];
    if (step.inputOutRefs.some((outRef) => spent.includes(outRef))) continue;
    const shared = walletUtxos.find((utxo) =>
      step.inputOutRefs.includes(outRefLabel(utxo)),
    );
    if (shared !== undefined) forced.push(shared);
  }
  return forced;
};

/** Fail closed if a signed replacement could land beside an abandoned
 * attempt although it could have spent one of that attempt's inputs. */
export const assertTimeoutCorrectionExclusion = ({
  journal,
  inputOutRefs,
  walletUtxos,
}: {
  readonly journal: TimeoutCorrectionJournal;
  readonly inputOutRefs: readonly string[];
  readonly walletUtxos: readonly UTxO[];
}): void => {
  const wallet = walletUtxos.map((utxo) => outRefLabel(utxo));
  for (const step of journal.steps)
    if (
      step.status === "abandoned" &&
      !step.inputOutRefs.some((outRef) => inputOutRefs.includes(outRef)) &&
      step.inputOutRefs.some((outRef) => wallet.includes(outRef))
    )
      throw new Error(
        "Timeout correction replacement must share an input with each abandoned attempt.",
      );
};
