import {
  type CorrectionLockDatum,
  DA_ATTESTATION_TIMEOUT_MS,
  getStateQueueNodeFromStateQueueDatum,
  NO_DA_ATTESTATION,
  type StateQueueUTxO,
} from "@al-ft/midgard-sdk";
import { type Script, validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type ContractDeploymentInfo } from "./inspect-contracts.js";
import {
  headerHashOf,
  outRefsOf,
  replaceJournalStepStatus,
  type TimeoutCorrectionJournal,
  type TimeoutCorrectionStepReconciliation,
  type TimeoutCorrectionTransactionStatus,
  type TimeoutCorrectionTxKind,
} from "./remove-unattested-block.parse-timeout-correction-journal.js";
import { requireMatchingScriptHash } from "./runtime.js";
import {
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "./workflow/signed-transaction-reconciliation.js";

/**
 * Reconcile the sole recoverable transaction intent against both its
 * provider-authenticated status and the freshly authenticated queue. A
 * confirmed transaction is not journal-confirmed until its exact spent
 * outrefs and removed header have disappeared from the canonical queue.
 */
export const reconcileLastTimeoutCorrectionStep = (
  journal: TimeoutCorrectionJournal,
  queue: readonly StateQueueUTxO[],
  transactionStatus: TimeoutCorrectionTransactionStatus,
): TimeoutCorrectionStepReconciliation => {
  const stepIndex = journal.steps.findIndex(
    (step) => step.status === "prepared" || step.status === "submitted",
  );
  const lastStep = journal.steps[stepIndex];
  if (
    lastStep === undefined ||
    (lastStep.status !== "prepared" && lastStep.status !== "submitted")
  ) {
    return { disposition: "none", journal };
  }
  if (
    transactionStatus === "pending" ||
    transactionStatus === "unknown" ||
    transactionStatus === "not_found" ||
    transactionStatus === "failed"
  ) {
    return { disposition: "pending", journal };
  }
  if (transactionStatus === "expired" || transactionStatus === "invalidated") {
    return {
      disposition: "superseded",
      journal: replaceJournalStepStatus(journal, stepIndex, "retired"),
    };
  }

  const currentOutRefs = new Set(outRefsOf(queue));
  const recordedInputsAreSpent = lastStep.inputOutRefs.every(
    (outRef) => !currentOutRefs.has(outRef),
  );
  const removedHeaderIsAbsent = !queue.some(
    (node, index) =>
      index > 0 && headerHashOf(node) === lastStep.removedHeaderHash,
  );
  if (!recordedInputsAreSpent || !removedHeaderIsAbsent) {
    return { disposition: "pending", journal };
  }
  return {
    disposition: "confirmed",
    journal: replaceJournalStepStatus(journal, stepIndex, "confirmed"),
  };
};

export type TimeoutCorrectionPlan = {
  readonly kind: TimeoutCorrectionTxKind;
  readonly predecessor: StateQueueUTxO;
  readonly target: StateQueueUTxO;
  readonly removed: StateQueueUTxO;
  readonly inputOutRefs: readonly string[];
};

/** Locate the authenticated target without discarding its unaffected prefix. */
export const planNextTimeoutCorrection = (
  queue: readonly StateQueueUTxO[],
  targetHeaderHash: string,
): TimeoutCorrectionPlan | undefined => {
  if (queue[0]?.datum.key !== "Empty")
    throw new Error(
      "Canonical state queue is missing its confirmed-state root.",
    );
  const index = queue.findIndex(
    (node, i) => i > 0 && headerHashOf(node) === targetHeaderHash,
  );
  if (index < 0) return undefined;
  const predecessor = queue[index - 1]!;
  const target = queue[index]!;
  if (
    predecessor.datum.next === "Empty" ||
    predecessor.datum.next.Key.key !== targetHeaderHash
  )
    throw new Error(
      "Timeout target is not linked from its authenticated predecessor.",
    );
  const descendant = queue[index + 1];
  if (descendant !== undefined) {
    if (
      target.datum.next === "Empty" ||
      target.datum.next.Key.key !== headerHashOf(descendant)
    )
      throw new Error(
        "Timeout target does not link to its immediate descendant.",
      );
    return {
      kind: "prune-descendant",
      predecessor,
      target,
      removed: descendant,
      inputOutRefs: outRefsOf([target, descendant]),
    };
  }
  if (target.datum.next !== "Empty")
    throw new Error("Terminal timeout target retains a descendant link.");
  return {
    kind: "remove-block",
    predecessor,
    target,
    removed: target,
    inputOutRefs: outRefsOf([predecessor, target]),
  };
};

/** Resume the on-chain correction before considering a different expired block. */
export const selectTimeoutCorrectionTarget = async (
  queue: readonly StateQueueUTxO[],
  nowMs: bigint,
  lock: CorrectionLockDatum,
): Promise<{ target: StateQueueUTxO; deadline: bigint } | undefined> => {
  const lockedTarget =
    lock === "Idle" ? undefined : lock.Locked.target_header_hash;
  if (
    lock !== "Idle" &&
    lock.Locked.correction_identity !== "AttestationTimeout"
  )
    throw new Error(
      "State-queue correction lock is owned by another correction kind.",
    );
  let waiting: { target: StateQueueUTxO; deadline: bigint } | undefined;
  for (const target of queue.slice(1)) {
    if (lockedTarget !== undefined && headerHashOf(target) !== lockedTarget)
      continue;
    const node = await Effect.runPromise(
      getStateQueueNodeFromStateQueueDatum(target.datum),
    );
    if (node.da_attestation !== NO_DA_ATTESTATION) {
      if (lockedTarget !== undefined)
        throw new Error("Locked timeout target is already attested.");
      continue;
    }
    const deadline = node.header.endTime + DA_ATTESTATION_TIMEOUT_MS;
    if (deadline <= nowMs) return { target, deadline };
    if (lockedTarget !== undefined)
      throw new Error("Locked timeout target has not reached its deadline.");
    waiting ??= { target, deadline };
  }
  if (lockedTarget !== undefined)
    throw new Error(
      "Locked timeout target is absent from the canonical queue.",
    );
  return waiting;
};

/** Reopen reverted effects without replacing their retained signed attempts. */
export const reopenRolledBackTimeoutCorrectionSteps = (
  journal: TimeoutCorrectionJournal,
  queue: readonly StateQueueUTxO[],
): TimeoutCorrectionJournal => {
  const liveInputs = new Set(outRefsOf(queue));
  const liveHeaders = new Set(queue.slice(1).map(headerHashOf));
  let reopened = false;
  const steps = journal.steps.map((step) => {
    if (
      (step.status !== "confirmed" && step.status !== "superseded") ||
      (!liveHeaders.has(step.removedHeaderHash) &&
        !step.inputOutRefs.some((input) => liveInputs.has(input)))
    )
      return step;
    reopened = true;
    return { ...step, status: "prepared" as const };
  });
  return reopened ? { ...journal, completed: false, steps } : journal;
};

/** A rollback reopens the objective; it does not retire previously signed attempts. */
export const reconcileCompletedTimeoutCorrectionJournal = (
  journal: TimeoutCorrectionJournal,
  queue: readonly StateQueueUTxO[],
): TimeoutCorrectionJournal | undefined => {
  const reconciled = reopenRolledBackTimeoutCorrectionSteps(journal, queue);
  if (!reconciled.completed) return reconciled;
  if (
    queue.some(
      (node, index) =>
        index > 0 && headerHashOf(node) === journal.targetHeaderHash,
    )
  )
    return { ...reconciled, completed: false };
  return queue.length === 1 ? reconciled : undefined;
};

export const hasCompetingCorrection = (
  lock: CorrectionLockDatum,
  target?: string,
): boolean =>
  lock !== "Idle" &&
  (lock.Locked.correction_identity !== "AttestationTimeout" ||
    (target !== undefined && lock.Locked.target_header_hash !== target));

export const pendingTimeoutCorrection = (
  journal?: TimeoutCorrectionJournal,
): SubmitUnattestedTimeoutCorrectionResult => ({
  status: "pending",
  targetHeaderHash: journal?.targetHeaderHash ?? null,
  deadlineMs: journal?.targetDeadlineMs ?? null,
  pendingTxHash:
    journal?.steps.find(
      (step) => step.status === "prepared" || step.status === "submitted",
    )?.txHash ?? null,
  submittedTxHashes: journal?.steps.map((step) => step.txHash) ?? [],
  removedHeaderHashes:
    journal?.steps
      .filter((step) => step.status === "confirmed")
      .map((step) => step.removedHeaderHash) ?? [],
});

export const releaseTimeoutCorrectionLeaseBeforeYield = async (
  lease: { readonly release: () => Promise<void> } | undefined,
): Promise<boolean> => {
  if (lease === undefined) {
    return false;
  }
  await lease.release();
  return true;
};

export const requireDeploymentScript = (
  deploymentInfo: ContractDeploymentInfo,
  name:
    | "correctionLockSpend"
    | "stateQueueSpend"
    | "stateQueueMint"
    | "stateQueueUnattestedTimeoutWithdraw",
): Script => {
  const entry = deploymentInfo[name];
  if (entry?.contract === undefined) {
    throw new Error(
      `Deployment info entry "${name}" is missing contract CBOR.`,
    );
  }
  const script = {
    type: entry.contract.type,
    script: entry.contract.cborHex,
  } as Script;
  requireMatchingScriptHash({
    label: `${name} script`,
    deployed: entry.scriptHash,
    derived: validatorToScriptHash(script),
  });
  return script;
};

export type TimeoutCorrectionRecovery = Readonly<{
  observeSignedTransaction(
    input: SignedWorkflowTransaction,
  ): Promise<SignedTransactionRecoveryObservation>;
  rebroadcastSignedTransaction(
    input: SignedWorkflowTransaction & {
      readonly authorizeResubmission: (
        input: SignedWorkflowTransaction,
      ) => Promise<void>;
    },
  ): Promise<string>;
}>;

export type SubmitUnattestedTimeoutCorrectionResult = {
  readonly status: "empty" | "not-ready" | "pending" | "complete";
  readonly targetHeaderHash: string | null;
  readonly deadlineMs: string | null;
  readonly pendingTxHash: string | null;
  readonly submittedTxHashes: readonly string[];
  readonly removedHeaderHashes: readonly string[];
};
