/**
 * A header removed against a record this run did not create leaves nothing
 * of the run to prove, but a computation thread of ours on that header may
 * still hold funds on L1. Before the run may resolve, it reclaims each such
 * thread to our wallet through the family's cancel: a journaled move like
 * any other, which burns the thread token and pays its value back to us.
 * The reclaim returns our own deposit. It is never a reward or a conviction.
 */
import { WorkflowFundingShortfallError } from "./funding-reservation-permit.workflow-funding-shortfall.js";
import type { FraudProofWorkflowJournalEntry } from "./journal.js";
import type { FraudProofWorkflowNamedState } from "./orchestrator.fraud-proof-workflow-run-result.js";

/** The `actionKind` of a reclaim action's input. */
export const RECLAIM_THREAD_ACTION_KIND = "reclaim_thread" as const;

/** The reason prefix of a reclaim our wallet cannot fund. */
export const FRAUD_PROOF_THREAD_RECLAIM_UNFUNDED =
  "fraud-proof-thread-reclaim-unfunded" as const;

/** The reason prefix of a thread of ours that stands on a removed header and
 * that this run cannot reclaim. */
export const FRAUD_PROOF_THREAD_RECLAIM_UNAVAILABLE =
  "fraud-proof-thread-reclaim-unavailable" as const;

const NAMED_STATES = [
  FRAUD_PROOF_THREAD_RECLAIM_UNFUNDED,
  FRAUD_PROOF_THREAD_RECLAIM_UNAVAILABLE,
] as const;

/** The operator's alarm a pending reason names, if it is a reclaim hold. */
export const threadReclaimNamedState = (
  reason: string,
): FraudProofWorkflowNamedState | undefined => {
  const name = NAMED_STATES.find((candidate) =>
    reason.startsWith(`${candidate}: `),
  );
  return name === undefined ? undefined : Object.freeze({ name, alarm: true });
};

/** The pending reason of a thread of ours this run cannot reclaim. */
export const threadReclaimUnavailableReason = (
  threadOutRef: string,
  detail: string,
): string =>
  `${FRAUD_PROOF_THREAD_RECLAIM_UNAVAILABLE}: our thread ${threadOutRef} still holds funds on the removed header and cannot be reclaimed: ${detail}`;

/**
 * A reclaim that does not build for want of our wallet's ada. The header is
 * already gone, so no deadline runs against the thread: it waits for a
 * top-up and is tried again at each observation.
 */
export class FraudProofThreadReclaimUnfundedError extends WorkflowFundingShortfallError {
  constructor(
    input: Readonly<{ actionId: string; threadOutRef: string; cause: unknown }>,
  ) {
    super(input.actionId, input.cause);
    this.name = "FraudProofThreadReclaimUnfundedError";
    this.message = `${FRAUD_PROOF_THREAD_RECLAIM_UNFUNDED}: the prover wallet cannot fund reclaiming our thread ${input.threadOutRef}; the header is already removed, so no deadline applies; top up the wallet's plain-ADA coins: ${this.message}`;
  }
}

/** One thread of ours that this run reclaimed to our wallet. */
export type FraudProofThreadReclaim = Readonly<{
  actionId: string;
  txHash: string;
  threadOutRef: string;
  /** The lovelace the thread held, returned to our wallet less the fee. */
  lovelace: string;
}>;

/** The input of a reclaim action, as the journal holds it. */
export type FraudProofThreadReclaimInput = Readonly<{
  actionKind: typeof RECLAIM_THREAD_ACTION_KIND;
  threadOutRef: string;
  step: number;
  lovelace: string;
}>;

export const isThreadReclaimInput = (
  input: Readonly<Record<string, unknown>>,
): input is FraudProofThreadReclaimInput =>
  input.actionKind === RECLAIM_THREAD_ACTION_KIND &&
  typeof input.threadOutRef === "string" &&
  typeof input.step === "number" &&
  typeof input.lovelace === "string";

/** The action that reclaims `thread`. */
export const threadReclaimAction = (
  thread: Readonly<{ threadOutRef: string; step: number; lovelace: string }>,
) =>
  Object.freeze({
    actionId: `reclaim:${thread.threadOutRef}`,
    input: Object.freeze({
      actionKind: RECLAIM_THREAD_ACTION_KIND,
      threadOutRef: thread.threadOutRef,
      step: thread.step,
      lovelace: thread.lovelace,
    }),
  });

/** The transaction hashes of this journal's reclaim intents. */
export const threadReclaimIntents = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): ReadonlySet<string> =>
  new Set(
    entries.flatMap(({ event }) =>
      event.kind === "submission_intent" &&
      isThreadReclaimInput(event.actionInput)
        ? [event.txHash]
        : [],
    ),
  );

/** Every reclaim this journal confirms, in journal order. */
export const reclaimedThreads = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): readonly FraudProofThreadReclaim[] => {
  const intents = new Map<string, FraudProofThreadReclaimInput>();
  let reclaimed: FraudProofThreadReclaim[] = [];
  for (const { event } of entries) {
    if (
      event.kind === "submission_intent" &&
      isThreadReclaimInput(event.actionInput)
    )
      intents.set(event.txHash, event.actionInput);
    // A rollback reopened the reclaim: it counts again once reconfirmed.
    if (event.kind === "reobserved")
      reclaimed = reclaimed.filter(({ txHash }) => txHash !== event.txHash);
    if (event.kind !== "confirmed") continue;
    const input = intents.get(event.txHash);
    if (
      input === undefined ||
      reclaimed.some(({ txHash }) => txHash === event.txHash)
    )
      continue;
    reclaimed.push(
      Object.freeze({
        actionId: event.actionId,
        txHash: event.txHash,
        threadOutRef: input.threadOutRef,
        lovelace: input.lovelace,
      }),
    );
  }
  return Object.freeze(reclaimed);
};
