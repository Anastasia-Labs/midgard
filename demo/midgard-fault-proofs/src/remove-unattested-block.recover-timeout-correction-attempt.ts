import {
  slotAlignedLowerBoundAtOrAfter,
  type SlotClock,
  type StateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Network,
  type TxSignBuilder,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
  STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
  type StateQueueMutationLeaseCoordinator,
} from "./remove-fraudulent-block.js";
import {
  type TimeoutCorrectionJournal,
  type TimeoutCorrectionJournalStore,
  type TimeoutCorrectionStepReconciliation,
} from "./remove-unattested-block.parse-timeout-correction-journal.js";
import {
  reconcileLastTimeoutCorrectionStep,
  type TimeoutCorrectionAttemptObservation,
  timeoutCorrectionAttemptStatus,
  type TimeoutCorrectionRecovery,
} from "./remove-unattested-block.reconcile-last-timeout-correction-step.js";
import { type ResolvedProverSigner } from "./runtime.js";
import {
  inspectSignedWorkflowTransaction,
  type SignedWorkflowTransaction,
} from "./workflow/signed-transaction-reconciliation.js";
import { type SupersededAttemptReadSchedule } from "./workflow/superseded-attempt-read-schedule.js";

/**
 * Reconciles the sole unresolved attempt from one observation of it. Nothing
 * here resubmits: the node's intent reconciler resends a live journaled
 * attempt, and a CLI run has no reconciler (see `submitUnattestedTimeoutCorrection`).
 */
export const recoverTimeoutCorrectionAttempt = async (input: {
  readonly journal: TimeoutCorrectionJournal;
  readonly queue: readonly StateQueueUTxO[];
  readonly observe: (
    signed: SignedWorkflowTransaction,
  ) => Promise<TimeoutCorrectionAttemptObservation>;
}): Promise<TimeoutCorrectionStepReconciliation> => {
  const step = input.journal.steps.find(
    (entry) => entry.status === "prepared" || entry.status === "submitted",
  );
  if (step === undefined)
    return { disposition: "none", journal: input.journal };
  const signed = {
    transactionHash: step.txHash,
    signedTransactionCborHex: step.signedCbor,
  };
  inspectSignedWorkflowTransaction(signed);
  // An unreadable source decides nothing: the attempt stays pending.
  const observed = await input.observe(signed).catch(
    (cause: unknown): TimeoutCorrectionAttemptObservation => ({
      status: "unknown",
      final: false,
      canonicalPoint: null,
      releaseFinalPoint: null,
      reason: `observation failed: ${String(cause)}`,
    }),
  );
  return reconcileLastTimeoutCorrectionStep(
    input.journal,
    input.queue,
    timeoutCorrectionAttemptStatus(observed),
  );
};

/**
 * A CLI run's observation of its attempts: it has no follower, so no
 * recovery reader. An attempt the provider reports confirmed is included;
 * one this run submitted is pending until its validity passes by the clock;
 * any other is abandoned, so its replacement shares its inputs and the
 * ledger refuses that replacement while the attempt is in flight.
 */
export const unjournaledTimeoutCorrectionObserver =
  (
    lucid: LucidEvolution,
    nowMs: () => number,
    submittedThisRun: ReadonlySet<string>,
  ) =>
  async (
    signed: SignedWorkflowTransaction,
  ): Promise<TimeoutCorrectionAttemptObservation> => {
    const unread = {
      final: false,
      canonicalPoint: null,
      releaseFinalPoint: null,
    } as const;
    const { status } = await lucid
      .transactionStatus(signed.transactionHash)
      .catch(() => ({ status: "not_found" as const }));
    if (status === "confirmed")
      return { ...unread, status: "included", reason: "provider confirmed" };
    const { expiresAtSlot } = inspectSignedWorkflowTransaction(signed);
    return submittedThisRun.has(signed.transactionHash) &&
      expiresAtSlot !== undefined &&
      nowMs() < lucid.slotToUnixTime(Number(expiresAtSlot))
      ? { ...unread, status: "pending", reason: "submitted by this run" }
      : { ...unread, status: "abandoned", reason: "no recovery reader" };
  };

/**
 * A CLI run (no follower, so its submissions are unjournaled: `no_follower`)
 * submitted a correction whose inputs the ledger already holds spent: an
 * earlier attempt, ours or another actor's, is still in flight or has just
 * landed. Nothing was sent. Run the command again once it settles.
 */
export class TimeoutCorrectionAttemptInFlightError extends Error {
  readonly txHash: string;
  constructor(txHash: string, options?: { readonly cause?: unknown }) {
    super(
      `Timeout correction ${txHash} was refused: its inputs are already spent, so an earlier attempt is still in flight or has just landed. Run the command again once it settles.`,
      options,
    );
    this.name = "TimeoutCorrectionAttemptInFlightError";
    this.txHash = txHash;
  }
}

const SPENT_INPUT_REJECTION =
  /BadInputsUTxO|UnknownInput|unknownOutputReferences|JSON-RPC error 3117\b|"code":\s*3117\b|does not exist or was already spent/u;

/**
 * The ledger refused a submission because an input is spent or unknown
 * (Ogmios 3117 / `BadInputsUTxO`, Blockfrost's ledger text, the emulator's
 * "already spent"), searched through the error's message, cause chain and
 * structured fields.
 */
export const isSpentInputSubmitRejection = (error: unknown): boolean => {
  const seen = new Set<unknown>();
  const search = (value: unknown): boolean => {
    if (typeof value === "string") return SPENT_INPUT_REJECTION.test(value);
    if (typeof value !== "object" || value === null || seen.has(value))
      return false;
    seen.add(value);
    if (
      value instanceof Error &&
      (search(value.message) || search(value.cause))
    )
      return true;
    const record = value as Record<string, unknown>;
    if ("unknownOutputReferences" in record || "badInputs" in record)
      return true;
    return Object.values(record).some(search);
  };
  return search(error);
};

/**
 * The wallet a timeout correction is funded and signed from: the inputs its
 * coin selection may spend (`presetWalletInputs`, read just before the
 * build) and its signing.
 */
export type TimeoutCorrectionWallet = {
  readonly utxos: () => Promise<UTxO[]>;
  readonly sign: (unsigned: TxSignBuilder) => Promise<TxSigned>;
};

/**
 * `wallet`, or where none is given (a CLI run) the provider's UTxOs at the
 * selected wallet's address and the selected wallet's own signature.
 */
export const timeoutCorrectionWallet = (
  lucid: LucidEvolution,
  wallet: TimeoutCorrectionWallet | undefined,
): TimeoutCorrectionWallet =>
  wallet ?? {
    utxos: async () => lucid.utxosAt(await lucid.wallet().address()),
    sign: (unsigned) => unsigned.sign.withWallet().complete(),
  };

export type SubmitUnattestedTimeoutCorrectionParams = {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly journalStore: TimeoutCorrectionJournalStore;
  readonly awaitConfirmation?: boolean;
  readonly nowMs?: () => number;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
  /**
   * The node's read of its retained attempts, from its intent journal and
   * follower facts. Without one (a CLI run), an attempt is confirmed by the
   * provider's transaction status, waited on only while this run submitted
   * it and its validity has not passed, and otherwise abandoned: a
   * replacement then shares its inputs, and the ledger refuses that
   * replacement while the attempt is still in flight
   * (`TimeoutCorrectionAttemptInFlightError`).
   */
  readonly recovery?: TimeoutCorrectionRecovery;
  /** Bounds re-reads of abandoned attempts; process-wide by default. */
  readonly attemptReadSchedule?: SupersededAttemptReadSchedule;
  /**
   * The node's wallet view (§8.5) and its signing over it. Without one (a
   * CLI run), the provider's UTxOs at the selected wallet's address, and the
   * selected wallet's own signature. Nothing is pinned on `lucid` either way.
   */
  readonly wallet?: TimeoutCorrectionWallet;
};

/**
 * The validity range of a timeout-correction transaction built at `nowMs`.
 *
 * The validator requires `inclusive lower bound >= header.end_time +
 * da_attestation_timeout`. Block end times usually end in 999 ms, and the
 * ledger presents the lower bound as the start of its slot, so the deadline
 * itself is rounded up to the next slot boundary before it is used as the
 * earliest admissible lower bound. The lower bound is otherwise backdated to
 * tolerate submit latency and provider clock skew.
 */
export const resolveTimeoutCorrectionValidityRange = (
  slotClock: SlotClock,
  deadlineMs: bigint,
  nowMs: bigint,
): { readonly validFrom: bigint; readonly validTo: bigint } => {
  const earliestAdmitted = slotAlignedLowerBoundAtOrAfter(
    slotClock,
    deadlineMs,
  );
  const backdated = nowMs - STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS;
  const validFrom = backdated > earliestAdmitted ? backdated : earliestAdmitted;
  return {
    validFrom,
    validTo: validFrom + STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
  };
};
