import { type EventHistoryWitness } from "./history-query.js";

/** Explicit deployment/inclusion assumptions, never inferred from a retry count.
 * Submission includes construction, signing, propagation and L1 inclusion.
 * Remaining proof time includes output visibility and every subsequent stage. */
export type EventHistoryCaptureTiming = {
  readonly visibilityBudgetMs: bigint;
  readonly submissionBudgetMs: bigint;
  readonly remainingProofBudgetMs: bigint;
  readonly slotLengthMs: bigint;
};

const validateTiming = (timing: EventHistoryCaptureTiming) => {
  if (
    timing.visibilityBudgetMs < 0n ||
    timing.submissionBudgetMs <= 0n ||
    timing.remainingProofBudgetMs < 0n ||
    timing.slotLengthMs <= 0n
  ) {
    throw new Error(
      "History capture requires explicit nonnegative timing budgets and a positive submission/slot duration",
    );
  }
};

/** A mutation included at t creates protection until at least t + duration.
 * Given visibility within V and inclusion within S, V + S + one slot must
 * fit strictly inside that protection. This checks the assumed inequality;
 * it does not establish the provider or chain's actual inclusion guarantee. */
export const requireEventHistoryCaptureProtection = (
  protectionDurationMs: bigint,
  timing: EventHistoryCaptureTiming,
): void => {
  validateTiming(timing);
  if (
    protectionDurationMs <=
    timing.visibilityBudgetMs + timing.submissionBudgetMs + timing.slotLengthMs
  ) {
    throw new Error(
      "History protection does not cover the declared visibility and submission budgets",
    );
  }
};

export class EventHistoryCaptureDeadlineError extends Error {
  constructor() {
    super(
      "Insufficient time to capture history and finish the remaining proof before merge",
    );
    this.name = "EventHistoryCaptureDeadlineError";
  }
}

export type EventHistoryCaptureWindow = {
  /** Inclusive lower bound; the header end remains strictly excluded. */
  readonly validFrom: bigint;
  /** Exclusive upper bound for Lucid, with one slot of rounding slack. */
  readonly validTo: bigint;
  /** False permits an optimistic attempt at an old, stable node. A conflicting
   * mutation must yield a fresh protected witness before claiming liveness. */
  readonly protected: boolean;
};

export const eventHistoryCaptureWindow = ({
  now,
  headerEnd,
  mergeDeadline,
  protectedUntil,
  timing,
}: {
  readonly now: bigint;
  readonly headerEnd: bigint;
  readonly mergeDeadline: bigint;
  readonly protectedUntil: bigint;
  readonly timing: EventHistoryCaptureTiming;
}): EventHistoryCaptureWindow => {
  validateTiming(timing);
  if (
    now < headerEnd + timing.slotLengthMs ||
    headerEnd < 0n ||
    mergeDeadline <= headerEnd ||
    protectedUntil < 0n
  )
    throw new Error(
      "History capture must start after a valid accused interval",
    );
  const validTo = now + timing.submissionBudgetMs + timing.slotLengthMs;
  if (validTo + timing.remainingProofBudgetMs >= mergeDeadline)
    throw new EventHistoryCaptureDeadlineError();
  return {
    validFrom:
      now - 60_000n > headerEnd + timing.slotLengthMs
        ? now - 60_000n
        : headerEnd + timing.slotLengthMs,
    validTo,
    protected: validTo <= protectedUntil,
  };
};

/** Retry only an authoritative reference-input conflict. A transport timeout,
 * ambiguous submission or confirmed-but-not-visible output must be reconciled
 * by the submitter, never mapped to ReferenceConflict. Other failures propagate.
 * fetch must use current L1 authority, including after a rollback. */
export const captureEventHistoryWithRetry = async <T>({
  fetch,
  submit,
  now,
  headerEnd,
  mergeDeadline,
  timing,
  maxAttempts,
  onConflict,
}: {
  readonly fetch: () => Promise<EventHistoryWitness>;
  readonly submit: (
    witness: EventHistoryWitness,
    window: EventHistoryCaptureWindow,
    attempt: number,
  ) => Promise<
    | { readonly kind: "Captured"; readonly value: T }
    | { readonly kind: "ReferenceConflict" }
  >;
  readonly now: () => bigint;
  readonly headerEnd: bigint;
  readonly mergeDeadline: bigint;
  readonly timing: EventHistoryCaptureTiming;
  readonly maxAttempts: number;
  readonly onConflict?: (
    witness: EventHistoryWitness,
    attempt: number,
  ) => Promise<void>;
}): Promise<T> => {
  if (!Number.isSafeInteger(maxAttempts) || maxAttempts < 1)
    throw new Error(
      "History capture requires a positive bounded attempt count",
    );
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    // Check before fetching as well as after potentially slow provider reads.
    eventHistoryCaptureWindow({
      now: now(),
      headerEnd,
      mergeDeadline,
      protectedUntil: 0n,
      timing,
    });
    const witness = await fetch();
    const window = eventHistoryCaptureWindow({
      now: now(),
      headerEnd,
      mergeDeadline,
      protectedUntil: witness.anchor.node.protected_until,
      timing,
    });
    const result = await submit(witness, window, attempt);
    if (result.kind === "Captured") return result.value;
    if (attempt < maxAttempts) await onConflict?.(witness, attempt);
  }
  throw new Error(
    "History capture exhausted its reference-conflict attempts; refresh/reconcile before resuming",
  );
};
