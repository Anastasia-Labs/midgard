import { type TxSignBuilder, type UTxO } from "@lucid-evolution/lucid";

import { MAX_VALIDITY_RANGE_LENGTH_MS } from "../protocol-parameters.js";
import {
  buildEventHistoryAdmission,
  buildEventHistoryPublication,
  type EventHistoryBuildContext,
  EventHistoryPredecessorConflictError,
  EventHistoryPredecessorProtectedError,
} from "./history-build.js";
import {
  prepareEventHistoryPayload,
  prepareEventHistoryPayloadCbor,
} from "./history-payload.js";
import {
  abandonAttempts,
  abandonPublicationReceipt,
  adoptLandedAbandonedAdmission,
  adoptLandedAbandonedPublication,
  type EventHistoryAbandonedAttempt,
} from "./history-submit-abandoned.js";
import {
  type EventHistorySubmissionRequest,
  eventHistorySubmissionRequestHash,
} from "./history-submit-request.js";

export {
  type EventHistorySubmissionRequest,
  eventHistorySubmissionRequestHash,
} from "./history-submit-request.js";

type OutRef = Pick<UTxO, "txHash" | "outputIndex">;
export type EventHistorySubmissionAttempt = OutRef & {
  readonly phase: "Publication" | "Admission";
  /** Completed unsigned body/witnesses, including validity bounds, for restart
   * reconciliation. Signing must preserve this body's hash. */
  readonly transactionCbor: string;
};

/** Persist before broadcasting. A restart reconciles the exact pending hash
 * before building anything else. Publication is retained across admission retries. */
export type EventHistorySubmissionCheckpoint = {
  readonly requestHash: string;
  readonly publication?: OutRef;
  /** Retain the completed publication for exact-body recovery after a
   * rollback, until its validity ends unseen and it is abandoned. */
  readonly publicationAttempt?: EventHistorySubmissionAttempt;
  readonly admission?: EventHistorySubmissionAttempt;
  readonly pending?: EventHistorySubmissionAttempt;
  /** Attempts settled as InputConflict. They hold no input reservations, and
   * each is kept while a rollback could still land it (see
   * adoptLandedAbandonedAdmission). */
  readonly abandoned?: readonly EventHistoryAbandonedAttempt[];
};

/** InputConflict means the attempt cannot land on the current chain: a
 * definitive ledger rejection, or a validity interval that ended below a
 * chain point the provider indexed without seeing it. Never a confirmation
 * timeout or an absent provider output alone. A confirmed transaction whose
 * outputs are not visible is still Confirmed. The driver polls the specific
 * publication separately. */
export type EventHistorySubmissionOutcome =
  | { readonly kind: "Confirmed" }
  | { readonly kind: "InputConflict" }
  | { readonly kind: "Pending" };

export class EventHistorySubmissionPendingError extends Error {
  constructor(
    readonly checkpoint: EventHistorySubmissionCheckpoint,
    message: string,
    readonly cause?: unknown,
    /** Set only when the submission stopped on time or predecessor
     * protection with no transaction in flight: the earliest time a rerun
     * from this checkpoint can make progress. */
    readonly resumeAfterMs?: number,
  ) {
    super(message);
    this.name = "EventHistorySubmissionPendingError";
  }
}

/** Thrown by a driver's `save` when it recorded nothing because another local
 * submission already holds one of the pending attempt's inputs. The attempt
 * was neither persisted nor broadcast, so the workflow rebuilds against the
 * current chain, as after an L1 input conflict. */
export class EventHistoryInputReservedError extends Error {
  constructor(readonly outRef: string) {
    super(`History transaction input ${outRef} is held by another submission`);
    this.name = "EventHistoryInputReservedError";
  }
}

export type EventHistorySubmissionDriver = {
  /** Must durably replace the checkpoint before resolving. When the new
   * checkpoint carries a pending attempt whose inputs another local
   * submission holds, it must write nothing and throw
   * EventHistoryInputReservedError; that is the only error the workflow
   * retries. Any other failure stops it for reconciliation. */
  readonly save: (
    checkpoint: EventHistorySubmissionCheckpoint,
  ) => Promise<void>;
  /** Signs the supplied completed body without changing it, broadcasts it and
   * confirms its exact hash. Wallet/output caches must be reconciled on success. */
  readonly submit: (
    tx: TxSignBuilder,
    attempt: EventHistorySubmissionAttempt,
  ) => Promise<EventHistorySubmissionOutcome>;
  readonly reconcile: (
    attempt: EventHistorySubmissionAttempt,
  ) => Promise<EventHistorySubmissionOutcome>;
  /** Reads the exact hash's L1 status without broadcasting. Without it, an
   * abandoned admission that a rollback lands is never adopted. */
  readonly observe?: (
    attempt: EventHistorySubmissionAttempt,
  ) => Promise<EventHistorySubmissionOutcome>;
  readonly funding: () => Promise<readonly UTxO[]>;
  readonly now: () => number;
  readonly waitUntil: (unixTimeMs: number) => Promise<void>;
};

/** Admission lower bounds trail the local clock by this backoff, so a
 * protected predecessor is retried this long after its protection ends. */
const ADMISSION_LOWER_BOUND_BACKOFF_MS = 60_000;

/** Longest predecessor-protection wait one admission attempt can meet. Any
 * list mutation may use the maximum validity range, and its nodes stay
 * protected for the recipe duration after that upper bound. A submission
 * deadline must add this to its own publication and confirmation budget. */
export const eventHistoryProtectionWaitBoundMs = (
  recipe: Pick<EventHistoryBuildContext["recipe"], "protectionDurationMs">,
): number =>
  Number(MAX_VALIDITY_RANGE_LENGTH_MS + recipe.protectionDurationMs) +
  ADMISSION_LOWER_BOUND_BACKOFF_MS;

/** Both event kinds use the same publication/admission state machine. It never
 * changes the nonce, original funds, payload or reclaim authorization on retry.
 * Only authoritative input rejection permits another broadcast. */
export const submitEventHistory = async ({
  context,
  request,
  driver,
  checkpoint: resumed,
  maxAttempts,
  deadlineMs,
  validityDurationMs,
  outputVisibilityAttempts,
  retryDelayMs,
}: {
  readonly context: Omit<EventHistoryBuildContext, "fundingInputs">;
  readonly request: EventHistorySubmissionRequest;
  readonly driver: EventHistorySubmissionDriver;
  readonly checkpoint?: EventHistorySubmissionCheckpoint;
  readonly maxAttempts: number;
  readonly deadlineMs: number;
  readonly validityDurationMs: number;
  readonly outputVisibilityAttempts: number;
  readonly retryDelayMs: number;
}): Promise<
  EventHistorySubmissionCheckpoint & { readonly admission: OutRef }
> => {
  if (
    ![
      maxAttempts,
      deadlineMs,
      validityDurationMs,
      outputVisibilityAttempts,
      retryDelayMs,
    ].every((value) => Number.isSafeInteger(value) && value > 0) ||
    validityDurationMs <= ADMISSION_LOWER_BOUND_BACKOFF_MS ||
    BigInt(validityDurationMs) > MAX_VALIDITY_RANGE_LENGTH_MS
  )
    throw new Error("Invalid bounded history submission timing or attempts");
  const plan =
    request.payloadCbor === undefined
      ? prepareEventHistoryPayload(
          request.payload,
          request.reclaimAuth,
          context.recipe,
        )
      : prepareEventHistoryPayloadCbor(
          request.payloadCbor,
          request.reclaimAuth,
          context.recipe,
        );
  const id =
    "DepositPayload" in plan.payload
      ? plan.payload.DepositPayload.event.id
      : plan.payload.WithdrawalPayload.event.id;
  if (
    "DepositPayload" in plan.payload !== (context.recipe.kind === "Deposit") ||
    id.transactionId !== request.nonce.txHash ||
    id.outputIndex !== BigInt(request.nonce.outputIndex)
  )
    throw new Error("History submission payload kind or nonce does not match");
  const requestHash = eventHistorySubmissionRequestHash(
    context.applied.policyId,
    request,
    context.recipe,
  );
  if (resumed !== undefined && resumed.requestHash !== requestHash)
    throw new Error(
      "History checkpoint belongs to a different request or deployment",
    );
  let checkpoint: EventHistorySubmissionCheckpoint = resumed ?? { requestHash };
  const save = async (next: EventHistorySubmissionCheckpoint) => {
    await driver.save(next);
    checkpoint = next;
  };
  // Every checkTime and waitUntil runs after resolve() settled the pending
  // attempt or before a broadcast saved one, so their deferrals are resumable.
  const checkTime = () => {
    const now = driver.now();
    if (!Number.isSafeInteger(now) || now < 60_000)
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "History submission clock is invalid",
      );
    if (now >= deadlineMs)
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "History submission deadline reached",
        undefined,
        now,
      );
    return now;
  };
  const waitUntil = async (target: number) => {
    if (!Number.isSafeInteger(target) || target >= deadlineMs)
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "History submission cannot wait past its deadline",
        undefined,
        Number.isSafeInteger(target) ? target : undefined,
      );
    await driver.waitUntil(target);
    checkTime();
  };
  const nonceSpent = async () =>
    (await context.lucid.utxosByOutRef([request.nonce])).length !== 1;
  const revive = () =>
    adoptLandedAbandonedAdmission(checkpoint, driver, nonceSpent, save);
  const resolve = async (
    attempt: EventHistorySubmissionAttempt,
    operation: () => Promise<EventHistorySubmissionOutcome>,
  ) => {
    let outcome: EventHistorySubmissionOutcome;
    try {
      outcome = await operation();
    } catch (cause) {
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "Reconcile the pending history transaction before resubmission",
        cause,
      );
    }
    if (outcome.kind === "Pending")
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "History transaction confirmation is unresolved",
      );
    const { pending: _pending, ...settled } = checkpoint;
    const outRef = { txHash: attempt.txHash, outputIndex: attempt.outputIndex };
    await save(
      outcome.kind === "InputConflict"
        ? {
            ...settled,
            abandoned: abandonAttempts(checkpoint, [attempt], driver.now()),
          }
        : {
            ...settled,
            ...(attempt.phase === "Publication"
              ? { publication: outRef, publicationAttempt: attempt }
              : { admission: attempt }),
          },
    );
    return outcome;
  };
  // Publication outputs may disappear in a rollback. Reconcile the exact body
  // before admission, even when an earlier run saved a confirmation receipt;
  // one that can no longer land is abandoned and published again.
  if (checkpoint.publication !== undefined) {
    const publication = checkpoint.publicationAttempt;
    if (
      publication === undefined ||
      publication.phase !== "Publication" ||
      publication.txHash !== checkpoint.publication.txHash ||
      publication.outputIndex !== checkpoint.publication.outputIndex
    )
      throw new Error(
        "History publication checkpoint is missing its exact completed attempt",
      );
    let outcome: EventHistorySubmissionOutcome;
    try {
      outcome = await driver.reconcile(publication);
    } catch (cause) {
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "Reconcile the original history publication",
        cause,
      );
    }
    if (outcome.kind === "InputConflict") {
      // Settle a pending admission first. Rewriting it unchanged could meet
      // another submission's hold on its inputs; once settled it holds none.
      const pending = checkpoint.pending;
      if (pending !== undefined)
        await resolve(pending, () => driver.reconcile(pending));
      await save(abandonPublicationReceipt(checkpoint, driver.now()));
    } else if (outcome.kind !== "Confirmed")
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "Original history publication confirmation is unresolved",
      );
  }
  // A stored confirmation is a receipt, not current L1 authority. Reconcile it
  // on resume too, so a rollback cannot turn a local checkpoint into evidence.
  if (checkpoint.admission !== undefined) {
    const { admission, ...rest } = checkpoint;
    // Another submission's stale-view attempt can hold a receipt input.
    for (;;)
      try {
        await save({ ...rest, pending: admission });
        break;
      } catch (cause) {
        if (!(cause instanceof EventHistoryInputReservedError)) throw cause;
        await waitUntil(checkTime() + retryDelayMs);
      }
  }
  if (checkpoint.pending?.phase === "Admission") await revive();
  if (checkpoint.pending !== undefined) {
    const pending = checkpoint.pending;
    await resolve(pending, () => driver.reconcile(pending));
  }
  if (checkpoint.admission !== undefined)
    return { ...checkpoint, admission: checkpoint.admission };
  const broadcast = async (
    phase: EventHistorySubmissionAttempt["phase"],
    tx: TxSignBuilder,
    outputIndex: number,
  ) => {
    checkTime();
    const attempt = {
      phase,
      txHash: tx.toHash(),
      outputIndex,
      transactionCbor: tx.toCBOR(),
    };
    // resolve() settled every earlier attempt, so this adds a pending attempt
    // and never replaces one that could still land.
    try {
      await save({ ...checkpoint, pending: attempt });
    } catch (cause) {
      // Another local submission is spending an input, so this body would
      // meet an L1 input conflict. Nothing is in flight; the caller rebuilds.
      if (cause instanceof EventHistoryInputReservedError)
        return { kind: "InputReserved" } as const;
      throw cause;
    }
    return resolve(attempt, () => driver.submit(tx, attempt));
  };
  // Every loop below settles its broadcasts before exhausting its attempts,
  // so nothing is in flight and a rerun can continue at once.
  const exhausted = (message: string) =>
    new EventHistorySubmissionPendingError(
      checkpoint,
      message,
      undefined,
      checkTime(),
    );
  const requireNonce = async () => {
    if ((await nonceSpent()) && !(await revive()))
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "History nonce is unavailable; reconcile its spending transaction",
      );
  };
  if (plan.kind === "External" && checkpoint.publication === undefined) {
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      const now = checkTime();
      await requireNonce();
      // An abandoned admission (adopted by requireNonce) or publication that
      // a rollback landed replaces publishing again.
      if (checkpoint.admission !== undefined)
        return { ...checkpoint, admission: checkpoint.admission };
      if (await adoptLandedAbandonedPublication(checkpoint, driver, save))
        break;
      const built = await buildEventHistoryPublication(
        { ...context, fundingInputs: await driver.funding() },
        plan.payloadCbor,
        request.reclaimAuth,
        // An admission's upper bound: a holder that dies with this attempt
        // pending releases its funding once the bound passes.
        now - ADMISSION_LOWER_BOUND_BACKOFF_MS + validityDurationMs,
      );
      const outcome = await broadcast(
        "Publication",
        built.tx,
        built.publicationOutputIndex,
      );
      if (outcome.kind === "Confirmed") break;
      // A local reservation lasts until its holder settles on L1; only the
      // deadline bounds waiting for that, not the rebuild attempts.
      if (outcome.kind === "InputReserved") attempt--;
      if (attempt < maxAttempts) await waitUntil(checkTime() + retryDelayMs);
    }
    if (checkpoint.publication === undefined)
      throw exhausted(
        "History publication exhausted its input-conflict attempts",
      );
  }
  let externalData: UTxO | undefined;
  if (checkpoint.publication !== undefined) {
    if (plan.kind !== "External")
      throw new Error("Inline history checkpoint cannot contain a publication");
    for (let attempt = 1; attempt <= outputVisibilityAttempts; attempt++) {
      checkTime();
      [externalData] = await context.lucid.utxosByOutRef([
        checkpoint.publication,
      ]);
      if (externalData !== undefined) break;
      if (attempt < outputVisibilityAttempts)
        await waitUntil(checkTime() + retryDelayMs);
    }
    if (externalData === undefined)
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "Confirmed history publication is not visible; retain its exact out-ref for reconciliation",
      );
  }
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    const now = checkTime();
    await requireNonce();
    if (checkpoint.admission !== undefined)
      return { ...checkpoint, admission: checkpoint.admission };
    let built: Awaited<ReturnType<typeof buildEventHistoryAdmission>>;
    try {
      built = await buildEventHistoryAdmission(
        { ...context, fundingInputs: await driver.funding() },
        {
          ...request,
          externalData,
          validFrom: now - ADMISSION_LOWER_BOUND_BACKOFF_MS,
          validTo: now - ADMISSION_LOWER_BOUND_BACKOFF_MS + validityDurationMs,
        },
      );
    } catch (cause) {
      if (cause instanceof EventHistoryPredecessorConflictError) {
        if (attempt === maxAttempts) break;
        await waitUntil(checkTime() + retryDelayMs);
        continue;
      }
      if (!(cause instanceof EventHistoryPredecessorProtectedError))
        throw cause;
      // Round up the protection timestamp to a ledger slot, retaining the
      // production lower-bound backoff when attempting the mutation again.
      const protectedTime = Number(cause.protectedUntil);
      const slot = context.lucid.unixTimeToSlot(protectedTime);
      const atSlot = context.lucid.slotToUnixTime(slot);
      const retryAt =
        (atSlot < protectedTime
          ? context.lucid.slotToUnixTime(slot + 1)
          : atSlot) + ADMISSION_LOWER_BOUND_BACKOFF_MS;
      if (attempt === maxAttempts)
        throw new EventHistorySubmissionPendingError(
          checkpoint,
          "History admission exhausted its attempts on a protected predecessor",
          cause,
          retryAt,
        );
      await waitUntil(retryAt);
      continue;
    }
    const outcome = await broadcast(
      "Admission",
      built.tx,
      built.orderOutputIndex,
    );
    if (outcome.kind === "Confirmed" && checkpoint.admission !== undefined)
      return { ...checkpoint, admission: checkpoint.admission };
    if (outcome.kind === "InputReserved") attempt--;
    if (attempt < maxAttempts) await waitUntil(checkTime() + retryDelayMs);
  }
  throw exhausted("History admission exhausted its input-conflict attempts");
};
