import {
  CML,
  Data,
  datumToHash,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import { CredentialD, Value } from "../common.js";
import { MAX_VALIDITY_RANGE_LENGTH_MS } from "../protocol-parameters.js";
import { assetsToValue } from "../reserve-payout/assets.js";
import {
  buildEventHistoryAdmission,
  buildEventHistoryPublication,
  type EventHistoryAdmission,
  type EventHistoryBuildContext,
  type EventHistoryPayloadInput,
  EventHistoryPredecessorConflictError,
  EventHistoryPredecessorProtectedError,
} from "./history-build.js";
import {
  prepareEventHistoryPayload,
  prepareEventHistoryPayloadCbor,
} from "./history-payload.js";

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
  /** Retain the completed publication for exact-body recovery after rollback. */
  readonly publicationAttempt?: EventHistorySubmissionAttempt;
  readonly admission?: EventHistorySubmissionAttempt;
  readonly pending?: EventHistorySubmissionAttempt;
};

/** InputConflict means a definitive ledger rejection, never a timeout or an
 * absent provider output. A confirmed transaction whose outputs are not visible
 * is still Confirmed. The driver polls the specific publication separately. */
export type EventHistorySubmissionOutcome =
  | { readonly kind: "Confirmed" }
  | { readonly kind: "InputConflict" }
  | { readonly kind: "Pending" };

export class EventHistorySubmissionPendingError extends Error {
  constructor(
    readonly checkpoint: EventHistorySubmissionCheckpoint,
    message: string,
    readonly cause?: unknown,
  ) {
    super(message);
    this.name = "EventHistorySubmissionPendingError";
  }
}

export type EventHistorySubmissionDriver = {
  /** Must durably replace the checkpoint before resolving. */
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
  readonly funding: () => Promise<readonly UTxO[]>;
  readonly now: () => number;
  readonly waitUntil: (unixTimeMs: number) => Promise<void>;
};

export type EventHistorySubmissionRequest = Omit<
  EventHistoryAdmission,
  "validFrom" | "validTo" | "externalData" | "payload" | "payloadCbor"
> &
  EventHistoryPayloadInput;

export const eventHistorySubmissionRequestHash = (
  policyId: string,
  request: EventHistorySubmissionRequest,
  recipe: EventHistoryBuildContext["recipe"],
): string => {
  if (request.payload !== undefined && request.payloadCbor !== undefined)
    throw new Error("History payload must have exactly one encoding source");
  const plan =
    request.payloadCbor === undefined
      ? prepareEventHistoryPayload(request.payload, request.reclaimAuth, recipe)
      : prepareEventHistoryPayloadCbor(
          request.payloadCbor,
          request.reclaimAuth,
          recipe,
        );
  const fields = [
    Data.to(policyId),
    plan.payloadCbor,
    Data.to(request.reclaimAuth, CredentialD),
    Data.to(assetsToValue(request.assets), Value),
    Data.to(request.structuralLovelace),
    Data.to(request.structuralRefundKey),
  ];
  const list = CML.PlutusData.from_cbor_hex(Data.to([])).as_list()!;
  for (const field of fields) list.add(CML.PlutusData.from_cbor_hex(field));
  return datumToHash(CML.PlutusData.new_list(list).to_cbor_hex());
};

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
    validityDurationMs <= 60_000 ||
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
  const checkTime = () => {
    const now = driver.now();
    if (!Number.isSafeInteger(now) || now < 60_000 || now >= deadlineMs)
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "History submission deadline reached or clock invalid",
      );
    return now;
  };
  const waitUntil = async (target: number) => {
    if (!Number.isSafeInteger(target) || target >= deadlineMs)
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "History submission cannot wait past its deadline",
      );
    await driver.waitUntil(target);
    checkTime();
  };
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
        ? settled
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
  // before admission, even when an earlier run saved a confirmation receipt.
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
    if (outcome.kind !== "Confirmed")
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "Original history publication confirmation is unresolved",
      );
  }
  // A stored confirmation is a receipt, not current L1 authority. Reconcile it
  // on resume too, so a rollback cannot turn a local checkpoint into evidence.
  if (checkpoint.admission !== undefined) {
    const { admission, ...rest } = checkpoint;
    await save({ ...rest, pending: admission });
  }
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
    await save({ ...checkpoint, pending: attempt });
    return resolve(attempt, () => driver.submit(tx, attempt));
  };
  const requireNonce = async () => {
    if ((await context.lucid.utxosByOutRef([request.nonce])).length !== 1)
      throw new EventHistorySubmissionPendingError(
        checkpoint,
        "History nonce is unavailable; reconcile its spending transaction",
      );
  };
  if (plan.kind === "External" && checkpoint.publication === undefined) {
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      checkTime();
      await requireNonce();
      const built = await buildEventHistoryPublication(
        { ...context, fundingInputs: await driver.funding() },
        plan.payloadCbor,
        request.reclaimAuth,
      );
      const outcome = await broadcast(
        "Publication",
        built.tx,
        built.publicationOutputIndex,
      );
      if (outcome.kind === "Confirmed") break;
      if (attempt < maxAttempts) await waitUntil(checkTime() + retryDelayMs);
    }
    if (checkpoint.publication === undefined)
      throw new EventHistorySubmissionPendingError(
        checkpoint,
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
    let built: Awaited<ReturnType<typeof buildEventHistoryAdmission>>;
    try {
      built = await buildEventHistoryAdmission(
        { ...context, fundingInputs: await driver.funding() },
        {
          ...request,
          externalData,
          validFrom: now - 60_000,
          validTo: now - 60_000 + validityDurationMs,
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
      if (attempt === maxAttempts) break;
      // Round up the protection timestamp to a ledger slot, retaining the
      // production lower-bound backoff when attempting the mutation again.
      const protectedTime = Number(cause.protectedUntil);
      const slot = context.lucid.unixTimeToSlot(protectedTime);
      const atSlot = context.lucid.slotToUnixTime(slot);
      await waitUntil(
        (atSlot < protectedTime
          ? context.lucid.slotToUnixTime(slot + 1)
          : atSlot) + 60_000,
      );
      continue;
    }
    const outcome = await broadcast(
      "Admission",
      built.tx,
      built.orderOutputIndex,
    );
    if (outcome.kind === "Confirmed" && checkpoint.admission !== undefined)
      return { ...checkpoint, admission: checkpoint.admission };
    if (attempt < maxAttempts) await waitUntil(checkTime() + retryDelayMs);
  }
  throw new EventHistorySubmissionPendingError(
    checkpoint,
    "History admission exhausted its protection/input-conflict attempts",
  );
};
