/**
 * Expiry of a submitted transaction: the outcome a confirmation wait reports
 * when a transaction can no longer land, and the bounded rebuild an
 * operation runs on it.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { type DepthParameters, isSafe } from "@al-ft/midgard-l1-follower/heads";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, type LucidEvolution, type OutRef } from "@lucid-evolution/lucid";
import { Data, Effect, Metric } from "effect";

import { providerViewPoint } from "../l1-provider-view.js";
import { RETRY_ATTEMPTS } from "./utils.parse-structured-outside-validity-interval-details.js";

/**
 * A submitted transaction that did not land before its upper validity bound
 * (`invalid_hereafter`): the bound lies the confirmation depth under the L1
 * tip and every one of its inputs is still unspent (`watchTxExpiry`). Unlike
 * a confirmation timeout this is settled up to a rollback deeper than that
 * depth, so the operation that built it may rebuild from fresh chain state.
 */
export class TxExpiredError extends Data.TaggedError("TxExpiredError")<
  SDK.GenericErrorFields & {
    readonly txHash: string;
    readonly invalidHereafterSlot: number;
    readonly tipSlot: number;
  }
> {}

/** What decides whether signed bytes expired: their upper bound and inputs. */
export type SignedTxExpiryFacts = Readonly<{
  invalidHereafterSlot: number;
  inputs: readonly OutRef[];
}>;

/**
 * The upper bound and inputs of signed bytes; `undefined` when the bytes
 * carry no upper bound (they never expire) or do not decode.
 */
export const signedTxExpiryFacts = (
  signedTxCbor: string,
): SignedTxExpiryFacts | undefined => {
  let transaction: CML.Transaction;
  try {
    transaction = CML.Transaction.from_cbor_hex(signedTxCbor);
  } catch {
    return undefined;
  }
  try {
    const body = transaction.body();
    const ttl = body.ttl();
    if (ttl === undefined) return undefined;
    const bodyInputs = body.inputs();
    const inputs: OutRef[] = [];
    for (let index = 0; index < bodyInputs.len(); index++) {
      const input = bodyInputs.get(index);
      inputs.push({
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      });
    }
    return { invalidHereafterSlot: Number(ttl), inputs };
  } finally {
    transaction.free();
  }
};

/** What one read of a bounded transaction's watch settled. */
export type TxExpiryRead =
  /** The bound lies the confirmation depth under the tip, every input unspent. */
  | Readonly<{ kind: "expired"; tipSlot: number }>
  /** The bound lies the confirmation depth under the tip, an input spent. */
  | Readonly<{ kind: "spent"; tipSlot: number }>
  /** The tip is still before the bound: the transaction may yet land. */
  | Readonly<{ kind: "open"; tipSlot: number }>
  /** The tip passed the bound, but not yet by the confirmation depth. */
  | Readonly<{ kind: "settling"; tipSlot: number; boundDepth: number }>
  /** Nothing settled: a read failed or did not answer in time. */
  | Readonly<{ kind: "undecided"; reason: string }>;

const outRefKey = (ref: OutRef) => `${ref.txHash}#${ref.outputIndex}`;

/**
 * Bounds one provider read by `deadlineMs`: a read that does not answer in
 * time fails like one that errored.
 */
export const withReadDeadline = <A, E>(
  read: Effect.Effect<A, E>,
  deadlineMs: number,
  what: string,
): Effect.Effect<A, E | Error> =>
  read.pipe(
    Effect.timeoutFail({
      duration: deadlineMs,
      onTimeout: () =>
        new Error(`${what} did not answer within ${deadlineMs.toString()} ms`),
    }),
  );

/**
 * Watches whether a bounded transaction expired, one read per call. A tip at
 * or past the bound (`invalid_hereafter`) proves only that the chain as now
 * observed cannot include it: a short rollback onto a fork whose block before
 * the bound holds it would revive it. So the watch settles only once the
 * bound lies `confirmationDepth` blocks under the tip, the depth the node
 * treats as safe elsewhere (`DEPLOYMENT_MANIFEST_L1_FINALITY`, as
 * `settlementDepthParameters` reads it), decided with `isSafe`. The tip
 * carries no block height, so the depth of the first block past the bound
 * is counted as the distinct tips observed at or past it, each at a higher
 * slot than the last: a block a poll missed is not counted, which only makes
 * the wait longer, and a tip that moves back (a rollback) starts the count
 * again. Each read's tip is read first and the
 * inputs after it, so they are answered at that tip or later. Every read has
 * its own deadline, `readDeadlineMs`.
 */
export const watchTxExpiry = (
  lucid: LucidEvolution,
  facts: SignedTxExpiryFacts,
  depthParameters: Pick<DepthParameters, "confirmationDepth">,
  readDeadlineMs: number,
): Effect.Effect<TxExpiryRead> => {
  let boundDepth = 0;
  let lastTipSlot: number | undefined;
  return Effect.gen(function* () {
    const view = yield* Effect.either(
      withReadDeadline(providerViewPoint(lucid), readDeadlineMs, "the L1 tip"),
    );
    if (view._tag === "Left")
      return {
        kind: "undecided",
        reason: `the L1 tip could not be read: ${view.left.message}`,
      } as const;
    const tipSlot = view.right.slot;
    if (lastTipSlot !== undefined && tipSlot < lastTipSlot) boundDepth = 0;
    const newTip = lastTipSlot === undefined || tipSlot > lastTipSlot;
    lastTipSlot = tipSlot;
    if (tipSlot < facts.invalidHereafterSlot) {
      boundDepth = 0;
      return { kind: "open", tipSlot } as const;
    }
    if (newTip) boundDepth += 1;
    if (!isSafe(boundDepth, depthParameters))
      return { kind: "settling", tipSlot, boundDepth } as const;
    const unspent = yield* Effect.either(
      withReadDeadline(
        Effect.tryPromise({
          try: () => lucid.utxosByOutRef([...facts.inputs]),
          catch: (cause) => cause,
        }),
        readDeadlineMs,
        "the inputs read",
      ),
    );
    if (unspent._tag === "Left")
      return {
        kind: "undecided",
        reason: `the inputs could not be read: ${formatUnknownError(unspent.left)}`,
      } as const;
    const present = new Set(unspent.right.map(outRefKey));
    return facts.inputs.every((ref) => present.has(outRefKey(ref)))
      ? ({ kind: "expired", tipSlot } as const)
      : ({ kind: "spent", tipSlot } as const);
  });
};

/**
 * Submitted transactions found expired (`MidgardTxExpiring` watches it). One
 * expiry the rebuild clears is expected under a slow chain; repeated ones
 * mean transactions are not reaching blocks within their validity window.
 */
export const txExpiredCounter = Metric.counter("midgard_tx_expired", {
  description:
    "Submitted L1 transactions found expired: their upper validity bound lies the confirmation depth under the tip with every input unspent",
  incremental: true,
});

/**
 * Runs `operation`, and on {@link TxExpiredError} runs it again from the
 * start, so it rebuilds from fresh chain state, at most `RETRY_ATTEMPTS`
 * times. The operation must re-read the chain and skip what already landed.
 * When the rebuilds run out it fails with the last {@link TxExpiredError},
 * logged as an error.
 *
 * Expiry is settled only up to a rollback deeper than the confirmation
 * depth, so the rebuild must spend at least one input of the transaction
 * that expired (the state-machine anchor or nonce the step consumes): then
 * at most one of the two can ever land. A transaction whose rebuild may pick
 * other wallet coins can, after such a rollback, land beside it.
 */
export const rebuildOnTxExpiry = <A, E, R>(
  label: string,
  operation: Effect.Effect<A, E | TxExpiredError, R>,
): Effect.Effect<A, E | TxExpiredError, R> => {
  const attempt = (rebuilds: number): Effect.Effect<A, E | TxExpiredError, R> =>
    operation.pipe(
      Effect.catchIf(
        (error): error is TxExpiredError => error instanceof TxExpiredError,
        (expired) =>
          rebuilds < RETRY_ATTEMPTS
            ? Effect.logWarning(
                `${label}: tx ${expired.txHash} expired (its bound, slot ${expired.invalidHereafterSlot.toString()}, lies the confirmation depth under tip slot ${expired.tipSlot.toString()} with every input unspent); rebuilding from fresh chain state (${(rebuilds + 1).toString()}/${RETRY_ATTEMPTS.toString()})`,
              ).pipe(Effect.zipRight(attempt(rebuilds + 1)))
            : Effect.logError(
                `${label}: tx ${expired.txHash} expired and the ${RETRY_ATTEMPTS.toString()} rebuild(s) allowed are used up; giving up`,
              ).pipe(Effect.zipRight(Effect.fail(expired))),
      ),
    );
  return attempt(0);
};
