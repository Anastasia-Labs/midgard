/**
 * Expiry of a submitted transaction: the outcome a confirmation wait reports
 * when a transaction can no longer land, and the bounded rebuild an
 * operation runs on it.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, type LucidEvolution, type OutRef } from "@lucid-evolution/lucid";
import { Data, Effect, Metric } from "effect";

import { providerViewPoint } from "../l1-provider-view.js";
import { RETRY_ATTEMPTS } from "./utils.parse-structured-outside-validity-interval-details.js";

/**
 * A submitted transaction that can never land: the L1 tip reached its upper
 * validity bound (`invalid_hereafter`) while every one of its inputs was
 * still unspent. Unlike a confirmation timeout this is settled, so the
 * operation that built it may rebuild from fresh chain state.
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

/** One expiry read of a bounded transaction. */
export type TxExpiryRead =
  /** The tip reached the bound with every input unspent: it never lands. */
  | Readonly<{ kind: "expired"; tipSlot: number }>
  /** The tip is still before the bound: the transaction may yet land. */
  | Readonly<{ kind: "open"; tipSlot: number }>
  /** Nothing settled: a read failed, or an input is spent. */
  | Readonly<{ kind: "undecided"; reason: string }>;

const outRefKey = (ref: OutRef) => `${ref.txHash}#${ref.outputIndex}`;

/**
 * Reads whether a bounded transaction expired. The tip is read first and
 * the inputs after it, so the inputs are answered at a ledger state at or
 * after a tip that already reached the bound: no later block can include
 * the transaction. An input that is spent settles nothing here (the
 * transaction landed, or another one spent it); the status read decides.
 */
export const readTxExpiry = (
  lucid: LucidEvolution,
  facts: SignedTxExpiryFacts,
): Effect.Effect<TxExpiryRead> =>
  Effect.gen(function* () {
    const view = yield* Effect.either(providerViewPoint(lucid));
    if (view._tag === "Left")
      return {
        kind: "undecided",
        reason: `the L1 tip could not be read: ${view.left.message}`,
      } as const;
    const tipSlot = view.right.slot;
    if (tipSlot < facts.invalidHereafterSlot)
      return { kind: "open", tipSlot } as const;
    const unspent = yield* Effect.either(
      Effect.tryPromise(() => lucid.utxosByOutRef([...facts.inputs])),
    );
    if (unspent._tag === "Left")
      return {
        kind: "undecided",
        reason: `the inputs could not be read: ${formatUnknownError(unspent.left.cause)}`,
      } as const;
    const present = new Set(unspent.right.map(outRefKey));
    return facts.inputs.every((ref) => present.has(outRefKey(ref)))
      ? ({ kind: "expired", tipSlot } as const)
      : ({
          kind: "undecided",
          reason: "an input is spent",
        } as const);
  });

/**
 * Submitted transactions found expired (`MidgardTxExpiring` watches it). One
 * expiry the rebuild clears is expected under a slow chain; repeated ones
 * mean transactions are not reaching blocks within their validity window.
 */
export const txExpiredCounter = Metric.counter("midgard_tx_expired", {
  description:
    "Submitted L1 transactions found expired: the tip reached their upper validity bound with every input unspent",
  incremental: true,
});

/**
 * Runs `operation`, and on {@link TxExpiredError} runs it again from the
 * start, so it rebuilds from fresh chain state, at most `RETRY_ATTEMPTS`
 * times. The operation must re-read the chain and skip what already landed.
 * When the rebuilds run out it fails with the last {@link TxExpiredError},
 * logged as an error.
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
                `${label}: tx ${expired.txHash} expired (tip slot ${expired.tipSlot.toString()} reached its bound ${expired.invalidHereafterSlot.toString()} with every input unspent); rebuilding from fresh chain state (${(rebuilds + 1).toString()}/${RETRY_ATTEMPTS.toString()})`,
              ).pipe(Effect.zipRight(attempt(rebuilds + 1)))
            : Effect.logError(
                `${label}: tx ${expired.txHash} expired and the ${RETRY_ATTEMPTS.toString()} rebuild(s) allowed are used up; giving up`,
              ).pipe(Effect.zipRight(Effect.fail(expired))),
      ),
    );
  return attempt(0);
};
