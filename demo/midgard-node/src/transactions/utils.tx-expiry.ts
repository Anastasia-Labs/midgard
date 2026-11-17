/**
 * Expiry of a submitted transaction: the outcome a confirmation wait reports
 * when a transaction can no longer land, and the bounded rebuild an
 * operation runs on it.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { type DepthParameters, isSafe } from "@al-ft/midgard-l1-follower/heads";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, type LucidEvolution, type OutRef } from "@lucid-evolution/lucid";
import { Data, Effect, Either, Metric } from "effect";

import type { L1ViewPoint } from "../l1-access.js";
import {
  providerPointOnChain,
  providerUtxosByOutRef,
  providerViewPoint,
} from "../l1-provider-view.js";
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
  /** Nothing settled: the `read` failed or did not answer in time. */
  | Readonly<{
      kind: "undecided";
      read: "tip" | "ancestry" | "inputs";
      reason: string;
    }>;

const outRefKey = (ref: OutRef) => `${ref.txHash}#${ref.outputIndex}`;

/**
 * Bounds one provider read by `deadlineMs`: a read that does not answer in
 * time fails like one that errored.
 */
const withReadDeadline = <A, E>(
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
 * How many reads of one kind may be in flight at once: the read being made,
 * and one earlier read that outlived its abort.
 *
 * A policy, not a derived bound: tolerate one read that outlives its
 * abort. A read that misses its deadline is aborted, and the next call
 * gives it one whole deadline more before it reads beside it
 * (`boundedReads`). The follower's store reads end at their abort; a read
 * from any other source settles in the end, but nothing bounds when: the
 * node transport bounds each request, not a read that waits behind other
 * requests or makes several. Such a read costs one call its deadline, after
 * which the reads go on beside it. A second read that outlives its abort
 * stops new reads of the kind until one of the two settles, so a source
 * that never answers holds at most two reads of a kind.
 */
export const READS_IN_FLIGHT_PER_KIND = 2;

/**
 * One kind of provider read, each call bounded by its `deadlineMs`
 * (`withReadDeadline`), at most `READS_IN_FLIGHT_PER_KIND` in flight. A
 * read that misses its deadline is aborted (its `signal` fires, as when the
 * caller is interrupted), so a source that honours the signal ends it. A
 * call that finds the latest read still unsettled waits for it, within its
 * own deadline, before it reads; once a call has waited a whole deadline
 * for it, later calls read beside it. An answer is only ever taken from a
 * read the same call started.
 */
export const boundedReads = (what: string) => {
  /** Reads started and not yet settled, oldest first. */
  const unsettled: Promise<void>[] = [];
  /** Unsettled reads a later call already waited a whole deadline for. */
  const outwaited = new WeakSet<Promise<void>>();
  return <A>(
    read: (signal: AbortSignal) => Promise<A>,
    deadlineMs: number,
  ): Effect.Effect<A, unknown> =>
    withReadDeadline(
      Effect.gen(function* () {
        const latest = unsettled.at(-1);
        if (latest !== undefined && !outwaited.has(latest))
          yield* Effect.promise(() => latest).pipe(
            Effect.onInterrupt(() => Effect.sync(() => outwaited.add(latest))),
          );
        while (unsettled.length >= READS_IN_FLIGHT_PER_KIND) {
          const inFlight = [...unsettled];
          yield* Effect.promise(() => Promise.race(inFlight));
        }
        return yield* Effect.tryPromise({
          try: (signal) => {
            const answer = read(signal);
            const settled = answer.then(
              () => undefined,
              () => undefined,
            );
            unsettled.push(settled);
            void settled.then(() => {
              void unsettled.splice(unsettled.indexOf(settled), 1);
            });
            return answer;
          },
          catch: (cause) => cause,
        });
      }),
      deadlineMs,
      what,
    );
};

/**
 * Watches whether a bounded transaction expired, one read per call. A tip at
 * or past the bound (`invalid_hereafter`) proves only that the chain as now
 * observed cannot include it: a short rollback onto a fork whose block before
 * the bound holds it would revive it. So the watch settles only once the
 * bound lies `confirmationDepth` blocks under the tip, the depth the node
 * treats as safe elsewhere (`DEPLOYMENT_MANIFEST_L1_FINALITY`, as
 * `settlementDepthParameters` reads it), decided with `isSafe`. The tip
 * carries no block height, so the depth of the first block past the bound
 * is counted as the distinct tips observed at or past it: a block a poll
 * missed is not counted, which only makes the wait longer.
 *
 * A rollback onto a fork starts the count again at the fork's tip, told by
 * the tip's identity, not by its slot moving back: a new tip at the same or
 * a lower slot than the last one is on a fork, and so is one at a higher
 * slot when the access says the last tip is no longer on its chain
 * (`providerPointOnChain`). An access that cannot say (the `kupmios` and
 * `blockfrost` tool adapters) misses a fork whose tip is at a higher slot
 * than the tip it replaced. A tip before the bound starts the count again
 * too.
 *
 * Each read's tip is read first and the inputs after it, so they are
 * answered at that tip or later. Every read has its own deadline,
 * `readDeadlineMs`; a read that misses it is aborted, which ends the
 * follower's store reads, and each kind of read is held to
 * `READS_IN_FLIGHT_PER_KIND` in flight (`boundedReads`).
 */
export const watchTxExpiry = (
  lucid: LucidEvolution,
  facts: SignedTxExpiryFacts,
  depthParameters: Pick<DepthParameters, "confirmationDepth">,
  readDeadlineMs: number,
): Effect.Effect<TxExpiryRead> => {
  const pointOnChain = providerPointOnChain(lucid);
  const tipReads = boundedReads("the L1 tip");
  const ancestryReads = boundedReads("the ancestry read of the last L1 tip");
  const inputReads = boundedReads("the inputs read");
  let boundDepth = 0;
  let lastTip: L1ViewPoint | undefined;
  return Effect.gen(function* () {
    const view = yield* Effect.either(
      // The signal goes to the access's read, not to this promise, so the
      // read counts as in flight until the access's read itself settles.
      tipReads(
        (signal) =>
          Effect.runPromise(
            Effect.either(providerViewPoint(lucid, { signal })),
          ).then(Either.getOrThrowWith((error) => error)),
        readDeadlineMs,
      ),
    );
    if (view._tag === "Left")
      return {
        kind: "undecided",
        read: "tip",
        reason: `the L1 tip could not be read: ${formatUnknownError(view.left)}`,
      } as const;
    const tip = view.right;
    const tipSlot = tip.slot;
    if (tipSlot < facts.invalidHereafterSlot) {
      boundDepth = 0;
      lastTip = tip;
      return { kind: "open", tipSlot } as const;
    }
    const previous = lastTip;
    const newTip = previous === undefined || tip.id !== previous.id;
    if (previous !== undefined && newTip && boundDepth > 0) {
      let forked = tipSlot <= previous.slot;
      if (!forked && pointOnChain !== undefined) {
        const onChain = yield* Effect.either(
          ancestryReads(
            (signal) => pointOnChain(previous, { signal }),
            readDeadlineMs,
          ),
        );
        if (onChain._tag === "Left")
          return {
            kind: "undecided",
            read: "ancestry",
            reason: `whether the last L1 tip is still on the chain could not be read: ${formatUnknownError(onChain.left)}`,
          } as const;
        forked = !onChain.right;
      }
      if (forked) boundDepth = 0;
    }
    if (newTip) boundDepth += 1;
    lastTip = tip;
    if (!isSafe(boundDepth, depthParameters))
      return { kind: "settling", tipSlot, boundDepth } as const;
    const unspent = yield* Effect.either(
      inputReads(
        (signal) => providerUtxosByOutRef(lucid, facts.inputs, { signal }),
        readDeadlineMs,
      ),
    );
    if (unspent._tag === "Left")
      return {
        kind: "undecided",
        read: "inputs",
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
