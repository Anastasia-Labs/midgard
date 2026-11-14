/**
 * The hand-off of a checked transaction to durable admission (design D9):
 * the duplicate lookup, load shedding, input claims and the writer's
 * registration, from memory. `/submit` and `/peer/txs` both admit through
 * it once `transaction-admission.ts`'s checks passed.
 */
import { computeMidgardNativeTxFullHashFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { Effect, Either, Option, Runtime } from "effect";

import { TxAdmissionsDB } from "../../database/index.js";
import {
  settleAdmissionBacklogSlot,
  tryReserveAdmissionBacklogSlot,
} from "../../fibers/index.js";
import { admissionBacklogLimitsFor } from "../../services/config.admission-backlog.js";
import {
  type AdmissionAck,
  AdmissionWriter,
  type AdmissionWriteRefusal,
  type AdmissionWriteRequest,
  Globals,
  MempoolLedgerCache,
  NodeConfig,
  UNKNOWN_SPENDABLE_VIEW,
} from "../../services/index.js";

/**
 * The part of a checked transaction admission needs. Both the checked value
 * `checkSubmittedTransaction` returns and a peer-forwarded tx that passed
 * the same checks fit it; `txFullHash` is recomputed only when absent.
 */
export type AdmissionCandidate = {
  readonly txId: Buffer;
  readonly txIdHex: string;
  readonly txCanonicalCbor: Buffer;
  readonly txFullHash?: Buffer;
  /** The outref items (hex) the tx spends and produces: its input claims. */
  readonly spentOutRefs: readonly string[];
  readonly producedOutRefs: readonly string[];
};

/** Why admission refused a checked tx; nothing is held for it. */
export type AdmissionRefusal =
  | TxAdmissionsDB.TxAdmissionBacklogFullError
  | AdmissionWriteRefusal;

/**
 * Admits a checked transaction under `submitSource` (design D9): the batch
 * admission below, for one tx.
 */
export const admitCheckedTransaction = <R>({
  normalized,
  submitSource,
  wakeTxQueueProcessor,
}: {
  readonly normalized: AdmissionCandidate;
  readonly submitSource: "native" | "peer";
  readonly wakeTxQueueProcessor: Effect.Effect<void, never, R>;
}): Effect.Effect<
  AdmissionAck,
  AdmissionRefusal,
  AdmissionWriter | NodeConfig | Globals | R
> =>
  Effect.flatMap(
    admitCheckedTransactions({
      candidates: [normalized],
      submitSource,
      wakeTxQueueProcessor,
    }),
    ([result]) => result!,
  );

type BatchEntry =
  | { readonly _tag: "Answered"; readonly result: AdmissionResult }
  | {
      readonly _tag: "Reserved";
      readonly request: AdmissionWriteRequest;
      /** Gives back the backlog slot and any input claims this call took. */
      readonly release: () => void;
    };

/** One tx's admission answer. */
export type AdmissionResult = Either.Either<AdmissionAck, AdmissionRefusal>;

/** Admits checked transactions under one submit source, in order; see below. */
export type CheckedTransactionsAdmitter = (
  candidates: readonly AdmissionCandidate[],
  submitSource: "native" | "peer",
) => readonly AdmissionResult[];

/**
 * Captures the services admission reads and returns it as a plain
 * synchronous function, so a caller outside Effect (the raw `/submit`
 * handler) admits without a fiber. It admits checked transactions under
 * `submitSource`, in order, in one writer registration (design D9, D5 batch
 * admit). It answers from memory and never waits for Postgres; each tx gets
 * its own answer, and one tx's refusal stops no other:
 *
 * - a tx id the writer holds pending, or wrote recently, answers its
 *   duplicate ack (or a `TxAdmissionConflictError` when the bytes differ)
 *   with no backlog reservation and no database read;
 * - otherwise it reserves a backlog slot (a full backlog refuses with
 *   `TxAdmissionBacklogFullError`), and every reserved tx goes to the writer
 *   at once, which acknowledges each as `new` once it is in the pending set;
 * - in the same step each tx takes its input claims (design D6): a `native`
 *   tx spending an input another pending tx claimed, or one that is neither
 *   in the mempool ledger state nor produced by a pending tx, is refused
 *   with `AdmissionInputSpentError`. A `peer` tx is never refused for its
 *   inputs. The ledger view is read without the Phase B lock.
 *
 * A `new` ack means accepted, not stored: the row is written by the
 * writer's next group commit, which then wakes the tx-queue processor. A
 * crash before that commit loses the tx. The call is one synchronous step,
 * so nothing interrupts it between a reservation and its hand-off.
 */
export const makeCheckedTransactionsAdmitter = <R>(
  wakeTxQueueProcessor: Effect.Effect<void, never, R>,
): Effect.Effect<
  CheckedTransactionsAdmitter,
  never,
  AdmissionWriter | NodeConfig | Globals | R
> =>
  Effect.gen(function* () {
    const admissionWriter = yield* AdmissionWriter;
    const nodeConfig = yield* NodeConfig;
    const globals = yield* Globals;
    const claims = globals.ADMISSION_INPUT_CLAIMS;
    const backlog = globals.ADMISSION_BACKLOG_GAUGE;
    const cache = yield* Effect.serviceOption(MempoolLedgerCache);
    const runtime = yield* Effect.runtime<Globals | R>();
    const runSync = Runtime.runSync(runtime);
    const wakeOnCommit = wakeTxQueueProcessor.pipe(
      Effect.provide(runtime.context),
    );
    const limits = {
      native: admissionBacklogLimitsFor(
        "native",
        nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
        nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG_BYTES,
      ),
      peer: admissionBacklogLimitsFor(
        "peer",
        nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
        nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG_BYTES,
      ),
    };
    return (candidates, submitSource) => {
      const entries: BatchEntry[] = [];
      for (const normalized of candidates) {
        const txFullHash =
          normalized.txFullHash ??
          computeMidgardNativeTxFullHashFromCanonicalCbor(
            normalized.txCanonicalCbor,
          );
        const known = admissionWriter.lookup(normalized.txId, txFullHash);
        if (known._tag !== "Unknown") {
          entries.push({
            _tag: "Answered",
            result:
              known._tag === "Duplicate"
                ? Either.right(known.ack)
                : Either.left(known.error),
          });
          continue;
        }
        // Load shedding (D5): an anonymous tx is refused from memory once
        // the backlog passes its share; a peer tx only once it is full.
        // Plain synchronous calls, not an Effect per tx: this runs for every
        // request on the main thread.
        const reservation = tryReserveAdmissionBacklogSlot(backlog, {
          ...limits[submitSource],
          bytes: normalized.txCanonicalCbor.length,
        });
        if (!reservation.reserved) {
          TxAdmissionsDB.admissionBacklogRejectCounter.unsafeUpdate(1n, []);
          entries.push({
            _tag: "Answered",
            result: Either.left(
              new TxAdmissionsDB.TxAdmissionBacklogFullError({
                backlog: reservation.backlog,
                maxBacklog: reservation.maxBacklog,
                unit: reservation.unit,
                message:
                  reservation.unit === "bytes"
                    ? "Durable submission admission byte backlog is full; retry later"
                    : "Durable submission admission backlog is full; retry later",
              }),
            ),
          });
          continue;
        }
        // Set when this call took the claims, so a write that stores no
        // new row gives them back; claims a pending copy holds stay with it.
        let claimed = false;
        const release = () => {
          if (claimed) claims.release(normalized.txIdHex);
          claimed = false;
          settleAdmissionBacklogSlot(backlog, reservation, false);
        };
        entries.push({
          _tag: "Reserved",
          release,
          request: {
            txId: normalized.txId,
            txCanonicalCbor: normalized.txCanonicalCbor,
            txFullHash,
            submitSource,
            settle: (written) =>
              Effect.sync(() =>
                written
                  ? settleAdmissionBacklogSlot(backlog, reservation, true)
                  : release(),
              ),
            wakeOnCommit,
            claimInputs: () => {
              const outcome = claims.take({
                txIdHex: normalized.txIdHex,
                spentOutRefs: normalized.spentOutRefs,
                producedOutRefs: normalized.producedOutRefs,
                check: submitSource === "native",
                spendable,
              });
              if (outcome._tag === "Refused") return outcome.error;
              claimed = outcome._tag === "Claimed";
              return undefined;
            },
          },
        });
      }
      const reserved = entries.filter((entry) => entry._tag === "Reserved");
      // Read right before the writer's registration, so the view is as
      // fresh as it can be when the claims are checked.
      const spendable =
        reserved.length > 0 && Option.isSome(cache)
          ? runSync(cache.value.spendableView)
          : UNKNOWN_SPENDABLE_VIEW;
      const written =
        reserved.length === 0
          ? []
          : admissionWriter.admitBatch(reserved.map((entry) => entry.request));
      let next = 0;
      return entries.map((entry) => {
        if (entry._tag === "Answered") return entry.result;
        const result: AdmissionResult = written[next++]!;
        // The writer owns the reservation only after answering `new`.
        if (!(Either.isRight(result) && result.right.kind === "new"))
          entry.release();
        return result;
      });
    };
  });

/**
 * Admits checked transactions under `submitSource`, in order: the admitter
 * above, for Effect callers. It runs in one synchronous step.
 */
export const admitCheckedTransactions = <R>({
  candidates,
  submitSource,
  wakeTxQueueProcessor,
}: {
  readonly candidates: readonly AdmissionCandidate[];
  readonly submitSource: "native" | "peer";
  readonly wakeTxQueueProcessor: Effect.Effect<void, never, R>;
}): Effect.Effect<
  readonly AdmissionResult[],
  never,
  AdmissionWriter | NodeConfig | Globals | R
> =>
  Effect.flatMap(
    makeCheckedTransactionsAdmitter(wakeTxQueueProcessor),
    (admit) => Effect.sync(() => admit(candidates, submitSource)),
  );
