import {
  createSupersededAttemptReadSchedule,
  type FraudProofWorkflowTerminalVerifier,
  parseWorkflowFundingAbandonmentHandoff,
  parseWorkflowFundingPreparedTransition,
  readSignedAttempt,
  type SignedWorkflowTransactionRetirement,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherDecisionHold } from "../fault-proofs/watcher-decision-hold.js";
import {
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
  WatcherProverFundingUnavailableError,
} from "./prover-funding-reservation.js";

export const WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION =
  "prover_funding_orphaned_reservation" as const;

/**
 * An objective waiting on a funding reservation that no workflow will drive
 * again, whose signed attempt could still land. It runs again on the next
 * observation; the wait ends once each attempt has expired, been
 * invalidated or landed, at the latest when its validity interval ends.
 */
export class WatcherProverFundingOrphanedReservationError extends WatcherProverFundingUnavailableError {
  constructor(readonly details: readonly string[]) {
    super(details.join("; "));
    this.name = "WatcherProverFundingOrphanedReservationError";
  }
}

export type WatcherOrphanedReservationObserver =
  FraudProofWorkflowTerminalVerifier["observeSignedTransaction"];

type Attempt = Readonly<{
  transactionHash: string;
  signedTransactionCborHex: string;
}>;

/** Every signed attempt of the reservation that is not yet retired. */
const unretiredAttempts = async (
  store: WatcherProverFundingReservationStore,
  record: WatcherProverFundingReservationRecord,
): Promise<readonly Attempt[]> => {
  const reserved = { reservationId: record.reservationId };
  const retired = new Set<string>();
  const attempts = new Map<string, string>();
  const add = (value: unknown) => {
    const transition = parseWorkflowFundingPreparedTransition(value);
    attempts.set(
      transition.transactionHash,
      transition.signedTransactionCborHex,
    );
  };
  for (const saved of (await store.readLegacyAbandonedTransactions?.(
    reserved,
  )) ?? []) {
    const { transition, handoff } = saved as {
      transition: unknown;
      handoff: unknown;
    };
    add(transition);
    const abandoned = parseWorkflowFundingAbandonmentHandoff(handoff);
    if (abandoned.reconciliation.retirement !== undefined)
      retired.add(abandoned.submissionIntent.txHash);
  }
  for (const saved of (await store.readRecordedSubmissions?.(reserved)) ?? [])
    add((saved as { transition: unknown }).transition);
  const pending = await store.readPendingTransition(reserved);
  if (pending !== null) add(pending);
  return [...attempts]
    .filter(([transactionHash]) => !retired.has(transactionHash))
    .map(([transactionHash, signedTransactionCborHex]) => ({
      transactionHash,
      signedTransactionCborHex,
    }));
};

type Reading = Readonly<{
  /** Every attempt can no longer land beside a replacement, or landed
   * within release finality, where a replacement adopts it. */
  handOver: boolean;
  /** Every attempt's retirement past release finality, when all have one. */
  retirements:
    | readonly Readonly<{
        transactionHash: string;
        retirement: SignedWorkflowTransactionRetirement;
      }>[]
    | null;
  /** Why it is held, when it cannot be handed over. */
  detail: string;
}>;

const readAttempts = async (
  observe: WatcherOrphanedReservationObserver,
  attempts: readonly Attempt[],
): Promise<Reading> => {
  let handOver = true;
  let detail = "";
  const retirements: {
    transactionHash: string;
    retirement: SignedWorkflowTransactionRetirement;
  }[] = [];
  for (const attempt of attempts) {
    const read = await readSignedAttempt(
      { observeSignedTransaction: observe },
      attempt,
    );
    const retirement =
      read.kind === "pending" || read.kind === "not_found"
        ? read.retirement
        : undefined;
    if (retirement !== undefined)
      retirements.push({
        transactionHash: attempt.transactionHash,
        retirement,
      });
    // Landed within k is adopted from the replacement, and absent at the
    // tip is excluded by it. A landing past k is final and is never
    // adopted: the reservation settles once its other attempts retire.
    if (read.kind === "confirmed" || read.kind === "not_found") continue;
    handOver = false;
    detail =
      retirement === undefined
        ? `attempt ${attempt.transactionHash} reads ${read.kind}${"reason" in read ? `: ${read.reason}` : ""}`
        : `attempt ${attempt.transactionHash} landed past release finality`;
  }
  return {
    handOver,
    retirements: retirements.length === attempts.length ? retirements : null,
    detail:
      detail === ""
        ? "its attempts are not retired past release finality yet"
        : detail,
  };
};

/**
 * Settles reservations whose workflow is gone from their own recorded signed
 * bytes. Each read is bounded: a reservation or target is read at most once
 * per backoff step, and a sweep reads at most `perPass` reservations.
 */
export const createWatcherOrphanedReservations = (input: {
  readonly store: WatcherProverFundingReservationStore;
  readonly observe: WatcherOrphanedReservationObserver;
  readonly now?: () => number;
  readonly perPass?: number;
}) => {
  const schedule = createSupersededAttemptReadSchedule(
    input.perPass === undefined ? {} : { perPass: input.perPass },
  );
  const now = input.now ?? Date.now;
  const describe = (
    record: WatcherProverFundingReservationRecord,
    detail: string,
  ) =>
    `reservation ${record.reservationId} has no workflow to drive its signed attempts: ${detail}`;
  return Object.freeze({
    /**
     * The orphans of one target, before a fresh execution reserves for it.
     * Each settles once every attempt is retired past release finality.
     * With `handOver`, the others are returned once every attempt can be
     * handed over to the fresh reservation. Anything else throws the
     * orphan hold.
     */
    resolveTarget: async ({
      target,
      orphans,
      handOver,
    }: {
      readonly target: string;
      readonly orphans: readonly WatcherProverFundingReservationRecord[];
      readonly handOver: boolean;
    }): Promise<
      Readonly<{
        handOver: readonly WatcherProverFundingReservationRecord[];
        settled: readonly WatcherProverFundingReservationRecord[];
      }>
    > => {
      if (orphans.length === 0) return { handOver: [], settled: [] };
      const at = now();
      if (schedule.due([target], at).length === 0)
        throw new WatcherProverFundingOrphanedReservationError(
          orphans.map((record) =>
            describe(record, "awaiting its next L1 read"),
          ),
        );
      const held: string[] = [];
      const handed: WatcherProverFundingReservationRecord[] = [];
      const settled: WatcherProverFundingReservationRecord[] = [];
      for (const record of orphans) {
        const reading = await readAttempts(
          input.observe,
          await unretiredAttempts(input.store, record),
        );
        // Nothing of it can land or roll back any more: it settles alone.
        if (reading.retirements !== null) {
          if (
            await input.store.settleOrphanedReservation?.({
              orphan: record,
              retirements: reading.retirements,
            })
          )
            settled.push(record);
          else held.push(describe(record, "the store refused to settle it"));
          continue;
        }
        // A completed action is never handed over: it settles once final.
        const completed =
          (await input.store.readCompletionHandoff({
            reservationId: record.reservationId,
          })) !== null;
        if (handOver && reading.handOver && !completed) handed.push(record);
        else
          held.push(
            describe(
              record,
              completed
                ? "it completed and settles once its attempts are release-final"
                : reading.detail,
            ),
          );
      }
      if (held.length !== 0) {
        schedule.unresolved(target, at);
        throw new WatcherProverFundingOrphanedReservationError(held);
      }
      schedule.forget(target);
      return { handOver: handed, settled };
    },
    /**
     * Settles orphans the sweep found once every attempt is retired past
     * release finality, reading at most `perPass` of them. Returns a hold
     * for each one left.
     */
    sweep: async (
      orphans: readonly WatcherProverFundingReservationRecord[],
    ): Promise<readonly WatcherDecisionHold[]> => {
      const at = now();
      const due = new Set(
        schedule.due(
          orphans.map(({ reservationId }) => reservationId),
          at,
        ),
      );
      const holds: WatcherDecisionHold[] = [];
      for (const record of orphans) {
        let detail = "awaiting its next L1 read";
        if (due.has(record.reservationId)) {
          const reading = await readAttempts(
            input.observe,
            await unretiredAttempts(input.store, record),
          );
          if (
            reading.retirements !== null &&
            (await input.store.settleOrphanedReservation?.({
              orphan: record,
              retirements: reading.retirements,
            })) === true
          ) {
            schedule.forget(record.reservationId);
            continue;
          }
          schedule.unresolved(record.reservationId, at);
          detail = reading.detail;
        }
        holds.push(
          Object.freeze({
            kind: "reservation",
            reservationId: record.reservationId,
            decisionDigest: record.decisionDigest,
            detail: describe(record, detail),
            readiness: WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION,
          }),
        );
      }
      return Object.freeze(holds);
    },
  });
};

export type WatcherOrphanedReservations = ReturnType<
  typeof createWatcherOrphanedReservations
>;
