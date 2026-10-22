import {
  createSupersededAttemptReadSchedule,
  type FraudProofWorkflowTerminalVerifier,
  parseWorkflowFundingAbandonmentHandoff,
  parseWorkflowFundingPreparedTransition,
  readSignedAttempt,
  type SignedWorkflowTransactionRetirement,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherDecisionHold } from "../fault-proofs/watcher-decision-hold.js";
import { SIGNED_INPUT_HISTORY_UNKNOWN_REASONS } from "../l1-follower/fault-proof-l1-source.signed.js";
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
 * invalidated or landed, or has left the mempool with its inputs unspent.
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

/** One attempt as this pass left it. */
type OrphanAttemptState =
  | Readonly<{ final: SignedWorkflowTransactionRetirement }>
  | Readonly<{
      final?: undefined;
      handOver: boolean;
      /** It landed within release finality. */
      landed: boolean;
      /** Neither landed nor in the mempool, with its inputs unspent or
       * outside the follower's history: it may never retire by itself. */
      stranded: boolean;
    }>;

type Retired = Readonly<{
  transactionHash: string;
  retirement: SignedWorkflowTransactionRetirement;
}>;

type Reading = Readonly<{
  /** Every attempt was read this pass or is final already. */
  complete: boolean;
  /** Some attempt of it was read this pass. */
  touched: boolean;
  /** Every attempt can no longer land beside a replacement, or landed
   * within release finality, where a replacement adopts it. */
  handOver: boolean;
  /** Every attempt's retirement past release finality, when all have one. */
  retirements: readonly Retired[] | null;
  /** Why it is held, when it cannot be handed over. */
  detail: string;
  /** Each attempt read this pass or final already. */
  states: ReadonlyMap<string, OrphanAttemptState>;
}>;

/** One attempt as a read left it: retired past release finality, or still
 * open, and then whether a replacement may take it over. */
type AttemptRead =
  | Readonly<{ final: SignedWorkflowTransactionRetirement }>
  | (Exclude<OrphanAttemptState, { final: object }> &
      Readonly<{ detail: string }>);

/** Attempts tracked beyond the schedule's own entries. */
const MAXIMUM_TRACKED_ATTEMPTS = 4096;

const remember = <T>(map: Map<string, T>, key: string, value: T) => {
  map.delete(key);
  map.set(key, value);
  while (map.size > MAXIMUM_TRACKED_ATTEMPTS)
    map.delete(map.keys().next().value!);
};

/**
 * Reads signed attempts within a bound of `perPass` L1 reads a pass, the
 * least recently read first, and every open attempt of the orphan first in
 * line. A retirement past release finality never
 * changes, so it is kept and never read again: reservations with more
 * attempts than one pass reads still settle over later passes.
 */
const createAttemptReads = (
  observe: WatcherOrphanedReservationObserver,
  perPass: number,
) => {
  const finals = new Map<string, SignedWorkflowTransactionRetirement>();
  const lastRead = new Map<string, number>();
  const readOne = async (attempt: Attempt): Promise<AttemptRead> => {
    const seen: {
      observed?: Awaited<ReturnType<WatcherOrphanedReservationObserver>>;
    } = {};
    const read = await readSignedAttempt(
      {
        observeSignedTransaction: async (signed) =>
          (seen.observed = await observe(signed)),
      },
      attempt,
    );
    const { observed } = seen;
    const retirement =
      read.kind === "pending" || read.kind === "not_found"
        ? read.retirement
        : undefined;
    if (retirement !== undefined) {
      remember(finals, attempt.transactionHash, retirement);
      return { final: retirement };
    }
    // Landed within k is adopted from the replacement, and absent at the
    // tip is excluded by it. Dropped from the mempool with every input
    // unspent, or holding an input whose history the follower lacks, it is
    // excluded the same way: the replacement must spend one of its funding
    // inputs, and it is adopted should it land after all.
    const landed = read.kind === "confirmed";
    const stranded =
      read.kind === "unknown" &&
      (observed?.status === "rebroadcast" ||
        (observed?.status === "unknown" &&
          SIGNED_INPUT_HISTORY_UNKNOWN_REASONS.has(observed.reason)));
    if (landed || stranded || read.kind === "not_found")
      return { handOver: true, landed, stranded, detail: "" };
    return {
      handOver: false,
      landed,
      stranded,
      detail: `attempt ${attempt.transactionHash} reads ${read.kind}${"reason" in read ? `: ${read.reason}` : ""}`,
    };
  };
  /** One pass over the attempts of each group, a reading for each. */
  return async (
    groups: readonly (readonly Attempt[])[],
    at: number,
  ): Promise<readonly Reading[]> => {
    const open = [
      ...new Map(
        groups
          .flat()
          .filter(({ transactionHash }) => !finals.has(transactionHash))
          .map((attempt) => [attempt.transactionHash, attempt]),
      ).values(),
    ];
    const ordered = open
      .map((attempt, order) => ({
        attempt,
        order,
        readAt: lastRead.get(attempt.transactionHash) ?? -1,
      }))
      .sort((left, right) =>
        left.readAt === right.readAt
          ? left.order - right.order
          : left.readAt - right.readAt,
      )
      .map(({ attempt }) => attempt);
    // The orphan first in line is read whole, past the budget if it must:
    // one with more open attempts than a pass reads still settles.
    const first = ordered[0]?.transactionHash;
    const due = new Map(
      (
        groups.find((attempts) =>
          attempts.some(({ transactionHash }) => transactionHash === first),
        ) ?? []
      )
        .filter(({ transactionHash }) => !finals.has(transactionHash))
        .map((attempt) => [attempt.transactionHash, attempt]),
    );
    for (const attempt of ordered) {
      if (due.size >= perPass) break;
      due.set(attempt.transactionHash, attempt);
    }
    const reads = new Map<string, AttemptRead>();
    for (const attempt of due.values()) {
      remember(lastRead, attempt.transactionHash, at);
      reads.set(attempt.transactionHash, await readOne(attempt));
    }
    return groups.map((attempts) => {
      let complete = true;
      let touched = false;
      let handOver = true;
      let detail = "";
      const retirements: Retired[] = [];
      const states = new Map<string, OrphanAttemptState>();
      for (const { transactionHash } of attempts) {
        const final = finals.get(transactionHash);
        const read: AttemptRead | undefined =
          final === undefined ? reads.get(transactionHash) : { final };
        if (read === undefined) {
          complete = false;
          continue;
        }
        states.set(transactionHash, read);
        if (final === undefined) touched = true;
        if (read.final !== undefined) {
          retirements.push({ transactionHash, retirement: read.final });
          // A landing past k is final and is never adopted: the
          // reservation settles once its other attempts retire.
          if (read.final.reason !== "included") continue;
          handOver = false;
          detail = `attempt ${transactionHash} landed past release finality`;
          continue;
        }
        if (read.handOver) continue;
        handOver = false;
        detail = read.detail;
      }
      return {
        complete,
        touched,
        handOver: complete && handOver,
        retirements:
          retirements.length === attempts.length ? retirements : null,
        detail: !complete
          ? "awaiting its next L1 read: it has more unsettled attempts than one pass reads"
          : detail === ""
            ? "its attempts are not retired past release finality yet"
            : detail,
        states,
      };
    });
  };
};

/** L1 reads of signed attempts one sweep or one target resolution makes. */
export const WATCHER_ORPHANED_RESERVATION_READS_PER_PASS = 16;

/**
 * Settles reservations whose workflow is gone from their own recorded signed
 * bytes. Each read is bounded: a reservation or target is read at most once
 * per backoff step, and a sweep or a target resolution makes at most
 * `perPass` L1 reads of attempts, or as many as the orphan first in line
 * has open.
 */
export const createWatcherOrphanedReservations = (input: {
  readonly store: WatcherProverFundingReservationStore;
  readonly observe: WatcherOrphanedReservationObserver;
  readonly now?: () => number;
  readonly perPass?: number;
}) => {
  const perPass = input.perPass ?? WATCHER_ORPHANED_RESERVATION_READS_PER_PASS;
  const schedule = createSupersededAttemptReadSchedule({ perPass });
  const readPass = createAttemptReads(input.observe, perPass);
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
      const readings = await readPass(
        await Promise.all(
          orphans.map(
            async (record) => await unretiredAttempts(input.store, record),
          ),
        ),
        at,
      );
      for (const [index, record] of orphans.entries()) {
        const reading = readings[index]!;
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
     * release finality, within the pass's L1 reads. Returns a hold
     * for each one left.
     */
    sweep: async (
      orphans: readonly WatcherProverFundingReservationRecord[],
    ): Promise<readonly WatcherDecisionHold[]> => {
      const at = now();
      const dueIds = new Set(
        schedule.due(
          orphans.map(({ reservationId }) => reservationId),
          at,
        ),
      );
      const due = orphans.filter(({ reservationId }) =>
        dueIds.has(reservationId),
      );
      const attempts = await Promise.all(
        due.map(async (record) => await unretiredAttempts(input.store, record)),
      );
      const readings = new Map(
        (await readPass(attempts, at)).map((reading, index) => [
          due[index]!.reservationId,
          { ...reading, attempts: attempts[index]! },
        ]),
      );
      const holds: WatcherDecisionHold[] = [];
      for (const record of orphans) {
        let detail = "awaiting its next L1 read";
        const reading = readings.get(record.reservationId);
        if (reading !== undefined) {
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
          // One left unread for want of reads stays first in line.
          if (reading.touched || reading.complete)
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
