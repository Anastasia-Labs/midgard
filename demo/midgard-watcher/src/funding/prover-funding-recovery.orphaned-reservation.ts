import {
  createSupersededAttemptReadSchedule,
  type FraudProofWorkflowTerminalVerifier,
  parseWorkflowFundingPreparedTransition,
  parseWorkflowFundingSupersededAttempt,
  readSignedAttempt,
  type SignedWorkflowTransactionRetirement,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherDecisionHold } from "../fault-proofs/watcher-decision-hold.js";
import {
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";

export const WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION =
  "prover_funding_orphaned_reservation" as const;

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
  for (const saved of (await store.readSupersededAttempts?.(reserved)) ?? []) {
    const { transition, retirement } =
      parseWorkflowFundingSupersededAttempt(saved);
    add(transition);
    if (retirement !== undefined) retired.add(transition.transactionHash);
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

type Retired = Readonly<{
  transactionHash: string;
  retirement: SignedWorkflowTransactionRetirement;
}>;

type Reading = Readonly<{
  /** Every attempt was read this pass or is final already. */
  complete: boolean;
  /** Some attempt of it was read this pass. */
  touched: boolean;
  /** Every attempt's retirement past release finality, when all have one. */
  retirements: readonly Retired[] | null;
  /** Why it is held. */
  detail: string;
}>;

/** One attempt as a read left it: retired past release finality, or still
 * open. */
type AttemptRead =
  | Readonly<{ final: SignedWorkflowTransactionRetirement }>
  | Readonly<{ final?: undefined; detail: string }>;

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
    const read = await readSignedAttempt(
      { observeSignedTransaction: observe },
      attempt,
    );
    const retirement =
      read.kind === "pending" || read.kind === "not_found"
        ? read.retirement
        : undefined;
    if (retirement !== undefined) {
      remember(finals, attempt.transactionHash, retirement);
      return { final: retirement };
    }
    if (read.kind === "confirmed" || read.kind === "not_found")
      return { detail: "" };
    return {
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
      let detail = "";
      const retirements: Retired[] = [];
      for (const { transactionHash } of attempts) {
        const final = finals.get(transactionHash);
        const read: AttemptRead | undefined =
          final === undefined ? reads.get(transactionHash) : { final };
        if (read === undefined) {
          complete = false;
          continue;
        }
        if (final === undefined) touched = true;
        if (read.final !== undefined) {
          retirements.push({ transactionHash, retirement: read.final });
          // A landing past k is final: the reservation settles once its
          // other attempts retire.
          if (read.final.reason === "included")
            detail = `attempt ${transactionHash} landed past release finality`;
          continue;
        }
        if (read.detail !== "") detail = read.detail;
      }
      return {
        complete,
        touched,
        retirements:
          retirements.length === attempts.length ? retirements : null,
        detail: !complete
          ? "awaiting its next L1 read: it has more unsettled attempts than one pass reads"
          : detail === ""
            ? "its attempts are not retired past release finality yet"
            : detail,
      };
    });
  };
};

/** L1 reads of signed attempts one sweep makes. */
export const WATCHER_ORPHANED_RESERVATION_READS_PER_PASS = 16;

/**
 * Settles reservations whose workflow is gone from their own recorded signed
 * bytes. Each read is bounded: a reservation is read at most once per
 * backoff step, and a sweep makes at most `perPass` L1 reads of attempts, or
 * as many as the orphan first in line has open.
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
     * Settles orphans the sweep found once every attempt is retired past
     * release finality, within the pass's L1 reads, each with the workflow
     * rows `workflows` names for it. Returns a hold for each one left.
     */
    sweep: async (
      orphans: readonly WatcherProverFundingReservationRecord[],
      workflows: ReadonlyMap<string, string> = new Map(),
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
          reading,
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
              ...(workflows.has(record.reservationId)
                ? { workflowId: workflows.get(record.reservationId)! }
                : {}),
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
