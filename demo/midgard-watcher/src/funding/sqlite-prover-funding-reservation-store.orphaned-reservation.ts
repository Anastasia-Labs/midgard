import type { DatabaseSync } from "node:sqlite";

import { parseSignedWorkflowTransactionRetirement } from "@al-ft/midgard-fault-proofs";

import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";
import { nextRecord } from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";
import type {
  RecordedAttempt,
  SupersededAttempt,
} from "./sqlite-prover-funding-reservation-store.row-parsers.js";

/**
 * Store operations that reclaim a reservation's funding. An unused one
 * returns its inputs to the wallet. One whose workflow will never drive it
 * again is settled, in one revision-checked transaction that
 * leaves it released, with nothing leased and no signed attempt of its own,
 * and deletes the workflow rows of its forgotten objective's execution.
 */
export const createProverFundingReclaimOperations = (deps: {
  readonly database: DatabaseSync;
  readonly transaction: <T>(operation: () => T) => T;
  readonly readOne: (
    reservationId: string,
  ) => WatcherProverFundingReservationRecord | null;
  readonly writeRecord: (
    currentDigest: string | null,
    next: WatcherProverFundingReservationRecord,
  ) => void;
  readonly rebuildLeases: () => void;
  readonly recordedSubmissions: (
    reservationId: string,
  ) => readonly RecordedAttempt[];
  readonly abandonmentsOf: (
    reservationId: string,
  ) => readonly SupersededAttempt[];
  readonly hasSignedHistory: (reservationId: string) => boolean;
  readonly forgetWorkflow: (workflowId: string) => void;
}): Pick<
  Required<WatcherProverFundingReservationStore>,
  "releaseUnused" | "settleOrphanedReservation"
> => {
  const { database } = deps;
  const deleteRows = (reservationId: string) => {
    for (const table of ["attempt", "superseded_attempt", "lineage"])
      database
        .prepare(
          `DELETE FROM watcher_prover_funding_${table}_v1 WHERE reservation_id = ?`,
        )
        .run(reservationId);
  };
  /** The orphan exactly as its caller classified it, still active. */
  const currentOrphan = (snapshot: WatcherProverFundingReservationRecord) => {
    const orphan = parseWatcherProverFundingReservationRecord(snapshot);
    const current = deps.readOne(orphan.reservationId);
    return current !== null &&
      current.recordDigest === orphan.recordDigest &&
      current.state === "active"
      ? current
      : null;
  };
  /** The orphan leaves nothing behind: released, with no rows of its own. */
  const settle = (orphan: WatcherProverFundingReservationRecord) => {
    deleteRows(orphan.reservationId);
    deps.writeRecord(
      orphan.recordDigest,
      nextRecord({
        current: orphan,
        state: "released",
        activeInputs: [],
        pendingTransition: null,
        lastConfirmedTransitionDigest: null,
        conflictCode: null,
      }),
    );
  };
  const clearUnused = async (
    expected: WatcherProverFundingReservationRecord,
  ): Promise<boolean> => {
    const snapshot = parseWatcherProverFundingReservationRecord(expected);
    return deps.transaction(() => {
      const current = deps.readOne(snapshot.reservationId);
      if (current === null)
        throw new Error("unused prover reservation is missing");
      if (
        current.recordDigest !== snapshot.recordDigest ||
        current.state !== "active" ||
        current.activeInputs.length === 0 ||
        current.pendingTransition !== null ||
        current.lastConfirmedTransitionDigest !== null
      )
        return false;
      // A reservation that ever signed an attempt is settled, never unused.
      if (deps.hasSignedHistory(current.reservationId)) return false;
      const next = nextRecord({ current, activeInputs: [] });
      deps.writeRecord(current.recordDigest, next);
      deps.rebuildLeases();
      return true;
    });
  };
  return Object.freeze({
    releaseUnused: clearUnused,
    settleOrphanedReservation: async ({
      orphan: snapshot,
      retirements,
      workflowId,
    }) =>
      deps.transaction(() => {
        const orphan = currentOrphan(snapshot);
        if (orphan === null) return false;
        const retired = new Set(
          deps
            .abandonmentsOf(orphan.reservationId)
            .filter(({ retirement }) => retirement !== undefined)
            .map(({ transition }) => transition.transactionHash),
        );
        // Each retirement must be of the attempt it names.
        for (const { transactionHash, retirement } of retirements)
          retired.add(
            parseSignedWorkflowTransactionRetirement(
              retirement,
              transactionHash,
            ).transactionHash,
          );
        const attempts = [
          ...deps.recordedSubmissions(orphan.reservationId),
          ...deps.abandonmentsOf(orphan.reservationId),
          ...(orphan.pendingTransition === null
            ? []
            : [{ transition: orphan.pendingTransition }]),
        ].map(({ transition }) => transition.transactionHash);
        // Only an attempt retired past k can never land again or roll back.
        if (
          (attempts.length === 0 &&
            !deps.hasSignedHistory(orphan.reservationId) &&
            orphan.lastConfirmedTransitionDigest === null) ||
          attempts.some((hash) => !retired.has(hash))
        )
          return false;
        settle(orphan);
        if (workflowId !== undefined) deps.forgetWorkflow(workflowId);
        deps.rebuildLeases();
        return true;
      }),
  });
};
