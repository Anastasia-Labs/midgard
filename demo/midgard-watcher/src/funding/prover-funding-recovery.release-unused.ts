import { openWatcherFaultDecisionJournal } from "../fault-proofs/fault-decision-journal.js";
import type { WatcherInstalledWorkflowCategory } from "../fault-proofs/fault-proof-application.js";
import { watcherProofObjectiveRecorded } from "../fault-proofs/fault-proof-objective-table.js";
import type { WatcherDecisionHold } from "../fault-proofs/watcher-decision-hold.js";
import { openWatcherJournalDatabase } from "../fault-proofs/watcher-journal-database.js";
import type { WatcherWorkflowJournal } from "../fault-proofs/watcher-workflow-journal.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import type { WatcherOrphanedReservations } from "./prover-funding-recovery.orphaned-reservation.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";

type ReservationHold = WatcherDecisionHold & Readonly<{ kind: "reservation" }>;

/** Whether the reservation holds a signed attempt or its history. */
const signedReservation = async (
  store: WatcherProverFundingReservationStore,
  record: WatcherProverFundingReservationRecord,
) =>
  record.pendingTransition !== null ||
  record.lastConfirmedTransitionDigest !== null ||
  (await store.hasSignedHistory?.({ reservationId: record.reservationId })) ===
    true;

/**
 * Reclaim unsubmitted work after its runner exits, or before startup
 * dispatch. A reservation whose recorded decision is missing is left as it
 * is. With `orphans`, signed funding nothing drives (no workflow journal
 * names it, or its objective was forgotten when its header left the queue)
 * is released once each of its attempts is retired past release finality,
 * read from its own bytes, and held until then
 * (prover_funding_orphaned_reservation); the holds are returned. With `decisionMissingOnly`, unsigned reservations
 * are left alone: that re-check runs beside live dispatch, which owns them.
 */
export const releaseUnusedWatcherProverFundingReservations = async (input: {
  readonly journalRoot: string;
  readonly journalAuthenticationKey: Uint8Array;
  readonly workflowJournal: WatcherWorkflowJournal;
  readonly launchScope: readonly WatcherInstalledWorkflowCategory[];
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly store: WatcherProverFundingReservationStore;
  readonly reservations?: readonly WatcherProverFundingReservationRecord[];
  readonly decisionMissingOnly?: boolean;
  readonly orphans?: WatcherOrphanedReservations;
}): Promise<readonly ReservationHold[]> => {
  const unused = (record: WatcherProverFundingReservationRecord) =>
    record.activeInputs.length !== 0 &&
    record.pendingTransition === null &&
    record.lastConfirmedTransitionDigest === null;
  const records = (input.reservations ?? (await input.store.readAll()))
    .map(parseWatcherProverFundingReservationRecord)
    .filter(
      (record) =>
        record.deploymentFingerprint === input.deploymentIdentity.manifestId &&
        record.state === "active" &&
        (unused(record) ||
          (input.orphans !== undefined &&
            (record.pendingTransition !== null ||
              record.lastConfirmedTransitionDigest !== null))),
    );
  if (records.length === 0) return [];
  if (input.store.releaseUnused === undefined)
    throw new Error("prover funding store cannot release unused reservations");
  const decisions = await openWatcherFaultDecisionJournal({
    directory: input.journalRoot,
    deploymentFingerprint: input.deploymentIdentity.manifestId,
    launchScope: input.launchScope,
    authenticationKey: input.journalAuthenticationKey,
  });
  const saved = await decisions.readAll();
  const journals = openWatcherJournalDatabase({
    journalRoot: input.journalRoot,
    authenticationKey: input.journalAuthenticationKey,
  });
  const holds: ReservationHold[] = [];
  const orphaned: WatcherProverFundingReservationRecord[] = [];
  for (const record of records) {
    const matches = saved.filter(
      ({ decision }) => decision.decisionDigest === record.decisionDigest,
    );
    const decision = matches[0]?.decision;
    const signed =
      input.orphans !== undefined &&
      (await signedReservation(input.store, record));
    if (matches.length !== 1 || decision?.decision !== "fault_detected")
      continue;
    if (input.decisionMissingOnly === true && !signed) continue;
    const entries =
      input.workflowJournal.readExecution({
        category: decision.category,
        headerHash: decision.headerHash,
      })?.entries ?? [];
    const identity = entries[0]?.identity;
    if (
      identity !== undefined &&
      identity.deploymentFingerprint !== record.deploymentFingerprint
    )
      throw new Error("unused funding journal changed its execution identity");
    const submitted =
      identity?.decisionDigest === record.decisionDigest &&
      (signed ||
        entries.some(({ event }) => event.kind === "submission_intent"));
    // A signed reservation is its run's to settle while its objective is
    // recorded; with no run or no objective left, it is orphaned.
    if (signed) {
      if (!submitted || !watcherProofObjectiveRecorded(journals, decision))
        orphaned.push(record);
    } else if (!submitted) await input.store.releaseUnused(record);
  }
  if (orphaned.length !== 0 && input.orphans !== undefined)
    holds.push(
      ...((await input.orphans.sweep(orphaned)) as readonly ReservationHold[]),
    );
  return Object.freeze(holds);
};
