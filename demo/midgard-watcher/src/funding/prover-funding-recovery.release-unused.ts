import type { Dirent } from "node:fs";
import { readdir, realpath } from "node:fs/promises";
import { join } from "node:path";

import { DirectoryFraudProofWorkflowJournalStore } from "@al-ft/midgard-fault-proofs";

import { openWatcherFaultDecisionJournal } from "../fault-proofs/fault-decision-journal.js";
import type { WatcherInstalledWorkflowCategory } from "../fault-proofs/fault-proof-application.js";
import type { WatcherDecisionHold } from "../fault-proofs/watcher-decision-hold.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import type { WatcherFundingInputFacts } from "./prover-funding-input-facts.js";
import type { WatcherOrphanedReservations } from "./prover-funding-recovery.orphaned-reservation.js";
import { signedReservation } from "./prover-funding-recovery.resume-without-journal.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";

type ReservationHold = WatcherDecisionHold & Readonly<{ kind: "reservation" }>;

/**
 * A reservation whose recorded decision is missing, so no journal can say
 * whether its work was submitted. Only L1 facts reclaim it: inputs all spent
 * at a final view drop it (its transaction or another landed); inputs
 * unspent at a final view and at the tip return to the wallet, and only
 * when the store holds no signed attempt for it, since every signed
 * transaction is persisted before it can be submitted. Anything else keeps
 * it held, read again on the next tick.
 */
const reclaimWithoutDecision = async (
  store: WatcherProverFundingReservationStore,
  facts: WatcherFundingInputFacts | undefined,
  record: WatcherProverFundingReservationRecord,
): Promise<ReservationHold | null> => {
  const hold = (detail: string): ReservationHold =>
    Object.freeze({
      kind: "reservation",
      reservationId: record.reservationId,
      decisionDigest: record.decisionDigest,
      detail: `reservation ${record.reservationId} has no unique recorded fault decision ${record.decisionDigest}: ${detail}`,
    });
  if (facts === undefined) return hold("no L1 facts can show its inputs");
  const outRefs = record.activeInputs.map(({ outRef }) => outRef);
  const standing = await facts.standing(outRefs);
  if (standing.undetermined !== null)
    return hold(`its inputs are not final yet: ${standing.undetermined}`);
  if (standing.spent.length === outRefs.length) {
    if (store.dropSpentUnused === undefined)
      return hold("the store cannot drop a spent reservation");
    return (await store.dropSpentUnused(record))
      ? null
      : hold("the store refused to drop it after its inputs were spent");
  }
  if (store.releaseUnused === undefined)
    return hold("the store cannot release unused reservations");
  return (await store.releaseUnused(record))
    ? null
    : hold(
        "its inputs are unspent at a final view but it has signed history or changed",
      );
};

/**
 * Reclaim unsubmitted work after its runner exits, or before startup
 * dispatch. Returns the reservations held because their decision is
 * missing (journal_decision_missing). With `orphans`, signed funding no
 * workflow will drive again (its decision is missing, or no workflow
 * journal names it) is released once each of its attempts is retired past
 * release finality, read from its own bytes, and held until then
 * (prover_funding_orphaned_reservation). With `decisionMissingOnly`, an
 * unsigned reservation whose decision is recorded again is left alone: that
 * re-check runs beside live dispatch, which owns such reservations.
 */
export const releaseUnusedWatcherProverFundingReservations = async (input: {
  readonly journalRoot: string;
  readonly journalAuthenticationKey: Uint8Array;
  readonly launchScope: readonly WatcherInstalledWorkflowCategory[];
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly store: WatcherProverFundingReservationStore;
  readonly reservations?: readonly WatcherProverFundingReservationRecord[];
  readonly fundingInputFacts?: WatcherFundingInputFacts;
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
    if (matches.length !== 1 || decision?.decision !== "fault_detected") {
      const held = unused(record)
        ? await reclaimWithoutDecision(
            input.store,
            input.fundingInputFacts,
            record,
          )
        : null;
      if (signed && (held !== null || !unused(record))) orphaned.push(record);
      else if (held !== null) holds.push(held);
      continue;
    }
    if (input.decisionMissingOnly === true && !signed) continue;
    const directory = join(
      input.journalRoot,
      "fault-proofs",
      decision.category,
      decision.headerHash,
    );
    let names: Dirent[];
    try {
      names = await readdir(directory, { withFileTypes: true });
    } catch (error) {
      if (error instanceof Error && "code" in error && error.code === "ENOENT")
        names = [];
      else throw error;
    }
    if (names.length > 0 && (await realpath(directory)) !== directory)
      throw new Error("unused funding journal traverses a symlink");
    let submitted = false;
    for (const name of names) {
      if (
        !name.isDirectory() ||
        !/^[0-9a-f]{64}$/u.test(name.name) ||
        (await realpath(join(directory, name.name))) !==
          join(directory, name.name)
      )
        throw new Error(
          "unused funding target contains an invalid journal entry",
        );
      const entries = await new DirectoryFraudProofWorkflowJournalStore(
        directory,
      ).load(name.name);
      const identity = entries[0]?.identity;
      if (
        identity !== undefined &&
        (identity.decisionDigest === undefined ||
          identity.deploymentFingerprint !== record.deploymentFingerprint ||
          identity.category !== decision.category ||
          identity.target.kind !== "state_queue_header" ||
          identity.target.headerHash !== decision.headerHash)
      )
        throw new Error(
          "unused funding journal changed its execution identity",
        );
      if (
        identity?.decisionDigest === record.decisionDigest &&
        (signed ||
          entries.some(({ event }) => event.kind === "submission_intent"))
      )
        submitted = true;
    }
    // A signed reservation is its journal's to settle; with none, it is orphaned.
    if (signed) {
      if (!submitted) orphaned.push(record);
    } else if (!submitted) await input.store.releaseUnused(record);
  }
  if (orphaned.length !== 0 && input.orphans !== undefined)
    holds.push(
      ...((await input.orphans.sweep(orphaned)) as readonly ReservationHold[]),
    );
  return Object.freeze(holds);
};
