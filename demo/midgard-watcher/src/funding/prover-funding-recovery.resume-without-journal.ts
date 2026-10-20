import {
  bindWorkflowActuationRecoveryIdentity,
  type HeaderFaultDecision,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { openWatcherFaultDecisionJournal } from "../fault-proofs/fault-decision-journal.js";
import { installedLaunchScope } from "../fault-proofs/fault-proof-application.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";

/**
 * With no workflow journal for its target, an objective resumes the
 * execution its funding reservation signed for, never a second one beside
 * it. A restart classifies the target again at a deeper confirmation depth,
 * so its decision is fresh; the reservation still names the original one.
 * As with a journal, the target's (deployment, category, header) finds it,
 * and the fresh classification must prove the identical fault before the
 * original decision becomes the execution identity. A reservation that
 * never signed holds no execution and stays for the unused-funding release.
 * Returns whether an original execution was resumed.
 */
export const resumeWatcherProverFundingWithoutJournal = async (
  input: Readonly<{
    journalRoot: string;
    journalAuthenticationKey: Uint8Array;
    actuationPermit: WorkflowActuationPermit;
    category: FraudProofCatalogueCategoryName;
    rollbackGeneration: string;
    store: WatcherProverFundingReservationStore;
  }>,
  authority: Readonly<{
    deploymentFingerprint: string;
    headerHash: string;
    launchScope: HeaderFaultDecision["launchScope"];
  }>,
): Promise<boolean> => {
  const records = (await input.store.readAll())
    .map(parseWatcherProverFundingReservationRecord)
    .filter(
      (record) =>
        record.deploymentFingerprint === authority.deploymentFingerprint,
    );
  if (records.length === 0) return false;
  const decisions = await (
    await openWatcherFaultDecisionJournal({
      directory: input.journalRoot,
      deploymentFingerprint: authority.deploymentFingerprint,
      launchScope: installedLaunchScope(authority.launchScope),
      authenticationKey: input.journalAuthenticationKey,
    })
  ).readAll();
  const originals: HeaderFaultDecision[] = [];
  for (const record of records) {
    const matches = decisions.filter(
      ({ decision }) => decision.decisionDigest === record.decisionDigest,
    );
    const decision = matches[0]?.decision;
    if (
      matches.length !== 1 ||
      decision?.decision !== "fault_detected" ||
      decision.category !== input.category ||
      decision.headerHash !== authority.headerHash ||
      !(
        record.pendingTransition !== null ||
        record.lastConfirmedTransitionDigest !== null ||
        (await input.store.hasSignedHistory?.({
          reservationId: record.reservationId,
        })) === true
      )
    )
      continue;
    if (record.state === "conflict")
      throw new Error(
        "workflow recovery has no unique non-conflicted original funding reservation",
      );
    originals.push(decision);
  }
  if (originals.length > 1)
    throw new Error(
      "workflow recovery has multiple signed funding reservations for one target",
    );
  const original = originals[0];
  if (original === undefined) return false;
  bindWorkflowActuationRecoveryIdentity({
    permit: input.actuationPermit,
    category: input.category,
    rollbackGeneration: input.rollbackGeneration,
    originalDecision: original,
  });
  return true;
};
