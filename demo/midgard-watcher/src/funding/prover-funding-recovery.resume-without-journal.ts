import {
  bindWorkflowActuationRecoveryIdentity,
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type HeaderFaultDecision,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { openWatcherFaultDecisionJournal } from "../fault-proofs/fault-decision-journal.js";
import {
  installedLaunchScope,
  type WatcherInstalledWorkflowCategory,
} from "../fault-proofs/fault-proof-application.js";
import { WatcherProofDecisionMissingError } from "../fault-proofs/watcher-decision-hold.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";

/**
 * The hold for an objective whose signed funding names a fault decision the
 * decision journal no longer holds: it never runs again beside that funding.
 * TODO(option-A lane): the hold lasts until the header leaves the queue.
 * Release the reservation once its in-flight attempt is expired or
 * invalidated from its own signed bytes, or adopt it if it landed, so a fresh
 * workflow can proceed.
 */
export const watcherProverFundingDecisionMissing = (
  category: FraudProofCatalogueCategoryName,
  headerHash: string,
  decisionDigest: string,
) =>
  new WatcherProofDecisionMissingError({
    kind: "objective",
    category: category as WatcherInstalledWorkflowCategory,
    headerHash,
    decisionDigest,
    detail: `${category}/${headerHash}: workflow recovery has no unique original fault decision (${decisionDigest})`,
  });

/** The workflow identities a reservation's stored handoffs name. */
const handoffIdentities = async (
  store: WatcherProverFundingReservationStore,
  reserved: Readonly<{ reservationId: string }>,
) =>
  [
    ...((await store.readRecordedSubmissions?.(reserved)) ?? []),
    ...((await store.readLegacyAbandonedTransactions?.(reserved)) ?? []),
    await store.readPendingHandoff(reserved),
    await store.readAbandonmentHandoff(reserved),
    { handoff: await store.readCompletionHandoff(reserved) },
  ].map(
    (value) =>
      (value as { handoff?: { identity?: unknown } } | null)?.handoff
        ?.identity as
        | {
            category?: unknown;
            target?: { headerHash?: unknown };
          }
        | undefined,
  );

/**
 * The decision of a reservation with durable funding history whose workflow
 * is the empty directory `workflowId`, or null when it has none.
 */
export const emptyExecutionHistory = async (
  input: Readonly<{
    category: FraudProofCatalogueCategoryName;
    store: WatcherProverFundingReservationStore;
  }>,
  authority: Readonly<{ headerHash: string }>,
  workflowId: string,
): Promise<string | null> => {
  const { store } = input;
  for (const record of (await store.readAll()).map(
    parseWatcherProverFundingReservationRecord,
  ))
    if (
      computeFraudProofWorkflowId({
        schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        deploymentFingerprint: record.deploymentFingerprint,
        category: input.category,
        target: {
          kind: "state_queue_header",
          headerHash: authority.headerHash,
        },
        decisionDigest: record.decisionDigest,
      }) === workflowId &&
      (record.state !== "active" ||
        record.pendingTransition !== null ||
        record.lastConfirmedTransitionDigest !== null ||
        store.hasSignedHistory === undefined ||
        (await store.hasSignedHistory({ reservationId: record.reservationId })))
    )
      return record.decisionDigest;
  return null;
};

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
    const signed =
      record.pendingTransition !== null ||
      record.lastConfirmedTransitionDigest !== null ||
      (await input.store.hasSignedHistory?.({
        reservationId: record.reservationId,
      })) === true;
    // Signed funding for this target whose decision is gone is held, never
    // run beside. Its handoffs are unauthenticated: they only ever hold.
    // TODO(option-A lane): release or adopt it, as the hold's doc says.
    if (
      signed &&
      (matches.length !== 1 || decision?.decision !== "fault_detected") &&
      (await handoffIdentities(input.store, record)).some(
        (identity) =>
          identity?.category === input.category &&
          identity.target?.headerHash === authority.headerHash,
      )
    )
      throw watcherProverFundingDecisionMissing(
        input.category,
        authority.headerHash,
        record.decisionDigest,
      );
    if (
      !signed ||
      matches.length !== 1 ||
      decision?.decision !== "fault_detected" ||
      decision.category !== input.category ||
      decision.headerHash !== authority.headerHash
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
