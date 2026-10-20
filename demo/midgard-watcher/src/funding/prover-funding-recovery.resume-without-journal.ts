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
import type { WatcherOrphanedReservations } from "./prover-funding-recovery.orphaned-reservation.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";

/**
 * The hold for an objective whose signed funding names a fault decision the
 * decision journal no longer holds, when the watcher cannot read that
 * funding's attempts on L1: it never runs again beside that funding. With
 * those reads, the funding is handed over to the fresh execution instead
 * (see `resumeWatcherProverFundingWithoutJournal`).
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

/** Whether the reservation holds a signed attempt or its history. */
export const signedReservation = async (
  store: WatcherProverFundingReservationStore,
  record: WatcherProverFundingReservationRecord,
) =>
  record.pendingTransition !== null ||
  record.lastConfirmedTransitionDigest !== null ||
  (await store.hasSignedHistory?.({ reservationId: record.reservationId })) ===
    true;

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

/** Whether a reservation's stored handoffs name this target. */
export const handoffsNameTarget = async (
  store: WatcherProverFundingReservationStore,
  record: WatcherProverFundingReservationRecord,
  category: string,
  headerHash: string,
) =>
  (await handoffIdentities(store, record)).some(
    (identity) =>
      identity?.category === category &&
      identity.target?.headerHash === headerHash,
  );

/** The active signed reservations whose handoffs name a target no workflow
 * journal holds, whatever decision they name. */
export const targetOrphans = async (
  store: WatcherProverFundingReservationStore,
  target: Readonly<{
    deploymentFingerprint: string;
    category: string;
    headerHash: string;
  }>,
) => {
  const orphans: WatcherProverFundingReservationRecord[] = [];
  for (const record of (await store.readAll()).map(
    parseWatcherProverFundingReservationRecord,
  ))
    if (
      record.deploymentFingerprint === target.deploymentFingerprint &&
      record.state === "active" &&
      (await signedReservation(store, record)) &&
      (await handoffsNameTarget(
        store,
        record,
        target.category,
        target.headerHash,
      ))
    )
      orphans.push(record);
  return orphans;
};

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
      // A reservation released by a hand-over keeps no history.
      (record.state === "conflict" ||
        store.hasSignedHistory === undefined ||
        (await signedReservation(store, record)))
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
 * Signed funding for this target whose decision is gone is never run
 * beside: with `orphans`, it is handed over to the fresh execution once
 * each of its attempts reads expired, invalidated or landed from its own
 * bytes, and held until then; without, it is held. Once every attempt is
 * retired past release finality it settles instead. Returns whether an
 * original execution was resumed, the funding to hand over, and the
 * funding settled.
 */
export const resumeWatcherProverFundingWithoutJournal = async (
  input: Readonly<{
    journalRoot: string;
    journalAuthenticationKey: Uint8Array;
    actuationPermit: WorkflowActuationPermit;
    category: FraudProofCatalogueCategoryName;
    rollbackGeneration: string;
    store: WatcherProverFundingReservationStore;
    orphans?: WatcherOrphanedReservations;
  }>,
  authority: Readonly<{
    deploymentFingerprint: string;
    headerHash: string;
    launchScope: HeaderFaultDecision["launchScope"];
  }>,
): Promise<
  Readonly<{
    resumed: boolean;
    handOver: readonly WatcherProverFundingReservationRecord[];
    settled: readonly WatcherProverFundingReservationRecord[];
  }>
> => {
  const records = (await input.store.readAll())
    .map(parseWatcherProverFundingReservationRecord)
    .filter(
      (record) =>
        record.deploymentFingerprint === authority.deploymentFingerprint,
    );
  if (records.length === 0)
    return { resumed: false, handOver: [], settled: [] };
  const decisions = await (
    await openWatcherFaultDecisionJournal({
      directory: input.journalRoot,
      deploymentFingerprint: authority.deploymentFingerprint,
      launchScope: installedLaunchScope(authority.launchScope),
      authenticationKey: input.journalAuthenticationKey,
    })
  ).readAll();
  const originals: HeaderFaultDecision[] = [];
  const orphans: WatcherProverFundingReservationRecord[] = [];
  for (const record of records) {
    const matches = decisions.filter(
      ({ decision }) => decision.decisionDigest === record.decisionDigest,
    );
    const decision = matches[0]?.decision;
    const signed = await signedReservation(input.store, record);
    // Signed funding for this target whose decision is gone is never run
    // beside. Its handoffs are unauthenticated: they only ever hold, or hand
    // its attempts over to be excluded or adopted.
    if (
      signed &&
      (matches.length !== 1 || decision?.decision !== "fault_detected") &&
      (await handoffsNameTarget(
        input.store,
        record,
        input.category,
        authority.headerHash,
      ))
    ) {
      if (input.orphans === undefined || record.state !== "active")
        throw watcherProverFundingDecisionMissing(
          input.category,
          authority.headerHash,
          record.decisionDigest,
        );
      orphans.push(record);
      continue;
    }
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
  // The resumed execution keeps its own funding; nothing is handed to it.
  if (original !== undefined && orphans[0] !== undefined)
    throw watcherProverFundingDecisionMissing(
      input.category,
      authority.headerHash,
      orphans[0].decisionDigest,
    );
  if (original === undefined)
    return {
      resumed: false,
      ...((await input.orphans?.resolveTarget({
        target: `${input.category}/${authority.headerHash}`,
        orphans,
        handOver: true,
      })) ?? { handOver: [], settled: [] }),
    };
  bindWorkflowActuationRecoveryIdentity({
    permit: input.actuationPermit,
    category: input.category,
    rollbackGeneration: input.rollbackGeneration,
    originalDecision: original,
  });
  return { resumed: true, handOver: [], settled: [] };
};
