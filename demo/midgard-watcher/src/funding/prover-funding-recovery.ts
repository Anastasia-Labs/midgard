import { readdir, realpath } from "node:fs/promises";
import { join } from "node:path";

import {
  assertMintWorkflowPreparedEvidence,
  assertWorkflowActuationPermitIdentity,
  assertWorkflowFundingAbandonmentHandoffJournal,
  assertWorkflowFundingCompletionHandoffJournal,
  bindWorkflowActuationRecoveryIdentity,
  DirectoryFraudProofWorkflowJournalStore,
  journalJsonDigest,
  lostWorkflowFundingJournalRecords,
  normalizeJournalJson,
  parseWorkflowFundingAbandonmentHandoff,
  parseWorkflowFundingCompletionHandoff,
  parseWorkflowFundingPreparedTransition,
  parseWorkflowFundingSubmissionHandoff,
  reconcileWorkflowFundingSubmissionHandoff,
  type WorkflowActuationPermit,
  workflowFundingAbandonmentJournalLost,
  workflowFundingCompletionJournalLost,
  workflowFundingHandoffJournalLost,
  workflowFundingJournalRecreatable,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { openWatcherFaultDecisionJournal } from "../fault-proofs/fault-decision-journal.js";
import {
  installedLaunchScope,
  type WatcherInstalledWorkflowCategory,
} from "../fault-proofs/fault-proof-application.js";
import { WatcherProofDecisionMissingError } from "../fault-proofs/watcher-decision-hold.js";
import {
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentReleaseFinalityAuthority,
} from "../runtime/deployment-identity.js";
import type { WatcherOrphanedReservations } from "./prover-funding-recovery.orphaned-reservation.js";
import {
  emptyExecutionHistory,
  resumeWatcherProverFundingWithoutJournal,
  targetOrphans,
  watcherProverFundingDecisionMissing,
} from "./prover-funding-recovery.resume-without-journal.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";

/** Resolve the existing execution before selecting wallet inputs. Permission
 * remains bound to the fresh decision; journals and leases retain their
 * identity. Returns the orphaned funding to hand over to the fresh
 * reservation (see `resumeWatcherProverFundingWithoutJournal`). */
export const authorizeWatcherProverFundingRecovery = async (input: {
  readonly journalRoot: string;
  readonly journalAuthenticationKey: Uint8Array;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly category: FraudProofCatalogueCategoryName;
  readonly rollbackGeneration: string;
  readonly store: WatcherProverFundingReservationStore;
  readonly orphans?: WatcherOrphanedReservations;
}): Promise<readonly WatcherProverFundingReservationRecord[]> => {
  const authority = assertWorkflowActuationPermitIdentity({
    permit: input.actuationPermit,
    category: input.category,
    rollbackGeneration: input.rollbackGeneration,
  });
  if (authority.deploymentFingerprint !== input.deploymentIdentity.manifestId)
    throw new Error("workflow recovery changed deployment identity");
  const directory = join(
    input.journalRoot,
    "fault-proofs",
    input.category,
    authority.headerHash,
  );
  // Reconciliation never creates a workflow journal, so it cannot resume
  // one. Signed funding naming its target settles once its attempts retire.
  const withoutJournal = async (history: string | null = null) => {
    if (authority.authority === "reconciliation") {
      await input.orphans?.resolveTarget({
        target: `${input.category}/${authority.headerHash}`,
        orphans: await targetOrphans(input.store, {
          deploymentFingerprint: authority.deploymentFingerprint,
          category: input.category,
          headerHash: authority.headerHash,
        }),
        handOver: false,
      });
      throw new Error(
        "reconciliation funding requires its existing durable workflow",
      );
    }
    // An empty workflow directory with history and nothing to resume holds
    // too, unless that history is handed over to the fresh execution.
    const { resumed, handOver, settled } =
      await resumeWatcherProverFundingWithoutJournal(input, authority);
    if (
      !resumed &&
      history !== null &&
      ![...handOver, ...settled].some(
        ({ decisionDigest }) => decisionDigest === history,
      )
    )
      throw watcherProverFundingDecisionMissing(
        input.category,
        authority.headerHash,
        history,
      );
    return handOver;
  };
  let names;
  try {
    names = await readdir(directory, { withFileTypes: true });
  } catch (error) {
    if (error instanceof Error && "code" in error && error.code === "ENOENT")
      return await withoutJournal();
    throw error;
  }
  if ((await realpath(directory)) !== directory)
    throw new Error("workflow recovery journal traverses a symlink");
  if (
    names.some(
      (entry) => !entry.isDirectory() || !/^[0-9a-f]{64}$/u.test(entry.name),
    )
  )
    throw new Error(
      "workflow recovery target contains an invalid journal entry",
    );
  const candidates: typeof names = [];
  let history: string | null = null;
  for (const candidate of names) {
    const candidateDirectory = join(directory, candidate.name);
    if ((await realpath(candidateDirectory)) !== candidateDirectory)
      throw new Error("workflow recovery execution traverses a symlink");
    // append creates its directory before validating the first event. A literally
    // empty one is pre-start debris, or with durable funding history a journal
    // lost before its first record. Any other needs strict journal recovery.
    if ((await readdir(candidateDirectory)).length !== 0) {
      candidates.push(candidate);
      continue;
    }
    history =
      (await emptyExecutionHistory(input, authority, candidate.name)) ??
      history;
  }
  if (history !== null && candidates.length !== 0)
    throw new Error("empty workflow directory has durable funding history");
  if (candidates.length > 1)
    throw new Error(
      "workflow recovery has multiple candidate executions for one target",
    );
  const candidate = candidates[0];
  if (candidate === undefined) return await withoutJournal(history);
  const entries = await new DirectoryFraudProofWorkflowJournalStore(
    directory,
  ).load(candidate.name);
  const identity = entries[0]?.identity;
  if (
    identity === undefined ||
    identity.decisionDigest === undefined ||
    identity.deploymentFingerprint !== authority.deploymentFingerprint ||
    identity.category !== input.category ||
    identity.target.kind !== "state_queue_header" ||
    identity.target.headerHash !== authority.headerHash
  )
    throw new Error(
      "workflow recovery journal has a foreign or missing execution identity",
    );
  if (entries.some(({ event }) => event.kind === "completed"))
    throw new Error(
      "workflow recovery refuses a new execution over a completed workflow",
    );
  const decisions = await openWatcherFaultDecisionJournal({
    directory: input.journalRoot,
    deploymentFingerprint: authority.deploymentFingerprint,
    launchScope: installedLaunchScope(authority.launchScope),
    authenticationKey: input.journalAuthenticationKey,
  });
  const matches = (await decisions.readAll()).filter(
    ({ decision }) => decision.decisionDigest === identity.decisionDigest,
  );
  if (
    matches.length !== 1 ||
    matches[0]!.decision.decision !== "fault_detected"
  )
    throw new WatcherProofDecisionMissingError({
      kind: "objective",
      category: input.category as WatcherInstalledWorkflowCategory,
      headerHash: authority.headerHash,
      decisionDigest: identity.decisionDigest,
      detail: `${input.category}/${authority.headerHash}: workflow recovery has no unique original fault decision (${identity.decisionDigest})`,
    });
  const originalDecision = matches[0]!.decision;
  const { decisionDigest, ...unsealed } = originalDecision;
  if (
    journalJsonDigest(normalizeJournalJson(unsealed)) !== decisionDigest ||
    originalDecision.deploymentFingerprint !== identity.deploymentFingerprint ||
    originalDecision.category !== identity.category ||
    originalDecision.headerHash !== identity.target.headerHash
  )
    throw new Error("workflow recovery changed its original fault identity");
  const prepared = entries.find(({ event }) => event.kind === "prepared");
  if (prepared?.event.kind === "prepared") {
    const envelope = prepared.event.artifact;
    const binding = envelope.evidenceBinding;
    const finality = await watcherDeploymentReleaseFinalityAuthority(
      input.deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: authority.deploymentFingerprint,
    });
    // Mint's immutable prepared artifact predates the generic envelope. Its
    // exact family evidence is bound to the sealed decision and signed attempts.
    // The verified manifest identity pins release finality, and the reservation
    // below must carry that same deployment and decision. The funding factory
    // separately revalidates its exact policy and reservation basis before use.
    if (input.category === "mintItemNonCanonical") {
      assertMintWorkflowPreparedEvidence(originalDecision, entries);
    } else if (
      binding === null ||
      typeof binding !== "object" ||
      Array.isArray(binding) ||
      !("headerHash" in binding) ||
      !("payloadEnvelopeSha256" in binding) ||
      !("payloadSha256" in binding) ||
      binding.headerHash !== originalDecision.headerHash ||
      binding.payloadEnvelopeSha256 !==
        originalDecision.payloadEnvelopeSha256 ||
      binding.payloadSha256 !== originalDecision.payloadSha256 ||
      journalJsonDigest(normalizeJournalJson(envelope.releaseFinality)) !==
        journalJsonDigest(normalizeJournalJson(finality))
    )
      throw new Error(
        "workflow recovery prepared artifact differs from its original decision or release identity",
      );
  }
  const reservations = (await input.store.readAll())
    .map(parseWatcherProverFundingReservationRecord)
    .filter(
      (record) =>
        record.deploymentFingerprint === authority.deploymentFingerprint &&
        record.decisionDigest === identity.decisionDigest,
    );
  if (reservations.length !== 1 || reservations[0]!.state === "conflict")
    throw new Error(
      "workflow recovery has no unique non-conflicted original funding reservation",
    );
  const record = reservations[0]!;
  const reserved = { reservationId: record.reservationId };
  const stored = await input.store.readCompletionHandoff(reserved);
  const completion =
    stored === null ? null : parseWorkflowFundingCompletionHandoff(stored);
  // The runner rebuilds a lost journal from the store's signed attempts.
  const records = workflowFundingJournalRecreatable(entries)
    ? lostWorkflowFundingJournalRecords({
        workflowId: candidate.name,
        abandoned:
          (await input.store.readLegacyAbandonedTransactions?.(reserved)) ?? [],
        submissions:
          (await input.store.readRecordedSubmissions?.(reserved)) ?? [],
        completion,
      })
    : [];
  if (record.state === "released") {
    const handoff = parseWorkflowFundingCompletionHandoff(stored);
    if (!workflowFundingCompletionJournalLost(handoff, entries, records))
      assertWorkflowFundingCompletionHandoffJournal({ handoff, entries });
    // The runner may only reverify and persist this exact completion; its
    // released funding permit cannot select inputs or prepare another action.
  }
  const abandoned = await input.store.readAbandonmentHandoff(reserved);
  if (abandoned !== null) {
    if (
      typeof abandoned !== "object" ||
      Array.isArray(abandoned) ||
      Object.keys(abandoned).sort().join(",") !== "handoff,transition" ||
      !("handoff" in abandoned) ||
      !("transition" in abandoned)
    )
      throw new Error("workflow recovery abandonment handoff is malformed");
    const handoff = parseWorkflowFundingAbandonmentHandoff(abandoned.handoff);
    const transition = parseWorkflowFundingPreparedTransition(
      abandoned.transition,
    );
    if (
      transition.transactionHash !== handoff.reconciliation.txHash ||
      record.pendingTransition !== null
    )
      throw new Error(
        "workflow recovery abandonment differs from its exact signed transaction",
      );
    if (!workflowFundingAbandonmentJournalLost(handoff, entries, records))
      assertWorkflowFundingAbandonmentHandoffJournal({ handoff, entries });
  }
  const pending = record.pendingTransition;
  if (pending !== null) {
    const intent = [...entries]
      .reverse()
      .find(({ event }) => event.kind === "submission_intent");
    const recovered: unknown = await input.store.readPendingHandoff(reserved);
    if (recovered !== null) {
      if (
        typeof recovered !== "object" ||
        Array.isArray(recovered) ||
        Object.keys(recovered).sort().join(",") !== "handoff,transition" ||
        !("handoff" in recovered) ||
        !("transition" in recovered)
      )
        throw new Error("workflow recovery submission handoff is malformed");
      const handoff = parseWorkflowFundingSubmissionHandoff(recovered.handoff);
      const transition = parseWorkflowFundingPreparedTransition(
        recovered.transition,
      );
      // A lost journal is re-recorded from this exact handoff by the runner.
      if (!workflowFundingHandoffJournalLost(handoff, entries, records))
        reconcileWorkflowFundingSubmissionHandoff({ handoff, entries });
      if (
        journalJsonDigest(normalizeJournalJson(transition)) !==
        pending.transitionDigest
      )
        throw new Error(
          "workflow recovery handoff changed its exact signed transaction",
        );
      if (
        intent?.event.kind === "submission_intent" &&
        intent.event.txHash === pending.transactionHash &&
        journalJsonDigest(normalizeJournalJson(intent.event)) !==
          journalJsonDigest(normalizeJournalJson(handoff.submissionIntent))
      )
        throw new Error(
          "workflow recovery pending funding lineage differs from its recorded transaction intent",
        );
    } else if (
      prepared === undefined ||
      intent?.event.kind !== "submission_intent" ||
      intent.event.txHash !== pending.transactionHash
    ) {
      // Existing signed intents already contain their action identity. A
      // missing intent requires the transactionally persisted handoff above.
      throw new Error(
        "workflow recovery pending funding lineage differs from its recorded transaction intent",
      );
    }
  }
  bindWorkflowActuationRecoveryIdentity({
    permit: input.actuationPermit,
    category: input.category,
    rollbackGeneration: input.rollbackGeneration,
    originalDecision,
  });
  return [];
};

export { releaseUnusedWatcherProverFundingReservations } from "./prover-funding-recovery.release-unused.js";
