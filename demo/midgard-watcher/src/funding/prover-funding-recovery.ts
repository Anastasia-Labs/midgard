import type { Dirent } from "node:fs";
import { readdir, realpath } from "node:fs/promises";
import { join } from "node:path";

import {
  assertMintWorkflowPreparedEvidence,
  assertWorkflowActuationPermitIdentity,
  assertWorkflowFundingAbandonmentHandoffJournal,
  assertWorkflowFundingCompletionHandoffJournal,
  bindWorkflowActuationRecoveryIdentity,
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  journalJsonDigest,
  normalizeJournalJson,
  parseWorkflowFundingAbandonmentHandoff,
  parseWorkflowFundingCompletionHandoff,
  parseWorkflowFundingPreparedTransition,
  parseWorkflowFundingSubmissionHandoff,
  reconcileWorkflowFundingSubmissionHandoff,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { openWatcherFaultDecisionJournal } from "../fault-proofs/fault-decision-journal.js";
import type { WatcherInstalledWorkflowCategory } from "../fault-proofs/fault-proof-application.js";
import {
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentReleaseFinalityAuthority,
} from "../runtime/deployment-identity.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";

/** Resolve the existing execution before selecting wallet inputs. Permission
 * remains bound to the fresh decision; journals and leases retain their identity. */
export const authorizeWatcherProverFundingRecovery = async (input: {
  readonly journalRoot: string;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly category: FraudProofCatalogueCategoryName;
  readonly rollbackGeneration: string;
  readonly store: WatcherProverFundingReservationStore;
}): Promise<void> => {
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
  let names;
  try {
    names = await readdir(directory, { withFileTypes: true });
  } catch (error) {
    if (error instanceof Error && "code" in error && error.code === "ENOENT") {
      if (authority.authority === "reconciliation")
        throw new Error(
          "reconciliation funding requires its existing durable workflow",
        );
      return;
    }
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
  for (const candidate of names) {
    const candidateDirectory = join(directory, candidate.name);
    if ((await realpath(candidateDirectory)) !== candidateDirectory)
      throw new Error("workflow recovery execution traverses a symlink");
    // append creates its directory before validating the first event. Only a
    // literally empty directory is pre-start debris: fsynced temporary files
    // and every other nonempty directory still require strict journal recovery.
    if ((await readdir(candidateDirectory)).length !== 0) {
      candidates.push(candidate);
      continue;
    }
    const reservations = (await input.store.readAll())
      .map(parseWatcherProverFundingReservationRecord)
      .filter(
        (record) =>
          computeFraudProofWorkflowId({
            schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
            deploymentFingerprint: record.deploymentFingerprint,
            category: input.category,
            target: {
              kind: "state_queue_header",
              headerHash: authority.headerHash,
            },
            decisionDigest: record.decisionDigest,
          }) === candidate.name,
      );
    for (const record of reservations) {
      if (
        record.state !== "active" ||
        record.pendingTransition !== null ||
        record.lastConfirmedTransitionDigest !== null ||
        input.store.hasSignedHistory === undefined ||
        (await input.store.hasSignedHistory({
          reservationId: record.reservationId,
        }))
      )
        throw new Error("empty workflow directory has durable funding history");
    }
  }
  if (candidates.length > 1)
    throw new Error(
      "workflow recovery has multiple candidate executions for one target",
    );
  const candidate = candidates[0];
  if (candidate === undefined) {
    if (authority.authority === "reconciliation")
      throw new Error(
        "reconciliation funding requires its existing durable workflow",
      );
    return;
  }
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
    launchScope: authority.launchScope,
  });
  const matches = (await decisions.readAll()).filter(
    ({ decision }) => decision.decisionDigest === identity.decisionDigest,
  );
  if (
    matches.length !== 1 ||
    matches[0]!.decision.decision !== "fault_detected"
  )
    throw new Error("workflow recovery has no unique original fault decision");
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
  if (record.state === "released") {
    const completion = parseWorkflowFundingCompletionHandoff(
      await input.store.readCompletionHandoff({
        reservationId: record.reservationId,
      }),
    );
    assertWorkflowFundingCompletionHandoffJournal({
      handoff: completion,
      entries,
    });
    // The runner may only reverify and persist this exact completion; its
    // released funding permit cannot select inputs or prepare another action.
  }
  const abandoned = await input.store.readAbandonmentHandoff({
    reservationId: record.reservationId,
  });
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
    assertWorkflowFundingAbandonmentHandoffJournal({ handoff, entries });
  }
  const pending = record.pendingTransition;
  if (pending !== null) {
    const intent = [...entries]
      .reverse()
      .find(({ event }) => event.kind === "submission_intent");
    const recovered: unknown = await input.store.readPendingHandoff({
      reservationId: record.reservationId,
    });
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
};

/** Reclaim unsubmitted work after its runner exits, or before startup dispatch. */
export const releaseUnusedWatcherProverFundingReservations = async (input: {
  readonly journalRoot: string;
  readonly launchScope: readonly WatcherInstalledWorkflowCategory[];
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly store: WatcherProverFundingReservationStore;
  readonly reservation?: WatcherProverFundingReservationRecord;
}): Promise<void> => {
  const records = (
    input.reservation === undefined
      ? await input.store.readAll()
      : [input.reservation]
  )
    .map(parseWatcherProverFundingReservationRecord)
    .filter(
      (record) =>
        record.deploymentFingerprint === input.deploymentIdentity.manifestId &&
        record.state === "active" &&
        record.activeInputs.length !== 0 &&
        record.pendingTransition === null &&
        record.lastConfirmedTransitionDigest === null,
    );
  if (records.length === 0) return;
  if (input.store.releaseUnused === undefined)
    throw new Error("prover funding store cannot release unused reservations");
  const decisions = await openWatcherFaultDecisionJournal({
    directory: input.journalRoot,
    deploymentFingerprint: input.deploymentIdentity.manifestId,
    launchScope: input.launchScope,
  });
  const saved = await decisions.readAll();
  for (const record of records) {
    const matches = saved.filter(
      ({ decision }) => decision.decisionDigest === record.decisionDigest,
    );
    const decision = matches[0]?.decision;
    if (matches.length !== 1 || decision?.decision !== "fault_detected")
      throw new Error(
        "unused prover reservation has no unique original fault decision",
      );
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
        entries.some(({ event }) => event.kind === "submission_intent")
      )
        submitted = true;
    }
    if (!submitted) await input.store.releaseUnused(record);
  }
};
