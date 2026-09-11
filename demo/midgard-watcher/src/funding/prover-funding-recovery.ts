import { readdir, realpath } from "node:fs/promises";
import { join } from "node:path";

import {
  assertWorkflowActuationPermitIdentity,
  bindWorkflowActuationRecoveryIdentity,
  DirectoryFraudProofWorkflowJournalStore,
  journalJsonDigest,
  normalizeJournalJson,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { openWatcherFaultDecisionJournal } from "../fault-proofs/fault-decision-journal.js";
import {
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentReleaseFinalityAuthority,
} from "../runtime/deployment-identity.js";
import {
  parseWatcherProverFundingReservationRecord,
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
    if (error instanceof Error && "code" in error && error.code === "ENOENT")
      return;
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
  if (names.length > 1)
    throw new Error(
      "workflow recovery has multiple candidate executions for one target",
    );
  const candidate = names[0];
  if (candidate === undefined) return;
  if (
    (await realpath(join(directory, candidate.name))) !==
    join(directory, candidate.name)
  )
    throw new Error("workflow recovery execution traverses a symlink");
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
  const prepared = entries.find(({ event }) => event.kind === "prepared");
  if (prepared?.event.kind === "prepared") {
    const envelope = prepared.event.artifact;
    const binding = envelope.evidenceBinding;
    const finality = await watcherDeploymentReleaseFinalityAuthority(
      input.deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: authority.deploymentFingerprint,
    });
    if (
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
  if (reservations.length !== 1 || reservations[0]!.state !== "active")
    throw new Error(
      "workflow recovery has no unique active original funding reservation",
    );
  const pending = reservations[0]!.pendingTransition;
  if (pending !== null) {
    const intent = [...entries]
      .reverse()
      .find(({ event }) => event.kind === "submission_intent");
    if (
      prepared === undefined ||
      intent?.event.kind !== "submission_intent" ||
      intent.event.txHash !== pending.transactionHash
    )
      throw new Error(
        "workflow recovery pending funding lineage differs from its recorded transaction intent",
      );
  }
  bindWorkflowActuationRecoveryIdentity({
    permit: input.actuationPermit,
    category: input.category,
    rollbackGeneration: input.rollbackGeneration,
    originalDecision,
  });
};
