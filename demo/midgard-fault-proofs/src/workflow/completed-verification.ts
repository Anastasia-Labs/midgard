import type { FraudProofTerminalDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  normalizeJournalJson,
  validateFraudProofWorkflowJournal,
} from "./journal.js";
import { normalizeWorkflowTerminal } from "./orchestrator.js";
import {
  deriveFraudProofRawL1CompletedTerminal,
  fraudProofRawL1SnapshotRequestForFamily,
} from "./raw-l1-family-derivation.js";
import {
  admitFraudProofRawL1Snapshot,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1SnapshotAuthority,
} from "./raw-l1-snapshot.js";

export type FraudProofCompletedVerification =
  | Readonly<{ kind: "applicable"; terminal: FraudProofWorkflowTerminal }>
  | Readonly<{
      kind: "pending";
      reason: "target_live" | "release_finality" | "checkpoint_changed";
    }>;

/** Recheck durable completion against canonical raw L1 without opening a signer,
 * authorizing an action, reserving funding, or rewriting the completed journal. */
export const verifyCompletedFraudProofWorkflow = async ({
  binding,
  authority,
  entries,
  terminal,
  decisionDigest,
}: {
  readonly binding: FraudProofTerminalDeploymentBinding;
  readonly authority: FraudProofRawL1SnapshotAuthority;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly terminal: FraudProofWorkflowTerminal;
  readonly decisionDigest: string;
}): Promise<FraudProofCompletedVerification> => {
  if (
    authority.authorityVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY ||
    binding.releaseFinality.deploymentIdentityDigest !==
      binding.deploymentFingerprint ||
    binding.releaseEconomics.deploymentIdentityDigest !==
      binding.deploymentFingerprint
  )
    throw new Error("completed workflow changed raw L1 or release authority");
  const first = entries[0];
  if (
    first === undefined ||
    first.identity.decisionDigest !== decisionDigest ||
    first.identity.deploymentFingerprint !== binding.deploymentFingerprint ||
    first.identity.category !== binding.definition.category ||
    first.identity.target.kind !== "state_queue_header" ||
    first.identity.target.headerHash !== binding.definition.headerHash
  )
    throw new Error(
      "completed workflow differs from its exact execution identity",
    );
  validateFraudProofWorkflowJournal({
    workflowId: first.workflowId,
    entries,
    expectedIdentity: first.identity,
  });
  const completed = entries.at(-1)?.event;
  if (
    completed?.kind !== "completed" ||
    journalJsonDigest(normalizeJournalJson(completed.terminal)) !==
      journalJsonDigest(normalizeJournalJson(terminal))
  )
    throw new Error("completed workflow changed its durable terminal");
  normalizeWorkflowTerminal({
    identity: first.identity,
    terminal,
    entries,
    releaseFinality: binding.releaseFinality,
  });
  const request = fraudProofRawL1SnapshotRequestForFamily({
    definition: binding.definition,
    releaseFinality: binding.releaseFinality,
  });
  const snapshot = admitFraudProofRawL1Snapshot({
    value: await authority.capture(request),
    request,
    releaseFinality: binding.releaseFinality,
    observationDepth: "inclusion",
  });
  const observed = await deriveFraudProofRawL1CompletedTerminal({
    snapshot,
    definition: binding.definition,
    releaseEconomics: binding.releaseEconomics,
  });
  if (observed === null) return { kind: "pending", reason: "target_live" };
  const facts = (value: FraudProofWorkflowTerminal) =>
    normalizeJournalJson({
      ...value,
      observedAt: { ...value.observedAt, confirmationDepth: 0 },
    });
  if (journalJsonDigest(facts(observed)) !== journalJsonDigest(facts(terminal)))
    throw new Error(
      "completed workflow terminal differs from authenticated L1 facts",
    );
  if (
    !snapshot.transactions.some(
      ({ txHash }) => txHash === terminal.proofToken.createdByTxHash,
    ) ||
    !snapshot.transactions.some(
      ({ txHash }) => txHash === terminal.correction.removalTxHash,
    )
  )
    throw new Error(
      "completed workflow transactions are not canonically authenticated",
    );
  if (
    observed.observedAt.confirmationDepth <
    binding.releaseFinality.policy.confirmationDepth
  )
    return { kind: "pending", reason: "release_finality" };
  normalizeWorkflowTerminal({
    identity: first.identity,
    terminal: observed,
    entries,
    releaseFinality: binding.releaseFinality,
  });
  return { kind: "applicable", terminal: observed };
};
