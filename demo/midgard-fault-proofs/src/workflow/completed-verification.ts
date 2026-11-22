import type { FraudProofTerminalDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowTerminal,
  normalizeFraudProofWorkflowIdentity,
  normalizeWorkflowJson,
  workflowJsonDigest,
} from "./identity.js";
import { normalizeWorkflowTerminal } from "./orchestrator.normalize-workflow-terminal.js";
import { deriveFraudProofRawL1CompletedTerminal } from "./raw-l1-family-derivation.derive-fraud-proof-raw-l1-family-stage.js";
import { fraudProofRawL1SnapshotRequestForFamily } from "./raw-l1-family-derivation.derive-terminal.js";
import { fraudProofRawL1HistoryReader } from "./raw-l1-family-derivation.lost-defense-slash.js";
import { admitFraudProofRawL1Snapshot } from "./raw-l1-snapshot.admit-fraud-proof-raw-l1-snapshot.js";
import {
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1SnapshotAuthority,
} from "./raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";

export type FraudProofCompletedVerification =
  | Readonly<{ kind: "applicable"; terminal: FraudProofWorkflowTerminal }>
  | Readonly<{
      kind: "pending";
      reason:
        | "target_live"
        | "terminal_not_ours"
        | "release_finality"
        | "checkpoint_changed";
    }>;

/**
 * Rechecks a completion against canonical raw L1 without opening a signer,
 * authorizing an action or reserving funding. The completion is derived
 * from the chain; when `terminal` is given (a runner's fresh result) the
 * derived terminal must state the same facts. An applicable result carries
 * the derived terminal, whose removal transaction a completion marker binds.
 */
export const verifyCompletedFraudProofWorkflow = async ({
  binding,
  authority,
  decisionDigest,
  terminal,
}: {
  readonly binding: FraudProofTerminalDeploymentBinding;
  readonly authority: FraudProofRawL1SnapshotAuthority;
  readonly decisionDigest: string;
  readonly terminal?: FraudProofWorkflowTerminal;
}): Promise<FraudProofCompletedVerification> => {
  if (authority.authorityVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY)
    throw new Error("completed workflow changed raw L1 or release authority");
  const identity = normalizeFraudProofWorkflowIdentity({
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    hubOraclePolicyId: binding.hubOraclePolicyId,
    category: binding.definition.category,
    target: {
      kind: "state_queue_header",
      headerHash: binding.definition.headerHash,
    },
    decisionDigest,
  });
  if (terminal !== undefined)
    normalizeWorkflowTerminal({
      identity,
      terminal,
      releaseFinality: binding.releaseFinality,
    });
  const request = fraudProofRawL1SnapshotRequestForFamily({
    definition: binding.definition,
    releaseFinality: binding.releaseFinality,
  });
  const snapshot = admitFraudProofRawL1Snapshot({
    value: (await authority.capture(request)).snapshot,
    request,
    releaseFinality: binding.releaseFinality,
    observationDepth: "inclusion",
  });
  const derived = await deriveFraudProofRawL1CompletedTerminal({
    snapshot,
    definition: binding.definition,
    releaseEconomics: binding.releaseEconomics,
    history: fraudProofRawL1HistoryReader({
      authority,
      releaseFinality: binding.releaseFinality,
      headerHash: binding.definition.headerHash,
    }),
  });
  // Another prover's record or receipt ended the header: our completion
  // does not stand on this view, which is a wait, never a failure.
  if (derived.kind !== "removed")
    return { kind: "pending", reason: derived.kind };
  const observed = derived.terminal;
  const facts = (value: FraudProofWorkflowTerminal) =>
    normalizeWorkflowJson({
      ...value,
      observedAt: { ...value.observedAt, confirmationDepth: 0 },
    });
  if (
    terminal !== undefined &&
    workflowJsonDigest(facts(observed)) !== workflowJsonDigest(facts(terminal))
  )
    throw new Error(
      "completed workflow terminal differs from authenticated L1 facts",
    );
  if (
    !snapshot.transactions.some(
      ({ txHash }) => txHash === observed.proofToken.createdByTxHash,
    ) ||
    !snapshot.transactions.some(
      ({ txHash }) => txHash === observed.correction.removalTxHash,
    )
  )
    throw new Error(
      "completed workflow transactions are not canonically authenticated",
    );
  if (
    observed.observedAt.confirmationDepth <
    binding.releaseFinality.policy.automaticRecoveryMaxDepth + 2
  )
    return { kind: "pending", reason: "release_finality" };
  normalizeWorkflowTerminal({
    identity,
    terminal: observed,
    releaseFinality: binding.releaseFinality,
  });
  return { kind: "applicable", terminal: observed };
};
