import {
  deriveFraudProofRawL1CompletedTerminal,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
} from "@al-ft/midgard-fault-proofs";
import { fixture as terminalFixture } from "@al-ft/midgard-fault-proofs/test-support/raw-l1-terminal-fixture";

import { watcherDeploymentReleaseEconomicsAuthority } from "../../src/runtime/deployment-identity.js";
import {
  deploymentIdentity,
  finality,
  key,
  type setupFundingRecoveryFixture,
} from "./fault-proof-funding-fixture.js";

/**
 * The raw L1 terminal of a funding-recovery fixture's correction, and the
 * writer that journals its confirmed actions and completion, as a run that
 * reached the chain would.
 */
export const completedTerminalWriter = async (
  fixture: Awaited<ReturnType<typeof setupFundingRecoveryFixture>>,
) => {
  const journal = new DirectoryFraudProofWorkflowJournalStore(
    fixture.journalDirectory,
  );
  const append = async (event: FraudProofWorkflowJournalEvent) => {
    const entries = await journal.load(fixture.initial.workflowId);
    await journal.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId: fixture.initial.workflowId,
        identity: fixture.initial.identity,
        sequence: entries.length,
        recordedAt: new Date().toISOString(),
        event,
      },
      entries.length,
    );
  };
  const verifiedFinality = await finality.verifyForWorkflow({
    deploymentFingerprint: deploymentIdentity.manifestId,
  });
  const verifiedEconomics = await watcherDeploymentReleaseEconomicsAuthority(
    deploymentIdentity,
  ).verifyForWorkflow({ deploymentFingerprint: deploymentIdentity.manifestId });
  const raw = await terminalFixture({
    proofCreation: true,
    header: fixture.fixture.header,
    deploymentFingerprint: deploymentIdentity.manifestId,
    blueprintHash: verifiedFinality.blueprintHash,
    verifiedFinality,
    verifiedEconomics,
    proverCredential: key.to_public().hash().to_hex(),
    confirmationDepth: verifiedFinality.policy.automaticRecoveryMaxDepth + 2,
  });
  const terminal = await deriveFraudProofRawL1CompletedTerminal({
    snapshot: raw.snapshot,
    definition: raw.definition,
    releaseEconomics: verifiedEconomics,
  });
  if (terminal === null)
    throw new Error("raw fixture omitted completed correction");
  const writeTerminal = async () => {
    for (const txHash of [
      terminal.proofToken.createdByTxHash,
      terminal.correction.removalTxHash,
    ]) {
      const action = { actionId: txHash, txHash };
      for (const event of [
        {
          kind: "preflight_passed",
          ...action,
          localEvaluator: "fixture-local-uplc",
          referenceScripts: [],
        },
        { kind: "submission_intent", ...action, attempt: 1, actionInput: {} },
        { kind: "submitted", ...action, attempt: 1 },
        { kind: "reconciled", ...action, outcome: "confirmed" },
        { kind: "confirmed", ...action },
      ] as const)
        await append(event);
    }
    await append({
      kind: "completed",
      terminal,
      terminalDigest: journalJsonDigest(terminal),
    });
  };
  return { raw, writeTerminal };
};
