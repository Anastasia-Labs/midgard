/**
 * Our removal of a classified doubleSpend fault as raw L1 shows it once it
 * landed beyond the recovery horizon: the raw view, the terminal derived from
 * it, the completion a run reports, and the application's completion check
 * over a view the caller chooses.
 */
import {
  fixture as terminalFixture,
  rollBackTerminalFixture,
} from "@al-ft/midgard-fault-proofs/test-support/raw-l1-terminal-fixture";
import { verifyCompletedFraudProofWorkflow } from "@al-ft/midgard-fault-proofs/workflow/completed-verification";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
} from "@al-ft/midgard-fault-proofs/workflow/identity";
import { deriveFraudProofRawL1CompletedTerminal } from "@al-ft/midgard-fault-proofs/workflow/raw-l1-family-derivation";
import {
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1Snapshot,
} from "@al-ft/midgard-fault-proofs/workflow/raw-l1-snapshot";

import type { WatcherFaultProofApplication } from "../../src/fault-proofs/fault-proof-application.js";
import { watcherDeploymentReleaseEconomicsAuthority } from "../../src/runtime/deployment-identity.js";
import {
  type classifiedDoubleSpend,
  deploymentIdentity,
  finality,
  key,
} from "./classified-fault-fixture.js";

export const landedRemoval = async (
  classified: Pick<
    Awaited<ReturnType<typeof classifiedDoubleSpend>>,
    "fixture"
  >,
) => {
  const verifiedFinality = await finality.verifyForWorkflow({
    deploymentFingerprint: deploymentIdentity.manifestId,
  });
  const verifiedEconomics = await watcherDeploymentReleaseEconomicsAuthority(
    deploymentIdentity,
  ).verifyForWorkflow({ deploymentFingerprint: deploymentIdentity.manifestId });
  const raw = await terminalFixture({
    proofCreation: true,
    header: classified.fixture.header,
    deploymentFingerprint: deploymentIdentity.manifestId,
    blueprintHash: verifiedFinality.blueprintHash,
    verifiedFinality,
    verifiedEconomics,
    proverCredential: key.to_public().hash().to_hex(),
    // A workflow completes only beyond the recovery horizon (k + 2).
    confirmationDepth: verifiedFinality.policy.automaticRecoveryMaxDepth + 2,
  });
  const derived = await deriveFraudProofRawL1CompletedTerminal({
    snapshot: raw.snapshot,
    definition: raw.definition,
    releaseEconomics: verifiedEconomics,
  });
  if (derived.kind !== "removed")
    throw new Error("raw fixture omitted completed correction");
  const terminal = derived.terminal;
  return {
    raw,
    terminal,
    /** The view before the removal landed: the target still live. */
    targetLive: (): FraudProofRawL1Snapshot => rollBackTerminalFixture(raw),
    /** A run's report that the removal landed, under `decisionDigest`. */
    completed: (decisionDigest: string) =>
      Object.freeze({
        kind: "completed" as const,
        workflowId: computeFraudProofWorkflowId({
          schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
          deploymentFingerprint: deploymentIdentity.manifestId,
          category: "doubleSpend",
          target: {
            kind: "state_queue_header",
            headerHash: raw.definition.headerHash,
          },
          decisionDigest,
        }),
        terminal,
      }),
    /** The application's completion check, reading `capture` as raw L1.
     * The view holds no removal of ours for any other header. */
    verifyCompleted:
      (
        capture: () => Promise<FraudProofRawL1Snapshot>,
        verify: typeof verifyCompletedFraudProofWorkflow = verifyCompletedFraudProofWorkflow,
      ): WatcherFaultProofApplication["verifyCompleted"] =>
      async ({ terminal: reported, decisionDigest, headerHash }) =>
        headerHash !== raw.definition.headerHash
          ? { kind: "pending", reason: "terminal_not_ours" }
          : await verify({
              binding: raw.binding,
              ...(reported === undefined ? {} : { terminal: reported }),
              decisionDigest,
              authority: {
                authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
                capture: async () => ({
                  snapshot: await capture(),
                  pending: [],
                }),
              },
            }),
  };
};
