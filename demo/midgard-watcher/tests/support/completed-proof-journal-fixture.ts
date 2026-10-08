import { join } from "node:path";

import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  computeFraudProofWorkflowId,
  createHeaderClassifier,
  DirectoryFraudProofWorkflowJournalStore,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
  normalizeJournalJson,
} from "@al-ft/midgard-fault-proofs";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";

import { watcherDeploymentReleaseFinalityAuthority } from "../../src/runtime/deployment-identity.js";
import { fundingTerminal } from "../funding/funding-handoff-fixture.js";
import { makeWatcherDeploymentAuthorityFixture } from "./deployment-authority-fixture.js";
import { recordObjectives } from "./watcher-journal-fixture.js";

/** Signed double-spend journals, real enough for the supervisor's journal,
 * decision and reconciliation-permit admission. */
export const deploymentIdentity =
  makeWatcherDeploymentAuthorityFixture().result;
const PROOF_TX = "aa".repeat(32);
const REMOVAL_TX = "bb".repeat(32);

/** `seed` picks the double-spent input, so each seed is a distinct header. */
export const classifyDoubleSpend = async (seed = 91) => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({ spendInputs: [outRefCbor(seed, 0n)], fee: 1n }),
      buildFixtureTransaction({ spendInputs: [outRefCbor(seed, 0n)], fee: 2n }),
    ],
  });
  const observation = {
    ...authenticatedHeaderObservation(fixture),
    confirmationDepth: 30,
  };
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: deploymentIdentity.manifestId,
    replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority:
      watcherDeploymentReleaseFinalityAuthority(deploymentIdentity),
  });
  const decision = await classifyHeader({
    classifier,
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: 30,
      }),
    sources: [
      {
        sourceId: "libp2p-test",
        fetchPayloadByHeaderHash: async () => ({
          ok: true as const,
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: "libp2p-test/peer-a",
            grade: "security" as const,
          },
          sourceId: "libp2p-test",
          sourcePeerId: "peer-a",
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          attempts: [],
        }),
      },
    ],
  });
  if (decision.decision !== "fault_detected")
    throw new Error("fixture must classify a double spend");
  return decision;
};

export type DoubleSpendDecision = Awaited<
  ReturnType<typeof classifyDoubleSpend>
>;

const submissionEvents = (
  actionId: string,
  txHash: string,
): FraudProofWorkflowJournalEvent[] => [
  {
    kind: "preflight_passed",
    actionId,
    txHash,
    localEvaluator: "lucid-local-uplc",
    referenceScripts: [],
  },
  {
    kind: "submission_intent",
    actionId,
    actionInput: { stage: actionId },
    attempt: 1,
    txHash,
  },
  { kind: "submitted", actionId, attempt: 1, txHash },
  { kind: "reconciled", actionId, outcome: "confirmed", txHash },
  { kind: "confirmed", actionId, txHash },
];

/** Writes the execution journal a proof leaves: prepared evidence, both
 * signed submissions and, when `completed`, its durable terminal. */
export const writeExecution = async (
  root: string,
  decision: DoubleSpendDecision,
  completed: boolean,
  preparedOnly = false,
): Promise<void> => {
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: deploymentIdentity.manifestId,
    category: "doubleSpend",
    target: { kind: "state_queue_header", headerHash: decision.headerHash },
    decisionDigest: decision.decisionDigest,
  } as const;
  // Reconciliation admits a signed journal only over this exact evidence.
  const artifact = {
    evidenceBinding: {
      headerHash: decision.headerHash,
      payloadSha256: decision.payloadSha256,
    },
    familyArtifact: { test: true },
  };
  const terminal = fundingTerminal(decision.headerHash, PROOF_TX, REMOVAL_TX);
  const events: FraudProofWorkflowJournalEvent[] = [
    { kind: "started" },
    { kind: "prepared", artifact, artifactDigest: journalJsonDigest(artifact) },
    ...(preparedOnly ? [] : submissionEvents("proof", PROOF_TX)),
    ...(preparedOnly ? [] : submissionEvents("remove", REMOVAL_TX)),
    ...(completed
      ? [
          {
            kind: "completed" as const,
            terminal,
            terminalDigest: journalJsonDigest(normalizeJournalJson(terminal)),
          },
        ]
      : []),
  ];
  // The queue records the objective before its workflow journal exists.
  recordObjectives(root, [
    { category: "doubleSpend", headerHash: decision.headerHash },
  ]);
  const journal = new DirectoryFraudProofWorkflowJournalStore(
    join(root, "fault-proofs", "doubleSpend", decision.headerHash),
  );
  const workflowId = computeFraudProofWorkflowId(identity);
  for (const [sequence, event] of events.entries()) {
    await journal.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        identity,
        workflowId,
        recordedAt: "2026-09-11T00:00:00.000Z",
        sequence,
        event,
      },
      sequence,
    );
  }
};
