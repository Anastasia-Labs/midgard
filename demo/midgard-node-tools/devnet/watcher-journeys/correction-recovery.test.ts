import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
} from "@al-ft/midgard-fault-proofs";
import { expect, it } from "vitest";

import { journeyWorkflowUpdates } from "./correction.js";

const identity = {
  schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  deploymentFingerprint: "12".repeat(32),
  category: "transitionTrace",
  target: { kind: "state_queue_header", headerHash: "34".repeat(28) },
} as const;

const entry = (
  sequence: number,
  event: FraudProofWorkflowJournalEntry["event"],
): FraudProofWorkflowJournalEntry => ({
  schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  workflowId: computeFraudProofWorkflowId(identity),
  identity,
  sequence,
  recordedAt: "2026-09-11T05:00:00.000Z",
  event,
});

const baseline = [entry(0, { kind: "stalled", reason: "previous attempt" })];

it("resumes after an immutable historical failure without hiding a new failure", () => {
  expect(journeyWorkflowUpdates(baseline, baseline)).toEqual([]);
  const pending = entry(1, {
    kind: "reconciled",
    actionId: "step_01",
    txHash: "56".repeat(32),
    outcome: "pending",
  });
  expect(journeyWorkflowUpdates([...baseline, pending], baseline)).toEqual([
    pending,
  ]);
  expect(() =>
    journeyWorkflowUpdates(
      [
        ...baseline,
        pending,
        entry(2, { kind: "stalled", reason: "new failure" }),
      ],
      baseline,
      1,
    ),
  ).toThrow("Workflow stalled: new failure");
});

it("rejects a changed or truncated pre-launch journal prefix", () => {
  expect(() => journeyWorkflowUpdates([], baseline)).toThrow();
  expect(() =>
    journeyWorkflowUpdates(
      [entry(0, { kind: "stalled", reason: "rewritten" })],
      baseline,
    ),
  ).toThrow();
});
