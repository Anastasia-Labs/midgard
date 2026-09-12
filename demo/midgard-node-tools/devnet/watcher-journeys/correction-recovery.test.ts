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

it("tolerates a stall the workflow is still retrying inside the allowance", () => {
  const at = (sequence: number, recordedAt: string, reason: string) => ({
    ...entry(sequence, { kind: "stalled", reason }),
    recordedAt,
  });
  const prepared = entry(1, {
    kind: "reconciled",
    actionId: "init:ab#0",
    txHash: "78".repeat(32),
    outcome: "pending",
  });
  const stalls = [
    at(2, "2026-09-12T15:45:25.000Z", "preflight failed for init:ab#0"),
    at(3, "2026-09-12T15:45:26.000Z", "preflight failed for init:ab#0"),
  ];
  const now = Date.parse("2026-09-12T15:47:00.000Z");
  const stall = { now, allowanceMs: 600_000 };
  expect(
    journeyWorkflowUpdates(
      [...baseline, prepared, ...stalls],
      baseline,
      1,
      stall,
    ),
  ).toEqual(stalls);
  expect(() =>
    journeyWorkflowUpdates([...baseline, prepared, ...stalls], baseline, 1, {
      now: now + 600_000,
      allowanceMs: 600_000,
    }),
  ).toThrow("Workflow stalled: preflight failed for init:ab#0");
  const moved = entry(4, {
    kind: "reconciled",
    actionId: "init:cd#0",
    txHash: "56".repeat(32),
    outcome: "pending",
  });
  expect(
    journeyWorkflowUpdates(
      [...baseline, prepared, ...stalls, moved],
      baseline,
      1,
      { now: now + 3_600_000, allowanceMs: 600_000 },
    ),
  ).toEqual([...stalls, moved]);
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
