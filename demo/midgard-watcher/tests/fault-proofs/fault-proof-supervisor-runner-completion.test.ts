import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  journalJsonDigest,
  normalizeJournalJson,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it, vi } from "vitest";

import type { WatcherProofExecution } from "../../src/fault-proofs/fault-proof-objective-journal.js";
import { admitWatcherProofRunnerCompletion } from "../../src/fault-proofs/fault-proof-supervisor.admit-runner-completion.js";
import { fundingTerminal } from "../funding/funding-handoff-fixture.js";

// The verifier is the authority seam here; these tests prove it is reached
// before the supervisor caches completion or releases a dependent objective.
const job = {
  mode: "run" as const,
  category: "doubleSpend" as const,
  headerHash: "11".repeat(28),
  decisionDigest: "22".repeat(32),
  rollbackGeneration: "0",
  deadline: null,
};
const identity = {
  schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  deploymentFingerprint: "33".repeat(32),
  category: job.category,
  target: { kind: "state_queue_header" as const, headerHash: job.headerHash },
  decisionDigest: job.decisionDigest,
};
const terminal = fundingTerminal(
  job.headerHash,
  "aa".repeat(32),
  "bb".repeat(32),
);
const workflowId = computeFraudProofWorkflowId(identity);
const execution: WatcherProofExecution = {
  workflowId,
  entries: [
    {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      recordedAt: "2026-10-01T00:00:00.000Z",
      sequence: 0,
      event: {
        kind: "completed",
        terminal,
        terminalDigest: journalJsonDigest(normalizeJournalJson(terminal)),
      },
    },
  ],
};
describe("fresh runner completion admission", () => {
  it("waits for canonical verification before caching completion", async () => {
    let resolve!: (value: { kind: "applicable" }) => void;
    const wait = new Promise<{ kind: "applicable" }>((done) => {
      resolve = done;
    });
    const verifyCompleted = vi.fn(async () => await wait);
    const onApplicable = vi.fn();
    const outcome = { kind: "completed" };
    const completion = admitWatcherProofRunnerCompletion({
      job,
      execution,
      actuationPermit: null,
      verifyCompleted,
      outcome,
      onApplicable,
    });
    expect(verifyCompleted).toHaveBeenCalledWith({
      job,
      execution,
      actuationPermit: null,
    });
    expect(onApplicable).not.toHaveBeenCalled();
    resolve({ kind: "applicable" });
    expect(await completion).toBe(outcome);
    expect(onApplicable).toHaveBeenCalledOnce();
  });
  it("keeps an inclusion-only terminal pending and propagates verifier failures", async () => {
    const onApplicable = vi.fn();
    expect(
      await admitWatcherProofRunnerCompletion({
        job,
        execution,
        actuationPermit: null,
        verifyCompleted: async () => ({
          kind: "pending",
          reason: "release_finality",
        }),
        outcome: { kind: "completed" },
        onApplicable,
      }),
    ).toEqual({
      kind: "pending",
      reason: "release_finality",
      resume: "await_observation",
    });
    await expect(
      admitWatcherProofRunnerCompletion({
        job,
        execution,
        actuationPermit: null,
        verifyCompleted: async () => {
          throw new Error("noncanonical terminal");
        },
        outcome: { kind: "completed" },
        onApplicable,
      }),
    ).rejects.toThrow("noncanonical terminal");
    expect(onApplicable).not.toHaveBeenCalled();
  });
});
