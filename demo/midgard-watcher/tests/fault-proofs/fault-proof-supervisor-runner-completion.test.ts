import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
} from "@al-ft/midgard-fault-proofs/workflow/journal";
import { describe, expect, it, vi } from "vitest";

import { admitWatcherProofRunnerCompletion } from "../../src/fault-proofs/fault-proof-supervisor.admit-runner-completion.js";
import { fundingTerminal } from "../funding/funding-handoff-fixture.js";

// The verifier is the authority seam here; these tests prove it is reached
// before the supervisor marks a completion.
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
/** A run's report that it completed, with the terminal it read. */
const completedOutcome = () => ({
  kind: "completed",
  result: { kind: "completed", workflowId, terminal },
});
describe("fresh runner completion admission", () => {
  it("waits for canonical verification of the reported terminal before marking its completion", async () => {
    type Applicable = {
      kind: "applicable";
      confirmationDepth: number;
      terminalTxHash: string;
    };
    let resolve!: (value: Applicable) => void;
    const wait = new Promise<Applicable>((done) => {
      resolve = done;
    });
    const verifyCompleted = vi.fn(async () => await wait);
    const onApplicable = vi.fn();
    const outcome = completedOutcome();
    const completion = admitWatcherProofRunnerCompletion({
      job,
      actuationPermit: null,
      verifyCompleted,
      outcome,
      onApplicable,
    });
    expect(verifyCompleted).toHaveBeenCalledWith({
      job,
      actuationPermit: null,
      terminal,
    });
    expect(onApplicable).not.toHaveBeenCalled();
    resolve({
      kind: "applicable",
      confirmationDepth: 12,
      terminalTxHash: "bb".repeat(32),
    });
    expect(await completion).toBe(outcome);
    expect(onApplicable).toHaveBeenCalledExactlyOnceWith({
      workflowId,
      terminalTxHash: "bb".repeat(32),
      confirmationDepth: 12,
    });
  });
  it("keeps an inclusion-only terminal pending and propagates verifier failures", async () => {
    const onApplicable = vi.fn();
    expect(
      await admitWatcherProofRunnerCompletion({
        job,
        actuationPermit: null,
        verifyCompleted: async () => ({
          kind: "pending",
          reason: "release_finality",
        }),
        outcome: completedOutcome(),
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
        actuationPermit: null,
        verifyCompleted: async () => {
          throw new Error("noncanonical terminal");
        },
        outcome: completedOutcome(),
        onApplicable,
      }),
    ).rejects.toThrow("noncanonical terminal");
    expect(onApplicable).not.toHaveBeenCalled();
  });
  it("refuses a completion that reports no terminal, before any verification", async () => {
    const verifyCompleted = vi.fn();
    await expect(
      admitWatcherProofRunnerCompletion({
        job,
        actuationPermit: null,
        verifyCompleted,
        outcome: { kind: "completed" },
        onApplicable: vi.fn(),
      }),
    ).rejects.toThrow("proof runner reported completion without its terminal");
    expect(verifyCompleted).not.toHaveBeenCalled();
  });
});
