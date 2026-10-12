import { describe, expect, it } from "vitest";

import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import {
  FraudProofL1RefusedError,
  FraudProofL1UnavailableError,
} from "../src/workflow/l1-source.js";
import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofFamilyWorkflowAdapter,
} from "../src/workflow/orchestrator.js";
import {
  canonicalEvidence,
  makeAdapter,
  run,
} from "./workflow.make-adapter.js";

// An L1 read the source could not answer, or refused, says nothing about the
// fault: every step hands it to the caller to wait on instead of stalling the
// workflow, which a watcher fails closed on.
const failures = [
  [
    "a refused read",
    () => new FraudProofL1RefusedError("beyond_retention", "pruned"),
  ],
  [
    "an unavailable read",
    () => new FraudProofL1UnavailableError("the follower is catching up"),
  ],
] as const;

const steps = {
  reconciliation: (error: Error) => ({
    adapter: makeAdapter({
      reconcile: async () => {
        throw error;
      },
    }),
  }),
  preflight: (error: Error) => ({
    adapter: {
      ...makeAdapter(),
      preflight: async () => {
        throw error;
      },
    } satisfies FraudProofFamilyWorkflowAdapter,
  }),
  "terminal verification": (error: Error) => ({
    adapter: makeAdapter(),
    verifier: {
      verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
      verify: async () => {
        throw error;
      },
    },
  }),
} as const;

describe("fraud-proof workflow L1 wait errors", () => {
  it.each(
    Object.keys(steps).flatMap((step) =>
      failures.map(([name, make]) => [step, name, make] as const),
    ),
  )("%s hands %s to its caller", async (step, _name, make) => {
    const error = make();
    await expect(
      run({
        evidence: await canonicalEvidence(),
        journal: new MemoryFraudProofWorkflowJournalStore(),
        ...steps[step as keyof typeof steps](error),
      }),
    ).rejects.toBe(error);
  });

  it.each(Object.keys(steps))(
    "%s still stalls on any other failure",
    async (step) => {
      const result = await run({
        evidence: await canonicalEvidence(),
        journal: new MemoryFraudProofWorkflowJournalStore(),
        ...steps[step as keyof typeof steps](new Error("integrity failure")),
      });
      expect(result).toMatchObject({
        kind: "stalled",
        reason: expect.stringContaining("integrity failure"),
      });
    },
  );
});
