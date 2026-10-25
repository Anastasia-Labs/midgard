import { describe, expect, it } from "vitest";

import { withOwnKeyActions } from "../src/validation-trace-dispute/own-key-actions.js";
import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import {
  FraudProofL1RefusedError,
  FraudProofL1UnavailableError,
  type FraudProofSignedTransactionRecovery,
} from "../src/workflow/l1-source.js";
import { type FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import { terminalVerifierOf } from "./support/terminal-verifier.js";
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
    verifier: terminalVerifierOf(async () => {
      throw error;
    }),
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

  // An action our key alone signs, whose build fails.
  const ownKeyBuildFailing = (error: Error) =>
    withOwnKeyActions({
      base: makeAdapter(),
      own: {
        owns: () => true,
        next: async () => undefined,
        build: async () => {
          throw error;
        },
      },
      signedRecovery: {} as FraudProofSignedTransactionRecovery,
      label: "own-key test",
    });

  it.each(failures)(
    "a build of an own-key action hands %s to its caller",
    async (_name, make) => {
      const error = make();
      await expect(
        run({
          evidence: await canonicalEvidence(),
          journal: new MemoryFraudProofWorkflowJournalStore(),
          adapter: ownKeyBuildFailing(error),
        }),
      ).rejects.toBe(error);
    },
  );

  it("a build of an own-key action that fails otherwise is its alarm, not a stall", async () => {
    const result = await run({
      evidence: await canonicalEvidence(),
      journal: new MemoryFraudProofWorkflowJournalStore(),
      adapter: ownKeyBuildFailing(new Error("integrity failure")),
    });
    expect(result).not.toMatchObject({ kind: "stalled" });
    expect(result).toMatchObject({
      alarms: [
        expect.objectContaining({
          actionId: "prove",
          reason: expect.stringContaining("integrity failure"),
        }),
      ],
    });
  });
});
