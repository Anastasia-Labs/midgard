import { mkdir, mkdtemp, realpath, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it, vi } from "vitest";

import { markWatcherProofCompletionBeyondRecovery } from "../../src/fault-proofs/fault-proof-completion-marker.js";
import type { WatcherProofExecution } from "../../src/fault-proofs/fault-proof-objective-journal.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import { WATCHER_ROLLBACK_BOUNDS } from "../../src/l1/rollback-engine/types.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";

// The recovery bound is a count over durable journal directories. Thousands of
// genuine signed journals would make this test the slowest in the package, so
// both journal readers return minimal records with a real journal's event
// shape, and reconciliation admission returns an opaque permit; only the
// counting is under test.
const journalState = vi.hoisted(() => ({
  headers: [] as string[],
  completed: true,
}));
const digestOf = (headerHash: string): string => headerHash.padEnd(64, "d");
vi.mock("@al-ft/midgard-fault-proofs", async (importOriginal) => ({
  ...(await importOriginal()),
  createWorkflowReconciliationPermitController: () => ({
    permit: Object.freeze({ reconciliation: true }),
  }),
}));
vi.mock("../../src/fault-proofs/fault-decision-journal.js", () => ({
  openWatcherFaultDecisionJournal: async () => ({
    readAll: async () =>
      journalState.headers.map((headerHash) => ({
        decision: {
          decision: "fault_detected",
          category: "doubleSpend",
          headerHash,
          decisionDigest: digestOf(headerHash),
        },
      })),
  }),
}));
const executionOf = (headerHash: string): WatcherProofExecution => {
  const identity = { decisionDigest: digestOf(headerHash) };
  const events = [
    { kind: "started" },
    { kind: "prepared" },
    { kind: "preflight_passed", actionId: "proof" },
    { kind: "submission_intent", actionId: "proof", attempt: 1 },
    ...(journalState.completed ? [{ kind: "completed" }] : []),
  ];
  return {
    workflowId: `workflow-${headerHash}`,
    entries: events.map((event) => ({ identity, event })),
  } as unknown as WatcherProofExecution;
};
vi.mock(
  "../../src/fault-proofs/fault-proof-objective-journal.js",
  async (importOriginal) => ({
    ...(await importOriginal()),
    readWatcherProofExecution: async ({
      objective,
    }: {
      objective: { headerHash: string };
    }) => executionOf(objective.headerHash),
  }),
);

const MAX_OBJECTIVES = 2_048;
const deploymentFingerprint = "ab".repeat(32);
const roots: string[] = [];
afterEach(async () => {
  await Promise.all(
    roots.splice(0).map((root) => rm(root, { recursive: true, force: true })),
  );
});

/** A watcher restarting over `count` durable proof directories. */
const restartOver = async (
  count: number,
  completed: boolean,
  marked = false,
) => {
  const journalRoot = await realpath(
    await mkdtemp(join(tmpdir(), "watcher-progress-bound-")),
  );
  roots.push(journalRoot);
  journalState.headers = Array.from({ length: count }, (_, index) =>
    index.toString(16).padStart(56, "0"),
  );
  journalState.completed = completed;
  await Promise.all(
    journalState.headers.map((headerHash) =>
      mkdir(join(journalRoot, "fault-proofs", "doubleSpend", headerHash), {
        recursive: true,
      }),
    ),
  );
  // Each marker write is fsynced; bounded batches keep this test quick.
  for (let start = 0; marked && start < count; start += 128)
    await Promise.all(
      journalState.headers.slice(start, start + 128).map((headerHash) =>
        markWatcherProofCompletionBeyondRecovery({
          journalRoot,
          deploymentFingerprint,
          objective: { category: "doubleSpend", headerHash },
          execution: executionOf(headerHash),
          confirmationDepth:
            Number(WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth) + 1,
        }),
      ),
    );
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot,
    deploymentFingerprint,
    categories: ["doubleSpend"],
  });
  return {
    authority,
    admit: () =>
      authority.admit({
        observation: progressObservation({ deploymentFingerprint }),
        rollbackGeneration: "0",
      }),
  };
};

describe("proof progress recovery bound", () => {
  it("restarts over more completed proofs than the bound and schedules each for verification", async () => {
    const restart = await restartOver(MAX_OBJECTIVES + 1, true);
    const contexts = await restart.admit();
    expect(contexts).toHaveLength(MAX_OBJECTIVES + 1);
    expect(contexts.every(({ deadline }) => deadline === null)).toBe(true);
    // Each stays indexed until canonical verification retires it.
    expect(restart.authority.unfinishedCount()).toBe(MAX_OBJECTIVES + 1);
  }, 60_000);

  it("restarts over more marked completions than the bound without indexing any", async () => {
    const restart = await restartOver(MAX_OBJECTIVES + 1, true, true);
    await expect(restart.admit()).resolves.toEqual([]);
    expect(restart.authority.unfinishedCount()).toBe(0);
  }, 120_000);

  it("still refuses to restart over more unfinished proofs than the bound", async () => {
    const restart = await restartOver(MAX_OBJECTIVES + 1, false);
    await expect(restart.admit()).rejects.toThrow(
      "proof progress exceeds its recovery bound",
    );
  }, 60_000);
});
