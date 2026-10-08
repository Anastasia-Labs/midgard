import { afterEach, describe, expect, it, vi } from "vitest";

import type { WatcherProofExecution } from "../../src/fault-proofs/fault-proof-objective-journal.js";
import { completeWatcherProofObjective } from "../../src/fault-proofs/fault-proof-objective-table.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
import type {
  WatcherProofRetention,
  WatcherProofRetentionTarget,
} from "../../src/l1-follower/proof-retention.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import {
  journalDirectory,
  recordObjectives,
  removeJournalDirectories,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";

// The progress authority holds each open objective's L1 history (E1 ruling)
// and releases it only once the objective's completion marker holds. The
// journal readers return minimal records with a real journal's event shape,
// as in the recovery-bound test; only the pin and release calls are under
// test.
const journalState = vi.hoisted(() => ({ headers: [] as string[] }));
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
const deploymentFingerprint = "ab".repeat(32);
const executionOf = (headerHash: string): WatcherProofExecution => {
  const identity = {
    deploymentFingerprint,
    category: "doubleSpend",
    target: { kind: "state_queue_header", headerHash },
    decisionDigest: digestOf(headerHash),
  };
  const events = [
    { kind: "started" },
    { kind: "prepared" },
    { kind: "preflight_passed", actionId: "proof" },
    { kind: "submission_intent", actionId: "proof", attempt: 1 },
    { kind: "completed" },
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

/** The mainnet and preprod k; one test runs a deployment with a larger k. */
const K = 2_160;
const BEYOND = K + 1;
afterEach(removeJournalDirectories);

const recordingRetention = (securityParameter: number) => {
  const pinned: WatcherProofRetentionTarget[] = [];
  const released: WatcherProofRetentionTarget[] = [];
  const retention: WatcherProofRetention = {
    securityParameter,
    pin: async ({ category, headerHash }) => {
      pinned.push({ category, headerHash });
      return { kind: "pinned" };
    },
    release: async ({ category, headerHash }) => {
      released.push({ category, headerHash });
    },
    holdUnits: async () => ({ kind: "held" }),
    pinned: async () => pinned,
    degradations: () => [],
  };
  return { retention, pinned, released };
};

/** A watcher restarting over completed proof journals. */
const restartOver = async (count: number, marked: boolean, k = K) => {
  const journalRoot = await journalDirectory("watcher-progress-retention");
  journalState.headers = Array.from({ length: count }, (_, index) =>
    index.toString(16).padStart(56, "0"),
  );
  const objectives = journalState.headers.map((headerHash) => ({
    category: "doubleSpend" as const,
    headerHash,
  }));
  recordObjectives(journalRoot, objectives);
  if (marked) {
    const database = openWatcherJournalDatabase({
      journalRoot,
      authenticationKey: TEST_JOURNAL_KEY,
    });
    for (const objective of objectives)
      completeWatcherProofObjective(
        database,
        objective,
        {
          execution: executionOf(objective.headerHash),
          confirmationDepth: BEYOND,
        },
        K,
      );
  }
  const recorded = recordingRetention(k);
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot,
    deploymentFingerprint,
    categories: ["doubleSpend"],
    authenticationKey: TEST_JOURNAL_KEY,
    retention: recorded.retention,
  });
  await authority.admit({
    observation: progressObservation({ deploymentFingerprint }),
    rollbackGeneration: "0",
  });
  return { authority, targets: objectives, ...recorded };
};

describe("proof progress holds each open objective's L1 history", () => {
  it("pins every restored objective that still awaits verification", async () => {
    const restart = await restartOver(2, false);
    expect(restart.pinned).toEqual(restart.targets);
    expect(restart.released).toEqual([]);
  });

  it("releases a marked completion's hold on restart, never pinning it", async () => {
    const restart = await restartOver(2, true);
    expect(restart.pinned).toEqual([]);
    expect(restart.released).toEqual(restart.targets);
  });

  it("releases the hold once the completion is marked beyond recovery", async () => {
    const restart = await restartOver(1, false);
    const [target] = restart.targets;
    await restart.authority.markCompleted(target, {
      execution: executionOf(target.headerHash),
      confirmationDepth: BEYOND,
    });
    expect(restart.released).toEqual([target]);
  });

  it("uses the deployment's k: with k above 2,160 a completion 2,161 deep keeps its hold, and one k + 1 deep releases it", async () => {
    const k = 3_000;
    const restart = await restartOver(1, false, k);
    const [target] = restart.targets;
    await restart.authority.markCompleted(target, {
      execution: executionOf(target.headerHash),
      confirmationDepth: BEYOND,
    });
    expect(restart.released).toEqual([]);
    const again = await restartOver(1, false, k);
    const [next] = again.targets;
    await again.authority.markCompleted(next, {
      execution: executionOf(next.headerHash),
      confirmationDepth: k + 1,
    });
    expect(again.released).toEqual([next]);
  });

  it("verifies again, and pins, a completion marked under a smaller k", async () => {
    const restart = await restartOver(2, true, 3_000);
    expect(restart.released).toEqual([]);
    expect(restart.pinned).toEqual(restart.targets);
  });

  it("keeps the hold when the completion is verified below recovery depth", async () => {
    const restart = await restartOver(1, false);
    const [target] = restart.targets;
    await restart.authority.markCompleted(target, {
      execution: executionOf(target.headerHash),
      confirmationDepth: BEYOND - 1,
    });
    expect(restart.released).toEqual([]);
  });
});
