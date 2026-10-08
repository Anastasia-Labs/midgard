import { mkdir, mkdtemp, realpath, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it, vi } from "vitest";

import { markWatcherProofCompletionBeyondRecovery } from "../../src/fault-proofs/fault-proof-completion-marker.js";
import type { WatcherProofExecution } from "../../src/fault-proofs/fault-proof-objective-journal.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import { WATCHER_ROLLBACK_BOUNDS } from "../../src/l1/rollback-engine/types.js";
import type {
  WatcherProofRetention,
  WatcherProofRetentionTarget,
} from "../../src/l1-follower/proof-retention.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";

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
const executionOf = (headerHash: string): WatcherProofExecution => {
  const identity = { decisionDigest: digestOf(headerHash) };
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

const deploymentFingerprint = "ab".repeat(32);
const BEYOND = Number(WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth) + 1;
const roots: string[] = [];
afterEach(async () => {
  await Promise.all(
    roots.splice(0).map((root) => rm(root, { recursive: true, force: true })),
  );
});

const recordingRetention = () => {
  const pinned: WatcherProofRetentionTarget[] = [];
  const released: WatcherProofRetentionTarget[] = [];
  const retention: WatcherProofRetention = {
    pin: async ({ category, headerHash }) => {
      pinned.push({ category, headerHash });
    },
    release: async ({ category, headerHash }) => {
      released.push({ category, headerHash });
    },
    holdUnits: async () => undefined,
    pinned: async () => pinned,
  };
  return { retention, pinned, released };
};

/** A watcher restarting over completed proof journals. */
const restartOver = async (count: number, marked: boolean) => {
  const journalRoot = await realpath(
    await mkdtemp(join(tmpdir(), "watcher-progress-retention-")),
  );
  roots.push(journalRoot);
  journalState.headers = Array.from({ length: count }, (_, index) =>
    index.toString(16).padStart(56, "0"),
  );
  for (const headerHash of journalState.headers) {
    await mkdir(join(journalRoot, "fault-proofs", "doubleSpend", headerHash), {
      recursive: true,
    });
    if (marked)
      await markWatcherProofCompletionBeyondRecovery({
        journalRoot,
        deploymentFingerprint,
        objective: { category: "doubleSpend", headerHash },
        execution: executionOf(headerHash),
        confirmationDepth: BEYOND,
      });
  }
  const recorded = recordingRetention();
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot,
    deploymentFingerprint,
    categories: ["doubleSpend"],
    retention: recorded.retention,
  });
  await authority.admit({
    observation: progressObservation({ deploymentFingerprint }),
    rollbackGeneration: "0",
  });
  const targets = journalState.headers.map((headerHash) => ({
    category: "doubleSpend" as const,
    headerHash,
  }));
  return { authority, targets, ...recorded };
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
