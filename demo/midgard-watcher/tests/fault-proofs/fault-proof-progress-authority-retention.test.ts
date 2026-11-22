import { afterEach, describe, expect, it, vi } from "vitest";

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
// decision journal reader returns minimal decisions; only the pin and
// release calls are under test.
const journalState = vi.hoisted(() => ({ headers: [] as string[] }));
const digestOf = (headerHash: string): string => headerHash.padEnd(64, "d");
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
const hubOraclePolicyId = "ab".repeat(28);
/** A verified completion of the objective at `confirmationDepth`. */
const doneAt = (headerHash: string, confirmationDepth: number) => ({
  workflowId: `workflow-${headerHash}`,
  terminalTxHash: headerHash.padEnd(64, "e"),
  confirmationDepth,
});

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
    holdEvents: async () => ({ kind: "held" }),
    passAtTip: async () => false,
    lifted: () => false,
    releaseUnowned: async () => [],
    pinned: async () => pinned,
    blockDepth: async () => null,
    degradations: () => [],
    readiness: () => [],
  };
  return { retention, pinned, released };
};

/** A watcher restarting over objective rows, marked or not. */
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
        doneAt(objective.headerHash, BEYOND),
        K,
      );
  }
  const recorded = recordingRetention(k);
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot,
    hubOraclePolicyId,
    categories: ["doubleSpend"],
    authenticationKey: TEST_JOURNAL_KEY,
    retention: recorded.retention,
  });
  await authority.admit({
    observation: progressObservation({ hubOraclePolicyId }),
    rollbackGeneration: "0",
  });
  return { authority, targets: objectives, ...recorded };
};

describe("proof progress holds each open objective's L1 history", () => {
  it("releases a marked completion's hold on restart, never pinning it", async () => {
    const restart = await restartOver(2, true);
    expect(restart.pinned).toEqual([]);
    expect(restart.released).toEqual(restart.targets);
  });

  it("releases the hold once the completion is marked beyond recovery", async () => {
    const restart = await restartOver(1, false);
    const [target] = restart.targets;
    await restart.authority.markCompleted(
      target,
      doneAt(target.headerHash, BEYOND),
    );
    expect(restart.released).toEqual([target]);
  });

  it("uses the deployment's k: with k above 2,160 a completion 2,161 deep keeps its hold, and one k + 1 deep releases it", async () => {
    const k = 3_000;
    const restart = await restartOver(1, false, k);
    const [target] = restart.targets;
    await restart.authority.markCompleted(
      target,
      doneAt(target.headerHash, BEYOND),
    );
    expect(restart.released).toEqual([]);
    const again = await restartOver(1, false, k);
    const [next] = again.targets;
    await again.authority.markCompleted(next, doneAt(next.headerHash, k + 1));
    expect(again.released).toEqual([next]);
  });

  it("keeps the hold of a completion marked under a smaller k, never pinning it again", async () => {
    // Not final under this k: its row is forgotten, and only a pass at tip
    // releases a hold no objective owns.
    const restart = await restartOver(2, true, 3_000);
    expect(restart.released).toEqual([]);
    expect(restart.pinned).toEqual([]);
  });

  it("releases, without verifying again, a completion marked under another k that is deep enough under this one", async () => {
    // Marked 2,161 deep under k = 2,160; this deployment's k is 2,000.
    const restart = await restartOver(2, true, 2_000);
    expect(restart.pinned).toEqual([]);
    expect(restart.released).toEqual(restart.targets);
  });

  it("keeps the hold when the completion is verified below recovery depth", async () => {
    const restart = await restartOver(1, false);
    const [target] = restart.targets;
    await restart.authority.markCompleted(
      target,
      doneAt(target.headerHash, BEYOND - 1),
    );
    expect(restart.released).toEqual([]);
  });
});
