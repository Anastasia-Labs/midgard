import { afterEach, describe, expect, it, vi } from "vitest";

import {
  canOpenWatcherProofObjective,
  MAX_OPEN_OBJECTIVES,
  watcherJournalCapacityReached,
} from "../../src/fault-proofs/fault-proof-objective-table.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import {
  closeWatcherJournalDatabase,
  openWatcherJournalDatabase,
} from "../../src/fault-proofs/watcher-journal-database.js";
import { watcherObjectiveScope } from "../../src/fault-proofs/watcher-journal-schema.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import {
  journalDirectory,
  removeJournalDirectories,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";

// L2 (ticket W2): restart cost and the objective cap follow live rows only,
// and a restart remembers nothing an earlier process sent: no row drives
// work by itself. The decision reader returns minimal records; the
// objective table is the real SQLite table.
const journalState = vi.hoisted(() => ({ headers: [] as string[] }));
const digestOf = (headerHash: string): string => headerHash.padEnd(64, "d");
const decisionOf = (headerHash: string) => ({
  decision: "fault_detected",
  category: "doubleSpend",
  headerHash,
  decisionDigest: digestOf(headerHash),
});
vi.mock("../../src/fault-proofs/fault-decision-journal.js", () => ({
  openWatcherFaultDecisionJournal: async () => ({
    readAll: async () =>
      journalState.headers.map((headerHash) => ({
        decision: decisionOf(headerHash),
      })),
    read: async (digest: string) => {
      const headerHash = journalState.headers.find(
        (header) => digestOf(header) === digest,
      );
      return headerHash === undefined
        ? undefined
        : { decision: decisionOf(headerHash) };
    },
  }),
}));

const deploymentFingerprint = "ab".repeat(32);
/** The deployment's k comes from the follower behind the retention. */
const K = storelessProofRetention.securityParameter;
const BEYOND_RECOVERY = K + 1;
afterEach(removeJournalDirectories);

type Seeded = "open" | "completed" | "marked";

/** A watcher restarting over `count` recorded objectives in one state. */
const restartOver = async (count: number, state: Seeded) => {
  const journalRoot = await journalDirectory("watcher-progress-bound");
  journalState.headers = Array.from({ length: count }, (_, index) =>
    index.toString(16).padStart(56, "0"),
  );
  const database = openWatcherJournalDatabase({
    journalRoot,
    authenticationKey: TEST_JOURNAL_KEY,
  });
  database.transaction((tx) => {
    for (const headerHash of journalState.headers) {
      const scope = watcherObjectiveScope("doubleSpend", headerHash);
      tx.put("fault_proof_objectives", {
        key: scope,
        scope,
        state,
        body: {
          category: "doubleSpend",
          headerHash,
          // A marker an earlier release wrote, bound to its workflow
          // record's digest: it still reads as a completion.
          ...(state === "marked"
            ? {
                marker: {
                  workflowId: `workflow-${headerHash}`,
                  journalDigest: "ef".repeat(32),
                  confirmationDepth: BEYOND_RECOVERY,
                  recoveryDepth: K.toString(),
                },
              }
            : {}),
        },
      });
      tx.put("fault_decisions", {
        key: digestOf(headerHash),
        scope,
        state: "fault_detected",
        body: decisionOf(headerHash),
      });
    }
  });
  // A process restart: the next open verifies every journal in full.
  closeWatcherJournalDatabase(journalRoot);
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot,
    deploymentFingerprint,
    categories: ["doubleSpend"],
    authenticationKey: TEST_JOURNAL_KEY,
    retention: storelessProofRetention,
  });
  return {
    journalRoot,
    authority,
    database: () =>
      openWatcherJournalDatabase({
        journalRoot,
        authenticationKey: TEST_JOURNAL_KEY,
      }),
    admit: () =>
      authority.admit({
        observation: progressObservation({ deploymentFingerprint }),
        rollbackGeneration: "0",
      }),
  };
};

describe("proof progress restart over the objective table (L2)", () => {
  it("skips and prunes marked completions: rows and decisions", async () => {
    const restart = await restartOver(MAX_OPEN_OBJECTIVES + 1, "marked");
    await expect(restart.admit()).resolves.toEqual([]);
    expect(restart.authority.unfinishedCount()).toBe(0);
    const database = restart.database();
    expect(database.count("fault_proof_objectives")).toBe(0);
    expect(database.count("fault_decisions")).toBe(0);
  }, 120_000);

  it("forgets an open objective whose job never started", async () => {
    const restart = await restartOver(2, "open");
    await expect(restart.admit()).resolves.toEqual([]);
    expect(restart.authority.unfinishedCount()).toBe(0);
    const database = restart.database();
    expect(database.count("fault_proof_objectives")).toBe(0);
    // Its decision stays: a live fault queues the objective again.
    expect(database.count("fault_decisions")).toBe(2);
  });

  it("forgets the rows of an objective a crash left active once its header has left the queue", async () => {
    const restart = await restartOver(2, "open");
    const headerHash = journalState.headers[0]!;
    const scope = watcherObjectiveScope("doubleSpend", headerHash);
    restart.database().transaction((tx) =>
      tx.put("fault_proof_queue", {
        key: "77".repeat(32),
        scope,
        state: "active",
        body: { identity: { category: "doubleSpend", headerHash } },
      }),
    );
    // No job of this process owns the row, and nothing was signed.
    await restart.admit();
    const database = restart.database();
    expect(database.row("fault_proof_objectives", scope)).toBeUndefined();
    expect(database.row("fault_proof_queue", "77".repeat(32))).toBeUndefined();
    expect(restart.authority.cleanupFailures()).toEqual([]);
  });

  it("restarts over more open objectives than the cap without throwing, and forgetting them frees the cap", async () => {
    const restart = await restartOver(MAX_OPEN_OBJECTIVES + 1, "open");
    expect(watcherJournalCapacityReached(restart.database())).toBe(true);
    await expect(restart.admit()).resolves.toEqual([]);
    const database = restart.database();
    expect(watcherJournalCapacityReached(database)).toBe(false);
    expect(
      canOpenWatcherProofObjective(database, {
        category: "doubleSpend",
        headerHash: "ff".repeat(28),
      }),
    ).toBe(true);
  }, 60_000);
});
