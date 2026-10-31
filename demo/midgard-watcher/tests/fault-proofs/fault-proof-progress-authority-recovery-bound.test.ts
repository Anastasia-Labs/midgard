import { DatabaseSync } from "node:sqlite";

import { afterEach, describe, expect, it, vi } from "vitest";

import type { WatcherProofExecution } from "../../src/fault-proofs/fault-proof-objective-journal.js";
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
import { createWatcherWorkflowJournal } from "../../src/fault-proofs/watcher-workflow-journal.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import {
  journalDirectory,
  removeJournalDirectories,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";

// L2 (ticket W2): restart cost and the objective cap follow live rows only.
// Thousands of genuine signed workflow journals would make this the slowest
// file in the package, so the execution and decision readers return minimal
// records with a real journal's event shape, and reconciliation admission
// returns an opaque permit. The objective table is the real SQLite table.
const journalState = vi.hoisted(() => ({
  headers: [] as string[],
  completed: true,
  missing: new Set<string>(),
  changed: new Set<string>(),
}));
const digestOf = (headerHash: string): string => headerHash.padEnd(64, "d");
vi.mock("@al-ft/midgard-fault-proofs", async (importOriginal) => ({
  ...(await importOriginal()),
  createWorkflowReconciliationPermitController: () => ({
    permit: Object.freeze({ reconciliation: true }),
  }),
}));
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
const executionOf = (
  headerHash: string,
  completed = journalState.completed,
): WatcherProofExecution => {
  const identity = {
    category: "doubleSpend",
    target: { kind: "state_queue_header", headerHash },
    decisionDigest: digestOf(headerHash),
  };
  const events = [
    { kind: "started" },
    { kind: "prepared" },
    { kind: "preflight_passed", actionId: "proof" },
    { kind: "submission_intent", actionId: "proof", attempt: 1 },
    ...(completed ? [{ kind: "completed" }] : []),
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
    readWatcherProofExecution: ({
      objective,
    }: {
      objective: { headerHash: string };
    }) => {
      if (journalState.missing.has(objective.headerHash)) return undefined;
      const execution = executionOf(objective.headerHash);
      return journalState.changed.has(objective.headerHash)
        ? { ...execution, workflowId: `replaced-${objective.headerHash}` }
        : execution;
    },
  }),
);

const deploymentFingerprint = "ab".repeat(32);
/** The deployment's k comes from the follower behind the retention. */
const K = storelessProofRetention.securityParameter;
const BEYOND_RECOVERY = K + 1;
afterEach(async () => {
  journalState.missing.clear();
  journalState.changed.clear();
  await removeJournalDirectories();
});

type Seeded = "open" | "completed" | "marked";

/** A watcher restarting over `count` recorded objectives in one state. */
const restartOver = async (count: number, state: Seeded) => {
  const journalRoot = await journalDirectory("watcher-progress-bound");
  journalState.headers = Array.from({ length: count }, (_, index) =>
    index.toString(16).padStart(56, "0"),
  );
  journalState.completed = state !== "open";
  const database = openWatcherJournalDatabase({
    journalRoot,
    authenticationKey: TEST_JOURNAL_KEY,
  });
  database.transaction((tx) => {
    for (const headerHash of journalState.headers) {
      const scope = watcherObjectiveScope("doubleSpend", headerHash);
      const execution = executionOf(headerHash);
      tx.put("fault_proof_objectives", {
        key: scope,
        scope,
        state,
        body: {
          category: "doubleSpend",
          headerHash,
          ...(state === "marked"
            ? {
                marker: {
                  workflowId: execution.workflowId,
                  journalDigest: watcherSha256CanonicalJson(execution.entries),
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
  // The workflow rows themselves; their content is read through the mock.
  const workflowDatabase = new DatabaseSync(":memory:");
  const workflowJournal = createWatcherWorkflowJournal(workflowDatabase);
  const workflowRows = (headerHash: string): number =>
    Number(
      (
        workflowDatabase
          .prepare(
            "SELECT count(*) AS count FROM watcher_workflow_journal_v1 WHERE header_hash = ?",
          )
          .get(headerHash) as { count: number }
      ).count,
    );
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot,
    workflowJournal,
    deploymentFingerprint,
    categories: ["doubleSpend"],
    authenticationKey: TEST_JOURNAL_KEY,
    retention: storelessProofRetention,
  });
  return {
    journalRoot,
    authority,
    workflowDatabase,
    workflowRows,
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
  it("schedules every completed objective not yet beyond recovery, past the open cap", async () => {
    const restart = await restartOver(MAX_OPEN_OBJECTIVES + 1, "completed");
    const contexts = await restart.admit();
    expect(contexts).toHaveLength(MAX_OPEN_OBJECTIVES + 1);
    expect(contexts.every(({ deadline }) => deadline === null)).toBe(true);
    expect(restart.authority.unfinishedCount()).toBe(MAX_OPEN_OBJECTIVES + 1);
    // Completed rows never count toward the cap.
    expect(watcherJournalCapacityReached(restart.database())).toBe(false);
  }, 60_000);

  it("skips and prunes marked completions: rows, decisions and workflow journals", async () => {
    const restart = await restartOver(MAX_OPEN_OBJECTIVES + 1, "marked");
    const pruned = journalState.headers[0]!;
    restart.workflowDatabase
      .prepare(
        "INSERT INTO watcher_workflow_journal_v1 VALUES (?, 0, 'doubleSpend', ?, 'started', 0, '{}')",
      )
      .run("ee".repeat(32), pruned);
    // The execution of this one is already gone: a crash after its workflow
    // rows were deleted and before its objective rows were.
    journalState.missing.add(journalState.headers[1]!);
    await expect(restart.admit()).resolves.toEqual([]);
    expect(restart.authority.unfinishedCount()).toBe(0);
    expect(restart.workflowRows(pruned)).toBe(0);
    const database = restart.database();
    expect(database.count("fault_proof_objectives")).toBe(0);
    expect(database.count("fault_decisions")).toBe(0);
  }, 120_000);

  it("verifies again a marked completion whose execution changed", async () => {
    const restart = await restartOver(2, "marked");
    journalState.changed.add(journalState.headers[0]!);
    const contexts = await restart.admit();
    expect(contexts.map(({ decision }) => decision.headerHash)).toEqual([
      journalState.headers[0],
    ]);
    expect(restart.authority.unfinishedCount()).toBe(1);
    expect(restart.database().count("fault_proof_objectives")).toBe(1);
  });

  it("forgets an open objective whose job never started", async () => {
    const restart = await restartOver(2, "open");
    journalState.missing.add(journalState.headers[0]!);
    const contexts = await restart.admit();
    expect(contexts.map(({ decision }) => decision.headerHash)).toEqual([
      journalState.headers[1],
    ]);
    const database = restart.database();
    expect(database.count("fault_proof_objectives")).toBe(1);
    // Its decision stays: a live fault queues the objective again.
    expect(database.count("fault_decisions")).toBe(2);
  });

  it("forgets the rows of an objective a crash left active with no execution once its header has left the queue", async () => {
    const restart = await restartOver(2, "open");
    const headerHash = journalState.headers[0]!;
    journalState.missing.add(headerHash);
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

  it("restarts over more open objectives than the cap without throwing and reports capacity", async () => {
    const restart = await restartOver(MAX_OPEN_OBJECTIVES + 1, "open");
    const contexts = await restart.admit();
    expect(contexts).toHaveLength(MAX_OPEN_OBJECTIVES + 1);
    const database = restart.database();
    expect(watcherJournalCapacityReached(database)).toBe(true);
    expect(
      canOpenWatcherProofObjective(database, {
        category: "doubleSpend",
        headerHash: "ff".repeat(28),
      }),
    ).toBe(false);
    expect(
      canOpenWatcherProofObjective(database, {
        category: "doubleSpend",
        headerHash: journalState.headers[0]!,
      }),
    ).toBe(true);
  }, 60_000);
});
