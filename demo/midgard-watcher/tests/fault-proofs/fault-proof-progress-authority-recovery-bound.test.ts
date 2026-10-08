import { mkdir, stat } from "node:fs/promises";
import { join } from "node:path";

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
import { WATCHER_ROLLBACK_BOUNDS } from "../../src/l1/rollback-engine/types.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
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
    readWatcherProofExecution: async ({
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
const BEYOND_RECOVERY =
  Number(WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth) + 1;
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
                  recoveryDepth:
                    WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth.toString(),
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

const exists = async (path: string): Promise<boolean> =>
  await stat(path).then(
    () => true,
    () => false,
  );

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
    const pruned = join(
      restart.journalRoot,
      "fault-proofs",
      "doubleSpend",
      journalState.headers[0]!,
    );
    await mkdir(pruned, { recursive: true });
    // The execution of this one is already gone: a crash after the directory
    // was removed and before its rows were deleted.
    journalState.missing.add(journalState.headers[1]!);
    await expect(restart.admit()).resolves.toEqual([]);
    expect(restart.authority.unfinishedCount()).toBe(0);
    expect(await exists(pruned)).toBe(false);
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

  it("leaves the rows of an objective whose job is active to that job's finish", async () => {
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
    await restart.admit();
    const database = restart.database();
    expect(database.row("fault_proof_objectives", scope)?.state).toBe("open");
    expect(database.row("fault_proof_queue", "77".repeat(32))?.state).toBe(
      "active",
    );
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
