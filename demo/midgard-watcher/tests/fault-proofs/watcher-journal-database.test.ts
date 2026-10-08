import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { afterEach, describe, expect, it } from "vitest";

import {
  openWatcherFaultProofQueueJournal,
  watcherFaultProofQueueIdentityDigest,
} from "../../src/fault-proofs/fault-proof-queue-journal.js";
import {
  closeWatcherJournalDatabase,
  openWatcherJournalDatabase,
  WATCHER_JOURNAL_DATABASE_FILE,
  type WatcherJournalDatabase,
} from "../../src/fault-proofs/watcher-journal-database.js";
import {
  WATCHER_JOURNAL_RETAINED_REVISIONS,
  watcherObjectiveScope,
} from "../../src/fault-proofs/watcher-journal-schema.js";
import {
  journalDirectory,
  removeJournalDirectories,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";

afterEach(removeJournalDirectories);

const QUEUE = "watcher_fault_proof_queue";
const open = (journalRoot: string): WatcherJournalDatabase =>
  openWatcherJournalDatabase({
    journalRoot,
    authenticationKey: TEST_JOURNAL_KEY,
  });

const put = (database: WatcherJournalDatabase, key: string, state: string) =>
  database.transaction((tx) =>
    tx.put("fault_proof_queue", {
      key,
      scope: `doubleSpend:${key}`,
      state,
      body: { key, state },
    }),
  );

/** Three rows over four revisions; the database is closed afterwards, as a
 * process exit would leave it. */
const seeded = async (): Promise<string> => {
  const root = await journalDirectory("midgard-journal-integrity");
  const database = open(root);
  put(database, "a", "queued");
  put(database, "b", "queued");
  put(database, "c", "queued");
  put(database, "a", "active");
  closeWatcherJournalDatabase(root);
  return root;
};

/** Edits the file with a raw connection, as a person or a bug could. */
const edit = (root: string, sql: string, ...params: string[]): void => {
  const raw = new DatabaseSync(join(root, WATCHER_JOURNAL_DATABASE_FILE));
  try {
    raw.prepare(sql).run(...params);
  } finally {
    raw.close();
  }
};

const rawRow = (root: string, key: string) => {
  const raw = new DatabaseSync(join(root, WATCHER_JOURNAL_DATABASE_FILE), {
    readOnly: true,
  });
  try {
    return raw
      .prepare(
        `SELECT row_key, scope, state, revision, body, mac FROM ${QUEUE} WHERE row_key = ?`,
      )
      .get(key) as Record<string, string | number>;
  } finally {
    raw.close();
  }
};

const restart = (root: string) => () => open(root);

describe("watcher journal database integrity at startup", () => {
  it("opens an untouched journal and serves its rows", async () => {
    const root = await seeded();
    const database = open(root);
    expect(database.head("fault_proof_queue")).toEqual({
      revision: 4,
      liveRows: 3,
    });
    expect(database.row("fault_proof_queue", "a")?.state).toBe("active");
  });

  it("refuses a tampered row body", async () => {
    const root = await seeded();
    edit(
      root,
      `UPDATE ${QUEUE} SET body = ? WHERE row_key = 'b'`,
      JSON.stringify({ key: "b", state: "finished" }),
    );
    expect(restart(root)).toThrow("row b MAC differs");
  });

  it("refuses a tampered row state", async () => {
    const root = await seeded();
    edit(root, `UPDATE ${QUEUE} SET state = 'finished' WHERE row_key = 'b'`);
    expect(restart(root)).toThrow("row b MAC differs");
  });

  it("refuses two revisions swapped in order", async () => {
    const root = await seeded();
    // Revision 2's row moves to revision 3 and back, one statement each.
    for (const [from, to] of [
      [2, 100],
      [3, 2],
      [100, 3],
    ])
      edit(
        root,
        "UPDATE watcher_journal_revisions SET revision = ? WHERE journal = 'fault_proof_queue' AND revision = ?",
        String(to),
        String(from),
      );
    expect(restart(root)).toThrow("revision 2 is out of order or altered");
  });

  it("refuses a deleted row", async () => {
    const root = await seeded();
    edit(root, `DELETE FROM ${QUEUE} WHERE row_key = 'c'`);
    expect(restart(root)).toThrow("rows differ from the authenticated head");
  });

  it("refuses an earlier version of a row replayed over its current one", async () => {
    const root = await journalDirectory("midgard-journal-integrity");
    const database = open(root);
    put(database, "a", "queued");
    closeWatcherJournalDatabase(root);
    const earlier = rawRow(root, "a");
    put(open(root), "a", "active");
    closeWatcherJournalDatabase(root);
    edit(
      root,
      `UPDATE ${QUEUE} SET state = ?, revision = ?, body = ?, mac = ? WHERE row_key = 'a'`,
      String(earlier.state),
      String(earlier.revision),
      String(earlier.body),
      String(earlier.mac),
    );
    expect(restart(root)).toThrow("rows differ from the authenticated head");
  });

  it("refuses a dropped revision", async () => {
    const root = await seeded();
    edit(
      root,
      "DELETE FROM watcher_journal_revisions WHERE journal = 'fault_proof_queue' AND revision = 3",
    );
    expect(restart(root)).toThrow("retained revision history is incomplete");
  });

  it("refuses a head rolled back behind its rows", async () => {
    const root = await seeded();
    edit(
      root,
      "UPDATE watcher_journal_heads SET revision = 3 WHERE journal = 'fault_proof_queue'",
    );
    expect(restart(root)).toThrow("head MAC differs");
  });

  it("refuses a journal authenticated under another key", async () => {
    const root = await seeded();
    expect(() =>
      openWatcherJournalDatabase({
        journalRoot: root,
        authenticationKey: Uint8Array.from({ length: 32 }, () => 0x5a),
      }),
    ).toThrow("head is authenticated by another key");
  });
});

describe("watcher journal growth under retries", () => {
  it("keeps one objective's tables at their live rows over 100,000 retry cycles, and restarts", async () => {
    const journalRoot = await journalDirectory("midgard-journal-growth", {
      memory: true,
    });
    const deploymentFingerprint = "11".repeat(32);
    const input = {
      journalRoot,
      deploymentFingerprint,
      authenticationKey: TEST_JOURNAL_KEY,
    };
    const journal = await openWatcherFaultProofQueueJournal(input);
    const identity = (generation: number) => ({
      category: "doubleSpend",
      headerHash: "22".repeat(28),
      decisionDigest: "33".repeat(32),
      rollbackGeneration: generation.toString(),
    });
    const pages = (): number => {
      const raw = new DatabaseSync(
        join(journalRoot, WATCHER_JOURNAL_DATABASE_FILE),
        { readOnly: true },
      );
      try {
        const count = raw.prepare("PRAGMA page_count").get() as {
          page_count: number;
        };
        const free = raw.prepare("PRAGMA freelist_count").get() as {
          freelist_count: number;
        };
        return Number(count.page_count) - Number(free.freelist_count);
      } finally {
        raw.close();
      }
    };
    // Each cycle is one retry: the job is queued under a newer rollback
    // generation, started, and finished without completing the objective.
    const cycle = async (generation: number) => {
      const digest = watcherFaultProofQueueIdentityDigest({
        deploymentFingerprint,
        identity: identity(generation),
      });
      await journal.register(identity(generation), "1000");
      await journal.markStarted(digest, "1001");
      await journal.markFinished(digest, "1002");
    };
    for (let generation = 0; generation < 1_000; generation += 1)
      await cycle(generation);
    const database = open(journalRoot);
    const pagesAfterWarmup = pages();
    for (let generation = 1_000; generation < 100_000; generation += 1)
      await cycle(generation);

    for (const name of ["fault_proof_queue", "fault_proof_objectives"] as const)
      expect(database.head(name).liveRows).toBe(1);
    expect(database.head("fault_proof_queue").revision).toBe(300_000);
    const raw = new DatabaseSync(
      join(journalRoot, WATCHER_JOURNAL_DATABASE_FILE),
      { readOnly: true },
    );
    const rowsOf = (sql: string): number =>
      Number((raw.prepare(sql).get() as { n: number }).n);
    expect(rowsOf(`SELECT count(*) AS n FROM ${QUEUE}`)).toBe(1);
    expect(
      rowsOf("SELECT count(*) AS n FROM watcher_fault_proof_objectives"),
    ).toBe(1);
    expect(
      rowsOf(
        "SELECT count(*) AS n FROM watcher_journal_revisions WHERE journal = 'fault_proof_queue'",
      ),
    ).toBe(WATCHER_JOURNAL_RETAINED_REVISIONS);
    raw.close();
    // The file holds no more live pages than after the first thousand.
    expect(pages()).toBeLessThanOrEqual(pagesAfterWarmup * 2);

    closeWatcherJournalDatabase(journalRoot);
    const restarted = await openWatcherFaultProofQueueJournal(input);
    expect(restarted.status()).toEqual({
      queuedJobCount: 0,
      oldestQueuedAtMs: null,
    });
    expect(
      open(journalRoot).row(
        "fault_proof_objectives",
        watcherObjectiveScope("doubleSpend", "22".repeat(28)),
      )?.state,
    ).toBe("open");
  }, 600_000);
});
