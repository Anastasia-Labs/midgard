import { mkdirSync, writeFileSync } from "node:fs";
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { performance } from "node:perf_hooks";
import { DatabaseSync } from "node:sqlite";

import { afterAll, describe, expect, it } from "vitest";

import {
  closeWatcherJournalDatabase,
  openWatcherJournalDatabase,
} from "../src/fault-proofs/watcher-journal-database.js";

/**
 * B6 (plan §16.2): one journal persist at 10^5 stored rows. A persist is one
 * write transaction of one row: its row, the journal head and one retained
 * revision, committed with `synchronous = FULL`.
 *
 * Target: p99 <= 20 ms. The report gives p50, p95 and p99 for an in-place
 * update of a stored row (a job moving between states) and for a new row,
 * and the same for a bare one-row SQLite commit on the same disk, the floor
 * any durable commit there pays.
 *
 * Run from demo/: `pnpm --filter midgard-watcher bench`. The directory
 * defaults to a fresh one under /var/tmp; `WATCHER_B6_DIRECTORY` points it
 * at the disk under test. `WATCHER_B6_ROWS` and `WATCHER_B6_COMMITS` scale it.
 * The report lands in `bench/output/b6-journal-persist.json`.
 */
const ROWS = Number(process.env.WATCHER_B6_ROWS ?? "100000");
const COMMITS = Number(process.env.WATCHER_B6_COMMITS ?? "1000");
const PRELOAD_BATCH = 1_000;
const TARGET_P99_MS = 20;
const KEY = Uint8Array.from({ length: 32 }, (_, index) => index + 1);

const roots: string[] = [];
afterAll(async () => {
  for (const root of roots.splice(0)) {
    closeWatcherJournalDatabase(root);
    await rm(root, { recursive: true, force: true });
  }
});

const body = (index: number, state: string) => ({
  identity: {
    category: "doubleSpend",
    headerHash: index.toString(16).padStart(56, "0"),
    detectionId: `double_spend_v1:${index.toString()}:${"bb".repeat(32)}`,
    violationId: `double_spend_v1:${index.toString()}`,
    decisionDigest: index.toString(16).padStart(64, "0"),
  },
  queuedAtMs: "1700000000000",
  observedAtMs: "1700000000000",
  state,
});

const rowKey = (index: number): string => index.toString(16).padStart(64, "0");

const percentile = (sorted: readonly number[], p: number): number =>
  sorted[
    Math.min(sorted.length - 1, Math.ceil((p / 100) * sorted.length) - 1)
  ]!;

const summarize = (samples: number[]) => {
  const sorted = [...samples].sort((left, right) => left - right);
  return {
    p50: percentile(sorted, 50),
    p95: percentile(sorted, 95),
    p99: percentile(sorted, 99),
    max: sorted.at(-1)!,
  };
};

describe("B6 watcher journal persist", () => {
  it(`commits one row at ${ROWS.toString()} stored rows with p99 <= ${TARGET_P99_MS.toString()} ms`, async () => {
    const root =
      process.env.WATCHER_B6_DIRECTORY ??
      (await mkdtemp("/var/tmp/midgard-watcher-b6-"));
    roots.push(root);
    let database = openWatcherJournalDatabase({
      journalRoot: root,
      authenticationKey: KEY,
    });
    const preloadStarted = performance.now();
    for (let start = 0; start < ROWS; start += PRELOAD_BATCH)
      database.transaction((tx) => {
        for (
          let index = start;
          index < Math.min(ROWS, start + PRELOAD_BATCH);
          index += 1
        )
          tx.put("fault_proof_queue", {
            key: rowKey(index),
            scope: `doubleSpend:${index.toString(16).padStart(56, "0")}`,
            state: "queued",
            body: body(index, "queued"),
          });
      });
    const preloadMs = performance.now() - preloadStarted;
    expect(database.head("fault_proof_queue").liveRows).toBe(ROWS);

    closeWatcherJournalDatabase(root);
    const verifyStarted = performance.now();
    database = openWatcherJournalDatabase({
      journalRoot: root,
      authenticationKey: KEY,
    });
    const startupVerifyMs = performance.now() - verifyStarted;

    const timed = (run: () => void): number => {
      const started = performance.now();
      run();
      return performance.now() - started;
    };
    const updates: number[] = [];
    const inserts: number[] = [];
    for (let commit = 0; commit < COMMITS; commit += 1) {
      const index = (commit * 7_919) % ROWS;
      updates.push(
        timed(() =>
          database.transaction((tx) => {
            const state = tx.row("fault_proof_queue", rowKey(index))?.state;
            const next = state === "queued" ? "active" : "queued";
            tx.put("fault_proof_queue", {
              key: rowKey(index),
              scope: `doubleSpend:${index.toString(16).padStart(56, "0")}`,
              state: next,
              body: body(index, next),
            });
          }),
        ),
      );
      const fresh = ROWS + commit;
      inserts.push(
        timed(() =>
          database.transaction((tx) => {
            tx.put("fault_proof_queue", {
              key: rowKey(fresh),
              scope: `doubleSpend:${fresh.toString(16).padStart(56, "0")}`,
              state: "queued",
              body: body(fresh, "queued"),
            });
          }),
        ),
      );
    }
    const all = summarize([...updates, ...inserts]);

    // The disk's floor for comparison: a bare one-row WAL commit with the
    // same durability, in the same directory, with no journal on top.
    const bare = new DatabaseSync(join(root, "b6-floor.sqlite"));
    bare.exec(`
      PRAGMA journal_mode = WAL;
      PRAGMA synchronous = FULL;
      CREATE TABLE IF NOT EXISTS floor (k INTEGER PRIMARY KEY, v TEXT) STRICT;
    `);
    const upsert = bare.prepare(
      "INSERT INTO floor (k, v) VALUES (?, ?) ON CONFLICT (k) DO UPDATE SET v = excluded.v",
    );
    const floor: number[] = [];
    for (let commit = 0; commit < COMMITS * 2; commit += 1)
      floor.push(
        timed(() => {
          bare.exec("BEGIN IMMEDIATE");
          upsert.run(commit % 64, `${commit.toString()}:${"x".repeat(400)}`);
          bare.exec("COMMIT");
        }),
      );
    bare.close();
    const report = {
      rows: ROWS,
      commits: COMMITS * 2,
      directory: root,
      preloadMs: Math.round(preloadMs),
      startupVerifyMs: Math.round(startupVerifyMs),
      update: summarize(updates),
      insert: summarize(inserts),
      all,
      bareSqliteFloor: summarize(floor),
    };
    const output = join(import.meta.dirname, "output");
    mkdirSync(output, { recursive: true });
    writeFileSync(
      join(output, "b6-journal-persist.json"),
      `${JSON.stringify(report, null, 2)}\n`,
    );
    expect(all.p99, JSON.stringify(report)).toBeLessThanOrEqual(TARGET_P99_MS);
  });
});
