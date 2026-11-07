import { Client } from "pg";
import { describe, expect, it } from "vitest";

import type { CommitteeStore } from "../src/store.js";
import {
  RETENTION_ALERT_ENTRY_LIMIT,
  type RetentionDeadlineOptions,
} from "../src/store/retention.js";
import {
  RETENTION_CYCLE_ROW_BUDGET,
  type RetentionCycleResult,
  type RetentionSweep,
  runRetentionCycle,
} from "../src/store/retention.scan.js";
import {
  openTestCommitteeStore,
  testStoreDatabase,
} from "./helpers/committee-store.js";
import {
  hashOf,
  NOW,
  openStore,
  PAST_RETENTION,
  REQUIRED_RETENTION_MS,
  retainedHeaderHashes,
  retentionOptions,
  seed,
} from "./store-retention.header-record.js";

/** A cap on the cycles of one sweep, so a sweep that never ends fails. */
const MAX_CYCLES = 10;

/** The cycles of one sweep over the retained set, from a fresh sweep. */
const sweepCycles = async (
  store: CommitteeStore,
  options: RetentionDeadlineOptions,
): Promise<readonly RetentionCycleResult[]> => {
  const sweep: RetentionSweep = {};
  const cycles: RetentionCycleResult[] = [];
  do {
    cycles.push(await runRetentionCycle(store, options, sweep));
  } while (!cycles.at(-1)!.sweepCompleted && cycles.length < MAX_CYCLES);
  return cycles;
};

/**
 * Copies the template's payload and header rows `count` times under new
 * header hashes, one statement per table, so a large retained set needs no
 * per-row writes.
 */
const cloneRetained = async (
  url: string,
  template: string,
  count: number,
): Promise<readonly string[]> => {
  const clones = `SELECT lpad(to_hex(i), 56, '0') AS hash
    FROM generate_series(1, $2::int) AS i`;
  const client = new Client({ connectionString: url });
  await client.connect();
  try {
    await client.query(
      `INSERT INTO committee_da_payloads (header_hash, record)
       SELECT clone.hash, jsonb_set(row.record, '{headerHash}', to_jsonb(clone.hash))
       FROM committee_da_payloads row, (${clones}) clone
       WHERE row.header_hash = $1`,
      [template, count],
    );
    await client.query(
      `INSERT INTO committee_state_queue_headers (header_hash, record)
       SELECT clone.hash, row.record || jsonb_build_object(
         'headerHash', clone.hash,
         'computedHeaderHash', clone.hash,
         'blockAssetName', clone.hash)
       FROM committee_state_queue_headers row, (${clones}) clone
       WHERE row.header_hash = $1`,
      [template, count],
    );
  } finally {
    await client.end();
  }
  return Array.from({ length: count }, (_, index) =>
    (index + 1).toString(16).padStart(56, "0"),
  );
};

describe("retention cycle budget", () => {
  it("decides at most the budget per cycle over a large retained set and prunes exactly the expired payloads", async () => {
    const database = await testStoreDatabase();
    const store = await openTestCommitteeStore(database);
    const template = hashOf(10);
    const expired = Array.from({ length: 50 }, (_, index) =>
      hashOf(20 + index),
    );
    await seed(store, [
      { headerHash: template, status: "merged" },
      ...expired.map((headerHash) => ({
        headerHash,
        endTimeMs: PAST_RETENTION,
        status: "merged" as const,
      })),
    ]);
    const inWindow = [
      template,
      ...(await cloneRetained(database.url, template, 4_000)),
    ];
    const total = inWindow.length + expired.length;

    const cycles = await sweepCycles(store, retentionOptions());
    for (const { prune } of cycles) {
      expect(prune.scanned).toBeLessThan(total);
      expect(prune.scanned).toBeLessThanOrEqual(RETENTION_CYCLE_ROW_BUDGET);
    }
    expect(cycles.at(-1)!.sweepCompleted).toBe(true);
    expect(cycles.reduce((sum, { prune }) => sum + prune.scanned, 0)).toBe(
      total,
    );
    expect(
      cycles.flatMap(({ prune }) => prune.prunedHeaderHashes).sort(),
    ).toEqual([...expired].sort());
    expect(await retainedHeaderHashes(store)).toEqual([...inWindow].sort());
    // The completed sweep reports the whole retained set it decided.
    expect(cycles.at(-1)!.deadlines).toMatchObject({
      scanned: total,
      prunable: expired.length,
      retained: inWindow.length,
    });
  });
});

describe("retention deadline report size", () => {
  // Remaining time falls as the index rises, so the nearest deadlines are
  // not the first header hashes.
  const ALERTING = Array.from({ length: 200 }, (_, index) => ({
    headerHash: hashOf(index),
    withoutHeader: true,
    fetchedAtMs: NOW - REQUIRED_RETENTION_MS + (199 - index) * 1_000,
  }));
  const OPTIONS = { ...retentionOptions(), alertThresholdMs: 1_000_000 };

  it.each([
    { scanPageSize: undefined, cycleRowBudget: undefined },
    { scanPageSize: 7, cycleRowBudget: undefined },
    { scanPageSize: 7, cycleRowBudget: 30 },
  ])(
    "counts every alerting payload but names only the nearest deadlines (page $scanPageSize, budget $cycleRowBudget)",
    async ({ scanPageSize, cycleRowBudget }) => {
      const store = await openStore();
      await seed(store, ALERTING);
      const cycles = await sweepCycles(store, {
        ...OPTIONS,
        ...(scanPageSize === undefined ? {} : { scanPageSize }),
        ...(cycleRowBudget === undefined ? {} : { cycleRowBudget }),
      });
      const { deadlines } = cycles.at(-1)!;
      expect(deadlines).toMatchObject({
        scanned: ALERTING.length,
        alerting: ALERTING.length,
      });
      expect(deadlines.entries.map(({ headerHash }) => headerHash)).toEqual(
        [...ALERTING]
          .reverse()
          .slice(0, RETENTION_ALERT_ENTRY_LIMIT)
          .map(({ headerHash }) => headerHash),
      );
      expect(deadlines.entries.every(({ alerting }) => alerting)).toBe(true);
    },
  );
});
