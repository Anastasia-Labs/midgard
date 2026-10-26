import { RETENTION_MS_PER_DAY } from "@al-ft/midgard-core";
import { Client } from "pg";
import { describe, expect, it } from "vitest";

import type { StateQueueHeaderStatus } from "../src/domain.js";
import { runRetentionCycle } from "../src/store/retention.scan.js";
import {
  openTestCommitteeStore,
  testStoreDatabase,
} from "./helpers/committee-store.js";
import {
  hashOf,
  NOW,
  openStore,
  PAST_RETENTION,
  retainedHeaderHashes,
  retentionOptions,
  seed,
} from "./store-retention.header-record.js";

const STATUSES: readonly StateQueueHeaderStatus[] = [
  "unattested",
  "attesting",
  "attested",
  "merged",
  "removed",
  "conflicted",
];

/** Sixty retained payloads of every status and age, some without a header. */
const ENTRIES = Array.from({ length: 60 }, (_, index) => ({
  headerHash: hashOf(20 + index),
  endTimeMs: NOW - index * RETENTION_MS_PER_DAY,
  status: STATUSES[index % STATUSES.length]!,
  withoutHeader: index % 7 === 0,
  fetchedAtMs: NOW - index * RETENTION_MS_PER_DAY,
}));

/** A production-shaped cycle: header release on, alerts a month ahead. */
const OPTIONS = {
  ...retentionOptions(),
  releaseHeader: true,
  alertThresholdMs: 30 * RETENTION_MS_PER_DAY,
  confirmedHeadHash: hashOf(79),
  liveQueueHeaderHashes: new Set([hashOf(78), hashOf(77)]),
};

const cycleOver = async (scanPageSize?: number) => {
  const store = await openStore();
  await seed(store, ENTRIES);
  const cycle = await runRetentionCycle(
    store,
    scanPageSize === undefined ? OPTIONS : { ...OPTIONS, scanPageSize },
  );
  return { cycle, retained: await retainedHeaderHashes(store) };
};

describe("retention scan pages", () => {
  it("keeps and prunes a many-payload store exactly as one page does", async () => {
    const whole = await cycleOver();
    expect(whole.cycle.prune.scanned).toBe(ENTRIES.length);
    expect(whole.cycle.prune.prunedHeaderHashes.length).toBeGreaterThan(0);
    expect(whole.retained.length).toBeGreaterThan(0);
    expect(whole.cycle.deadlines.alerting).toBeGreaterThan(0);
    expect(
      [...whole.cycle.prune.prunedHeaderHashes, ...whole.retained].sort(),
    ).toEqual(ENTRIES.map(({ headerHash }) => headerHash).sort());
    // 7 leaves a short last page; 6 and 1 end on a full page and an empty one.
    for (const scanPageSize of [7, 6, 1]) {
      const paged = await cycleOver(scanPageSize);
      expect(paged.cycle).toEqual(whole.cycle);
      expect(paged.retained).toEqual(whole.retained);
    }
  });

  it("decides and prunes on retention metadata without reading payload bytes", async () => {
    const database = await testStoreDatabase();
    const store = await openTestCommitteeStore(database);
    const expired = hashOf(10);
    const recent = hashOf(11);
    await seed(store, [
      { headerHash: expired, endTimeMs: PAST_RETENTION, status: "merged" },
      { headerHash: recent, status: "merged" },
    ]);
    // No read of a whole payload record accepts these rows any more: what
    // the cycle still decides on, it decides on the retention metadata.
    const client = new Client({ connectionString: database.url });
    await client.connect();
    try {
      await client.query(
        "UPDATE committee_da_payloads SET record = record - 'payloadCborHex'",
      );
    } finally {
      await client.end();
    }
    const cycle = await runRetentionCycle(store, {
      ...retentionOptions(),
      scanPageSize: 1,
    });
    expect(cycle.prune).toEqual({
      scanned: 2,
      prunedHeaderHashes: [expired],
      retained: 1,
    });
    expect(cycle.deadlines.scanned).toBe(2);
    expect(await retainedHeaderHashes(store)).toEqual([recent]);
  });
});
