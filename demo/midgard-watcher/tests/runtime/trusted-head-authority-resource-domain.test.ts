import { randomUUID } from "node:crypto";
import { stat } from "node:fs/promises";
import { join } from "node:path";
import { performance } from "node:perf_hooks";
import { DatabaseSync } from "node:sqlite";

import { expect, it } from "vitest";

import {
  importLegacyAuthorityStore,
  openWatcherTrustedHeadAuthorityStore,
} from "../../src/runtime/trusted-head-authority.js";
import { legacyScene } from "./trusted-head-authority.legacy-fixture.js";
import { directory, head } from "./trusted-head-authority.policy.js";

// Input/work domain evidence, not admission policy or a wall-clock SLA.
it.each([1, 2, 8, 64])(
  "measures cold SQLite-handle open/read/CAS with fixed K=%s and growing retired history",
  async (liveRecordLimit) => {
    const samples: unknown[] = [];
    for (const total of [128, 512, 2048]) {
      const legacy = await legacyScene(total);
      const input = {
        directory: await directory(),
        policy: legacy.policy,
        recordAuthenticationKey: legacy.recordAuthenticationKey,
        liveRecordLimit,
        generation: `generation-${randomUUID()}`,
        legacyDirectory: legacy.path,
      };
      const importStart = performance.now();
      await importLegacyAuthorityStore(input);
      const importMs = performance.now() - importStart;
      const dbPath = join(
        input.directory,
        input.generation,
        "authority.sqlite",
      );
      const timings: { openMs: number; readMs: number; casMs: number }[] = [];
      let prior = head(legacy.policy, total - 1, "77");
      for (let i = 0; i < 5; i++) {
        const beforeOpen = performance.now();
        const store = await openWatcherTrustedHeadAuthorityStore(input);
        const opened = performance.now();
        try {
          expect(await store.readCurrent()).toEqual(prior);
          const read = performance.now(),
            next = head(legacy.policy, total + i, "88");
          expect(
            await store.compareAndSwap({
              expectedTrustedHead: prior,
              nextTrustedHead: next,
            }),
          ).toEqual({ committed: true, head: next });
          timings.push({
            openMs: opened - beforeOpen,
            readMs: read - opened,
            casMs: performance.now() - read,
          });
          prior = next;
        } finally {
          store.close();
        }
      }
      const db = new DatabaseSync(dbPath);
      let records: unknown;
      try {
        records = db
          .prepare("SELECT count(*) AS count FROM authority_records")
          .get()!.count;
      } finally {
        db.close();
      }
      expect(records).toBe(liveRecordLimit);
      const databaseBytes = (await stat(dbPath)).size;
      // All history lengths exercise the same fixed live table cardinality.
      samples.push({
        totalLegacyRevisions: total,
        retiredRevisions: total + 5 - liveRecordLimit,
        retainedRecords: records,
        databaseBytes,
        importMs,
        timings,
      });
    }
    console.log(
      "AUTHORITY_RESOURCE_DOMAIN " +
        JSON.stringify({ liveRecordLimit, samples }),
    );
  },
  120_000,
);
