import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { afterEach, describe, expect, it } from "vitest";

import {
  encodeWatcherDurableStore,
  makeEmptyWatcherDurableStore,
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
  watcherCanonicalJson,
  watcherDurableStoreBytesSha256,
} from "../../src/storage/durable-store.js";
import { openWatcherSqliteDurableBackend } from "../../src/storage/sqlite-durable-backend.js";

const directories: string[] = [];
const hex32 = (value: number) => value.toString(16).padStart(64, "0");
const marker = {
  schemaVersion: "midgard-deployment-marker-v1" as const,
  manifestId: hex32(1),
};
const store = (count: number) => {
  const empty = makeEmptyWatcherDurableStore(marker);
  return makeWatcherDurableStore({
    deploymentMarker: marker,
    revision: count.toString(),
    records: {
      ...empty,
      chainPoints: Array.from({ length: count }, (_, index) => ({
        chainPointId: hex32(index + 1),
        providerId: "local-node",
        blockHash: hex32(index + 1),
        blockNo: (index + 1).toString(),
        slot: (index + 1).toString(),
        depth: "30",
      })),
      l1Observations: Array.from({ length: count }, (_, index) => ({
        observationId: hex32(index + 1),
        providerId: "local-node",
        chainPointId: hex32(index + 1),
        payload: makeWatcherDurablePayload("ab".repeat(4096)),
      })),
    },
  });
};
const open = async () => {
  const directory = await mkdtemp("/var/tmp/watcher-record-store-");
  directories.push(directory);
  const path = join(directory, "watcher.sqlite");
  return { path, opened: await openWatcherSqliteDurableBackend({ path }) };
};
afterEach(async () => {
  await Promise.all(
    directories.splice(0).map((path) => rm(path, { recursive: true })),
  );
});

describe("SQLite incremental watcher records", () => {
  it("writes only new records and reference pages, and restores exact state on restart", async () => {
    const { path, opened } = await open();
    const first = encodeWatcherDurableStore(store(192));
    const next = encodeWatcherDurableStore(store(193));
    expect(await opened.backend.compareAndSwap(null, first)).toBe(true);
    const inspection = new DatabaseSync(path);
    try {
      inspection.exec(`
        CREATE TABLE written_bytes (bytes INTEGER NOT NULL);
        CREATE TRIGGER record_bytes AFTER INSERT ON watcher_user_event_archive_v1
        BEGIN INSERT INTO written_bytes VALUES (length(NEW.bytes)); END;
      `);
      expect(
        await opened.backend.compareAndSwap(
          watcherDurableStoreBytesSha256(first),
          next,
        ),
      ).toBe(true);
      const written = inspection
        .prepare("SELECT sum(bytes) AS bytes FROM written_bytes")
        .get() as { bytes: number };
      const head = inspection
        .prepare(
          "SELECT encoding, length(bytes) AS bytes FROM watcher_durable_snapshot_v1",
        )
        .get() as { encoding: string; bytes: number };
      expect(first.length).toBeGreaterThan(1_500_000);
      expect(head.encoding).toBe("store");
      expect(head.bytes).toBeLessThan(8_192);
      expect(written.bytes).toBeLessThan(24_576);
      expect(await opened.backend.read()).toEqual(next);
      // The archive uses the same records as the current progress marker.
      const key = await opened.userEventArchive.put(next);
      expect(await opened.userEventArchive.read(key)).toEqual(next);
      opened.close();
      const restarted = await openWatcherSqliteDurableBackend({ path });
      try {
        expect(await restarted.backend.read()).toEqual(next);
        expect(await restarted.userEventArchive.read(key)).toEqual(next);
      } finally {
        restarted.close();
      }
    } finally {
      inspection.close();
    }
  });

  it("shares unchanged event state and raw blocks across progress records", async () => {
    const { path, opened } = await open();
    const inspection = new DatabaseSync(path);
    try {
      const encode = (value: unknown) =>
        Uint8Array.from(Buffer.from(watcherCanonicalJson(value), "utf8"));
      const events = Array.from({ length: 192 }, (_, index) => ({
        eventId: hex32(index + 1),
        eventCborHex: "ab".repeat(4096),
      }));
      const payload = (count: number) =>
        encode({
          schemaVersion:
            "midgard-watcher-local-user-event-checkpoint-payload-v1",
          snapshot: {
            schemaVersion: "midgard-watcher-user-event-snapshot-v1",
            activeEvents: events.slice(0, count),
            terminalEvents: [],
            snapshotDigest: hex32(count),
          },
          retainedEntries: Array.from({ length: count }, (_, index) => ({
            entryDigest: hex32(index + 1),
          })),
        });
      const first = payload(191);
      await opened.userEventArchive.put(first);
      inspection.exec(`
        CREATE TABLE written_bytes (bytes INTEGER NOT NULL);
        CREATE TRIGGER record_bytes AFTER INSERT ON watcher_user_event_archive_v1
        BEGIN INSERT INTO written_bytes VALUES (length(NEW.bytes)); END;
      `);
      const next = payload(192);
      const nextKey = await opened.userEventArchive.put(next);
      expect(
        (
          inspection
            .prepare("SELECT sum(bytes) AS bytes FROM written_bytes")
            .get() as { bytes: number }
        ).bytes,
      ).toBeLessThan(24_576);
      const block = "cd".repeat(8192);
      const evidence = encode({
        schemaVersion: "midgard-watcher-local-user-event-block-evidence-v1",
        first: { rawBlockCbor: block },
        current: { rawBlockCbor: block },
      });
      const evidenceKey = await opened.userEventArchive.put(evidence);
      const rawKey = watcherDurableStoreBytesSha256(encode(block));
      expect(
        (
          inspection
            .prepare(
              "SELECT count(*) AS count FROM watcher_user_event_archive_v1 WHERE digest = ?",
            )
            .get(rawKey) as { count: number }
        ).count,
      ).toBe(1);
      opened.close();
      const restarted = await openWatcherSqliteDurableBackend({ path });
      try {
        expect(await restarted.userEventArchive.read(nextKey)).toEqual(next);
        expect(await restarted.userEventArchive.read(evidenceKey)).toEqual(
          evidence,
        );
      } finally {
        restarted.close();
      }
    } finally {
      inspection.close();
    }
  });

  it("rolls back inserted records when the progress-marker write fails", async () => {
    const { path, opened } = await open();
    const inspection = new DatabaseSync(path);
    try {
      const first = encodeWatcherDurableStore(store(2));
      const next = encodeWatcherDurableStore(store(3));
      await opened.backend.compareAndSwap(null, first);
      const count = () =>
        inspection
          .prepare(
            "SELECT count(*) AS count FROM watcher_user_event_archive_v1",
          )
          .get();
      const before = count();
      inspection.exec(`
        CREATE TRIGGER fail_progress BEFORE UPDATE ON watcher_durable_snapshot_v1
        BEGIN SELECT RAISE(ABORT, 'injected progress failure'); END;
      `);
      await expect(
        opened.backend.compareAndSwap(
          watcherDurableStoreBytesSha256(first),
          next,
        ),
      ).rejects.toThrow("injected progress failure");
      expect(count()).toEqual(before);
      expect(await opened.backend.read()).toEqual(first);
      inspection.exec("DROP TRIGGER fail_progress");
      expect(
        await opened.backend.compareAndSwap(
          watcherDurableStoreBytesSha256(first),
          next,
        ),
      ).toBe(true);
      expect(await opened.backend.read()).toEqual(next);
    } finally {
      inspection.close();
      opened.close();
    }
  });

  it("invalidates warm state when an external writer damages a referenced record", async () => {
    const { path, opened } = await open();
    const inspection = new DatabaseSync(path);
    try {
      const current = encodeWatcherDurableStore(store(2));
      await opened.backend.compareAndSwap(null, current);
      expect(await opened.backend.read()).toEqual(current);
      inspection.exec(`
        UPDATE watcher_user_event_archive_v1 SET bytes = x'00'
        WHERE digest = (SELECT digest FROM watcher_user_event_archive_v1 ORDER BY length(bytes) DESC LIMIT 1);
      `);
      await expect(opened.backend.read()).rejects.toThrow("digest mismatch");
    } finally {
      inspection.close();
      opened.close();
    }
  });
});
