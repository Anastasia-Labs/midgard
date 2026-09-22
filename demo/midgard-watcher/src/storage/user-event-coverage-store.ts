import { createHmac, timingSafeEqual } from "node:crypto";
import type { DatabaseSync } from "node:sqlite";

import { watcherCanonicalJson } from "./durable-store.js";

export const WATCHER_USER_EVENT_COVERAGE_SCHEMA_VERSION =
  "midgard-watcher-user-event-coverage-v1" as const;

const HEX_32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const MAXIMUM_SQLITE_INTEGER = 9_223_372_036_854_775_807n;

/**
 * The one moving coverage checkpoint of the user-event history: the latest
 * native block scanned since the last recorded event observation without
 * finding anything, together with the head entry it sits on top of. Quiet
 * blocks advance it in place; an event block moves it onto the new entry.
 * It lives outside the observation digest chain and is authenticated with
 * the rollback authority key so a torn or foreign row is detected.
 */
export type WatcherUserEventCoverageRecord = Readonly<{
  blockHash: string;
  blockNo: string;
  slot: string;
  /** Digest of the head entry this coverage extends. */
  headEntryDigest: string;
  /** Digest of the protected checkpoint that published that head. */
  checkpointDigest: string;
}>;

export type WatcherUserEventCoverageStore = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_COVERAGE_SCHEMA_VERSION;
  read(): WatcherUserEventCoverageRecord | null;
  /** Replaces the single row in place. */
  write(record: WatcherUserEventCoverageRecord): void;
  clear(): void;
}>;

const authenticate = (
  key: Uint8Array,
  record: WatcherUserEventCoverageRecord,
): string =>
  createHmac("sha256", key)
    .update(WATCHER_USER_EVENT_COVERAGE_SCHEMA_VERSION)
    .update("\n")
    .update(watcherCanonicalJson(record))
    .digest("hex");

export const assertWatcherUserEventCoverageRecord = (
  record: WatcherUserEventCoverageRecord,
): WatcherUserEventCoverageRecord => {
  if (
    !HEX_32.test(record.blockHash) ||
    !NATURAL.test(record.blockNo) ||
    BigInt(record.blockNo) > MAXIMUM_SQLITE_INTEGER ||
    !NATURAL.test(record.slot) ||
    BigInt(record.slot) > MAXIMUM_SQLITE_INTEGER ||
    !HEX_32.test(record.headEntryDigest) ||
    !HEX_32.test(record.checkpointDigest)
  )
    throw new Error("watcher user-event coverage record is malformed");
  return Object.freeze({
    blockHash: record.blockHash,
    blockNo: record.blockNo,
    slot: record.slot,
    headEntryDigest: record.headEntryDigest,
    checkpointDigest: record.checkpointDigest,
  });
};

const assertKey = (key: Uint8Array): Uint8Array => {
  if (!(key instanceof Uint8Array) || key.length !== 32)
    throw new Error("user-event coverage store requires a 32-byte key");
  return Uint8Array.from(key);
};

export const createWatcherSqliteUserEventCoverageStore = (input: {
  readonly database: DatabaseSync;
  readonly authenticationKey: Uint8Array;
}): WatcherUserEventCoverageStore => {
  const key = assertKey(input.authenticationKey);
  const database = input.database;
  database.exec(`
    CREATE TABLE IF NOT EXISTS watcher_user_event_coverage_v1 (
      id INTEGER PRIMARY KEY CHECK (id = 1),
      block_hash TEXT NOT NULL CHECK (length(block_hash) = 64),
      block_no INTEGER NOT NULL,
      slot INTEGER NOT NULL,
      head_entry_digest TEXT NOT NULL CHECK (length(head_entry_digest) = 64),
      checkpoint_digest TEXT NOT NULL CHECK (length(checkpoint_digest) = 64),
      mac TEXT NOT NULL CHECK (length(mac) = 64)
    ) STRICT;
  `);
  const select = database.prepare(
    "SELECT block_hash, block_no, slot, head_entry_digest, checkpoint_digest, mac FROM watcher_user_event_coverage_v1 WHERE id = 1",
  );
  const upsert = database.prepare(
    "INSERT INTO watcher_user_event_coverage_v1(id, block_hash, block_no, slot, head_entry_digest, checkpoint_digest, mac) VALUES (1, ?, ?, ?, ?, ?, ?) ON CONFLICT(id) DO UPDATE SET block_hash = excluded.block_hash, block_no = excluded.block_no, slot = excluded.slot, head_entry_digest = excluded.head_entry_digest, checkpoint_digest = excluded.checkpoint_digest, mac = excluded.mac",
  );
  const remove = database.prepare(
    "DELETE FROM watcher_user_event_coverage_v1 WHERE id = 1",
  );
  const transaction = <T>(work: () => T): T => {
    database.exec("BEGIN IMMEDIATE");
    try {
      const result = work();
      database.exec("COMMIT");
      return result;
    } catch (error) {
      try {
        database.exec("ROLLBACK");
      } catch {
        // Preserve the original failure.
      }
      throw error;
    }
  };
  const verified = (row: Record<string, unknown>) => {
    if (
      typeof row.block_hash !== "string" ||
      typeof row.block_no !== "number" ||
      !Number.isSafeInteger(row.block_no) ||
      row.block_no < 0 ||
      typeof row.slot !== "number" ||
      !Number.isSafeInteger(row.slot) ||
      row.slot < 0 ||
      typeof row.head_entry_digest !== "string" ||
      typeof row.checkpoint_digest !== "string" ||
      typeof row.mac !== "string" ||
      !HEX_32.test(row.mac)
    )
      throw new Error("watcher SQLite user-event coverage row is malformed");
    const record = assertWatcherUserEventCoverageRecord({
      blockHash: row.block_hash,
      blockNo: row.block_no.toString(),
      slot: row.slot.toString(),
      headEntryDigest: row.head_entry_digest,
      checkpointDigest: row.checkpoint_digest,
    });
    if (
      !timingSafeEqual(
        Buffer.from(row.mac, "hex"),
        Buffer.from(authenticate(key, record), "hex"),
      )
    )
      throw new Error(
        "watcher SQLite user-event coverage row failed authentication",
      );
    return record;
  };
  return Object.freeze({
    schemaVersion: WATCHER_USER_EVENT_COVERAGE_SCHEMA_VERSION,
    read: () => {
      const row = select.get() as Record<string, unknown> | undefined;
      return row === undefined ? null : verified(row);
    },
    write: (input) =>
      transaction(() => {
        const record = assertWatcherUserEventCoverageRecord(input);
        upsert.run(
          record.blockHash,
          Number(record.blockNo),
          Number(record.slot),
          record.headEntryDigest,
          record.checkpointDigest,
          authenticate(key, record),
        );
        const readBack = select.get() as Record<string, unknown> | undefined;
        if (
          readBack === undefined ||
          watcherCanonicalJson(verified(readBack)) !==
            watcherCanonicalJson(record)
        )
          throw new Error("watcher user-event coverage write read-back failed");
      }),
    clear: () =>
      transaction(() => {
        remove.run();
      }),
  });
};

/** Volatile coverage for owners without a SQLite store (tests). Restart then
 * resumes from the head entry, which is always sound. */
export const createInMemoryWatcherUserEventCoverageStore =
  (): WatcherUserEventCoverageStore => {
    let current: WatcherUserEventCoverageRecord | null = null;
    return Object.freeze({
      schemaVersion: WATCHER_USER_EVENT_COVERAGE_SCHEMA_VERSION,
      read: () => current,
      write: (record) => {
        current = assertWatcherUserEventCoverageRecord(record);
      },
      clear: () => {
        current = null;
      },
    });
  };
