/** Shared record storage inside the watcher's existing SQLite transaction owner. */
import { createHash } from "node:crypto";
import { DatabaseSync } from "node:sqlite";

import {
  decodeWatcherDurableRecord,
  encodeWatcherDurableRecord,
  type WatcherEncodedRecord,
  type WatcherRecordEncoding,
} from "./durable-record-codec.js";
import type { WatcherDurableAtomicBackend } from "./durable-store.js";
import {
  copyWatcherUserEventArchiveBytes,
  type WatcherUserEventArchive,
} from "./user-event-checkpoint.js";

const HEX_32 = /^[0-9a-f]{64}$/u;
const MAX_CACHE_BYTES = 256 * 1024 * 1024;
const MAX_KNOWN_RECORDS = 65_536;
const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");
type StoredRow = Readonly<{
  sha256: unknown;
  encoding: unknown;
  storage_sha256: unknown;
  bytes: unknown;
}>;
const archiveIntegrity = new WeakMap<
  WatcherUserEventArchive,
  (digests: readonly string[]) => void
>();

/** Only archives created over this module's owned SQLite connection can avoid
 * copying validated bytes. A caller-supplied verify method grants no trust. */
export const verifyWatcherSqliteArchiveRecords = (
  archive: WatcherUserEventArchive,
  digests: readonly string[],
): boolean => {
  const verify = archiveIntegrity.get(archive);
  if (verify === undefined) return false;
  verify(digests);
  return true;
};

export const createWatcherSqliteRecordStore = (
  database: DatabaseSync,
): Readonly<{
  backend: WatcherDurableAtomicBackend;
  userEventArchive: WatcherUserEventArchive;
}> => {
  if (!(database instanceof DatabaseSync))
    throw new Error("watcher record store requires its SQLite connection");
  database.exec(`
    CREATE TABLE IF NOT EXISTS watcher_durable_snapshot_v1 (
      singleton INTEGER PRIMARY KEY CHECK (singleton = 1),
      sha256 TEXT NOT NULL CHECK (length(sha256) = 64),
      encoding TEXT NOT NULL CHECK (encoding IN ('raw', 'store', 'authority', 'event')),
      storage_sha256 TEXT NOT NULL CHECK (length(storage_sha256) = 64),
      bytes BLOB NOT NULL CHECK (length(bytes) > 0)
    ) STRICT;
    CREATE TABLE IF NOT EXISTS watcher_user_event_archive_v1 (
      digest TEXT PRIMARY KEY CHECK (length(digest) = 64),
      encoding TEXT NOT NULL CHECK (encoding IN ('raw', 'store', 'authority', 'event')),
      storage_sha256 TEXT NOT NULL CHECK (length(storage_sha256) = 64),
      bytes BLOB NOT NULL CHECK (length(bytes) > 0 AND length(bytes) <= 134217728)
    ) STRICT;
  `);
  const selectSnapshot = database.prepare(
    "SELECT sha256, encoding, storage_sha256, bytes FROM watcher_durable_snapshot_v1 WHERE singleton = 1",
  );
  const insertSnapshot = database.prepare(
    "INSERT INTO watcher_durable_snapshot_v1 VALUES (1, ?, ?, ?, ?)",
  );
  const updateSnapshot = database.prepare(
    "UPDATE watcher_durable_snapshot_v1 SET sha256 = ?, encoding = ?, storage_sha256 = ?, bytes = ? WHERE singleton = 1 AND sha256 = ?",
  );
  const selectArchive = database.prepare(
    "SELECT digest AS sha256, encoding, storage_sha256, bytes FROM watcher_user_event_archive_v1 WHERE digest = ?",
  );
  const insertArchive = database.prepare(
    "INSERT INTO watcher_user_event_archive_v1 VALUES (?, ?, ?, ?)",
  );
  const version = database.prepare("PRAGMA data_version");
  const cache = new Map<string, Uint8Array>();
  const knownRecords = new Set<string>();
  let cacheBytes = 0;
  let admittedVersion: number | undefined;

  const invalidate = () => {
    cache.clear();
    knownRecords.clear();
    cacheBytes = 0;
  };
  const remember = (key: string, bytes: Uint8Array) => {
    if (cache.has(key) || bytes.length > MAX_CACHE_BYTES) return;
    while (cacheBytes + bytes.length > MAX_CACHE_BYTES) {
      const oldest = cache.entries().next().value;
      if (oldest === undefined) break;
      cache.delete(oldest[0]);
      cacheBytes -= oldest[1].length;
    }
    cache.set(key, bytes);
    cacheBytes += bytes.length;
  };
  const transaction = <T>(work: () => T): T => {
    // The operation is synchronous and never yields with this lock held.
    // A stable database view is required while following record references.
    database.exec("BEGIN IMMEDIATE");
    try {
      const currentVersion = (version.get() as { data_version: number })
        .data_version;
      if (currentVersion !== admittedVersion) {
        invalidate();
        admittedVersion = currentVersion;
      }
      const result = work();
      database.exec("COMMIT");
      return result;
    } catch (cause) {
      invalidate();
      try {
        database.exec("ROLLBACK");
      } catch {
        // Preserve the original failure, including a failed COMMIT.
      }
      throw cause;
    }
  };
  const parseRow = (
    row: StoredRow,
  ): Readonly<{
    digest: string;
    record: WatcherEncodedRecord;
  }> => {
    if (
      typeof row.sha256 !== "string" ||
      !HEX_32.test(row.sha256) ||
      typeof row.storage_sha256 !== "string" ||
      !HEX_32.test(row.storage_sha256) ||
      !["raw", "store", "authority", "event"].includes(
        row.encoding as string,
      ) ||
      !(row.bytes instanceof Uint8Array) ||
      row.bytes.length === 0
    )
      throw new Error("watcher SQLite durable record is malformed");
    if (sha256(row.bytes) !== row.storage_sha256)
      throw new Error("watcher SQLite durable record digest mismatch");
    return {
      digest: row.sha256,
      record: {
        encoding: row.encoding as WatcherRecordEncoding,
        bytes: row.bytes,
      },
    };
  };
  const reading = new Set<string>();
  const readRecord = (key: string): Uint8Array => {
    const remembered = cache.get(key);
    if (remembered !== undefined) return remembered;
    if (reading.has(key) || reading.size >= 64)
      throw new Error(
        "watcher SQLite record references are cyclic or too deep",
      );
    const row = selectArchive.get(key) as StoredRow | undefined;
    if (row === undefined)
      throw new Error("watcher SQLite required archive record is missing");
    reading.add(key);
    try {
      const stored = parseRow(row);
      const decoded = decodeWatcherDurableRecord(stored.record, readRecord);
      if (stored.digest !== key || sha256(decoded) !== key)
        throw new Error("watcher SQLite archive object digest mismatch");
      remember(key, decoded);
      if (knownRecords.size >= MAX_KNOWN_RECORDS) knownRecords.clear();
      knownRecords.add(key);
      return decoded;
    } finally {
      reading.delete(key);
    }
  };
  const putRecord = (key: string, record: WatcherEncodedRecord): void => {
    if (knownRecords.has(key)) return;
    const existing = selectArchive.get(key) as StoredRow | undefined;
    if (existing === undefined) {
      insertArchive.run(
        key,
        record.encoding,
        sha256(record.bytes),
        record.bytes,
      );
      if (record.encoding === "raw") remember(key, record.bytes);
    } else {
      // An exact repeat must still detect an externally damaged old object.
      readRecord(key);
    }
    if (knownRecords.size >= MAX_KNOWN_RECORDS) knownRecords.clear();
    knownRecords.add(key);
  };
  const readSnapshot = (): Readonly<{
    digest: string;
    bytes: Uint8Array;
  }> | null => {
    const row = selectSnapshot.get() as StoredRow | undefined;
    if (row === undefined) return null;
    // Always freshly read the small marker. External database writes invalidate
    // cached record bytes before this operation, even if the marker is unchanged.
    const stored = parseRow(row);
    let bytes = cache.get(stored.digest);
    if (bytes === undefined) {
      bytes = decodeWatcherDurableRecord(stored.record, readRecord);
      if (sha256(bytes) !== stored.digest)
        throw new Error("watcher SQLite durable snapshot digest mismatch");
      remember(stored.digest, bytes);
    }
    return { digest: stored.digest, bytes };
  };
  const backend: WatcherDurableAtomicBackend = {
    read: async () =>
      transaction(() => {
        const current = readSnapshot();
        return current === null ? null : Uint8Array.from(current.bytes);
      }),
    compareAndSwap: async (expected, next, canonicalValue) => {
      if (
        (expected !== null && !HEX_32.test(expected)) ||
        !(next instanceof Uint8Array) ||
        next.length === 0
      )
        return false;
      const bytes = Uint8Array.from(next);
      const key = sha256(bytes);
      return transaction(() => {
        const current = readSnapshot();
        if ((current?.digest ?? null) !== expected) return false;
        const record = encodeWatcherDurableRecord(
          bytes,
          putRecord,
          canonicalValue,
          (digest) => knownRecords.has(digest),
        );
        if (current === null) {
          insertSnapshot.run(
            key,
            record.encoding,
            sha256(record.bytes),
            record.bytes,
          );
        } else {
          const updated = updateSnapshot.run(
            key,
            record.encoding,
            sha256(record.bytes),
            record.bytes,
            expected,
          );
          if (updated.changes !== 1)
            throw new Error("watcher SQLite snapshot CAS conflicted");
        }
        remember(key, bytes);
        return readSnapshot()?.digest === key;
      });
    },
  };
  const userEventArchive: WatcherUserEventArchive = {
    read: async (key) =>
      transaction(() => {
        if (!HEX_32.test(key))
          throw new Error("watcher user-event archive digest is invalid");
        if (selectArchive.get(key) === undefined) return null;
        return copyWatcherUserEventArchiveBytes(readRecord(key));
      }),
    put: async (input) => {
      const bytes = copyWatcherUserEventArchiveBytes(input);
      const key = sha256(bytes);
      return transaction(() => {
        if (selectArchive.get(key) !== undefined) {
          if (!Buffer.from(readRecord(key)).equals(bytes))
            throw new Error("watcher user-event archive object is immutable");
        } else {
          putRecord(key, encodeWatcherDurableRecord(bytes, putRecord));
          remember(key, bytes);
        }
        return key;
      });
    },
  };
  archiveIntegrity.set(userEventArchive, (digests) =>
    transaction(() => {
      for (const key of digests) {
        if (!HEX_32.test(key))
          throw new Error("watcher user-event archive digest is invalid");
        if (!knownRecords.has(key)) readRecord(key);
      }
    }),
  );
  return {
    backend: Object.freeze(backend),
    userEventArchive: Object.freeze(userEventArchive),
  };
};
