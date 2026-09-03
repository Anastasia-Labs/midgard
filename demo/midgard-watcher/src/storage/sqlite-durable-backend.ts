import { createHash } from "node:crypto";
import { mkdir, realpath, stat } from "node:fs/promises";
import { dirname, isAbsolute, normalize } from "node:path";
import { DatabaseSync } from "node:sqlite";

import {
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherNativeChainSyncPoint } from "../l1/native-chain-sync.js";
import type { WatcherDurableAtomicBackend } from "./durable-store.js";
import { watcherCanonicalJson } from "./durable-store.js";

export const WATCHER_SQLITE_DURABLE_BACKEND_SCHEMA_VERSION =
  "midgard-watcher-sqlite-durable-backend-v1" as const;

const HEX_32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const MAXIMUM_RETAINED_STATE_QUEUE_OBSERVATIONS = 2_160;
const MAXIMUM_SQLITE_INTEGER = 9_223_372_036_854_775_807n;

const canonicalDatabasePath = (value: unknown): string => {
  if (
    typeof value !== "string" ||
    value !== value.trim() ||
    !isAbsolute(value) ||
    normalize(value) !== value ||
    value === "/" ||
    value === "/tmp" ||
    value.startsWith("/tmp/")
  ) {
    throw new Error("watcher SQLite backend requires a canonical durable path");
  }
  return value;
};

const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

const deepFreezeJson = (value: unknown): unknown => {
  if (typeof value !== "object" || value === null) return value;
  if (Array.isArray(value)) {
    for (const item of value) deepFreezeJson(item);
  } else {
    for (const item of Object.values(value)) deepFreezeJson(item);
  }
  return Object.freeze(value);
};

export type WatcherSqliteDurableBackend = Readonly<{
  schemaVersion: typeof WATCHER_SQLITE_DURABLE_BACKEND_SCHEMA_VERSION;
  backend: WatcherDurableAtomicBackend;
  stateQueueObservations: WatcherSqliteStateQueueObservationStore;
  close(): void;
}>;

export type WatcherSqliteStateQueueObservationStore = Readonly<{
  /** Returns an untrusted cache. The production source must reauthenticate it. */
  readAll(): Promise<readonly unknown[]>;
  /** Persists only module-admitted observations and ignores an exact repeat. */
  append(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<"appended" | "unchanged">;
  /** Revokes cache entries on the discarded side of a native rollback. */
  rollbackTo(point: WatcherNativeChainSyncPoint): Promise<void>;
}>;

/**
 * Production complete-snapshot CAS over SQLite. The independent trusted-head
 * authority deliberately does not share this database or its backup domain.
 */
const openWatcherSqliteDurableBackendInternal = async (
  input: {
    readonly path: string;
    readonly busyTimeoutMs?: number;
  },
  assertStateQueueObservation: (
    observation: WatcherAuthenticatedStateQueueObservation,
  ) => void,
): Promise<WatcherSqliteDurableBackend> => {
  const path = canonicalDatabasePath(input.path);
  const directory = dirname(path);
  await mkdir(directory, { recursive: true, mode: 0o700 });
  if ((await realpath(directory)) !== directory) {
    throw new Error("watcher SQLite directory traverses a symlink");
  }
  try {
    if (
      (await stat(path)).isSymbolicLink() ||
      (await realpath(path)) !== path
    ) {
      throw new Error("watcher SQLite path traverses a symlink");
    }
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
  }
  const busyTimeoutMs = input.busyTimeoutMs ?? 5_000;
  if (
    !Number.isSafeInteger(busyTimeoutMs) ||
    busyTimeoutMs < 1 ||
    busyTimeoutMs > 120_000
  ) {
    throw new Error("watcher SQLite busy timeout is invalid");
  }

  const database = new DatabaseSync(path, {
    open: true,
    readOnly: false,
    enableForeignKeyConstraints: true,
  });
  database.exec(`
    PRAGMA journal_mode = WAL;
    PRAGMA synchronous = FULL;
    PRAGMA trusted_schema = OFF;
    PRAGMA busy_timeout = ${busyTimeoutMs.toString()};
    CREATE TABLE IF NOT EXISTS watcher_durable_snapshot_v1 (
      singleton INTEGER PRIMARY KEY CHECK (singleton = 1),
      sha256 TEXT NOT NULL CHECK (length(sha256) = 64),
      bytes BLOB NOT NULL CHECK (length(bytes) > 0)
    ) STRICT;
    CREATE TABLE IF NOT EXISTS watcher_state_queue_observation_v1 (
      sequence INTEGER PRIMARY KEY AUTOINCREMENT,
      observation_digest TEXT NOT NULL UNIQUE CHECK (length(observation_digest) = 64),
      previous_observation_digest TEXT CHECK (
        previous_observation_digest IS NULL OR length(previous_observation_digest) = 64
      ),
      block_hash TEXT NOT NULL CHECK (length(block_hash) = 64),
      block_no TEXT NOT NULL,
      slot TEXT NOT NULL,
      record_sha256 TEXT NOT NULL CHECK (length(record_sha256) = 64),
      canonical_json TEXT NOT NULL CHECK (length(canonical_json) > 0)
    ) STRICT;
  `);

  const select = database.prepare(
    "SELECT sha256, bytes FROM watcher_durable_snapshot_v1 WHERE singleton = 1",
  );
  const insert = database.prepare(
    "INSERT INTO watcher_durable_snapshot_v1(singleton, sha256, bytes) VALUES (1, ?, ?)",
  );
  const update = database.prepare(
    "UPDATE watcher_durable_snapshot_v1 SET sha256 = ?, bytes = ? WHERE singleton = 1 AND sha256 = ?",
  );

  const readRow = (): Readonly<{
    sha256: string;
    bytes: Uint8Array;
  }> | null => {
    const row = select.get() as
      | Readonly<{ sha256: unknown; bytes: unknown }>
      | undefined;
    if (row === undefined) return null;
    if (
      typeof row.sha256 !== "string" ||
      !HEX_32.test(row.sha256) ||
      !(row.bytes instanceof Uint8Array) ||
      row.bytes.byteLength === 0
    ) {
      throw new Error("watcher SQLite durable snapshot is malformed");
    }
    const bytes = Uint8Array.from(row.bytes);
    if (sha256(bytes) !== row.sha256) {
      throw new Error("watcher SQLite durable snapshot digest mismatch");
    }
    return Object.freeze({ sha256: row.sha256, bytes });
  };

  const backend: WatcherDurableAtomicBackend = Object.freeze({
    read: async () => readRow()?.bytes ?? null,
    compareAndSwap: async (expectedSha256, next) => {
      if (
        (expectedSha256 !== null && !HEX_32.test(expectedSha256)) ||
        !(next instanceof Uint8Array) ||
        next.byteLength === 0
      ) {
        return false;
      }
      const copy = Uint8Array.from(next);
      const nextSha256 = sha256(copy);
      database.exec("BEGIN IMMEDIATE");
      try {
        const current = readRow();
        if ((current === null ? null : current.sha256) !== expectedSha256) {
          database.exec("ROLLBACK");
          return false;
        }
        if (current === null) {
          insert.run(nextSha256, copy);
        } else {
          const result = update.run(nextSha256, copy, expectedSha256);
          if (result.changes !== 1) {
            database.exec("ROLLBACK");
            return false;
          }
        }
        database.exec("COMMIT");
        return readRow()?.sha256 === nextSha256;
      } catch (error) {
        try {
          database.exec("ROLLBACK");
        } catch {
          // Preserve the original persistence error.
        }
        throw error;
      }
    },
  });

  const selectStateQueueObservations = database.prepare(`
    SELECT sequence, observation_digest, previous_observation_digest,
           block_hash, block_no, slot, record_sha256, canonical_json
    FROM watcher_state_queue_observation_v1
    ORDER BY sequence ASC
  `);
  const selectLatestStateQueueObservation = database.prepare(`
    SELECT sequence, observation_digest, previous_observation_digest,
           block_hash, block_no, slot, record_sha256, canonical_json
    FROM watcher_state_queue_observation_v1
    ORDER BY sequence DESC
    LIMIT 1
  `);
  const insertStateQueueObservation = database.prepare(`
    INSERT INTO watcher_state_queue_observation_v1(
      observation_digest, previous_observation_digest, block_hash, block_no,
      slot, record_sha256, canonical_json
    ) VALUES (?, ?, ?, ?, ?, ?, ?)
  `);
  const compactStateQueueObservations = database.prepare(`
    DELETE FROM watcher_state_queue_observation_v1
    WHERE sequence NOT IN (
      SELECT sequence
      FROM watcher_state_queue_observation_v1
      ORDER BY sequence DESC
      LIMIT ?
    )
  `);
  const deleteAllStateQueueObservations = database.prepare(
    "DELETE FROM watcher_state_queue_observation_v1",
  );
  const rollbackStateQueueObservations = database.prepare(`
    DELETE FROM watcher_state_queue_observation_v1
    WHERE CAST(slot AS INTEGER) > CAST(? AS INTEGER)
       OR (slot = ? AND block_hash <> ?)
  `);

  type StateQueueObservationRow = Readonly<{
    sequence: unknown;
    observation_digest: unknown;
    previous_observation_digest: unknown;
    block_hash: unknown;
    block_no: unknown;
    slot: unknown;
    record_sha256: unknown;
    canonical_json: unknown;
  }>;

  const parseStateQueueObservationRow = (
    value: StateQueueObservationRow,
  ): Readonly<{
    sequence: number;
    observationDigest: string;
    previousObservationDigest: string | null;
    blockHash: string;
    blockNo: string;
    slot: string;
    canonicalJson: string;
    value: unknown;
  }> => {
    if (
      typeof value.sequence !== "number" ||
      !Number.isSafeInteger(value.sequence) ||
      value.sequence < 1 ||
      typeof value.observation_digest !== "string" ||
      !HEX_32.test(value.observation_digest) ||
      (value.previous_observation_digest !== null &&
        (typeof value.previous_observation_digest !== "string" ||
          !HEX_32.test(value.previous_observation_digest))) ||
      typeof value.block_hash !== "string" ||
      !HEX_32.test(value.block_hash) ||
      typeof value.block_no !== "string" ||
      !NATURAL.test(value.block_no) ||
      BigInt(value.block_no) > MAXIMUM_SQLITE_INTEGER ||
      typeof value.slot !== "string" ||
      !NATURAL.test(value.slot) ||
      BigInt(value.slot) > MAXIMUM_SQLITE_INTEGER ||
      typeof value.record_sha256 !== "string" ||
      !HEX_32.test(value.record_sha256) ||
      typeof value.canonical_json !== "string" ||
      value.canonical_json.length === 0 ||
      sha256(Buffer.from(value.canonical_json, "utf8")) !== value.record_sha256
    ) {
      throw new Error("watcher SQLite state-queue observation is malformed");
    }
    let parsed: unknown;
    try {
      parsed = JSON.parse(value.canonical_json);
    } catch {
      throw new Error("watcher SQLite state-queue observation is malformed");
    }
    if (
      watcherCanonicalJson(parsed) !== value.canonical_json ||
      typeof parsed !== "object" ||
      parsed === null ||
      (parsed as { observationDigest?: unknown }).observationDigest !==
        value.observation_digest ||
      (parsed as { previousObservationDigest?: unknown })
        .previousObservationDigest !== value.previous_observation_digest ||
      (parsed as { nativePoint?: { blockHash?: unknown } }).nativePoint
        ?.blockHash !== value.block_hash ||
      (parsed as { nativePoint?: { blockNo?: unknown } }).nativePoint
        ?.blockNo !== value.block_no ||
      (parsed as { nativePoint?: { slot?: unknown } }).nativePoint?.slot !==
        value.slot
    ) {
      throw new Error(
        "watcher SQLite state-queue observation metadata mismatch",
      );
    }
    return Object.freeze({
      sequence: value.sequence,
      observationDigest: value.observation_digest,
      previousObservationDigest: value.previous_observation_digest,
      blockHash: value.block_hash,
      blockNo: value.block_no,
      slot: value.slot,
      canonicalJson: value.canonical_json,
      value: deepFreezeJson(parsed),
    });
  };

  const readStateQueueObservationRows = () => {
    const rows =
      selectStateQueueObservations.all() as StateQueueObservationRow[];
    const parsed = rows.map(parseStateQueueObservationRow);
    for (let index = 1; index < parsed.length; index += 1) {
      const previous = parsed[index - 1]!;
      const current = parsed[index]!;
      if (
        current.previousObservationDigest !== previous.observationDigest ||
        BigInt(current.blockNo) <= BigInt(previous.blockNo) ||
        BigInt(current.slot) <= BigInt(previous.slot)
      ) {
        throw new Error(
          "watcher SQLite state-queue observation chain is discontinuous",
        );
      }
    }
    if (parsed.length > MAXIMUM_RETAINED_STATE_QUEUE_OBSERVATIONS) {
      throw new Error(
        "watcher SQLite state-queue observation cache exceeds its release bound",
      );
    }
    return parsed;
  };

  const latestStateQueueObservation = () => {
    const row = selectLatestStateQueueObservation.get() as
      | StateQueueObservationRow
      | undefined;
    return row === undefined ? null : parseStateQueueObservationRow(row);
  };

  const stateQueueObservations: WatcherSqliteStateQueueObservationStore =
    Object.freeze({
      readAll: async () =>
        Object.freeze(
          readStateQueueObservationRows().map(({ value }) => value),
        ),
      append: async (observation) => {
        assertStateQueueObservation(observation);
        const canonicalJson = watcherCanonicalJson(observation);
        database.exec("BEGIN IMMEDIATE");
        try {
          const latest = latestStateQueueObservation();
          if (latest?.observationDigest === observation.observationDigest) {
            if (latest.canonicalJson !== canonicalJson) {
              throw new Error(
                "watcher state-queue observation digest was substituted",
              );
            }
            database.exec("COMMIT");
            return "unchanged" as const;
          }
          if (
            latest !== null &&
            (observation.previousObservationDigest !==
              latest.observationDigest ||
              BigInt(observation.nativePoint.blockNo) <=
                BigInt(latest.blockNo) ||
              BigInt(observation.nativePoint.slot) <= BigInt(latest.slot))
          ) {
            throw new Error(
              "watcher state-queue observation append is non-successor",
            );
          }
          insertStateQueueObservation.run(
            observation.observationDigest,
            observation.previousObservationDigest,
            observation.nativePoint.blockHash,
            observation.nativePoint.blockNo,
            observation.nativePoint.slot,
            sha256(Buffer.from(canonicalJson, "utf8")),
            canonicalJson,
          );
          compactStateQueueObservations.run(
            MAXIMUM_RETAINED_STATE_QUEUE_OBSERVATIONS,
          );
          database.exec("COMMIT");
          const appended = latestStateQueueObservation();
          if (
            appended?.observationDigest !== observation.observationDigest ||
            appended.canonicalJson !== canonicalJson
          ) {
            throw new Error(
              "watcher state-queue observation append read-back failed",
            );
          }
          return "appended" as const;
        } catch (error) {
          try {
            database.exec("ROLLBACK");
          } catch {
            // Preserve the original persistence failure.
          }
          throw error;
        }
      },
      rollbackTo: async (point) => {
        database.exec("BEGIN IMMEDIATE");
        try {
          if (point.kind === "origin") {
            deleteAllStateQueueObservations.run();
          } else {
            rollbackStateQueueObservations.run(
              point.slot,
              point.slot,
              point.blockHash,
            );
          }
          // A rollback can reveal corruption in the retained prefix; audit it
          // before the caller is allowed to resume authenticated replay.
          readStateQueueObservationRows();
          database.exec("COMMIT");
        } catch (error) {
          try {
            database.exec("ROLLBACK");
          } catch {
            // Preserve the original persistence failure.
          }
          throw error;
        }
      },
    });

  return Object.freeze({
    schemaVersion: WATCHER_SQLITE_DURABLE_BACKEND_SCHEMA_VERSION,
    backend,
    stateQueueObservations,
    close: () => database.close(),
  });
};

export const openWatcherSqliteDurableBackend = async (input: {
  readonly path: string;
  readonly busyTimeoutMs?: number;
}): Promise<WatcherSqliteDurableBackend> =>
  await openWatcherSqliteDurableBackendInternal(
    input,
    assertWatcherStateQueueObservation,
  );

/** Test-only authority seam; production always uses the opaque source guard. */
export const unsafeOpenWatcherSqliteDurableBackendForTest = async (
  input: {
    readonly path: string;
    readonly busyTimeoutMs?: number;
  },
  unsafeAssertStateQueueObservationForTest: (
    observation: WatcherAuthenticatedStateQueueObservation,
  ) => void,
): Promise<WatcherSqliteDurableBackend> =>
  await openWatcherSqliteDurableBackendInternal(
    input,
    unsafeAssertStateQueueObservationForTest,
  );
