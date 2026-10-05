import { DatabaseSync } from "node:sqlite";
import { pathToFileURL } from "node:url";

import type { WatcherRollbackDurableTrustedHead } from "../l1/rollback-engine.js";
import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  AUTHORITY_ENVELOPE_MAX_BYTES,
  type AuthorityEnvelopeCodec,
} from "./trusted-head-authority.envelope-codec.js";
import {
  makeAuthorityRecord,
  MAX_RECORD_BYTES,
  recordKeyId,
  revision,
  sameHead,
  sha256,
} from "./trusted-head-authority.exact-record.js";
import type { AuthorityRecordCodec } from "./trusted-head-authority.record-codec.js";
import { SCHEMAS, TABLES } from "./trusted-head-authority.sqlite-schema.js";

const DIGEST = /^[0-9a-f]{64}$/u;
const bytes = (value: unknown): Uint8Array => {
  if (!(value instanceof Uint8Array))
    throw new Error("trusted-head authority bounded bytes missing or invalid");
  return value;
};
const digest = (value: unknown): string => {
  if (typeof value !== "string" || !DIGEST.test(value))
    throw new Error("trusted-head authority digest is invalid");
  return value;
};
const key = (r: bigint) => r.toString().padStart(20, "0");

export type AuthorityCasResult = Readonly<{
  committed: boolean;
  head: WatcherRollbackDurableTrustedHead | null;
}>;
export type SqliteAuthorityStore = Readonly<{
  readCurrent(): Promise<WatcherRollbackDurableTrustedHead | null>;
  compareAndSwap(input: {
    expectedTrustedHead: unknown | null;
    nextTrustedHead: unknown;
  }): Promise<AuthorityCasResult>;
  readRecordAuthenticationKeyId(): Promise<string>;
  close(): void;
}>;

/** Selected opens use SQLite URI mode=rw, never implicit database creation. */
export const openSqliteAuthority = (
  input: Readonly<{
    databasePath: string;
    records: AuthorityRecordCodec;
    envelopes: AuthorityEnvelopeCodec;
    initializationSha256: string;
  }>,
): SqliteAuthorityStore => {
  const { records, envelopes, initializationSha256 } = input;
  const db = new DatabaseSync(
    `${pathToFileURL(input.databasePath).href}?mode=rw`,
  );
  try {
    if (db.prepare("PRAGMA database_list").get()?.file !== input.databasePath)
      throw new Error(
        "trusted-head authority SQLite path differs from selection",
      );
    if (db.prepare("PRAGMA journal_mode").get()?.journal_mode !== "wal")
      throw new Error(
        "trusted-head authority SQLite requires persisted WAL mode",
      );
    db.exec(
      "PRAGMA synchronous=FULL; PRAGMA busy_timeout=1000; PRAGMA wal_autocheckpoint=64;",
    );
    if (db.prepare("PRAGMA synchronous").get()?.synchronous !== 2)
      throw new Error(
        "trusted-head authority SQLite FULL synchronization unavailable",
      );
  } catch (error) {
    db.close();
    throw error;
  }
  const singleton = (name: (typeof TABLES)[number], required: boolean) => {
    const rows = db
      .prepare(
        `SELECT id, CASE WHEN length(bytes) BETWEEN 1 AND ${AUTHORITY_ENVELOPE_MAX_BYTES} THEN bytes ELSE NULL END AS bytes FROM ${name} ORDER BY id LIMIT 2`,
      )
      .all();
    if (
      rows.length !== (required ? 1 : 0) &&
      !(name === "authority_checkpoint" && rows.length === 1)
    )
      throw new Error(
        "trusted-head authority singleton cardinality is invalid",
      );
    if (rows.length === 0) return null;
    if (rows[0]!.id !== 1)
      throw new Error("trusted-head authority singleton identity is invalid");
    return bytes(rows[0]!.bytes);
  };
  const verify = () => {
    const schemas = db
      .prepare(
        "SELECT name, type, substr(sql,1,8192) AS sql FROM sqlite_schema WHERE name NOT GLOB 'sqlite_*' ORDER BY name LIMIT 5",
      )
      .all();
    if (
      schemas.length !== 4 ||
      schemas.some(
        (row) =>
          row.type !== "table" ||
          typeof row.name !== "string" ||
          SCHEMAS.get(row.name) !== row.sql,
      )
    )
      throw new Error("trusted-head authority SQLite schema differs");
    const initBytes = singleton("authority_initialization", true)!;
    if (sha256(initBytes) !== initializationSha256)
      throw new Error(
        "trusted-head authority initialization receipt differs from selector",
      );
    const initial = envelopes.decode("initialization", initBytes, [
      "sourceKind",
      "initialHead",
      "initialRecordSha256",
      "sourceChainSha256",
    ]);
    if (initial.sourceKind !== "new" && initial.sourceKind !== "legacy")
      throw new Error("trusted-head authority initialization source invalid");
    const initialHead =
      initial.initialHead === null
        ? null
        : records.admitHead(initial.initialHead);
    if ((initialHead === null) !== (initial.initialRecordSha256 === null))
      throw new Error(
        "trusted-head authority initialization head/hash differs",
      );
    if (initial.initialRecordSha256 !== null)
      digest(initial.initialRecordSha256);
    if (
      initial.sourceKind === "new" &&
      (initialHead !== null || initial.sourceChainSha256 !== null)
    )
      throw new Error(
        "trusted-head authority new initialization was not empty",
      );
    if (initial.sourceKind === "legacy") digest(initial.sourceChainSha256);
    const currentBytes = singleton("authority_current", true)!;
    const current = envelopes.decode("current", currentBytes, [
      "head",
      "recordSha256",
      "checkpointSha256",
      "initializationSha256",
    ]);
    if (current.initializationSha256 !== initializationSha256)
      throw new Error("trusted-head authority current receipt differs");
    const checkpointBytes = singleton("authority_checkpoint", false);
    const checkpoint =
      checkpointBytes === null
        ? null
        : envelopes.decode("checkpoint", checkpointBytes, ["boundaryRecord"]);
    if (
      current.checkpointSha256 !==
      (checkpointBytes === null ? null : sha256(checkpointBytes))
    )
      throw new Error("trusted-head authority checkpoint binding differs");
    const rows = db
      .prepare(
        `SELECT revision, CASE WHEN length(bytes) BETWEEN 1 AND ${MAX_RECORD_BYTES} THEN bytes ELSE NULL END AS bytes FROM authority_records ORDER BY revision LIMIT ?`,
      )
      .all(envelopes.liveRecordLimit + 1);
    const head = current.head === null ? null : records.admitHead(current.head);
    if (head === null) {
      if (
        initialHead !== null ||
        current.recordSha256 !== null ||
        checkpoint !== null ||
        rows.length !== 0
      )
        throw new Error("trusted-head authority empty state is inconsistent");
      return {
        head,
        recordSha256: null,
        rows: [] as Readonly<{
          record: ReturnType<typeof records.admitRecord>;
          recordSha256: string;
        }>[],
      };
    }
    const r = revision(head),
      k = BigInt(envelopes.liveRecordLimit);
    if (initialHead !== null && r < revision(initialHead))
      throw new Error("trusted-head authority predates imported head");
    const expectedCount = Number(r + 1n < k ? r + 1n : k);
    if (rows.length !== expectedCount || (checkpoint === null) !== r < k)
      throw new Error("trusted-head authority live suffix geometry is invalid");
    let prior: string | null = null;
    if (checkpoint !== null) {
      const boundary = records.admitRecord(checkpoint.boundaryRecord);
      if (revision(boundary.head) !== r - k)
        throw new Error("trusted-head authority checkpoint revision differs");
      prior = sha256(watcherCanonicalJson(boundary));
    }
    const admitted = rows.map((row, i) => {
      const entry = records.admitRecordBytes(bytes(row.bytes));
      const expected = r - BigInt(expectedCount) + 1n + BigInt(i);
      if (
        row.revision !== key(expected) ||
        revision(entry.record.head) !== expected ||
        entry.record.priorRecordSha256 !== prior
      )
        throw new Error("trusted-head authority live record chain differs");
      prior = entry.recordSha256;
      return entry;
    });
    if (
      current.recordSha256 !== prior ||
      !sameHead(admitted.at(-1)!.record.head, head)
    )
      throw new Error("trusted-head authority current is not exact suffix tip");
    return { head, recordSha256: prior, rows: admitted };
  };
  const transaction = <T>(write: boolean, run: () => T): T => {
    db.exec(write ? "BEGIN IMMEDIATE" : "BEGIN");
    try {
      const result = run();
      db.exec("COMMIT");
      return result;
    } catch (error) {
      try {
        db.exec("ROLLBACK");
      } catch {
        /* Commit/IO failure is ambiguous; preserve original failure. */
      }
      throw error;
    }
  };
  try {
    transaction(false, verify);
  } catch (error) {
    db.close();
    throw error;
  }
  return Object.freeze({
    readRecordAuthenticationKeyId: async () => {
      transaction(false, verify);
      return recordKeyId(records.recordAuthenticationKey);
    },
    readCurrent: async () => transaction(false, verify).head,
    compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) =>
      transaction(true, () => {
        const current = verify();
        const expected =
          expectedTrustedHead === null
            ? null
            : records.admitHead(expectedTrustedHead, true);
        const next = records.admitHead(nextTrustedHead, true);
        const nextRevision = revision(next);
        if (
          !sameHead(current.head, expected) ||
          (expected === null
            ? nextRevision !== 0n
            : nextRevision !== revision(expected) + 1n)
        )
          return { committed: false, head: current.head };
        const record = makeAuthorityRecord({
          head: next,
          priorRecordSha256: current.recordSha256,
          recordAuthenticationKey: records.recordAuthenticationKey,
        });
        const recordBytes = Buffer.from(watcherCanonicalJson(record));
        records.admitRecordBytes(recordBytes);
        db.prepare(
          "INSERT INTO authority_records(revision,bytes) VALUES(?,?)",
        ).run(key(nextRevision), recordBytes);
        let checkpointSha256: string | null = null;
        if (nextRevision >= BigInt(envelopes.liveRecordLimit)) {
          const boundary = current.rows[0]!;
          const checkpointBytes = envelopes.encode("checkpoint", {
            boundaryRecord: boundary.record,
          });
          checkpointSha256 = sha256(checkpointBytes);
          db.prepare(
            "INSERT OR REPLACE INTO authority_checkpoint(id,bytes) VALUES(1,?)",
          ).run(checkpointBytes);
          db.prepare("DELETE FROM authority_records WHERE revision = ?").run(
            key(revision(boundary.record.head)),
          );
        }
        db.prepare("UPDATE authority_current SET bytes=? WHERE id=1").run(
          envelopes.encode("current", {
            head: next,
            recordSha256: sha256(recordBytes),
            checkpointSha256,
            initializationSha256,
          }),
        );
        verify();
        return { committed: true, head: next };
      }),
    close: () => db.close(),
  });
};

/** A killed pre-commit CREATE transaction leaves no application schema. Only an
 * authenticated, unselected preparation owner may rebuild this exact state. */
export const isUncommittedAuthorityDatabase = (
  databasePath: string,
): boolean => {
  const db = new DatabaseSync(`${pathToFileURL(databasePath).href}?mode=rw`);
  try {
    return (
      db
        .prepare(
          "SELECT name FROM sqlite_schema WHERE name NOT GLOB 'sqlite_*' LIMIT 1",
        )
        .all().length === 0
    );
  } finally {
    db.close();
  }
};
