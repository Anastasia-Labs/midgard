import { createHash } from "node:crypto";
import type { DatabaseSync } from "node:sqlite";

import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";

import {
  assertWatcherAuthenticatedReplayTranscript,
  type WatcherAuthenticatedReplayTranscript,
  watcherAuthenticatedReplayTranscriptCborHex,
} from "../verification/authenticated-replay-transcript.js";

export type WatcherReplayTranscriptIdentity = Readonly<{
  deploymentFingerprint: string;
  headerHash: string;
  inclusionPoint: Readonly<{
    transactionHash: string;
    blockHash: string;
    blockNo: string;
    slot: string;
    chainPointId: string;
  }>;
}>;

/** Persisted bytes are untrusted input to fresh semantic replay, never authority. */
export type WatcherReplayTranscriptStore = Readonly<{
  read(identity: WatcherReplayTranscriptIdentity): Promise<Readonly<{
    headTranscriptDigest: string;
    previousTranscriptDigest: string | null;
    persistedTranscriptCborHex: string;
    chainLength: number;
  }> | null>;
  compareAndSwap(input: {
    readonly expectedTranscriptDigest: string | null;
    readonly transcript: WatcherAuthenticatedReplayTranscript;
  }): Promise<boolean>;
}>;

/** Operational ceilings fail closed; the archive never prunes an original. */
export type WatcherReplayTranscriptStorageLimits = Readonly<{
  maximumTranscriptBytes: number;
  maximumChainLength: number;
  maximumRows: number;
  maximumTotalBytes: number;
}>;

export const WATCHER_REPLAY_TRANSCRIPT_STORAGE_LIMITS: WatcherReplayTranscriptStorageLimits =
  Object.freeze({
    maximumTranscriptBytes: 128 * 1024 * 1024,
    maximumChainLength: 4_096,
    maximumRows: 100_000,
    maximumTotalBytes: 64 * 1024 * 1024 * 1024,
  });

const HEX_32 = /^[0-9a-f]{64}$/u;
const sha256 = (bytes: Uint8Array | string): string =>
  createHash("sha256").update(bytes).digest("hex");
const identityKey = (identity: WatcherReplayTranscriptIdentity): string => {
  const point = identity.inclusionPoint;
  const fields = [
    identity.deploymentFingerprint,
    identity.headerHash,
    point.transactionHash,
    point.blockHash,
    point.blockNo,
    point.slot,
    point.chainPointId,
  ];
  if (
    fields.some((value) => typeof value !== "string") ||
    !HEX_32.test(fields[0]!) ||
    !/^[0-9a-f]{56}$/u.test(fields[1]!) ||
    !HEX_32.test(fields[2]!) ||
    !HEX_32.test(fields[3]!) ||
    !/^(?:0|[1-9][0-9]{0,19})$/u.test(fields[4]!) ||
    !/^(?:0|[1-9][0-9]{0,19})$/u.test(fields[5]!) ||
    !HEX_32.test(fields[6]!)
  ) {
    throw new Error("watcher replay transcript identity is invalid");
  }
  return JSON.stringify(fields);
};

type ArchiveRow = Readonly<{
  transcript_digest: unknown;
  previous_digest: unknown;
  bytes_sha256: unknown;
  record_sha256: unknown;
  bytes: unknown;
}>;

/** Attaches separate tables to the backend's existing WAL/FULL database. */
export const createWatcherSqliteReplayTranscriptStore = (
  database: DatabaseSync,
  overrides: Partial<WatcherReplayTranscriptStorageLimits> = {},
): WatcherReplayTranscriptStore => {
  const limits = { ...WATCHER_REPLAY_TRANSCRIPT_STORAGE_LIMITS, ...overrides };
  for (const key of Object.keys(limits) as (keyof typeof limits)[]) {
    if (
      !Number.isSafeInteger(limits[key]) ||
      limits[key] < 1 ||
      limits[key] > WATCHER_REPLAY_TRANSCRIPT_STORAGE_LIMITS[key]
    ) {
      throw new Error("watcher replay transcript storage limit is invalid");
    }
  }
  database.exec(`
    CREATE TABLE IF NOT EXISTS watcher_replay_transcript (
      identity TEXT NOT NULL CHECK (length(identity) BETWEEN 1 AND 512),
      transcript_digest TEXT NOT NULL CHECK (length(transcript_digest) = 64),
      previous_digest TEXT CHECK (previous_digest IS NULL OR length(previous_digest) = 64),
      bytes_sha256 TEXT NOT NULL CHECK (length(bytes_sha256) = 64),
      record_sha256 TEXT NOT NULL CHECK (length(record_sha256) = 64),
      bytes BLOB NOT NULL CHECK (length(bytes) BETWEEN 1 AND 134217728),
      PRIMARY KEY (identity, transcript_digest)
    ) STRICT;
    CREATE TABLE IF NOT EXISTS watcher_replay_transcript_head (
      identity TEXT PRIMARY KEY CHECK (length(identity) BETWEEN 1 AND 512),
      root_digest TEXT NOT NULL CHECK (length(root_digest) = 64),
      current_digest TEXT NOT NULL CHECK (length(current_digest) = 64),
      chain_length INTEGER NOT NULL CHECK (chain_length BETWEEN 1 AND 4096)
    ) STRICT;
  `);
  const head = database.prepare(
    "SELECT root_digest, current_digest, chain_length FROM watcher_replay_transcript_head WHERE identity = ?",
  );
  const row = database.prepare(
    `SELECT transcript_digest, previous_digest, bytes_sha256, record_sha256, CASE WHEN length(bytes) BETWEEN 1 AND ${limits.maximumTranscriptBytes.toString()} THEN bytes ELSE NULL END AS bytes FROM watcher_replay_transcript WHERE identity = ? AND transcript_digest = ?`,
  );
  const count = database.prepare(
    "SELECT count(*) AS count FROM watcher_replay_transcript WHERE identity = ?",
  );
  const total = database.prepare(
    "SELECT count(*) AS count, coalesce(sum(length(bytes)), 0) AS bytes FROM watcher_replay_transcript",
  );
  const insert = database.prepare(
    "INSERT INTO watcher_replay_transcript VALUES (?, ?, ?, ?, ?, ?)",
  );
  const insertHead = database.prepare(
    "INSERT INTO watcher_replay_transcript_head VALUES (?, ?, ?, 1)",
  );
  const updateHead = database.prepare(
    "UPDATE watcher_replay_transcript_head SET current_digest = ?, chain_length = chain_length + 1 WHERE identity = ? AND current_digest = ?",
  );
  const recordDigest = (
    key: string,
    digest: string,
    previous: string | null,
    bytesDigest: string,
  ) => sha256(JSON.stringify([key, digest, previous, bytesDigest]));
  const usage = () => {
    const value = total.get() as { count: number; bytes: number };
    if (
      !Number.isSafeInteger(value.count) ||
      !Number.isSafeInteger(value.bytes) ||
      value.count > limits.maximumRows ||
      value.bytes > limits.maximumTotalBytes
    ) {
      throw new Error(
        "watcher replay transcript archive exceeds storage limits",
      );
    }
    return value;
  };
  const readRow = (key: string, digest: string) => {
    const value = row.get(key, digest) as ArchiveRow | undefined;
    if (
      value === undefined ||
      value.transcript_digest !== digest ||
      !HEX_32.test(digest) ||
      (value.previous_digest !== null &&
        (typeof value.previous_digest !== "string" ||
          !HEX_32.test(value.previous_digest))) ||
      !(value.bytes instanceof Uint8Array) ||
      value.bytes.length < 1 ||
      value.bytes.length > limits.maximumTranscriptBytes ||
      typeof value.bytes_sha256 !== "string" ||
      sha256(value.bytes) !== value.bytes_sha256 ||
      recordDigest(key, digest, value.previous_digest, value.bytes_sha256) !==
        value.record_sha256
    ) {
      throw new Error(
        "watcher replay transcript archive row is missing or corrupt",
      );
    }
    return { digest, previous: value.previous_digest, bytes: value.bytes };
  };
  const audit = (key: string) => {
    usage();
    const current = head.get(key) as
      | { root_digest: unknown; current_digest: unknown; chain_length: unknown }
      | undefined;
    const rowCount = (count.get(key) as { count: number }).count;
    if (current === undefined) {
      if (rowCount !== 0)
        throw new Error("watcher replay transcript archive has no head");
      return null;
    }
    if (
      typeof current.root_digest !== "string" ||
      !HEX_32.test(current.root_digest) ||
      typeof current.current_digest !== "string" ||
      !HEX_32.test(current.current_digest) ||
      typeof current.chain_length !== "number" ||
      !Number.isSafeInteger(current.chain_length) ||
      current.chain_length < 1 ||
      current.chain_length > limits.maximumChainLength ||
      current.chain_length !== rowCount
    ) {
      throw new Error(
        "watcher replay transcript archive head or chain length is corrupt",
      );
    }
    const latest = readRow(key, current.current_digest);
    const seen = new Set<string>();
    let cursor: string | null = latest.digest;
    let original: string | null = null;
    // Only one historical blob is read at a time. No authority is reconstructed.
    while (cursor !== null) {
      if (seen.has(cursor) || seen.size >= current.chain_length) {
        throw new Error(
          "watcher replay transcript archive chain is cyclic or excessive",
        );
      }
      seen.add(cursor);
      const entry: ReturnType<typeof readRow> =
        cursor === latest.digest ? latest : readRow(key, cursor);
      original = entry.digest;
      cursor = entry.previous;
    }
    if (
      seen.size !== current.chain_length ||
      original !== current.root_digest
    ) {
      throw new Error(
        "watcher replay transcript archive chain is incomplete or detached",
      );
    }
    const decoded = decodeSingleCbor(latest.bytes);
    if (!(decoded instanceof Map))
      throw new Error(
        "watcher replay transcript archive head is not a CBOR record",
      );
    const point: unknown = decoded.get("inclusionPoint");
    if (
      !(point instanceof Map) ||
      decoded.get("transcriptDigest") !== latest.digest ||
      identityKey({
        deploymentFingerprint: decoded.get("deploymentFingerprint") as string,
        headerHash: decoded.get("headerHash") as string,
        inclusionPoint: {
          transactionHash: point.get("transactionHash") as string,
          blockHash: point.get("blockHash") as string,
          blockNo: point.get("blockNo") as string,
          slot: point.get("slot") as string,
          chainPointId: point.get("chainPointId") as string,
        },
      }) !== key
    ) {
      throw new Error(
        "watcher replay transcript archive head identity differs",
      );
    }
    return Object.freeze({
      headTranscriptDigest: latest.digest,
      previousTranscriptDigest: latest.previous,
      persistedTranscriptCborHex: Buffer.from(latest.bytes).toString("hex"),
      chainLength: current.chain_length,
    });
  };
  const transaction = <T>(
    mode: "BEGIN" | "BEGIN IMMEDIATE",
    action: () => T,
  ): T => {
    database.exec(mode);
    try {
      const result = action();
      database.exec("COMMIT");
      return result;
    } catch (error) {
      try {
        database.exec("ROLLBACK");
      } catch {
        /* Preserve the original storage failure. */
      }
      throw error;
    }
  };
  const store: WatcherReplayTranscriptStore = Object.freeze({
    read: async (identity) =>
      transaction("BEGIN", () => audit(identityKey(identity))),
    compareAndSwap: async ({ expectedTranscriptDigest, transcript }) => {
      assertWatcherAuthenticatedReplayTranscript(transcript);
      const cborHex = watcherAuthenticatedReplayTranscriptCborHex(transcript);
      if (
        (expectedTranscriptDigest !== null &&
          !HEX_32.test(expectedTranscriptDigest)) ||
        !HEX_32.test(transcript.transcriptDigest) ||
        cborHex.length / 2 > limits.maximumTranscriptBytes
      ) {
        throw new Error(
          "watcher replay transcript append exceeds its bound or has an invalid digest",
        );
      }
      const key = identityKey(transcript);
      const bytes = Buffer.from(cborHex, "hex");
      const digest = transcript.transcriptDigest;
      const bytesDigest = sha256(bytes);
      const committed = transaction("BEGIN IMMEDIATE", () => {
        const current = audit(key);
        if (
          (current?.headTranscriptDigest ?? null) !== expectedTranscriptDigest
        )
          return false;
        if (current?.headTranscriptDigest === digest) {
          if (current.persistedTranscriptCborHex !== cborHex)
            throw new Error("watcher replay transcript digest was substituted");
          return true;
        }
        if (row.get(key, digest) !== undefined)
          throw new Error(
            "watcher replay transcript cannot revive an archived head",
          );
        const retained = usage();
        if (
          (current?.chainLength ?? 0) >= limits.maximumChainLength ||
          retained.count >= limits.maximumRows ||
          retained.bytes + bytes.length > limits.maximumTotalBytes
        ) {
          throw new Error(
            "watcher replay transcript append exceeds storage limits",
          );
        }
        insert.run(
          key,
          digest,
          expectedTranscriptDigest,
          bytesDigest,
          recordDigest(key, digest, expectedTranscriptDigest, bytesDigest),
          bytes,
        );
        const written =
          current === null
            ? insertHead.run(key, digest, digest)
            : updateHead.run(digest, key, expectedTranscriptDigest);
        if (written.changes !== 1)
          throw new Error("watcher replay transcript head CAS failed");
        return true;
      });
      if (committed && !Buffer.from(readRow(key, digest).bytes).equals(bytes)) {
        throw new Error("watcher replay transcript archive read-back differs");
      }
      return committed;
    },
  });
  return store;
};
