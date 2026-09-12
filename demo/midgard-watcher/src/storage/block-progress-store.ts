import { createHmac, timingSafeEqual } from "node:crypto";
import type { DatabaseSync } from "node:sqlite";

import type { WatcherNativeChainSyncPoint } from "../l1/native-chain-sync.js";
import type { WatcherBlockRelevance } from "../runtime/block-relevance.js";
import { watcherCanonicalJson } from "./durable-store.js";

export const WATCHER_BLOCK_PROGRESS_SCHEMA_VERSION =
  "midgard-watcher-block-progress-v1" as const;

const HEX_32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const MAXIMUM_SQLITE_INTEGER = 9_223_372_036_854_775_807n;
/** Rows retained behind the head when no older retention floor is requested. */
export const WATCHER_BLOCK_PROGRESS_RETAINED_ROWS = 2_160;
/** Restart intersection candidates offered to the native node, newest first. */
export const WATCHER_BLOCK_PROGRESS_CANDIDATE_LIMIT = 96;

/**
 * "I have processed through this block." One row per release-final block the
 * coordinator finished, quiet or touched, authenticated with the rollback
 * authority key so a foreign or damaged row cannot become a resume point.
 */
export type WatcherBlockProgressRecord = Readonly<{
  blockHash: string;
  parentBlockHash: string;
  blockNo: string;
  slot: string;
  relevance: WatcherBlockRelevance;
}>;

export type WatcherBlockProgressStore = Readonly<{
  schemaVersion: typeof WATCHER_BLOCK_PROGRESS_SCHEMA_VERSION;
  /** Latest authenticated row after verifying the whole retained chain. */
  readHead(): WatcherBlockProgressRecord | null;
  /** Ascending rows with `afterBlockNo < blockNo <= throughBlockNo`. */
  readRange(input: {
    readonly afterBlockNo: string;
    readonly throughBlockNo: string;
  }): readonly WatcherBlockProgressRecord[];
  /** Newest-first spaced resume candidates for native FindIntersect. */
  readCandidates(): readonly WatcherBlockProgressRecord[];
  /**
   * Appends the head's direct child. `retainFromBlockNo` keeps every row at or
   * above that block regardless of the ring size, so the ancestry back to
   * the durable finality authority is always reconstructible.
   */
  record(
    record: WatcherBlockProgressRecord,
    options?: Readonly<{ retainFromBlockNo?: string }>,
  ): void;
  /** Deletes every row above the fork point. */
  rollbackTo(point: WatcherNativeChainSyncPoint): void;
}>;

const authenticate = (
  key: Uint8Array,
  record: WatcherBlockProgressRecord,
): string =>
  createHmac("sha256", key)
    .update(WATCHER_BLOCK_PROGRESS_SCHEMA_VERSION)
    .update("\n")
    .update(watcherCanonicalJson(record))
    .digest("hex");

const parseRecord = (value: unknown): WatcherBlockProgressRecord => {
  const row = value as Record<string, unknown>;
  if (
    typeof row.block_hash !== "string" ||
    !HEX_32.test(row.block_hash) ||
    typeof row.parent_hash !== "string" ||
    !HEX_32.test(row.parent_hash) ||
    typeof row.block_no !== "number" ||
    !Number.isSafeInteger(row.block_no) ||
    row.block_no < 0 ||
    typeof row.slot !== "number" ||
    !Number.isSafeInteger(row.slot) ||
    row.slot < 0 ||
    (row.relevance !== "quiet" && row.relevance !== "touched")
  ) {
    throw new Error("watcher SQLite block progress row is malformed");
  }
  return Object.freeze({
    blockHash: row.block_hash,
    parentBlockHash: row.parent_hash,
    blockNo: row.block_no.toString(),
    slot: row.slot.toString(),
    relevance: row.relevance,
  });
};

export const createWatcherSqliteBlockProgressStore = (input: {
  readonly database: DatabaseSync;
  readonly authenticationKey: Uint8Array;
}): WatcherBlockProgressStore => {
  if (
    !(input.authenticationKey instanceof Uint8Array) ||
    input.authenticationKey.length !== 32
  )
    throw new Error("block progress store requires a 32-byte key");
  const key = Uint8Array.from(input.authenticationKey);
  const database = input.database;
  database.exec(`
    CREATE TABLE IF NOT EXISTS watcher_block_progress_v1 (
      block_no INTEGER PRIMARY KEY,
      block_hash TEXT NOT NULL CHECK (length(block_hash) = 64),
      parent_hash TEXT NOT NULL CHECK (length(parent_hash) = 64),
      slot INTEGER NOT NULL,
      relevance TEXT NOT NULL CHECK (relevance IN ('quiet', 'touched')),
      mac TEXT NOT NULL CHECK (length(mac) = 64)
    ) STRICT;
  `);
  const selectAll = database.prepare(
    "SELECT block_no, block_hash, parent_hash, slot, relevance, mac FROM watcher_block_progress_v1 ORDER BY block_no ASC",
  );
  const selectHead = database.prepare(
    "SELECT block_no, block_hash, parent_hash, slot, relevance, mac FROM watcher_block_progress_v1 ORDER BY block_no DESC LIMIT 1",
  );
  const selectRange = database.prepare(
    "SELECT block_no, block_hash, parent_hash, slot, relevance, mac FROM watcher_block_progress_v1 WHERE block_no > ? AND block_no <= ? ORDER BY block_no ASC",
  );
  const insert = database.prepare(
    "INSERT INTO watcher_block_progress_v1(block_no, block_hash, parent_hash, slot, relevance, mac) VALUES (?, ?, ?, ?, ?, ?)",
  );
  const prune = database.prepare(
    "DELETE FROM watcher_block_progress_v1 WHERE block_no < ?",
  );
  const deleteAbove = database.prepare(
    "DELETE FROM watcher_block_progress_v1 WHERE slot > ? OR (slot = ? AND block_hash <> ?)",
  );
  const deleteAll = database.prepare("DELETE FROM watcher_block_progress_v1");

  const verified = (row: unknown): WatcherBlockProgressRecord => {
    const record = parseRecord(row);
    const mac = (row as { mac?: unknown }).mac;
    const expected = Buffer.from(authenticate(key, record), "hex");
    if (
      typeof mac !== "string" ||
      !HEX_32.test(mac) ||
      !timingSafeEqual(Buffer.from(mac, "hex"), expected)
    )
      throw new Error(
        "watcher SQLite block progress row failed authentication",
      );
    return record;
  };
  const verifiedChain = (): readonly WatcherBlockProgressRecord[] => {
    const rows = selectAll.all().map(verified);
    for (let index = 1; index < rows.length; index += 1) {
      const previous = rows[index - 1]!;
      const current = rows[index]!;
      if (
        current.parentBlockHash !== previous.blockHash ||
        BigInt(current.blockNo) !== BigInt(previous.blockNo) + 1n ||
        BigInt(current.slot) <= BigInt(previous.slot)
      )
        throw new Error("watcher SQLite block progress chain is discontinuous");
    }
    return Object.freeze(rows);
  };
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
  const toInteger = (value: string, label: string): number => {
    if (!NATURAL.test(value) || BigInt(value) > MAXIMUM_SQLITE_INTEGER)
      throw new Error(`watcher block progress ${label} is invalid`);
    return Number(value);
  };

  return Object.freeze({
    schemaVersion: WATCHER_BLOCK_PROGRESS_SCHEMA_VERSION,
    readHead: () => {
      const chain = verifiedChain();
      return chain.at(-1) ?? null;
    },
    readRange: ({ afterBlockNo, throughBlockNo }) =>
      Object.freeze(
        selectRange
          .all(
            toInteger(afterBlockNo, "range start"),
            toInteger(throughBlockNo, "range end"),
          )
          .map(verified),
      ),
    readCandidates: () => {
      const chain = verifiedChain();
      if (chain.length === 0) return Object.freeze([]);
      const selected: WatcherBlockProgressRecord[] = [];
      const head = chain.length - 1;
      // Dense near the head, then geometrically sparser: a short outage costs
      // a one-block replay while a long fork still finds an ancestor.
      let offset = 0;
      let step = 1;
      while (
        head - offset >= 0 &&
        selected.length < WATCHER_BLOCK_PROGRESS_CANDIDATE_LIMIT
      ) {
        selected.push(chain[head - offset]!);
        offset += step;
        if (selected.length >= 8 && step < 1024) step *= 2;
      }
      const oldest = chain[0]!;
      if (
        selected.at(-1) !== oldest &&
        selected.length < WATCHER_BLOCK_PROGRESS_CANDIDATE_LIMIT
      )
        selected.push(oldest);
      return Object.freeze(selected);
    },
    record: (record, options) =>
      transaction(() => {
        if (
          !HEX_32.test(record.blockHash) ||
          !HEX_32.test(record.parentBlockHash) ||
          (record.relevance !== "quiet" && record.relevance !== "touched")
        )
          throw new Error("watcher block progress record is malformed");
        const blockNo = toInteger(record.blockNo, "block number");
        const slot = toInteger(record.slot, "slot");
        const headRow = selectHead.get();
        const head = headRow === undefined ? null : verified(headRow);
        if (
          head !== null &&
          (record.parentBlockHash !== head.blockHash ||
            BigInt(record.blockNo) !== BigInt(head.blockNo) + 1n ||
            BigInt(record.slot) <= BigInt(head.slot))
        )
          throw new Error(
            "watcher block progress record is not the head's direct child",
          );
        const canonical = Object.freeze({
          blockHash: record.blockHash,
          parentBlockHash: record.parentBlockHash,
          blockNo: record.blockNo,
          slot: record.slot,
          relevance: record.relevance,
        });
        insert.run(
          blockNo,
          record.blockHash,
          record.parentBlockHash,
          slot,
          record.relevance,
          authenticate(key, canonical),
        );
        const ringFloor = blockNo - WATCHER_BLOCK_PROGRESS_RETAINED_ROWS;
        const retainFrom =
          options?.retainFromBlockNo === undefined
            ? ringFloor
            : Math.min(
                ringFloor,
                toInteger(options.retainFromBlockNo, "retention floor"),
              );
        if (retainFrom > 0) prune.run(retainFrom);
      }),
    rollbackTo: (point) =>
      transaction(() => {
        if (point.kind === "origin") {
          deleteAll.run();
          return;
        }
        if (!HEX_32.test(point.blockHash))
          throw new Error("watcher block progress rollback point is malformed");
        const slot = toInteger(point.slot, "rollback slot");
        deleteAbove.run(slot, slot, point.blockHash);
        verifiedChain();
      }),
  });
};
