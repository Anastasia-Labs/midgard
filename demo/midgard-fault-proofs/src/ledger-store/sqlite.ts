/**
 * The SQLite storage adapter, for the watcher. A separate entry point
 * (`./ledger-store/sqlite`) so that loading the ledger store never loads
 * `node:sqlite`.
 *
 * Nodes are kept in the trie library's JSON shape, keyed by their 32 hash
 * bytes. Every node and output row carries the write epoch it was last
 * written or kept in; a sweep deletes only rows stamped at or before the
 * epoch it began in.
 */
import { DatabaseSync } from "node:sqlite";

import {
  isSqliteMutexBusy,
  SqliteProcessMutex,
} from "@al-ft/midgard-core/sqlite-process-mutex";

import { LedgerNodeMissing, LedgerOutputMissing } from "./errors.js";
import type {
  LedgerStorage,
  NodeHash,
  OutRefHex,
  StoredNode,
} from "./storage.js";

export type SqliteLedgerStorage = LedgerStorage & {
  /** Closes the database and releases the writer lease. */
  close(): void;
};

const SCHEMA = `
  CREATE TABLE IF NOT EXISTS ledger_meta (
    key TEXT PRIMARY KEY,
    value INTEGER NOT NULL
  ) WITHOUT ROWID;
  CREATE TABLE IF NOT EXISTS ledger_nodes (
    hash BLOB PRIMARY KEY,
    node TEXT NOT NULL,
    epoch INTEGER NOT NULL
  ) WITHOUT ROWID;
  CREATE TABLE IF NOT EXISTS ledger_outputs (
    out_ref BLOB PRIMARY KEY,
    output BLOB NOT NULL,
    epoch INTEGER NOT NULL
  ) WITHOUT ROWID;
  CREATE TABLE IF NOT EXISTS ledger_roots (
    root BLOB PRIMARY KEY
  ) WITHOUT ROWID;
`;

const bytes = (hex: string): Buffer => Buffer.from(hex, "hex");
/** Whether a keep found no row to stamp. */
const missed = ({ changes }: { changes: number | bigint }): boolean =>
  Number(changes) === 0;
const hex = (value: Uint8Array): string => Buffer.from(value).toString("hex");

/**
 * Opens (creating when absent) the ledger database at `path`. The process
 * holds a writer lease on `<path>.writer-lease` until `close`; a second
 * opener is refused while it is held.
 */
export const openSqliteLedgerStorage = (path: string): SqliteLedgerStorage => {
  let lease: SqliteProcessMutex;
  try {
    lease = SqliteProcessMutex.acquire(`${path}.writer-lease`);
  } catch (error) {
    if (isSqliteMutexBusy(error))
      throw new Error(`ledger database ${path} is open in another process`);
    throw error;
  }
  const database = new DatabaseSync(path, {
    enableForeignKeyConstraints: true,
  });
  try {
    database.exec(`
      PRAGMA journal_mode = WAL;
      PRAGMA synchronous = FULL;
      PRAGMA busy_timeout = 5000;
      ${SCHEMA}
      INSERT OR IGNORE INTO ledger_meta (key, value) VALUES ('write_epoch', 1);
    `);
  } catch (error) {
    database.close();
    lease.close();
    throw error;
  }

  const statement = {
    epoch: database.prepare(
      "SELECT value FROM ledger_meta WHERE key = 'write_epoch'",
    ),
    setEpoch: database.prepare(
      "UPDATE ledger_meta SET value = ? WHERE key = 'write_epoch'",
    ),
    node: database.prepare("SELECT node FROM ledger_nodes WHERE hash = ?"),
    output: database.prepare(
      "SELECT output FROM ledger_outputs WHERE out_ref = ?",
    ),
    root: database.prepare("SELECT 1 AS held FROM ledger_roots WHERE root = ?"),
    putNode: database.prepare(
      `INSERT INTO ledger_nodes (hash, node, epoch) VALUES (?, ?, ?)
       ON CONFLICT (hash) DO UPDATE SET epoch = excluded.epoch`,
    ),
    putOutput: database.prepare(
      `INSERT INTO ledger_outputs (out_ref, output, epoch) VALUES (?, ?, ?)
       ON CONFLICT (out_ref) DO UPDATE SET epoch = excluded.epoch`,
    ),
    keepNode: database.prepare(
      "UPDATE ledger_nodes SET epoch = ? WHERE hash = ?",
    ),
    keepOutput: database.prepare(
      "UPDATE ledger_outputs SET epoch = ? WHERE out_ref = ?",
    ),
    putRoot: database.prepare(
      "INSERT OR IGNORE INTO ledger_roots (root) VALUES (?)",
    ),
    clearRoots: database.prepare("DELETE FROM ledger_roots"),
    roots: database.prepare("SELECT root FROM ledger_roots"),
    sweepRunning: database.prepare(
      "SELECT 1 AS held FROM ledger_meta WHERE key = 'sweep_running'",
    ),
    beginSweep: database.prepare(
      "INSERT OR IGNORE INTO ledger_meta (key, value) VALUES ('sweep_running', 1)",
    ),
    endSweep: database.prepare(
      "DELETE FROM ledger_meta WHERE key = 'sweep_running'",
    ),
    sweepNodes: database.prepare(
      `SELECT hash AS key FROM ledger_nodes WHERE epoch <= ? AND hash > ?
       ORDER BY hash LIMIT ?`,
    ),
    sweepOutputs: database.prepare(
      `SELECT out_ref AS key FROM ledger_outputs WHERE epoch <= ? AND out_ref > ?
       ORDER BY out_ref LIMIT ?`,
    ),
    deleteNode: database.prepare(
      "DELETE FROM ledger_nodes WHERE hash = ? AND epoch <= ?",
    ),
    deleteOutput: database.prepare(
      "DELETE FROM ledger_outputs WHERE out_ref = ? AND epoch <= ?",
    ),
    countNodes: database.prepare("SELECT COUNT(*) AS n FROM ledger_nodes"),
    countOutputs: database.prepare("SELECT COUNT(*) AS n FROM ledger_outputs"),
    countRoots: database.prepare("SELECT COUNT(*) AS n FROM ledger_roots"),
  };

  const writeEpoch = () =>
    Number((statement.epoch.get() as { value: number | bigint }).value);
  let sweepEpoch: number | undefined;

  const transaction = (run: () => void) => {
    database.exec("BEGIN IMMEDIATE");
    try {
      run();
      database.exec("COMMIT");
    } catch (error) {
      database.exec("ROLLBACK");
      throw error;
    }
  };

  const page = (
    query: typeof statement.sweepNodes,
    after: string | undefined,
    limit: number,
  ): string[] => {
    if (sweepEpoch === undefined) return [];
    const rows = query.all(
      sweepEpoch,
      after === undefined ? Buffer.alloc(0) : bytes(after),
      limit,
    ) as { key: Uint8Array }[];
    return rows.map(({ key }) => hex(key));
  };

  const count = (query: typeof statement.countNodes) =>
    Number((query.get() as { n: number | bigint }).n);

  return {
    readNodes: async (hashes: readonly NodeHash[]) =>
      hashes.map((hash) => {
        const row = statement.node.get(bytes(hash)) as
          | { node: string }
          | undefined;
        return row === undefined
          ? undefined
          : (JSON.parse(row.node) as StoredNode);
      }),
    readOutputs: async (outRefs: readonly OutRefHex[]) =>
      outRefs.map((outRef) => {
        const row = statement.output.get(bytes(outRef)) as
          | { output: Uint8Array }
          | undefined;
        return row === undefined ? undefined : Buffer.from(row.output);
      }),
    isRetained: async (root) => statement.root.get(bytes(root)) !== undefined,
    writeBlock: async (block) =>
      transaction(() => {
        const epoch = writeEpoch();
        for (const [hash, node] of block.nodes)
          statement.putNode.run(bytes(hash), JSON.stringify(node), epoch);
        for (const [outRef, output] of block.outputs)
          statement.putOutput.run(bytes(outRef), output, epoch);
        for (const hash of block.keptNodes)
          if (missed(statement.keepNode.run(epoch, bytes(hash))))
            throw new LedgerNodeMissing(hash);
        for (const outRef of block.keptOutputs)
          if (missed(statement.keepOutput.run(epoch, bytes(outRef))))
            throw new LedgerOutputMissing(outRef);
        statement.putRoot.run(bytes(block.root));
      }),
    beginSweep: async (retained) => {
      const epoch = writeEpoch();
      transaction(() => {
        statement.setEpoch.run(epoch + 1);
        statement.clearRoots.run();
        for (const root of retained) statement.putRoot.run(bytes(root));
        statement.beginSweep.run();
      });
      sweepEpoch = epoch;
    },
    sweepNodes: async (after, limit) =>
      page(statement.sweepNodes, after, limit),
    sweepOutputs: async (after, limit) =>
      page(statement.sweepOutputs, after, limit),
    deleteSwept: async (records) => {
      const epoch = sweepEpoch;
      if (epoch === undefined) throw new Error("no sweep is running");
      transaction(() => {
        for (const hash of records.nodes)
          statement.deleteNode.run(bytes(hash), epoch);
        for (const outRef of records.outputs)
          statement.deleteOutput.run(bytes(outRef), epoch);
      });
    },
    endSweep: async () => {
      statement.endSweep.run();
      sweepEpoch = undefined;
    },
    unfinishedSweep: async () =>
      statement.sweepRunning.get() === undefined
        ? undefined
        : new Set(
            (statement.roots.all() as { root: Uint8Array }[]).map(({ root }) =>
              hex(root),
            ),
          ),
    counts: async () => ({
      nodes: count(statement.countNodes),
      outputs: count(statement.countOutputs),
      roots: count(statement.countRoots),
    }),
    close: () => {
      try {
        database.close();
      } finally {
        lease.close();
      }
    },
  };
};
