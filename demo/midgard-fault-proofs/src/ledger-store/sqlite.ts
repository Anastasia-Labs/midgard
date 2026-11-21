/**
 * The SQLite storage adapter, for the watcher. A separate entry point
 * (`./ledger-store/sqlite`) so that loading the ledger store never loads
 * `node:sqlite`.
 *
 * Nodes are kept in the trie library's JSON shape, keyed by their 32 hash
 * bytes, and outputs keyed by their out-ref bytes; each row carries its
 * reference count. The dead queue is a table of node hashes. Every change is
 * one IMMEDIATE transaction.
 */
import { DatabaseSync } from "node:sqlite";

import {
  isSqliteMutexBusy,
  SqliteProcessMutex,
} from "@al-ft/midgard-core/sqlite-process-mutex";

import type {
  LedgerStorage,
  LedgerStorageChange,
  NodeHash,
  OutRefHex,
  StoredNode,
} from "./storage.js";

export type SqliteLedgerStorage = LedgerStorage & {
  /** Closes the database and releases the writer lease. */
  close(): void;
};

const SCHEMA = `
  CREATE TABLE IF NOT EXISTS ledger_nodes (
    hash BLOB PRIMARY KEY,
    refs INTEGER NOT NULL,
    node TEXT NOT NULL
  ) WITHOUT ROWID;
  CREATE TABLE IF NOT EXISTS ledger_outputs (
    out_ref BLOB PRIMARY KEY,
    refs INTEGER NOT NULL,
    output BLOB NOT NULL
  ) WITHOUT ROWID;
  CREATE TABLE IF NOT EXISTS ledger_roots (
    root BLOB PRIMARY KEY
  ) WITHOUT ROWID;
  CREATE TABLE IF NOT EXISTS ledger_dead (
    hash BLOB PRIMARY KEY
  ) WITHOUT ROWID;
`;

const bytes = (hex: string): Buffer => Buffer.from(hex, "hex");
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
    `);
  } catch (error) {
    database.close();
    lease.close();
    throw error;
  }

  const statement = {
    node: database.prepare("SELECT node FROM ledger_nodes WHERE hash = ?"),
    nodeRefs: database.prepare("SELECT refs FROM ledger_nodes WHERE hash = ?"),
    putNode: database.prepare(
      "INSERT INTO ledger_nodes (hash, refs, node) VALUES (?, ?, ?)",
    ),
    setNodeRefs: database.prepare(
      "UPDATE ledger_nodes SET refs = ? WHERE hash = ?",
    ),
    deleteNode: database.prepare("DELETE FROM ledger_nodes WHERE hash = ?"),
    output: database.prepare(
      "SELECT output FROM ledger_outputs WHERE out_ref = ?",
    ),
    outputRefs: database.prepare(
      "SELECT refs FROM ledger_outputs WHERE out_ref = ?",
    ),
    putOutput: database.prepare(
      "INSERT INTO ledger_outputs (out_ref, refs, output) VALUES (?, ?, ?)",
    ),
    setOutputRefs: database.prepare(
      "UPDATE ledger_outputs SET refs = ? WHERE out_ref = ?",
    ),
    deleteOutput: database.prepare(
      "DELETE FROM ledger_outputs WHERE out_ref = ?",
    ),
    root: database.prepare("SELECT 1 AS held FROM ledger_roots WHERE root = ?"),
    roots: database.prepare("SELECT root FROM ledger_roots"),
    putRoot: database.prepare(
      "INSERT OR IGNORE INTO ledger_roots (root) VALUES (?)",
    ),
    deleteRoot: database.prepare("DELETE FROM ledger_roots WHERE root = ?"),
    dead: database.prepare("SELECT hash FROM ledger_dead LIMIT ?"),
    queueDead: database.prepare(
      "INSERT OR IGNORE INTO ledger_dead (hash) VALUES (?)",
    ),
    unqueueDead: database.prepare("DELETE FROM ledger_dead WHERE hash = ?"),
    countNodes: database.prepare(
      "SELECT COUNT(*) AS n, COALESCE(SUM(refs), 0) AS refs FROM ledger_nodes",
    ),
    countOutputs: database.prepare(
      "SELECT COUNT(*) AS n, COALESCE(SUM(refs), 0) AS refs FROM ledger_outputs",
    ),
    countRoots: database.prepare(
      "SELECT COUNT(*) AS n, 0 AS refs FROM ledger_roots",
    ),
    countDead: database.prepare(
      "SELECT COUNT(*) AS n, 0 AS refs FROM ledger_dead",
    ),
  };

  const refsOf = (query: typeof statement.nodeRefs, keys: readonly string[]) =>
    keys.map((key) => {
      const row = query.get(bytes(key)) as
        | { refs: number | bigint }
        | undefined;
      return row === undefined ? undefined : Number(row.refs);
    });

  const apply = (change: LedgerStorageChange) => {
    for (const [hash, { node, refs }] of change.putNodes)
      statement.putNode.run(bytes(hash), refs, JSON.stringify(node));
    for (const [hash, refs] of change.nodeRefs)
      statement.setNodeRefs.run(refs, bytes(hash));
    for (const hash of change.deleteNodes)
      statement.deleteNode.run(bytes(hash));
    for (const [outRef, { output, refs }] of change.putOutputs)
      statement.putOutput.run(bytes(outRef), refs, output);
    for (const [outRef, refs] of change.outputRefs)
      statement.setOutputRefs.run(refs, bytes(outRef));
    for (const outRef of change.deleteOutputs)
      statement.deleteOutput.run(bytes(outRef));
    for (const root of change.addRoots) statement.putRoot.run(bytes(root));
    for (const root of change.removeRoots)
      statement.deleteRoot.run(bytes(root));
    for (const hash of change.queueDead) statement.queueDead.run(bytes(hash));
    for (const hash of change.unqueueDead)
      statement.unqueueDead.run(bytes(hash));
  };

  const count = (query: typeof statement.countNodes) => {
    const row = query.get() as { n: number | bigint; refs: number | bigint };
    return { n: Number(row.n), refs: Number(row.refs) };
  };

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
    nodeRefs: async (hashes) => refsOf(statement.nodeRefs, hashes),
    outputRefs: async (outRefs) => refsOf(statement.outputRefs, outRefs),
    isRetained: async (root) => statement.root.get(bytes(root)) !== undefined,
    retainedRoots: async () =>
      (statement.roots.all() as { root: Uint8Array }[]).map(({ root }) =>
        hex(root),
      ),
    deadNodes: async (limit) =>
      (statement.dead.all(limit) as { hash: Uint8Array }[]).map(({ hash }) =>
        hex(hash),
      ),
    write: async (change) => {
      database.exec("BEGIN IMMEDIATE");
      try {
        apply(change);
        database.exec("COMMIT");
      } catch (error) {
        database.exec("ROLLBACK");
        throw error;
      }
    },
    counts: async () => {
      const nodes = count(statement.countNodes);
      const outputs = count(statement.countOutputs);
      return {
        nodes: nodes.n,
        outputs: outputs.n,
        roots: count(statement.countRoots).n,
        nodeRefs: nodes.refs,
        outputRefs: outputs.refs,
        dead: count(statement.countDead).n,
      };
    },
    close: () => {
      try {
        database.close();
      } finally {
        lease.close();
      }
    },
  };
};
