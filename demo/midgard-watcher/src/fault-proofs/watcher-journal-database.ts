import { createHash } from "node:crypto";
import { mkdirSync, realpathSync } from "node:fs";
import { isAbsolute, join, normalize } from "node:path";
import { DatabaseSync, type StatementSync } from "node:sqlite";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  createWatcherJournalCodec,
  MODULUS,
  sameHex,
  sha256Hex,
  ZERO_DIGEST,
} from "./watcher-journal-database.codec.js";
import {
  type Staged,
  type StoredHead,
  type StoredRevision,
  type StoredRow,
  WATCHER_JOURNAL_DATABASE_FILE,
  type WatcherJournalDatabase,
  WatcherJournalIntegrityError,
  type WatcherJournalRow,
  type WatcherJournalRowFilter,
  type WatcherJournalTransaction,
} from "./watcher-journal-database.types.js";
import { verifyWatcherJournal } from "./watcher-journal-database.verify.js";
import {
  WATCHER_JOURNAL_LEDGER_SQL,
  WATCHER_JOURNAL_MIGRATIONS,
  WATCHER_JOURNAL_RETAINED_REVISIONS,
  WATCHER_JOURNAL_TABLES,
  WATCHER_JOURNALS,
  type WatcherJournalName,
} from "./watcher-journal-schema.js";

export {
  isWatcherJournalCapacityError,
  WATCHER_JOURNAL_DATABASE_FILE,
  WatcherJournalCapacityError,
  type WatcherJournalDatabase,
  type WatcherJournalHead,
  WatcherJournalIntegrityError,
  type WatcherJournalRow,
  type WatcherJournalTransaction,
} from "./watcher-journal-database.types.js";

/**
 * The watcher's fault-proof journals as per-row SQLite tables (plan §3.4 WC1,
 * WC4; ticket W2). A commit writes only the rows it changes, so a persist is
 * O(delta), never a rewrite or rescan of the journal.
 *
 * Integrity, checked in full at startup:
 * - each row carries a MAC over its journal, key, scope, state, revision and
 *   body, so an edited row or a row moved to another revision is refused;
 * - each journal's head carries a MAC over its revision, its chained revision
 *   digest, its live row count and a keyed sum of its rows' MACs, so a
 *   deleted, added or replayed row is refused;
 * - the latest revisions keep their chained digest and delta, so a reordered
 *   or substituted revision is refused.
 * A whole-file rollback to an older consistent copy is not detectable without
 * an external anchor; the journals never claimed that.
 *
 * The file must sit on a local disk. Network filesystems break SQLite's
 * locking and its WAL.
 */

const KEY = /^[A-Za-z0-9:._/-]{1,512}$/u;

const canonicalDirectory = (value: string): string => {
  if (
    value.trim() !== value ||
    !isAbsolute(value) ||
    normalize(value) !== value ||
    value === "/" ||
    value === "/tmp" ||
    value.startsWith("/tmp/")
  )
    throw new Error("watcher journals require a canonical durable directory");
  return value;
};

type RowFilter = WatcherJournalRowFilter & Readonly<{ afterRevision?: number }>;

const createDatabase = (
  journalRoot: string,
  authenticationKey: Uint8Array,
): WatcherJournalDatabase => {
  const codec = createWatcherJournalCodec(authenticationKey);
  const directory = canonicalDirectory(journalRoot);
  mkdirSync(directory, { recursive: true, mode: 0o700 });
  if (realpathSync(directory) !== directory)
    throw new Error("watcher journal directory traverses a symlink");
  const path = join(directory, WATCHER_JOURNAL_DATABASE_FILE);

  const database = new DatabaseSync(path);
  database.exec(`
    PRAGMA journal_mode = WAL;
    PRAGMA synchronous = FULL;
    PRAGMA busy_timeout = 5000;
  `);
  const statements = new Map<string, StatementSync>();
  const prepare = (sql: string): StatementSync => {
    let statement = statements.get(sql);
    if (statement === undefined) {
      statement = database.prepare(sql);
      statements.set(sql, statement);
    }
    return statement;
  };
  const inTransaction = <T>(begin: string, run: () => T): T => {
    database.exec(begin);
    try {
      const result = run();
      database.exec("COMMIT");
      return result;
    } catch (error) {
      if (database.isTransaction) database.exec("ROLLBACK");
      throw error;
    }
  };

  const migrate = (): void =>
    inTransaction("BEGIN IMMEDIATE", () => {
      database.exec(WATCHER_JOURNAL_LEDGER_SQL);
      for (const migration of WATCHER_JOURNAL_MIGRATIONS.migrations) {
        const checksum = sha256Hex(migration.sql);
        const applied = prepare(
          "SELECT checksum FROM watcher_journal_migrations WHERE namespace = ? AND id = ?",
        ).get(WATCHER_JOURNAL_MIGRATIONS.namespace, migration.id) as
          | { checksum: string }
          | undefined;
        if (applied !== undefined) {
          if (applied.checksum !== checksum)
            throw new Error(
              `watcher journal migration ${migration.id} changed after it was applied`,
            );
          continue;
        }
        database.exec(migration.sql);
        prepare(
          "INSERT INTO watcher_journal_migrations (namespace, id, checksum) VALUES (?, ?, ?)",
        ).run(WATCHER_JOURNAL_MIGRATIONS.namespace, migration.id, checksum);
      }
    });

  const table = (journal: WatcherJournalName): string => {
    const name = WATCHER_JOURNAL_TABLES[journal];
    if (name === undefined)
      throw new Error(`unknown watcher journal ${journal}`);
    return name;
  };

  const readHead = (journal: WatcherJournalName): StoredHead => {
    const stored = prepare(
      "SELECT revision, chain, live_rows, accumulator, key_id, mac FROM watcher_journal_heads WHERE journal = ?",
    ).get(journal) as
      | {
          revision: number;
          chain: string;
          live_rows: number;
          accumulator: string;
          key_id: string;
          mac: string;
        }
      | undefined;
    if (stored === undefined)
      return { revision: 0, chain: ZERO_DIGEST, liveRows: 0, accumulator: 0n };
    if (stored.key_id !== codec.keyId)
      throw new WatcherJournalIntegrityError(
        journal,
        "head is authenticated by another key",
      );
    const head: StoredHead = {
      revision: Number(stored.revision),
      chain: stored.chain,
      liveRows: Number(stored.live_rows),
      accumulator: BigInt(`0x${stored.accumulator}`),
    };
    if (!sameHex(stored.mac, codec.headMac(journal, head)))
      throw new WatcherJournalIntegrityError(journal, "head MAC differs");
    return head;
  };

  const admitRow = (
    journal: WatcherJournalName,
    stored: StoredRow,
  ): WatcherJournalRow => {
    const revision = Number(stored.revision);
    if (
      !Number.isSafeInteger(revision) ||
      revision < 1 ||
      !sameHex(stored.mac, codec.rowMac(journal, { ...stored, revision }))
    )
      throw new WatcherJournalIntegrityError(
        journal,
        `row ${stored.row_key} MAC differs`,
      );
    let body: unknown;
    try {
      body = JSON.parse(stored.body);
    } catch {
      throw new WatcherJournalIntegrityError(
        journal,
        `row ${stored.row_key} body is malformed`,
      );
    }
    return Object.freeze({
      key: stored.row_key,
      scope: stored.scope,
      state: stored.state,
      revision,
      body,
    });
  };

  const storedRow = (
    journal: WatcherJournalName,
    key: string,
  ): StoredRow | undefined =>
    prepare(
      `SELECT row_key, scope, state, revision, body, mac FROM ${table(journal)} WHERE row_key = ?`,
    ).get(key) as StoredRow | undefined;

  const selectRows = (
    journal: WatcherJournalName,
    filter: RowFilter = {},
  ): StoredRow[] => {
    const clauses: string[] = [];
    const params: (string | number)[] = [];
    if (filter.scope !== undefined) {
      clauses.push("scope = ?");
      params.push(filter.scope);
    }
    if (filter.state !== undefined) {
      clauses.push("state = ?");
      params.push(filter.state);
    }
    if (filter.afterRevision !== undefined) {
      clauses.push("revision > ?");
      params.push(filter.afterRevision);
    }
    return prepare(
      `SELECT row_key, scope, state, revision, body, mac FROM ${table(journal)}${
        clauses.length === 0 ? "" : ` WHERE ${clauses.join(" AND ")}`
      } ORDER BY revision, row_key`,
    ).all(...params) as StoredRow[];
  };

  // The head's authenticated live count is O(1); a state needs its index.
  const countRows = (journal: WatcherJournalName, state?: string): number =>
    state === undefined
      ? readHead(journal).liveRows
      : Number(
          (
            prepare(
              `SELECT count(*) AS n FROM ${table(journal)} WHERE state = ?`,
            ).get(state) as { n: number }
          ).n,
        );

  /** Applies one journal's staged writes as one revision. */
  const applyStaged = (journal: WatcherJournalName, staged: Staged): void => {
    const head = readHead(journal);
    const revision = head.revision + 1;
    let accumulator = head.accumulator;
    let liveRows = head.liveRows;
    const delta: [string, string | null][] = [];
    for (const key of [...staged.keys()].sort()) {
      const write = staged.get(key)!;
      const prior = storedRow(journal, key);
      if (prior !== undefined) {
        admitRow(journal, prior);
        accumulator =
          (accumulator - codec.element(prior.mac) + MODULUS) % MODULUS;
        liveRows -= 1;
      }
      if (write === null) {
        if (prior === undefined) continue;
        prepare(`DELETE FROM ${table(journal)} WHERE row_key = ?`).run(key);
        delta.push([key, null]);
        continue;
      }
      const rowMac = codec.rowMac(journal, {
        row_key: key,
        ...write,
        revision,
      });
      prepare(
        `INSERT INTO ${table(journal)} (row_key, scope, state, revision, body, mac)
         VALUES (?, ?, ?, ?, ?, ?)
         ON CONFLICT (row_key) DO UPDATE SET scope = excluded.scope,
           state = excluded.state, revision = excluded.revision,
           body = excluded.body, mac = excluded.mac`,
      ).run(key, write.scope, write.state, revision, write.body, rowMac);
      accumulator = (accumulator + codec.element(rowMac)) % MODULUS;
      liveRows += 1;
      delta.push([key, rowMac]);
    }
    if (delta.length === 0) return;
    const deltaText = watcherCanonicalJson(delta);
    const chain = codec.chainOf(journal, revision, head.chain, deltaText);
    const next: StoredHead = { revision, chain, liveRows, accumulator };
    prepare(
      `INSERT INTO watcher_journal_heads (journal, revision, chain, live_rows, accumulator, key_id, mac)
       VALUES (?, ?, ?, ?, ?, ?, ?)
       ON CONFLICT (journal) DO UPDATE SET revision = excluded.revision,
         chain = excluded.chain, live_rows = excluded.live_rows,
         accumulator = excluded.accumulator, key_id = excluded.key_id,
         mac = excluded.mac`,
    ).run(
      journal,
      revision,
      chain,
      liveRows,
      accumulator.toString(16).padStart(64, "0"),
      codec.keyId,
      codec.headMac(journal, next),
    );
    prepare(
      "INSERT INTO watcher_journal_revisions (journal, revision, chain, delta, mac) VALUES (?, ?, ?, ?, ?)",
    ).run(
      journal,
      revision,
      chain,
      deltaText,
      codec.revisionMac(journal, revision, chain, deltaText),
    );
    prepare(
      "DELETE FROM watcher_journal_revisions WHERE journal = ? AND revision <= ?",
    ).run(journal, revision - WATCHER_JOURNAL_RETAINED_REVISIONS);
  };

  const verify = (): void =>
    inTransaction("BEGIN", () => {
      for (const journal of WATCHER_JOURNALS)
        verifyWatcherJournal({
          journal,
          head: readHead(journal),
          rows: selectRows(journal),
          revisions: prepare(
            "SELECT revision, chain, delta, mac FROM watcher_journal_revisions WHERE journal = ? ORDER BY revision",
          ).all(journal) as StoredRevision[],
          codec,
          admit: (stored) => admitRow(journal, stored),
        });
    });

  const transaction = <T>(run: (tx: WatcherJournalTransaction) => T): T => {
    const staged = new Map<WatcherJournalName, Staged>();
    const stagedFor = (journal: WatcherJournalName): Staged => {
      let entries = staged.get(journal);
      if (entries === undefined) {
        entries = new Map();
        staged.set(journal, entries);
      }
      return entries;
    };
    const view = (
      journal: WatcherJournalName,
      key: string,
    ): WatcherJournalRow | undefined => {
      const pending = staged.get(journal)?.get(key);
      if (pending === null) return undefined;
      if (pending !== undefined)
        return Object.freeze({
          key,
          scope: pending.scope,
          state: pending.state,
          // Staged rows take their revision at commit.
          revision: 0,
          body: JSON.parse(pending.body) as unknown,
        });
      const stored = storedRow(journal, key);
      return stored === undefined ? undefined : admitRow(journal, stored);
    };
    const tx: WatcherJournalTransaction = Object.freeze({
      row: view,
      rows: (journal, filter = {}) => {
        const keys = new Set(
          selectRows(journal, filter).map(({ row_key }) => row_key),
        );
        for (const key of staged.get(journal)?.keys() ?? []) keys.add(key);
        return [...keys]
          .map((key) => view(journal, key))
          .filter(
            (row): row is WatcherJournalRow =>
              row !== undefined &&
              (filter.scope === undefined || row.scope === filter.scope) &&
              (filter.state === undefined || row.state === filter.state),
          );
      },
      count: (journal, state) => {
        let count = countRows(journal, state);
        for (const [key, pending] of staged.get(journal) ?? []) {
          const stored = storedRow(journal, key);
          if (
            stored !== undefined &&
            (state === undefined || stored.state === state)
          )
            count -= 1;
          if (
            pending !== null &&
            (state === undefined || pending.state === state)
          )
            count += 1;
        }
        return count;
      },
      put: (journal, row) => {
        if (!KEY.test(row.key) || row.scope.length > 512 || row.state === "")
          throw new Error(`watcher journal ${journal} row key is invalid`);
        stagedFor(journal).set(row.key, {
          scope: row.scope,
          state: row.state,
          body: watcherCanonicalJson(row.body),
        });
      },
      delete: (journal, key) => {
        stagedFor(journal).set(key, null);
      },
    });
    return inTransaction("BEGIN IMMEDIATE", () => {
      const result = run(tx);
      for (const journal of WATCHER_JOURNALS) {
        const entries = staged.get(journal);
        if (entries !== undefined && entries.size > 0)
          applyStaged(journal, entries);
      }
      return result;
    });
  };

  try {
    migrate();
    verify();
  } catch (error) {
    database.close();
    throw error;
  }
  return Object.freeze({
    path,
    transaction,
    row: (journal, key) => {
      const stored = storedRow(journal, key);
      return stored === undefined ? undefined : admitRow(journal, stored);
    },
    rows: (journal, filter) =>
      selectRows(journal, filter).map((stored) => admitRow(journal, stored)),
    count: countRows,
    head: (journal) => {
      const head = readHead(journal);
      return Object.freeze({
        revision: head.revision,
        liveRows: head.liveRows,
      });
    },
    verify,
    close: () => database.close(),
  });
};

const opened = new Map<
  string,
  Readonly<{ keyId: string; database: WatcherJournalDatabase }>
>();

/**
 * The process's one connection to the journals under `journalRoot`. The
 * first open migrates and verifies every journal in full; later opens share
 * that connection and must present the same key.
 */
export const openWatcherJournalDatabase = (input: {
  readonly journalRoot: string;
  readonly authenticationKey: Uint8Array;
}): WatcherJournalDatabase => {
  const keyId = createHash("sha256")
    .update(input.authenticationKey)
    .digest("hex");
  const existing = opened.get(input.journalRoot);
  if (existing !== undefined) {
    if (existing.keyId !== keyId)
      throw new Error("watcher journals are already open under another key");
    return existing.database;
  }
  const database = createDatabase(input.journalRoot, input.authenticationKey);
  const shared: WatcherJournalDatabase = Object.freeze({
    ...database,
    close: () => {
      opened.delete(input.journalRoot);
      database.close();
    },
  });
  opened.set(input.journalRoot, { keyId, database: shared });
  return shared;
};

/** Closes the process's connection, as a process exit would. */
export const closeWatcherJournalDatabase = (journalRoot: string): void => {
  opened.get(journalRoot)?.database.close();
};
