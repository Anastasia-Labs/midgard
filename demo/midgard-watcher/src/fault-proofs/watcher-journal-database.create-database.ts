import { join } from "node:path";
import { DatabaseSync, type StatementSync } from "node:sqlite";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  createWatcherJournalCodec,
  sameHex,
} from "./watcher-journal-database.codec.js";
import {
  prepareWatcherJournalDirectory,
  watcherJournalsWrittenUnderAnotherKey,
} from "./watcher-journal-database.configuration.js";
import { migrateWatcherJournals } from "./watcher-journal-database.migrate.js";
import { type WatcherJournalRefuse } from "./watcher-journal-database.refusal.js";
import {
  type Staged,
  type StoredRow,
  WATCHER_JOURNAL_DATABASE_FILE,
  WatcherJournalConfigurationError,
  type WatcherJournalDatabase,
  type WatcherJournalRow,
  type WatcherJournalRowFilter,
  type WatcherJournalTransaction,
} from "./watcher-journal-database.types.js";
import {
  WATCHER_JOURNAL_TABLES,
  WATCHER_JOURNALS,
  type WatcherJournalName,
} from "./watcher-journal-schema.js";

const KEY = /^[A-Za-z0-9:._/-]{1,512}$/u;

type RowFilter = WatcherJournalRowFilter & Readonly<{ afterRevision?: number }>;

/** Opens, migrates and fully verifies the journals' database file. Callers
 * reach it through `openWatcherJournalDatabase`, which shares one connection
 * per root and guards it with the root's refusal latch. */
export const createWatcherJournalDatabase = (
  journalRoot: string,
  authenticationKey: Uint8Array,
  refuse: WatcherJournalRefuse,
): Omit<WatcherJournalDatabase, "refuse"> => {
  const codec = createWatcherJournalCodec(authenticationKey);
  const directory = prepareWatcherJournalDirectory(journalRoot);
  const path = join(directory, WATCHER_JOURNAL_DATABASE_FILE);

  const database = new DatabaseSync(path);
  try {
    database.exec(`
      PRAGMA journal_mode = WAL;
      PRAGMA synchronous = FULL;
      PRAGMA busy_timeout = 5000;
    `);
  } catch (error) {
    database.close();
    throw error;
  }
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

  const table = (journal: WatcherJournalName): string => {
    const name = WATCHER_JOURNAL_TABLES[journal];
    if (name === undefined)
      throw new Error(`unknown watcher journal ${journal}`);
    return name;
  };

  // A journal's revision counter; the head's key is read only to tell a
  // wrong key from corruption (watcherJournalsWrittenUnderAnotherKey).
  const readRevision = (journal: WatcherJournalName): number => {
    const stored = prepare(
      "SELECT revision FROM watcher_journal_heads WHERE journal = ?",
    ).get(journal) as { revision: number } | undefined;
    return stored === undefined ? 0 : Number(stored.revision);
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
      refuse(journal, `row ${stored.row_key} MAC differs`);
    let body: unknown;
    try {
      body = JSON.parse(stored.body);
    } catch {
      refuse(journal, `row ${stored.row_key} body is malformed`);
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

  const countRows = (journal: WatcherJournalName, state?: string): number =>
    Number(
      (
        (state === undefined
          ? prepare(`SELECT count(*) AS n FROM ${table(journal)}`).get()
          : prepare(
              `SELECT count(*) AS n FROM ${table(journal)} WHERE state = ?`,
            ).get(state)) as { n: number }
      ).n,
    );

  /** Applies one journal's staged writes as one revision. */
  const applyStaged = (journal: WatcherJournalName, staged: Staged): void => {
    const revision = readRevision(journal) + 1;
    let changed = false;
    for (const key of [...staged.keys()].sort()) {
      const write = staged.get(key)!;
      if (write === null) {
        const { changes } = prepare(
          `DELETE FROM ${table(journal)} WHERE row_key = ?`,
        ).run(key);
        if (Number(changes) > 0) changed = true;
        continue;
      }
      prepare(
        `INSERT INTO ${table(journal)} (row_key, scope, state, revision, body, mac)
         VALUES (?, ?, ?, ?, ?, ?)
         ON CONFLICT (row_key) DO UPDATE SET scope = excluded.scope,
           state = excluded.state, revision = excluded.revision,
           body = excluded.body, mac = excluded.mac`,
      ).run(
        key,
        write.scope,
        write.state,
        revision,
        write.body,
        codec.rowMac(journal, { row_key: key, ...write, revision }),
      );
      changed = true;
    }
    if (!changed) return;
    prepare(
      `INSERT INTO watcher_journal_heads (journal, revision, key_id)
       VALUES (?, ?, ?)
       ON CONFLICT (journal) DO UPDATE SET revision = excluded.revision,
         key_id = excluded.key_id`,
    ).run(journal, revision, codec.keyId);
  };

  const verify = (): void =>
    inTransaction("BEGIN", () => {
      for (const journal of WATCHER_JOURNALS) {
        // A lower counter would reuse revisions and hide rows from readers
        // that reload after a revision.
        const revision = readRevision(journal);
        for (const stored of selectRows(journal))
          if (admitRow(journal, stored).revision > revision)
            refuse(journal, `row ${stored.row_key} is newer than the head`);
      }
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
          const prior =
            stored === undefined ? undefined : admitRow(journal, stored);
          if (
            prior !== undefined &&
            (state === undefined || prior.state === state)
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
    migrateWatcherJournals(database, prepare, inTransaction);
    if (watcherJournalsWrittenUnderAnotherKey(prepare, codec))
      throw new WatcherJournalConfigurationError(
        "the journals were written under another authentication key",
      );
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
    head: (journal) =>
      Object.freeze({
        revision: readRevision(journal),
        liveRows: countRows(journal),
      }),
    verify,
    close: () => database.close(),
  });
};
