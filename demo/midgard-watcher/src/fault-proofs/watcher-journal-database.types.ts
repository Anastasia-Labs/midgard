import type { WatcherJournalName } from "./watcher-journal-schema.js";

/** The journals' database file, inside the workflow journal directory. */
export const WATCHER_JOURNAL_DATABASE_FILE = "watcher-journals.sqlite";

export class WatcherJournalIntegrityError extends Error {
  constructor(journal: string, detail: string) {
    super(`watcher journal ${journal} failed integrity: ${detail}`);
    this.name = "WatcherJournalIntegrityError";
  }
}

/** A journal at its live-row cap refuses new live rows; never fatal. */
export class WatcherJournalCapacityError extends Error {
  readonly journal: WatcherJournalName;
  constructor(journal: WatcherJournalName, cap: number) {
    super(`watcher journal ${journal} holds its cap of ${cap} live rows`);
    this.name = "WatcherJournalCapacityError";
    this.journal = journal;
  }
}

export const isWatcherJournalCapacityError = (
  error: unknown,
): error is WatcherJournalCapacityError =>
  error instanceof WatcherJournalCapacityError;

export type WatcherJournalRow = Readonly<{
  key: string;
  scope: string;
  state: string;
  revision: number;
  body: unknown;
}>;

export type WatcherJournalHead = Readonly<{
  revision: number;
  liveRows: number;
}>;

export type WatcherJournalRowFilter = Readonly<{
  scope?: string;
  state?: string;
}>;

/** Reads see the transaction's own staged writes. */
export type WatcherJournalTransaction = Readonly<{
  row(journal: WatcherJournalName, key: string): WatcherJournalRow | undefined;
  rows(
    journal: WatcherJournalName,
    filter?: WatcherJournalRowFilter,
  ): readonly WatcherJournalRow[];
  count(journal: WatcherJournalName, state?: string): number;
  put(
    journal: WatcherJournalName,
    row: Readonly<{ key: string; scope: string; state: string; body: unknown }>,
  ): void;
  delete(journal: WatcherJournalName, key: string): void;
}>;

export type WatcherJournalDatabase = Readonly<{
  path: string;
  /** One write transaction; each touched journal gains one revision. */
  transaction<T>(run: (tx: WatcherJournalTransaction) => T): T;
  row(journal: WatcherJournalName, key: string): WatcherJournalRow | undefined;
  rows(
    journal: WatcherJournalName,
    filter?: WatcherJournalRowFilter & Readonly<{ afterRevision?: number }>,
  ): readonly WatcherJournalRow[];
  count(journal: WatcherJournalName, state?: string): number;
  head(journal: WatcherJournalName): WatcherJournalHead;
  /** Full integrity check of every journal; startup runs it once. */
  verify(): void;
  close(): void;
}>;

export type StoredRow = {
  row_key: string;
  scope: string;
  state: string;
  revision: number | bigint;
  body: string;
  mac: string;
};

export type StoredHead = {
  revision: number;
  chain: string;
  liveRows: number;
  accumulator: bigint;
};

export type StoredRevision = {
  revision: number | bigint;
  chain: string;
  delta: string;
  mac: string;
};

/** One journal's staged writes: a row to upsert, or null to delete. */
export type Staged = Map<
  string,
  Readonly<{ scope: string; state: string; body: string }> | null
>;
