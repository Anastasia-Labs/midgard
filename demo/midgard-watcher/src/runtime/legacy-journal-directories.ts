import { readdir } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { WATCHER_JOURNAL_DATABASE_FILE } from "../fault-proofs/watcher-journal-database.types.js";
import { WATCHER_PACKAGE_NAME } from "./scaffold.js";

/**
 * The file journals SQLite tables replaced, each with the file whose fresh
 * journal took its place. No importer exists: a deployment starts its
 * journals fresh, and a leftover directory never blocks readiness.
 */
const LEGACY_JOURNAL_DIRECTORIES = Object.freeze([
  ["fault-decisions", WATCHER_JOURNAL_DATABASE_FILE],
  ["fault-proof-queue-v1", WATCHER_JOURNAL_DATABASE_FILE],
  ["fault-proof-completions-v1", WATCHER_JOURNAL_DATABASE_FILE],
] as const);

/** The per-proof workflow journals of the earlier layout. */
const LEGACY_WORKFLOW_DIRECTORY = "fault-proofs";

/** The earlier layout's funding tables that tied signed attempts to their
 * workflows; the store no longer reads them. */
const LEGACY_FUNDING_TABLES = Object.freeze([
  "watcher_prover_funding_handoff_v1",
  "watcher_prover_funding_abandonment_v1",
] as const);

export type WatcherLegacyJournalIgnored = Readonly<{
  event: "legacy_journal_ignored";
  path: string;
  detail: string;
}>;

export type WatcherLegacyProofsNotCarriedOver = Readonly<{
  event: "legacy_proofs_not_carried_over";
  present: readonly string[];
  detail: string;
}>;

const writeLine = (
  level: "warn" | "error",
  line: WatcherLegacyJournalIgnored | WatcherLegacyProofsNotCarriedOver,
): void => {
  process.stderr.write(
    `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level, ...line })}\n`,
  );
};

/** Advisory only: a missing or unreadable entry holds nothing to import. */
const nonEmpty = async (path: string) =>
  (await readdir(path).catch(() => [])).length > 0;

/** Warns once per non-empty legacy journal directory under `journalRoot`. */
export const warnWatcherLegacyJournalDirectories = async (
  journalRoot: string,
  warn: (warning: WatcherLegacyJournalIgnored) => void = (warning) =>
    writeLine("warn", warning),
): Promise<void> => {
  for (const [name, replacement] of LEGACY_JOURNAL_DIRECTORIES) {
    const path = join(journalRoot, name);
    if (await nonEmpty(path))
      warn(
        Object.freeze({
          event: "legacy_journal_ignored",
          path,
          detail: `legacy file journal is ignored and never imported; ${replacement} starts this journal fresh`,
        }),
      );
  }
};

/** The legacy funding tables of the database at `path` that hold rows;
 * none when it is missing or unreadable. */
const legacyFundingRows = (path: string): readonly string[] => {
  let database: DatabaseSync | undefined;
  try {
    database = new DatabaseSync(path, { readOnly: true });
    const opened = database;
    return LEGACY_FUNDING_TABLES.filter(
      (table) =>
        opened
          .prepare(
            "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?",
          )
          .get(table) !== undefined &&
        opened.prepare(`SELECT 1 FROM ${table} LIMIT 1`).get() !== undefined,
    );
  } catch {
    return [];
  } finally {
    database?.close();
  }
};

/**
 * Raises one error-level alert when the data directory still holds in-flight
 * proof state of the earlier layout: a non-empty `fault-proofs/` under
 * `journalRoot`, or rows in the legacy funding tables of `storagePath`.
 * Nothing imports that state and the watcher still starts.
 */
export const alertWatcherLegacyProofLayout = async (
  input: Readonly<{ journalRoot: string; storagePath: string }>,
  alert: (alert: WatcherLegacyProofsNotCarriedOver) => void = (line) =>
    writeLine("error", line),
): Promise<void> => {
  const workflows = join(input.journalRoot, LEGACY_WORKFLOW_DIRECTORY);
  const present = [
    ...((await nonEmpty(workflows)) ? [workflows] : []),
    ...legacyFundingRows(input.storagePath).map(
      (table) => `${input.storagePath} table ${table}`,
    ),
  ];
  if (present.length === 0) return;
  alert(
    Object.freeze({
      event: "legacy_proofs_not_carried_over",
      present: Object.freeze(present),
      detail:
        "the data directory holds in-flight proof state of an earlier layout; in-flight proofs from the old layout are not carried over: nothing imports or resumes them",
    }),
  );
};
