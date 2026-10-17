import { mkdirSync, realpathSync } from "node:fs";
import { isAbsolute, normalize } from "node:path";
import type { StatementSync } from "node:sqlite";

import {
  sameHex,
  type WatcherJournalCodec,
} from "./watcher-journal-database.codec.js";
import { WatcherJournalConfigurationError } from "./watcher-journal-database.types.js";

/**
 * Creates the journals' directory, refusing one that is not canonical, sits
 * under /tmp, cannot be created, or traverses a symlink: bad configuration,
 * which startup refuses before the operations server binds.
 */
export const prepareWatcherJournalDirectory = (journalRoot: string): string => {
  if (
    journalRoot.trim() !== journalRoot ||
    !isAbsolute(journalRoot) ||
    normalize(journalRoot) !== journalRoot ||
    journalRoot === "/" ||
    journalRoot === "/tmp" ||
    journalRoot.startsWith("/tmp/")
  )
    throw new WatcherJournalConfigurationError(
      "the journals require a canonical durable directory",
    );
  let resolved: string;
  try {
    mkdirSync(journalRoot, { recursive: true, mode: 0o700 });
    resolved = realpathSync(journalRoot);
  } catch (error) {
    throw new WatcherJournalConfigurationError(
      `the journal directory cannot be created: ${
        error instanceof Error ? error.message : String(error)
      }`,
    );
  }
  if (resolved !== journalRoot)
    throw new WatcherJournalConfigurationError(
      "the journal directory traverses a symlink",
    );
  return journalRoot;
};

type StoredKeyedHead = {
  journal: string;
  revision: number;
  chain: string;
  live_rows: number;
  accumulator: string;
  key_id: string;
  mac: string;
};

/**
 * A wrong key, told from corruption: every head names one other key and none
 * carries a MAC under this key. A head written under this key keeps a MAC
 * that verifies, whatever damage its key column took.
 */
export const watcherJournalsWrittenUnderAnotherKey = (
  prepare: (sql: string) => StatementSync,
  codec: WatcherJournalCodec,
): boolean => {
  const heads = prepare(
    "SELECT journal, revision, chain, live_rows, accumulator, key_id, mac FROM watcher_journal_heads",
  ).all() as StoredKeyedHead[];
  const verifies = (head: StoredKeyedHead): boolean => {
    try {
      return sameHex(
        head.mac,
        codec.headMac(head.journal, {
          revision: Number(head.revision),
          chain: head.chain,
          liveRows: Number(head.live_rows),
          accumulator: BigInt(`0x${head.accumulator}`),
        }),
      );
    } catch {
      return false;
    }
  };
  return (
    new Set(heads.map(({ key_id }) => key_id)).size === 1 &&
    heads[0]!.key_id !== codec.keyId &&
    !heads.some(verifies)
  );
};
