import { existsSync } from "node:fs";
import { mkdtemp, rm } from "node:fs/promises";

import type { WatcherProofObjective } from "../../src/fault-proofs/fault-proof-objective-journal.js";
import { openWatcherProofObjective } from "../../src/fault-proofs/fault-proof-objective-table.js";
import {
  closeWatcherJournalDatabase,
  openWatcherJournalDatabase,
} from "../../src/fault-proofs/watcher-journal-database.js";
import { watcherObjectiveScope } from "../../src/fault-proofs/watcher-journal-schema.js";

/** The key the test supervisor authenticates its journals with by default. */
export const TEST_JOURNAL_KEY = Uint8Array.from({ length: 32 }, () => 0xa5);

const roots: string[] = [];

/**
 * A fresh journal directory on a local disk; `/tmp` is refused as durable.
 * `memory` puts it on `/dev/shm` where that exists, for tests that commit
 * hundreds of thousands of times and measure growth, not durability.
 */
export const journalDirectory = async (
  prefix: string,
  options: Readonly<{ memory?: boolean }> = {},
): Promise<string> => {
  const base =
    options.memory === true && existsSync("/dev/shm") ? "/dev/shm" : "/var/tmp";
  const root = await mkdtemp(`${base}/${prefix}-`);
  roots.push(root);
  return root;
};

/** Closes every journal connection, as a process exit would, and removes
 * the directories. Call it from `afterEach`. */
export const removeJournalDirectories = async (): Promise<void> => {
  await Promise.all(
    roots.splice(0).map(async (root) => {
      closeWatcherJournalDatabase(root);
      await rm(root, { recursive: true, force: true });
    }),
  );
};

/**
 * Records objectives as open, as the queue does before any workflow
 * directory exists, for tests that seed workflow journals directly. The
 * connection stays open: journals the test opens under the same key share it.
 */
export const recordObjectives = (
  journalRoot: string,
  objectives: readonly WatcherProofObjective[],
  authenticationKey: Uint8Array = TEST_JOURNAL_KEY,
): void => {
  openWatcherJournalDatabase({ journalRoot, authenticationKey }).transaction(
    (tx) => {
      for (const objective of objectives)
        openWatcherProofObjective(tx, objective);
    },
  );
};

/** Writes an authenticated objective row with an arbitrary body, keyed by
 * its body's scope unless `key` says otherwise, as a bug in an earlier
 * writer could have; recovery must refuse it. */
export const recordRawObjectiveRow = (
  journalRoot: string,
  body: Readonly<{ category: string; headerHash: string }>,
  key?: string,
): void => {
  const scope = watcherObjectiveScope(body.category, body.headerHash);
  openWatcherJournalDatabase({
    journalRoot,
    authenticationKey: TEST_JOURNAL_KEY,
  }).transaction((tx) =>
    tx.put("fault_proof_objectives", {
      key: key ?? scope,
      scope,
      state: "open",
      body,
    }),
  );
  closeWatcherJournalDatabase(journalRoot);
};
