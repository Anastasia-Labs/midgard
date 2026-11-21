import { DatabaseSync } from "node:sqlite";

import {
  createWatcherWorkflowJournal,
  type WatcherWorkflowJournal,
} from "../../src/fault-proofs/watcher-workflow-journal.js";

/** The workflow journal of the watcher store at `path`, for tests in other
 * packages that write workflow rows a reader under test reads back. */
export const openWatcherWorkflowStoreForTest = (
  path: string,
): WatcherWorkflowJournal & { close(): void } => {
  const database = new DatabaseSync(path);
  database.exec("PRAGMA journal_mode = WAL");
  return {
    ...createWatcherWorkflowJournal(database),
    close: () => database.close(),
  };
};
