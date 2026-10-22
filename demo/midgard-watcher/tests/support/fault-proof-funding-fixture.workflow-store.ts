import { DatabaseSync } from "node:sqlite";

import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalStore,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherProofObjective } from "../../src/fault-proofs/fault-proof-objective-journal.js";
import type { WatcherWorkflowJournal } from "../../src/fault-proofs/watcher-workflow-journal.js";
import { openWatcherSqliteProverFundingReservationStore } from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import { deploymentIdentity } from "./fault-proof-funding-fixture.sources-for.js";

/** Opens the fixture's watcher.sqlite as the watcher does. */
export const openFixtureFundingStore = (path: string) =>
  openWatcherSqliteProverFundingReservationStore({
    path,
    protocolParameterHistory: {
      deploymentIdentity,
      authenticationKey: Buffer.alloc(32, 0x91),
    },
  });

/** Runs `edit` on a raw connection to the (closed) store file. */
export const editFixtureStore = (
  path: string,
  edit: (raw: DatabaseSync) => void,
): void => {
  const raw = new DatabaseSync(path);
  try {
    edit(raw);
  } finally {
    raw.close();
  }
};

/** Overwrites committed workflow rows in place. */
export const rewriteWorkflowRows = (
  raw: DatabaseSync,
  entries: readonly FraudProofWorkflowJournalEntry[],
): void => {
  const update = raw.prepare(
    "UPDATE watcher_workflow_journal_v1 SET body = ? WHERE workflow_id = ? AND sequence = ?",
  );
  for (const entry of entries)
    update.run(JSON.stringify(entry), entry.workflowId, entry.sequence);
};

/** A fresh objective store per call, as each watcher invocation gets; it
 * reads whichever store `journal()` has open now, across restarts. */
export const objectiveWorkflowJournal =
  (journal: () => WatcherWorkflowJournal, objective: WatcherProofObjective) =>
  (): FraudProofWorkflowJournalStore => ({
    load: async (workflowId) =>
      await journal().forObjective(objective).load(workflowId),
    append: async (entry, expectedSequence) =>
      await journal().forObjective(objective).append(entry, expectedSequence),
  });

/** Moves every workflow row aside, out of the watcher's sight; the returned
 * function moves them back. */
export const stageWorkflowRows = (path: string): (() => void) => {
  editFixtureStore(path, (raw) =>
    raw.exec(`
      CREATE TABLE staged_proof_fixture AS
        SELECT * FROM watcher_workflow_journal_v1;
      DELETE FROM watcher_workflow_journal_v1;
    `),
  );
  return () =>
    editFixtureStore(path, (raw) =>
      raw.exec(`
        INSERT INTO watcher_workflow_journal_v1
          SELECT * FROM staged_proof_fixture;
        DROP TABLE staged_proof_fixture;
      `),
    );
};

/** A journal that forwards every call to whichever store `journal()` has
 * open now, so a factory created before a restart reads the reopened one. */
export const currentWorkflowJournal = (
  journal: () => WatcherWorkflowJournal,
): WatcherWorkflowJournal =>
  Object.freeze({
    forObjective: (objective) => journal().forObjective(objective),
    appendWithin: (entries) => journal().appendWithin(entries),
    readExecution: (objective) => journal().readExecution(objective),
    readWorkflowIds: (limit) => journal().readWorkflowIds(limit),
    forgetObjective: (objective) => journal().forgetObjective(objective),
  });
