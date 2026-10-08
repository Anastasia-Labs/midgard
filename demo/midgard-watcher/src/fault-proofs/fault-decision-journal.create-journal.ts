import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import {
  type HeaderDecision,
  headerDecisionEnvelope,
} from "@al-ft/midgard-fault-proofs";

import {
  DIGEST,
  exactLaunchScope,
  exactString,
  MAX_RECORDS,
  type UnsafeWatcherFaultDecisionJournalForTest,
  WATCHER_FAULT_DECISION_JOURNAL_SCHEMA_VERSION,
  WATCHER_FAULT_DECISION_RECORD_SCHEMA_VERSION,
  type WatcherFaultDecisionJournal,
  type WatcherPersistedFaultDecisionRecord,
} from "./fault-decision-journal.exact-record.js";
import { parseDecision } from "./fault-decision-journal.parse-decision.js";
import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";
import {
  openWatcherJournalDatabase,
  WATCHER_JOURNAL_DATABASE_FILE,
  WatcherJournalCapacityError,
  type WatcherJournalDatabase,
  type WatcherJournalRow,
} from "./watcher-journal-database.js";
import {
  WATCHER_JOURNAL_TABLES,
  watcherObjectiveScope,
} from "./watcher-journal-schema.js";

const JOURNAL = "fault_decisions" as const;

type JournalInput = Readonly<{
  /** The workflow journal directory; the journals' database lives in it. */
  directory: string;
  deploymentFingerprint: string;
  launchScope: readonly WatcherInstalledWorkflowCategory[];
  /** The 32-byte key the journal rows are authenticated with. */
  authenticationKey: Uint8Array;
}>;

// A fault decision shares its objective's scope, so pruning the objective
// prunes it; the production writer appends no other kind.
const decisionScope = (decision: HeaderDecision): string =>
  decision.decision === "fault_detected"
    ? watcherObjectiveScope(decision.category, decision.headerHash)
    : `${decision.decision}:${decision.headerHash}`;

/** A row whose body is not a decision of this deployment, or differs from
 * its key, refuses the journals for this process. */
const parseRow = (
  database: WatcherJournalDatabase,
  row: WatcherJournalRow,
  deploymentFingerprint: string,
  launchScope: readonly WatcherInstalledWorkflowCategory[],
): WatcherPersistedFaultDecisionRecord => {
  let decision: HeaderDecision;
  try {
    decision = parseDecision(row.body, deploymentFingerprint, launchScope);
  } catch (error) {
    return database.refuse(
      JOURNAL,
      `row ${row.key} body is not a decision: ${
        error instanceof Error ? error.message : String(error)
      }`,
    );
  }
  if (
    row.key !== decision.decisionDigest ||
    row.scope !== decisionScope(decision) ||
    row.state !== decision.decision
  )
    database.refuse(JOURNAL, `row ${row.key} differs from its decision`);
  return Object.freeze({
    schemaVersion: WATCHER_FAULT_DECISION_RECORD_SCHEMA_VERSION,
    revision: row.revision.toString(),
    decision,
  });
};

const createJournal = (
  input: JournalInput,
  exposeUnsafeAppendForTest: boolean,
): WatcherFaultDecisionJournal | UnsafeWatcherFaultDecisionJournalForTest => {
  const deploymentFingerprint = exactString(
    input.deploymentFingerprint,
    DIGEST,
    "watcher fault decision deployment fingerprint",
  );
  const launchScope = exactLaunchScope(input.launchScope, input.launchScope);
  const database = openWatcherJournalDatabase({
    journalRoot: input.directory,
    authenticationKey: input.authenticationKey,
  });
  // The admitted rows, refreshed from the rows written since the cached
  // revision; a prune elsewhere (a changed row count) reloads them all.
  let cachedRevision = -1;
  const byDigest = new Map<string, WatcherPersistedFaultDecisionRecord>();
  const reload = (afterRevision?: number): void => {
    for (const row of database.rows(
      JOURNAL,
      afterRevision === undefined ? {} : { afterRevision },
    ))
      byDigest.set(
        row.key,
        parseRow(database, row, deploymentFingerprint, launchScope),
      );
  };
  const refresh = (): void => {
    const head = database.head(JOURNAL);
    if (head.revision === cachedRevision) return;
    if (cachedRevision >= 0 && head.revision > cachedRevision)
      reload(cachedRevision);
    if (byDigest.size !== head.liveRows || cachedRevision < 0) {
      byDigest.clear();
      reload();
    }
    cachedRevision = head.revision;
  };
  refresh();

  const appendEnvelope = async (
    value: unknown,
  ): Promise<WatcherPersistedFaultDecisionRecord> => {
    const decision = parseDecision(value, deploymentFingerprint, launchScope);
    refresh();
    const existing = byDigest.get(decision.decisionDigest);
    if (existing !== undefined) return existing;
    database.transaction((tx) => {
      if (tx.row(JOURNAL, decision.decisionDigest) !== undefined) return;
      if (tx.count(JOURNAL) >= MAX_RECORDS)
        throw new WatcherJournalCapacityError(JOURNAL, MAX_RECORDS);
      tx.put(JOURNAL, {
        key: decision.decisionDigest,
        scope: decisionScope(decision),
        state: decision.decision,
        body: decision,
      });
    });
    refresh();
    const appended = byDigest.get(decision.decisionDigest);
    if (appended === undefined)
      throw new Error("watcher fault decision journal failed append read-back");
    return appended;
  };

  const journal: WatcherFaultDecisionJournal = Object.freeze({
    schemaVersion: WATCHER_FAULT_DECISION_JOURNAL_SCHEMA_VERSION,
    readAll: async () => {
      refresh();
      return Object.freeze(
        [...byDigest.values()].sort(
          (left, right) =>
            Number(left.revision) - Number(right.revision) ||
            Number(
              left.decision.decisionDigest > right.decision.decisionDigest,
            ) -
              Number(
                left.decision.decisionDigest < right.decision.decisionDigest,
              ),
        ),
      );
    },
    read: async (decisionDigest) => {
      refresh();
      return byDigest.get(decisionDigest);
    },
    appendLiveDecision: async (decision: HeaderDecision) =>
      await appendEnvelope(headerDecisionEnvelope(decision)),
  });
  return exposeUnsafeAppendForTest
    ? Object.freeze({
        ...journal,
        unsafeAppendDecisionEnvelopeForTest: appendEnvelope,
      })
    : journal;
};

export const openWatcherFaultDecisionJournal = async (
  input: JournalInput,
): Promise<WatcherFaultDecisionJournal> =>
  createJournal(input, false) as WatcherFaultDecisionJournal;

/** Test-only structural seeding seam; production append still requires admission. */
export const unsafeOpenWatcherFaultDecisionJournalForTest = async (
  input: JournalInput,
): Promise<UnsafeWatcherFaultDecisionJournalForTest> =>
  createJournal(input, true) as UnsafeWatcherFaultDecisionJournalForTest;

/**
 * Evidence for tooling outside the watcher process, which holds no journal
 * key: the recorded decisions in journal order, each checked against its own
 * digest. It opens the database read-only and never authenticates rows, so
 * it is evidence, never authority.
 */
export const readWatcherFaultDecisionEvidence = (input: {
  readonly directory: string;
  readonly deploymentFingerprint: string;
  readonly launchScope: readonly WatcherInstalledWorkflowCategory[];
}): readonly HeaderDecision[] => {
  const database = new DatabaseSync(
    join(input.directory, WATCHER_JOURNAL_DATABASE_FILE),
    { readOnly: true },
  );
  try {
    const rows = database
      .prepare(
        `SELECT body FROM ${WATCHER_JOURNAL_TABLES.fault_decisions} ORDER BY revision, row_key`,
      )
      .all() as { body: string }[];
    return Object.freeze(
      rows.map(({ body }) =>
        parseDecision(
          JSON.parse(body) as unknown,
          input.deploymentFingerprint,
          input.launchScope,
        ),
      ),
    );
  } finally {
    database.close();
  }
};
