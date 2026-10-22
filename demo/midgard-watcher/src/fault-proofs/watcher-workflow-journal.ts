import type { DatabaseSync } from "node:sqlite";

import {
  ConcurrentFraudProofWorkflowWriteError,
  createFraudProofWorkflowJournalFold,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalFold,
  type FraudProofWorkflowJournalStore,
} from "@al-ft/midgard-fault-proofs";

import type {
  WatcherProofExecution,
  WatcherProofObjective,
} from "./fault-proof-objective-journal.js";

const WORKFLOW_ID = /^[0-9a-f]{64}$/u;

/**
 * The per-proof workflow journal, kept in watcher.sqlite beside the funding
 * reservations so a funding change and the journal entries it implies
 * commit in one transaction. Rows are immutable once committed; an
 * objective's rows are deleted together once its funding is settled.
 */
export const WATCHER_WORKFLOW_JOURNAL_SCHEMA = `
  CREATE TABLE IF NOT EXISTS watcher_workflow_journal_v1 (
    workflow_id TEXT NOT NULL CHECK (length(workflow_id) = 64),
    sequence INTEGER NOT NULL CHECK (sequence >= 0),
    category TEXT NOT NULL,
    header_hash TEXT NOT NULL,
    event_kind TEXT NOT NULL,
    recorded_at INTEGER NOT NULL,
    body TEXT NOT NULL,
    PRIMARY KEY (workflow_id, sequence)
  ) WITHOUT ROWID, STRICT;
  CREATE INDEX IF NOT EXISTS watcher_workflow_journal_v1_objective
    ON watcher_workflow_journal_v1 (category, header_hash);
`;

type Folded = {
  readonly entries: FraudProofWorkflowJournalEntry[];
  readonly fold: FraudProofWorkflowJournalFold;
};

type EntryRow = Readonly<{ body: unknown }>;

const headerHashOf = (entry: FraudProofWorkflowJournalEntry): string => {
  const target = entry.identity.target;
  if (target.kind !== "state_queue_header")
    throw new Error("workflow journal entry has no queued header target");
  return target.headerHash;
};

/** Executions beyond this many in one objective are refused, never read. */
const OBJECTIVE_EXECUTION_BOUND = 2;

export type WatcherWorkflowJournal = Readonly<{
  /** A journal store scoped to one objective: it loads and appends only
   * that objective's executions. Each call returns a fresh store object, so
   * each workflow invocation binds its own authority to it. */
  forObjective: (
    objective: WatcherProofObjective,
  ) => FraudProofWorkflowJournalStore;
  /** Whether `store` is a journal store of this journal, whose entries a
   * funding change of the same connection commits with it. */
  owns: (store: object) => boolean;
  /** Validates `entries` as the next entries of their workflow and inserts
   * them. Call only inside an open transaction on this connection; `commit`
   * must run once that transaction has committed, so the cached fold never
   * runs ahead of the rows. */
  appendWithin: (entries: readonly FraudProofWorkflowJournalEntry[]) => {
    readonly commit: () => void;
  };
  /** The objective's one execution, or undefined when it has none. */
  readExecution: (
    objective: WatcherProofObjective,
  ) => WatcherProofExecution | undefined;
  /** The workflow ids of every execution with rows, at most `limit`. */
  readWorkflowIds: (limit: number) => readonly string[];
  /** Deletes the objective's rows. Idempotent. */
  forgetObjective: (objective: WatcherProofObjective) => void;
}>;

export const createWatcherWorkflowJournal = (
  database: DatabaseSync,
): WatcherWorkflowJournal => {
  database.exec(WATCHER_WORKFLOW_JOURNAL_SCHEMA);
  const selectFrom = database.prepare(`
    SELECT body FROM watcher_workflow_journal_v1
    WHERE workflow_id = ? AND sequence >= ?
    ORDER BY sequence ASC
  `);
  const selectCount = database.prepare(`
    SELECT count(*) AS count FROM watcher_workflow_journal_v1
    WHERE workflow_id = ?
  `);
  const insert = database.prepare(`
    INSERT INTO watcher_workflow_journal_v1(
      workflow_id, sequence, category, header_hash, event_kind, recorded_at,
      body
    ) VALUES (?, ?, ?, ?, ?, ?, ?)
  `);
  const selectObjectiveWorkflows = database.prepare(`
    SELECT DISTINCT workflow_id FROM watcher_workflow_journal_v1
    WHERE category = ? AND header_hash = ?
    ORDER BY workflow_id ASC
    LIMIT ${(OBJECTIVE_EXECUTION_BOUND + 1).toString()}
  `);
  const selectWorkflowIds = database.prepare(`
    SELECT DISTINCT workflow_id FROM watcher_workflow_journal_v1
    ORDER BY workflow_id ASC
    LIMIT ?
  `);
  const deleteObjective = database.prepare(`
    DELETE FROM watcher_workflow_journal_v1
    WHERE category = ? AND header_hash = ?
  `);

  // Rows never change once committed, so a cached fold extends with the
  // rows past its length. A failed push poisons the fold: the cache entry is
  // dropped before any push and restored only after the rows are durable.
  const cache = new Map<string, Folded>();

  const loadFolded = (workflowId: string): Folded => {
    if (!WORKFLOW_ID.test(workflowId))
      throw new Error("workflowId must be 32-byte lowercase hex");
    const cached = cache.get(workflowId);
    cache.delete(workflowId);
    const folded: Folded = cached ?? {
      entries: [],
      fold: createFraudProofWorkflowJournalFold({ workflowId }),
    };
    for (const row of selectFrom.all(
      workflowId,
      folded.entries.length,
    ) as EntryRow[]) {
      if (typeof row.body !== "string")
        throw new Error("workflow journal row is malformed");
      const entry = JSON.parse(row.body) as FraudProofWorkflowJournalEntry;
      folded.fold.push(entry);
      folded.entries.push(entry);
    }
    cache.set(workflowId, folded);
    return folded;
  };

  const countOf = (workflowId: string): number =>
    Number((selectCount.get(workflowId) as { count: unknown }).count);

  const appendWithin = (
    entries: readonly FraudProofWorkflowJournalEntry[],
  ): { readonly commit: () => void } => {
    const first = entries[0];
    if (first === undefined) return { commit: () => undefined };
    const workflowId = first.workflowId;
    const folded = loadFolded(workflowId);
    if (countOf(workflowId) !== folded.entries.length)
      throw new Error("workflow journal rows changed beneath their fold");
    cache.delete(workflowId);
    entries.forEach((entry, index) => {
      const expected = folded.entries.length;
      if (
        entry.workflowId !== workflowId ||
        entry.sequence !== expected ||
        first.sequence + index !== expected
      )
        throw new ConcurrentFraudProofWorkflowWriteError(
          `journal sequence changed: expected=${expected.toString()} actual=${entry.sequence.toString()}`,
        );
      folded.fold.push(entry);
      folded.entries.push(entry);
      insert.run(
        workflowId,
        entry.sequence,
        entry.identity.category,
        headerHashOf(entry),
        entry.event.kind,
        Date.parse(entry.recordedAt),
        JSON.stringify(entry),
      );
    });
    return { commit: () => cache.set(workflowId, folded) };
  };

  const append = (entry: FraudProofWorkflowJournalEntry) => {
    database.exec("BEGIN IMMEDIATE");
    let committed;
    try {
      committed = appendWithin([entry]);
      database.exec("COMMIT");
    } catch (error) {
      try {
        database.exec("ROLLBACK");
      } catch {
        // Preserve the first storage failure.
      }
      throw error;
    }
    committed.commit();
  };

  const assertObjective = (
    objective: WatcherProofObjective,
    entry: FraudProofWorkflowJournalEntry | undefined,
  ) => {
    if (
      entry !== undefined &&
      (entry.identity.category !== objective.category ||
        headerHashOf(entry) !== objective.headerHash)
    )
      throw new Error("workflow journal entry belongs to another objective");
  };

  const readExecution = (
    objective: WatcherProofObjective,
  ): WatcherProofExecution | undefined => {
    const ids = (
      selectObjectiveWorkflows.all(
        objective.category,
        objective.headerHash,
      ) as Readonly<{ workflow_id: unknown }>[]
    ).map(({ workflow_id }) => String(workflow_id));
    if (ids.length > 1)
      throw new Error("proof objective has multiple durable executions");
    const workflowId = ids[0];
    if (workflowId === undefined) return undefined;
    const { entries } = loadFolded(workflowId);
    assertObjective(objective, entries[0]);
    return { workflowId, entries: [...entries] };
  };

  const stores = new WeakSet<object>();
  const forObjective = (
    objective: WatcherProofObjective,
  ): FraudProofWorkflowJournalStore => {
    const store: FraudProofWorkflowJournalStore = Object.freeze({
      load: async (workflowId: string) => {
        const { entries } = loadFolded(workflowId);
        assertObjective(objective, entries[0]);
        return [...entries];
      },
      append: async (
        entry: FraudProofWorkflowJournalEntry,
        expectedSequence: number,
      ) => {
        assertObjective(objective, entry);
        if (entry.sequence !== expectedSequence)
          throw new ConcurrentFraudProofWorkflowWriteError(
            `entry sequence ${entry.sequence.toString()} does not equal expected ${expectedSequence.toString()}`,
          );
        append(entry);
      },
    });
    stores.add(store);
    return store;
  };

  return Object.freeze({
    forObjective,
    owns: (store) => stores.has(store),
    appendWithin,
    readExecution,
    readWorkflowIds: (limit) =>
      (
        selectWorkflowIds.all(limit) as Readonly<{ workflow_id: unknown }>[]
      ).map(({ workflow_id }) => String(workflow_id)),
    forgetObjective: (objective) => {
      deleteObjective.run(objective.category, objective.headerHash);
      cache.clear();
    },
  });
};
