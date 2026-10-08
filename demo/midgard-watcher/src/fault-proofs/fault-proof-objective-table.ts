import { rm } from "node:fs/promises";
import { join } from "node:path";

import { isFinal } from "@al-ft/midgard-l1-follower/heads";

import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import { MAX_RECORDS as MAX_DECISIONS } from "./fault-decision-journal.exact-record.js";
import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";
import type {
  WatcherProofExecution,
  WatcherProofObjective,
} from "./fault-proof-objective-journal.js";
import {
  WatcherJournalCapacityError,
  type WatcherJournalDatabase,
  type WatcherJournalTransaction,
} from "./watcher-journal-database.js";
import {
  type WatcherJournalName,
  watcherObjectiveScope,
} from "./watcher-journal-schema.js";

/**
 * The proof objective table (ticket W2, L2): one row per objective, written
 * when its first job is queued, before any workflow directory exists. Startup
 * lists these rows instead of scanning the workflow directories.
 *
 * - `open`: the objective holds unfinished work. Only open rows count toward
 *   the cap, and reaching it is a readiness condition, never an exit.
 * - `completed`: its completion was verified, but not yet beyond rollback
 *   recovery, so the next start verifies it again.
 * - `marked`: its completion was verified beyond rollback recovery; the row
 *   records which execution, so the next start skips it and prunes it.
 */
const JOURNAL = "fault_proof_objectives" satisfies WatcherJournalName;

export const MAX_OPEN_OBJECTIVES = 2_048;

export type WatcherProofObjectiveState = "open" | "completed" | "marked";

export type WatcherProofCompletionMarker = Readonly<{
  workflowId: string;
  journalDigest: string;
  confirmationDepth: number;
  recoveryDepth: string;
}>;

export type WatcherProofObjectiveRow = Readonly<{
  objective: WatcherProofObjective;
  state: WatcherProofObjectiveState;
  marker: WatcherProofCompletionMarker | null;
}>;

/** A completion deeper than the deployment's k (the security parameter the
 * watcher's follower rewinds and prunes with) cannot be undone by any
 * rollback the watcher recovers from automatically. */
export const isBeyondWatcherRollbackRecovery = (
  confirmationDepth: number,
  securityParameter: number,
): boolean =>
  Number.isSafeInteger(confirmationDepth) &&
  isFinal(confirmationDepth, { securityParameter });

const STATES = new Set<string>(["open", "completed", "marked"]);

/** Records the objective as open unless a row already exists. Opening one
 * more past the cap throws `WatcherJournalCapacityError`. */
export const openWatcherProofObjective = (
  tx: WatcherJournalTransaction,
  objective: WatcherProofObjective,
): void => {
  const scope = watcherObjectiveScope(objective.category, objective.headerHash);
  if (tx.row(JOURNAL, scope) !== undefined) return;
  if (tx.count(JOURNAL, "open") >= MAX_OPEN_OBJECTIVES)
    throw new WatcherJournalCapacityError(JOURNAL, MAX_OPEN_OBJECTIVES);
  tx.put(JOURNAL, {
    key: scope,
    scope,
    state: "open",
    body: { category: objective.category, headerHash: objective.headerHash },
  });
};

/** Whether the objective has a row or the table can open one more. */
export const canOpenWatcherProofObjective = (
  database: WatcherJournalDatabase,
  objective: Readonly<{ category: string; headerHash: string }>,
): boolean =>
  database.row(
    JOURNAL,
    watcherObjectiveScope(objective.category, objective.headerHash),
  ) !== undefined || database.count(JOURNAL, "open") < MAX_OPEN_OBJECTIVES;

/** The completion marker for this exact completed execution, or null when
 * the completion is not yet beyond rollback recovery. */
const completionMarker = (
  execution: WatcherProofExecution,
  confirmationDepth: number,
  securityParameter: number,
): WatcherProofCompletionMarker | null =>
  execution.entries.at(-1)?.event.kind === "completed" &&
  isBeyondWatcherRollbackRecovery(confirmationDepth, securityParameter)
    ? Object.freeze({
        workflowId: execution.workflowId,
        journalDigest: watcherSha256CanonicalJson(execution.entries),
        confirmationDepth,
        recoveryDepth: securityParameter.toString(),
      })
    : null;

/** Records a verified completion; one verified beyond rollback recovery (k
 * deep, the follower's `securityParameter`) is marked with its execution so
 * the next start skips and prunes it. Without a k nothing is marked. True
 * once the row holds a marker. */
export const completeWatcherProofObjective = (
  database: WatcherJournalDatabase,
  objective: WatcherProofObjective,
  verified?: Readonly<{
    execution: WatcherProofExecution;
    confirmationDepth: number;
  }>,
  securityParameter?: number,
): boolean => {
  const marker =
    verified === undefined || securityParameter === undefined
      ? null
      : completionMarker(
          verified.execution,
          verified.confirmationDepth,
          securityParameter,
        );
  const scope = watcherObjectiveScope(objective.category, objective.headerHash);
  return database.transaction((tx) => {
    const current = tx.row(JOURNAL, scope);
    if (current?.state === "marked") return true;
    if (current?.state === "completed" && marker === null) return false;
    tx.put(JOURNAL, {
      key: scope,
      scope,
      state: marker === null ? "completed" : "marked",
      body: {
        category: objective.category,
        headerHash: objective.headerHash,
        ...(marker === null ? {} : { marker }),
      },
    });
    return marker !== null;
  });
};

/** True only for a marker bound to this exact completed execution whose
 * recorded confirmation depth is beyond rollback recovery under the current
 * k: the depth alone decides, so a marker written under a smaller k that is
 * not that deep, or one checked without a k, is verified again. The k a
 * marker was written under (`recoveryDepth`) is a record only. */
export const watcherProofMarkerMatches = (
  marker: WatcherProofCompletionMarker,
  execution: WatcherProofExecution | undefined,
  securityParameter: number | undefined,
): boolean => {
  if (execution === undefined || securityParameter === undefined) return false;
  const expected = completionMarker(
    execution,
    marker.confirmationDepth,
    securityParameter,
  );
  return (
    expected !== null &&
    expected.workflowId === marker.workflowId &&
    expected.journalDigest === marker.journalDigest
  );
};

const parseRow = (
  body: unknown,
  state: string,
  categories: ReadonlySet<string>,
  refuse: (detail: string) => never,
): WatcherProofObjectiveRow => {
  const value = body as {
    category?: unknown;
    headerHash?: unknown;
    marker?: Partial<Record<keyof WatcherProofCompletionMarker, unknown>>;
  };
  const marker = value.marker;
  if (
    typeof value.category !== "string" ||
    !categories.has(value.category) ||
    typeof value.headerHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(value.headerHash) ||
    !STATES.has(state) ||
    (state === "marked") !== (marker !== undefined) ||
    (marker !== undefined &&
      (typeof marker.workflowId !== "string" ||
        typeof marker.journalDigest !== "string" ||
        typeof marker.confirmationDepth !== "number" ||
        typeof marker.recoveryDepth !== "string"))
  )
    refuse("proof objective row is malformed");
  return Object.freeze({
    objective: Object.freeze({
      category: value.category as WatcherInstalledWorkflowCategory,
      headerHash: value.headerHash,
    }),
    state: state as WatcherProofObjectiveState,
    marker:
      marker === undefined
        ? null
        : (Object.freeze({ ...marker }) as WatcherProofCompletionMarker),
  });
};

/** Every recorded objective, in commit order. A row that does not parse or
 * differs from its key refuses the journals for this process. */
export const listWatcherProofObjectives = (
  database: WatcherJournalDatabase,
  categories: readonly WatcherInstalledWorkflowCategory[],
): readonly WatcherProofObjectiveRow[] => {
  const allowed = new Set<string>(categories);
  return Object.freeze(
    database.rows(JOURNAL).map((row) => {
      const refuse = (detail: string): never =>
        database.refuse(JOURNAL, detail);
      const parsed = parseRow(row.body, row.state, allowed, refuse);
      if (
        row.key !==
        watcherObjectiveScope(
          parsed.objective.category,
          parsed.objective.headerHash,
        )
      )
        refuse("proof objective row differs from its key");
      return parsed;
    }),
  );
};

/** The objective's completion marker when its row holds one that matches
 * this exact execution under the current k (`watcherProofMarkerMatches`),
 * else null. A row that does not parse refuses the journals. */
export const matchingWatcherProofMarker = (
  database: WatcherJournalDatabase,
  objective: WatcherProofObjective,
  execution: WatcherProofExecution,
  securityParameter: number | undefined,
  categories: readonly WatcherInstalledWorkflowCategory[],
): WatcherProofCompletionMarker | null => {
  const scope = watcherObjectiveScope(objective.category, objective.headerHash);
  const row = database.row(JOURNAL, scope);
  if (row?.state !== "marked") return null;
  const { marker } = parseRow(
    row.body,
    row.state,
    new Set<string>(categories),
    (detail) => database.refuse(JOURNAL, detail),
  );
  return marker !== null &&
    watcherProofMarkerMatches(marker, execution, securityParameter)
    ? marker
    : null;
};

/**
 * Deletes the objective's row and its queued jobs in one commit; a prune
 * also deletes its fault decisions. The caller removes the workflow
 * directory first, so a crash in between leaves a row whose execution is
 * gone, which the next start prunes again.
 */
export const forgetWatcherProofObjective = (
  database: WatcherJournalDatabase,
  objective: WatcherProofObjective,
  options: Readonly<{ decisions: boolean }>,
): void => {
  const scope = watcherObjectiveScope(objective.category, objective.headerHash);
  const journals: WatcherJournalName[] = [
    JOURNAL,
    "fault_proof_queue",
    ...(options.decisions ? (["fault_decisions"] as const) : []),
  ];
  database.transaction((tx) => {
    for (const journal of journals)
      for (const row of tx.rows(journal, { scope }))
        tx.delete(journal, row.key);
  });
};

/** Whether a job of this objective is active: running in this process, or
 * interrupted by a crash, whose next run settles it. Startup pruning leaves
 * such an objective's rows alone, so the job's finish always finds its row. */
export const watcherProofJobActive = (
  database: WatcherJournalDatabase,
  objective: WatcherProofObjective,
): boolean =>
  database.rows("fault_proof_queue", {
    scope: watcherObjectiveScope(objective.category, objective.headerHash),
    state: "active",
  }).length > 0;

/** Whether a job of this objective is queued or active. Its next queue
 * transition needs its row, so a release leaves the objective's rows to it. */
export const watcherProofJobPending = (
  database: WatcherJournalDatabase,
  objective: WatcherProofObjective,
): boolean =>
  (["queued", "active"] as const).some(
    (state) =>
      database.rows("fault_proof_queue", {
        scope: watcherObjectiveScope(objective.category, objective.headerHash),
        state,
      }).length > 0,
  );

/** Prunes a completion verified beyond rollback recovery: its workflow
 * journal, then its rows, decisions included. A crash in between leaves a
 * marked row with no execution, which the next start prunes. */
export const pruneWatcherProofObjective = async (
  database: WatcherJournalDatabase,
  journalRoot: string,
  objective: WatcherProofObjective,
): Promise<void> => {
  await rm(
    join(journalRoot, "fault-proofs", objective.category, objective.headerHash),
    { recursive: true, force: true },
  );
  forgetWatcherProofObjective(database, objective, { decisions: true });
};

/** Whether a journal holds its cap of live rows: unready, never an exit. */
export const watcherJournalCapacityReached = (
  database: WatcherJournalDatabase,
): boolean =>
  database.count(JOURNAL, "open") >= MAX_OPEN_OBJECTIVES ||
  database.count("fault_decisions") >= MAX_DECISIONS;
