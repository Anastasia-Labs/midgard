import { isFinal } from "@al-ft/midgard-l1-follower/heads";

import { MAX_RECORDS as MAX_DECISIONS } from "./fault-decision-journal.exact-record.js";
import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";
import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
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
 * The proof objective table: one row per objective, written
 * when its first job is queued. Startup lists these rows; only a marked one
 * says anything about the objective's progress, and every other one is
 * forgotten, since a live fault admits its objective again and its run
 * reads what it did from the chain.
 *
 * - `open`: the objective holds unfinished work. Only open rows count toward
 *   the cap, and reaching it is a readiness condition, never an exit.
 * - `completed`: its completion was verified, but not yet beyond rollback
 *   recovery.
 * - `marked`: its completion was verified beyond rollback recovery; the row
 *   records the removal transaction; once its job is done the row is
 *   pruned, by the next release pass or the next start.
 */
const JOURNAL = "fault_proof_objectives" satisfies WatcherJournalName;

export const MAX_OPEN_OBJECTIVES = 2_048;

export type WatcherProofObjectiveState = "open" | "completed" | "marked";

/** A completion verified beyond rollback recovery. It binds the removal
 * transaction the chain shows; a row written before that binding names a
 * digest of the run's stored record instead (`journalDigest`) and counts as
 * a marker all the same. */
export type WatcherProofCompletionMarker = Readonly<
  {
    workflowId: string;
    confirmationDepth: number;
    recoveryDepth: string;
  } & ({ terminalTxHash: string } | { journalDigest: string })
>;

/** The completion a marker records: the run's workflow, the removal
 * transaction the chain shows and that transaction's confirmation depth. */
export type WatcherProofVerifiedCompletion = Readonly<{
  workflowId: string;
  terminalTxHash: string;
  confirmationDepth: number;
}>;

export type WatcherProofObjectiveRow = Readonly<{
  objective: WatcherProofObjective;
  state: WatcherProofObjectiveState;
  marker: WatcherProofCompletionMarker | null;
}>;

/** A completion deeper than the deployment's k (the security parameter the
 * watcher's follower rewinds with, automaticRecoveryMaxDepth) cannot be
 * undone by any rollback the watcher recovers from automatically. */
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

/** Whether the objective has a row: no row means nothing drives it. */
export const watcherProofObjectiveRecorded = (
  database: WatcherJournalDatabase,
  objective: Readonly<{ category: string; headerHash: string }>,
): boolean =>
  database.row(
    JOURNAL,
    watcherObjectiveScope(objective.category, objective.headerHash),
  ) !== undefined;

/** The completion marker for this verified completion, or null when it is
 * not yet beyond rollback recovery. */
const completionMarker = (
  verified: WatcherProofVerifiedCompletion,
  securityParameter: number,
): WatcherProofCompletionMarker | null =>
  isBeyondWatcherRollbackRecovery(verified.confirmationDepth, securityParameter)
    ? Object.freeze({
        workflowId: verified.workflowId,
        terminalTxHash: verified.terminalTxHash,
        confirmationDepth: verified.confirmationDepth,
        recoveryDepth: securityParameter.toString(),
      })
    : null;

/** Records a verified completion; one verified beyond rollback recovery
 * (deeper than k, the follower's `securityParameter`) is marked with its
 * removal transaction, so it is never run or verified again and is pruned. The
 * supervisor's completion check applies the same threshold, so every
 * completion it finishes is marked unless the write fails. Without a k
 * nothing is marked. True once the row holds a marker. */
export const completeWatcherProofObjective = (
  database: WatcherJournalDatabase,
  objective: WatcherProofObjective,
  verified?: WatcherProofVerifiedCompletion,
  securityParameter?: number,
): boolean => {
  const marker =
    verified === undefined || securityParameter === undefined
      ? null
      : completionMarker(verified, securityParameter);
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

/** True only for a marker whose recorded confirmation depth is beyond
 * rollback recovery under the current k: the depth alone decides, so a
 * marker written under a smaller k that is not that deep, or one checked
 * without a k, is verified again. The k a marker was written under
 * (`recoveryDepth`) is a record only. */
export const watcherProofMarkerMatches = (
  marker: WatcherProofCompletionMarker,
  securityParameter: number | undefined,
): boolean =>
  securityParameter !== undefined &&
  isBeyondWatcherRollbackRecovery(marker.confirmationDepth, securityParameter);

const parseRow = (
  body: unknown,
  state: string,
  categories: ReadonlySet<string>,
  refuse: (detail: string) => never,
): WatcherProofObjectiveRow => {
  const value = body as {
    category?: unknown;
    headerHash?: unknown;
    marker?: Partial<
      Record<
        | "workflowId"
        | "terminalTxHash"
        | "journalDigest"
        | "confirmationDepth"
        | "recoveryDepth",
        unknown
      >
    >;
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
        (typeof marker.terminalTxHash !== "string") ===
          (typeof marker.journalDigest !== "string") ||
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

/** The objective's completion marker when its row holds one beyond rollback
 * recovery under the current k (`watcherProofMarkerMatches`), else null. A
 * row that does not parse refuses the journals. */
export const matchingWatcherProofMarker = (
  database: WatcherJournalDatabase,
  objective: WatcherProofObjective,
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
  return marker !== null && watcherProofMarkerMatches(marker, securityParameter)
    ? marker
    : null;
};

/**
 * Deletes the objective's row and its queued jobs in one commit; a prune
 * also deletes its fault decisions, a supersede the one `decision` row.
 */
export const forgetWatcherProofObjective = (
  database: WatcherJournalDatabase,
  objective: WatcherProofObjective,
  options: Readonly<{ decisions: boolean; decision?: string }>,
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
    const decision =
      options.decision === undefined
        ? undefined
        : tx.row("fault_decisions", options.decision);
    if (decision?.scope === scope) tx.delete("fault_decisions", decision.key);
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

/** Whether a journal holds its cap of live rows: unready, never an exit. */
export const watcherJournalCapacityReached = (
  database: WatcherJournalDatabase,
): boolean =>
  database.count(JOURNAL, "open") >= MAX_OPEN_OBJECTIVES ||
  database.count("fault_decisions") >= MAX_DECISIONS;
