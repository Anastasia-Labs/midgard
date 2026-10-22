import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import {
  isWatcherProofJournalRefusal,
  readWatcherProofExecution,
  type WatcherProofObjective,
} from "./fault-proof-objective-journal.js";
import {
  forgetWatcherProofObjective,
  watcherProofJobPending,
} from "./fault-proof-objective-table.js";
import type { WatcherJournalDatabase } from "./watcher-journal-database.js";
import type { WatcherWorkflowJournal } from "./watcher-workflow-journal.js";

/** An objective whose workflow rows a release or a final completion could
 * not delete (its rows stay); the next retry pass retries it. */
export type WatcherProofCleanupFailure = Readonly<{
  category: WatcherProofObjective["category"];
  headerHash: string;
  detail: string;
}>;

export type WatcherProofObjectiveCleanup = Readonly<{
  /** Marks an objective whose queue rows a previous process left queued or
   * active: no job of this process owns them, so they never defer it. */
  markStale(objective: WatcherProofObjective): void;
  /**
   * Forgets an objective no longer driven, with its workflow rows once no
   * job of it is queued or active: a final completion's always, decisions
   * included; a released one's unless it holds a signed attempt, whose rows
   * the funding settle deletes once the attempt is retired, or cannot be
   * read for a refusal. A departed objective's header has left the
   * finalized queue; any other waits for an observation that drops it.
   */
  forget(
    objective: WatcherProofObjective,
    how: Readonly<{ final: boolean; departed: boolean }>,
  ): Promise<void>;
  /** Retries every deferred or failed objective that is not open again. */
  retry(
    observation: WatcherAuthenticatedStateQueueObservation,
    open: (objective: WatcherProofObjective) => boolean,
  ): Promise<void>;
  failures(): readonly WatcherProofCleanupFailure[];
}>;

/**
 * Removal deletes the objective's workflow rows in watcher.sqlite, then
 * forgets its rows in watcher-journals.sqlite. A crash before the delete
 * leaves both whole, so the next start settles them again; one after it
 * leaves rows whose execution is gone, which the next start forgets. An
 * objective whose execution a refusal keeps from being read (a corrupt row,
 * a foreign execution) is forgotten with its workflow rows left in place:
 * what they hold is never trusted or deleted. A released one with a signed
 * attempt is forgotten with its rows kept too; the funding settle deletes
 * them with the attempt's funding. A removal that fails on a storage error
 * is named and retried; it never fails the caller.
 */
export const createWatcherProofObjectiveCleanup = (input: {
  readonly workflowJournal: WatcherWorkflowJournal;
  readonly deploymentFingerprint: string;
  readonly database: () => WatcherJournalDatabase;
  /** Whether a job of the objective is queued, running or handed over in
   * this process. */
  readonly jobInProcess?: (objective: WatcherProofObjective) => boolean;
  /** Told after the objective's workflow rows are deleted. */
  readonly onRemoved?: (objective: WatcherProofObjective) => void;
  /** Told after the objective is forgotten with its workflow rows kept: its
   * signed attempt's funding and those rows are the settle sweep's. */
  readonly onUnattended?: (objective: WatcherProofObjective) => void;
}): WatcherProofObjectiveCleanup => {
  type Entry = {
    readonly objective: WatcherProofObjective;
    final: boolean;
    departed: boolean;
    failure?: WatcherProofCleanupFailure;
  };
  const entries = new Map<string, Entry>();
  const stale = new Set<string>();
  const keyOf = ({ category, headerHash }: WatcherProofObjective): string =>
    `${category}:${headerHash}`;
  const pending = (objective: WatcherProofObjective): boolean =>
    input.jobInProcess?.(objective) === true ||
    (!stale.has(keyOf(objective)) &&
      watcherProofJobPending(input.database(), objective));
  const settle = (entry: Entry): void => {
    const { objective } = entry;
    if (pending(objective)) return;
    try {
      let keep = false;
      if (!entry.final)
        try {
          keep =
            readWatcherProofExecution({
              journal: input.workflowJournal,
              deploymentFingerprint: input.deploymentFingerprint,
              objective,
            })?.entries.some(
              ({ event }) => event.kind === "submission_intent",
            ) === true;
        } catch (error) {
          if (!isWatcherProofJournalRefusal(error)) throw error;
          keep = true;
        }
      if (!keep) input.workflowJournal.forgetObjective(objective);
      forgetWatcherProofObjective(input.database(), objective, {
        decisions: entry.final,
      });
      entries.delete(keyOf(objective));
      stale.delete(keyOf(objective));
      if (keep) input.onUnattended?.(objective);
      else input.onRemoved?.(objective);
    } catch (error) {
      entry.failure = Object.freeze({
        category: objective.category,
        headerHash: objective.headerHash,
        detail: `${objective.category}/${objective.headerHash}: ${
          error instanceof Error ? error.message : String(error)
        }`,
      });
    }
  };
  return Object.freeze({
    markStale: (objective) => void stale.add(keyOf(objective)),
    forget: async (objective, how) => {
      const previous = entries.get(keyOf(objective));
      const entry: Entry = {
        objective: Object.freeze({
          category: objective.category,
          headerHash: objective.headerHash,
        }),
        final: how.final || previous?.final === true,
        departed: how.departed || previous?.departed === true,
      };
      entries.set(keyOf(objective), entry);
      if (entry.departed) settle(entry);
    },
    retry: async (observation, open) => {
      for (const [key, entry] of [...entries]) {
        if (open(entry.objective)) {
          entries.delete(key);
          continue;
        }
        entry.departed ||= !observation.finalizedHeaders.some(
          ({ headerHash }) => headerHash === entry.objective.headerHash,
        );
        if (entry.departed) settle(entry);
      }
    },
    failures: () =>
      Object.freeze(
        [...entries.values()].flatMap(({ failure }) =>
          failure === undefined ? [] : [failure],
        ),
      ),
  });
};
