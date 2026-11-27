import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
import {
  forgetWatcherProofObjective,
  watcherProofJobPending,
} from "./fault-proof-objective-table.js";
import type { WatcherJournalDatabase } from "./watcher-journal-database.js";

/** An objective whose rows a release or a final completion could not
 * delete (its rows stay); the next retry pass retries it. */
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
   * Forgets an objective no longer driven once no job of it is queued or
   * active: a final completion's decisions go with its rows. A departed
   * objective's header has left the finalized queue; any other waits for an
   * observation that drops it. A superseded objective's departure is its
   * supersession, and `decision` names the one decision row that goes with
   * its rows.
   */
  forget(
    objective: WatcherProofObjective,
    how: Readonly<{ final: boolean; departed: boolean; decision?: string }>,
  ): Promise<void>;
  /** Whether a job of the objective is queued or active in this process,
   * or queued or active in its rows and not left by an earlier process. */
  pending(objective: WatcherProofObjective): boolean;
  /** Retries every deferred or failed objective that is not open again. */
  retry(
    observation: WatcherAuthenticatedStateQueueObservation,
    open: (objective: WatcherProofObjective) => boolean,
  ): Promise<void>;
  failures(): readonly WatcherProofCleanupFailure[];
}>;

/**
 * Removal forgets the objective's rows in watcher-journals.sqlite in one
 * commit; a crash before it leaves them whole, so the next start forgets
 * them again. A removal that fails on a storage error is named and retried;
 * it never fails the caller.
 */
export const createWatcherProofObjectiveCleanup = (input: {
  readonly database: () => WatcherJournalDatabase;
  /** Whether a job of the objective is queued, running or handed over in
   * this process. */
  readonly jobInProcess?: (objective: WatcherProofObjective) => boolean;
  /** Told after the objective's rows are deleted. */
  readonly onRemoved?: (objective: WatcherProofObjective) => void;
}): WatcherProofObjectiveCleanup => {
  type Entry = {
    readonly objective: WatcherProofObjective;
    final: boolean;
    departed: boolean;
    decision?: string;
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
      forgetWatcherProofObjective(input.database(), objective, {
        decisions: entry.final,
        ...(entry.decision === undefined ? {} : { decision: entry.decision }),
      });
      entries.delete(keyOf(objective));
      stale.delete(keyOf(objective));
      input.onRemoved?.(objective);
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
    pending,
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
      const decision = how.decision ?? previous?.decision;
      if (decision !== undefined) entry.decision = decision;
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
