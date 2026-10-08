import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import {
  readWatcherProofExecution,
  removeWatcherProofObjectiveDirectory,
  type WatcherProofObjective,
} from "./fault-proof-objective-journal.js";
import {
  forgetWatcherProofObjective,
  watcherProofJobPending,
} from "./fault-proof-objective-table.js";
import type { WatcherJournalDatabase } from "./watcher-journal-database.js";

/** A workflow directory a release or a final completion could not remove:
 * its rows stay, and the next release pass retries it. */
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
   * Forgets an objective no longer driven, with its workflow directory once
   * no job of it is queued or active: a final completion's always, decisions
   * included; a released one's unless it holds a signed attempt, which the
   * funding sweep reads. A departed objective's header has left the
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
 * The directory goes before the rows, so a crash in between leaves a row
 * whose execution is gone, which the next start forgets. A removal that
 * fails is named and retried; it never fails the caller.
 */
export const createWatcherProofObjectiveCleanup = (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly database: () => WatcherJournalDatabase;
  /** Whether a job of the objective is queued, running or handed over in
   * this process. */
  readonly jobInProcess?: (objective: WatcherProofObjective) => boolean;
  /** Told after the objective's workflow directory is removed. */
  readonly onRemoved?: (objective: WatcherProofObjective) => void;
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
  const settle = async (entry: Entry): Promise<void> => {
    const { objective } = entry;
    if (pending(objective)) return;
    try {
      const signed =
        !entry.final &&
        (
          await readWatcherProofExecution({
            journalRoot: input.journalRoot,
            deploymentFingerprint: input.deploymentFingerprint,
            objective,
          })
        )?.entries.some(({ event }) => event.kind === "submission_intent") ===
          true;
      if (!signed) {
        await removeWatcherProofObjectiveDirectory(
          input.journalRoot,
          objective,
        );
        input.onRemoved?.(objective);
      }
    } catch (error) {
      entry.failure = Object.freeze({
        category: objective.category,
        headerHash: objective.headerHash,
        detail: `${objective.category}/${objective.headerHash}: ${
          error instanceof Error ? error.message : String(error)
        }`,
      });
      return;
    }
    forgetWatcherProofObjective(input.database(), objective, {
      decisions: entry.final,
    });
    entries.delete(keyOf(objective));
    stale.delete(keyOf(objective));
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
      if (entry.departed) await settle(entry);
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
        if (entry.departed) await settle(entry);
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
