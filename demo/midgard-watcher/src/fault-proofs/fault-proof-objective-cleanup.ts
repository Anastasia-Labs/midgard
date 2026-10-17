import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import {
  isWatcherProofJournalRefusal,
  readWatcherProofExecution,
  removeWatcherProofObjectiveDirectory,
  sweepWatcherProofTombstones,
  type WatcherProofObjective,
} from "./fault-proof-objective-journal.js";
import {
  forgetWatcherProofObjective,
  watcherProofJobPending,
} from "./fault-proof-objective-table.js";
import type { WatcherJournalDatabase } from "./watcher-journal-database.js";

/** A workflow directory a release or a final completion could not remove
 * (its rows stay), or a removed directory's tombstone the sweep could not
 * delete; the next retry pass retries either. */
export type WatcherProofCleanupFailure = Readonly<{
  category: WatcherProofObjective["category"];
  headerHash: string;
  detail: string;
}>;

export type WatcherProofObjectiveCleanup = Readonly<{
  /** Marks an objective whose queue rows a previous process left queued or
   * active: no job of this process owns them, so they never defer it. */
  markStale(objective: WatcherProofObjective): void;
  /** Deletes the tombstones earlier removals left; startup runs it, and so
   * does every retry pass. */
  sweep(): Promise<void>;
  /**
   * Forgets an objective no longer driven, with its workflow directory once
   * no job of it is queued or active: a final completion's always, decisions
   * included; a released one's unless it holds a signed attempt, which the
   * funding sweep reads, or cannot be read for a refusal. A departed objective's header has left the
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
 * Removal renames the directory to a tombstone, forgets the rows, then
 * deletes the tombstone (`removeWatcherProofObjectiveDirectory`). A crash
 * before the rename leaves the directory and rows whole, so the next start
 * settles them again; one after it leaves rows whose execution is gone,
 * which the next start forgets; one during the delete leaves only the
 * tombstone, which startup and every retry pass sweep. An objective whose
 * directory a refusal keeps from being read or moved (a symlinked path, a
 * sequence gap, a foreign execution) is forgotten with its directory left in
 * place, as a released one with a signed attempt is: what it holds is never
 * trusted or deleted. A removal that fails on a filesystem error is named
 * and retried; it never fails the caller.
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
  // Tombstones the last sweep could not delete.
  let tombstoneFailures: readonly WatcherProofCleanupFailure[] = [];
  const keyOf = ({ category, headerHash }: WatcherProofObjective): string =>
    `${category}:${headerHash}`;
  const pending = (objective: WatcherProofObjective): boolean =>
    input.jobInProcess?.(objective) === true ||
    (!stale.has(keyOf(objective)) &&
      watcherProofJobPending(input.database(), objective));
  const sweep = async (): Promise<void> => {
    tombstoneFailures = Object.freeze(
      (await sweepWatcherProofTombstones(input.journalRoot)).map(
        ({ name, detail }) => {
          const [category = name, headerHash = ""] = name.split(".");
          return Object.freeze({
            category: category as WatcherProofObjective["category"],
            headerHash,
            detail: `tombstone ${name}: ${detail}`,
          });
        },
      ),
    );
  };
  const settle = async (entry: Entry): Promise<void> => {
    const { objective } = entry;
    if (pending(objective)) return;
    let forgotten = false;
    const forgetRows = (): void => {
      forgetWatcherProofObjective(input.database(), objective, {
        decisions: entry.final,
      });
      entries.delete(keyOf(objective));
      stale.delete(keyOf(objective));
      forgotten = true;
    };
    try {
      let keep = false;
      if (!entry.final)
        try {
          keep =
            (
              await readWatcherProofExecution({
                journalRoot: input.journalRoot,
                deploymentFingerprint: input.deploymentFingerprint,
                objective,
              })
            )?.entries.some(
              ({ event }) => event.kind === "submission_intent",
            ) === true;
        } catch (error) {
          if (!isWatcherProofJournalRefusal(error)) throw error;
          keep = true;
        }
      if (
        keep ||
        (await removeWatcherProofObjectiveDirectory(
          input.journalRoot,
          objective,
          forgetRows,
        )) === "refused"
      ) {
        forgetRows();
        return;
      }
      input.onRemoved?.(objective);
    } catch (error) {
      // Renamed and forgotten: only the tombstone's delete failed, and the
      // next sweep retries it.
      if (forgotten) {
        input.onRemoved?.(objective);
        return;
      }
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
    sweep,
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
      await sweep();
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
      Object.freeze([
        ...[...entries.values()].flatMap(({ failure }) =>
          failure === undefined ? [] : [failure],
        ),
        ...tombstoneFailures,
      ]),
  });
};
