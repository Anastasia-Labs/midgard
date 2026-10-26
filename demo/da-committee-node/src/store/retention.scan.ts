import type { CommitteeStore } from "../store.js";
import {
  nearestAlertingEntries,
  pageCandidates,
  pruneRetentionCandidates,
  type RetentionDeadlineOptions,
  type RetentionDeadlineReport,
  retentionDeadlineReportFromCandidates,
  type RetentionPruneResult,
} from "./retention.js";

/**
 * A retention scan reads and decides this many retained payloads at a time,
 * by a keyset cursor over the header hash, so a cycle never holds the table.
 */
const RETENTION_SCAN_PAGE_SIZE = 512;

/**
 * The most retained payloads one cycle decides. A cycle runs inside the
 * committee tick, so its cost is bounded by this budget, never by the
 * retained set: a larger set is swept over successive cycles, each one
 * continuing where the last stopped.
 */
export const RETENTION_CYCLE_ROW_BUDGET = 2_048;

/**
 * Where a sweep of the retained set stands between cycles; a fresh sweep is
 * `{}`. Each cycle advances it.
 */
export type RetentionSweep = {
  /** The last header hash decided; the next cycle continues after it. */
  afterHeaderHash?: string;
  /** The report over what the sweep in progress has decided so far. */
  progress?: RetentionDeadlineReport;
  /** The report of the last completed sweep. */
  completed?: RetentionDeadlineReport;
};

const mergePruneResults = (
  results: readonly RetentionPruneResult[],
): RetentionPruneResult => ({
  scanned: results.reduce((total, { scanned }) => total + scanned, 0),
  prunedHeaderHashes: results.flatMap(({ prunedHeaderHashes }) => [
    ...prunedHeaderHashes,
  ]),
  retained: results.reduce((total, { retained }) => total + retained, 0),
});

/**
 * One report over `reports`, stamped with `empty`'s time and window: counts
 * are summed and only the nearest alerting entries are kept.
 */
const mergeDeadlineReports = (
  empty: RetentionDeadlineReport,
  reports: readonly RetentionDeadlineReport[],
): RetentionDeadlineReport => {
  const sum = (count: (report: RetentionDeadlineReport) => number) =>
    reports.reduce((total, report) => total + count(report), 0);
  const missing = sum((report) => report.recoveryProofUnavailable ?? 0);
  return {
    ...empty,
    scanned: sum((report) => report.scanned),
    retained: sum((report) => report.retained),
    prunable: sum((report) => report.prunable),
    alerting: sum((report) => report.alerting),
    entries: nearestAlertingEntries(reports.flatMap(({ entries }) => entries)),
    ...(missing === 0 ? {} : { recoveryProofUnavailable: missing }),
  };
};

export type RetentionCycleResult = {
  /**
   * The report of the last completed sweep; while the first sweep is still
   * in progress, the report over what it has decided so far.
   */
  readonly deadlines: RetentionDeadlineReport;
  /** Whether this cycle reached the end of the retained set. */
  readonly sweepCompleted: boolean;
  /** What this cycle decided and pruned. */
  readonly prune: RetentionPruneResult;
};

/**
 * One non-overlapping production retention cycle: it continues `sweep`,
 * deciding at most the cycle's row budget, each page reported before
 * deletion.
 */
export const runRetentionCycle = async (
  store: CommitteeStore,
  options: RetentionDeadlineOptions,
  sweep: RetentionSweep,
): Promise<RetentionCycleResult> => {
  const budget = options.cycleRowBudget ?? RETENTION_CYCLE_ROW_BUDGET;
  if (!Number.isSafeInteger(budget) || budget < 1)
    throw new Error("retention cycle budget must be a positive safe integer");
  const pageSize = options.scanPageSize ?? RETENTION_SCAN_PAGE_SIZE;
  const empty = retentionDeadlineReportFromCandidates([], options);
  const prunes: RetentionPruneResult[] = [];
  let decided = 0;
  let sweepCompleted = false;
  while (decided < budget) {
    const limit = Math.min(pageSize, budget - decided);
    const { afterHeaderHash } = sweep;
    const page = await store.listDaPayloadRetentionPage({
      ...(afterHeaderHash === undefined ? {} : { afterHeaderHash }),
      limit,
    });
    if (page.length > 0) {
      // Each page is reported and then pruned from one joined snapshot.
      // Incoming DA writes may run concurrently with the committee node
      // tick; a second scan could otherwise delete a record the report
      // never named.
      const candidates = await pageCandidates(store, page, options);
      const report = retentionDeadlineReportFromCandidates(candidates, options);
      prunes.push(await pruneRetentionCandidates(store, candidates, options));
      sweep.progress = mergeDeadlineReports(empty, [
        ...(sweep.progress === undefined ? [] : [sweep.progress]),
        report,
      ]);
      sweep.afterHeaderHash = page.at(-1)!.headerHash;
      decided += page.length;
    }
    if (page.length < limit) {
      sweepCompleted = true;
      break;
    }
  }
  if (sweepCompleted) {
    sweep.completed = mergeDeadlineReports(
      empty,
      sweep.progress === undefined ? [] : [sweep.progress],
    );
    sweep.progress = undefined;
    sweep.afterHeaderHash = undefined;
  }
  return {
    deadlines: sweep.completed ?? sweep.progress ?? empty,
    sweepCompleted,
    prune: mergePruneResults(prunes),
  };
};
