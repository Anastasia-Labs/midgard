import type { CommitteeStore } from "../store.js";
import {
  pageCandidates,
  pruneRetentionCandidates,
  type RetentionCandidate,
  type RetentionDeadlineOptions,
  type RetentionDeadlineReport,
  retentionDeadlineReportFromCandidates,
  type RetentionPruneResult,
  type RetentionScanOptions,
} from "./retention.js";

/**
 * A retention scan reads and decides this many retained payloads at a time,
 * by a keyset cursor over the header hash, so a cycle never holds the table.
 */
const RETENTION_SCAN_PAGE_SIZE = 512;

/**
 * The retention candidates of every retained payload, one page at a time.
 * A page holds retention metadata only, never the payload bytes.
 */
async function* retentionCandidatePages(
  store: CommitteeStore,
  options: RetentionScanOptions,
): AsyncGenerator<readonly RetentionCandidate[]> {
  const limit = options.scanPageSize ?? RETENTION_SCAN_PAGE_SIZE;
  let afterHeaderHash: string | undefined;
  for (;;) {
    const page = await store.listDaPayloadRetentionPage({
      ...(afterHeaderHash === undefined ? {} : { afterHeaderHash }),
      limit,
    });
    if (page.length > 0) yield await pageCandidates(store, page, options);
    if (page.length < limit) return;
    afterHeaderHash = page.at(-1)!.headerHash;
  }
}

/** Every retained payload's retention candidate. */
export const retentionCandidates = async (
  store: CommitteeStore,
  options: RetentionScanOptions,
): Promise<readonly RetentionCandidate[]> => {
  const candidates: RetentionCandidate[] = [];
  for await (const page of retentionCandidatePages(store, options))
    candidates.push(...page);
  return candidates;
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

/** One report over the pages' reports; `empty` is the report of no page. */
const mergeDeadlineReports = (
  empty: RetentionDeadlineReport,
  pages: readonly RetentionDeadlineReport[],
): RetentionDeadlineReport => {
  const sum = (count: (page: RetentionDeadlineReport) => number) =>
    pages.reduce((total, page) => total + count(page), 0);
  const missing = sum((page) => page.recoveryProofUnavailable ?? 0);
  return {
    ...empty,
    scanned: sum((page) => page.scanned),
    retained: sum((page) => page.retained),
    prunable: sum((page) => page.prunable),
    alerting: sum((page) => page.alerting),
    entries: pages.flatMap(({ entries }) => [...entries]),
    ...(missing === 0 ? {} : { recoveryProofUnavailable: missing }),
  };
};

export type RetentionCycleResult = {
  readonly deadlines: RetentionDeadlineReport;
  readonly prune: RetentionPruneResult;
};

/** Reports each page of the retained set, then prunes it when `prune` is set. */
const scanRetention = async (
  store: CommitteeStore,
  options: RetentionDeadlineOptions,
  prune: boolean,
): Promise<RetentionCycleResult> => {
  // Each page is reported and then pruned from one joined snapshot. Incoming
  // DA writes may run concurrently with the committee node tick; a second
  // scan could otherwise delete a record the report never named.
  const empty = retentionDeadlineReportFromCandidates([], options);
  const pages: RetentionDeadlineReport[] = [];
  const prunes: RetentionPruneResult[] = [];
  for await (const page of retentionCandidatePages(store, options)) {
    pages.push(retentionDeadlineReportFromCandidates(page, options));
    if (prune)
      prunes.push(await pruneRetentionCandidates(store, page, options));
  }
  return {
    deadlines: mergeDeadlineReports(empty, pages),
    prune: mergePruneResults(prunes),
  };
};

/** Deletes every retained DA payload the core retention decision prunes. */
export const pruneExpiredDaPayloads = async (
  store: CommitteeStore,
  options: RetentionScanOptions,
): Promise<RetentionPruneResult> =>
  (await scanRetention(store, options, true)).prune;

/** Executable deadline report over the retained DA payload set. */
export const retentionDeadlineReport = async (
  store: CommitteeStore,
  options: RetentionDeadlineOptions,
): Promise<RetentionDeadlineReport> =>
  (await scanRetention(store, options, false)).deadlines;

/** One non-overlapping production retention cycle: report before deletion. */
export const runRetentionCycle = (
  store: CommitteeStore,
  options: RetentionDeadlineOptions,
): Promise<RetentionCycleResult> => scanRetention(store, options, true);
