import type { CommitteeStore } from "../../src/store.js";
import {
  pageCandidates,
  type RetentionCandidate,
  type RetentionDeadlineOptions,
  type RetentionDeadlineReport,
  retentionDeadlineReportFromCandidates,
  type RetentionPruneResult,
  type RetentionScanOptions,
} from "../../src/store/retention.js";
import { runRetentionCycle } from "../../src/store/retention.scan.js";

/**
 * Whole-set views of the retained payloads for tests. Production decides
 * the set only in bounded cycles (`runRetentionCycle`); these collect all
 * of a test's small set at once.
 */

/** Every retained payload's retention candidate, read a page at a time. */
export const retentionCandidates = async (
  store: CommitteeStore,
  options: RetentionScanOptions,
): Promise<readonly RetentionCandidate[]> => {
  const limit = options.scanPageSize ?? 512;
  const candidates: RetentionCandidate[] = [];
  let afterHeaderHash: string | undefined;
  for (;;) {
    const page = await store.listDaPayloadRetentionPage({
      ...(afterHeaderHash === undefined ? {} : { afterHeaderHash }),
      limit,
    });
    if (page.length > 0)
      candidates.push(...(await pageCandidates(store, page, options)));
    if (page.length < limit) return candidates;
    afterHeaderHash = page.at(-1)!.headerHash;
  }
};

/** The deadline report over every retained payload, deleting nothing. */
export const retentionDeadlineReport = async (
  store: CommitteeStore,
  options: RetentionDeadlineOptions,
): Promise<RetentionDeadlineReport> =>
  retentionDeadlineReportFromCandidates(
    await retentionCandidates(store, options),
    options,
  );

/** One production cycle over the whole retained set, from a fresh sweep. */
export const pruneExpiredDaPayloads = async (
  store: CommitteeStore,
  options: RetentionScanOptions,
): Promise<RetentionPruneResult> =>
  (
    await runRetentionCycle(
      store,
      { ...options, cycleRowBudget: Number.MAX_SAFE_INTEGER },
      {},
    )
  ).prune;
