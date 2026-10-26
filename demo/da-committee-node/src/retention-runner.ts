import {
  type CommitteeRetentionReadinessSnapshot,
  retentionReadinessFromDeadlines,
} from "./committee-service.js";
import type { CommitteeStore } from "./store.js";
import {
  retentionCycleOptions,
  type RetentionL1View,
} from "./store/retention.js";
import {
  type RetentionSweep,
  runRetentionCycle,
} from "./store/retention.scan.js";

/**
 * The node's retention passes and the readiness they leave. Each pass runs
 * one bounded cycle, continuing one sweep of the retained set across ticks.
 */
export const createRetentionRunner = (deps: {
  readonly store: CommitteeStore;
  readonly config: Parameters<typeof retentionCycleOptions>[0];
  readonly compactRetainedPromises: () => Promise<unknown>;
  /** `--once`: a single pass, which has no later tick to finish a sweep in. */
  readonly once: boolean;
  readonly startupCompactionFailure: string | undefined;
}) => {
  let readiness: CommitteeRetentionReadinessSnapshot =
    deps.startupCompactionFailure === undefined
      ? {
          status: "not_checked",
          scanned: 0,
          retained: 0,
          prunable: 0,
          alerting: 0,
        }
      : {
          status: "failed",
          checkedAt: new Date().toISOString(),
          scanned: 0,
          retained: 0,
          prunable: 0,
          alerting: 0,
          error: deps.startupCompactionFailure,
        };
  const sweep: RetentionSweep = {};
  const run = async (view: RetentionL1View): Promise<void> => {
    // The exemption sets come from the L1 view the poller accepted this tick.
    const options = retentionCycleOptions(deps.config, view, Date.now());
    try {
      await deps.compactRetainedPromises();
      const { deadlines, prune, sweepCompleted } = await runRetentionCycle(
        deps.store,
        deps.once
          ? { ...options, cycleRowBudget: Number.MAX_SAFE_INTEGER }
          : options,
        sweep,
      );
      readiness = retentionReadinessFromDeadlines(deadlines);
      if (prune.prunedHeaderHashes.length > 0) {
        process.stdout.write(
          `${JSON.stringify({ event: "da_retention_pruned", ...prune })}\n`,
        );
      }
      // Once per sweep: counts and only the nearest alerting deadlines.
      if (sweepCompleted && deadlines.alerting > 0) {
        process.stderr.write(
          `${JSON.stringify({ event: "da_retention_deadline_alert", report: deadlines })}\n`,
        );
      }
    } catch (error) {
      readiness = {
        status: "failed",
        checkedAt: new Date(options.nowMs).toISOString(),
        scanned: 0,
        retained: 0,
        prunable: 0,
        alerting: 0,
        error: error instanceof Error ? error.message : String(error),
      };
      throw error;
    }
  };
  return {
    run,
    readiness: (): CommitteeRetentionReadinessSnapshot => readiness,
    update: (
      change: (
        previous: CommitteeRetentionReadinessSnapshot,
      ) => CommitteeRetentionReadinessSnapshot,
    ): void => {
      readiness = change(readiness);
    },
  };
};
