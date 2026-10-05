import type { SettlementHealth } from "./settlement.reconcile-attempt.js";

/** Worker runs that must die back to back before readiness names settlement.
 * One or two deaths are a provider or Postgres blip the replacement absorbs;
 * three in a row is a crash loop no restart will fix. */
export const SETTLEMENT_WORKER_FAILURE_STREAK = 3;
/** How long the streak must last. Longer than the chaos drills' L1 provider
 * (60 s) and Postgres (30 s) outages, which report their own reasons, so a
 * worker that recovers after one never takes the node out of service. */
export const SETTLEMENT_WORKER_FAILURE_WINDOW_MS = 120_000;
/** Completed settlement ticks in a row, within one worker run, that forget
 * the streak (fibers/settlement.ts). Staying up while every tick fails is not
 * recovery, and neither is one cheap tick (a pending body's reconcile, an
 * eligibility wait) before the run dies building a job: a run that does that
 * every time would otherwise clear the streak on each restart and never reach
 * readiness. */
export const SETTLEMENT_WORKER_RECOVERY_TICKS = 3;

const shortDetail = (detail: string) =>
  (detail.split("\n", 1)[0] ?? "").trim().slice(0, 160);

/** Job-level failures, eligibility waits and pending confirmations are payout
 * delays and stay out of readiness; only a worker that cannot stay up is. */
export const settlementReadinessReason = (
  health: SettlementHealth,
  now: number,
): string | undefined => {
  const failures = health.workerFailures;
  if (
    failures === undefined ||
    failures.count < SETTLEMENT_WORKER_FAILURE_STREAK ||
    now - failures.since < SETTLEMENT_WORKER_FAILURE_WINDOW_MS
  )
    return undefined;
  return `settlement_worker_failing:${failures.count}:${shortDetail(failures.last)}`;
};
