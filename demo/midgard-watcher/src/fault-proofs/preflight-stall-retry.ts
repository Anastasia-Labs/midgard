/**
 * Preflight builds the next family transaction against the live L1 tip while
 * the stage it acts on comes from release-final observation. Between the two,
 * transactions that are not yet release-final (DA attestations re-outputting
 * the state-queue node, for example) can spend the exact output the stage
 * names, so the orchestrator stalls at preflight although the fault and the
 * workflow are sound. The stage catches up once those transactions reach
 * release finality, so a preflight stall is resumed for a bounded window
 * instead of failing the process closed.
 */
export const WATCHER_PREFLIGHT_STALL_RETRY_DELAY_MS = 60_000;
export const WATCHER_PREFLIGHT_STALL_RETRY_BUDGET_MS = 30 * 60_000;

export type WatcherPreflightStalledResult = Readonly<{
  kind: "stalled";
  phase: "preflight";
  reason: string;
}>;

export const isWatcherPreflightStalledResult = (
  result: unknown,
): result is WatcherPreflightStalledResult =>
  typeof result === "object" &&
  result !== null &&
  "kind" in result &&
  result.kind === "stalled" &&
  "phase" in result &&
  result.phase === "preflight" &&
  "reason" in result &&
  typeof result.reason === "string";

export const runWorkflowWithPreflightStallRetries = async ({
  run,
  resume,
  sleep,
  now = () => Date.now(),
  isLive = () => true,
  delayMs = WATCHER_PREFLIGHT_STALL_RETRY_DELAY_MS,
  budgetMs = WATCHER_PREFLIGHT_STALL_RETRY_BUDGET_MS,
  onStall,
}: {
  /** The job's own invocation. */
  readonly run: () => Promise<unknown>;
  /** A later resume of the same journal; `attempt` counts from 1. */
  readonly resume: (attempt: number) => Promise<unknown>;
  readonly sleep: (ms: number) => Promise<void>;
  readonly now?: () => number;
  /** False once the runtime is closing; no further resume is started. */
  readonly isLive?: () => boolean;
  readonly delayMs?: number;
  readonly budgetMs?: number;
  readonly onStall?: (
    input: Readonly<{ attempt: number; reason: string; retrying: boolean }>,
  ) => void;
}): Promise<unknown> => {
  if (!Number.isSafeInteger(delayMs) || delayMs <= 0) {
    throw new Error("preflight stall retry delay must be a positive integer");
  }
  if (!Number.isSafeInteger(budgetMs) || budgetMs < 0) {
    throw new Error("preflight stall retry budget must be a natural number");
  }
  let result = await run();
  const startedAt = now();
  for (let attempt = 1; isWatcherPreflightStalledResult(result); attempt += 1) {
    const retrying = isLive() && now() - startedAt + delayMs <= budgetMs;
    onStall?.({ attempt, reason: result.reason, retrying });
    if (!retrying) return result;
    await sleep(delayMs);
    if (!isLive()) return result;
    result = await resume(attempt);
  }
  return result;
};
