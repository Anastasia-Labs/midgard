import { isWatcherL1TransientFailure } from "../l1/transient-failure.js";
import { retryWatcherL1Transient } from "../l1/transient-retry.js";

/**
 * How long a startup stage's L1 read waits out transients before startup
 * fails (`WatcherL1UnavailableError`): the node's L1 node budget, which
 * covers a Cardano node that is restarting or still opening its database
 * beside the watcher. Past it the process exits non-zero, and its
 * supervisor's restart policy is the outer retry.
 */
export const WATCHER_STARTUP_L1_BUDGET_MS = 10 * 60_000;

export type WatcherStartupProgress = Readonly<{
  stage: string;
  outcome: "started" | "pending" | "completed" | "failed";
  elapsedMs: number;
  observedAt: string;
  error?: string;
  /** Set on `pending` while the stage waits out an L1 transient or a hold. */
  retryAfterMs?: number;
}>;

/**
 * A stage that cannot complete until the chain or a peer moves, with nothing
 * wrong in its inputs. Thrown only by code that knows waiting is the answer;
 * any stage that throws it is run again after a capped backoff.
 */
export class WatcherStartupStageHeld extends Error {
  constructor(message: string) {
    super(message);
    this.name = "WatcherStartupStageHeld";
  }
}

const held = (error: unknown): error is WatcherStartupStageHeld =>
  error instanceof WatcherStartupStageHeld;

/** What a stage's action is handed. */
export type WatcherStartupStage = Readonly<{
  /**
   * Runs one read again through L1 transients, reporting each wait as
   * `pending` on this stage. Only for a read that is safe to repeat and
   * allocates nothing; any other error is rethrown unchanged.
   */
  retryL1Read: <U>(read: () => Promise<U>) => Promise<U>;
}>;

/**
 * Startup diagnostics remain available before the operations server binds. A
 * stage is run again whole only when it throws {@link WatcherStartupStageHeld}
 * (waiting on the chain or a peer: no deadline); an L1 transient repeats just
 * the read passed to `retryL1Read`, for at most `l1BudgetMs`
 * (`WATCHER_STARTUP_L1_BUDGET_MS`); every other error fails startup.
 */
export const createWatcherStartupProgress =
  (
    report: ((progress: WatcherStartupProgress) => void) | undefined,
    retryDelayMs?: (retry: number) => number,
    budget: Readonly<{ l1BudgetMs?: number; now?: () => number }> = {},
  ) =>
  async <T>(
    stage: string,
    action: (context: WatcherStartupStage) => Promise<T>,
  ): Promise<T> => {
    const startedAt = performance.now();
    const emit = (
      outcome: WatcherStartupProgress["outcome"],
      cause?: Readonly<{ error: unknown; retryAfterMs?: number }>,
    ) =>
      report?.({
        stage,
        outcome,
        elapsedMs: performance.now() - startedAt,
        observedAt: new Date().toISOString(),
        ...(cause === undefined
          ? {}
          : {
              error:
                cause.error instanceof Error
                  ? cause.error.message
                  : String(cause.error),
            }),
        ...(cause?.retryAfterMs === undefined
          ? {}
          : { retryAfterMs: cause.retryAfterMs }),
      });
    const retrying = (transient: (error: unknown) => error is Error) =>
      Object.freeze({
        transient,
        onRetry: (error: Error, _retry: number, retryAfterMs: number) =>
          emit("pending", { error, retryAfterMs }),
        ...(retryDelayMs === undefined ? {} : { delayMs: retryDelayMs }),
      });
    const context: WatcherStartupStage = Object.freeze({
      retryL1Read: (read) =>
        retryWatcherL1Transient(read, {
          ...retrying(isWatcherL1TransientFailure),
          budgetMs: budget.l1BudgetMs ?? WATCHER_STARTUP_L1_BUDGET_MS,
          ...(budget.now === undefined ? {} : { now: budget.now }),
        }),
    });
    const run = () =>
      retryWatcherL1Transient(() => action(context), retrying(held));
    if (report === undefined) return await run();
    emit("started");
    const timer = setInterval(() => emit("pending"), 30_000);
    timer.unref();
    try {
      const result = await run();
      emit("completed");
      return result;
    } catch (error) {
      emit("failed", { error });
      throw error;
    } finally {
      clearInterval(timer);
    }
  };
