import { isWatcherL1TransientFailure } from "../l1/transient-failure.js";
import { retryWatcherL1Transient } from "../l1/transient-retry.js";

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
 * Stages that read only the L1 provider and leave nothing allocated when they
 * fail, so an L1 transient (Kupo, Ogmios or the node did not answer) runs them
 * again in place instead of failing startup. Every other stage, and every
 * other error, still fails startup: the deployment identity and configuration
 * stages compare durable bytes, which no wait changes. A stage that allocates
 * resources it keeps (`workflow_readiness`) is not repeated whole; it repeats
 * only its L1 reads, through the `retryL1Read` it is handed. A stage that
 * consumes prepared work (`workflow_recovery`) is not repeated at all.
 * `user_event_catchup` and `header_classification` wait inside their own
 * components.
 */
export const WATCHER_STARTUP_L1_RETRIED_STAGES: ReadonlySet<string> = new Set([
  "state_queue_recovery",
  "user_event_runtime",
]);

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

const heldOrL1Transient = (error: unknown): error is Error =>
  held(error) || isWatcherL1TransientFailure(error);

/** What a stage's action is handed. */
export type WatcherStartupStage = Readonly<{
  /**
   * Runs one read again through L1 transients, reporting each wait as
   * `pending` on this stage. Only for a read that is safe to repeat and
   * allocates nothing; any other error is rethrown unchanged.
   */
  retryL1Read: <U>(read: () => Promise<U>) => Promise<U>;
}>;

/** Startup diagnostics remain available before the operations server binds. */
export const createWatcherStartupProgress =
  (
    report: ((progress: WatcherStartupProgress) => void) | undefined,
    retryDelayMs?: (retry: number) => number,
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
        retryWatcherL1Transient(read, retrying(isWatcherL1TransientFailure)),
    });
    const run = () =>
      retryWatcherL1Transient(
        () => action(context),
        retrying(
          WATCHER_STARTUP_L1_RETRIED_STAGES.has(stage)
            ? heldOrL1Transient
            : held,
        ),
      );
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
