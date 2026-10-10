/**
 * Child restart bookkeeping of the native MPF owner, on the monotonic clock (a
 * wall-clock step must neither refill nor drain a window).
 *
 * A child failure (the child died, or a restart of it failed) restarts the
 * child from the durable root marker, each restart after an exponential
 * backoff over the restarts still inside `restartWindowMs`. A restart repairs
 * a failure only some of the time, so the restarts are bounded: the same
 * failure `max(1, restartLimit)` times in a row holds the owner (`held`). A
 * held owner restarts no more and refuses every operation until the node
 * restarts. A failure no restart repairs holds at once: the binary on disk is
 * not the pinned one (`NativeOwnerBinaryPinMismatchError`). A different
 * failure starts the count over, and so does the death of a child that ran
 * `restartWindowMs` or longer.
 */
export type NativeOwnerRestartPolicyOptions = {
  readonly restartLimit: number;
  readonly restartWindowMs: number;
  readonly restartBackoffBaseMs: number;
  readonly restartBackoffMaxMs: number;
};

export const DEFAULT_RESTART_BACKOFF_BASE_MS = 1_000;
export const DEFAULT_RESTART_BACKOFF_MAX_MS = 60_000;

/** The owner binary on disk is not the pinned one: no restart repairs it. */
export class NativeOwnerBinaryPinMismatchError extends Error {
  override readonly name = "NativeOwnerBinaryPinMismatchError";
}

export type NativeOwnerRestartHealth = {
  /** Child restarts started inside the window. */
  readonly restartsInWindow: number;
  /** How many times in a row the latest failure has repeated. */
  readonly failuresInARow: number;
  readonly restartLimit: number;
  readonly restartWindowMs: number;
  /** The owner holds: it restarts no more until the node restarts. */
  readonly held: boolean;
};

/** A failure's identity: its message without the child's stderr tail, which
 * differs between two deaths of the same cause. */
export const nativeOwnerFailureKey = (error: Error): string =>
  error.message.replace(/,stderr=[\s\S]*$/u, "");

export class NativeOwnerRestartPolicy {
  private restartsAt: number[] = [];
  private failure: { key: string; count: number } | undefined;
  private heldBy: Error | undefined;
  private childStartedAt: number;
  private cancelWait: (() => void) | undefined;

  public constructor(
    private readonly options: NativeOwnerRestartPolicyOptions,
    private readonly now: () => number = () => performance.now(),
  ) {
    this.childStartedAt = now();
  }

  private prune(now: number): void {
    this.restartsAt = this.restartsAt.filter(
      (at) => now - at < this.options.restartWindowMs,
    );
  }

  /** The terminal hold, once a failure repeated `restartLimit` times in a
   * row or one no restart repairs came; undefined otherwise. */
  public held(): Error | undefined {
    return this.heldBy;
  }

  /** Records a restart about to start and returns its backoff delay: none for
   * the first restart inside the window, then base, 2 x base, ... up to max. */
  public startRestart(): number {
    const now = this.now();
    this.prune(now);
    const previous = this.restartsAt.length;
    this.restartsAt.push(now);
    if (previous === 0) return 0;
    return Math.min(
      this.options.restartBackoffMaxMs,
      this.options.restartBackoffBaseMs * 2 ** Math.min(previous - 1, 30),
    );
  }

  /** A restart started a child; a death of it counts from here. */
  public recordStarted(): void {
    this.childStartedAt = this.now();
  }

  /**
   * Records a child failure (`death`: the running child failed; `restart`: a
   * restart of it failed) and returns whether the owner now holds.
   */
  public recordFailure(error: Error, kind: "death" | "restart"): boolean {
    if (this.heldBy !== undefined) return true;
    const key = nativeOwnerFailureKey(error);
    const ranLong =
      kind === "death" &&
      this.now() - this.childStartedAt >= this.options.restartWindowMs;
    const count =
      !ranLong && this.failure?.key === key ? this.failure.count + 1 : 1;
    this.failure = { key, count };
    if (error instanceof NativeOwnerBinaryPinMismatchError)
      this.heldBy = new Error(
        `Native MPF owner holds: no restart repairs this failure, so it restarts no more until the node restarts. Restore the pinned binary, then restart the node: ${error.message}`,
        { cause: error },
      );
    else if (count >= Math.max(1, this.options.restartLimit))
      this.heldBy = new Error(
        `Native MPF owner holds: the same failure ${count.toString()} time(s) in a row, so it restarts no more until the node restarts. Read the child's error, fix its cause, then restart the node: ${error.message}`,
        { cause: error },
      );
    return this.heldBy !== undefined;
  }

  public health(): NativeOwnerRestartHealth {
    this.prune(this.now());
    return {
      restartsInWindow: this.restartsAt.length,
      failuresInARow: this.failure?.count ?? 0,
      restartLimit: this.options.restartLimit,
      restartWindowMs: this.options.restartWindowMs,
      held: this.heldBy !== undefined,
    };
  }

  /** Waits `ms`, or until `cancel` is called (on close). */
  public wait(ms: number): Promise<void> {
    if (ms <= 0) return Promise.resolve();
    return new Promise((resolve) => {
      const timer = setTimeout(() => {
        this.cancelWait = undefined;
        resolve();
      }, ms);
      this.cancelWait = () => {
        clearTimeout(timer);
        this.cancelWait = undefined;
        resolve();
      };
    });
  }

  public cancel(): void {
    this.cancelWait?.();
  }
}
