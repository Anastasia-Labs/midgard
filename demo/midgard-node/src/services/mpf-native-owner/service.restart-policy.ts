/**
 * Child restart bookkeeping of the native MPF owner, on the monotonic clock (a
 * wall-clock step must neither refill nor drain a window).
 *
 * A child that dies is always restarted from the durable root marker: the
 * number of restarts is unbounded, each one after an exponential backoff over
 * the restarts still inside `restartWindowMs`. Only a restart that itself
 * fails (the durable root cannot be read, or the child cannot be started from
 * it) counts toward exhaustion: `max(1, restartLimit)` consecutive failed
 * restarts inside the window exhaust the owner, which refuses every operation
 * until the oldest of them leaves the window. A restart that succeeds resets
 * the failure count.
 */
export type NativeOwnerRestartPolicyOptions = {
  readonly restartLimit: number;
  readonly restartWindowMs: number;
  readonly restartBackoffBaseMs: number;
  readonly restartBackoffMaxMs: number;
};

export const DEFAULT_RESTART_BACKOFF_BASE_MS = 1_000;
export const DEFAULT_RESTART_BACKOFF_MAX_MS = 60_000;

export type NativeOwnerRestartHealth = {
  /** Child restarts started inside the window. */
  readonly restartsInWindow: number;
  /** Consecutive failed restarts inside the window. */
  readonly failedRestartsInWindow: number;
  readonly restartLimit: number;
  readonly restartWindowMs: number;
  readonly exhausted: boolean;
};

export class NativeOwnerRestartPolicy {
  private restartsAt: number[] = [];
  private failuresAt: number[] = [];
  private lastFailure: Error | undefined;
  private cancelWait: (() => void) | undefined;

  public constructor(
    private readonly options: NativeOwnerRestartPolicyOptions,
    private readonly now: () => number = () => performance.now(),
  ) {}

  private prune(now: number): void {
    const inWindow = (at: number) => now - at < this.options.restartWindowMs;
    this.restartsAt = this.restartsAt.filter(inWindow);
    this.failuresAt = this.failuresAt.filter(inWindow);
  }

  /** The exhaustion error while `restartLimit` (at least one) consecutive
   * failed restarts are inside the window; undefined otherwise. */
  public exhaustion(): Error | undefined {
    this.prune(this.now());
    const failed = this.failuresAt.length;
    if (failed === 0 || failed < Math.max(1, this.options.restartLimit))
      return undefined;
    return new Error(
      `Native MPF owner restart limit exhausted: ${failed.toString()} failed restart(s) within ${this.options.restartWindowMs.toString()} ms; restarts resume once the oldest leaves the window: ${this.lastFailure?.message ?? "unknown failure"}`,
      { cause: this.lastFailure },
    );
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

  public recordSuccess(): void {
    this.failuresAt = [];
    this.lastFailure = undefined;
  }

  public recordFailure(error: Error): void {
    this.failuresAt.push(this.now());
    this.lastFailure = error;
  }

  public health(): NativeOwnerRestartHealth {
    const exhausted = this.exhaustion() !== undefined;
    return {
      restartsInWindow: this.restartsAt.length,
      failedRestartsInWindow: this.failuresAt.length,
      restartLimit: this.options.restartLimit,
      restartWindowMs: this.options.restartWindowMs,
      exhausted,
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
