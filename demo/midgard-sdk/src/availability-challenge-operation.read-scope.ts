/** A bounded read attempt is not a bound on successful chain recovery. */
export class DaAvailabilityReadScopeExpiredError extends Error {
  constructor(readonly deadlineEpochMs?: number) {
    super(
      deadlineEpochMs === undefined
        ? "Availability read attempt expired"
        : `Availability read deadline ${deadlineEpochMs} reached`,
    );
    this.name = "DaAvailabilityReadScopeExpiredError";
  }
}

export type DaAvailabilityReadScope = Readonly<{
  deadlineEpochMs?: number;
  expiresMonotonicMs: number;
  signal: AbortSignal;
  remainingMs: () => number;
  assertCurrent: () => void;
  /** Only reads/unsigned isolated work. Never race signing, submission, or a
   * mutation of a provider shared with a subsequent attempt. Transports must
   * honor the supplied signal; noncooperative results are fenced on return. */
  read: <T>(
    run: (signal: AbortSignal) => Promise<T>,
    options?: Readonly<{ timeoutMs?: number }>,
  ) => Promise<T>;
  close: () => void;
}>;

/** One scope is passed through nested reads/retries. Monotonic expiry prevents
 * a wall-clock rollback extending the configured attempt or protocol remainder. */
export const createDaAvailabilityReadScope = (
  input: Readonly<{
    deadlineEpochMs?: number;
    attemptTimeoutMs: number;
    signal?: AbortSignal;
    nowMs?: () => number;
    monotonicMs?: () => number;
  }>,
): DaAvailabilityReadScope => {
  if (
    !Number.isSafeInteger(input.attemptTimeoutMs) ||
    input.attemptTimeoutMs <= 0 ||
    (input.deadlineEpochMs !== undefined &&
      (!Number.isSafeInteger(input.deadlineEpochMs) ||
        input.deadlineEpochMs < 0))
  )
    throw new Error("Invalid availability read scope deadline/cap");
  const now = input.nowMs ?? Date.now;
  const monotonic = input.monotonicMs ?? (() => performance.now());
  const protocolRemaining =
    input.deadlineEpochMs === undefined
      ? Infinity
      : input.deadlineEpochMs - now();
  const expiresMonotonicMs =
    monotonic() +
    Math.min(input.attemptTimeoutMs, Math.max(0, protocolRemaining));
  const controller = new AbortController();
  let timer: ReturnType<typeof setTimeout> | undefined;
  const remainingMs = () =>
    Math.max(
      0,
      Math.min(
        expiresMonotonicMs - monotonic(),
        input.deadlineEpochMs === undefined
          ? Infinity
          : input.deadlineEpochMs - now(),
      ),
    );
  const expire = () =>
    controller.abort(
      new DaAvailabilityReadScopeExpiredError(input.deadlineEpochMs),
    );
  const assertCurrent = () => {
    if (!controller.signal.aborted && remainingMs() <= 0) expire();
    controller.signal.throwIfAborted();
  };
  const forward = () => controller.abort(input.signal?.reason);
  if (input.signal?.aborted) forward();
  else input.signal?.addEventListener("abort", forward, { once: true });
  const check = () => {
    if (controller.signal.aborted) return;
    const left = remainingMs();
    if (left <= 0) {
      expire();
      return;
    }
    timer = setTimeout(check, Math.min(left, 2_147_483_647));
  };
  check();
  return Object.freeze({
    deadlineEpochMs: input.deadlineEpochMs,
    expiresMonotonicMs,
    signal: controller.signal,
    remainingMs,
    assertCurrent,
    read: async <T>(
      run: (signal: AbortSignal) => Promise<T>,
      options?: Readonly<{ timeoutMs?: number }>,
    ): Promise<T> => {
      assertCurrent();
      if (
        options?.timeoutMs !== undefined &&
        (!Number.isSafeInteger(options.timeoutMs) || options.timeoutMs <= 0)
      )
        throw new Error("Invalid availability request cap");
      const child = new AbortController();
      const abort = () => child.abort(controller.signal.reason);
      controller.signal.addEventListener("abort", abort, { once: true });
      let requestTimer: ReturnType<typeof setTimeout> | undefined;
      let rejectAbort!: (reason: unknown) => void;
      const stopped = new Promise<never>((_resolve, reject) => {
        rejectAbort = reject;
      });
      const reject = () => rejectAbort(child.signal.reason);
      child.signal.addEventListener("abort", reject, { once: true });
      if (options?.timeoutMs !== undefined)
        requestTimer = setTimeout(
          () =>
            child.abort(
              new DaAvailabilityReadScopeExpiredError(input.deadlineEpochMs),
            ),
          Math.min(options.timeoutMs, remainingMs(), 2_147_483_647),
        );
      try {
        const result = await Promise.race([
          Promise.resolve().then(() => {
            assertCurrent();
            return run(child.signal);
          }),
          stopped,
        ]);
        child.signal.throwIfAborted();
        assertCurrent();
        return result;
      } finally {
        if (requestTimer !== undefined) clearTimeout(requestTimer);
        controller.signal.removeEventListener("abort", abort);
        child.signal.removeEventListener("abort", reject);
      }
    },
    close: () => {
      if (timer !== undefined) clearTimeout(timer);
      input.signal?.removeEventListener("abort", forward);
      controller.abort(new Error("Availability read scope closed"));
    },
  });
};
