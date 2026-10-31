import { setTimeout as pause } from "node:timers/promises";

export const isJourneyOperationsConnectionRefused = (
  cause: unknown,
): boolean => {
  if (!(cause instanceof Error)) return false;
  if ("code" in cause && cause.code === "ECONNREFUSED") return true;
  if (cause instanceof AggregateError)
    return (
      cause.errors.length > 0 &&
      cause.errors.every(isJourneyOperationsConnectionRefused)
    );
  return isJourneyOperationsConnectionRefused(cause.cause);
};

/** Only an authorized restart may wait for its operations listener to return. */
export const waitForRestartedJourneyOperations = async <T>({
  readStatus,
  requireLive,
  timeoutMs = 1_800_000,
}: {
  readStatus(): Promise<T>;
  requireLive(): void;
  timeoutMs?: number;
}) => {
  if (!Number.isFinite(timeoutMs) || timeoutMs <= 0)
    throw new Error("Restart readiness timeout must be finite and positive");
  const deadline = performance.now() + timeoutMs;
  for (;;) {
    requireLive();
    try {
      return await readStatus();
    } catch (cause) {
      if (!isJourneyOperationsConnectionRefused(cause)) throw cause;
      if (performance.now() >= deadline)
        throw new Error("Timed out waiting for restarted watcher operations", {
          cause,
        });
      await pause(1000);
    }
  }
};

/** A slow operations read must not discard an otherwise completed journey. */
export const readJourneyOperations = async (endpoint: string, path: string) => {
  for (let attempt = 1; ; attempt += 1) {
    const signal = AbortSignal.timeout(5000);
    try {
      const response = await fetch(`${endpoint}${path}`, { signal });
      if (!response.ok)
        throw new Error(`Operations HTTP returned ${response.status}`);
      return await response.json();
    } catch (error) {
      if (
        attempt === 3 ||
        !signal.aborted ||
        !(signal.reason instanceof Error) ||
        signal.reason.name !== "TimeoutError" ||
        !(error instanceof Error) ||
        (error.name !== "TimeoutError" && error.name !== "AbortError")
      )
        throw error;
      await pause(250);
    }
  }
};
