import { setTimeout as pause } from "node:timers/promises";

import { httpFailureDetail } from "./http-failure.js";
import {
  httpAnswerFailure,
  isTransientTransportError,
} from "./transport-failure.js";

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

/** How long a restarted journey service may take to answer again. */
export const JOURNEY_RESTART_TIMEOUT_MS = 1_800_000;

/** Only an authorized restart may wait for its operations listener to return. */
export const waitForRestartedJourneyOperations = async <T>({
  readStatus,
  requireLive,
  timeoutMs = JOURNEY_RESTART_TIMEOUT_MS,
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

/**
 * One operations read. A read that got no answer (its own five-second signal
 * timed out, a lost connection or a 5xx: `isTransientTransportError`) is
 * tried again, up to three tries in all; an answer that names a refusal (any
 * other non-2xx, a malformed body) fails at once.
 */
export const readJourneyOperations = async (endpoint: string, path: string) => {
  for (let attempt = 1; ; attempt += 1) {
    const signal = AbortSignal.timeout(5000);
    try {
      const response = await fetch(`${endpoint}${path}`, { signal });
      // A refusing watcher names why in its body (`journal_integrity`,
      // `journal_unavailable`, `starting`).
      if (!response.ok)
        throw httpAnswerFailure(
          response.status,
          `Operations HTTP returned ${response.status}${await httpFailureDetail(response)}`,
        );
      return await response.json();
    } catch (error) {
      // A body read our own timeout cut short rejects as an `AbortError`.
      const ownTimeout =
        signal.aborted &&
        signal.reason instanceof Error &&
        signal.reason.name === "TimeoutError" &&
        error instanceof Error &&
        (error.name === "TimeoutError" || error.name === "AbortError");
      if (attempt === 3 || !(ownTimeout || isTransientTransportError(error)))
        throw error;
      await pause(250);
    }
  }
};
