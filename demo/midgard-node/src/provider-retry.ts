import { Duration, Effect } from "effect";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";

export type ProviderRetryOptions = {
  readonly maxAttempts: number;
  readonly baseDelayMs: number;
  readonly maxDelayMs: number;
  readonly jitterRatio?: number;
  readonly isRetryable?: (error: unknown) => boolean;
};

export const isRetryableProviderError = (error: unknown): boolean => {
  const message = formatUnknownError(error, {
    includeCause: true,
  }).toLowerCase();
  return (
    message.includes("failed to fetch ") ||
    message.includes("failed to query ") ||
    message.includes("fetch failed") ||
    message.includes("status code 429") ||
    message.includes("response code 429") ||
    message.includes("status 429") ||
    message.includes("status code 500") ||
    message.includes("response code 500") ||
    message.includes("status 500") ||
    message.includes("status code 502") ||
    message.includes("response code 502") ||
    message.includes("status 502") ||
    message.includes("status code 503") ||
    message.includes("response code 503") ||
    message.includes("status 503") ||
    message.includes("status code 504") ||
    message.includes("response code 504") ||
    message.includes("status 504") ||
    message.includes("service unavailable") ||
    message.includes("temporarily unavailable") ||
    message.includes("timeout") ||
    message.includes("timed out") ||
    message.includes("socket") ||
    message.includes("econnrefused") ||
    message.includes("econnreset") ||
    message.includes("rate limit") ||
    message.includes("too many requests")
  );
};

const retryDelayMs = (
  attempt: number,
  options: ProviderRetryOptions,
): Effect.Effect<number> =>
  Effect.sync(() => {
    const baseDelayMs = Math.max(0, Math.floor(options.baseDelayMs));
    const maxDelayMs = Math.max(baseDelayMs, Math.floor(options.maxDelayMs));
    const exponentialDelayMs = Math.min(
      maxDelayMs,
      baseDelayMs * 2 ** Math.max(0, attempt - 1),
    );
    const jitterRatio = Math.max(0, Math.min(1, options.jitterRatio ?? 0.25));
    const jitterWindowMs = exponentialDelayMs * jitterRatio;
    const jitteredDelayMs =
      exponentialDelayMs - jitterWindowMs + Math.random() * jitterWindowMs * 2;
    return Math.max(0, Math.floor(jitteredDelayMs));
  });

export const runProviderStepWithRetry = <A, E, R>(
  label: string,
  step: Effect.Effect<A, E, R>,
  options: ProviderRetryOptions,
): Effect.Effect<A, E, R> =>
  Effect.gen(function* () {
    const maxAttempts = Math.max(1, Math.floor(options.maxAttempts));
    const shouldRetry = options.isRetryable ?? isRetryableProviderError;
    let lastError: E | undefined;

    for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
      const result = yield* Effect.either(step);
      if (result._tag === "Right") {
        if (attempt > 1) {
          yield* Effect.logInfo(
            `${label} succeeded after ${attempt.toString()} attempt(s).`,
          );
        }
        return result.right;
      }

      lastError = result.left;
      if (!shouldRetry(lastError)) {
        return yield* Effect.fail(lastError);
      }
      if (attempt < maxAttempts) {
        const delayMs = yield* retryDelayMs(attempt, options);
        yield* Effect.logWarning(
          `${label} failed with a retryable provider error (attempt ${attempt.toString()}/${maxAttempts.toString()}); retrying in ${delayMs.toString()}ms. cause=${formatUnknownError(lastError, { includeCause: true })}`,
        );
        if (delayMs > 0) {
          yield* Effect.sleep(Duration.millis(delayMs));
        }
      }
    }

    return yield* Effect.fail(lastError as E);
  });
