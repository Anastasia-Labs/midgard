/**
 * Shared HTTP response and error-mapping helpers for the command server.
 * Keeping response shaping here prevents route handlers from each inventing
 * their own failure formatting and status-code behavior.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { HttpServerResponse } from "@effect/platform";
import type { HttpBodyError } from "@effect/platform/HttpBody";
import { Effect } from "effect";

const MERGE_ERROR_CODE_PATTERN = /^(E_MERGE_[A-Z0-9_]+):/;

/**
 * Extracts a structured merge/state-queue error code when present.
 */
export const extractStateQueueErrorCode = (
  e: SDK.StateQueueError,
): string | undefined => {
  const cause = e.cause;
  if (
    typeof cause === "object" &&
    cause !== null &&
    "error_code" in cause &&
    typeof (cause as { error_code?: unknown }).error_code === "string"
  ) {
    return (cause as { error_code: string }).error_code;
  }
  const match = MERGE_ERROR_CODE_PATTERN.exec(e.message);
  return match?.[1];
};

/**
 * Emits a 500 JSON response for a failed route.
 */
export const failWith500 = (
  method: "GET" | "POST",
  endpoint: string,
  error: unknown,
  msgOverride?: string,
): Effect.Effect<HttpServerResponse.HttpServerResponse, HttpBodyError, never> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `${method} /${endpoint} - failure: ${formatUnknownError(error)}`,
    );
    return yield* HttpServerResponse.json(
      { error: msgOverride ?? "Something went wrong" },
      { status: 500 },
    );
  });

/**
 * Maps state-queue failures into a JSON response, preserving an extracted
 * error code when available.
 */
export const handleStateQueueGetFailure = (
  endpoint: string,
  e: SDK.StateQueueError,
) =>
  Effect.gen(function* () {
    const errorCode = extractStateQueueErrorCode(e);
    const cause = formatUnknownError(e.cause);
    yield* Effect.logInfo(
      `GET /${endpoint} - state queue failure: message=${e.message},code=${errorCode ?? "unknown"},cause=${cause}`,
    );
    return yield* HttpServerResponse.json(
      errorCode === undefined
        ? { error: e.message }
        : { error: e.message, error_code: errorCode },
      { status: 500 },
    );
  });
