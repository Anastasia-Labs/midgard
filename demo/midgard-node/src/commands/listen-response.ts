/**
 * Shared HTTP response and error-mapping helpers for the command server.
 * Keeping response shaping here prevents route handlers from each inventing
 * their own failure formatting and status-code behavior.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { HttpServerResponse } from "@effect/platform";
import type { HttpBodyError } from "@effect/platform/HttpBody";
import { Effect } from "effect";

const MERGE_ERROR_CODES: ReadonlySet<string> = new Set([
  "E_MERGE_BLOCK_TX_DECODE_FAILED",
  "E_MERGE_MISSING_BLOCK_TXS",
  "E_MERGE_LAYOUT_DERIVATION_FAILED",
  "E_MERGE_REDEEMER_INDEX_MISMATCH",
  "E_MERGE_UPLC_EVAL_FAILED",
  "E_MERGE_PRODUCER_UNAVAILABLE",
]);

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
    const code = (cause as { error_code: string }).error_code;
    return MERGE_ERROR_CODES.has(code) ? code : undefined;
  }
  return [...MERGE_ERROR_CODES].find((code) =>
    e.message.startsWith(`${code}:`),
  );
};

/**
 * Emits a 500 JSON response for a failed route.
 */
export const failWith500 = (
  _method: "GET" | "POST",
  _endpoint: string,
  _error: unknown,
  _msgOverride?: string,
): Effect.Effect<HttpServerResponse.HttpServerResponse, HttpBodyError, never> =>
  Effect.gen(function* () {
    yield* Effect.logInfo({ error_code: "E_HTTP_INTERNAL" });
    return yield* HttpServerResponse.json(
      {
        error: "Request could not be completed",
        error_code: "E_HTTP_INTERNAL",
      },
      { status: 500 },
    );
  });

/**
 * Maps state-queue failures into a JSON response, preserving an extracted
 * error code when available.
 */
export const handleStateQueueGetFailure = (
  _endpoint: string,
  e: SDK.StateQueueError,
) =>
  Effect.gen(function* () {
    const errorCode = extractStateQueueErrorCode(e);
    yield* Effect.logInfo({
      error_code: errorCode ?? "E_STATE_QUEUE_UNAVAILABLE",
    });
    return yield* HttpServerResponse.json(
      errorCode === undefined
        ? {
            error: "State queue is unavailable",
            error_code: "E_STATE_QUEUE_UNAVAILABLE",
          }
        : { error: "State queue is unavailable", error_code: errorCode },
      { status: 500 },
    );
  });
