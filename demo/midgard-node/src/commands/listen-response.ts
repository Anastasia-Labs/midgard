/**
 * Shared HTTP response and error-mapping helpers for the command server.
 * Keeping response shaping here prevents route handlers from each inventing
 * their own failure formatting and status-code behavior.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import {
  HttpBodyError,
  HttpServerResponse,
} from "@effect/platform";
import { DatabaseError } from "@/database/utils/common.js";
import {
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";

const MERGE_ERROR_CODE_PATTERN = /^(E_MERGE_[A-Z0-9_]+):/;

/**
 * Converts arbitrary error causes into a stable loggable string.
 */
const stringifyErrorCause = (cause: unknown): string => {
  if (typeof cause === "string") {
    return cause;
  }
  try {
    return JSON.stringify(cause);
  } catch {
    return `${cause}`;
  }
};

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
 * Shared internal helper for 500 responses with logging.
 */
const failWith500Helper = (
  logLabel: string,
  logMsg: string,
  error: any,
  msgOverride?: string,
) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${logLabel} - ${logMsg}: ${error}`);
    return yield* HttpServerResponse.json(
      { error: msgOverride ?? "Something went wrong" },
      { status: 500 },
    );
  });

/**
 * Emits a 500 JSON response for a failed route.
 */
export const failWith500 = (
  method: "GET" | "POST",
  endpoint: string,
  error: HttpBodyError | string | any,
  msgOverride?: string,
) => failWith500Helper(`${method} /${endpoint}`, "failure", error, msgOverride);

/**
 * Maps database lookup failures into a standardized 500 response.
 */
export const handleDBGetFailure = (endpoint: string, e: DatabaseError) =>
  failWith500("GET", endpoint, e.cause, `db failure with table ${e.table}`);

/**
 * Maps transaction-building or submission failures into a standardized 500
 * response.
 */
export const handleTxGetFailure = (
  endpoint: string,
  e: TxSignError | TxConfirmError | TxSubmitError,
) => failWith500("GET", endpoint, e.cause, `${e._tag}: ${e.message}`);

/**
 * Maps generic SDK failures into a standardized 500 response.
 */
export const handleGenericGetFailure = (
  endpoint: string,
  e: SDK.GenericErrorFields,
) => failWith500("GET", endpoint, e.cause, e.message);

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
    const cause = stringifyErrorCause(e.cause);
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
