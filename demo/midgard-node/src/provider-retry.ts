import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { isTransientOgmiosJsonRpcFailure } from "@al-ft/midgard-core/ogmios-json-rpc-error";
import { L1SubmitOutcomeUnknownError } from "@al-ft/midgard-l1-follower/provider";
import { Cause, Duration, Effect, Runtime } from "effect";

export type ProviderRetryOptions = {
  readonly maxAttempts: number;
  readonly baseDelayMs: number;
  readonly maxDelayMs: number;
  readonly jitterRatio?: number;
  readonly isRetryable?: (error: unknown) => boolean;
};

// Socket- and resolver-level failures of an endpoint that is restarting,
// briefly unroutable or shedding connections. Node sets these on the error
// itself or on the `cause` a fetch failure wraps.
const TRANSIENT_NODE_NET_CODES: ReadonlySet<string> = new Set([
  "ECONNREFUSED",
  "ECONNRESET",
  "ECONNABORTED",
  "ETIMEDOUT",
  "ENOTFOUND",
  "EAI_AGAIN",
  "EPIPE",
  "ENETUNREACH",
  "ENETDOWN",
  "EHOSTUNREACH",
  "EHOSTDOWN",
]);

// undici (the fetch behind every HTTP provider read) connection failures.
const TRANSIENT_UNDICI_CODES: ReadonlySet<string> = new Set([
  "UND_ERR_CONNECT_TIMEOUT",
  "UND_ERR_HEADERS_TIMEOUT",
  "UND_ERR_BODY_TIMEOUT",
  "UND_ERR_SOCKET",
  "UND_ERR_CLOSED",
]);

// postgres.js connection-state codes and the SQLSTATEs of a server that is
// starting, shutting down, in recovery or out of connection slots. A query
// the server answered with any other SQLSTATE is not a connection failure.
const TRANSIENT_POSTGRES_CODES: ReadonlySet<string> = new Set([
  "CONNECTION_CLOSED",
  "CONNECTION_DESTROYED",
  "CONNECTION_ENDED",
  "CONNECT_TIMEOUT",
  "57P01", // admin_shutdown
  "57P02", // crash_shutdown
  "57P03", // cannot_connect_now: starting up or in recovery
  "53300", // too_many_connections
  "08000", // connection_exception
  "08001", // sqlclient_unable_to_establish_sqlconnection
  "08003", // connection_does_not_exist
  "08004", // sqlserver_rejected_establishment_of_sqlconnection
  "08006", // connection_failure
]);

// undici rejects with these exact messages when a peer drops a response.
const TRANSIENT_EXACT_MESSAGES: ReadonlySet<string> = new Set([
  "terminated",
  "other side closed",
  "connect timeout",
]);

const POSTGRES_CONNECTION_MESSAGES: readonly string[] = [
  "the database system is starting up",
  "the database system is in recovery mode",
  "the database system is shutting down",
  "the database system is not yet accepting connections",
  "sorry, too many clients already",
  "remaining connection slots are reserved",
];

// @effect/sql-pg fails a pool whose first `select 1` outlives
// `connectTimeout` with this SqlError around an uncoded Error. Its timer
// starts before postgres.js's own connect_timeout, so a server that accepts
// the socket and never answers (a stalled host, a network still coming up)
// usually surfaces as this shape rather than as CONNECT_TIMEOUT.
const isPgClientPoolOpenTimeout = (link: Record<string, unknown>): boolean =>
  link._tag === "SqlError" && link.message === "PgClient: Connection timed out";

const errorRecord = (value: unknown): Record<string, unknown> | undefined =>
  typeof value === "object" && value !== null
    ? (value as Record<string, unknown>)
    : undefined;

/** The failures and defects an Effect `Cause` carries. */
const effectCauseMembers = (cause: Cause.Cause<unknown>): unknown[] => [
  ...Cause.failures(cause),
  ...Cause.defects(cause),
];

/**
 * Every error on `error`'s cause chain, including AggregateError members and
 * what an Effect `FiberFailure` or `Cause` carries: a provider read run with
 * `Effect.runPromise` rejects with a FiberFailure around its KupmiosError.
 */
const causeChain = (error: unknown): readonly Record<string, unknown>[] => {
  const seen = new Set<unknown>();
  const chain: Record<string, unknown>[] = [];
  const pending: unknown[] = [error];
  while (pending.length > 0 && chain.length < 64) {
    const next = pending.shift();
    if (Cause.isCause(next)) {
      pending.push(...effectCauseMembers(next));
      continue;
    }
    const current = errorRecord(next);
    if (current === undefined || seen.has(current)) {
      continue;
    }
    seen.add(current);
    chain.push(current);
    if (Runtime.isFiberFailure(next)) {
      pending.push(...effectCauseMembers(next[Runtime.FiberFailureCauseId]));
    }
    pending.push(current.cause);
    if (Array.isArray(current.errors)) {
      pending.push(...(current.errors as unknown[]));
    }
  }
  return chain;
};

const hasTransientConnectionShape = (
  link: Record<string, unknown>,
  codes: readonly ReadonlySet<string>[],
): boolean => {
  const code = link.code;
  if (typeof code === "string" && codes.some((set) => set.has(code))) {
    return true;
  }
  if (isPgClientPoolOpenTimeout(link)) {
    return true;
  }
  const message =
    typeof link.message === "string" ? link.message.trim().toLowerCase() : "";
  return (
    TRANSIENT_EXACT_MESSAGES.has(message) ||
    POSTGRES_CONNECTION_MESSAGES.some((known) => message.includes(known))
  );
};

/**
 * The follower provider's {@link L1SubmitOutcomeUnknownError} on `error`'s
 * cause chain, or `undefined`: a submission the node may have taken, so its
 * id must be looked up before anything is sent or built in its place. Matched
 * by name too, like `L1ProviderTransientError` above, so a second loaded copy
 * of the follower package is still recognised.
 */
export const findSubmitOutcomeUnknown = (
  error: unknown,
): L1SubmitOutcomeUnknownError | undefined =>
  causeChain(error).find(
    (link): link is Record<string, unknown> & L1SubmitOutcomeUnknownError =>
      link instanceof L1SubmitOutcomeUnknownError ||
      (link.name === "L1SubmitOutcomeUnknownError" &&
        link.outcomeUnknown === true),
  );

/** True when any error on `error`'s cause chain carries the string `code`. */
export const hasCauseCode = (error: unknown, code: string): boolean =>
  causeChain(error).some((link) => link.code === code);

/**
 * True when the database could not be reached or would not take a
 * connection: the server restarting, in recovery, out of slots, or the
 * socket dropping. A query the server answered and refused is not one.
 */
export const isConnectionClassError = (error: unknown): boolean =>
  causeChain(error).some((link) =>
    hasTransientConnectionShape(link, [
      TRANSIENT_NODE_NET_CODES,
      TRANSIENT_POSTGRES_CODES,
    ]),
  );

/**
 * True for a provider failure that a later attempt can clear: an error that
 * declares itself retryable (`KupmiosError`, the Ogmios slot-evidence and DA
 * quorum errors, the L1 ledger tip behind wall time), the follower
 * provider's `L1ProviderTransientError`, an Ogmios error answer whose code says the node cannot
 * answer now (Kupmios marks every Ogmios error answer non-retryable, because
 * Ogmios sends them all as HTTP 400), a structured transport, resolver or
 * connection code on any cause, or a known transient provider message. A
 * failure that names a logic problem (a malformed datum, a wrong network)
 * carries none of these, and an error that declares itself non-retryable
 * keeps its text out of the message fallback (a DA capability mismatch on
 * `request_timeout_ms` is not a timeout; a Kupo HTTP 400 wrapped in a
 * "Failed to fetch ..." error is not a transient read).
 */
export const isRetryableProviderError = (error: unknown): boolean => {
  const chain = causeChain(error);
  if (
    chain.some(
      (link) =>
        link.retryable === true ||
        // The follower provider's node, sidecar, store or follower outage.
        link.name === "L1ProviderTransientError" ||
        isTransientOgmiosJsonRpcFailure(link) ||
        link.name === "TimeoutError" ||
        hasTransientConnectionShape(link, [
          TRANSIENT_NODE_NET_CODES,
          TRANSIENT_UNDICI_CODES,
          TRANSIENT_POSTGRES_CODES,
        ]),
    )
  ) {
    return true;
  }
  if (chain.some((link) => link.retryable === false)) {
    return false;
  }
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
    message.includes("too many requests") ||
    POSTGRES_CONNECTION_MESSAGES.some((known) => message.includes(known))
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
