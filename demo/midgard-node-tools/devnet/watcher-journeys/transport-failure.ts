/**
 * A read that got no answer from the service it asked: the service answered
 * 5xx, or a CLI call to it (`docker compose exec`, `docker compose ps`)
 * exited without printing one. Its message names what was asked and what
 * came back.
 */
export class JourneyTransportError extends Error {
  override readonly name = "JourneyTransportError";
}

/**
 * The error for a non-2xx HTTP answer described by `message`: a 5xx is no
 * answer to the read (the service, or a proxy in front of it, could not
 * serve it), so it is a `JourneyTransportError`; any other status is the
 * service's answer.
 */
export const httpAnswerFailure = (status: number, message: string): Error =>
  status >= 500 ? new JourneyTransportError(message) : new Error(message);

/** Socket and resolver failures that lose a request before any answer. */
const LOST_CONNECTION_CODES = new Set([
  "ECONNREFUSED",
  "ECONNRESET",
  "ECONNABORTED",
  "EPIPE",
  "ETIMEDOUT",
  "EHOSTUNREACH",
  "ENETUNREACH",
  "EAI_AGAIN",
  "UND_ERR_SOCKET",
  "UND_ERR_CLOSED",
  "UND_ERR_CONNECT_TIMEOUT",
  "UND_ERR_HEADERS_TIMEOUT",
  "UND_ERR_BODY_TIMEOUT",
]);

/** SQLite's primary result code for a database another connection locks. */
const SQLITE_BUSY = 5;

/**
 * Whether `error` is a read that got no answer, which a wait may try again
 * within its own bound: a request that timed out (`AbortSignal.timeout`), a
 * lost or refused connection (undici's `TypeError("fetch failed")` wraps one;
 * `"terminated"` is a body reset mid-read), a `JourneyTransportError` (a 5xx
 * answer, or a CLI call that exited without one), or a SQLite read refused
 * as busy (`errcode` 5, with any extended code). An `AggregateError` is one
 * only when every error in it is. Any answer that says something (a named
 * refusal, a 4xx, a malformed or foreign body, an abort) is never one.
 */
export const isTransientTransportError = (error: unknown): boolean => {
  if (typeof error !== "object" || error === null) return false;
  if (error instanceof JourneyTransportError) return true;
  if (error instanceof AggregateError)
    return (
      error.errors.length > 0 && error.errors.every(isTransientTransportError)
    );
  const { name, message, code, errcode } = error as {
    name?: unknown;
    message?: unknown;
    code?: unknown;
    errcode?: unknown;
  };
  if (name === "TimeoutError") return true;
  if (typeof code === "string" && LOST_CONNECTION_CODES.has(code)) return true;
  // SQLite's extended codes keep the primary code in their low byte.
  if (typeof errcode === "number" && (errcode & 0xff) === SQLITE_BUSY)
    return true;
  return (
    error instanceof TypeError &&
    (message === "fetch failed" || message === "terminated")
  );
};

/**
 * `read`'s answer, or `undefined` for a read that got no answer
 * (`isTransientTransportError`): inside a bounded wait that is "not yet".
 * Any other error is rethrown.
 */
export const answerOrNotYet = async <T>(
  read: () => Promise<T>,
): Promise<T | undefined> => {
  try {
    return await read();
  } catch (cause) {
    if (!isTransientTransportError(cause)) throw cause;
    return undefined;
  }
};

/**
 * Runs the read `read` again after a transport failure
 * (`isTransientTransportError`), pausing with `pause` between tries, until it
 * answers or `boundMs` has passed since the first try. Any other error is
 * rethrown at once; at the bound the last transport failure is rethrown.
 */
export const readThroughTransportFailures = async <T>(
  read: () => Promise<T>,
  { boundMs, pause }: Readonly<{ boundMs: number; pause(): Promise<unknown> }>,
): Promise<T> => {
  const deadline = performance.now() + boundMs;
  for (;;) {
    try {
      return await read();
    } catch (error) {
      if (!isTransientTransportError(error) || performance.now() >= deadline)
        throw error;
    }
    await pause();
  }
};
