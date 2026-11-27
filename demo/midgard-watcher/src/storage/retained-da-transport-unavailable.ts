/**
 * The watcher's own public-DA libp2p node could not start (a bind or dial
 * failure), or a lease was refused while the next start attempt is still
 * spaced out. No source was asked, so nothing is known about any payload: a
 * caller defers and asks again, it never treats this as evidence.
 */
export class WatcherRetainedDaTransportUnavailableError extends Error {
  override readonly name = "WatcherRetainedDaTransportUnavailableError";
  /** How long until the owner tries the start again; 0 when it may now. */
  readonly retryAfterMs: number;

  constructor(cause: unknown, retryAfterMs: number) {
    super(
      `public retained-DA transport is unavailable: ${
        cause instanceof Error ? cause.message : String(cause)
      }`,
      { cause },
    );
    this.retryAfterMs = retryAfterMs;
  }
}

/** How far down a `cause` chain the typed error is still recognised. */
const MAXIMUM_CAUSE_DEPTH = 8;

export const isWatcherRetainedDaTransportUnavailable = (
  error: unknown,
): error is Error => {
  let current: unknown = error;
  for (let depth = 0; depth < MAXIMUM_CAUSE_DEPTH; depth += 1) {
    if (current instanceof WatcherRetainedDaTransportUnavailableError)
      return true;
    if (!(current instanceof Error)) return false;
    current = current.cause;
  }
  return false;
};
