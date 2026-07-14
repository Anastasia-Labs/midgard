export class DaRequestTimeoutError extends Error {
  readonly timeoutMs: number;

  constructor(timeoutMs: number) {
    super(`DA libp2p request exceeded the ${timeoutMs.toString()}ms deadline`);
    this.name = "DaRequestTimeoutError";
    this.timeoutMs = timeoutMs;
  }
}

export const withDaRequestDeadline = async <Stream, Result>({
  timeoutMs,
  open,
  run,
  abort,
}: {
  readonly timeoutMs: number;
  readonly open: (signal: AbortSignal) => Promise<Stream>;
  readonly run: (stream: Stream, signal: AbortSignal) => Promise<Result>;
  readonly abort: (stream: Stream, error: Error) => void;
}): Promise<Result> => {
  if (!Number.isSafeInteger(timeoutMs) || timeoutMs <= 0) {
    throw new RangeError("DA request timeout must be a positive safe integer");
  }
  const controller = new AbortController();
  let stream: Stream | undefined;
  let rejectDeadline!: (error: Error) => void;
  const deadline = new Promise<never>((_resolve, reject) => {
    rejectDeadline = reject;
  });
  const timer = setTimeout(() => {
    const error = new DaRequestTimeoutError(timeoutMs);
    controller.abort(error);
    if (stream !== undefined) {
      try {
        abort(stream, error);
      } catch {
        // The deadline error remains the authoritative result.
      }
    }
    rejectDeadline(error);
  }, timeoutMs);
  timer.unref?.();
  const operation = (async () => {
    stream = await open(controller.signal);
    try {
      controller.signal.throwIfAborted();
    } catch (cause) {
      const error =
        cause instanceof Error ? cause : new DaRequestTimeoutError(timeoutMs);
      try {
        abort(stream, error);
      } catch {
        // The absolute deadline remains the authoritative result.
      }
      throw cause;
    }
    return run(stream, controller.signal);
  })();
  try {
    return await Promise.race([operation, deadline]);
  } finally {
    clearTimeout(timer);
  }
};
