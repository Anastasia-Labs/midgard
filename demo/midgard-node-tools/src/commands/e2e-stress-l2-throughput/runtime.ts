export const timestampForPath = (date = new Date()): string =>
  date
    .toISOString()
    .replaceAll(/[-:]/g, "")
    .replace(/\.\d{3}Z$/, "Z");

export const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

export class StressInterruptedError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "StressInterruptedError";
  }
}

export const abortReason = (signal: AbortSignal | undefined): string => {
  const reason = signal?.reason as unknown;
  if (reason instanceof Error) {
    return reason.message;
  }
  if (typeof reason === "string" && reason.length > 0) {
    return reason;
  }
  return "stress run interrupted";
};

export const isAbortLikeError = (error: unknown): boolean =>
  error instanceof StressInterruptedError ||
  (error instanceof Error &&
    (error.name === "AbortError" || error.name === "TimeoutError"));

export const throwIfAborted = (signal: AbortSignal | undefined): void => {
  if (signal?.aborted === true) {
    throw new StressInterruptedError(abortReason(signal));
  }
};

export const signalWasAborted = (signal: AbortSignal | undefined): boolean =>
  signal?.aborted === true;

export const sleepWithAbort = async (
  sleepImpl: (ms: number) => Promise<void>,
  ms: number,
  signal: AbortSignal | undefined,
): Promise<void> => {
  if (ms <= 0) {
    throwIfAborted(signal);
    return;
  }
  if (signal === undefined) {
    await sleepImpl(ms);
    return;
  }
  throwIfAborted(signal);
  let onAbort: (() => void) | undefined;
  try {
    await Promise.race([
      sleepImpl(ms),
      new Promise<never>((_resolve, reject) => {
        onAbort = (): void => {
          reject(new StressInterruptedError(abortReason(signal)));
        };
        signal.addEventListener("abort", onAbort, { once: true });
      }),
    ]);
  } finally {
    if (onAbort !== undefined) {
      signal.removeEventListener("abort", onAbort);
    }
  }
  throwIfAborted(signal);
};
