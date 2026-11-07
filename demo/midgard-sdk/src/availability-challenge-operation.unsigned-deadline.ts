/** An unsigned operation cannot start signing after its absolute deadline.
 * This never expires a persisted signed intent or releases its reservations. */
export class DaAvailabilityUnsignedDeadlineError extends Error {
  constructor(readonly deadlineMs: number) {
    super(`Unsigned availability operation deadline ${deadlineMs} reached`);
    this.name = "DaAvailabilityUnsignedDeadlineError";
  }
}

export const assertUnsignedAvailabilityDeadline = (
  deadlineMs: number | undefined,
  now: () => number,
): void => {
  if (deadlineMs === undefined) return;
  if (!Number.isSafeInteger(deadlineMs) || deadlineMs < 0)
    throw new Error("Invalid absolute unsigned availability deadline");
  if (now() >= deadlineMs)
    throw new DaAvailabilityUnsignedDeadlineError(deadlineMs);
};

/** One absolute deadline covers unsigned work and its final authority read.
 * The signal reaches cooperative builders; a late noncooperative result is
 * discarded, so it can never resume into signing after this promise rejects. */
export const withUnsignedAvailabilityDeadline = async <T>(
  deadlineMs: number | undefined,
  now: () => number,
  run: (signal: AbortSignal) => Promise<T>,
): Promise<T> => {
  assertUnsignedAvailabilityDeadline(deadlineMs, now);
  const controller = new AbortController();
  if (deadlineMs === undefined) return run(controller.signal);
  let timer: ReturnType<typeof setTimeout> | undefined;
  const expired = new Promise<never>((_resolve, reject) => {
    const check = () => {
      const remaining = deadlineMs - now();
      if (remaining > 0) {
        timer = setTimeout(check, Math.min(remaining, 2_147_483_647));
        return;
      }
      const error = new DaAvailabilityUnsignedDeadlineError(deadlineMs);
      controller.abort(error);
      reject(error);
    };
    check();
  });
  try {
    const result = await Promise.race([run(controller.signal), expired]);
    assertUnsignedAvailabilityDeadline(deadlineMs, now);
    return result;
  } finally {
    if (timer !== undefined) clearTimeout(timer);
  }
};
