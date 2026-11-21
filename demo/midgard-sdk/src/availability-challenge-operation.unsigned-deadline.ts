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
