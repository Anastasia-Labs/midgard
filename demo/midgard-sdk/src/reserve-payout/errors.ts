import { Data as EffectData, Effect } from "effect";

export class ReservePayoutTxError extends EffectData.TaggedError(
  "ReservePayoutTxError",
)<{
  message: string;
  cause: unknown;
}> {}

/** A history retirement cannot start before the later of its predecessor's and
 * Order's protection bounds; a caller may wait until `protectedUntilMs`. The
 * list's `protectionDurationMs` bounds how far ahead an honest bound can be. */
export class HistoryRetirementProtectedError extends Error {
  constructor(
    readonly protectedUntilMs: bigint,
    readonly nowMs: number,
    readonly protectionDurationMs: bigint,
  ) {
    super(
      `History predecessor or Order is still protected until ${protectedUntilMs.toString()} (now ${nowMs.toString()})`,
    );
    this.name = "HistoryRetirementProtectedError";
  }
}

export const fail = (
  message: string,
  cause: unknown,
): Effect.Effect<never, ReservePayoutTxError> =>
  Effect.fail(new ReservePayoutTxError({ message, cause }));
