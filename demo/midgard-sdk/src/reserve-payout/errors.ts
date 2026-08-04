import { Data as EffectData, Effect } from "effect";

export class ReservePayoutTxError extends EffectData.TaggedError(
  "ReservePayoutTxError",
)<{
  message: string;
  cause: unknown;
}> {}

export const fail = (
  message: string,
  cause: unknown,
): Effect.Effect<never, ReservePayoutTxError> =>
  Effect.fail(new ReservePayoutTxError({ message, cause }));
