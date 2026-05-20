import type { TxBuilder, TxSignBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const completeTxWithLocalUPLCEvalProgram = <E>(
  tx: Pick<TxBuilder, "complete">,
  catchError: (error: unknown) => E,
): Effect.Effect<TxSignBuilder, E> =>
  Effect.tryPromise({
    try: () => tx.complete({ localUPLCEval: true }),
    catch: catchError,
  });
