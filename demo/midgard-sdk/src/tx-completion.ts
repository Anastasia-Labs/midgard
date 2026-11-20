import type { TxBuilder, TxSignBuilder, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type TxCompleteOptions = NonNullable<
  Parameters<TxBuilder["complete"]>[0]
>;

export const completeOptionsWithLocalEval = ({
  presetWalletInputs,
  coinSelection,
}: {
  readonly presetWalletInputs?: readonly UTxO[];
  readonly coinSelection?: boolean;
} = {}): TxCompleteOptions => ({
  localUPLCEval: true,
  ...(coinSelection === undefined ? {} : { coinSelection }),
  ...(presetWalletInputs === undefined
    ? {}
    : { presetWalletInputs: [...presetWalletInputs] }),
});

export const completeTxWithLocalUPLCEvalProgram = <E>(
  tx: Pick<TxBuilder, "complete">,
  catchError: (error: unknown) => E,
): Effect.Effect<TxSignBuilder, E> =>
  Effect.tryPromise({
    try: () => tx.complete(completeOptionsWithLocalEval()),
    catch: catchError,
  });
