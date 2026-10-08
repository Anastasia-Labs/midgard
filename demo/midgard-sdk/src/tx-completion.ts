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

/**
 * The refusal for explicit wallet inputs that hold nothing, or `null` when
 * there is something to spend.
 *
 * A builder handed `walletInputs` must select coins from exactly those inputs.
 * Lucid reads the wallet from the provider whenever its preset inputs are
 * empty, so an empty explicit set is refused by name before Lucid runs: the
 * caller's view has nothing to offer, and the provider is not a fallback.
 */
export const emptyWalletInputsRefusal = (
  walletInputs: readonly UTxO[] | undefined,
  transactionLabel: string,
): string | null =>
  walletInputs !== undefined && walletInputs.length === 0
    ? `No wallet inputs available to fund ${transactionLabel}`
    : null;

/** The cause every builder attaches to an empty explicit wallet-input set. */
export const EMPTY_WALLET_INPUTS_CAUSE = "explicit wallet inputs are empty";

/**
 * Completes with local evaluation. With `walletInputs`, Lucid selects coins
 * and collateral from exactly those inputs and never reads the provider;
 * without them it reads the wallet as before. The caller refuses an empty
 * explicit set first (`emptyWalletInputsRefusal`).
 */
export const completeTxWithLocalUPLCEvalProgram = <E>(
  tx: Pick<TxBuilder, "complete">,
  catchError: (error: unknown) => E,
  walletInputs?: readonly UTxO[],
): Effect.Effect<TxSignBuilder, E> =>
  Effect.tryPromise({
    try: () =>
      tx.complete(
        completeOptionsWithLocalEval({ presetWalletInputs: walletInputs }),
      ),
    catch: catchError,
  });
