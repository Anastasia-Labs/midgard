import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type { OutRefLike } from "@al-ft/midgard-core/out-ref";
import {
  type LucidEvolution,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { EMPTY_WALLET_INPUTS_CAUSE } from "../tx-completion.js";
import { ReservePayoutTxError } from "./errors.js";
import {
  disposableFeeInputCandidates,
  fetchProviderVisibleWalletInputsProgram,
} from "./inputs.js";

export type BuiltReservePayoutTx<L> = {
  readonly tx: TxSignBuilder;
  readonly layout: L;
};

type CompleteWithLayoutParams<L> = {
  readonly label: string;
  readonly lucid: LucidEvolution;
  readonly walletInputExclusions?: readonly OutRefLike[];
  /**
   * The wallet's coins as the caller's view holds them. When given, Lucid
   * selects coins and collateral from exactly these and never reads the
   * provider.
   */
  readonly walletInputs?: readonly UTxO[];
  readonly makeTx: () => TxBuilder;
  readonly resolveLayout: () => L;
};

/**
 * The preset inputs for a build handed explicit wallet inputs: the disposable
 * coins outside the protocol and fee inputs. When the fee input is the
 * wallet's only disposable coin, the preset is the fee input itself: Lucid
 * never collects an input the transaction already spends, so it serves as
 * collateral only. An empty preset would make Lucid read the provider, so
 * a wallet with no disposable coin at all is refused by name.
 */
const explicitPresetInputs = (
  label: string,
  walletInputs: readonly UTxO[],
  walletInputExclusions: readonly OutRefLike[],
): Effect.Effect<readonly UTxO[], ReservePayoutTxError> => {
  const outside = disposableFeeInputCandidates(
    walletInputs,
    walletInputExclusions,
  );
  const preset =
    outside.length > 0
      ? outside
      : disposableFeeInputCandidates(walletInputs, []);
  return preset.length > 0
    ? Effect.succeed(preset)
    : Effect.fail(
        new ReservePayoutTxError({
          message: `No disposable wallet input available to complete the ${label} transaction`,
          cause: EMPTY_WALLET_INPUTS_CAUSE,
        }),
      );
};

export const completeWithFinalLayoutProgram = <L>({
  label,
  lucid,
  walletInputExclusions = [],
  walletInputs: explicitWalletInputs,
  makeTx,
  resolveLayout,
}: CompleteWithLayoutParams<L>): Effect.Effect<
  BuiltReservePayoutTx<L>,
  ReservePayoutTxError
> =>
  Effect.gen(function* () {
    const walletInputs =
      explicitWalletInputs === undefined
        ? yield* fetchProviderVisibleWalletInputsProgram(lucid).pipe(
            Effect.mapError(
              (cause) =>
                new ReservePayoutTxError({
                  message: `Failed to fetch wallet inputs for ${label} transaction completion: ${formatUnknownError(cause)}`,
                  cause,
                }),
            ),
            Effect.map((utxos) =>
              disposableFeeInputCandidates(utxos, walletInputExclusions),
            ),
          )
        : yield* explicitPresetInputs(
            label,
            explicitWalletInputs,
            walletInputExclusions,
          );
    const final = yield* Effect.tryPromise({
      try: () =>
        makeTx().complete({
          localUPLCEval: true,
          presetWalletInputs: [...walletInputs],
        }),
      catch: (cause) =>
        new ReservePayoutTxError({
          message: `Failed to build final ${label} transaction with real local UPLC evaluation and disposable wallet fee inputs: ${formatUnknownError(cause)}`,
          cause,
        }),
    });

    const layout = yield* Effect.try({
      try: resolveLayout,
      catch: (cause) =>
        new ReservePayoutTxError({
          message: `Failed to resolve ${label} layout from BuildTxWithRedeemer`,
          cause,
        }),
    });

    return {
      tx: final,
      layout,
    };
  });
