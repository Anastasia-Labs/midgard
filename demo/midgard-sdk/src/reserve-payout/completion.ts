import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  type LucidEvolution,
  type TxBuilder,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { ReservePayoutTxError } from "@/reserve-payout/errors.js";
import {
  disposableFeeInputCandidates,
  fetchProviderVisibleWalletInputsProgram,
} from "@/reserve-payout/inputs.js";
import type { OutRefLike } from "@al-ft/midgard-core/out-ref";

export type BuiltReservePayoutTx<L> = {
  readonly tx: TxSignBuilder;
  readonly layout: L;
};

type CompleteWithLayoutParams<L> = {
  readonly label: string;
  readonly lucid: LucidEvolution;
  readonly walletInputExclusions?: readonly OutRefLike[];
  readonly makeTx: () => TxBuilder;
  readonly resolveLayout: () => L;
};

export const completeWithFinalLayoutProgram = <L>({
  label,
  lucid,
  walletInputExclusions = [],
  makeTx,
  resolveLayout,
}: CompleteWithLayoutParams<L>): Effect.Effect<
  BuiltReservePayoutTx<L>,
  ReservePayoutTxError
> =>
  Effect.gen(function* () {
    const walletInputs = yield* fetchProviderVisibleWalletInputsProgram(
      lucid,
    ).pipe(
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
