import * as SDK from "@al-ft/midgard-sdk";
import {
  type CML,
  type LucidEvolution,
  type TxBuilder,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import type { OutRefLike } from "@/tx-context.js";
import { formatCauseSummary } from "@/transactions/reserve-payout/diagnostics.js";
import { ReservePayoutTxError } from "@/transactions/reserve-payout/errors.js";
import {
  disposableFeeInputCandidates,
  fetchProviderVisibleWalletInputsProgram,
} from "@/transactions/reserve-payout/inputs.js";

export type BuiltReservePayoutTx<L> = {
  readonly tx: TxSignBuilder;
  readonly layout: L;
};

type CompleteWithLayoutParams<L> = {
  readonly label: string;
  readonly lucid: LucidEvolution;
  readonly initialLayout: L;
  readonly walletInputExclusions?: readonly OutRefLike[];
  readonly makeTx: (layout: L) => TxBuilder;
  readonly deriveLayout: (tx: CML.Transaction) => L;
  readonly sameLayout: (left: L, right: L) => boolean;
};

export const completeWithTwoPassLayoutProgram = <L>({
  label,
  lucid,
  initialLayout,
  walletInputExclusions = [],
  makeTx,
  deriveLayout,
  sameLayout,
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
            message: `Failed to fetch wallet inputs for ${label} transaction completion: ${formatCauseSummary(cause)}`,
            cause,
          }),
      ),
      Effect.map((utxos) =>
        disposableFeeInputCandidates(utxos, walletInputExclusions),
      ),
    );
    const draft = yield* Effect.tryPromise({
      try: () =>
        SDK.withStubbedProviderEvaluation(lucid, () =>
          makeTx(initialLayout).complete({
            localUPLCEval: true,
            presetWalletInputs: [...walletInputs],
          }),
        ),
      catch: (cause) =>
        new ReservePayoutTxError({
          message: `Failed to build ${label} draft transaction with disposable wallet fee inputs: ${formatCauseSummary(cause)}`,
          cause,
        }),
    });
    const resolvedLayout = yield* Effect.try({
      try: () => deriveLayout(draft.toTransaction()),
      catch: (cause) =>
        new ReservePayoutTxError({
          message: `Failed to derive ${label} layout from balanced draft transaction`,
          cause,
        }),
    });

    const final = yield* Effect.tryPromise({
      try: () =>
        makeTx(resolvedLayout).complete({
          localUPLCEval: true,
          presetWalletInputs: [...walletInputs],
        }),
      catch: (cause) =>
        new ReservePayoutTxError({
          message: `Failed to build final ${label} transaction with real local UPLC evaluation and disposable wallet fee inputs: ${formatCauseSummary(cause)}`,
          cause,
        }),
    });

    const finalLayout = yield* Effect.try({
      try: () => deriveLayout(final.toTransaction()),
      catch: (cause) =>
        new ReservePayoutTxError({
          message: `Failed to derive ${label} layout from final transaction`,
          cause,
        }),
    });

    if (!sameLayout(resolvedLayout, finalLayout)) {
      return yield* Effect.fail(
        new ReservePayoutTxError({
          message: `${label} transaction layout was unstable`,
          cause: {
            initialLayout,
            resolvedLayout,
            finalLayout,
          },
        }),
      );
    }

    return {
      tx: final,
      layout: finalLayout,
    };
  });
