import {
  compareOutRefs,
  outRefLabel,
  type OutRefLike,
} from "@al-ft/midgard-core/out-ref";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  EMPTY_WALLET_INPUTS_CAUSE,
  emptyWalletInputsRefusal,
} from "../tx-completion.js";
import { isPureAdaUtxo } from "./assets.js";
import { fail, ReservePayoutTxError } from "./errors.js";
import * as SDK from "./primitives.js";

type FeeInputRejection = {
  readonly message: string;
  readonly cause: unknown;
};

const feeInputRejection = (utxo: UTxO): FeeInputRejection | undefined => {
  const feeInput = outRefLabel(utxo);
  if (utxo.scriptRef !== undefined) {
    return {
      message:
        "Explicit fee input for reserve/payout transaction must not carry a reference script",
      cause: { feeInput },
    };
  }
  if (utxo.datum !== undefined) {
    return {
      message:
        "Explicit fee input for reserve/payout transaction must not carry an inline datum",
      cause: { feeInput },
    };
  }
  if (utxo.datumHash !== undefined) {
    return {
      message:
        "Explicit fee input for reserve/payout transaction must not carry a datum hash",
      cause: { feeInput },
    };
  }
  if (!isPureAdaUtxo(utxo)) {
    return {
      message:
        "Explicit fee input for reserve/payout transaction must be pure ADA",
      cause: { feeInput, assets: utxo.assets },
    };
  }
  if ((utxo.assets.lovelace ?? 0n) <= 0n) {
    return {
      message: "Explicit fee input for reserve/payout transaction has no ADA",
      cause: { feeInput, assets: utxo.assets },
    };
  }
  return undefined;
};

export const isDisposableFeeInputUtxo = (utxo: UTxO): boolean =>
  feeInputRejection(utxo) === undefined;

export const disposableFeeInputCandidates = (
  utxos: readonly UTxO[],
  excluded: readonly OutRefLike[],
): readonly UTxO[] => {
  const excludedKeys = new Set(excluded.map(outRefLabel));
  return utxos
    .filter((utxo) => !excludedKeys.has(outRefLabel(utxo)))
    .filter(isDisposableFeeInputUtxo)
    .sort((left, right) => {
      const leftLovelace = left.assets.lovelace ?? 0n;
      const rightLovelace = right.assets.lovelace ?? 0n;
      if (leftLovelace === rightLovelace) {
        return compareOutRefs(left, right);
      }
      return leftLovelace > rightLovelace ? -1 : 1;
    });
};

const fetchWalletAddressProgram = (
  lucid: LucidEvolution,
): Effect.Effect<string, SDK.LucidError> =>
  Effect.tryPromise({
    try: () => lucid.wallet().address(),
    catch: (cause) =>
      new SDK.LucidError({
        message:
          "Failed to fetch wallet address for reserve/payout transaction",
        cause,
      }),
  });

export const fetchProviderVisibleWalletInputsProgram = (
  lucid: LucidEvolution,
): Effect.Effect<readonly UTxO[], SDK.LucidError> =>
  Effect.gen(function* () {
    const walletAddress = yield* fetchWalletAddressProgram(lucid);
    return yield* Effect.tryPromise({
      try: () => lucid.utxosAt(walletAddress),
      catch: (cause) =>
        new SDK.LucidError({
          message:
            "Failed to fetch provider-visible wallet UTxOs for reserve/payout transaction",
          cause,
        }),
    });
  });

/**
 * The wallet's coins for a reserve/payout build: exactly `walletInputs` when
 * the caller hands them (its view of the wallet), otherwise the coins the
 * provider shows at the selected wallet's address.
 */
export const fetchWalletInputsProgram = (
  lucid: LucidEvolution,
  walletInputs: readonly UTxO[] | undefined,
): Effect.Effect<readonly UTxO[], SDK.LucidError> =>
  walletInputs === undefined
    ? fetchProviderVisibleWalletInputsProgram(lucid)
    : Effect.succeed(walletInputs);

/**
 * Selects the transaction's disposable fee input. With `walletInputs` the
 * input is chosen from exactly those (an empty set is refused by name, and an
 * explicit fee input must be one of them); without them, from the provider's
 * view of the selected wallet.
 */
export const selectFeeInputProgram = (
  lucid: LucidEvolution,
  explicitFeeInput: UTxO | undefined,
  excluded: readonly OutRefLike[],
  walletInputs?: readonly UTxO[],
): Effect.Effect<UTxO, ReservePayoutTxError | SDK.LucidError> =>
  Effect.gen(function* () {
    const emptyInputs = emptyWalletInputsRefusal(
      walletInputs,
      "the reserve/payout transaction",
    );
    if (emptyInputs !== null) {
      return yield* fail(emptyInputs, EMPTY_WALLET_INPUTS_CAUSE);
    }
    const excludedKeys = new Set(excluded.map(outRefLabel));
    if (explicitFeeInput !== undefined) {
      if (
        walletInputs !== undefined &&
        !walletInputs.some(
          (utxo) => outRefLabel(utxo) === outRefLabel(explicitFeeInput),
        )
      ) {
        return yield* fail(
          "Explicit fee input for reserve/payout transaction is not among the wallet inputs",
          { feeInput: outRefLabel(explicitFeeInput) },
        );
      }
      if (excludedKeys.has(outRefLabel(explicitFeeInput))) {
        return yield* fail(
          "Explicit fee input overlaps a protected reserve/payout transaction input",
          {
            feeInput: outRefLabel(explicitFeeInput),
          },
        );
      }
      const rejection = feeInputRejection(explicitFeeInput);
      if (rejection !== undefined) {
        return yield* fail(rejection.message, rejection.cause);
      }
      const walletAddress = yield* fetchWalletAddressProgram(lucid);
      if (explicitFeeInput.address !== walletAddress) {
        return yield* fail(
          "Explicit fee input for reserve/payout transaction must belong to the selected wallet",
          {
            feeInput: outRefLabel(explicitFeeInput),
            feeInputAddress: explicitFeeInput.address,
            walletAddress,
          },
        );
      }
      return explicitFeeInput;
    }
    const walletUtxos = yield* fetchWalletInputsProgram(lucid, walletInputs);
    const candidates = disposableFeeInputCandidates(walletUtxos, excluded);
    const selected = candidates[0];
    if (selected === undefined) {
      return yield* fail(
        "Failed to select fee input for reserve/payout transaction",
        "wallet has no disposable pure-ADA UTxO outside the protocol input set",
      );
    }
    return selected;
  });
