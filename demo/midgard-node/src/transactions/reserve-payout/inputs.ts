import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { compareOutRefs, outRefLabel, type OutRefLike } from "@/tx-context.js";
import {
  isPlainPureAdaUtxo,
  isPureAdaUtxo,
} from "@/transactions/reserve-payout/assets.js";
import {
  fail,
  ReservePayoutTxError,
} from "@/transactions/reserve-payout/errors.js";

type ProviderLedgerEntry = {
  readonly utxo?: UTxO;
  readonly spent?: boolean;
};

type ProviderWithVisibleLedger = {
  readonly ledger?: Record<string, ProviderLedgerEntry | undefined>;
  readonly mempool?: Record<string, ProviderLedgerEntry | undefined>;
};

type FeeInputRejection = {
  readonly message: string;
  readonly cause: unknown;
};

const feeInputRejection = (utxo: UTxO): FeeInputRejection | undefined => {
  if (utxo.scriptRef !== undefined) {
    return {
      message:
        "Explicit fee input for reserve/payout transaction must not carry a reference script",
      cause: {
        feeInput: outRefLabel(utxo),
      },
    };
  }
  if (utxo.datum !== undefined) {
    return {
      message:
        "Explicit fee input for reserve/payout transaction must not carry an inline datum",
      cause: {
        feeInput: outRefLabel(utxo),
      },
    };
  }
  if (utxo.datumHash !== undefined) {
    return {
      message:
        "Explicit fee input for reserve/payout transaction must not carry a datum hash",
      cause: {
        feeInput: outRefLabel(utxo),
      },
    };
  }
  if (!isPureAdaUtxo(utxo)) {
    return {
      message:
        "Explicit fee input for reserve/payout transaction must be pure ADA",
      cause: {
        feeInput: outRefLabel(utxo),
        assets: utxo.assets,
      },
    };
  }
  if ((utxo.assets.lovelace ?? 0n) <= 0n) {
    return {
      message: "Explicit fee input for reserve/payout transaction has no ADA",
      cause: {
        feeInput: outRefLabel(utxo),
        assets: utxo.assets,
      },
    };
  }
  return undefined;
};

export const isDisposableFeeInputUtxo = (utxo: UTxO): boolean =>
  feeInputRejection(utxo) === undefined && isPlainPureAdaUtxo(utxo);

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

export const isProviderSpendableUtxo = (
  lucid: LucidEvolution,
  utxo: UTxO,
): boolean => {
  const provider = lucid.config().provider as ProviderWithVisibleLedger;
  const outRefKey = `${utxo.txHash}${utxo.outputIndex.toString()}`;
  const hasVisibleProviderState =
    provider.ledger !== undefined || provider.mempool !== undefined;
  const entry = provider.ledger?.[outRefKey] ?? provider.mempool?.[outRefKey];
  if (entry === undefined) {
    return !hasVisibleProviderState;
  }
  return entry.spent !== true;
};

export const fetchProviderVisibleWalletInputsProgram = (
  lucid: LucidEvolution,
): Effect.Effect<readonly UTxO[], SDK.LucidError> =>
  Effect.gen(function* () {
    const walletAddress = yield* fetchWalletAddressProgram(lucid);
    const provider = lucid.config().provider as ProviderWithVisibleLedger;
    const visibleProviderEntries = [
      ...Object.values(provider.ledger ?? {}),
      ...Object.values(provider.mempool ?? {}),
    ];
    if (visibleProviderEntries.length > 0) {
      return visibleProviderEntries.flatMap((entry) => {
        if (
          entry === undefined ||
          entry.spent === true ||
          entry.utxo === undefined ||
          entry.utxo.address !== walletAddress
        ) {
          return [];
        }
        return [entry.utxo];
      });
    }
    const walletUtxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(walletAddress),
      catch: (cause) =>
        new SDK.LucidError({
          message:
            "Failed to fetch provider-visible wallet UTxOs for reserve/payout transaction",
          cause,
        }),
    });
    return walletUtxos.filter((utxo) => isProviderSpendableUtxo(lucid, utxo));
  });

export const selectFeeInputProgram = (
  lucid: LucidEvolution,
  explicitFeeInput: UTxO | undefined,
  excluded: readonly OutRefLike[],
): Effect.Effect<UTxO, ReservePayoutTxError | SDK.LucidError> =>
  Effect.gen(function* () {
    const excludedKeys = new Set(excluded.map(outRefLabel));
    if (explicitFeeInput !== undefined) {
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
    const walletUtxos = yield* fetchProviderVisibleWalletInputsProgram(lucid);
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
