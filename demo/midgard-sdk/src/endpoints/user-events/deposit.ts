import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { Deposit } from "@/tx-builder/user-events/index.js";
import { Effect } from "effect";
import {
  utxosAtByNFTPolicyId,
  LucidError,
  HashingError,
} from "@/utils/common.js";
import {
  DepositError,
  utxosToDepositUTxOs,
} from "@/utils/user-events/deposit.js";
import { POSIXTime } from "@/tx-builder/common.js";
import { makeReturn } from "@/core.js";

const isUTxOTimeInBounds = (
  depositUTxO: Deposit.DepositUTxO,
  inclusionTimeLowerBound?: POSIXTime,
  inclusionTimeUpperBound?: POSIXTime,
): boolean => {
  const depositData = depositUTxO.datum;

  const biggerThanLower = (inclusionTimeLowerBound === undefined) || (inclusionTimeLowerBound < depositData.inclusionTime);
  const smallerThanUpper = (inclusionTimeUpperBound === undefined) || (depositData.inclusionTime <= inclusionTimeUpperBound);

  return biggerThanLower && smallerThanUpper
};

export const fetchDepositUTxOsProgram = (
  lucid: LucidEvolution,
  config: Deposit.FetchConfig,
): Effect.Effect<Deposit.DepositUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.depositAddress,
      config.depositPolicyId,
    );
    let depositUTxOs = yield* utxosToDepositUTxOs(
      allUTxOs,
      config.depositPolicyId,
    );

    return depositUTxOs.filter((utxo) =>
      isUTxOTimeInBounds(utxo, config.inclusionTimeLowerBound, config.inclusionTimeUpperBound),
    );
  });

export const depositTxProgram = (
  lucid: LucidEvolution,
  depositParams: Deposit.DepositParams,
): Effect.Effect<TxSignBuilder, HashingError | LucidError | DepositError> =>
  Effect.gen(function* () {
    const commitTx = yield* Deposit.depositTxBuilder(lucid, depositParams);
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => commitTx.complete({ localUPLCEval: false }),
      catch: (e) =>
        new DepositError({
          message: `Failed to build the transaction: ${e}`,
          cause: e,
        }),
    });
    return completedTx;
  });

/**
 * Builds completed tx for submitting deposits using the provided
 * `LucidEvolution` instance and a deposit config.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param depositParams - Parameters required for commiting deposits.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const depositTx = (
  lucid: LucidEvolution,
  depositParams: Deposit.DepositParams,
): Promise<TxSignBuilder> =>
  makeReturn(depositTxProgram(lucid, depositParams)).unsafeRun();
