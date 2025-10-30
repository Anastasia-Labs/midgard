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

const isUTxOTimeValid = (
  depositUTxO: Deposit.DepositUTxO,
  inclusionStartTime: POSIXTime,
  inclusionEndTime: POSIXTime,
): boolean => {
  const depositData = depositUTxO.datum;
  return (
    inclusionStartTime < depositData.inclusionTime &&
    depositData.inclusionTime <= inclusionEndTime
  );
};

export const fetchAllDepositUTxOsProgram = (
  lucid: LucidEvolution,
  config: Deposit.FetchConfig,
): Effect.Effect<Deposit.DepositUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.depositAddress,
      config.depositPolicyId,
    );
    const depositUTxOs = yield* utxosToDepositUTxOs(
      allUTxOs,
      config.depositPolicyId,
    );

    return depositUTxOs;
  });

export const fetchDepositUTxOsProgram = (
  lucid: LucidEvolution,
  config: Deposit.FetchConfig,
  inclusionStartTime: POSIXTime,
  inclusionEndTime: POSIXTime,
): Effect.Effect<Deposit.DepositUTxO[], LucidError> =>
  Effect.gen(function* () {
    const depositUTxOs = yield* fetchAllDepositUTxOsProgram(lucid, config)
    const validDepositUTxOs = depositUTxOs.filter((utxo) =>
      isUTxOTimeValid(utxo, inclusionStartTime, inclusionEndTime),
    );
    return validDepositUTxOs;
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
