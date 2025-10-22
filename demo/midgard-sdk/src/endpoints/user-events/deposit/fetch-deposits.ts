import { LucidEvolution, PolicyId, UTxO } from "@lucid-evolution/lucid";
import { Deposit } from "@/tx-builder/index.js";
import { Effect } from "effect";
import {
  utxosAtByNFTPolicyId,
  LucidError,
  DataCoercionError,
  AssetError,
  UnauthenticUtxoError,
} from "@/utils/common.js";
import { utxosToDepositUTxOs } from "@/utils/user-events/deposit.js";
import { POSIXTime } from "@/tx-builder/common.js";

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

export const fetchDepositUTxOsProgram = (
  lucid: LucidEvolution,
  config: Deposit.FetchConfig,
): Effect.Effect<
  Deposit.DepositUTxO[],
  LucidError
> =>
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

    const validDepositUTxOs = depositUTxOs.filter((utxo) =>
      isUTxOTimeValid(utxo, config.inclusionStartTime, config.inclusionEndTime),
    );
    return validDepositUTxOs;
  });
