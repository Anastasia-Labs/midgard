import { LucidEvolution, PolicyId, UTxO } from "@lucid-evolution/lucid";
import { Deposit } from "@/tx-builder/index.js";
import { Effect } from "effect";
import { utxosAtByNFTPolicyId } from "@/utils/common.js";
import { utxosToDepositUTxOs } from "@/utils/user-events/deposit.js";

const isUTxOTimeValid = (
  depositUTxO: Deposit.DepositUTxO,
): Effect.Effect<Boolean, Error> =>
  Effect.gen(function* () {
    const { data: depositData } = depositUTxO.datum;
    const currentTime = BigInt(Date.now());
    return (
      depositData.startTime <= currentTime && currentTime <= depositData.endTime
    );
  });

export const fetchDepositUTxOsProgram = (
  lucid: LucidEvolution,
  config: Deposit.FetchConfig,
): Effect.Effect<Deposit.DepositUTxO[], Error> =>
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
    return depositUTxOs.filter(isUTxOTimeValid);
  });
