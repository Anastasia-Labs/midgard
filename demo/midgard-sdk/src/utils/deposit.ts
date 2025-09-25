import { Data, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { Deposit } from "../tx-builder/index.js";
import { Datum } from "@/tx-builder/state-queue/types.js";
import { getSingleAssetApartFromAda } from "./common.js";

export const getDepositDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<Deposit.Datum, Error> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, Deposit.Datum);
      return Effect.succeed(nodeDatum);
    } catch {
      return Effect.fail(new Error("Could not coerce to a deposit datum"));
    }
  } else {
    return Effect.fail(new Error("No datum found"));
  }
};

/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToDepositUTxO = (
  utxo: UTxO,
  nftPolicy: string,
): Effect.Effect<Deposit.DepositUTxO, Error> =>
  Effect.gen(function* () {
    const datum = yield* getDepositDatumFromUTxO(utxo);
    const [sym, assetName, _qty] = yield* getSingleAssetApartFromAda(
      utxo.assets,
    );
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new Error("UTxO's NFT policy ID is not the same as the state queue's"),
      );
    }
    return { utxo, datum, assetName };
  });

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToDepositUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<Deposit.DepositUTxO[], Error> => {
  const effects = utxos.map((u) => utxoToDepositUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};
