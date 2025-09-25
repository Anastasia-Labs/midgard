import { Data, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { Datum, DepositUTxO } from "@/tx-builder/user-events/deposit/types.js";
import { getSingleAssetApartFromAda } from "@/utils/common.js";

export const getDepositDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<Datum, Error> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, Datum);
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
): Effect.Effect<DepositUTxO, Error> =>
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
): Effect.Effect<DepositUTxO[], Error> => {
  const effects = utxos.map((u) => utxoToDepositUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};
