import { Data, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { Datum, DepositUTxO } from "@/tx-builder/user-events/deposit/types.js";
import {
  AssetError,
  DataCoercionError,
  UnauthenticUtxoError,
  getStateToken,
} from "@/utils/common.js";

export const getDepositDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<Datum, DataCoercionError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const depositDatum = Data.from(datumCBOR, Datum);
      return Effect.succeed(depositDatum);
    } catch {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce to a deposit datum`,
          cause: `CBOR couldn't be coursed automaticaly`,
        }),
      );
    }
  } else {
    return Effect.fail(
      new DataCoercionError({
        message: `Could not coerce to a deposit datum`,
        cause: `No CBOR datum found`,
      }),
    );
  }
};

/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToDepositUTxO = (
  utxo: UTxO,
  nftPolicy: string,
): Effect.Effect<
  DepositUTxO,
  AssetError | DataCoercionError | UnauthenticUtxoError
> =>
  Effect.gen(function* () {
    const datum = yield* getDepositDatumFromUTxO(utxo);
    const [sym, assetName] = yield* getStateToken(
      utxo.assets,
    );
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: "Failed to convert UTxO to `DepositUTxO`",
          cause: "UTxO's NFT policy ID is not the same as the deposit's",
        }),
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
): Effect.Effect<
  DepositUTxO[],
  AssetError | DataCoercionError | UnauthenticUtxoError
> => {
  const effects = utxos.map((u) => utxoToDepositUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};
