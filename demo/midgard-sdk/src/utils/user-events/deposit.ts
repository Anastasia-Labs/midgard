import { Data, UTxO } from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";
import { Datum, DepositUTxO } from "@/tx-builder/user-events/deposit.js";
import {
  DataCoercionError,
  UnauthenticUtxoError,
  getStateToken,
  GenericErrorFields,
} from "@/utils/common.js";

export class DepositError extends EffectData.TaggedError(
  "DepositError",
)<GenericErrorFields> {}

export const getDepositDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<Datum, DataCoercionError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const depositDatum = Data.from(datumCBOR, Datum);
      return Effect.succeed(depositDatum);
    } catch (e) {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to a deposit datum`,
          cause: e,
        }),
      );
    }
  } else {
    return Effect.fail(
      new DataCoercionError({
        message: `Deposit datum coercion failed`,
        cause: `No datum found`,
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
): Effect.Effect<DepositUTxO, DataCoercionError | UnauthenticUtxoError> =>
  Effect.gen(function* () {
    const datum = yield* getDepositDatumFromUTxO(utxo);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
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
): Effect.Effect<DepositUTxO[]> => {
  const effects = utxos.map((u) => utxoToDepositUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};
