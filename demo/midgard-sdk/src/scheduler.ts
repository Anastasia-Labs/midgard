import { Address, Data, LucidEvolution, PolicyId, toUnit, TxBuilder, UTxO } from "@lucid-evolution/lucid";
import { Effect,  Data as EffectData } from "effect";
import {
  DataCoercionError,
  GenericErrorFields,
  getStateToken,
  LucidError,
  POSIXTimeSchema,
  UnauthenticUtxoError,
} from "@/common.js";

export type SchedulerInitParams = {};
export type SchedulerDeinitParams = {};
export type SchedulerAdvanceParams = {};
export type SchedulerRewindParams = {};

export const SchedulerDatumSchema = Data.Object({
  operator: Data.Bytes(),
  startTime: POSIXTimeSchema,
});
export type SchedulerDatum = Data.Static<typeof SchedulerDatumSchema>;
export const SchedulerDatum = SchedulerDatumSchema as unknown as SchedulerDatum;

export type SchedulerUTxO = {
  utxo: UTxO;
  datum: SchedulerDatum;
};

export type SchedulerConfig = {
  schedulerAddress: Address;
  schedulerPolicyId: PolicyId;
};

export const schedulerAssetName = "";

export const getSchedulerDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<SchedulerDatum, DataCoercionError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const schedulerDatum = Data.from(datumCBOR, SchedulerDatum);
      return Effect.succeed(schedulerDatum);
    } catch (e) {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to a scheduler datum`,
          cause: e,
        }),
      );
    }
  } else {
    return Effect.fail(
      new DataCoercionError({
        message: `Scheduler datum coercion failed`,
        cause: `No datum found`,
      }),
    );
  }
};

/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToSchedulerUTxO = (
  utxo: UTxO,
  nftPolicy: string,
): Effect.Effect<SchedulerUTxO, DataCoercionError | UnauthenticUtxoError> =>
  Effect.gen(function* () {
    const datum = yield* getSchedulerDatumFromUTxO(utxo);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: "Failed to convert UTxO to `SchedulerUTxO`",
          cause: "UTxO's NFT policy ID is not the same as the scheduler's",
        }),
      );
    }
    return {
      utxo,
      datum,
      assetName,
    };
  });

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToSchedulerUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<SchedulerUTxO[]> => {
  const effects = utxos.map((u) => utxoToSchedulerUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};

export class SchedulerError extends EffectData.TaggedError(
  "SchedulerError",
)<GenericErrorFields> {}

export const fetchSchedulerUTxOProgram = (
  lucid: LucidEvolution,
  config: SchedulerConfig,
): Effect.Effect<
  { utxo: UTxO; datum: SchedulerDatum },
  SchedulerError | LucidError
> =>
  Effect.gen(function* () {
    const errorMessage = "Failed to fetch the scheduler UTxO";
    const SchedulerUTxOs: UTxO[] = yield* Effect.tryPromise({
      try: async () => {
        return await lucid.utxosAtWithUnit(
          config.schedulerAddress,
          toUnit(config.schedulerPolicyId, schedulerAssetName),
        );
      },
      catch: (e) =>
        new LucidError({
          message: errorMessage,
          cause: e,
        }),
    });
    if (SchedulerUTxOs.length === 1) {
      const utxo = SchedulerUTxOs[0];
      const datum = yield* Effect.try({
        try: () => {
          if (utxo.datum) {
            const coerced = Data.from(utxo.datum, SchedulerDatum);
            return coerced;
          } else {
            throw new SchedulerError({
              message: errorMessage,
              cause: "Scheduler UTxO datum is missing",
            });
          }
        },
        catch: (e) => {
          return new SchedulerError({
            message: errorMessage,
            cause: `Failed to parse the scheduler datum: ${e}`,
          });
        },
      });
      return { utxo, datum };
    } else {
      return yield* Effect.fail(
        new SchedulerError({
          message: errorMessage,
          cause:
            "Exactly one scheduler UTxO was expected, but none or more were found",
        }),
      );
    }
  });

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteSchedulerInitTxProgram = (
  lucid: LucidEvolution,
  params: SchedulerInitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Deinit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteSchedulerDeinitTxProgram = (
  lucid: LucidEvolution,
  params: SchedulerDeinitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Advance
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteSchedulerAdvanceTxProgram = (
  lucid: LucidEvolution,
  params: SchedulerAdvanceParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Rewind
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteSchedulerRewindTxProgram = (
  lucid: LucidEvolution,
  params: SchedulerRewindParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
