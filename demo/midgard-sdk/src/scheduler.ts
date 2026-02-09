import {
  Address,
  Data,
  LucidEvolution,
  PolicyId,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Data as EffectData } from "effect";
import {
  BaseEntityUTxO,
  GenericErrorFields,
  LucidError,
  POSIXTimeSchema,
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

export type SchedulerUTxO = BaseEntityUTxO<SchedulerDatum>;

export type SchedulerConfig = {
  schedulerAddress: Address;
  schedulerPolicyId: PolicyId;
};

export const schedulerAssetName = "";

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
