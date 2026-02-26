import {
  Address,
  Assets,
  Data,
  LucidEvolution,
  PolicyId,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import {
  AuthenticatedValidator,
  authenticateUTxOs,
  AuthenticUTxO,
  GenericErrorFields,
  LucidError,
  POSIXTimeSchema,
} from "@/common.js";
import { SCHEDULER_ASSET_NAME } from "@/constants.js";
import { Effect, Data as EffectData } from "effect";

export const SchedulerDatumSchema = Data.Object({
  operator: Data.Bytes(),
  startTime: POSIXTimeSchema,
});
export type SchedulerDatum = Data.Static<typeof SchedulerDatumSchema>;
export const SchedulerDatum = SchedulerDatumSchema as unknown as SchedulerDatum;

export const SchedulerMintRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
]);

export type SchedulerMintRedeemer = Data.Static<
  typeof SchedulerMintRedeemerSchema
>;
export const SchedulerMintRedeemer =
  SchedulerMintRedeemerSchema as unknown as SchedulerMintRedeemer;

export const SchedulerSpendRedeemerSchema = Data.Enum([
  Data.Object({
    Advance: Data.Object({
      scheduler_output_index: Data.Integer(),
      active_node_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    Rewind: Data.Object({
      scheduler_output_index: Data.Integer(),
      active_node_ref_input_index: Data.Integer(),
      active_root_node_ref_input_index: Data.Integer(),
      registered_node_ref_input_index: Data.Integer(),
    }),
  }),
]);

export type SchedulerSpendRedeemer = Data.Static<
  typeof SchedulerSpendRedeemerSchema
>;
export const SchedulerSpendRedeemer =
  SchedulerSpendRedeemerSchema as unknown as SchedulerSpendRedeemer;

export type SchedulerInitParams = {
  validator: AuthenticatedValidator;
};

export type SchedulerDeinitParams = {};
export type SchedulerAdvanceParams = {};
export type SchedulerRewindParams = {};

export type SchedulerUTxO = AuthenticUTxO<SchedulerDatum>;

export const utxosToSchedulerUTxOs = (
  utxos: UTxO[],
  nftPolicy: PolicyId,
): Effect.Effect<SchedulerUTxO[], LucidError> =>
  authenticateUTxOs<SchedulerDatum>(utxos, nftPolicy, SchedulerDatum);

export type SchedulerConfig = {
  schedulerAddress: Address;
  schedulerPolicyId: PolicyId;
};

export class SchedulerError extends EffectData.TaggedError(
  "SchedulerError",
)<GenericErrorFields> {}

export const fetchSchedulerUTxOProgram = (
  lucid: LucidEvolution,
  config: SchedulerConfig,
): Effect.Effect<
  SchedulerUTxO,
  SchedulerError | LucidError
> =>
  Effect.gen(function* () {
    const errorMessage = "Failed to fetch the scheduler UTxO";
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(config.schedulerAddress),
      catch: (e) => {
        return new LucidError({
          message: `Failed to fetch the scheduler UTxO at: ${config.schedulerAddress}`,
          cause: e,
        });
      },
    });
    const schedulerUTxOs = yield* utxosToSchedulerUTxOs(allUTxOs, config.schedulerPolicyId);
    if (schedulerUTxOs.length === 1) {
      return schedulerUTxOs[0];
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
  const assets: Assets = {
    [toUnit(params.validator.policyId, SCHEDULER_ASSET_NAME)]: 1n,
  };

  const redeemer = Data.to("Init", SchedulerMintRedeemer);

  return lucid
    .newTx()
    .mintAssets(assets, redeemer)
    .pay.ToAddress(params.validator.spendingScriptAddress, assets)
    .attach.Script(params.validator.mintingScript);
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
