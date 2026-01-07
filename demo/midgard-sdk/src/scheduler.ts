import {
  Assets,
  Data,
  fromText,
  LucidEvolution,
  toUnit,
  TxBuilder,
} from "@lucid-evolution/lucid";
import { AuthenticatedValidator } from "@/common.js";
import { SCHEDULER_ASSET_NAME } from "./constants.js";

export const SchedulerDatumSchema = Data.Object({
  operator: Data.Bytes(),
  startTime: Data.Integer(),
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
    [toUnit(params.validator.policyId, fromText(SCHEDULER_ASSET_NAME))]: 1n,
  };

  const redeemer = Data.to("Init", SchedulerMintRedeemer);

  const encodedDatum = Data.void();

  return lucid
    .newTx()
    .mintAssets(assets, redeemer)
    .pay.ToAddressWithData(
      params.validator.spendScriptAddress,
      { kind: "inline", value: encodedDatum },
      assets,
    )
    .attach.Script(params.validator.mintScript);
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
