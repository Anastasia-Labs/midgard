import {
  Assets,
  Data,
  LucidEvolution,
  toUnit,
  TxBuilder,
} from "@lucid-evolution/lucid";
import { AuthenticatedValidator } from "@/common.js";
import { SCHEDULER_ASSET_NAME } from "@/constants.js";

/**
 * Scheduler SDK helpers and redeemer/data schemas.
 *
 * The scheduler coordinates operator turns and time windows, so even the simple
 * initialization helpers here are part of the protocol's state-machine
 * bootstrap path.
 */
export const SchedulerDatumSchema = Data.Object({
  operator: Data.Bytes(),
  startTime: Data.Integer(),
});

export type SchedulerDatum = Data.Static<typeof SchedulerDatumSchema>;
export const SchedulerDatum = SchedulerDatumSchema as unknown as SchedulerDatum;

/**
 * Mint redeemers understood by the scheduler policy.
 */
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

/**
 * Parameters for creating the initial scheduler UTxO.
 */
export type SchedulerInitParams = {
  validator: AuthenticatedValidator;
  datum?: SchedulerDatum;
  lovelace?: bigint;
};

export type SchedulerDeinitParams = {};
export type SchedulerAdvanceParams = {};
export type SchedulerRewindParams = {};

const DEFAULT_SCHEDULER_INIT_LOVELACE = 5_000_000n;
export const INITIAL_SCHEDULER_DATUM: SchedulerDatum = {
  operator: "",
  startTime: 0n,
};

/**
 * Builds the scheduler initialization transaction fragment.
 *
 * This mints the scheduler witness NFT and creates the first scheduler output
 * with either the provided datum/lovelace or the protocol defaults.
 */
export const incompleteSchedulerInitTxProgram = (
  lucid: LucidEvolution,
  params: SchedulerInitParams,
): TxBuilder => {
  const assets: Assets = {
    [toUnit(params.validator.policyId, SCHEDULER_ASSET_NAME)]: 1n,
  };
  const outputAssets: Assets = {
    lovelace: params.lovelace ?? DEFAULT_SCHEDULER_INIT_LOVELACE,
    ...assets,
  };
  const initialDatum: SchedulerDatum = params.datum ?? INITIAL_SCHEDULER_DATUM;

  const redeemer = Data.to("Init", SchedulerMintRedeemer);

  return lucid
    .newTx()
    .mintAssets(assets, redeemer)
    .pay.ToContract(
      params.validator.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to(initialDatum, SchedulerDatum),
      },
      outputAssets,
    )
    .attach.Script(params.validator.mintingScript);
};

/**
 * Stub entry point for scheduler de-initialization flows.
 */
export const incompleteSchedulerDeinitTxProgram = (
  lucid: LucidEvolution,
  params: SchedulerDeinitParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for advancing the scheduler state machine.
 */
export const incompleteSchedulerAdvanceTxProgram = (
  lucid: LucidEvolution,
  params: SchedulerAdvanceParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for rewinding the scheduler state machine.
 */
export const incompleteSchedulerRewindTxProgram = (
  lucid: LucidEvolution,
  params: SchedulerRewindParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};
