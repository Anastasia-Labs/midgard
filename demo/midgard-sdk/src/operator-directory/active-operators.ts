import {
  AuthenticatedValidator,
  LucidError,
  POSIXTimeSchema,
} from "@/common.js";
import { AuthenticUTxO, authenticateUTxOs } from "@/internals.js";
import {
  LucidEvolution,
  TxBuilder,
  UTxO,
  Data,
  fromText,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { incompleteInitLinkedListTxProgram } from "../linked-list.js";
import { SlashingArgumentsSchema } from "./common.js";

export const ACTIVE_OPERATORS_ROOT_ASSET_NAME = fromText(
  "MIDGARD_ACTIVE_OPERATORS",
);

export const ActiveOperatorSpendRedeemerSchema = Data.Enum([
  Data.Literal("ListStateTransition"),
  Data.Object({
    UpdateBondHoldNewState: Data.Object({
      active_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      active_node_input_index: Data.Integer(),
      active_node_output_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      state_queue_input_index: Data.Integer(),
      state_queue_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    UpdateBondHoldNewSettlement: Data.Object({
      active_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      active_node_input_index: Data.Integer(),
      active_node_output_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      settlement_input_index: Data.Integer(),
      settlement_redeemer_index: Data.Integer(),
      resolution_time: POSIXTimeSchema,
    }),
  }),
  Data.Object({
    StrikeForInactivity: Data.Object({
      active_node_input_index: Data.Integer(),
      active_node_output_index: Data.Integer(),
      operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      active_node_link: Data.Any(),
      scheduler_input_index: Data.Integer(),
      scheduler_redeemer_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
    }),
  }),
]);
export type ActiveOperatorSpendRedeemer = Data.Static<
  typeof ActiveOperatorSpendRedeemerSchema
>;
export const ActiveOperatorSpendRedeemer =
  ActiveOperatorSpendRedeemerSchema as unknown as ActiveOperatorSpendRedeemer;

export const OperatorRemovalSchedulerSyncSchema = Data.Enum([
  Data.Object({
    ShowOperatorIsInactive: Data.Object({
      scheduler_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ShowSchedulerIsAdvancing: Data.Object({
      scheduler_input_index: Data.Integer(),
      scheduler_redeemer_index: Data.Integer(),
      removing_operators_anchor_element_key: Data.Nullable(Data.Bytes()),
      removing_operator_is_the_last_member: Data.Boolean(),
    }),
  }),
]);
export type OperatorRemovalSchedulerSync = Data.Static<
  typeof OperatorRemovalSchedulerSyncSchema
>;
export const OperatorRemovalSchedulerSync =
  OperatorRemovalSchedulerSyncSchema as unknown as OperatorRemovalSchedulerSync;

export const ActiveOperatorMintRedeemerSchema = Data.Enum([
  Data.Object({
    Init: Data.Object({
      output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    Deinit: Data.Object({
      input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ActivateOperator: Data.Object({
      new_active_operator_key: Data.Bytes({ minLength: 28, maxLength: 28 }),
      new_active_operator_bond_unlock_time: Data.Nullable(POSIXTimeSchema),
      active_operator_anchor_element_input_index: Data.Integer(),
      active_operator_anchor_element_output_index: Data.Integer(),
      active_operator_inserted_node_output_index: Data.Integer(),
      registered_operators_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RetireOperator: Data.Object({
      active_operator_key: Data.Bytes({ minLength: 28, maxLength: 28 }),
      hub_oracle_ref_input_index: Data.Integer(),
      active_operator_anchor_element_input_index: Data.Integer(),
      active_operator_removed_node_input_index: Data.Integer(),
      active_operator_anchor_element_output_index: Data.Integer(),
      retired_operators_redeemer_index: Data.Integer(),
      penalize_for_inactivity: Data.Boolean(),
      operator_removal_scheduler_sync: OperatorRemovalSchedulerSyncSchema,
    }),
  }),
  Data.Object({
    SlashOperator: Data.Object({
      slashing_arguments: SlashingArgumentsSchema,
      operator_removal_scheduler_sync: OperatorRemovalSchedulerSyncSchema,
    }),
  }),
]);
export type ActiveOperatorMintRedeemer = Data.Static<
  typeof ActiveOperatorMintRedeemerSchema
>;
export const ActiveOperatorMintRedeemer =
  ActiveOperatorMintRedeemerSchema as unknown as ActiveOperatorMintRedeemer;

export const ActiveOperatorDatumSchema = Data.Object({
  bond_unlock_time: Data.Nullable(POSIXTimeSchema),
  inactivity_strikes: Data.Integer(),
});
export type ActiveOperatorDatum = Data.Static<typeof ActiveOperatorDatumSchema>;
export const ActiveOperatorDatum =
  ActiveOperatorDatumSchema as unknown as ActiveOperatorDatum;
export const castActiveOperatorDatumToData = (
  datum: ActiveOperatorDatum,
): unknown => Data.castTo(datum, ActiveOperatorDatum);

export type ActiveOperatorInitParams = {
  validator: AuthenticatedValidator;
  outputIndex?: bigint;
  lovelace?: bigint;
};
export type ActiveOperatorDeinitParams = {};
export type ActiveOperatorActivateParams = {};
export type ActiveOperatorRetireParams = {};
export type ActiveOperatorListStateTransitionParams = {};
export type ActiveOperatorRemoveSlashBondParams = {};
export type ActiveOperatorUpdateCommitmentTimeParams = {};

export type ActiveOperatorUTxO = AuthenticUTxO<ActiveOperatorDatum>;

export type FetchActiveOperatorParams = {
  activeOperatorAddress: string;
  operator: string;
  activeOperatorPolicyId: string;
};

export const fetchActiveOperatorUTxOs = (
  params: FetchActiveOperatorParams,
  lucid: LucidEvolution,
): Effect.Effect<ActiveOperatorUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.activeOperatorAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Active Operators UTxOs",
          cause: err,
        }),
    });
    if (allUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Active Operators transaction",
        cause: "No UTxOs found in Active Operators Contract address",
      });
    }
    const activeOperatorUTxOs: ActiveOperatorUTxO[] =
      yield* authenticateUTxOs<ActiveOperatorDatum>(
        allUtxos,
        params.activeOperatorPolicyId,
        ActiveOperatorDatum,
      );
    return activeOperatorUTxOs;
  });

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorInitTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorInitParams,
): Effect.Effect<TxBuilder, never> =>
  Effect.gen(function* () {
    const rootData = "";

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      rootAssetName: ACTIVE_OPERATORS_ROOT_ASSET_NAME,
      data: rootData,
      redeemer: Data.to(
        { Init: { output_index: params.outputIndex ?? 0n } },
        ActiveOperatorMintRedeemer,
      ),
      lovelace: params.lovelace,
    });
  });

/**
 * The program that performs the deinit of an operator.
 *
 * @param lucid - The LucidEvolution instance to use for the deinit.
 * @param params - The parameters required for deinit.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorDeinitTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorDeinitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
/**
 * The program that performs the activation of an operator.
 *
 * @param lucid - The LucidEvolution instance to use for the activation.
 * @param params - The parameters required for activation.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorActivateTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorActivateParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Retire
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorRetireTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorRetireParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * ListStateTransition
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorListStateTransitionTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorListStateTransitionParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Remove slash bond
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorRemoveSlashBondTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorRemoveSlashBondParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * UpdateCommitmentTime
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorUpdateCommitmentTimeTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorUpdateCommitmentTimeParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
