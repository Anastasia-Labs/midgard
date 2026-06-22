import {
  Data,
  fromText,
  LucidEvolution,
  TxBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  LucidError,
  OutputReferenceSchema,
  POSIXTimeSchema,
} from "@/common.js";
import { authenticateUTxOs, AuthenticUTxO } from "@/internals.js";

import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  incompleteInitLinkedListTxProgram,
} from "./linked-list.js";

export const ACTIVE_OPERATORS_ROOT_ASSET_NAME = fromText(
  "MIDGARD_ACTIVE_OPERATORS",
);

export const SlashingReasonSchema = Data.Enum([
  Data.Object({
    SlashOperatorForBadState: Data.Object({
      state_queue_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    SlashOperatorForBadSettlement: Data.Object({
      settlement_input_index: Data.Integer(),
      settlement_redeemer_index: Data.Integer(),
    }),
  }),
]);
export type SlashingReason = Data.Static<typeof SlashingReasonSchema>;
export const SlashingReason = SlashingReasonSchema as unknown as SlashingReason;

export const SlashingArgumentsSchema = Data.Object({
  slashed_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
  hub_oracle_ref_input_index: Data.Integer(),
  slashed_operator_anchor_element_input_outref: OutputReferenceSchema,
  slashed_operator_anchor_element_output_index: Data.Integer(),
  slashing_reason: SlashingReasonSchema,
});
export type SlashingArguments = Data.Static<typeof SlashingArgumentsSchema>;
export const SlashingArguments =
  SlashingArgumentsSchema as unknown as SlashingArguments;

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

export const ActiveOperatorSpendRedeemerSchema = Data.Enum([
  Data.Literal("ListStateTransition"),
  Data.Object({
    UpdateBondHoldNewState: Data.Object({
      active_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      active_node_input_index: Data.Integer(),
      active_node_output_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
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

export const ActiveOperatorMintRedeemerSchema = Data.Enum([
  Data.Object({
    Init: Data.Object({
      output_index: Data.Integer(),
    }),
  }),
  Data.Literal("Deinit"),
  Data.Object({
    ActivateOperator: Data.Object({
      new_active_operator_key: Data.Bytes({ minLength: 28, maxLength: 28 }),
      active_operator_anchor_element_output_index: Data.Integer(),
      active_operator_inserted_node_output_index: Data.Integer(),
      registered_operators_redeemer_index: Data.Integer(),
      active_operators_set_was_empty: Data.Boolean(),
    }),
  }),
  Data.Object({
    RetireOperator: Data.Object({
      active_operator_key: Data.Bytes({ minLength: 28, maxLength: 28 }),
      hub_oracle_ref_input_index: Data.Integer(),
      active_operator_anchor_element_input_outref: OutputReferenceSchema,
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
  lovelace?: bigint;
};

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
    const allUtxos = yield* Effect.tryPromise({
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
    return yield* authenticateUTxOs<ActiveOperatorDatum>(
      allUtxos,
      params.activeOperatorPolicyId,
      ActiveOperatorDatum,
    );
  });

export const requireActiveOperatorUTxO = (
  utxos: readonly ActiveOperatorUTxO[],
  operatorKeyHash: string,
): Effect.Effect<ActiveOperatorUTxO, LucidError> => {
  const assetName = ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash;
  const match = utxos.find((utxo) => utxo.assetName === assetName);
  return match === undefined
    ? Effect.fail(
        new LucidError({
          message: `No Active Operator UTxO with key "${operatorKeyHash}" found`,
          cause: `Expected active operator asset name ${assetName}`,
        }),
      )
    : Effect.succeed(match);
};

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
  incompleteInitLinkedListTxProgram(lucid, {
    validator: params.validator,
    rootAssetName: ACTIVE_OPERATORS_ROOT_ASSET_NAME,
    data: "",
    redeemer: (outputIndex) =>
      Data.to(
        { Init: { output_index: outputIndex } },
        ActiveOperatorMintRedeemer,
      ),
    lovelace: params.lovelace,
  });
