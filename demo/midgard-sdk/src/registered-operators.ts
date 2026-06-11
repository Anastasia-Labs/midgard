import {
  Data,
  fromText,
  LucidEvolution,
  TxBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  OutputReferenceSchema,
  POSIXTimeSchema,
} from "@/common.js";

import { incompleteInitLinkedListTxProgram } from "./linked-list.js";

export const REGISTERED_OPERATORS_ROOT_ASSET_NAME = fromText(
  "MIDGARD_REGISTERED_OPERATORS",
);

export const RegisteredOperatorDatumSchema = Data.Object({
  operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
  bond_unlock_time: Data.Nullable(POSIXTimeSchema),
});
export type RegisteredOperatorDatum = Data.Static<
  typeof RegisteredOperatorDatumSchema
>;
export const RegisteredOperatorDatum =
  RegisteredOperatorDatumSchema as unknown as RegisteredOperatorDatum;
export const castRegisteredOperatorDatumToData = (
  datum: RegisteredOperatorDatum,
): unknown => Data.castTo(datum, RegisteredOperatorDatum);

export const DuplicateOperatorStatusSchema = Data.Enum([
  Data.Literal("DuplicateIsRegistered"),
  Data.Object({
    DuplicateIsActive: Data.Object({
      hub_oracle_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Literal("DuplicateIsRetired"),
]);
export type DuplicateOperatorStatus = Data.Static<
  typeof DuplicateOperatorStatusSchema
>;
export const DuplicateOperatorStatus =
  DuplicateOperatorStatusSchema as unknown as DuplicateOperatorStatus;

export const OperatorOriginSchema = Data.Enum([
  Data.Object({
    ReturningOperator: Data.Object({
      retired_operators_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    NewOperator: Data.Object({
      retired_operators_element_ref_input_index: Data.Integer(),
    }),
  }),
]);
export type OperatorOrigin = Data.Static<typeof OperatorOriginSchema>;
export const OperatorOrigin = OperatorOriginSchema as unknown as OperatorOrigin;

export const RegisteredOperatorMintRedeemerSchema = Data.Enum([
  Data.Object({
    Init: Data.Object({
      output_index: Data.Integer(),
    }),
  }),
  Data.Literal("Deinit"),
  Data.Object({
    RegisterOperator: Data.Object({
      registering_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      root_output_index: Data.Integer(),
      registered_node_output_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      active_operators_element_ref_input_index: Data.Integer(),
      operator_origin: OperatorOriginSchema,
    }),
  }),
  Data.Object({
    ActivateOperator: Data.Object({
      activating_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      anchor_element_input_outref: OutputReferenceSchema,
      anchor_element_output_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      retired_operators_element_ref_input_index: Data.Integer(),
      active_operators_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    DeregisterOperator: Data.Object({
      deregistering_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      anchor_element_input_outref: OutputReferenceSchema,
      anchor_element_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    SlashDuplicateOperator: Data.Object({
      duplicate_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      anchor_element_input_outref: OutputReferenceSchema,
      anchor_element_output_index: Data.Integer(),
      duplicate_node_ref_input_index: Data.Integer(),
      duplicate_operator_status: DuplicateOperatorStatusSchema,
    }),
  }),
  Data.Object({
    SlashFraudulentOperator: Data.Object({
      slashing_arguments: Data.Any(),
    }),
  }),
]);
export type RegisteredOperatorMintRedeemer = Data.Static<
  typeof RegisteredOperatorMintRedeemerSchema
>;
export const RegisteredOperatorMintRedeemer =
  RegisteredOperatorMintRedeemerSchema as unknown as RegisteredOperatorMintRedeemer;

export type RegisteredOperatorInitParams = {
  validator: AuthenticatedValidator;
  lovelace?: bigint;
};

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteRegisteredOperatorInitTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorInitParams,
): Effect.Effect<TxBuilder, never> =>
  incompleteInitLinkedListTxProgram(lucid, {
    validator: params.validator,
    rootAssetName: REGISTERED_OPERATORS_ROOT_ASSET_NAME,
    data: "",
    redeemer: (outputIndex) =>
      Data.to(
        { Init: { output_index: outputIndex } },
        RegisteredOperatorMintRedeemer,
      ),
    lovelace: params.lovelace,
  });
