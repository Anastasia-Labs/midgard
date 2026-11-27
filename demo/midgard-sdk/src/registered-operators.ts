import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data, fromText } from "@lucid-evolution/lucid";

import { OutputReferenceSchema } from "./common.js";

export const REGISTERED_OPERATORS_ROOT_ASSET_NAME = fromText(
  "MIDGARD_REGISTERED_OPERATORS",
);

export const RegisteredOperatorDatumSchema = Data.Object({
  operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
});
export type RegisteredOperatorDatum = Data.Static<
  typeof RegisteredOperatorDatumSchema
>;
export const RegisteredOperatorDatum = asDataType<RegisteredOperatorDatum>(
  RegisteredOperatorDatumSchema,
);
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
export const DuplicateOperatorStatus = asDataType<DuplicateOperatorStatus>(
  DuplicateOperatorStatusSchema,
);

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
      retired_operators_element_ref_input_index: Data.Integer(),
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
      bounty_claimant: Data.Bytes({ minLength: 28, maxLength: 28 }),
    }),
  }),
]);
export type RegisteredOperatorMintRedeemer = Data.Static<
  typeof RegisteredOperatorMintRedeemerSchema
>;
export const RegisteredOperatorMintRedeemer =
  asDataType<RegisteredOperatorMintRedeemer>(
    RegisteredOperatorMintRedeemerSchema,
  );
