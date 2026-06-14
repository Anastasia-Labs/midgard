import { VerificationKeyHashSchema } from "@/common.js";
import { Data } from "@lucid-evolution/lucid";

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
  slashed_operator: VerificationKeyHashSchema,
  hub_oracle_ref_input_index: Data.Integer(),
  slashed_operator_anchor_element_input_index: Data.Integer(),
  slashed_operator_node_input_index: Data.Integer(),
  slashed_operator_anchor_element_output_index: Data.Integer(),
  slashing_reason: SlashingReasonSchema,
});
export type SlashingArguments = Data.Static<typeof SlashingArgumentsSchema>;
export const SlashingArguments =
  SlashingArgumentsSchema as unknown as SlashingArguments;
