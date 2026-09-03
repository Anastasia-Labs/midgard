/**
 * Spend-side `withdrawn-input` fault-proof wire twin.
 *
 * A block is faulty when one of its committed native transactions spends an
 * out-ref that a valid withdrawal leaf in the same header also consumes.  This
 * module intentionally does not register a catalogue category; registration is
 * a separate deployment wave.  It only mirrors the three Aiken step datums and
 * redeemers field-for-field.
 */
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "../common.js";
import { type WithdrawalInfo, WithdrawalInfoSchema } from "../ledger-state.js";
import {
  type RootMembershipProof,
  WithdrawalSourceMembershipProofSchema,
} from "../transition-trace.js";
import { FieldOpeningSchema } from "./field-opening-v1.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  type MidgardTxInput,
  MidgardTxInputSchema,
  NativeTxInclusionCarriage,
  NativeTxInclusionCarriageSchema,
} from "./native.js";

export const WITHDRAWN_INPUT_VIOLATION_ID = "withdrawn-input" as const;

export const WithdrawnInputStepCancelSchema = FaultProofStepCancelSchema;
export type WithdrawnInputStepCancel = FaultProofStepCancel;
export const WithdrawnInputStepCancel =
  FaultProofStepCancel as unknown as WithdrawnInputStepCancel;

// Step 01: authenticate one normal transaction under the challenged header.
export const WithdrawnInputStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type WithdrawnInputStep01Datum = Data.Static<
  typeof WithdrawnInputStep01DatumSchema
>;
export const WithdrawnInputStep01Datum =
  WithdrawnInputStep01DatumSchema as unknown as WithdrawnInputStep01Datum;

export const WithdrawnInputStep01ArgsSchema = NativeTxInclusionCarriageSchema;
export type WithdrawnInputStep01Args = NativeTxInclusionCarriage;
export const WithdrawnInputStep01Args =
  NativeTxInclusionCarriage as unknown as WithdrawnInputStep01Args;

export const WithdrawnInputStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawnInputStep01ArgsSchema);
export type WithdrawnInputStep01SpendRedeemer = Data.Static<
  typeof WithdrawnInputStep01SpendRedeemerSchema
>;
export const WithdrawnInputStep01SpendRedeemer =
  WithdrawnInputStep01SpendRedeemerSchema as unknown as WithdrawnInputStep01SpendRedeemer;

// Step 02: open literal body field 0 and select the accused spend input.
export const WithdrawnInputStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  blocks_withdrawals_root: H32Schema,
  blocks_withdrawal_count: Data.Integer(),
});
export type WithdrawnInputStep02State = Data.Static<
  typeof WithdrawnInputStep02StateSchema
>;
export const WithdrawnInputStep02State =
  WithdrawnInputStep02StateSchema as unknown as WithdrawnInputStep02State;

export const WithdrawnInputStep02DatumSchema = faultProofStepDatumSchema(
  WithdrawnInputStep02StateSchema,
);
export type WithdrawnInputStep02Datum = Data.Static<
  typeof WithdrawnInputStep02DatumSchema
>;
export const WithdrawnInputStep02Datum =
  WithdrawnInputStep02DatumSchema as unknown as WithdrawnInputStep02Datum;

export const WithdrawnInputStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  spend_inputs_opening: FieldOpeningSchema,
  bad_input_index: Data.Integer(),
});
export type WithdrawnInputStep02Args = Data.Static<
  typeof WithdrawnInputStep02ArgsSchema
>;
export const WithdrawnInputStep02Args =
  WithdrawnInputStep02ArgsSchema as unknown as WithdrawnInputStep02Args;

export const WithdrawnInputStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawnInputStep02ArgsSchema);
export type WithdrawnInputStep02SpendRedeemer = Data.Static<
  typeof WithdrawnInputStep02SpendRedeemerSchema
>;
export const WithdrawnInputStep02SpendRedeemer =
  WithdrawnInputStep02SpendRedeemerSchema as unknown as WithdrawnInputStep02SpendRedeemer;

// Step 03: prove a valid withdrawal of that same out-ref under the same header.
export const WithdrawnInputStep03StateSchema = Data.Object({
  withdrawn_input: MidgardTxInputSchema,
  blocks_withdrawals_root: H32Schema,
  blocks_withdrawal_count: Data.Integer(),
});
export type WithdrawnInputStep03State = Data.Static<
  typeof WithdrawnInputStep03StateSchema
>;
export const WithdrawnInputStep03State =
  WithdrawnInputStep03StateSchema as unknown as WithdrawnInputStep03State;

export const WithdrawnInputStep03DatumSchema = faultProofStepDatumSchema(
  WithdrawnInputStep03StateSchema,
);
export type WithdrawnInputStep03Datum = Data.Static<
  typeof WithdrawnInputStep03DatumSchema
>;
export const WithdrawnInputStep03Datum =
  WithdrawnInputStep03DatumSchema as unknown as WithdrawnInputStep03Datum;

export const WithdrawnInputStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  withdrawal_membership: WithdrawalSourceMembershipProofSchema,
});
export type WithdrawnInputStep03Args = Data.Static<
  typeof WithdrawnInputStep03ArgsSchema
>;
export const WithdrawnInputStep03Args =
  WithdrawnInputStep03ArgsSchema as unknown as WithdrawnInputStep03Args;

export const WithdrawnInputStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(WithdrawnInputStep03ArgsSchema);
export type WithdrawnInputStep03SpendRedeemer = Data.Static<
  typeof WithdrawnInputStep03SpendRedeemerSchema
>;
export const WithdrawnInputStep03SpendRedeemer =
  WithdrawnInputStep03SpendRedeemerSchema as unknown as WithdrawnInputStep03SpendRedeemer;

export type WithdrawnInputWithdrawalMembership = RootMembershipProof<
  Data.Static<typeof WithdrawalSourceMembershipProofSchema>["key"],
  WithdrawalInfo
>;

export const withdrawnInputStep03State = ({
  input,
  withdrawalsRoot,
  withdrawalCount,
}: {
  readonly input: MidgardTxInput;
  readonly withdrawalsRoot: string;
  readonly withdrawalCount: bigint;
}): WithdrawnInputStep03State => ({
  withdrawn_input: input,
  blocks_withdrawals_root: withdrawalsRoot,
  blocks_withdrawal_count: withdrawalCount,
});

/** Pure predicate twin of step-03's semantic checks (membership is separate). */
export const isWithdrawnInputViolation = ({
  input,
  withdrawal,
}: {
  readonly input: MidgardTxInput;
  readonly withdrawal: WithdrawalInfo;
}): boolean =>
  withdrawal.validity === "WithdrawalIsValid" &&
  withdrawal.body.l2_outref.transactionId === input.tx_id &&
  withdrawal.body.l2_outref.outputIndex === input.output_index;

/** Exported only to make the schema dependency explicit to generated d.ts. */
export const WithdrawnInputWithdrawalInfoSchema = WithdrawalInfoSchema;
