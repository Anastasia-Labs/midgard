import { Data } from "@lucid-evolution/lucid";

// Lucid encodes one-constructor enums from the field record directly; the
// generated blueprint still records the Aiken constructor title as `Spend`.
export const ReserveSpendRedeemerSchema = Data.Enum([
  Data.Object({
    Spend: Data.Object({
      reserve_input_index: Data.Integer(),
      payout_input_index: Data.Integer(),
      payout_spend_redeemer_index: Data.Integer(),
      hub_ref_input_index: Data.Integer(),
    }),
  }),
]);
export type ReserveSpendRedeemer = {
  reserve_input_index: bigint;
  payout_input_index: bigint;
  payout_spend_redeemer_index: bigint;
  hub_ref_input_index: bigint;
};
export const ReserveSpendRedeemer =
  ReserveSpendRedeemerSchema as unknown as ReserveSpendRedeemer;
