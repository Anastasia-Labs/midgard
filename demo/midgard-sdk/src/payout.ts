import { Data } from "@lucid-evolution/lucid";
import { AddressSchema, OutputReferenceSchema, ValueSchema } from "@/common.js";
import { CardanoDatumSchema } from "@/ledger-state.js";

export const PayoutDatumSchema = Data.Object({
  l2_value: ValueSchema,
  l1_address: AddressSchema,
  l1_datum: CardanoDatumSchema,
});
export type PayoutDatum = Data.Static<typeof PayoutDatumSchema>;
export const PayoutDatum = PayoutDatumSchema as unknown as PayoutDatum;

export const PayoutSpendRedeemerSchema = Data.Enum([
  Data.Object({
    AddFunds: Data.Object({
      payout_input_index: Data.Integer(),
      payout_output_index: Data.Integer(),
      reserve_input_index: Data.Integer(),
      reserve_change_output_index: Data.Nullable(Data.Integer()),
      reserve_spend_redeemer_index: Data.Integer(),
      payout_spend_redeemer_index: Data.Integer(),
      hub_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ConcludeWithdrawal: Data.Object({
      payout_input_index: Data.Integer(),
      l1_output_index: Data.Integer(),
      burn_redeemer_index: Data.Integer(),
      hub_ref_input_index: Data.Integer(),
    }),
  }),
]);
export type PayoutSpendRedeemer = Data.Static<typeof PayoutSpendRedeemerSchema>;
export const PayoutSpendRedeemer =
  PayoutSpendRedeemerSchema as unknown as PayoutSpendRedeemer;

export const PayoutMintRedeemerSchema = Data.Enum([
  Data.Object({
    MintPayout: Data.Object({
      withdrawal_utxo_out_ref: OutputReferenceSchema,
      withdrawal_input_index: Data.Integer(),
      withdrawal_spend_redeemer_index: Data.Integer(),
      hub_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    BurnPayout: Data.Object({
      payout_input_index: Data.Integer(),
      payout_asset_name: Data.Bytes(),
      payout_spend_redeemer_index: Data.Integer(),
      hub_ref_input_index: Data.Integer(),
    }),
  }),
]);
export type PayoutMintRedeemer = Data.Static<typeof PayoutMintRedeemerSchema>;
export const PayoutMintRedeemer =
  PayoutMintRedeemerSchema as unknown as PayoutMintRedeemer;
