import { Address, PolicyId, Data, UTxO } from "@lucid-evolution/lucid";
import { POSIXTimeSchema } from "@/tx-builder/common.js";

export const DepositDatumSchema = Data.Object({
  data: Data.Object({
    startTime: POSIXTimeSchema,
    endTime: POSIXTimeSchema,
  }),
});

export type Datum = Data.Static<typeof DepositDatumSchema>;
export const Datum = DepositDatumSchema as unknown as Datum;

export type DepositUTxO = {
  utxo: UTxO;
  datum: Datum;
};

export type FetchConfig = {
  depositAddress: Address;
  depositPolicyId: PolicyId;
};
