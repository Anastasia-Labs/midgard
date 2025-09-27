import { Address, PolicyId, Script, Data, UTxO, ScriptHash,Datum as CardanoDatum } from "@lucid-evolution/lucid";
import { AddressSchema, OutputReferenceSchema, POSIXTime, POSIXTimeSchema } from "../common.js";
import { NodeDatumSchema } from "../linked-list.js";
import { Header } from "../ledger-state.js";
import { depositTxBuilder } from "../user-events/deposit.js";

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

