import { Address, Data, PolicyId, UTxO, Script } from "@lucid-evolution/lucid";
import {
  OutputReferenceSchema,
  POSIXTime,
  POSIXTimeSchema,
} from "@/tx-builder/common.js";
import { DepositEventSchema, DepositInfo } from "@/tx-builder/ledger-state.js";

export type DepositParams = {
  depositScriptAddress: string;
  mintingPolicy: Script;
  policyId: string;
  depositInfo: DepositInfo;
  inclusionTime: POSIXTime;
};

export const DatumSchema = Data.Object({
  event: DepositEventSchema,
  inclusionTime: POSIXTimeSchema, // inclusion time is important , time range ,
});

export type Datum = Data.Static<typeof DatumSchema>;
export const Datum = DatumSchema as unknown as Datum;

export type DepositUTxO = {
  utxo: UTxO;
  datum: Datum;
};

export const MintRedeemerSchema = Data.Enum([
  Data.Object({
    AuthenticateEvent: Data.Object({
      nonceInputIndex: Data.Integer(),
      eventOutputIndex: Data.Integer(),
      hubRefInputIndex: Data.Integer(),
      witnessRegistrationRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    BurnEventNFT: Data.Object({
      nonceAssetName: Data.Bytes(),
      witnessUnregistrationRedeemerIndex: Data.Integer(),
    }),
  }),
]);

export type MintRedeemer = Data.Static<typeof MintRedeemerSchema>;
export const MintRedeemer = MintRedeemerSchema as unknown as MintRedeemer;

export type FetchConfig = {
  depositAddress: Address;
  depositPolicyId: PolicyId;
  inclusionStartTime: POSIXTime;
  inclusionEndTime: POSIXTime;
};
