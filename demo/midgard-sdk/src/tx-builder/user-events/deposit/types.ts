import { Address, Data, PolicyId, UTxO } from "@lucid-evolution/lucid";
import { OutputReferenceSchema, POSIXTime, POSIXTimeSchema } from "@/tx-builder/common.js";

export const DepositEventSchema = Data.Object({
    id: OutputReferenceSchema,
    info: Data.Object({
      l2Address: Data.Bytes(),
      l2Datum: Data.Nullable(Data.Bytes())
    })
});

export type DepositEvent = Data.Static<typeof DepositEventSchema>;
export const DepositEvent = DepositEventSchema as unknown as DepositEvent;

export const DepositInfoSchema = Data.Object({
    l2Address: Data.Bytes(),
    l2Datum: Data.Nullable(Data.Bytes())
});

export type DepositInfo = Data.Static<typeof DepositInfoSchema>;
export const DepositInfo = DepositInfoSchema as unknown as DepositInfo;

export const DepositDatumSchema = Data.Object({
    event: DepositEventSchema,
    inclusionTime: POSIXTimeSchema, // inclusion time is important , time range ,
    })

export type DepositDatum = Data.Static<typeof DepositDatumSchema>;
export const DepositDatum = DepositDatumSchema as unknown as DepositDatum;

export type DepositUTxO = {
  utxo: UTxO;
  datum: DepositDatum;
};

export const MintRedeemerSchema = Data.Enum([
     Data.Object({ AuthenticateEvent: Data.Object({ nonceInputIndex: Data.Integer(), eventOutputIndex: Data.Integer(),
                                         hubRefInputIndex: Data.Integer(),witnessRegistrationRedeemerIndex:Data.Integer()})
                }),
     Data.Object({ BurnEventNFT: Data.Object({ nonceAssetName: Data.Bytes(), witnessUnregistrationRedeemerIndex: Data.Integer() }) })
]);

export type MintRedeemer = Data.Static<typeof MintRedeemerSchema>;
export const MintRedeemer = MintRedeemerSchema as unknown as MintRedeemer;

export type FetchConfig = {
  depositAddress: Address;
  depositPolicyId: PolicyId;
  inclusionStartTime: POSIXTime;
  inclusionEndTime: POSIXTime;
};
