import { Data, UTxO } from "@lucid-evolution/lucid";
import { OutputReferenceSchema, POSIXTimeSchema } from "@/tx-builder/common.js";

export const DepositEventSchema = Data.Object({
    id: OutputReferenceSchema,
    info: Data.Object({
      l2_address: Data.Bytes(),
      l2_datum: Data.Nullable(Data.Bytes())
    })
});

export type DepositEvent = Data.Static<typeof DepositEventSchema>;
export const DepositEvent = DepositEventSchema as unknown as DepositEvent;

export const DepositInfoSchema = Data.Object({
    l2_address: Data.Bytes(),
    l2_datum: Data.Nullable(Data.Bytes())
});

export type DepositInfo = Data.Static<typeof DepositInfoSchema>;
export const DepositInfo = DepositInfoSchema as unknown as DepositInfo;

export const DepositDatumSchema = Data.Object({
    event: DepositEventSchema,
    inclusion_time: POSIXTimeSchema, // inclusion time is important , time range ,
    })

export type DepositDatum = Data.Static<typeof DepositDatumSchema>;
export const DepositDatum = DepositDatumSchema as unknown as DepositDatum;

export type DepositUTxO = {
  utxo: UTxO;
  datum: DepositDatum;
};

export const MintRedeemerSchema = Data.Enum([
     Data.Object({ AuthenticateEvent: Data.Object({ nonce_input_index: Data.Integer(), event_output_index: Data.Integer(),
                                         hub_ref_input_index: Data.Integer(),witness_registration_redeemer_index:Data.Integer()})
                }),
     Data.Object({ BurnEventNFT: Data.Object({ nonce_asset_name: Data.Bytes(), witness_unregistration_redeemer_index: Data.Integer() }) })
]);

export type MintRedeemer = Data.Static<typeof MintRedeemerSchema>;
export const MintRedeemer = MintRedeemerSchema as unknown as MintRedeemer;
