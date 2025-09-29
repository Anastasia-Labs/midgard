import { Address, PolicyId, Script, Data, UTxO, ScriptHash,Datum as CardanoDatum } from "@lucid-evolution/lucid";
import { AddressSchema, OutputReferenceSchema, POSIXTime, POSIXTimeSchema } from "../common.js";
import { NodeDatumSchema } from "../linked-list.js";
import { Header } from "../ledger-state.js";
import { depositTxBuilder } from "../user-events/deposit.js";

export const DepositEventSchema = Data.Object({
    id: OutputReferenceSchema,
    info: Data.Object({
      l2_address: Data.Bytes(),
      l2_datum: Data.Nullable(Data.Bytes())
    })
});

export type DepositEvent = Data.Static<typeof DepositEventSchema>;
export const DepositEvent = DepositEventSchema as unknown as DepositEvent;

export const DepositDatumSchema = Data.Object({
    event: DepositEventSchema,
    inclusion_time: POSIXTimeSchema, // inclusion time is important , time range , 
    witness: Data.Bytes(),
    refund_address: Data.Bytes(),
    refund_datum: Data.Bytes(),
    })

export type DepositDatum = Data.Static<typeof DepositDatumSchema>;
export const DepositDatum = DepositDatumSchema as unknown as DepositDatum;

export const MintRedeemerSchema = Data.Enum([
     Data.Object({ AuthenticateEvent: Data.Object({ nonce_input_index: Data.Integer(), event_output_index: Data.Integer(),
                                         hub_ref_input_index: Data.Integer(),witness_registration_redeemer_index:Data.Integer()}) 
                }),
     Data.Object({ BurnEventNFT: Data.Object({ nonce_asset_name: Data.Bytes(), witness_unregistration_redeemer_index: Data.Integer() }) })           
]);

export type MintRedeemer = Data.Static<typeof MintRedeemerSchema>;
export const MintRedeemer = MintRedeemerSchema as unknown as MintRedeemer;
