import { Address, PolicyId, Script, Data, UTxO, ScriptHash,Datum as CardanoDatum } from "@lucid-evolution/lucid";
import { AddressSchema, OutputReferenceSchema, POSIXTime, POSIXTimeSchema } from "../common.js";
import { NodeDatumSchema } from "../linked-list.js";
import { Header } from "../ledger-state.js";
import { depositTxBuilder } from "../user-events/deposit.js";

// export const DepositDatumSchema = Data.Object({
//   data: Data.Object({
//     startTime: POSIXTimeSchema,
//     endTime: POSIXTimeSchema,
//   }),
// });

// export type Datum = Data.Static<typeof DepositDatumSchema>;
// export const Datum = DepositDatumSchema as unknown as Datum;

// export type DepositUTxO = {
//   utxo: UTxO;
//   datum: Datum;
// };

// export type FetchConfig = {
//   depositAddress: Address;
//   depositPolicyId: PolicyId;
// };


// pub type DepositId =
//   OutputReference

// pub type DepositInfo {
//   l2_address: Address,
//   l2_datum: Option<Data>,
// }

// pub type DepositEvent {
//   id: DepositId,
//   info: DepositInfo,
// }


export const DepositEventSchema = Data.Object({
    id: OutputReferenceSchema,
    info: Data.Object({
      l2_address: Data.Bytes(),
//   l2_datum: Option<Data>
    })
});

export type DepositEvent = Data.Static<typeof DepositEventSchema>;
export const DepositEvent = DepositEventSchema as unknown as DepositEvent;

// pub type Datum<event_type> {
//   event: event_type,
//   inclusion_time: PosixTime,
//   witness: ScriptHash,
//   refund_address: Address,
//   refund_datum: CardanoDatum,
// }

// export const DepositDatum = {
//   event: DepositEvent,
//   inclusion_time: POSIXTime,
//   witness: ScriptHash,
//   refund_address: AddressSchema,
//   refund_datum: CardanoDatum,
// };


export const DepositDatumSchema = Data.Object({
    event: DepositEventSchema,
    inclusion_time: POSIXTimeSchema,
    witness: Data.Bytes(),
    refund_address: Data.Bytes(),
    refund_datum: Data.Bytes(),
    })

export type DepositDatum = Data.Static<typeof DepositDatumSchema>;
export const DepositDatum = DepositDatumSchema as unknown as DepositDatum;

// pub type MintRedeemer {
//   AuthenticateEvent {
//     nonce_input_index: Int,
//     event_output_index: Int,
//     hub_ref_input_index: Int,
//     witness_registration_redeemer_index: Int,
//   }
//   BurnEventNFT {
//     nonce_asset_name: AssetName,
//     witness_unregistration_redeemer_index: Int,
//   }
// }

export const MintRedeemerSchema = Data.Enum([
     Data.Object({ AuthenticateEvent: Data.Object({ nonce_input_index: Data.Integer(), event_output_index: Data.Integer(),
                                         hub_ref_input_index: Data.Integer(),witness_registration_redeemer_index:Data.Integer()}) 
                }),
     Data.Object({ BurnEventNFT: Data.Object({ nonce_asset_name: Data.Bytes(), witness_unregistration_redeemer_index: Data.Integer() }) })           
]);

export type MintRedeemer = Data.Static<typeof MintRedeemerSchema>;
export const MintRedeemer = MintRedeemerSchema as unknown as MintRedeemer;

