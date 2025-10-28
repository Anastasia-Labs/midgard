

// TxOrderEvent ≔ (TxOrderId, MidgardTx)
// TxOrderId ≔ OutputRef

import { AddressSchema, OutputReferenceSchema, POSIXTime, POSIXTimeSchema } from "@/tx-builder/common.js";
import { MidgardTxCompactSchema, TxOrderEventSchema } from "@/tx-builder/ledger-state.js";
import { Data, Script } from "@lucid-evolution/lucid";

export type TransactionOrderParams = {
  txOrderAddress: string; 
  mintingPolicy: Script; // tx_order 
  policyId: string;
  refundAddress: string,
  refundDatum: string;
  inclusionTime: POSIXTime;
  midgardTxBody: string;
  midgardTxWits: string;
};

export const DatumSchema = Data.Object({
  event: TxOrderEventSchema,
  inclusionTime: POSIXTimeSchema, // inclusion time is important , time range ,
  refundAddress: Data.Bytes(),
  refundDatum: Data.Nullable(Data.Bytes())
});
export type Datum = Data.Static<typeof DatumSchema>;
export const Datum = DatumSchema as unknown as Datum;




