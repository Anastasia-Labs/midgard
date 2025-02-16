import { Data } from "@lucid-evolution/lucid";
import { OutputReferenceSchema, POSIXTimeSchema } from "./common.js";
import { NodeDatumSchema } from "./linked-list/index.js";

export const ConfigSchema = Data.Object({
  initUTxO: OutputReferenceSchema,
  refundWaitingPeriod: POSIXTimeSchema,
});
export type Config = Data.Static<typeof ConfigSchema>;
export const Config = ConfigSchema as unknown as Config;

export const RedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Object({
    CommitBlockHeader: Data.Object({ operator: Data.Bytes() }),
  }),
  Data.Literal("MergeToConfirmedState"),
  Data.Object({
    RemoveFraudulentBlockHeader: Data.Object({
      fraudulent_operator: Data.Bytes(),
    }),
  }),
]);
export type Redeemer = Data.Static<typeof RedeemerSchema>;
export const Redeemer = RedeemerSchema as unknown as Redeemer;

export type Datum = Data.Static<typeof NodeDatumSchema>;
export const Datum = NodeDatumSchema as unknown as Datum;
