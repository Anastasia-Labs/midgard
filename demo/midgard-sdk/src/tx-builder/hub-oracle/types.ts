import { Data } from "@lucid-evolution/lucid";
import { AddressSchema, PolicyIdSchema } from "../common.js";

export const DatumSchema = Data.Object({
  registeredOperators: PolicyIdSchema,
  activeOperators: PolicyIdSchema,
  retiredOperators: PolicyIdSchema,
  scheduler: PolicyIdSchema,
  stateQueue: PolicyIdSchema,
  fraudProofCatalogue: PolicyIdSchema,
  fraudProof: PolicyIdSchema,
  registeredOperatorsAddr: AddressSchema,
  activeOperatorsAddr: AddressSchema,
  retiredOperatorsAddr: AddressSchema,
  schedulerAddr: AddressSchema,
  state_queueAddr: AddressSchema,
  fraudProofCatalogueAddr: AddressSchema,
  fraudProofAddr: AddressSchema,
});
export type Datum = Data.Static<typeof DatumSchema>;
export const Datum = DatumSchema as unknown as Datum;
