import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { DirectSourceBoundIndexSchema } from "./direct-source-workers.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

export const DirectReceiveStartArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  outputs_opening: FieldOpeningSchema,
});
export type DirectReceiveStartArgs = Data.Static<
  typeof DirectReceiveStartArgsSchema
>;
export const DirectReceiveStartArgs = asDataType<DirectReceiveStartArgs>(
  DirectReceiveStartArgsSchema,
);
export const DirectReceiveStartDatumSchema = faultProofStepDatumSchema(
  DirectSourceBoundIndexSchema,
);
export type DirectReceiveStartDatum = Data.Static<
  typeof DirectReceiveStartDatumSchema
>;
export const DirectReceiveStartDatum = asDataType<DirectReceiveStartDatum>(
  DirectReceiveStartDatumSchema,
);
export const DirectReceiveStartRedeemerSchema = faultProofStepRedeemerSchema(
  DirectReceiveStartArgsSchema,
);
export type DirectReceiveStartRedeemer = Data.Static<
  typeof DirectReceiveStartRedeemerSchema
>;
export const DirectReceiveStartRedeemer =
  asDataType<DirectReceiveStartRedeemer>(DirectReceiveStartRedeemerSchema);
