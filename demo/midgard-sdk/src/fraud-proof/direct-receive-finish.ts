import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { DirectReceiveOutputStateSchema } from "./direct-script-derivation.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

export const DirectReceiveFinishArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  outputs_opening: FieldOpeningSchema,
  checkpoint_bytes: Data.Bytes(),
  target_ordinal: Data.Nullable(Data.Integer()),
});
export type DirectReceiveFinishArgs = Data.Static<
  typeof DirectReceiveFinishArgsSchema
>;
export const DirectReceiveFinishArgs = asDataType<DirectReceiveFinishArgs>(
  DirectReceiveFinishArgsSchema,
);
export const DirectReceiveFinishDatumSchema = faultProofStepDatumSchema(
  DirectReceiveOutputStateSchema,
);
export type DirectReceiveFinishDatum = Data.Static<
  typeof DirectReceiveFinishDatumSchema
>;
export const DirectReceiveFinishDatum = asDataType<DirectReceiveFinishDatum>(
  DirectReceiveFinishDatumSchema,
);
export const DirectReceiveFinishRedeemerSchema = faultProofStepRedeemerSchema(
  DirectReceiveFinishArgsSchema,
);
export type DirectReceiveFinishRedeemer = Data.Static<
  typeof DirectReceiveFinishRedeemerSchema
>;
export const DirectReceiveFinishRedeemer =
  asDataType<DirectReceiveFinishRedeemer>(DirectReceiveFinishRedeemerSchema);
