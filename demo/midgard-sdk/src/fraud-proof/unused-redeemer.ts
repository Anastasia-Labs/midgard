/** Direct unused-redeemer wire: the field-8 walk
 * (`unused-redeemer/walk.ak`). The family entry opens the walk over the
 * source-finish binding; grammar certification and the pointer walk share the
 * direct progress and scan actions. Constructor and field order is wire
 * format. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import {
  DirectProgressArgsSchema,
  DirectScanActionSchema,
} from "./direct-purpose-workers.js";
import { DirectFieldCursorSchema } from "./direct-script-derivation.js";
import { DirectSourceBoundIndexSchema } from "./direct-source-workers.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

/** `repeated`: an item before `redeemer_index` carries the declared pointer.
 * `reached`: the item at `redeemer_index` was read and carries it.
 * `complete`: the field is exhausted. */
export const UnusedRedeemerWalkStateSchema = Data.Object({
  bound: DirectSourceBoundIndexSchema,
  redeemer_index: Data.Integer(),
  pointer_tag: Data.Integer(),
  pointer_index: Data.Integer(),
  cursor: DirectFieldCursorSchema,
  repeated: Data.Boolean(),
  reached: Data.Boolean(),
  complete: Data.Boolean(),
});
export type UnusedRedeemerWalkState = Data.Static<
  typeof UnusedRedeemerWalkStateSchema
>;
export const UnusedRedeemerWalkState = asDataType<UnusedRedeemerWalkState>(
  UnusedRedeemerWalkStateSchema,
);

export const UnusedRedeemerEntryArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  redeemer_index: Data.Integer(),
  pointer_tag: Data.Integer(),
  pointer_index: Data.Integer(),
  opening: FieldOpeningSchema,
});
export type UnusedRedeemerEntryArgs = Data.Static<
  typeof UnusedRedeemerEntryArgsSchema
>;
export const UnusedRedeemerEntryArgs = asDataType<UnusedRedeemerEntryArgs>(
  UnusedRedeemerEntryArgsSchema,
);

export const UnusedRedeemerEntryRedeemerSchema = faultProofStepRedeemerSchema(
  UnusedRedeemerEntryArgsSchema,
);
export type UnusedRedeemerEntryRedeemer = Data.Static<
  typeof UnusedRedeemerEntryRedeemerSchema
>;
export const UnusedRedeemerEntryRedeemer =
  asDataType<UnusedRedeemerEntryRedeemer>(UnusedRedeemerEntryRedeemerSchema);

export const UnusedRedeemerWalkDatumSchema = faultProofStepDatumSchema(
  UnusedRedeemerWalkStateSchema,
);
export type UnusedRedeemerWalkDatum = Data.Static<
  typeof UnusedRedeemerWalkDatumSchema
>;
export const UnusedRedeemerWalkDatum = asDataType<UnusedRedeemerWalkDatum>(
  UnusedRedeemerWalkDatumSchema,
);

export const UnusedRedeemerGrammarRedeemerSchema = faultProofStepRedeemerSchema(
  DirectProgressArgsSchema,
);
export type UnusedRedeemerGrammarRedeemer = Data.Static<
  typeof UnusedRedeemerGrammarRedeemerSchema
>;
export const UnusedRedeemerGrammarRedeemer =
  asDataType<UnusedRedeemerGrammarRedeemer>(
    UnusedRedeemerGrammarRedeemerSchema,
  );

export const UnusedRedeemerScanRedeemerSchema = faultProofStepRedeemerSchema(
  DirectScanActionSchema,
);
export type UnusedRedeemerScanRedeemer = Data.Static<
  typeof UnusedRedeemerScanRedeemerSchema
>;
export const UnusedRedeemerScanRedeemer =
  asDataType<UnusedRedeemerScanRedeemer>(UnusedRedeemerScanRedeemerSchema);
