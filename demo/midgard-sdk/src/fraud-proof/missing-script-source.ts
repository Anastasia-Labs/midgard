/** Direct missing-script-source wire: the observed decision
 * (`missing-script-source/observe.ak`). The family entry and step-01 share
 * the direct family wire (`direct-family.ts`). Constructor and field order
 * is wire format. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { DirectSourceSelectionSchema } from "./direct-purpose-workers.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

export const MissingScriptSourceObserveArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
});
export type MissingScriptSourceObserveArgs = Data.Static<
  typeof MissingScriptSourceObserveArgsSchema
>;
export const MissingScriptSourceObserveArgs =
  asDataType<MissingScriptSourceObserveArgs>(
    MissingScriptSourceObserveArgsSchema,
  );

/** The source lookup's selection, whose source the decision requires absent. */
export const MissingScriptSourceObserveDatumSchema = faultProofStepDatumSchema(
  DirectSourceSelectionSchema,
);
export type MissingScriptSourceObserveDatum = Data.Static<
  typeof MissingScriptSourceObserveDatumSchema
>;
export const MissingScriptSourceObserveDatum =
  asDataType<MissingScriptSourceObserveDatum>(
    MissingScriptSourceObserveDatumSchema,
  );

export const MissingScriptSourceObserveRedeemerSchema =
  faultProofStepRedeemerSchema(MissingScriptSourceObserveArgsSchema);
export type MissingScriptSourceObserveRedeemer = Data.Static<
  typeof MissingScriptSourceObserveRedeemerSchema
>;
export const MissingScriptSourceObserveRedeemer =
  asDataType<MissingScriptSourceObserveRedeemer>(
    MissingScriptSourceObserveRedeemerSchema,
  );
