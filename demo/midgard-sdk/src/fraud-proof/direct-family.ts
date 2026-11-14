/** Wire shared by the direct purpose families: step-01
 * (`direct-script-derivation/step-01.ak`) and the observe-or-claim family
 * entry (`<family>/entry.ak`). Constructor and field order is wire format. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { faultProofStepRedeemerSchema } from "./native.js";
import { ProofThreadSubjectSourceSchema } from "./proof-thread-source.js";

export const DirectStep01ArgsSchema = Data.Object({
  source: ProofThreadSubjectSourceSchema,
});
export type DirectStep01Args = Data.Static<typeof DirectStep01ArgsSchema>;
export const DirectStep01Args = asDataType<DirectStep01Args>(
  DirectStep01ArgsSchema,
);

export const DirectStep01RedeemerSchema = faultProofStepRedeemerSchema(
  DirectStep01ArgsSchema,
);
export type DirectStep01Redeemer = Data.Static<
  typeof DirectStep01RedeemerSchema
>;
export const DirectStep01Redeemer = asDataType<DirectStep01Redeemer>(
  DirectStep01RedeemerSchema,
);

const indices = { input_index: Data.Integer(), output_index: Data.Integer() };

/** 0 Observe, 1 Claim. */
export const DirectFamilyEntryActionSchema = Data.Enum([
  Data.Object({ Observe: Data.Object(indices) }),
  Data.Object({ Claim: Data.Object(indices) }),
]);
export type DirectFamilyEntryAction = Data.Static<
  typeof DirectFamilyEntryActionSchema
>;
export const DirectFamilyEntryAction = asDataType<DirectFamilyEntryAction>(
  DirectFamilyEntryActionSchema,
);

export const DirectFamilyEntryRedeemerSchema = faultProofStepRedeemerSchema(
  DirectFamilyEntryActionSchema,
);
export type DirectFamilyEntryRedeemer = Data.Static<
  typeof DirectFamilyEntryRedeemerSchema
>;
export const DirectFamilyEntryRedeemer = asDataType<DirectFamilyEntryRedeemer>(
  DirectFamilyEntryRedeemerSchema,
);
