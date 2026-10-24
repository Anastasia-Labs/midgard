import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { ScriptHashSchema } from "../common.js";
import {
  DirectFieldCursorSchema,
  DirectResolvedSourceWitnessSchema,
  DirectScriptBoundSchema,
  DirectScriptSourceSchema,
} from "./direct-script-derivation.js";
import {
  DirectSourceAdmissionSchema,
  DirectSourceIndexSchema,
} from "./direct-source-index.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

export const DirectSourceScanStateSchema = Data.Object({
  cursor: DirectFieldCursorSchema,
  source_index: DirectSourceIndexSchema,
  complete: Data.Boolean(),
});
export type DirectSourceScanState = Data.Static<
  typeof DirectSourceScanStateSchema
>;
export const DirectSourceScanState = asDataType<DirectSourceScanState>(
  DirectSourceScanStateSchema,
);
export const DirectSourceBoundIndexSchema = Data.Object({
  bound: DirectScriptBoundSchema,
  source_index: DirectSourceIndexSchema,
});
export type DirectSourceBoundIndex = Data.Static<
  typeof DirectSourceBoundIndexSchema
>;
export const DirectSourceBoundIndex = asDataType<DirectSourceBoundIndex>(
  DirectSourceBoundIndexSchema,
);
export const DirectSourceResumeStateSchema = Data.Object({
  resume_script_hash: ScriptHashSchema,
  state: DirectSourceScanStateSchema,
});
export const DirectSourceResolvedStateSchema = Data.Object({
  resume_script_hash: ScriptHashSchema,
  resolved: Data.Object({
    advanced: DirectSourceScanStateSchema,
    first_item_index: Data.Integer(),
    out_refs: Data.Array(Data.Bytes()),
  }),
});
export type DirectSourceResolvedState = Data.Static<
  typeof DirectSourceResolvedStateSchema
>;
export const DirectSourceResolvedState = asDataType<DirectSourceResolvedState>(
  DirectSourceResolvedStateSchema,
);
export const DirectSourcePendingStateSchema = Data.Object({
  resume_script_hash: ScriptHashSchema,
  pending: Data.Object({
    advanced: DirectSourceScanStateSchema,
    candidates: Data.Array(Data.Nullable(DirectScriptSourceSchema)),
  }),
});
export type DirectSourcePendingState = Data.Static<
  typeof DirectSourcePendingStateSchema
>;
export const DirectSourcePendingState = asDataType<DirectSourcePendingState>(
  DirectSourcePendingStateSchema,
);
const indices = { input_index: Data.Integer(), output_index: Data.Integer() };
const opened = {
  ...indices,
  opening: FieldOpeningSchema,
  checkpoint: Data.Bytes(),
};
export const DirectSourceStartArgsSchema = Data.Object({
  ...indices,
  opening: FieldOpeningSchema,
});
export const DirectSourceResolveActionSchema = Data.Enum([
  Data.Object({
    ResolveScan: Data.Object({
      ...opened,
      budget: Data.Integer(),
    }),
  }),
  Data.Object({ Advance: Data.Object(indices) }),
]);
export const DirectSourceAuthenticateArgsSchema = Data.Object({
  ...indices,
  witnesses: Data.Array(DirectResolvedSourceWitnessSchema),
});
export const DirectSourceMapArgsSchema = Data.Object({
  ...indices,
  admissions: Data.Array(Data.Nullable(DirectSourceAdmissionSchema)),
});
export const DirectSourceInputFinishArgsSchema = Data.Object({
  ...opened,
  next_opening: FieldOpeningSchema,
});
export const DirectSourceFinishArgsSchema = Data.Object(opened);
export const DirectSourceGrammarArgsSchema = Data.Object({
  ...opened,
  budget: Data.Integer(),
});
export const DirectSourceWitnessActionSchema = Data.Enum([
  Data.Object({
    WitnessScan: Data.Object({ ...opened, budget: Data.Integer() }),
  }),
  Data.Object({ Finish: Data.Object(indices) }),
]);
export const DirectSourceResumeDatumSchema = faultProofStepDatumSchema(
  DirectSourceResumeStateSchema,
);
export type DirectSourceResumeDatum = Data.Static<
  typeof DirectSourceResumeDatumSchema
>;
export const DirectSourceResumeDatum = asDataType<DirectSourceResumeDatum>(
  DirectSourceResumeDatumSchema,
);
export const DirectSourceScanDatumSchema = faultProofStepDatumSchema(
  DirectSourceScanStateSchema,
);
export type DirectSourceScanDatum = Data.Static<
  typeof DirectSourceScanDatumSchema
>;
export const DirectSourceScanDatum = asDataType<DirectSourceScanDatum>(
  DirectSourceScanDatumSchema,
);
export const DirectSourcePendingDatumSchema = faultProofStepDatumSchema(
  DirectSourcePendingStateSchema,
);
export type DirectSourcePendingDatum = Data.Static<
  typeof DirectSourcePendingDatumSchema
>;
export const DirectSourcePendingDatum = asDataType<DirectSourcePendingDatum>(
  DirectSourcePendingDatumSchema,
);
export const DirectSourceBoundIndexDatumSchema = faultProofStepDatumSchema(
  DirectSourceBoundIndexSchema,
);
export type DirectSourceBoundIndexDatum = Data.Static<
  typeof DirectSourceBoundIndexDatumSchema
>;
export const DirectSourceBoundIndexDatum =
  asDataType<DirectSourceBoundIndexDatum>(DirectSourceBoundIndexDatumSchema);
export const DirectSourceStartRedeemerSchema = faultProofStepRedeemerSchema(
  DirectSourceStartArgsSchema,
);
export type DirectSourceStartRedeemer = Data.Static<
  typeof DirectSourceStartRedeemerSchema
>;
export const DirectSourceStartRedeemer = asDataType<DirectSourceStartRedeemer>(
  DirectSourceStartRedeemerSchema,
);
export const DirectSourceResolveRedeemerSchema = faultProofStepRedeemerSchema(
  DirectSourceResolveActionSchema,
);
export type DirectSourceResolveRedeemer = Data.Static<
  typeof DirectSourceResolveRedeemerSchema
>;
export const DirectSourceResolveRedeemer =
  asDataType<DirectSourceResolveRedeemer>(DirectSourceResolveRedeemerSchema);
export const DirectSourceMapRedeemerSchema = faultProofStepRedeemerSchema(
  DirectSourceMapArgsSchema,
);
export type DirectSourceMapRedeemer = Data.Static<
  typeof DirectSourceMapRedeemerSchema
>;
export const DirectSourceMapRedeemer = asDataType<DirectSourceMapRedeemer>(
  DirectSourceMapRedeemerSchema,
);
export const DirectSourceGrammarRedeemerSchema = faultProofStepRedeemerSchema(
  DirectSourceGrammarArgsSchema,
);
export type DirectSourceGrammarRedeemer = Data.Static<
  typeof DirectSourceGrammarRedeemerSchema
>;
export const DirectSourceGrammarRedeemer =
  asDataType<DirectSourceGrammarRedeemer>(DirectSourceGrammarRedeemerSchema);
export const DirectSourceWitnessRedeemerSchema = faultProofStepRedeemerSchema(
  DirectSourceWitnessActionSchema,
);
export type DirectSourceWitnessRedeemer = Data.Static<
  typeof DirectSourceWitnessRedeemerSchema
>;
export const DirectSourceWitnessRedeemer =
  asDataType<DirectSourceWitnessRedeemer>(DirectSourceWitnessRedeemerSchema);

export const DirectSourceInputFinishRedeemerSchema =
  faultProofStepRedeemerSchema(DirectSourceInputFinishArgsSchema);
export type DirectSourceInputFinishRedeemer = Data.Static<
  typeof DirectSourceInputFinishRedeemerSchema
>;
export const DirectSourceInputFinishRedeemer =
  asDataType<DirectSourceInputFinishRedeemer>(
    DirectSourceInputFinishRedeemerSchema,
  );
export const DirectSourceFinishRedeemerSchema = faultProofStepRedeemerSchema(
  DirectSourceFinishArgsSchema,
);
export type DirectSourceFinishRedeemer = Data.Static<
  typeof DirectSourceFinishRedeemerSchema
>;
export const DirectSourceFinishRedeemer =
  asDataType<DirectSourceFinishRedeemer>(DirectSourceFinishRedeemerSchema);

export const DirectSourceResolvedDatumSchema = faultProofStepDatumSchema(
  DirectSourceResolvedStateSchema,
);
export type DirectSourceResolvedDatum = Data.Static<
  typeof DirectSourceResolvedDatumSchema
>;
export const DirectSourceResolvedDatum = asDataType<DirectSourceResolvedDatum>(
  DirectSourceResolvedDatumSchema,
);
export const DirectSourceAuthenticateRedeemerSchema =
  faultProofStepRedeemerSchema(DirectSourceAuthenticateArgsSchema);
export type DirectSourceAuthenticateRedeemer = Data.Static<
  typeof DirectSourceAuthenticateRedeemerSchema
>;
export const DirectSourceAuthenticateRedeemer =
  asDataType<DirectSourceAuthenticateRedeemer>(
    DirectSourceAuthenticateRedeemerSchema,
  );
