import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ProofThreadSubjectSourceSchema,
  VerdictSubjectSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const ProtectedOutputSignerBoundSchema = Data.Object({
  subject: VerdictSubjectSchema,
  output_index: Data.Integer(),
});
export const ProtectedOutputSignerStep02StateSchema = Data.Object({
  bound: ProtectedOutputSignerBoundSchema,
  witness_set_hash: Data.Bytes(),
});
export const ProtectedOutputSignerStep01RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      source: ProofThreadSubjectSourceSchema,
      output_index: Data.Integer(),
    }),
  );
export const ProtectedOutputSignerStep02DatumSchema = faultProofStepDatumSchema(
  ProtectedOutputSignerStep02StateSchema,
);
export const ProtectedOutputSignerStep02RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      coordinate_out_of_range: Data.Boolean(),
      grammar_ref_input_index: Data.Nullable(Data.Integer()),
    }),
  );
