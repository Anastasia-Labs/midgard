import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  MembershipCarriageSchema,
  ProofThreadSubjectSourceSchema,
  VerdictSubjectSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const SpendInputSignerBoundSchema = Data.Object({
  subject: VerdictSubjectSchema,
  input_index: Data.Integer(),
  prior_root: Data.Bytes(),
  witness_set_hash: Data.Bytes(),
});
export const SpendInputSignerStep01RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      source: ProofThreadSubjectSourceSchema,
      input_index: Data.Integer(),
    }),
  );
export const SpendInputSignerStep02DatumSchema = faultProofStepDatumSchema(
  SpendInputSignerBoundSchema,
);
export const SpendInputSignerStep02RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      spend_inputs_opening: FieldOpeningSchema,
      descriptor_cbor: Data.Bytes(),
      membership: MembershipCarriageSchema,
    }),
  );
