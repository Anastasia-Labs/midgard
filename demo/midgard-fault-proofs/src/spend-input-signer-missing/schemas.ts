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
export const SpendInputSignerAuthenticatedSchema = Data.Object({
  subject: VerdictSubjectSchema,
  transaction_id: Data.Bytes(),
  witness_set_hash: Data.Bytes(),
  payment_credential: Data.Bytes(),
});
export const SpendInputSignerScanSchema = Data.Object({
  authenticated: SpendInputSignerAuthenticatedSchema,
  checkpoint_hash: Data.Bytes(),
});
export const SpendInputSignerVerdictSchema = Data.Object({
  subject: VerdictSubjectSchema,
  signer_required: Data.Boolean(),
  signer_missing: Data.Boolean(),
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
export const SpendInputSignerStep03DatumSchema = faultProofStepDatumSchema(
  SpendInputSignerAuthenticatedSchema,
);
export const SpendInputSignerStep03RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      witnesses_opening: FieldOpeningSchema,
    }),
  );
export const SpendInputSignerStep04DatumSchema = faultProofStepDatumSchema(
  SpendInputSignerScanSchema,
);
export const SpendInputSignerStep04RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      witnesses_opening: FieldOpeningSchema,
      checkpoint_cbor: Data.Bytes(),
    }),
  );
export const SpendInputSignerStep05DatumSchema = faultProofStepDatumSchema(
  SpendInputSignerVerdictSchema,
);
export const SpendInputSignerStep05RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  );
