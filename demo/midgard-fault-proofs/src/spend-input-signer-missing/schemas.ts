import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ForcedInclusionTxV1Schema,
  HeaderSchema,
  MembershipCarriageSchema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  RejectionReasonSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const SpendInputSignerVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const SpendInputSignerBoundSchema = Data.Object({
  subject: SpendInputSignerVerdictSubjectSchema,
  input_index: Data.Integer(),
  prior_root: Data.Bytes(),
  witness_set_hash: Data.Bytes(),
});
export const SpendInputSignerAuthenticatedSchema = Data.Object({
  subject: SpendInputSignerVerdictSubjectSchema,
  transaction_id: Data.Bytes(),
  witness_set_hash: Data.Bytes(),
  payment_credential: Data.Bytes(),
});
export const SpendInputSignerScanSchema = Data.Object({
  authenticated: SpendInputSignerAuthenticatedSchema,
  checkpoint_hash: Data.Bytes(),
});
export const SpendInputSignerVerdictSchema = Data.Object({
  subject: SpendInputSignerVerdictSubjectSchema,
  signer_missing: Data.Boolean(),
});
export const SpendInputSignerStep01SourceSchema = Data.Enum([
  Data.Object({
    AcceptedSource: Data.Object({ inclusion: NativeTxInclusionCarriageSchema }),
  }),
  Data.Object({
    ForcedSource: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      header: HeaderSchema,
      membership: rootMembershipProofSchema(
        OutputReferenceSchema,
        ForcedInclusionTxV1Schema,
      ),
      direction: Data.Integer(),
    }),
  }),
]);
export const SpendInputSignerStep01RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      source: SpendInputSignerStep01SourceSchema,
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
