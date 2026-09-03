import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ForcedInclusionTxV1Schema,
  HeaderSchema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  RejectionReasonSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const ProtectedOutputSignerVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const ProtectedOutputSignerBoundSchema = Data.Object({
  subject: ProtectedOutputSignerVerdictSubjectSchema,
  output_index: Data.Integer(),
});
export const ProtectedOutputSignerStep02StateSchema = Data.Object({
  bound: ProtectedOutputSignerBoundSchema,
  witness_set_hash: Data.Bytes(),
});
export const ProtectedOutputSignerCredentialSchema = Data.Object({
  subject: ProtectedOutputSignerVerdictSubjectSchema,
  transaction_id: Data.Bytes(),
  witness_set_hash: Data.Bytes(),
  output_index: Data.Integer(),
  payment_credential: Data.Bytes(),
});
export const ProtectedOutputSignerScanSchema = Data.Object({
  protected: ProtectedOutputSignerCredentialSchema,
  checkpoint_hash: Data.Bytes(),
  signer_present: Data.Boolean(),
});
export const ProtectedOutputSignerVerdictSchema = Data.Object({
  subject: ProtectedOutputSignerVerdictSubjectSchema,
  signer_present: Data.Boolean(),
});
export const ProtectedOutputSignerStep01SourceSchema = Data.Enum([
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
export const ProtectedOutputSignerStep01RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      source: ProtectedOutputSignerStep01SourceSchema,
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
    }),
  );
export const ProtectedOutputSignerStep03DatumSchema = faultProofStepDatumSchema(
  ProtectedOutputSignerCredentialSchema,
);
export const ProtectedOutputSignerStep03RedeemerSchema =
  ProtectedOutputSignerStep02RedeemerSchema;
export const ProtectedOutputSignerStep04DatumSchema = faultProofStepDatumSchema(
  ProtectedOutputSignerScanSchema,
);
export const ProtectedOutputSignerStep04RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningSchema,
      checkpoint_cbor: Data.Bytes(),
    }),
  );
export const ProtectedOutputSignerStep05DatumSchema = faultProofStepDatumSchema(
  ProtectedOutputSignerVerdictSchema,
);
export const ProtectedOutputSignerStep05RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  );
