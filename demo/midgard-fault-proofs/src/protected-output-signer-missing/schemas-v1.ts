import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningV1Schema,
  ForcedInclusionTxV1Schema,
  HeaderV1Schema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  RejectionReasonV1Schema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const ProtectedOutputSignerVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const ProtectedOutputSignerBoundV1Schema = Data.Object({
  subject: ProtectedOutputSignerVerdictSubjectV1Schema,
  output_index: Data.Integer(),
});
export const ProtectedOutputSignerStep02StateV1Schema = Data.Object({
  bound: ProtectedOutputSignerBoundV1Schema,
  witness_set_hash: Data.Bytes(),
});
export const ProtectedOutputSignerCredentialV1Schema = Data.Object({
  subject: ProtectedOutputSignerVerdictSubjectV1Schema,
  transaction_id: Data.Bytes(),
  witness_set_hash: Data.Bytes(),
  output_index: Data.Integer(),
  payment_credential: Data.Bytes(),
});
export const ProtectedOutputSignerScanV1Schema = Data.Object({
  protected: ProtectedOutputSignerCredentialV1Schema,
  checkpoint_hash: Data.Bytes(),
  signer_present: Data.Boolean(),
});
export const ProtectedOutputSignerVerdictV1Schema = Data.Object({
  subject: ProtectedOutputSignerVerdictSubjectV1Schema,
  signer_present: Data.Boolean(),
});
export const ProtectedOutputSignerStep01SourceV1Schema = Data.Enum([
  Data.Object({
    AcceptedSource: Data.Object({ inclusion: NativeTxInclusionCarriageSchema }),
  }),
  Data.Object({
    ForcedSource: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      header: HeaderV1Schema,
      membership: rootMembershipProofSchema(
        OutputReferenceSchema,
        ForcedInclusionTxV1Schema,
      ),
      direction: Data.Integer(),
    }),
  }),
]);
export const ProtectedOutputSignerStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      source: ProtectedOutputSignerStep01SourceV1Schema,
      output_index: Data.Integer(),
    }),
  );
export const ProtectedOutputSignerStep02DatumV1Schema =
  faultProofStepDatumSchema(ProtectedOutputSignerStep02StateV1Schema);
export const ProtectedOutputSignerStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
    }),
  );
export const ProtectedOutputSignerStep03DatumV1Schema =
  faultProofStepDatumSchema(ProtectedOutputSignerCredentialV1Schema);
export const ProtectedOutputSignerStep03RedeemerV1Schema =
  ProtectedOutputSignerStep02RedeemerV1Schema;
export const ProtectedOutputSignerStep04DatumV1Schema =
  faultProofStepDatumSchema(ProtectedOutputSignerScanV1Schema);
export const ProtectedOutputSignerStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      opening: FieldOpeningV1Schema,
      checkpoint_cbor: Data.Bytes(),
    }),
  );
export const ProtectedOutputSignerStep05DatumV1Schema =
  faultProofStepDatumSchema(ProtectedOutputSignerVerdictV1Schema);
export const ProtectedOutputSignerStep05RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  );
