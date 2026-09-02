import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningV1Schema,
  ForcedInclusionTxV1Schema,
  HeaderV1Schema,
  MembershipCarriageSchema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  RejectionReasonV1Schema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const SpendInputSignerVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const SpendInputSignerBoundV1Schema = Data.Object({
  subject: SpendInputSignerVerdictSubjectV1Schema,
  input_index: Data.Integer(),
  prior_root: Data.Bytes(),
  witness_set_hash: Data.Bytes(),
});
export const SpendInputSignerAuthenticatedV1Schema = Data.Object({
  subject: SpendInputSignerVerdictSubjectV1Schema,
  transaction_id: Data.Bytes(),
  witness_set_hash: Data.Bytes(),
  payment_credential: Data.Bytes(),
});
export const SpendInputSignerScanV1Schema = Data.Object({
  authenticated: SpendInputSignerAuthenticatedV1Schema,
  checkpoint_hash: Data.Bytes(),
});
export const SpendInputSignerVerdictV1Schema = Data.Object({
  subject: SpendInputSignerVerdictSubjectV1Schema,
  signer_missing: Data.Boolean(),
});
export const SpendInputSignerStep01SourceV1Schema = Data.Enum([
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
export const SpendInputSignerStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      source: SpendInputSignerStep01SourceV1Schema,
      input_index: Data.Integer(),
    }),
  );
export const SpendInputSignerStep02DatumV1Schema = faultProofStepDatumSchema(
  SpendInputSignerBoundV1Schema,
);
export const SpendInputSignerStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      spend_inputs_opening: FieldOpeningV1Schema,
      descriptor_cbor: Data.Bytes(),
      membership: MembershipCarriageSchema,
    }),
  );
export const SpendInputSignerStep03DatumV1Schema = faultProofStepDatumSchema(
  SpendInputSignerAuthenticatedV1Schema,
);
export const SpendInputSignerStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      witnesses_opening: FieldOpeningV1Schema,
    }),
  );
export const SpendInputSignerStep04DatumV1Schema = faultProofStepDatumSchema(
  SpendInputSignerScanV1Schema,
);
export const SpendInputSignerStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      witnesses_opening: FieldOpeningV1Schema,
      checkpoint_cbor: Data.Bytes(),
    }),
  );
export const SpendInputSignerStep05DatumV1Schema = faultProofStepDatumSchema(
  SpendInputSignerVerdictV1Schema,
);
export const SpendInputSignerStep05RedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  );
