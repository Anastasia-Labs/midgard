import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  ForcedInclusionTxV1Schema,
  HeaderSchema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  TransactionOutputBoundOutputSchema,
  TransactionOutputVerdictSubjectSchema,
} from "./transaction-output-non-canonical.js";

const FrontierPeakSchema = Data.Object({
  height: Data.Integer(),
  hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
});
export const TransactionOutputScanControlSchema = Data.Object({
  version: Data.Integer(),
  stage: Data.Integer(),
  cursor: Data.Integer(),
  map_entry_count: Data.Integer(),
  optional_field_count: Data.Integer(),
  address: Data.Bytes(),
  lovelace: Data.Integer(),
  cardano_value_size: Data.Integer(),
  policy_remaining: Data.Integer(),
  asset_remaining: Data.Integer(),
  policy_asset_cursor: Data.Integer(),
  previous_policy: Data.Bytes(),
  current_policy: Data.Bytes(),
  previous_asset_name: Data.Bytes(),
  asset_count: Data.Integer(),
  asset_peaks: Data.Array(FrontierPeakSchema),
  datum_offset: Data.Integer(),
  datum_length: Data.Integer(),
  payload_remaining: Data.Integer(),
  reference_script_language: Data.Integer(),
  reference_script_item_offset: Data.Integer(),
  reference_script_offset: Data.Integer(),
  reference_script_length: Data.Integer(),
});

export const TransactionOutputStep01SourceSchema = Data.Enum([
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
export const TransactionOutputStep01ArgsSchema = Data.Object({
  source: TransactionOutputStep01SourceSchema,
  output_index: Data.Integer(),
});
export const TransactionOutputStep01RedeemerSchema =
  faultProofStepRedeemerSchema(TransactionOutputStep01ArgsSchema);

export const TransactionOutputStep02DatumSchema = faultProofStepDatumSchema(
  TransactionOutputBoundOutputSchema,
);
export const TransactionOutputStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningSchema,
});
export const TransactionOutputStep02RedeemerSchema =
  faultProofStepRedeemerSchema(TransactionOutputStep02ArgsSchema);
export const TransactionOutputScanStateSchema = Data.Object({
  subject: TransactionOutputVerdictSubjectSchema,
  output_index: Data.Integer(),
  item_length: Data.Integer(),
  item_hash: Data.Bytes(),
  chunk_hashes: Data.Array(Data.Bytes({ minLength: 32, maxLength: 32 })),
  control: TransactionOutputScanControlSchema,
  outcome: Data.Integer(),
});
export const TransactionOutputStep03DatumSchema = faultProofStepDatumSchema(
  TransactionOutputScanStateSchema,
);
export const TransactionOutputStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  window: Data.Bytes(),
});
export const TransactionOutputStep03RedeemerSchema =
  faultProofStepRedeemerSchema(TransactionOutputStep03ArgsSchema);
export const TransactionOutputStep04DatumSchema = faultProofStepDatumSchema(
  TransactionOutputScanStateSchema,
);
export const TransactionOutputStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const TransactionOutputStep04RedeemerSchema =
  faultProofStepRedeemerSchema(TransactionOutputStep04ArgsSchema);
