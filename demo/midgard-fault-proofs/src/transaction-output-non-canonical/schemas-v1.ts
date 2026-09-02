import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningV1Schema,
  ForcedInclusionTxV1Schema,
  HeaderV1Schema,
  NativeTxInclusionCarriageSchema,
  OutputReferenceSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  TransactionOutputBoundOutputV1Schema,
  TransactionOutputVerdictSubjectV1Schema,
} from "./transaction-output-non-canonical-v1.js";

const FrontierPeakV1Schema = Data.Object({
  height: Data.Integer(),
  hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
});
export const TransactionOutputScanControlV1Schema = Data.Object({
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
  asset_peaks: Data.Array(FrontierPeakV1Schema),
  datum_offset: Data.Integer(),
  datum_length: Data.Integer(),
  payload_remaining: Data.Integer(),
  reference_script_language: Data.Integer(),
  reference_script_item_offset: Data.Integer(),
  reference_script_offset: Data.Integer(),
  reference_script_length: Data.Integer(),
});

export const TransactionOutputStep01SourceV1Schema = Data.Enum([
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
export const TransactionOutputStep01ArgsV1Schema = Data.Object({
  source: TransactionOutputStep01SourceV1Schema,
  output_index: Data.Integer(),
});
export const TransactionOutputStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(TransactionOutputStep01ArgsV1Schema);

export const TransactionOutputStep02DatumV1Schema = faultProofStepDatumSchema(
  TransactionOutputBoundOutputV1Schema,
);
export const TransactionOutputStep02ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  opening: FieldOpeningV1Schema,
});
export const TransactionOutputStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(TransactionOutputStep02ArgsV1Schema);
export const TransactionOutputScanStateV1Schema = Data.Object({
  subject: TransactionOutputVerdictSubjectV1Schema,
  output_index: Data.Integer(),
  item_length: Data.Integer(),
  item_hash: Data.Bytes(),
  chunk_hashes: Data.Array(Data.Bytes({ minLength: 32, maxLength: 32 })),
  control: TransactionOutputScanControlV1Schema,
  outcome: Data.Integer(),
});
export const TransactionOutputStep03DatumV1Schema = faultProofStepDatumSchema(
  TransactionOutputScanStateV1Schema,
);
export const TransactionOutputStep03ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  window: Data.Bytes(),
});
export const TransactionOutputStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(TransactionOutputStep03ArgsV1Schema);
export const TransactionOutputStep04DatumV1Schema = faultProofStepDatumSchema(
  TransactionOutputScanStateV1Schema,
);
export const TransactionOutputStep04ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const TransactionOutputStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(TransactionOutputStep04ArgsV1Schema);
