import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  MinAdaOutputScanControlSchema,
  ProofThreadSubjectSourceSchema,
  StreamFrameSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  TransactionOutputBoundOutputSchema,
  TransactionOutputVerdictSubjectSchema,
} from "./transaction-output-non-canonical.js";

export const TransactionOutputScanControlSchema = MinAdaOutputScanControlSchema;

export const TransactionOutputStep01ArgsSchema = Data.Object({
  source: ProofThreadSubjectSourceSchema,
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
  grammar_ref_input_index: Data.Nullable(Data.Integer()),
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
export const TransactionOutputDatumScanStateSchema = Data.Object({
  scan: TransactionOutputScanStateSchema,
  cursor: Data.Integer(),
  stack_root: Data.Bytes(),
});
export const TransactionOutputStep03DatumDatumSchema =
  faultProofStepDatumSchema(TransactionOutputDatumScanStateSchema);
export const TransactionOutputStep03DatumArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  window: Data.Bytes(),
  revealed: Data.Array(StreamFrameSchema),
  tail_root: Data.Bytes(),
  node_budget: Data.Integer(),
});
export const TransactionOutputStep03DatumRedeemerSchema =
  faultProofStepRedeemerSchema(TransactionOutputStep03DatumArgsSchema);
