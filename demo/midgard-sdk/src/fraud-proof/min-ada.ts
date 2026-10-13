/** Q27 `min-ada` non-interactive wire types. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, OutputReferenceSchema } from "../common.js";
import { ForcedInclusionTxV1Schema, HeaderSchema } from "../ledger-state.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";
import { NativeScriptInvalidVerdictSubjectSchema } from "./native-script-invalid.js";

export const MIN_ADA_VIOLATION_ID = "min-ada" as const;

/**
 * `MinAdaFaultV1` has the single constructor `MinAdaTx { output_index }`
 * (Constr 0). Lucid collapses a one-arm `Data.Enum` to its arm, so the
 * constructor is spelled as the object it encodes to.
 */
export const MinAdaFaultSchema = Data.Object({ output_index: Data.Integer() });
export type MinAdaFault = Data.Static<typeof MinAdaFaultSchema>;
export const MinAdaFault = asDataType<MinAdaFault>(MinAdaFaultSchema);

export const MinAdaForcedSourcePayloadSchema = Data.Object({
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
  direction: Data.Integer(),
});

export const MinAdaStep01DatumSchema = faultProofStepDatumSchema(Data.Any());
export const MinAdaStep01ArgsSchema = Data.Object({
  forced_source: Data.Nullable(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      header: HeaderSchema,
      membership: rootMembershipProofSchema(
        OutputReferenceSchema,
        ForcedInclusionTxV1Schema,
      ),
      direction: Data.Integer(),
    }),
  ),
  tx_inclusion: Data.Nullable(NativeTxInclusionCarriageSchema),
  fault: MinAdaFaultSchema,
});
export const MinAdaStep01SpendRedeemerSchema = faultProofStepRedeemerSchema(
  MinAdaStep01ArgsSchema,
);

export const MinAdaStep02StateSchema = Data.Object({
  grammar_checkpoint_hash: Data.Bytes(),
  grammar_complete: Data.Boolean(),
  walk_checkpoint_hash: Data.Bytes(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  bad_tx_id: H32Schema,
  fault: MinAdaFaultSchema,
});
export type MinAdaStep02State = Data.Static<typeof MinAdaStep02StateSchema>;
export const MinAdaStep02State = asDataType<MinAdaStep02State>(
  MinAdaStep02StateSchema,
);

export const MinAdaStep02DatumSchema = faultProofStepDatumSchema(
  MinAdaStep02StateSchema,
);
export const MinAdaStep02ArgsSchema = Data.Object({
  grammar_checkpoint_bytes: Data.Bytes(),
  walk_checkpoint_bytes: Data.Bytes(),
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  yield_to_ref_input_index: Data.Integer(),
  outputs_opening: FieldOpeningSchema,
});
export const MinAdaStep02SpendRedeemerSchema = faultProofStepRedeemerSchema(
  MinAdaStep02ArgsSchema,
);

const FrontierPeakSchema = Data.Object({
  height: Data.Integer(),
  hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
});
export const MinAdaOutputScanControlSchema = Data.Object({
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

export const MinAdaOutputScanStateSchema = Data.Object({
  subject: NativeScriptInvalidVerdictSubjectSchema,
  output_index: Data.Integer(),
  item_length: Data.Integer(),
  item_hash: Data.Bytes(),
  chunk_hashes: Data.Array(Data.Bytes({ minLength: 32, maxLength: 32 })),
  control: MinAdaOutputScanControlSchema,
  outcome: Data.Integer(),
});
export const MinAdaStep03StateSchema = Data.Enum([
  Data.Object({
    MinAdaTxScan: Data.Object({
      direction: Data.Integer(),
      scan: MinAdaOutputScanStateSchema,
    }),
  }),
  Data.Object({
    MinAdaTxDescriptor: Data.Object({
      direction: Data.Integer(),
      total_length: Data.Integer(),
      lovelace: Data.Integer(),
    }),
  }),
]);
export const MinAdaStep03DatumSchema = faultProofStepDatumSchema(
  MinAdaStep03StateSchema,
);
export const MinAdaStep03ArgsSchema = Data.Object({
  window: Data.Bytes(),
  input_index: Data.Integer(),
  output_index: Data.Integer(),
});
export const MinAdaStep03SpendRedeemerSchema = faultProofStepRedeemerSchema(
  MinAdaStep03ArgsSchema,
);

export const MinAdaStep04StateSchema = Data.Enum([
  Data.Literal("PredicateAuthenticated"),
  // Lucid collapses a singleton enum to `void`, which cannot encode the
  // constructor name inside the surrounding nullable step datum. Keep the
  // canonical constructor at index zero; the second arm is never emitted and
  // is rejected by the on-chain step-04 datum decoder.
  Data.Literal("ReservedMinAdaStep04State"),
]);
export const MinAdaStep04DatumSchema = faultProofStepDatumSchema(
  MinAdaStep04StateSchema,
);
export const MinAdaStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const MinAdaStep04SpendRedeemerSchema = faultProofStepRedeemerSchema(
  MinAdaStep04ArgsSchema,
);

export const MIN_ADA_STEP_NAMES = [
  "step_01",
  "step_02",
  "step_03",
  "step_04",
] as const;
