/** Q34 `native-script-invalid` non-interactive wire types. */
import { Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { H32Schema } from "../common.js";
import { FieldOpeningV1Schema } from "./field-opening-v1.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";
import {
  FrontierPeakV1Schema,
  NativeScriptPushdownFrameV1Schema,
  SignerSetProofV1Schema,
} from "./validation-auxiliary-witness-v1.js";

export const NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1 =
  "native-script-invalid" as const;

/** Exact twin of Aiken `blake2b_256(script_item_cbor)`. */
export const nativeScriptItemCommitmentV1 = (item: Uint8Array): string =>
  Buffer.from(blake2b(item, { dkLen: 32 })).toString("hex");

export const NativeScriptInvalidStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export const NativeScriptInvalidStep01ArgsSchema = Data.Object({
  carriage: NativeTxInclusionCarriageSchema,
});
export const NativeScriptInvalidStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptInvalidStep01ArgsSchema);

export const NativeScriptInvalidStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
});
export const NativeScriptInvalidStep02DatumSchema = faultProofStepDatumSchema(
  NativeScriptInvalidStep02StateSchema,
);
export const NativeScriptInvalidStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  script_index: Data.Integer(),
  script_tx_wits_opening: FieldOpeningV1Schema,
});
export const NativeScriptInvalidStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptInvalidStep02ArgsSchema);

export const NativeScriptInvalidStep03StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
  script_item_hash: H32Schema,
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
});
export const NativeScriptInvalidStep03DatumSchema = faultProofStepDatumSchema(
  NativeScriptInvalidStep03StateSchema,
);
export const NativeScriptInvalidStep03ArgsSchema = Data.Enum([
  Data.Object({
    DirectFinalize: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
      script_item_cbor: Data.Bytes(),
      addr_tx_wits_opening: FieldOpeningV1Schema,
    }),
  }),
  Data.Object({
    StartSignerScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_item_cbor: Data.Bytes(),
      addr_tx_wits_opening: FieldOpeningV1Schema,
      item_budget: Data.Integer(),
    }),
  }),
]);
export const NativeScriptInvalidStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptInvalidStep03ArgsSchema);

export const NativeScriptInvalidStep04StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
  script_item_hash: H32Schema,
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
  signer_checkpoint_hash: H32Schema,
  previous_signer_hash: Data.Bytes(),
  signer_count: Data.Integer(),
  signer_peaks: Data.Array(FrontierPeakV1Schema),
});
export const NativeScriptInvalidStep04DatumSchema = faultProofStepDatumSchema(
  NativeScriptInvalidStep04StateSchema,
);
export const NativeScriptInvalidStep04ArgsSchema = Data.Enum([
  Data.Object({
    ResumeSignerScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      addr_tx_wits_opening: FieldOpeningV1Schema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    FinalizeSignerScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      addr_tx_wits_opening: FieldOpeningV1Schema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
]);
export const NativeScriptInvalidStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptInvalidStep04ArgsSchema);

export const NativeScriptInvalidStep05PhaseV1Schema = Data.Enum([
  Data.Literal("ScriptReady"),
  Data.Object({ ScriptWalk: Data.Object({ cursor_hash: H32Schema }) }),
]);
export const NativeScriptInvalidStep05StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  script_item_hash: H32Schema,
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
  signer_count: Data.Integer(),
  signer_peaks: Data.Array(FrontierPeakV1Schema),
  phase: NativeScriptInvalidStep05PhaseV1Schema,
});
export const NativeScriptInvalidSignerQueryV1Schema = Data.Object({
  signer_hash: Data.Bytes(),
  proof: SignerSetProofV1Schema,
});
export const NativeScriptInvalidStep05DatumSchema = faultProofStepDatumSchema(
  NativeScriptInvalidStep05StateSchema,
);
const NativeScriptInvalidScriptScanBaseV1Schema = {
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  script_item_cbor: Data.Bytes(),
  node_budget: Data.Integer(),
  signer_queries: Data.Array(NativeScriptInvalidSignerQueryV1Schema),
};
export const NativeScriptInvalidStep05ArgsSchema = Data.Enum([
  Data.Object({
    StartScriptScan: Data.Object(NativeScriptInvalidScriptScanBaseV1Schema),
  }),
  Data.Object({
    ResumeScriptScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_item_cbor: Data.Bytes(),
      cursor_bytes: Data.Bytes(),
      frames: Data.Array(NativeScriptPushdownFrameV1Schema),
      node_budget: Data.Integer(),
      signer_queries: Data.Array(NativeScriptInvalidSignerQueryV1Schema),
    }),
  }),
  Data.Object({
    StartScriptFinalize: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
      script_item_cbor: Data.Bytes(),
      node_budget: Data.Integer(),
      signer_queries: Data.Array(NativeScriptInvalidSignerQueryV1Schema),
    }),
  }),
  Data.Object({
    FinalizeScriptScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
      script_item_cbor: Data.Bytes(),
      cursor_bytes: Data.Bytes(),
      frames: Data.Array(NativeScriptPushdownFrameV1Schema),
      node_budget: Data.Integer(),
      signer_queries: Data.Array(NativeScriptInvalidSignerQueryV1Schema),
    }),
  }),
]);
export const NativeScriptInvalidStep05SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptInvalidStep05ArgsSchema);

export const NATIVE_SCRIPT_INVALID_STEP_NAMES_V1 = [
  "step_01",
  "step_02",
  "step_03",
  "step_04",
  "step_05",
] as const;
