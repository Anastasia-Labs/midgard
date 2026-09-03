/** Q33 `missing-native-script-utxo` non-interactive wire types. */
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, OutputReferenceSchema } from "../common.js";
import { FieldOpeningSchema } from "./field-opening-v1.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MembershipCarriageSchema,
  MidgardTxInputSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";

export const MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID =
  "missing-native-script-utxo" as const;

export const MissingNativeScriptUtxoStep01DatumSchema =
  faultProofStepDatumSchema(Data.Any());
export const MissingNativeScriptUtxoStep01ArgsSchema = Data.Object({
  carriage: NativeTxInclusionCarriageSchema,
});
export const MissingNativeScriptUtxoStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptUtxoStep01ArgsSchema);

export const MissingNativeScriptUtxoStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
  prev_utxos_root: H32Schema,
});
export const MissingNativeScriptUtxoStep02DatumSchema =
  faultProofStepDatumSchema(MissingNativeScriptUtxoStep02StateSchema);
export const MissingNativeScriptUtxoStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  bad_input_index: Data.Integer(),
  spend_inputs_opening: FieldOpeningSchema,
});
export const MissingNativeScriptUtxoStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptUtxoStep02ArgsSchema);

export const MissingNativeScriptUtxoStep03StateSchema = Data.Object({
  input_with_missing_script: MidgardTxInputSchema,
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
  prev_utxos_root: H32Schema,
});
export const MissingNativeScriptUtxoStep03DatumSchema =
  faultProofStepDatumSchema(MissingNativeScriptUtxoStep03StateSchema);
export const MissingNativeScriptUtxoStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  out_ref: OutputReferenceSchema,
  descriptor_cbor: Data.Bytes(),
  membership: MembershipCarriageSchema,
});
export const MissingNativeScriptUtxoStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptUtxoStep03ArgsSchema);

export const MissingNativeScriptUtxoStep04StateSchema = Data.Object({
  out_ref: OutputReferenceSchema,
  descriptor_cbor: Data.Bytes(),
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
});
export const MissingNativeScriptUtxoStep04DatumSchema =
  faultProofStepDatumSchema(MissingNativeScriptUtxoStep04StateSchema);
export const MissingNativeScriptUtxoStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  missing_native_script_bytes: Data.Bytes(),
});
export const MissingNativeScriptUtxoStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptUtxoStep04ArgsSchema);

export const MissingNativeScriptUtxoPhaseSchema = Data.Enum([
  Data.Literal("Ready"),
  Data.Object({
    GrammarCertification: Data.Object({ checkpoint_hash: H32Schema }),
  }),
  Data.Object({
    SemanticScan: Data.Object({
      checkpoint_hash: H32Schema,
      required_script_is_present: Data.Boolean(),
    }),
  }),
]);
export type MissingNativeScriptUtxoPhase = Data.Static<
  typeof MissingNativeScriptUtxoPhaseSchema
>;

export const MissingNativeScriptUtxoStep05StateSchema = Data.Object({
  expected_missing_script_hash: Data.Bytes(),
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
  phase: MissingNativeScriptUtxoPhaseSchema,
});
export const MissingNativeScriptUtxoStep05DatumSchema =
  faultProofStepDatumSchema(MissingNativeScriptUtxoStep05StateSchema);
export const MissingNativeScriptUtxoStep05ArgsSchema = Data.Enum([
  Data.Object({
    DirectFinalize: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
    }),
  }),
  Data.Object({
    StartGrammarCertification: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
      item_budget: Data.Integer(),
    }),
  }),
]);
export const MissingNativeScriptUtxoStep05SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptUtxoStep05ArgsSchema);

export const MissingNativeScriptUtxoStep06DatumSchema =
  faultProofStepDatumSchema(MissingNativeScriptUtxoStep05StateSchema);
export const MissingNativeScriptUtxoStep06ArgsSchema = Data.Enum([
  Data.Object({
    ResumeGrammarCertification: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    StartSemanticScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
      grammar_checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
]);
export const MissingNativeScriptUtxoStep06SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptUtxoStep06ArgsSchema);

export const MissingNativeScriptUtxoStep07DatumSchema =
  faultProofStepDatumSchema(MissingNativeScriptUtxoStep05StateSchema);
export const MissingNativeScriptUtxoStep07ArgsSchema = Data.Enum([
  Data.Object({
    ResumeSemanticScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    FinalizeSemanticScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
]);
export const MissingNativeScriptUtxoStep07SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptUtxoStep07ArgsSchema);

export const MISSING_NATIVE_SCRIPT_UTXO_STEP_NAMES = [
  "step_01",
  "step_02",
  "step_03",
  "step_04",
  "step_05",
  "step_06",
  "step_07",
] as const;
