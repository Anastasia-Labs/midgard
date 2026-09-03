import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningSchema,
  FrontierPeakSchema,
  H32Schema,
  MembershipCarriageSchema,
  NativeScriptInvalidSignerQuerySchema,
  NativeScriptInvalidStep03ArgsSchema,
  NativeScriptInvalidStep04ArgsSchema,
  NativeScriptInvalidStep05ArgsSchema,
  NativeScriptInvalidStep05PhaseSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export {
  ExecutionSourceStep01RedeemerSchema as ExecutionNativeScriptInvalidStep01RedeemerSchema,
  ExecutionSourceStep02RedeemerSchema as ExecutionNativeScriptInvalidStep02RedeemerSchema,
  NativeScriptsControlSchema,
} from "../execution-source-script-decoding/schemas-v1.js";
import {
  ExecutionSourceStep01RedeemerSchema,
  ExecutionSourceStep02RedeemerSchema,
  ExecutionSourceVerdictSubjectSchema,
} from "../execution-source-script-decoding/schemas-v1.js";

export const ExecutionNativeScriptInvalidBoundSchema = Data.Object({
  subject: ExecutionSourceVerdictSubjectSchema,
  validation_traces_root: H32Schema,
  validation_trace_count: Data.Integer(),
  execution_index: Data.Integer(),
  accused_class: Data.Integer(),
  prior_ledger_root: H32Schema,
  compact_cbor: Data.Bytes(),
});

export const ExecutionNativeScriptInvalidStep02DatumSchema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidBoundSchema);

export const AcceptedSelectedPurposeSchema = Data.Object({
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
  script_hash: Data.Bytes(),
  subject: Data.Bytes(),
});
export const AcceptedSelectedSourceSchema = Data.Object({
  source_index: Data.Integer(),
  origin_kind: Data.Integer(),
  source_key: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  total_length: Data.Integer(),
  item_commitment: H32Schema,
});
export const ExecutionNativeScriptInvalidAcceptedStateSchema = Data.Object({
  bound: ExecutionNativeScriptInvalidBoundSchema,
  phase: Data.Integer(),
  field_cursor: Data.Integer(),
  execution_cursor: Data.Integer(),
  previous_key: Data.Bytes(),
  receive_candidate: Data.Bytes(),
  source_base_index: Data.Integer(),
  source_cursor: Data.Integer(),
  selected_purpose: Data.Nullable(AcceptedSelectedPurposeSchema),
  selected_source: Data.Nullable(AcceptedSelectedSourceSchema),
  next_expected_script_hash: Data.Bytes(),
  checkpoint_hash: H32Schema,
});
export const ExecutionNativeScriptInvalidAcceptedDatumSchema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidAcceptedStateSchema);
export const ExecutionNativeScriptInvalidAcceptedInitRedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({ input_index: Data.Integer(), output_index: Data.Integer() }),
  );
const acceptedFieldAction = (
  scanName: string,
  finishName: string,
  openingName: string,
) =>
  faultProofStepRedeemerSchema(
    Data.Enum([
      Data.Object({
        [scanName]: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          [openingName]: FieldOpeningSchema,
        }),
      }),
      Data.Object({
        [finishName]: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          [openingName]: FieldOpeningSchema,
        }),
      }),
    ]),
  );
export const ExecutionNativeScriptInvalidAcceptedSpendRedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Enum([
      Data.Object({
        ScanSpend: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          spend_inputs_opening: FieldOpeningSchema,
          descriptor_cbor: Data.Bytes(),
          membership: MembershipCarriageSchema,
        }),
      }),
      Data.Object({
        FinishSpends: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          spend_inputs_opening: FieldOpeningSchema,
        }),
      }),
    ]),
  );
export const ExecutionNativeScriptInvalidAcceptedMintRedeemerSchema =
  acceptedFieldAction("ScanMint", "FinishMint", "mint_opening");
export const ExecutionNativeScriptInvalidAcceptedObserverRedeemerSchema =
  acceptedFieldAction("ScanObserver", "FinishObservers", "observer_opening");
export const ExecutionNativeScriptInvalidAcceptedReceiveRedeemerSchema =
  acceptedFieldAction("ScanOutput", "FinishOutputPass", "outputs_opening");
export const ExecutionNativeScriptInvalidAcceptedInlineRedeemerSchema =
  acceptedFieldAction("ScanInline", "FinishInline", "scripts_opening");
export const ExecutionNativeScriptInvalidAcceptedReferenceRedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      reference_inputs_opening: FieldOpeningSchema,
      descriptor_cbor: Data.Bytes(),
      membership: MembershipCarriageSchema,
    }),
  );
export const ExecutionNativeScriptInvalidSourceSchema = Data.Object({
  bound: ExecutionNativeScriptInvalidBoundSchema,
  prior_ledger_root: H32Schema,
  source_index: Data.Integer(),
  origin_kind: Data.Integer(),
  source_key: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  total_length: Data.Integer(),
  item_commitment: H32Schema,
  compact_cbor: Data.Bytes(),
});
export const ExecutionNativeScriptInvalidStep03DatumSchema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidSourceSchema);
export const ExecutionNativeScriptInvalidStep03RedeemerSchema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_item_cbor: Data.Bytes(),
    }),
  );

const identity = {
  direction: Data.Integer(),
  execution_index: Data.Integer(),
  source_index: Data.Integer(),
  origin_kind: Data.Integer(),
  item_commitment: H32Schema,
};
export const ExecutionNativeScriptInvalidStep04StateSchema = Data.Object({
  ...identity,
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
  script_item_hash: H32Schema,
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
});
export const ExecutionNativeScriptInvalidStep04DatumSchema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidStep04StateSchema);
export const ExecutionNativeScriptInvalidStep04RedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptInvalidStep03ArgsSchema);
export const ExecutionNativeScriptInvalidStep05StateSchema = Data.Object({
  ...identity,
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
  script_item_hash: H32Schema,
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
  signer_checkpoint_hash: H32Schema,
  previous_signer_hash: Data.Bytes(),
  signer_count: Data.Integer(),
  signer_peaks: Data.Array(FrontierPeakSchema),
});
export const ExecutionNativeScriptInvalidStep05DatumSchema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidStep05StateSchema);
export const ExecutionNativeScriptInvalidStep05RedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptInvalidStep04ArgsSchema);
export const ExecutionNativeScriptInvalidStep06StateSchema = Data.Object({
  ...identity,
  bad_tx_id: H32Schema,
  script_item_hash: H32Schema,
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
  signer_count: Data.Integer(),
  signer_peaks: Data.Array(FrontierPeakSchema),
  phase: NativeScriptInvalidStep05PhaseSchema,
});
export const ExecutionNativeScriptInvalidStep06DatumSchema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidStep06StateSchema);
export const ExecutionNativeScriptInvalidStep06RedeemerSchema =
  faultProofStepRedeemerSchema(NativeScriptInvalidStep05ArgsSchema);
export { NativeScriptInvalidSignerQuerySchema as ExecutionNativeScriptInvalidSignerQueryV1Schema };

void ExecutionSourceStep01RedeemerSchema;
void ExecutionSourceStep02RedeemerSchema;
