import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldOpeningV1Schema,
  FrontierPeakV1Schema,
  H32Schema,
  MembershipCarriageSchema,
  NativeScriptInvalidSignerQueryV1Schema,
  NativeScriptInvalidStep03ArgsSchema,
  NativeScriptInvalidStep04ArgsSchema,
  NativeScriptInvalidStep05ArgsSchema,
  NativeScriptInvalidStep05PhaseV1Schema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export {
  ExecutionSourceStep01RedeemerV1Schema as ExecutionNativeScriptInvalidStep01RedeemerV1Schema,
  ExecutionSourceStep02RedeemerV1Schema as ExecutionNativeScriptInvalidStep02RedeemerV1Schema,
  NativeScriptsControlV1Schema,
} from "../execution-source-script-decoding/schemas-v1.js";
import {
  ExecutionSourceStep01RedeemerV1Schema,
  ExecutionSourceStep02RedeemerV1Schema,
  ExecutionSourceVerdictSubjectV1Schema,
} from "../execution-source-script-decoding/schemas-v1.js";

export const ExecutionNativeScriptInvalidBoundV1Schema = Data.Object({
  subject: ExecutionSourceVerdictSubjectV1Schema,
  validation_traces_root: H32Schema,
  validation_trace_count: Data.Integer(),
  execution_index: Data.Integer(),
  accused_class: Data.Integer(),
  prior_ledger_root: H32Schema,
  compact_cbor: Data.Bytes(),
});

export const ExecutionNativeScriptInvalidStep02DatumV1Schema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidBoundV1Schema);

export const AcceptedSelectedPurposeV1Schema = Data.Object({
  purpose_kind: Data.Integer(),
  purpose_index: Data.Integer(),
  script_hash: Data.Bytes(),
  subject: Data.Bytes(),
});
export const AcceptedSelectedSourceV1Schema = Data.Object({
  source_index: Data.Integer(),
  origin_kind: Data.Integer(),
  source_key: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: Data.Bytes(),
  total_length: Data.Integer(),
  item_commitment: H32Schema,
});
export const ExecutionNativeScriptInvalidAcceptedStateV1Schema = Data.Object({
  bound: ExecutionNativeScriptInvalidBoundV1Schema,
  phase: Data.Integer(),
  field_cursor: Data.Integer(),
  execution_cursor: Data.Integer(),
  previous_key: Data.Bytes(),
  receive_candidate: Data.Bytes(),
  source_base_index: Data.Integer(),
  source_cursor: Data.Integer(),
  selected_purpose: Data.Nullable(AcceptedSelectedPurposeV1Schema),
  selected_source: Data.Nullable(AcceptedSelectedSourceV1Schema),
  next_expected_script_hash: Data.Bytes(),
  checkpoint_hash: H32Schema,
});
export const ExecutionNativeScriptInvalidAcceptedDatumV1Schema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidAcceptedStateV1Schema);
export const ExecutionNativeScriptInvalidAcceptedInitRedeemerV1Schema =
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
          [openingName]: FieldOpeningV1Schema,
        }),
      }),
      Data.Object({
        [finishName]: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          [openingName]: FieldOpeningV1Schema,
        }),
      }),
    ]),
  );
export const ExecutionNativeScriptInvalidAcceptedSpendRedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Enum([
      Data.Object({
        ScanSpend: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          spend_inputs_opening: FieldOpeningV1Schema,
          descriptor_cbor: Data.Bytes(),
          membership: MembershipCarriageSchema,
        }),
      }),
      Data.Object({
        FinishSpends: Data.Object({
          input_index: Data.Integer(),
          output_index: Data.Integer(),
          spend_inputs_opening: FieldOpeningV1Schema,
        }),
      }),
    ]),
  );
export const ExecutionNativeScriptInvalidAcceptedMintRedeemerV1Schema =
  acceptedFieldAction("ScanMint", "FinishMint", "mint_opening");
export const ExecutionNativeScriptInvalidAcceptedObserverRedeemerV1Schema =
  acceptedFieldAction("ScanObserver", "FinishObservers", "observer_opening");
export const ExecutionNativeScriptInvalidAcceptedReceiveRedeemerV1Schema =
  acceptedFieldAction("ScanOutput", "FinishOutputPass", "outputs_opening");
export const ExecutionNativeScriptInvalidAcceptedInlineRedeemerV1Schema =
  acceptedFieldAction("ScanInline", "FinishInline", "scripts_opening");
export const ExecutionNativeScriptInvalidAcceptedReferenceRedeemerV1Schema =
  faultProofStepRedeemerSchema(
    Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      reference_inputs_opening: FieldOpeningV1Schema,
      descriptor_cbor: Data.Bytes(),
      membership: MembershipCarriageSchema,
    }),
  );
export const ExecutionNativeScriptInvalidSourceV1Schema = Data.Object({
  bound: ExecutionNativeScriptInvalidBoundV1Schema,
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
export const ExecutionNativeScriptInvalidStep03DatumV1Schema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidSourceV1Schema);
export const ExecutionNativeScriptInvalidStep03RedeemerV1Schema =
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
export const ExecutionNativeScriptInvalidStep04StateV1Schema = Data.Object({
  ...identity,
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
  script_item_hash: H32Schema,
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
});
export const ExecutionNativeScriptInvalidStep04DatumV1Schema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidStep04StateV1Schema);
export const ExecutionNativeScriptInvalidStep04RedeemerV1Schema =
  faultProofStepRedeemerSchema(NativeScriptInvalidStep03ArgsSchema);
export const ExecutionNativeScriptInvalidStep05StateV1Schema = Data.Object({
  ...identity,
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
export const ExecutionNativeScriptInvalidStep05DatumV1Schema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidStep05StateV1Schema);
export const ExecutionNativeScriptInvalidStep05RedeemerV1Schema =
  faultProofStepRedeemerSchema(NativeScriptInvalidStep04ArgsSchema);
export const ExecutionNativeScriptInvalidStep06StateV1Schema = Data.Object({
  ...identity,
  bad_tx_id: H32Schema,
  script_item_hash: H32Schema,
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
  signer_count: Data.Integer(),
  signer_peaks: Data.Array(FrontierPeakV1Schema),
  phase: NativeScriptInvalidStep05PhaseV1Schema,
});
export const ExecutionNativeScriptInvalidStep06DatumV1Schema =
  faultProofStepDatumSchema(ExecutionNativeScriptInvalidStep06StateV1Schema);
export const ExecutionNativeScriptInvalidStep06RedeemerV1Schema =
  faultProofStepRedeemerSchema(NativeScriptInvalidStep05ArgsSchema);
export { NativeScriptInvalidSignerQueryV1Schema as ExecutionNativeScriptInvalidSignerQueryV1Schema };

void ExecutionSourceStep01RedeemerV1Schema;
void ExecutionSourceStep02RedeemerV1Schema;
