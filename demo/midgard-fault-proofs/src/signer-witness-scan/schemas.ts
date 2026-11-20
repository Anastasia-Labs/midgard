import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "@al-ft/midgard-sdk/core";
import {
  FieldOpeningSchema,
  VerdictSubjectSchema,
} from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

/** `signer_witness_scan_v1/state.ScanV1`: `checkpoint_hash` is empty before
 * the opening turn. */
export const SignerWitnessScanStateSchema = Data.Object({
  subject: VerdictSubjectSchema,
  witness_set_hash: Data.Bytes(),
  payment_credential: Data.Bytes(),
  checkpoint_hash: Data.Bytes(),
});
/** `signer_witness_scan_v1/state.VerdictV1`. */
export const SignerWitnessVerdictSchema = Data.Object({
  subject: VerdictSubjectSchema,
  signer_required: Data.Boolean(),
  signer_present: Data.Boolean(),
});
export const SignerWitnessScanDatumSchema = faultProofStepDatumSchema(
  SignerWitnessScanStateSchema,
);
export const SignerWitnessScanRedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    witnesses_opening: FieldOpeningSchema,
    checkpoint_cbor: Data.Nullable(Data.Bytes()),
  }),
);
export const SignerWitnessTerminalDatumSchema = faultProofStepDatumSchema(
  SignerWitnessVerdictSchema,
);
export const SignerWitnessTerminalRedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    input_index: Data.Integer(),
    output_index: Data.Integer(),
    fraud_proof_mint_redeemer_index: Data.Integer(),
  }),
);
