use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/signer_witness_scan_v1/state.{ScanV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  witnesses_opening: FieldOpeningV1,
  /// Ignored on the opening turn (empty `checkpoint_hash`); every resume
  /// supplies the exact bytes `checkpoint_hash` commits.
  checkpoint_cbor: Option<ByteArray>,
}

pub type Datum =
  ct.StepDatum<ScanV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
