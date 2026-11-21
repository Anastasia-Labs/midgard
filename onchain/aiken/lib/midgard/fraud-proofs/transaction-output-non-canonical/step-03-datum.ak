use midgard/canonical_plutus_data_stream_v1.{StreamFrameV1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/transaction_output_non_canonical/rule.{
  DatumScanStateV1,
}

pub type Datum =
  ct.StepDatum<DatumScanStateV1>

/// `window` carries the item chunks the step reads; `revealed` and
/// `tail_root` open the stored container stack; `node_budget` bounds the
/// nodes read in this transaction.
pub type Args {
  input_index: Int,
  output_index: Int,
  window: ByteArray,
  revealed: List<StreamFrameV1>,
  tail_root: ByteArray,
  node_budget: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
