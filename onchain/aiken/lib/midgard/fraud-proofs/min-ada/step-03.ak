//// Q27 min-Ada predicate step. The preceding step opened the claimed output;
//// this step scans it to its descriptor and applies the compiled production
//// min-Ada predicate.

use midgard/computation_thread as ct
use midgard/fraud_proofs/transaction_output_non_canonical/rule.{
  OutputScanStateV1,
}

pub type State {
  MinAdaTxScan { direction: Int, scan: OutputScanStateV1 }
  MinAdaTxDescriptor { direction: Int, total_length: Int, lovelace: Int }
  /// The claimed output is one field 2 does not have: a negative coordinate,
  /// or one at or past the field's certified count.
  MinAdaTxAbsent { direction: Int }
}

pub type Args {
  window: ByteArray,
  input_index: Int,
  output_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
