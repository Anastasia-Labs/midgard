//// Family entry for `scriptIntegrityHashMismatch`. The shared source graph
//// hands over its exact binding; the entry opens the all-purposes walk with no
//// probe, since this family reads only the language set.

use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
}

pub type Datum =
  ct.StepDatum<BoundIndexV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
