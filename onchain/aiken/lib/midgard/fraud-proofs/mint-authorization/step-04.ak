//// Datum and redeemer wire shapes for the mint-authorization thread's
//// fourth step — the direction-A resolved-input scan (self-loop). Each
//// `ResolveNext` opens the committed input-field item at the cursor, resolves
//// it under the pre-state ledger root, and proves the resolved output carries
//// no reference script of the claimed policy hash; `AdvanceComplete` moves
//// from the reference inputs (field 1) to the spent inputs (field 0) once the
//// cursor has covered field 1, and closes the scan once it has covered
//// field 0.

use aiken/merkle_patricia_forestry as mpf
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type State {
  policy_id: ByteArray,
  bad_tx_id: ByteArray,
  prior_ledger_root: ByteArray,
  /// The input field under scan: step-03's direction-A arm writes field 1,
  /// and the scan then moves to field 0.
  field_index: Int,
  /// Next ordinal of `field_index` to resolve; each field starts at 0.
  cursor: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  /// Self-loop: resolve the input at `cursor` and advance.
  ResolveNext {
    input_index: Int,
    output_index: Int,
    inputs_opening: FieldOpeningV1,
    descriptor_cbor: ByteArray,
    ledger_membership_proof: mpf.Proof,
  }
  /// The cursor equals the authenticated item count of `field_index`: every
  /// resolved output of that field has been cleared.
  AdvanceComplete {
    input_index: Int,
    output_index: Int,
    inputs_opening: FieldOpeningV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
