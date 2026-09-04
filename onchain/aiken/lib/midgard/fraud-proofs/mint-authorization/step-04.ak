//// Datum and redeemer wire shapes for the mint-authorization thread's
//// fourth step — the direction-A reference-input scan (self-loop). Each
//// `ResolveNext` opens the committed field-1 item at the cursor, resolves it
//// under the pre-state ledger root, and proves the resolved output carries
//// no reference script of the claimed policy hash; `AdvanceComplete` closes
//// the scan once the cursor has covered the authenticated item count.

use aiken/merkle_patricia_forestry as mpf
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type State {
  policy_id: ByteArray,
  bad_tx_id: ByteArray,
  prior_ledger_root: ByteArray,
  /// Next field-1 ordinal to resolve. Step-03's direction-A arm writes 0.
  ref_cursor: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  /// Self-loop: resolve the reference input at `ref_cursor` and advance.
  ResolveNext {
    input_index: Int,
    output_index: Int,
    reference_inputs_opening: FieldOpeningV1,
    descriptor_cbor: ByteArray,
    ledger_membership_proof: mpf.Proof,
  }
  /// The cursor equals the authenticated field-1 item count: every resolved
  /// reference script has been cleared, so the absence claim is complete.
  AdvanceComplete {
    input_index: Int,
    output_index: Int,
    reference_inputs_opening: FieldOpeningV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
