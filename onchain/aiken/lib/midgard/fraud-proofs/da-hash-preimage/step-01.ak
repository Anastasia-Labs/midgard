//// `da-hash-preimage` step-01 schema (GOAL_SPEC.md Q44).
////
//// Step 01 binds one raw committed `transactions_root` leaf to the block
//// header and forwards the exact evidence triple step 02 adjudicates
//// (`step_02.State`).

use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionArgs}

pub type Datum =
  ct.StepDatum<Data>

/// The committed leaf key travels in `native_tx_id` and the committed leaf
/// value in `native_tx_compact_cbor`; neither is required to satisfy the
/// native codec, which is exactly the fault under proof.
pub type Args =
  NativeTxInclusionArgs

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
