//// `da-hash-preimage` step-01 schema (GOAL_SPEC.md Q44).
////
//// Step 01 binds one raw committed `transactions_root` leaf to the block
//// header and forwards the exact evidence triple step 02 adjudicates
//// (`step_02.State`).

use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionArgs}

pub type Datum =
  ct.StepDatum<Data>

/// The committed leaf key travels in `native_tx_id` and the raw committed
/// `Data(L2TransactionSourceV1)` bytes in `l2_transaction_source_cbor`;
/// neither is required to satisfy the normal source codec, which is exactly
/// the property under proof.
pub type Args =
  NativeTxInclusionArgs

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
