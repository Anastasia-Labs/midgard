//// Datum and redeemer wire shapes for the mint-authorization thread's first
//// step (W-C14): bind the operator-ACCEPTED committed transaction through
//// the counted `transactions_root` and carry the §2.5 anchors downstream.

use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}

/// No input state: the thread NFT arrives with `data: None`.
pub type Datum =
  ct.StepDatum<Data>

pub type Args {
  carriage: NativeTxInclusionCarriage,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
