//// Datum and redeemer wire shapes for the decoding-fault thread's first
//// step. The step has no input state; its `Continue` payload selects how the
//// verdict subject is bound (#635, #633).

use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}

/// No input state: the thread NFT arrives with `data: None`.
pub type Datum =
  ct.StepDatum<Data>

pub type Args {
  /// Direction A over a normal-source subject: bind the accepted transaction
  /// through the counted `transactions_root` (a normal leaf IS an acceptance
  /// verdict). Direction is structurally `wrongful_acceptance`.
  BindNormalTransaction { carriage: NativeTxInclusionCarriage }
  /// Either direction over a forced-source subject: record the direction now,
  /// bind the forced leaf (and its verdict) at step-02.
  RecordForcedSource { direction: Int, input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
