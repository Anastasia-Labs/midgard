use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}
use midgard/fraud_proofs/script_integrity_hash_missing/state.{BindStateV1}

pub type Datum =
  ct.StepDatum<Data>

pub type Args {
  BindAccepted { carriage: NativeTxInclusionCarriage }
  RecordForced { direction: Int, input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
