use midgard/computation_thread as ct
use midgard/fraud_proofs/canonical_decodability/rule.{CommittedFieldClaimV1}
use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}

pub type Args {
  BindAccepted {
    inclusion: NativeTxInclusionCarriage,
    claim: CommittedFieldClaimV1,
  }
  RecordForced { direction: Int, input_index: Int, output_index: Int }
}

pub type Datum =
  ct.StepDatum<Data>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
