use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{MembershipCarriage}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction.{
  StateV1,
}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type Action {
  ScanSpend {
    input_index: Int,
    output_index: Int,
    spend_inputs_opening: FieldOpeningV1,
    descriptor_cbor: ByteArray,
    membership: MembershipCarriage,
  }
  FinishSpends {
    input_index: Int,
    output_index: Int,
    spend_inputs_opening: FieldOpeningV1,
  }
}

pub type Datum =
  ct.StepDatum<StateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Action>
