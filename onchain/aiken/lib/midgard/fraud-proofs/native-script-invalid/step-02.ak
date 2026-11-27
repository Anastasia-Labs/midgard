use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}

pub type State {
  subject: VerdictSubjectV1,
  bad_tx_id: ByteArray,
  bad_tx_witness_set_hash: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  grammar_checkpoint_hash: ByteArray,
  grammar_complete: Bool,
  script_checkpoint_hash: ByteArray,
}

pub type Args {
  Args {
    input_index: Int,
    output_index: Int,
    script_index: Int,
    script_tx_wits_opening: FieldOpeningV1,
  }
  CertifyScriptField {
    input_index: Int,
    output_index: Int,
    script_tx_wits_opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
  SelectCertifiedScript {
    input_index: Int,
    output_index: Int,
    script_index: Int,
    script_tx_wits_opening: FieldOpeningV1,
    grammar_checkpoint_bytes: ByteArray,
    walk_checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
  /// A rejection naming a script position field 6 does not have: a negative
  /// one, or one at or past the field's count. The count is the opened view's
  /// before grammar certification, and the certified walk's once it is
  /// complete; `grammar_checkpoint_bytes` is empty before.
  SelectAbsentScript {
    input_index: Int,
    output_index: Int,
    script_index: Int,
    script_tx_wits_opening: FieldOpeningV1,
    grammar_checkpoint_bytes: ByteArray,
  }
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
