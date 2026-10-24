use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/min_ada/step_01.{MinAdaFaultV1}
use midgard/ledger_state.{MidgardTxId}

pub type State {
  grammar_checkpoint_hash: ByteArray,
  grammar_complete: Bool,
  walk_checkpoint_hash: ByteArray,
  /// Verdict polarity (`proof_thread_substrate_v1.direction_*`): 0 convicts an
  /// accepted output below the floor, 1 contradicts a forced rejection.
  direction: Int,
  /// Root the transaction was authenticated under
  /// (`proof_thread_substrate_v1.source_kind_*`); anchors the outputs opening.
  source_kind: Int,
  bad_tx_id: MidgardTxId,
  fault: MinAdaFaultV1,
}

pub type Args {
  grammar_checkpoint_bytes: ByteArray,
  walk_checkpoint_bytes: ByteArray,
  input_index: Int,
  output_index: Int,
  /// Authenticated reference input carrying the rewarding script.
  yield_to_ref_input_index: Int,
  outputs_opening: FieldOpeningV1,
}

pub type YieldRedeemer {
  YieldMinAdaStep02V1
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
