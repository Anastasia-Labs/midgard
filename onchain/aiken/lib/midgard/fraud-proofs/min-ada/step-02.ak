use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{MembershipCarriage}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/min_ada/step_01.{MinAdaFaultV1, PostUtxoStateV1}
use midgard/ledger_state.{MidgardTxId}

pub type State {
  bad_tx_id: MidgardTxId,
  fault: MinAdaFaultV1,
  post_utxo: Option<PostUtxoStateV1>,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  /// Present exactly for `MinAdaTx`.
  outputs_opening: Option<FieldOpeningV1>,
  /// Present exactly for `MinAdaUtxo`; authenticates the descriptor frozen by
  /// step 01 against the challenged post-block UTxO root.
  post_membership: Option<MembershipCarriage>,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
