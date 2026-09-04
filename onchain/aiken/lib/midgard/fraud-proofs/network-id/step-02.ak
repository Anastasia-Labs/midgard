use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{MembershipCarriage, NonMembershipCarriage}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/network_id/step_01.{NetworkIdFaultV1, PostUtxoStateV1}
use midgard/ledger_state.{MidgardTxId}

pub type PostUtxoPredecessorCarriageV1 {
  IntroducedPredecessor(NonMembershipCarriage)
  NetworkChangedPredecessor(MembershipCarriage)
}

/// Everything the final step trusts was written from the authenticated
/// transactions-root leaf by step-01. `expected_network_id` is a parameter of
/// that registered first step, rather than a prover-selected redeemer value.
pub type State {
  bad_tx_id: MidgardTxId,
  committed_tx_network_id: Int,
  expected_network_id: Int,
  fault: NetworkIdFaultV1,
  post_utxo: Option<PostUtxoStateV1>,
  /// Canonical CBOR of the authenticated forced root key. `None` preserves
  /// every accepted-invalid route; `Some` fixes wrongful-rejection polarity.
  forced_source_key: Option<ByteArray>,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  /// Present exactly for `OutputNetwork`; opens native-V1 field 2 through the
  /// complete §8.8 reference-input set supplied by the proof transaction.
  outputs_opening: Option<FieldOpeningV1>,
  /// Present exactly for `OutputNetworkUtxo`; authenticates culpability under
  /// the predecessor root frozen by step 01.
  predecessor_carriage: Option<PostUtxoPredecessorCarriageV1>,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
