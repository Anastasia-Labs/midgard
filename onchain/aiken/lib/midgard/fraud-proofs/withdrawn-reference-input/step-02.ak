use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId, WithdrawalsRoot}

/// The thread carries the disputed transaction's **id**; step-02 re-opens
/// field 1 through the §8.8 door from it.
pub type State {
  bad_tx_id: MidgardTxId,
  blocks_withdrawals_root: WithdrawalsRoot,
  blocks_withdrawal_count: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  /// The prover's chosen §8 carriage for field 1's preimage, together with the
  /// compact structures `bad_tx_id` authenticates.
  reference_inputs_opening: FieldOpeningV1,
  bad_reference_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
