use cardano/transaction.{OutputReference}
use midgard/computation_thread as ct
use midgard/fraud_proofs/withdrawal_mistag/step_04

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type Datum =
  ct.StepDatum<step_04.State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

pub fn mistag_fault_is_established_v1(state: step_04.State) -> Bool {
  and {
    state.claimed_valid != state.actual_valid,
    state.exact_output_bytes > 0,
    state.required_lovelace >= 0,
  }
}

test accepts_exact_mistag_fault() {
  mistag_fault_is_established_v1(
    step_04.State {
      challenged_header_hash: #"14141414141414141414141414141414141414141414141414141414",
      withdrawal_id: OutputReference { transaction_id: #"01", output_index: 0 },
      claimed_valid: True,
      actual_valid: False,
      exact_output_bytes: 42,
      required_lovelace: 1,
    },
  )
}

test rejects_valid_block() {
  !mistag_fault_is_established_v1(
    step_04.State {
      challenged_header_hash: #"14141414141414141414141414141414141414141414141414141414",
      withdrawal_id: OutputReference { transaction_id: #"01", output_index: 0 },
      claimed_valid: True,
      actual_valid: True,
      exact_output_bytes: 42,
      required_lovelace: 1,
    },
  )
}

test rejects_malformed_handoff() {
  !mistag_fault_is_established_v1(
    step_04.State {
      challenged_header_hash: #"14141414141414141414141414141414141414141414141414141414",
      withdrawal_id: OutputReference { transaction_id: #"01", output_index: 0 },
      claimed_valid: False,
      actual_valid: True,
      exact_output_bytes: 0,
      required_lovelace: 1,
    },
  )
}

test rejects_wrong_terminal() {
  !mistag_fault_is_established_v1(
    step_04.State {
      challenged_header_hash: #"14141414141414141414141414141414141414141414141414141414",
      withdrawal_id: OutputReference { transaction_id: #"01", output_index: 0 },
      claimed_valid: False,
      actual_valid: False,
      exact_output_bytes: 42,
      required_lovelace: 1,
    },
  )
}
