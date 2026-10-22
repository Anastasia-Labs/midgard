//// Direction-complete decisive rule for `zeroInput` (`00000005`).

use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{EmptyInputs}

pub const spend_inputs_field_index: Int = 0

pub type StateV1 {
  subject: substrate.VerdictSubjectV1,
}

/// `subject` is the value `open_subject_v1` returned in this Step 01 run, so it
/// is canonical by construction (see `substrate.opened_rejection_reason_v1`).
pub fn bind_state_v1(subject: substrate.VerdictSubjectV1) -> StateV1 {
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    Some(substrate.bind_opened_rejection_reason_v1(subject, EmptyInputs))
  } else {
    None
  }
  StateV1 { subject }
}

pub fn empty_inputs_fault_holds_v1(input_count: Int) -> Bool {
  expect input_count >= 0
  input_count == 0
}

pub fn terminal_contradiction_v1(state: StateV1, input_count: Int) -> Bool {
  substrate.terminal_contradiction_v1(
    state.subject,
    empty_inputs_fault_holds_v1(input_count),
  )
}
