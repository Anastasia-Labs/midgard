//// Narrow machine twin for `observersForbiddenOnUntaggedNetwork` (`00000024`).

use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{ObserversForbiddenOnUntaggedNetwork}

pub const observer_field_index: Int = 3

pub const untagged_network_id: Int = 255

pub type StateV1 {
  subject: substrate.VerdictSubjectV1,
  network_id: Int,
}

pub fn bind_state_v1(
  subject: substrate.VerdictSubjectV1,
  network_id: Int,
) -> StateV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect or {
      network_id == 0,
      network_id == 1,
      network_id == untagged_network_id,
    }
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    Some(
      substrate.bind_exact_rejection_reason_v1(
        subject,
        ObserversForbiddenOnUntaggedNetwork,
      ),
    )
  } else {
    None
  }
  StateV1 { subject, network_id }
}

pub fn forbidden_observers_hold_v1(observer_count: Int, network_id: Int) -> Bool {
  and {
    observer_count > 0,
    network_id == untagged_network_id,
  }
}

pub fn terminal_contradiction_v1(state: StateV1, observer_count: Int) -> Bool {
  expect observer_count >= 0
  substrate.terminal_contradiction_v1(
    state.subject,
    forbidden_observers_hold_v1(observer_count, state.network_id),
  )
}
