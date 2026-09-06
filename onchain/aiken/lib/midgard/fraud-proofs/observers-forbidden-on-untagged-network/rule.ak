//// Narrow machine twin for `observersForbiddenOnUntaggedNetwork` (`00000024`).

use aiken/primitive/bytearray
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{ObserversForbiddenOnUntaggedNetwork}
use midgard/script_language_views_v1.{empty_script_integrity_hash}

pub const observer_field_index: Int = 3

pub const untagged_network_id: Int = 255

/// The canonical absent `script_integrity_hash`. Phase-A script
/// preconditions reach the observer arm only past this value: the absent hash
/// either needs no Plutus evaluation at all or is rejected first as
/// `ScriptIntegrityHashMissing`.
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

/// Exact twin of `phase_a_script_preconditions_rejection`'s observer arm.
/// That arm is reached only when Plutus evaluation is required and the
/// `script_integrity_hash` is present; a present hash itself requires
/// Plutus evaluation, so the guard collapses to the hash alone. Without it
/// a native-only transaction carrying observers on scalar 255, which the
/// machine accepts, would convict an honest operator.
pub fn forbidden_observers_hold_v1(
  observer_count: Int,
  network_id: Int,
  script_integrity_hash: ByteArray,
) -> Bool {
  expect bytearray.length(script_integrity_hash) == 32
  and {
    script_integrity_hash != empty_script_integrity_hash,
    observer_count > 0,
    network_id == untagged_network_id,
  }
}

pub fn terminal_contradiction_v1(
  state: StateV1,
  observer_count: Int,
  script_integrity_hash: ByteArray,
) -> Bool {
  expect observer_count >= 0
  substrate.terminal_contradiction_v1(
    state.subject,
    forbidden_observers_hold_v1(
      observer_count,
      state.network_id,
      script_integrity_hash,
    ),
  )
}
