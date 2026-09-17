//// Narrow machine twin for `observerOrderInvalid` (`00000025`).

use aiken/cbor
use aiken/primitive/bytearray
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{ObserverOrderInvalid}

pub const observer_field_index: Int = 3

pub const staged_observer_budget: Int = 24

pub const outcome_scanning: Int = 0

pub const outcome_violation: Int = 1

pub const outcome_ordered: Int = 2

pub type BoundObserverV1 {
  subject: substrate.VerdictSubjectV1,
  observer_index: Int,
}

pub type ScanStateV1 {
  subject: substrate.VerdictSubjectV1,
  observer_index: Int,
  checkpoint_hash: ByteArray,
  seen: Int,
  previous_observer: ByteArray,
  outcome: Int,
}

pub type DecisionStateV1 {
  subject: substrate.VerdictSubjectV1,
  observer_index: Int,
  violation: Bool,
}

pub fn bind_observer_v1(
  subject: substrate.VerdictSubjectV1,
  observer_index: Int,
) -> BoundObserverV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect observer_index >= 0
  expect
    if subject.direction == substrate.direction_wrongful_rejection {
      // The forced leaf's typed reason is bound exactly. Every ordinal it
      // can name is admissible here: a rejection naming ordinal 0, or an
      // ordinal the field never reaches, names no offending later item, and
      // the walk contradicts it by exhausting the field strictly ordered.
      expect _reason = substrate.bind_exact_rejection_reason_v1(
        subject,
        ObserverOrderInvalid { observer_index },
      )
      True
    } else {
      // An offending item is necessarily the later member of an adjacent
      // pair.
      observer_index > 0
    }
  BoundObserverV1 { subject, observer_index }
}

pub fn initial_scan_v1(
  bound: BoundObserverV1,
  checkpoint_hash: ByteArray,
) -> ScanStateV1 {
  expect bytearray.length(checkpoint_hash) == 32
  ScanStateV1 {
    subject: bound.subject,
    observer_index: bound.observer_index,
    checkpoint_hash,
    seen: 0,
    previous_observer: #"",
    outcome: outcome_scanning,
  }
}

pub fn scan_item_v1(
  state: ScanStateV1,
  item_index: Int,
  observer: ByteArray,
) -> ScanStateV1 {
  expect and {
      state.outcome == outcome_scanning,
      item_index == state.seen,
      item_index <= state.observer_index,
      bytearray.length(observer) == 28,
      or {
        item_index == 0,
        bytearray.length(state.previous_observer) == 28,
      },
    }
  if item_index == state.observer_index {
    let violation = bytearray.compare(state.previous_observer, observer) != Less
    ScanStateV1 {
      ..state,
      seen: state.seen + 1,
      previous_observer: observer,
      outcome: if violation {
        outcome_violation
      } else {
        outcome_ordered
      },
    }
  } else {
    // The named coordinate must be the first offending later item.
    expect or {
        item_index == 0,
        bytearray.compare(state.previous_observer, observer) == Less,
      }
    ScanStateV1 { ..state, seen: state.seen + 1, previous_observer: observer }
  }
}

/// The authenticated walk ended before it reached the cited ordinal, every
/// visited adjacent pair strictly ascending. The canonical machine walks the
/// whole field and rejects at the first later item that is not greater than
/// its predecessor, so a field it exhausts has no offending item at any
/// ordinal: an `ObserverOrderInvalid` rejection naming one is wrong, and an
/// acceptance is honest. Only reachable when the cited ordinal is at or past
/// the field's item count, because `scan_item_v1` decides at the ordinal
/// itself whenever the walk reaches it.
pub fn exhaust_scan_v1(state: ScanStateV1) -> ScanStateV1 {
  expect and {
      state.outcome == outcome_scanning,
      state.seen <= state.observer_index,
    }
  ScanStateV1 { ..state, outcome: outcome_ordered }
}

pub fn decision_v1(state: ScanStateV1) -> DecisionStateV1 {
  expect or {
      state.outcome == outcome_violation,
      state.outcome == outcome_ordered,
    }
  DecisionStateV1 {
    subject: state.subject,
    observer_index: state.observer_index,
    violation: state.outcome == outcome_violation,
  }
}

pub fn terminal_contradiction_v1(state: DecisionStateV1) -> Bool {
  substrate.terminal_contradiction_v1(state.subject, state.violation)
}

pub fn encode_bound_observer_v1(value: BoundObserverV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_scan_state_v1(value: ScanStateV1) -> ByteArray {
  cbor.serialise(value)
}
