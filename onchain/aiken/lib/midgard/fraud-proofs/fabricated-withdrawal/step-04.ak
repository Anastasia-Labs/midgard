//// Classify absence at capture, content mismatch, or an ineligible event.
//// Terminal validators additionally require the completed-fraud queue marker.

use cardano/assets.{PolicyId}
use midgard/common/types.{PosixTime}
use midgard/computation_thread as ct
use midgard/event_history/horizon
use midgard/fraud_proofs/fabricated_withdrawal/step_01
use midgard/ledger_state
use midgard/state_queue

// ## The fault

/// The `FabricatedWithdrawal` violation, in its two shapes.
pub type FabricatedWithdrawalFaultV1 {
  /// No L1 withdrawal event with the committed `WithdrawalId` was ever
  /// authenticated.
  NonexistentWithdrawalIdentity
  /// The authentic L1 event exists and was due, but its `(body, signature)`
  /// content is not the committed one. The committed `validity` verdict is
  /// excluded from the comparison: the operator owns it (decision 0007), and a
  /// verdict the chain contradicts is `withdrawalMistag`'s fault.
  MismatchedWithdrawalContent {
    committed_withdrawal_content_hash: step_01.WithdrawalContentHash,
    authentic_withdrawal_content_hash: step_01.WithdrawalContentHash,
    event_inclusion_time: PosixTime,
  }
  IneligibleWithdrawalEvent { event_inclusion_time: PosixTime }
}

// ## Step-04 thread types

/// What step-03 hands forward: the challenged header's identity and window, the
/// committed withdrawal identity, and the classified fault.
pub type State {
  state_queue_policy: PolicyId,
  challenged_header_hash: ledger_state.HeaderHash,
  header_start_time: PosixTime,
  header_end_time: PosixTime,
  committed_withdrawal_id: ledger_state.WithdrawalId,
  fault: FabricatedWithdrawalFaultV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  completed_fraud_witness: state_queue.CompletedFraudWitness,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

// ## Establishment

/// Re-asserts that the carried fault is a fabricated-withdrawal fault before it
/// is made permanent.
pub fn fabricated_withdrawal_fault_is_established_v1(state: State) -> Bool {
  when state.fault is {
    // Authenticated current-list absence is relative to this pending header;
    // it also covers authorized retirement followed by later ID reuse.
    NonexistentWithdrawalIdentity -> True
    IneligibleWithdrawalEvent { event_inclusion_time } ->
      !horizon.eligible(
        event_inclusion_time,
        state.header_start_time,
        state.header_end_time,
      )
    MismatchedWithdrawalContent {
      committed_withdrawal_content_hash,
      authentic_withdrawal_content_hash,
      event_inclusion_time,
    } -> and {
        committed_withdrawal_content_hash != authentic_withdrawal_content_hash,
        // The `timed_l1_event_is_due` window, restated: it is private to
        // `midgard/fraud_proofs/transition_trace/proof`, so this family cannot
        // call it. See the step-04 validator's `rejects_stale_evidence`.
        state.header_start_time < event_inclusion_time,
        event_inclusion_time <= state.header_end_time,
      }
  }
}
