//// ## `fabricated-withdrawal` step-04 — the established fault
//// (`GOAL_SPEC.md` §9.1 / Q40)
////
//// Step-04 is where the thread becomes a permanent fraud-proof token, so it is
//// the last place the family can refuse. It re-checks the two things a permanent
//// conviction must not get wrong:
////
//// 1. **Identity.** The fault must be filed against the header the thread token
////    names — this family's category id followed by the challenged header hash.
////    The minted fraud-proof token inherits that asset name, so a thread whose
////    state names a different header would file its conviction under the wrong
////    block.
////
//// 2. **The fault is really a fault.** `MismatchedWithdrawalContent` must carry
////    two *different* content commitments, and its event must have been due for
////    the challenged block: `header.start_time < inclusion_time <=
////    header.end_time`, the same window
////    `midgard/fraud_proofs/transition_trace/proof` applies to a timed L1 event.
////    An event outside that window is not evidence that *this* block fabricated a
////    withdrawal — whether the block should have included it at all is the
////    inclusion-window family's rule, and convicting on it here would let one
////    fault be proven twice under two categories.
////
//// ### Mirror of `fabricated-deposit` (Q39)
////
//// This mirrors `midgard/fraud_proofs/fabricated_deposit/step_04`'s establishment
//// rule; it lives in this family's own lib module because `GOAL_PROGRESS.md`'s
//// triage requires Q39 and Q40 to keep disjoint family paths.

use midgard/common/types.{PosixTime}
use midgard/computation_thread as ct
use midgard/fraud_proofs/fabricated_withdrawal/step_01
use midgard/ledger_state

// ## The fault

/// The `FabricatedWithdrawal` violation, in its two shapes.
pub type FabricatedWithdrawalFaultV1 {
  /// No L1 withdrawal event with the committed `WithdrawalId` was ever
  /// authenticated.
  NonexistentWithdrawalIdentity
  /// The authentic L1 event exists and was due, but its `WithdrawalInfo` — its
  /// body, its signature or its validity verdict — is not the committed one.
  MismatchedWithdrawalContent {
    committed_withdrawal_info_hash: step_01.WithdrawalInfoHash,
    authentic_withdrawal_info_hash: step_01.WithdrawalInfoHash,
    event_inclusion_time: PosixTime,
  }
}

// ## Step-04 thread types

/// What step-03 hands forward: the challenged header's identity and window, the
/// committed withdrawal identity, and the classified fault.
pub type State {
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
    // Nothing numeric is left to re-check: the identity fault rests entirely on
    // the still-unspent output at the committed `WithdrawalId` that step-02
    // authenticated, and on step-03 refusing the opening that would have
    // manufactured this arm out of a present-event verdict.
    NonexistentWithdrawalIdentity -> True
    MismatchedWithdrawalContent {
      committed_withdrawal_info_hash,
      authentic_withdrawal_info_hash,
      event_inclusion_time,
    } -> and {
        committed_withdrawal_info_hash != authentic_withdrawal_info_hash,
        // The `timed_l1_event_is_due` window, restated: it is private to
        // `midgard/fraud_proofs/transition_trace/proof`, so this family cannot
        // call it. See the step-04 validator's `rejects_stale_evidence`.
        state.header_start_time < event_inclusion_time,
        event_inclusion_time <= state.header_end_time,
      }
  }
}
