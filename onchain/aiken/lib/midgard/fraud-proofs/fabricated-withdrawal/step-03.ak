//// ## `fabricated-withdrawal` step-03 — the fault classification
//// (`GOAL_SPEC.md` §9.1 / Q40)
////
//// Step-03 turns step-02's authenticated verdict about L1 into the named fault
//// step-04 finalizes. It reads no reference input: everything it needs is either
//// in the thread state (authenticated at steps 01–02) or is a preimage of a
//// commitment already in that state.
////
//// ### The two arms must pair with the verdict
////
//// A `WithdrawalIdentityAbsent` verdict admits only the `NoAuthenticContent`
//// opening, and a `WithdrawalEventObserved` verdict only `RetainedEventDatum`. The
//// cross pairs are refused, and that refusal is load-bearing: allowing a
//// `NoAuthenticContent` opening on top of a `WithdrawalEventObserved` verdict
//// would let a prover who found the authentic event convert its dispute into a
//// *non-existence* conviction, which is a strictly stronger claim than the
//// evidence supports.
////
//// ### One comparison settles body, signature and validity
////
//// The mismatch arm compares the committed `WithdrawalInfo` commitment with the
//// authentic event's. Because a `WithdrawalInfo` is exactly
//// `(body, signature, validity)`, that single 32-byte inequality convicts a header
//// that diverted the payout address or value (body), that attached an
//// authorisation the owner never produced (signature), or that overrode the
//// verdict `transition_trace/proof`'s `ValidWithdrawalTransition` arm reads out of
//// the committed leaf (validity). The family does not need — and deliberately does
//// not have — three separate faults for that: a per-field classification would
//// invite disagreement about which field "really" differs when a header changes
//// two at once, and every one of the three is equally a fabrication of the
//// authentic order.
////
//// ### Why the event datum arrives in the redeemer
////
//// Step-02 retained `blake2b_256(cbor.serialise(event_datum))`. Re-opening that
//// preimage here rather than referencing the event UTxO again means the dispute
//// survives the event NFT being burned when the withdrawal settles or refunds, and
//// it keeps the L1 footprint of the thread state independent of the withdrawer's
//// chosen `l2_value`/`l1_datum` sizes. The hash equality is the only thing that
//// makes the supplied bytes authentic — see `rejects_wrong_body_bytes` in the
//// step-03 validator.
////
//// ### Mirror of `fabricated-deposit` (Q39)
////
//// This mirrors `midgard/fraud_proofs/fabricated_deposit/step_03`'s opening rule
//// against `WithdrawalEvent`; it lives in this family's own lib module because
//// `GOAL_PROGRESS.md`'s triage requires Q39 and Q40 to keep disjoint family paths.

use midgard/common/types.{PosixTime}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/fabricated_withdrawal/step_01
use midgard/fraud_proofs/fabricated_withdrawal/step_02.{
  WithdrawalEventObserved, WithdrawalEvidenceVerdictV1, WithdrawalIdentityAbsent,
}
use midgard/fraud_proofs/fabricated_withdrawal/step_04
use midgard/ledger_state
use midgard/user_events/withdrawal

// ## Step-03 thread types

/// What step-02 hands forward: the committed leaf's identity and value
/// commitment, plus the authenticated verdict about L1.
pub type State {
  challenged_header_hash: ledger_state.HeaderHash,
  header_start_time: PosixTime,
  header_end_time: PosixTime,
  committed_withdrawal_id: ledger_state.WithdrawalId,
  committed_withdrawal_info_hash: step_01.WithdrawalInfoHash,
  verdict: WithdrawalEvidenceVerdictV1,
}

/// The preimage the prover opens for step-02's retained commitment.
pub type AuthenticContentOpeningV1 {
  /// Nothing to open: L1 holds no event for the committed identity.
  NoAuthenticContent
  /// The authentic withdrawal event datum step-02 committed to.
  RetainedEventDatum { event_datum: Data }
}

pub type Args {
  input_index: Int,
  output_index: Int,
  /// The prover's opening of step-02's retained event-datum commitment.
  authentic_content: AuthenticContentOpeningV1,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

/// The step-03 thread state step-02 really produces from `input_state` and its
/// authenticated `verdict` — the exact handoff step-02's positive selector pins.
pub fn fixture_state_v1(
  input_state: step_02.State,
  verdict: WithdrawalEvidenceVerdictV1,
) -> State {
  State {
    challenged_header_hash: input_state.challenged_header_hash,
    header_start_time: input_state.header_start_time,
    header_end_time: input_state.header_end_time,
    committed_withdrawal_id: input_state.committed_withdrawal_id,
    committed_withdrawal_info_hash: input_state.committed_withdrawal_info_hash,
    verdict,
  }
}

/// The step-04 thread state step-03 really produces from `state` and the fault it
/// classified — the exact handoff step-03's positive selector pins.
pub fn fixture_next_state_v1(
  state: State,
  fault: step_04.FabricatedWithdrawalFaultV1,
) -> step_04.State {
  step_04.State {
    challenged_header_hash: state.challenged_header_hash,
    header_start_time: state.header_start_time,
    header_end_time: state.header_end_time,
    committed_withdrawal_id: state.committed_withdrawal_id,
    fault,
  }
}

// ## Fault classification

/// Classifies the fault from the authenticated verdict and the prover's opening,
/// or fails if no fabricated-withdrawal fault follows.
pub fn open_authentic_withdrawal_content_v1(
  state: State,
  authentic_content: AuthenticContentOpeningV1,
) -> step_04.FabricatedWithdrawalFaultV1 {
  when (state.verdict, authentic_content) is {
    (WithdrawalIdentityAbsent, NoAuthenticContent) ->
      step_04.NonexistentWithdrawalIdentity
    (
      WithdrawalEventObserved { event_datum_hash, event_inclusion_time },
      RetainedEventDatum { event_datum },
    ) -> {
      // 1. The supplied bytes must be the ones step-02 authenticated. Without
      //    this the prover could open any datum it liked and the "authentic"
      //    content would be its own invention.
      expect utils.verify_hash_32(event_datum, event_datum_hash)

      // 2. Decode the authenticated preimage and re-derive its identity. The
      //    identity check is already implied by step-02, and is repeated here so
      //    this function is sound read on its own.
      expect authentic_datum: withdrawal.Datum = event_datum
      expect authentic_datum.event.id == state.committed_withdrawal_id

      // 3. The fault: the authentic event's content is not what the header
      //    committed. Comparing the two 32-byte commitments is equivalent to
      //    comparing the `WithdrawalInfo`s, and the committed side is the exact
      //    hash of the bytes step-01 fed to the counted-root membership check.
      let authentic_withdrawal_info_hash = utils.serialise_and_hash_32(
        authentic_datum.event.info,
      )
      expect
        authentic_withdrawal_info_hash != state.committed_withdrawal_info_hash

      step_04.MismatchedWithdrawalContent {
        committed_withdrawal_info_hash: state.committed_withdrawal_info_hash,
        authentic_withdrawal_info_hash,
        event_inclusion_time,
      }
    }
    // The cross pairs claim more than the evidence supports, and they are
    // enumerated rather than caught by a wildcard so that a new verdict or
    // opening constructor is a compile error here instead of a silent `fail`
    // path — `demo/midgard-fault-proofs`' permissive-dispatch scanner reads a
    // wildcard arm as hidden unenumerated cases.
    (WithdrawalIdentityAbsent, RetainedEventDatum { .. }) ->
      fail @"fabricated-withdrawal: absence verdict admits no opening"
    (WithdrawalEventObserved { .. }, NoAuthenticContent) ->
      fail @"fabricated-withdrawal: observed event needs its datum opened"
  }
}
