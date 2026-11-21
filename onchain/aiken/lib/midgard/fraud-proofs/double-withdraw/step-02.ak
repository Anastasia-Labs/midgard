//// ## `double-withdraw` step-02 — second payable leaf and finalization
//// (work item W-C3, catalogue row DOUBLE-WITHDRAW)
////
//// The terminal step, and therefore the last place the family can refuse. It
//// re-authenticates the challenged header out of the state-queue node the
//// thread token names, binds the **second** committed withdrawal leaf through
//// the same counted-root walk step-01 used for the first, and finalizes the
//// thread into a permanent fraud-proof token exactly when the decisive
//// predicate holds:
////
//// - the second leaf's identity differs from the first's (two distinct
////   withdrawal events),
//// - both leaves drain the same `body.l2_outref`, and
//// - the second leaf is also `WithdrawalIsValid` (step-01 already required it
////   of the first).
////
//// Two payable leaves over one L2 UTxO pay the same funds out twice at
//// settlement — the fund theft this family proves. A prover who presents the
//// *same* leaf twice fails the identity inequality, so an honest operator
//// whose block commits a single payable withdrawal (or an honest duplicate
//// tagged non-payable) is out of reach.

use cardano/transaction
use midgard/computation_thread as ct
use midgard/fraud_proofs/double_withdraw/step_01.{
  CommittedWithdrawalSourceProofV1,
}
use midgard/ledger_state

// ## Step-02 thread types

/// What step-01 hands forward: the challenged header's identity and the first
/// payable leaf's identity and drained L2 UTxO. Every field is fixed-size, so
/// the handoff's L1 footprint never depends on withdrawer-chosen value or
/// datum sizes.
pub type State {
  challenged_header_hash: ledger_state.HeaderHash,
  first_withdrawal_id: ledger_state.WithdrawalId,
  first_l2_outref: transaction.OutputReference,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  /// The second committed payable withdrawal leaf.
  committed_withdrawal: CommittedWithdrawalSourceProofV1,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

// ## The rule

/// The decisive `DoubleWithdraw` predicate, over the carried first-leaf facts
/// and the second committed leaf: distinct withdrawal identities, one drained
/// L2 UTxO, second leaf payable. (The first leaf's payability was step-01's
/// entry condition and is not restated in the state — a thread cannot reach
/// this step without it.)
pub fn double_withdraw_fault_is_established_v1(
  state: State,
  second: CommittedWithdrawalSourceProofV1,
) -> Bool {
  and {
    second.key != state.first_withdrawal_id,
    second.value.body.l2_outref == state.first_l2_outref,
    second.value.validity == ledger_state.WithdrawalIsValid,
  }
}
