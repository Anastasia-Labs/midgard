//// Datum and redeemer wire shapes for the mint-authorization thread's
//// second step: bind the disputed block's header to the thread NFT, open
//// the committed claim (pre-state ledger root), and read the claimed policy
//// id off the committed field-5 item.

use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/mint_authorization/mint_scan
use midgard/fraud_proofs/structured_data_carriage
use midgard/ledger_state
use midgard/transition_trace

pub type State {
  State {
    bad_tx_id: ByteArray,
    bad_tx_witness_set_hash: ByteArray,
    validity_interval_start: Int,
    validity_interval_end: Int,
  }
  Scan {
    bad_tx_id: ByteArray,
    bad_tx_witness_set_hash: ByteArray,
    validity_interval_start: Int,
    validity_interval_end: Int,
    prior_ledger_root: ByteArray,
    policy_index: Int,
    direction: Int,
    field_hash: ByteArray,
    control: mint_scan.Control,
  }
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  Args {
    input_index: Int,
    output_index: Int,
    /// The disputed block's header; checked against the thread NFT's asset
    /// name tail before any of its roots are trusted.
    header: ledger_state.HeaderV1,
    event_to_step_membership: transition_trace.RootMembershipProof<
      ledger_state.EventKey,
      ledger_state.EventToStepValue,
    >,
    transition_step_membership: transition_trace.RootMembershipProof<
      Int,
      ledger_state.TransitionStep,
    >,
    /// Ordinal of the accused field-5 policy item. The policy id itself is
    /// read off the committed item, never supplied.
    policy_index: Int,
    /// `engine.direction_script_absent` or
    /// `engine.direction_script_unsatisfied`.
    direction: Int,
    mint_opening: FieldOpeningV1,
  }
  PublishedArgs {
    input_index: Int,
    output_index: Int,
    evidence: structured_data_carriage.Evidence,
    mint_opening: FieldOpeningV1,
  }
  AdvanceScan {
    input_index: Int,
    output_index: Int,
    mint_opening: FieldOpeningV1,
  }
}

pub type ClaimEvidence {
  header: ledger_state.HeaderV1,
  event_to_step_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  transition_step_membership: transition_trace.RootMembershipProof<
    Int,
    ledger_state.TransitionStep,
  >,
  policy_index: Int,
  direction: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
