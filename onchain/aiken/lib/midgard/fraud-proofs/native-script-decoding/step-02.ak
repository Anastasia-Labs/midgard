//// Datum and redeemer wire shapes for the decoding-fault thread's second
//// step: the header rides the redeemer and is bound to the thread NFT's
//// asset name; the committed-claim openings authenticate the pre-state
//// ledger root and (for forced threads) the verdict leaf (#635, #633).

use midgard/computation_thread as ct
use midgard/fraud_proofs/native_script_decoding/engine.{BindStateV1}
use midgard/ledger_state
use midgard/transition_trace

pub type State =
  BindStateV1

pub type Datum =
  ct.StepDatum<State>

pub type Args {
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
  /// Forced threads: the verdict leaf. `None` for normal threads.
  forced_membership: Option<
    transition_trace.RootMembershipProof<
      ledger_state.TxOrderId,
      ledger_state.ForcedInclusionTxV1,
    >,
  >,
  /// Direction A: the prover-chosen accused pair (2026-08-25 ruling).
  /// Ignored for direction B, whose pair is copied from the forced leaf.
  chosen_outpoint_source_kind: Int,
  chosen_outpoint_cursor: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
