use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NonMembershipCarriage}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput} as native_tx_types
use midgard/ledger_state.{MidgardLedgerRoot, MidgardTxsRoot}
use midgard/transition_trace

pub type State {
  State {
    missing_reference_input: MidgardTxInput,
    blocks_prev_utxos_root: MidgardLedgerRoot,
    blocks_transactions_root: MidgardTxsRoot,
  }
  ForcedState {
    event_key: ledger_state.EventKey,
    trace_root: ByteArray,
    trace_count: Int,
    step_index: Int,
    selected_input: Option<MidgardTxInput>,
  }
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  Args {
    input_index: Int,
    output_index: Int,
    /// The prover's chosen carriage for the initial-ledger absence proof.
    non_membership_in_ledger: NonMembershipCarriage,
  }
  ForcedArgs {
    input_index: Int,
    output_index: Int,
    transition_membership: transition_trace.RootMembershipProof<
      Int,
      ledger_state.TransitionStep,
    >,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
