use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/ledger_state.{MidgardLedgerRoot, MidgardTxId, MidgardTxsRoot}
use midgard/transition_trace

/// The thread carries the disputed transaction's **id**, not its reference-inputs
/// commitment. Step-02 re-opens field 1 through the §8.8 door from that id
/// (`midgard/fraud_proofs/field_opening_v1`), which is what lets it read one
/// input arithmetically instead of reproducing the whole collection to re-hash
/// it — the Q1X-F6 shape (#551).
pub type State {
  State {
    bad_tx_id: MidgardTxId,
    blocks_prev_utxos_root: MidgardLedgerRoot,
    blocks_transactions_root: MidgardTxsRoot,
  }
  ForcedState {
    subject: VerdictSubjectV1,
    event_key: ledger_state.EventKey,
    event_root: ByteArray,
    event_count: Int,
    trace_root: ByteArray,
    trace_count: Int,
  }
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  Args {
    input_index: Int,
    output_index: Int,
    /// The prover's chosen §8 carriage for field 1's preimage, together with the
    /// compact structures `bad_tx_id` authenticates.
    reference_inputs_opening: FieldOpeningV1,
    bad_reference_input_index: Int,
  }
  ForcedArgs {
    input_index: Int,
    output_index: Int,
    reference_inputs_opening: FieldOpeningV1,
    event_membership: transition_trace.RootMembershipProof<
      ledger_state.EventKey,
      ledger_state.EventToStepValue,
    >,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
