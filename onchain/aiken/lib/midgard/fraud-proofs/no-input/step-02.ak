use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardLedgerRoot, MidgardTxId, MidgardTxsRoot}

/// The thread carries the disputed transaction's **id**, not its spend-inputs
/// commitment. Step-02 re-opens field 0 through the §8.8 door from that id
/// (`midgard/fraud_proofs/field_opening_v1`), which is what lets it read one
/// input arithmetically instead of reproducing the whole collection to re-hash
/// it — the Q1X-F6 shape (#551).
pub type State {
  bad_tx_id: MidgardTxId,
  blocks_prev_utxos_root: MidgardLedgerRoot,
  blocks_transactions_root: MidgardTxsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  /// The prover's chosen §8 carriage for field 0's preimage, together with the
  /// compact structures `bad_tx_id` authenticates.
  spend_inputs_opening: FieldOpeningV1,
  bad_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
