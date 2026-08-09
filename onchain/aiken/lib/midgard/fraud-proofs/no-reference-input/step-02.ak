use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardLedgerRoot, MidgardTxId, MidgardTxsRoot}

/// The thread carries the disputed transaction's **id**. Step-02 re-opens
/// field 1 through the §8.8 door from it — which is also what keeps field 1
/// distinguishable from field 0, since §4's plain hashing gives the two the
/// same commitment for the same items and only the position tells them apart.
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
  /// The prover's chosen §8 carriage for field 1's preimage, together with the
  /// compact structures `bad_tx_id` authenticates.
  reference_inputs_opening: FieldOpeningV1,
  bad_reference_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
