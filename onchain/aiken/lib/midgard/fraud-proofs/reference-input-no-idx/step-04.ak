use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{MidgardTxOutput}
use midgard/ledger_state.{MidgardOutputsHash}

pub type State {
  producing_tx_outputs_hash: MidgardOutputsHash,
  bad_reference_input_output_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

/// `outputs_preimage` is the producing transaction's native outputs list. It is
/// re-committed with `bounded_collection_v1.from_items` under consensus field 2
/// — the same commitment the native codec builds — so a preimage that opens the
/// forwarded `producing_tx_outputs_hash` also fixes the exact output count, and
/// the out-of-range check is sound without decoding each output structurally.
pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  outputs_preimage: List<MidgardTxOutput>,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
