use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardOutputsHash}

pub type State {
  producing_tx_outputs_hash: MidgardOutputsHash,
  bad_reference_input_output_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

/// `outputs_preimage` is the producing transaction's native outputs preimage as
/// the raw per-output CBOR byte list (each element is `encode_midgard_tx_output`
/// of one output). Re-wrapping it with `encode_native_byte_list` and hashing
/// reproduces the committed `outputs_hash`; because that encoding is length
/// prefixed, a preimage matching the hash also fixes the exact output count, so
/// the out-of-range check is sound without decoding each output structurally.
pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  outputs_preimage: List<ByteArray>,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
