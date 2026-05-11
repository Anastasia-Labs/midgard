use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardOutputs, MidgardOutputsHash}

pub type State {
  producing_tx_outputs_hash: MidgardOutputsHash,
  bad_input_output_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  outputs_preimage: MidgardOutputs,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
