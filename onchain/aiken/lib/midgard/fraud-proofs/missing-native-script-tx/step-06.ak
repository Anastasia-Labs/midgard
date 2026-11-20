use aiken/crypto.{ScriptHash}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardScriptWits, MidgardScriptWitsHash}

pub type State {
  expected_missing_script_hash: ScriptHash,
  verified_script_wits_hash: MidgardScriptWitsHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  script_tx_wits_preimage: MidgardScriptWits,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
