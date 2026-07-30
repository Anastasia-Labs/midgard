use aiken/crypto.{ScriptHash}
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{MidgardScriptWitness}
use midgard/ledger_state.{MidgardScriptWitsHash}

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
  /// Canonical native script-witness collection preimage.
  script_tx_wits_preimage: List<MidgardScriptWitness>,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
