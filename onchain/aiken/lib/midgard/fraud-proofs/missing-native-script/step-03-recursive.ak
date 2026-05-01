use aiken/crypto.{ScriptHash}
use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardScript, MidgardScriptWitsRoot}

pub type State {
  expected_missing_script_hash: ScriptHash,
  verified_script_wits_root: MidgardScriptWitsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  Recurse {
    input_index: Int,
    output_index: Int,
    script_hash_to_delete: ScriptHash,
    script_to_delete: MidgardScript,
    membership_proof: Proof,
    merkle_delete_script_withdraw_redeemer_index: Int,
  }
  Finalize {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
