use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use midgard/common/types.{MerkleRoot}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{TxInclusionArgs}
use midgard/ledger_state.{MidgardScript}

pub type State {
  bad_tx_script_wits: MerkleRoot<ScriptHash, MidgardScript>,
  bad_output_script_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  tx_inclusion_args: TxInclusionArgs,
  script_wit_to_delete_key: ScriptHash,
  script_wit_to_delete_value: MidgardScript,
  script_wits_delete_proof: mpf.Proof,
  plutarch_pdelete_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
