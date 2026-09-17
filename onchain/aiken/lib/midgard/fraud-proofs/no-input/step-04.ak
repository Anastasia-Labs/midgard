use aiken/merkle_patricia_forestry as mpf
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NonMembershipCarriage}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/ledger_state.{MidgardTxId, MidgardTxsRoot}

pub type State {
  State {
    missing_input_tx_id: MidgardTxId,
    blocks_transactions_root: MidgardTxsRoot,
  }
  ForcedState {
    selected_input: Option<MidgardTxInput>,
    pre_utxos_root: ByteArray,
  }
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  Args {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
    /// The prover's chosen carriage for the transactions-root absence proof.
    non_membership_in_txs: NonMembershipCarriage,
  }
  ForcedArgs {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
    membership: Option<LedgerMembership>,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

pub type LedgerMembership {
  value_hash: ByteArray,
  proof: mpf.Proof,
}
