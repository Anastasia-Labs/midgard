//// `da-hash-preimage` step-02 schema (GOAL_SPEC.md Q44).
////
//// Step 02 finalizes the proof by adjudicating the evidence triple step 01
//// committed into the computation thread.

use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxId}

pub type State {
  /// MPF key the block committed the leaf under.
  committed_tx_id: MidgardTxId,
  /// Canonical native-V1 transaction id the committed leaf value itself
  /// commits to, derived by `rule.derive_committed_leaf_tx_id_v1`.
  derived_tx_id: MidgardTxId,
  /// Byte length of the committed leaf value.
  committed_leaf_byte_count: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
