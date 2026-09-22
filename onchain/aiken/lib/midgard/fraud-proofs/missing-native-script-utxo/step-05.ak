use aiken/crypto.{ScriptHash}
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub const direct_script_witness_limit: Int = 64

pub const staged_batch_limit: Int = 32

pub type PhaseV1 {
  Ready
  GrammarCertification { checkpoint_hash: ByteArray }
  SemanticScan { checkpoint_hash: ByteArray, required_script_is_present: Bool }
}

pub type State {
  expected_missing_script_hash: ScriptHash,
  bad_tx_id: ByteArray,
  bad_tx_witness_set_hash: ByteArray,
  phase: PhaseV1,
}

pub type Args {
  DirectFinalize {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
    script_tx_wits_opening: FieldOpeningV1,
  }
  StartGrammarCertification {
    input_index: Int,
    output_index: Int,
    script_tx_wits_opening: FieldOpeningV1,
    item_budget: Int,
  }
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
