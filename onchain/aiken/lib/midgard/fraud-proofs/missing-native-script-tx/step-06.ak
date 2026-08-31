use aiken/crypto.{ScriptHash}
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

/// Direct absence evaluation remains the canonical simple route through the
/// largest cardinality measured with comfortable ledger-basis margin. Larger
/// fields first certify their envelope grammar and then evaluate in bounded
/// semantic batches.
pub const direct_script_witness_limit: Int = 64

/// Per-L1-transaction work bound for both staged phases, measured at the exact
/// 224-witness Cardano maximum by the shared machine suite.
pub const staged_batch_limit: Int = 32

/// Constructor order is computation-thread datum wire format.
pub type PhaseV1 {
  Ready
  GrammarCertification { checkpoint_hash: ByteArray }
  SemanticScan { checkpoint_hash: ByteArray, required_script_is_present: Bool }
}

/// `bad_tx_witness_set_hash` is the second half of the §2.5 anchor, carried the
/// same distance as `bad_tx_id`. §3's transaction id preimage is the body alone,
/// so the id authenticates fields 0–5 and says nothing about the witness set;
/// step-06 opens field 6 and needs the `witness_set_hash` step-01 read off the
/// compact structure the block's counted `transactions_root` committed. It costs
/// the 32 bytes the retired `bad_tx_script_wits_hash` cost, and covers all three
/// witness-set fields instead of one.
pub type State {
  expected_missing_script_hash: ScriptHash,
  bad_tx_id: MidgardTxId,
  bad_tx_witness_set_hash: ByteArray,
  phase: PhaseV1,
}

pub type Datum =
  ct.StepDatum<State>

/// Constructor order is the step-06 redeemer ABI. Every staged action reopens
/// the committed field in the fresh L1 context; the only carried authority is
/// the checkpoint digest in `State`.
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

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
