use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/missing_native_script_utxo/step_05.{State}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  ResumeGrammarCertification {
    input_index: Int,
    output_index: Int,
    script_tx_wits_opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
  StartSemanticScan {
    input_index: Int,
    output_index: Int,
    script_tx_wits_opening: FieldOpeningV1,
    grammar_checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
