use cardano/transaction.{OutputReference}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{TxInclusionArgs}
use midgard/ledger_state.{MidgardScriptWitsHash}

pub type State {
  input_with_missing_script: OutputReference,
  verified_script_wits_hash: MidgardScriptWitsHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args =
  TxInclusionArgs

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
