use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionArgs}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/ledger_state.{MidgardScriptWitsHash}

pub type State {
  input_with_missing_script: MidgardTxInput,
  verified_script_wits_hash: MidgardScriptWitsHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args =
  NativeTxInclusionArgs

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
