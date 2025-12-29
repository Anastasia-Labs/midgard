use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use midgard/common/types.{PosixTime}
use midgard/ledger_state

pub type Datum {
  event: ledger_state.DepositEvent,
  inclusion_time: PosixTime,
  witness: ScriptHash,
}

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  mint_redeemer_index: Int,
  membership_proof: mpf.Proof,
}
