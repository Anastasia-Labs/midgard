use aiken/crypto.{ScriptHash}
use midgard/common/types.{PosixTime}
use midgard/ledger_state
use midgard/transition_trace

pub type DepositDatum {
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
  membership_proof: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}
