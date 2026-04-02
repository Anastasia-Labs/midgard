use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use midgard/common/types.{PosixTime, Proof}
use midgard/common/utils
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
  membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}

pub fn get_datum(
  reference_inputs: List<Input>,
  deposit_policy_id: PolicyId,
  deposit_input_index: Int,
) -> Datum {
  expect Input {
    output: Output { datum: InlineDatum(deposit_datum_data), .. },
    ..
  } =
    utils.get_authentic_input_with_policy_at(
      reference_inputs,
      deposit_policy_id,
      deposit_input_index,
    )
  expect deposit_datum: Datum = deposit_datum_data
  deposit_datum
}
