use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/types.{Proof}
use midgard/common/utils
use midgard/ledger_state.{WithdrawalEvent, WithdrawalValidity}
use midgard/user_events

pub type Datum =
  user_events.OptimisticDatum<WithdrawalEvent>

pub type SpendPurpose {
  InitializePayout
  Refund { validity_override: WithdrawalValidity }
}

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  burn_redeemer_index: Int,
  payout_mint_redeemer_index: Int,
  membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
  purpose: SpendPurpose,
}

pub fn get_datum(
  reference_inputs: List<Input>,
  withdrawal_policy_id: PolicyId,
  withdrawal_input_index: Int,
) -> Datum {
  expect Input {
    output: Output { datum: InlineDatum(withdrawal_datum_data), .. },
    ..
  } =
    utils.get_authentic_input_with_policy_at(
      reference_inputs,
      withdrawal_policy_id,
      withdrawal_input_index,
    )
  expect withdrawal_datum: Datum = withdrawal_datum_data
  withdrawal_datum
}
