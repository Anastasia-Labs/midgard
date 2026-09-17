use aiken/collection/list
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/availability_challenge as availability
use midgard/availability_challenge_validation as validation
use midgard/availability_challenge_yield as yielding
use midgard/state_queue_yield

fn script_hash(address: Address) -> PolicyId {
  expect Script(hash) = address.payment_credential
  hash
}

validator availability_challenge(
  hub_oracle_policy_id: PolicyId,
  reference_script_auth_policy_id: PolicyId,
  parameters: availability.ParametersV1,
) {
  mint(
    redeemer: availability.MintRedeemerV1,
    _own_policy_id: PolicyId,
    tx: Transaction,
  ) {
    let Pair(role, yield_to_ref_input_index) = when redeemer is {
      availability.MintBondFromAttestation { yield_to_ref_input_index, .. } ->
        Pair(yielding.bond_role, yield_to_ref_input_index)
      availability.OpenChallenge { yield_to_ref_input_index, .. } ->
        Pair(yielding.open_role, yield_to_ref_input_index)
      availability.SettleTranche { yield_to_ref_input_index, .. } ->
        Pair(yielding.settle_role, yield_to_ref_input_index)
      availability.CloseChallenge { yield_to_ref_input_index, .. } ->
        Pair(yielding.close_role, yield_to_ref_input_index)
      availability.TimeoutChallenge { yield_to_ref_input_index, .. } ->
        Pair(yielding.timeout_role, yield_to_ref_input_index)
    }
    expect _yield_script_hash = state_queue_yield.require_authenticated_zero_yield(
      tx.reference_inputs,
      tx.withdrawals,
      tx.redeemers,
      reference_script_auth_policy_id,
      role,
      yield_to_ref_input_index,
    )
    True
  }

  spend(
    datum: Option<Data>,
    redeemer: availability.SpendRedeemerV1,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(own_input) = tx.inputs
      |> list.find(
          fn(Input { output_reference, .. }) { output_reference == own_ref },
        )
    when redeemer is {
      availability.AdvanceTranche {
        thread_output_index,
        carrier_output_index,
        m_previous_carrier_input_index,
      } -> {
        expect Some(datum_data) = datum
        expect current_datum: availability.TrancheDatumV1 = datum_data
        validation.validate_advance_tranche(
          parameters,
          script_hash(own_input.output.address),
          current_datum,
          own_input,
          thread_output_index,
          carrier_output_index,
          m_previous_carrier_input_index,
          tx,
        )
      }
      availability.ConsumeCarrier {
        thread_input_index,
        thread_spend_redeemer_index,
      } ->
        validation.validate_consume_carrier(
          own_ref,
          thread_input_index,
          thread_spend_redeemer_index,
          tx,
        )
      availability.Coordinate { mint_redeemer_index } ->
        validation.validate_coordinate_spend(
          script_hash(own_input.output.address),
          own_ref,
          mint_redeemer_index,
          tx,
        )
    }
  }

  else(_) {
    fail
  }
}
