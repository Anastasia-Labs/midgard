use cardano/address.{Credential}
use cardano/assets.{PolicyId}
use cardano/transaction.{Transaction}
use midgard/availability_challenge as availability
use midgard/availability_challenge_validation as validation
use midgard/common/utils

validator bond(
  availability_policy_id: PolicyId,
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
) {
  withdraw(
    _yield_redeemer: availability.YieldRedeemerV1,
    _credential: Credential,
    tx: Transaction,
  ) {
    expect availability.MintBondFromAttestation {
      hub_oracle_ref_input_index,
      da_attestation_input_index,
      da_attestation_mint_redeemer_index,
      bond_output_index,
      state_queue_input_index,
      state_queue_output_index,
      ..
    }: availability.MintRedeemerV1 =
      utils.get_unique_mint_redeemer(tx.redeemers, availability_policy_id)
    validation.validate_mint_bond_from_attestation(
      hub_oracle_policy_id,
      parameters,
      availability_policy_id,
      tx,
      hub_oracle_ref_input_index,
      da_attestation_input_index,
      da_attestation_mint_redeemer_index,
      bond_output_index,
      state_queue_input_index,
      state_queue_output_index,
    )
  }

  else(_) {
    fail
  }
}

validator open(
  availability_policy_id: PolicyId,
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
) {
  withdraw(
    _yield_redeemer: availability.YieldRedeemerV1,
    _credential: Credential,
    tx: Transaction,
  ) {
    expect availability.OpenChallenge {
      hub_oracle_ref_input_index,
      bond_input_index,
      bond_output_index,
      challenger_input_index,
      state_queue_input_index,
      state_queue_output_index,
      first_tranche_output_index,
      terminal_accumulator_output_index,
      challenger,
      ..
    }: availability.MintRedeemerV1 =
      utils.get_unique_mint_redeemer(tx.redeemers, availability_policy_id)
    validation.validate_open_challenge(
      hub_oracle_policy_id,
      parameters,
      availability_policy_id,
      tx,
      hub_oracle_ref_input_index,
      bond_input_index,
      bond_output_index,
      challenger_input_index,
      state_queue_input_index,
      state_queue_output_index,
      first_tranche_output_index,
      terminal_accumulator_output_index,
      challenger,
    )
  }

  else(_) {
    fail
  }
}

validator settle(
  availability_policy_id: PolicyId,
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
) {
  withdraw(
    _yield_redeemer: availability.YieldRedeemerV1,
    _credential: Credential,
    tx: Transaction,
  ) {
    expect availability.SettleTranche {
      bond_ref_input_index,
      terminal_accumulator_input_index,
      terminal_accumulator_output_index,
      tranche_input_index,
      carrier_input_index,
      ..
    }: availability.MintRedeemerV1 =
      utils.get_unique_mint_redeemer(tx.redeemers, availability_policy_id)
    validation.validate_settle_tranche(
      hub_oracle_policy_id,
      parameters,
      availability_policy_id,
      tx,
      bond_ref_input_index,
      terminal_accumulator_input_index,
      terminal_accumulator_output_index,
      tranche_input_index,
      carrier_input_index,
    )
  }

  else(_) {
    fail
  }
}

validator close(
  availability_policy_id: PolicyId,
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
) {
  withdraw(
    _yield_redeemer: availability.YieldRedeemerV1,
    _credential: Credential,
    tx: Transaction,
  ) {
    expect availability.CloseChallenge {
      hub_oracle_ref_input_index,
      bond_input_index,
      terminal_accumulator_input_index,
      state_queue_input_index,
      state_queue_output_index,
      da_refund_output_index,
      challenger_refund_output_index,
      ..
    }: availability.MintRedeemerV1 =
      utils.get_unique_mint_redeemer(tx.redeemers, availability_policy_id)
    validation.validate_close_challenge(
      hub_oracle_policy_id,
      parameters,
      availability_policy_id,
      tx,
      hub_oracle_ref_input_index,
      bond_input_index,
      terminal_accumulator_input_index,
      state_queue_input_index,
      state_queue_output_index,
      da_refund_output_index,
      challenger_refund_output_index,
    )
  }

  else(_) {
    fail
  }
}

validator timeout(
  availability_policy_id: PolicyId,
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
) {
  withdraw(
    _yield_redeemer: availability.YieldRedeemerV1,
    _credential: Credential,
    tx: Transaction,
  ) {
    expect availability.TimeoutChallenge {
      hub_oracle_ref_input_index,
      bond_input_index,
      terminal_accumulator_input_index,
      state_queue_mint_redeemer_index,
      da_slash_output_index,
      challenger_refund_output_index,
      ..
    }: availability.MintRedeemerV1 =
      utils.get_unique_mint_redeemer(tx.redeemers, availability_policy_id)
    validation.validate_timeout_challenge(
      hub_oracle_policy_id,
      parameters,
      availability_policy_id,
      tx,
      hub_oracle_ref_input_index,
      bond_input_index,
      terminal_accumulator_input_index,
      state_queue_mint_redeemer_index,
      da_slash_output_index,
      challenger_refund_output_index,
    )
  }

  else(_) {
    fail
  }
}
