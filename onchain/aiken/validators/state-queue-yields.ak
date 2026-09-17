use aiken/crypto.{VerificationKeyHash}
use cardano/address.{Address, Credential}
use cardano/assets.{PolicyId}
use cardano/transaction.{Transaction}
use midgard/common/utils
use midgard/state_queue.{
  CommitBlockHeader, MergeToConfirmedStateV1, MintRedeemer,
  RemoveFraudulentBlockHeader, RemoveUnattestedBlockAfterTimeout,
  RemoveUnavailableBlockAfterTimeout, YieldRedeemerV1,
}
use state_queue as state_queue_validator

validator commit(
  state_queue_policy_id: PolicyId,
  hub_oracle_script_hash: PolicyId,
  correction_lock_script_hash: ByteArray,
  active_operators_script_hash: PolicyId,
  active_operators_addr: Address,
  scheduler_script_hash: PolicyId,
  da_attestation_policy_id: PolicyId,
) {
  withdraw(
    _yield_redeemer: YieldRedeemerV1,
    _credential: Credential,
    tx: Transaction,
  ) {
    expect
        CommitBlockHeader {
          new_block_output_index,
          continued_latest_block_output_index,
          operator,
          scheduler_ref_input_index,
          active_operators_input_index,
          active_operators_redeemer_index,
          m_confirmed_state_ref_input_index,
          m_head_state_queue_node_ref_input_index,
          ..
        }: MintRedeemer
      = utils.get_unique_mint_redeemer(tx.redeemers, state_queue_policy_id)
    state_queue_validator.validate_commit_block_header_v1(
      hub_oracle_script_hash,
      correction_lock_script_hash,
      active_operators_script_hash,
      active_operators_addr,
      scheduler_script_hash,
      da_attestation_policy_id,
      state_queue_policy_id,
      tx,
      new_block_output_index,
      continued_latest_block_output_index,
      operator,
      scheduler_ref_input_index,
      active_operators_input_index,
      active_operators_redeemer_index,
      m_confirmed_state_ref_input_index,
      m_head_state_queue_node_ref_input_index,
    )
  }

  else(_) {
    fail
  }
}

validator remove_unattested(
  state_queue_policy_id: PolicyId,
  hub_oracle_script_hash: PolicyId,
  correction_lock_script_hash: ByteArray,
) {
  withdraw(
    _yield_redeemer: YieldRedeemerV1,
    _credential: Credential,
    tx: Transaction,
  ) {
    expect
        RemoveUnattestedBlockAfterTimeout {
          timed_out_header_hash,
          removal_approach,
          ..
        }: MintRedeemer
      = utils.get_unique_mint_redeemer(tx.redeemers, state_queue_policy_id)
    state_queue_validator.validate_remove_unattested_v1(
      hub_oracle_script_hash,
      correction_lock_script_hash,
      state_queue_policy_id,
      tx,
      timed_out_header_hash,
      removal_approach,
    )
  }

  else(_) {
    fail
  }
}

validator remove_unavailable(
  state_queue_policy_id: PolicyId,
  hub_oracle_script_hash: PolicyId,
  correction_lock_script_hash: ByteArray,
  availability_policy_id: PolicyId,
) {
  withdraw(
    _yield_redeemer: YieldRedeemerV1,
    _credential: Credential,
    tx: Transaction,
  ) {
    expect
        RemoveUnavailableBlockAfterTimeout {
          unavailable_header_hash,
          challenge_asset_name,
          removal_approach,
          ..
        }: MintRedeemer
      = utils.get_unique_mint_redeemer(tx.redeemers, state_queue_policy_id)
    state_queue_validator.validate_remove_unavailable_v1(
      hub_oracle_script_hash,
      correction_lock_script_hash,
      availability_policy_id,
      state_queue_policy_id,
      tx,
      unavailable_header_hash,
      challenge_asset_name,
      removal_approach,
    )
  }

  else(_) {
    fail
  }
}

validator remove_fraudulent(
  state_queue_policy_id: PolicyId,
  hub_oracle_script_hash: PolicyId,
  correction_lock_script_hash: ByteArray,
  active_operators_script_hash: PolicyId,
  retired_operators_script_hash: PolicyId,
  fraud_proof_script_hash: PolicyId,
) {
  withdraw(
    _yield_redeemer: YieldRedeemerV1,
    _credential: Credential,
    tx: Transaction,
  ) {
    expect
        RemoveFraudulentBlockHeader {
          fraudulent_operator,
          fraudulent_blocks_header_hash,
          slashing_approach,
          fraud_proof_ref_input_index,
          block_removal_approach,
          ..
        }: MintRedeemer
      = utils.get_unique_mint_redeemer(tx.redeemers, state_queue_policy_id)
    state_queue_validator.validate_remove_fraudulent_v1(
      hub_oracle_script_hash,
      correction_lock_script_hash,
      active_operators_script_hash,
      retired_operators_script_hash,
      fraud_proof_script_hash,
      state_queue_policy_id,
      tx,
      fraudulent_operator,
      fraudulent_blocks_header_hash,
      slashing_approach,
      fraud_proof_ref_input_index,
      block_removal_approach,
    )
  }

  else(_) {
    fail
  }
}

validator merge(
  state_queue_policy_id: PolicyId,
  hub_oracle_script_hash: PolicyId,
  correction_lock_script_hash: ByteArray,
  settlement_script_hash: PolicyId,
  da_attestation_policy_id: PolicyId,
) {
  withdraw(
    _yield_redeemer: YieldRedeemerV1,
    _credential: Credential,
    tx: Transaction,
  ) {
    expect
        MergeToConfirmedStateV1 {
          header_node_key,
          confirmed_state_input_outref,
          confirmed_state_output_index,
          m_settlement_redeemer_index,
          merged_block_withdrawals_root,
          merged_block_forced_transactions_root,
          merged_block_transactions_root,
          merged_block_deposits_root,
          merged_block_transition_trace_root,
          merged_block_event_to_step_root,
          merged_block_validation_traces_root,
          merged_block_withdrawal_count,
          merged_block_forced_transaction_count,
          merged_block_l2_transaction_count,
          merged_block_deposit_count,
          merged_block_total_event_count,
          merged_block_transition_step_count,
          merged_block_validation_trace_count,
          ..
        }: MintRedeemer
      = utils.get_unique_mint_redeemer(tx.redeemers, state_queue_policy_id)
    state_queue_validator.validate_merge_v1(
      hub_oracle_script_hash,
      correction_lock_script_hash,
      settlement_script_hash,
      da_attestation_policy_id,
      state_queue_policy_id,
      tx,
      header_node_key,
      confirmed_state_input_outref,
      confirmed_state_output_index,
      m_settlement_redeemer_index,
      merged_block_withdrawals_root,
      merged_block_forced_transactions_root,
      merged_block_transactions_root,
      merged_block_deposits_root,
      merged_block_transition_trace_root,
      merged_block_event_to_step_root,
      merged_block_validation_traces_root,
      merged_block_withdrawal_count,
      merged_block_forced_transaction_count,
      merged_block_l2_transaction_count,
      merged_block_deposit_count,
      merged_block_total_event_count,
      merged_block_transition_step_count,
      merged_block_validation_trace_count,
    )
  }

  else(_) {
    fail
  }
}
