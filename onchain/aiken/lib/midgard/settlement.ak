use aiken/builtin
use aiken/cbor
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, Output, Redeemer, ScriptPurpose}
use midgard/common/types.{PosixTime}
use midgard/common/utils
use midgard/ledger_state
use midgard/transition_trace
use midgard/user_events/tx_order_v1 as tx_order

pub type Datum {
  deposits_root: ledger_state.DepositsRoot,
  withdrawals_root: ledger_state.WithdrawalsRoot,
  forced_transactions_root: ledger_state.ForcedTransactionsRoot,
  transactions_root: ledger_state.MidgardTxsRoot,
  resolution_claim: Option<ResolutionClaim>,
}

pub type ResolutionClaim {
  resolution_time: PosixTime,
  operator: VerificationKeyHash,
}

pub type SpendRedeemer {
  AttachResolutionClaim {
    settlement_input_index: Int,
    settlement_output_index: Int,
    hub_ref_input_index: Int,
    active_operators_node_input_index: Int,
    active_operators_redeemer_index: Int,
    operator: VerificationKeyHash,
    scheduler_ref_input_index: Int,
  }
  DisproveResolutionClaim {
    settlement_input_index: Int,
    settlement_output_index: Int,
    hub_ref_input_index: Int,
    operators_redeemer_index: Int,
    operator: VerificationKeyHash,
    operator_is_active: Bool,
    unresolved_event_ref_input_index: Int,
    unresolved_event_asset_name: AssetName,
    event_type: EventType,
    membership_proof: EventMembershipProof,
    inclusion_proof_script_withdraw_redeemer_index: Int,
  }
  Resolve { settlement_id: ByteArray }
}

pub type EventType {
  Deposit
  Withdrawal { validity_override: ledger_state.WithdrawalValidity }
  TxOrder { validity_override: ledger_state.MidgardTxValidity }
}

pub type EventMembershipProof {
  DepositMembership {
    witness: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
  }
  WithdrawalMembership {
    witness: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
  }
  TxOrderMembership {
    witness: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
  }
}

pub type MintRedeemer {
  Spawn {
    settlement_id: ByteArray,
    output_index: Int,
    state_queue_merge_redeemer_index: Int,
    hub_ref_input_index: Int,
  }
  Remove {
    settlement_id: ByteArray,
    input_index: Int,
    spend_redeemer_index: Int,
  }
}

pub fn valid_counted_membership(
  expected_domain: transition_trace.RootDomain,
  expected_root: ByteArray,
  witness: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
  key: Data<key>,
  value: Data<value>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  inclusion_proof_script_withdraw_redeemer_index: Int,
) -> Bool {
  let key_bytes = cbor.serialise(key)
  let value_bytes = cbor.serialise(value)
  and {
    witness.count > 0,
    witness.key == key_bytes,
    witness.value == value_bytes,
    transition_trace.verify_root_count_proof(
      transition_trace.RootCountProof {
        domain: witness.domain,
        root: witness.root,
        phas_root: witness.phas_root,
        count: witness.count,
      },
      expected_domain,
      expected_root,
      witness.count,
    ),
    utils.plutarch_phas_raw(
      merkle_root: witness.phas_root,
      key_bytes: key_bytes,
      value_bytes: value_bytes,
      membership_proof: witness.proof,
      redeemers: redeemers,
      withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
    ),
  }
}

pub fn get_datum(
  reference_inputs: List<Input>,
  settlement_policy_id: PolicyId,
  settlement_ref_input_index: Int,
) -> Datum {
  expect
      Input {
        output: Output { datum: InlineDatum(settlement_datum_data), .. },
        ..
      }
    = reference_inputs
    |> utils.get_authentic_input_with_policy_at(
        settlement_policy_id,
        settlement_ref_input_index,
      )

  expect Pair(0, settlement_fields) = builtin.un_constr_data(
    settlement_datum_data,
  )
  expect
      [
        deposits_root_data, withdrawals_root_data, forced_transactions_root_data,
        transactions_root_data, resolution_claim_data,
      ]
    = settlement_fields

  Datum {
    deposits_root: builtin.un_b_data(deposits_root_data),
    withdrawals_root: builtin.un_b_data(withdrawals_root_data),
    forced_transactions_root: builtin.un_b_data(forced_transactions_root_data),
    transactions_root: builtin.un_b_data(transactions_root_data),
    resolution_claim: parse_resolution_claim(resolution_claim_data),
  }
}

fn parse_resolution_claim(claim_data: Data) -> Option<ResolutionClaim> {
  let Pair(option_constructor, option_fields) = builtin.un_constr_data(
    claim_data,
  )
  if option_constructor == 1 {
    expect [] = option_fields
    None
  } else {
    expect option_constructor == 0
    expect [wrapped_claim_data] = option_fields
    expect Pair(0, claim_fields) = builtin.un_constr_data(wrapped_claim_data)
    expect [resolution_time_data, operator_data] = claim_fields
    Some(
      ResolutionClaim {
        resolution_time: builtin.un_i_data(resolution_time_data),
        operator: builtin.un_b_data(operator_data),
      },
    )
  }
}

pub fn valid_event_inclusion(
  event_type: EventType,
  deposit_script_hash: ScriptHash,
  withdrawal_script_hash: ScriptHash,
  tx_order_script_hash: ScriptHash,
  deposits_root: ledger_state.DepositsRoot,
  withdrawals_root: ledger_state.WithdrawalsRoot,
  forced_transactions_root: ledger_state.ForcedTransactionsRoot,
  membership_proof: EventMembershipProof,
  unresolved_event_asset_name: AssetName,
  unresolved_event_ref_input_index: Int,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  inclusion_proof_script_withdraw_redeemer_index: Int,
) -> Bool {
  when event_type is {
    Deposit -> {
      // 1. The transaction must include a deposit event utxo as a reference
      //    input. Let that reference input be `unprocessed_deposit`.
      expect
          Input {
            output: Output { datum: InlineDatum(deposit_datum_data), .. },
            ..
          }
        = utils.get_authentic_input_with_nft_at(
        reference_inputs,
        deposit_script_hash,
        unresolved_event_asset_name,
        unresolved_event_ref_input_index,
      )
      // Unsafe coercion is fine here as we have already validated the
      // authenticity of the utxo.
      let Pair(deposit_key, deposit_value) = ledger_state.unsafe_event_to_key_value_pair(
        deposit_datum_data,
      )
      expect deposit_id: ledger_state.DepositId = deposit_key
      expect deposit_info: ledger_state.DepositInfo = deposit_value
      expect DepositMembership { witness } = membership_proof

      // 2. A valid membership proof must be provided, proving that
      //    `unprocessed_deposit` is a member of the deposits tree in the
      //    settlement utxo.
      valid_counted_membership(
        expected_domain: transition_trace.DepositsRootDomain,
        expected_root: deposits_root,
        witness: witness,
        key: deposit_id,
        value: deposit_info,
        redeemers: redeemers,
        inclusion_proof_script_withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
      )
    }
    Withdrawal { validity_override } -> {
      // 1. The transaction must include a withdrawal order as a reference
      //    input. Let that reference input be `unprocessed_withdrawal`.
      expect
          Input {
            output: Output { datum: InlineDatum(withdrawal_datum_data), .. },
            ..
          }
        = utils.get_authentic_input_with_nft_at(
        reference_inputs,
        withdrawal_script_hash,
        unresolved_event_asset_name,
        unresolved_event_ref_input_index,
      )

      let Pair(withdrawal_key, withdrawal_info_data) = ledger_state.unsafe_event_to_key_value_pair(
        withdrawal_datum_data,
      )
      expect withdrawal_id: ledger_state.WithdrawalId = withdrawal_key

      // `validity` of `unprocessed_withdrawal` must be set via the redeemer.
      expect withdrawal_info: ledger_state.WithdrawalInfo = withdrawal_info_data
      //
      let overridden_withdrawal_info = ledger_state.WithdrawalInfo {
        ..withdrawal_info,
        validity: validity_override,
      }
      expect WithdrawalMembership { witness } = membership_proof

      // 2. A valid membership proof must be provided, proving that
      //    `unprocessed_withdrawal` with overridden `validity` is a member of
      //    the withdrawals tree in the settlement utxo.
      valid_counted_membership(
        expected_domain: transition_trace.WithdrawalsRootDomain,
        expected_root: withdrawals_root,
        witness: witness,
        key: withdrawal_id,
        value: overridden_withdrawal_info,
        redeemers: redeemers,
        inclusion_proof_script_withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
      )
    }
    TxOrder { validity_override } -> {
      // 1. The transaction must include an L2 transaction order as a reference
      //    input. Let that reference input be `unprocessed_tx_order`.
      expect
          Input {
            output: Output { datum: InlineDatum(tx_order_datum_data), .. },
            ..
          }
        = utils.get_authentic_input_with_nft_at(
        reference_inputs,
        tx_order_script_hash,
        unresolved_event_asset_name,
        unresolved_event_ref_input_index,
      )

      // `validity` of `unprocessed_tx_order` must be set via the redeemer.
      expect tx_order_datum: tx_order.Datum = tx_order_datum_data
      let tx_order_id, forced_inclusion_tx <- tx_order.forced_inclusion_key_value(
        tx_order_event_data: tx_order_datum.event,
        validity_override: validity_override,
      )
      expect TxOrderMembership { witness } = membership_proof

      // 2. A valid membership proof must be provided, proving that
      //    `unprocessed_tx_order` with the operator-provided validity is a
      //    member of the forced transactions tree in the settlement utxo.
      valid_counted_membership(
        expected_domain: transition_trace.ForcedTransactionsV1RootDomain,
        expected_root: forced_transactions_root,
        witness: witness,
        key: tx_order_id,
        value: forced_inclusion_tx,
        redeemers: redeemers,
        inclusion_proof_script_withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
      )
    }
  }
}
