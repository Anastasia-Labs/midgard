use aiken/crypto.{ScriptHash, VerificationKeyHash}
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, Output, Redeemer, ScriptPurpose}
use midgard/common/types.{MerkleRoot, PosixTime, Proof}
use midgard/common/utils
use midgard/ledger_state

pub type Datum {
  deposits_root: MerkleRoot,
  withdrawals_root: MerkleRoot,
  transactions_root: MerkleRoot,
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
    membership_proof: Proof,
    inclusion_proof_script_withdraw_redeemer_index: Int,
  }
  Resolve { settlement_id: ByteArray }
}

pub type EventType {
  Deposit
  Withdrawal { validity_override: ledger_state.WithdrawalValidity }
  TxOrder { validity_override: ledger_state.MidgardTxValidity }
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

pub fn get_datum(
  reference_inputs: List<Input>,
  settlement_policy_id: PolicyId,
  settlement_ref_input_index: Int,
) -> Datum {
  expect Input {
    output: Output { datum: InlineDatum(settlement_datum_data), .. },
    ..
  } =
    reference_inputs
      |> utils.get_authentic_input_with_policy_at(
          settlement_policy_id,
          settlement_ref_input_index,
        )

  expect parsed_settlement_datum: Datum = settlement_datum_data

  parsed_settlement_datum
}

pub fn valid_event_inclusion(
  event_type: EventType,
  deposit_script_hash: ScriptHash,
  withdrawal_script_hash: ScriptHash,
  tx_order_script_hash: ScriptHash,
  deposits_root: MerkleRoot,
  withdrawals_root: MerkleRoot,
  transactions_root: MerkleRoot,
  membership_proof: Proof,
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
      expect Input {
        output: Output { datum: InlineDatum(deposit_datum_data), .. },
        ..
      } =
        utils.get_authentic_input_with_nft_at(
          reference_inputs,
          deposit_script_hash,
          unresolved_event_asset_name,
          unresolved_event_ref_input_index,
        )
      // Unsafe coercion is fine here as we have already validated the
      // authenticity of the utxo.
      let Pair(deposit_key, deposit_value) =
        ledger_state.unsafe_event_to_key_value_pair(deposit_datum_data)

      // 2. A valid membership proof must be provided, proving that
      //    `unprocessed_deposit` is a member of the deposits tree in the
      //    settlement utxo.
      utils.plutarch_phas(
        merkle_root: deposits_root,
        key: deposit_key,
        value: deposit_value,
        membership_proof: membership_proof,
        redeemers: redeemers,
        withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
      )
    }
    Withdrawal { validity_override } -> {
      // 1. The transaction must include a withdrawal order as a reference
      //    input. Let that reference input be `unprocessed_withdrawal`.
      expect Input {
        output: Output { datum: InlineDatum(withdrawal_datum_data), .. },
        ..
      } =
        utils.get_authentic_input_with_nft_at(
          reference_inputs,
          withdrawal_script_hash,
          unresolved_event_asset_name,
          unresolved_event_ref_input_index,
        )

      let Pair(withdrawal_key, withdrawal_info_data) =
        ledger_state.unsafe_event_to_key_value_pair(withdrawal_datum_data)

      // `validity` of `unprocessed_withdrawal` must be set via the redeemer.
      expect withdrawal_info: ledger_state.WithdrawalInfo = withdrawal_info_data
      //
      let overridden_withdrawal_info: Data =
        ledger_state.WithdrawalInfo {
          ..withdrawal_info,
          validity: validity_override,
        }

      // 2. A valid membership proof must be provided, proving that
      //    `unprocessed_withdrawal` with overridden `validity` is a member of
      //    the withdrawals tree in the settlement utxo.
      utils.plutarch_phas(
        merkle_root: withdrawals_root,
        key: withdrawal_key,
        value: overridden_withdrawal_info,
        membership_proof: membership_proof,
        redeemers: redeemers,
        withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
      )
    }
    TxOrder { validity_override } -> {
      // 1. The transaction must include an L2 transaction order as a reference
      //    input. Let that reference input be `unprocessed_tx_order`.
      expect Input {
        output: Output { datum: InlineDatum(tx_order_datum_data), .. },
        ..
      } =
        utils.get_authentic_input_with_nft_at(
          reference_inputs,
          tx_order_script_hash,
          unresolved_event_asset_name,
          unresolved_event_ref_input_index,
        )

      let Pair(tx_order_key, tx_order_tx_data) =
        ledger_state.unsafe_event_to_key_value_pair(tx_order_datum_data)

      // `validity` of `unprocessed_tx_order` must be set via the redeemer.
      expect tx_order_tx: ledger_state.MidgardTxCompact = tx_order_tx_data
      //
      let overridden_tx_order_tx: Data =
        ledger_state.MidgardTxCompact {
          ..tx_order_tx,
          validity: validity_override,
        }

      // 2. A valid membership proof must be provided, proving that
      //    `unprocessed_tx_order` with overridden `validity` is a member of the
      //    transactions tree in the settlement utxo.
      utils.plutarch_phas(
        merkle_root: transactions_root,
        key: tx_order_key,
        value: overridden_tx_order_tx,
        membership_proof: membership_proof,
        redeemers: redeemers,
        withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
      )
    }
  }
}
