use aiken/cbor
use aiken_design_patterns/linked_list
use cardano/address
use cardano/transaction.{Datum, InlineDatum, NoDatum, OutputReference}
use midgard/ledger_state
use midgard/operator_directory
use midgard/operator_directory/active_operators as active
use midgard/operator_directory/registered_operators as registered
use midgard/operator_directory/retired_operators as retired
use midgard/payout
use midgard/rejection_reason_v1
use midgard/reserve
use midgard/settlement
use midgard/transition_trace
use midgard/user_events
use midgard/user_events/deposit
use midgard/user_events/withdrawal
use midgard/user_events/witness as event_witness

const h28_a = #"11111111111111111111111111111111111111111111111111111111"

const h28_b = #"22222222222222222222222222222222222222222222222222222222"

const h32_a =
  #"3333333333333333333333333333333333333333333333333333333333333333"

const h32_b =
  #"4444444444444444444444444444444444444444444444444444444444444444"

fn output_reference() -> OutputReference {
  OutputReference { transaction_id: h32_a, output_index: 1 }
}

fn sample_address() -> address.Address {
  address.from_script(h28_a)
}

fn raw_membership_proof(
  domain: transition_trace.RootDomain,
) -> transition_trace.RootMembershipProof<ByteArray, ByteArray> {
  transition_trace.RootMembershipProof {
    domain,
    root: h32_a,
    phas_root: h32_b,
    count: 1,
    key: #"aa",
    value: #"bb",
    proof: [],
  }
}

fn deposit_datum() -> deposit.DepositDatum {
  deposit.DepositDatum {
    event: ledger_state.DepositEvent {
      id: output_reference(),
      info: ledger_state.DepositInfo {
        l2_address: sample_address(),
        l2_network_id: 0,
        l2_datum: None,
      },
    },
    inclusion_time: 2,
    witness: h28_b,
  }
}

fn withdrawal_datum() -> withdrawal.Datum {
  user_events.OptimisticDatum {
    event: ledger_state.WithdrawalEvent {
      id: output_reference(),
      info: ledger_state.WithdrawalInfo {
        body: ledger_state.WithdrawalBody {
          l2_outref: output_reference(),
          l2_owner: h28_a,
          l2_value: [],
          l1_address: sample_address(),
          l1_datum: NoDatum,
        },
        signature: Pair(#"aa", #"bb"),
        validity: ledger_state.WithdrawalIsValid,
      },
    },
    inclusion_time: 3,
    witness: h28_b,
    refund_address: sample_address(),
    refund_datum: NoDatum,
  }
}

fn slashing_arguments() -> operator_directory.SlashingArguments {
  operator_directory.SlashingArguments {
    slashed_operator: h28_a,
    hub_oracle_ref_input_index: 1,
    slashed_operator_anchor_element_input_outref: output_reference(),
    slashed_operator_anchor_element_output_index: 2,
    slashing_reason: operator_directory.SlashOperatorForBadState {
      state_queue_redeemer_index: 3,
    },
  }
}

fn active_root_datum() -> active.Datum {
  linked_list.Element { data: linked_list.Root { data: #"" }, link: None }
}

test canonical_event_reserve_and_payout_v1_typescript_abi_vectors() {
  and {
    cbor.serialise(
      user_events.AuthenticateEvent {
        nonce_input_index: 1,
        event_output_index: 2,
        hub_ref_input_index: 3,
        witness_registration_redeemer_index: 4,
      },
    ) == #"d8799f01020304ff",
    cbor.serialise(
      user_events.BurnEventNFT {
        nonce_asset_name: #"aa",
        witness_unregistration_redeemer_index: 1,
      },
    ) == #"d87a9f41aa01ff",
    cbor.serialise(event_witness.MintOrBurn { target_policy: #"aa" }) == #"d8799f41aaff",
    cbor.serialise(
      event_witness.RegisterToProveNotRegistered {
        registration_certificate_index: 1,
      },
    ) == #"d87a9f01ff",
    cbor.serialise(
      event_witness.UnregisterToProveNotRegistered {
        registration_certificate_index: 1,
      },
    ) == #"d87b9f01ff",
    cbor.serialise(deposit_datum()) == #"d8799fd8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333301ffd8799fd8799fd87a9f581c11111111111111111111111111111111111111111111111111111111ffd87a80ff00d87a80ffff02581c22222222222222222222222222222222222222222222222222222222ff",
    cbor.serialise(
      deposit.SpendRedeemer {
        input_index: 1,
        output_index: 2,
        hub_ref_input_index: 3,
        settlement_ref_input_index: 4,
        mint_redeemer_index: 5,
        membership_proof: raw_membership_proof(
          transition_trace.DepositsRootDomain,
        ),
        inclusion_proof_script_withdraw_redeemer_index: 6,
      },
    ) == #"d8799f0102030405d8799fd87c8058203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444440141aa41bb80ff06ff",
    cbor.serialise(withdrawal_datum()) == #"d8799fd8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333301ffd8799fd8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333301ff581c11111111111111111111111111111111111111111111111111111111a0d8799fd87a9f581c11111111111111111111111111111111111111111111111111111111ffd87a80ffd87980ff9f41aa41bbffd87980ffff03581c22222222222222222222222222222222222222222222222222222222d8799fd87a9f581c11111111111111111111111111111111111111111111111111111111ffd87a80ffd87980ff",
    cbor.serialise(withdrawal.InitializePayout) == #"d87980",
    cbor.serialise(
      withdrawal.Refund {
        validity_override: ledger_state.IncorrectWithdrawalSignature,
      },
    ) == #"d87a9fd87e80ff",
    cbor.serialise(
      withdrawal.SpendRedeemer {
        input_index: 1,
        output_index: 2,
        hub_ref_input_index: 3,
        settlement_ref_input_index: 4,
        burn_redeemer_index: 5,
        payout_mint_redeemer_index: 6,
        membership_proof: raw_membership_proof(
          transition_trace.WithdrawalsRootDomain,
        ),
        inclusion_proof_script_withdraw_redeemer_index: 7,
        purpose: withdrawal.InitializePayout,
      },
    ) == #"d8799f010203040506d8799fd8798058203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444440141aa41bb80ff07d87980ff",
    cbor.serialise(
      reserve.Spend {
        reserve_input_index: 1,
        payout_input_index: 2,
        payout_spend_redeemer_index: 3,
        hub_ref_input_index: 4,
      },
    ) == #"d8799f01020304ff",
    cbor.serialise(
      payout.Datum {
        l2_value: [],
        l1_address: sample_address(),
        l1_datum: NoDatum,
      },
    ) == #"d8799fa0d8799fd87a9f581c11111111111111111111111111111111111111111111111111111111ffd87a80ffd87980ff",
    cbor.serialise(
      payout.AddFunds {
        payout_input_index: 1,
        payout_output_index: 2,
        reserve_input_index: 3,
        reserve_change_output_index: Some(4),
        reserve_spend_redeemer_index: 5,
        payout_spend_redeemer_index: 6,
        hub_ref_input_index: 7,
      },
    ) == #"d8799f010203d8799f04ff050607ff",
    cbor.serialise(
      payout.ConcludeWithdrawal {
        payout_input_index: 1,
        l1_output_index: 2,
        burn_redeemer_index: 3,
        hub_ref_input_index: 4,
      },
    ) == #"d87a9f01020304ff",
    cbor.serialise(
      payout.MintPayout {
        withdrawal_utxo_out_ref: output_reference(),
        withdrawal_input_index: 1,
        withdrawal_spend_redeemer_index: 2,
        hub_ref_input_index: 3,
      },
    ) == #"d8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333301ff010203ff",
    cbor.serialise(
      payout.BurnPayout {
        payout_input_index: 1,
        payout_asset_name: #"aa",
        payout_spend_redeemer_index: 2,
        hub_ref_input_index: 3,
      },
    ) == #"d87a9f0141aa0203ff",
  }
}

test canonical_operator_directory_v1_typescript_abi_vectors() {
  and {
    cbor.serialise(
      operator_directory.SlashOperatorForBadState {
        state_queue_redeemer_index: 1,
      },
    ) == #"d8799f01ff",
    cbor.serialise(
      operator_directory.SlashOperatorForBadSettlement {
        settlement_input_index: 1,
        settlement_redeemer_index: 2,
      },
    ) == #"d87a9f0102ff",
    cbor.serialise(slashing_arguments()) == #"d8799f581c1111111111111111111111111111111111111111111111111111111101d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff02d8799f03ffff",
    cbor.serialise(
      active.ShowOperatorIsInactive { scheduler_ref_input_index: 1 },
    ) == #"d8799f01ff",
    cbor.serialise(
      active.ShowSchedulerIsAdvancing {
        scheduler_input_index: 1,
        scheduler_redeemer_index: 2,
        removing_operators_anchor_element_key: Some(#"aa"),
        removing_operator_is_the_last_member: True,
      },
    ) == #"d87a9f0102d8799f41aaffd87a80ff",
    cbor.serialise(active.ListStateTransition) == #"d87980",
    cbor.serialise(
      active.UpdateBondHoldNewState {
        active_operator: h28_a,
        active_node_input_index: 1,
        active_node_output_index: 2,
        hub_oracle_ref_input_index: 3,
        state_queue_redeemer_index: 4,
      },
    ) == #"d87a9f581c1111111111111111111111111111111111111111111111111111111101020304ff",
    cbor.serialise(
      active.UpdateBondHoldNewSettlement {
        active_operator: h28_a,
        active_node_input_index: 1,
        active_node_output_index: 2,
        hub_oracle_ref_input_index: 3,
        settlement_input_index: 4,
        settlement_redeemer_index: 5,
        resolution_time: 6,
      },
    ) == #"d87b9f581c11111111111111111111111111111111111111111111111111111111010203040506ff",
    cbor.serialise(
      active.StrikeForInactivity {
        active_node_input_index: 1,
        active_node_output_index: 2,
        operator: h28_a,
        active_node_link: Some(#"aa"),
        scheduler_input_index: 3,
        scheduler_redeemer_index: 4,
        hub_oracle_ref_input_index: 5,
      },
    ) == #"d87c9f0102581c11111111111111111111111111111111111111111111111111111111d8799f41aaff030405ff",
    cbor.serialise(active.Init { output_index: 1 }) == #"d8799f01ff",
    cbor.serialise(active.Deinit) == #"d87a80",
    cbor.serialise(
      active.ActivateOperator {
        new_active_operator_key: h28_a,
        active_operator_anchor_element_output_index: 1,
        active_operator_inserted_node_output_index: 2,
        registered_operators_redeemer_index: 3,
        active_operators_set_was_empty: False,
      },
    ) == #"d87b9f581c11111111111111111111111111111111111111111111111111111111010203d87980ff",
    cbor.serialise(
      active.RetireOperator {
        active_operator_key: h28_a,
        hub_oracle_ref_input_index: 1,
        active_operator_anchor_element_input_outref: output_reference(),
        active_operator_anchor_element_output_index: 2,
        retired_operators_redeemer_index: 3,
        penalize_for_inactivity: True,
        operator_removal_scheduler_sync: active.ShowSchedulerIsAdvancing {
          scheduler_input_index: 4,
          scheduler_redeemer_index: 5,
          removing_operators_anchor_element_key: None,
          removing_operator_is_the_last_member: False,
        },
      },
    ) == #"d87c9f581c1111111111111111111111111111111111111111111111111111111101d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff0203d87a80d87a9f0405d87a80d87980ffff",
    cbor.serialise(
      active.SlashOperator {
        slashing_arguments: slashing_arguments(),
        operator_removal_scheduler_sync: active.ShowOperatorIsInactive {
          scheduler_ref_input_index: 4,
        },
      },
    ) == #"d87d9fd8799f581c1111111111111111111111111111111111111111111111111111111101d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff02d8799f03ffffd8799f04ffff",
    cbor.serialise(
      active.NodeData { bond_unlock_time: None, inactivity_strikes: 1 },
    ) == #"d8799fd87a8001ff",
    cbor.serialise(active_root_datum()) == #"d8799fd8799f40ffd87a80ff",
    cbor.serialise(
      linked_list.Element {
        data: linked_list.Node {
          data: active.NodeData {
            bond_unlock_time: None,
            inactivity_strikes: 1,
          },
        },
        link: Some(#"aa"),
      },
    ) == #"d8799fd87a9fd8799fd87a8001ffffd8799f41aaffff",
    cbor.serialise(registered.NodeData { operator: h28_a }) == #"d8799f581c11111111111111111111111111111111111111111111111111111111ff",
    cbor.serialise(registered.DuplicateIsRegistered) == #"d87980",
    cbor.serialise(
      registered.DuplicateIsActive { hub_oracle_ref_input_index: 1 },
    ) == #"d87a9f01ff",
    cbor.serialise(registered.DuplicateIsRetired) == #"d87b80",
    cbor.serialise(registered.Init { output_index: 1 }) == #"d8799f01ff",
    cbor.serialise(registered.Deinit) == #"d87a80",
    cbor.serialise(
      registered.RegisterOperator {
        registering_operator: h28_a,
        root_output_index: 1,
        registered_node_output_index: 2,
        hub_oracle_ref_input_index: 3,
        active_operators_element_ref_input_index: 4,
        retired_operators_element_ref_input_index: 5,
      },
    ) == #"d87b9f581c111111111111111111111111111111111111111111111111111111110102030405ff",
    cbor.serialise(
      registered.ActivateOperator {
        activating_operator: h28_a,
        anchor_element_input_outref: output_reference(),
        anchor_element_output_index: 1,
        hub_oracle_ref_input_index: 2,
        retired_operators_element_ref_input_index: 3,
        active_operators_redeemer_index: 4,
      },
    ) == #"d87c9f581c11111111111111111111111111111111111111111111111111111111d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff01020304ff",
    cbor.serialise(
      registered.DeregisterOperator {
        deregistering_operator: h28_a,
        anchor_element_input_outref: output_reference(),
        anchor_element_output_index: 1,
      },
    ) == #"d87d9f581c11111111111111111111111111111111111111111111111111111111d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff01ff",
    cbor.serialise(
      registered.SlashDuplicateOperator {
        duplicate_operator: h28_a,
        anchor_element_input_outref: output_reference(),
        anchor_element_output_index: 1,
        duplicate_node_ref_input_index: 2,
        duplicate_operator_status: registered.DuplicateIsActive {
          hub_oracle_ref_input_index: 3,
        },
      },
    ) == #"d87e9f581c11111111111111111111111111111111111111111111111111111111d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff0102d87a9f03ffff",
    cbor.serialise(
      linked_list.Element {
        data: linked_list.Node { data: registered.NodeData { operator: h28_a } },
        link: None,
      },
    ) == #"d8799fd87a9fd8799f581c11111111111111111111111111111111111111111111111111111111ffffd87a80ff",
    cbor.serialise(retired.NodeData { bond_unlock_time: Some(9) }) == #"d8799fd8799f09ffff",
    cbor.serialise(retired.Init { output_index: 1 }) == #"d8799f01ff",
    cbor.serialise(retired.Deinit) == #"d87a80",
    cbor.serialise(
      retired.RetireOperator {
        new_retired_operator_key: h28_a,
        bond_unlock_time: None,
        hub_oracle_ref_input_index: 1,
        retired_operator_anchor_element_output_index: 2,
        retired_operator_inserted_node_output_index: 3,
        active_operators_redeemer_index: 4,
      },
    ) == #"d87b9f581c11111111111111111111111111111111111111111111111111111111d87a8001020304ff",
    cbor.serialise(
      retired.RecoverOperatorBond {
        retired_operator_key: h28_a,
        retired_operator_anchor_element_input_outref: output_reference(),
        retired_operator_anchor_element_output_index: 1,
      },
    ) == #"d87c9f581c11111111111111111111111111111111111111111111111111111111d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff01ff",
    cbor.serialise(
      retired.SlashOperator { slashing_arguments: slashing_arguments() },
    ) == #"d87d9fd8799f581c1111111111111111111111111111111111111111111111111111111101d8799f5820333333333333333333333333333333333333333333333333333333333333333301ff02d8799f03ffffff",
    cbor.serialise(
      linked_list.Element {
        data: linked_list.Node {
          data: retired.NodeData { bond_unlock_time: Some(9) },
        },
        link: None,
      },
    ) == #"d8799fd87a9fd8799fd8799f09ffffffd87a80ff",
  }
}

test canonical_settlement_v1_typescript_abi_vectors() {
  and {
    cbor.serialise(
      settlement.ResolutionClaim { resolution_time: 1, operator: h28_a },
    ) == #"d8799f01581c11111111111111111111111111111111111111111111111111111111ff",
    cbor.serialise(
      settlement.Datum {
        deposits_root: h32_a,
        withdrawals_root: h32_b,
        forced_transactions_root: h32_a,
        transactions_root: h32_b,
        resolution_claim: None,
      },
    ) == #"d8799f58203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444445820333333333333333333333333333333333333333333333333333333333333333358204444444444444444444444444444444444444444444444444444444444444444d87a80ff",
    cbor.serialise(
      settlement.Datum {
        deposits_root: h32_a,
        withdrawals_root: h32_b,
        forced_transactions_root: h32_a,
        transactions_root: h32_b,
        resolution_claim: Some(
          settlement.ResolutionClaim { resolution_time: 1, operator: h28_a },
        ),
      },
    ) == #"d8799f58203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444445820333333333333333333333333333333333333333333333333333333333333333358204444444444444444444444444444444444444444444444444444444444444444d8799fd8799f01581c11111111111111111111111111111111111111111111111111111111ffffff",
    cbor.serialise(settlement.Deposit) == #"d87980",
    cbor.serialise(
      settlement.Withdrawal {
        validity_override: ledger_state.IncorrectWithdrawalValue,
      },
    ) == #"d87a9fd87d80ff",
    cbor.serialise(
      settlement.TxOrder {
        validity_override: rejection_reason_v1.ForcedTxValid,
      },
    ) == #"d87b9fd87980ff",
    cbor.serialise(
      settlement.TxOrder {
        validity_override: rejection_reason_v1.ForcedTxInvalid {
          reason: rejection_reason_v1.EmptyInputs,
        },
      },
    ) == #"d87b9fd87a9fd87b80ffff",
    cbor.serialise(
      settlement.DepositMembership {
        witness: raw_membership_proof(transition_trace.DepositsRootDomain),
      },
    ) == #"d8799fd8799fd87c8058203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444440141aa41bb80ffff",
    cbor.serialise(
      settlement.WithdrawalMembership {
        witness: raw_membership_proof(transition_trace.WithdrawalsRootDomain),
      },
    ) == #"d87a9fd8799fd8798058203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444440141aa41bb80ffff",
    cbor.serialise(
      settlement.TxOrderMembership {
        witness: raw_membership_proof(transition_trace.TransactionsV1RootDomain),
      },
    ) == #"d87b9fd8799fd87b8058203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444440141aa41bb80ffff",
    cbor.serialise(
      settlement.AttachResolutionClaim {
        settlement_input_index: 1,
        settlement_output_index: 2,
        hub_ref_input_index: 3,
        active_operators_node_input_index: 4,
        active_operators_redeemer_index: 5,
        operator: h28_a,
        scheduler_ref_input_index: 6,
      },
    ) == #"d8799f0102030405581c1111111111111111111111111111111111111111111111111111111106ff",
    cbor.serialise(
      settlement.DisproveResolutionClaim {
        settlement_input_index: 1,
        settlement_output_index: 2,
        hub_ref_input_index: 3,
        operators_redeemer_index: 4,
        operator: h28_a,
        operator_is_active: True,
        unresolved_event_ref_input_index: 5,
        unresolved_event_asset_name: #"aa",
        event_type: settlement.Deposit,
        membership_proof: settlement.DepositMembership {
          witness: raw_membership_proof(transition_trace.DepositsRootDomain),
        },
        inclusion_proof_script_withdraw_redeemer_index: 6,
      },
    ) == #"d87a9f01020304581c11111111111111111111111111111111111111111111111111111111d87a800541aad87980d8799fd8799fd87c8058203333333333333333333333333333333333333333333333333333333333333333582044444444444444444444444444444444444444444444444444444444444444440141aa41bb80ffff06ff",
    cbor.serialise(settlement.Resolve { settlement_id: #"aa" }) == #"d87b9f41aaff",
    cbor.serialise(
      settlement.Spawn {
        settlement_id: #"aa",
        output_index: 1,
        state_queue_merge_redeemer_index: 2,
        hub_ref_input_index: 3,
      },
    ) == #"d8799f41aa010203ff",
    cbor.serialise(
      settlement.Remove {
        settlement_id: #"aa",
        input_index: 1,
        spend_redeemer_index: 2,
      },
    ) == #"d87a9f41aa0102ff",
  }
}

// V-11 (#532). Every rejection test below used to end in
// `cbor.serialise(decoded) == cbor.serialise(decoded)`, a tautology that is
// True for every value the generated decoder could possibly produce, so no
// round-trip property was established. The intended property is the canonical
// round trip, and the final expression now compares the re-serialised value
// against the exact source bytes. The `expect` still carries the rejection for
// the malformed inputs pinned here; the round trip is what lets the test
// distinguish a decodable-but-noncanonical source from a canonical one, which
// the tautology never could.
//
// Control (run under the fork, reverted): replacing the source of
// `event_v1_rejects_a_wrong_deposit_datum_arity` with the well-formed
// `deposit_datum()` encoding turns the test red, so the pinned inputs are
// rejected because these decoders reject them, not because the module always
// fails.
test event_v1_rejects_an_adjacent_active_operator_mint_tag() fail {
  let source = #"d87e80"
  expect Some(data) = cbor.deserialise(source)
  expect decoded: active.MintRedeemer = data
  cbor.serialise(decoded) == source
}

test event_v1_rejects_an_adjacent_user_event_mint_tag() fail {
  let source = #"d87b80"
  expect Some(data) = cbor.deserialise(source)
  expect decoded: user_events.MintRedeemer = data
  cbor.serialise(decoded) == source
}

test event_v1_rejects_an_adjacent_user_event_witness_tag() fail {
  let source = #"d87c80"
  expect Some(data) = cbor.deserialise(source)
  expect decoded: event_witness.PublishRedeemer = data
  cbor.serialise(decoded) == source
}

test event_v1_rejects_a_wrong_deposit_datum_arity() fail {
  let source = #"d87980"
  expect Some(data) = cbor.deserialise(source)
  expect decoded: deposit.DepositDatum = data
  cbor.serialise(decoded) == source
}

test event_v1_rejects_a_decorative_reserve_version_field() fail {
  let source = #"d8799f0102030400ff"
  expect Some(data) = cbor.deserialise(source)
  expect decoded: reserve.SpendRedeemer = data
  cbor.serialise(decoded) == source
}

test event_v1_rejects_a_malformed_active_operator_link() fail {
  let source = #"d87c9f0102581c1111111111111111111111111111111111111111111111111111111101030405ff"
  expect Some(data) = cbor.deserialise(source)
  expect decoded: active.SpendRedeemer = data
  cbor.serialise(decoded) == source
}

test event_v1_rejects_an_arbitrary_retired_slashing_payload() fail {
  let source = #"d87d9f01ff"
  expect Some(data) = cbor.deserialise(source)
  expect decoded: retired.MintRedeemer = data
  cbor.serialise(decoded) == source
}

test event_v1_rejects_an_adjacent_settlement_event_tag() fail {
  let source = #"d87c80"
  expect Some(data) = cbor.deserialise(source)
  expect decoded: settlement.EventType = data
  cbor.serialise(decoded) == source
}

// Issue #521 regression pins. `deposit.DepositDatum` used to be named `Datum`,
// colliding with stdlib `cardano/transaction.Datum` in stock aiken v1.1.22's
// generated-decoder cache. `transaction.Datum` won, so `scheduler.spend`'s
// `NeglectedDeposit` branch admitted `NoDatum`/`InlineDatum` shapes and could
// not decode a well-formed deposit datum at all. This first test forces both
// decoders into one compiled program, the only shape in which `aiken check`
// can observe the collision.
test event_v1_deposit_and_transaction_datum_decoders_stay_distinct() {
  expect Some(deposit_data) = cbor.deserialise(cbor.serialise(deposit_datum()))
  expect decoded_deposit: deposit.DepositDatum = deposit_data
  expect Some(no_datum_data) = cbor.deserialise(#"d87980")
  expect decoded_no_datum: Datum = no_datum_data
  expect Some(inline_datum_data) = cbor.deserialise(#"d87b9f182aff")
  expect decoded_inline_datum: Datum = inline_datum_data
  and {
    decoded_deposit == deposit_datum(),
    decoded_no_datum == NoDatum,
    decoded_inline_datum == InlineDatum(42),
  }
}

test event_v1_accepts_a_well_formed_deposit_datum() {
  expect Some(data) = cbor.deserialise(cbor.serialise(deposit_datum()))
  expect decoded: deposit.DepositDatum = data
  decoded == deposit_datum()
}

test event_v1_rejects_a_deposit_datum_with_an_inline_datum_tag() fail {
  let source = #"d87b9f182aff"
  expect Some(data) = cbor.deserialise(source)
  expect decoded: deposit.DepositDatum = data
  cbor.serialise(decoded) == source
}
