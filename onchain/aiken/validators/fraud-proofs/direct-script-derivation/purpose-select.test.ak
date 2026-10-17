use aiken/cbor
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference, Transaction}
use fraud_proofs/direct_script_derivation/purpose_select as select_validator
use fraud_proofs/direct_script_derivation/spend_select as spend_validator
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{RedeemerCarriedMembership}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1,
}
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as door
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{BodyFieldOpening}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1 as descriptor
use midgard/native_tx_field_access_v1.{Inline}

// The selection door and the spend door split one selection step. These rows
// run both validators against L1 transactions: the door may hand its state
// only to the spend door, and the spend door's state must equal what the
// single door produced (selected credential plus field-0 cursor).

const low = #"1111111111111111111111111111111111111111111111111111111111111111"

const middle =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const high = #"3333333333333333333333333333333333333333333333333333333333333333"

const script_hash = #"22222222222222222222222222222222222222222222222222222222"

const other_script_hash =
  #"99999999999999999999999999999999999999999999999999999999"

fn selected_descriptor(output_index: Int) -> ByteArray {
  let summary = DataSummaryV1 { root: low, cbor_length: 1, memory: 1 }
  descriptor.encode(
    descriptor.LedgerOutputCommitmentV1 {
      version: 1,
      output_index,
      total_length: 50,
      item_commitment: low,
      address: bytearray.concat(#"78", script_hash),
      lovelace: 5000000,
      asset_count: 0,
      asset_frontier_commitment: low,
      cardano_value_size: 5,
      reference_script_language: -1,
      reference_script_hash: #"",
      reference_script_total_length: 0,
      reference_script_item_commitment: #"",
      cardano_tx_out: summary,
      midgard_tx_out: summary,
      cardano_spend_datum: summary,
    },
  )
}

type SpendFixture {
  source: BoundIndexV1,
  args: door.SpendSelectionArgsV1,
  tx: Transaction,
  thread_name: ByteArray,
}

// Raw item 1 of three spent inputs, an output of a script credential that is
// a member of the event pre-state.
fn spend_fixture() -> SpendFixture {
  let inputs = [
    native_types.MidgardTxInput { tx_id: low, output_index: 7 },
    native_types.MidgardTxInput { tx_id: middle, output_index: 2 },
    native_types.MidgardTxInput { tx_id: high, output_index: 9 },
  ]
  let base = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..base,
      body: native_types.MidgardTransactionBody { ..base.body, inputs },
    },
  )
  let key = ledger_outref_key(
    OutputReference { transaction_id: middle, output_index: 2 },
  )
  let value = selected_descriptor(2)
  let pre_root = mpf.insert(mpf.empty, key, value, []) |> mpf.root
  SpendFixture {
    source: BoundIndexV1 {
      bound: BoundV1 {
        subject: VerdictSubjectV1 {
          version: 1,
          direction: 0,
          source_kind: 0,
          transaction_id: block.tx_id,
          source_key: #"",
          rejection_reason: None,
        },
        compact_cbor: block.compact_cbor,
        network_order_context: NetworkOrderContextV1 {
          body_network_mismatch: False,
        },
        event_pre_root: pre_root,
      },
      source_index: source_index.StateV1 { root: high, count: 3 },
    },
    args: door.SpendSelectionArgsV1 {
      input_index: 0,
      output_index: 0,
      raw_item_index: 1,
      opening: fixture.inline_body_opening(
        block.compact_cbor,
        list.map(inputs, encode_midgard_tx_input),
      ),
      descriptor_cbor: value,
      membership: RedeemerCarriedMembership {
        membership_proof: [],
        membership_proof_script_redeemer_index: 0,
      },
    },
    tx: Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(block.tx_id, Void),
      redeemers: [fixture.phas_withdraw_redeemer_entry(pre_root, key, value)],
      extra_signatories: [fixture.fraud_prover_key_hash],
    },
    thread_name: block.tx_id,
  }
}

// The single selection door's spend state: the opened candidate, then the
// field-0 cursor initialized from the same bound and opening.
fn single_door_spend_state(spend: SpendFixture) -> door.SpendStateV1 {
  expect Some(target) = purpose.open_spend_v1(
    spend.source.bound,
    spend.args.raw_item_index,
    spend.args.opening,
    spend.args.descriptor_cbor,
    spend.args.membership,
    spend.tx.reference_inputs,
    spend.tx.redeemers,
    fixture.test_certificate_policy_id,
  )
  let (field_cursor, _, complete) = cursor.initial_with_completion_v1(
    spend.source.bound,
    0,
    spend.args.opening,
    spend.tx.reference_inputs,
    fixture.test_certificate_policy_id,
  )
  door.SpendStateV1 {
    source_index: spend.source.source_index,
    cursor: field_cursor,
    raw_item_index: spend.args.raw_item_index,
    target,
    rank: 0,
    complete,
  }
}

fn with_output(spend: SpendFixture, state: Data) -> Transaction {
  Transaction {
    ..spend.tx,
    outputs: fixture.continue_mid_step_l1_tx_v1(spend.thread_name, state).outputs,
  }
}

fn run_spend_door(
  spend: SpendFixture,
  destination: ByteArray,
  output_state: Data,
  signed: Bool,
) -> Bool {
  spend_validator.main.spend(
    destination,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(spend.source),
      },
    ),
    ct.Continue(spend.args),
    fixture.own_out_ref(),
    Transaction {
      ..with_output(spend, output_state),
      extra_signatories: if signed {
        spend.tx.extra_signatories
      } else {
        []
      },
    },
  )
}

fn run_selection_door(
  spend_select: ByteArray,
  spend_scan: ByteArray,
  output_state: Data,
) -> Bool {
  let spend = spend_fixture()
  select_validator.main.spend(
    other_script_hash,
    spend_select,
    other_script_hash,
    spend_scan,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(spend.source),
      },
    ),
    ct.Continue(door.Spend { input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    with_output(spend, output_state),
  )
}

test spend_door_state_equals_the_single_door_state() {
  let spend = spend_fixture()
  door.initial_spend_v1(
    spend.source,
    spend.args,
    spend.tx.reference_inputs,
    spend.tx.redeemers,
    fixture.test_certificate_policy_id,
  ) == single_door_spend_state(spend)
}

test spend_door_hands_the_single_door_state_to_the_rank_pass() {
  let spend = spend_fixture()
  run_spend_door(
    spend,
    fixture.next_step_script_hash,
    single_door_spend_state(spend),
    True,
  )
}

test spend_door_refuses_another_destination() {
  let spend = spend_fixture()
  !run_spend_door(
    spend,
    other_script_hash,
    single_door_spend_state(spend),
    True,
  )
}

test spend_door_refuses_an_uninitialized_cursor() {
  let spend = spend_fixture()
  let state = single_door_spend_state(spend)
  !run_spend_door(
    spend,
    fixture.next_step_script_hash,
    door.SpendStateV1 {
      ..state,
      cursor: cursor.CursorV1 { ..state.cursor, checkpoint_hash: #"" },
    },
    True,
  )
}

test spend_door_refuses_another_raw_item() fail {
  let spend = spend_fixture()
  run_spend_door(
    SpendFixture {
      ..spend,
      args: door.SpendSelectionArgsV1 { ..spend.args, raw_item_index: 0 },
    },
    fixture.next_step_script_hash,
    single_door_spend_state(spend),
    True,
  )
}

test spend_door_requires_the_thread_owner() fail {
  let spend = spend_fixture()
  run_spend_door(
    spend,
    fixture.next_step_script_hash,
    single_door_spend_state(spend),
    False,
  )
}

test selection_door_hands_its_state_to_the_spend_door() {
  run_selection_door(
    fixture.next_step_script_hash,
    other_script_hash,
    spend_fixture().source,
  )
}

// The spend scan is the next-step script here; the spend door is elsewhere.
test selection_door_cannot_hand_its_state_past_the_spend_door() {
  !run_selection_door(
    other_script_hash,
    fixture.next_step_script_hash,
    spend_fixture().source,
  )
}

test selection_door_cannot_alter_the_handed_state() {
  let spend = spend_fixture()
  !run_selection_door(
    fixture.next_step_script_hash,
    other_script_hash,
    BoundIndexV1 {
      ..spend.source,
      source_index: source_index.StateV1 { root: low, count: 3 },
    },
  )
}

// The same encodings are pinned off-chain in
// demo/midgard-sdk/tests/direct-purpose-workers.test.ts.
test selection_door_spend_hand_off_encodes_as_off_chain() {
  let redeemer: door.SelectionRedeemer = ct.Continue(
    door.Spend { input_index: 5, output_index: 6 },
  )
  cbor.serialise(redeemer) == #"d87a9fd8799f0506ffff"
}

test spend_door_arguments_encode_as_off_chain() {
  let redeemer: door.SpendSelectionRedeemer = ct.Continue(
    door.SpendSelectionArgsV1 {
      input_index: 1,
      output_index: 2,
      raw_item_index: 3,
      opening: BodyFieldOpening {
        native_tx_compact_cbor: #"a1",
        carriage: Inline { preimage: #"b2" },
      },
      descriptor_cbor: #"c3",
      membership: RedeemerCarriedMembership {
        membership_proof: [],
        membership_proof_script_redeemer_index: 4,
      },
    },
  )
  cbor.serialise(redeemer) == #"d87a9fd8799f010203d8799f41a1d8799f41b2ffff41c3d8799f8004ffffff"
}
