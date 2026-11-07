use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/input_existence_fixture_v1 as carriage_fixture
use midgard/fraud_proofs/input_existence_v1/select_step
use midgard/fraud_proofs/input_existence_v1/types.{SelectArgsV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput} as native_types
use midgard/fraud_proofs/no_input/step_02.{Datum, SpendRedeemer}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    select_step.select_step_v1(
      0,
      step_03_validator_script_hash,
      computation_thread_token_policy_id,
      field_preimage_certificate_policy_id,
      datum,
      redeemer,
      own_out_ref,
      tx,
    )
  }

  else(_) {
    fail
  }
}

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)
//
// Step-02 opens the disputed transaction's field-0 preimage through the one
// door and selects the input the prover claims does not exist. Because the
// expected commitment is extracted positionally inside the door from the
// tx-id-verified compact structures, a substituted preimage cannot be opened at
// all — and because the read is arithmetic, selecting input 295 costs what
// selecting input 0 costs.

fn prev_utxos_root() -> ByteArray {
  fixture.challenged_block_v1().header.prev_utxos_root
}

fn transactions_root() -> ByteArray {
  fixture.challenged_block_v1().transactions_phas_root
}

fn run_step_02(
  block: fixture.NativeBlockFixtureV1,
  carried_inputs: List<MidgardTxInput>,
  bad_input_index: Int,
  expected_missing_input: MidgardTxInput,
) -> Bool {
  let claim = tf.thread_claim_v1(
    ledger_state.L2TransactionEventKey { tx_id: block.tx_id },
    ledger_state.L2Transaction,
    block.header.prev_utxos_root,
    None,
  )
  let expected_output_state: Data = types.SelectedInputStateV1 {
    compact: carriage_fixture.compact_view_v1(block.compact),
    source_kind: 0,
    input_index: bad_input_index,
    claimed_input: None,
    subject: substrate.VerdictSubjectV1 {
      version: substrate.version,
      direction: substrate.direction_wrongful_acceptance,
      source_kind: substrate.source_kind_accepted,
      transaction_id: block.tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    header: claim.header,
    selected_input: Some(expected_missing_input),
    step_index: claim.event_to_step_membership.value.step_index,
  }
  let asset_name = fixture.ct_token_asset_name(fixture.challenged_block_v1())
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          types.SourceStateV1 {
            compact: carriage_fixture.compact_view_v1(block.compact),
            subject: substrate.VerdictSubjectV1 {
              version: substrate.version,
              direction: substrate.direction_wrongful_acceptance,
              source_kind: substrate.source_kind_accepted,
              transaction_id: block.tx_id,
              source_key: #"",
              rejection_reason: None,
            },
            header: claim.header,
          },
        ),
      },
    ),
    ct.Continue(
      SelectArgsV1 {
        input_index: 0,
        output_index: 0,
        inputs_opening: fixture.inline_body_opening(
          block.compact_cbor,
          carried_inputs |> list.map(encode_midgard_tx_input),
        ),
        selected_input_index: bad_input_index,
        claimed_inputs_opening: None,
        event_membership: carriage_fixture.direct_carried_membership_v1(
          claim.event_to_step_membership,
          0,
        ),
      },
    ),
    fixture.own_out_ref(),
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
      redeemers: [
        fixture.phas_withdraw_redeemer_entry(
          claim.event_to_step_membership.phas_root,
          cbor.serialise(claim.event_to_step_membership.key),
          cbor.serialise(claim.event_to_step_membership.value),
        ),
      ],
    },
  )
}

fn sample_input(tx_id: ByteArray, output_index: Int) -> MidgardTxInput {
  MidgardTxInput { tx_id, output_index }
}

/// The spend inputs the fixture transaction actually commits — the only
/// preimage the door will open for it.
fn challenged_inputs() -> List<MidgardTxInput> {
  fixture.challenged_block_v1().tx.body.inputs
}

fn first_challenged_input() -> MidgardTxInput {
  expect Some(input) = list.at(challenged_inputs(), 0)
  input
}

/// Positive selector: the carried preimage is the one the disputed
/// transaction's field-0 commitment authenticates, so the input at the
test no_input_step_02_forwards_the_challenged_spend_input() {
  run_step_02(
    fixture.challenged_block_v1(),
    challenged_inputs(),
    0,
    first_challenged_input(),
  )
}

/// Valid-block negative selector: a valid block commits each transaction's real
/// spend-inputs collection, so a prover who carries a different preimage fails
/// the door's authenticate-once check and cannot select an input the
/// transaction never spent.
test no_input_step_02_rejects_a_substituted_inputs_preimage() fail {
  let injected = sample_input(
    #"3333333333333333333333333333333333333333333333333333333333333333",
    0,
  )
  run_step_02(
    fixture.challenged_block_v1(),
    [injected, ..challenged_inputs()],
    0,
    injected,
  )
}

/// §7.3 abort-never-clamp, at the family's own call site: an index past the
/// committed collection fails the read instead of returning the nearest item.
test no_input_step_02_rejects_an_out_of_range_input_index() fail {
  run_step_02(
    fixture.challenged_block_v1(),
    challenged_inputs(),
    64,
    first_challenged_input(),
  )
}
