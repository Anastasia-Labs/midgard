use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/double_spend/step_03.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/double_spend/step_04
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, spend_inputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
}
use midgard/native_tx_machine_walk_v1.{spend_input_at}

validator main(
  step_04_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        tx1_spend_inputs_opening,
        double_spent_input_index,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )

        expect Some(State { verified_tx1_id, verified_tx2_id }) = m_input_state_data

        // 2. tx1's field-0 preimage is authenticated through the §8.8 door
        //    against the commitment the door extracts positionally from the
        //    compact structures `verified_tx1_id` binds. Nothing here holds a
        //    free-standing field hash: under §4's plain hashing a field-0 and a
        //    field-1 preimage over the same items commit identically, so the
        //    slot comes from the position, not from the number.
        let tx1_spend_inputs_view = opened_field_view(
          tx1_spend_inputs_opening,
          BodyAnchor { tx_id: verified_tx1_id },
          spend_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )

        // 3. The provided index must correspond to an input in that collection.
        //    §5.3's fixed 38-byte item makes this one multiplication and one
        //    slice however far into the collection the index points (§10.5).
        let tx1_double_spent_input = spend_input_at(
          tx1_spend_inputs_view,
          double_spent_input_index,
        )

        // 4. The fetched input, along with the unused second transaction id,
        //    must be carried over in the datum of the next step.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          verified_tx2_id,
          double_spent_input: tx1_double_spent_input,
        }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)
//
// Step-03 opens the first transaction's field-0 preimage through the one door
// and selects the allegedly double-spent input. The expected commitment is
// neither in the redeemer nor in the datum — the door derives it from the
// tx-id-verified compact structures — so a substituted preimage cannot be
// opened at all.

/// The second conflicting transaction's verified id. Opaque to this step —
/// step-04 opens it — but a real committed id, not a placeholder.
fn tx2_block() -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      body: MidgardTransactionBody { ..base.body, fee: 2_000_000 },
    },
  )
}

fn tx1_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn tx1_spend_inputs() -> List<MidgardTxInput> {
  tx1_block().tx.body.inputs
}

fn first_tx1_spend_input() -> MidgardTxInput {
  expect Some(input) = list.at(tx1_spend_inputs(), 0)
  input
}

fn run_step_03(
  carried_tx1_id: ByteArray,
  opening_compact_cbor: ByteArray,
  carried_tx1_inputs: List<MidgardTxInput>,
  double_spent_input_index: Int,
  expected_double_spent_input: MidgardTxInput,
) -> Bool {
  let verified_tx2_id = tx2_block().tx_id
  let expected_output_state: Data = step_04.State {
    verified_tx2_id,
    double_spent_input: expected_double_spent_input,
  }
  let asset_name = fixture.ct_token_asset_name(tx1_block())
  let l1_tx = Transaction {
    ..transaction.placeholder,
    inputs: [fixture.ct_thread_input(asset_name)],
    outputs: [fixture.next_step_output(asset_name, expected_output_state)],
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(State { verified_tx1_id: carried_tx1_id, verified_tx2_id }),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        tx1_spend_inputs_opening: fixture.inline_body_opening(
          opening_compact_cbor,
          carried_tx1_inputs |> list.map(encode_midgard_tx_input),
        ),
        double_spent_input_index,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive selector: the carried preimage is the one tx1's field-0 commitment
/// authenticates, so the selected input is carried to step-04 alongside the
/// untouched second transaction id.
test double_spend_step_03_forwards_the_double_spent_input() {
  let block = tx1_block()
  run_step_03(
    block.tx_id,
    block.compact_cbor,
    tx1_spend_inputs(),
    0,
    first_tx1_spend_input(),
  )
}

/// Valid-block negative selector: distinct transactions of a valid block spend
/// disjoint inputs, so a conflict can only be manufactured by carrying a
/// preimage that is not the bound transaction's. The door's authenticate-once
/// hash check rejects it.
test double_spend_step_03_rejects_an_unauthenticated_inputs_preimage() fail {
  let block = tx1_block()
  let injected = MidgardTxInput {
    tx_id: #"3131313131313131313131313131313131313131313131313131313131313131",
    output_index: 1,
  }
  run_step_03(block.tx_id, block.compact_cbor, [injected], 0, injected)
}

/// The compact structures must be the thread's transaction's: carrying another
/// committed transaction's compact CBOR fails the tx-id re-derivation before
/// the door is reached, so the positional extraction cannot be pointed at a
/// transaction the thread never bound.
test double_spend_step_03_rejects_compact_structures_of_another_transaction() fail {
  let block = tx1_block()
  run_step_03(
    block.tx_id,
    tx2_block().compact_cbor,
    tx1_spend_inputs(),
    0,
    first_tx1_spend_input(),
  )
}

/// §7.3 abort-never-clamp: an index past the committed collection fails the
/// read instead of returning the nearest item.
test double_spend_step_03_rejects_an_out_of_range_input_index() fail {
  let block = tx1_block()
  run_step_03(
    block.tx_id,
    block.compact_cbor,
    tx1_spend_inputs(),
    9,
    first_tx1_spend_input(),
  )
}
