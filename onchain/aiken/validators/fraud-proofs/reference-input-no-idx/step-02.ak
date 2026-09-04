use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, reference_inputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
}
use midgard/fraud_proofs/reference_input_no_idx/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/reference_input_no_idx/step_03
use midgard/native_tx_machine_walk_v1.{spend_input_at}

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
        reference_inputs_opening,
        bad_reference_input_index,
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

        expect Some(State { verified_tx_id }) = m_input_state_data

        // 2. The challenged reference input is read out of field 1's preimage
        //    through the §8.8 door. The field index is a literal here, never a
        //    redeemer argument, so a step meaning to read field 1 cannot be
        //    steered onto field 0 — which matters because §4 removed the domain
        //    separation that used to make reference- and spend-input preimages
        //    distinguishable by hash.
        //
        //    Reference inputs share field 0's fixed 38-byte stride (§5.3/§10.5),
        //    so `spend_input_at` locates item `n` by one multiplication and one
        //    slice: no walk is entered and the cost does not grow with the
        //    disputed transaction. This is where the family's counted
        //    `field_commitment_from_items` re-hash of a reproduced list lived.
        let reference_inputs_view = opened_field_view(
          reference_inputs_opening,
          BodyAnchor { tx_id: verified_tx_id },
          reference_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let bad_reference_input = spend_input_at(
          reference_inputs_view,
          bad_reference_input_index,
        )

        // 3. The fetched reference input's tx id and output index must be passed
        //    to the next step so that its producing transaction can be verified
        //    against the included hash.
        expect output_script_hash == step_03_validator_script_hash
        let
            MidgardTxInput {
              tx_id: bad_reference_input_tx_id,
              output_index: bad_reference_input_output_index,
            }
          = bad_reference_input
        let expected_output_state: Data = step_03.State {
          bad_reference_input_tx_id,
          bad_reference_input_output_index,
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

// ## Selectors
//
// Step-02 opens the disputed transaction's field-1 (reference-inputs) preimage
// through the one door and selects the reference input whose producing output
// index the family goes on to prove does not exist.

/// A committed block whose transaction reads an out-of-range output index of
/// `producing_tx_id` through a single reference input.
fn reference_input_no_idx_bad_block(
  producing_tx_id: ByteArray,
) -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      body: MidgardTransactionBody {
        ..base.body,
        reference_inputs: [
          MidgardTxInput { tx_id: producing_tx_id, output_index: 7 },
        ],
      },
    },
  )
}

fn run_step_02(
  block: fixture.NativeBlockFixtureV1,
  carried_reference_inputs: List<MidgardTxInput>,
  bad_reference_input_index: Int,
  expected_bad_input: MidgardTxInput,
) -> Bool {
  let expected_output_state: Data = step_03.State {
    bad_reference_input_tx_id: expected_bad_input.tx_id,
    bad_reference_input_output_index: expected_bad_input.output_index,
  }
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(State { verified_tx_id: block.tx_id }),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        reference_inputs_opening: fixture.inline_body_opening(
          block.compact_cbor,
          carried_reference_inputs |> list.map(encode_midgard_tx_input),
        ),
        bad_reference_input_index,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

/// Positive selector: the canonical reference-inputs preimage of the committed
/// bad transaction opens field 1 through the door, and the out-of-range
/// reference input it names is forwarded verbatim to step-03.
test reference_input_no_idx_step_02_forwards_the_committed_out_of_range_input() {
  let producing = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let block = reference_input_no_idx_bad_block(producing.tx_id)
  run_step_02(
    block,
    block.tx.body.reference_inputs,
    0,
    MidgardTxInput { tx_id: producing.tx_id, output_index: 7 },
  )
}

/// Valid-block negative selector: a reference-inputs preimage the transaction
/// never committed fails the door's authenticate-once check, so a fabricated
/// out-of-range reference input cannot be walked towards a non-existence
/// verdict.
test reference_input_no_idx_step_02_rejects_a_forged_inputs_preimage() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged = MidgardTxInput { tx_id: valid_block.tx_id, output_index: 7 }
  run_step_02(valid_block, [forged], 0, forged)
}

/// §7.3 abort-never-clamp at the family's own call site: the index is a range
/// guard the door enforces, not a clamp that silently returns the last
/// reference input. The committed bad transaction has exactly one reference
/// input, so index 3 is out of range and the step fails rather than pass.
test reference_input_no_idx_step_02_rejects_an_out_of_range_reference_input_index() fail {
  let producing = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let block = reference_input_no_idx_bad_block(producing.tx_id)
  run_step_02(
    block,
    block.tx.body.reference_inputs,
    3,
    MidgardTxInput { tx_id: producing.tx_id, output_index: 7 },
  )
}
