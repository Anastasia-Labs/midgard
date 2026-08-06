use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/bounded_collection_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
}
use midgard/fraud_proofs/reference_input_no_idx/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/reference_input_no_idx/step_03

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
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
        reference_inputs_preimage,
        bad_reference_input_index,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx

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

        expect Some(State { verified_tx_reference_inputs_hash }) = m_input_state_data

        // 2. Provided reference-inputs preimage must open the native
        //    reference-inputs commitment carried from step-01: consensus field
        //    1, canonical per-item CBOR. Reference inputs and spend inputs share
        //    this per-item encoding but not the field index, so a spend-inputs
        //    preimage cannot be substituted here.
        expect
          bounded_collection_v1.from_items(
            1,
            list.map(reference_inputs_preimage, encode_midgard_tx_input),
          ) == verified_tx_reference_inputs_hash

        let bad_reference_input = reference_inputs_preimage
          |> list.expect_at(bad_reference_input_index)

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
// Step-02 opens the reference-inputs commitment forwarded by step-01, and may
// forward only a reference input that preimage actually contains.

/// A transaction reading an out-of-range output index of `producing_tx_id`.
fn reference_input_no_idx_bad_tx(
  producing_tx_id: ByteArray,
) -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      reference_inputs: [
        MidgardTxInput { tx_id: producing_tx_id, output_index: 7 },
      ],
    },
  }
}

fn run_step_02(
  block: fixture.NativeBlockFixtureV1,
  verified_tx_reference_inputs_hash: ByteArray,
  reference_inputs_preimage: List<MidgardTxInput>,
  bad_reference_input_index: Int,
  forwarded: MidgardTxInput,
) -> Bool {
  let expected_output_state: Data = step_03.State {
    bad_reference_input_tx_id: forwarded.tx_id,
    bad_reference_input_output_index: forwarded.output_index,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(State { verified_tx_reference_inputs_hash }),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        reference_inputs_preimage,
        bad_reference_input_index,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      expected_output_state,
    ),
  )
}

/// Positive selector: the canonical reference-inputs preimage of the committed
/// bad transaction opens the hash carried from step-01, and the out-of-range
/// reference input it names is forwarded verbatim to step-03.
test reference_input_no_idx_step_02_forwards_the_committed_out_of_range_input() {
  let producing = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let block = fixture.native_block_fixture_v1(
    reference_input_no_idx_bad_tx(producing.tx_id),
  )
  run_step_02(
    block,
    block.compact.body.reference_inputs_hash,
    block.tx.body.reference_inputs,
    0,
    MidgardTxInput { tx_id: producing.tx_id, output_index: 7 },
  )
}

/// Valid-block negative selector: every transaction of a valid block reads a
/// reference input its producer really has, so a prover who substitutes a
/// fabricated out-of-range reference input for the committed preimage cannot
/// open the committed reference-inputs commitment.
test reference_input_no_idx_step_02_rejects_a_forged_inputs_preimage() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged = MidgardTxInput { tx_id: valid_block.tx_id, output_index: 7 }
  run_step_02(
    valid_block,
    valid_block.compact.body.reference_inputs_hash,
    [forged],
    0,
    forged,
  )
}
