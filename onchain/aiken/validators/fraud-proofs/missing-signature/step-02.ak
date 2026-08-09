use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, required_signers_field_index,
}
use midgard/fraud_proofs/missing_signature/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/missing_signature/step_03
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/native_tx_field_access_v1.{field_item_at}

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
        required_signers_opening,
        bad_required_signer_hash_index,
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

        expect Some(State { verified_tx_id, verified_witness_set_hash }) = m_input_state_data

        // 2. Fetch the required signer hash out of field 4's authenticated
        //    preimage through the §8.8 door. §5.3 fixes the item at a raw
        //    28-byte hash, so signer `n` is one multiplication and one slice —
        //    no reproduction of the signer list, and no re-hash of it.
        let required_signers_view = opened_field_view(
          required_signers_opening,
          BodyAnchor { tx_id: verified_tx_id },
          required_signers_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let missing_required_signer_hash = field_item_at(
          required_signers_view,
          bad_required_signer_hash_index,
        )

        // 3. The required signer (whose signature is being proven missing),
        //    along with the verified transaction id, must be passed to the next
        //    step.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data = step_03.State {
          missing_required_signer_hash,
          verified_tx_id,
          verified_witness_set_hash,
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
// Step-02 opens field 4 of the disputed transaction through the one door and
// selects the required signer whose signature step-04 proves absent.

fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn challenged_required_signers() -> List<ByteArray> {
  challenged_block().tx.body.required_signers
}

fn first_required_signer() -> ByteArray {
  expect Some(signer) = list.at(challenged_required_signers(), 0)
  signer
}

fn run_step_02(
  block: fixture.NativeBlockFixtureV1,
  carried_signers: List<ByteArray>,
  bad_required_signer_hash_index: Int,
  expected_missing_required_signer_hash: ByteArray,
) -> Bool {
  let expected_output_state: Data = step_03.State {
    missing_required_signer_hash: expected_missing_required_signer_hash,
    verified_tx_id: block.tx_id,
    verified_witness_set_hash: block.compact.witness_set_hash,
  }
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            verified_tx_id: block.tx_id,
            verified_witness_set_hash: block.compact.witness_set_hash,
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        required_signers_opening: fixture.inline_body_opening(
          block.compact_cbor,
          carried_signers,
        ),
        bad_required_signer_hash_index,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

/// Positive selector: the carried preimage is what field 4 commits, so the
/// selected required signer reaches step-03 with the transaction id.
test missing_signature_step_02_forwards_the_required_signer() {
  run_step_02(
    challenged_block(),
    challenged_required_signers(),
    0,
    first_required_signer(),
  )
}

/// Valid-block negative selector: a signer list the transaction never committed
/// fails the door's authenticate-once check, so no invented required signer can
/// be carried onward.
test missing_signature_step_02_rejects_a_substituted_signer_list() fail {
  let injected = #"30303030303030303030303030303030303030303030303030303030"
  run_step_02(challenged_block(), [injected], 0, injected)
}

/// §7.3 abort-never-clamp at the family's own call site.
test missing_signature_step_02_rejects_an_out_of_range_signer_index() fail {
  run_step_02(
    challenged_block(),
    challenged_required_signers(),
    4,
    first_required_signer(),
  )
}
