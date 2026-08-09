use aiken/collection/list
use aiken/crypto.{verify_ed25519_signature}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, address_witnesses_field_index, opened_field_view,
}
use midgard/fraud_proofs/invalid_signature/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, encode_midgard_address_witness,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransactionWitnessSet, NativeTxWitnessSetCompact,
}
use midgard/native_tx_field_access_v1.{field_item_at}

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
        addr_tx_wits_opening,
        bad_addr_tx_wit_index,
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data
          <- finalize(
          computation_thread_token_policy_id,
          fraud_proof_token_policy_id,
          fraud_proof_token_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_out_ref,
          inputs,
          outputs,
          redeemers,
        )

        expect Some(State { bad_tx_id, bad_tx_witness_set_hash }) = m_input_state_data

        // 2. Field 7's preimage is authenticated through the §8.8 door. Being a
        //    witness-set field, the door additionally re-derives the supplied
        //    witness set against `bad_tx_witness_set_hash` before it will read
        //    anything — so a witness set the transaction never committed cannot
        //    be passed off here. That hash is thread state, written by step-01
        //    off the block-committed compact structure, and *not* something the
        //    transaction id could be asked for: §3's id preimage is the body.
        let addr_tx_wits_view = opened_field_view(
          addr_tx_wits_opening,
          WitnessAnchor {
            tx_id: bad_tx_id,
            witness_set_hash: bad_tx_witness_set_hash,
          },
          address_witnesses_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )

        // 3. The referenced witness's signature over the native transaction ID
        //    must be invalid. §5.3 fixes the address-witness item at 101 bytes,
        //    so reaching witness `n` is arithmetic rather than a walk.
        let bad_addr_tx_wit = decode_midgard_address_witness_cbor(
          field_item_at(addr_tx_wits_view, bad_addr_tx_wit_index),
        )
        expect
          verify_ed25519_signature(
            bad_addr_tx_wit.verification_key,
            bad_tx_id,
            bad_addr_tx_wit.signature,
          ) == False

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

fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

/// Runs the step with the thread anchored to `anchored_block` while the
/// redeemer's opening is taken from `opened_block`.
///
/// The two are the same fixture in every selector but the both-sides forgery,
/// which is exactly the separation that makes that selector expressible: it is
/// the *only* way to hand the step compact bytes and a witness set that agree
/// with each other but not with what the thread authenticated.
fn run_finalize_against(
  anchored_block: fixture.NativeBlockFixtureV1,
  opened_block: fixture.NativeBlockFixtureV1,
  carried_witnesses: List<MidgardAddressWitness>,
  bad_addr_tx_wit_index: Int,
) -> Bool {
  let asset_name = fixture.ct_token_asset_name(anchored_block)
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            bad_tx_id: anchored_block.tx_id,
            bad_tx_witness_set_hash: anchored_block.compact.witness_set_hash,
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        addr_tx_wits_opening: fixture.inline_witness_opening(
          opened_block.compact_cbor,
          opened_block.witness_set_compact,
          carried_witnesses |> list.map(encode_midgard_address_witness),
        ),
        bad_addr_tx_wit_index,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

fn run_finalize(
  block: fixture.NativeBlockFixtureV1,
  witness_set: NativeTxWitnessSetCompact,
  carried_witnesses: List<MidgardAddressWitness>,
  bad_addr_tx_wit_index: Int,
) -> Bool {
  run_finalize_against(
    block,
    fixture.NativeBlockFixtureV1 { ..block, witness_set_compact: witness_set },
    carried_witnesses,
    bad_addr_tx_wit_index,
  )
}

/// The authenticated field-7 preimage of the committed transaction reveals an
/// invalid signature over the native transaction ID, finalizing the proof.
test invalid_signature_step_02_accepts_invalid_signature_witness() {
  let block = challenged_block()
  run_finalize(
    block,
    block.witness_set_compact,
    block.tx.witness_set.addr_tx_wits,
    0,
  )
}

/// A preimage that is not what field 7 commits (for example the empty
/// collection) fails the door's authenticate-once check and cannot be used as
/// witness evidence.
test invalid_signature_step_02_rejects_mismatched_witness_preimage() fail {
  let block = challenged_block()
  run_finalize(block, block.witness_set_compact, [], 0)
}

/// The witness set itself is authenticated: a forged one does not re-derive to
/// the `witness_set_hash` the transaction id commits, so the door refuses
/// before any witness is read. This is the guard step-01 used to run a step
/// early, against a redeemer-supplied preimage nothing downstream re-checked.
test invalid_signature_step_02_rejects_a_forged_witness_set() fail {
  let block = challenged_block()
  let forged = NativeTxWitnessSetCompact {
    ..block.witness_set_compact,
    addr_tx_wits_hash: #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
  }
  run_finalize(block, forged, block.tx.witness_set.addr_tx_wits, 0)
}

/// **Both sides forged.** The previous selector only forges the witness set,
/// which the door catches because the honest compact structure's
/// `witness_set_hash` no longer re-derives. The real attack forges *both*: §3's
/// transaction id commits the body and nothing else, so a prover can append its
/// own `witness_set_hash` to the genuine body — the bytes still re-derive to
/// this very `bad_tx_id` — and then present the witness set that hash commits.
/// Every check that looks only at the transaction id is satisfied. What refuses
/// it is the thread-anchored `bad_tx_witness_set_hash`, which step-01 read off
/// the block-committed structure and the prover cannot restate.
///
/// Here the invented witness is a verification key with a garbage signature: it
/// would prove an invalid-signature fault against a transaction whose real
/// witnesses are all sound, and slash an honest operator.
test invalid_signature_step_02_rejects_a_forged_witness_set_and_compact() fail {
  let block = challenged_block()
  let invented = [
    MidgardAddressWitness {
      verification_key: #"9090909090909090909090909090909090909090909090909090909090909090",
      signature: fixture.sample_witness_signature(),
    },
  ]
  let forged_block = fixture.forged_witness_set_block_v1(
    block,
    fixture.witness_set_compact_v1(
      MidgardTransactionWitnessSet {
        ..block.tx.witness_set,
        addr_tx_wits: invented,
      },
    ),
  )
  run_finalize_against(block, forged_block, invented, 0)
}

/// §7.3 abort-never-clamp: a witness index past the committed collection fails
/// the read instead of returning the nearest witness.
test invalid_signature_step_02_rejects_an_out_of_range_witness_index() fail {
  let block = challenged_block()
  run_finalize(
    block,
    block.witness_set_compact,
    block.tx.witness_set.addr_tx_wits,
    5,
  )
}
