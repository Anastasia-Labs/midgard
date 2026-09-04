use aiken/crypto.{ScriptHash, blake2b_224, verify_ed25519_signature}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, resume_opened_field_walk,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, encode_midgard_address_witness,
}
use midgard/fraud_proofs/native_tx/types.{MidgardAddressWitness}
use midgard/fraud_proofs/protected_output_signer_missing/rule.{
  VerdictV1, WitnessScanV1, address_witnesses_field_index,
  witness_scan_batch_size,
}
use midgard/fraud_proofs/protected_output_signer_missing/step_04.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_machine_walk_v1.{
  field_walk_checkpoint_hash, walk_fold, walk_remaining,
}

fn minimum(left: Int, right: Int) -> Int {
  if left < right {
    left
  } else {
    right
  }
}

fn valid_target_fold(
  target: ByteArray,
  transaction_id: ByteArray,
) -> fn(Bool, Int, ByteArray) -> Bool {
  fn(found, _index, item) {
    let witness = decode_midgard_address_witness_cbor(item)
    or {
      found,
      and {
        blake2b_224(witness.verification_key) == target,
        verify_ed25519_signature(
          witness.verification_key,
          transaction_id,
          witness.signature,
        ),
      },
    }
  }
}

const test_tx_id =
  #"0101010101010101010101010101010101010101010101010101010101010101"

const test_vkey =
  #"ea4a6c63e29c520abef5507b132ec5f9954776aebebe7b92421eea691446d22c"

const test_signature =
  #"013f9d903c1a0a90b0beea2534582e2bb694712503215236622851f5afd54ad5f2a89983965e192edd928d484bdb1e1e521ba704fbe37968371a11597363fb0d"

fn test_witness(signature: ByteArray) -> ByteArray {
  encode_midgard_address_witness(
    MidgardAddressWitness { verification_key: test_vkey, signature },
  )
}

test protected_output_valid_signature_enters_frontier() {
  valid_target_fold(blake2b_224(test_vkey), test_tx_id)(
    False,
    0,
    test_witness(test_signature),
  )
}

test protected_output_invalid_signature_is_excluded() {
  !valid_target_fold(blake2b_224(test_vkey), test_tx_id)(
    False,
    0,
    test_witness(
      #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    ),
  )
}

test protected_output_wrong_credential_is_excluded() {
  !valid_target_fold(
    #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    test_tx_id,
  )(
    False,
    0,
    test_witness(test_signature),
  )
}

validator main(
  step_05_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args { input_index, output_index, opening, checkpoint_cbor }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _name, _prover, prior, next_hash, next_data <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(state) = prior
        let protected = state.protected
        let Pair(view, checkpoint) = resume_opened_field_walk(
          opening,
          WitnessAnchor {
            tx_id: protected.transaction_id,
            witness_set_hash: protected.witness_set_hash,
          },
          address_witnesses_field_index,
          state.checkpoint_hash,
          checkpoint_cbor,
          reference_inputs,
          certificate_policy,
        )
        let batch = minimum(witness_scan_batch_size, walk_remaining(checkpoint))
        let Pair(found, next_checkpoint) = walk_fold(
          view,
          checkpoint,
          batch,
          state.signer_present,
          valid_target_fold(
            protected.payment_credential,
            protected.transaction_id,
          ),
        )
        if walk_remaining(next_checkpoint) == 0 {
          let expected: Data = VerdictV1 {
            subject: protected.subject,
            signer_present: found,
          }
          and {
            next_hash == step_05_hash,
            next_data == expected,
          }
        } else {
          let expected: Data = WitnessScanV1 {
            ..state,
            checkpoint_hash: field_walk_checkpoint_hash(next_checkpoint),
            signer_present: found,
          }
          and {
            next_hash == own_hash,
            next_data == expected,
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
