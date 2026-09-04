use aiken/crypto.{ScriptHash, verify_ed25519_signature}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, address_witnesses_field_index, resume_opened_field_walk,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, encode_midgard_address_witness,
}
use midgard/fraud_proofs/native_tx/types.{MidgardAddressWitness}
use midgard/fraud_proofs/spend_input_signer_missing/rule.{
  VerdictV1, WitnessScanV1, witness_scan_batch_size,
}
use midgard/fraud_proofs/spend_input_signer_missing/step_04.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_machine_walk_v1.{
  field_walk_checkpoint_hash, walk_fold, walk_remaining,
}

validator main(
  step_05_hash: ScriptHash,
  thread_policy: PolicyId,
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
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        witnesses_opening,
        checkpoint_cbor,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, state, next_hash, next_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(scan) = state
        let Pair(view, checkpoint) = resume_opened_field_walk(
          witnesses_opening,
          WitnessAnchor {
            tx_id: scan.authenticated.transaction_id,
            witness_set_hash: scan.authenticated.witness_set_hash,
          },
          address_witnesses_field_index,
          scan.checkpoint_hash,
          checkpoint_cbor,
          reference_inputs,
          certificate_policy,
        )
        let remaining = walk_remaining(checkpoint)
        let budget = if remaining > witness_scan_batch_size {
          witness_scan_batch_size
        } else {
          remaining
        }
        let Pair(signer_present, next_checkpoint) = walk_fold(
          view,
          checkpoint,
          budget,
          False,
          valid_signer_fold(
            scan.authenticated.transaction_id,
            scan.authenticated.payment_credential,
          ),
        )
        let terminal = signer_present || walk_remaining(next_checkpoint) == 0
        let expected_hash = if terminal {
          step_05_hash
        } else {
          own_hash
        }
        let expected_state: Data = if terminal {
          let value: Data = VerdictV1 {
            subject: scan.authenticated.subject,
            signer_missing: !signer_present,
          }
          value
        } else {
          let value: Data = WitnessScanV1 {
            ..scan,
            checkpoint_hash: field_walk_checkpoint_hash(next_checkpoint),
          }
          value
        }
        and {
          next_hash == expected_hash,
          next_state == expected_state,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn valid_signer_fold(
  transaction_id: ByteArray,
  payment_credential: ByteArray,
) -> fn(Bool, Int, ByteArray) -> Bool {
  fn(found, _index, item) {
    let witness = decode_midgard_address_witness_cbor(item)
    or {
      found,
      and {
        verify_ed25519_signature(
          witness.verification_key,
          transaction_id,
          witness.signature,
        ),
        utils.get_verification_key_hash(witness.verification_key) == payment_credential,
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

test valid_signature_enters_the_signer_frontier() {
  let fold = valid_signer_fold(
    test_tx_id,
    utils.get_verification_key_hash(test_vkey),
  )
  fold(False, 0, test_witness(test_signature))
}

test invalid_signature_never_enters_the_signer_frontier() {
  let fold = valid_signer_fold(
    test_tx_id,
    utils.get_verification_key_hash(test_vkey),
  )
  !fold(
    False,
    0,
    test_witness(
      #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    ),
  )
}

test valid_signature_from_wrong_credential_does_not_match() {
  let fold = valid_signer_fold(
    test_tx_id,
    #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
  )
  !fold(False, 0, test_witness(test_signature))
}
