//// The permissionless field grammar certificate validator.
////
//// A field grammar certificate lets a step read one item of a variable-width
//// field without walking the whole field, so the step's cost no longer grows
//// with the field's item count. This validator builds one: it walks the
//// field's §5.1 envelope with the grammar-certification machine, one batch
//// per transaction, and records the certified item count and a wrapper
//// offset every `field_grammar_waypoint_interval` items
//// (`midgard/field_grammar_certificate_v1` is the consumer side).
////
//// **One script, two handlers**, as for the field-preimage certificate:
//// `mint` and `spend` share one script hash, so the policy id is the spend
//// credential and the token always lands at its own address.
////
//// **Nothing here is privileged.** `Open` and `Advance` check content and
//// never identity: the transaction is authenticated by re-deriving its tx-id
//// from the compact structure, the field by the door's positional §4
//// commitment, and every value of the next datum is computed here and
//// required byte for byte. Anyone may advance anyone's certificate. Only the
//// deposit has an owner, and reclaiming it burns the token; a reclaimed
//// certificate is rebuilt by anyone, byte-identical apart from `owner`.
////
//// **The token never leaves this address unburnt.** `Open` mints exactly one
//// into a datum it computed; `Advance` moves exactly one into the datum it
//// computed, with nothing of this policy minted or burnt; `Reclaim` requires
//// that no output holds it. So every token of the policy sits over a datum
//// this validator computed.
////
//// The only parameter is the field-preimage certificate policy, which the
//// door needs to open a tier-3 field.

use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use midgard/common/utils
use midgard/field_grammar_certificate_v1.{
  Complete, FieldGrammarCertificateV1, InProgress,
  field_grammar_certificate_asset_name,
}
use midgard/field_grammar_certification_v1.{
  Advance, FieldGrammarCertificateMintRedeemerV1,
  FieldGrammarCertificateSpendRedeemerV1, Open, Reclaim, Retire,
  field_grammar_certificate_after_batch,
}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_witness_set_compact, validation_source_kind_v1,
  verify_tx_compact_cbor_v1,
}
use midgard/native_tx_field_access_v1.{field_total_length}
use midgard/native_tx_machine_walk_v1.{
  field_grammar_field_commitment, field_grammar_field_index,
  open_materialised_field_grammar_certification,
  resume_materialised_field_grammar_certification_from_commitment,
}

/// The output `output_index` names is a certificate output holding exactly
/// one token of this policy and inline datum `expected`.
fn is_certificate_output(
  tx: Transaction,
  output_index: Int,
  own_policy_id: PolicyId,
  expected: Data,
) -> Bool {
  expect output_index >= 0
  expect Some(Output { address, value, datum, reference_script }) = list.at(
    tx.outputs,
    output_index,
  )
  and {
    address.payment_credential == Script(own_policy_id),
    address.stake_credential == None,
    reference_script == None,
    datum == InlineDatum(expected),
    dict.to_pairs(assets.tokens(value, own_policy_id)) == [
      Pair(field_grammar_certificate_asset_name, 1),
    ],
  }
}

validator field_grammar_certificate(
  field_preimage_certificate_policy_id: PolicyId,
) {
  mint(
    redeemer: FieldGrammarCertificateMintRedeemerV1,
    own_policy_id: PolicyId,
    tx: Transaction,
  ) {
    when redeemer is {
      Open {
        source_kind,
        compact_cbor,
        witness_set_compact_cbor,
        carriage,
        batch,
        output_index,
      } -> {
        // 1. Exactly one token of this policy is minted in the transaction,
        //    and it is the constant name at quantity 1. The quantity is what
        //    keeps a second unit from riding out to an output whose datum no
        //    one computed.
        expect [Pair(minted_name, minted_quantity)] = tx.mint
          |> assets.tokens(own_policy_id)
          |> dict.to_pairs
        expect minted_name == field_grammar_certificate_asset_name
        expect minted_quantity == 1
        // 2. The new certificate names its owner, transaction and field.
        //    Every other value of it is computed below.
        expect Some(Output { datum: InlineDatum(datum), .. }) = list.at(
          tx.outputs,
          output_index,
        )
        expect named: FieldGrammarCertificateV1 = datum
        let Pair(owner, Pair(tx_id, field_index)) = when named is {
          InProgress { owner, tx_id, field_index, .. } ->
            Pair(owner, Pair(tx_id, field_index))
          Complete { owner, tx_id, field_index, .. } ->
            Pair(owner, Pair(tx_id, field_index))
        }
        expect bytearray.length(owner) == 28
        // 3. The binding: the tx-id is re-derived from the compact structure,
        //    and the door reads the field's commitment positionally from it
        //    (fields 6–8 through the witness set it hashes against
        //    `witness_set_hash`).
        let verified = verify_tx_compact_cbor_v1(
          validation_source_kind_v1(source_kind),
          tx_id,
          compact_cbor,
        )
        let Pair(view, checkpoint) = open_materialised_field_grammar_certification(
          verified,
          decode_native_tx_witness_set_compact(witness_set_compact_cbor),
          field_index,
          carriage,
          tx.reference_inputs,
          field_preimage_certificate_policy_id,
        )
        // 4. The first batch, and the output holds exactly its result.
        let expected: Data = field_grammar_certificate_after_batch(
          owner,
          tx_id,
          field_index,
          field_grammar_field_commitment(checkpoint),
          field_total_length(view),
          view,
          checkpoint,
          #"",
          batch,
        )
        is_certificate_output(tx, output_index, own_policy_id, expected)
      }

      // Burning only. The spend handler releasing the token checks the owner.
      Retire ->
        tx.mint
          |> assets.tokens(own_policy_id)
          |> dict.to_pairs
          |> list.all(fn(minted) { minted.2nd < 0 })
    }
  }

  spend(
    datum: Option<FieldGrammarCertificateV1>,
    redeemer: FieldGrammarCertificateSpendRedeemerV1,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(certificate) = datum
    expect Some(own_input) = list.find(
      tx.inputs,
      fn(input) { input.output_reference == own_ref },
    )
    expect Script(own_policy_id) = own_input.output.address.payment_credential
    let asset_name = field_grammar_certificate_asset_name
    when redeemer is {
      Advance {
        source_kind,
        compact_cbor,
        witness_set_compact_cbor,
        checkpoint,
        carriage,
        batch,
        output_index,
      } -> {
        // 1. Only a walk under way advances.
        expect
            InProgress {
              owner,
              tx_id,
              field_index,
              field_hash,
              total_length,
              grammar_checkpoint_hash,
              waypoints,
            }
          = certificate
        // 2. Exactly one certificate moves, and none is minted or burnt: the
        //    spent input holds one token, nothing of this policy is minted,
        //    and the outputs hold one token between them. By the ledger's
        //    value balance no other input holds one.
        expect
          dict.to_pairs(assets.tokens(own_input.output.value, own_policy_id)) == [
            Pair(asset_name, 1),
          ]
        expect dict.is_empty(assets.tokens(tx.mint, own_policy_id))
        expect
          list.foldl(
            tx.outputs,
            0,
            fn(output, held) {
              held + assets.quantity_of(output.value, own_policy_id, asset_name)
            },
          ) == 1
        // 3. Resume from exactly the checkpoint the datum committed, against
        //    the same transaction and field.
        let verified = verify_tx_compact_cbor_v1(
          validation_source_kind_v1(source_kind),
          tx_id,
          compact_cbor,
        )
        let Pair(view, resumed) = resume_materialised_field_grammar_certification_from_commitment(
          verified,
          decode_native_tx_witness_set_compact(witness_set_compact_cbor),
          grammar_checkpoint_hash,
          checkpoint,
          carriage,
          tx.reference_inputs,
          field_preimage_certificate_policy_id,
        )
        expect field_grammar_field_index(resumed) == field_index
        expect field_grammar_field_commitment(resumed) == field_hash
        // 4. The next batch, and the continuing output holds exactly its
        //    result. The deposit only grows, so an advance by anyone can
        //    never take the owner's lovelace.
        let expected: Data = field_grammar_certificate_after_batch(
          owner,
          tx_id,
          field_index,
          field_hash,
          total_length,
          view,
          resumed,
          waypoints,
          batch,
        )
        expect Some(continuing) = list.at(tx.outputs, output_index)
        and {
          is_certificate_output(tx, output_index, own_policy_id, expected),
          assets.lovelace_of(continuing.value) >= assets.lovelace_of(
            own_input.output.value,
          ),
        }
      }

      // Owner-discretionary, no time-lock: a reclaimed certificate is
      // rebuilt by anyone. The token must not survive the transaction, stated
      // over outputs so that two certificates spent together cannot share one
      // burn.
      Reclaim -> and {
          list.all(
            tx.outputs,
            fn(output) {
              assets.quantity_of(output.value, own_policy_id, asset_name) == 0
            },
          ),
          utils.has_signed(
            when certificate is {
              InProgress { owner, .. } -> owner
              Complete { owner, .. } -> owner
            },
            tx.extra_signatories,
          ),
        }
    }
  }

  else(_) {
    fail
  }
}
