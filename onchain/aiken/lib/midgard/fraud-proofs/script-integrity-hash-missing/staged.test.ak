use aiken/collection/list
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, opened_certified_field_walk_from_grammar,
  opened_field_grammar_certification, redeemers_field_index,
  script_witnesses_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_redeemer_witness, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardExecutionUnits, MidgardRedeemerWitness, MidgardTransaction,
  MidgardTransactionWitnessSet, MidgardVersionedScript, NativeCardanoScript,
  PlutusV3Script, SpendRedeemer,
}
use midgard/fraud_proofs/script_integrity_hash_missing/rule.{
  contains_non_native_script_items_v1,
}
use midgard/native_tx_field_access_v1.{encode_field_preimage}
use midgard/native_tx_machine_walk_v1.{
  certify_field_grammar, encode_field_grammar_checkpoint,
  field_grammar_checkpoint_hash, field_grammar_is_complete, walk_fold,
  walk_is_complete, walk_remaining,
}

const maximum_supported_items: Int = 224

fn scripts(remaining: Int) -> List<MidgardVersionedScript> {
  if remaining <= 0 {
    []
  } else {
    [
      MidgardVersionedScript {
        language: if remaining == 1 {
          PlutusV3Script
        } else {
          NativeCardanoScript
        },
        script_bytes: fixture.repeated_bytes_v1(#"00", 70),
      },
      ..scripts(remaining - 1)
    ]
  }
}

fn redeemers(remaining: Int) -> List<MidgardRedeemerWitness> {
  if remaining <= 0 {
    []
  } else {
    [
      MidgardRedeemerWitness {
        purpose: SpendRedeemer,
        index: remaining - 1,
        redeemer_cbor: fixture.repeated_bytes_v1(#"00", 70),
        execution_units: MidgardExecutionUnits { memory: 1, steps: 1 },
      },
      ..redeemers(remaining - 1)
    ]
  }
}

fn maximum_block() -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      witness_set: MidgardTransactionWitnessSet {
        ..base.witness_set,
        script_tx_wits: scripts(maximum_supported_items),
        redeemer_tx_wits: redeemers(maximum_supported_items),
      },
    },
  )
}

test maximum_certified_script_and_redeemer_fields_reach_authenticated_terminals() {
  let block = maximum_block()
  let script_preimage = encode_field_preimage(
    list.map(scripts(maximum_supported_items), encode_midgard_versioned_script),
  )
  let script_opening = fixture.certified_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    script_preimage,
  )
  let script_refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    script_witnesses_field_index,
    script_preimage,
  )
  let anchor = WitnessAnchor {
    tx_id: block.tx_id,
    witness_set_hash: block.compact.witness_set_hash,
  }
  let Pair(script_view, script_grammar_start) = opened_field_grammar_certification(
    script_opening,
    anchor,
    script_witnesses_field_index,
    script_refs,
    fixture.test_certificate_policy_id,
  )
  let script_grammar = certify_field_grammar(
    script_view,
    script_grammar_start,
    maximum_supported_items,
  )
  expect field_grammar_is_complete(script_grammar)
  let Pair(script_semantic_view, script_walk_start) = opened_certified_field_walk_from_grammar(
    script_opening,
    anchor,
    script_witnesses_field_index,
    field_grammar_checkpoint_hash(script_grammar),
    encode_field_grammar_checkpoint(script_grammar),
    script_refs,
    fixture.test_certificate_policy_id,
  )
  let Pair(found, script_terminal) = walk_fold(
    script_semantic_view,
    script_walk_start,
    maximum_supported_items,
    False,
    fn(found, _index, item) {
      or {
        found,
        contains_non_native_script_items_v1([item]),
      }
    },
  )
  expect and {
      found,
      walk_is_complete(script_terminal),
    }

  let redeemer_preimage = encode_field_preimage(
    list.map(
      redeemers(maximum_supported_items),
      encode_midgard_redeemer_witness,
    ),
  )
  let redeemer_opening = fixture.certified_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    redeemer_preimage,
  )
  let redeemer_refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    redeemers_field_index,
    redeemer_preimage,
  )
  let Pair(redeemer_view, redeemer_grammar_start) = opened_field_grammar_certification(
    redeemer_opening,
    anchor,
    redeemers_field_index,
    redeemer_refs,
    fixture.test_certificate_policy_id,
  )
  let redeemer_grammar = certify_field_grammar(
    redeemer_view,
    redeemer_grammar_start,
    maximum_supported_items,
  )
  expect field_grammar_is_complete(redeemer_grammar)
  let Pair(_, redeemer_walk_start) = opened_certified_field_walk_from_grammar(
    redeemer_opening,
    anchor,
    redeemers_field_index,
    field_grammar_checkpoint_hash(redeemer_grammar),
    encode_field_grammar_checkpoint(redeemer_grammar),
    redeemer_refs,
    fixture.test_certificate_policy_id,
  )
  walk_remaining(redeemer_walk_start) == maximum_supported_items
}
