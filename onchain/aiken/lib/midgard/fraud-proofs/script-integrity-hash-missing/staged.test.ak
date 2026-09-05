use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, NativeTxAnchorV1, WitnessAnchor,
  opened_certified_field_walk_from_grammar, opened_field_grammar_certification,
  redeemers_field_index, resume_opened_field_grammar_certification,
  resume_opened_field_walk, script_witnesses_field_index,
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
  FieldGrammarCheckpointV1, certify_field_grammar,
  encode_field_grammar_checkpoint, encode_field_walk_checkpoint,
  field_grammar_checkpoint_hash, field_grammar_is_complete,
  field_walk_checkpoint_hash, walk_fold, walk_is_complete, walk_remaining,
}

const maximum_supported_items: Int = 224

/// The lifecycle batch: `SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET`.
const item_budget: Int = 24

/// One past `state.direct_field_item_limit`: the smallest redeemer field that
/// has no direct route and must reach the decision through the staged walk.
const adjacent_over_direct_limit: Int = 65

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

fn found_non_native(found: Bool, _index: Int, item: ByteArray) -> Bool {
  or {
    found,
    contains_non_native_script_items_v1([item]),
  }
}

/// No script witnesses and one more redeemer than the direct route admits:
/// the shape that only the staged route can prove.
fn adjacent_block() -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      witness_set: MidgardTransactionWitnessSet {
        ..base.witness_set,
        script_tx_wits: [],
        redeemer_tx_wits: redeemers(adjacent_over_direct_limit),
      },
    },
  )
}

fn anchor_of(block: fixture.NativeBlockFixtureV1) -> NativeTxAnchorV1 {
  WitnessAnchor {
    tx_id: block.tx_id,
    witness_set_hash: block.compact.witness_set_hash,
  }
}

test empty_script_field_and_raw_redeemer_field_reach_terminals_through_the_staged_route() {
  let block = adjacent_block()
  let anchor = anchor_of(block)
  let policy = fixture.test_certificate_policy_id
  // Step 03 `StartStaged`: the first batch completes an empty field's grammar.
  let script_opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    [],
  )
  let Pair(script_view, script_start) = opened_field_grammar_certification(
    script_opening,
    anchor,
    script_witnesses_field_index,
    [],
    policy,
  )
  let script_grammar = certify_field_grammar(
    script_view,
    script_start,
    item_budget,
  )
  expect field_grammar_is_complete(script_grammar)
  // Script grammar `StartScan`: the walk completes inside its first batch.
  let Pair(scan_view, walk_start) = opened_certified_field_walk_from_grammar(
    script_opening,
    anchor,
    script_witnesses_field_index,
    field_grammar_checkpoint_hash(script_grammar),
    encode_field_grammar_checkpoint(script_grammar),
    [],
    policy,
  )
  let Pair(found, terminal) = walk_fold(
    scan_view,
    walk_start,
    item_budget,
    False,
    found_non_native,
  )
  expect and {
      !found,
      walk_is_complete(terminal),
    }
  // Script scan: the committed terminal resumes, folds nothing, and closes.
  let Pair(_, resumed) = resume_opened_field_walk(
    script_opening,
    anchor,
    script_witnesses_field_index,
    field_walk_checkpoint_hash(terminal),
    encode_field_walk_checkpoint(terminal),
    [],
    policy,
  )
  let Pair(still_found, still_terminal) = walk_fold(
    scan_view,
    resumed,
    item_budget,
    found,
    found_non_native,
  )
  expect and {
      !still_found,
      walk_is_complete(still_terminal),
    }
  // Redeemer grammar over 65 raw items: three 24-item batches, then `Finish`
  // derives `has_redeemers` from the authenticated terminal count.
  let redeemer_opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    list.map(
      redeemers(adjacent_over_direct_limit),
      encode_midgard_redeemer_witness,
    ),
  )
  let Pair(redeemer_view, redeemer_start) = opened_field_grammar_certification(
    redeemer_opening,
    anchor,
    redeemers_field_index,
    [],
    policy,
  )
  let first = certify_field_grammar(redeemer_view, redeemer_start, item_budget)
  expect !field_grammar_is_complete(first)
  let Pair(_, first_resumed) = resume_opened_field_grammar_certification(
    redeemer_opening,
    anchor,
    redeemers_field_index,
    field_grammar_checkpoint_hash(first),
    encode_field_grammar_checkpoint(first),
    [],
    policy,
  )
  let second = certify_field_grammar(redeemer_view, first_resumed, item_budget)
  expect !field_grammar_is_complete(second)
  let Pair(_, second_resumed) = resume_opened_field_grammar_certification(
    redeemer_opening,
    anchor,
    redeemers_field_index,
    field_grammar_checkpoint_hash(second),
    encode_field_grammar_checkpoint(second),
    [],
    policy,
  )
  let third = certify_field_grammar(redeemer_view, second_resumed, item_budget)
  expect field_grammar_is_complete(third)
  let Pair(_, redeemer_walk) = opened_certified_field_walk_from_grammar(
    redeemer_opening,
    anchor,
    redeemers_field_index,
    field_grammar_checkpoint_hash(third),
    encode_field_grammar_checkpoint(third),
    [],
    policy,
  )
  walk_remaining(redeemer_walk) == adjacent_over_direct_limit
}

fn partial_and_complete_redeemer_grammar(
  block: fixture.NativeBlockFixtureV1,
) -> (FieldOpeningV1, FieldGrammarCheckpointV1, FieldGrammarCheckpointV1) {
  let opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    list.map(
      redeemers(adjacent_over_direct_limit),
      encode_midgard_redeemer_witness,
    ),
  )
  let Pair(view, start) = opened_field_grammar_certification(
    opening,
    anchor_of(block),
    redeemers_field_index,
    [],
    fixture.test_certificate_policy_id,
  )
  let partial = certify_field_grammar(view, start, item_budget)
  let complete = certify_field_grammar(
    view,
    partial,
    adjacent_over_direct_limit,
  )
  expect field_grammar_is_complete(complete)
  (opening, partial, complete)
}

test malformed_checkpoint_bytes_are_refused() fail {
  let block = adjacent_block()
  let (opening, _, complete) = partial_and_complete_redeemer_grammar(block)
  let bytes = encode_field_grammar_checkpoint(complete)
  let Pair(_, _) = opened_certified_field_walk_from_grammar(
    opening,
    anchor_of(block),
    redeemers_field_index,
    field_grammar_checkpoint_hash(complete),
    bytearray.take(bytes, bytearray.length(bytes) - 1),
    [],
    fixture.test_certificate_policy_id,
  )
  True
}

test substituted_checkpoint_position_is_refused() fail {
  let block = adjacent_block()
  let (opening, partial, complete) = partial_and_complete_redeemer_grammar(
    block,
  )
  // The thread committed the terminal; the redeemer replays the partial
  // position's bytes to re-walk items the commitment already consumed.
  let Pair(_, _) = resume_opened_field_grammar_certification(
    opening,
    anchor_of(block),
    redeemers_field_index,
    field_grammar_checkpoint_hash(complete),
    encode_field_grammar_checkpoint(partial),
    [],
    fixture.test_certificate_policy_id,
  )
  True
}

test substituted_certified_preimage_is_refused() fail {
  let block = maximum_block()
  let committed = encode_field_preimage(
    list.map(scripts(maximum_supported_items), encode_midgard_versioned_script),
  )
  let foreign = encode_field_preimage(
    list.map(
      scripts(maximum_supported_items - 1),
      encode_midgard_versioned_script,
    ),
  )
  // A certificate minted over another preimage wears that preimage's field
  // hash, which the door refuses against the anchored commitment.
  let Pair(_, _) = opened_field_grammar_certification(
    fixture.certified_witness_opening(
      block.compact_cbor,
      block.witness_set_compact,
      committed,
    ),
    anchor_of(block),
    script_witnesses_field_index,
    fixture.certified_reference_inputs_v1(
      block.tx_id,
      script_witnesses_field_index,
      foreign,
    ),
    fixture.test_certificate_policy_id,
  )
  True
}

test foreign_field_certificate_is_refused() fail {
  let block = maximum_block()
  let script_preimage = encode_field_preimage(
    list.map(scripts(maximum_supported_items), encode_midgard_versioned_script),
  )
  // Field 8's certificate cannot open field 6, even for the same transaction.
  let Pair(_, _) = opened_field_grammar_certification(
    fixture.certified_witness_opening(
      block.compact_cbor,
      block.witness_set_compact,
      script_preimage,
    ),
    anchor_of(block),
    script_witnesses_field_index,
    fixture.certified_reference_inputs_v1(
      block.tx_id,
      redeemers_field_index,
      script_preimage,
    ),
    fixture.test_certificate_policy_id,
  )
  True
}
