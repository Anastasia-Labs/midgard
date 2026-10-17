use aiken/builtin
use aiken/collection/list
use env
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1,
}
use midgard/fraud_proofs/direct_script_derivation/mint_purpose as mint
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1, mint_field_index}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/preimages.{mint_policy_items}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}

const a = #"22222222222222222222222222222222222222222222222222222222"

const b = #"33333333333333333333333333333333333333333333333333333333"

fn semantic(
  policies: List<ByteArray>,
) -> (cursor.CursorV1, ByteArray, FieldOpeningV1) {
  let value = if list.is_empty(policies) {
    builtin.list_data([])
  } else {
    builtin.map_data(
      list.map(
        policies,
        fn(hash) {
          Pair(
            builtin.b_data(hash),
            builtin.map_data([Pair(builtin.b_data(#"41"), builtin.i_data(1))]),
          )
        },
      ),
    )
  }
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody { ..tx.body, mint: value },
    },
  )
  let opening = fixture.inline_body_opening(
    block.compact_cbor,
    mint_policy_items(value),
  )
  let Pair(initial, checkpoint) = cursor.initial_v1(
    BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction: 0,
        source_kind: 0,
        transaction_id: block.tx_id,
        source_key: #"",
        rejection_reason: None,
      },
      compact_cbor: block.compact_cbor,
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: env.empty_merkle_tree_root,
    },
    mint_field_index,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(ready, checkpoint) = cursor.certify_v1(
    initial,
    opening,
    checkpoint,
    10,
    [],
    fixture.test_certificate_policy_id,
  )
  (ready, checkpoint, opening)
}

test named_mint_ordinal_resumes_over_the_authenticated_policy_set() {
  let (ready, checkpoint, opening) = semantic([a, b])
  let Pair(partial, next) = mint.advance_v1(
    mint.initial_v1(ready, 1),
    opening,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(finished, _) = mint.advance_v1(
    partial,
    opening,
    next,
    1,
    [],
    fixture.test_certificate_policy_id,
  )
  expect Some(selected) = mint.selected_purpose_v1(finished)
  and {
    !partial.complete,
    selected.coordinate.index == 1,
    selected.coordinate.kind == 1,
    selected.script_hash == b,
    selected.subject == b,
  }
}

test absent_mint_ordinal_requires_the_whole_set() {
  let (ready, checkpoint, opening) = semantic([a, b])
  let Pair(finished, _) = mint.advance_v1(
    mint.initial_v1(ready, 2),
    opening,
    checkpoint,
    2,
    [],
    fixture.test_certificate_policy_id,
  )
  mint.selected_purpose_v1(finished) == None
}

test empty_mint_set_needs_no_synthetic_policy() {
  let (ready, checkpoint, opening) = semantic([])
  let Pair(finished, _) = mint.advance_v1(
    mint.initial_v1(ready, 0),
    opening,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  )
  mint.selected_purpose_v1(finished) == None
}

test partial_mint_scan_cannot_claim_a_purpose() fail {
  let (ready, checkpoint, opening) = semantic([a, b])
  let Pair(partial, _) = mint.advance_v1(
    mint.initial_v1(ready, 1),
    opening,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  )
  mint.selected_purpose_v1(partial) != None
}

test negative_mint_ordinal_is_refused() fail {
  let (ready, _, _) = semantic([a])
  mint.initial_v1(ready, -1).target_ordinal >= 0
}
