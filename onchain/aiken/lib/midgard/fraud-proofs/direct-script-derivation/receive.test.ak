use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry.{Leaf}
use aiken/merkle_patricia_forestry/helpers.{nibble}
use env
use midgard/fraud_proofs/direct_script_derivation/receive as direct
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/first_fault_v1.{NetworkOrderContextV1}
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}

const a = #"22222222222222222222222222222222222222222222222222222222"

const b = #"33333333333333333333333333333333333333333333333333333333"

fn divergence(left: ByteArray, right: ByteArray, at: Int) -> Int {
  if nibble(left, at) == nibble(right, at) {
    divergence(left, right, at + 1)
  } else {
    at
  }
}

fn proof_against(key: ByteArray, neighbor: ByteArray, first: Int) {
  [
    Leaf {
      skip: divergence(blake2b_256(key), blake2b_256(neighbor), 0),
      key: blake2b_256(neighbor),
      value: blake2b_256(cbor.serialise(first)),
    },
  ]
}

fn output_map() -> Pair<ByteArray, Int> {
  let Pair(root, count) = direct.admit_output_v1(
    env.empty_merkle_tree_root,
    0,
    0,
    Some(b),
    direct.FirstCredential { proof: [] },
  )
  direct.admit_output_v1(
    root,
    count,
    1,
    Some(a),
    direct.FirstCredential { proof: proof_against(a, b, 0) },
  )
}

fn declaration() -> direct.DeclarationStateV1 {
  let Pair(root, count) = output_map()
  direct.DeclarationStateV1 {
    bound: BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction: 0,
        source_kind: 0,
        transaction_id: #"1111111111111111111111111111111111111111111111111111111111111111",
        source_key: #"",
        rejection_reason: None,
      },
      compact_cbor: #"00",
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: env.empty_merkle_tree_root,
    },
    source_index: source_index.StateV1 {
      root: #"4444444444444444444444444444444444444444444444444444444444444444",
      count: 7,
    },
    output_root: root,
    expected_count: count,
    declared_root: env.empty_merkle_tree_root,
    declared_count: 0,
    previous_credential: #"",
    target_ordinal: Some(1),
    selected: None,
  }
}

test unsorted_outputs_match_one_sorted_exact_declaration() {
  let finished = direct.advance_declaration_v1(
    declaration(),
    [
      direct.DeclaredCredentialV1 {
        script_hash: a,
        first_output_index: 1,
        proof: [],
      },
      direct.DeclaredCredentialV1 {
        script_hash: b,
        first_output_index: 0,
        proof: proof_against(b, a, 1),
      },
    ],
  )
  expect Some(selected) = direct.selected_purpose_v1(finished)
  and {
    direct.declaration_is_complete_v1(finished),
    selected.purpose.coordinate.index == 1,
    selected.first_output_index == 0,
    finished.source_index == declaration().source_index,
    finished.bound == declaration().bound,
  }
}

test duplicate_outputs_preserve_earliest_index() {
  let Pair(root, count) = direct.admit_output_v1(
    env.empty_merkle_tree_root,
    0,
    4,
    Some(a),
    direct.FirstCredential { proof: [] },
  )
  direct.admit_output_v1(
    root,
    count,
    9,
    Some(a),
    direct.RepeatedCredential { first_output_index: 4, proof: [] },
  ) == Pair(root, count)
}

test missing_declared_credential_cannot_complete() {
  let unfinished = direct.advance_declaration_v1(
    declaration(),
    [
      direct.DeclaredCredentialV1 {
        script_hash: a,
        first_output_index: 1,
        proof: [],
      },
    ],
  )
  !direct.declaration_is_complete_v1(unfinished)
}

test false_earliest_index_cannot_complete() {
  let finished = direct.advance_declaration_v1(
    declaration(),
    [
      direct.DeclaredCredentialV1 {
        script_hash: a,
        first_output_index: 0,
        proof: [],
      },
      direct.DeclaredCredentialV1 {
        script_hash: b,
        first_output_index: 0,
        proof: proof_against(b, a, 0),
      },
    ],
  )
  !direct.declaration_is_complete_v1(finished)
}

test descending_declaration_isrefused() fail {
  let refused = direct.advance_declaration_v1(
    declaration(),
    [
      direct.DeclaredCredentialV1 {
        script_hash: b,
        first_output_index: 0,
        proof: [],
      },
      direct.DeclaredCredentialV1 {
        script_hash: a,
        first_output_index: 1,
        proof: proof_against(a, b, 0),
      },
    ],
  )
  refused.declared_count >= 0
}

test duplicate_declaration_isrefused() fail {
  let refused = direct.advance_declaration_v1(
    declaration(),
    [
      direct.DeclaredCredentialV1 {
        script_hash: a,
        first_output_index: 1,
        proof: [],
      },
      direct.DeclaredCredentialV1 {
        script_hash: a,
        first_output_index: 1,
        proof: [],
      },
    ],
  )
  refused.declared_count >= 0
}

test repeated_output_with_false_first_index_isrefused() fail {
  let Pair(root, count) = direct.admit_output_v1(
    env.empty_merkle_tree_root,
    0,
    4,
    Some(a),
    direct.FirstCredential { proof: [] },
  )
  let refused = direct.admit_output_v1(
    root,
    count,
    9,
    Some(a),
    direct.RepeatedCredential { first_output_index: 3, proof: [] },
  )
  refused.2nd >= 0
}

test omitted_actual_credential_isrefused() fail {
  let refused = direct.admit_output_v1(
    env.empty_merkle_tree_root,
    0,
    0,
    Some(a),
    direct.NoCredential,
  )
  refused.2nd >= 0
}

test invented_credential_on_a_plain_output_isrefused() fail {
  let refused = direct.admit_output_v1(
    env.empty_merkle_tree_root,
    0,
    0,
    None,
    direct.FirstCredential { proof: [] },
  )
  refused.2nd >= 0
}
