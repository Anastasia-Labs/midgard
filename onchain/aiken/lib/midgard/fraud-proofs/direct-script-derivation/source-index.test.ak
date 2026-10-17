use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as library
use aiken/merkle_patricia_forestry/helpers.{nibble}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index as index
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}

const hash = #"22222222222222222222222222222222222222222222222222222222"

const txid = #"3333333333333333333333333333333333333333333333333333333333333333"

const commitment =
  #"4444444444444444444444444444444444444444444444444444444444444444"

fn source(field: Int, item: Int, out: Int) -> SourceV1 {
  SourceV1 {
    field_index: field,
    item_index: item,
    source_key: if field == 6 {
      cbor.serialise(item)
    } else {
      encode_midgard_tx_input(MidgardTxInput { tx_id: txid, output_index: out })
    },
    language_tag: 0,
    script_hash: hash,
    total_length: 4,
    item_commitment: commitment,
  }
}

fn added(candidate: SourceV1) -> index.StateV1 {
  index.admit_v1(index.initial_v1(), candidate, index.First { proof: [] })
}

fn expected_root(candidate: SourceV1) -> ByteArray {
  library.root(
    library.insert(library.empty, hash, cbor.serialise(candidate), []),
  )
}

test first_source_inserts_its_exact_descriptor() {
  let candidate = source(6, 4, 0)
  added(candidate) == index.StateV1 { root: expected_root(candidate), count: 1 }
}

test reference_source_replaces_an_inline_match_under_t224() {
  let old = source(6, 0, 0)
  let candidate = source(1, 8, 2)
  index.admit_v1(
    added(old),
    candidate,
    index.Existing { previous: old, proof: [] },
  ) == index.StateV1 { root: expected_root(candidate), count: 1 }
}

test spent_script_ref_also_shadows_a_matching_witness() {
  let old = source(6, 0, 0)
  let candidate = source(0, 2, 3)
  index.admit_v1(
    added(old),
    candidate,
    index.Existing { previous: old, proof: [] },
  ) == index.StateV1 { root: expected_root(candidate), count: 1 }
}

test witness_cannot_replace_an_already_admitted_input_source() {
  let old = source(0, 2, 3)
  index.admit_v1(
    added(old),
    source(6, 0, 0),
    index.Existing { previous: old, proof: [] },
  ) == added(old)
}

test duplicate_witness_keeps_the_first_actual_item() {
  let old = source(6, 1, 0)
  index.admit_v1(
    added(old),
    source(6, 9, 0),
    index.Existing { previous: old, proof: [] },
  ) == added(old)
}

test lower_canonical_outref_has_input_source_priority_across_fields() {
  let old = source(0, 2, 9)
  let candidate = source(1, 8, 2)
  index.admit_v1(
    added(old),
    candidate,
    index.Existing { previous: old, proof: [] },
  ) == index.StateV1 { root: expected_root(candidate), count: 1 }
}

test existing_source_value_cannot_be_substituted() fail {
  let old = source(1, 2, 0)
  index.admit_v1(
    added(old),
    source(6, 0, 0),
    index.Existing { previous: SourceV1 { ..old, language_tag: 3 }, proof: [] },
  ).count == 1
}

test existing_source_cannot_be_admitted_as_new() fail {
  let old = source(6, 0, 0)
  index.admit_v1(added(old), source(1, 2, 0), index.First { proof: [] }).count == 2
}

test completed_source_lookup_authenticates_exact_descriptor() {
  let old = source(0, 4, 2)
  index.lookup_v1(added(old), hash, Some(old), [])
}

test completed_source_lookup_refuses_omitting_a_present_source() {
  !index.lookup_v1(added(source(0, 4, 2)), hash, None, [])
}

// Opaque-sibling maximum-envelope models, independently reconstructed with
// the pinned JS MPF library. These are not full materialized honest DA tries.
fn maximum_branches() -> library.Proof {
  list.repeat(
    library.Branch { skip: 0, neighbors: builtin.replicate_byte(128, 17) },
    64,
  )
}

fn maximum_forks(index: Int, path: ByteArray) -> library.Proof {
  if index == 63 {
    [library.Branch { skip: 0, neighbors: builtin.replicate_byte(128, 17) }]
  } else {
    [
      library.Fork {
        skip: 0,
        neighbor: library.Neighbor {
          nibble: ( nibble(path, index) + 1 ) % 16,
          prefix: builtin.replicate_byte(62 - index, 15),
          root: builtin.replicate_byte(32, 17),
        },
      },
      ..maximum_forks(index + 1, path)
    ]
  }
}

test maximum_64_branches_source_map_insertion_preserves_both_roots() {
  index.admit_v1(
    index.StateV1 {
      root: #"448dff39dac4ee03d1da890df9de74c4b53d5497270f123787204a81a58529ff",
      count: 129,
    },
    source(6, 4, 0),
    index.First { proof: maximum_branches() },
  ) == index.StateV1 {
    root: #"7a87b94cec6d17f0a9fe103de0250f97f10748c60d76c33f421b25a7c358a0db",
    count: 130,
  }
}

test maximum_64_forks_source_map_insertion_preserves_both_roots() {
  index.admit_v1(
    index.StateV1 {
      root: #"88bc5bc95f4487e1d5bc5fc6ca9edebece573c37a580c81bf0b4470d7b49b5a2",
      count: 129,
    },
    source(6, 4, 0),
    index.First { proof: maximum_forks(0, blake2b_256(hash)) },
  ) == index.StateV1 {
    root: #"567b6bad44c6b21e687c5043d7635fcbd2814a4cff577b34b961b764f9a9e0a3",
    count: 130,
  }
}
