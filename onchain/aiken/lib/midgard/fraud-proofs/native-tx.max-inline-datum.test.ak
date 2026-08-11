// The `maximum_cardano_inline_datum_*` constants in this module are GENERATED;
// the tests around them are written by hand.
//
//   producer: pnpm --dir demo/midgard-validation run fixtures:ordered-collection-boundary-aiken:sync
//
// Their source is the genuine signed-Cardano field-2 inline-datum boundary found
// by `demo/midgard-validation/tests/blob-chunk-boundary-v1.test.ts`, which is the
// only implementation of that search; the producer runs the suite and rebinds
// these constants from the vector it publishes (#588).

use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/validation_machine_v1
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

const maximum_cardano_inline_datum_transaction_id =
  #"46001b1343578d758a9b543c9d673e65a517038007b8943d223fb795f58844ac"

const maximum_cardano_inline_datum_transaction_commitment =
  #"7eee0e0bdb2a5da4bf0d593800014ec57b6919e9f80008504968f4574a21295a"

const maximum_cardano_inline_datum_compact_cbor =
  #"84018c58202d56d604247c43792618a75b77864f8a6c6d35b9b5a66d25b944476d6930588e582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c058201e56c960b230680d781f77f08341cb9936b0f94813dfc441d8ccfc21ab1f04151a000d5ec92020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff58203f3ac9a4ac6b4ea0ec0a27b54d7c19126e56c5601b3dacbb8bafdc6a85e75b2c00"

const maximum_cardano_inline_datum_witness_set_compact_cbor =
  #"835820b404be1942a40954073b837534858a12148fcc2a6b1d27f5b859056a2d683605582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"

const maximum_cardano_inline_datum_field_preimage_lengths_cbor =
  #"89182901193f6101010101186801"

const maximum_cardano_inline_datum_validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

const maximum_cardano_inline_datum_terminal_pre_work_root =
  #"f35a2d6843514959acd6bae4307ae79288b282ea911daeacfeb1b70e6b7b2437"

const maximum_cardano_inline_datum_terminal_post_work_root =
  #"bb748d14a67b2932d004a45d7bed13b9a619c3ea0ecbf022e367f46bd8cc8303"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

const forty_one_z_bytes =
  #"5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a"

const eight_z_bytes = #"5a5a5a5a5a5a5a5a"

fn sixty_four_z_bytes() -> ByteArray {
  eight_z_bytes
    |> bytearray.concat(eight_z_bytes)
    |> bytearray.concat(eight_z_bytes)
    |> bytearray.concat(eight_z_bytes)
    |> bytearray.concat(eight_z_bytes)
    |> bytearray.concat(eight_z_bytes)
    |> bytearray.concat(eight_z_bytes)
    |> bytearray.concat(eight_z_bytes)
}

fn definite_z_chunks(count: Int) -> ByteArray {
  if count == 0 {
    #""
  } else {
    #"5840"
      |> bytearray.concat(sixty_four_z_bytes())
      |> bytearray.concat(definite_z_chunks(count - 1))
  }
}

fn maximum_cardano_inline_datum_terminal_chunk() -> ByteArray {
  forty_one_z_bytes
    |> bytearray.concat(definite_z_chunks(59))
    |> bytearray.concat(#"ff")
}

test maximum_cardano_inline_datum_terminal_fold_matches_typescript() {
  let chunk = maximum_cardano_inline_datum_terminal_chunk()
  let work_witness_cbor = validation_machine_v1.encode_transaction_field_scan_witness(
    maximum_cardano_inline_datum_compact_cbor,
    maximum_cardano_inline_datum_witness_set_compact_cbor,
    maximum_cardano_inline_datum_field_preimage_lengths_cbor,
    maximum_cardano_inline_datum_validation_context_cbor,
    2,
    0,
    3,
    1,
    1,
  )
  let successor_work_witness_cbor = validation_machine_v1.encode_transaction_field_scan_witness(
    maximum_cardano_inline_datum_compact_cbor,
    maximum_cardano_inline_datum_witness_set_compact_cbor,
    maximum_cardano_inline_datum_field_preimage_lengths_cbor,
    maximum_cardano_inline_datum_validation_context_cbor,
    3,
    0,
    0,
    -1,
    0,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: maximum_cardano_inline_datum_transaction_id,
    transaction_id: maximum_cardano_inline_datum_transaction_id,
    transaction_commitment: maximum_cardano_inline_datum_transaction_commitment,
    validation_context_hash: validation_trace_v1.hash_validation_context(
      maximum_cardano_inline_datum_validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: maximum_cardano_inline_datum_transaction_commitment,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 40,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      40,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      41,
      successor_work_witness_cbor,
    ),
  }
  let collection_proof = bounded_collection_v1.ItemProofV1 {
    version: bounded_collection_v1.version,
    field_index: 2,
    item_count: 1,
    item_index: 0,
    item_length: 16221,
    item_commitment: #"0b98a091df9a97ac8b6c858f8ac3a50125986f551df3011575575dc58ae422e9",
    frontier: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: #"a68306b1789aa1bfe5ea824fa0eafe7b7a7f35a9b5b8bad7897d694383c5d99d",
      },
    ],
    siblings: [],
  }
  let chunk_proof = bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 2,
    item_index: 0,
    total_length: 16221,
    chunk_index: 3,
    chunk,
    frontier: [
      validation_merkle_v1.FrontierPeak {
        height: 2,
        hash: #"e28dcceb8c768fd8cfc2c05cb0f713bad5b2e47fb5791ded037f9fe5cf59338b",
      },
    ],
    siblings: [
      #"5d5a9d6259d53c1663a3e797b3ba87e4b738b2231ded6225a47b2fb68a3f9837",
      #"a427ef63d23f12f91b7c02e0febef3a98754273827f46a36e5545c92e693edf6",
    ],
  }

  and {
    bytearray.length(chunk) == 3936,
    pre.work_root == maximum_cardano_inline_datum_terminal_pre_work_root,
    post.work_root == maximum_cardano_inline_datum_terminal_post_work_root,
    validation_machine_v1.verify_canonical_decode_one_step_v1(
      pre,
      validation_machine_v1.ValidationOneStepEvidenceV1 {
        transition: validation_machine_v1.ValidationOneStepWitnessV1 {
          work_witness_cbor,
          claimed_successor: post,
        },
        auxiliary: validation_machine_v1.TransactionFieldChunkWitness {
          collection_proof,
          chunk_proof,
        },
      },
    ),
  }
}
