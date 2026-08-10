use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/validation_machine_v1
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

// Derived by the exact Cardano maxTxSize boundary fixture in
// demo/midgard-validation/tests/blob-chunk-boundary-v1.test.ts.
const maximum_cardano_inline_datum_transaction_id =
  #"e6a945b4b120666ac555e9ca42e714179943da5ebbc3edba81200787d8c7250b"

const maximum_cardano_inline_datum_transaction_commitment =
  #"c6c216b46181a13fa8ad40f5b652842677feb6222ff2b82fbd49df75ded123bc"

const maximum_cardano_inline_datum_compact_cbor =
  #"84018c58204dc5462fa75f970091526b50e11ff2161e020020f791756d77ed6cd8c45d111c5820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f5820d40dc24540734968ab6b8212814ab280ab51e0102c18294b1abf2c9e21db5a871a000d5ec920205820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff58206295d6e5a837fa5a95389ebbd7ad38ffa316e09cd29d28a9e71639cae906aa2c00"

const maximum_cardano_inline_datum_witness_set_compact_cbor =
  #"835820650b4c39edb0d2b447c9d9f25b892ef1b1e272201ddae9519989ed3ee927f4815820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3"

const maximum_cardano_inline_datum_field_preimage_lengths_cbor =
  #"89182901193f6101010101186801"

const maximum_cardano_inline_datum_validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

const maximum_cardano_inline_datum_terminal_pre_work_root =
  #"7deabb98099010b3e4bdd504ee11ff1e801104d07c834ab5314a0915a4e9da93"

const maximum_cardano_inline_datum_terminal_post_work_root =
  #"444630db7213867459910732a2014ef541e9df14c899cdc119607838f96880b0"

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
