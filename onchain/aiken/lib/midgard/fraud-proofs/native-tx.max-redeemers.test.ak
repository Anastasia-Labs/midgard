use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/components.{encode_midgard_redeemer_witness}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  encode_redeemer_witness_preimage,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardExecutionUnits, MidgardRedeemerWitness, SpendRedeemer,
}
use midgard/native_tx_field_access_v1
use midgard/validation_machine_v1
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

// Derived by the genuine Cardano boundary fixture in
// demo/midgard-validation/tests/ordered-collection-redeemer-boundary-v1.test.ts.
const maximum_cardano_spend_redeemer_count = 296

const maximum_cardano_spend_redeemer_preimage_bytes = 5053

const maximum_cardano_spend_redeemer_preimage_hash =
  #"680079f9aebb6ab20240bf0a4b46a9b607181843413e0cdfbb293942aebe3d0a"

const maximum_cardano_spend_redeemer_collection_commitment =
  #"07da3c8aea4dd252510b18f872268ea7b7d752fe9d6874f3321286ec6d8c4133"

const maximum_cardano_transaction_id =
  #"82c56f324a18a66255e3d48ddcf80a86f5b7db89dd8f5b1e0c3d3cce02668b40"

const maximum_cardano_transaction_commitment =
  #"d44e343be6c481bf241cc393197ddbba3e8909f2d01ba9501486585bd412a04c"

const maximum_cardano_compact_cbor =
  #"84018c58207fac00ce59ee1a8c6f84fe48c8ff61af01e76a9f4cfe210a6245f0cbbe7781265820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f58205581fd909e08e4dea9336a8928f0d2731f2f31e1ce31a16cb1f5b2ebc2c9dccf1a000de2ec20205820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d5820e650a24c14c0e6a48877805b4185f8ff2ee711e964e6aa63ce05c29ddeb1bd26582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820723dfb187dd11e5d8b44a3ebc9b44da9037807f5ff794e07f962798509df1f6100"

const maximum_cardano_witness_set_compact_cbor =
  #"83582047e04a3a41997bc4fc6c3ad161b44ccdee9caa0f5ff5d1fa76d3b071108629e958207f31164435b45870b2761140e4ed3e5ee4535de19f223b22f2ba21735503b386582007da3c8aea4dd252510b18f872268ea7b7d752fe9d6874f3321286ec6d8c4133"

const maximum_cardano_field_preimage_lengths_cbor =
  #"89192d550118300101011618681913bd"

const maximum_cardano_validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

const maximum_cardano_terminal_pre_work_root =
  #"e8d2c995785e73b5f3d4343d33adf421bd003019c9116f689a1478e5e886400a"

const maximum_cardano_terminal_post_work_root =
  #"c95c88e209891c909d9a4d8ac18883caa51d432efd150613ed904c0cb74f7036"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

fn maximum_cardano_spend_redeemers_from(
  pointer_index: Int,
) -> List<MidgardRedeemerWitness> {
  if pointer_index > maximum_cardano_spend_redeemer_count {
    []
  } else {
    [
      MidgardRedeemerWitness {
        purpose: SpendRedeemer,
        index: pointer_index,
        redeemer_cbor: #"d87980",
        execution_units: MidgardExecutionUnits { memory: 1601, steps: 316149 },
      },
      ..maximum_cardano_spend_redeemers_from(pointer_index + 1)
    ]
  }
}

fn redeemer_pointer_sequence_is_exact(
  redeemers: List<MidgardRedeemerWitness>,
  expected_pointer_index: Int,
) -> Bool {
  when redeemers is {
    [] -> expected_pointer_index == maximum_cardano_spend_redeemer_count + 1
    [redeemer, ..rest] -> and {
        redeemer.index == expected_pointer_index,
        redeemer_pointer_sequence_is_exact(rest, expected_pointer_index + 1),
      }
  }
}

test maximum_cardano_spend_redeemer_field_matches_typescript_terminal_commitment() {
  let redeemers = maximum_cardano_spend_redeemers_from(1)
  let preimage_cbor = encode_redeemer_witness_preimage(redeemers)
  let decoded = decode_midgard_tx_redeemer_witnesses_preimage_cbor(
    preimage_cbor,
  )
  let item_cbors = redeemers |> list.map(encode_midgard_redeemer_witness)
  expect Some(first) = list.at(decoded, 0)
  expect Some(last) = list.last(decoded)

  and {
    list.length(decoded) == maximum_cardano_spend_redeemer_count,
    decoded == redeemers,
    redeemer_pointer_sequence_is_exact(decoded, 1),
    first.index == 1,
    last.index == maximum_cardano_spend_redeemer_count,
    list.all(
      decoded,
      fn(redeemer) {
        and {
          redeemer.purpose == SpendRedeemer,
          redeemer.redeemer_cbor == #"d87980",
          redeemer.execution_units.memory == 1601,
          redeemer.execution_units.steps == 316149,
        }
      },
    ),
    bytearray.length(preimage_cbor) == maximum_cardano_spend_redeemer_preimage_bytes,
    blake2b_256(preimage_cbor) == maximum_cardano_spend_redeemer_preimage_hash,
    native_tx_field_access_v1.field_commitment_from_items(item_cbors) == maximum_cardano_spend_redeemer_collection_commitment,
  }
}

test maximum_cardano_spend_redeemer_terminal_fold_matches_typescript() {
  let work_witness_cbor = validation_machine_v1.encode_transaction_field_scan_witness(
    maximum_cardano_compact_cbor,
    maximum_cardano_witness_set_compact_cbor,
    maximum_cardano_field_preimage_lengths_cbor,
    maximum_cardano_validation_context_cbor,
    8,
    295,
    0,
    maximum_cardano_spend_redeemer_count,
    5035,
  )
  let compact_binding_witness_cbor = validation_machine_v1.encode_compact_binding_witness(
    maximum_cardano_transaction_id,
    maximum_cardano_transaction_commitment,
    maximum_cardano_compact_cbor,
    maximum_cardano_witness_set_compact_cbor,
    maximum_cardano_field_preimage_lengths_cbor,
    maximum_cardano_validation_context_cbor,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: maximum_cardano_transaction_id,
    transaction_id: maximum_cardano_transaction_id,
    transaction_commitment: maximum_cardano_transaction_commitment,
    validation_context_hash: validation_trace_v1.hash_validation_context(
      maximum_cardano_validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: maximum_cardano_transaction_commitment,
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
    phase: validation_trace_v1.CompactBinding,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CompactBinding,
      41,
      compact_binding_witness_cbor,
    ),
  }
  let collection_proof = bounded_collection_v1.ItemProofV1 {
    version: bounded_collection_v1.version,
    field_index: 8,
    item_count: maximum_cardano_spend_redeemer_count,
    item_index: 295,
    item_length: 18,
    item_commitment: #"0b7517c996b4be98c145b61a84789c337d9a394529322f0e4ff2b00825a13fe5",
    frontier: [
      validation_merkle_v1.FrontierPeak {
        height: 3,
        hash: #"599fb8883e9753ebff787e9ba693c9d266a94e2e4c2412f5cc8def17a37efc4b",
      },
      validation_merkle_v1.FrontierPeak {
        height: 5,
        hash: #"d3ee1a26b14495f4b4e5196a4035453be00e5947a29d7a106e78df0ffb840942",
      },
      validation_merkle_v1.FrontierPeak {
        height: 8,
        hash: #"baff8cd322326841f8dbf9a8fa0464a67cdf9c4161e429fbb569e0346adffde1",
      },
    ],
    siblings: [
      #"b2a18a9249e13b7f2c75032bf73d6d447b196d970097d8276f450ddcbd45ff21",
      #"8277569ed239c02bb472113a2804537ce7e9977f6fc3e8e01104f1c38c696c18",
      #"07c8bf5bbf9c9e2b477a1c8ccfe1a7d4ce62846164c5fbebc6481c4da4fc6f22",
    ],
  }
  let chunk_proof = bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 8,
    item_index: 295,
    total_length: 18,
    chunk_index: 0,
    chunk: #"840019012843d87980821906411a0004d2f5",
    frontier: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: #"bd5765879c3e766f6cbc89ea728e263b73af278ed4091e26cabaf5b7fb04d91e",
      },
    ],
    siblings: [],
  }

  and {
    pre.work_root == maximum_cardano_terminal_pre_work_root,
    post.work_root == maximum_cardano_terminal_post_work_root,
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
