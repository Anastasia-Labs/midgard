use aiken/builtin
use midgard/bounded_item_v1
use midgard/ledger_output_commitment_v1
use midgard/validation_merkle_v1.{FrontierPeak}

const expected_descriptor =
  #"840107191388582013e167684e9dc284acc6ebbe972cd2cf0763d03bba558bae463825f3f35990d6"

const first_chunk_hash =
  #"cace1f183183522da906fabee1bafe52ab2e01a31d719845b0e055a23f1b1061"

const second_chunk_hash =
  #"b7d94afeaec000c32c2354c892e3e83989bc98985ade659f423d39da45913334"

const item_commitment =
  #"13e167684e9dc284acc6ebbe972cd2cf0763d03bba558bae463825f3f35990d6"

fn output_frontier() -> List<FrontierPeak> {
  [
    FrontierPeak {
      height: 1,
      hash: #"2ef9190c09f85ce5dd740c0b009582d71f6fcf7f8045daafbb88e9df13028704",
    },
  ]
}

test canonical_cross_language_descriptor_vector() {
  let output_cbor = builtin.replicate_byte(5000, 90)
  let descriptor = ledger_output_commitment_v1.from_output(7, output_cbor)
  and {
    descriptor.item_commitment == item_commitment,
    ledger_output_commitment_v1.encode(descriptor) == expected_descriptor,
    ledger_output_commitment_v1.decode(expected_descriptor) == descriptor,
  }
}

test every_bounded_output_chunk_is_authenticated() {
  let descriptor = ledger_output_commitment_v1.decode(expected_descriptor)
  and {
    ledger_output_commitment_v1.verify_chunk(
      descriptor,
      bounded_item_v1.ChunkProofV1 {
        version: bounded_item_v1.version,
        field_index: 2,
        item_index: 7,
        total_length: 5000,
        chunk_index: 0,
        chunk: builtin.replicate_byte(4095, 90),
        frontier: output_frontier(),
        siblings: [second_chunk_hash],
      },
    ),
    ledger_output_commitment_v1.verify_chunk(
      descriptor,
      bounded_item_v1.ChunkProofV1 {
        version: bounded_item_v1.version,
        field_index: 2,
        item_index: 7,
        total_length: 5000,
        chunk_index: 1,
        chunk: builtin.replicate_byte(905, 90),
        frontier: output_frontier(),
        siblings: [first_chunk_hash],
      },
    ),
  }
}

test descriptor_substitution_fails_closed() {
  let descriptor = ledger_output_commitment_v1.decode(expected_descriptor)
  !ledger_output_commitment_v1.verify_chunk(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      ..descriptor,
      output_index: 8,
    },
    bounded_item_v1.ChunkProofV1 {
      version: bounded_item_v1.version,
      field_index: 2,
      item_index: 7,
      total_length: 5000,
      chunk_index: 0,
      chunk: builtin.replicate_byte(4095, 90),
      frontier: output_frontier(),
      siblings: [second_chunk_hash],
    },
  )
}
