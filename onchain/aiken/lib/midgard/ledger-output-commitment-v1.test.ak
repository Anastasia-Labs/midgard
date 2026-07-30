use aiken/builtin
use midgard/bounded_item_v1
use midgard/cek_data_v1
use midgard/ledger_output_commitment_v1
use midgard/validation_merkle_v1.{FrontierPeak}

const expected_descriptor =
  #"900107191388582013e167684e9dc284acc6ebbe972cd2cf0763d03bba558bae463825f3f35990d6581d60111111111111111111111111111111111111111111111111111111111a004c4b40005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd05204000408358202222222222222222222222222222222222222222222222222222222222222222186518ca8358203333333333333333333333333333333333333333333333333333333333333333186718cc83582044444444444444444444444444444444444444444444444444444444444444440304"

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

fn descriptor_fixture() -> ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
  ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index: 7,
    total_length: 5000,
    item_commitment,
    address: #"6011111111111111111111111111111111111111111111111111111111",
    lovelace: 5000000,
    asset_count: 0,
    asset_frontier_commitment: #"b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd",
    cardano_value_size: 5,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out: cek_data_v1.DataSummaryV1 {
      root: #"2222222222222222222222222222222222222222222222222222222222222222",
      cbor_length: 101,
      memory: 202,
    },
    midgard_tx_out: cek_data_v1.DataSummaryV1 {
      root: #"3333333333333333333333333333333333333333333333333333333333333333",
      cbor_length: 103,
      memory: 204,
    },
    cardano_spend_datum: cek_data_v1.DataSummaryV1 {
      root: #"4444444444444444444444444444444444444444444444444444444444444444",
      cbor_length: 3,
      memory: 4,
    },
  }
}

test canonical_cross_language_descriptor_vector() {
  let output_cbor = builtin.replicate_byte(5000, 90)
  let descriptor = descriptor_fixture()
  and {
    ledger_output_commitment_v1.output_item_commitment(7, output_cbor) == item_commitment,
    descriptor.item_commitment == item_commitment,
    ledger_output_commitment_v1.encode(descriptor) == expected_descriptor,
    ledger_output_commitment_v1.decode(expected_descriptor) == descriptor,
  }
}

test canonical_cross_language_asset_frontier_vector() {
  let longer_name = ledger_output_commitment_v1.asset_leaf_hash(
    builtin.replicate_byte(28, 85),
    #"0102",
    42,
  )
  let shorter_name = ledger_output_commitment_v1.asset_leaf_hash(
    builtin.replicate_byte(28, 85),
    #"03",
    7,
  )
  let first_peaks = validation_merkle_v1.append_leaf(0, [], shorter_name)
  let peaks = validation_merkle_v1.append_leaf(1, first_peaks, longer_name)
  and {
    longer_name == #"9ed4bad2bc3c66d009d021f8fa57eb32d6035c71aa6f1c415e8f66bf35c6661c",
    shorter_name == #"d4fab6956ed316d6c8094e2f8ba31c07f9b8e11d6b9ed1236541a102187a58ce",
    validation_merkle_v1.frontier_commitment(2, peaks) == #"1af1c3eeb6379f3bb3dac82d19faffa1318a2a64f739b38c5fc5640b58196cc5",
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

test exact_cardano_value_size_bound_is_supported() {
  let descriptor = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    ..descriptor_fixture(),
    cardano_value_size: ledger_output_commitment_v1.max_cardano_value_cbor_bytes,
  }
  ledger_output_commitment_v1.decode(
    ledger_output_commitment_v1.encode(descriptor),
  ) == descriptor
}

test cardano_value_above_mainnet_bound_fails_closed() fail {
  ledger_output_commitment_v1.encode(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      ..descriptor_fixture(),
      cardano_value_size: ledger_output_commitment_v1.max_cardano_value_cbor_bytes + 1,
    },
  ) == #""
}
