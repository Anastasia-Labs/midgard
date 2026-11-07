use aiken/builtin
use aiken/cbor
use aiken/crypto
use aiken/primitive/bytearray
use midgard/availability_challenge as availability
use midgard/validation_merkle_v1

const deployment: ByteArray =
  #"11111111111111111111111111111111111111111111111111111111"

const other_deployment: ByteArray =
  #"12121212121212121212121212121212121212121212121212121212"

const header: ByteArray =
  #"22222222222222222222222222222222222222222222222222222222"

const other_header: ByteArray =
  #"23232323232323232323232323232323232323232323232323232323"

const owner: ByteArray =
  #"33333333333333333333333333333333333333333333333333333333"

const challenge: ByteArray =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const other_challenge: ByteArray =
  #"4545454545454545454545454545454545454545454545454545454545454545"

const terminal: ByteArray =
  #"5555555555555555555555555555555555555555555555555555555555555555"

/// Literal cross-language vectors shared with the SDK: the two-byte tranche
/// index follows the on-chain little-endian asset-name encoding.
test tranche_asset_names_match_little_endian_cross_language_vectors() {
  let challenge_asset_name = #"4441434822222222222222222222222222222222222222222222222222222222"
  and {
    availability.tranche_asset_name_v1(challenge_asset_name, 0) == #"4454222222222222222222222222222222222222222222222222222222220000",
    availability.tranche_asset_name_v1(challenge_asset_name, 1) == #"4454222222222222222222222222222222222222222222222222222222220100",
    availability.tranche_asset_name_v1(challenge_asset_name, 15) == #"4454222222222222222222222222222222222222222222222222222222220f00",
    availability.tranche_asset_name_v1(challenge_asset_name, 63) == #"4454222222222222222222222222222222222222222222222222222222223f00",
  }
}

/// Literal cross-language vector shared with the SDK
/// (`demo/midgard-sdk/tests/availability-challenge.test.ts`): every field
/// carries a different value, so a reordered field on either side changes the
/// bytes.
test parameters_v1_field_order_matches_cross_language_vector() {
  let parameters = availability.ParametersV1 {
    response_geometry: availability.ResponseGeometryV1 {
      chunk_byte_length: 1,
      tranche_byte_length: 2,
      max_tranche_count: 3,
    },
    da_bond_lovelace: 4,
    challenger_bond_lovelace: 5,
    max_open_fee_lovelace: 6,
    max_publication_fee_lovelace: 7,
    max_settlement_fee_lovelace: 8,
    max_close_fee_lovelace: 9,
    max_timeout_fee_lovelace: 10,
    da_slash_penalty_lovelace: 11,
    da_bond_min_top_up_lovelace: 12,
    da_bond_pool_floor_lovelace: 13,
    challenge_record_lovelace: 14,
  }
  cbor.serialise(parameters) == #"d8799fd8799f010203ff0405060708090a0b0c0d0eff"
}

fn candidate_geometry() -> availability.ResponseGeometryV1 {
  availability.ResponseGeometryV1 {
    chunk_byte_length: 14_020,
    tranche_byte_length: 4 * 1024 * 1024,
    max_tranche_count: 16,
  }
}

fn alternate_geometry() -> availability.ResponseGeometryV1 {
  availability.ResponseGeometryV1 {
    chunk_byte_length: 8000,
    tranche_byte_length: 8 * 1024 * 1024,
    max_tranche_count: 8,
  }
}

fn candidate_parameters() -> availability.ParametersV1 {
  availability.ParametersV1 {
    response_geometry: candidate_geometry(),
    da_bond_lovelace: 10_000_000_000,
    challenger_bond_lovelace: 10_000_000_000,
    max_open_fee_lovelace: 500_000,
    max_publication_fee_lovelace: 500_000,
    max_settlement_fee_lovelace: 500_000,
    max_close_fee_lovelace: 1_000_000,
    max_timeout_fee_lovelace: 1_200_000,
    da_slash_penalty_lovelace: 2_500_000_000,
    da_bond_min_top_up_lovelace: 100_000_000,
    da_bond_pool_floor_lovelace: 5_000_000,
    challenge_record_lovelace: 27_000_000,
  }
}

fn descriptor(
  tranche_index: Int,
  start_offset: Int,
  byte_length: Int,
) -> availability.TrancheDescriptorV1 {
  availability.TrancheDescriptorV1 {
    tranche_index,
    start_offset,
    byte_length,
    chunk_count: ( byte_length + candidate_geometry().chunk_byte_length - 1 ) / candidate_geometry().chunk_byte_length,
    chunk_commitment: terminal,
    terminal_accumulator: terminal,
  }
}

fn small_commitment() -> availability.CommitmentV1 {
  availability.CommitmentV1 {
    version: availability.commitment_version_v1,
    deployment_identity: deployment,
    header_hash: header,
    payload_byte_length: 1024,
    response_geometry: candidate_geometry(),
    tranche_descriptors: [descriptor(0, 0, 1024)],
  }
}

test q58_deadline_classes_are_exact() {
  and {
    availability.response_window_ms_v1(64 * 1024) == Some(
      availability.small_response_window_ms_v1,
    ),
    availability.response_window_ms_v1(64 * 1024 + 1) == Some(
      availability.full_response_window_ms_v1,
    ),
    availability.response_window_ms_v1(0) == None,
    availability.response_window_ms_v1(64 * 1024 * 1024 + 1) == None,
  }
}

test q58_geometry_is_release_authenticated_not_probe_frozen() {
  let alternate = availability.ParametersV1 {
    ..candidate_parameters(),
    response_geometry: alternate_geometry(),
    da_bond_lovelace: 12_000_000_000,
    challenger_bond_lovelace: 12_000_000_000,
  }
  and {
    availability.response_geometry_is_canonical_v1(candidate_geometry()),
    availability.response_geometry_is_canonical_v1(alternate_geometry()),
    availability.parameters_are_canonical_v1(candidate_parameters()),
    availability.parameters_are_canonical_v1(alternate),
  }
}

/// The DA bond and the challenger bond are independent: a challenger bond one
/// lovelace off the DA bond, a 500 tADA DA bond under a 10,000 ADA challenger
/// bond, and a DA bond larger than the challenger bond are all canonical.
/// The fee reserve is exact to the lovelace and binds the challenger bond
/// only. Under the candidate geometry a full response costs at most
/// `16 * ceil(4 MiB / 14,020) = 4,800` publications, so the reserve is
/// `4,800 * publication + 16 * 500,000 settlement + max(close, timeout) =
/// 4,800 * publication + 9,200,000`, which must stay strictly under the
/// 10,000,000,000 challenger bond. The last funded publication fee is
/// therefore 2,081,416 and the first unfunded one 2,081,417 — and it is the
/// timeout ceiling, not the smaller close ceiling, that binds there. No DA
/// bond, however large, funds a reserve the challenger bond does not.
test q58_unequal_bonds_and_fee_reserve_are_exact() {
  let off_by_one = availability.ParametersV1 {
    ..candidate_parameters(),
    challenger_bond_lovelace: 9_999_999_999,
  }
  let testing_da_bond = availability.ParametersV1 {
    ..candidate_parameters(),
    da_bond_lovelace: 500_000_000,
    da_slash_penalty_lovelace: 100_000_000,
    da_bond_min_top_up_lovelace: 5_000_000,
  }
  let public_da_bond = availability.ParametersV1 {
    ..candidate_parameters(),
    da_bond_lovelace: 100_000_000_000,
    da_slash_penalty_lovelace: 25_000_000_000,
    da_bond_min_top_up_lovelace: 1_000_000_000,
  }
  let funded_publication_fee = availability.ParametersV1 {
    ..candidate_parameters(),
    max_publication_fee_lovelace: 2_081_416,
  }
  let excessive_publication_fee = availability.ParametersV1 {
    ..candidate_parameters(),
    max_publication_fee_lovelace: 2_081_417,
  }
  let excessive_fee_under_large_da_bond = availability.ParametersV1 {
    ..public_da_bond,
    max_publication_fee_lovelace: 2_081_417,
  }
  and {
    availability.parameters_are_canonical_v1(off_by_one),
    availability.parameters_are_canonical_v1(testing_da_bond),
    availability.parameters_are_canonical_v1(public_da_bond),
    availability.parameters_are_canonical_v1(funded_publication_fee),
    !availability.parameters_are_canonical_v1(excessive_publication_fee),
    !availability.parameters_are_canonical_v1(excessive_fee_under_large_da_bond),
  }
}

/// A slash penalty of at least the whole bond leaves the challenger no
/// reward, and a zero penalty burns nothing; both bounds are exact.
test q58_slash_penalty_is_strictly_inside_the_da_bond() {
  let largest = availability.ParametersV1 {
    ..candidate_parameters(),
    da_slash_penalty_lovelace: 9_999_999_999,
  }
  let smallest = availability.ParametersV1 {
    ..candidate_parameters(),
    da_slash_penalty_lovelace: 1,
  }
  let whole_bond = availability.ParametersV1 {
    ..candidate_parameters(),
    da_slash_penalty_lovelace: 10_000_000_000,
  }
  let zero = availability.ParametersV1 {
    ..candidate_parameters(),
    da_slash_penalty_lovelace: 0,
  }
  and {
    availability.parameters_are_canonical_v1(largest),
    availability.parameters_are_canonical_v1(smallest),
    !availability.parameters_are_canonical_v1(whole_bond),
    !availability.parameters_are_canonical_v1(zero),
  }
}

test q58_zero_min_top_up_is_refused() {
  !availability.parameters_are_canonical_v1(
    availability.ParametersV1 {
      ..candidate_parameters(),
      da_bond_min_top_up_lovelace: 0,
    },
  )
}

test q58_zero_pool_floor_is_refused() {
  !availability.parameters_are_canonical_v1(
    availability.ParametersV1 {
      ..candidate_parameters(),
      da_bond_pool_floor_lovelace: 0,
    },
  )
}

test q58_zero_challenge_record_lovelace_is_refused() {
  !availability.parameters_are_canonical_v1(
    availability.ParametersV1 {
      ..candidate_parameters(),
      challenge_record_lovelace: 0,
    },
  )
}

test q58_pool_amounts_of_one_lovelace_are_accepted() {
  availability.parameters_are_canonical_v1(
    availability.ParametersV1 {
      ..candidate_parameters(),
      da_bond_min_top_up_lovelace: 1,
      da_bond_pool_floor_lovelace: 1,
      challenge_record_lovelace: 1,
    },
  )
}

/// 4 MiB / 14,020 is non-integral. The reserve must charge 300 publications
/// for every full tranche, rather than ceil(64 MiB / 14,020), which would
/// undercount the independent per-tranche terminal fragments: 16 * 300 *
/// 500,000 + 16 * 500,000 + 1,200,000 = 2,409,200,000 must stay strictly
/// under the challenger bond.
test q58_non_divisible_tranche_publication_rounding_is_funded() {
  let underfunded = availability.ParametersV1 {
    ..candidate_parameters(),
    challenger_bond_lovelace: 2_409_200_000,
  }
  let funded = availability.ParametersV1 {
    ..candidate_parameters(),
    challenger_bond_lovelace: 2_409_200_001,
  }
  and {
    !availability.parameters_are_canonical_v1(underfunded),
    availability.parameters_are_canonical_v1(funded),
  }
}

test q58_canonical_commitment_accepts() {
  availability.commitment_is_canonical_v1(
    small_commitment(),
    candidate_parameters(),
  )
}

test q58_commitment_geometry_must_equal_release_parameters() {
  !availability.commitment_is_canonical_v1(
    small_commitment(),
    availability.ParametersV1 {
      ..candidate_parameters(),
      response_geometry: alternate_geometry(),
    },
  )
}

test q58_reordered_or_gapped_descriptor_rejects() {
  let reordered = availability.CommitmentV1 {
    ..small_commitment(),
    payload_byte_length: 4 * 1024 * 1024 + 1,
    tranche_descriptors: [
      descriptor(1, 4 * 1024 * 1024, 1),
      descriptor(0, 0, 4 * 1024 * 1024),
    ],
  }
  let gapped = availability.CommitmentV1 {
    ..small_commitment(),
    payload_byte_length: 4 * 1024 * 1024 + 1,
    tranche_descriptors: [
      descriptor(0, 0, 4 * 1024 * 1024),
      descriptor(1, 4 * 1024 * 1024 + 1, 1),
    ],
  }
  and {
    !availability.commitment_is_canonical_v1(reordered, candidate_parameters()),
    !availability.commitment_is_canonical_v1(gapped, candidate_parameters()),
  }
}

fn response_fixture(
  publication_deployment: ByteArray,
  publication_header: ByteArray,
  publication_challenge: ByteArray,
  publication_offset: Int,
  publication_length: Int,
  publication_hash: ByteArray,
) -> (availability.TrancheDatumV1, availability.PublicationDatumV1) {
  let chunk = #"010203"
  let provisional = descriptor(0, 0, bytearray.length(chunk))
  let expected_chunk_hash = crypto.blake2b_256(chunk)
  let leaf_hash = availability.chunk_leaf_hash_v1(
    0,
    0,
    0,
    bytearray.length(chunk),
    expected_chunk_hash,
  )
  let frontier = validation_merkle_v1.build_frontier([leaf_hash])
  let start = availability.tranche_start_accumulator_v1(
    deployment,
    header,
    provisional,
  )
  let next = availability.tranche_step_accumulator_v1(
    deployment,
    header,
    0,
    0,
    chunk,
    start,
  )
  let exact_descriptor = availability.TrancheDescriptorV1 {
    ..provisional,
    chunk_count: frontier.count,
    chunk_commitment: validation_merkle_v1.frontier_commitment(
      frontier.count,
      frontier.peaks,
    ),
    terminal_accumulator: next,
  }
  (
    availability.Active {
      deployment_identity: deployment,
      header_hash: header,
      challenge_asset_name: challenge,
      descriptor: exact_descriptor,
      next_offset: 0,
      accumulator: start,
      latest_carrier_output_index: None,
      response_deadline: 1_000_000,
      challenger: owner,
    },
    availability.PublicationDatumV1 {
      deployment_identity: publication_deployment,
      header_hash: publication_header,
      challenge_asset_name: publication_challenge,
      tranche_index: 0,
      chunk_index: 0,
      chunk_offset: publication_offset,
      chunk_byte_length: publication_length,
      chunk_hash: publication_hash,
      chunk_frontier: frontier.peaks,
      chunk_siblings: [],
      previous_accumulator: start,
      next_accumulator: next,
      chunk,
    },
  )
}

test q58_exact_publication_yields_compact_receipt() {
  let (active, publication) = response_fixture(
    deployment,
    header,
    challenge,
    0,
    3,
    #"11c0e79b71c3976ccd0c02d1310e2516c08edc9d8b6f57ccd680d63a4d8e72da",
  )
  when
    availability.publication_advances_active_tranche_v1(
      active,
      publication,
      4095,
      1,
    )
  is {
    Some(availability.Receipt { terminal_accumulator, .. }) ->
      terminal_accumulator == publication.next_accumulator
    _ -> False
  }
}

test q58_wrong_identity_offset_length_or_hash_rejects() {
  let (active_deployment, wrong_deployment) = response_fixture(
    other_deployment,
    header,
    challenge,
    0,
    3,
    #"11c0e79b71c3976ccd0c02d1310e2516c08edc9d8b6f57ccd680d63a4d8e72da",
  )
  let (active_header, wrong_header) = response_fixture(
    deployment,
    other_header,
    challenge,
    0,
    3,
    #"11c0e79b71c3976ccd0c02d1310e2516c08edc9d8b6f57ccd680d63a4d8e72da",
  )
  let (active_challenge, wrong_challenge) = response_fixture(
    deployment,
    header,
    other_challenge,
    0,
    3,
    #"11c0e79b71c3976ccd0c02d1310e2516c08edc9d8b6f57ccd680d63a4d8e72da",
  )
  let (active_offset, wrong_offset) = response_fixture(
    deployment,
    header,
    challenge,
    1,
    3,
    #"11c0e79b71c3976ccd0c02d1310e2516c08edc9d8b6f57ccd680d63a4d8e72da",
  )
  let (active_length, wrong_length) = response_fixture(
    deployment,
    header,
    challenge,
    0,
    2,
    #"11c0e79b71c3976ccd0c02d1310e2516c08edc9d8b6f57ccd680d63a4d8e72da",
  )
  let (active_hash, wrong_hash) = response_fixture(
    deployment,
    header,
    challenge,
    0,
    3,
    #"0000000000000000000000000000000000000000000000000000000000000000",
  )
  and {
    availability.publication_advances_active_tranche_v1(
      active_deployment,
      wrong_deployment,
      4095,
      1,
    ) == None,
    availability.publication_advances_active_tranche_v1(
      active_header,
      wrong_header,
      4095,
      1,
    ) == None,
    availability.publication_advances_active_tranche_v1(
      active_challenge,
      wrong_challenge,
      4095,
      1,
    ) == None,
    availability.publication_advances_active_tranche_v1(
      active_offset,
      wrong_offset,
      4095,
      1,
    ) == None,
    availability.publication_advances_active_tranche_v1(
      active_length,
      wrong_length,
      4095,
      1,
    ) == None,
    availability.publication_advances_active_tranche_v1(
      active_hash,
      wrong_hash,
      4095,
      1,
    ) == None,
  }
}

test q58_wrong_first_chunk_rejects_before_state_advance() {
  let (active, correct) = response_fixture(
    deployment,
    header,
    challenge,
    0,
    3,
    #"11c0e79b71c3976ccd0c02d1310e2516c08edc9d8b6f57ccd680d63a4d8e72da",
  )
  let wrong_chunk = #"020304"
  let wrong = availability.PublicationDatumV1 {
    ..correct,
    chunk_hash: crypto.blake2b_256(wrong_chunk),
    next_accumulator: availability.tranche_step_accumulator_v1(
      deployment,
      header,
      0,
      0,
      wrong_chunk,
      correct.previous_accumulator,
    ),
    chunk: wrong_chunk,
  }
  availability.publication_advances_active_tranche_v1(active, wrong, 4095, 1) == None
}

test q58_wrong_chunk_index_frontier_sibling_or_count_rejects() {
  let (active, correct) = response_fixture(
    deployment,
    header,
    challenge,
    0,
    3,
    #"11c0e79b71c3976ccd0c02d1310e2516c08edc9d8b6f57ccd680d63a4d8e72da",
  )
  expect
      availability.Active {
        deployment_identity,
        header_hash,
        challenge_asset_name,
        descriptor,
        next_offset,
        accumulator,
        latest_carrier_output_index,
        response_deadline,
        challenger,
      }
    = active
  let wrong_index = availability.PublicationDatumV1 {
    ..correct,
    chunk_index: 1,
  }
  let wrong_frontier = availability.PublicationDatumV1 {
    ..correct,
    chunk_frontier: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: #"0000000000000000000000000000000000000000000000000000000000000000",
      },
    ],
  }
  let wrong_sibling = availability.PublicationDatumV1 {
    ..correct,
    chunk_siblings: [
      #"0000000000000000000000000000000000000000000000000000000000000000",
    ],
  }
  let wrong_count_descriptor = availability.TrancheDescriptorV1 {
    ..descriptor,
    chunk_count: 2,
  }
  let wrong_count_active: availability.TrancheDatumV1 = availability.Active {
    deployment_identity,
    header_hash,
    challenge_asset_name,
    descriptor: wrong_count_descriptor,
    next_offset,
    accumulator,
    latest_carrier_output_index,
    response_deadline,
    challenger,
  }
  and {
    availability.publication_advances_active_tranche_v1(
      active,
      wrong_index,
      4095,
      1,
    ) == None,
    availability.publication_advances_active_tranche_v1(
      active,
      wrong_frontier,
      4095,
      1,
    ) == None,
    availability.publication_advances_active_tranche_v1(
      active,
      wrong_sibling,
      4095,
      1,
    ) == None,
    availability.publication_advances_active_tranche_v1(
      wrong_count_active,
      correct,
      4095,
      1,
    ) == None,
  }
}

test q58_wrong_middle_chunk_rejects_before_state_advance() {
  let chunk_0 = #"01"
  let chunk_1 = #"02"
  let chunk_2 = #"03"
  let provisional = descriptor(0, 0, 3)
  let leaf_0 = availability.chunk_leaf_hash_v1(
    0,
    0,
    0,
    1,
    crypto.blake2b_256(chunk_0),
  )
  let leaf_1 = availability.chunk_leaf_hash_v1(
    0,
    1,
    1,
    1,
    crypto.blake2b_256(chunk_1),
  )
  let leaf_2 = availability.chunk_leaf_hash_v1(
    0,
    2,
    2,
    1,
    crypto.blake2b_256(chunk_2),
  )
  let frontier = validation_merkle_v1.build_frontier([leaf_0, leaf_1, leaf_2])
  let exact_descriptor = availability.TrancheDescriptorV1 {
    ..provisional,
    chunk_count: 3,
    chunk_commitment: validation_merkle_v1.frontier_commitment(
      3,
      frontier.peaks,
    ),
  }
  let start = availability.tranche_start_accumulator_v1(
    deployment,
    header,
    exact_descriptor,
  )
  let after_first = availability.tranche_step_accumulator_v1(
    deployment,
    header,
    0,
    0,
    chunk_0,
    start,
  )
  let active: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: deployment,
    header_hash: header,
    challenge_asset_name: challenge,
    descriptor: exact_descriptor,
    next_offset: 1,
    accumulator: after_first,
    latest_carrier_output_index: Some(1),
    response_deadline: 1_000_000,
    challenger: owner,
  }
  let wrong_chunk = #"ff"
  let wrong: availability.PublicationDatumV1 = availability.PublicationDatumV1 {
    deployment_identity: deployment,
    header_hash: header,
    challenge_asset_name: challenge,
    tranche_index: 0,
    chunk_index: 1,
    chunk_offset: 1,
    chunk_byte_length: 1,
    chunk_hash: crypto.blake2b_256(wrong_chunk),
    chunk_frontier: frontier.peaks,
    chunk_siblings: [leaf_0],
    previous_accumulator: after_first,
    next_accumulator: availability.tranche_step_accumulator_v1(
      deployment,
      header,
      0,
      1,
      wrong_chunk,
      after_first,
    ),
    chunk: wrong_chunk,
  }
  availability.publication_advances_active_tranche_v1(active, wrong, 1, 1) == None
}

/// C1: the commitment hash is `blake2b_256(serialise_data(commitment))`,
/// untagged, the formula the SDK twin and the attested node status share.
test commitment_hash_v1_is_blake2b_256_of_the_serialised_commitment() {
  let commitment_data: Data = small_commitment()
  availability.commitment_hash_v1(small_commitment()) == crypto.blake2b_256(
    builtin.serialise_data(commitment_data),
  )
}

/// A hash that ignored a field would let an open's record carry a commitment
/// the committee never signed. Each test below changes one field of
/// `small_commitment()` and requires a different hash.
fn commitment_hash_differs_from_small(
  changed: availability.CommitmentV1,
) -> Bool {
  availability.commitment_hash_v1(changed) != availability.commitment_hash_v1(
    small_commitment(),
  )
}

test commitment_hash_v1_binds_version() {
  commitment_hash_differs_from_small(
    availability.CommitmentV1 {
      ..small_commitment(),
      version: availability.commitment_version_v1 + 1,
    },
  )
}

test commitment_hash_v1_binds_deployment_identity() {
  commitment_hash_differs_from_small(
    availability.CommitmentV1 {
      ..small_commitment(),
      deployment_identity: other_deployment,
    },
  )
}

test commitment_hash_v1_binds_header_hash() {
  commitment_hash_differs_from_small(
    availability.CommitmentV1 {
      ..small_commitment(),
      header_hash: other_header,
    },
  )
}

test commitment_hash_v1_binds_payload_byte_length() {
  commitment_hash_differs_from_small(
    availability.CommitmentV1 {
      ..small_commitment(),
      payload_byte_length: 1025,
    },
  )
}

test commitment_hash_v1_binds_response_geometry() {
  commitment_hash_differs_from_small(
    availability.CommitmentV1 {
      ..small_commitment(),
      response_geometry: alternate_geometry(),
    },
  )
}

test commitment_hash_v1_binds_tranche_descriptors() {
  commitment_hash_differs_from_small(
    availability.CommitmentV1 {
      ..small_commitment(),
      tranche_descriptors: [
        availability.TrancheDescriptorV1 {
          ..descriptor(0, 0, 1024),
          chunk_commitment: challenge,
        },
      ],
    },
  )
}
