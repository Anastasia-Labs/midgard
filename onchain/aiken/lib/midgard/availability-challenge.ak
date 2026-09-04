use aiken/builtin
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash, blake2b_256}
use aiken/option
use aiken/primitive/bytearray
use cardano/assets.{AssetName}
use cardano/transaction.{OutputReference}
use midgard/common/utils
use midgard/ledger_state.{HeaderHash}
use midgard/validation_merkle_v1

pub const commitment_version_v1: Int = 1

pub const small_payload_max_bytes_v1: Int = 64 * 1024

pub const full_payload_max_bytes_v1: Int = 64 * 1024 * 1024

pub const small_response_window_ms_v1: Int = 60 * 60 * 1000

pub const full_response_window_ms_v1: Int = 48 * 60 * 60 * 1000

/// Hard safety ceilings only. Activated geometry remains signed release data.
pub const max_response_chunk_safety_bytes_v1: Int = 15_148

pub const max_tranche_count_safety_v1: Int = 64

pub type ResponseGeometryV1 {
  chunk_byte_length: Int,
  tranche_byte_length: Int,
  max_tranche_count: Int,
}

pub type ParametersV1 {
  response_geometry: ResponseGeometryV1,
  da_bond_lovelace: Int,
  challenger_bond_lovelace: Int,
  max_open_fee_lovelace: Int,
  max_publication_fee_lovelace: Int,
  max_settlement_fee_lovelace: Int,
  max_close_fee_lovelace: Int,
  max_timeout_fee_lovelace: Int,
}

pub type TrancheDescriptorV1 {
  tranche_index: Int,
  start_offset: Int,
  byte_length: Int,
  chunk_count: Int,
  chunk_commitment: ByteArray,
  terminal_accumulator: ByteArray,
}

pub type CommitmentV1 {
  version: Int,
  deployment_identity: ByteArray,
  header_hash: HeaderHash,
  payload_byte_length: Int,
  response_geometry: ResponseGeometryV1,
  tranche_descriptors: List<TrancheDescriptorV1>,
  bond_owner: VerificationKeyHash,
}

/// Exact DA lifecycle carried by every state-queue node. Asset names are
/// additionally bound to the deployment's availability policy by each
/// transition validator.
pub type StateQueueStatusV1 {
  Unattested
  Attested { da_bond_asset_name: ByteArray }
  Challenged { da_bond_asset_name: ByteArray, challenge_asset_name: ByteArray }
  Published { terminal_commitment: ByteArray }
}

/// The availability policy also owns the dedicated bond, challenge and
/// tranche script. Every minting constructor is cross-bound to the
/// corresponding state transition.
pub type MintRedeemerV1 {
  MintBondFromAttestation {
    hub_oracle_ref_input_index: Int,
    da_attestation_input_index: Int,
    da_attestation_mint_redeemer_index: Int,
    bond_output_index: Int,
    state_queue_input_index: Int,
    state_queue_output_index: Int,
  }
  OpenChallenge {
    hub_oracle_ref_input_index: Int,
    bond_input_index: Int,
    bond_output_index: Int,
    challenger_input_index: Int,
    state_queue_input_index: Int,
    state_queue_output_index: Int,
    first_tranche_output_index: Int,
    terminal_accumulator_output_index: Int,
    challenger: VerificationKeyHash,
  }
  SettleTranche {
    bond_ref_input_index: Int,
    terminal_accumulator_input_index: Int,
    terminal_accumulator_output_index: Int,
    tranche_input_index: Int,
    carrier_input_index: Option<Int>,
  }
  CloseChallenge {
    hub_oracle_ref_input_index: Int,
    bond_input_index: Int,
    terminal_accumulator_input_index: Int,
    state_queue_input_index: Int,
    state_queue_output_index: Int,
    da_refund_output_index: Int,
    challenger_refund_output_index: Int,
  }
  TimeoutChallenge {
    hub_oracle_ref_input_index: Int,
    bond_input_index: Int,
    terminal_accumulator_input_index: Int,
    state_queue_mint_redeemer_index: Int,
    da_slash_output_index: Int,
    challenger_refund_output_index: Int,
  }
}

pub type SpendRedeemerV1 {
  AdvanceTranche {
    thread_output_index: Int,
    carrier_output_index: Int,
    m_previous_carrier_input_index: Option<Int>,
  }
  ConsumeCarrier { thread_input_index: Int, thread_spend_redeemer_index: Int }
  Coordinate { mint_redeemer_index: Int }
}

pub type BondDatumV1 {
  Available {
    commitment: CommitmentV1,
    da_bond_asset_name: ByteArray,
    committee_signers_hash: ByteArray,
    attested_signers: ByteArray,
  }
  ChallengedBond {
    commitment: CommitmentV1,
    da_bond_asset_name: ByteArray,
    committee_signers_hash: ByteArray,
    attested_signers: ByteArray,
    challenge_asset_name: ByteArray,
    challenger: VerificationKeyHash,
    opened_at: Int,
    response_deadline: Int,
  }
}

pub type TrancheDatumV1 {
  Active {
    deployment_identity: ByteArray,
    header_hash: HeaderHash,
    challenge_asset_name: ByteArray,
    descriptor: TrancheDescriptorV1,
    next_offset: Int,
    accumulator: ByteArray,
    latest_carrier_output_index: Option<Int>,
    response_deadline: Int,
    challenger: VerificationKeyHash,
  }
  Receipt {
    deployment_identity: ByteArray,
    header_hash: HeaderHash,
    challenge_asset_name: ByteArray,
    descriptor: TrancheDescriptorV1,
    terminal_accumulator: ByteArray,
    terminal_carrier_output_index: Int,
    challenger: VerificationKeyHash,
  }
}

pub type PublicationDatumV1 {
  deployment_identity: ByteArray,
  header_hash: HeaderHash,
  challenge_asset_name: ByteArray,
  tranche_index: Int,
  chunk_index: Int,
  chunk_offset: Int,
  chunk_byte_length: Int,
  chunk_hash: ByteArray,
  chunk_frontier: List<validation_merkle_v1.FrontierPeak>,
  chunk_siblings: List<ByteArray>,
  previous_accumulator: ByteArray,
  next_accumulator: ByteArray,
  chunk: ByteArray,
}

pub type TrancheTerminalStatusV1 {
  PublishedTranche { terminal_accumulator: ByteArray }
  TimedOutTranche { next_offset: Int, partial_accumulator: ByteArray }
}

pub type TerminalAccumulatorDatumV1 {
  deployment_identity: ByteArray,
  header_hash: HeaderHash,
  challenge_asset_name: ByteArray,
  next_tranche_index: Int,
  folded_terminal_accumulator: ByteArray,
  has_timed_out_tranche: Bool,
  response_deadline: Int,
  challenger: VerificationKeyHash,
  remaining_challenger_lovelace: Int,
}

pub const da_bond_asset_name_prefix_v1: AssetName = "DABN"

pub const challenge_asset_name_prefix_v1: AssetName = "DACH"

pub const tranche_asset_name_prefix_v1: AssetName = "DT"

pub const terminal_accumulator_asset_name_prefix_v1: AssetName = "DACT"

pub fn da_bond_asset_name_v1(
  attestation_input_outref: OutputReference,
) -> AssetName {
  let outref_data: Data = attestation_input_outref
  da_bond_asset_name_prefix_v1
    |> bytearray.concat(utils.serialise_and_hash_28(outref_data))
}

pub fn challenge_asset_name_v1(bond_input_outref: OutputReference) -> AssetName {
  let outref_data: Data = bond_input_outref
  challenge_asset_name_prefix_v1
    |> bytearray.concat(utils.serialise_and_hash_28(outref_data))
}

pub fn tranche_asset_name_v1(
  challenge_asset_name: AssetName,
  tranche_index: Int,
) -> AssetName {
  expect bytearray.length(challenge_asset_name) == 32
  expect tranche_index >= 0
  expect tranche_index < max_tranche_count_safety_v1
  tranche_asset_name_prefix_v1
    |> bytearray.concat(bytearray.drop(challenge_asset_name, 4))
    |> bytearray.concat(builtin.integer_to_bytearray(False, 2, tranche_index))
}

pub fn terminal_accumulator_asset_name_v1(
  challenge_asset_name: AssetName,
) -> AssetName {
  expect bytearray.length(challenge_asset_name) == 32
  terminal_accumulator_asset_name_prefix_v1
    |> bytearray.concat(bytearray.drop(challenge_asset_name, 4))
}

type TrancheStartV1 {
  version: Int,
  deployment_identity: ByteArray,
  header_hash: HeaderHash,
  tranche_index: Int,
  start_offset: Int,
  byte_length: Int,
}

type TrancheStepV1 {
  version: Int,
  deployment_identity: ByteArray,
  header_hash: HeaderHash,
  tranche_index: Int,
  chunk_offset: Int,
  chunk_byte_length: Int,
  chunk_hash: ByteArray,
  previous_accumulator: ByteArray,
}

type TerminalAccumulatorStartV1 {
  version: Int,
  deployment_identity: ByteArray,
  header_hash: HeaderHash,
  challenge_asset_name: ByteArray,
}

type TerminalAccumulatorStepV1 {
  version: Int,
  previous_accumulator: ByteArray,
  tranche_index: Int,
  status: TrancheTerminalStatusV1,
}

type ChunkLeafV1 {
  version: Int,
  tranche_index: Int,
  chunk_index: Int,
  chunk_offset: Int,
  chunk_byte_length: Int,
  chunk_hash: ByteArray,
}

fn ceil_div(numerator: Int, denominator: Int) -> Int {
  ( numerator + denominator - 1 ) / denominator
}

pub fn response_window_ms_v1(payload_byte_length: Int) -> Option<Int> {
  if and {
    payload_byte_length > 0,
    payload_byte_length <= small_payload_max_bytes_v1,
  } {
    Some(small_response_window_ms_v1)
  } else if and {
    payload_byte_length > small_payload_max_bytes_v1,
    payload_byte_length <= full_payload_max_bytes_v1,
  } {
    Some(full_response_window_ms_v1)
  } else {
    None
  }
}

pub fn response_deadline_v1(
  payload_byte_length: Int,
  opened_at: Int,
) -> Option<Int> {
  if opened_at < 0 {
    None
  } else {
    response_window_ms_v1(payload_byte_length)
      |> option.map(fn(window) { opened_at + window })
  }
}

pub fn response_geometry_is_canonical_v1(geometry: ResponseGeometryV1) -> Bool {
  and {
    geometry.chunk_byte_length > 0,
    geometry.chunk_byte_length <= max_response_chunk_safety_bytes_v1,
    geometry.tranche_byte_length >= small_payload_max_bytes_v1,
    geometry.tranche_byte_length <= full_payload_max_bytes_v1,
    geometry.max_tranche_count > 0,
    geometry.max_tranche_count <= max_tranche_count_safety_v1,
    ceil_div(full_payload_max_bytes_v1, geometry.tranche_byte_length) <= geometry.max_tranche_count,
  }
}

pub fn parameters_are_canonical_v1(parameters: ParametersV1) -> Bool {
  let geometry = parameters.response_geometry
  let full_tranches = full_payload_max_bytes_v1 / geometry.tranche_byte_length
  let remainder = full_payload_max_bytes_v1 % geometry.tranche_byte_length
  let publications_per_full_tranche = ceil_div(
    geometry.tranche_byte_length,
    geometry.chunk_byte_length,
  )
  let maximum_publications = full_tranches * publications_per_full_tranche + if remainder == 0 {
    0
  } else {
    ceil_div(remainder, geometry.chunk_byte_length)
  }
  and {
    response_geometry_is_canonical_v1(geometry),
    parameters.da_bond_lovelace > 0,
    parameters.challenger_bond_lovelace == parameters.da_bond_lovelace,
    parameters.max_open_fee_lovelace > 0,
    parameters.max_publication_fee_lovelace > 0,
    parameters.max_settlement_fee_lovelace > 0,
    parameters.max_close_fee_lovelace > 0,
    parameters.max_timeout_fee_lovelace > 0,
    maximum_publications * parameters.max_publication_fee_lovelace + geometry.max_tranche_count * parameters.max_settlement_fee_lovelace + parameters.max_close_fee_lovelace < parameters.challenger_bond_lovelace,
    maximum_publications * parameters.max_publication_fee_lovelace + geometry.max_tranche_count * parameters.max_settlement_fee_lovelace + parameters.max_timeout_fee_lovelace < parameters.challenger_bond_lovelace,
  }
}

fn descriptors_are_canonical_v1(
  descriptors: List<TrancheDescriptorV1>,
  payload_byte_length: Int,
  tranche_byte_length: Int,
  chunk_byte_length: Int,
  expected_index: Int,
  expected_offset: Int,
) -> Bool {
  when descriptors is {
    [] -> and {
        expected_offset == payload_byte_length,
        expected_index == ceil_div(payload_byte_length, tranche_byte_length),
      }
    [descriptor, ..rest] -> {
      let remaining = payload_byte_length - expected_offset
      let expected_length = if remaining < tranche_byte_length {
        remaining
      } else {
        tranche_byte_length
      }
      and {
        expected_length > 0,
        descriptor.tranche_index == expected_index,
        descriptor.start_offset == expected_offset,
        descriptor.byte_length == expected_length,
        descriptor.chunk_count == ceil_div(expected_length, chunk_byte_length),
        bytearray.length(descriptor.chunk_commitment) == 32,
        bytearray.length(descriptor.terminal_accumulator) == 32,
        descriptors_are_canonical_v1(
          rest,
          payload_byte_length,
          tranche_byte_length,
          chunk_byte_length,
          expected_index + 1,
          expected_offset + expected_length,
        ),
      }
    }
  }
}

pub fn commitment_is_canonical_v1(
  commitment: CommitmentV1,
  authenticated_parameters: ParametersV1,
) -> Bool {
  and {
    commitment.version == commitment_version_v1,
    bytearray.length(commitment.deployment_identity) == 28,
    bytearray.length(commitment.header_hash) == 28,
    bytearray.length(commitment.bond_owner) == 28,
    response_window_ms_v1(commitment.payload_byte_length) != None,
    parameters_are_canonical_v1(authenticated_parameters),
    commitment.response_geometry == authenticated_parameters.response_geometry,
    list.length(commitment.tranche_descriptors) <= commitment.response_geometry.max_tranche_count,
    descriptors_are_canonical_v1(
      commitment.tranche_descriptors,
      commitment.payload_byte_length,
      commitment.response_geometry.tranche_byte_length,
      commitment.response_geometry.chunk_byte_length,
      0,
      0,
    ),
  }
}

fn descriptor_publication_count_v1(
  descriptor: TrancheDescriptorV1,
  chunk_byte_length: Int,
) -> Int {
  ceil_div(descriptor.byte_length, chunk_byte_length)
}

fn total_publication_reserve_v1(
  descriptors: List<TrancheDescriptorV1>,
  chunk_byte_length: Int,
  maximum_publication_fee: Int,
) -> Int {
  descriptors
    |> list.foldl(
        0,
        fn(descriptor, total) {
          total + descriptor_publication_count_v1(descriptor, chunk_byte_length) * maximum_publication_fee
        },
      )
}

pub fn terminal_accumulator_reserve_lovelace_v1(parameters: ParametersV1) -> Int {
  if parameters.max_close_fee_lovelace > parameters.max_timeout_fee_lovelace {
    parameters.max_close_fee_lovelace
  } else {
    parameters.max_timeout_fee_lovelace
  }
}

fn initial_tranche_lovelaces_v1(
  descriptors: List<TrancheDescriptorV1>,
  chunk_byte_length: Int,
  maximum_publication_fee: Int,
  maximum_settlement_fee: Int,
  base: Int,
  remainder: Int,
  tranche_index: Int,
) -> List<Int> {
  when descriptors is {
    [] -> []
    [descriptor, ..rest] ->
      [
        descriptor_publication_count_v1(descriptor, chunk_byte_length) * maximum_publication_fee + maximum_settlement_fee + base + if tranche_index < remainder {
          1
        } else {
          0
        },
        ..initial_tranche_lovelaces_v1(
          rest,
          chunk_byte_length,
          maximum_publication_fee,
          maximum_settlement_fee,
          base,
          remainder,
          tranche_index + 1,
        )
      ]
  }
}

/// Compute the exact challenger-bond split in one canonical pass. Open uses
/// this vector directly so validating N tranche outputs remains O(N), rather
/// than revalidating and rescanning the full commitment for every tranche.
pub fn tranche_initial_lovelaces_v1(
  commitment: CommitmentV1,
  parameters: ParametersV1,
) -> Option<List<Int>> {
  let descriptors = commitment.tranche_descriptors
  let descriptor_count = list.length(descriptors)
  if or {
    !commitment_is_canonical_v1(commitment, parameters),
    descriptor_count <= 0,
  } {
    None
  } else {
    let terminal_reserve = terminal_accumulator_reserve_lovelace_v1(parameters)
    let total_publication_reserve = total_publication_reserve_v1(
      descriptors,
      parameters.response_geometry.chunk_byte_length,
      parameters.max_publication_fee_lovelace,
    )
    let total_settlement_reserve = descriptor_count * parameters.max_settlement_fee_lovelace
    let distributable = parameters.challenger_bond_lovelace - total_publication_reserve - total_settlement_reserve - terminal_reserve
    let base = distributable / descriptor_count
    let remainder = distributable % descriptor_count
    Some(
      initial_tranche_lovelaces_v1(
        descriptors,
        parameters.response_geometry.chunk_byte_length,
        parameters.max_publication_fee_lovelace,
        parameters.max_settlement_fee_lovelace,
        base,
        remainder,
        0,
      ),
    )
  }
}

/// Deterministic exact split of the challenger bond. Every tranche first gets
/// its worst-case publication reserve, tranche zero gets the single terminal
/// reserve, and the remaining lovelace is split with the remainder assigned to
/// the earliest descriptors.
pub fn tranche_initial_lovelace_v1(
  commitment: CommitmentV1,
  parameters: ParametersV1,
  tranche_index: Int,
) -> Option<Int> {
  tranche_initial_lovelaces_v1(commitment, parameters)
    |> option.and_then(fn(lovelaces) { list.at(lovelaces, tranche_index) })
}

pub fn terminal_accumulator_initial_lovelace_v1(
  commitment: CommitmentV1,
  parameters: ParametersV1,
) -> Option<Int> {
  if !commitment_is_canonical_v1(commitment, parameters) {
    None
  } else {
    Some(terminal_accumulator_reserve_lovelace_v1(parameters))
  }
}

pub fn terminal_accumulator_start_v1(
  commitment: CommitmentV1,
  challenge_asset_name: AssetName,
) -> ByteArray {
  let start = TerminalAccumulatorStartV1 {
    version: commitment_version_v1,
    deployment_identity: commitment.deployment_identity,
    header_hash: commitment.header_hash,
    challenge_asset_name,
  }
  let start_data: Data = start
  hash_domain_and_data(
    bytearray.from_string(@"MidgardDaAvailabilityTerminalStartV1"),
    start_data,
  )
}

pub fn fold_terminal_accumulator_v1(
  previous_accumulator: ByteArray,
  tranche_index: Int,
  status: TrancheTerminalStatusV1,
) -> ByteArray {
  let step = TerminalAccumulatorStepV1 {
    version: commitment_version_v1,
    previous_accumulator,
    tranche_index,
    status,
  }
  let step_data: Data = step
  hash_domain_and_data(
    bytearray.from_string(@"MidgardDaAvailabilityTerminalStepV1"),
    step_data,
  )
}

fn hash_domain_and_data(domain: ByteArray, data: Data) -> ByteArray {
  blake2b_256(domain |> bytearray.concat(builtin.serialise_data(data)))
}

pub fn attestation_message_v1(commitment: CommitmentV1) -> ByteArray {
  let commitment_data: Data = commitment
  hash_domain_and_data(
    bytearray.from_string(@"MidgardDaAvailabilityAttestationV1"),
    commitment_data,
  )
}

pub fn published_terminal_commitment_v1(commitment: CommitmentV1) -> ByteArray {
  let commitment_data: Data = commitment
  hash_domain_and_data(
    bytearray.from_string(@"MidgardDaAvailabilityPublishedV1"),
    commitment_data,
  )
}

pub fn tranche_start_accumulator_v1(
  deployment_identity: ByteArray,
  header_hash: HeaderHash,
  descriptor: TrancheDescriptorV1,
) -> ByteArray {
  let start = TrancheStartV1 {
    version: commitment_version_v1,
    deployment_identity,
    header_hash,
    tranche_index: descriptor.tranche_index,
    start_offset: descriptor.start_offset,
    byte_length: descriptor.byte_length,
  }
  let start_data: Data = start
  hash_domain_and_data(
    bytearray.from_string(@"MidgardDaAvailabilityTrancheStartV1"),
    start_data,
  )
}

pub fn tranche_step_accumulator_v1(
  deployment_identity: ByteArray,
  header_hash: HeaderHash,
  tranche_index: Int,
  chunk_offset: Int,
  chunk: ByteArray,
  previous_accumulator: ByteArray,
) -> ByteArray {
  let step = TrancheStepV1 {
    version: commitment_version_v1,
    deployment_identity,
    header_hash,
    tranche_index,
    chunk_offset,
    chunk_byte_length: bytearray.length(chunk),
    chunk_hash: blake2b_256(chunk),
    previous_accumulator,
  }
  let step_data: Data = step
  hash_domain_and_data(
    bytearray.from_string(@"MidgardDaAvailabilityTrancheStepV1"),
    step_data,
  )
}

pub fn chunk_leaf_hash_v1(
  tranche_index: Int,
  chunk_index: Int,
  chunk_offset: Int,
  chunk_byte_length: Int,
  chunk_hash: ByteArray,
) -> ByteArray {
  let leaf = ChunkLeafV1 {
    version: commitment_version_v1,
    tranche_index,
    chunk_index,
    chunk_offset,
    chunk_byte_length,
    chunk_hash,
  }
  let leaf_data: Data = leaf
  hash_domain_and_data(
    bytearray.from_string(@"MidgardDaAvailabilityChunkLeafV1"),
    leaf_data,
  )
}

fn publication_chunk_is_committed_v1(
  descriptor: TrancheDescriptorV1,
  publication: PublicationDatumV1,
) -> Bool {
  if and {
    descriptor.chunk_count > 0,
    publication.chunk_index >= 0,
    publication.chunk_index < descriptor.chunk_count,
    validation_merkle_v1.frontier_is_well_formed(
      descriptor.chunk_count,
      publication.chunk_frontier,
    ),
  } {
    let leaf_hash = chunk_leaf_hash_v1(
      descriptor.tranche_index,
      publication.chunk_index,
      publication.chunk_offset,
      publication.chunk_byte_length,
      publication.chunk_hash,
    )
    and {
      validation_merkle_v1.frontier_commitment(
        descriptor.chunk_count,
        publication.chunk_frontier,
      ) == descriptor.chunk_commitment,
      validation_merkle_v1.verify_membership(
        descriptor.chunk_count,
        publication.chunk_frontier,
        publication.chunk_index,
        leaf_hash,
        publication.chunk_siblings,
      ),
    }
  } else {
    False
  }
}

pub fn publication_advances_active_tranche_v1(
  active: TrancheDatumV1,
  publication: PublicationDatumV1,
  chunk_byte_length: Int,
  carrier_output_index: Int,
) -> Option<TrancheDatumV1> {
  when active is {
    Receipt { .. } -> None
    Active {
      deployment_identity,
      header_hash,
      challenge_asset_name,
      descriptor,
      next_offset,
      accumulator,
      latest_carrier_output_index: _,
      response_deadline,
      challenger,
    } -> {
      let end_offset = descriptor.start_offset + descriptor.byte_length
      let remaining = end_offset - next_offset
      let expected_chunk_length = if remaining < chunk_byte_length {
        remaining
      } else {
        chunk_byte_length
      }
      let next_accumulator = tranche_step_accumulator_v1(
        deployment_identity,
        header_hash,
        descriptor.tranche_index,
        next_offset,
        publication.chunk,
        accumulator,
      )
      let expected_chunk_index = ( next_offset - descriptor.start_offset ) / chunk_byte_length
      if and {
        expected_chunk_length > 0,
        publication.deployment_identity == deployment_identity,
        publication.header_hash == header_hash,
        publication.challenge_asset_name == challenge_asset_name,
        publication.tranche_index == descriptor.tranche_index,
        publication.chunk_index == expected_chunk_index,
        publication.chunk_offset == next_offset,
        publication.chunk_byte_length == expected_chunk_length,
        bytearray.length(publication.chunk) == expected_chunk_length,
        publication.chunk_hash == blake2b_256(publication.chunk),
        publication.previous_accumulator == accumulator,
        publication.next_accumulator == next_accumulator,
        publication_chunk_is_committed_v1(descriptor, publication),
        carrier_output_index >= 0,
      } {
        let advanced_offset = next_offset + expected_chunk_length
        if advanced_offset == end_offset {
          if next_accumulator == descriptor.terminal_accumulator {
            Some(
              Receipt {
                deployment_identity,
                header_hash,
                challenge_asset_name,
                descriptor,
                terminal_accumulator: next_accumulator,
                terminal_carrier_output_index: carrier_output_index,
                challenger,
              },
            )
          } else {
            None
          }
        } else {
          Some(
            Active {
              deployment_identity,
              header_hash,
              challenge_asset_name,
              descriptor,
              next_offset: advanced_offset,
              accumulator: next_accumulator,
              latest_carrier_output_index: Some(carrier_output_index),
              response_deadline,
              challenger,
            },
          )
        }
      } else {
        None
      }
    }
  }
}
