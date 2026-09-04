use aiken/builtin
use aiken/collection/list
use aiken/crypto
use aiken/interval.{between}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use availability_challenge as availability_validator
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Spend, Transaction,
}
use midgard/availability_challenge as availability
use midgard/da_attestation_types as da_attestation
use midgard/hub_oracle as hub
use midgard/ledger_state.{ConfirmedState, HeaderV1, protocol_version_v1}
use midgard/state_queue
use midgard/validation_merkle_v1

const hub_policy = #"11111111111111111111111111111111111111111111111111111111"

const state_queue_policy =
  #"22222222222222222222222222222222222222222222222222222222"

const availability_policy =
  #"33333333333333333333333333333333333333333333333333333333"

const da_policy = #"34343434343434343434343434343434343434343434343434343434"

const miscellaneous_policy =
  #"44444444444444444444444444444444444444444444444444444444"

const bond_owner = #"55555555555555555555555555555555555555555555555555555555"

const challenger = #"66666666666666666666666666666666666666666666666666666666"

const terminal =
  #"7777777777777777777777777777777777777777777777777777777777777777"

const empty_root =
  #"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

const da_bond_asset_name =
  #"4441424e88888888888888888888888888888888888888888888888888888888"

const committee_hash =
  #"9999999999999999999999999999999999999999999999999999999999999999"

const close_fee = 100

fn parameters() -> availability.ParametersV1 {
  availability.ParametersV1 {
    response_geometry: availability.ResponseGeometryV1 {
      chunk_byte_length: 14_020,
      tranche_byte_length: 4 * 1024 * 1024,
      max_tranche_count: 16,
    },
    da_bond_lovelace: 10_000_000_000,
    challenger_bond_lovelace: 10_000_000_000,
    max_open_fee_lovelace: 500_000,
    max_publication_fee_lovelace: 500_000,
    max_settlement_fee_lovelace: 500_000,
    max_close_fee_lovelace: 1_000_000,
    max_timeout_fee_lovelace: 1_200_000,
  }
}

fn outref(output_index: Int) -> OutputReference {
  OutputReference {
    transaction_id: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    output_index,
  }
}

fn header() -> HeaderV1 {
  HeaderV1 {
    prev_utxos_root: empty_root,
    utxos_root: empty_root,
    withdrawals_root: empty_root,
    forced_transactions_root: empty_root,
    transactions_root: empty_root,
    deposits_root: empty_root,
    transition_trace_root: empty_root,
    event_to_step_root: empty_root,
    validation_traces_root: empty_root,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time: 0,
    end_time: 1,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: miscellaneous_policy,
    operator_vkey: bond_owner,
    protocol_version: protocol_version_v1,
  }
}

fn header_hash() -> ByteArray {
  let header_data: Data = header()
  crypto.blake2b_224(builtin.serialise_data(header_data))
}

fn descriptor() -> availability.TrancheDescriptorV1 {
  let leaf = availability.chunk_leaf_hash_v1(
    0,
    0,
    0,
    1,
    crypto.blake2b_256(#"01"),
  )
  let frontier = validation_merkle_v1.build_frontier([leaf])
  availability.TrancheDescriptorV1 {
    tranche_index: 0,
    start_offset: 0,
    byte_length: 1,
    chunk_count: frontier.count,
    chunk_commitment: validation_merkle_v1.frontier_commitment(
      frontier.count,
      frontier.peaks,
    ),
    terminal_accumulator: terminal,
  }
}

fn commitment_for(
  deployment_identity: ByteArray,
  owner: ByteArray,
) -> availability.CommitmentV1 {
  availability.CommitmentV1 {
    version: availability.commitment_version_v1,
    deployment_identity,
    header_hash: header_hash(),
    payload_byte_length: 1,
    response_geometry: parameters().response_geometry,
    tranche_descriptors: [descriptor()],
    bond_owner: owner,
  }
}

fn commitment(owner: ByteArray) -> availability.CommitmentV1 {
  commitment_for(hub_policy, owner)
}

fn maximum_descriptor(index: Int) -> availability.TrancheDescriptorV1 {
  let index_data: Data = index
  availability.TrancheDescriptorV1 {
    tranche_index: index,
    start_offset: index * parameters().response_geometry.tranche_byte_length,
    byte_length: parameters().response_geometry.tranche_byte_length,
    chunk_count: (
      parameters().response_geometry.tranche_byte_length + parameters().response_geometry.chunk_byte_length - 1
    ) / parameters().response_geometry.chunk_byte_length,
    chunk_commitment: terminal,
    terminal_accumulator: crypto.blake2b_256(builtin.serialise_data(index_data)),
  }
}

fn maximum_descriptors() -> List<availability.TrancheDescriptorV1> {
  list.map(list.range(0, 15), maximum_descriptor)
}

fn maximum_commitment(owner: ByteArray) -> availability.CommitmentV1 {
  availability.CommitmentV1 {
    version: availability.commitment_version_v1,
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    payload_byte_length: availability.full_payload_max_bytes_v1,
    response_geometry: parameters().response_geometry,
    tranche_descriptors: maximum_descriptors(),
    bond_owner: owner,
  }
}

fn challenge_asset_name() -> ByteArray {
  availability.challenge_asset_name_v1(outref(0))
}

fn tranche_asset_name() -> ByteArray {
  availability.tranche_asset_name_v1(challenge_asset_name(), 0)
}

fn terminal_accumulator_asset_name() -> ByteArray {
  availability.terminal_accumulator_asset_name_v1(challenge_asset_name())
}

fn node_output(status: availability.StateQueueStatusV1) -> Output {
  let datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Node(
      state_queue.StateQueueNode { header: header(), da_attestation: status },
    ),
    link: None,
  }
  Output {
    address: address.from_script(state_queue_policy),
    value: assets.from_lovelace(2_000_000)
      |> assets.add(
          state_queue_policy,
          state_queue.block_asset_name_prefix
            |> bytearray.concat(header_hash()),
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn state_queue_input() -> Input {
  Input {
    output_reference: outref(1),
    output: node_output(
      availability.Challenged {
        da_bond_asset_name,
        challenge_asset_name: challenge_asset_name(),
      },
    ),
  }
}

fn state_queue_input_with_status(
  status: availability.StateQueueStatusV1,
) -> Input {
  Input { output_reference: outref(1), output: node_output(status) }
}

fn bond_input(owner: ByteArray, claimant: ByteArray) -> Input {
  let datum: availability.BondDatumV1 = availability.ChallengedBond {
    commitment: commitment(owner),
    da_bond_asset_name,
    committee_signers_hash: committee_hash,
    attested_signers: #"",
    challenge_asset_name: challenge_asset_name(),
    challenger: claimant,
    opened_at: 0,
    response_deadline: availability.small_response_window_ms_v1,
  }
  Input {
    output_reference: outref(0),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(parameters().da_bond_lovelace)
        |> assets.add(availability_policy, da_bond_asset_name, 1)
        |> assets.add(availability_policy, challenge_asset_name(), 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn available_bond_input(owner: ByteArray) -> Input {
  let datum: availability.BondDatumV1 = availability.Available {
    commitment: commitment(owner),
    da_bond_asset_name,
    committee_signers_hash: committee_hash,
    attested_signers: #"",
  }
  Input {
    output_reference: outref(0),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(parameters().da_bond_lovelace)
        |> assets.add(availability_policy, da_bond_asset_name, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn challenged_bond_output(owner: ByteArray, claimant: ByteArray) -> Output {
  let datum: availability.BondDatumV1 = availability.ChallengedBond {
    commitment: commitment(owner),
    da_bond_asset_name,
    committee_signers_hash: committee_hash,
    attested_signers: #"",
    challenge_asset_name: challenge_asset_name(),
    challenger: claimant,
    opened_at: 0,
    response_deadline: availability.small_response_window_ms_v1,
  }
  Output {
    address: address.from_script(availability_policy),
    value: assets.from_lovelace(parameters().da_bond_lovelace)
      |> assets.add(availability_policy, da_bond_asset_name, 1)
      |> assets.add(availability_policy, challenge_asset_name(), 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn initial_tranche_output(claimant: ByteArray) -> Output {
  expect Some(lovelace) = availability.tranche_initial_lovelace_v1(
    commitment(bond_owner),
    parameters(),
    0,
  )
  let datum: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor: descriptor(),
    next_offset: 0,
    accumulator: availability.tranche_start_accumulator_v1(
      hub_policy,
      header_hash(),
      descriptor(),
    ),
    latest_carrier_output_index: None,
    response_deadline: availability.small_response_window_ms_v1,
    challenger: claimant,
  }
  Output {
    address: address.from_script(availability_policy),
    value: assets.from_lovelace(lovelace)
      |> assets.add(availability_policy, tranche_asset_name(), 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn initial_terminal_accumulator_output(claimant: ByteArray) -> Output {
  expect Some(lovelace) = availability.terminal_accumulator_initial_lovelace_v1(
    commitment(bond_owner),
    parameters(),
  )
  let datum: availability.TerminalAccumulatorDatumV1 = availability.TerminalAccumulatorDatumV1 {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    next_tranche_index: 0,
    folded_terminal_accumulator: availability.terminal_accumulator_start_v1(
      commitment(bond_owner),
      challenge_asset_name(),
    ),
    has_timed_out_tranche: False,
    response_deadline: availability.small_response_window_ms_v1,
    challenger: claimant,
    remaining_challenger_lovelace: lovelace,
  }
  Output {
    address: address.from_script(availability_policy),
    value: assets.from_lovelace(lovelace)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn challenger_input(claimant: ByteArray) -> Input {
  Input {
    output_reference: outref(2),
    output: Output {
      address: address.from_verification_key(claimant),
      value: assets.from_lovelace(
        parameters().challenger_bond_lovelace + close_fee,
      ),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn open_redeemer(
  bond_output_index: Int,
  state_queue_output_index: Int,
  claimant: ByteArray,
) -> availability.MintRedeemerV1 {
  availability.OpenChallenge {
    hub_oracle_ref_input_index: 0,
    bond_input_index: 0,
    bond_output_index,
    challenger_input_index: 2,
    state_queue_input_index: 1,
    state_queue_output_index,
    first_tranche_output_index: 2,
    terminal_accumulator_output_index: 3,
    challenger: claimant,
  }
}

fn open_transaction(
  bond_output_index: Int,
  state_queue_output_index: Int,
  claimant: ByteArray,
  signatory: ByteArray,
) -> Transaction {
  let redeemer = open_redeemer(
    bond_output_index,
    state_queue_output_index,
    claimant,
  )
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      available_bond_input(bond_owner),
      state_queue_input_with_status(
        availability.Attested { da_bond_asset_name },
      ),
      challenger_input(claimant),
    ],
    outputs: [
      challenged_bond_output(bond_owner, claimant),
      node_output(
        availability.Challenged {
          da_bond_asset_name,
          challenge_asset_name: challenge_asset_name(),
        },
      ),
      initial_tranche_output(claimant),
      initial_terminal_accumulator_output(claimant),
    ],
    reference_inputs: [hub_reference_input()],
    mint: assets.from_asset(availability_policy, challenge_asset_name(), 1)
      |> assets.add(availability_policy, tranche_asset_name(), 1)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), 1),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    extra_signatories: [signatory],
    fee: close_fee,
    validity_range: between(0, 1_000),
  }
}

fn da_attestation_asset_name() -> ByteArray {
  da_attestation.attestation_asset_name_prefix
    |> bytearray.concat(header_hash())
}

fn attestation_input(owner: ByteArray, deployment_identity: ByteArray) -> Input {
  let datum: da_attestation.DaAttestationDatum = da_attestation.DaAttestationDatum {
    header_hash: header_hash(),
    availability_commitment: commitment_for(deployment_identity, owner),
    da_threshold: 1,
    committee_signers_hash: committee_hash,
    rescue_beneficiary: address.from_verification_key(owner),
    attested_signers: #"80",
    attestation_count: 1,
  }
  Input {
    output_reference: outref(0),
    output: Output {
      address: address.from_script(da_policy),
      value: assets.from_lovelace(parameters().da_bond_lovelace)
        |> assets.add(da_policy, da_attestation_asset_name(), 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn minted_bond_output(
  owner: ByteArray,
  deployment_identity: ByteArray,
) -> Output {
  let bond_asset_name = availability.da_bond_asset_name_v1(outref(0))
  let datum: availability.BondDatumV1 = availability.Available {
    commitment: commitment_for(deployment_identity, owner),
    da_bond_asset_name: bond_asset_name,
    committee_signers_hash: committee_hash,
    attested_signers: #"80",
  }
  Output {
    address: address.from_script(availability_policy),
    value: assets.from_lovelace(parameters().da_bond_lovelace)
      |> assets.add(availability_policy, bond_asset_name, 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn mint_bond_redeemer(
  bond_output_index: Int,
  state_queue_output_index: Int,
) -> availability.MintRedeemerV1 {
  availability.MintBondFromAttestation {
    hub_oracle_ref_input_index: 0,
    da_attestation_input_index: 0,
    da_attestation_mint_redeemer_index: 0,
    bond_output_index,
    state_queue_input_index: 1,
    state_queue_output_index,
  }
}

fn mint_bond_transaction(
  owner: ByteArray,
  deployment_identity: ByteArray,
  bond_output_index: Int,
  state_queue_output_index: Int,
) -> Transaction {
  let availability_redeemer = mint_bond_redeemer(
    bond_output_index,
    state_queue_output_index,
  )
  let availability_redeemer_data: Data = availability_redeemer
  let da_redeemer: da_attestation.MintRedeemer = da_attestation.ApplyToStateQueue {
    da_attestation_input_index: 0,
    da_params_ref_input_index: 0,
    state_queue_input_index: 1,
    state_queue_output_index,
    state_queue_mint_ref_script_input_index: 0,
    availability_mint_redeemer_index: 0,
  }
  let da_redeemer_data: Data = da_redeemer
  let bond_asset_name = availability.da_bond_asset_name_v1(outref(0))
  Transaction {
    ..transaction.placeholder,
    inputs: [
      attestation_input(owner, deployment_identity),
      state_queue_input_with_status(availability.Unattested),
    ],
    outputs: [
      minted_bond_output(owner, deployment_identity),
      node_output(availability.Attested { da_bond_asset_name: bond_asset_name }),
    ],
    reference_inputs: [hub_reference_input()],
    mint: assets.from_asset(availability_policy, bond_asset_name, 1)
      |> assets.add(da_policy, da_attestation_asset_name(), -1),
    redeemers: [
      Pair(Mint(da_policy), da_redeemer_data),
      Pair(Mint(availability_policy), availability_redeemer_data),
    ],
  }
}

fn receipt_input(claimant: ByteArray, lovelace: Int) -> Input {
  let datum: availability.TrancheDatumV1 = availability.Receipt {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor: descriptor(),
    terminal_accumulator: terminal,
    terminal_carrier_output_index: 3,
    challenger: claimant,
  }
  Input {
    output_reference: outref(2),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace)
        |> assets.add(availability_policy, tranche_asset_name(), 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn carrier_input(lovelace: Int) -> Input {
  let datum: availability.PublicationDatumV1 = availability.PublicationDatumV1 {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    tranche_index: 0,
    chunk_index: 0,
    chunk_offset: 0,
    chunk_byte_length: 1,
    chunk_hash: crypto.blake2b_256(#"01"),
    chunk_frontier: validation_merkle_v1.build_frontier(
      [availability.chunk_leaf_hash_v1(0, 0, 0, 1, crypto.blake2b_256(#"01"))],
    ).peaks,
    chunk_siblings: [],
    previous_accumulator: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    next_accumulator: terminal,
    chunk: #"01",
  }
  Input {
    output_reference: outref(3),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn terminal_accumulator_input(
  committed: availability.CommitmentV1,
  next_tranche_index: Int,
  has_timed_out_tranche: Bool,
  remaining_lovelace: Int,
  response_deadline: Int,
  output_reference_index: Int,
) -> Input {
  let datum: availability.TerminalAccumulatorDatumV1 = availability.TerminalAccumulatorDatumV1 {
    deployment_identity: committed.deployment_identity,
    header_hash: committed.header_hash,
    challenge_asset_name: challenge_asset_name(),
    next_tranche_index,
    folded_terminal_accumulator: terminal,
    has_timed_out_tranche,
    response_deadline,
    challenger,
    remaining_challenger_lovelace: remaining_lovelace,
  }
  Input {
    output_reference: outref(output_reference_index),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(remaining_lovelace)
        |> assets.add(availability_policy, terminal_accumulator_asset_name(), 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn terminal_accumulator_output(
  committed: availability.CommitmentV1,
  next_tranche_index: Int,
  folded_terminal_accumulator: ByteArray,
  has_timed_out_tranche: Bool,
  remaining_lovelace: Int,
  response_deadline: Int,
) -> Output {
  let datum: availability.TerminalAccumulatorDatumV1 = availability.TerminalAccumulatorDatumV1 {
    deployment_identity: committed.deployment_identity,
    header_hash: committed.header_hash,
    challenge_asset_name: challenge_asset_name(),
    next_tranche_index,
    folded_terminal_accumulator,
    has_timed_out_tranche,
    response_deadline,
    challenger,
    remaining_challenger_lovelace: remaining_lovelace,
  }
  Output {
    address: address.from_script(availability_policy),
    value: assets.from_lovelace(remaining_lovelace)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn active_tranche_input(lovelace: Int) -> Input {
  let datum: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor: descriptor(),
    next_offset: 0,
    accumulator: availability.tranche_start_accumulator_v1(
      hub_policy,
      header_hash(),
      descriptor(),
    ),
    latest_carrier_output_index: None,
    response_deadline: availability.small_response_window_ms_v1,
    challenger,
  }
  Input {
    output_reference: outref(2),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace)
        |> assets.add(availability_policy, tranche_asset_name(), 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn settlement_redeemer(
  carrier_input_index: Option<Int>,
) -> availability.MintRedeemerV1 {
  availability.SettleTranche {
    bond_ref_input_index: 0,
    terminal_accumulator_input_index: 0,
    terminal_accumulator_output_index: 0,
    tranche_input_index: 1,
    carrier_input_index,
  }
}

fn receipt_settlement_transaction(fee: Int) -> Transaction {
  expect Some(thread_lovelace) = availability.tranche_initial_lovelace_v1(
    commitment(bond_owner),
    parameters(),
    0,
  )
  expect Some(terminal_lovelace) = availability.terminal_accumulator_initial_lovelace_v1(
    commitment(bond_owner),
    parameters(),
  )
  let carrier_lovelace = 2_000_000
  let receipt_lovelace = thread_lovelace - carrier_lovelace
  let output_lovelace = terminal_lovelace + thread_lovelace - fee
  let next_fold = availability.fold_terminal_accumulator_v1(
    terminal,
    0,
    availability.PublishedTranche { terminal_accumulator: terminal },
  )
  let redeemer = settlement_redeemer(Some(2))
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      terminal_accumulator_input(
        commitment(bond_owner),
        0,
        False,
        terminal_lovelace,
        availability.small_response_window_ms_v1,
        4,
      ),
      receipt_input(challenger, receipt_lovelace),
      carrier_input(carrier_lovelace),
    ],
    outputs: [
      terminal_accumulator_output(
        commitment(bond_owner),
        1,
        next_fold,
        False,
        output_lovelace,
        availability.small_response_window_ms_v1,
      ),
    ],
    reference_inputs: [bond_input(bond_owner, challenger)],
    mint: assets.from_asset(availability_policy, tranche_asset_name(), -1),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    fee,
    validity_range: between(0, 1_000),
  }
}

fn active_timeout_settlement_transaction(
  fee: Int,
  validity_lower_bound: Int,
) -> Transaction {
  expect Some(thread_lovelace) = availability.tranche_initial_lovelace_v1(
    commitment(bond_owner),
    parameters(),
    0,
  )
  expect Some(terminal_lovelace) = availability.terminal_accumulator_initial_lovelace_v1(
    commitment(bond_owner),
    parameters(),
  )
  let start = availability.tranche_start_accumulator_v1(
    hub_policy,
    header_hash(),
    descriptor(),
  )
  let output_lovelace = terminal_lovelace + thread_lovelace - fee
  let next_fold = availability.fold_terminal_accumulator_v1(
    terminal,
    0,
    availability.TimedOutTranche { next_offset: 0, partial_accumulator: start },
  )
  let redeemer = settlement_redeemer(None)
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      terminal_accumulator_input(
        commitment(bond_owner),
        0,
        False,
        terminal_lovelace,
        availability.small_response_window_ms_v1,
        4,
      ),
      active_tranche_input(thread_lovelace),
    ],
    outputs: [
      terminal_accumulator_output(
        commitment(bond_owner),
        1,
        next_fold,
        True,
        output_lovelace,
        availability.small_response_window_ms_v1,
      ),
    ],
    reference_inputs: [bond_input(bond_owner, challenger)],
    mint: assets.from_asset(availability_policy, tranche_asset_name(), -1),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    fee,
    validity_range: between(validity_lower_bound, validity_lower_bound + 1_000),
  }
}

fn maximum_bond_input(owner: ByteArray, claimant: ByteArray) -> Input {
  let datum: availability.BondDatumV1 = availability.ChallengedBond {
    commitment: maximum_commitment(owner),
    da_bond_asset_name,
    committee_signers_hash: committee_hash,
    attested_signers: #"",
    challenge_asset_name: challenge_asset_name(),
    challenger: claimant,
    opened_at: 0,
    response_deadline: availability.full_response_window_ms_v1,
  }
  Input {
    output_reference: outref(0),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(parameters().da_bond_lovelace)
        |> assets.add(availability_policy, da_bond_asset_name, 1)
        |> assets.add(availability_policy, challenge_asset_name(), 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn maximum_available_bond_input(commitment: availability.CommitmentV1) -> Input {
  let datum: availability.BondDatumV1 = availability.Available {
    commitment,
    da_bond_asset_name,
    committee_signers_hash: committee_hash,
    attested_signers: #"",
  }
  Input {
    output_reference: outref(0),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(parameters().da_bond_lovelace)
        |> assets.add(availability_policy, da_bond_asset_name, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn maximum_challenged_bond_output(
  commitment: availability.CommitmentV1,
) -> Output {
  let datum: availability.BondDatumV1 = availability.ChallengedBond {
    commitment,
    da_bond_asset_name,
    committee_signers_hash: committee_hash,
    attested_signers: #"",
    challenge_asset_name: challenge_asset_name(),
    challenger,
    opened_at: 0,
    response_deadline: availability.full_response_window_ms_v1,
  }
  Output {
    address: address.from_script(availability_policy),
    value: assets.from_lovelace(parameters().da_bond_lovelace)
      |> assets.add(availability_policy, da_bond_asset_name, 1)
      |> assets.add(availability_policy, challenge_asset_name(), 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn maximum_initial_tranche_output(
  committed: availability.CommitmentV1,
  descriptor: availability.TrancheDescriptorV1,
  lovelace: Int,
) -> Output {
  let datum: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: committed.deployment_identity,
    header_hash: committed.header_hash,
    challenge_asset_name: challenge_asset_name(),
    descriptor,
    next_offset: descriptor.start_offset,
    accumulator: availability.tranche_start_accumulator_v1(
      committed.deployment_identity,
      committed.header_hash,
      descriptor,
    ),
    latest_carrier_output_index: None,
    response_deadline: availability.full_response_window_ms_v1,
    challenger,
  }
  Output {
    address: address.from_script(availability_policy),
    value: assets.from_lovelace(lovelace)
      |> assets.add(
          availability_policy,
          availability.tranche_asset_name_v1(
            challenge_asset_name(),
            descriptor.tranche_index,
          ),
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn maximum_initial_tranche_outputs(
  committed: availability.CommitmentV1,
  descriptors: List<availability.TrancheDescriptorV1>,
  lovelaces: List<Int>,
) -> List<Output> {
  when Pair(descriptors, lovelaces) is {
    Pair([], []) -> []
    Pair([descriptor, ..rest], [lovelace, ..rest_lovelaces]) ->
      [
        maximum_initial_tranche_output(committed, descriptor, lovelace),
        ..maximum_initial_tranche_outputs(committed, rest, rest_lovelaces)
      ]
    _ -> fail
  }
}

fn maximum_initial_terminal_accumulator_output(
  commitment: availability.CommitmentV1,
) -> Output {
  let lovelace = availability.terminal_accumulator_reserve_lovelace_v1(
    parameters(),
  )
  terminal_accumulator_output(
    commitment,
    0,
    availability.terminal_accumulator_start_v1(
      commitment,
      challenge_asset_name(),
    ),
    False,
    lovelace,
    availability.full_response_window_ms_v1,
  )
}

fn maximum_open_mint_value() -> assets.Value {
  list.range(0, 15)
    |> list.foldl(
        assets.from_asset(availability_policy, challenge_asset_name(), 1)
          |> assets.add(
              availability_policy,
              terminal_accumulator_asset_name(),
              1,
            ),
        fn(index, value) {
          value
            |> assets.add(
                availability_policy,
                availability.tranche_asset_name_v1(
                  challenge_asset_name(),
                  index,
                ),
                1,
              )
        },
      )
}

fn maximum_open_redeemer() -> availability.MintRedeemerV1 {
  availability.OpenChallenge {
    hub_oracle_ref_input_index: 0,
    bond_input_index: 0,
    bond_output_index: 0,
    challenger_input_index: 2,
    state_queue_input_index: 1,
    state_queue_output_index: 1,
    first_tranche_output_index: 2,
    terminal_accumulator_output_index: 18,
    challenger,
  }
}

fn maximum_open_transaction() -> Transaction {
  let redeemer = maximum_open_redeemer()
  let redeemer_data: Data = redeemer
  let commitment = maximum_commitment(bond_owner)
  expect Some(initial_lovelaces) = availability.tranche_initial_lovelaces_v1(
    commitment,
    parameters(),
  )
  Transaction {
    ..transaction.placeholder,
    inputs: [
      maximum_available_bond_input(commitment),
      state_queue_input_with_status(
        availability.Attested { da_bond_asset_name },
      ),
      challenger_input(challenger),
    ],
    outputs: [
      maximum_challenged_bond_output(commitment),
      node_output(
        availability.Challenged {
          da_bond_asset_name,
          challenge_asset_name: challenge_asset_name(),
        },
      ),
      ..list.concat(
        maximum_initial_tranche_outputs(
          commitment,
          commitment.tranche_descriptors,
          initial_lovelaces,
        ),
        [maximum_initial_terminal_accumulator_output(commitment)],
      )
    ],
    reference_inputs: [hub_reference_input()],
    mint: maximum_open_mint_value(),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    extra_signatories: [challenger],
    fee: close_fee,
    validity_range: between(0, 1_000),
  }
}

fn maximum_receipt_input(
  descriptor_index: Int,
  output_reference_index: Int,
  lovelace: Int,
) -> Input {
  let descriptor = maximum_descriptor(descriptor_index)
  let datum: availability.TrancheDatumV1 = availability.Receipt {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor,
    terminal_accumulator: descriptor.terminal_accumulator,
    terminal_carrier_output_index: output_reference_index + 1,
    challenger,
  }
  Input {
    output_reference: outref(output_reference_index),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace)
        |> assets.add(
            availability_policy,
            availability.tranche_asset_name_v1(
              challenge_asset_name(),
              descriptor_index,
            ),
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn maximum_carrier_input(
  descriptor_index: Int,
  output_reference_index: Int,
  lovelace: Int,
) -> Input {
  let descriptor = maximum_descriptor(descriptor_index)
  let chunk = maximum_sized_chunk(descriptor_index)
  let datum: availability.PublicationDatumV1 = availability.PublicationDatumV1 {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    tranche_index: descriptor_index,
    chunk_index: descriptor.chunk_count - 1,
    chunk_offset: descriptor.start_offset + descriptor.byte_length - bytearray.length(
      chunk,
    ),
    chunk_byte_length: bytearray.length(chunk),
    chunk_hash: crypto.blake2b_256(chunk),
    chunk_frontier: [],
    chunk_siblings: [],
    previous_accumulator: crypto.blake2b_256(#"70726576696f7573"),
    next_accumulator: descriptor.terminal_accumulator,
    chunk,
  }
  Input {
    output_reference: outref(output_reference_index),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn maximum_sized_chunk(byte: Int) -> ByteArray {
  builtin.replicate_byte(8_192, byte)
    |> bytearray.concat(
        builtin.replicate_byte(
          parameters().response_geometry.chunk_byte_length - 8_192,
          byte,
        ),
      )
}

fn maximum_receipt_inputs(first_input_index: Int, lovelace: Int) -> List<Input> {
  list.map(
    list.range(0, 15),
    fn(index) {
      maximum_receipt_input(index, first_input_index + index, lovelace)
    },
  )
}

fn maximum_carrier_inputs(first_input_index: Int, lovelace: Int) -> List<Input> {
  list.map(
    list.range(0, 15),
    fn(index) {
      maximum_carrier_input(index, first_input_index + index, lovelace)
    },
  )
}

fn maximum_last_tranche_lovelace() -> Int {
  expect Some(lovelace) = availability.tranche_initial_lovelace_v1(
    maximum_commitment(bond_owner),
    parameters(),
    15,
  )
  lovelace
}

fn maximum_terminal_before_last_lovelace() -> Int {
  parameters().challenger_bond_lovelace - maximum_last_tranche_lovelace() - 15 * close_fee
}

fn maximum_receipt_settlement_transaction() -> Transaction {
  let terminal_lovelace = maximum_terminal_before_last_lovelace()
  let carrier_lovelace = maximum_carrier_lovelace
  let receipt_lovelace = maximum_last_tranche_lovelace() - carrier_lovelace
  let descriptor = maximum_descriptor(15)
  let next_fold = availability.fold_terminal_accumulator_v1(
    terminal,
    15,
    availability.PublishedTranche {
      terminal_accumulator: descriptor.terminal_accumulator,
    },
  )
  let redeemer = settlement_redeemer(Some(2))
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      terminal_accumulator_input(
        maximum_commitment(bond_owner),
        15,
        False,
        terminal_lovelace,
        availability.full_response_window_ms_v1,
        40,
      ),
      maximum_receipt_input(15, 41, receipt_lovelace),
      maximum_carrier_input(15, 42, carrier_lovelace),
    ],
    outputs: [
      terminal_accumulator_output(
        maximum_commitment(bond_owner),
        16,
        next_fold,
        False,
        terminal_lovelace + maximum_last_tranche_lovelace() - close_fee,
        availability.full_response_window_ms_v1,
      ),
    ],
    reference_inputs: [maximum_bond_input(bond_owner, challenger)],
    mint: assets.from_asset(
      availability_policy,
      availability.tranche_asset_name_v1(challenge_asset_name(), 15),
      -1,
    ),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    fee: close_fee,
    validity_range: between(0, 1_000),
  }
}

fn maximum_partial_chunk() -> ByteArray {
  maximum_sized_chunk(15)
}

fn maximum_partial_accumulator() -> ByteArray {
  let descriptor = maximum_descriptor(15)
  availability.tranche_step_accumulator_v1(
    hub_policy,
    header_hash(),
    15,
    descriptor.start_offset,
    maximum_partial_chunk(),
    availability.tranche_start_accumulator_v1(
      hub_policy,
      header_hash(),
      descriptor,
    ),
  )
}

fn maximum_partial_active_input(lovelace: Int) -> Input {
  let descriptor = maximum_descriptor(15)
  let datum: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor,
    next_offset: descriptor.start_offset + bytearray.length(
      maximum_partial_chunk(),
    ),
    accumulator: maximum_partial_accumulator(),
    latest_carrier_output_index: Some(42),
    response_deadline: availability.full_response_window_ms_v1,
    challenger,
  }
  Input {
    output_reference: outref(41),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace)
        |> assets.add(
            availability_policy,
            availability.tranche_asset_name_v1(challenge_asset_name(), 15),
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn maximum_partial_carrier_input(lovelace: Int) -> Input {
  let descriptor = maximum_descriptor(15)
  let chunk = maximum_partial_chunk()
  let start = availability.tranche_start_accumulator_v1(
    hub_policy,
    header_hash(),
    descriptor,
  )
  let datum: availability.PublicationDatumV1 = availability.PublicationDatumV1 {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    tranche_index: 15,
    chunk_index: 0,
    chunk_offset: descriptor.start_offset,
    chunk_byte_length: bytearray.length(chunk),
    chunk_hash: crypto.blake2b_256(chunk),
    chunk_frontier: [],
    chunk_siblings: [],
    previous_accumulator: start,
    next_accumulator: maximum_partial_accumulator(),
    chunk,
  }
  Input {
    output_reference: outref(42),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(lovelace),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn maximum_partial_timeout_settlement_transaction() -> Transaction {
  let terminal_lovelace = maximum_terminal_before_last_lovelace()
  let carrier_lovelace = maximum_carrier_lovelace
  let publication_fee = close_fee
  let thread_lovelace = maximum_last_tranche_lovelace() - carrier_lovelace - publication_fee
  let descriptor = maximum_descriptor(15)
  let next_fold = availability.fold_terminal_accumulator_v1(
    terminal,
    15,
    availability.TimedOutTranche {
      next_offset: descriptor.start_offset + bytearray.length(
        maximum_partial_chunk(),
      ),
      partial_accumulator: maximum_partial_accumulator(),
    },
  )
  let redeemer = settlement_redeemer(Some(2))
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      terminal_accumulator_input(
        maximum_commitment(bond_owner),
        15,
        False,
        terminal_lovelace,
        availability.full_response_window_ms_v1,
        40,
      ),
      maximum_partial_active_input(thread_lovelace),
      maximum_partial_carrier_input(carrier_lovelace),
    ],
    outputs: [
      terminal_accumulator_output(
        maximum_commitment(bond_owner),
        16,
        next_fold,
        True,
        terminal_lovelace + maximum_last_tranche_lovelace() - publication_fee - close_fee,
        availability.full_response_window_ms_v1,
      ),
    ],
    reference_inputs: [maximum_bond_input(bond_owner, challenger)],
    mint: assets.from_asset(
      availability_policy,
      availability.tranche_asset_name_v1(challenge_asset_name(), 15),
      -1,
    ),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    fee: close_fee,
    validity_range: between(
      availability.full_response_window_ms_v1,
      availability.full_response_window_ms_v1 + 1_000,
    ),
  }
}

fn maximum_publication_parameters() -> availability.ParametersV1 {
  availability.ParametersV1 {
    ..parameters(),
    response_geometry: availability.ResponseGeometryV1 {
      chunk_byte_length: 14_020,
      tranche_byte_length: 4 * 1024 * 1024,
      max_tranche_count: 16,
    },
  }
}

fn maximum_publication_frontier() -> List<validation_merkle_v1.FrontierPeak> {
  [
    validation_merkle_v1.FrontierPeak {
      height: 2,
      hash: #"1a916a0649ad791a11e9e723fea7da14b128fee860cd129d772f09272465e9ad",
    },
    validation_merkle_v1.FrontierPeak {
      height: 3,
      hash: #"02cf6052bcd6b453f2b27afa65bd4545cc81fb73d3b452ef4c21fa3400fa58e5",
    },
    validation_merkle_v1.FrontierPeak {
      height: 5,
      hash: #"e0c5aceb218e4482322bc4e2d2164261295f65cfd31d61469cf1f08fc3ae66b7",
    },
    validation_merkle_v1.FrontierPeak {
      height: 8,
      hash: #"90019f795c3b49fb01f60fd89a3b82c54aa1f939d442bbd3c3ea8b425019fefa",
    },
  ]
}

fn maximum_publication_siblings() -> List<ByteArray> {
  [
    #"cd89e524959690a742777d6fcfee00b67f49680d7db88f11f0b50d544685e8c3",
    #"b13af5471d8dd476547a5db0d45e60e9d7adfb8e3d92129b3b7173173480f5a2",
    #"356b52f863d490ab79e3f11c9c09f2945dc137b25ee3b1098c78ebd48181753d",
    #"6561a840018eec9cfb52705a20982d5f501ef5531c883ba21c9ba1f625a7ed81",
    #"0337d34252a47edf2ff04623473692a90255a79662d460de9f8be66524249450",
    #"b08429687e352fa2c76576b98d1a27fe8c4e6e28ed8eec75fe284dc5e843a054",
    #"88a59d200f5edb186dd53d8d60cdbcb170f8a99774dfb2a198f48d5ede33dd35",
    #"a6adec45f394267b46affeb0180a47a33ca2b7618304d98d61c7d71eaaf0b732",
  ]
}

fn maximum_publication_descriptor() -> availability.TrancheDescriptorV1 {
  availability.TrancheDescriptorV1 {
    tranche_index: 0,
    start_offset: 0,
    byte_length: 4 * 1024 * 1024,
    chunk_count: ( 4 * 1024 * 1024 + 14_020 - 1 ) / 14_020,
    chunk_commitment: #"406476e3fcfcc50f07f11bb09b9d59bf4101c258327cb8b7ab116757338edba5",
    terminal_accumulator: terminal,
  }
}

fn maximum_publication_commitment() -> availability.CommitmentV1 {
  let descriptor = maximum_publication_descriptor()
  availability.CommitmentV1 {
    version: availability.commitment_version_v1,
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    payload_byte_length: descriptor.byte_length,
    response_geometry: maximum_publication_parameters().response_geometry,
    tranche_descriptors: [descriptor],
    bond_owner,
  }
}

fn maximum_publication_chunk() -> ByteArray {
  builtin.replicate_byte(8_192, 42)
    |> bytearray.concat(builtin.replicate_byte(14_020 - 8_192, 42))
}

fn maximum_publication_transaction() -> Transaction {
  let descriptor = maximum_publication_descriptor()
  let committed = maximum_publication_commitment()
  let chunk = maximum_publication_chunk()
  let start = availability.tranche_start_accumulator_v1(
    hub_policy,
    header_hash(),
    descriptor,
  )
  let next = availability.tranche_step_accumulator_v1(
    hub_policy,
    header_hash(),
    0,
    0,
    chunk,
    start,
  )
  let current_datum: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor,
    next_offset: 0,
    accumulator: start,
    latest_carrier_output_index: None,
    response_deadline: availability.full_response_window_ms_v1,
    challenger,
  }
  let publication: availability.PublicationDatumV1 = availability.PublicationDatumV1 {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    tranche_index: 0,
    chunk_index: 0,
    chunk_offset: 0,
    chunk_byte_length: bytearray.length(chunk),
    chunk_hash: crypto.blake2b_256(chunk),
    chunk_frontier: maximum_publication_frontier(),
    chunk_siblings: maximum_publication_siblings(),
    previous_accumulator: start,
    next_accumulator: next,
    chunk,
  }
  let next_datum: availability.TrancheDatumV1 = availability.Active {
    deployment_identity: hub_policy,
    header_hash: header_hash(),
    challenge_asset_name: challenge_asset_name(),
    descriptor,
    next_offset: bytearray.length(chunk),
    accumulator: next,
    latest_carrier_output_index: Some(1),
    response_deadline: availability.full_response_window_ms_v1,
    challenger,
  }
  let thread_lovelace = 20_000_000
  let carrier_lovelace = 2_000_000
  let input = Input {
    output_reference: outref(0),
    output: Output {
      address: address.from_script(availability_policy),
      value: assets.from_lovelace(thread_lovelace)
        |> assets.add(availability_policy, tranche_asset_name(), 1),
      datum: InlineDatum(current_datum),
      reference_script: None,
    },
  }
  let redeemer: availability.SpendRedeemerV1 = availability.AdvanceTranche {
    thread_output_index: 0,
    carrier_output_index: 1,
    m_previous_carrier_input_index: None,
  }
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [input],
    outputs: [
      Output {
        address: address.from_script(availability_policy),
        value: assets.from_lovelace(
          thread_lovelace - carrier_lovelace - close_fee,
        )
          |> assets.add(availability_policy, tranche_asset_name(), 1),
        datum: InlineDatum(next_datum),
        reference_script: None,
      },
      Output {
        address: address.from_script(availability_policy),
        value: assets.from_lovelace(carrier_lovelace),
        datum: InlineDatum(publication),
        reference_script: None,
      },
    ],
    redeemers: [Pair(Spend(input.output_reference), redeemer_data)],
    fee: close_fee,
    validity_range: between(0, 1_000),
  }
}

fn validates_maximum_publication(tx: Transaction) -> Bool {
  expect [input] = tx.inputs
  expect InlineDatum(datum) = input.output.datum
  availability_validator.availability_challenge.spend(
    hub_policy,
    maximum_publication_parameters(),
    Some(datum),
    availability.AdvanceTranche {
      thread_output_index: 0,
      carrier_output_index: 1,
      m_previous_carrier_input_index: None,
    },
    input.output_reference,
    tx,
  )
}

fn maximum_burn_value() -> assets.Value {
  list.range(0, 15)
    |> list.foldl(
        assets.from_asset(availability_policy, da_bond_asset_name, -1)
          |> assets.add(availability_policy, challenge_asset_name(), -1),
        fn(index, value) {
          value
            |> assets.add(
                availability_policy,
                availability.tranche_asset_name_v1(
                  challenge_asset_name(),
                  index,
                ),
                -1,
              )
        },
      )
}

const maximum_thread_lovelace = 10_000_000

const maximum_carrier_lovelace = 2_000_000

fn maximum_challenger_component_lovelace() -> Int {
  16 * ( maximum_thread_lovelace + maximum_carrier_lovelace )
}

fn hub_reference_input() -> Input {
  let script_address = address.from_script(miscellaneous_policy)
  let datum: hub.Datum = hub.Datum {
    registered_operators: miscellaneous_policy,
    active_operators: miscellaneous_policy,
    retired_operators: miscellaneous_policy,
    scheduler: miscellaneous_policy,
    state_queue: state_queue_policy,
    fraud_proof_catalogue: miscellaneous_policy,
    fraud_proof: miscellaneous_policy,
    deposit: miscellaneous_policy,
    withdrawal: miscellaneous_policy,
    tx_order: miscellaneous_policy,
    settlement: miscellaneous_policy,
    payout: miscellaneous_policy,
    registered_operators_addr: script_address,
    active_operators_addr: script_address,
    retired_operators_addr: script_address,
    scheduler_addr: script_address,
    state_queue_addr: address.from_script(state_queue_policy),
    fraud_proof_catalogue_addr: script_address,
    fraud_proof_addr: script_address,
    deposit_addr: script_address,
    withdrawal_addr: script_address,
    tx_order_addr: script_address,
    settlement_addr: script_address,
    reserve_addr: script_address,
    payout_addr: script_address,
    reserve_observer: miscellaneous_policy,
  }
  Input {
    output_reference: outref(9),
    output: Output {
      address: address.from_script(hub_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(hub_policy, hub.asset_name, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn enterprise_output(owner: ByteArray, lovelace: Int) -> Output {
  Output {
    address: address.from_verification_key(owner),
    value: assets.from_lovelace(lovelace),
    datum: NoDatum,
    reference_script: None,
  }
}

fn close_transaction(
  owner: ByteArray,
  claimant: ByteArray,
  receipt_lovelace: Int,
  carrier_lovelace: Int,
  da_refund_output_index: Int,
  challenger_refund_output_index: Int,
) -> Transaction {
  let challenger_component = receipt_lovelace + carrier_lovelace
  let redeemer: availability.MintRedeemerV1 = availability.CloseChallenge {
    hub_oracle_ref_input_index: 0,
    bond_input_index: 0,
    terminal_accumulator_input_index: 2,
    state_queue_input_index: 1,
    state_queue_output_index: 0,
    da_refund_output_index,
    challenger_refund_output_index,
  }
  let redeemer_data: Data = redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      bond_input(owner, claimant),
      state_queue_input(),
      terminal_accumulator_input(
        commitment(owner),
        1,
        False,
        challenger_component,
        availability.small_response_window_ms_v1,
        3,
      ),
    ],
    outputs: [
      node_output(
        availability.Published {
          terminal_commitment: availability.published_terminal_commitment_v1(
            commitment(owner),
          ),
        },
      ),
      enterprise_output(owner, parameters().da_bond_lovelace),
      enterprise_output(claimant, challenger_component - close_fee),
    ],
    reference_inputs: [hub_reference_input()],
    mint: assets.from_asset(availability_policy, da_bond_asset_name, -1)
      |> assets.add(availability_policy, challenge_asset_name(), -1)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), -1),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    fee: close_fee,
    validity_range: between(0, 1_000),
  }
}

fn maximum_close_redeemer() -> availability.MintRedeemerV1 {
  availability.CloseChallenge {
    hub_oracle_ref_input_index: 0,
    bond_input_index: 0,
    terminal_accumulator_input_index: 2,
    state_queue_input_index: 1,
    state_queue_output_index: 0,
    da_refund_output_index: 1,
    challenger_refund_output_index: 2,
  }
}

fn maximum_close_transaction() -> Transaction {
  let redeemer = maximum_close_redeemer()
  let redeemer_data: Data = redeemer
  let remaining = maximum_challenger_component_lovelace()
  Transaction {
    ..transaction.placeholder,
    inputs: [
      maximum_bond_input(bond_owner, challenger),
      state_queue_input(),
      terminal_accumulator_input(
        maximum_commitment(bond_owner),
        16,
        False,
        remaining,
        availability.full_response_window_ms_v1,
        40,
      ),
    ],
    outputs: [
      node_output(
        availability.Published {
          terminal_commitment: availability.published_terminal_commitment_v1(
            maximum_commitment(bond_owner),
          ),
        },
      ),
      enterprise_output(bond_owner, parameters().da_bond_lovelace),
      enterprise_output(
        challenger,
        maximum_challenger_component_lovelace() - close_fee,
      ),
    ],
    reference_inputs: [hub_reference_input()],
    mint: assets.from_asset(availability_policy, da_bond_asset_name, -1)
      |> assets.add(availability_policy, challenge_asset_name(), -1)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), -1),
    redeemers: [Pair(Mint(availability_policy), redeemer_data)],
    fee: close_fee,
    validity_range: between(0, 1_000),
  }
}

fn validates_close(
  tx: Transaction,
  redeemer: availability.MintRedeemerV1,
) -> Bool {
  availability_validator.availability_challenge.mint(
    hub_policy,
    parameters(),
    redeemer,
    availability_policy,
    tx,
  )
}

fn validates_coordinate_input(
  tx: Transaction,
  input_index: Int,
  mint_redeemer_index: Int,
) -> Bool {
  expect Some(input) = list.at(tx.inputs, input_index)
  expect InlineDatum(datum) = input.output.datum
  availability_validator.availability_challenge.spend(
    hub_policy,
    parameters(),
    Some(datum),
    availability.Coordinate { mint_redeemer_index },
    input.output_reference,
    tx,
  )
}

fn validates_coordinate_inputs(
  tx: Transaction,
  indices: List<Int>,
  mint_redeemer_index: Int,
) -> Bool {
  when indices is {
    [] -> True
    [index, ..rest] ->
      validates_coordinate_input(tx, index, mint_redeemer_index) && validates_coordinate_inputs(
        tx,
        rest,
        mint_redeemer_index,
      )
  }
}

fn close_redeemer(
  da_refund_output_index: Int,
  challenger_refund_output_index: Int,
) -> availability.MintRedeemerV1 {
  availability.CloseChallenge {
    hub_oracle_ref_input_index: 0,
    bond_input_index: 0,
    terminal_accumulator_input_index: 2,
    state_queue_input_index: 1,
    state_queue_output_index: 0,
    da_refund_output_index,
    challenger_refund_output_index,
  }
}

fn timeout_redeemer(
  state_queue_mint_redeemer_index: Int,
  da_slash_output_index: Int,
  challenger_refund_output_index: Int,
) -> availability.MintRedeemerV1 {
  availability.TimeoutChallenge {
    hub_oracle_ref_input_index: 0,
    bond_input_index: 0,
    terminal_accumulator_input_index: 1,
    state_queue_mint_redeemer_index,
    da_slash_output_index,
    challenger_refund_output_index,
  }
}

fn timeout_state_queue_redeemer(
  bound_challenge_asset_name: ByteArray,
) -> state_queue.MintRedeemer {
  state_queue.RemoveUnavailableBlockAfterTimeout {
    yield_to_ref_input_index: 0,
    unavailable_header_hash: header_hash(),
    challenge_asset_name: bound_challenge_asset_name,
    removal_approach: state_queue.PruneTimedOutBlockDescendant {
      confirmed_state_ref_input_index: 0,
      timed_out_node_input_outref: outref(4),
      timed_out_node_output_index: 0,
    },
  }
}

fn timeout_transaction(
  bound_challenge_asset_name: ByteArray,
  fee: Int,
  da_slash_output_index: Int,
  challenger_refund_output_index: Int,
) -> Transaction {
  let receipt_lovelace = 6_000_000_000
  let carrier_lovelace = 4_000_000_000
  let redeemer = timeout_redeemer(
    0,
    da_slash_output_index,
    challenger_refund_output_index,
  )
  let redeemer_data: Data = redeemer
  let state_queue_redeemer_data: Data = timeout_state_queue_redeemer(
    bound_challenge_asset_name,
  )
  Transaction {
    ..transaction.placeholder,
    inputs: [
      bond_input(bond_owner, challenger),
      terminal_accumulator_input(
        commitment(bond_owner),
        1,
        True,
        receipt_lovelace + carrier_lovelace,
        availability.small_response_window_ms_v1,
        3,
      ),
    ],
    outputs: [
      enterprise_output(challenger, parameters().da_bond_lovelace),
      enterprise_output(challenger, receipt_lovelace + carrier_lovelace - fee),
    ],
    reference_inputs: [hub_reference_input()],
    mint: assets.from_asset(availability_policy, da_bond_asset_name, -1)
      |> assets.add(availability_policy, challenge_asset_name(), -1)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), -1),
    redeemers: [
      Pair(Mint(state_queue_policy), state_queue_redeemer_data),
      Pair(Mint(availability_policy), redeemer_data),
    ],
    fee,
    validity_range: between(
      availability.small_response_window_ms_v1,
      availability.small_response_window_ms_v1 + 1_000,
    ),
  }
}

fn maximum_timeout_redeemer() -> availability.MintRedeemerV1 {
  availability.TimeoutChallenge {
    hub_oracle_ref_input_index: 0,
    bond_input_index: 0,
    terminal_accumulator_input_index: 1,
    state_queue_mint_redeemer_index: 0,
    da_slash_output_index: 0,
    challenger_refund_output_index: 1,
  }
}

fn maximum_timeout_transaction() -> Transaction {
  let redeemer = maximum_timeout_redeemer()
  let redeemer_data: Data = redeemer
  let state_queue_redeemer_data: Data = timeout_state_queue_redeemer(
    challenge_asset_name(),
  )
  let remaining = maximum_challenger_component_lovelace()
  Transaction {
    ..transaction.placeholder,
    inputs: [
      maximum_bond_input(bond_owner, challenger),
      terminal_accumulator_input(
        maximum_commitment(bond_owner),
        16,
        True,
        remaining,
        availability.full_response_window_ms_v1,
        40,
      ),
    ],
    outputs: [
      enterprise_output(challenger, parameters().da_bond_lovelace),
      enterprise_output(
        challenger,
        maximum_challenger_component_lovelace() - close_fee,
      ),
    ],
    reference_inputs: [hub_reference_input()],
    mint: assets.from_asset(availability_policy, da_bond_asset_name, -1)
      |> assets.add(availability_policy, challenge_asset_name(), -1)
      |> assets.add(availability_policy, terminal_accumulator_asset_name(), -1),
    redeemers: [
      Pair(Mint(state_queue_policy), state_queue_redeemer_data),
      Pair(Mint(availability_policy), redeemer_data),
    ],
    fee: close_fee,
    validity_range: between(
      availability.full_response_window_ms_v1,
      availability.full_response_window_ms_v1 + 1_000,
    ),
  }
}

test q58_close_accepts_exact_distinct_terminal_outputs() {
  let tx = close_transaction(
    bond_owner,
    challenger,
    6_000_000_000,
    4_000_000_000,
    1,
    2,
  )
  and {
    validates_close(tx, close_redeemer(1, 2)),
    validates_coordinate_inputs(tx, [0, 2], 0),
  }
}

test q58_close_rejects_aliased_da_and_challenger_refunds() fail {
  // Make the two expected amounts and credentials coincide. Without the
  // explicit index inequality, output 1 satisfies both named refunds while
  // output 2 can redirect the remaining value.
  validates_close(
    close_transaction(
      bond_owner,
      bond_owner,
      6_000_000_000,
      4_000_000_100,
      1,
      1,
    ),
    close_redeemer(1, 1),
  )
}

test q58_open_accepts_exact_distinct_inputs_outputs_and_signer() {
  let redeemer = open_redeemer(0, 1, challenger)
  let tx = open_transaction(0, 1, challenger, challenger)
  and {
    validates_close(tx, redeemer),
    validates_coordinate_inputs(tx, [0], 0),
  }
}

test q58_open_rejects_missing_challenger_signature() fail {
  let redeemer = open_redeemer(0, 1, challenger)
  validates_close(open_transaction(0, 1, challenger, bond_owner), redeemer)
}

test q58_open_rejects_aliased_bond_and_state_queue_outputs() fail {
  let redeemer = open_redeemer(0, 0, challenger)
  validates_close(open_transaction(0, 0, challenger, challenger), redeemer)
}

test q58_settle_published_tranche_accepts_exact_thread_and_carrier() {
  let tx = receipt_settlement_transaction(close_fee)
  and {
    validates_close(tx, settlement_redeemer(Some(2))),
    validates_coordinate_inputs(tx, [0, 1, 2], 0),
  }
}

test q58_settle_timed_out_unpublished_tranche_accepts_without_carrier() {
  let tx = active_timeout_settlement_transaction(
    close_fee,
    availability.small_response_window_ms_v1,
  )
  and {
    validates_close(tx, settlement_redeemer(None)),
    validates_coordinate_inputs(tx, [0, 1], 0),
  }
}

test q58_settle_rejects_substituted_terminal_carrier_outref() fail {
  let base = receipt_settlement_transaction(close_fee)
  expect [terminal_input, receipt, carrier] = base.inputs
  let substituted_carrier = Input { ..carrier, output_reference: outref(8) }
  let tx = Transaction {
    ..base,
    inputs: [terminal_input, receipt, substituted_carrier],
  }
  validates_close(tx, settlement_redeemer(Some(2)))
}

test q58_settle_rejects_premature_active_timeout() fail {
  let tx = active_timeout_settlement_transaction(
    close_fee,
    availability.small_response_window_ms_v1 - 1_000,
  )
  validates_close(tx, settlement_redeemer(None))
}

test q58_settle_rejects_excessive_fee() fail {
  let fee = parameters().max_settlement_fee_lovelace + 1
  validates_close(
    receipt_settlement_transaction(fee),
    settlement_redeemer(Some(2)),
  )
}

test q58_settle_rejects_aliased_terminal_and_tranche_roles() fail {
  let redeemer: availability.MintRedeemerV1 = availability.SettleTranche {
    bond_ref_input_index: 0,
    terminal_accumulator_input_index: 0,
    terminal_accumulator_output_index: 0,
    tranche_input_index: 0,
    carrier_input_index: Some(2),
  }
  validates_close(receipt_settlement_transaction(close_fee), redeemer)
}

test q58_settle_rejects_batched_second_tranche_fee_charge() fail {
  let base = receipt_settlement_transaction(close_fee)
  let extra = Input {
    ..receipt_input(challenger, 2_000_000),
    output_reference: outref(8),
  }
  let tx = Transaction { ..base, inputs: list.concat(base.inputs, [extra]) }
  validates_close(tx, settlement_redeemer(Some(2)))
}

test q58_maximum_16_tranche_published_settlement_applied_topology_fits() {
  let tx = maximum_receipt_settlement_transaction()
  and {
    validates_close(tx, settlement_redeemer(Some(2))),
    validates_coordinate_inputs(tx, [0, 1, 2], 0),
  }
}

test q58_maximum_16_tranche_partial_timeout_settlement_applied_topology_fits() {
  let tx = maximum_partial_timeout_settlement_transaction()
  and {
    validates_close(tx, settlement_redeemer(Some(2))),
    validates_coordinate_inputs(tx, [0, 1, 2], 0),
  }
}

test q58_maximum_14020_byte_publication_with_merkle_proof_applied_fits() {
  validates_maximum_publication(maximum_publication_transaction())
}

test q58_maximum_16_tranche_open_applied_topology_fits() {
  let tx = maximum_open_transaction()
  and {
    list.length(tx.outputs) == 19,
    validates_close(tx, maximum_open_redeemer()),
    validates_coordinate_inputs(tx, [0], 0),
  }
}

test q58_maximum_16_tranche_open_mint_policy_measurement() {
  validates_close(maximum_open_transaction(), maximum_open_redeemer())
}

test q58_maximum_publication_first_leaf_cross_language_vector_matches() {
  let chunk = maximum_publication_chunk()
  availability.chunk_leaf_hash_v1(
    0,
    0,
    0,
    bytearray.length(chunk),
    crypto.blake2b_256(chunk),
  ) == #"2b89672abc40b0ba8c2d4db8cd236b2ca6bda31ab8db93517f88e88dff114675"
}

test q58_mint_bond_accepts_exact_attestation_and_queue_transition() {
  let redeemer = mint_bond_redeemer(0, 1)
  validates_close(mint_bond_transaction(bond_owner, hub_policy, 0, 1), redeemer)
}

test q58_mint_bond_accepts_wallet_fee_input_and_change_output() {
  let redeemer = mint_bond_redeemer(0, 1)
  let tx = mint_bond_transaction(bond_owner, hub_policy, 0, 1)
  validates_close(
    Transaction {
      ..tx,
      inputs: list.concat(tx.inputs, [challenger_input(bond_owner)]),
      outputs: list.concat(tx.outputs, [challenger_input(bond_owner).output]),
    },
    redeemer,
  )
}

test q58_mint_bond_rejects_substituted_deployment_identity() fail {
  let redeemer = mint_bond_redeemer(0, 1)
  validates_close(
    mint_bond_transaction(bond_owner, miscellaneous_policy, 0, 1),
    redeemer,
  )
}

test q58_mint_bond_rejects_aliased_bond_and_queue_outputs() fail {
  let redeemer = mint_bond_redeemer(0, 0)
  validates_close(mint_bond_transaction(bond_owner, hub_policy, 0, 0), redeemer)
}

test q58_timeout_accepts_exact_challenger_slash_and_unused_bond_refund() {
  let redeemer = timeout_redeemer(0, 0, 1)
  let tx = timeout_transaction(challenge_asset_name(), close_fee, 0, 1)
  and {
    validates_close(tx, redeemer),
    validates_coordinate_inputs(tx, [0, 1], 1),
  }
}

test q58_coordinate_rejects_undeclared_availability_input() fail {
  let base = close_transaction(
    bond_owner,
    challenger,
    6_000_000_000,
    4_000_000_000,
    1,
    2,
  )
  let unrelated = Input {
    ..receipt_input(challenger, 2_000_000),
    output_reference: outref(8),
  }
  let tx = Transaction { ..base, inputs: list.concat(base.inputs, [unrelated]) }
  validates_coordinate_input(tx, 3, 0)
}

test q58_timeout_rejects_aliased_challenger_outputs() fail {
  let redeemer = timeout_redeemer(0, 0, 0)
  validates_close(
    timeout_transaction(challenge_asset_name(), close_fee, 0, 0),
    redeemer,
  )
}

test q58_timeout_rejects_excessive_fee() fail {
  let fee = parameters().max_timeout_fee_lovelace + 1
  let redeemer = timeout_redeemer(0, 0, 1)
  validates_close(
    timeout_transaction(challenge_asset_name(), fee, 0, 1),
    redeemer,
  )
}

test q58_timeout_rejects_premature_validity() fail {
  let redeemer = timeout_redeemer(0, 0, 1)
  let base = timeout_transaction(challenge_asset_name(), close_fee, 0, 1)
  let tx = Transaction {
    ..base,
    validity_range: between(0, availability.small_response_window_ms_v1 - 1),
  }
  validates_close(tx, redeemer)
}

test q58_timeout_rejects_wrong_state_queue_challenge_identity() fail {
  let redeemer = timeout_redeemer(0, 0, 1)
  validates_close(timeout_transaction(terminal, close_fee, 0, 1), redeemer)
}

test q58_timeout_rejects_aliased_bond_and_terminal_input_index() fail {
  let redeemer: availability.MintRedeemerV1 = availability.TimeoutChallenge {
    hub_oracle_ref_input_index: 0,
    bond_input_index: 0,
    terminal_accumulator_input_index: 0,
    state_queue_mint_redeemer_index: 0,
    da_slash_output_index: 0,
    challenger_refund_output_index: 1,
  }
  validates_close(
    timeout_transaction(challenge_asset_name(), close_fee, 0, 1),
    redeemer,
  )
}

test q58_timeout_rejects_third_challenger_output() fail {
  let redeemer = timeout_redeemer(0, 0, 1)
  let base = timeout_transaction(challenge_asset_name(), close_fee, 0, 1)
  let extended_outputs = list.concat(
    base.outputs,
    [enterprise_output(challenger, 1)],
  )
  let tx = Transaction { ..base, outputs: extended_outputs }
  validates_close(tx, redeemer)
}

test q58_maximum_16_tranche_final_close_applied_topology_fits() {
  let tx = maximum_close_transaction()
  and {
    list.length(tx.inputs) == 3,
    validates_close(tx, maximum_close_redeemer()),
    validates_coordinate_inputs(tx, [0, 2], 0),
  }
}

test q58_maximum_16_tranche_final_close_mint_policy_measurement() {
  let tx = maximum_close_transaction()
  validates_close(tx, maximum_close_redeemer())
}

test q58_maximum_16_tranche_final_timeout_applied_topology_fits() {
  let tx = maximum_timeout_transaction()
  and {
    list.length(tx.inputs) == 2,
    validates_close(tx, maximum_timeout_redeemer()),
    validates_coordinate_inputs(tx, [0, 1], 1),
  }
}

test q58_maximum_16_tranche_final_timeout_mint_policy_measurement() {
  let tx = maximum_timeout_transaction()
  validates_close(tx, maximum_timeout_redeemer())
}
