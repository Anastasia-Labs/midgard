use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address.{Address, Script, from_script, from_verification_key}
use cardano/assets.{AssetName, PolicyId, Value, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Spend, Transaction,
}
use midgard/availability_challenge as availability
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/state_queue

fn output_at(outputs: List<Output>, index: Int) -> Output {
  outputs |> list.expect_at(index)
}

fn input_at(inputs: List<Input>, index: Int) -> Input {
  expect Some(input) = inputs |> list.at(index)
  input
}

fn script_hash(address: Address) -> PolicyId {
  expect Script(hash) = address.payment_credential
  hash
}

fn inline_datum(output: Output) -> Data {
  expect InlineDatum(data) = output.datum
  data
}

fn exact_enterprise_ada_output(
  output: Output,
  owner: ByteArray,
  lovelace: Int,
) -> Bool {
  and {
    output.address == from_verification_key(owner),
    output.value == assets.from_lovelace(lovelace),
    output.datum == NoDatum,
    output.reference_script == None,
  }
}

fn exact_script_output(output: Output, address: Address, value: Value) -> Bool {
  and {
    output.address == address,
    output.value == value,
    output.reference_script == None,
  }
}

fn mint_pairs(mint: Value, policy_id: PolicyId) -> List<Pair<AssetName, Int>> {
  mint |> assets.tokens(policy_id) |> dict.to_pairs
}

fn index_outside_contiguous_range(index: Int, first: Int, count: Int) -> Bool {
  or {
    index < first,
    index >= first + count,
  }
}

/// A challenge record authenticated by its challenge token under the
/// availability policy at the availability address. The record's lovelace is
/// exactly `challenge_record_lovelace` and it holds nothing else, so the name
/// in its datum is the token it holds.
fn authenticated_challenge_record(
  output: Output,
  own_policy_id: PolicyId,
  parameters: availability.ParametersV1,
) -> availability.ChallengeRecordV1 {
  expect record: availability.ChallengeRecordV1 = inline_datum(output)
  expect output.address == from_script(own_policy_id)
  expect
    output.value == (
      assets.from_lovelace(parameters.challenge_record_lovelace)
        |> assets.add(own_policy_id, record.challenge_asset_name, 1)
    )
  record
}

fn expected_tranche_mint_pairs(
  descriptors: List<availability.TrancheDescriptorV1>,
  challenge_asset_name: AssetName,
  index: Int,
  quantity: Int,
) -> List<Pair<AssetName, Int>> {
  when descriptors is {
    [] -> []
    [_descriptor, ..rest] ->
      [
        Pair(
          availability.tranche_asset_name_v1(challenge_asset_name, index),
          quantity,
        ),
        ..expected_tranche_mint_pairs(
          rest,
          challenge_asset_name,
          index + 1,
          quantity,
        )
      ]
  }
}

fn validate_initial_tranche_outputs(
  outputs: List<Output>,
  own_policy_id: PolicyId,
  commitment: availability.CommitmentV1,
  challenge_asset_name: AssetName,
  challenger: ByteArray,
  response_deadline: Int,
  first_output_index: Int,
  descriptor_index: Int,
  descriptors: List<availability.TrancheDescriptorV1>,
  initial_lovelaces: List<Int>,
) -> Bool {
  when Pair(descriptors, initial_lovelaces) is {
    Pair([], []) -> True
    Pair([descriptor, ..rest], [initial_lovelace, ..rest_lovelaces]) -> {
      let tranche_asset_name = availability.tranche_asset_name_v1(
        challenge_asset_name,
        descriptor_index,
      )
      let output = output_at(outputs, first_output_index + descriptor_index)
      let expected_datum: availability.TrancheDatumV1 = availability.Active {
        deployment_identity: commitment.deployment_identity,
        header_hash: commitment.header_hash,
        challenge_asset_name,
        descriptor,
        next_offset: descriptor.start_offset,
        accumulator: availability.tranche_start_accumulator_v1(
          commitment.deployment_identity,
          commitment.header_hash,
          descriptor,
        ),
        latest_carrier_output_index: None,
        response_deadline,
        challenger,
      }
      let expected_datum_data: Data = expected_datum
      and {
        descriptor.tranche_index == descriptor_index,
        exact_script_output(
          output,
          from_script(own_policy_id),
          assets.from_lovelace(initial_lovelace)
            |> assets.add(own_policy_id, tranche_asset_name, 1),
        ),
        inline_datum(output) == expected_datum_data,
        validate_initial_tranche_outputs(
          outputs,
          own_policy_id,
          commitment,
          challenge_asset_name,
          challenger,
          response_deadline,
          first_output_index,
          descriptor_index + 1,
          rest,
          rest_lovelaces,
        ),
      }
    }
    _ -> False
  }
}

fn validate_initial_terminal_accumulator_output(
  output: Output,
  own_policy_id: PolicyId,
  commitment: availability.CommitmentV1,
  challenge_asset_name: AssetName,
  challenger: ByteArray,
  response_deadline: Int,
  initial_lovelace: Int,
) -> Bool {
  let expected_datum: availability.TerminalAccumulatorDatumV1 = availability.TerminalAccumulatorDatumV1 {
    deployment_identity: commitment.deployment_identity,
    header_hash: commitment.header_hash,
    challenge_asset_name,
    next_tranche_index: 0,
    folded_terminal_accumulator: availability.terminal_accumulator_start_v1(
      commitment,
      challenge_asset_name,
    ),
    has_timed_out_tranche: False,
    response_deadline,
    challenger,
    remaining_challenger_lovelace: initial_lovelace,
  }
  let expected_datum_data: Data = expected_datum
  exact_script_output(
    output,
    from_script(own_policy_id),
    assets.from_lovelace(initial_lovelace)
      |> assets.add(
          own_policy_id,
          availability.terminal_accumulator_asset_name_v1(challenge_asset_name),
          1,
        ),
  ) && inline_datum(output) == expected_datum_data
}

pub fn validate_open_challenge(
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
  own_policy_id: PolicyId,
  tx: Transaction,
  hub_oracle_ref_input_index: Int,
  record_output_index: Int,
  challenger_input_index: Int,
  state_queue_input_index: Int,
  state_queue_output_index: Int,
  first_tranche_output_index: Int,
  terminal_accumulator_output_index: Int,
  challenger: ByteArray,
) -> Bool {
  let hub_datum = hub.get_datum(
    tx.reference_inputs,
    hub_oracle_policy_id,
    hub_oracle_ref_input_index,
  )
  let challenger_input = input_at(tx.inputs, challenger_input_index)
  let challenge_asset_name = availability.challenge_asset_name_v1(
    challenger_input.output_reference,
  )
  let record_output = output_at(tx.outputs, record_output_index)
  // The commitment preimage travels in the record output. The node input's
  // `Attested{h}` binds it: the status transition below requires
  // `commitment_hash_v1(commitment) == h`.
  expect record: availability.ChallengeRecordV1 = inline_datum(record_output)
  let commitment = record.commitment
  // The response window opens at the inclusive upper validity bound. The
  // ledger admits the open only at or before that instant, so the committee
  // always has at least the full window after the open lands. The challenger
  // chooses the lower bound freely; anchoring there would let a backdated
  // range shrink the window by up to the maximum validity range, or place the
  // deadline in the past on landing. A later upper bound only lengthens the
  // window, and by at most the maximum validity range past landing.
  let (_inclusive_lower_bound, inclusive_upper_bound) = utils.get_inclusive_bounds_of_a_short_validity_range(
    tx.validity_range,
  )
  expect Some(response_deadline) = availability.response_deadline_v1(
    commitment.payload_byte_length,
    inclusive_upper_bound,
  )
  let expected_record: availability.ChallengeRecordV1 = availability.ChallengeRecordV1 {
    commitment,
    challenge_asset_name,
    challenger,
    opened_at: inclusive_upper_bound,
    response_deadline,
  }
  let expected_record_data: Data = expected_record
  let node_end_time = state_queue.validate_da_challenge_open_transition_v1(
    tx.inputs,
    tx.outputs,
    hub_datum.state_queue,
    state_queue_input_index,
    state_queue_output_index,
    commitment.header_hash,
    availability.commitment_hash_v1(commitment),
    challenge_asset_name,
  )
  let descriptors = commitment.tranche_descriptors
  expect Some(initial_tranche_lovelaces) = availability.tranche_initial_lovelaces_v1(
    commitment,
    parameters,
  )
  let terminal_accumulator_lovelace = availability.terminal_accumulator_reserve_lovelace_v1(
    parameters,
  )
  let expected_mint_pairs = [
    Pair(challenge_asset_name, 1),
    Pair(
      availability.terminal_accumulator_asset_name_v1(challenge_asset_name),
      1,
    ),
    ..expected_tranche_mint_pairs(descriptors, challenge_asset_name, 0, 1)
  ]
  let descriptor_count = list.length(descriptors)
  and {
    list.length(tx.inputs) == 2,
    list.length(tx.outputs) == 3 + descriptor_count,
    tx.fee > 0,
    tx.fee <= parameters.max_open_fee_lovelace,
    challenger_input_index != state_queue_input_index,
    record_output_index != state_queue_output_index,
    index_outside_contiguous_range(
      record_output_index,
      first_tranche_output_index,
      descriptor_count,
    ),
    index_outside_contiguous_range(
      state_queue_output_index,
      first_tranche_output_index,
      descriptor_count,
    ),
    terminal_accumulator_output_index != record_output_index,
    terminal_accumulator_output_index != state_queue_output_index,
    index_outside_contiguous_range(
      terminal_accumulator_output_index,
      first_tranche_output_index,
      descriptor_count,
    ),
    first_tranche_output_index + descriptor_count <= list.length(tx.outputs),
    availability.commitment_is_canonical_v1(commitment, parameters),
    commitment.deployment_identity == hub_oracle_policy_id,
    inclusive_upper_bound < node_end_time + availability.da_challenge_window_ms_v1,
    utils.has_signed(challenger, tx.extra_signatories),
    challenger_input.output.address == from_verification_key(challenger),
    challenger_input.output.value == assets.from_lovelace(
      parameters.challenger_bond_lovelace + parameters.challenge_record_lovelace + tx.fee,
    ),
    challenger_input.output.datum == NoDatum,
    challenger_input.output.reference_script == None,
    mint_pairs(tx.mint, own_policy_id) == expected_mint_pairs,
    exact_script_output(
      record_output,
      from_script(own_policy_id),
      assets.from_lovelace(parameters.challenge_record_lovelace)
        |> assets.add(own_policy_id, challenge_asset_name, 1),
    ),
    inline_datum(record_output) == expected_record_data,
    validate_initial_tranche_outputs(
      tx.outputs,
      own_policy_id,
      commitment,
      challenge_asset_name,
      challenger,
      response_deadline,
      first_tranche_output_index,
      0,
      descriptors,
      initial_tranche_lovelaces,
    ),
    validate_initial_terminal_accumulator_output(
      output_at(tx.outputs, terminal_accumulator_output_index),
      own_policy_id,
      commitment,
      challenge_asset_name,
      challenger,
      response_deadline,
      terminal_accumulator_lovelace,
    ),
  }
}

fn validate_settlement_status(
  tx: Transaction,
  own_policy_id: PolicyId,
  commitment: availability.CommitmentV1,
  challenge_asset_name: AssetName,
  challenger: ByteArray,
  response_deadline: Int,
  descriptor: availability.TrancheDescriptorV1,
  thread_input_reference: OutputReference,
  thread_datum: availability.TrancheDatumV1,
  carrier_input_index: Option<Int>,
) -> Option<(availability.TrancheTerminalStatusV1, Bool, Int)> {
  when thread_datum is {
    availability.Receipt {
      deployment_identity,
      header_hash,
      challenge_asset_name: thread_challenge_asset_name,
      descriptor: thread_descriptor,
      terminal_accumulator,
      terminal_carrier_output_index,
      challenger: thread_challenger,
    } -> {
      expect Some(carrier_index) = carrier_input_index
      let carrier_input = input_at(tx.inputs, carrier_index)
      expect Some(carrier_lovelace) = timeout_carrier_lovelace(
        tx.inputs,
        own_policy_id,
        commitment,
        challenge_asset_name,
        descriptor,
        terminal_accumulator,
        descriptor.start_offset + descriptor.byte_length,
        Some(carrier_index),
      )
      if and {
        deployment_identity == commitment.deployment_identity,
        header_hash == commitment.header_hash,
        thread_challenge_asset_name == challenge_asset_name,
        thread_descriptor == descriptor,
        terminal_accumulator == descriptor.terminal_accumulator,
        carrier_input.output_reference.transaction_id == thread_input_reference.transaction_id,
        carrier_input.output_reference.output_index == terminal_carrier_output_index,
        thread_challenger == challenger,
      } {
        Some(
          (
            availability.PublishedTranche { terminal_accumulator },
            False,
            carrier_lovelace,
          ),
        )
      } else {
        None
      }
    }
    availability.Active {
      deployment_identity,
      header_hash,
      challenge_asset_name: thread_challenge_asset_name,
      descriptor: thread_descriptor,
      next_offset,
      accumulator,
      latest_carrier_output_index,
      response_deadline: thread_response_deadline,
      challenger: thread_challenger,
    } -> {
      let carrier_reference_is_valid = when latest_carrier_output_index is {
        None -> carrier_input_index == None
        Some(expected_output_index) -> {
          expect Some(carrier_index) = carrier_input_index
          let carrier_input = input_at(tx.inputs, carrier_index)
          and {
            carrier_input.output_reference.transaction_id == thread_input_reference.transaction_id,
            carrier_input.output_reference.output_index == expected_output_index,
          }
        }
      }
      expect Some(carrier_lovelace) = timeout_carrier_lovelace(
        tx.inputs,
        own_policy_id,
        commitment,
        challenge_asset_name,
        descriptor,
        accumulator,
        next_offset,
        carrier_input_index,
      )
      let inclusive_lower_bound = utils.get_inclusive_lower_bound_of_interval(
        tx.validity_range,
      )
      if and {
        inclusive_lower_bound >= response_deadline,
        deployment_identity == commitment.deployment_identity,
        header_hash == commitment.header_hash,
        thread_challenge_asset_name == challenge_asset_name,
        thread_descriptor == descriptor,
        thread_response_deadline == response_deadline,
        thread_challenger == challenger,
        carrier_reference_is_valid,
      } {
        Some(
          (
            availability.TimedOutTranche {
              next_offset,
              partial_accumulator: accumulator,
            },
            True,
            carrier_lovelace,
          ),
        )
      } else {
        None
      }
    }
  }
}

pub fn validate_settle_tranche(
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
  own_policy_id: PolicyId,
  tx: Transaction,
  record_ref_input_index: Int,
  terminal_accumulator_input_index: Int,
  terminal_accumulator_output_index: Int,
  tranche_input_index: Int,
  carrier_input_index: Option<Int>,
) -> Bool {
  let record_ref_input = input_at(tx.reference_inputs, record_ref_input_index)
  let
      availability.ChallengeRecordV1 {
        commitment,
        challenge_asset_name,
        challenger,
        response_deadline,
        ..
      }
    = authenticated_challenge_record(
    record_ref_input.output,
    own_policy_id,
    parameters,
  )
  let terminal_input = input_at(tx.inputs, terminal_accumulator_input_index)
  expect terminal_datum: availability.TerminalAccumulatorDatumV1 = inline_datum(
    terminal_input.output,
  )
  let descriptor_index = terminal_datum.next_tranche_index
  expect Some(descriptor) = list.at(
    commitment.tranche_descriptors,
    descriptor_index,
  )
  let tranche_input = input_at(tx.inputs, tranche_input_index)
  expect thread_datum: availability.TrancheDatumV1 = inline_datum(
    tranche_input.output,
  )
  expect Some((status, timed_out, carrier_lovelace)) = validate_settlement_status(
    tx,
    own_policy_id,
    commitment,
    challenge_asset_name,
    challenger,
    response_deadline,
    descriptor,
    tranche_input.output_reference,
    thread_datum,
    carrier_input_index,
  )
  let terminal_asset_name = availability.terminal_accumulator_asset_name_v1(
    challenge_asset_name,
  )
  let tranche_asset_name = availability.tranche_asset_name_v1(
    challenge_asset_name,
    descriptor_index,
  )
  let output_lovelace = terminal_datum.remaining_challenger_lovelace + assets.lovelace_of(
    tranche_input.output.value,
  ) + carrier_lovelace - tx.fee
  let expected_output_datum: availability.TerminalAccumulatorDatumV1 = availability.TerminalAccumulatorDatumV1 {
    ..terminal_datum,
    next_tranche_index: descriptor_index + 1,
    folded_terminal_accumulator: availability.fold_terminal_accumulator_v1(
      terminal_datum.folded_terminal_accumulator,
      descriptor_index,
      status,
    ),
    has_timed_out_tranche: terminal_datum.has_timed_out_tranche || timed_out,
    remaining_challenger_lovelace: output_lovelace,
  }
  let expected_output_datum_data: Data = expected_output_datum
  let output = output_at(tx.outputs, terminal_accumulator_output_index)
  and {
    availability.commitment_is_canonical_v1(commitment, parameters),
    commitment.deployment_identity == hub_oracle_policy_id,
    descriptor.tranche_index == descriptor_index,
    descriptor_index >= 0,
    descriptor_index < list.length(commitment.tranche_descriptors),
    terminal_accumulator_input_index != tranche_input_index,
    carrier_input_index != Some(terminal_accumulator_input_index),
    carrier_input_index != Some(tranche_input_index),
    list.length(tx.inputs) == if carrier_input_index == None {
      2
    } else {
      3
    },
    list.length(tx.outputs) == 1,
    tx.fee > 0,
    tx.fee <= parameters.max_settlement_fee_lovelace,
    output_lovelace > 0,
    terminal_input.output.address == from_script(own_policy_id),
    terminal_input.output.reference_script == None,
    terminal_input.output.value == (
      assets.from_lovelace(terminal_datum.remaining_challenger_lovelace)
        |> assets.add(own_policy_id, terminal_asset_name, 1)
    ),
    terminal_datum.deployment_identity == commitment.deployment_identity,
    terminal_datum.header_hash == commitment.header_hash,
    terminal_datum.challenge_asset_name == challenge_asset_name,
    terminal_datum.response_deadline == response_deadline,
    terminal_datum.challenger == challenger,
    bytearray.length(terminal_datum.folded_terminal_accumulator) == 32,
    tranche_input.output.address == from_script(own_policy_id),
    tranche_input.output.reference_script == None,
    assets.without_lovelace(tranche_input.output.value) == assets.from_asset(
      own_policy_id,
      tranche_asset_name,
      1,
    ),
    mint_pairs(tx.mint, own_policy_id) == [Pair(tranche_asset_name, -1)],
    exact_script_output(
      output,
      from_script(own_policy_id),
      assets.from_lovelace(output_lovelace)
        |> assets.add(own_policy_id, terminal_asset_name, 1),
    ),
    inline_datum(output) == expected_output_datum_data,
  }
}

pub fn validate_close_challenge(
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
  own_policy_id: PolicyId,
  tx: Transaction,
  hub_oracle_ref_input_index: Int,
  record_input_index: Int,
  terminal_accumulator_input_index: Int,
  state_queue_input_index: Int,
  state_queue_output_index: Int,
  challenger_refund_output_index: Int,
) -> Bool {
  let hub_datum = hub.get_datum(
    tx.reference_inputs,
    hub_oracle_policy_id,
    hub_oracle_ref_input_index,
  )
  let record_input = input_at(tx.inputs, record_input_index)
  let
      availability.ChallengeRecordV1 {
        commitment,
        challenge_asset_name,
        challenger,
        ..
      }
    = authenticated_challenge_record(
    record_input.output,
    own_policy_id,
    parameters,
  )
  let terminal_input = input_at(tx.inputs, terminal_accumulator_input_index)
  expect terminal_datum: availability.TerminalAccumulatorDatumV1 = inline_datum(
    terminal_input.output,
  )
  let terminal_asset_name = availability.terminal_accumulator_asset_name_v1(
    challenge_asset_name,
  )
  let expected_burn_pairs = [
    Pair(challenge_asset_name, -1),
    Pair(terminal_asset_name, -1),
  ]
  and {
    availability.commitment_is_canonical_v1(commitment, parameters),
    commitment.deployment_identity == hub_oracle_policy_id,
    tx.fee > 0,
    tx.fee <= parameters.max_close_fee_lovelace,
    list.length(tx.inputs) == 3,
    list.length(tx.outputs) == 2,
    record_input_index != state_queue_input_index,
    record_input_index != terminal_accumulator_input_index,
    state_queue_input_index != terminal_accumulator_input_index,
    challenger_refund_output_index != state_queue_output_index,
    terminal_input.output.address == from_script(own_policy_id),
    terminal_input.output.value == (
      assets.from_lovelace(terminal_datum.remaining_challenger_lovelace)
        |> assets.add(own_policy_id, terminal_asset_name, 1)
    ),
    terminal_input.output.reference_script == None,
    terminal_datum.deployment_identity == commitment.deployment_identity,
    terminal_datum.header_hash == commitment.header_hash,
    terminal_datum.challenge_asset_name == challenge_asset_name,
    terminal_datum.next_tranche_index == list.length(
      commitment.tranche_descriptors,
    ),
    terminal_datum.has_timed_out_tranche == False,
    terminal_datum.challenger == challenger,
    mint_pairs(tx.mint, own_policy_id) == expected_burn_pairs,
    state_queue.validate_da_availability_status_transition(
      tx.inputs,
      tx.outputs,
      hub_datum.state_queue,
      state_queue_input_index,
      state_queue_output_index,
      commitment.header_hash,
      availability.Challenged {
        commitment_hash: availability.commitment_hash_v1(commitment),
        challenge_asset_name,
      },
      availability.Published {
        terminal_commitment: availability.published_terminal_commitment_v1(
          commitment,
        ),
      },
    ),
    // The challenger recovers the unspent bond and the record's lovelace in
    // one output. The committee holds no per-block bond to refund.
    exact_enterprise_ada_output(
      output_at(tx.outputs, challenger_refund_output_index),
      challenger,
      terminal_datum.remaining_challenger_lovelace - tx.fee + parameters.challenge_record_lovelace,
    ),
    terminal_datum.remaining_challenger_lovelace > tx.fee,
  }
}

fn timeout_carrier_lovelace(
  inputs: List<Input>,
  own_policy_id: PolicyId,
  commitment: availability.CommitmentV1,
  challenge_asset_name: AssetName,
  descriptor: availability.TrancheDescriptorV1,
  expected_accumulator: ByteArray,
  expected_next_offset: Int,
  m_carrier_input_index: Option<Int>,
) -> Option<Int> {
  when m_carrier_input_index is {
    None ->
      if and {
        expected_next_offset == descriptor.start_offset,
        expected_accumulator == availability.tranche_start_accumulator_v1(
          commitment.deployment_identity,
          commitment.header_hash,
          descriptor,
        ),
      } {
        Some(0)
      } else {
        None
      }
    Some(carrier_input_index) -> {
      let carrier_input = input_at(inputs, carrier_input_index)
      expect publication: availability.PublicationDatumV1 = inline_datum(
        carrier_input.output,
      )
      if and {
        carrier_input.output.address == from_script(own_policy_id),
        carrier_input.output.reference_script == None,
        assets.without_lovelace(carrier_input.output.value) == assets.zero,
        publication.deployment_identity == commitment.deployment_identity,
        publication.header_hash == commitment.header_hash,
        publication.challenge_asset_name == challenge_asset_name,
        publication.tranche_index == descriptor.tranche_index,
        publication.next_accumulator == expected_accumulator,
        publication.chunk_offset + publication.chunk_byte_length == expected_next_offset,
      } {
        Some(assets.lovelace_of(carrier_input.output.value))
      } else {
        None
      }
    }
  }
}

fn validate_state_queue_timeout_binding(
  tx: Transaction,
  state_queue_policy_id: PolicyId,
  state_queue_mint_redeemer_index: Int,
  header_hash: ByteArray,
  challenge_asset_name: AssetName,
) -> Bool {
  let redeemer_data = tx.redeemers
    |> utils.get_redeemer_at(
        Mint(state_queue_policy_id),
        state_queue_mint_redeemer_index,
      )
  expect
      state_queue.RemoveUnavailableBlockAfterTimeout {
        unavailable_header_hash,
        challenge_asset_name: bound_challenge_asset_name,
        ..
      }: state_queue.MintRedeemer
    = redeemer_data
  and {
    unavailable_header_hash == header_hash,
    bound_challenge_asset_name == challenge_asset_name,
  }
}

/// The node the removal consumes, located by its block token under the state
/// queue policy, must still carry exactly the challenged status the record
/// implies: both the commitment hash and the challenge name.
fn timeout_node_status_matches(
  inputs: List<Input>,
  state_queue_policy_id: PolicyId,
  header_hash: ByteArray,
  expected_status: availability.StateQueueStatusV1,
) -> Bool {
  let node_asset_name = state_queue.block_asset_name_prefix
    |> bytearray.concat(header_hash)
  expect [node_input] = inputs
    |> list.filter(
        fn(input) {
          quantity_of(
            input.output.value,
            state_queue_policy_id,
            node_asset_name,
          ) == 1
        },
      )
  expect node_datum: state_queue.Datum = inline_datum(node_input.output)
  expect linked_list.Node { data: node } = node_datum.data
  node.da_attestation == expected_status
}

/// Interim timeout (until the pooled-bond slash lands): the expired record is
/// burned with its terminal accumulator and the challenger recovers the
/// record's lovelace with the unspent bond. Nothing here reads the pool or
/// pins a slash output, and `da_slash_output_index` is not read. The pool's
/// `Slash` arm can still co-run with this timeout, and nothing pins where its
/// taken lovelace goes; #693 binds it (decisions D1-D3).
pub fn validate_timeout_challenge(
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
  own_policy_id: PolicyId,
  tx: Transaction,
  hub_oracle_ref_input_index: Int,
  record_input_index: Int,
  terminal_accumulator_input_index: Int,
  state_queue_mint_redeemer_index: Int,
  challenger_refund_output_index: Int,
) -> Bool {
  let hub_datum = hub.get_datum(
    tx.reference_inputs,
    hub_oracle_policy_id,
    hub_oracle_ref_input_index,
  )
  let record_input = input_at(tx.inputs, record_input_index)
  let
      availability.ChallengeRecordV1 {
        commitment,
        challenge_asset_name,
        challenger,
        response_deadline,
        ..
      }
    = authenticated_challenge_record(
    record_input.output,
    own_policy_id,
    parameters,
  )
  let terminal_input = input_at(tx.inputs, terminal_accumulator_input_index)
  expect terminal_datum: availability.TerminalAccumulatorDatumV1 = inline_datum(
    terminal_input.output,
  )
  let terminal_asset_name = availability.terminal_accumulator_asset_name_v1(
    challenge_asset_name,
  )
  let expected_burn_pairs = [
    Pair(challenge_asset_name, -1),
    Pair(terminal_asset_name, -1),
  ]
  let inclusive_lower_bound = utils.get_inclusive_lower_bound_of_interval(
    tx.validity_range,
  )
  let challenger_output_count = tx.outputs
    |> list.filter(
        fn(output) { output.address == from_verification_key(challenger) },
      )
    |> list.length
  and {
    availability.commitment_is_canonical_v1(commitment, parameters),
    commitment.deployment_identity == hub_oracle_policy_id,
    bytearray.length(challenge_asset_name) == 32,
    tx.fee > 0,
    tx.fee <= parameters.max_timeout_fee_lovelace,
    inclusive_lower_bound >= response_deadline,
    record_input_index != terminal_accumulator_input_index,
    challenger_refund_output_index >= 0,
    terminal_input.output.address == from_script(own_policy_id),
    terminal_input.output.value == (
      assets.from_lovelace(terminal_datum.remaining_challenger_lovelace)
        |> assets.add(own_policy_id, terminal_asset_name, 1)
    ),
    terminal_input.output.reference_script == None,
    terminal_datum.deployment_identity == commitment.deployment_identity,
    terminal_datum.header_hash == commitment.header_hash,
    terminal_datum.challenge_asset_name == challenge_asset_name,
    terminal_datum.next_tranche_index == list.length(
      commitment.tranche_descriptors,
    ),
    terminal_datum.has_timed_out_tranche,
    terminal_datum.response_deadline == response_deadline,
    terminal_datum.challenger == challenger,
    mint_pairs(tx.mint, own_policy_id) == expected_burn_pairs,
    validate_state_queue_timeout_binding(
      tx,
      hub_datum.state_queue,
      state_queue_mint_redeemer_index,
      commitment.header_hash,
      challenge_asset_name,
    ),
    timeout_node_status_matches(
      tx.inputs,
      hub_datum.state_queue,
      commitment.header_hash,
      availability.Challenged {
        commitment_hash: availability.commitment_hash_v1(commitment),
        challenge_asset_name,
      },
    ),
    exact_enterprise_ada_output(
      output_at(tx.outputs, challenger_refund_output_index),
      challenger,
      terminal_datum.remaining_challenger_lovelace - tx.fee + parameters.challenge_record_lovelace,
    ),
    terminal_datum.remaining_challenger_lovelace > tx.fee,
    challenger_output_count == 1,
  }
}

pub fn validate_advance_tranche(
  parameters: availability.ParametersV1,
  own_policy_id: PolicyId,
  current_datum: availability.TrancheDatumV1,
  own_input: Input,
  thread_output_index: Int,
  carrier_output_index: Int,
  m_previous_carrier_input_index: Option<Int>,
  tx: Transaction,
) -> Bool {
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
    = current_datum
  let tranche_asset_name = availability.tranche_asset_name_v1(
    challenge_asset_name,
    descriptor.tranche_index,
  )
  expect
    quantity_of(own_input.output.value, own_policy_id, tranche_asset_name) == 1
  expect
    assets.without_lovelace(own_input.output.value) == assets.from_asset(
      own_policy_id,
      tranche_asset_name,
      1,
    )
  let thread_output = output_at(tx.outputs, thread_output_index)
  let carrier_output = output_at(tx.outputs, carrier_output_index)
  expect publication: availability.PublicationDatumV1 = inline_datum(
    carrier_output,
  )
  expect Some(expected_next_datum) = availability.publication_advances_active_tranche_v1(
    current_datum,
    publication,
    parameters.response_geometry.chunk_byte_length,
    carrier_output_index,
  )
  expect next_datum: availability.TrancheDatumV1 = inline_datum(thread_output)
  let previous_carrier_lovelace = when m_previous_carrier_input_index is {
    None -> {
      expect latest_carrier_output_index == None
      expect next_offset == descriptor.start_offset
      expect
        accumulator == availability.tranche_start_accumulator_v1(
          deployment_identity,
          header_hash,
          descriptor,
        )
      0
    }
    Some(previous_carrier_input_index) -> {
      expect Some(expected_previous_carrier_output_index) = latest_carrier_output_index
      let previous_carrier = input_at(tx.inputs, previous_carrier_input_index)
      expect previous_carrier.output_reference != own_input.output_reference
      expect
        previous_carrier.output_reference.transaction_id == own_input.output_reference.transaction_id
      expect
        previous_carrier.output_reference.output_index == expected_previous_carrier_output_index
      expect previous_publication: availability.PublicationDatumV1 = inline_datum(
        previous_carrier.output,
      )
      expect previous_carrier.output.address == from_script(own_policy_id)
      expect
        assets.without_lovelace(previous_carrier.output.value) == assets.zero
      expect previous_carrier.output.reference_script == None
      expect previous_publication.deployment_identity == deployment_identity
      expect previous_publication.header_hash == header_hash
      expect previous_publication.challenge_asset_name == challenge_asset_name
      expect previous_publication.tranche_index == descriptor.tranche_index
      expect previous_publication.next_accumulator == accumulator
      expect
        previous_publication.chunk_offset + previous_publication.chunk_byte_length == next_offset
      assets.lovelace_of(previous_carrier.output.value)
    }
  }
  let (_, inclusive_upper_bound) = utils.get_inclusive_bounds_of_a_short_validity_range(
    tx.validity_range,
  )
  and {
    tx.mint == assets.zero,
    tx.fee > 0,
    tx.fee <= parameters.max_publication_fee_lovelace,
    inclusive_upper_bound <= response_deadline,
    next_datum == expected_next_datum,
    exact_script_output(
      thread_output,
      from_script(own_policy_id),
      assets.from_lovelace(assets.lovelace_of(thread_output.value))
        |> assets.add(own_policy_id, tranche_asset_name, 1),
    ),
    exact_script_output(
      carrier_output,
      from_script(own_policy_id),
      assets.from_lovelace(assets.lovelace_of(carrier_output.value)),
    ),
    assets.lovelace_of(thread_output.value) + assets.lovelace_of(
      carrier_output.value,
    ) + tx.fee == assets.lovelace_of(own_input.output.value) + previous_carrier_lovelace,
    list.length(tx.inputs) == when m_previous_carrier_input_index is {
      None -> 1
      Some(_) -> 2
    },
    list.length(tx.outputs) == 2,
    thread_output_index != carrier_output_index,
  }
}

pub fn validate_consume_carrier(
  own_ref: OutputReference,
  thread_input_index: Int,
  thread_spend_redeemer_index: Int,
  tx: Transaction,
) -> Bool {
  let thread_input = input_at(tx.inputs, thread_input_index)
  let thread_redeemer_data = tx.redeemers
    |> utils.get_redeemer_at(
        Spend(thread_input.output_reference),
        thread_spend_redeemer_index,
      )
  expect
      availability.AdvanceTranche {
        m_previous_carrier_input_index: Some(previous_carrier_input_index),
        ..
      }: availability.SpendRedeemerV1
    = thread_redeemer_data
  expect previous_carrier = input_at(tx.inputs, previous_carrier_input_index)
  previous_carrier.output_reference == own_ref
}

pub fn validate_coordinate_spend(
  own_policy_id: PolicyId,
  own_ref: OutputReference,
  mint_redeemer_index: Int,
  tx: Transaction,
) -> Bool {
  let redeemer_data = tx.redeemers
    |> utils.get_redeemer_at(Mint(own_policy_id), mint_redeemer_index)
  expect redeemer: availability.MintRedeemerV1 = redeemer_data
  when redeemer is {
    // An open spends nothing at the availability address.
    availability.OpenChallenge { .. } -> fail
    availability.SettleTranche {
      terminal_accumulator_input_index,
      tranche_input_index,
      carrier_input_index,
      ..
    } -> or {
        input_at(tx.inputs, terminal_accumulator_input_index).output_reference == own_ref,
        input_at(tx.inputs, tranche_input_index).output_reference == own_ref,
        when carrier_input_index is {
          None -> False
          Some(index) -> input_at(tx.inputs, index).output_reference == own_ref
        },
      }
    availability.CloseChallenge {
      record_input_index,
      terminal_accumulator_input_index,
      ..
    } -> or {
        input_at(tx.inputs, record_input_index).output_reference == own_ref,
        input_at(tx.inputs, terminal_accumulator_input_index).output_reference == own_ref,
      }
    availability.TimeoutChallenge {
      record_input_index,
      terminal_accumulator_input_index,
      ..
    } -> or {
        input_at(tx.inputs, record_input_index).output_reference == own_ref,
        input_at(tx.inputs, terminal_accumulator_input_index).output_reference == own_ref,
      }
  }
}
