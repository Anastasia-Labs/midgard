use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Address, Script, from_script, from_verification_key}
use cardano/assets.{AssetName, PolicyId, Value, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Spend, Transaction,
}
use midgard/availability_challenge as availability
use midgard/common/utils
use midgard/da_attestation_types as da_attestation
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

fn da_attestation_asset_name(header_hash: ByteArray) -> AssetName {
  expect bytearray.length(header_hash) == 28
  da_attestation.attestation_asset_name_prefix
    |> bytearray.concat(header_hash)
}

fn validate_state_queue_status_transition(
  tx: Transaction,
  state_queue_policy_id: PolicyId,
  state_queue_input_index: Int,
  state_queue_output_index: Int,
  header_hash: ByteArray,
  previous_status: availability.StateQueueStatusV1,
  next_status: availability.StateQueueStatusV1,
) -> Bool {
  let state_queue_input = input_at(tx.inputs, state_queue_input_index)
  let state_queue_output = output_at(tx.outputs, state_queue_output_index)
  and {
    state_queue_input.output.value == state_queue_output.value,
    state_queue.validate_da_availability_status_transition(
      tx.inputs,
      tx.outputs,
      state_queue_policy_id,
      state_queue_input_index,
      state_queue_output_index,
      header_hash,
      previous_status,
      next_status,
    ),
  }
}

fn validate_da_apply_binding(
  tx: Transaction,
  da_policy_id: PolicyId,
  da_mint_redeemer_index: Int,
  da_attestation_input_index: Int,
  state_queue_input_index: Int,
  state_queue_output_index: Int,
) -> Bool {
  let redeemer_data = tx.redeemers
    |> utils.get_redeemer_at(Mint(da_policy_id), da_mint_redeemer_index)
  expect
      da_attestation.ApplyToStateQueue {
        da_attestation_input_index: bound_attestation_input_index,
        state_queue_input_index: bound_state_queue_input_index,
        state_queue_output_index: bound_state_queue_output_index,
        ..
      }: da_attestation.MintRedeemer
    = redeemer_data
  and {
    bound_attestation_input_index == da_attestation_input_index,
    bound_state_queue_input_index == state_queue_input_index,
    bound_state_queue_output_index == state_queue_output_index,
  }
}

fn validate_mint_bond_from_attestation(
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
  own_policy_id: PolicyId,
  tx: Transaction,
  hub_oracle_ref_input_index: Int,
  da_attestation_input_index: Int,
  da_attestation_mint_redeemer_index: Int,
  bond_output_index: Int,
  state_queue_input_index: Int,
  state_queue_output_index: Int,
) -> Bool {
  let hub_datum = hub.get_datum(
    tx.reference_inputs,
    hub_oracle_policy_id,
    hub_oracle_ref_input_index,
  )
  let attestation_input = input_at(tx.inputs, da_attestation_input_index)
  let da_policy_id = script_hash(attestation_input.output.address)
  expect attestation_datum: da_attestation.DaAttestationDatum = inline_datum(
    attestation_input.output,
  )
  let commitment = attestation_datum.availability_commitment
  let da_asset_name = da_attestation_asset_name(attestation_datum.header_hash)
  let bond_asset_name = availability.da_bond_asset_name_v1(
    attestation_input.output_reference,
  )
  let bond_output = output_at(tx.outputs, bond_output_index)
  let expected_bond_datum: availability.BondDatumV1 = availability.Available {
    commitment,
    da_bond_asset_name: bond_asset_name,
    committee_signers_hash: attestation_datum.committee_signers_hash,
    attested_signers: attestation_datum.attested_signers,
  }
  let expected_bond_datum_data: Data = expected_bond_datum
  and {
    list.length(tx.inputs) == 2,
    list.length(tx.outputs) == 2,
    da_attestation_input_index != state_queue_input_index,
    bond_output_index != state_queue_output_index,
    availability.parameters_are_canonical_v1(parameters),
    availability.commitment_is_canonical_v1(commitment, parameters),
    commitment.deployment_identity == hub_oracle_policy_id,
    commitment.header_hash == attestation_datum.header_hash,
    attestation_input.output.value == (
      assets.from_lovelace(parameters.da_bond_lovelace)
        |> assets.add(da_policy_id, da_asset_name, 1)
    ),
    mint_pairs(tx.mint, own_policy_id) == [Pair(bond_asset_name, 1)],
    exact_script_output(
      bond_output,
      from_script(own_policy_id),
      assets.from_lovelace(parameters.da_bond_lovelace)
        |> assets.add(own_policy_id, bond_asset_name, 1),
    ),
    inline_datum(bond_output) == expected_bond_datum_data,
    validate_da_apply_binding(
      tx,
      da_policy_id,
      da_attestation_mint_redeemer_index,
      da_attestation_input_index,
      state_queue_input_index,
      state_queue_output_index,
    ),
    validate_state_queue_status_transition(
      tx,
      hub_datum.state_queue,
      state_queue_input_index,
      state_queue_output_index,
      commitment.header_hash,
      availability.Unattested,
      availability.Attested { da_bond_asset_name: bond_asset_name },
    ),
  }
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

fn validate_open_challenge(
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
  own_policy_id: PolicyId,
  tx: Transaction,
  hub_oracle_ref_input_index: Int,
  bond_input_index: Int,
  bond_output_index: Int,
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
  let bond_input = input_at(tx.inputs, bond_input_index)
  expect
      availability.Available {
        commitment,
        da_bond_asset_name,
        committee_signers_hash,
        attested_signers,
      }: availability.BondDatumV1
    = inline_datum(bond_input.output)
  let challenge_asset_name = availability.challenge_asset_name_v1(
    bond_input.output_reference,
  )
  let challenger_input = input_at(tx.inputs, challenger_input_index)
  let bond_output = output_at(tx.outputs, bond_output_index)
  let (inclusive_lower_bound, _inclusive_upper_bound) = utils.get_inclusive_bounds_of_a_short_validity_range(
    tx.validity_range,
  )
  expect Some(response_deadline) = availability.response_deadline_v1(
    commitment.payload_byte_length,
    inclusive_lower_bound,
  )
  let expected_bond_datum: availability.BondDatumV1 = availability.ChallengedBond {
    commitment,
    da_bond_asset_name,
    committee_signers_hash,
    attested_signers,
    challenge_asset_name,
    challenger,
    opened_at: inclusive_lower_bound,
    response_deadline,
  }
  let expected_bond_datum_data: Data = expected_bond_datum
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
    list.length(tx.inputs) == 3,
    list.length(tx.outputs) == 3 + descriptor_count,
    tx.fee > 0,
    tx.fee <= parameters.max_open_fee_lovelace,
    bond_input_index != challenger_input_index,
    bond_input_index != state_queue_input_index,
    challenger_input_index != state_queue_input_index,
    bond_output_index != state_queue_output_index,
    index_outside_contiguous_range(
      bond_output_index,
      first_tranche_output_index,
      descriptor_count,
    ),
    index_outside_contiguous_range(
      state_queue_output_index,
      first_tranche_output_index,
      descriptor_count,
    ),
    terminal_accumulator_output_index != bond_output_index,
    terminal_accumulator_output_index != state_queue_output_index,
    index_outside_contiguous_range(
      terminal_accumulator_output_index,
      first_tranche_output_index,
      descriptor_count,
    ),
    first_tranche_output_index + descriptor_count <= list.length(tx.outputs),
    commitment.deployment_identity == hub_oracle_policy_id,
    utils.has_signed(challenger, tx.extra_signatories),
    bond_input.output.address == from_script(own_policy_id),
    bond_input.output.value == (
      assets.from_lovelace(parameters.da_bond_lovelace)
        |> assets.add(own_policy_id, da_bond_asset_name, 1)
    ),
    challenger_input.output.address == from_verification_key(challenger),
    challenger_input.output.value == assets.from_lovelace(
      parameters.challenger_bond_lovelace + tx.fee,
    ),
    challenger_input.output.datum == NoDatum,
    challenger_input.output.reference_script == None,
    mint_pairs(tx.mint, own_policy_id) == expected_mint_pairs,
    exact_script_output(
      bond_output,
      from_script(own_policy_id),
      assets.from_lovelace(parameters.da_bond_lovelace)
        |> assets.add(own_policy_id, da_bond_asset_name, 1)
        |> assets.add(own_policy_id, challenge_asset_name, 1),
    ),
    inline_datum(bond_output) == expected_bond_datum_data,
    validate_state_queue_status_transition(
      tx,
      hub_datum.state_queue,
      state_queue_input_index,
      state_queue_output_index,
      commitment.header_hash,
      availability.Attested { da_bond_asset_name },
      availability.Challenged { da_bond_asset_name, challenge_asset_name },
    ),
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

fn validate_settle_tranche(
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
  own_policy_id: PolicyId,
  tx: Transaction,
  bond_ref_input_index: Int,
  terminal_accumulator_input_index: Int,
  terminal_accumulator_output_index: Int,
  tranche_input_index: Int,
  carrier_input_index: Option<Int>,
) -> Bool {
  let bond_ref_input = input_at(tx.reference_inputs, bond_ref_input_index)
  expect
      availability.ChallengedBond {
        commitment,
        da_bond_asset_name,
        challenge_asset_name,
        challenger,
        response_deadline,
        ..
      }: availability.BondDatumV1
    = inline_datum(bond_ref_input.output)
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
    bond_ref_input.output.address == from_script(own_policy_id),
    bond_ref_input.output.value == (
      assets.from_lovelace(parameters.da_bond_lovelace)
        |> assets.add(own_policy_id, da_bond_asset_name, 1)
        |> assets.add(own_policy_id, challenge_asset_name, 1)
    ),
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

fn validate_close_challenge(
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
  own_policy_id: PolicyId,
  tx: Transaction,
  hub_oracle_ref_input_index: Int,
  bond_input_index: Int,
  terminal_accumulator_input_index: Int,
  state_queue_input_index: Int,
  state_queue_output_index: Int,
  da_refund_output_index: Int,
  challenger_refund_output_index: Int,
) -> Bool {
  let hub_datum = hub.get_datum(
    tx.reference_inputs,
    hub_oracle_policy_id,
    hub_oracle_ref_input_index,
  )
  let bond_input = input_at(tx.inputs, bond_input_index)
  expect
      availability.ChallengedBond {
        commitment,
        da_bond_asset_name,
        challenge_asset_name,
        challenger,
        ..
      }: availability.BondDatumV1
    = inline_datum(bond_input.output)
  let terminal_input = input_at(tx.inputs, terminal_accumulator_input_index)
  expect terminal_datum: availability.TerminalAccumulatorDatumV1 = inline_datum(
    terminal_input.output,
  )
  let terminal_asset_name = availability.terminal_accumulator_asset_name_v1(
    challenge_asset_name,
  )
  let expected_burn_pairs = [
    Pair(da_bond_asset_name, -1),
    Pair(challenge_asset_name, -1),
    Pair(terminal_asset_name, -1),
  ]
  and {
    availability.commitment_is_canonical_v1(commitment, parameters),
    commitment.deployment_identity == hub_oracle_policy_id,
    tx.fee > 0,
    tx.fee <= parameters.max_close_fee_lovelace,
    list.length(tx.inputs) == 3,
    list.length(tx.outputs) == 3,
    bond_input_index != state_queue_input_index,
    bond_input_index != terminal_accumulator_input_index,
    state_queue_input_index != terminal_accumulator_input_index,
    da_refund_output_index != challenger_refund_output_index,
    da_refund_output_index != state_queue_output_index,
    challenger_refund_output_index != state_queue_output_index,
    bond_input.output.address == from_script(own_policy_id),
    bond_input.output.value == (
      assets.from_lovelace(parameters.da_bond_lovelace)
        |> assets.add(own_policy_id, da_bond_asset_name, 1)
        |> assets.add(own_policy_id, challenge_asset_name, 1)
    ),
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
    validate_state_queue_status_transition(
      tx,
      hub_datum.state_queue,
      state_queue_input_index,
      state_queue_output_index,
      commitment.header_hash,
      availability.Challenged { da_bond_asset_name, challenge_asset_name },
      availability.Published {
        terminal_commitment: availability.published_terminal_commitment_v1(
          commitment,
        ),
      },
    ),
    exact_enterprise_ada_output(
      output_at(tx.outputs, da_refund_output_index),
      commitment.bond_owner,
      parameters.da_bond_lovelace,
    ),
    exact_enterprise_ada_output(
      output_at(tx.outputs, challenger_refund_output_index),
      challenger,
      terminal_datum.remaining_challenger_lovelace - tx.fee,
    ),
    terminal_datum.remaining_challenger_lovelace > tx.fee,
  }
}

fn present_carrier_indices(indices: List<Option<Int>>) -> List<Int> {
  when indices is {
    [] -> []
    [None, ..rest] -> present_carrier_indices(rest)
    [Some(index), ..rest] -> [index, ..present_carrier_indices(rest)]
  }
}

fn indices_are_unique_nonnegative(indices: List<Int>) -> Bool {
  when indices is {
    [] -> True
    [index, ..rest] ->
      index >= 0 && !list.has(rest, index) && indices_are_unique_nonnegative(
        rest,
      )
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

fn validate_timeout_tranche_inputs(
  inputs: List<Input>,
  own_policy_id: PolicyId,
  commitment: availability.CommitmentV1,
  challenge_asset_name: AssetName,
  challenger: ByteArray,
  response_deadline: Int,
  descriptor_index: Int,
  descriptors: List<availability.TrancheDescriptorV1>,
  tranche_input_indices: List<Int>,
  carrier_input_indices: List<Option<Int>>,
) -> Option<Int> {
  when descriptors is {
    [] ->
      if and {
        tranche_input_indices == [],
        carrier_input_indices == [],
      } {
        Some(0)
      } else {
        None
      }
    [descriptor, ..rest_descriptors] -> {
      expect [thread_input_index, ..rest_thread_indices] = tranche_input_indices
      expect [m_carrier_input_index, ..rest_carrier_indices] = carrier_input_indices
      let thread_input = input_at(inputs, thread_input_index)
      let tranche_asset_name = availability.tranche_asset_name_v1(
        challenge_asset_name,
        descriptor_index,
      )
      expect thread_datum: availability.TrancheDatumV1 = inline_datum(
        thread_input.output,
      )
      let (thread_identity_is_valid, expected_accumulator, expected_next_offset)
        = when thread_datum is {
        availability.Active {
          deployment_identity,
          header_hash,
          challenge_asset_name: thread_challenge_asset_name,
          descriptor: thread_descriptor,
          next_offset,
          accumulator,
          latest_carrier_output_index: _,
          response_deadline: thread_response_deadline,
          challenger: thread_challenger,
        } -> (and {
              deployment_identity == commitment.deployment_identity,
              header_hash == commitment.header_hash,
              thread_challenge_asset_name == challenge_asset_name,
              thread_descriptor == descriptor,
              thread_response_deadline == response_deadline,
              thread_challenger == challenger,
            }, accumulator, next_offset)
        availability.Receipt {
          deployment_identity,
          header_hash,
          challenge_asset_name: thread_challenge_asset_name,
          descriptor: thread_descriptor,
          terminal_accumulator,
          terminal_carrier_output_index: _,
          challenger: thread_challenger,
        } -> (and {
              deployment_identity == commitment.deployment_identity,
              header_hash == commitment.header_hash,
              thread_challenge_asset_name == challenge_asset_name,
              thread_descriptor == descriptor,
              terminal_accumulator == descriptor.terminal_accumulator,
              thread_challenger == challenger,
              m_carrier_input_index != None,
            }, terminal_accumulator, descriptor.start_offset + descriptor.byte_length)
      }
      expect Some(carrier_lovelace) = timeout_carrier_lovelace(
        inputs,
        own_policy_id,
        commitment,
        challenge_asset_name,
        descriptor,
        expected_accumulator,
        expected_next_offset,
        m_carrier_input_index,
      )
      expect Some(rest_lovelace) = validate_timeout_tranche_inputs(
        inputs,
        own_policy_id,
        commitment,
        challenge_asset_name,
        challenger,
        response_deadline,
        descriptor_index + 1,
        rest_descriptors,
        rest_thread_indices,
        rest_carrier_indices,
      )
      if and {
        descriptor.tranche_index == descriptor_index,
        thread_identity_is_valid,
        thread_input.output.address == from_script(own_policy_id),
        thread_input.output.reference_script == None,
        assets.without_lovelace(thread_input.output.value) == assets.from_asset(
          own_policy_id,
          tranche_asset_name,
          1,
        ),
      } {
        Some(
          assets.lovelace_of(thread_input.output.value) + carrier_lovelace + rest_lovelace,
        )
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

fn validate_timeout_challenge(
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
  own_policy_id: PolicyId,
  tx: Transaction,
  hub_oracle_ref_input_index: Int,
  bond_input_index: Int,
  terminal_accumulator_input_index: Int,
  state_queue_mint_redeemer_index: Int,
  da_slash_output_index: Int,
  challenger_refund_output_index: Int,
) -> Bool {
  let hub_datum = hub.get_datum(
    tx.reference_inputs,
    hub_oracle_policy_id,
    hub_oracle_ref_input_index,
  )
  let bond_input = input_at(tx.inputs, bond_input_index)
  expect
      availability.ChallengedBond {
        commitment,
        da_bond_asset_name,
        challenge_asset_name,
        challenger,
        response_deadline,
        ..
      }: availability.BondDatumV1
    = inline_datum(bond_input.output)
  let terminal_input = input_at(tx.inputs, terminal_accumulator_input_index)
  expect terminal_datum: availability.TerminalAccumulatorDatumV1 = inline_datum(
    terminal_input.output,
  )
  let terminal_asset_name = availability.terminal_accumulator_asset_name_v1(
    challenge_asset_name,
  )
  let expected_burn_pairs = [
    Pair(da_bond_asset_name, -1),
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
    bond_input_index != terminal_accumulator_input_index,
    da_slash_output_index >= 0,
    challenger_refund_output_index >= 0,
    da_slash_output_index != challenger_refund_output_index,
    bond_input.output.address == from_script(own_policy_id),
    bond_input.output.value == (
      assets.from_lovelace(parameters.da_bond_lovelace)
        |> assets.add(own_policy_id, da_bond_asset_name, 1)
        |> assets.add(own_policy_id, challenge_asset_name, 1)
    ),
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
    exact_enterprise_ada_output(
      output_at(tx.outputs, da_slash_output_index),
      challenger,
      parameters.da_bond_lovelace,
    ),
    exact_enterprise_ada_output(
      output_at(tx.outputs, challenger_refund_output_index),
      challenger,
      terminal_datum.remaining_challenger_lovelace - tx.fee,
    ),
    terminal_datum.remaining_challenger_lovelace > tx.fee,
    challenger_output_count == 2,
  }
}

fn validate_advance_tranche(
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

fn validate_consume_carrier(
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

fn validate_coordinate_spend(
  own_policy_id: PolicyId,
  own_ref: OutputReference,
  mint_redeemer_index: Int,
  tx: Transaction,
) -> Bool {
  let redeemer_data = tx.redeemers
    |> utils.get_redeemer_at(Mint(own_policy_id), mint_redeemer_index)
  expect redeemer: availability.MintRedeemerV1 = redeemer_data
  when redeemer is {
    availability.OpenChallenge { bond_input_index, .. } ->
      input_at(tx.inputs, bond_input_index).output_reference == own_ref
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
      bond_input_index,
      terminal_accumulator_input_index,
      ..
    } -> or {
        input_at(tx.inputs, bond_input_index).output_reference == own_ref,
        input_at(tx.inputs, terminal_accumulator_input_index).output_reference == own_ref,
      }
    availability.TimeoutChallenge {
      bond_input_index,
      terminal_accumulator_input_index,
      ..
    } -> or {
        input_at(tx.inputs, bond_input_index).output_reference == own_ref,
        input_at(tx.inputs, terminal_accumulator_input_index).output_reference == own_ref,
      }
    availability.MintBondFromAttestation { .. } -> fail
  }
}

fn contiguous_input_range_contains_ref(
  inputs: List<Input>,
  first_index: Int,
  count: Int,
  own_ref: OutputReference,
) -> Bool {
  if count <= 0 {
    False
  } else if input_at(inputs, first_index).output_reference == own_ref {
    True
  } else {
    contiguous_input_range_contains_ref(
      inputs,
      first_index + 1,
      count - 1,
      own_ref,
    )
  }
}

fn indexed_inputs_contain_ref(
  inputs: List<Input>,
  indices: List<Int>,
  own_ref: OutputReference,
) -> Bool {
  when indices is {
    [] -> False
    [index, ..rest] ->
      input_at(inputs, index).output_reference == own_ref || indexed_inputs_contain_ref(
        inputs,
        rest,
        own_ref,
      )
  }
}

fn optional_indexed_inputs_contain_ref(
  inputs: List<Input>,
  indices: List<Option<Int>>,
  own_ref: OutputReference,
) -> Bool {
  when indices is {
    [] -> False
    [None, ..rest] -> optional_indexed_inputs_contain_ref(inputs, rest, own_ref)
    [Some(index), ..rest] ->
      input_at(inputs, index).output_reference == own_ref || optional_indexed_inputs_contain_ref(
        inputs,
        rest,
        own_ref,
      )
  }
}

validator availability_challenge(
  hub_oracle_policy_id: PolicyId,
  parameters: availability.ParametersV1,
) {
  mint(
    redeemer: availability.MintRedeemerV1,
    own_policy_id: PolicyId,
    tx: Transaction,
  ) {
    when redeemer is {
      availability.MintBondFromAttestation {
        hub_oracle_ref_input_index,
        da_attestation_input_index,
        da_attestation_mint_redeemer_index,
        bond_output_index,
        state_queue_input_index,
        state_queue_output_index,
      } ->
        validate_mint_bond_from_attestation(
          hub_oracle_policy_id,
          parameters,
          own_policy_id,
          tx,
          hub_oracle_ref_input_index,
          da_attestation_input_index,
          da_attestation_mint_redeemer_index,
          bond_output_index,
          state_queue_input_index,
          state_queue_output_index,
        )
      availability.OpenChallenge {
        hub_oracle_ref_input_index,
        bond_input_index,
        bond_output_index,
        challenger_input_index,
        state_queue_input_index,
        state_queue_output_index,
        first_tranche_output_index,
        terminal_accumulator_output_index,
        challenger,
      } ->
        validate_open_challenge(
          hub_oracle_policy_id,
          parameters,
          own_policy_id,
          tx,
          hub_oracle_ref_input_index,
          bond_input_index,
          bond_output_index,
          challenger_input_index,
          state_queue_input_index,
          state_queue_output_index,
          first_tranche_output_index,
          terminal_accumulator_output_index,
          challenger,
        )
      availability.SettleTranche {
        bond_ref_input_index,
        terminal_accumulator_input_index,
        terminal_accumulator_output_index,
        tranche_input_index,
        carrier_input_index,
      } ->
        validate_settle_tranche(
          hub_oracle_policy_id,
          parameters,
          own_policy_id,
          tx,
          bond_ref_input_index,
          terminal_accumulator_input_index,
          terminal_accumulator_output_index,
          tranche_input_index,
          carrier_input_index,
        )
      availability.CloseChallenge {
        hub_oracle_ref_input_index,
        bond_input_index,
        terminal_accumulator_input_index,
        state_queue_input_index,
        state_queue_output_index,
        da_refund_output_index,
        challenger_refund_output_index,
      } ->
        validate_close_challenge(
          hub_oracle_policy_id,
          parameters,
          own_policy_id,
          tx,
          hub_oracle_ref_input_index,
          bond_input_index,
          terminal_accumulator_input_index,
          state_queue_input_index,
          state_queue_output_index,
          da_refund_output_index,
          challenger_refund_output_index,
        )
      availability.TimeoutChallenge {
        hub_oracle_ref_input_index,
        bond_input_index,
        terminal_accumulator_input_index,
        state_queue_mint_redeemer_index,
        da_slash_output_index,
        challenger_refund_output_index,
      } ->
        validate_timeout_challenge(
          hub_oracle_policy_id,
          parameters,
          own_policy_id,
          tx,
          hub_oracle_ref_input_index,
          bond_input_index,
          terminal_accumulator_input_index,
          state_queue_mint_redeemer_index,
          da_slash_output_index,
          challenger_refund_output_index,
        )
    }
  }

  spend(
    datum: Option<Data>,
    redeemer: availability.SpendRedeemerV1,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(own_input) = tx.inputs
      |> list.find(
          fn(Input { output_reference, .. }) { output_reference == own_ref },
        )
    when redeemer is {
      availability.AdvanceTranche {
        thread_output_index,
        carrier_output_index,
        m_previous_carrier_input_index,
      } -> {
        expect Some(datum_data) = datum
        expect current_datum: availability.TrancheDatumV1 = datum_data
        validate_advance_tranche(
          parameters,
          script_hash(own_input.output.address),
          current_datum,
          own_input,
          thread_output_index,
          carrier_output_index,
          m_previous_carrier_input_index,
          tx,
        )
      }
      availability.ConsumeCarrier {
        thread_input_index,
        thread_spend_redeemer_index,
      } ->
        validate_consume_carrier(
          own_ref,
          thread_input_index,
          thread_spend_redeemer_index,
          tx,
        )
      availability.Coordinate { mint_redeemer_index } ->
        validate_coordinate_spend(
          script_hash(own_input.output.address),
          own_ref,
          mint_redeemer_index,
          tx,
        )
    }
  }

  else(_) {
    fail
  }
}
