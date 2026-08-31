use aiken/collection/list
use aiken/interval.{between}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{PolicyId, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Redeemer, ScriptPurpose,
  Transaction,
}
use midgard/availability_challenge as availability
use midgard/common/utils
use midgard/correction_lock
use midgard/fraud_proof
use midgard/hub_oracle as hub
use midgard/state_queue

fn state_queue_mint_redeemer(
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  state_queue_policy_id: PolicyId,
) -> state_queue.MintRedeemer {
  expect Some(Pair(_, redeemer_data)) = redeemers
    |> list.find(
        fn(Pair(purpose, _)) { purpose == Mint(state_queue_policy_id) },
      )
  expect redeemer: state_queue.MintRedeemer = redeemer_data
  redeemer
}

fn hub_datum_from_spent_input(
  inputs: List<Input>,
  hub_oracle_policy_id: PolicyId,
  hub_oracle_input_index: Int,
) -> hub.Datum {
  let hub_input = utils.get_authentic_input_of(
    inputs,
    hub_oracle_policy_id,
    hub.asset_name,
    hub_oracle_input_index,
  )
  expect InlineDatum(hub_datum_data) = hub_input.output.datum
  expect hub_datum: hub.Datum = hub_datum_data
  hub_datum
}

fn correction_transition(
  current_datum: correction_lock.Datum,
  own_input: Input,
  hub_oracle_policy_id: PolicyId,
  availability_policy_id: PolicyId,
  hub_datum: hub.Datum,
  tx: Transaction,
) -> Bool {
  let mint_redeemer = state_queue_mint_redeemer(
    tx.redeemers,
    hub_datum.state_queue,
  )
  let (expected_locked, terminal) = when mint_redeemer is {
    state_queue.RemoveFraudulentBlockHeader {
      fraudulent_blocks_header_hash,
      fraud_proof_ref_input_index,
      block_removal_approach,
      ..
    } -> {
      let
          referenced_header_hash,
          _fraud_prover,
          referenced_fraud_proof_asset_name
        <- fraud_proof.get_proven_fraud_record_with_identity(
        tx.reference_inputs,
        hub_datum.fraud_proof,
        fraud_proof_ref_input_index,
      )
      expect referenced_header_hash == fraudulent_blocks_header_hash
      (
        correction_lock.Locked {
          target_header_hash: fraudulent_blocks_header_hash,
          correction_identity: correction_lock.FraudProof {
            fraud_proof_asset_name: referenced_fraud_proof_asset_name,
          },
        },
        when block_removal_approach is {
          state_queue.RemoveFraudulentBlocksLink { .. } -> False
          state_queue.RemoveLastFraudulentBlock { .. } -> True
        },
      )
    }
    state_queue.RemoveUnattestedBlockAfterTimeout {
      timed_out_header_hash,
      removal_approach,
    } ->
      (
        correction_lock.Locked {
          target_header_hash: timed_out_header_hash,
          correction_identity: correction_lock.AttestationTimeout,
        },
        when removal_approach is {
          state_queue.PruneTimedOutBlockDescendant { .. } -> False
          state_queue.RemoveTimedOutHead { .. } -> True
        },
      )
    state_queue.RemoveUnavailableBlockAfterTimeout {
      unavailable_header_hash,
      challenge_asset_name,
      removal_approach,
    } -> {
      expect bytearray.length(challenge_asset_name) == 32
      let expected_locked = correction_lock.Locked {
        target_header_hash: unavailable_header_hash,
        correction_identity: correction_lock.AvailabilityChallenge {
          challenge_asset_name,
        },
      }
      expect
        when current_datum is {
          correction_lock.Idle -> {
            expect [challenge_input] = tx.inputs
              |> list.filter(
                  fn(Input { output, .. }) {
                    quantity_of(
                      output.value,
                      availability_policy_id,
                      challenge_asset_name,
                    ) == 1
                  },
                )
            expect
              challenge_input.output.address == address.from_script(
                availability_policy_id,
              )
            expect InlineDatum(challenge_datum_data) = challenge_input.output.datum
            expect
                availability.ChallengedBond {
                  commitment,
                  da_bond_asset_name,
                  challenge_asset_name: authenticated_challenge_asset_name,
                  response_deadline,
                  ..
                }: availability.BondDatumV1
              = challenge_datum_data
            expect
              quantity_of(
                challenge_input.output.value,
                availability_policy_id,
                da_bond_asset_name,
              ) == 1
            and {
              authenticated_challenge_asset_name == challenge_asset_name,
              commitment.header_hash == unavailable_header_hash,
              utils.get_inclusive_lower_bound_of_interval(tx.validity_range) >= response_deadline,
            }
          }
          current -> current == expected_locked
        }
      (
        expected_locked,
        when removal_approach is {
          state_queue.PruneTimedOutBlockDescendant { .. } -> False
          state_queue.RemoveTimedOutHead { .. } -> True
        },
      )
    }
    _ -> fail
  }
  let lock_output = correction_lock.unique_output(
    tx.outputs,
    hub_oracle_policy_id,
    own_input.output.address,
  )
  and {
    quantity_of(tx.mint, hub_oracle_policy_id, correction_lock.asset_name) == 0,
    correction_lock.datum_transition_is_valid(
      current_datum,
      correction_lock.decode_datum(lock_output),
      expected_locked,
      terminal,
    ),
    lock_output.address == own_input.output.address,
    lock_output.value == own_input.output.value,
  }
}

validator spend(
  hub_oracle_policy_id: PolicyId,
  availability_policy_id: PolicyId,
) {
  spend(
    datum: Option<correction_lock.Datum>,
    redeemer: correction_lock.Redeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(current_datum) = datum
    expect Some(own_input) = tx.inputs
      |> list.find(
          fn(Input { output_reference, .. }) { output_reference == own_ref },
        )
    let authenticated_own_input = correction_lock.unique_input(
      tx.inputs,
      hub_oracle_policy_id,
      own_input.output.address,
    )
    expect authenticated_own_input.output_reference == own_ref
    expect correction_lock.decode_datum(own_input.output) == current_datum

    when redeemer is {
      correction_lock.Correct { hub_oracle_ref_input_index } -> {
        let hub_datum = hub.get_datum(
          tx.reference_inputs,
          hub_oracle_policy_id,
          hub_oracle_ref_input_index,
        )
        correction_transition(
          current_datum,
          own_input,
          hub_oracle_policy_id,
          availability_policy_id,
          hub_datum,
          tx,
        )
      }
      correction_lock.Deinit { hub_oracle_input_index } -> {
        let hub_datum = hub_datum_from_spent_input(
          tx.inputs,
          hub_oracle_policy_id,
          hub_oracle_input_index,
        )
        expect
          state_queue_mint_redeemer(tx.redeemers, hub_datum.state_queue) == state_queue.Deinit
        and {
          current_datum == correction_lock.Idle,
          quantity_of(tx.mint, hub_oracle_policy_id, correction_lock.asset_name) == -1,
          correction_lock.has_no_output(tx.outputs, hub_oracle_policy_id),
        }
      }
    }
  }

  else(_) {
    fail
  }
}

const test_hub_policy: ByteArray =
  #"11111111111111111111111111111111111111111111111111111111"

const test_state_queue_policy: ByteArray =
  #"22222222222222222222222222222222222222222222222222222222"

const test_other_state_queue_policy: ByteArray =
  #"23232323232323232323232323232323232323232323232323232323"

const test_fraud_proof_policy: ByteArray =
  #"33333333333333333333333333333333333333333333333333333333"

const test_lock_script: ByteArray =
  #"44444444444444444444444444444444444444444444444444444444"

const test_misc_policy: ByteArray =
  #"55555555555555555555555555555555555555555555555555555555"

const test_target: ByteArray =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const test_other_target: ByteArray =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const test_proof_asset_name: ByteArray =
  #"00000001aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const test_other_proof_asset_name: ByteArray =
  #"00000002aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const test_operator: ByteArray =
  #"66666666666666666666666666666666666666666666666666666666"

fn test_outref(_byte: ByteArray, index: Int) -> OutputReference {
  OutputReference {
    transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    output_index: index,
  }
}

fn test_hub_datum(state_queue_policy: ByteArray) -> hub.Datum {
  let misc_address = address.from_script(test_misc_policy)
  hub.Datum {
    registered_operators: test_misc_policy,
    active_operators: test_misc_policy,
    retired_operators: test_misc_policy,
    scheduler: test_misc_policy,
    state_queue: state_queue_policy,
    fraud_proof_catalogue: test_misc_policy,
    fraud_proof: test_fraud_proof_policy,
    deposit: test_misc_policy,
    withdrawal: test_misc_policy,
    tx_order: test_misc_policy,
    settlement: test_misc_policy,
    payout: test_misc_policy,
    registered_operators_addr: misc_address,
    active_operators_addr: misc_address,
    retired_operators_addr: misc_address,
    scheduler_addr: misc_address,
    state_queue_addr: address.from_script(state_queue_policy),
    fraud_proof_catalogue_addr: misc_address,
    fraud_proof_addr: address.from_script(test_fraud_proof_policy),
    deposit_addr: misc_address,
    withdrawal_addr: misc_address,
    tx_order_addr: misc_address,
    settlement_addr: misc_address,
    reserve_addr: misc_address,
    payout_addr: misc_address,
    reserve_observer: test_misc_policy,
  }
}

fn test_hub_ref_input(policy: ByteArray, state_queue_policy: ByteArray) -> Input {
  Input {
    output_reference: test_outref(#"01", 0),
    output: Output {
      address: address.from_script(policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.merge(assets.from_asset(policy, hub.asset_name, 1)),
      datum: InlineDatum(test_hub_datum(state_queue_policy)),
      reference_script: None,
    },
  }
}

fn test_lock_output(datum: correction_lock.Datum) -> Output {
  Output {
    address: address.from_script(test_lock_script),
    value: assets.from_lovelace(2_000_000)
      |> assets.merge(
          assets.from_asset(test_hub_policy, correction_lock.asset_name, 1),
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn test_lock_input(datum: correction_lock.Datum) -> Input {
  Input {
    output_reference: test_outref(#"02", 0),
    output: test_lock_output(datum),
  }
}

fn test_timeout_locked(target: ByteArray) -> correction_lock.Datum {
  correction_lock.Locked {
    target_header_hash: target,
    correction_identity: correction_lock.AttestationTimeout,
  }
}

fn test_fraud_locked(asset_name: ByteArray) -> correction_lock.Datum {
  correction_lock.Locked {
    target_header_hash: test_target,
    correction_identity: correction_lock.FraudProof {
      fraud_proof_asset_name: asset_name,
    },
  }
}

fn test_availability_locked(asset_name: ByteArray) -> correction_lock.Datum {
  correction_lock.Locked {
    target_header_hash: test_target,
    correction_identity: correction_lock.AvailabilityChallenge {
      challenge_asset_name: asset_name,
    },
  }
}

fn test_timeout_redeemer(terminal: Bool) -> state_queue.MintRedeemer {
  let removal_approach = if terminal {
    state_queue.RemoveTimedOutHead {
      confirmed_state_input_outref: test_outref(#"03", 0),
      confirmed_state_output_index: 0,
    }
  } else {
    state_queue.PruneTimedOutBlockDescendant {
      confirmed_state_ref_input_index: 1,
      timed_out_node_input_outref: test_outref(#"04", 0),
      timed_out_node_output_index: 0,
    }
  }
  state_queue.RemoveUnattestedBlockAfterTimeout {
    timed_out_header_hash: test_target,
    removal_approach,
  }
}

fn test_fraud_redeemer(terminal: Bool) -> state_queue.MintRedeemer {
  let removal_approach = if terminal {
    state_queue.RemoveLastFraudulentBlock {
      anchor_element_input_outref: test_outref(#"05", 0),
      anchor_element_output_index: 0,
    }
  } else {
    state_queue.RemoveFraudulentBlocksLink {
      fraudulent_node_input_outref: test_outref(#"06", 0),
      fraudulent_node_output_index: 0,
    }
  }
  state_queue.RemoveFraudulentBlockHeader {
    fraudulent_operator: test_operator,
    fraudulent_blocks_header_hash: test_target,
    slashing_approach: state_queue.OperatorAlreadySlashed {
      active_operators_element_ref_input_index: 0,
      retired_operators_element_ref_input_index: 0,
    },
    fraud_proof_ref_input_index: 1,
    block_removal_approach: removal_approach,
  }
}

fn test_availability_redeemer(
  asset_name: ByteArray,
  terminal: Bool,
) -> state_queue.MintRedeemer {
  let removal_approach = if terminal {
    state_queue.RemoveTimedOutHead {
      confirmed_state_input_outref: test_outref(#"03", 0),
      confirmed_state_output_index: 0,
    }
  } else {
    state_queue.PruneTimedOutBlockDescendant {
      confirmed_state_ref_input_index: 1,
      timed_out_node_input_outref: test_outref(#"04", 0),
      timed_out_node_output_index: 0,
    }
  }
  state_queue.RemoveUnavailableBlockAfterTimeout {
    unavailable_header_hash: test_target,
    challenge_asset_name: asset_name,
    removal_approach,
  }
}

fn test_availability_challenge_input(asset_name: ByteArray) -> Input {
  let commitment: availability.CommitmentV1 = availability.CommitmentV1 {
    version: availability.commitment_version_v1,
    deployment_identity: test_hub_policy,
    header_hash: test_target,
    payload_byte_length: 1,
    response_geometry: availability.ResponseGeometryV1 {
      chunk_byte_length: 4096,
      tranche_byte_length: 4 * 1024 * 1024,
      max_tranche_count: 16,
    },
    tranche_descriptors: [],
    bond_owner: test_operator,
  }
  let da_bond_asset_name = #"4441424e88888888888888888888888888888888888888888888888888888888"
  let datum: availability.BondDatumV1 = availability.ChallengedBond {
    commitment,
    da_bond_asset_name,
    committee_signers_hash: #"9999999999999999999999999999999999999999999999999999999999999999",
    attested_signers: #"",
    challenge_asset_name: asset_name,
    challenger: test_operator,
    opened_at: 0,
    response_deadline: 100,
  }
  Input {
    output_reference: test_outref(#"08", 0),
    output: Output {
      address: address.from_script(test_misc_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.merge(
            assets.from_asset(test_misc_policy, da_bond_asset_name, 1),
          )
        |> assets.merge(assets.from_asset(test_misc_policy, asset_name, 1)),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn test_availability_acquire_tx(asset_name: ByteArray) -> Transaction {
  let state_queue_redeemer = test_availability_redeemer(asset_name, False)
  let state_queue_redeemer_data: Redeemer = state_queue_redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [
      test_lock_input(correction_lock.Idle),
      test_availability_challenge_input(asset_name),
    ],
    outputs: [test_lock_output(test_availability_locked(asset_name))],
    reference_inputs: [
      test_hub_ref_input(test_hub_policy, test_state_queue_policy),
    ],
    mint: assets.from_asset(test_state_queue_policy, "MBLC", -1)
      |> assets.merge(assets.from_asset(test_misc_policy, asset_name, -1)),
    redeemers: [Pair(Mint(test_state_queue_policy), state_queue_redeemer_data)],
    validity_range: between(100, 101),
  }
}

fn test_fraud_proof_ref_input(asset_name: ByteArray) -> Input {
  Input {
    output_reference: test_outref(#"07", 0),
    output: Output {
      address: address.from_script(test_fraud_proof_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.merge(
            assets.from_asset(test_fraud_proof_policy, asset_name, 1),
          ),
      datum: InlineDatum(fraud_proof.Datum { fraud_prover: test_operator }),
      reference_script: None,
    },
  }
}

fn test_correction_tx(
  current: correction_lock.Datum,
  next: correction_lock.Datum,
  state_queue_redeemer: state_queue.MintRedeemer,
  hub_ref_input: Input,
  extra_reference_inputs: List<Input>,
) -> Transaction {
  let state_queue_redeemer_data: Redeemer = state_queue_redeemer
  Transaction {
    ..transaction.placeholder,
    inputs: [test_lock_input(current)],
    outputs: [test_lock_output(next)],
    reference_inputs: [hub_ref_input, ..extra_reference_inputs],
    mint: assets.from_asset(test_state_queue_policy, "MBLC", -1),
    redeemers: [Pair(Mint(test_state_queue_policy), state_queue_redeemer_data)],
  }
}

fn run_test_correction(current: correction_lock.Datum, tx: Transaction) -> Bool {
  spend.spend(
    test_hub_policy,
    test_misc_policy,
    Some(current),
    correction_lock.Correct { hub_oracle_ref_input_index: 0 },
    test_lock_input(current).output_reference,
    tx,
  )
}

test correction_lock_handler_accepts_permissionless_timeout_acquire_resume_and_clear() {
  let hub_input = test_hub_ref_input(test_hub_policy, test_state_queue_policy)
  let locked = test_timeout_locked(test_target)
  and {
    run_test_correction(
      correction_lock.Idle,
      test_correction_tx(
        correction_lock.Idle,
        locked,
        test_timeout_redeemer(False),
        hub_input,
        [],
      ),
    ),
    run_test_correction(
      locked,
      test_correction_tx(
        locked,
        locked,
        test_timeout_redeemer(False),
        hub_input,
        [],
      ),
    ),
    run_test_correction(
      locked,
      test_correction_tx(
        locked,
        correction_lock.Idle,
        test_timeout_redeemer(True),
        hub_input,
        [],
      ),
    ),
  }
}

test correction_lock_handler_rejects_other_target_resume() fail {
  let current = test_timeout_locked(test_other_target)
  run_test_correction(
    current,
    test_correction_tx(
      current,
      test_timeout_locked(test_target),
      test_timeout_redeemer(False),
      test_hub_ref_input(test_hub_policy, test_state_queue_policy),
      [],
    ),
  )
}

test correction_lock_handler_rejects_nonterminal_unlock() fail {
  let current = test_timeout_locked(test_target)
  run_test_correction(
    current,
    test_correction_tx(
      current,
      correction_lock.Idle,
      test_timeout_redeemer(False),
      test_hub_ref_input(test_hub_policy, test_state_queue_policy),
      [],
    ),
  )
}

test correction_lock_handler_rejects_non_correction_state_queue_redeemer() fail {
  run_test_correction(
    correction_lock.Idle,
    test_correction_tx(
      correction_lock.Idle,
      correction_lock.Idle,
      state_queue.InitV1 { output_index: 0 },
      test_hub_ref_input(test_hub_policy, test_state_queue_policy),
      [],
    ),
  )
}

test correction_lock_handler_rejects_cross_deployment_state_queue_policy() fail {
  let current = correction_lock.Idle
  run_test_correction(
    current,
    test_correction_tx(
      current,
      test_timeout_locked(test_target),
      test_timeout_redeemer(False),
      test_hub_ref_input(test_hub_policy, test_other_state_queue_policy),
      [],
    ),
  )
}

test correction_lock_handler_rejects_forged_hub_policy() fail {
  let current = correction_lock.Idle
  run_test_correction(
    current,
    test_correction_tx(
      current,
      test_timeout_locked(test_target),
      test_timeout_redeemer(False),
      test_hub_ref_input(test_misc_policy, test_state_queue_policy),
      [],
    ),
  )
}

test correction_lock_handler_binds_fraud_proof_identity() {
  let expected = test_fraud_locked(test_proof_asset_name)
  run_test_correction(
    correction_lock.Idle,
    test_correction_tx(
      correction_lock.Idle,
      expected,
      test_fraud_redeemer(False),
      test_hub_ref_input(test_hub_policy, test_state_queue_policy),
      [test_fraud_proof_ref_input(test_proof_asset_name)],
    ),
  )
}

test correction_lock_handler_rejects_other_fraud_identity_resume() fail {
  let current = test_fraud_locked(test_other_proof_asset_name)
  run_test_correction(
    current,
    test_correction_tx(
      current,
      test_fraud_locked(test_proof_asset_name),
      test_fraud_redeemer(False),
      test_hub_ref_input(test_hub_policy, test_state_queue_policy),
      [test_fraud_proof_ref_input(test_proof_asset_name)],
    ),
  )
}

test correction_lock_handler_accepts_availability_acquire_resume_and_clear() {
  let challenge_asset_name = #"4441434888888888888888888888888888888888888888888888888888888888"
  let locked = test_availability_locked(challenge_asset_name)
  let hub_input = test_hub_ref_input(test_hub_policy, test_state_queue_policy)
  and {
    run_test_correction(
      correction_lock.Idle,
      test_availability_acquire_tx(challenge_asset_name),
    ),
    run_test_correction(
      locked,
      test_correction_tx(
        locked,
        locked,
        test_availability_redeemer(challenge_asset_name, False),
        hub_input,
        [],
      ),
    ),
    run_test_correction(
      locked,
      test_correction_tx(
        locked,
        correction_lock.Idle,
        test_availability_redeemer(challenge_asset_name, True),
        hub_input,
        [],
      ),
    ),
  }
}

test correction_lock_handler_rejects_other_availability_identity_resume() fail {
  let first = #"4441434888888888888888888888888888888888888888888888888888888888"
  let other = #"4441434899999999999999999999999999999999999999999999999999999999"
  let current = test_availability_locked(first)
  run_test_correction(
    current,
    test_correction_tx(
      current,
      test_availability_locked(other),
      test_availability_redeemer(other, False),
      test_hub_ref_input(test_hub_policy, test_state_queue_policy),
      [],
    ),
  )
}

test correction_lock_handler_rejects_availability_acquire_without_challenged_bond() fail {
  let challenge_asset_name = #"4441434888888888888888888888888888888888888888888888888888888888"
  let base = test_availability_acquire_tx(challenge_asset_name)
  let tx = Transaction {
    ..base,
    inputs: [test_lock_input(correction_lock.Idle)],
  }
  run_test_correction(correction_lock.Idle, tx)
}
