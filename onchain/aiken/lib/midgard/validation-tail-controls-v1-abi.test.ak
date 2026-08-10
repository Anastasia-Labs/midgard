use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference}
use midgard/bounded_item_v1
use midgard/ledger_state
use midgard/mpf_proof_fold_v1
use midgard/transition_trace
use midgard/validation_claim_v1
use midgard/validation_machine_v1
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

const h41 = #"4141414141414141414141414141414141414141414141414141414141414141"

const h45 = #"4545454545454545454545454545454545454545454545454545454545454545"

const h46 = #"4646464646464646464646464646464646464646464646464646464646464646"

const h47 = #"4747474747474747474747474747474747474747474747474747474747474747"

const h48 = #"4848484848484848484848484848484848484848484848484848484848484848"

const h51 = #"5151515151515151515151515151515151515151515151515151515151515151"

const h52 = #"5252525252525252525252525252525252525252525252525252525252525252"

const h53 = #"5353535353535353535353535353535353535353535353535353535353535353"

const h54 = #"5454545454545454545454545454545454545454545454545454545454545454"

const h55 = #"5555555555555555555555555555555555555555555555555555555555555555"

const h56 = #"5656565656565656565656565656565656565656565656565656565656565656"

const h57 = #"5757575757575757575757575757575757575757575757575757575757575757"

const h58 = #"5858585858585858585858585858585858585858585858585858585858585858"

const h61 = #"6161616161616161616161616161616161616161616161616161616161616161"

const h62 = #"6262626262626262626262626262626262626262626262626262626262626262"

const h63 = #"6363636363636363636363636363636363636363636363636363636363636363"

const h64 = #"6464646464646464646464646464646464646464646464646464646464646464"

const h65 = #"6565656565656565656565656565656565656565656565656565656565656565"

const h71 = #"7171717171717171717171717171717171717171717171717171717171717171"

const h72 = #"7272727272727272727272727272727272727272727272727272727272727272"

const h73 = #"7373737373737373737373737373737373737373737373737373737373737373"

const expected_value_accumulator_cbor =
  #"8407582054545454545454545454545454545454545454545454545454545454545454540201"

const expected_pending_mutation_cbor =
  #"8a01000142010242030445840100008020404000"

const expected_terminal_acceptance_cbor =
  #"840140582072727272727272727272727272727272727272727272727272727272727272725827820281820158207171717171717171717171717171717171717171717171717171717171717171"

const expected_terminal_rejection_cbor =
  #"840255455f56414c55455f4e4f545f505245534552564544582073737373737373737373737373737373737373737373737373737373737373734180"

const forced_source_membership_cbor =
  #"d8799fd8799fd87a80582011111111111111111111111111111111111111111111111111111111111111115820121212121212121212121212121212121212121212121212121212121212121201d8799f5820131313131313131313131313131313131313131313131313131313131313131304ffd8799f58201414141414141414141414141414141414141414141414141414141414141414d8799f428101428102428103ffd87c80ff80ffff"

const normal_source_membership_cbor =
  #"d87a9fd8799fd87b8058202121212121212121212121212121212121212121212121212121212121212121582022222222222222222222222222222222222222222222222222222222222222220158202323232323232323232323232323232323232323232323232323232323232323d8799f58202323232323232323232323232323232323232323232323232323232323232323d8799f43820102428104428105ffff80ffff"

fn native_control() -> validation_machine_v1.NativeScriptsControlV1 {
  validation_machine_v1.NativeScriptsControlV1 {
    compact_cbor: #"01",
    witness_set_compact_cbor: #"02",
    field_preimage_lengths_cbor: #"03",
    context_cbor: #"04",
    resolved_input_count: 0,
    resolved_inputs_accumulator: h51,
    spend_input_count: 0,
    resolved_item_peaks: [],
    signer_count: 0,
    signer_frontier_commitment: h52,
    source_count: 0,
    source_peaks: [],
    redeemer_count: 0,
    redeemer_peaks: [],
    purpose_count: 0,
    purpose_peaks: [],
    output_count: 0,
    output_peaks: [],
    output_descriptor_peaks: [],
    mint_count: 0,
    mint_peaks: [],
    execution_count: 0,
    execution_peaks: [],
    execution_cursor: 0,
    language_bitmap: 0,
    resolution_schedule_hash: h53,
  }
}

fn value_accumulator() -> validation_machine_v1.ValueAccumulatorV1 {
  validation_machine_v1.ValueAccumulatorV1 {
    lovelace_delta: 7,
    asset_root: h54,
    seen_asset_count: 2,
    nonzero_asset_count: 1,
  }
}

fn value_mutation() -> validation_machine_v1.ValueAssetMutationWitnessV1 {
  validation_machine_v1.ValueAssetMutationWitnessV1 {
    delta_was_present: False,
    old_delta: 0,
    delta_proof: [],
  }
}

fn proof_descriptor() -> mpf_proof_fold_v1.ProofDescriptorV1 {
  mpf_proof_fold_v1.ProofDescriptorV1 {
    version: 1,
    frame_count: 0,
    terminal_cursor: 0,
    peaks: [],
  }
}

fn proof_frame() -> mpf_proof_fold_v1.ProofFrameV1 {
  mpf_proof_fold_v1.ProofFrameV1 {
    version: 1,
    frame_index: 0,
    cursor: 0,
    next_cursor: 1,
    step: mpf.Branch { skip: 0, neighbors: #"" },
  }
}

fn chunk_proof() -> bounded_item_v1.ChunkProofV1 {
  bounded_item_v1.ChunkProofV1 {
    version: 1,
    field_index: 5,
    item_index: 0,
    total_length: 1,
    chunk_index: 0,
    chunk: #"12",
    frontier: [],
    siblings: [],
  }
}

fn tail_auxiliaries() -> List<
  validation_machine_v1.ValidationAuxiliaryWitnessV1,
> {
  [
    validation_machine_v1.ValueInputAssetWitness {
      source_kind: 0,
      key: #"01",
      next_schedule_hash: h41,
      descriptor_cbor: #"80",
      asset_index: 0,
      policy_id: #"42424242424242424242424242424242424242424242424242424242",
      asset_name: #"abcd",
      quantity: 5,
      asset_peaks: [],
      asset_siblings: [],
      mutation: value_mutation(),
    },
    validation_machine_v1.ValueOutputAssetWitness {
      output_index: 1,
      descriptor_cbor: #"80",
      asset_index: 0,
      policy_id: #"43434343434343434343434343434343434343434343434343434343",
      asset_name: #"beef",
      quantity: 7,
      asset_peaks: [],
      asset_siblings: [],
      mutation: value_mutation(),
    },
    validation_machine_v1.ValueMintAssetWitness {
      mint_index: 2,
      policy_id: #"44444444444444444444444444444444444444444444444444444444",
      asset_name: #"cafe",
      quantity: -5,
      siblings: [],
      mutation: value_mutation(),
    },
    validation_machine_v1.LedgerDeltaReplayWitness {
      source_kind: 1,
      key: #"02",
      next_schedule_hash: h45,
      value: #"03",
    },
    validation_machine_v1.LedgerDeltaOutputWitness {
      output_index: 3,
      descriptor_cbor: #"8100",
      siblings: [h46],
    },
    validation_machine_v1.LedgerDeltaProofFrameWitness {
      frame: proof_frame(),
      siblings: [h47],
    },
    validation_machine_v1.LedgerDeltaOperationWitness {
      operation_kind: 0,
      key: #"01",
      value: #"",
      operation_proof: validation_machine_v1.LedgerDeltaOperationProofV1 {
        descriptor: proof_descriptor(),
        operation_count: 0,
        operation_peaks: [],
        operation_index: 0,
        operation_siblings: [],
      },
    },
    validation_machine_v1.ValueOutputDescriptorWitness {
      output_index: 4,
      descriptor_cbor: #"8101",
      siblings: [h48],
    },
    validation_machine_v1.MintFoldAssetWitness {
      chunk_proof: chunk_proof(),
      next_chunk_proof: None,
    },
  ]
}

fn constructors_are_exact(
  items: List<validation_machine_v1.ValidationAuxiliaryWitnessV1>,
  tags: List<Int>,
  arities: List<Int>,
) -> Bool {
  when items is {
    [] -> and {
        tags == [],
        arities == [],
      }
    [item, ..rest] -> {
      expect [tag, ..remaining_tags] = tags
      expect [arity, ..remaining_arities] = arities
      expect Some(data) = cbor.deserialise(cbor.serialise(item))
      let Pair(actual_tag, fields) = builtin.un_constr_data(data)
      and {
        actual_tag == tag,
        list.length(fields) == arity,
        constructors_are_exact(rest, remaining_tags, remaining_arities),
      }
    }
  }
}

test validation_tail_auxiliary_v1_typescript_corpus_is_exact() {
  let items = tail_auxiliaries()
  and {
    constructors_are_exact(
      items,
      [24, 25, 26, 27, 28, 34, 35, 38, 39],
      [11, 9, 6, 4, 3, 2, 4, 3, 2],
    ),
    blake2b_256(cbor.serialise(items)) == #"8916ad7c26d34eafe62c93ed9c36be30d880fb102b918bb37f0b6d3dc27111e1",
  }
}

test value_accumulator_update_and_control_v1_abi_is_exact() {
  let accumulator = value_accumulator()
  let updated = validation_machine_v1.ValueAccumulatorUpdated(accumulator)
  let asset_limit = validation_machine_v1.ValueAccumulatorAssetLimitExceeded
  let mutation_invalid = validation_machine_v1.ValueAccumulatorMutationInvalid
  expect Some(updated_data) = cbor.deserialise(cbor.serialise(updated))
  expect Some(asset_limit_data) = cbor.deserialise(cbor.serialise(asset_limit))
  expect Some(mutation_invalid_data) = cbor.deserialise(
    cbor.serialise(mutation_invalid),
  )
  let Pair(updated_tag, updated_fields) = builtin.un_constr_data(updated_data)
  let Pair(asset_limit_tag, asset_limit_fields) = builtin.un_constr_data(
    asset_limit_data,
  )
  let Pair(mutation_invalid_tag, mutation_invalid_fields) = builtin.un_constr_data(
    mutation_invalid_data,
  )
  let control = validation_machine_v1.ValueAndMintControlV1 {
    native_control: native_control(),
    stage: 3,
    replay_schedule_hash: h55,
    replay_cursor: 4,
    replay_asset_cursor: 5,
    replay_value_hash: h56,
    replay_accumulator: h57,
    replay_remaining_schedule_hash: h58,
    output_cursor: 6,
    output_asset_cursor: 7,
    mint_cursor: 8,
    value_accumulator: accumulator,
  }
  let control_cbor = validation_machine_v1.encode_value_and_mint_control_v1(
    control,
  )
  and {
    validation_machine_v1.encode_value_accumulator_v1(accumulator) == expected_value_accumulator_cbor,
    updated_tag == 0,
    list.length(updated_fields) == 1,
    asset_limit_tag == 1,
    list.length(asset_limit_fields) == 0,
    mutation_invalid_tag == 2,
    list.length(mutation_invalid_fields) == 0,
    blake2b_256(control_cbor) == #"d30dfeaa4f1f3323bf2824a1051ef943fee27a31779e171678abe4c05ba2b2e0",
    when cbor.deserialise(control_cbor) is {
      None -> False
      Some(data) -> list.length(builtin.un_list_data(data)) == 12
    },
  }
}

fn pending_mutation() -> validation_machine_v1.LedgerDeltaPendingMutationV1 {
  validation_machine_v1.LedgerDeltaPendingMutationV1 {
    authorization_stage: 0,
    operation_kind: 1,
    key: #"0102",
    value: #"0304",
    descriptor: proof_descriptor(),
    fold_control: mpf_proof_fold_v1.ProofFoldControlV1 {
      next_frame_index: -1,
      expected_next_cursor: 0,
      including_root: #"",
      excluding_root: #"",
    },
  }
}

test ledger_delta_control_operation_and_frontier_v1_abi_is_exact() {
  let pending = pending_mutation()
  let control = validation_machine_v1.LedgerDeltaControlV1 {
    resolved_input_count: 0,
    resolved_inputs_accumulator: h61,
    output_count: 0,
    output_descriptor_peaks: [],
    stage: 1,
    replay_schedule_hash: h62,
    replay_cursor: 0,
    replay_accumulator: h63,
    replay_remaining_schedule_hash: h64,
    current_ledger_root: h65,
    output_cursor: 0,
    operation_count: 0,
    operation_peaks: [],
    pending_mutation_cbor: expected_pending_mutation_cbor,
  }
  let control_cbor = validation_machine_v1.encode_ledger_delta_control_v1(
    control,
  )
  let deletion = validation_machine_v1.ledger_delta_operation_leaf_hash(
    0,
    #"010203",
    #"",
    proof_descriptor(),
  )
  let insertion = validation_machine_v1.ledger_delta_operation_leaf_hash(
    1,
    #"0405",
    #"060708",
    proof_descriptor(),
  )
  let frontier = validation_merkle_v1.build_frontier([deletion, insertion])
  and {
    validation_machine_v1.encode_ledger_delta_pending_mutation_v1(pending) == expected_pending_mutation_cbor,
    blake2b_256(control_cbor) == #"92e07c0c935ac73750a521ed638aed060414828d766774495885b56a04f5481b",
    when cbor.deserialise(control_cbor) is {
      None -> False
      Some(data) -> list.length(builtin.un_list_data(data)) == 14
    },
    deletion == #"d70952a4347195627444cfbb1874f6857de1ad78f095460b76fc826cd267a589",
    insertion == #"f8bc7029f5f58f0436ebdf6cbbb85bd9adac05d5f6dc1b9238c8166a517aa8db",
    validation_merkle_v1.frontier_commitment(frontier.count, frontier.peaks) == #"b6d017c71f3fc974f620b22764385bf9ad56ee5627009e57dbeb9418e486dcb2",
  }
}

test terminal_acceptance_and_rejection_v1_typescript_vectors_are_exact() {
  let acceptance = validation_machine_v1.encode_terminal_acceptance_witness_v1(
    h72,
    2,
    [validation_merkle_v1.FrontierPeak { height: 1, hash: h71 }],
  )
  let rejection = validation_machine_v1.encode_terminal_rejection_witness(
    #"455f56414c55455f4e4f545f505245534552564544",
    h73,
  )
  and {
    acceptance == expected_terminal_acceptance_cbor,
    rejection == expected_terminal_rejection_cbor,
    validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      9,
      acceptance,
    ) == #"0b3defd802c8cc6ee1112724ef19532be5b8f61817ab0282a56db645a2b20948",
    validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      9,
      rejection,
    ) == #"6b15a4122dc6437ca54930248e9df11979d21dc484d5ea373373b43c489f1ce6",
  }
}

fn forced_source_membership() -> validation_claim_v1.ValidationSourceMembershipV1 {
  validation_claim_v1.ForcedValidationSource {
    membership: transition_trace.RootMembershipProof {
      domain: transition_trace.ForcedTransactionsV1RootDomain,
      root: #"1111111111111111111111111111111111111111111111111111111111111111",
      phas_root: #"1212121212121212121212121212121212121212121212121212121212121212",
      count: 1,
      key: OutputReference {
        transaction_id: #"1313131313131313131313131313131313131313131313131313131313131313",
        output_index: 4,
      },
      value: ledger_state.ForcedInclusionTxV1 {
        tx_id: #"1414141414141414141414141414141414141414141414141414141414141414",
        source: ledger_state.NativeTxProofSourceV1 {
          compact_cbor: #"8101",
          witness_set_compact_cbor: #"8102",
          field_preimage_lengths_cbor: #"8103",
        },
        operator_validity: ledger_state.FailedScript,
      },
      proof: [],
    },
  }
}

fn normal_source_membership() -> validation_claim_v1.ValidationSourceMembershipV1 {
  validation_claim_v1.NormalValidationSource {
    membership: transition_trace.RootMembershipProof {
      domain: transition_trace.TransactionsV1RootDomain,
      root: #"2121212121212121212121212121212121212121212121212121212121212121",
      phas_root: #"2222222222222222222222222222222222222222222222222222222222222222",
      count: 1,
      key: #"2323232323232323232323232323232323232323232323232323232323232323",
      value: ledger_state.L2TransactionSourceV1 {
        tx_id: #"2323232323232323232323232323232323232323232323232323232323232323",
        source: ledger_state.NativeTxProofSourceV1 {
          compact_cbor: #"820102",
          witness_set_compact_cbor: #"8104",
          field_preimage_lengths_cbor: #"8105",
        },
      },
      proof: [],
    },
  }
}

test validation_source_membership_v1_typescript_vectors_are_exact() {
  let forced = forced_source_membership()
  let normal = normal_source_membership()
  expect Some(forced_data) = cbor.deserialise(forced_source_membership_cbor)
  expect forced_decoded: validation_claim_v1.ValidationSourceMembershipV1 = forced_data
  expect Some(normal_data) = cbor.deserialise(normal_source_membership_cbor)
  expect normal_decoded: validation_claim_v1.ValidationSourceMembershipV1 = normal_data
  and {
    cbor.serialise(forced) == forced_source_membership_cbor,
    cbor.serialise(normal) == normal_source_membership_cbor,
    forced_decoded == forced,
    normal_decoded == normal,
    blake2b_256(cbor.serialise([forced, normal])) == #"5cba4fd71994ac1a802cd3a6edcda14f6acc7be43f7c1993839a65665b856f32",
  }
}

test validation_tail_auxiliary_v1_rejects_adjacent_tag_40() fail {
  expect Some(data) = cbor.deserialise(#"d9052180")
  expect _: validation_machine_v1.ValidationAuxiliaryWitnessV1 = data
  True
}

test validation_tail_auxiliary_v1_rejects_wrong_arity() fail {
  expect Some(data) = cbor.deserialise(#"d905149f00ff")
  expect _: validation_machine_v1.ValidationAuxiliaryWitnessV1 = data
  True
}

test validation_source_membership_v1_rejects_adjacent_tag() fail {
  expect Some(data) = cbor.deserialise(#"d87b80")
  expect _: validation_claim_v1.ValidationSourceMembershipV1 = data
  True
}

test validation_source_membership_v1_rejects_wrong_arity() fail {
  expect Some(data) = cbor.deserialise(#"d87980")
  expect _: validation_claim_v1.ValidationSourceMembershipV1 = data
  True
}

test validation_source_membership_v1_rejects_wrong_nesting() fail {
  expect Some(data) = cbor.deserialise(#"d8799fd87980ff")
  expect _: validation_claim_v1.ValidationSourceMembershipV1 = data
  True
}

test validation_source_membership_v1_rejects_swapped_kind() fail {
  let swapped = #"d87a"
    |> bytearray.concat(bytearray.drop(forced_source_membership_cbor, 2))
  expect Some(data) = cbor.deserialise(swapped)
  expect _: validation_claim_v1.ValidationSourceMembershipV1 = data
  True
}
