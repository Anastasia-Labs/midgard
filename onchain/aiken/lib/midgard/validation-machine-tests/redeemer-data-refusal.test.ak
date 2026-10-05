use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_frame_v1
use midgard/cek_data_integer_v1
use midgard/cek_data_traverse_v1
use midgard/fraud_proofs/native_tx/components.{encode_midgard_redeemer_witness}
use midgard/fraud_proofs/native_tx/types.{
  MidgardExecutionUnits, MidgardRedeemerWitness, MidgardTransaction,
  MidgardTransactionWitnessSet, SpendRedeemer,
}
use midgard/redeemer_item_proof_v1.{RedeemerItemProofAdvanced}
use midgard/rejection_reason_v1
use midgard/validation_machine/machine_types
use midgard/validation_machine/one_step
use midgard/validation_machine_tests/fixtures_phase_a.{
  empty_v1_transaction, exact_rejection_post, inline_field_door,
  script_sources_pending_redeemer_item_fixture, singleton_field_item_chunk_proof,
}
use midgard/validation_merkle_v1

fn traverse(
  control: redeemer_item_proof_v1.RedeemerItemProofControlV1,
  header: redeemer_item_proof_v1.RedeemerItemProofWitnessV1,
  action: cek_data_traverse_v1.DataTraverseActionV1,
) {
  expect Some(RedeemerItemProofAdvanced { control: next }) = redeemer_item_proof_v1.step_v1(
    control,
    redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
      ..header,
      chunk_proof: if redeemer_item_proof_v1.next_source_span_v1(control) == None {
        None
      } else {
        header.chunk_proof
      },
      action: redeemer_item_proof_v1.RedeemerItemTraverseData { action },
    },
  )
  next
}

fn finish_integer(
  control: redeemer_item_proof_v1.RedeemerItemProofControlV1,
  header: redeemer_item_proof_v1.RedeemerItemProofWitnessV1,
) {
  expect Some(traversal) = control.traversal
  expect Some(integer) = traversal.integer
  if integer.stage == cek_data_integer_v1.stage_terminal {
    control
  } else {
    finish_integer(
      traverse(control, header, cek_data_traverse_v1.NoAction),
      header,
    )
  }
}

fn refusal_evidence(data: ByteArray) {
  let base = empty_v1_transaction()
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: data,
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let item = encode_midgard_redeemer_witness(redeemer)
  let proof = singleton_field_item_chunk_proof(8, item)
  let initial = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_data,
    0,
    1,
    bytearray.length(item),
    bounded_item_v1.from_bytes(8, 0, item),
    -1,
    -1,
  )
  let header = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenHeader,
    chunk_proof: Some(proof),
    next_chunk_proof: None,
  }
  expect Some(RedeemerItemProofAdvanced { control: tail }) = redeemer_item_proof_v1.step_v1(
    initial,
    header,
  )
  expect Some(RedeemerItemProofAdvanced { control: head }) = redeemer_item_proof_v1.step_v1(
    tail,
    redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
      ..header,
      action: redeemer_item_proof_v1.RedeemerItemOpenTail,
    },
  )
  let is_later = data == #"d8799f01580100ff" || data == #"d8799f01810102ff" || data == #"d8799f019f01ff02ff" || data == #"d8799f011801ff" || data == #"d8799f0101ff"
  let head = if is_later || data == #"d8799f580100ff" || data == #"d8799f8101ff" || data == #"d8799f9f01ffff" || data == #"d8799f1801ff" || data == #"d8799f01ff" {
    let nested = traverse(head, header, cek_data_traverse_v1.HeadSequence)
    if is_later {
      let scalar = traverse(nested, header, cek_data_traverse_v1.HeadScalar)
      let blob = finish_integer(scalar, header)
      traverse(
        blob,
        header,
        cek_data_traverse_v1.AttachScalar {
          parent: Some(cek_data_frame_v1.initial_small_constr_frame_v1(0, #"")),
        },
      )
    } else {
      nested
    }
  } else if data == #"d8668218808101" || data == #"d8668218809f01ff" {
    let constructor = traverse(
      head,
      header,
      cek_data_traverse_v1.HeadLargeConstructor,
    )
    let integer = finish_integer(constructor, header)
    traverse(integer, header, cek_data_traverse_v1.NoAction)
  } else {
    head
  }
  let Pair(pre, work_witness_cbor) = script_sources_pending_redeemer_item_fixture(
    tx,
    validation_merkle_v1.build_frontier([]),
    1,
    redeemer_item_proof_v1.hash_control_v1(head),
    12,
  )
  let auxiliary = machine_types.RedeemerItemStepWitness {
    redeemer_control: None,
    control: head,
    witness: redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
      ..header,
      action: redeemer_item_proof_v1.RedeemerItemTraverseData {
        action: cek_data_traverse_v1.NoAction,
      },
    },
  }
  Pair(
    pre,
    machine_types.ValidationOneStepEvidenceV1 {
      transition: machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: exact_rejection_post(
          pre,
          rejection_reason_v1.reject_invalid_field_type,
        ),
      },
      auxiliary,
    },
  )
}

test constructor_data_refusal_has_one_exact_rejected_successor() {
  let Pair(pre, evidence) = refusal_evidence(#"d8798101")
  and {
    one_step.verify_one_step_evidence(pre, evidence, inline_field_door()),
    !one_step.verify_one_step_evidence(
      pre,
      machine_types.ValidationOneStepEvidenceV1 {
        ..evidence,
        transition: machine_types.ValidationOneStepWitnessV1 {
          ..evidence.transition,
          claimed_successor: exact_rejection_post(
            pre,
            rejection_reason_v1.reject_missing_required_witness,
          ),
        },
      },
      inline_field_door(),
    ),
    !one_step.verify_one_step_evidence(
      pre,
      machine_types.ValidationOneStepEvidenceV1 {
        ..evidence,
        transition: machine_types.ValidationOneStepWitnessV1 {
          ..evidence.transition,
          claimed_successor: pre,
        },
      },
      inline_field_door(),
    ),
  }
}

test honest_constructor_cannot_take_refusal_rejected_successor() {
  let Pair(pre, evidence) = refusal_evidence(#"d8799f01ff")
  !one_step.verify_one_step_evidence(pre, evidence, inline_field_door())
}

test nested_sequence_refusal_has_exact_successor_and_refuses_truncated_view() {
  let Pair(pre, evidence) = refusal_evidence(#"d8799f8101ff")
  expect machine_types.RedeemerItemStepWitness { control, .. } = evidence.auxiliary
  expect Some(traversal) = control.traversal
  and {
    traversal.offset == 3,
    cek_data_traverse_v1.noncanonical_definite_sequence_head_v1(
      traversal,
      Some(#"8101ff"),
    ),
    !cek_data_traverse_v1.noncanonical_definite_sequence_head_v1(
      traversal,
      Some(#"8101"),
    ),
    one_step.verify_one_step_evidence(pre, evidence, inline_field_door()),
    !one_step.verify_one_step_evidence(
      pre,
      machine_types.ValidationOneStepEvidenceV1 {
        ..evidence,
        transition: machine_types.ValidationOneStepWitnessV1 {
          ..evidence.transition,
          claimed_successor: pre,
        },
      },
      inline_field_door(),
    ),
  }
}

test honest_nested_sequence_cannot_take_refusal_successor() {
  let Pair(pre, evidence) = refusal_evidence(#"d8799f9f01ffff")
  !one_step.verify_one_step_evidence(pre, evidence, inline_field_door())
}

test later_nested_sequence_refusal_has_exact_successor_and_refuses_truncated_view() {
  let Pair(pre, evidence) = refusal_evidence(#"d8799f01810102ff")
  expect machine_types.RedeemerItemStepWitness { control, .. } = evidence.auxiliary
  expect Some(traversal) = control.traversal
  and {
    traversal.offset == 4,
    traversal.stage == cek_data_traverse_v1.stage_close,
    cek_data_traverse_v1.noncanonical_definite_sequence_head_v1(
      traversal,
      Some(#"810102ff"),
    ),
    !cek_data_traverse_v1.noncanonical_definite_sequence_head_v1(
      traversal,
      Some(#"8101"),
    ),
    one_step.verify_one_step_evidence(pre, evidence, inline_field_door()),
    !one_step.verify_one_step_evidence(
      pre,
      machine_types.ValidationOneStepEvidenceV1 {
        ..evidence,
        transition: machine_types.ValidationOneStepWitnessV1 {
          ..evidence.transition,
          claimed_successor: pre,
        },
      },
      inline_field_door(),
    ),
  }
}

test honest_later_nested_sequence_cannot_take_refusal_successor() {
  let Pair(pre, evidence) = refusal_evidence(#"d8799f019f01ff02ff")
  expect machine_types.RedeemerItemStepWitness { control, .. } = evidence.auxiliary
  expect Some(traversal) = control.traversal
  and {
    traversal.offset == 4,
    traversal.stage == cek_data_traverse_v1.stage_close,
    !cek_data_traverse_v1.noncanonical_definite_sequence_head_v1(
      traversal,
      Some(#"9f01ff02ff"),
    ),
    !cek_data_traverse_v1.noncanonical_definite_sequence_head_v1(
      traversal,
      Some(#"8101"),
    ),
    !one_step.verify_one_step_evidence(pre, evidence, inline_field_door()),
  }
}

fn exact_malformed_successor(data: ByteArray, offset: Int, stage: Int) -> Bool {
  let Pair(pre, evidence) = refusal_evidence(data)
  expect machine_types.RedeemerItemStepWitness { control, witness, .. } = evidence.auxiliary
  expect Some(traversal) = control.traversal
  and {
    traversal.offset == offset,
    traversal.stage == stage,
    one_step.verify_one_step_evidence(pre, evidence, inline_field_door()),
    !one_step.verify_one_step_evidence(
      pre,
      machine_types.ValidationOneStepEvidenceV1 {
        ..evidence,
        transition: machine_types.ValidationOneStepWitnessV1 {
          ..evidence.transition,
          claimed_successor: exact_rejection_post(
            pre,
            rejection_reason_v1.reject_missing_required_witness,
          ),
        },
      },
      inline_field_door(),
    ),
    redeemer_item_proof_v1.step_v1(
      control,
      redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
        ..witness,
        chunk_proof: None,
      },
    ) == None,
    redeemer_item_proof_v1.step_v1(
      control,
      redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
        ..witness,
        next_chunk_proof: witness.chunk_proof,
      },
    ) == None,
    !one_step.verify_one_step_evidence(
      pre,
      machine_types.ValidationOneStepEvidenceV1 {
        ..evidence,
        transition: machine_types.ValidationOneStepWitnessV1 {
          ..evidence.transition,
          claimed_successor: pre,
        },
      },
      inline_field_door(),
    ),
  }
}

test nonminimal_scalar_has_exact_rejected_successor() {
  exact_malformed_successor(#"1801", 0, cek_data_traverse_v1.stage_head)
}

test first_nested_nonminimal_scalar_has_exact_rejected_successor() {
  exact_malformed_successor(#"d8799f1801ff", 3, cek_data_traverse_v1.stage_head)
}

test later_nonminimal_scalar_has_exact_rejected_successor() {
  exact_malformed_successor(
    #"d8799f011801ff",
    4,
    cek_data_traverse_v1.stage_close,
  )
}

test indefinite_map_has_exact_rejected_successor() {
  exact_malformed_successor(#"bf0101ff", 0, cek_data_traverse_v1.stage_head)
}

test large_constructor_fields_have_exact_rejected_successor() {
  exact_malformed_successor(
    #"d8668218808101",
    5,
    cek_data_traverse_v1.stage_large_fields,
  )
}

test canonical_scalar_and_map_cannot_take_refusal_successor() {
  let Pair(scalar_pre, scalar) = refusal_evidence(#"01")
  let Pair(map_pre, map) = refusal_evidence(#"a10101")
  and {
    !one_step.verify_one_step_evidence(scalar_pre, scalar, inline_field_door()),
    !one_step.verify_one_step_evidence(map_pre, map, inline_field_door()),
  }
}

test canonical_later_scalar_and_large_fields_cannot_take_refusal_successor() {
  let Pair(scalar_pre, scalar) = refusal_evidence(#"d8799f0101ff")
  let Pair(large_pre, large) = refusal_evidence(#"d8668218809f01ff")
  and {
    !one_step.verify_one_step_evidence(scalar_pre, scalar, inline_field_door()),
    !one_step.verify_one_step_evidence(large_pre, large, inline_field_door()),
  }
}

test nonminimal_byte_length_has_exact_rejected_successor() {
  exact_malformed_successor(#"580100", 0, cek_data_traverse_v1.stage_head)
}

test nonminimal_byte_length_widths_have_exact_rejected_successor() {
  and {
    exact_malformed_successor(#"59000100", 0, cek_data_traverse_v1.stage_head),
    exact_malformed_successor(
      #"5a0000000100",
      0,
      cek_data_traverse_v1.stage_head,
    ),
    exact_malformed_successor(
      #"5b000000000000000100",
      0,
      cek_data_traverse_v1.stage_head,
    ),
  }
}

test nested_byte_length_has_exact_rejected_successor() {
  and {
    exact_malformed_successor(
      #"d8799f580100ff",
      3,
      cek_data_traverse_v1.stage_head,
    ),
    exact_malformed_successor(
      #"d8799f01580100ff",
      4,
      cek_data_traverse_v1.stage_close,
    ),
  }
}

test nonminimal_map_and_tag_have_exact_rejected_successor() {
  and {
    exact_malformed_successor(#"b800", 0, cek_data_traverse_v1.stage_head),
    exact_malformed_successor(#"b8010101", 0, cek_data_traverse_v1.stage_head),
    exact_malformed_successor(#"d9007980", 0, cek_data_traverse_v1.stage_head),
  }
}

test canonical_byte_map_and_tag_cannot_take_refusal_successor() {
  let Pair(bytes_pre, bytes) = refusal_evidence(#"4100")
  let Pair(map_pre, map) = refusal_evidence(#"a0")
  let Pair(tag_pre, tag) = refusal_evidence(#"d87980")
  and {
    !one_step.verify_one_step_evidence(bytes_pre, bytes, inline_field_door()),
    !one_step.verify_one_step_evidence(map_pre, map, inline_field_door()),
    !one_step.verify_one_step_evidence(tag_pre, tag, inline_field_door()),
  }
}
