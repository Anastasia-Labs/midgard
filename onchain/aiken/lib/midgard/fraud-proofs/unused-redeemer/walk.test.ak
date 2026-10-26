use aiken/collection/list
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, FinalStateV1, RedeemerPointer,
}
use midgard/fraud_proofs/direct_script_derivation/claim_adjudicate as adjudicate
use midgard/fraud_proofs/direct_script_derivation/claim_purpose.{
  ExistingPurpose,
}
use midgard/fraud_proofs/direct_script_derivation/claim_start as start
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_redeemer_witness}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/fraud_proofs/unused_redeemer/walk
use midgard/rejection_reason_v1 as reasons

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

// ## Pointer coordinates

test pointer_tags_name_their_purpose_kinds() {
  and {
    walk.coordinate_of_pointer_v1(0, 4) == PurposeCoordinateV1 {
      kind: 0,
      index: 4,
    },
    walk.coordinate_of_pointer_v1(1, 4) == PurposeCoordinateV1 {
      kind: 1,
      index: 4,
    },
    walk.coordinate_of_pointer_v1(3, 4) == PurposeCoordinateV1 {
      kind: 2,
      index: 4,
    },
    walk.coordinate_of_pointer_v1(6, 4) == PurposeCoordinateV1 {
      kind: 3,
      index: 4,
    },
  }
}

test pointer_tags_without_a_purpose_name_an_invalid_kind() {
  list.all(
    [2, 4, 5, 7],
    fn(tag) { walk.coordinate_of_pointer_v1(tag, 0).kind == -1 },
  )
}

// ## Fixtures over authenticated transaction fields
//
// Observers are required observer script hashes (purpose kind 2, pointer
// tag 3). The source index holds at most one source, for `hash`.

fn block(
  observers: List<ByteArray>,
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> fixture.NativeBlockFixtureV1 {
  let tx = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        required_observers: observers,
      },
      witness_set: native_types.MidgardTransactionWitnessSet {
        ..tx.witness_set,
        redeemer_tx_wits: witnesses,
      },
    },
  )
}

fn witness(
  purpose: native_types.MidgardRedeemerPurpose,
  index: Int,
) -> native_types.MidgardRedeemerWitness {
  native_types.MidgardRedeemerWitness {
    purpose,
    index,
    redeemer_cbor: #"00",
    execution_units: native_types.MidgardExecutionUnits { memory: 1, steps: 2 },
  }
}

fn reward(index: Int) -> native_types.MidgardRedeemerWitness {
  witness(native_types.RewardRedeemer, index)
}

fn source(language: Int) -> SourceV1 {
  SourceV1 {
    field_index: 6,
    item_index: 2,
    source_key: #"02",
    language_tag: language,
    script_hash: hash,
    total_length: 8,
    item_commitment: root,
  }
}

/// The walk and claim custody never read the leaf's verdict, so the route
/// runs under the accepted subject the fixture's openings anchor to;
/// `with_verdict` then states the leaf the adjudication judges.
fn bound(
  fixture_block: fixture.NativeBlockFixtureV1,
  entry: Option<SourceV1>,
) -> BoundIndexV1 {
  BoundIndexV1 {
    bound: BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction: 0,
        source_kind: 0,
        transaction_id: fixture_block.tx_id,
        source_key: #"",
        rejection_reason: None,
      },
      compact_cbor: fixture_block.compact_cbor,
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: root,
    },
    source_index: when entry is {
      None -> source_index.initial_v1()
      Some(candidate) ->
        source_index.admit_v1(
          source_index.initial_v1(),
          candidate,
          source_index.First { proof: [] },
        )
    },
  }
}

fn redeemer_opening(
  fixture_block: fixture.NativeBlockFixtureV1,
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> FieldOpeningV1 {
  fixture.inline_witness_opening(
    fixture_block.compact_cbor,
    fixture_block.witness_set_compact,
    list.map(witnesses, encode_midgard_redeemer_witness),
  )
}

fn args(opening: FieldOpeningV1, bytes: ByteArray) -> workers.ProgressArgsV1 {
  workers.ProgressArgsV1 {
    input_index: 0,
    output_index: 0,
    opening,
    checkpoint_bytes: bytes,
    budget: 16,
  }
}

/// Family entry, one grammar batch and one walk batch over field 8.
fn walked(
  fixture_block: fixture.NativeBlockFixtureV1,
  witnesses: List<native_types.MidgardRedeemerWitness>,
  state: BoundIndexV1,
  redeemer_index: Int,
  pointer: Pair<Int, Int>,
) -> walk.WalkStateV1 {
  let opening = redeemer_opening(fixture_block, witnesses)
  let opened = walk.open_v1(
    state,
    walk.EntryArgsV1 {
      input_index: 0,
      output_index: 0,
      redeemer_index,
      pointer_tag: pointer.1st,
      pointer_index: pointer.2nd,
      opening,
    },
    [],
    fixture.test_certificate_policy_id,
  )
  let (_, grammar, _) = cursor.initial_redeemers_v1(
    state.bound,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let certified = walk.certify_v1(
    opened,
    args(opening, grammar),
    [],
    fixture.test_certificate_policy_id,
  )
  if certified.complete {
    certified
  } else {
    let (_, semantic, _) = cursor.certify_with_completion_at_v1(
      opened.cursor,
      8,
      opening,
      grammar,
      16,
      [],
      fixture.test_certificate_policy_id,
    )
    walk.advance_v1(
      certified,
      args(opening, semantic),
      [],
      fixture.test_certificate_policy_id,
    )
  }
}

/// The whole route from family entry to the terminal's final state.
fn route(
  observers: List<ByteArray>,
  witnesses: List<native_types.MidgardRedeemerWitness>,
  entry: Option<SourceV1>,
  reason: Option<reasons.RejectionReasonV1>,
  redeemer_index: Int,
  pointer: Pair<Int, Int>,
) -> FinalStateV1 {
  let fixture_block = block(observers, witnesses)
  let state = bound(fixture_block, entry)
  let read = walked(fixture_block, witnesses, state, redeemer_index, pointer)
  let final_state = if read.reached && !read.repeated {
    let context = walk.claim_context_v1(read)
    let purposes = if pointer.1st == 3 {
      start.observe_v1(
        context,
        fixture.inline_body_opening(fixture_block.compact_cbor, observers),
        [],
        fixture.test_certificate_policy_id,
      )
    } else {
      start.complete_v1(context)
    }
    when purposes.purpose is {
      ExistingPurpose { .. } -> adjudicate.lookup_v1(purposes, entry, [])
      _ -> adjudicate.resolve_v1(purposes)
    }
  } else {
    walk.final_state_v1(read)
  }
  with_verdict(final_state, reason)
}

/// A rejected leaf as the canonical subject carries it: a forced source with
/// its key, judged as a wrongful rejection.
fn with_verdict(
  final_state: FinalStateV1,
  reason: Option<reasons.RejectionReasonV1>,
) -> FinalStateV1 {
  when reason is {
    None -> final_state
    Some(_) -> {
      let context = final_state.context
      let index = context.bound
      let subject = index.bound.subject
      FinalStateV1 {
        ..final_state,
        context: ClaimContextV1 {
          ..context,
          bound: BoundIndexV1 {
            ..index,
            bound: BoundV1 {
              ..index.bound,
              subject: VerdictSubjectV1 {
                ..subject,
                direction: 1,
                source_kind: 1,
                source_key: #"01",
                rejection_reason: reason,
              },
            },
          },
        },
      }
    }
  }
}

fn unused(index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(reasons.UnusedRedeemer { redeemer_index: index })
}

fn reward_pointer(index: Int) -> Pair<Int, Int> {
  Pair(3, index)
}

// ## The walk

test walk_hands_a_first_pointer_to_claim_custody() {
  let witnesses = [reward(1), reward(0)]
  let fixture_block = block([hash], witnesses)
  let state = bound(fixture_block, Some(source(3)))
  let read = walked(fixture_block, witnesses, state, 1, reward_pointer(0))
  and {
    read.reached,
    !read.repeated,
    walk.claim_context_v1(read) == ClaimContextV1 {
      bound: state,
      observed: None,
      target: RedeemerPointer {
        redeemer_index: 1,
        coordinate: PurposeCoordinateV1 { kind: 2, index: 0 },
      },
    },
  }
}

test walk_stops_at_the_named_redeemer() {
  let witnesses = [reward(0), reward(0)]
  let fixture_block = block([hash], witnesses)
  let state = bound(fixture_block, Some(source(3)))
  let read = walked(fixture_block, witnesses, state, 0, reward_pointer(0))
  and {
    read.reached,
    !read.repeated,
    !read.complete,
  }
}

test walk_refuses_a_misdeclared_pointer() fail {
  let witnesses = [reward(0)]
  let fixture_block = block([hash], witnesses)
  let state = bound(fixture_block, Some(source(3)))
  walked(fixture_block, witnesses, state, 0, reward_pointer(1)).reached
}

test walk_refuses_a_misdeclared_tag() fail {
  let witnesses = [reward(0)]
  let fixture_block = block([hash], witnesses)
  let state = bound(fixture_block, Some(source(3)))
  walked(fixture_block, witnesses, state, 0, Pair(1, 0)).reached
}

test repeated_pointer_needs_no_purpose_lookup() {
  let state = route(
    [hash],
    [reward(0), witness(native_types.MintRedeemer, 0), reward(0)],
    Some(source(3)),
    None,
    2,
    reward_pointer(0),
  )
  state.claim == claim.RedeemerRepeated
}

test exhausted_field_before_the_index_is_out_of_range() {
  and {
    route([hash], [reward(0)], Some(source(3)), None, 1, reward_pointer(0)).claim == claim.RedeemerOutOfRange,
    route([hash], [], Some(source(3)), None, 0, reward_pointer(0)).claim == claim.RedeemerOutOfRange,
  }
}

test unread_redeemer_cannot_finish() fail {
  let witnesses = [reward(0)]
  let fixture_block = block([hash], witnesses)
  let state = bound(fixture_block, Some(source(3)))
  let opened = walk.open_v1(
    state,
    walk.EntryArgsV1 {
      input_index: 0,
      output_index: 0,
      redeemer_index: 0,
      pointer_tag: 3,
      pointer_index: 0,
      opening: redeemer_opening(fixture_block, witnesses),
    },
    [],
    fixture.test_certificate_policy_id,
  )
  walk.final_state_v1(opened).claim == claim.RedeemerOutOfRange
}

test first_pointer_cannot_finish_as_a_repeat() fail {
  let witnesses = [reward(0)]
  let fixture_block = block([hash], witnesses)
  let state = bound(fixture_block, Some(source(3)))
  let read = walked(fixture_block, witnesses, state, 0, reward_pointer(0))
  walk.final_state_v1(read).claim == claim.RedeemerRepeated
}

test repeated_pointer_cannot_reach_claim_custody() fail {
  let witnesses = [reward(0), reward(0)]
  let fixture_block = block([hash], witnesses)
  let state = bound(fixture_block, Some(source(3)))
  let read = walked(fixture_block, witnesses, state, 1, reward_pointer(0))
  walk.claim_context_v1(read).observed == None
}

// ## Accepted leaves: any unused redeemer convicts

test accepted_leaf_convicts_on_a_repeated_pointer() {
  adjudicate.convicts_v1(
    route(
      [hash],
      [reward(0), reward(0)],
      Some(source(3)),
      None,
      1,
      reward_pointer(0),
    ),
  )
}

test accepted_leaf_convicts_on_a_native_purpose_pointer() {
  let state = route(
    [hash],
    [reward(0)],
    Some(source(0)),
    None,
    0,
    reward_pointer(0),
  )
  and {
    when state.claim is {
      claim.NativeSource { .. } -> True
      _ -> False
    },
    adjudicate.convicts_v1(state),
  }
}

test accepted_leaf_convicts_on_a_pointer_past_its_domain() {
  let state = route(
    [hash],
    [reward(1)],
    Some(source(3)),
    None,
    0,
    reward_pointer(1),
  )
  and {
    state.claim == claim.ImpossiblePurpose {
      coordinate: PurposeCoordinateV1 { kind: 2, index: 1 },
    },
    adjudicate.convicts_v1(state),
  }
}

test accepted_leaf_convicts_on_a_tag_without_a_purpose() {
  let state = route(
    [hash],
    [witness(native_types.CertRedeemer, 0)],
    Some(source(3)),
    None,
    0,
    Pair(2, 0),
  )
  and {
    state.claim == claim.ImpossiblePurpose {
      coordinate: PurposeCoordinateV1 { kind: -1, index: 0 },
    },
    adjudicate.convicts_v1(state),
  }
}

test accepted_leaf_is_not_convicted_by_a_consumed_redeemer() {
  let state = route(
    [hash],
    [reward(0)],
    Some(source(3)),
    None,
    0,
    reward_pointer(0),
  )
  and {
    when state.claim is {
      claim.SourcePresent { .. } -> True
      _ -> False
    },
    !adjudicate.convicts_v1(state),
  }
}

test accepted_leaf_is_not_convicted_by_a_purpose_without_a_source() {
  let state = route([hash], [reward(0)], None, None, 0, reward_pointer(0))
  and {
    when state.claim is {
      claim.SourceAbsent { .. } -> True
      _ -> False
    },
    !adjudicate.convicts_v1(state),
  }
}

test accepted_leaf_is_not_convicted_out_of_range() {
  !adjudicate.convicts_v1(
    route([hash], [reward(0)], Some(source(3)), None, 3, reward_pointer(0)),
  )
}

// ## A leaf naming the walked redeemer

test false_unused_leaf_for_a_consumed_redeemer_convicts() {
  adjudicate.convicts_v1(
    route(
      [hash],
      [reward(0)],
      Some(source(128)),
      unused(0),
      0,
      reward_pointer(0),
    ),
  )
}

test false_unused_leaf_past_the_field_convicts() {
  adjudicate.convicts_v1(
    route([hash], [reward(0)], Some(source(3)), unused(1), 1, reward_pointer(0)),
  )
}

test honest_unused_leaf_is_refused() {
  and {
    !adjudicate.convicts_v1(
      route(
        [hash],
        [reward(0), reward(0)],
        Some(source(3)),
        unused(1),
        1,
        reward_pointer(0),
      ),
    ),
    !adjudicate.convicts_v1(
      route(
        [hash],
        [reward(0)],
        Some(source(0)),
        unused(0),
        0,
        reward_pointer(0),
      ),
    ),
    !adjudicate.convicts_v1(
      route(
        [hash],
        [reward(1)],
        Some(source(3)),
        unused(0),
        0,
        reward_pointer(1),
      ),
    ),
  }
}

test unused_leaf_on_a_purpose_without_a_source_is_left_to_its_family() {
  !adjudicate.convicts_v1(
    route([hash], [reward(0)], None, unused(0), 0, reward_pointer(0)),
  )
}

// ## Another leaf: the earlier audit position wins

test earlier_unused_redeemer_convicts_a_later_unused_leaf() {
  adjudicate.convicts_v1(
    route(
      [hash],
      [reward(1), reward(0), reward(0)],
      Some(source(3)),
      unused(2),
      0,
      reward_pointer(1),
    ),
  )
}

test later_unused_redeemer_admits_an_earlier_unused_leaf() {
  !adjudicate.convicts_v1(
    route(
      [hash],
      [reward(0), reward(0), reward(1)],
      Some(source(3)),
      unused(1),
      2,
      reward_pointer(1),
    ),
  )
}

test unused_witness_leaf_precedes_every_unused_redeemer() {
  !adjudicate.convicts_v1(
    route(
      [hash],
      [reward(1)],
      Some(source(3)),
      Some(reasons.UnusedScriptWitness { script_index: 4 }),
      0,
      reward_pointer(1),
    ),
  )
}

test purpose_reason_precedes_every_unused_redeemer() {
  !adjudicate.convicts_v1(
    route(
      [hash],
      [reward(1)],
      Some(source(3)),
      Some(reasons.RedeemerMissing { purpose_kind: 2, purpose_index: 0 }),
      0,
      reward_pointer(1),
    ),
  )
}

test later_phase_reason_convicts_on_an_unused_redeemer() {
  adjudicate.convicts_v1(
    route(
      [hash],
      [reward(1)],
      Some(source(3)),
      Some(reasons.ScriptIntegrityHashMismatch),
      0,
      reward_pointer(1),
    ),
  )
}

test later_phase_reason_is_not_convicted_by_a_consumed_redeemer() {
  !adjudicate.convicts_v1(
    route(
      [hash],
      [reward(0)],
      Some(source(3)),
      Some(reasons.ScriptIntegrityHashMismatch),
      0,
      reward_pointer(0),
    ),
  )
}
