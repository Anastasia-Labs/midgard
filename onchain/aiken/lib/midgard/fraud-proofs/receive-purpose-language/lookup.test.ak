use aiken/cbor
use aiken/collection/list
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimedRedeemerScanV1, FinalStateV1, ImpossiblePurpose, RedeemerAbsent,
  RedeemerPresent, SourceAbsent, SourcePresent,
}
use midgard/fraud_proofs/direct_script_derivation/claim_adjudicate as adjudicate
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_redeemer_witness}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/fraud_proofs/receive_purpose_language/entry
use midgard/fraud_proofs/receive_purpose_language/lookup
use midgard/fraud_proofs/receive_purpose_language/terminal
use midgard/rejection_reason_v1 as reasons

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

fn at(kind: Int, index: Int) -> PurposeCoordinateV1 {
  PurposeCoordinateV1 { kind, index }
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

fn index_of(entry: Option<SourceV1>) -> source_index.StateV1 {
  when entry is {
    None -> source_index.initial_v1()
    Some(candidate) ->
      source_index.admit_v1(
        source_index.initial_v1(),
        candidate,
        source_index.First { proof: [] },
      )
  }
}

fn subject(
  tx_id: ByteArray,
  reason: Option<reasons.RejectionReasonV1>,
) -> VerdictSubjectV1 {
  when reason is {
    None ->
      VerdictSubjectV1 {
        version: 1,
        direction: 0,
        source_kind: 0,
        transaction_id: tx_id,
        source_key: #"",
        rejection_reason: None,
      }
    Some(_) ->
      VerdictSubjectV1 {
        version: 1,
        direction: 1,
        source_kind: 1,
        transaction_id: tx_id,
        source_key: #"00",
        rejection_reason: reason,
      }
  }
}

fn bound_of(
  tx_id: ByteArray,
  compact_cbor: ByteArray,
  reason: Option<reasons.RejectionReasonV1>,
  entry: Option<SourceV1>,
) -> BoundIndexV1 {
  BoundIndexV1 {
    bound: BoundV1 {
      subject: subject(tx_id, reason),
      compact_cbor,
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: root,
    },
    source_index: index_of(entry),
  }
}

fn selected(
  b: BoundIndexV1,
  coordinate: PurposeCoordinateV1,
  exists: Bool,
) -> SelectionV1 {
  SelectionV1 {
    bound: b.bound,
    source_index: b.source_index,
    coordinate,
    purpose: if exists {
      Some(PurposeV1 { coordinate, script_hash: hash, subject: hash })
    } else {
      None
    },
  }
}

fn synthetic(entry: Option<SourceV1>, exists: Bool) -> SelectionV1 {
  selected(bound_of(root, #"", None, entry), at(3, 1), exists)
}

// ## Resolve: the receive declaration selected no purpose

test resolve_names_an_ordinal_past_the_last_receive_purpose() {
  lookup.resolve_v1(synthetic(None, False)).claim == ImpossiblePurpose {
    coordinate: at(3, 1),
  }
}

test resolve_refuses_a_selected_purpose() fail {
  cbor.serialise(lookup.resolve_v1(synthetic(None, True))) != #""
}

// ## Lookup: a source decided without field 8

test lookup_proves_an_absent_source() {
  when lookup.lookup_v1(synthetic(None, True), None, []).claim is {
    SourceAbsent { selection } -> selection.source == None
    _ -> False
  }
}

test lookup_proves_a_native_or_midgard_source() {
  list.all(
    [0, 128],
    fn(language) {
      when
        lookup.lookup_v1(
          synthetic(Some(source(language)), True),
          Some(source(language)),
          [],
        ).claim
      is {
        SourcePresent { selection } ->
          selection.source == Some(source(language))
        _ -> False
      }
    },
  )
}

test lookup_refuses_a_plutus_v3_source() fail {
  cbor.serialise(
    lookup.lookup_v1(synthetic(Some(source(3)), True), Some(source(3)), []),
  ) != #""
}

test lookup_cannot_hide_a_present_source() fail {
  cbor.serialise(lookup.lookup_v1(synthetic(Some(source(3)), True), None, [])) != #""
}

test lookup_refuses_an_absent_purpose() fail {
  cbor.serialise(lookup.lookup_v1(synthetic(None, False), None, [])) != #""
}

// ## Lookup scan: a PlutusV3 source and its field-8 pointer

fn block(
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> fixture.NativeBlockFixtureV1 {
  let tx = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
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

fn opening_of(
  fixture_block: fixture.NativeBlockFixtureV1,
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> FieldOpeningV1 {
  fixture.inline_witness_opening(
    fixture_block.compact_cbor,
    fixture_block.witness_set_compact,
    list.map(witnesses, encode_midgard_redeemer_witness),
  )
}

fn scanned(
  initial: ClaimedRedeemerScanV1,
  checkpoint: ByteArray,
  opening: FieldOpeningV1,
) -> FinalStateV1 {
  let args = fn(bytes) {
    workers.ProgressArgsV1 {
      input_index: 0,
      output_index: 0,
      opening,
      checkpoint_bytes: bytes,
      budget: 16,
    }
  }
  let certified = adjudicate.claimed_certify_v1(
    initial,
    args(checkpoint),
    [],
    fixture.test_certificate_policy_id,
  )
  let ready = if certified.scan.scan.complete {
    certified
  } else {
    let (_, semantic, _) = cursor.certify_with_completion_at_v1(
      initial.scan.scan.cursor,
      8,
      opening,
      checkpoint,
      16,
      [],
      fixture.test_certificate_policy_id,
    )
    adjudicate.claimed_advance_v1(
      certified,
      args(semantic),
      [],
      fixture.test_certificate_policy_id,
    )
  }
  adjudicate.claimed_decision_v1(ready)
}

/// Receive purpose (3, 0) with a PlutusV3 source, through the lookup scan
/// and the claimed pointer scan, to the terminal's final state. Only an
/// accepted (source kind 0) subject carries field 8: a rejected subject is a
/// forced transaction, which has no redeemers, so the recorded-arm rows are
/// decided over synthetic final states in `terminal.test`.
fn route(
  witnesses: List<native_types.MidgardRedeemerWitness>,
  reason: Option<reasons.RejectionReasonV1>,
) -> FinalStateV1 {
  let fixture_block = block(witnesses)
  let b = bound_of(
    fixture_block.tx_id,
    fixture_block.compact_cbor,
    reason,
    Some(source(3)),
  )
  let selection = selected(b, at(3, 0), True)
  let opening = opening_of(fixture_block, witnesses)
  let initial = lookup.lookup_scan_v1(
    selection,
    Some(source(3)),
    [],
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, checkpoint) = redeemer_scan.initial_scan_v1(
    initial.scan.authenticated,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  scanned(initial, checkpoint, opening)
}

test scan_with_the_receive_redeemer_convicts_an_accepted_leaf() {
  let state = route([witness(native_types.ReceiveRedeemer, 0)], None)
  and {
    when state.claim is {
      RedeemerPresent { .. } -> True
      _ -> False
    },
    terminal.convicts_v1(state),
  }
}

test scan_without_the_receive_redeemer_refuses_an_accepted_leaf() {
  let state = route([], None)
  and {
    when state.claim is {
      RedeemerAbsent { decision } -> decision.redeemer_missing
      _ -> False
    },
    !terminal.convicts_v1(state),
  }
}

test scan_ignores_redeemers_of_other_purposes() {
  !terminal.convicts_v1(
    route(
      [
        witness(native_types.ReceiveRedeemer, 1),
        witness(native_types.SpendRedeemer, 0),
        witness(native_types.RewardRedeemer, 0),
      ],
      None,
    ),
  )
}

test lookup_scan_refuses_another_language() fail {
  let fixture_block = block([])
  let b = bound_of(
    fixture_block.tx_id,
    fixture_block.compact_cbor,
    None,
    Some(source(128)),
  )
  cbor.serialise(
    lookup.lookup_scan_v1(
      selected(b, at(3, 0), True),
      Some(source(128)),
      [],
      opening_of(fixture_block, []),
      [],
      fixture.test_certificate_policy_id,
    ),
  ) != #""
}

test lookup_scan_refuses_an_absent_source() fail {
  let fixture_block = block([])
  let b = bound_of(fixture_block.tx_id, fixture_block.compact_cbor, None, None)
  cbor.serialise(
    lookup.lookup_scan_v1(
      selected(b, at(3, 0), True),
      None,
      [],
      opening_of(fixture_block, []),
      [],
      fixture.test_certificate_policy_id,
    ),
  ) != #""
}

// ## Family entry contradiction

fn entry_bound(reason: Option<reasons.RejectionReasonV1>) -> BoundIndexV1 {
  bound_of(root, #"", reason, None)
}

fn arm(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(
    reasons.ReceivePurposePlutusV3Forbidden {
      purpose_kind: kind,
      purpose_index: index,
    },
  )
}

test contradiction_names_a_non_receive_coordinate() {
  and {
    entry.contradiction_v1(entry_bound(arm(0, 0))).claim == ImpossiblePurpose {
      coordinate: at(0, 0),
    },
    entry.contradiction_v1(entry_bound(arm(3, -1))).claim == ImpossiblePurpose {
      coordinate: at(3, -1),
    },
    entry.contradiction_v1(entry_bound(arm(7, 2))).claim == ImpossiblePurpose {
      coordinate: at(7, 2),
    },
  }
}

test contradiction_refuses_a_receive_coordinate() fail {
  cbor.serialise(entry.contradiction_v1(entry_bound(arm(3, 0)))) != #""
}

test contradiction_refuses_another_reason() fail {
  cbor.serialise(
    entry.contradiction_v1(
      entry_bound(
        Some(
          reasons.PlutusExecutionFailed { purpose_kind: 0, purpose_index: 0 },
        ),
      ),
    ),
  ) != #""
}

test contradiction_refuses_an_accepted_leaf() fail {
  cbor.serialise(entry.contradiction_v1(entry_bound(None))) != #""
}
