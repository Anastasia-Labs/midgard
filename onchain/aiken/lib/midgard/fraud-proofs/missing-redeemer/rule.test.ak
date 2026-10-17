use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  SourceSelectionV1,
}
use midgard/fraud_proofs/direct_script_derivation/redeemer
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/missing_redeemer/rule
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

fn state(found: Bool, complete: Bool) -> rule.ScanStateV1 {
  let coordinate = PurposeCoordinateV1 { kind: 3, index: 7 }
  let bound = BoundV1 {
    subject: substrate.VerdictSubjectV1 {
      version: 1,
      direction: 0,
      source_kind: 0,
      transaction_id: root,
      source_key: #"",
      rejection_reason: None,
    },
    compact_cbor: #"",
    network_order_context: NetworkOrderContextV1 {
      body_network_mismatch: False,
    },
    event_pre_root: root,
  }
  rule.ScanStateV1 {
    authenticated: SourceSelectionV1 {
      selection: SelectionV1 {
        bound,
        source_index: source_index.StateV1 { root, count: 7 },
        coordinate,
        purpose: Some(
          PurposeV1 { coordinate, script_hash: hash, subject: hash },
        ),
      },
      source: Some(
        SourceV1 {
          field_index: 6,
          item_index: 3,
          source_key: #"03",
          language_tag: 3,
          script_hash: hash,
          total_length: 8,
          item_commitment: root,
        },
      ),
    },
    scan: redeemer.PointerStateV1 {
      cursor: cursor.CursorV1 {
        bound,
        field_index: 8,
        phase: cursor.Walking,
        checkpoint_hash: root,
      },
      coordinate,
      found,
      complete,
    },
  }
}

test only_plutus_and_midgard_require_redeemers() {
  and {
    rule.is_redeemer_bearing_language_v1(3),
    rule.is_redeemer_bearing_language_v1(128),
    !rule.is_redeemer_bearing_language_v1(0),
    !rule.is_redeemer_bearing_language_v1(2),
  }
}

test exhaustion_decides_local_absence_and_preserves_source_receipt() {
  let initial = state(False, True)
  let decision = rule.decision_v1(initial)
  and {
    decision.redeemer_missing,
    decision.authenticated == initial.authenticated,
  }
}

test exhausted_presence_decides_only_local_presence() {
  !rule.decision_v1(state(True, True)).redeemer_missing
}

test premature_absence_refuses() fail {
  rule.decision_v1(state(False, False)).redeemer_missing
}

test substituted_scan_coordinate_refuses() fail {
  let initial = state(False, True)
  rule.decision_v1(
    rule.ScanStateV1 {
      ..initial,
      scan: redeemer.PointerStateV1 {
        ..initial.scan,
        coordinate: PurposeCoordinateV1 { kind: 3, index: 8 },
      },
    },
  ).redeemer_missing
}

test substituted_scan_bound_refuses() fail {
  let initial = state(False, True)
  rule.decision_v1(
    rule.ScanStateV1 {
      ..initial,
      scan: redeemer.PointerStateV1 {
        ..initial.scan,
        cursor: cursor.CursorV1 {
          ..initial.scan.cursor,
          bound: BoundV1 { ..initial.scan.cursor.bound, event_pre_root: #"" },
        },
      },
    },
  ).redeemer_missing
}

test certifying_cursor_cannot_decide_absence() fail {
  let initial = state(False, True)
  rule.decision_v1(
    rule.ScanStateV1 {
      ..initial,
      scan: redeemer.PointerStateV1 {
        ..initial.scan,
        cursor: cursor.CursorV1 {
          ..initial.scan.cursor,
          phase: cursor.Certifying,
        },
      },
    },
  ).redeemer_missing
}
