use cardano/transaction.{Transaction}
use env
use fraud_proofs/direct_script_derivation/receive_declaration as entry
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/direct_script_derivation/receive_declaration as door
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}

fn bound() -> BoundV1 {
  BoundV1 {
    subject: VerdictSubjectV1 {
      version: 1,
      direction: 0,
      source_kind: 0,
      transaction_id: #"1111111111111111111111111111111111111111111111111111111111111111",
      source_key: #"",
      rejection_reason: None,
    },
    compact_cbor: #"00",
    network_order_context: NetworkOrderContextV1 {
      body_network_mismatch: False,
    },
    event_pre_root: env.empty_merkle_tree_root,
  }
}

fn empty_declaration(expected_count: Int) -> receive.DeclarationStateV1 {
  receive.DeclarationStateV1 {
    bound: bound(),
    source_index: source_receipt(),
    output_root: env.empty_merkle_tree_root,
    expected_count,
    declared_root: env.empty_merkle_tree_root,
    declared_count: 0,
    previous_credential: #"",
    target_ordinal: Some(0),
    selected: None,
  }
}

fn source_receipt() -> source_index.StateV1 {
  source_index.StateV1 {
    root: #"4444444444444444444444444444444444444444444444444444444444444444",
    count: 7,
  }
}

fn finish(
  expected_count: Int,
  changed_coordinate: Bool,
  changed_receipt: Int,
) -> Bool {
  let expected: Data = SelectionV1 {
    bound: if changed_receipt == 3 {
      BoundV1 {
        ..bound(),
        event_pre_root: #"5555555555555555555555555555555555555555555555555555555555555555",
      }
    } else {
      bound()
    },
    source_index: source_index.StateV1 {
      root: if changed_receipt == 1 {
        #"5555555555555555555555555555555555555555555555555555555555555555"
      } else {
        source_receipt().root
      },
      count: if changed_receipt == 2 {
        8
      } else {
        7
      },
    },
    coordinate: PurposeCoordinateV1 {
      kind: 3,
      index: if changed_coordinate {
        1
      } else {
        0
      },
    },
    purpose: None,
  }
  entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(empty_declaration(expected_count)),
      },
    ),
    ct.Continue(door.Finish { input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(bound().subject.transaction_id, expected),
  )
}

test complete_empty_receive_set_proves_absent_ordinal_without_a_synthetic_item() {
  finish(0, False, 0)
}

test incomplete_receive_declaration_cannot_finish() fail {
  finish(1, False, 0)
}

test completed_receive_selection_cannot_change_its_coordinate() {
  !finish(0, True, 0)
}

test completed_receive_selection_preserves_source_root() {
  !finish(0, False, 1)
}

test completed_receive_selection_preserves_source_count() {
  !finish(0, False, 2)
}

test completed_receive_selection_preserves_event_bound() {
  !finish(0, False, 3)
}

const declared_hash =
  #"33333333333333333333333333333333333333333333333333333333"

fn declaration_scan(signatures: List<ByteArray>) -> Bool {
  let Pair(output_root, expected_count) = receive.admit_output_v1(
    env.empty_merkle_tree_root,
    0,
    0,
    Some(declared_hash),
    receive.FirstCredential { proof: [] },
  )
  let state = receive.DeclarationStateV1 {
    ..empty_declaration(expected_count),
    output_root,
  }
  let entries = [
    receive.DeclaredCredentialV1 {
      script_hash: declared_hash,
      first_output_index: 0,
      proof: [],
    },
  ]
  let output: Data = receive.advance_declaration_v1(state, entries)
  let tx = fixture.continue_same_step_l1_tx_v1(
    bound().subject.transaction_id,
    output,
  )
  entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(door.Scan { input_index: 0, output_index: 0, entries }),
    fixture.own_out_ref(),
    Transaction { ..tx, extra_signatories: signatures },
  )
}

test declaration_scan_by_the_prover_is_accepted() {
  declaration_scan([fixture.fraud_prover_key_hash])
}

test declaration_scan_without_a_signature_refuses() fail {
  declaration_scan([])
}

test declaration_scan_by_another_signer_refuses() fail {
  declaration_scan([declared_hash])
}
