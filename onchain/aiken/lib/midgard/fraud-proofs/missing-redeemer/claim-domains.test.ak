use aiken/builtin
use aiken/cbor
use aiken/collection/list
use env
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/mint_purpose
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/missing_redeemer/claim.{ClaimContextV1}
use midgard/fraud_proofs/missing_redeemer/claim_mint as mint
use midgard/fraud_proofs/missing_redeemer/claim_purpose.{
  ExistingPurpose, ImpossiblePurpose,
}
use midgard/fraud_proofs/missing_redeemer/claim_receive as receive_claim
use midgard/fraud_proofs/missing_redeemer/claim_start as start
use midgard/fraud_proofs/missing_redeemer/rule
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/preimages.{mint_policy_items}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/rejection_reason_v1 as reasons

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

fn decision(
  block: fixture.NativeBlockFixtureV1,
  reason: Option<reasons.RejectionReasonV1>,
  pre_root: ByteArray,
) -> rule.DecisionStateV1 {
  let coordinate = PurposeCoordinateV1 { kind: 2, index: 9 }
  rule.DecisionStateV1 {
    authenticated: workers.SourceSelectionV1 {
      selection: SelectionV1 {
        bound: BoundV1 {
          subject: VerdictSubjectV1 {
            version: 1,
            direction: 0,
            source_kind: 0,
            transaction_id: block.tx_id,
            source_key: #"",
            rejection_reason: reason,
          },
          compact_cbor: block.compact_cbor,
          network_order_context: NetworkOrderContextV1 {
            body_network_mismatch: False,
          },
          event_pre_root: pre_root,
        },
        source_index: source_index.StateV1 { root, count: 7 },
        coordinate,
        purpose: Some(
          PurposeV1 { coordinate, script_hash: hash, subject: hash },
        ),
      },
      source: Some(
        SourceV1 {
          field_index: 6,
          item_index: 0,
          source_key: #"00",
          language_tag: 3,
          script_hash: hash,
          total_length: 2,
          item_commitment: root,
        },
      ),
    },
    redeemer_missing: True,
  }
}

fn observed(
  block: fixture.NativeBlockFixtureV1,
  reason: Option<reasons.RejectionReasonV1>,
  pre_root: ByteArray,
) -> ClaimContextV1 {
  claim.observed_context_v1(decision(block, reason, pre_root))
}

fn reason(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(reasons.RedeemerMissing { purpose_kind: kind, purpose_index: index })
}

fn impossible(kind: Int, index: Int) -> claim_purpose.ClaimedPurposeFactV1 {
  ImpossiblePurpose { coordinate: PurposeCoordinateV1 { kind, index } }
}

fn is_existing(
  fact: claim_purpose.ClaimedPurposeFactV1,
  kind: Int,
  index: Int,
) -> Bool {
  when fact is {
    ExistingPurpose { purpose } -> and {
        purpose.coordinate == PurposeCoordinateV1 { kind, index },
        purpose.script_hash == hash,
      }
    _ -> False
  }
}

fn mint_fixture(policies: List<ByteArray>, target: Int) {
  let value = if list.is_empty(policies) {
    builtin.list_data([])
  } else {
    builtin.map_data(
      list.map(
        policies,
        fn(policy) {
          Pair(
            builtin.b_data(policy),
            builtin.map_data([Pair(builtin.b_data(#"41"), builtin.i_data(1))]),
          )
        },
      ),
    )
  }
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody { ..tx.body, mint: value },
    },
  )
  let initial = observed(block, reason(1, target), root)
  let opening = fixture.inline_body_opening(
    block.compact_cbor,
    mint_policy_items(value),
  )
  let state = mint.initial_v1(
    initial,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, checkpoint) = cursor.initial_v1(
    initial.bound.bound,
    5,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let args = workers.ProgressArgsV1 {
    input_index: 0,
    output_index: 0,
    opening,
    checkpoint_bytes: checkpoint,
    budget: 16,
  }
  let ready = mint.certify_v1(
    state,
    args,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, semantic) = cursor.certify_v1(
    state.work.scan.cursor,
    opening,
    checkpoint,
    16,
    [],
    fixture.test_certificate_policy_id,
  )
  (ready, opening, semantic)
}

fn completed_mint(policies: List<ByteArray>, target: Int) {
  let (ready, opening, checkpoint) = mint_fixture(policies, target)
  if ready.work.scan.complete {
    (ready, opening, checkpoint)
  } else {
    let args = workers.ProgressArgsV1 {
      input_index: 0,
      output_index: 0,
      opening,
      checkpoint_bytes: checkpoint,
      budget: 16,
    }
    let final = mint.advance_v1(
      ready,
      args,
      [],
      fixture.test_certificate_policy_id,
    )
    let Pair(_, checkpoint) = mint_purpose.advance_v1(
      ready.work.scan,
      opening,
      checkpoint,
      16,
      [],
      fixture.test_certificate_policy_id,
    )
    (final, opening, checkpoint)
  }
}

fn finish_mint(policies: List<ByteArray>, target: Int) {
  let (state, opening, checkpoint) = completed_mint(policies, target)
  mint.finish_v1(
    state,
    start.FinishOpeningArgsV1 {
      input_index: 0,
      output_index: 0,
      opening,
      checkpoint_bytes: checkpoint,
    },
    [],
    fixture.test_certificate_policy_id,
  )
}

test empty_mint_domain_proves_impossible_from_authenticated_exhaustion() {
  finish_mint([], 0).purpose == impossible(1, 0)
}

test mint_existing_policy_is_a_purpose() {
  finish_mint([hash], 0).purpose |> is_existing(1, 0)
}

test mint_count_boundary_is_impossible() {
  finish_mint([hash], 1).purpose == impossible(1, 1)
}

// Successful returns serialize to nonempty CBOR; only an abort satisfies fail.
test mint_grammar_completion_is_not_semantic_completion() fail {
  let (complete, opening, checkpoint) = completed_mint([hash], 1)
  // Isolate the semantic completion guard from checkpoint exhaustion.
  let state = mint.StateV1 {
    ..complete,
    work: workers.MintStateV1 {
      ..complete.work,
      scan: mint_purpose.StateV1 { ..complete.work.scan, complete: False },
    },
  }
  cbor.serialise(
    mint.finish_v1(
      state,
      start.FinishOpeningArgsV1 {
        input_index: 0,
        output_index: 0,
        opening,
        checkpoint_bytes: checkpoint,
      },
      [],
      fixture.test_certificate_policy_id,
    ),
  ) != #""
}

test mint_cannot_substitute_the_leaf_ordinal() fail {
  let (state, opening, checkpoint) = completed_mint([hash], 1)
  let changed = mint.StateV1 {
    ..state,
    work: workers.MintStateV1 {
      ..state.work,
      scan: mint_purpose.StateV1 { ..state.work.scan, target_ordinal: 0 },
    },
  }
  cbor.serialise(
    mint.finish_v1(
      changed,
      start.FinishOpeningArgsV1 {
        input_index: 0,
        output_index: 0,
        opening,
        checkpoint_bytes: checkpoint,
      },
      [],
      fixture.test_certificate_policy_id,
    ),
  ) != #""
}

test mint_count_must_come_from_the_final_authenticated_checkpoint() fail {
  let (complete, opening, checkpoint) = completed_mint([hash], 1)
  let (earlier, _, _) = mint_fixture([hash], 1)
  // The supplied completed checkpoint is valid except for the CT commitment.
  let state = mint.StateV1 {
    ..complete,
    work: workers.MintStateV1 {
      ..complete.work,
      scan: mint_purpose.StateV1 {
        ..complete.work.scan,
        cursor: cursor.CursorV1 {
          ..complete.work.scan.cursor,
          checkpoint_hash: earlier.work.scan.cursor.checkpoint_hash,
        },
      },
    },
  }
  cbor.serialise(
    mint.finish_v1(
      state,
      start.FinishOpeningArgsV1 {
        input_index: 0,
        output_index: 0,
        opening,
        checkpoint_bytes: checkpoint,
      },
      [],
      fixture.test_certificate_policy_id,
    ),
  ) != #""
}

fn receive_fixture(empty: Bool, target: Int) {
  let tx = fixture.valid_native_tx_v1()
  let output = fixture.sample_native_output()
  let outputs = if empty {
    []
  } else {
    [
      native_types.MidgardTxOutput {
        ..output,
        address: native_types.MidgardAddress {
          ..output.address,
          protected: True,
          payment_credential: native_types.ScriptCredential(hash),
        },
      },
    ]
  }
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody { ..tx.body, outputs },
    },
  )
  let initial = observed(block, reason(3, target), root)
  let opening = fixture.inline_body_opening(
    block.compact_cbor,
    list.map(outputs, encode_midgard_tx_output),
  )
  let state = receive_claim.initial_v1(
    initial,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, checkpoint) = cursor.initial_v1(
    initial.bound.bound,
    2,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let args = workers.ProgressArgsV1 {
    input_index: 0,
    output_index: 0,
    opening,
    checkpoint_bytes: checkpoint,
    budget: 16,
  }
  let ready = receive_claim.certify_v1(
    state,
    args,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, semantic) = cursor.certify_v1(
    state.scan.cursor,
    opening,
    checkpoint,
    16,
    [],
    fixture.test_certificate_policy_id,
  )
  (ready, opening, semantic)
}

fn output_complete(empty: Bool, target: Int) {
  let (state, opening, checkpoint) = receive_fixture(empty, target)
  if empty {
    (state, opening, checkpoint)
  } else {
    let witnesses = [receive.FirstCredential { proof: [] }]
    let next = receive_claim.advance_outputs_v1(
      state,
      opening,
      checkpoint,
      witnesses,
      [],
      fixture.test_certificate_policy_id,
    )
    let Pair(_, checkpoint) = receive.advance_outputs_v1(
      state.scan,
      opening,
      checkpoint,
      witnesses,
      [],
      fixture.test_certificate_policy_id,
    )
    (next, opening, checkpoint)
  }
}

fn declaration(empty: Bool, target: Int) {
  let (state, opening, checkpoint) = output_complete(empty, target)
  let state = receive_claim.finish_outputs_v1(
    state,
    start.FinishOpeningArgsV1 {
      input_index: 0,
      output_index: 0,
      opening,
      checkpoint_bytes: checkpoint,
    },
    [],
    fixture.test_certificate_policy_id,
  )
  if empty {
    state
  } else {
    receive_claim.advance_declaration_v1(
      state,
      [
        receive.DeclaredCredentialV1 {
          script_hash: hash,
          first_output_index: 0,
          proof: [],
        },
      ],
    )
  }
}

test empty_receive_domain_proves_impossible_after_output_and_declaration_exhaustion() {
  receive_claim.finish_declaration_v1(declaration(True, 0)).purpose == impossible(
    3,
    0,
  )
}

test protected_script_credential_creates_one_receive_purpose() {
  receive_claim.finish_declaration_v1(declaration(False, 0)).purpose
    |> is_existing(3, 0)
}

test receive_unique_count_boundary_is_impossible() {
  receive_claim.finish_declaration_v1(declaration(False, 1)).purpose == impossible(
    3,
    1,
  )
}

test receive_grammar_does_not_prove_output_exhaustion() fail {
  let (state, opening, checkpoint) = receive_fixture(False, 0)
  receive_claim.finish_outputs_v1(
    state,
    start.FinishOpeningArgsV1 {
      input_index: 0,
      output_index: 0,
      opening,
      checkpoint_bytes: checkpoint,
    },
    [],
    fixture.test_certificate_policy_id,
  ).scan.declared_count == 0
}

test receive_declaration_must_match_the_discovered_root() fail {
  let state = declaration(False, 0)
  cbor.serialise(
    receive_claim.finish_declaration_v1(
      receive_claim.DeclarationStateV1 {
        ..state,
        scan: receive.DeclarationStateV1 {
          ..state.scan,
          declared_root: env.empty_merkle_tree_root,
        },
      },
    ),
  ) != #""
}

test receive_target_is_the_leaf_ordinal_not_first_output_index() fail {
  let state = declaration(False, 1)
  cbor.serialise(
    receive_claim.finish_declaration_v1(
      receive_claim.DeclarationStateV1 {
        ..state,
        scan: receive.DeclarationStateV1 {
          ..state.scan,
          target_ordinal: Some(0),
        },
      },
    ),
  ) != #""
}
