use cardano/transaction.{Transaction}
use fraud_proofs/missing_redeemer/family_entry
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, LeafPurpose,
}
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan as rule
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/missing_redeemer/entry
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/rejection_reason_v1 as reasons

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

const other = #"55555555555555555555555555555555555555555555555555555555"

fn missing(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(reasons.RedeemerMissing { purpose_kind: kind, purpose_index: index })
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

fn block(
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> fixture.NativeBlockFixtureV1 {
  let tx = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        required_observers: [hash],
      },
      witness_set: native_types.MidgardTransactionWitnessSet {
        ..tx.witness_set,
        redeemer_tx_wits: witnesses,
      },
    },
  )
}

fn bound(
  fixture_block: fixture.NativeBlockFixtureV1,
  reason: Option<reasons.RejectionReasonV1>,
) -> BoundIndexV1 {
  BoundIndexV1 {
    bound: BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction: 0,
        source_kind: 0,
        transaction_id: fixture_block.tx_id,
        source_key: #"",
        rejection_reason: reason,
      },
      compact_cbor: fixture_block.compact_cbor,
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: root,
    },
    source_index: source_index.admit_v1(
      source_index.initial_v1(),
      source(3),
      source_index.First { proof: [] },
    ),
  }
}

fn step_datum(state: a) -> Option<ct.StepDatum<a>> {
  Some(
    ct.StepDatum {
      fraud_prover: fixture.fraud_prover_key_hash,
      data: Some(state),
    },
  )
}

fn step_tx(state: Data, signatures: List<ByteArray>, same: Bool) -> Transaction {
  let tx = if same {
    fixture.continue_same_step_l1_tx_v1(root, state)
  } else {
    fixture.continue_mid_step_l1_tx_v1(root, state)
  }
  Transaction { ..tx, extra_signatories: signatures }
}

// ## family-entry

fn entry_run(
  reason: Option<reasons.RejectionReasonV1>,
  claim_route: Bool,
  output: fn(BoundIndexV1) -> Data,
  signatures: List<ByteArray>,
) -> Bool {
  let state = bound(block([]), reason)
  let action = if claim_route {
    entry.Claim { input_index: 0, output_index: 0 }
  } else {
    entry.Observe { input_index: 0, output_index: 0 }
  }
  family_entry.main.spend(
    if claim_route {
      other
    } else {
      fixture.next_step_script_hash
    },
    if claim_route {
      fixture.next_step_script_hash
    } else {
      other
    },
    fixture.computation_thread_policy_id,
    step_datum(state),
    ct.Continue(action),
    fixture.own_out_ref(),
    step_tx(output(state), signatures, False),
  )
}

fn unchanged(state: BoundIndexV1) -> Data {
  let output: Data = state
  output
}

fn claimed_only(state: BoundIndexV1) -> Data {
  let output: Data = ClaimContextV1 {
    bound: state,
    observed: None,
    target: LeafPurpose,
  }
  output
}

test entry_observe_hands_the_exact_binding_to_purpose_selection() {
  entry_run(None, False, unchanged, [fixture.fraud_prover_key_hash])
}

test entry_observe_cannot_alter_the_binding() {
  !entry_run(
    None,
    False,
    fn(state) {
      let changed: Data = BoundIndexV1 {
        ..state,
        source_index: source_index.initial_v1(),
      }
      changed
    },
    [fixture.fraud_prover_key_hash],
  )
}

test entry_claim_starts_a_claimed_only_context() {
  entry_run(missing(2, 0), True, claimed_only, [fixture.fraud_prover_key_hash])
}

test entry_claim_cannot_carry_an_invented_observation() {
  !entry_run(
    missing(2, 0),
    True,
    fn(state) {
      let changed: Data = ClaimContextV1 {
        bound: state,
        observed: Some(
          rule.DecisionStateV1 {
            authenticated: workers.SourceSelectionV1 {
              selection: SelectionV1 {
                bound: state.bound,
                source_index: state.source_index,
                coordinate: PurposeCoordinateV1 { kind: 2, index: 0 },
                purpose: None,
              },
              source: None,
            },
            redeemer_missing: True,
          },
        ),
        target: LeafPurpose,
      }
      changed
    },
    [fixture.fraud_prover_key_hash],
  )
}

test entry_claim_refuses_an_accepted_leaf() fail {
  entry_run(None, True, claimed_only, [fixture.fraud_prover_key_hash])
}

test entry_claim_refuses_another_rejection_reason() fail {
  entry_run(
    Some(reasons.ScriptSourceMissing { purpose_kind: 2, purpose_index: 0 }),
    True,
    claimed_only,
    [fixture.fraud_prover_key_hash],
  )
}

test entry_route_choice_requires_the_prover_signature() fail {
  entry_run(None, False, unchanged, [])
}

test entry_route_choice_refuses_another_signature() fail {
  entry_run(missing(2, 0), True, claimed_only, [other])
}
