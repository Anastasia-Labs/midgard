use cardano/transaction.{Transaction}
use fraud_proofs/missing_script_source/observed_decision
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, LeafPurpose,
}
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  SourceSelectionV1,
}
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan.{
  DecisionStateV1,
}
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/missing_script_source/observe
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

const other = #"55555555555555555555555555555555555555555555555555555555"

fn plutus_source() -> SourceV1 {
  SourceV1 {
    field_index: 6,
    item_index: 0,
    source_key: #"00",
    language_tag: 3,
    script_hash: hash,
    total_length: 2,
    item_commitment: root,
  }
}

fn selection(source: Option<SourceV1>) -> SourceSelectionV1 {
  let at = PurposeCoordinateV1 { kind: 2, index: 0 }
  SourceSelectionV1 {
    selection: SelectionV1 {
      bound: BoundV1 {
        subject: VerdictSubjectV1 {
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
      },
      source_index: source_index.StateV1 { root, count: 1 },
      coordinate: at,
      purpose: Some(
        PurposeV1 { coordinate: at, script_hash: hash, subject: hash },
      ),
    },
    source,
  }
}

fn observed(state: SourceSelectionV1) -> Data {
  let output: Data = ClaimContextV1 {
    bound: BoundIndexV1 {
      bound: state.selection.bound,
      source_index: state.selection.source_index,
    },
    observed: Some(
      DecisionStateV1 { authenticated: state, redeemer_missing: False },
    ),
    target: LeafPurpose,
  }
  output
}

fn run(
  state: SourceSelectionV1,
  output: fn(SourceSelectionV1) -> Data,
  next: ByteArray,
  signatures: List<ByteArray>,
) -> Bool {
  let tx = fixture.continue_mid_step_l1_tx_v1(root, output(state))
  observed_decision.main.spend(
    next,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(observe.Args { input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    Transaction { ..tx, extra_signatories: signatures },
  )
}

test observed_decision_hands_a_missing_source_to_claim_start() {
  run(
    selection(None),
    observed,
    fixture.next_step_script_hash,
    [fixture.fraud_prover_key_hash],
  )
}

test observed_decision_refuses_another_destination() {
  !run(selection(None), observed, other, [fixture.fraud_prover_key_hash])
}

test observed_decision_cannot_claim_a_missing_redeemer() {
  !run(
    selection(None),
    fn(state) {
      let output: Data = ClaimContextV1 {
        bound: BoundIndexV1 {
          bound: state.selection.bound,
          source_index: state.selection.source_index,
        },
        observed: Some(
          DecisionStateV1 { authenticated: state, redeemer_missing: True },
        ),
        target: LeafPurpose,
      }
      output
    },
    fixture.next_step_script_hash,
    [fixture.fraud_prover_key_hash],
  )
}

test observed_decision_refuses_a_present_source() fail {
  run(
    selection(Some(plutus_source())),
    observed,
    fixture.next_step_script_hash,
    [fixture.fraud_prover_key_hash],
  )
}

test observed_decision_requires_the_prover_signature() fail {
  run(selection(None), observed, fixture.next_step_script_hash, [])
}
