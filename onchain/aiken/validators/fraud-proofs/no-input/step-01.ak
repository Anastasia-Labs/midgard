use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/input_existence_fixture_v1 as carriage_fixture
use midgard/fraud_proofs/input_existence_v1/bind
use midgard/fraud_proofs/input_existence_v1/source.{AcceptedSource, Args}
use midgard/fraud_proofs/input_existence_v1/types
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput} as native_types
use midgard/fraud_proofs/no_input/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

validator main(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    bind.bind_source_v1(
      step_02_validator_script_hash,
      computation_thread_token_policy_id,
      hub_oracle,
      datum,
      redeemer,
      own_out_ref,
      tx,
    )
  }

  else(_) {
    fail
  }
}

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)

fn run_step_01(claimed: fixture.NativeBlockFixtureV1) -> Bool {
  let expected_output_state: Data = types.SourceStateV1 {
    compact: carriage_fixture.compact_view_v1(claimed.compact),
    subject: substrate.VerdictSubjectV1 {
      version: substrate.version,
      direction: substrate.direction_wrongful_acceptance,
      source_kind: substrate.source_kind_accepted,
      transaction_id: claimed.tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    header: claimed.header,
  }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    claimed,
    expected_output_state,
    claimed.transactions_phas_root,
    claimed.tx_id,
    claimed.source_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        source: AcceptedSource {
          inclusion: fixture.native_inclusion_carriage_v1(claimed),
        },
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive selector: the challenged transaction is genuinely committed under
/// the block's counted `transactions_root`, so step-01 forwards its canonical
test no_input_step_01_binds_native_v1_block_fixture() {
  run_step_01(fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()))
}

// ## Published-chunk carriage (issue #545)
//
// Branch level 22 is the depth at which the direct route exhausts the
// 16,384-byte L1 envelope for this family. Carried by published chunks, the
// step transaction pays two small integers and two reference inputs instead.

const adversarial_branch_levels: Int = 22

const adversarial_chunk_count: Int = 2

fn deep_no_input_block() -> (fixture.NativeBlockFixtureV1, mpf.Proof) {
  fixture.deep_membership_block_fixture_v1(
    fixture.valid_native_tx_v1(),
    adversarial_branch_levels,
  )
}

fn run_step_01_published(
  block: fixture.NativeBlockFixtureV1,
  carriage_block: fixture.NativeBlockFixtureV1,
  chunk_count: Int,
  claim_chunk_count: Int,
  steps: mpf.Proof,
) -> Bool {
  let expected_output_state: Data = types.SourceStateV1 {
    compact: carriage_fixture.compact_view_v1(block.compact),
    subject: substrate.VerdictSubjectV1 {
      version: substrate.version,
      direction: substrate.direction_wrongful_acceptance,
      source_kind: substrate.source_kind_accepted,
      transaction_id: block.tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    header: block.header,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        source: AcceptedSource {
          inclusion: fixture.published_chunk_inclusion_carriage_v1(
            carriage_block,
            chunk_count,
          ),
        },
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_step_chunked_l1_tx_v1(
      block,
      expected_output_state,
      fixture.proof_chunk_reference_inputs(steps),
      fixture.chunked_membership_claim_v1(carriage_block, claim_chunk_count),
    ),
  )
}

/// Positive selector: the challenged transaction is bound through a 22-level
/// membership proof carried by published chunks, and the authenticated raw
/// transactions root still reaches step-02 for the later absence checks.
test no_input_step_01_binds_a_published_chunk_membership_proof() {
  let (block, steps) = deep_no_input_block()
  and {
    list.length(steps) == adversarial_branch_levels,
    list.length(fixture.proof_chunk_reference_inputs(steps)) == adversarial_chunk_count,
    run_step_01_published(
      block,
      block,
      adversarial_chunk_count,
      adversarial_chunk_count,
      steps,
    ),
  }
}

/// The published chunks stay bound to the header's counted transactions root.
test no_input_step_01_rejects_published_chunks_under_a_forged_root() fail {
  let (block, steps) = deep_no_input_block()
  run_step_01_published(
    block,
    fixture.NativeBlockFixtureV1 {
      ..block,
      transactions_phas_root: #"4040404040404040404040404040404040404040404040404040404040404040",
    },
    adversarial_chunk_count,
    adversarial_chunk_count,
    steps,
  )
}

/// The step is bound to the exact claim its redeemer named: a merkelized
/// verifier invoked on a different chunk order proves something this step did
/// not ask for. Whether that order reconstructs the root is the verifier's
/// business, and its own module's tests.
test no_input_step_01_rejects_a_claim_the_redeemer_did_not_name() fail {
  let (block, steps) = deep_no_input_block()
  run_step_01_published(block, block, adversarial_chunk_count, 1, steps)
}

/// committed by the header's counted `transactions_root` cannot bind, so a
/// transaction fabricated against a valid block's header is rejected before any
/// non-existence claim can be made.
test no_input_step_01_rejects_forged_transactions_root() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_block = fixture.native_block_fixture_v1(uncommitted_bad_tx())
  run_step_01(
    fixture.NativeBlockFixtureV1 {
      ..forged_block,
      header: valid_block.header,
      header_hash: valid_block.header_hash,
    },
  )
}

/// A native-V1 transaction that the fixture block does not commit, used as the
/// fabricated evidence of the valid-block negative.
fn uncommitted_bad_tx() -> native_types.MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  native_types.MidgardTransaction {
    ..base,
    body: native_types.MidgardTransactionBody { ..base.body, fee: 2_000_000 },
  }
}
