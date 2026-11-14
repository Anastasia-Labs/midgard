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
use midgard/fraud_proofs/no_reference_input/step_01.{Datum, SpendRedeemer}
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

/// Positive binding selector: a native-V1 transaction referencing an input,
/// genuinely committed under the block's counted `transactions_root`, passes
/// the first step and forwards its canonical reference-inputs hash together
test no_reference_input_step_01_binds_native_v1_block_fixture() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
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
  let l1_tx = fixture.continue_step_l1_tx_v1(
    block,
    expected_output_state,
    block.transactions_phas_root,
    block.tx_id,
    block.source_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        source: AcceptedSource {
          inclusion: fixture.native_inclusion_carriage_v1(block),
        },
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// re-commit to the valid header's counted `transactions_root` cannot bind
/// fabricated evidence, so a valid block cannot be challenged.
test no_reference_input_step_01_rejects_forged_transactions_root() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_tx = fixture.valid_native_tx_v1()
  let forged_block = {
    let base = fixture.native_block_fixture_v1(forged_tx)
    fixture.NativeBlockFixtureV1 {
      ..base,
      transactions_phas_root: #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
      header: valid_block.header,
      header_hash: valid_block.header_hash,
    }
  }
  let expected_output_state: Data = types.SourceStateV1 {
    compact: carriage_fixture.compact_view_v1(forged_block.compact),
    subject: substrate.VerdictSubjectV1 {
      version: substrate.version,
      direction: substrate.direction_wrongful_acceptance,
      source_kind: substrate.source_kind_accepted,
      transaction_id: forged_block.tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    header: forged_block.header,
  }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    forged_block,
    expected_output_state,
    forged_block.transactions_phas_root,
    forged_block.tx_id,
    forged_block.source_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        source: AcceptedSource {
          inclusion: fixture.native_inclusion_carriage_v1(forged_block),
        },
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}
