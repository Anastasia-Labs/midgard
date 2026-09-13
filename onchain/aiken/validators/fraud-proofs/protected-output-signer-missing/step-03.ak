use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, WitnessAnchor, opened_field_walk, script_witnesses_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_address_witness}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionWitnessSet,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/protected_output_signer_missing/rule.{
  ProtectedCredentialV1, WitnessScanV1, address_witnesses_field_index,
}
use midgard/fraud_proofs/protected_output_signer_missing/step_03.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_field_access_v1.{encode_field_preimage}
use midgard/native_tx_machine_walk_v1.{field_walk_checkpoint_hash}

validator main(
  step_04_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args { input_index, output_index, opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _own, _name, _prover, prior, next_hash, next_data <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(protected) = prior
        let Pair(_view, checkpoint) = opened_field_walk(
          opening,
          WitnessAnchor {
            tx_id: protected.transaction_id,
            witness_set_hash: protected.witness_set_hash,
            source_kind: protected.subject.source_kind,
          },
          address_witnesses_field_index,
          reference_inputs,
          certificate_policy,
        )
        let expected: Data = WitnessScanV1 {
          protected,
          checkpoint_hash: field_walk_checkpoint_hash(checkpoint),
          signer_present: False,
        }
        and {
          next_hash == step_04_hash,
          next_data == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

// ### Selectors
//
// Field 7 of the fixture block holds `count` copies of the fixture's sample
// address witness. Whether a signature is valid is step 04's question; step
// 03 only derives the walk's item-zero position from an authenticated opening
// and hands the thread to step 04 with an empty frontier.

const test_payment_credential =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

/// One witness past the aggregate field bound: `floor((32,768 - 3) / 103)` is
/// 318, and 319 witnesses encode to 32,860 bytes.
const adjacent_over_bound_witness_count: Int = 319

fn witness_items(count: Int) -> List<ByteArray> {
  list.repeat(
    encode_midgard_address_witness(fixture.sample_address_witness()),
    count,
  )
}

fn block_with_witnesses(count: Int) -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      witness_set: MidgardTransactionWitnessSet {
        ..base.witness_set,
        addr_tx_wits: list.repeat(fixture.sample_address_witness(), count),
      },
    },
  )
}

fn protected_of(block: fixture.NativeBlockFixtureV1) -> ProtectedCredentialV1 {
  ProtectedCredentialV1 {
    subject: substrate.VerdictSubjectV1 {
      version: substrate.version,
      direction: substrate.direction_wrongful_acceptance,
      source_kind: substrate.source_kind_accepted,
      transaction_id: block.tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    transaction_id: block.tx_id,
    witness_set_hash: block.compact.witness_set_hash,
    output_index: 0,
    payment_credential: test_payment_credential,
  }
}

fn scan_state_of(
  block: fixture.NativeBlockFixtureV1,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
) -> Data {
  let protected = protected_of(block)
  let Pair(_, checkpoint) = opened_field_walk(
    opening,
    WitnessAnchor {
      tx_id: protected.transaction_id,
      witness_set_hash: protected.witness_set_hash,
      source_kind: protected.subject.source_kind,
    },
    address_witnesses_field_index,
    reference_inputs,
    fixture.test_certificate_policy_id,
  )
  let state: Data = WitnessScanV1 {
    protected,
    checkpoint_hash: field_walk_checkpoint_hash(checkpoint),
    signer_present: False,
  }
  state
}

fn run_step_03(
  protected: ProtectedCredentialV1,
  opening: FieldOpeningV1,
  tx: Transaction,
) -> Bool {
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(protected),
      },
    ),
    ct.Continue(Args { input_index: 0, output_index: 0, opening }),
    fixture.own_out_ref(),
    tx,
  )
}

fn handoff_tx(
  block: fixture.NativeBlockFixtureV1,
  output_state: Data,
  reference_inputs: List<Input>,
) -> Transaction {
  Transaction {
    ..fixture.continue_mid_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      output_state,
    ),
    reference_inputs,
  }
}

/// One inline witness: the frontier initializes at item zero of a one-item
/// walk.
test protected_output_step_03_initializes_the_frontier_from_an_inline_field() {
  let block = block_with_witnesses(1)
  let opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    witness_items(1),
  )
  run_step_03(
    protected_of(block),
    opening,
    handoff_tx(block, scan_state_of(block, opening, []), []),
  )
}

/// An empty witness field still opens: its walk is complete at item zero and
/// step 04 terminates it without admitting anything.
test protected_output_step_03_initializes_an_empty_witness_field() {
  let block = block_with_witnesses(0)
  let opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    [],
  )
  run_step_03(
    protected_of(block),
    opening,
    handoff_tx(block, scan_state_of(block, opening, []), []),
  )
}

/// A certified field above the raw-carriage bound: the fixed stride makes the
/// count authenticated at the door, so the walk initializes without a grammar.
test protected_output_step_03_initializes_the_frontier_from_a_certified_field() {
  let count = 160
  let block = block_with_witnesses(count)
  let preimage = encode_field_preimage(witness_items(count))
  let opening = fixture.certified_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    preimage,
  )
  let refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    address_witnesses_field_index,
    preimage,
  )
  run_step_03(
    protected_of(block),
    opening,
    handoff_tx(block, scan_state_of(block, opening, refs), refs),
  )
}

/// Field 6's certificate cannot open field 7, even for the same transaction.
test protected_output_step_03_refuses_a_foreign_field_certificate() fail {
  let count = 160
  let block = block_with_witnesses(count)
  let preimage = encode_field_preimage(witness_items(count))
  let opening = fixture.certified_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    preimage,
  )
  let honest_refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    address_witnesses_field_index,
    preimage,
  )
  let foreign_refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    script_witnesses_field_index,
    preimage,
  )
  run_step_03(
    protected_of(block),
    opening,
    handoff_tx(block, scan_state_of(block, opening, honest_refs), foreign_refs),
  )
}

/// A certificate over bytes the transaction never committed wears the wrong
/// field hash and is refused at the door.
test protected_output_step_03_refuses_a_certificate_over_a_substituted_preimage() fail {
  let count = 160
  let block = block_with_witnesses(count)
  let committed = encode_field_preimage(witness_items(count))
  let foreign = encode_field_preimage(witness_items(count - 1))
  let opening = fixture.certified_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    committed,
  )
  let honest_refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    address_witnesses_field_index,
    committed,
  )
  let foreign_refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    address_witnesses_field_index,
    foreign,
  )
  run_step_03(
    protected_of(block),
    opening,
    handoff_tx(block, scan_state_of(block, opening, honest_refs), foreign_refs),
  )
}

/// Witness-set anchor: the carried anchor must be the one the thread
/// authenticated at step 01.
test protected_output_step_03_refuses_a_substituted_witness_set_anchor() fail {
  let block = block_with_witnesses(1)
  let opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    witness_items(1),
  )
  let forged = ProtectedCredentialV1 {
    ..protected_of(block),
    witness_set_hash: #"2222222222222222222222222222222222222222222222222222222222222222",
  }
  run_step_03(
    forged,
    opening,
    handoff_tx(block, scan_state_of(block, opening, []), []),
  )
}

/// Field preimage: witnesses the transaction did not commit fail the flat
/// commitment under the anchored witness set.
test protected_output_step_03_refuses_a_substituted_witness_preimage() fail {
  let block = block_with_witnesses(2)
  let opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    witness_items(1),
  )
  run_step_03(
    protected_of(block),
    opening,
    handoff_tx(block, scan_state_of(block, opening, []), []),
  )
}

/// The frontier starts empty: a handoff claiming a present signer before any
/// item was read is refused.
test protected_output_step_03_refuses_a_prefilled_frontier() fail {
  let block = block_with_witnesses(1)
  let opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    witness_items(1),
  )
  let protected = protected_of(block)
  let Pair(_, checkpoint) = opened_field_walk(
    opening,
    WitnessAnchor {
      tx_id: protected.transaction_id,
      witness_set_hash: protected.witness_set_hash,
      source_kind: protected.subject.source_kind,
    },
    address_witnesses_field_index,
    [],
    fixture.test_certificate_policy_id,
  )
  let prefilled: Data = WitnessScanV1 {
    protected,
    checkpoint_hash: field_walk_checkpoint_hash(checkpoint),
    signer_present: True,
  }
  run_step_03(protected, opening, handoff_tx(block, prefilled, []))
}

/// The initialized scan may only continue at step 04.
test protected_output_step_03_refuses_a_wrong_successor() fail {
  let block = block_with_witnesses(1)
  let opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    witness_items(1),
  )
  run_step_03(
    protected_of(block),
    opening,
    Transaction {
      ..fixture.continue_same_step_l1_tx_v1(
        fixture.ct_token_asset_name(block),
        scan_state_of(block, opening, []),
      ),
      reference_inputs: [],
    },
  )
}

/// The aggregate field bound is the family's adjacent consensus bound: a
/// 319-witness field is 32,860 bytes, which only tier-3 carriage could bring
/// to the door, and the door refuses the certified view over it. No frontier
/// over that field can be initialized.
test protected_output_step_03_refuses_an_adjacent_over_bound_witness_field() fail {
  let block = block_with_witnesses(adjacent_over_bound_witness_count)
  let preimage = encode_field_preimage(
    witness_items(adjacent_over_bound_witness_count),
  )
  let opening = fixture.certified_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    preimage,
  )
  let refs = fixture.certified_reference_inputs_v1(
    block.tx_id,
    address_witnesses_field_index,
    preimage,
  )
  run_step_03(
    protected_of(block),
    opening,
    handoff_tx(block, scan_state_of(block, opening, refs), refs),
  )
}
