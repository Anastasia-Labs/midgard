//// Accepted-direction source rows for execution-native-script-invalid.
////
//// Each row drives the applied source doors (`accepted_inline_source`,
//// `accepted_reference_source`) and then evaluator steps 03 and 04 as a
//// prover would, starting from the honest state in which the spent input's
//// script purpose is the challenged execution. A row convicts when every door
//// accepts its successor and step 04 finalizes the proof.

use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/transaction.{Transaction}
use fraud_proofs/execution_native_script_invalid/accepted_inline_source as inline_door
use fraud_proofs/execution_native_script_invalid/accepted_reference_source as resolved_door
use fraud_proofs/execution_native_script_invalid/step_03 as step_03_door
use fraud_proofs/execution_native_script_invalid/step_04 as step_04_door
use midgard/bounded_item_v1
use midgard/cek_data_v1
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{RedeemerCarriedMembership}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_inline_source.{
  FinishInline, ScanInline,
}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction as reconstruction
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reference_source.{
  FinishResolvedOutputs, ScanResolvedOutput,
}
use midgard/fraud_proofs/execution_native_script_invalid/rule
use midgard/fraud_proofs/execution_native_script_invalid/step_03
use midgard/fraud_proofs/execution_native_script_invalid/step_04
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_tx_input,
  encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionWitnessSet, MidgardTxInput,
  MidgardVersionedScript, NativeCardanoScript,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_output_commitment_v1
use midgard/native_tx_field_access_v1.{Inline, encode_field_preimage}
use midgard/script_proof_v1

const prior_root =
  #"abababababababababababababababababababababababababababababababab"

const h32 = #"1111111111111111111111111111111111111111111111111111111111111111"

// ---------------------------------------------------------------------------
// Fixture
// ---------------------------------------------------------------------------

fn sig_script(key_hash: ByteArray) -> MidgardVersionedScript {
  MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes: #"8200581c" |> bytearray.concat(key_hash),
  }
}

/// Unsatisfied by the fixture's only address witness.
fn false_script() -> MidgardVersionedScript {
  sig_script(#"39393939393939393939393939393939393939393939393939393939")
}

/// Satisfied by the fixture's only address witness.
fn true_script() -> MidgardVersionedScript {
  sig_script(
    utils.get_verification_key_hash(fixture.sample_witness_verification_key()),
  )
}

/// The fixture's spend input; its out-ref key sorts before the reference
/// input's.
fn spent_input() -> MidgardTxInput {
  expect Some(input) = list.head(fixture.valid_native_tx_v1().body.inputs)
  input
}

fn reference_input() -> MidgardTxInput {
  expect Some(input) = list.head(
    fixture.valid_native_tx_v1().body.reference_inputs,
  )
  input
}

fn block_with_inline(
  inline: List<MidgardVersionedScript>,
) -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      witness_set: MidgardTransactionWitnessSet {
        ..base.witness_set,
        script_tx_wits: inline,
      },
    },
  )
}

/// The ledger descriptor of `input`'s output, carrying `script` as its
/// reference script when given.
fn descriptor_cbor(
  input: MidgardTxInput,
  script: Option<MidgardVersionedScript>,
) -> ByteArray {
  let summary = cek_data_v1.DataSummaryV1 {
    root: h32,
    cbor_length: 1,
    memory: 1,
  }
  let base = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index: input.output_index,
    total_length: 100,
    item_commitment: h32,
    address: #"6011111111111111111111111111111111111111111111111111111111",
    lovelace: 5_000_000,
    asset_count: 0,
    asset_frontier_commitment: h32,
    cardano_value_size: 5,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out: summary,
    midgard_tx_out: summary,
    cardano_spend_datum: summary,
  }
  ledger_output_commitment_v1.encode(
    when script is {
      None -> base
      Some(script) -> {
        let script_cbor = encode_midgard_versioned_script(script)
        ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
          ..base,
          reference_script_language: 0,
          reference_script_hash: script_proof_v1.versioned_script_hash(script),
          reference_script_total_length: bytearray.length(script_cbor),
          reference_script_item_commitment: bounded_item_v1.from_bytes(
            2,
            input.output_index,
            script_cbor,
          ),
        }
      }
    },
  )
}

fn bound(block: fixture.NativeBlockFixtureV1) -> rule.BoundExecutionV1 {
  rule.BoundExecutionV1 {
    subject: substrate.VerdictSubjectV1 {
      version: substrate.version,
      direction: substrate.direction_wrongful_acceptance,
      source_kind: substrate.source_kind_accepted,
      transaction_id: block.tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    validation_traces_root: h32,
    validation_trace_count: 1,
    execution_index: 0,
    accused_class: rule.result_pending,
    prior_ledger_root: prior_root,
    compact_cbor: block.compact_cbor,
  }
}

// ---------------------------------------------------------------------------
// Prover
// ---------------------------------------------------------------------------

/// One resolved output the prover walks: item `item_index` of field
/// `field_index`, whose output carries `script`.
type ResolvedStep {
  field_index: Int,
  item_index: Int,
  input: MidgardTxInput,
  script: Option<MidgardVersionedScript>,
}

fn spent(script: Option<MidgardVersionedScript>) -> ResolvedStep {
  ResolvedStep { field_index: 0, item_index: 0, input: spent_input(), script }
}

fn referenced(script: Option<MidgardVersionedScript>) -> ResolvedStep {
  ResolvedStep {
    field_index: 1,
    item_index: 0,
    input: reference_input(),
    script,
  }
}

fn datum(state: a) -> Option<ct.StepDatum<a>> {
  Some(
    ct.StepDatum {
      fraud_prover: fixture.fraud_prover_key_hash,
      data: Some(state),
    },
  )
}

fn same_step_tx(block: fixture.NativeBlockFixtureV1, state: Data) -> Transaction {
  fixture.continue_same_step_l1_tx_v1(fixture.ct_token_asset_name(block), state)
}

fn next_step_tx(block: fixture.NativeBlockFixtureV1, state: Data) -> Transaction {
  fixture.continue_mid_step_l1_tx_v1(fixture.ct_token_asset_name(block), state)
}

/// The honest state entering source discovery, addressed to the inline door.
fn selected_state(
  block: fixture.NativeBlockFixtureV1,
  script: MidgardVersionedScript,
) -> reconstruction.StateV1 {
  let key = encode_midgard_tx_input(spent_input())
  reconstruction.append_purpose_v1(
    reconstruction.initial_v1(bound(block), fixture.step_script_hash),
    reconstruction.phase_spend,
    0,
    script_proof_v1.versioned_script_hash(script),
    key,
    key,
    fixture.step_script_hash,
  )
}

fn inline_source(
  index: Int,
  script: MidgardVersionedScript,
) -> reconstruction.SelectedSourceV1 {
  let script_cbor = encode_midgard_versioned_script(script)
  reconstruction.SelectedSourceV1 {
    source_index: index,
    origin_kind: 0,
    source_key: cbor.serialise(index),
    language_tag: 0,
    script_hash: script_proof_v1.versioned_script_hash(script),
    total_length: bytearray.length(script_cbor),
    item_commitment: bounded_item_v1.from_bytes(6, index, script_cbor),
  }
}

fn witness_opening(
  block: fixture.NativeBlockFixtureV1,
  inline: List<MidgardVersionedScript>,
) {
  fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    list.map(inline, encode_midgard_versioned_script),
  )
}

/// Walk every inline witness through the field-6 door and finish the inline
/// phase. Returns the reference-phase state addressed to the resolved door.
fn walk_inline(
  block: fixture.NativeBlockFixtureV1,
  state: reconstruction.StateV1,
  inline: List<MidgardVersionedScript>,
  remaining: List<MidgardVersionedScript>,
) -> reconstruction.StateV1 {
  when remaining is {
    [] -> {
      let handed: Data = reconstruction.finish_inline_sources_v1(
        state,
        fixture.next_step_script_hash,
      )
      expect
        inline_door.main.spend(
          fixture.next_step_script_hash,
          fixture.computation_thread_policy_id,
          fixture.test_certificate_policy_id,
          datum(state),
          ct.Continue(
            FinishInline {
              input_index: 0,
              output_index: 0,
              scripts_opening: witness_opening(block, inline),
            },
          ),
          fixture.own_out_ref(),
          next_step_tx(block, handed),
        )
      reconstruction.finish_inline_sources_v1(state, fixture.step_script_hash)
    }
    [script, ..rest] -> {
      let next = reconstruction.append_inline_source_v1(
        state,
        inline_source(state.field_cursor, script),
        fixture.step_script_hash,
      )
      let expected: Data = next
      expect
        inline_door.main.spend(
          fixture.next_step_script_hash,
          fixture.computation_thread_policy_id,
          fixture.test_certificate_policy_id,
          datum(state),
          ct.Continue(
            ScanInline {
              input_index: 0,
              output_index: 0,
              scripts_opening: witness_opening(block, inline),
            },
          ),
          fixture.own_out_ref(),
          same_step_tx(block, expected),
        )
      walk_inline(block, next, inline, rest)
    }
  }
}

fn field_items(
  block: fixture.NativeBlockFixtureV1,
  field_index: Int,
) -> List<ByteArray> {
  list.map(
    if field_index == 0 {
      block.tx.body.inputs
    } else {
      block.tx.body.reference_inputs
    },
    encode_midgard_tx_input,
  )
}

/// Walk `steps` through the resolved-output door, in the order given.
fn walk_resolved(
  block: fixture.NativeBlockFixtureV1,
  state: reconstruction.StateV1,
  steps: List<ResolvedStep>,
) -> reconstruction.StateV1 {
  when steps is {
    [] -> state
    [step, ..rest] -> {
      let descriptor = descriptor_cbor(step.input, step.script)
      let key = encode_midgard_tx_input(step.input)
      let next = reconstruction.scan_resolved_output_v1(
        state,
        key,
        when step.script is {
          None -> None
          Some(script) -> {
            let script_cbor = encode_midgard_versioned_script(script)
            Some(
              reconstruction.SelectedSourceV1 {
                source_index: state.source_cursor,
                origin_kind: 1,
                source_key: key,
                language_tag: 0,
                script_hash: script_proof_v1.versioned_script_hash(script),
                total_length: bytearray.length(script_cbor),
                item_commitment: bounded_item_v1.from_bytes(
                  2,
                  step.input.output_index,
                  script_cbor,
                ),
              },
            )
          }
        },
        fixture.step_script_hash,
      )
      let expected: Data = next
      let tx = same_step_tx(block, expected)
      expect
        resolved_door.main.spend(
          fixture.next_step_script_hash,
          fixture.computation_thread_policy_id,
          fixture.test_certificate_policy_id,
          datum(state),
          ct.Continue(
            ScanResolvedOutput {
              input_index: 0,
              output_index: 0,
              field_index: step.field_index,
              item_index: step.item_index,
              inputs_opening: fixture.inline_body_opening(
                block.compact_cbor,
                field_items(block, step.field_index),
              ),
              descriptor_cbor: descriptor,
              membership: RedeemerCarriedMembership {
                membership_proof: [],
                membership_proof_script_redeemer_index: 0,
              },
            },
          ),
          fixture.own_out_ref(),
          Transaction {
            ..tx,
            redeemers: [
              fixture.phas_withdraw_redeemer_entry(prior_root, key, descriptor),
            ],
          },
        )
      walk_resolved(block, next, rest)
    }
  }
}

/// Close the resolved walk through the door, handing the source to step 03.
fn finish_resolved(
  block: fixture.NativeBlockFixtureV1,
  state: reconstruction.StateV1,
) -> rule.AuthenticatedExecutionSourceV1 {
  let source = reconstruction.finish_resolved_outputs_v1(
    state,
    list.length(block.tx.body.inputs) + list.length(
      block.tx.body.reference_inputs,
    ),
  )
  let expected: Data = source
  expect
    resolved_door.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.test_certificate_policy_id,
      datum(state),
      ct.Continue(
        FinishResolvedOutputs {
          input_index: 0,
          output_index: 0,
          native_tx_compact_cbor: block.compact_cbor,
          spend_inputs_carriage: Inline {
            preimage: encode_field_preimage(field_items(block, 0)),
          },
          reference_inputs_carriage: Inline {
            preimage: encode_field_preimage(field_items(block, 1)),
          },
        },
      ),
      fixture.own_out_ref(),
      next_step_tx(block, expected),
    )
  source
}

fn reconstruct(
  script: MidgardVersionedScript,
  inline: List<MidgardVersionedScript>,
  steps: List<ResolvedStep>,
) -> rule.AuthenticatedExecutionSourceV1 {
  let block = block_with_inline(inline)
  let references = walk_inline(
    block,
    selected_state(block, script),
    inline,
    inline,
  )
  finish_resolved(block, walk_resolved(block, references, steps))
}

/// Step 03 binds the source item to the script bytes; returns step 04's state.
fn bind_source(
  inline: List<MidgardVersionedScript>,
  source: rule.AuthenticatedExecutionSourceV1,
  script: MidgardVersionedScript,
) -> step_04.State {
  let block = block_with_inline(inline)
  let script_item_cbor = encode_midgard_versioned_script(script)
  let evaluator_state = step_04.State {
    direction: substrate.direction_wrongful_acceptance,
    source_kind: source.bound.subject.source_kind,
    execution_index: source.bound.execution_index,
    source_index: source.source_index,
    origin_kind: source.origin_kind,
    item_commitment: source.item_commitment,
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
    script_item_hash: blake2b_256(script_item_cbor),
    validity_interval_start: block.compact.body.validity_interval_start,
    validity_interval_end: block.compact.body.validity_interval_end,
  }
  let expected: Data = evaluator_state
  expect
    step_03_door.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      datum(source),
      ct.Continue(
        step_03.Args { input_index: 0, output_index: 0, script_item_cbor },
      ),
      fixture.own_out_ref(),
      next_step_tx(block, expected),
    )
  evaluator_state
}

/// Step 04 evaluates the bound native script and finalizes the proof.
fn finalizes(
  inline: List<MidgardVersionedScript>,
  evaluator_state: step_04.State,
  script: MidgardVersionedScript,
) -> Bool {
  let block = block_with_inline(inline)
  step_04_door.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    datum(evaluator_state),
    ct.Continue(
      step_04.DirectFinalize {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        script_item_cbor: encode_midgard_versioned_script(script),
        addr_tx_wits_opening: fixture.inline_witness_opening(
          block.compact_cbor,
          block.witness_set_compact,
          [encode_midgard_address_witness(fixture.sample_address_witness())],
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
  )
}

fn convicts(
  script: MidgardVersionedScript,
  inline: List<MidgardVersionedScript>,
  steps: List<ResolvedStep>,
) -> Bool {
  finalizes(
    inline,
    bind_source(inline, reconstruct(script, inline, steps), script),
    script,
  )
}

// ---------------------------------------------------------------------------
// Rows
// ---------------------------------------------------------------------------

/// The native script is carried only by the spent output's reference script.
test accepted_spent_output_native_source_convicts() {
  let script = false_script()
  let source = reconstruct(script, [], [spent(Some(script)), referenced(None)])
  and {
    source.origin_kind == 1,
    source.source_key == encode_midgard_tx_input(spent_input()),
    source.source_index == 0,
    convicts(script, [], [spent(Some(script)), referenced(None)]),
  }
}

/// Honest negative: the same spent-output source is found and bound, and
/// step 04 refuses because the script evaluates true.
test accepted_spent_output_satisfied_native_source_reaches_the_evaluator() {
  let script = true_script()
  let source = reconstruct(script, [], [spent(Some(script)), referenced(None)])
  let evaluator_state = bind_source([], source, script)
  evaluator_state.origin_kind == 1
}

test accepted_spent_output_satisfied_native_source_is_refused() fail {
  convicts(true_script(), [], [spent(Some(true_script())), referenced(None)])
}

/// Control: the source is the reference input's reference script.
test accepted_reference_input_native_source_convicts() {
  let script = false_script()
  let source = reconstruct(script, [], [spent(None), referenced(Some(script))])
  and {
    source.origin_kind == 1,
    source.source_key == encode_midgard_tx_input(reference_input()),
    convicts(script, [], [spent(None), referenced(Some(script))]),
  }
}

/// Control: the source is the only inline witness, and no resolved output
/// carries the hash.
test accepted_inline_native_source_convicts() {
  let script = false_script()
  let source = reconstruct(script, [script], [spent(None), referenced(None)])
  and {
    source.origin_kind == 0,
    source.source_index == 0,
    convicts(script, [script], [spent(None), referenced(None)]),
  }
}

/// Lookup is reference-first: a spent output carrying the hash is selected
/// over an inline witness carrying it, at physical index `inline_count + 0`.
test accepted_resolved_source_is_looked_up_before_inline() {
  let script = false_script()
  let source = reconstruct(
    script,
    [script],
    [spent(Some(script)), referenced(None)],
  )
  and {
    source.origin_kind == 1,
    source.source_key == encode_midgard_tx_input(spent_input()),
    source.source_index == 1,
  }
}

/// Two resolved outputs carry the hash: the first in canonical out-ref order
/// is selected.
test accepted_first_resolved_source_in_out_ref_order_is_selected() {
  let script = false_script()
  let source = reconstruct(
    script,
    [],
    [spent(Some(script)), referenced(Some(script))],
  )
  and {
    source.source_key == encode_midgard_tx_input(spent_input()),
    source.source_index == 0,
  }
}

/// No source carries the hash: there is nothing to hand to step 03.
test accepted_missing_native_source_is_refused() fail {
  let script = false_script()
  reconstruct(script, [], [spent(None), referenced(None)]).origin_kind == 1
}

/// The walk cannot finish while an output is unwalked.
test accepted_resolved_walk_refuses_an_early_finish() fail {
  let script = false_script()
  reconstruct(script, [], [spent(Some(script))]).origin_kind == 1
}

/// Keys must ascend across both fields: the reference input cannot be walked
/// before the spent input whose key precedes it.
test accepted_resolved_walk_refuses_descending_keys() fail {
  let script = false_script()
  reconstruct(script, [], [referenced(None), spent(Some(script))]).origin_kind == 1
}

/// The same out-ref cannot be walked twice to reach the fields' total.
test accepted_resolved_walk_refuses_a_repeated_output() fail {
  let script = false_script()
  reconstruct(script, [], [spent(Some(script)), spent(Some(script))]).origin_kind == 1
}

/// Only spend and reference inputs are resolved outputs.
test accepted_resolved_walk_refuses_another_field() fail {
  let script = false_script()
  reconstruct(
    script,
    [],
    [ResolvedStep { ..spent(Some(script)), field_index: 2 }, referenced(None)],
  ).origin_kind == 1
}
