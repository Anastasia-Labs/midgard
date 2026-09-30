//// The redeemer fold at context stage 9. The script-context redeemer map is
//// in ledger order, (tag, index) ascending, which is the purpose frontier's
//// order, so the fold walks the execution frontier downwards from the
//// purpose bound and prepends. The execution leaf at `purpose_bound - 1` was
//// appended with that purpose's own leaf, source and redeemer item, so each
//// pre-state admits exactly one step: a select when the leaf names a
//// redeemer, a skip when it is a native execution. A step verifier that
//// admitted two successors would let a challenger win against an honest
//// block.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_redeemer_witness, encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardExecutionUnits, MidgardRedeemerWitness, MidgardTransaction,
  MidgardTransactionWitnessSet, MidgardTxInput, MintRedeemer, ReceiveRedeemer,
  RewardRedeemer, SpendRedeemer,
}
use midgard/redeemer_item_proof_v1
use midgard/script_context_v1
use midgard/script_proof_v1
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/shared as vm_shared
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{h_28_a, h_28_b, h_a, h_b}
use midgard/validation_machine_tests/fixtures_phase_a.{
  cek_context_native_control_fixture, cek_state_fixture,
  empty_cek_context_control_fixture, empty_v1_transaction, inline_field_door,
  one_step_evidence,
}
use midgard/validation_merkle_v1

/// One execution-frontier entry: the purpose it runs for, the language of
/// the script that runs, its source leaf and the witness-list redeemer item
/// it consumes (`None` for a native script).
type Execution {
  language_tag: Int,
  purpose_kind: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  source_leaf: ByteArray,
  item: Option<Int>,
}

/// A two-purpose transaction: its witness list, its execution frontier in
/// ledger order, and the language of the script whose context is built.
type Fold {
  redeemers: List<MidgardRedeemerWitness>,
  executions: List<Execution>,
  context_language: Int,
}

fn redeemer(purpose, value: Int) -> MidgardRedeemerWitness {
  MidgardRedeemerWitness {
    purpose,
    index: 0,
    redeemer_cbor: cbor.serialise(builtin.i_data(value)),
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
}

fn spend_subject() -> ByteArray {
  encode_midgard_tx_input(MidgardTxInput { tx_id: h_b, output_index: 0 })
}

fn spend(language_tag: Int, item: Option<Int>) -> Execution {
  Execution {
    language_tag,
    purpose_kind: 0,
    script_hash: h_28_a,
    subject: spend_subject(),
    source_leaf: h_a,
    item,
  }
}

fn above_spend(
  language_tag: Int,
  purpose_kind: Int,
  item: Option<Int>,
) -> Execution {
  Execution {
    language_tag,
    purpose_kind,
    script_hash: h_28_b,
    subject: h_28_b,
    source_leaf: h_b,
    item,
  }
}

/// Two Plutus scripts. The witness list holds the mint redeemer before the
/// spend redeemer, the reverse of ledger order: item 0 is the mint (frontier
/// index 1), item 1 the spend (frontier index 0).
fn spend_and_mint() -> Fold {
  Fold {
    redeemers: [redeemer(MintRedeemer, 1), redeemer(SpendRedeemer, 0)],
    executions: [spend(3, Some(1)), above_spend(3, 1, Some(0))],
    context_language: 3,
  }
}

/// A native mint policy above a Plutus spend: the fold skips the mint and
/// then selects the spend, the one redeemer.
fn native_above_plutus() -> Fold {
  Fold {
    redeemers: [redeemer(SpendRedeemer, 0)],
    executions: [spend(3, Some(0)), above_spend(0, 1, None)],
    context_language: 3,
  }
}

/// A native spend below a Plutus mint.
fn plutus_above_native() -> Fold {
  Fold {
    redeemers: [redeemer(MintRedeemer, 1)],
    executions: [spend(0, None), above_spend(3, 1, Some(0))],
    context_language: 3,
  }
}

fn spend_and_receive(language_tag: Int) -> Fold {
  Fold {
    redeemers: [redeemer(ReceiveRedeemer, 1), redeemer(SpendRedeemer, 0)],
    executions: [
      spend(language_tag, Some(1)),
      above_spend(language_tag, 3, Some(0)),
    ],
    context_language: language_tag,
  }
}

fn spend_and_observe() -> Fold {
  Fold {
    redeemers: [redeemer(RewardRedeemer, 1), redeemer(SpendRedeemer, 0)],
    executions: [spend(3, Some(1)), above_spend(3, 2, Some(0))],
    context_language: 3,
  }
}

fn item_cbor(fold: Fold, item: Int) -> ByteArray {
  expect Some(witness) = list.at(fold.redeemers, item)
  encode_midgard_redeemer_witness(witness)
}

fn item_commitment(fold: Fold, item: Int) -> ByteArray {
  bounded_item_v1.from_bytes(8, item, item_cbor(fold, item))
}

/// The item's chunk-frontier root. Every item here fits one chunk.
fn item_frontier_root(fold: Fold, item: Int) -> ByteArray {
  let bytes = item_cbor(fold, item)
  expect bounded_item_v1.chunk_count(bytearray.length(bytes)) == 1
  let built = validation_merkle_v1.build_frontier(
    [bounded_item_v1.hash_chunk(8, item, 0, bytes)],
  )
  validation_merkle_v1.frontier_commitment(built.count, built.peaks)
}

fn redeemer_leaf(fold: Fold, item: Int) -> ByteArray {
  script_proof_v1.redeemer_item_leaf_hash(item, item_commitment(fold, item))
}

fn purpose_leaf(execution: Execution) -> ByteArray {
  script_proof_v1.purpose_leaf_hash(
    execution.purpose_kind,
    0,
    execution.script_hash,
    execution.subject,
  )
}

fn execution_leaf(fold: Fold, execution: Execution) -> ByteArray {
  script_proof_v1.execution_leaf_hash(
    execution.language_tag,
    purpose_leaf(execution),
    execution.source_leaf,
    when execution.item is {
      Some(item) -> redeemer_leaf(fold, item)
      None -> #""
    },
  )
}

fn execution_at(fold: Fold, index: Int) -> Execution {
  expect Some(execution) = list.at(fold.executions, index)
  execution
}

/// The genuine membership path of frontier index `index`: with two leaves it
/// is the other leaf.
fn execution_siblings(fold: Fold, index: Int) -> List<ByteArray> {
  [execution_leaf(fold, execution_at(fold, 1 - index))]
}

fn initial(fold: Fold) -> vm_machine_types.CekRedeemerContextControlV1 {
  vm_witness.initial_cek_redeemer_context_control_v1(
    list.length(fold.executions),
  )
}

/// A select of the purpose at frontier `at` consuming witness item `item`,
/// with every field genuine except that `length_delta` is added to the
/// item's length and `language_tag` replaces the execution's language.
fn select_witness(
  fold: Fold,
  control: vm_machine_types.CekRedeemerContextControlV1,
  at: Int,
  item: Int,
  language_tag: Int,
  length_delta: Int,
) -> vm_machine_types.ValidationAuxiliaryWitnessV1 {
  let execution = execution_at(fold, at)
  vm_machine_types.CekRedeemerContextSelectWitness {
    control,
    item_index: item,
    total_length: bytearray.length(item_cbor(fold, item)) + length_delta,
    item_commitment: item_commitment(fold, item),
    purpose_kind: execution.purpose_kind,
    purpose_index: 0,
    script_hash: execution.script_hash,
    subject: execution.subject,
    execution_language_tag: language_tag,
    source_leaf: execution.source_leaf,
    execution_siblings: execution_siblings(fold, at),
    item_frontier_root: item_frontier_root(fold, item),
  }
}

fn honest_select(
  fold: Fold,
  control: vm_machine_types.CekRedeemerContextControlV1,
  at: Int,
  item: Int,
) -> vm_machine_types.ValidationAuxiliaryWitnessV1 {
  select_witness(
    fold,
    control,
    at,
    item,
    execution_at(fold, at).language_tag,
    0,
  )
}

fn skip_witness(
  fold: Fold,
  control: vm_machine_types.CekRedeemerContextControlV1,
  at: Int,
) -> vm_machine_types.ValidationAuxiliaryWitnessV1 {
  let execution = execution_at(fold, at)
  vm_machine_types.CekRedeemerContextSkipWitness {
    control,
    purpose_leaf: purpose_leaf(execution),
    source_leaf: execution.source_leaf,
    execution_siblings: execution_siblings(fold, at),
  }
}

/// The redeemer control a select of the purpose at frontier `at` yields,
/// seen through the Midgard purpose encoding when `midgard_view` holds. The
/// bound always moves to `purpose_bound - 1`, whatever the witness names.
fn select_successor(
  fold: Fold,
  control: vm_machine_types.CekRedeemerContextControlV1,
  at: Int,
  item: Int,
  length_delta: Int,
  midgard_view: Bool,
) -> vm_machine_types.CekRedeemerContextControlV1 {
  let execution = execution_at(fold, at)
  let summary = script_context_v1.script_purpose_summary_v1(
    execution.purpose_kind,
    execution.script_hash,
    execution.subject,
    midgard_view,
  )
  expect Some(tag) = vm_shared.redeemer_tag_for_purpose_kind_v1(
    execution.purpose_kind,
  )
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    when summary is {
      None -> redeemer_item_proof_v1.mode_descriptor
      Some(_) -> redeemer_item_proof_v1.mode_data
    },
    item,
    list.length(fold.redeemers),
    bytearray.length(item_cbor(fold, item)) + length_delta,
    item_commitment(fold, item),
    tag,
    0,
  )
  vm_machine_types.CekRedeemerContextControlV1 {
    ..control,
    active_scan_hash: redeemer_item_proof_v1.hash_control_v1(item_control),
    active_redeemer_leaf: redeemer_leaf(fold, item),
    active_purpose: when summary is {
      None -> control.active_purpose
      Some(purpose) -> purpose
    },
    purpose_bound: control.purpose_bound - 1,
  }
}

fn honest_select_successor(
  fold: Fold,
  control: vm_machine_types.CekRedeemerContextControlV1,
  at: Int,
  item: Int,
) -> vm_machine_types.CekRedeemerContextControlV1 {
  select_successor(fold, control, at, item, 0, fold.context_language == 128)
}

fn skip_successor(
  control: vm_machine_types.CekRedeemerContextControlV1,
) -> vm_machine_types.CekRedeemerContextControlV1 {
  vm_machine_types.CekRedeemerContextControlV1 {
    ..control,
    purpose_bound: control.purpose_bound - 1,
  }
}

/// One stage-9 step of the fold from `control` to `next`, through the
/// monolithic one-step verifier.
fn fold_step(
  fold: Fold,
  control: vm_machine_types.CekRedeemerContextControlV1,
  auxiliary: vm_machine_types.ValidationAuxiliaryWitnessV1,
  next: vm_machine_types.CekRedeemerContextControlV1,
) -> Bool {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: fold.redeemers,
    },
  }
  let redeemer_count = list.length(fold.redeemers)
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    redeemer_count,
    redeemer_peaks: validation_merkle_v1.build_frontier(
      list.indexed_map(
        fold.redeemers,
        fn(index, _) { redeemer_leaf(fold, index) },
      ),
    ).peaks,
    purpose_count: 2,
    purpose_peaks: validation_merkle_v1.build_frontier(
      list.map(fold.executions, purpose_leaf),
    ).peaks,
    execution_count: 2,
    execution_peaks: validation_merkle_v1.build_frontier(
      list.map(fold.executions, fn(e) { execution_leaf(fold, e) }),
    ).peaks,
    execution_cursor: 2,
  }
  // The context's observer collection is encoded for its language.
  let observer_items = vm_witness.empty_observer_items_v1(fold.context_language)
  let context_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(9),
    language_tag: fold.context_language,
    subject: spend_subject(),
    observer_items,
    observer_summary: vm_shared.finalize_cek_observer_items_v1(
      observer_items,
      fold.context_language == 128,
    ),
    mint_summary: cek_data_v1.map_data_summary_v1(
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
    redeemer_context_control_hash: vm_witness.hash_cek_redeemer_context_control_v1(
      control,
    ),
  }
  let next_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    redeemer_context_control_hash: vm_witness.hash_cek_redeemer_context_control_v1(
      next,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    53,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(next_control),
    0,
    54,
  )
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(work_witness_cbor, post, auxiliary),
    inline_field_door(),
  )
}

fn honest_select_step(
  fold: Fold,
  control: vm_machine_types.CekRedeemerContextControlV1,
  at: Int,
  item: Int,
) -> Bool {
  fold_step(
    fold,
    control,
    honest_select(fold, control, at, item),
    honest_select_successor(fold, control, at, item),
  )
}

/// The control after the mint (frontier index 1) was selected and folded.
fn after_mint() -> vm_machine_types.CekRedeemerContextControlV1 {
  expect Some(mint) = script_context_v1.script_purpose_summary_v1(
    1,
    h_28_b,
    h_28_b,
    False,
  )
  vm_machine_types.CekRedeemerContextControlV1 {
    ..initial(spend_and_mint()),
    cursor: 1,
    map_items: cek_data_v1.prepend_data_pair_summary_v1(
      mint,
      cek_data_v1.semantic_data_summary_v1(builtin.i_data(1)),
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
    purpose_bound: 1,
  }
}

/// The control after the spend (frontier index 0) was selected and folded
/// first, as the retired witness-list order did.
fn after_spend() -> vm_machine_types.CekRedeemerContextControlV1 {
  expect Some(spend_purpose) = script_context_v1.script_purpose_summary_v1(
    0,
    h_28_a,
    spend_subject(),
    False,
  )
  let value = cek_data_v1.semantic_data_summary_v1(builtin.i_data(0))
  vm_machine_types.CekRedeemerContextControlV1 {
    ..initial(spend_and_mint()),
    cursor: 1,
    map_items: cek_data_v1.prepend_data_pair_summary_v1(
      spend_purpose,
      value,
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
    current_redeemer: value,
    purpose_bound: 0,
  }
}

// M1. The first select takes the highest purpose, the mint.
test redeemer_fold_selects_the_highest_purpose_first() {
  honest_select_step(spend_and_mint(), initial(spend_and_mint()), 1, 0)
}

// M2. The next select takes the next lower purpose, the spend.
test redeemer_fold_then_selects_the_next_lower_purpose() {
  honest_select_step(spend_and_mint(), after_mint(), 0, 1)
}

// M3. From the first state a select could name the spend and pass over the
// mint, reaching a successor the honest fold never reaches. Every field is
// genuine for the spend, but the execution leaf at `purpose_bound - 1` is
// the mint's, so [execution-leaf] refuses it.
test redeemer_fold_refuses_a_select_that_skips_a_purpose() fail {
  let fold = spend_and_mint()
  fold_step(
    fold,
    initial(fold),
    honest_select(fold, initial(fold), 0, 1),
    honest_select_successor(fold, initial(fold), 0, 1),
  )
}

// M3's pair: the same select from the bound it belongs to is accepted, so the
// frontier index alone refuses it.
test redeemer_fold_select_that_skips_hinges_on_the_frontier() {
  let fold = spend_and_mint()
  let control = vm_machine_types.CekRedeemerContextControlV1 {
    ..initial(fold),
    purpose_bound: 1,
  }
  honest_select_step(fold, control, 0, 1)
}

// M4. The right purpose with the other purpose's redeemer item: the execution
// leaf binds the redeemer leaf too.
test redeemer_fold_refuses_a_select_with_another_redeemer_item() fail {
  let fold = spend_and_mint()
  fold_step(
    fold,
    initial(fold),
    honest_select(fold, initial(fold), 1, 1),
    honest_select_successor(fold, initial(fold), 1, 1),
  )
}

// M5. The retired witness-list order folded the spend first. Its second
// select names the mint at frontier index 1 from the bound 0 that step left.
test redeemer_fold_refuses_the_witness_list_order() fail {
  honest_select_step(spend_and_mint(), after_spend(), 1, 0)
}

test redeemer_fold_witness_list_order_hinges_on_the_bound() {
  honest_select_step(
    spend_and_mint(),
    vm_machine_types.CekRedeemerContextControlV1 {
      ..after_spend(),
      purpose_bound: 2,
    },
    1,
    0,
  )
}

test redeemer_fold_refuses_the_same_purpose_twice() fail {
  honest_select_step(spend_and_mint(), after_mint(), 1, 0)
}

test redeemer_fold_same_purpose_twice_hinges_on_the_bound() {
  honest_select_step(
    spend_and_mint(),
    vm_machine_types.CekRedeemerContextControlV1 {
      ..after_mint(),
      purpose_bound: 2,
    },
    1,
    0,
  )
}

// M6. A native execution above a Plutus one: the fold skips it, then selects
// the spend from the lowered bound.
test redeemer_fold_skips_a_native_execution() {
  let fold = native_above_plutus()
  fold_step(
    fold,
    initial(fold),
    skip_witness(fold, initial(fold), 1),
    skip_successor(initial(fold)),
  )
}

test redeemer_fold_selects_below_a_skipped_native_execution() {
  let fold = native_above_plutus()
  honest_select_step(fold, skip_successor(initial(fold)), 0, 0)
}

// M6's negative: the spend cannot be selected over the native execution.
test redeemer_fold_refuses_a_select_over_a_native_execution() fail {
  let fold = native_above_plutus()
  honest_select_step(fold, initial(fold), 0, 0)
}

// M7. A skip where the frontier leaf names a redeemer: the purpose and source
// leaves are genuine, but the leaf is not a native execution, so
// [native-execution] refuses it. M1 is the honest step from the same state.
test redeemer_fold_refuses_a_skip_of_a_plutus_execution() fail {
  let fold = spend_and_mint()
  fold_step(
    fold,
    initial(fold),
    skip_witness(fold, initial(fold), 1),
    skip_successor(initial(fold)),
  )
}

// M8. The item commitment fixes the item's length, so a select cannot hand
// the item proof a different `total_length`.
test redeemer_fold_refuses_a_forged_item_length() fail {
  let fold = spend_and_mint()
  let control = initial(fold)
  fold_step(
    fold,
    control,
    select_witness(fold, control, 1, 0, 3, 1),
    select_successor(fold, control, 1, 0, 1, False),
  )
}

// M9. There is no execution leaf below frontier index 0.
test redeemer_fold_refuses_a_skip_at_bound_zero() fail {
  let fold = plutus_above_native()
  let control = vm_machine_types.CekRedeemerContextControlV1 {
    ..initial(fold),
    purpose_bound: 0,
  }
  fold_step(
    fold,
    control,
    skip_witness(fold, control, 0),
    skip_successor(control),
  )
}

// M9's pair: the same skip from bound 1 is accepted.
test redeemer_fold_skip_at_bound_zero_hinges_on_the_bound() {
  let fold = plutus_above_native()
  let control = vm_machine_types.CekRedeemerContextControlV1 {
    ..initial(fold),
    purpose_bound: 1,
  }
  fold_step(
    fold,
    control,
    skip_witness(fold, control, 0),
    skip_successor(control),
  )
}

// M10. A select cannot claim a native execution's language: a native leaf
// carries no redeemer leaf.
test redeemer_fold_refuses_a_select_with_the_native_language() fail {
  let fold = native_above_plutus()
  let control = initial(fold)
  fold_step(
    fold,
    control,
    select_witness(fold, control, 1, 0, 0, 0),
    select_successor(fold, control, 1, 0, 0, False),
  )
}

// M11. A PlutusV3 context does not show Midgard's receive redeemers: the
// select over a receive goes through the descriptor arm and leaves the active
// purpose empty.
test redeemer_fold_selects_a_receive_by_descriptor_in_a_v3_context() {
  let fold = spend_and_receive(3)
  honest_select_step(fold, initial(fold), 1, 0)
}

test redeemer_fold_refuses_a_receive_purpose_in_a_v3_context() fail {
  let fold = spend_and_receive(3)
  fold_step(
    fold,
    initial(fold),
    honest_select(fold, initial(fold), 1, 0),
    select_successor(fold, initial(fold), 1, 0, 0, True),
  )
}

test redeemer_fold_refuses_a_skip_of_a_receive() fail {
  let fold = spend_and_receive(3)
  fold_step(
    fold,
    initial(fold),
    skip_witness(fold, initial(fold), 1),
    skip_successor(initial(fold)),
  )
}

// A MidgardV1 context must show them: the receive (tag 6) goes through the
// data arm. It is the highest purpose, so the fold selects and prepends it
// first and it ends last in the map.
test redeemer_fold_selects_a_receive_by_data_in_a_midgard_context() {
  let fold = spend_and_receive(128)
  honest_select_step(fold, initial(fold), 1, 0)
}

test redeemer_fold_refuses_a_hidden_receive_in_a_midgard_context() fail {
  let fold = spend_and_receive(128)
  fold_step(
    fold,
    initial(fold),
    honest_select(fold, initial(fold), 1, 0),
    select_successor(fold, initial(fold), 1, 0, 0, False),
  )
}

// M12. A PlutusV3 observer goes through the data arm.
test redeemer_fold_selects_an_observer_by_data_in_a_v3_context() {
  let fold = spend_and_observe()
  honest_select_step(fold, initial(fold), 1, 0)
}

test script_purpose_summary_shows_a_receive_only_to_midgard() {
  and {
    script_context_v1.script_purpose_summary_v1(3, h_28_b, h_28_b, True) == Some(
      cek_data_v1.semantic_data_summary_v1(
        builtin.constr_data(3, [builtin.b_data(h_28_b)]),
      ),
    ),
    script_context_v1.script_purpose_summary_v1(3, h_28_b, h_28_b, False) == None,
  }
}
