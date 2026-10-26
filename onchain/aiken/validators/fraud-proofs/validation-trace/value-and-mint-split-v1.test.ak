//// R5 item 1 (#617): validator-level vectors for the ValueAndMint
//// decomposition.
////
//// Index 12 of the validation-dispute machine used to be one aggregate
//// "direct" resolver over six stages. It is now `value_and_mint_v1` (a
//// `prepare_selected` over a group of eleven) plus one semantic validator per
//// auxiliary kind:
////
////    0  begin              stage 0 -> 1, installs the replay schedule
////    1  replay_begin       stage 1 -> 2
////    2  replay_input       stage 2, schedule pending, asset cursor 0
////    3  replay_asset       stage 2, asset cursor > 0
////    4  replay_finish      stage 2, schedule exhausted -> 3
////    5  output_descriptor  stage 3, outputs pending, asset cursor 0
////    6  output_asset       stage 3, asset cursor > 0
////    7  output_finish      stage 3, outputs exhausted -> 4
////    8  mint_asset         stage 4, mints pending
////    9  mint_finish        stage 4, mints exhausted -> 5
////   10  finalize           stage 5 -> accepting Terminal
////
//// These vectors drive all twelve validators through `main.spend` on the
//// smallest honest step of each kind, pin the redeemer wire layouts, show
//// that the prepare refuses the retired group cardinality, and show that a
//// semantic validator refuses the honest steps of its stage neighbours (the
//// pairs a cursor or stage discriminator is all that separates). The full
//// eleven-by-eleven partition is proven at the library level in
//// `lib/midgard/validation-machine-tests/tests-parity.test.ak`
//// (`value_and_mint_kinds_partition_the_value_and_mint_step_space`).
////
//// The stage bodies themselves (`value_and_mint_stage_zero` ..
//// `..._five`) are not re-tested here: the split moved dispatch, not
//// semantics, and every fixture below is shown to be an honest step of the
//// aggregate route before it is used against a validator.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/fuzz
use aiken/merkle_patricia_forestry/helpers.{combine, suffix}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use cardano/address.{Script}
use cardano/assets
use cardano/transaction.{Input,
  Output, Spend, Transaction, Withdraw} as l1_transaction
use env
use fraud_proofs/validation_trace/value_and_mint_asset_fold_yield as asset_fold_yield
use fraud_proofs/validation_trace/value_and_mint_begin_semantic_v1 as begin_validator
use fraud_proofs/validation_trace/value_and_mint_finalize_semantic_v1 as finalize_validator
use fraud_proofs/validation_trace/value_and_mint_mint_asset_semantic_v1 as mint_asset_validator
use fraud_proofs/validation_trace/value_and_mint_mint_finish_semantic_v1 as mint_finish_validator
use fraud_proofs/validation_trace/value_and_mint_output_asset_semantic_v1 as output_asset_validator
use fraud_proofs/validation_trace/value_and_mint_output_descriptor_semantic_v1 as output_descriptor_validator
use fraud_proofs/validation_trace/value_and_mint_output_finish_semantic_v1 as output_finish_validator
use fraud_proofs/validation_trace/value_and_mint_replay_asset_semantic_v1 as replay_asset_validator
use fraud_proofs/validation_trace/value_and_mint_replay_begin_semantic_v1 as replay_begin_validator
use fraud_proofs/validation_trace/value_and_mint_replay_finish_semantic_v1 as replay_finish_validator
use fraud_proofs/validation_trace/value_and_mint_replay_input_semantic_v1 as replay_input_validator
use fraud_proofs/validation_trace/value_and_mint_v1 as prepare_validator
use midgard/cek_data_v1
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/transaction.{midgard_transaction_to_compact}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  TxIsValid, native_tx_version_v1,
}
use midgard/fraud_proofs/thread_census_fixture_v1 as census
use midgard/ledger_output_commitment_v1
use midgard/rejection_reason_v1.{OutputBelowMinAda, RejectionReasonV1}
use midgard/script_proof_v1
use midgard/validation_asset_fold.{asset_fold_role}
use midgard/validation_machine/machine_types.{
  AssetDescriptorClaim, AssetFoldClaim, NativeScriptsControlV1,
  NoAuxiliaryWitness, ResolvedInputReplayWitness, ValidationAuxiliaryWitnessV1,
  ValidationOneStepEvidenceV1, ValidationOneStepWitnessV1,
  ValueAccumulatorAssetLimitExceeded, ValueAccumulatorV1, ValueAndMintControlV1,
  ValueAssetMutationWitnessV1, ValueInputAssetWitness, ValueMintAssetWitness,
  ValueOutputAssetWitness, ValueOutputDescriptorWitness,
}
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/shared as vm_shared
use midgard/validation_machine/value_and_mint as value_fold
use midgard/validation_machine/witness as vm_witness
use midgard/validation_merkle_v1.{FrontierPeak}
use midgard/validation_resolution_v1.{
  PreparedValidationResolutionStateV1, ValidationResolutionStateV1,
  hash_one_step_evidence, prepare_semantic_resolution,
  prepared_resolution_version, resolution_version, winning_resolution,
}
use midgard/validation_resolver_v1.{PrepareSelected}
use midgard/validation_trace_v1

/// The mid-step continue transaction as the thread's fraud prover submits it:
/// prover-only moves need the prover's signature, and the moves anyone may
/// make accept it too.
fn prover_continue_tx(asset_name: ByteArray, output_state: Data) -> Transaction {
  Transaction {
    ..fixture.continue_mid_step_l1_tx_v1(asset_name, output_state),
    extra_signatories: [fixture.fraud_prover_key_hash],
  }
}

// ---------------------------------------------------------------------------
// Shared fixtures
// ---------------------------------------------------------------------------

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const h_28_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_28_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

const validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

const ct_asset = "value-and-mint-split-thread"

const kind_count = 11

/// A deployed group of `count` hashes with the next-step hash in `slot` and
/// the unrelated step hash everywhere else, so a mis-routed index fails on
/// the output script.
fn group_with_slot(slot: Int, count: Int) -> List<ByteArray> {
  if count <= 0 {
    []
  } else {
    let head = if slot == 0 {
      fixture.next_step_script_hash
    } else {
      fixture.step_script_hash
    }
    [head, ..group_with_slot(slot - 1, count - 1)]
  }
}

fn singleton_root(key: ByteArray, value: ByteArray) -> ByteArray {
  combine(suffix(blake2b_256(key), 0), blake2b_256(value))
}

fn empty_v1_transaction() -> MidgardTransaction {
  let empty_mint: Data = builtin.list_data([])
  MidgardTransaction {
    version: native_tx_version_v1,
    validity: TxIsValid,
    body: MidgardTransactionBody {
      inputs: [],
      reference_inputs: [],
      outputs: [],
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers: [],
      required_signers: [],
      mint: empty_mint,
      script_integrity_hash: zero,
      auxiliary_data_hash: zero,
      network_id: 255,
    },
    witness_set: MidgardTransactionWitnessSet {
      addr_tx_wits: [],
      script_tx_wits: [],
      redeemer_tx_wits: [],
    },
  }
}

/// The native control of a witness-free transaction with nothing to
/// resolve, spend, mint or execute.
fn empty_native_control(tx: MidgardTransaction) -> NativeScriptsControlV1 {
  let proof_source = forced_fixture.from_native_transaction(tx)
  let empty = validation_merkle_v1.build_frontier([])
  NativeScriptsControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    spend_input_count: 0,
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    signer_count: empty.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      empty.count,
      empty.peaks,
    ),
    source_count: 0,
    source_peaks: validation_merkle_v1.empty_frontier(),
    redeemer_count: 0,
    redeemer_peaks: validation_merkle_v1.empty_frontier(),
    purpose_count: 0,
    purpose_peaks: validation_merkle_v1.empty_frontier(),
    output_count: 0,
    output_peaks: validation_merkle_v1.empty_frontier(),
    output_descriptor_peaks: validation_merkle_v1.empty_frontier(),
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
    execution_count: 0,
    execution_peaks: validation_merkle_v1.empty_frontier(),
    execution_cursor: 0,
    language_bitmap: 0,
    resolution_schedule_hash: vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
}

/// A ValueAndMint control at `stage` with every cursor at zero, every
/// accumulator initial and no replay schedule.
fn idle_control(
  native_control: NativeScriptsControlV1,
  stage: Int,
) -> ValueAndMintControlV1 {
  ValueAndMintControlV1 {
    native_control,
    stage,
    replay_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_asset_cursor: 0,
    replay_value_hash: zero,
    replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    output_cursor: 0,
    output_asset_cursor: 0,
    mint_cursor: 0,
    value_accumulator: ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: null_hash,
      asset_entry_count: 0,
      nonzero_asset_count: 0,
    },
  }
}

fn state_of(
  tx: MidgardTransaction,
  control: ValueAndMintControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = vm_witness.encode_value_and_mint_control_v1(control)
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id: tx_id,
      transaction_commitment: forced_fixture.commitment_of_native_transaction(
        tx,
      ),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: h_c,
      phase: validation_trace_v1.ValueAndMint,
      program_counter,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ValueAndMint,
        program_counter,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
    },
    work_witness_cbor,
  )
}

/// One honest ValueAndMint step of some kind.
type StepFixtureV1 {
  kind: Int,
  pre: validation_trace_v1.ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
}

/// One ValueAndMint-to-ValueAndMint step from `control` to `next_control`.
fn control_step(
  kind: Int,
  tx: MidgardTransaction,
  control: ValueAndMintControlV1,
  next_control: ValueAndMintControlV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> StepFixtureV1 {
  let Pair(pre, work_witness_cbor) = state_of(tx, control, 40)
  let Pair(post, _) = state_of(tx, next_control, 41)
  StepFixtureV1 {
    kind,
    pre,
    transition: ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary,
  }
}

fn empty_summary() -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 { root: h_a, cbor_length: 0, memory: 0 }
}

/// The min-ADA rejection naming the descriptor's output (cursor 0). Written out
/// here rather than read off the validator, so the rejection vector below pins
/// the exact reason a fraud proof has to carry.
const reject_min_ada_reason: RejectionReasonV1 =
  OutputBelowMinAda { output_index: 0 }

/// The serialized canonical length every descriptor fixture in this module
/// declares, and the exact minimum-Ada floor for it at the production rate.
/// The floor is derived from `env.coins_per_utxo_byte` rather than written
/// out, so a rate change moves these fixtures with the rule.
const descriptor_total_length: Int = 16384

fn descriptor_min_ada_floor() -> Int {
  vm_shared.min_ada_lovelace_v1(
    env.coins_per_utxo_byte,
    descriptor_total_length,
  )
}

/// A ledger output descriptor with `lovelace` lovelace and `asset_count`
/// (zero or one) copies of the asset `(h_28_a, "beef", 7)`.
///
/// RE-AUTHORED, NOT SUPPRESSED (#618 ruling 1). `lovelace` became a parameter
/// when the ValueAndMint output-descriptor step began convicting an
/// under-funded output with `E_MIN_ADA`: the stage-three output fold now has
/// to fund the floor, while the stage-two RESOLVED-INPUT replay fixtures keep
/// their original 3-lovelace descriptor. That asymmetry is the rule, not an
/// oversight -- the wiring gates outputs a transaction PRODUCES (MIN-ADA-TX),
/// not inputs it resolves from prior state, whose under-funding is the
/// separate MIN-ADA-UTXO shape Q27 owns.
fn descriptor_with_assets(
  asset_count: Int,
  lovelace: Int,
) -> ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
  let asset_frontier = if asset_count == 0 {
    validation_merkle_v1.build_frontier([])
  } else {
    validation_merkle_v1.build_frontier(
      [ledger_output_commitment_v1.asset_leaf_hash(h_28_a, #"beef", 7)],
    )
  }
  ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index: 0,
    total_length: descriptor_total_length,
    item_commitment: h_b,
    address: #"60" |> bytearray.concat(h_28_b),
    lovelace,
    asset_count,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(
      asset_frontier.count,
      asset_frontier.peaks,
    ),
    cardano_value_size: 5000,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out: empty_summary(),
    midgard_tx_out: empty_summary(),
    cardano_spend_datum: empty_summary(),
  }
}

/// The replayed input every stage-2 fixture resolves.
const replayed_input_key =
  #"8258200101010101010101010101010101010101010101010101010101010101010101010100"

/// A native control whose single resolved input is `replayed_input_key`
/// resolving to `descriptor_cbor`, with the matching one-node schedule and the
/// one-leaf resolved-item frontier ScriptSources stage 3 builds for it.
fn single_replayed_input_native_control(
  tx: MidgardTransaction,
  descriptor_cbor: ByteArray,
) -> NativeScriptsControlV1 {
  NativeScriptsControlV1 {
    ..empty_native_control(tx),
    resolved_input_count: 1,
    spend_input_count: 1,
    resolved_item_peaks: validation_merkle_v1.build_frontier(
      [
        script_proof_v1.resolved_context_item_leaf_hash(
          0,
          0,
          replayed_input_key,
          descriptor_cbor,
        ),
      ],
    ).peaks,
    resolution_schedule_hash: vm_witness.resolution_schedule_node_hash(
      0,
      0,
      replayed_input_key,
      vm_witness.empty_resolution_schedule_hash(),
    ),
  }
}

fn no_mutation() -> ValueAssetMutationWitnessV1 {
  ValueAssetMutationWitnessV1 {
    delta_was_present: False,
    old_delta: 0,
    delta_proof: [],
  }
}

/// Kind 0: `begin` installs the native resolution schedule as the replay
/// schedule.
fn begin_fixture() -> StepFixtureV1 {
  let tx = empty_v1_transaction()
  let native_control = NativeScriptsControlV1 {
    ..empty_native_control(tx),
    resolution_schedule_hash: h_a,
  }
  let control = idle_control(native_control, 0)
  control_step(
    0,
    tx,
    control,
    ValueAndMintControlV1 {
      ..control,
      stage: 1,
      replay_schedule_hash: h_a,
      replay_remaining_schedule_hash: h_a,
    },
    NoAuxiliaryWitness,
  )
}

/// A transition-only step from an idle control at `stage` to `stage + 1`.
fn idle_stage_step(kind: Int, stage: Int) -> StepFixtureV1 {
  let tx = empty_v1_transaction()
  let control = idle_control(empty_native_control(tx), stage)
  control_step(
    kind,
    tx,
    control,
    ValueAndMintControlV1 { ..control, stage: stage + 1 },
    NoAuxiliaryWitness,
  )
}

fn replay_begin_fixture() -> StepFixtureV1 {
  idle_stage_step(1, 1)
}

/// Kind 2: `replay_input` over an asset-free descriptor completes the input
/// in one step.
fn replay_input_fixture() -> StepFixtureV1 {
  replay_input_fixture_over(3, 3, [])
}

/// Kind 2 over a frontier that resolves the input to a `ledger_lovelace`
/// descriptor, replaying a `replayed_lovelace` descriptor with `siblings`. The
/// claimed successor is the one the replayed descriptor yields, so only the
/// frontier membership tells the two apart.
fn replay_input_fixture_over(
  ledger_lovelace: Int,
  replayed_lovelace: Int,
  siblings: List<ByteArray>,
) -> StepFixtureV1 {
  let tx = empty_v1_transaction()
  let ledger_cbor = ledger_output_commitment_v1.encode(
    descriptor_with_assets(0, ledger_lovelace),
  )
  let descriptor_cbor = ledger_output_commitment_v1.encode(
    descriptor_with_assets(0, replayed_lovelace),
  )
  let native_control = single_replayed_input_native_control(tx, ledger_cbor)
  let control = ValueAndMintControlV1 {
    ..idle_control(native_control, 2),
    replay_schedule_hash: native_control.resolution_schedule_hash,
    replay_remaining_schedule_hash: native_control.resolution_schedule_hash,
  }
  control_step(
    2,
    tx,
    control,
    ValueAndMintControlV1 {
      ..control,
      replay_cursor: 1,
      replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
      value_accumulator: ValueAccumulatorV1 {
        ..control.value_accumulator,
        lovelace_delta: replayed_lovelace,
      },
    },
    ResolvedInputReplayWitness {
      source_kind: 0,
      item_index: 0,
      key: replayed_input_key,
      next_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
      value: descriptor_cbor,
      proof: [],
      siblings,
    },
  )
}

/// Kind 3: `replay_asset` folds the single asset of a one-asset descriptor,
/// which also completes the input.
fn replay_asset_fixture() -> StepFixtureV1 {
  let tx = empty_v1_transaction()
  let descriptor_cbor = ledger_output_commitment_v1.encode(
    descriptor_with_assets(1, 3),
  )
  let native_control = single_replayed_input_native_control(tx, descriptor_cbor)
  let unit = h_28_a |> bytearray.concat(#"beef")
  let control = ValueAndMintControlV1 {
    ..idle_control(native_control, 2),
    replay_schedule_hash: native_control.resolution_schedule_hash,
    replay_remaining_schedule_hash: native_control.resolution_schedule_hash,
    replay_asset_cursor: 1,
    replay_value_hash: blake2b_256(descriptor_cbor),
    value_accumulator: ValueAccumulatorV1 {
      lovelace_delta: 3,
      asset_root: null_hash,
      asset_entry_count: 0,
      nonzero_asset_count: 0,
    },
  }
  control_step(
    3,
    tx,
    control,
    ValueAndMintControlV1 {
      ..control,
      replay_cursor: 1,
      replay_asset_cursor: 0,
      replay_value_hash: zero,
      replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
      value_accumulator: ValueAccumulatorV1 {
        lovelace_delta: 3,
        asset_root: singleton_root(unit, cbor.serialise(7)),
        asset_entry_count: 1,
        nonzero_asset_count: 1,
      },
    },
    ValueInputAssetWitness {
      source_kind: 0,
      item_index: 0,
      key: replayed_input_key,
      next_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
      descriptor_cbor,
      asset_index: 0,
      policy_id: h_28_a,
      asset_name: #"beef",
      quantity: 7,
      asset_peaks: validation_merkle_v1.build_frontier(
        [ledger_output_commitment_v1.asset_leaf_hash(h_28_a, #"beef", 7)],
      ).peaks,
      asset_siblings: [],
      mutation: no_mutation(),
    },
  )
}

fn replay_finish_fixture() -> StepFixtureV1 {
  idle_stage_step(4, 2)
}

/// The three controls of a one-output, one-asset transaction's output fold:
/// before the descriptor, after the descriptor (asset cursor 1) and after
/// the asset (output cursor 1).
type OutputFoldV1 {
  tx: MidgardTransaction,
  initial: ValueAndMintControlV1,
  active: ValueAndMintControlV1,
  final: ValueAndMintControlV1,
  descriptor_cbor: ByteArray,
  asset_peaks: List<FrontierPeak>,
}

fn output_fold_at(output_lovelace: Int) -> OutputFoldV1 {
  let tx = empty_v1_transaction()
  let unit = h_28_a |> bytearray.concat(#"beef")
  let asset_frontier = validation_merkle_v1.build_frontier(
    [ledger_output_commitment_v1.asset_leaf_hash(h_28_a, #"beef", 7)],
  )
  let descriptor_cbor = ledger_output_commitment_v1.encode(
    descriptor_with_assets(1, output_lovelace),
  )
  let descriptor_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_descriptor_leaf_hash(0, descriptor_cbor)],
  )
  let native_control = NativeScriptsControlV1 {
    ..empty_native_control(tx),
    output_count: 1,
    output_peaks: validation_merkle_v1.build_frontier([h_c]).peaks,
    output_descriptor_peaks: descriptor_frontier.peaks,
  }
  let initial = idle_control(native_control, 3)
  let active = ValueAndMintControlV1 {
    ..initial,
    output_asset_cursor: 1,
    replay_value_hash: blake2b_256(descriptor_cbor),
    value_accumulator: ValueAccumulatorV1 {
      ..initial.value_accumulator,
      lovelace_delta: -output_lovelace,
    },
  }
  let final = ValueAndMintControlV1 {
    ..active,
    output_cursor: 1,
    output_asset_cursor: 0,
    replay_value_hash: zero,
    value_accumulator: ValueAccumulatorV1 {
      lovelace_delta: -output_lovelace,
      asset_root: singleton_root(unit, cbor.serialise(-7)),
      asset_entry_count: 1,
      nonzero_asset_count: 1,
    },
  }
  OutputFoldV1 {
    tx,
    initial,
    active,
    final,
    descriptor_cbor,
    asset_peaks: asset_frontier.peaks,
  }
}

fn output_fold() -> OutputFoldV1 {
  output_fold_at(descriptor_min_ada_floor())
}

fn output_descriptor_fixture() -> StepFixtureV1 {
  let f = output_fold()
  control_step(
    5,
    f.tx,
    f.initial,
    f.active,
    ValueOutputDescriptorWitness {
      output_index: 0,
      descriptor_cbor: f.descriptor_cbor,
      siblings: [],
    },
  )
}

/// The canonical exact-rejection successor: Terminal, one program counter on,
/// `Rejected` with `hash_rejection_code(code)`, and the work root
/// `hash_work_witness(Terminal, pc + 1, encode_terminal_rejection_witness(code,
/// pre.prior_ledger_root))`.
fn exact_rejection_post(
  pre: validation_trace_v1.ValidationMachineStateV1,
  rejection_code: RejectionReasonV1,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      pre.program_counter + 1,
      vm_shared.encode_terminal_rejection_witness(
        rejection_code,
        pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
}

/// E_MIN_ADA at the validator boundary (#618 ruling 1; R8 of decision 0005).
/// The same pre-state, the same authenticated descriptor and the same
/// `ValueOutputDescriptorWitness` as `output_descriptor_fixture`, one lovelace
/// short of the floor -- so the only successor the machine admits is the exact
/// rejecting terminal rather than the fold. This is the step the deployed
/// `value_and_mint_output_descriptor_semantic_v1` has to award the challenger
/// for the wiring to be worth anything on L1, and it is the ONE kind of step
/// no other fixture in this module exercises: every fixture above advances.
fn output_descriptor_min_ada_rejection_fixture() -> StepFixtureV1 {
  let f = output_fold_at(descriptor_min_ada_floor() - 1)
  let Pair(pre, work_witness_cbor) = state_of(f.tx, f.initial, 40)
  StepFixtureV1 {
    kind: 5,
    pre,
    transition: ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(pre, reject_min_ada_reason),
    },
    auxiliary: ValueOutputDescriptorWitness {
      output_index: 0,
      descriptor_cbor: f.descriptor_cbor,
      siblings: [],
    },
  }
}

fn output_asset_fixture() -> StepFixtureV1 {
  let f = output_fold()
  control_step(
    6,
    f.tx,
    f.active,
    f.final,
    ValueOutputAssetWitness {
      output_index: 0,
      descriptor_cbor: f.descriptor_cbor,
      asset_index: 0,
      policy_id: h_28_a,
      asset_name: #"beef",
      quantity: 7,
      asset_peaks: f.asset_peaks,
      asset_siblings: [],
      mutation: no_mutation(),
    },
  )
}

fn output_finish_fixture() -> StepFixtureV1 {
  idle_stage_step(7, 3)
}

/// Kind 8: `mint_asset` -- a signed burn of five units settling a
/// five-unit accumulator entry to zero.
fn mint_asset_fixture() -> StepFixtureV1 {
  let tx = empty_v1_transaction()
  let policy_id = #"11111111111111111111111111111111111111111111111111111111"
  let asset_name = #"beef"
  let unit = policy_id |> bytearray.concat(asset_name)
  let mint_frontier = validation_merkle_v1.build_frontier(
    [vm_shared.mint_asset_leaf_hash(policy_id, asset_name, -5)],
  )
  let native_control = NativeScriptsControlV1 {
    ..empty_native_control(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  let control = ValueAndMintControlV1 {
    ..idle_control(native_control, 4),
    value_accumulator: ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: singleton_root(unit, cbor.serialise(5)),
      asset_entry_count: 1,
      nonzero_asset_count: 1,
    },
  }
  control_step(
    8,
    tx,
    control,
    ValueAndMintControlV1 {
      ..control,
      mint_cursor: 1,
      value_accumulator: ValueAccumulatorV1 {
        lovelace_delta: 0,
        asset_root: singleton_root(unit, cbor.serialise(0)),
        asset_entry_count: 2,
        nonzero_asset_count: 0,
      },
    },
    ValueMintAssetWitness {
      mint_index: 0,
      policy_id,
      asset_name,
      quantity: -5,
      siblings: [],
      mutation: ValueAssetMutationWitnessV1 {
        delta_was_present: True,
        old_delta: 5,
        delta_proof: [],
      },
    },
  )
}

fn mint_finish_fixture() -> StepFixtureV1 {
  idle_stage_step(9, 4)
}

/// Kind 10: `finalize` accepts a settled accumulator: the successor is the
/// accepting Terminal.
fn finalize_fixture() -> StepFixtureV1 {
  let tx = empty_v1_transaction()
  let native_control = empty_native_control(tx)
  let control = idle_control(native_control, 5)
  let Pair(pre, work_witness_cbor) = state_of(tx, control, 40)
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 41,
    verdict: validation_trace_v1.Accepted,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      41,
      vm_shared.terminal_acceptance_witness,
    ),
  }
  StepFixtureV1 {
    kind: 10,
    pre,
    transition: ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary: NoAuxiliaryWitness,
  }
}

fn fixture_of_kind(kind: Int) -> StepFixtureV1 {
  if kind == 0 {
    begin_fixture()
  } else if kind == 1 {
    replay_begin_fixture()
  } else if kind == 2 {
    replay_input_fixture()
  } else if kind == 3 {
    replay_asset_fixture()
  } else if kind == 4 {
    replay_finish_fixture()
  } else if kind == 5 {
    output_descriptor_fixture()
  } else if kind == 6 {
    output_asset_fixture()
  } else if kind == 7 {
    output_finish_fixture()
  } else if kind == 8 {
    mint_asset_fixture()
  } else if kind == 9 {
    mint_finish_fixture()
  } else {
    finalize_fixture()
  }
}

// ---------------------------------------------------------------------------
// Resolution and prepared states
// ---------------------------------------------------------------------------

fn resolution(f: StepFixtureV1) -> ValidationResolutionStateV1 {
  ValidationResolutionStateV1 {
    version: resolution_version,
    pre_state: f.pre,
    operator_successor_hash: h_b,
    challenger_successor_hash: validation_trace_v1.hash_machine_state(
      f.transition.claimed_successor,
    ),
  }
}

fn auxiliary_data(f: StepFixtureV1) -> Data {
  let auxiliary: Data = f.auxiliary
  auxiliary
}

fn prepared(f: StepFixtureV1) -> PreparedValidationResolutionStateV1 {
  let transition: Data = f.transition
  PreparedValidationResolutionStateV1 {
    version: prepared_resolution_version,
    resolution: resolution(f),
    evidence_hash: hash_one_step_evidence(transition, auxiliary_data(f)),
  }
}

fn resolver_datum(f: StepFixtureV1) -> ct.StepDatum<ValidationResolutionStateV1> {
  ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(resolution(f)),
  }
}

fn semantic_datum(
  base: PreparedValidationResolutionStateV1,
) -> ct.StepDatum<PreparedValidationResolutionStateV1> {
  ct.StepDatum { fraud_prover: fixture.fraud_prover_key_hash, data: Some(base) }
}

fn winning_output() -> Data {
  let output: Data = winning_resolution()
  output
}

// ---------------------------------------------------------------------------
// Wire-layout goldens
// ---------------------------------------------------------------------------

type TransitionOnlyMirrorV1 {
  TransitionOnlyMirror {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
  }
}

type ReplayInputMirrorV1 {
  ReplayInputMirror {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    source_kind: Int,
    item_index: Int,
    key: ByteArray,
    next_schedule_hash: ByteArray,
    value: ByteArray,
    siblings: List<ByteArray>,
  }
}

type ReplayAssetMirrorV1 {
  ReplayAssetMirror {
    claim: AssetFoldClaim,
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    source_kind: Int,
    item_index: Int,
    key: ByteArray,
    next_schedule_hash: ByteArray,
    yield_to_ref_input_index: Int,
  }
}

type OutputDescriptorMirrorV1 {
  OutputDescriptorMirror {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    ledger_output_index: Int,
    descriptor_cbor: ByteArray,
    siblings: List<ByteArray>,
  }
}

type OutputAssetMirrorV1 {
  OutputAssetMirror {
    claim: AssetFoldClaim,
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    ledger_output_index: Int,
    yield_to_ref_input_index: Int,
  }
}

type MintAssetMirrorV1 {
  MintAssetMirror {
    claim: AssetFoldClaim,
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    mint_index: Int,
    siblings: List<ByteArray>,
    yield_to_ref_input_index: Int,
  }
}

fn golden_transition() -> ValidationOneStepWitnessV1 {
  ValidationOneStepWitnessV1 {
    work_witness_cbor: #"",
    claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: zero,
      transaction_id: zero,
      transaction_commitment: zero,
      validation_context_hash: zero,
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: zero,
      phase: validation_trace_v1.ValueAndMint,
      program_counter: 0,
      work_root: zero,
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
    },
  }
}

/// The six transition-only actions (begin, replay_begin, replay_finish,
/// output_finish, mint_finish, finalize) share one wire layout: constructor
/// 0 over `(input_index, output_index, transition)` inside the shared
/// `StepRedeemer` envelope. Each is pinned to the same mirror and decodes
/// back from it.
test transition_only_wire_layouts_are_pinned() {
  let mirror: ct.StepRedeemer<TransitionOnlyMirrorV1> = ct.Continue(
    TransitionOnlyMirror {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
    },
  )
  let mirror_cbor = cbor.serialise(mirror)
  let mirror_data: Data = mirror
  let begin: begin_validator.SpendRedeemer = ct.Continue(
    begin_validator.VerifyBegin {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
    },
  )
  let replay_begin: replay_begin_validator.SpendRedeemer = ct.Continue(
    replay_begin_validator.VerifyReplayBegin {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
    },
  )
  let replay_finish: replay_finish_validator.SpendRedeemer = ct.Continue(
    replay_finish_validator.VerifyReplayFinish {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
    },
  )
  let output_finish: output_finish_validator.SpendRedeemer = ct.Continue(
    output_finish_validator.VerifyOutputFinish {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
    },
  )
  let mint_finish: mint_finish_validator.SpendRedeemer = ct.Continue(
    mint_finish_validator.VerifyMintFinish {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
    },
  )
  let finalize: finalize_validator.SpendRedeemer = ct.Continue(
    finalize_validator.VerifyFinalize {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
    },
  )
  expect decoded_begin: begin_validator.SpendRedeemer = mirror_data
  expect decoded_replay_begin: replay_begin_validator.SpendRedeemer = mirror_data
  expect decoded_replay_finish: replay_finish_validator.SpendRedeemer = mirror_data
  expect decoded_output_finish: output_finish_validator.SpendRedeemer = mirror_data
  expect decoded_mint_finish: mint_finish_validator.SpendRedeemer = mirror_data
  expect decoded_finalize: finalize_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(begin) == mirror_cbor,
    cbor.serialise(replay_begin) == mirror_cbor,
    cbor.serialise(replay_finish) == mirror_cbor,
    cbor.serialise(output_finish) == mirror_cbor,
    cbor.serialise(mint_finish) == mirror_cbor,
    cbor.serialise(finalize) == mirror_cbor,
    decoded_begin == begin,
    decoded_replay_begin == replay_begin,
    decoded_replay_finish == replay_finish,
    decoded_output_finish == output_finish,
    decoded_mint_finish == mint_finish,
    decoded_finalize == finalize,
  }
}

test replay_input_wire_layout_is_pinned() {
  let real: replay_input_validator.SpendRedeemer = ct.Continue(
    replay_input_validator.VerifyReplayInput {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      source_kind: 0,
      item_index: 0,
      key: replayed_input_key,
      next_schedule_hash: h_a,
      value: #"8106",
      siblings: [h_b],
    },
  )
  let mirror: ct.StepRedeemer<ReplayInputMirrorV1> = ct.Continue(
    ReplayInputMirror {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      source_kind: 0,
      item_index: 0,
      key: replayed_input_key,
      next_schedule_hash: h_a,
      value: #"8106",
      siblings: [h_b],
    },
  )
  let mirror_data: Data = mirror
  expect decoded: replay_input_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

test replay_asset_wire_layout_is_pinned() {
  let f = replay_asset_fixture()
  expect
      ValueInputAssetWitness {
        source_kind,
        item_index,
        key,
        next_schedule_hash,
        descriptor_cbor,
        asset_index,
        policy_id,
        asset_name,
        quantity,
        asset_peaks,
        asset_siblings,
        mutation,
      }
    = f.auxiliary
  let real: replay_asset_validator.SpendRedeemer = ct.Continue(
    replay_asset_validator.VerifyReplayAsset {
      claim: asset_claim(f.transition, f.auxiliary),
      yield_to_ref_input_index: 0,
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      source_kind,
      item_index,
      key,
      next_schedule_hash,
    },
  )
  let mirror: ct.StepRedeemer<ReplayAssetMirrorV1> = ct.Continue(
    ReplayAssetMirror {
      claim: asset_claim(f.transition, f.auxiliary),
      yield_to_ref_input_index: 0,
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      source_kind,
      item_index,
      key,
      next_schedule_hash,
    },
  )
  let mirror_data: Data = mirror
  expect decoded: replay_asset_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

test output_descriptor_wire_layout_is_pinned() {
  let real: output_descriptor_validator.SpendRedeemer = ct.Continue(
    output_descriptor_validator.VerifyOutputDescriptor {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      ledger_output_index: 3,
      descriptor_cbor: #"8106",
      siblings: [h_a],
    },
  )
  let mirror: ct.StepRedeemer<OutputDescriptorMirrorV1> = ct.Continue(
    OutputDescriptorMirror {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      ledger_output_index: 3,
      descriptor_cbor: #"8106",
      siblings: [h_a],
    },
  )
  let mirror_data: Data = mirror
  expect decoded: output_descriptor_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

test output_asset_wire_layout_is_pinned() {
  let f = output_asset_fixture()
  expect
      ValueOutputAssetWitness {
        descriptor_cbor,
        asset_index,
        policy_id,
        asset_name,
        quantity,
        asset_peaks,
        asset_siblings,
        mutation,
        ..
      }
    = f.auxiliary
  let real: output_asset_validator.SpendRedeemer = ct.Continue(
    output_asset_validator.VerifyOutputAsset {
      claim: asset_claim(f.transition, f.auxiliary),
      yield_to_ref_input_index: 0,
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      ledger_output_index: 3,
    },
  )
  let mirror: ct.StepRedeemer<OutputAssetMirrorV1> = ct.Continue(
    OutputAssetMirror {
      claim: asset_claim(f.transition, f.auxiliary),
      yield_to_ref_input_index: 0,
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      ledger_output_index: 3,
    },
  )
  let mirror_data: Data = mirror
  expect decoded: output_asset_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

test mint_asset_wire_layout_is_pinned() {
  let f = mint_asset_fixture()
  expect
      ValueMintAssetWitness {
        mint_index,
        policy_id,
        asset_name,
        quantity,
        siblings,
        mutation,
      }
    = f.auxiliary
  let real: mint_asset_validator.SpendRedeemer = ct.Continue(
    mint_asset_validator.VerifyMintAsset {
      claim: asset_claim(f.transition, f.auxiliary),
      yield_to_ref_input_index: 0,
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      mint_index,
      siblings,
    },
  )
  let mirror: ct.StepRedeemer<MintAssetMirrorV1> = ct.Continue(
    MintAssetMirror {
      claim: asset_claim(f.transition, f.auxiliary),
      yield_to_ref_input_index: 0,
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      mint_index,
      siblings,
    },
  )
  let mirror_data: Data = mirror
  expect decoded: mint_asset_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

// ---------------------------------------------------------------------------
// Prepare vectors
// ---------------------------------------------------------------------------

fn run_prepare(
  f: StepFixtureV1,
  group: List<ByteArray>,
  semantic_resolver_index: Int,
  output_state: Data,
) -> Bool {
  shaped_prepare(census.honest, f, group, semantic_resolver_index, output_state)
}

/// `run_prepare`, its L1 transaction reshaped by `shape`.
fn shaped_prepare(
  shape: fn(Transaction) -> Transaction,
  f: StepFixtureV1,
  group: List<ByteArray>,
  semantic_resolver_index: Int,
  output_state: Data,
) -> Bool {
  prepare_validator.main.spend(
    group,
    fixture.computation_thread_policy_id,
    Some(resolver_datum(f)),
    ct.Continue(
      PrepareSelected {
        input_index: 0,
        output_index: 0,
        semantic_resolver_index,
        transition: f.transition,
        auxiliary: auxiliary_data(f),
      },
    ),
    fixture.own_out_ref(),
    shape(prover_continue_tx(ct_asset, output_state)),
  )
}

fn expected_prepared_output(f: StepFixtureV1) -> Data {
  let output: Data = prepare_semantic_resolution(
    resolution(f),
    f.transition,
    auxiliary_data(f),
  )
  output
}

fn prepare_routes_kind(kind: Int) -> Bool {
  let f = fixture_of_kind(kind)
  let output = expected_prepared_output(f)
  let expected: Data = prepared(f)
  and {
    output == expected,
    run_prepare(f, group_with_slot(kind, kind_count), kind, output),
  }
}

test prepare_routes_begin_to_slot_zero() {
  prepare_routes_kind(0)
}

test prepare_routes_replay_begin_to_slot_one() {
  prepare_routes_kind(1)
}

test prepare_routes_replay_input_to_slot_two() {
  prepare_routes_kind(2)
}

test prepare_routes_replay_asset_to_slot_three() {
  prepare_routes_kind(3)
}

test prepare_routes_replay_finish_to_slot_four() {
  prepare_routes_kind(4)
}

test prepare_routes_output_descriptor_to_slot_five() {
  prepare_routes_kind(5)
}

test prepare_routes_output_asset_to_slot_six() {
  prepare_routes_kind(6)
}

test prepare_routes_output_finish_to_slot_seven() {
  prepare_routes_kind(7)
}

test prepare_routes_mint_asset_to_slot_eight() {
  prepare_routes_kind(8)
}

test prepare_routes_mint_finish_to_slot_nine() {
  prepare_routes_kind(9)
}

test prepare_routes_finalize_to_slot_ten() {
  prepare_routes_kind(10)
}

test prepare_refuses_a_twelfth_semantic_resolver_index() fail {
  let f = begin_fixture()
  run_prepare(
    f,
    group_with_slot(-1, kind_count),
    kind_count,
    expected_prepared_output(f),
  )
}

test prepare_refuses_a_misrouted_index() fail {
  let f = begin_fixture()
  run_prepare(f, group_with_slot(0, kind_count), 1, expected_prepared_output(f))
}

/// The phase pin: a resolution state parked in Cek cannot be prepared
/// through the ValueAndMint resolver.
test prepare_refuses_a_cek_phase_state() fail {
  let f = begin_fixture()
  let foreign_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..f.pre,
    phase: validation_trace_v1.Cek,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      f.pre.program_counter,
      f.transition.work_witness_cbor,
    ),
  }
  let foreign = StepFixtureV1 { ..f, pre: foreign_pre }
  run_prepare(
    foreign,
    group_with_slot(0, kind_count),
    0,
    expected_prepared_output(foreign),
  )
}

// ---------------------------------------------------------------------------
// Semantic vectors
// ---------------------------------------------------------------------------

fn run_begin(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  shaped_begin(census.honest, base, transition)
}

/// `run_begin`, its L1 transaction reshaped by `shape`.
fn shaped_begin(
  shape: fn(Transaction) -> Transaction,
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  begin_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      begin_validator.VerifyBegin {
        input_index: 0,
        output_index: 0,
        transition,
      },
    ),
    fixture.own_out_ref(),
    shape(prover_continue_tx(ct_asset, winning_output())),
  )
}

fn run_replay_begin(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  shaped_replay_begin(census.honest, base, transition)
}

/// `run_replay_begin`, its L1 transaction reshaped by `shape`.
fn shaped_replay_begin(
  shape: fn(Transaction) -> Transaction,
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  replay_begin_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      replay_begin_validator.VerifyReplayBegin {
        input_index: 0,
        output_index: 0,
        transition,
      },
    ),
    fixture.own_out_ref(),
    shape(prover_continue_tx(ct_asset, winning_output())),
  )
}

fn run_replay_input(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  shaped_replay_input(census.honest, base, transition, auxiliary)
}

/// `run_replay_input`, its L1 transaction reshaped by `shape`.
fn shaped_replay_input(
  shape: fn(Transaction) -> Transaction,
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect
      ResolvedInputReplayWitness {
        source_kind,
        item_index,
        key,
        next_schedule_hash,
        value,
        proof: _,
        siblings,
      }
    = auxiliary
  replay_input_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      replay_input_validator.VerifyReplayInput {
        input_index: 0,
        output_index: 0,
        transition,
        source_kind,
        item_index,
        key,
        next_schedule_hash,
        value,
        siblings,
      },
    ),
    fixture.own_out_ref(),
    shape(prover_continue_tx(ct_asset, winning_output())),
  )
}

fn run_replay_asset(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  shaped_replay_asset(census.honest, base, transition, auxiliary)
}

/// `run_replay_asset`, its L1 transaction reshaped by `shape`.
fn shaped_replay_asset(
  shape: fn(Transaction) -> Transaction,
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect
      ValueInputAssetWitness {
        source_kind,
        item_index,
        key,
        next_schedule_hash,
        descriptor_cbor,
        asset_index,
        policy_id,
        asset_name,
        quantity,
        asset_peaks,
        asset_siblings,
        mutation,
      }
    = auxiliary
  let redeemer: replay_asset_validator.SpendRedeemer = ct.Continue(
    replay_asset_validator.VerifyReplayAsset {
      claim: asset_claim(transition, auxiliary),
      yield_to_ref_input_index: 0,
      input_index: 0,
      output_index: 0,
      transition,
      source_kind,
      item_index,
      key,
      next_schedule_hash,
    },
  )
  let redeemer_data: Data = redeemer
  let tx = shape(asset_fold_tx(redeemer_data))
  and {
    replay_asset_validator.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fold_auth_policy,
      Some(semantic_datum(base)),
      redeemer,
      fixture.own_out_ref(),
      tx,
    ),
    check_asset_fold(3, tx),
  }
}

fn run_replay_finish(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  shaped_replay_finish(census.honest, base, transition)
}

/// `run_replay_finish`, its L1 transaction reshaped by `shape`.
fn shaped_replay_finish(
  shape: fn(Transaction) -> Transaction,
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  replay_finish_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      replay_finish_validator.VerifyReplayFinish {
        input_index: 0,
        output_index: 0,
        transition,
      },
    ),
    fixture.own_out_ref(),
    shape(prover_continue_tx(ct_asset, winning_output())),
  )
}

fn run_output_descriptor(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  shaped_output_descriptor(census.honest, base, transition, auxiliary)
}

/// `run_output_descriptor`, its L1 transaction reshaped by `shape`.
fn shaped_output_descriptor(
  shape: fn(Transaction) -> Transaction,
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect
      ValueOutputDescriptorWitness { output_index, descriptor_cbor, siblings }
    = auxiliary
  output_descriptor_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      output_descriptor_validator.VerifyOutputDescriptor {
        input_index: 0,
        output_index: 0,
        transition,
        ledger_output_index: output_index,
        descriptor_cbor,
        siblings,
      },
    ),
    fixture.own_out_ref(),
    shape(prover_continue_tx(ct_asset, winning_output())),
  )
}

fn run_output_asset(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  shaped_output_asset(census.honest, base, transition, auxiliary)
}

/// `run_output_asset`, its L1 transaction reshaped by `shape`.
fn shaped_output_asset(
  shape: fn(Transaction) -> Transaction,
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect
      ValueOutputAssetWitness {
        output_index,
        descriptor_cbor,
        asset_index,
        policy_id,
        asset_name,
        quantity,
        asset_peaks,
        asset_siblings,
        mutation,
      }
    = auxiliary
  let redeemer: output_asset_validator.SpendRedeemer = ct.Continue(
    output_asset_validator.VerifyOutputAsset {
      claim: asset_claim(transition, auxiliary),
      yield_to_ref_input_index: 0,
      input_index: 0,
      output_index: 0,
      transition,
      ledger_output_index: output_index,
    },
  )
  let redeemer_data: Data = redeemer
  let tx = shape(asset_fold_tx(redeemer_data))
  and {
    output_asset_validator.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fold_auth_policy,
      Some(semantic_datum(base)),
      redeemer,
      fixture.own_out_ref(),
      tx,
    ),
    check_asset_fold(6, tx),
  }
}

fn run_output_finish(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  shaped_output_finish(census.honest, base, transition)
}

/// `run_output_finish`, its L1 transaction reshaped by `shape`.
fn shaped_output_finish(
  shape: fn(Transaction) -> Transaction,
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  output_finish_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      output_finish_validator.VerifyOutputFinish {
        input_index: 0,
        output_index: 0,
        transition,
      },
    ),
    fixture.own_out_ref(),
    shape(prover_continue_tx(ct_asset, winning_output())),
  )
}

fn run_mint_asset(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  shaped_mint_asset(census.honest, base, transition, auxiliary)
}

/// `run_mint_asset`, its L1 transaction reshaped by `shape`.
fn shaped_mint_asset(
  shape: fn(Transaction) -> Transaction,
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect
      ValueMintAssetWitness {
        mint_index,
        policy_id,
        asset_name,
        quantity,
        siblings,
        mutation,
      }
    = auxiliary
  let redeemer: mint_asset_validator.SpendRedeemer = ct.Continue(
    mint_asset_validator.VerifyMintAsset {
      claim: asset_claim(transition, auxiliary),
      yield_to_ref_input_index: 0,
      input_index: 0,
      output_index: 0,
      transition,
      mint_index,
      siblings,
    },
  )
  let redeemer_data: Data = redeemer
  let tx = shape(asset_fold_tx(redeemer_data))
  and {
    mint_asset_validator.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fold_auth_policy,
      Some(semantic_datum(base)),
      redeemer,
      fixture.own_out_ref(),
      tx,
    ),
    check_asset_fold(8, tx),
  }
}

fn run_mint_finish(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  shaped_mint_finish(census.honest, base, transition)
}

/// `run_mint_finish`, its L1 transaction reshaped by `shape`.
fn shaped_mint_finish(
  shape: fn(Transaction) -> Transaction,
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  mint_finish_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      mint_finish_validator.VerifyMintFinish {
        input_index: 0,
        output_index: 0,
        transition,
      },
    ),
    fixture.own_out_ref(),
    shape(prover_continue_tx(ct_asset, winning_output())),
  )
}

fn run_finalize(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  shaped_finalize(census.honest, base, transition)
}

/// `run_finalize`, its L1 transaction reshaped by `shape`.
fn shaped_finalize(
  shape: fn(Transaction) -> Transaction,
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  finalize_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      finalize_validator.VerifyFinalize {
        input_index: 0,
        output_index: 0,
        transition,
      },
    ),
    fixture.own_out_ref(),
    shape(prover_continue_tx(ct_asset, winning_output())),
  )
}

/// Runs the semantic validator of kind `kind` over the prepared state and
/// transition of fixture `f`, with the action fields kind `kind` carries
/// (taken from the kind's own honest fixture when `f` is of another kind).
fn run_kind(kind: Int, f: StepFixtureV1) -> Bool {
  shaped_kind(census.honest, kind, f)
}

/// `run_kind`, its L1 transaction reshaped by `shape`.
fn shaped_kind(
  shape: fn(Transaction) -> Transaction,
  kind: Int,
  f: StepFixtureV1,
) -> Bool {
  let own = if f.kind == kind {
    f
  } else {
    fixture_of_kind(kind)
  }
  let base = prepared(f)
  if kind == 0 {
    shaped_begin(shape, base, f.transition)
  } else if kind == 1 {
    shaped_replay_begin(shape, base, f.transition)
  } else if kind == 2 {
    shaped_replay_input(shape, base, f.transition, own.auxiliary)
  } else if kind == 3 {
    shaped_replay_asset(shape, base, f.transition, own.auxiliary)
  } else if kind == 4 {
    shaped_replay_finish(shape, base, f.transition)
  } else if kind == 5 {
    shaped_output_descriptor(shape, base, f.transition, own.auxiliary)
  } else if kind == 6 {
    shaped_output_asset(shape, base, f.transition, own.auxiliary)
  } else if kind == 7 {
    shaped_output_finish(shape, base, f.transition)
  } else if kind == 8 {
    shaped_mint_asset(shape, base, f.transition, own.auxiliary)
  } else if kind == 9 {
    shaped_mint_finish(shape, base, f.transition)
  } else {
    shaped_finalize(shape, base, f.transition)
  }
}

test begin_validator_wins_the_schedule_installation() {
  run_kind(0, begin_fixture())
}

test replay_begin_validator_wins_the_stage_one_step() {
  run_kind(1, replay_begin_fixture())
}

test replay_input_validator_wins_an_asset_free_input_replay() {
  run_kind(2, replay_input_fixture())
}

// The replayed value is the resolved-item leaf ScriptSources stage 3 appended
// at this cursor: a schedule-consistent step that replays any other value, or
// opens the leaf with the wrong siblings, is refused.

test replay_input_validator_refuses_a_value_the_resolved_frontier_does_not_hold() {
  !run_kind(2, replay_input_fixture_over(3, 5, []))
}

test replay_input_validator_refuses_a_value_under_wrong_siblings() {
  !run_kind(2, replay_input_fixture_over(3, 3, [h_a]))
}

test replay_asset_validator_wins_a_single_asset_fold() {
  run_kind(3, replay_asset_fixture())
}

test replay_finish_validator_wins_the_exhausted_schedule() {
  run_kind(4, replay_finish_fixture())
}

test output_descriptor_validator_wins_the_descriptor_step() {
  run_kind(5, output_descriptor_fixture())
}

test output_descriptor_validator_wins_the_min_ada_rejection() {
  run_kind(5, output_descriptor_min_ada_rejection_fixture())
}

test output_asset_validator_wins_the_output_asset_fold() {
  run_kind(6, output_asset_fixture())
}

test output_finish_validator_wins_the_exhausted_outputs() {
  run_kind(7, output_finish_fixture())
}

test mint_asset_validator_wins_the_signed_burn() {
  run_kind(8, mint_asset_fixture())
}

test mint_finish_validator_wins_the_exhausted_mints() {
  run_kind(9, mint_finish_fixture())
}

test finalize_validator_wins_the_accepting_terminal() {
  run_kind(10, finalize_fixture())
}

/// The settled-accumulator pre-state handed on as a pending, non-terminal
/// state one program counter later: the shape of a successor that defers
/// acceptance to a further ledger phase.
fn deferred_hand_off_fixture() -> StepFixtureV1 {
  let f = finalize_fixture()
  StepFixtureV1 {
    ..f,
    transition: ValidationOneStepWitnessV1 {
      ..f.transition,
      claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
        ..f.pre,
        program_counter: 41,
        work_root: validation_trace_v1.hash_work_witness(
          validation_trace_v1.ValueAndMint,
          41,
          f.transition.work_witness_cbor,
        ),
      },
    },
  }
}

/// One pre-state, two candidate successors: the accepting Terminal and a
/// deferred hand-off. The accepting Terminal is the only successor the
/// machine admits.
test the_accepting_terminal_is_the_only_finalize_successor() {
  let candidates = [finalize_fixture(), deferred_hand_off_fixture()]
  let verifies = fn(f: StepFixtureV1) {
    vm_one_step.verify_value_and_mint_one_step_v1(
      f.pre,
      ValidationOneStepEvidenceV1 {
        transition: f.transition,
        auxiliary: f.auxiliary,
      },
    )
  }
  and {
    finalize_fixture().pre == deferred_hand_off_fixture().pre,
    list.count(candidates, verifies) == 1,
    verifies(finalize_fixture()),
    !verifies(deferred_hand_off_fixture()),
  }
}

test finalize_validator_refuses_a_deferred_hand_off() fail {
  run_kind(10, deferred_hand_off_fixture())
}

// Stage-neighbour partition at the validator boundary. Each row is its own
// `fail` test so a regression names the pair. The cross-stage pairs refuse
// on the stage discriminator; the same-stage pairs (replay_input /
// replay_asset / replay_finish, output_descriptor / output_asset /
// output_finish, mint_asset / mint_finish) refuse on the cursor
// discriminators alone, which is exactly what the split introduced.

test begin_validator_refuses_replay_begin() fail {
  run_kind(0, replay_begin_fixture())
}

test replay_begin_validator_refuses_begin() fail {
  run_kind(1, begin_fixture())
}

test replay_begin_validator_refuses_replay_finish() fail {
  run_kind(1, replay_finish_fixture())
}

test replay_input_validator_refuses_replay_asset() fail {
  run_kind(2, replay_asset_fixture())
}

test replay_input_validator_refuses_replay_finish() fail {
  run_kind(2, replay_finish_fixture())
}

test replay_asset_validator_refuses_replay_input() fail {
  run_kind(3, replay_input_fixture())
}

test replay_asset_validator_refuses_replay_finish() fail {
  run_kind(3, replay_finish_fixture())
}

test replay_finish_validator_refuses_replay_input() fail {
  run_kind(4, replay_input_fixture())
}

test replay_finish_validator_refuses_replay_asset() fail {
  run_kind(4, replay_asset_fixture())
}

test replay_finish_validator_refuses_output_finish() fail {
  run_kind(4, output_finish_fixture())
}

test output_descriptor_validator_refuses_output_asset() fail {
  run_kind(5, output_asset_fixture())
}

test output_descriptor_validator_refuses_output_finish() fail {
  run_kind(5, output_finish_fixture())
}

test output_asset_validator_refuses_output_descriptor() fail {
  run_kind(6, output_descriptor_fixture())
}

test output_asset_validator_refuses_output_finish() fail {
  run_kind(6, output_finish_fixture())
}

test output_finish_validator_refuses_output_descriptor() fail {
  run_kind(7, output_descriptor_fixture())
}

test output_finish_validator_refuses_output_asset() fail {
  run_kind(7, output_asset_fixture())
}

test output_finish_validator_refuses_mint_finish() fail {
  run_kind(7, mint_finish_fixture())
}

test mint_asset_validator_refuses_mint_finish() fail {
  run_kind(8, mint_finish_fixture())
}

test mint_finish_validator_refuses_mint_asset() fail {
  run_kind(9, mint_asset_fixture())
}

test mint_finish_validator_refuses_finalize() fail {
  run_kind(9, finalize_fixture())
}

test finalize_validator_refuses_mint_finish() fail {
  run_kind(10, mint_finish_fixture())
}

test finalize_validator_refuses_begin() fail {
  run_kind(10, begin_fixture())
}

/// The evidence pin at the semantic boundary: a prepared state whose hash
/// was computed over another step's evidence is refused even though the
/// transition itself is honest.
test begin_validator_refuses_a_foreign_evidence_commitment() fail {
  let f = begin_fixture()
  let foreign = PreparedValidationResolutionStateV1 {
    ..prepared(f),
    evidence_hash: prepared(replay_begin_fixture()).evidence_hash,
  }
  run_begin(foreign, f.transition)
}

/// A parameterised validator rebuilds the auxiliary from its action fields
/// and commits to it: the same honest transition under a different asset
/// quantity is refused on the evidence hash before any semantics run.
test mint_asset_validator_refuses_a_retargeted_quantity() fail {
  let f = mint_asset_fixture()
  expect ValueMintAssetWitness { mutation, .. } = f.auxiliary
  run_mint_asset(
    prepared(f),
    f.transition,
    ValueMintAssetWitness {
      mint_index: 0,
      policy_id: #"11111111111111111111111111111111111111111111111111111111",
      asset_name: #"beef",
      quantity: -4,
      siblings: [],
      mutation,
    },
  )
}

test begin_validator_refuses_a_swapped_claimed_successor() fail {
  let f = begin_fixture()
  let swapped = ValidationOneStepWitnessV1 {
    ..f.transition,
    claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
      ..f.transition.claimed_successor,
      work_root: h_b,
    },
  }
  run_begin(prepared(f), swapped)
}

test begin_validator_refuses_a_wrong_award_output() fail {
  let f = begin_fixture()
  let base = prepared(f)
  let not_winning: Data = base
  begin_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      begin_validator.VerifyBegin {
        input_index: 0,
        output_index: 0,
        transition: f.transition,
      },
    ),
    fixture.own_out_ref(),
    prover_continue_tx(ct_asset, not_winning),
  )
}

/// `pre.transaction_commitment` is what binds the ValueAndMint control's
/// native transaction proof to the transaction under dispute; it is checked
/// once, in the verified-body preamble every stage shares, so an otherwise
/// honest step whose pre-state commits to a different transaction must be
/// refused.
test begin_validator_refuses_a_foreign_transaction_commitment() fail {
  let f = begin_fixture()
  let forged = StepFixtureV1 {
    ..f,
    pre: validation_trace_v1.ValidationMachineStateV1 {
      ..f.pre,
      transaction_commitment: zero,
    },
    transition: ValidationOneStepWitnessV1 {
      ..f.transition,
      claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
        ..f.transition.claimed_successor,
        transaction_commitment: zero,
      },
    },
  }
  run_begin(prepared(forged), forged.transition)
}

/// Sanity on the fixtures themselves: every honest step is accepted by the
/// aggregate evidence route the machine defines, so a validator that
/// accepts it agrees with the machine and not with a broken vector.
fn fixture_is_an_honest_step(kind: Int) -> Bool {
  let f = fixture_of_kind(kind)
  vm_one_step.verify_value_and_mint_one_step_v1(
    f.pre,
    ValidationOneStepEvidenceV1 {
      transition: f.transition,
      auxiliary: f.auxiliary,
    },
  )
}

test every_fixture_is_an_honest_machine_step() {
  and {
    fixture_is_an_honest_step(0),
    fixture_is_an_honest_step(1),
    fixture_is_an_honest_step(2),
    fixture_is_an_honest_step(3),
    fixture_is_an_honest_step(4),
    fixture_is_an_honest_step(5),
    fixture_is_an_honest_step(6),
    fixture_is_an_honest_step(7),
    fixture_is_an_honest_step(8),
    fixture_is_an_honest_step(9),
    fixture_is_an_honest_step(10),
  }
}

// Both script purposes run over one transaction in every yielding fixture.
const fold_auth_policy =
  #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const fold_yield_hash =
  #"abababababababababababababababababababababababababababab"

const fold_other_hash =
  #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

const fold_third_hash =
  #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

fn asset_claim(
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> AssetFoldClaim {
  let control = vm_witness.value_and_mint_control_from_witness(
    transition.work_witness_cbor,
  )
  let (policy_id, asset_name, quantity, mutation, descriptor) = when
    auxiliary
  is {
    ValueInputAssetWitness {
      policy_id,
      asset_name,
      quantity,
      mutation,
      descriptor_cbor,
      asset_index,
      asset_peaks,
      asset_siblings,
      ..
    } ->
      (
        policy_id,
        asset_name,
        quantity,
        mutation,
        Some(
          AssetDescriptorClaim {
            descriptor_cbor,
            asset_index,
            asset_peaks,
            asset_siblings,
            asset_count: ledger_output_commitment_v1.decode(descriptor_cbor).asset_count,
          },
        ),
      )
    ValueOutputAssetWitness {
      policy_id,
      asset_name,
      quantity,
      mutation,
      descriptor_cbor,
      asset_index,
      asset_peaks,
      asset_siblings,
      ..
    } ->
      (
        policy_id,
        asset_name,
        quantity,
        mutation,
        Some(
          AssetDescriptorClaim {
            descriptor_cbor,
            asset_index,
            asset_peaks,
            asset_siblings,
            asset_count: ledger_output_commitment_v1.decode(descriptor_cbor).asset_count,
          },
        ),
      )
    ValueMintAssetWitness { policy_id, asset_name, quantity, mutation, .. } ->
      (policy_id, asset_name, quantity, mutation, None)
    _ -> fail
  }
  AssetFoldClaim {
    policy_id,
    asset_name,
    quantity,
    mutation,
    pre_value_accumulator: control.value_accumulator,
    outcome: value_fold.apply_value_asset_mutation(
      control.value_accumulator,
      policy_id |> bytearray.concat(asset_name),
      when auxiliary is {
        ValueOutputAssetWitness { .. } -> 0 - quantity
        _ -> quantity
      },
      mutation,
    ),
    descriptor,
  }
}

fn asset_fold_tx(redeemer: Data) -> Transaction {
  let tx = prover_continue_tx(ct_asset, winning_output())
  expect [input] = tx.inputs
  let reference_output = Output {
    ..input.output,
    value: assets.from_asset(fold_auth_policy, asset_fold_role, 1),
    reference_script: Some(fold_yield_hash),
  }
  Transaction {
    ..tx,
    reference_inputs: [
      Input {
        output_reference: l1_transaction.OutputReference {
          transaction_id: zero,
          output_index: 9,
        },
        output: reference_output,
      },
    ],
    withdrawals: [Pair(Script(fold_yield_hash), 0)],
    redeemers: [
      Pair(Spend(fixture.own_out_ref()), redeemer),
      Pair(Withdraw(Script(fold_yield_hash)), builtin.constr_data(0, [])),
    ],
  }
}

fn check_asset_fold(kind: Int, tx: Transaction) -> Bool {
  asset_fold_yield.main.withdraw(
    if kind == 3 {
      fixture.step_script_hash
    } else {
      fold_other_hash
    },
    if kind == 6 {
      fixture.step_script_hash
    } else {
      fold_third_hash
    },
    if kind == 8 {
      fixture.step_script_hash
    } else {
      fold_yield_hash
    },
    builtin.constr_data(0, []),
    Script(fold_yield_hash),
    tx,
  )
}

fn replay_claim_redeemer(
  claim: AssetFoldClaim,
) -> replay_asset_validator.SpendRedeemer {
  let f = replay_asset_fixture()
  expect
      ValueInputAssetWitness {
        source_kind,
        item_index,
        key,
        next_schedule_hash,
        ..
      }
    = f.auxiliary
  ct.Continue(
    replay_asset_validator.VerifyReplayAsset {
      claim,
      input_index: 0,
      output_index: 0,
      transition: f.transition,
      source_kind,
      item_index,
      key,
      next_schedule_hash,
      yield_to_ref_input_index: 0,
    },
  )
}

fn check_replay_dispatch(
  redeemer: replay_asset_validator.SpendRedeemer,
  tx: Transaction,
) -> Bool {
  let f = replay_asset_fixture()
  replay_asset_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fold_auth_policy,
    Some(semantic_datum(prepared(f))),
    redeemer,
    fixture.own_out_ref(),
    tx,
  )
}

test asset_fold_role_fits_an_asset_name() {
  bytearray.length(asset_fold_role) <= 32
}

test asset_fold_refuses_omitted_withdrawal() fail {
  let f = replay_asset_fixture()
  let redeemer = replay_claim_redeemer(asset_claim(f.transition, f.auxiliary))
  let tx = asset_fold_tx(redeemer)
  check_replay_dispatch(redeemer, Transaction { ..tx, withdrawals: [] })
}

test asset_fold_refuses_nonzero_withdrawal() fail {
  let f = replay_asset_fixture()
  let redeemer = replay_claim_redeemer(asset_claim(f.transition, f.auxiliary))
  let tx = asset_fold_tx(redeemer)
  check_replay_dispatch(
    redeemer,
    Transaction { ..tx, withdrawals: [Pair(Script(fold_yield_hash), 1)] },
  )
}

test asset_fold_refuses_wrong_role() fail {
  let f = replay_asset_fixture()
  let redeemer = replay_claim_redeemer(asset_claim(f.transition, f.auxiliary))
  let tx = asset_fold_tx(redeemer)
  expect [ref] = tx.reference_inputs
  check_replay_dispatch(
    redeemer,
    Transaction {
      ..tx,
      reference_inputs: [
        Input {
          ..ref,
          output: Output {
            ..ref.output,
            value: assets.from_asset(fold_auth_policy, "wrong-role", 1),
          },
        },
      ],
    },
  )
}

test asset_fold_refuses_wrong_reference_script() fail {
  let f = replay_asset_fixture()
  let redeemer = replay_claim_redeemer(asset_claim(f.transition, f.auxiliary))
  let tx = asset_fold_tx(redeemer)
  expect [ref] = tx.reference_inputs
  check_replay_dispatch(
    redeemer,
    Transaction {
      ..tx,
      reference_inputs: [
        Input {
          ..ref,
          output: Output {
            ..ref.output,
            reference_script: Some(fold_other_hash),
          },
        },
      ],
    },
  )
}

test asset_fold_refuses_duplicate_dispatcher_inputs() fail {
  let f = replay_asset_fixture()
  let redeemer = replay_claim_redeemer(asset_claim(f.transition, f.auxiliary))
  let tx = asset_fold_tx(redeemer)
  expect [input] = tx.inputs
  let other = Input {
    ..input,
    output_reference: l1_transaction.OutputReference {
      transaction_id: zero,
      output_index: 10,
    },
  }
  check_asset_fold(3, Transaction { ..tx, inputs: [input, other] })
}

test asset_fold_refuses_forged_descriptor_count() {
  let f = replay_asset_fixture()
  let claim = asset_claim(f.transition, f.auxiliary)
  expect Some(descriptor) = claim.descriptor
  let wrong = AssetFoldClaim {
    ..claim,
    descriptor: Some(
      AssetDescriptorClaim {
        ..descriptor,
        asset_count: descriptor.asset_count + 1,
      },
    ),
  }
  !check_asset_fold(3, asset_fold_tx(replay_claim_redeemer(wrong)))
}

test asset_fold_refuses_forged_outcome() {
  let f = replay_asset_fixture()
  let claim = asset_claim(f.transition, f.auxiliary)
  let wrong = AssetFoldClaim {
    ..claim,
    outcome: ValueAccumulatorAssetLimitExceeded,
  }
  !check_asset_fold(3, asset_fold_tx(replay_claim_redeemer(wrong)))
}

test asset_fold_refuses_retargeted_pre_accumulator() {
  let f = replay_asset_fixture()
  let claim = asset_claim(f.transition, f.auxiliary)
  let wrong = AssetFoldClaim {
    ..claim,
    pre_value_accumulator: ValueAccumulatorV1 {
      ..claim.pre_value_accumulator,
      lovelace_delta: 99,
    },
  }
  let redeemer = replay_claim_redeemer(wrong)
  !check_replay_dispatch(redeemer, asset_fold_tx(redeemer))
}

test asset_fold_refuses_mint_descriptor_substitution() {
  let f = replay_asset_fixture()
  let redeemer = replay_claim_redeemer(asset_claim(f.transition, f.auxiliary))
  !check_asset_fold(8, asset_fold_tx(redeemer))
}

test replay_asset_preserves_prover_cancellation_without_yield() {
  replay_asset_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fold_auth_policy,
    Some(semantic_datum(prepared(replay_asset_fixture()))),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(ct_asset, ct_asset, True),
  )
}

test output_asset_preserves_prover_cancellation_without_yield() {
  output_asset_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fold_auth_policy,
    Some(semantic_datum(prepared(output_asset_fixture()))),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(ct_asset, ct_asset, True),
  )
}

test mint_asset_preserves_prover_cancellation_without_yield() {
  mint_asset_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fold_auth_policy,
    Some(semantic_datum(prepared(mint_asset_fixture()))),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(ct_asset, ct_asset, True),
  )
}

fn mutated_asset_auxiliary(
  auxiliary: ValidationAuxiliaryWitnessV1,
  quantity: Int,
  old_delta: Int,
) -> ValidationAuxiliaryWitnessV1 {
  when auxiliary is {
    ValueInputAssetWitness {
      source_kind,
      item_index,
      key,
      next_schedule_hash,
      descriptor_cbor,
      asset_index,
      policy_id,
      asset_name,
      asset_peaks,
      asset_siblings,
      mutation,
      ..
    } ->
      ValueInputAssetWitness {
        source_kind,
        item_index,
        key,
        next_schedule_hash,
        descriptor_cbor,
        asset_index,
        policy_id,
        asset_name,
        asset_peaks,
        asset_siblings,
        quantity,
        mutation: ValueAssetMutationWitnessV1 {
          ..mutation,
          old_delta,
          delta_was_present: old_delta != 0,
        },
      }
    ValueOutputAssetWitness {
      output_index,
      descriptor_cbor,
      asset_index,
      policy_id,
      asset_name,
      asset_peaks,
      asset_siblings,
      mutation,
      ..
    } ->
      ValueOutputAssetWitness {
        output_index,
        descriptor_cbor,
        asset_index,
        policy_id,
        asset_name,
        asset_peaks,
        asset_siblings,
        quantity,
        mutation: ValueAssetMutationWitnessV1 {
          ..mutation,
          old_delta,
          delta_was_present: old_delta != 0,
        },
      }
    ValueMintAssetWitness {
      mint_index,
      policy_id,
      asset_name,
      siblings,
      mutation,
      ..
    } ->
      ValueMintAssetWitness {
        mint_index,
        policy_id,
        asset_name,
        siblings,
        quantity,
        mutation: ValueAssetMutationWitnessV1 {
          ..mutation,
          old_delta,
          delta_was_present: old_delta != 0,
        },
      }
    _ -> fail
  }
}

test asset_yield_split_agrees_with_aggregate_for_mutated_quantities_and_deltas(
  values via fuzz.tuple(fuzz.int_between(-10, 10), fuzz.int_between(-10, 10)),
) {
  let (quantity, old_delta) = values
  list.all(
    [3, 6, 8],
    fn(kind) {
      let f = fixture_of_kind(kind)
      let valid_quantity = if kind == 8 {
        if quantity == 0 {
          1
        } else {
          quantity
        }
      } else {
        if quantity < 0 {
          0 - quantity
        } else {
          quantity
        } + 1
      }
      let auxiliary = mutated_asset_auxiliary(
        f.auxiliary,
        valid_quantity,
        old_delta,
      )
      let aggregate = vm_one_step.verify_value_and_mint_one_step_v1(
        f.pre,
        ValidationOneStepEvidenceV1 { transition: f.transition, auxiliary },
      )
      let split = when kind is {
        3 -> run_replay_asset(prepared(f), f.transition, auxiliary)
        6 -> run_output_asset(prepared(f), f.transition, auxiliary)
        _ -> run_mint_asset(prepared(f), f.transition, auxiliary)
      }
      aggregate == split
    },
  )
}

// ---------------------------------------------------------------------------
// Census: who funds and who signs each move
// ---------------------------------------------------------------------------
//
// Every semantic validator only ever awards, so anyone may move it; the
// prepare is the prover's alone. A lone thread input pays up to the cap and no
// more; beside another input the thread keeps its value.

fn kind_shaped(kind: Int, shape: fn(Transaction) -> Transaction) -> Bool {
  shaped_kind(shape, kind, fixture_of_kind(kind))
}

fn unsigned_paying_the_cap(tx: Transaction) -> Transaction {
  census.without_signers(census.lone_input_paying(census.cap)(tx))
}

test census_anyone_moves_begin_paying_the_cap_from_the_thread() {
  kind_shaped(0, unsigned_paying_the_cap)
}

test census_begin_paying_over_the_cap_is_refused() fail {
  kind_shaped(0, census.lone_input_paying(census.cap + 1))
}

test census_begin_beside_a_wallet_input_keeps_the_thread_value() {
  kind_shaped(0, census.beside_wallet_input(0))
}

test census_begin_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  kind_shaped(0, census.beside_wallet_input(1))
}

test census_anyone_moves_replay_begin_paying_the_cap_from_the_thread() {
  kind_shaped(1, unsigned_paying_the_cap)
}

test census_replay_begin_paying_over_the_cap_is_refused() fail {
  kind_shaped(1, census.lone_input_paying(census.cap + 1))
}

test census_replay_begin_beside_a_wallet_input_keeps_the_thread_value() {
  kind_shaped(1, census.beside_wallet_input(0))
}

test census_replay_begin_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  kind_shaped(1, census.beside_wallet_input(1))
}

test census_anyone_moves_replay_input_paying_the_cap_from_the_thread() {
  kind_shaped(2, unsigned_paying_the_cap)
}

test census_replay_input_paying_over_the_cap_is_refused() fail {
  kind_shaped(2, census.lone_input_paying(census.cap + 1))
}

test census_replay_input_beside_a_wallet_input_keeps_the_thread_value() {
  kind_shaped(2, census.beside_wallet_input(0))
}

test census_replay_input_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  kind_shaped(2, census.beside_wallet_input(1))
}

test census_anyone_moves_replay_asset_paying_the_cap_from_the_thread() {
  kind_shaped(3, unsigned_paying_the_cap)
}

test census_replay_asset_paying_over_the_cap_is_refused() fail {
  kind_shaped(3, census.lone_input_paying(census.cap + 1))
}

test census_replay_asset_beside_a_wallet_input_keeps_the_thread_value() {
  kind_shaped(3, census.beside_wallet_input(0))
}

test census_replay_asset_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  kind_shaped(3, census.beside_wallet_input(1))
}

test census_anyone_moves_replay_finish_paying_the_cap_from_the_thread() {
  kind_shaped(4, unsigned_paying_the_cap)
}

test census_replay_finish_paying_over_the_cap_is_refused() fail {
  kind_shaped(4, census.lone_input_paying(census.cap + 1))
}

test census_replay_finish_beside_a_wallet_input_keeps_the_thread_value() {
  kind_shaped(4, census.beside_wallet_input(0))
}

test census_replay_finish_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  kind_shaped(4, census.beside_wallet_input(1))
}

test census_anyone_moves_output_descriptor_paying_the_cap_from_the_thread() {
  kind_shaped(5, unsigned_paying_the_cap)
}

test census_output_descriptor_paying_over_the_cap_is_refused() fail {
  kind_shaped(5, census.lone_input_paying(census.cap + 1))
}

test census_output_descriptor_beside_a_wallet_input_keeps_the_thread_value() {
  kind_shaped(5, census.beside_wallet_input(0))
}

test census_output_descriptor_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  kind_shaped(5, census.beside_wallet_input(1))
}

test census_anyone_moves_output_asset_paying_the_cap_from_the_thread() {
  kind_shaped(6, unsigned_paying_the_cap)
}

test census_output_asset_paying_over_the_cap_is_refused() fail {
  kind_shaped(6, census.lone_input_paying(census.cap + 1))
}

test census_output_asset_beside_a_wallet_input_keeps_the_thread_value() {
  kind_shaped(6, census.beside_wallet_input(0))
}

test census_output_asset_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  kind_shaped(6, census.beside_wallet_input(1))
}

test census_anyone_moves_output_finish_paying_the_cap_from_the_thread() {
  kind_shaped(7, unsigned_paying_the_cap)
}

test census_output_finish_paying_over_the_cap_is_refused() fail {
  kind_shaped(7, census.lone_input_paying(census.cap + 1))
}

test census_output_finish_beside_a_wallet_input_keeps_the_thread_value() {
  kind_shaped(7, census.beside_wallet_input(0))
}

test census_output_finish_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  kind_shaped(7, census.beside_wallet_input(1))
}

test census_anyone_moves_mint_asset_paying_the_cap_from_the_thread() {
  kind_shaped(8, unsigned_paying_the_cap)
}

test census_mint_asset_paying_over_the_cap_is_refused() fail {
  kind_shaped(8, census.lone_input_paying(census.cap + 1))
}

test census_mint_asset_beside_a_wallet_input_keeps_the_thread_value() {
  kind_shaped(8, census.beside_wallet_input(0))
}

test census_mint_asset_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  kind_shaped(8, census.beside_wallet_input(1))
}

test census_anyone_moves_mint_finish_paying_the_cap_from_the_thread() {
  kind_shaped(9, unsigned_paying_the_cap)
}

test census_mint_finish_paying_over_the_cap_is_refused() fail {
  kind_shaped(9, census.lone_input_paying(census.cap + 1))
}

test census_mint_finish_beside_a_wallet_input_keeps_the_thread_value() {
  kind_shaped(9, census.beside_wallet_input(0))
}

test census_mint_finish_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  kind_shaped(9, census.beside_wallet_input(1))
}

test census_anyone_moves_finalize_paying_the_cap_from_the_thread() {
  kind_shaped(10, unsigned_paying_the_cap)
}

test census_finalize_paying_over_the_cap_is_refused() fail {
  kind_shaped(10, census.lone_input_paying(census.cap + 1))
}

test census_finalize_beside_a_wallet_input_keeps_the_thread_value() {
  kind_shaped(10, census.beside_wallet_input(0))
}

test census_finalize_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  kind_shaped(10, census.beside_wallet_input(1))
}

fn prepare_shaped(shape: fn(Transaction) -> Transaction) -> Bool {
  let f = fixture_of_kind(0)
  shaped_prepare(
    shape,
    f,
    group_with_slot(0, kind_count),
    0,
    expected_prepared_output(f),
  )
}

test census_the_prover_prepares_paying_the_cap_from_the_thread() {
  prepare_shaped(census.lone_input_paying(census.cap))
}

test census_prepare_paying_over_the_cap_is_refused() fail {
  prepare_shaped(census.lone_input_paying(census.cap + 1))
}

test census_prepare_beside_a_wallet_input_keeps_the_thread_value() {
  prepare_shaped(census.beside_wallet_input(0))
}

test census_prepare_beside_a_wallet_input_taking_thread_value_is_refused() fail {
  prepare_shaped(census.beside_wallet_input(1))
}

test census_prepare_unsigned_is_refused() fail {
  prepare_shaped(census.without_signers)
}
