use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_224}
use aiken/primitive/bytearray
use cardano/address as item_address
use cardano/assets as item_assets
use cardano/transaction as item_l1
use fraud_proofs/validation_trace/phase_a_native_scripts_item_semantic_v1 as item_dispatcher
use fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1 as item_yields
use midgard/bounded_item_v1
use midgard/computation_thread as item_ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/native_binding_fixture_v1 as item_fixture
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_versioned_script}
use midgard/fraud_proofs/native_tx/transaction.{midgard_transaction_to_compact}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionWitnessSet, MidgardTxInput, MidgardVersionedScript,
  NativeCardanoScript, TxIsValid, native_tx_version_v1,
}
use midgard/fraud_proofs/validation_trace/phase_a_native_item_yield_v1 as item_yield
use midgard/native_tx_field_access_v1
use midgard/script_proof_v1
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/witness as vm_witness
use midgard/validation_merkle_v1
use midgard/validation_resolution_v1 as item_resolution
use midgard/validation_trace_v1

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

/// §8's door context for a step whose carriage is tier 1.
///
/// Every hand-written step in this module names an `Inline` carriage — the
/// preimage travels in the evidence — so the door needs neither the
/// consuming transaction's reference inputs (§8.2, tier 2) nor the §8.6
/// certificate minting policy id (§8.4, tier 3). Both are supplied empty, and
/// that is a *fact about these vectors* rather than a convenience: the tier-2
/// and tier-3 ladders are exercised where they belong, in
/// `native-tx-field-access-v1.test.ak`, against the door itself. A step here
/// that accidentally named a reference tier would fail closed rather than pass
/// with an unused context.
fn inline_field_carriage(
  items: List<ByteArray>,
) -> native_tx_field_access_v1.FieldCarriageV1 {
  native_tx_field_access_v1.Inline {
    preimage: native_tx_field_access_v1.encode_field_preimage(items),
  }
}

const validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

// C49 (#540). The C70 target snapshot: the preprod epoch-303 protocol
// parameters pinned by `PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1`
// (demo/midgard-validation/tests/helpers/ordered-collection-boundary.ts:78),
// which is the same snapshot every TypeScript Cardano boundary corpus is built
// against. These three numbers are the only free parameters in the min-fee and
// min-Ada formulas; a C70 parameter change re-derives both.

fn empty_v1_transaction() -> MidgardTransaction {
  let empty_mint: Data = []
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

fn address_witness_key_hashes(
  witnesses: List<MidgardAddressWitness>,
) -> List<ByteArray> {
  when witnesses is {
    [] -> []
    [witness, ..rest] ->
      [
        blake2b_224(witness.verification_key),
        ..address_witness_key_hashes(rest)
      ]
  }
}

fn signer_frontier_for_tx(
  tx: MidgardTransaction,
) -> validation_merkle_v1.BuiltFrontier {
  tx.witness_set.addr_tx_wits
    |> address_witness_key_hashes
    |> list.sort(bytearray.compare)
    |> list.unique
    |> list.map(script_proof_v1.signer_leaf_hash)
    |> validation_merkle_v1.build_frontier
}

fn contains_non_native_script(scripts: List<MidgardVersionedScript>) -> Bool {
  when scripts is {
    [] -> False
    [script, ..rest] -> or {
        script.language != NativeCardanoScript,
        contains_non_native_script(rest),
      }
  }
}

fn signatures_scan_witness(
  tx: MidgardTransaction,
  stage: Int,
  address_count: Int,
  required_count: Int,
  address_seen: Int,
  required_seen: Int,
  previous_order_key: ByteArray,
  previous_signer_hash: ByteArray,
  signer_frontier: validation_merkle_v1.BuiltFrontier,
  invalid_signature_seen: Int,
) -> ByteArray {
  let proof_source = forced_fixture.from_native_transaction(tx)
  vm_witness.encode_signatures_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    stage,
    address_count,
    required_count,
    address_seen,
    required_seen,
    previous_order_key,
    previous_signer_hash,
    signer_frontier.count,
    signer_frontier.peaks,
    invalid_signature_seen,
  )
}

fn initial_signatures_witness(tx: MidgardTransaction) -> ByteArray {
  signatures_scan_witness(
    tx,
    0,
    if tx.witness_set.addr_tx_wits == [] {
      0
    } else {
      -1
    },
    if tx.body.required_signers == [] {
      0
    } else {
      -1
    },
    0,
    0,
    #"",
    #"",
    validation_merkle_v1.build_frontier([]),
    0,
  )
}

fn signatures_step_fixture(
  tx: MidgardTransaction,
  ledger_delta_root: ByteArray,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = initial_signatures_witness(tx)
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
      phase: validation_trace_v1.Signatures,
      program_counter: 4,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.Signatures,
        4,
        work_witness_cbor,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root,
    },
    work_witness_cbor,
  )
}

fn phase_a_native_step_fixture(
  tx: MidgardTransaction,
  ledger_delta_root: ByteArray,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let Pair(signature_pre, _signature_work_witness_cbor) = signatures_step_fixture(
    tx,
    ledger_delta_root,
  )
  let proof_source = forced_fixture.from_native_transaction(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let script_count = if tx.witness_set.script_tx_wits == [] {
    0
  } else {
    -1
  }
  let work_witness_cbor = vm_witness.encode_phase_a_native_scripts_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    0,
    script_count,
    0,
    0,
    0,
    #"",
    0,
    #"",
    0,
    0,
    -1,
    signer_frontier.count,
    signer_frontier.peaks,
    #"",
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      ..signature_pre,
      phase: validation_trace_v1.PhaseANativeScripts,
      program_counter: 5,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.PhaseANativeScripts,
        5,
        work_witness_cbor,
      ),
    },
    work_witness_cbor,
  )
}

fn phase_a_native_scan_witness(
  tx: MidgardTransaction,
  stage: Int,
  script_count: Int,
  script_seen: Int,
  contains_non_native_script: Int,
  item_length: Int,
  item_commitment: ByteArray,
  cursor: Int,
  stack_root: ByteArray,
  stack_depth: Int,
  node_count: Int,
  result: Int,
) -> ByteArray {
  let proof_source = forced_fixture.from_native_transaction(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  vm_witness.encode_phase_a_native_scripts_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    stage,
    script_count,
    script_seen,
    contains_non_native_script,
    item_length,
    item_commitment,
    cursor,
    stack_root,
    stack_depth,
    node_count,
    result,
    signer_frontier.count,
    signer_frontier.peaks,
    #"",
  )
}

fn next_phase_a_native_state(
  pre: validation_trace_v1.ValidationMachineStateV1,
  work_witness_cbor: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      pre.program_counter + 1,
      work_witness_cbor,
    ),
  }
}

/// The single-chunk `bounded_item_v1` proof over item 0 of a single-item field.
///
/// This is what survives of the retired `singleton_field_item_proofs`. Its
/// collection half is gone: no step opens a per-item collection proof against a
/// §4 flat field commitment any more, and the carriage that replaced it is
/// `inline_field_carriage([key])`. The **chunk** half stays because four phases
/// hand a partially-read item forward across steps and their control state
/// carries a genuine `bounded_item_v1` root the later step opens against
/// (`NativeScriptTokenWitness`, `NativeExecutionScanWitness.first_chunk_proof`,
/// the redeemer item proof) — a satisfiable opening the flat reversion never
/// touched.
fn native_item_yield_transaction(mutation: Int) -> Bool {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes: #"82041864",
  }
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      validity_interval_start: 100,
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_native_step_fixture(tx, h_a)
  let item_cbor = encode_midgard_versioned_script(script)
  let next_work = phase_a_native_scan_witness(
    tx,
    1,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    3,
    #"",
    0,
    0,
    -1,
  )
  let post = next_phase_a_native_state(pre, next_work)
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let carriage = inline_field_carriage([item_cbor])
  let auxiliary: Data = vm_machine_types.TransactionFieldChunkWitness {
    field_index: 6,
    item_index: 0,
    carriage,
  }
  let transition_data: Data = transition
  let state = item_resolution.PreparedValidationResolutionStateV1 {
    version: 1,
    resolution: item_resolution.ValidationResolutionStateV1 {
      version: 1,
      pre_state: pre,
      operator_successor_hash: h_b,
      challenger_successor_hash: validation_trace_v1.hash_machine_state(post),
    },
    evidence_hash: item_resolution.hash_one_step_evidence(
      transition_data,
      auxiliary,
    ),
  }
  let datum = item_ct.StepDatum {
    fraud_prover: item_fixture.fraud_prover_key_hash,
    data: Some(state),
  }
  let datum_data: Data = datum
  let action = item_yield.VerifyItem {
    input_index: 0,
    output_index: 0,
    transition,
    field_index: 6,
    item_index: 0,
    carriage,
    yield_to_ref_input_index: if mutation == 5 {
      1
    } else {
      0
    },
    yield_kind: 0,
  }
  let redeemer = item_ct.Continue(action)
  let raw_redeemer: Data = redeemer
  let carriage_data: Data = carriage
  expect
    raw_redeemer == builtin.constr_data(
      1,
      [
        builtin.constr_data(
          0,
          [
            builtin.i_data(0),
            builtin.i_data(0),
            transition_data,
            builtin.i_data(6),
            builtin.i_data(0),
            carriage_data,
            builtin.i_data(
              if mutation == 5 {
                1
              } else {
                0
              },
            ),
            builtin.i_data(0),
          ],
        ),
      ],
    )
  let input_template = item_fixture.ct_thread_input("item-split")
  let input = item_l1.Input {
    ..input_template,
    output: item_l1.Output {
      ..input_template.output,
      datum: item_l1.InlineDatum(datum_data),
    },
  }
  let winning: Data = item_resolution.winning_resolution()
  let output = item_fixture.next_step_output(
    "item-split",
    if mutation == 6 {
      builtin.i_data(9)
    } else {
      winning
    },
  )
  let template = item_fixture.continue_mid_step_l1_tx_v1("item-split", winning)
  let reward_hash = item_fixture.catalogue_script_hash
  let reward_redeemer: Data = 0
  let transaction = item_l1.Transaction {
    ..template,
    outputs: [output],
    inputs: if mutation == 4 {
      [input, input]
    } else {
      [input]
    },
    reference_inputs: [
      item_l1.Input {
        ..input_template,
        output: item_l1.Output {
          address: item_address.from_script(reward_hash),
          value: item_assets.from_asset(
            item_fixture.test_certificate_policy_id,
            if mutation == 3 {
              item_yield.foreign_role
            } else {
              item_yield.native_role
            },
            1,
          ),
          datum: item_l1.NoDatum,
          reference_script: Some(reward_hash),
        },
      },
    ],
    withdrawals: if mutation == 1 {
      []
    } else {
      [
        Pair(
          item_address.Script(reward_hash),
          if mutation == 2 {
            1
          } else {
            0
          },
        ),
      ]
    },
    redeemers: [
      Pair(item_l1.Spend(item_fixture.own_out_ref()), raw_redeemer),
      Pair(item_l1.Withdraw(item_address.Script(reward_hash)), reward_redeemer),
    ],
  }
  and {
    item_dispatcher.main.spend(
      item_fixture.next_step_script_hash,
      item_fixture.computation_thread_policy_id,
      item_fixture.test_certificate_policy_id,
      Some(datum),
      redeemer,
      item_fixture.own_out_ref(),
      transaction,
    ),
    if mutation == 8 {
      item_yields.foreign.withdraw(
        item_fixture.step_script_hash,
        item_fixture.next_step_script_hash,
        item_fixture.test_certificate_policy_id,
        reward_redeemer,
        item_address.Script(reward_hash),
        transaction,
      )
    } else {
      item_yields.native.withdraw(
        if mutation == 7 {
          reward_hash
        } else {
          item_fixture.step_script_hash
        },
        item_fixture.next_step_script_hash,
        item_fixture.test_certificate_policy_id,
        reward_redeemer,
        item_address.Script(reward_hash),
        transaction,
      )
    },
  }
}

test native_item_dispatch_and_yield_accept_exact_transition() {
  native_item_yield_transaction(0)
}

test native_item_refuses_missing_zero_withdrawal() fail {
  native_item_yield_transaction(1)
}

test native_item_refuses_nonzero_withdrawal() fail {
  native_item_yield_transaction(2)
}

test native_item_refuses_foreign_role_nft() fail {
  native_item_yield_transaction(3)
}

test native_item_refuses_two_dispatcher_inputs() fail {
  native_item_yield_transaction(4)
}

test native_item_refuses_wrong_reference_index() fail {
  native_item_yield_transaction(5)
}

test native_item_refuses_nonwinning_output() fail {
  native_item_yield_transaction(6)
}

test native_item_refuses_wrong_dispatcher_hash() fail {
  native_item_yield_transaction(7)
}

test foreign_item_yield_refuses_native_header() {
  !native_item_yield_transaction(8)
}
