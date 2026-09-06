//// Transaction-level vectors for the shared ledger-output-proof FINALIZE
//// yield conjunction (size plan
//// `validation-trace-script-sources-output-proof-finalize-semantic-v1.md` §8).
////
//// The ScriptSources output-proof finalize dispatcher opens the stage-five
//// frame, pins the terminal LOP control bytes to frame item 30, and requires
//// the four authenticated descriptor yields
//// (`ledger-output-descriptor-*-yield.ak`) in every branch; the yields read
//// the dispatcher redeemer back through `unique_semantic_dispatch_v1` and
//// re-state `descriptor_is_exact_v1` piecewise. These vectors drive the
//// deployed validators through `main.spend` / `main.withdraw` on one honest
//// finalize of a protected script output, pin the nine-field redeemer wire
//// layout and the prepare routing (resolver 8, semantic slot 3), and show
//// that the conjunction refuses an omitted yield, a role permutation, a
//// forged leaf summary, a receive-scan splice that skips the descriptor
//// leaf, a non-terminal control and a duplicated dispatcher input, while the
//// two rejection arms (network-id mismatch, missing required witness) still
//// win with all four yields present. The predicate-level partition itself is
//// proven in `lib/midgard/ledger-output-proof-descriptor.test.ak`; what
//// these add is that the deployed validators reach those predicates with the
//// right arguments over a full transaction.
////
//// Note on the plan's `finalize_refuses_forged_receive_splice_offset`: the
//// shipped redeemer carries no splice offsets (they are derived on-chain
//// from the stage-five frame constants), so the offset forgery of the plan
//// is untestable at the redeemer surface. Its nearest real refusal is
//// `finalize_refuses_forged_receive_scan_splice` below: a claimed successor
//// whose item-24 receive scan was spliced without appending the descriptor
//// leaf.

use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address
use cardano/assets
use cardano/transaction as l1
use fraud_proofs/validation_trace/ledger_output_descriptor_datum_summary_yield as datum_summary_yield
use fraud_proofs/validation_trace/ledger_output_descriptor_reference_script_yield as reference_script_yield
use fraud_proofs/validation_trace/ledger_output_descriptor_scan_facts_yield as scan_facts_yield
use fraud_proofs/validation_trace/ledger_output_descriptor_value_summary_yield as value_summary_yield
use fraud_proofs/validation_trace/script_sources_output_proof_finalize_semantic_v1 as finalize_validator
use fraud_proofs/validation_trace/script_sources_v1 as prepare_validator
use midgard/bounded_item_v1
use midgard/cek_data_frame_v1 as data_frame
use midgard/cek_data_traverse_v1 as traverse
use midgard/cek_data_v1 as summary
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{native_tx_proof_commitment_v1}
use midgard/ledger_output_commitment_v1 as commitment
use midgard/ledger_output_proof_roles as roles
use midgard/ledger_output_proof_v1 as proof
use midgard/ledger_output_v1
use midgard/rejection_reason_v1.{
  reject_missing_required_witness, reject_network_id_mismatch,
}
use midgard/script_proof_v1
use midgard/script_sources_raw_frame_v1 as ss_raw
use midgard/validation_machine/machine_types.{
  EmptySignerSetProof, LedgerOutputProofFinalizeWitness, NoSignerSetProof,
  ReceivePurposeScanControlV1, SignerSetProofV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/script_sources_middle_stages.{
  receive_source_successor,
}
use midgard/validation_machine/shared.{encode_terminal_rejection_witness}
use midgard/validation_machine/witness as vm_witness
use midgard/validation_merkle_v1 as merkle
use midgard/validation_resolution_v1.{
  PreparedValidationResolutionStateV1, ValidationResolutionStateV1,
  hash_one_step_evidence, prepare_semantic_resolution,
  prepared_resolution_version, resolution_version, winning_resolution,
}
use midgard/validation_resolver_v1.{PrepareSelected}
use midgard/validation_trace_v1

// ---------------------------------------------------------------------------
// Shared identities
// ---------------------------------------------------------------------------

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

const ct_asset = "lop-descriptor-yields-thread"

/// `[1, "midgard-consensus-v1", 100, 0, 0, 0, 100]`: expected network id 0.
const context_network_zero =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

/// The same context with expected network id 1, so the honest network-0
/// output address is a mismatch.
const context_network_one =
  #"8701546d6964676172642d636f6e73656e7375732d763118640100001864"

/// Protected script-payment address (header 0x78, network 0), inline datum
/// `d87980`, no reference script.
const protected_script_datum_output =
  #"a300581d7811111111111111111111111111111111111111111111111111111111018200a00243d87980"

/// The same protected script address without a datum.
const protected_script_plain_output =
  #"a200581d7811111111111111111111111111111111111111111111111111111111018200a0"

/// Protected key-payment address (header 0x68, network 0): finalizing it
/// needs a signer-set (non-)membership fact.
const protected_pubkey_output =
  #"a200581d6811111111111111111111111111111111111111111111111111111111018200a0"

/// The four descriptor-yield script hashes, in `descriptor_roles` order.
const scan_facts_yield_hash =
  #"10101010101010101010101010101010101010101010101010101010"

const reference_script_yield_hash =
  #"20202020202020202020202020202020202020202020202020202020"

const datum_summary_yield_hash =
  #"30303030303030303030303030303030303030303030303030303030"

const value_summary_yield_hash =
  #"40404040404040404040404040404040404040404040404040404040"

fn role_pairs() -> List<(ByteArray, ByteArray)> {
  list.zip(
    roles.descriptor_roles(),
    [
      scan_facts_yield_hash, reference_script_yield_hash,
      datum_summary_yield_hash, value_summary_yield_hash,
    ],
  )
}

/// The dispatcher input of these vectors is the computation thread at
/// `fixture.step_script_hash`; the yields authenticate it by that hash.
fn dispatcher_hashes() -> List<ByteArray> {
  [fixture.step_script_hash]
}

fn winning_output() -> Data {
  let output: Data = winning_resolution()
  output
}

fn reward_redeemer() -> Data {
  let redeemer: Data = 0
  redeemer
}

// ---------------------------------------------------------------------------
// Terminal ledger-output-proof fixtures (the lib descriptor test's walk)
// ---------------------------------------------------------------------------

fn one_chunk_material(
  output_index: Int,
  output: ByteArray,
) -> Pair<ByteArray, bounded_item_v1.ChunkProofV1> {
  let frontier = merkle.append_leaf(
    0,
    merkle.empty_frontier(),
    bounded_item_v1.hash_chunk(2, output_index, 0, output),
  )
  let item_commitment = bounded_item_v1.commitment(
    2,
    output_index,
    bytearray.length(output),
    frontier,
  )
  Pair(
    item_commitment,
    bounded_item_v1.ChunkProofV1 {
      version: 1,
      field_index: 2,
      item_index: output_index,
      total_length: bytearray.length(output),
      chunk_index: 0,
      chunk: output,
      frontier,
      siblings: [],
    },
  )
}

fn advanced(
  result: Option<proof.LedgerOutputProofStepResultV1>,
) -> proof.LedgerOutputProofControlV1 {
  expect Some(proof.LedgerOutputProofAdvanced { control }) = result
  control
}

/// Drive the typed proof to its terminal stage; the only datum these outputs
/// carry is the trivial `d87980`.
fn until_terminal(
  control: proof.LedgerOutputProofControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  fuel: Int,
) -> proof.LedgerOutputProofControlV1 {
  expect fuel > 0
  if control.stage == proof.stage_terminal {
    control
  } else {
    let witness = if control.stage == proof.stage_datum_traversal {
      expect Some(datum_control) = control.datum
      if datum_control.stage == traverse.stage_head {
        proof.LedgerOutputProofDatum {
          action: traverse.HeadSequence { expected_children: 0 },
          chunk_proof: Some(chunk_proof),
          next_chunk_proof: None,
        }
      } else if datum_control.stage == traverse.stage_fold {
        proof.LedgerOutputProofDatum {
          action: traverse.FinalizeFrame {
            frame: data_frame.initial_small_constr_frame_v1(0, #"", 0),
            parent: None,
          },
          chunk_proof: None,
          next_chunk_proof: None,
        }
      } else {
        expect datum_control.stage == traverse.stage_terminal
        proof.LedgerOutputProofNoWitness
      }
    } else {
      when proof.step_v1(control, proof.LedgerOutputProofNoWitness) is {
        Some(_) -> proof.LedgerOutputProofNoWitness
        None ->
          proof.LedgerOutputProofChunks { chunk_proof, next_chunk_proof: None }
      }
    }
    until_terminal(
      advanced(proof.step_v1(control, witness)),
      chunk_proof,
      fuel - 1,
    )
  }
}

fn terminal_of(output: ByteArray) -> proof.LedgerOutputProofControlV1 {
  let Pair(item_commitment, chunk_proof) = one_chunk_material(0, output)
  until_terminal(
    proof.initial_control_v1(0, bytearray.length(output), item_commitment),
    chunk_proof,
    256,
  )
}

/// The honest descriptor of a terminal control, every field derived from the
/// control itself.
fn honest_descriptor(
  terminal: proof.LedgerOutputProofControlV1,
) -> commitment.LedgerOutputCommitmentV1 {
  expect Some(cardano_tx_out) = proof.cardano_tx_out_summary_v1(terminal)
  expect Some(midgard_tx_out) = proof.midgard_tx_out_summary_v1(terminal)
  expect Some(cardano_spend_datum) = proof.cardano_spend_datum_summary_v1(
    terminal,
  )
  let language = terminal.output_scan.reference_script_language
  commitment.LedgerOutputCommitmentV1 {
    version: commitment.version,
    output_index: terminal.output_index,
    total_length: terminal.total_length,
    item_commitment: terminal.item_commitment,
    address: terminal.output_scan.address,
    lovelace: terminal.output_scan.lovelace,
    asset_count: terminal.output_scan.asset_count,
    asset_frontier_commitment: merkle.frontier_commitment(
      terminal.output_scan.asset_count,
      terminal.output_scan.asset_peaks,
    ),
    cardano_value_size: terminal.output_scan.cardano_value_size,
    reference_script_language: language,
    reference_script_hash: if language == -1 {
      #""
    } else {
      expect Some(digest) = proof.reference_script_digest_v1(terminal)
      digest
    },
    reference_script_total_length: if language == -1 {
      0
    } else {
      terminal.total_length - terminal.output_scan.reference_script_item_offset
    },
    reference_script_item_commitment: if language == -1 {
      #""
    } else {
      expect Some(pinned) = proof.reference_script_item_commitment_v1(terminal)
      pinned
    },
    cardano_tx_out,
    midgard_tx_out,
    cardano_spend_datum,
  }
}

fn honest_value(
  terminal: proof.LedgerOutputProofControlV1,
) -> summary.DataSummaryV1 {
  expect Some(value_summary) = proof.value_summary_v1(terminal)
  value_summary
}

fn honest_datum(
  terminal: proof.LedgerOutputProofControlV1,
) -> Option<summary.DataSummaryV1> {
  if terminal.output_scan.datum_offset == -1 {
    None
  } else {
    expect Some(datum_control) = terminal.datum
    expect Some(datum_summary) = traverse.finalize_v1(datum_control)
    Some(datum_summary)
  }
}

fn address_of(output: ByteArray) {
  expect Some(midgard_address) = ledger_output_v1.decode_canonical_address_bytes(
    terminal_of(output).output_scan.address,
  )
  midgard_address
}

// ---------------------------------------------------------------------------
// Stage-five ScriptSources frames and the one-step fixtures over them
// ---------------------------------------------------------------------------

/// A canonical 30-item stage-five ScriptSources witness: one committed output
/// (count 1), cursor as given, everything unrelated at its smallest honest
/// shape. The finalize dispatcher's frame adds the terminal LOP control as
/// extension item 30.
fn base_witness_cbor(
  context_cbor: ByteArray,
  output_cursor: Int,
  receive_scan: ReceivePurposeScanControlV1,
) -> ByteArray {
  vm_witness.encode_script_sources_witness(
    #"80",
    #"80",
    #"80",
    context_cbor,
    0,
    zero,
    0,
    merkle.frontier_commitment(0, []),
    [],
    5,
    0,
    [],
    0,
    [],
    0,
    zero,
    zero,
    0,
    0,
    [],
    output_cursor,
    1,
    merkle.build_frontier([h_a]).peaks,
    1,
    receive_scan,
    0,
    0,
    vm_witness.empty_observer_purpose_scan_control(),
    vm_witness.empty_mint_fold_control(),
    zero,
  )
}

fn machine_pre(
  context_cbor: ByteArray,
  work_cbor: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: h_a,
    transaction_commitment: native_tx_proof_commitment_v1(#"80", #"80", #"80"),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_b,
    phase: validation_trace_v1.ScriptSources,
    program_counter: 10,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      10,
      work_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
}

/// One honest finalize step: the pre-state whose stage-five frame carries the
/// terminal control of `output` as its extension, the transition, and the
/// honest redeemer facts.
type FinalizeFixtureV1 {
  pre: validation_trace_v1.ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  descriptor_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
  control_cbor: ByteArray,
  value_claim: summary.DataSummaryV1,
  datum_claim: Option<summary.DataSummaryV1>,
}

fn pre_of(output: ByteArray, context_cbor: ByteArray) -> FinalizeFixtureV1 {
  let terminal = terminal_of(output)
  let control_cbor = proof.encode_control_v1(terminal)
  let descriptor_cbor = commitment.encode(honest_descriptor(terminal))
  let work_cbor = ss_raw.append_extension_v1(
    base_witness_cbor(
      context_cbor,
      0,
      vm_witness.empty_receive_purpose_scan_control(),
    ),
    control_cbor,
  )
  let pre = machine_pre(context_cbor, work_cbor)
  FinalizeFixtureV1 {
    pre,
    transition: ValidationOneStepWitnessV1 {
      work_witness_cbor: work_cbor,
      claimed_successor: pre,
    },
    descriptor_cbor,
    signer_proof: NoSignerSetProof,
    control_cbor,
    value_claim: honest_value(terminal),
    datum_claim: honest_datum(terminal),
  }
}

/// The receive scan the finalize splices in: the descriptor leaf appended at
/// the cursor, then `receive_source_successor` for the output's address.
fn spliced_receive_scan(
  output: ByteArray,
  descriptor_cbor: ByteArray,
) -> ReceivePurposeScanControlV1 {
  receive_source_successor(
    ReceivePurposeScanControlV1 {
      ..vm_witness.empty_receive_purpose_scan_control(),
      descriptor_peaks: merkle.append_leaf(
        0,
        [],
        script_proof_v1.output_descriptor_leaf_hash(0, descriptor_cbor),
      ),
    },
    address_of(output),
  )
}

fn with_successor_cbor(
  f: FinalizeFixtureV1,
  successor_cbor: ByteArray,
) -> FinalizeFixtureV1 {
  FinalizeFixtureV1 {
    ..f,
    transition: ValidationOneStepWitnessV1 {
      work_witness_cbor: f.transition.work_witness_cbor,
      claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
        ..f.pre,
        program_counter: 11,
        work_root: validation_trace_v1.hash_work_witness(
          validation_trace_v1.ScriptSources,
          11,
          successor_cbor,
        ),
      },
    },
  }
}

/// The honest accepting finalize of `output` under the network-0 context.
fn accepting_fixture(output: ByteArray) -> FinalizeFixtureV1 {
  let f = pre_of(output, context_network_zero)
  with_successor_cbor(
    f,
    base_witness_cbor(
      context_network_zero,
      1,
      spliced_receive_scan(output, f.descriptor_cbor),
    ),
  )
}

/// An honest rejecting finalize: the successor is the exact rejection
/// terminal for `rejection_code`.
fn rejecting_fixture(
  output: ByteArray,
  context_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
  rejection_code: ByteArray,
) -> FinalizeFixtureV1 {
  let f = pre_of(output, context_cbor)
  FinalizeFixtureV1 {
    ..f,
    signer_proof,
    transition: ValidationOneStepWitnessV1 {
      work_witness_cbor: f.transition.work_witness_cbor,
      claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
        ..f.pre,
        phase: validation_trace_v1.Terminal,
        program_counter: 11,
        verdict: validation_trace_v1.Rejected,
        rejection_code_hash: validation_trace_v1.hash_rejection_code(
          rejection_code,
        ),
        work_root: validation_trace_v1.hash_work_witness(
          validation_trace_v1.Terminal,
          11,
          encode_terminal_rejection_witness(
            rejection_code,
            f.pre.prior_ledger_root,
          ),
        ),
      },
    },
  }
}

// ---------------------------------------------------------------------------
// Resolution states, redeemers and the L1 transaction
// ---------------------------------------------------------------------------

fn auxiliary_of(f: FinalizeFixtureV1) -> Data {
  let auxiliary: Data = LedgerOutputProofFinalizeWitness {
    descriptor_cbor: f.descriptor_cbor,
    signer_proof: f.signer_proof,
  }
  auxiliary
}

fn resolution_of(f: FinalizeFixtureV1) -> ValidationResolutionStateV1 {
  ValidationResolutionStateV1 {
    version: resolution_version,
    pre_state: f.pre,
    operator_successor_hash: h_b,
    challenger_successor_hash: validation_trace_v1.hash_machine_state(
      f.transition.claimed_successor,
    ),
  }
}

fn prepared_of(f: FinalizeFixtureV1) -> PreparedValidationResolutionStateV1 {
  let transition_data: Data = f.transition
  PreparedValidationResolutionStateV1 {
    version: prepared_resolution_version,
    resolution: resolution_of(f),
    evidence_hash: hash_one_step_evidence(transition_data, auxiliary_of(f)),
  }
}

fn semantic_datum(
  f: FinalizeFixtureV1,
) -> ct.StepDatum<PreparedValidationResolutionStateV1> {
  ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(prepared_of(f)),
  }
}

fn finalize_redeemer(
  f: FinalizeFixtureV1,
  value_claim: Data,
  datum_claim: Data,
  control_cbor: ByteArray,
  yield_ref_input_indices: List<Int>,
) -> finalize_validator.SpendRedeemer {
  ct.Continue(
    finalize_validator.VerifyOutputProofFinalize {
      input_index: 0,
      output_index: 0,
      transition: f.transition,
      descriptor_cbor: f.descriptor_cbor,
      signer_proof: f.signer_proof,
      control_cbor,
      claimed_value_summary: value_claim,
      claimed_datum_summary: datum_claim,
      yield_ref_input_indices,
    },
  )
}

fn honest_redeemer(f: FinalizeFixtureV1) -> finalize_validator.SpendRedeemer {
  let value_claim: Data = f.value_claim
  let datum_claim: Data = f.datum_claim
  finalize_redeemer(f, value_claim, datum_claim, f.control_cbor, [0, 1, 2, 3])
}

/// The full finalize transaction: the computation-thread input carrying the
/// prepared state inline, one authenticated reference input plus zero
/// withdrawal per descriptor-yield role, the dispatcher spend redeemer and
/// one withdraw redeemer per yield, and the winning-resolution output.
fn finalize_tx(
  f: FinalizeFixtureV1,
  redeemer: finalize_validator.SpendRedeemer,
  pairs: List<(ByteArray, ByteArray)>,
  extra_inputs: List<l1.Input>,
) -> l1.Transaction {
  let raw_redeemer: Data = redeemer
  let raw_datum: Data = semantic_datum(f)
  let input = fixture.ct_thread_input(ct_asset)
  let template = fixture.continue_mid_step_l1_tx_v1(ct_asset, winning_output())
  l1.Transaction {
    ..template,
    inputs: [
      l1.Input {
        ..input,
        output: l1.Output { ..input.output, datum: l1.InlineDatum(raw_datum) },
      },
      ..extra_inputs
    ],
    reference_inputs: list.map(
      pairs,
      fn(pair) {
        l1.Input {
          ..input,
          output: l1.Output {
            address: address.from_script(pair.2nd),
            value: assets.from_asset(
              fixture.test_certificate_policy_id,
              pair.1st,
              1,
            ),
            datum: l1.NoDatum,
            reference_script: Some(pair.2nd),
          },
        }
      },
    ),
    withdrawals: list.map(pairs, fn(pair) { Pair(address.Script(pair.2nd), 0) }),
    redeemers: [
      Pair(l1.Spend(fixture.own_out_ref()), raw_redeemer),
      ..list.map(
        pairs,
        fn(pair) {
          Pair(l1.Withdraw(address.Script(pair.2nd)), reward_redeemer())
        },
      )
    ],
  }
}

fn run_dispatcher(
  f: FinalizeFixtureV1,
  redeemer: finalize_validator.SpendRedeemer,
  tx: l1.Transaction,
) -> Bool {
  finalize_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(semantic_datum(f)),
    redeemer,
    fixture.own_out_ref(),
    tx,
  )
}

fn run_scan_facts_yield(tx: l1.Transaction) -> Bool {
  scan_facts_yield.main.withdraw(
    dispatcher_hashes(),
    reward_redeemer(),
    address.Script(scan_facts_yield_hash),
    tx,
  )
}

fn run_reference_script_yield(tx: l1.Transaction) -> Bool {
  reference_script_yield.main.withdraw(
    dispatcher_hashes(),
    reward_redeemer(),
    address.Script(reference_script_yield_hash),
    tx,
  )
}

fn run_datum_summary_yield(tx: l1.Transaction) -> Bool {
  datum_summary_yield.main.withdraw(
    dispatcher_hashes(),
    reward_redeemer(),
    address.Script(datum_summary_yield_hash),
    tx,
  )
}

fn run_value_summary_yield(tx: l1.Transaction) -> Bool {
  value_summary_yield.main.withdraw(
    dispatcher_hashes(),
    reward_redeemer(),
    address.Script(value_summary_yield_hash),
    tx,
  )
}

fn run_all_yields(tx: l1.Transaction) -> Bool {
  and {
    run_scan_facts_yield(tx),
    run_reference_script_yield(tx),
    run_datum_summary_yield(tx),
    run_value_summary_yield(tx),
  }
}

// ---------------------------------------------------------------------------
// Wire layout and prepare routing
// ---------------------------------------------------------------------------

type FinalizeWireMirrorV1 {
  VerifyOutputProofFinalizeMirror {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    descriptor_cbor: ByteArray,
    signer_proof: SignerSetProofV1,
    control_cbor: ByteArray,
    claimed_value_summary: Data,
    claimed_datum_summary: Data,
    yield_ref_input_indices: List<Int>,
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
      phase: validation_trace_v1.ScriptSources,
      program_counter: 0,
      work_root: zero,
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root: zero,
    },
  }
}

/// The dispatcher redeemer is exactly the nine-field constructor, in this
/// order, wrapped in the shared `StepRedeemer` `Continue` arm.
test output_proof_finalize_wire_layout_is_pinned() {
  let claim: Data = 0
  let real: finalize_validator.SpendRedeemer = ct.Continue(
    finalize_validator.VerifyOutputProofFinalize {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      descriptor_cbor: #"11",
      signer_proof: NoSignerSetProof,
      control_cbor: #"22",
      claimed_value_summary: claim,
      claimed_datum_summary: claim,
      yield_ref_input_indices: [0, 1, 2, 3],
    },
  )
  let mirror: ct.StepRedeemer<FinalizeWireMirrorV1> = ct.Continue(
    VerifyOutputProofFinalizeMirror {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      descriptor_cbor: #"11",
      signer_proof: NoSignerSetProof,
      control_cbor: #"22",
      claimed_value_summary: claim,
      claimed_datum_summary: claim,
      yield_ref_input_indices: [0, 1, 2, 3],
    },
  )
  let mirror_data: Data = mirror
  expect decoded: finalize_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

/// The 29-slot ScriptSources group, with only the slot under test pointing at
/// the L1 fixture's next-step hash.
fn group_with_slot(slot: Int) -> List<ByteArray> {
  list.map(
    list.range(0, 28),
    fn(index) {
      if index == slot {
        fixture.next_step_script_hash
      } else {
        fixture.step_script_hash
      }
    },
  )
}

/// The ScriptSources prepare routes semantic slot 3 to the output-proof
/// finalize dispatcher, installing exactly the prepared state whose evidence
/// hash the dispatcher recommits to.
test prepare_routes_output_proof_finalize_to_slot_three() {
  let f = accepting_fixture(protected_script_datum_output)
  let resolver_datum = ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(resolution_of(f)),
  }
  let output: Data = prepare_semantic_resolution(
    resolution_of(f),
    f.transition,
    auxiliary_of(f),
  )
  let expected: Data = prepared_of(f)
  and {
    output == expected,
    prepare_validator.main.spend(
      group_with_slot(3),
      fixture.computation_thread_policy_id,
      Some(resolver_datum),
      ct.Continue(
        PrepareSelected {
          input_index: 0,
          output_index: 0,
          semantic_resolver_index: 3,
          transition: f.transition,
          auxiliary: auxiliary_of(f),
        },
      ),
      fixture.own_out_ref(),
      fixture.continue_mid_step_l1_tx_v1(ct_asset, output),
    ),
  }
}

// ---------------------------------------------------------------------------
// The honest finalize and its refusals
// ---------------------------------------------------------------------------

/// The full five-execution transaction: the dispatcher wins the accepting
/// finalize of a protected script output with an inline datum, and every
/// descriptor yield attests its fact of `descriptor_is_exact_v1`.
test finalize_wins_with_four_yields() {
  let f = accepting_fixture(protected_script_datum_output)
  let redeemer = honest_redeemer(f)
  let tx = finalize_tx(f, redeemer, role_pairs(), [])
  and {
    run_dispatcher(f, redeemer, tx),
    run_all_yields(tx),
  }
}

/// Dropping any one yield (here the value-summary role, its reference input,
/// withdrawal and redeemer, and its index) starves the dispatcher's
/// exact-length role walk.
test finalize_refuses_three_yields() fail {
  let f = accepting_fixture(protected_script_datum_output)
  let value_claim: Data = f.value_claim
  let datum_claim: Data = f.datum_claim
  let redeemer = finalize_redeemer(
    f,
    value_claim,
    datum_claim,
    f.control_cbor,
    [0, 1, 2],
  )
  run_dispatcher(
    f,
    redeemer,
    finalize_tx(f, redeemer, list.take(role_pairs(), 3), []),
  )
}

/// Swapping two entries of `yield_ref_input_indices` cross-binds the roles:
/// position 0 (`V1VtLopDescScanFactsYield`) resolves to the reference input
/// carrying the reference-script role token.
test finalize_refuses_role_permutation() fail {
  let f = accepting_fixture(protected_script_datum_output)
  let value_claim: Data = f.value_claim
  let datum_claim: Data = f.datum_claim
  let redeemer = finalize_redeemer(
    f,
    value_claim,
    datum_claim,
    f.control_cbor,
    [1, 0, 2, 3],
  )
  run_dispatcher(f, redeemer, finalize_tx(f, redeemer, role_pairs(), []))
}

/// A forged datum leaf summary passes the dispatcher (which never reads the
/// claims) and the two yields that do not consume it, and is refused exactly
/// by the datum-summary yield and the scan-facts composite rebuild.
test finalize_refuses_forged_leaf_summary() {
  let f = accepting_fixture(protected_script_datum_output)
  let value_claim: Data = f.value_claim
  let forged_option: Option<summary.DataSummaryV1> = Some(f.value_claim)
  let forged_claim: Data = forged_option
  let redeemer = finalize_redeemer(
    f,
    value_claim,
    forged_claim,
    f.control_cbor,
    [0, 1, 2, 3],
  )
  let tx = finalize_tx(f, redeemer, role_pairs(), [])
  and {
    run_dispatcher(f, redeemer, tx),
    run_reference_script_yield(tx),
    run_value_summary_yield(tx),
    !run_datum_summary_yield(tx),
    !run_scan_facts_yield(tx),
  }
}

/// The plan's forged-splice-offset case, restated at the shipped surface: a
/// claimed successor whose item-24 receive scan was spliced without appending
/// the descriptor leaf fails the dispatcher's successor re-derivation.
test finalize_refuses_forged_receive_scan_splice() fail {
  let output = protected_script_datum_output
  let f = accepting_fixture(output)
  let forged = with_successor_cbor(
    f,
    base_witness_cbor(
      context_network_zero,
      1,
      receive_source_successor(
        vm_witness.empty_receive_purpose_scan_control(),
        address_of(output),
      ),
    ),
  )
  let redeemer = honest_redeemer(forged)
  run_dispatcher(
    forged,
    redeemer,
    finalize_tx(forged, redeemer, role_pairs(), []),
  )
}

/// An output addressed to the wrong network wins only the exact
/// `reject_network_id_mismatch` terminal -- with all four yields still
/// required and passing.
test finalize_emits_network_id_terminal() {
  let f = rejecting_fixture(
    protected_script_plain_output,
    context_network_one,
    NoSignerSetProof,
    reject_network_id_mismatch,
  )
  let redeemer = honest_redeemer(f)
  let tx = finalize_tx(f, redeemer, role_pairs(), [])
  and {
    run_dispatcher(f, redeemer, tx),
    run_all_yields(tx),
  }
}

/// A protected key-payment output whose signer is provably absent from the
/// (empty) signer set wins only the exact `reject_missing_required_witness`
/// terminal.
test finalize_emits_missing_witness_terminal() {
  let f = rejecting_fixture(
    protected_pubkey_output,
    context_network_zero,
    EmptySignerSetProof { peaks: [] },
    reject_missing_required_witness,
  )
  let redeemer = honest_redeemer(f)
  let tx = finalize_tx(f, redeemer, role_pairs(), [])
  and {
    run_dispatcher(f, redeemer, tx),
    run_all_yields(tx),
  }
}

/// A yield fed a dispatcher redeemer whose pinned control is not terminal
/// (the initial LOP control) aborts in the shared `terminal_scan` clause.
test yield_refuses_non_terminal_control() fail {
  let output = protected_script_datum_output
  let f = accepting_fixture(output)
  let Pair(item_commitment, _chunk_proof) = one_chunk_material(0, output)
  let initial = proof.initial_control_v1(
    0,
    bytearray.length(output),
    item_commitment,
  )
  let value_claim: Data = f.value_claim
  let datum_claim: Data = f.datum_claim
  let redeemer = finalize_redeemer(
    f,
    value_claim,
    datum_claim,
    proof.encode_control_v1(initial),
    [0, 1, 2, 3],
  )
  run_scan_facts_yield(finalize_tx(f, redeemer, role_pairs(), []))
}

/// Two inputs at dispatcher script addresses defeat the yields' unique
/// dispatch: `unique_semantic_dispatch_v1` requires exactly one.
test yield_refuses_two_dispatcher_inputs() fail {
  let f = accepting_fixture(protected_script_datum_output)
  let redeemer = honest_redeemer(f)
  let base = fixture.ct_thread_input(ct_asset)
  let second = l1.Input {
    ..base,
    output_reference: l1.OutputReference {
      transaction_id: h_b,
      output_index: 7,
    },
  }
  run_scan_facts_yield(finalize_tx(f, redeemer, role_pairs(), [second]))
}
