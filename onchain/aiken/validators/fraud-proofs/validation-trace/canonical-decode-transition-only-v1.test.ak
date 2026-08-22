//// Option B (#620, spec #619): the canonical-decode complete-item path
//// commits to the transition alone. These are the validator-level vectors for
//// the adversarial-matrix rows this change owns — wrong-content door aborts
//// through every carriage tier and the publication arm (row A), retired-shape
//// replays and the old carriage-committed preimage (row D), and transition
//// swaps against the work-root and successor pins (row E) — plus wire-layout
//// goldens pinning the constructor indices after the arm retirements (the
//// 9f191e9a dead-arm lesson: an ABI that moves without a golden moves again,
//// silently).
////
//// Division of labour: the §8.8 door's own tier ladder — certificates for
//// another transaction or field, the mint-welded field-hash equality, chunk
//// substitution on lazy reads, the §8.4 shape partition — is exercised against
//// the door itself in `native-tx-field-access-v1.test.ak` and none of that
//// code moves here. What these vectors prove is the property Option B
//// newly relies on: the observe validator reaches that door with the right
//// arguments (the pre-state's own transaction id, the consuming transaction's
//// reference inputs, the certificate policy parameter) and has no content
//// path around it now that the frozen-hash equality is gone.
////
//// Every negative here was first run without its `fail` marker and shown
//// aborting (or returning False) for its stated reason — the red-then-green
//// record lives with the change.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use fraud_proofs/validation_trace/canonical_decode_item_observe_v1 as observe_validator
use fraud_proofs/validation_trace/canonical_decode_item_semantic_v1 as authenticate_validator
use fraud_proofs/validation_trace/canonical_decode_v1 as prepare_validator
use midgard/canonical_decode_item_staging_v1 as staging
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_versioned_script}
use midgard/fraud_proofs/native_tx/transaction.{
  midgard_transaction_proof_commitment_v1, midgard_transaction_proof_source_v1,
  midgard_transaction_to_compact,
} as native_tx_transaction
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionWitnessSet, MidgardVersionedScript,
  PlutusV3Script, native_tx_version_v1,
}
use midgard/native_tx_field_access_v1 as field_access
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_machine_v1.{
  NoAuxiliaryWitness, TransactionFieldItemWitness, ValidationOneStepWitnessV1,
  ValidationProofItemDatumV1, bind_canonical_decode_item_source_v1,
  encode_transaction_field_scan_witness, observe_canonical_decode_item_v1,
}
use midgard/validation_resolution_v1.{
  PreparedValidationResolutionStateV1, ValidationResolutionStateV1,
  hash_one_step_evidence, prepared_resolution_version, resolution_version,
}
use midgard/validation_trace_v1

// ---------------------------------------------------------------------------
// Shared fixture: one genuine complete-item step over field 6, item 0
// ---------------------------------------------------------------------------

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

const context_cbor = #"80"

const ct_asset = "option-b-thread"

const proof_item_hash =
  #"9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c"

/// Everything one genuine canonical-decode complete-item step needs, derived
/// from a real `MidgardTransaction` so every anchor — transaction id, proof
/// commitment, witness-set hash, field commitment, preimage lengths — is the
/// value the committed structures themselves produce.
type OptionBStepFixtureV1 {
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  resolution: ValidationResolutionStateV1,
  base: PreparedValidationResolutionStateV1,
  field_preimage: ByteArray,
  old_evidence_hash: ByteArray,
}

fn scripted_tx(script_bytes: ByteArray) -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [
        MidgardVersionedScript { language: PlutusV3Script, script_bytes },
      ],
    },
  }
}

fn transition_only_hash(transition: ValidationOneStepWitnessV1) -> ByteArray {
  let transition_data: Data = transition
  let auxiliary_data: Data = NoAuxiliaryWitness
  hash_one_step_evidence(transition_data, auxiliary_data)
}

/// The retired preimage: the hash the pre-Option-B pipeline committed for an
/// inline-carried item — `(transition, TransactionFieldItemWitness{Inline{…}})`.
fn carriage_committed_hash(
  transition: ValidationOneStepWitnessV1,
  preimage: ByteArray,
) -> ByteArray {
  let transition_data: Data = transition
  let auxiliary_data: Data = TransactionFieldItemWitness {
    carriage: field_access.Inline { preimage },
  }
  hash_one_step_evidence(transition_data, auxiliary_data)
}

fn step_fixture(script_bytes: ByteArray) -> OptionBStepFixtureV1 {
  let tx = scripted_tx(script_bytes)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let work_witness_cbor = encode_transaction_field_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    context_cbor,
    6,
    0,
    0,
    -1,
    0,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_c,
    transaction_id: tx_id,
    transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      7,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_c,
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 8,
    work_root: h_b,
  }
  let transition = ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let resolution = ValidationResolutionStateV1 {
    version: resolution_version,
    pre_state: pre,
    operator_successor_hash: h_b,
    challenger_successor_hash: validation_trace_v1.hash_machine_state(post),
  }
  let field_preimage = field_access.encode_field_preimage(
    [
      encode_midgard_versioned_script(
        MidgardVersionedScript { language: PlutusV3Script, script_bytes },
      ),
    ],
  )
  OptionBStepFixtureV1 {
    pre,
    post,
    transition,
    resolution,
    base: PreparedValidationResolutionStateV1 {
      version: prepared_resolution_version,
      resolution,
      evidence_hash: transition_only_hash(transition),
    },
    field_preimage,
    old_evidence_hash: carriage_committed_hash(transition, field_preimage),
  }
}

fn small_fixture() -> OptionBStepFixtureV1 {
  step_fixture(fixture.sample_native_script_bytes())
}

/// A field-6 preimage past `chunk_bytes_k`, so tier 3 is its only reference
/// carriage — the §8.4 partition is a lower bound as well as an upper one.
fn boundary_fixture() -> OptionBStepFixtureV1 {
  step_fixture(fixture.repeated_bytes_v1(#"ab", 15_400))
}

fn resolver_datum(
  f: OptionBStepFixtureV1,
) -> ct.StepDatum<ValidationResolutionStateV1> {
  ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(f.resolution),
  }
}

fn semantic_datum(
  base: PreparedValidationResolutionStateV1,
) -> ct.StepDatum<PreparedValidationResolutionStateV1> {
  ct.StepDatum { fraud_prover: fixture.fraud_prover_key_hash, data: Some(base) }
}

fn observe_datum(
  prepared: staging.PreparedCanonicalDecodeItemV1,
) -> ct.StepDatum<staging.PreparedCanonicalDecodeItemV1> {
  ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(prepared),
  }
}

fn prepared_stage(
  f: OptionBStepFixtureV1,
  base: PreparedValidationResolutionStateV1,
) -> staging.PreparedCanonicalDecodeItemV1 {
  staging.prepare(
    staging.authenticate(base, f.transition),
    bind_canonical_decode_item_source_v1(f.pre, f.transition),
  )
}

fn inline_door() -> field_door.MachineFieldDoorV1 {
  field_door.MachineFieldDoorV1 {
    reference_inputs: [],
    certificate_policy_id: fixture.test_certificate_policy_id,
  }
}

fn honest_observation(f: OptionBStepFixtureV1) {
  observe_canonical_decode_item_v1(
    f.pre,
    f.transition,
    inline_door(),
    field_access.Inline { preimage: f.field_preimage },
  )
}

/// Same length, different bytes — content forgery that survives every shape
/// check and dies only on the hash.
fn flip_first_byte(bytes: ByteArray) -> ByteArray {
  let first = builtin.index_bytearray(bytes, 0)
  bytearray.push(bytearray.drop(bytes, 1), ( first + 1 ) % 256)
}

fn raw_bytes_input(bytes: ByteArray) -> Input {
  Input {
    output_reference: OutputReference { transaction_id: h_b, output_index: 7 },
    output: Output {
      address: address.from_verification_key(fixture.fraud_prover_key_hash),
      value: assets.from_lovelace(2_000_000),
      datum: InlineDatum(builtin.b_data(bytes)),
      reference_script: None,
    },
  }
}

fn publication_input(datum: ValidationProofItemDatumV1) -> Input {
  let datum_data: Data = datum
  Input {
    output_reference: OutputReference { transaction_id: h_b, output_index: 9 },
    output: Output {
      address: address.from_script(proof_item_hash),
      value: assets.from_lovelace(2_000_000),
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
  }
}

fn honest_publication(f: OptionBStepFixtureV1) -> ValidationProofItemDatumV1 {
  ValidationProofItemDatumV1 {
    version: 1,
    transaction_id: f.pre.transaction_id,
    transaction_commitment: f.pre.transaction_commitment,
    field_preimage: f.field_preimage,
  }
}

// ---------------------------------------------------------------------------
// Wire-layout goldens and retired-shape replays (row D1)
// ---------------------------------------------------------------------------

/// The pre-Option-B prepare ABI, mirrored constructor for constructor. The
/// mirror is what lets a replay vector state its bytes precisely: upcasting a
/// mirror value to `Data` reproduces the retired wire encoding exactly.
type RetiredPrepareActionV1 {
  RetiredPrepareSelected {
    input_index: Int,
    output_index: Int,
    semantic_resolver_index: Int,
    transition: ValidationOneStepWitnessV1,
    auxiliary: Data,
  }
  RetiredPrepareSelectedByEvidenceHash {
    input_index: Int,
    output_index: Int,
    semantic_resolver_index: Int,
    transition: ValidationOneStepWitnessV1,
    evidence_hash: ByteArray,
  }
}

/// The pre-Option-B authenticate ABI.
type RetiredVerifyActionV1 {
  RetiredVerify {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    carriage: field_access.FieldCarriageV1,
  }
  RetiredVerifyReference {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    reference_input_index: Int,
  }
}

/// Field-for-field mirrors of the shipped shapes, used to pin that the
/// shipped constructors sit at the indices and arities the off-chain twins
/// emit.
type PrepareWireMirrorV1 {
  PrepareSelectedMirror {
    input_index: Int,
    output_index: Int,
    semantic_resolver_index: Int,
    transition: ValidationOneStepWitnessV1,
  }
}

type VerifyWireMirrorV1 {
  VerifyMirror {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
  }
}

type ObserveWireMirrorV1 {
  ObserveMirror {
    input_index: Int,
    output_index: Int,
    carriage: field_access.FieldCarriageV1,
  }
  ObserveReferenceMirror {
    input_index: Int,
    output_index: Int,
    reference_input_index: Int,
  }
}

fn golden_state() -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: zero,
    transaction_id: zero,
    transaction_commitment: zero,
    validation_context_hash: zero,
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: zero,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 0,
    work_root: zero,
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: zero,
  }
}

fn golden_transition() -> ValidationOneStepWitnessV1 {
  ValidationOneStepWitnessV1 {
    work_witness_cbor: #"",
    claimed_successor: golden_state(),
  }
}

test prepare_selected_wire_layout_is_pinned() {
  let real: prepare_validator.SpendRedeemer = ct.Continue(
    prepare_validator.PrepareSelected {
      input_index: 1,
      output_index: 2,
      semantic_resolver_index: 1,
      transition: golden_transition(),
    },
  )
  let mirror: ct.StepRedeemer<PrepareWireMirrorV1> = ct.Continue(
    PrepareSelectedMirror {
      input_index: 1,
      output_index: 2,
      semantic_resolver_index: 1,
      transition: golden_transition(),
    },
  )
  let mirror_data: Data = mirror
  expect decoded: prepare_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

test verify_wire_layout_is_pinned() {
  let real: authenticate_validator.SpendRedeemer = ct.Continue(
    authenticate_validator.Verify {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
    },
  )
  let mirror: ct.StepRedeemer<VerifyWireMirrorV1> = ct.Continue(
    VerifyMirror {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
    },
  )
  let mirror_data: Data = mirror
  expect decoded: authenticate_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

/// The observe redeemer did not move: both delivery arms stay, at the same
/// indices, with the same arities.
test observe_wire_layout_is_unmoved() {
  let direct: observe_validator.SpendRedeemer = ct.Continue(
    observe_validator.Observe {
      input_index: 1,
      output_index: 2,
      carriage: field_access.Inline { preimage: #"" },
    },
  )
  let direct_mirror: ct.StepRedeemer<ObserveWireMirrorV1> = ct.Continue(
    ObserveMirror {
      input_index: 1,
      output_index: 2,
      carriage: field_access.Inline { preimage: #"" },
    },
  )
  let referenced: observe_validator.SpendRedeemer = ct.Continue(
    observe_validator.ObserveReference {
      input_index: 1,
      output_index: 2,
      reference_input_index: 3,
    },
  )
  let referenced_mirror: ct.StepRedeemer<ObserveWireMirrorV1> = ct.Continue(
    ObserveReferenceMirror {
      input_index: 1,
      output_index: 2,
      reference_input_index: 3,
    },
  )
  and {
    cbor.serialise(direct) == cbor.serialise(direct_mirror),
    cbor.serialise(referenced) == cbor.serialise(referenced_mirror),
  }
}

/// Positive control for the replay vectors below: the retired shapes are
/// well-formed under the retired ABI, so their refusal under the shipped ABI
/// is the narrowing at work, not malformed test data.
test retired_shapes_decode_under_the_retired_abi() {
  let aux_bearing: ct.StepRedeemer<RetiredPrepareActionV1> = ct.Continue(
    RetiredPrepareSelected {
      input_index: 1,
      output_index: 2,
      semantic_resolver_index: 1,
      transition: golden_transition(),
      auxiliary: NoAuxiliaryWitness,
    },
  )
  let aux_bearing_data: Data = aux_bearing
  expect _decoded_aux: ct.StepRedeemer<RetiredPrepareActionV1> = aux_bearing_data
  let by_hash: ct.StepRedeemer<RetiredPrepareActionV1> = ct.Continue(
    RetiredPrepareSelectedByEvidenceHash {
      input_index: 1,
      output_index: 2,
      semantic_resolver_index: 1,
      transition: golden_transition(),
      evidence_hash: zero,
    },
  )
  let by_hash_data: Data = by_hash
  expect _decoded_by_hash: ct.StepRedeemer<RetiredPrepareActionV1> = by_hash_data
  let carriage_bearing: ct.StepRedeemer<RetiredVerifyActionV1> = ct.Continue(
    RetiredVerify {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      carriage: field_access.Inline { preimage: #"" },
    },
  )
  let carriage_bearing_data: Data = carriage_bearing
  expect _decoded_carriage: ct.StepRedeemer<RetiredVerifyActionV1> = carriage_bearing_data
  True
}

/// D1: the old five-field aux-bearing `PrepareSelected` no longer decodes.
test retired_aux_bearing_prepare_selected_is_refused() fail {
  let retired: ct.StepRedeemer<RetiredPrepareActionV1> = ct.Continue(
    RetiredPrepareSelected {
      input_index: 1,
      output_index: 2,
      semantic_resolver_index: 1,
      transition: golden_transition(),
      auxiliary: NoAuxiliaryWitness,
    },
  )
  let retired_data: Data = retired
  expect _replayed: prepare_validator.SpendRedeemer = retired_data
  True
}

/// D1: the retired by-hash arm's constructor index no longer exists.
test retired_by_hash_prepare_arm_is_refused() fail {
  let retired: ct.StepRedeemer<RetiredPrepareActionV1> = ct.Continue(
    RetiredPrepareSelectedByEvidenceHash {
      input_index: 1,
      output_index: 2,
      semantic_resolver_index: 1,
      transition: golden_transition(),
      evidence_hash: zero,
    },
  )
  let retired_data: Data = retired
  expect _replayed: prepare_validator.SpendRedeemer = retired_data
  True
}

/// D1: the old four-field carriage-bearing `Verify` no longer decodes.
test retired_carriage_bearing_verify_is_refused() fail {
  let retired: ct.StepRedeemer<RetiredVerifyActionV1> = ct.Continue(
    RetiredVerify {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      carriage: field_access.Inline { preimage: #"" },
    },
  )
  let retired_data: Data = retired
  expect _replayed: authenticate_validator.SpendRedeemer = retired_data
  True
}

/// D1: the retired `VerifyReference` arm's constructor index no longer exists.
test retired_verify_reference_arm_is_refused() fail {
  let retired: ct.StepRedeemer<RetiredVerifyActionV1> = ct.Continue(
    RetiredVerifyReference {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      reference_input_index: 0,
    },
  )
  let retired_data: Data = retired
  expect _replayed: authenticate_validator.SpendRedeemer = retired_data
  True
}

/// The narrowed commitment and the retired one cannot collide: their
/// serialized auxiliary constructors differ, so the preimages differ at every
/// transition. What the pipeline commits after Option B is exactly the
/// empty-path shape the cross-language fixture pins.
test transition_only_commitment_diverges_from_the_retired_preimage() {
  let f = small_fixture()
  and {
    f.base.evidence_hash != f.old_evidence_hash,
    f.base.evidence_hash == transition_only_hash(f.transition),
  }
}

// ---------------------------------------------------------------------------
// Prepare vectors (rows E1/E2 and the D-class at the output boundary)
// ---------------------------------------------------------------------------

fn run_prepare(
  f: OptionBStepFixtureV1,
  resolver_hashes: List<ByteArray>,
  semantic_resolver_index: Int,
  transition: ValidationOneStepWitnessV1,
  output_state: Data,
) -> Bool {
  prepare_validator.main.spend(
    resolver_hashes,
    fixture.computation_thread_policy_id,
    Some(resolver_datum(f)),
    ct.Continue(
      prepare_validator.PrepareSelected {
        input_index: 0,
        output_index: 0,
        semantic_resolver_index,
        transition,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(ct_asset, output_state),
  )
}

fn expected_prepared_output(f: OptionBStepFixtureV1) -> Data {
  let expected: Data = f.base
  expected
}

test prepare_selected_advances_with_the_transition_only_commitment() {
  let f = small_fixture()
  run_prepare(
    f,
    [fixture.step_script_hash, fixture.next_step_script_hash],
    1,
    f.transition,
    expected_prepared_output(f),
  )
}

/// The resolver-index pin serves both slots; slot 0 (the empty-diff semantic)
/// is untouched by Option B and stays reachable through the same single arm.
test prepare_selected_serves_the_empty_semantic_slot_unchanged() {
  let f = small_fixture()
  run_prepare(
    f,
    [fixture.next_step_script_hash, fixture.step_script_hash],
    0,
    f.transition,
    expected_prepared_output(f),
  )
}

/// E1: same claimed successor, different work witness — the work-root pin in
/// `structural_transition_is_valid` refuses the swap.
test prepare_refuses_a_swapped_work_witness() fail {
  let f = small_fixture()
  run_prepare(
    f,
    [fixture.step_script_hash, fixture.next_step_script_hash],
    1,
    ValidationOneStepWitnessV1 {
      ..f.transition,
      work_witness_cbor: bytearray.concat(f.transition.work_witness_cbor, #"00"),
    },
    expected_prepared_output(f),
  )
}

/// E2: structurally valid witness, wrong claimed successor — the
/// challenger-successor hash pin refuses it. The forged successor steps to
/// `CompactBinding`, a legal phase transition, so the *only* guard between it
/// and the prepared state is the successor equality.
test prepare_refuses_a_wrong_claimed_successor() fail {
  let f = small_fixture()
  run_prepare(
    f,
    [fixture.step_script_hash, fixture.next_step_script_hash],
    1,
    ValidationOneStepWitnessV1 {
      ..f.transition,
      claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
        ..f.post,
        phase: validation_trace_v1.CompactBinding,
      },
    },
    expected_prepared_output(f),
  )
}

/// D at the output boundary: a prepared datum carrying the retired
/// carriage-committed hash is not producible any more — the validator computes
/// the transition-only hash itself and requires the output to carry exactly
/// that.
test prepare_refuses_an_output_carrying_the_retired_commitment() fail {
  let f = small_fixture()
  let retired_output: Data = PreparedValidationResolutionStateV1 {
    version: prepared_resolution_version,
    resolution: f.resolution,
    evidence_hash: f.old_evidence_hash,
  }
  run_prepare(
    f,
    [fixture.step_script_hash, fixture.next_step_script_hash],
    1,
    f.transition,
    retired_output,
  )
}

test prepare_refuses_a_third_semantic_resolver_index() fail {
  let f = small_fixture()
  run_prepare(
    f,
    [fixture.step_script_hash, fixture.next_step_script_hash],
    2,
    f.transition,
    expected_prepared_output(f),
  )
}

test prepare_refuses_a_widened_resolver_list() fail {
  let f = small_fixture()
  run_prepare(
    f,
    [
      fixture.step_script_hash,
      fixture.next_step_script_hash,
      fixture.step_script_hash,
    ],
    1,
    f.transition,
    expected_prepared_output(f),
  )
}

/// The phase pin: a resolution state parked in another phase cannot be
/// prepared through the canonical-decode resolver, even when its transition
/// is structurally coherent for that phase.
test prepare_refuses_a_non_canonical_decode_phase() fail {
  let f = small_fixture()
  let foreign_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..f.pre,
    phase: validation_trace_v1.CompactBinding,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CompactBinding,
      7,
      f.transition.work_witness_cbor,
    ),
  }
  let foreign_post = validation_trace_v1.ValidationMachineStateV1 {
    ..f.post,
    phase: validation_trace_v1.StaticLedgerRules,
  }
  let foreign_transition = ValidationOneStepWitnessV1 {
    work_witness_cbor: f.transition.work_witness_cbor,
    claimed_successor: foreign_post,
  }
  let foreign_resolution = ValidationResolutionStateV1 {
    version: resolution_version,
    pre_state: foreign_pre,
    operator_successor_hash: h_b,
    challenger_successor_hash: validation_trace_v1.hash_machine_state(
      foreign_post,
    ),
  }
  let foreign_output: Data = PreparedValidationResolutionStateV1 {
    version: prepared_resolution_version,
    resolution: foreign_resolution,
    evidence_hash: transition_only_hash(foreign_transition),
  }
  prepare_validator.main.spend(
    [fixture.step_script_hash, fixture.next_step_script_hash],
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(foreign_resolution),
      },
    ),
    ct.Continue(
      prepare_validator.PrepareSelected {
        input_index: 0,
        output_index: 0,
        semantic_resolver_index: 1,
        transition: foreign_transition,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(ct_asset, foreign_output),
  )
}

/// `state_is_well_formed`: a boundary whose two successor claims agree is not
/// a dispute, and preparation refuses it.
test prepare_refuses_a_degenerate_resolution_state() fail {
  let f = small_fixture()
  let degenerate = ValidationResolutionStateV1 {
    ..f.resolution,
    operator_successor_hash: f.resolution.challenger_successor_hash,
  }
  let degenerate_output: Data = PreparedValidationResolutionStateV1 {
    version: prepared_resolution_version,
    resolution: degenerate,
    evidence_hash: transition_only_hash(f.transition),
  }
  prepare_validator.main.spend(
    [fixture.step_script_hash, fixture.next_step_script_hash],
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(degenerate),
      },
    ),
    ct.Continue(
      prepare_validator.PrepareSelected {
        input_index: 0,
        output_index: 0,
        semantic_resolver_index: 1,
        transition: f.transition,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(ct_asset, degenerate_output),
  )
}

// ---------------------------------------------------------------------------
// Authenticate vectors (rows D2 and E at the commitment recompute)
// ---------------------------------------------------------------------------

fn run_authenticate(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  output_state: Data,
) -> Bool {
  authenticate_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      authenticate_validator.Verify {
        input_index: 0,
        output_index: 0,
        transition,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(ct_asset, output_state),
  )
}

fn authenticated_output(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Data {
  let output: Data = staging.authenticate(base, transition)
  output
}

test authenticate_installs_the_transition_under_the_narrowed_commitment() {
  let f = small_fixture()
  run_authenticate(
    f.base,
    f.transition,
    authenticated_output(f.base, f.transition),
  )
}

/// D2: a hand-written prepared datum whose hash was computed over the retired
/// `(transition, TransactionFieldItemWitness{…})` preimage. The narrowed
/// recompute refuses it — the two preimages differ by their serialized
/// auxiliary constructor at every transition.
test authenticate_refuses_the_retired_carriage_committed_hash() fail {
  let f = small_fixture()
  let retired_base = PreparedValidationResolutionStateV1 {
    ..f.base,
    evidence_hash: f.old_evidence_hash,
  }
  run_authenticate(
    retired_base,
    f.transition,
    authenticated_output(retired_base, f.transition),
  )
}

/// E1 at authenticate: the commitment recompute pins the witness bytes.
test authenticate_refuses_a_swapped_work_witness() fail {
  let f = small_fixture()
  let swapped = ValidationOneStepWitnessV1 {
    ..f.transition,
    work_witness_cbor: bytearray.concat(f.transition.work_witness_cbor, #"00"),
  }
  run_authenticate(f.base, swapped, authenticated_output(f.base, swapped))
}

/// E2 at authenticate: the commitment recompute pins the claimed successor.
test authenticate_refuses_a_swapped_claimed_successor() fail {
  let f = small_fixture()
  let swapped = ValidationOneStepWitnessV1 {
    ..f.transition,
    claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
      ..f.post,
      phase: validation_trace_v1.CompactBinding,
    },
  }
  run_authenticate(f.base, swapped, authenticated_output(f.base, swapped))
}

/// Output exactness: the staging installation is computed, not trusted.
test authenticate_refuses_a_mismatched_installation() fail {
  let f = small_fixture()
  let forged: Data = staging.AuthenticatedCanonicalDecodeItemV1 {
    version: 2,
    base: f.base,
    transition: f.transition,
  }
  run_authenticate(f.base, f.transition, forged)
}

// ---------------------------------------------------------------------------
// Observe vectors (row A: the door is the sole content gate, on every tier
// and through the publication arm)
// ---------------------------------------------------------------------------

fn run_observe(
  f: OptionBStepFixtureV1,
  base: PreparedValidationResolutionStateV1,
  action: observe_validator.ActionV1,
  reference_inputs: List<Input>,
  output_state: Data,
) -> Bool {
  let base_tx = fixture.continue_mid_step_l1_tx_v1(ct_asset, output_state)
  observe_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    proof_item_hash,
    fixture.test_certificate_policy_id,
    Some(observe_datum(prepared_stage(f, base))),
    ct.Continue(action),
    fixture.own_out_ref(),
    Transaction { ..base_tx, reference_inputs },
  )
}

fn observed_output(
  f: OptionBStepFixtureV1,
  base: PreparedValidationResolutionStateV1,
) -> Data {
  let output: Data = staging.observe(
    prepared_stage(f, base),
    honest_observation(f),
  )
  output
}

test observe_opens_the_committed_item_inline() {
  let f = small_fixture()
  run_observe(
    f,
    f.base,
    observe_validator.Observe {
      input_index: 0,
      output_index: 0,
      carriage: field_access.Inline { preimage: f.field_preimage },
    },
    [],
    observed_output(f, f.base),
  )
}

/// Tier 2 delivers the same bytes to the same observation — reveal-time route
/// freedom is a property of the door, not of anything committed earlier.
test observe_opens_the_same_item_by_raw_reference() {
  let f = small_fixture()
  run_observe(
    f,
    f.base,
    observe_validator.Observe {
      input_index: 0,
      output_index: 0,
      carriage: field_access.RawUtxo { ref_input_index: 0 },
    },
    [raw_bytes_input(f.field_preimage)],
    observed_output(f, f.base),
  )
}

/// The publication arm reconstructs tier 1 from a proof-item UTxO pinned to
/// the pre-state's transaction id and commitment.
test observe_opens_the_same_item_from_a_publication() {
  let f = small_fixture()
  run_observe(
    f,
    f.base,
    observe_validator.ObserveReference {
      input_index: 0,
      output_index: 0,
      reference_input_index: 0,
    },
    [publication_input(honest_publication(f))],
    observed_output(f, f.base),
  )
}

/// Tier 3, at a width where tier 3 is the only reference carriage.
test observe_certified_carriage_opens_a_boundary_item() {
  let f = boundary_fixture()
  let observation = observe_canonical_decode_item_v1(
    f.pre,
    f.transition,
    field_door.MachineFieldDoorV1 {
      reference_inputs: fixture.certified_reference_inputs_v1(
        f.pre.transaction_id,
        6,
        f.field_preimage,
      ),
      certificate_policy_id: fixture.test_certificate_policy_id,
    },
    fixture.certified_carriage_v1(f.field_preimage),
  )
  let output: Data = staging.observe(prepared_stage(f, f.base), observation)
  run_observe(
    f,
    f.base,
    observe_validator.Observe {
      input_index: 0,
      output_index: 0,
      carriage: fixture.certified_carriage_v1(f.field_preimage),
    },
    fixture.certified_reference_inputs_v1(
      f.pre.transaction_id,
      6,
      f.field_preimage,
    ),
    output,
  )
}

/// A1: wrong content inline — the door's flat-hash check aborts the step.
test observe_aborts_a_byte_flipped_inline_preimage() fail {
  let f = small_fixture()
  run_observe(
    f,
    f.base,
    observe_validator.Observe {
      input_index: 0,
      output_index: 0,
      carriage: field_access.Inline {
        preimage: flip_first_byte(f.field_preimage),
      },
    },
    [],
    observed_output(f, f.base),
  )
}

/// A2: wrong content behind a raw reference — same check, tier 2 route.
test observe_aborts_a_byte_flipped_raw_reference() fail {
  let f = small_fixture()
  run_observe(
    f,
    f.base,
    observe_validator.Observe {
      input_index: 0,
      output_index: 0,
      carriage: field_access.RawUtxo { ref_input_index: 0 },
    },
    [raw_bytes_input(flip_first_byte(f.field_preimage))],
    observed_output(f, f.base),
  )
}

/// A7-class: a certified manifest whose chunk carriage substitutes one chunk
/// of equal length — every shape check passes, and the materialized
/// concatenation's re-hash against the anchored commitment aborts.
test observe_aborts_a_substituted_certified_chunk() fail {
  let f = boundary_fixture()
  let honest_inputs = fixture.certified_reference_inputs_v1(
    f.pre.transaction_id,
    6,
    f.field_preimage,
  )
  let substituted = list.indexed_map(
    honest_inputs,
    fn(index, input) {
      if index == 1 {
        expect InlineDatum(chunk_datum) = input.output.datum
        let chunk = builtin.un_b_data(chunk_datum)
        Input {
          ..input,
          output: Output {
            ..input.output,
            datum: InlineDatum(builtin.b_data(flip_first_byte(chunk))),
          },
        }
      } else {
        input
      }
    },
  )
  run_observe(
    f,
    f.base,
    observe_validator.Observe {
      input_index: 0,
      output_index: 0,
      carriage: fixture.certified_carriage_v1(f.field_preimage),
    },
    substituted,
    observed_output(f, f.base),
  )
}

/// A3: a publication that names the right transaction and commitment but
/// carries flipped bytes — the reconstruction feeds tier 1 and the door
/// aborts on content.
test observe_aborts_a_publication_with_flipped_bytes() fail {
  let f = small_fixture()
  run_observe(
    f,
    f.base,
    observe_validator.ObserveReference {
      input_index: 0,
      output_index: 0,
      reference_input_index: 0,
    },
    [
      publication_input(
        ValidationProofItemDatumV1 {
          ..honest_publication(f),
          field_preimage: flip_first_byte(f.field_preimage),
        },
      ),
    ],
    observed_output(f, f.base),
  )
}

/// A4: a publication for another transaction is refused by the arm before the
/// door ever runs.
test observe_refuses_a_publication_for_another_transaction() fail {
  let f = small_fixture()
  run_observe(
    f,
    f.base,
    observe_validator.ObserveReference {
      input_index: 0,
      output_index: 0,
      reference_input_index: 0,
    },
    [
      publication_input(
        ValidationProofItemDatumV1 {
          ..honest_publication(f),
          transaction_id: h_b,
        },
      ),
    ],
    observed_output(f, f.base),
  )
}

/// A4: same refusal on the commitment pin.
test observe_refuses_a_publication_with_a_wrong_commitment() fail {
  let f = small_fixture()
  run_observe(
    f,
    f.base,
    observe_validator.ObserveReference {
      input_index: 0,
      output_index: 0,
      reference_input_index: 0,
    },
    [
      publication_input(
        ValidationProofItemDatumV1 {
          ..honest_publication(f),
          transaction_commitment: h_b,
        },
      ),
    ],
    observed_output(f, f.base),
  )
}

/// Documented consequence of the deletion, asserted so the deletion cannot
/// silently regress into a recheck: a staged datum whose `evidence_hash` is
/// the *retired* carriage-committed value still observes. On L1 such a datum
/// is unreachable — the authenticate stage upstream refuses the retired hash
/// (see `authenticate_refuses_the_retired_carriage_committed_hash`), and the
/// staging chain's script-hash hand-off is the provenance of everything this
/// stage consumes. Content is the door's question, answered above.
test observe_no_longer_rechecks_the_evidence_hash() {
  let f = small_fixture()
  let retired_base = PreparedValidationResolutionStateV1 {
    ..f.base,
    evidence_hash: f.old_evidence_hash,
  }
  run_observe(
    f,
    retired_base,
    observe_validator.Observe {
      input_index: 0,
      output_index: 0,
      carriage: field_access.Inline { preimage: f.field_preimage },
    },
    [],
    observed_output(f, retired_base),
  )
}

/// Output exactness: the observation the thread carries forward is computed
/// by the door, not asserted by the prover.
test observe_refuses_a_forged_observation() fail {
  let f = small_fixture()
  let honest = honest_observation(f)
  let forged: Data = staging.observe(
    prepared_stage(f, f.base),
    validation_machine_v1.CanonicalDecodeItemObservationV1 {
      ..honest,
      item_length: honest.item_length + 1,
    },
  )
  run_observe(
    f,
    f.base,
    observe_validator.Observe {
      input_index: 0,
      output_index: 0,
      carriage: field_access.Inline { preimage: f.field_preimage },
    },
    [],
    forged,
  )
}

/// The staged hand-off pin: the observation must leave under the proof
/// verifier's script hash.
test observe_refuses_a_wrong_output_script() fail {
  let f = small_fixture()
  observe_validator.main.spend(
    fixture.step_script_hash,
    fixture.computation_thread_policy_id,
    proof_item_hash,
    fixture.test_certificate_policy_id,
    Some(observe_datum(prepared_stage(f, f.base))),
    ct.Continue(
      observe_validator.Observe {
        input_index: 0,
        output_index: 0,
        carriage: field_access.Inline { preimage: f.field_preimage },
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(ct_asset, observed_output(f, f.base)),
  )
}
