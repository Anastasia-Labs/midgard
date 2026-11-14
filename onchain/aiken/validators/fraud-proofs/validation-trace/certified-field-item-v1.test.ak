//// The certified route of the variable-width field door
//// (`validation_machine_field_door_v1.MachineFieldItemRouteV1`), from the
//// door itself up through every deployed stage that reads a variable-width
//// field: the canonical-decode item observation (fields 2 and 6), the
//// script-sources stage-zero begin (field 6), stage-one begin (field 8),
//// stage-four begin (field 2) and stage-six policy begin (field 5).
////
//// Through the field grammar certificate a stage reads the field's item
//// without walking the field, so its cost no longer grows with the field's
//// item count. Three things are shown here:
////
//// - **Scale.** Each stage runs at a field as wide as §5.4 admits, its
////   preimage carried by tier 3 as an L1 transaction has to carry it. Every
////   `..._at_scale` vector is a budget row paired with a `..._fixture_only`
////   control that builds the same transaction without running the handler.
//// - **Same successor.** On a small field, the walking route and the
////   certified route accept the same claimed successor for the same step, and
////   refuse the same wrong one. Stage-one begin and the stage-six yield read
////   their item through the certificate alone, so their walking route is the
////   walking reference machine (`script_sources_stage_one`,
////   `script_sources_stage_six`) on the same step.
//// - **The route is not evidence.** The certificate's reference input rides
////   the stage's own redeemer. A step whose committed evidence folds it into
////   the auxiliary is refused, while the honest evidence is accepted whichever
////   route the redeemer names.
////
//// The door-level vectors refuse every certificate that does not certify the
//// committed field: another field, another transaction, another commitment,
//// another length, one still `InProgress`, one under another policy, an index
//// past the certified count, and a fixed-stride field, which has no
//// certificate.

use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Spend, Transaction, placeholder,
}
use fraud_proofs/validation_trace/canonical_decode_item_observe_v1 as observe_validator
use fraud_proofs/validation_trace/script_sources_middle_yields_v1 as middle_yields
use fraud_proofs/validation_trace/script_sources_stage_one_redeemer_semantic_v1 as stage_one_validator
use fraud_proofs/validation_trace/script_sources_stage_zero_begin_semantic_v1 as stage_zero_validator
use midgard/blake2b_224_trace_v1
use midgard/bounded_item_v1
use midgard/canonical_decode_item_staging_v1 as staging
use midgard/computation_thread as ct
use midgard/field_grammar_certificate_v1.{Complete, InProgress}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/native_tx_field_access_v1.{Inline, field_commitment}
use midgard/redeemer_item_proof_v1
use midgard/script_proof_v1
use midgard/validation_machine/canonical_decode.{
  bind_canonical_decode_item_source_v1,
}
use midgard/validation_machine/machine_types.{
  CanonicalDecodeItemObservationV1, InlineSourceHashControlV1, MintFoldControlV1,
  NoAuxiliaryWitness, ScriptSourcesControlV1, TransactionFieldChunkWitness,
  TransactionRedeemerItemBeginWitness, ValidationAuxiliaryWitnessV1,
  ValidationOneStepWitnessV1,
}
use midgard/validation_machine/script_sources_early_stages as early_stages
use midgard/validation_machine/script_sources_middle_stages as middle_stages
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_field_door_v1.{
  CertifiedFieldItem, MachineFieldDoorV1, MachineFieldItemRouteV1,
  MachineFieldItemV1, WalkFieldItem, machine_field_item_bytes,
  machine_field_item_count, open_machine_certified_field_payload_by_commitment,
  open_machine_variable_field_item_by_commitment,
}
use midgard/validation_machine_tests/constants.{
  h_a, h_b, h_c, validation_context_cbor,
}
use midgard/validation_machine_tests/fixtures_certified_field_item as cfi
use midgard/validation_machine_tests/fixtures_script_identity.{
  stage_zero_script_sources_work_witness,
}
use midgard/validation_merkle_v1
use midgard/validation_resolution_v1.{
  PreparedValidationResolutionStateV1, ValidationResolutionStateV1,
  hash_one_step_evidence, prepared_resolution_version, resolution_version,
  winning_resolution,
}
use midgard/validation_trace_v1.{ValidationMachineStateV1}

const proof_item_hash =
  #"9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a"

const dispatcher = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const other_policy = #"6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e"

const ct_asset = "certified-field-item"

// ---------------------------------------------------------------------------
// Field shapes
// ---------------------------------------------------------------------------

/// Field-2 outputs of 41 payload bytes: 761 of them fill §5.4's 32,768 bytes.
fn output_payload() -> ByteArray {
  cfi.repeat(#"a1", 41)
}

/// Field-6 scripts of 8 payload bytes: 3,640 of them fill the field.
fn short_script_payload() -> ByteArray {
  cfi.repeat(#"a2", 8)
}

/// A field-6 versioned PlutusV3 script of 30 bytes: `[3, bytes]`.
fn script_witness_payload() -> ByteArray {
  bytearray.concat(#"8203581e", cfi.repeat(#"5a", 30))
}

/// A field-8 redeemer item of 18 payload bytes: 1,724 of them fill the field.
fn redeemer_payload() -> ByteArray {
  cfi.repeat(#"a3", 18)
}

const mint_policy = #"b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1"

/// A field-5 mint entry `[policy, {"": 1}]`.
fn mint_payload() -> ByteArray {
  #"82581c"
    |> bytearray.concat(mint_policy)
    |> bytearray.concat(#"a14001")
}

/// Payload `k` is `1 + (7k mod 40)` bytes, crossing the one- and two-byte
/// wrapper forms, so every offset is a sum and not a product.
fn varied_field(n: Int) -> ByteArray {
  cfi.field_of(
    list.map(list.range(0, n - 1), fn(k) { cfi.repeat(#"c3", 1 + 7 * k % 40) }),
  )
}

fn delivery(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
  tier3: Bool,
) -> cfi.CertifiedDeliveryV1 {
  if tier3 {
    cfi.certified_delivery(tx_id, field_index, preimage)
  } else {
    cfi.inline_delivery(tx_id, field_index, preimage)
  }
}

fn route_of(
  delivered: cfi.CertifiedDeliveryV1,
  certified: Bool,
) -> MachineFieldItemRouteV1 {
  if certified {
    CertifiedFieldItem { cert_ref_input_index: delivered.grammar_index }
  } else {
    WalkFieldItem
  }
}

fn leaves(count: Int) -> validation_merkle_v1.BuiltFrontier {
  validation_merkle_v1.build_frontier(list.repeat(h_a, count))
}

// ---------------------------------------------------------------------------
// The door: either route opens the same item; a certificate that does not
// certify the committed field is refused
// ---------------------------------------------------------------------------

fn door_with(reference_inputs: List<Input>) -> MachineFieldDoorV1 {
  MachineFieldDoorV1 {
    reference_inputs,
    certificate_policy_id: fixture.test_certificate_policy_id,
  }
}

fn open_at(
  door: MachineFieldDoorV1,
  route: MachineFieldItemRouteV1,
  field_index: Int,
  preimage: ByteArray,
  item_index: Int,
) -> MachineFieldItemV1 {
  open_machine_variable_field_item_by_commitment(
    door,
    cfi.grammar_certificate_policy_id,
    route,
    h_a,
    field_commitment(preimage),
    field_index,
    item_index,
    Inline { preimage },
  )
}

fn routes_agree(field_index: Int, n: Int) -> Bool {
  let preimage = varied_field(n)
  let door = door_with([cfi.certificate_input(h_a, field_index, preimage)])
  list.all(
    list.range(0, n - 1),
    fn(k) {
      open_at(door, WalkFieldItem, field_index, preimage, k) == open_at(
        door,
        CertifiedFieldItem { cert_ref_input_index: 0 },
        field_index,
        preimage,
        k,
      )
    },
  )
}

/// Every item of fields with one, sixteen, seventeen and forty items — on
/// every waypoint, beside it and between two — opens to the same item, view,
/// offset and length on either route.
test certified_route_opens_every_item_the_walk_opens() {
  and {
    routes_agree(2, 1),
    routes_agree(5, 16),
    routes_agree(6, 17),
    routes_agree(8, 40),
  }
}

const door_items = 20

/// Item `item_index` of a 20-item field-2 preimage, through the certificate
/// `certificate_input` at reference input `cert_index`.
fn certified_open(
  certificate_input: Input,
  cert_index: Int,
  item_index: Int,
) -> Bool {
  let item = open_at(
    door_with([certificate_input]),
    CertifiedFieldItem { cert_ref_input_index: cert_index },
    2,
    varied_field(door_items),
    item_index,
  )
  item.item_length > 0
}

/// The honest certificate with one field changed.
fn altered(
  change: fn(ByteArray, Int, ByteArray, Int, Int, ByteArray) ->
    field_grammar_certificate_v1.FieldGrammarCertificateV1,
) -> Input {
  expect
      Complete {
        tx_id,
        field_index,
        field_hash,
        total_length,
        item_count,
        waypoints,
        ..
      }
    = cfi.complete_certificate(h_a, 2, varied_field(door_items))
  cfi.certificate_input_under(
    cfi.grammar_certificate_policy_id,
    change(tx_id, field_index, field_hash, total_length, item_count, waypoints),
  )
}

fn complete_of(
  tx_id: ByteArray,
  field_index: Int,
  field_hash: ByteArray,
  total_length: Int,
  item_count: Int,
  waypoints: ByteArray,
) -> field_grammar_certificate_v1.FieldGrammarCertificateV1 {
  Complete {
    owner: cfi.grammar_certificate_owner,
    tx_id,
    field_index,
    field_hash,
    total_length,
    item_count,
    waypoints,
  }
}

test door_reads_the_last_item_through_an_honest_certificate() {
  certified_open(cfi.certificate_input(h_a, 2, varied_field(door_items)), 0, 19)
}

test door_refuses_a_certificate_for_another_field() fail {
  certified_open(cfi.certificate_input(h_a, 5, varied_field(door_items)), 0, 19)
}

test door_refuses_a_certificate_for_another_transaction() fail {
  certified_open(cfi.certificate_input(h_b, 2, varied_field(door_items)), 0, 19)
}

/// Same length, same grammar, another commitment.
test door_refuses_a_certificate_over_another_commitment() fail {
  certified_open(
    altered(
      fn(tx_id, field_index, _, total_length, item_count, waypoints) {
        complete_of(
          tx_id,
          field_index,
          field_commitment(#"80"),
          total_length,
          item_count,
          waypoints,
        )
      },
    ),
    0,
    19,
  )
}

test door_refuses_a_certificate_over_another_length() fail {
  certified_open(
    altered(
      fn(tx_id, field_index, field_hash, total_length, item_count, waypoints) {
        complete_of(
          tx_id,
          field_index,
          field_hash,
          total_length + 1,
          item_count,
          waypoints,
        )
      },
    ),
    0,
    19,
  )
}

test door_refuses_an_in_progress_certificate() fail {
  certified_open(
    altered(
      fn(tx_id, field_index, field_hash, total_length, _, waypoints) {
        InProgress {
          owner: cfi.grammar_certificate_owner,
          tx_id,
          field_index,
          field_hash,
          total_length,
          grammar_checkpoint_hash: h_c,
          waypoints,
        }
      },
    ),
    0,
    19,
  )
}

test door_refuses_a_certificate_under_another_policy() fail {
  certified_open(
    cfi.certificate_input_under(
      other_policy,
      cfi.complete_certificate(h_a, 2, varied_field(door_items)),
    ),
    0,
    19,
  )
}

test door_refuses_an_index_past_the_certified_count() fail {
  certified_open(
    cfi.certificate_input(h_a, 2, varied_field(door_items)),
    0,
    door_items,
  )
}

test door_refuses_a_negative_index() fail {
  certified_open(cfi.certificate_input(h_a, 2, varied_field(door_items)), 0, -1)
}

test door_refuses_a_certificate_index_naming_no_input() fail {
  certified_open(cfi.certificate_input(h_a, 2, varied_field(door_items)), 1, 0)
}

fn fixed_stride_open(route: MachineFieldItemRouteV1) -> Bool {
  let preimage = cfi.uniform_field(
    3,
    bytearray.concat(#"581c", cfi.repeat(#"d4", 28)),
  )
  let item = open_at(
    door_with([cfi.certificate_input(h_a, 4, preimage)]),
    route,
    4,
    preimage,
    2,
  )
  item.item_length == 28
}

/// Field 4 has a fixed stride: the walking route opens it by arithmetic.
test door_walking_route_opens_a_fixed_stride_field() {
  fixed_stride_open(WalkFieldItem)
}

/// A fixed-stride field has no certificate, so the certified route refuses
/// it even beside a certificate naming it.
test door_refuses_the_certified_route_on_a_fixed_stride_field() fail {
  fixed_stride_open(CertifiedFieldItem { cert_ref_input_index: 0 })
}

// ---------------------------------------------------------------------------
// The payload door: the certified-only read stage-one begin makes
// ---------------------------------------------------------------------------

/// Item `item_index` of field `field_index`'s `preimage`, carried inline,
/// through the payload door, under commitment `commitment`.
fn payload_at(
  door: MachineFieldDoorV1,
  field_index: Int,
  commitment: ByteArray,
  preimage: ByteArray,
  item_index: Int,
) -> Pair<Int, ByteArray> {
  open_machine_certified_field_payload_by_commitment(
    door,
    cfi.grammar_certificate_policy_id,
    0,
    h_a,
    commitment,
    field_index,
    item_index,
    Inline { preimage },
  )
}

fn payloads_agree(field_index: Int, n: Int) -> Bool {
  let preimage = varied_field(n)
  let door = door_with([cfi.certificate_input(h_a, field_index, preimage)])
  list.all(
    list.range(0, n - 1),
    fn(k) {
      let walked = open_at(door, WalkFieldItem, field_index, preimage, k)
      payload_at(door, field_index, field_commitment(preimage), preimage, k) == Pair(
        machine_field_item_count(walked),
        machine_field_item_bytes(walked),
      )
    },
  )
}

/// Every item of fields with one, sixteen, seventeen and forty items reads
/// through the payload door as the walking route's item count and bytes.
test certified_payload_reads_every_item_the_walk_opens() {
  and {
    payloads_agree(2, 1),
    payloads_agree(5, 16),
    payloads_agree(6, 17),
    payloads_agree(8, 40),
  }
}

/// The payload door checks the preimage against the commitment itself: a
/// preimage of the certified length and grammar, one payload byte changed,
/// is refused beside the honest certificate.
test certified_payload_refuses_a_preimage_off_its_commitment() fail {
  let preimage = varied_field(door_items)
  let last = bytearray.length(preimage) - 1
  let changed = bytearray.concat(bytearray.take(preimage, last), #"c4")
  let door = door_with([cfi.certificate_input(h_a, 2, preimage)])
  payload_at(door, 2, field_commitment(preimage), changed, door_items - 1).1st == door_items
}

/// The payload door refuses a fixed-stride field, which has no certificate,
/// even beside a certificate naming it.
test certified_payload_refuses_a_fixed_stride_field() fail {
  let preimage = cfi.uniform_field(
    3,
    bytearray.concat(#"581c", cfi.repeat(#"d4", 28)),
  )
  let door = door_with([cfi.certificate_input(h_a, 4, preimage)])
  payload_at(door, 4, field_commitment(preimage), preimage, 2).1st == 3
}

// ---------------------------------------------------------------------------
// Shared step plumbing
// ---------------------------------------------------------------------------

fn prepared_of(
  pre: ValidationMachineStateV1,
  post: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: Data,
) -> PreparedValidationResolutionStateV1 {
  let transition_data: Data = transition
  PreparedValidationResolutionStateV1 {
    version: prepared_resolution_version,
    resolution: ValidationResolutionStateV1 {
      version: resolution_version,
      pre_state: pre,
      operator_successor_hash: h_b,
      challenger_successor_hash: validation_trace_v1.hash_machine_state(post),
    },
    evidence_hash: hash_one_step_evidence(transition_data, auxiliary),
  }
}

fn continue_tx(output_state: Data, reference_inputs: List<Input>) -> Transaction {
  Transaction {
    ..fixture.continue_mid_step_l1_tx_v1(ct_asset, output_state),
    reference_inputs,
  }
}

/// The auxiliary with `route` appended to its fields: what a step would have
/// committed had the route been part of its evidence.
fn with_route_folded_in(auxiliary: Data, route: MachineFieldItemRouteV1) -> Data {
  let route_data: Data = route
  let Pair(tag, fields) = builtin.un_constr_data(auxiliary)
  builtin.constr_data(tag, list.concat(fields, [route_data]))
}

/// The script-sources control every stage below starts from: nothing folded
/// yet, at `stage`, over `source`.
fn script_sources_control(
  source: cfi.FieldSourceV1,
  stage: Int,
) -> ScriptSourcesControlV1 {
  ScriptSourcesControlV1 {
    compact_cbor: source.compact_cbor,
    witness_set_compact_cbor: source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    signer_count: 0,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      0,
      validation_merkle_v1.empty_frontier(),
    ),
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    stage,
    source_count: 0,
    source_peaks: validation_merkle_v1.empty_frontier(),
    redeemer_count: 0,
    redeemer_peaks: validation_merkle_v1.empty_frontier(),
    replay_cursor: 0,
    replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    spend_index: 0,
    purpose_count: 0,
    purpose_peaks: validation_merkle_v1.empty_frontier(),
    output_cursor: 0,
    output_count: 0,
    output_peaks: validation_merkle_v1.empty_frontier(),
    output_total_count: 0,
    receive_scan: vm_witness.empty_receive_purpose_scan_control(),
    inline_source_count: 0,
    redeemer_total_count: 0,
    observer_scan: vm_witness.empty_observer_purpose_scan_control(),
    discovery: vm_witness.empty_script_discovery_control(),
    output_proof: None,
    pending_source_cbor: #"",
    mint_fold: vm_witness.empty_mint_fold_control(),
    resolution_schedule_hash: vm_witness.transaction_resolution_schedule_hash(
      [],
      [],
    ),
  }
}

fn script_sources_state(
  source: cfi.FieldSourceV1,
  program_counter: Int,
  work_witness_cbor: ByteArray,
) -> ValidationMachineStateV1 {
  cfi.source_state(
    source,
    validation_context_cbor,
    validation_trace_v1.ScriptSources,
    program_counter,
    work_witness_cbor,
  )
}

/// One semantic-validator spend: its datum, redeemer and L1 transaction.
type SpendCaseV1<redeemer> {
  datum: Option<ct.StepDatum<PreparedValidationResolutionStateV1>>,
  redeemer: redeemer,
  tx: Transaction,
}

/// Builds a case's datum, redeemer and transaction and runs no handler.
fn fixture_built(
  datum: Option<ct.StepDatum<PreparedValidationResolutionStateV1>>,
  redeemer: Data,
  tx: Transaction,
) -> Bool {
  and {
    list.length(tx.reference_inputs) > 0,
    datum != None,
    redeemer != builtin.i_data(0),
  }
}

fn stage_zero_fixture_only(
  case: SpendCaseV1<stage_zero_validator.SpendRedeemer>,
) -> Bool {
  let redeemer: Data = case.redeemer
  fixture_built(case.datum, redeemer, case.tx)
}

fn stage_one_fixture_only(
  case: SpendCaseV1<stage_one_validator.SpendRedeemer>,
) -> Bool {
  let redeemer: Data = case.redeemer
  fixture_built(case.datum, redeemer, case.tx)
}

// ---------------------------------------------------------------------------
// Canonical-decode item observation (fields 2 and 6)
// ---------------------------------------------------------------------------

type ObserveCaseV1 {
  datum: Option<observe_validator.Datum>,
  redeemer: observe_validator.SpendRedeemer,
  tx: Transaction,
}

/// The observation of item `k` of `n` copies of `payload` in field
/// `field_index`, its expected output claiming `claimed_count` items.
fn observe_case(
  field_index: Int,
  payload: ByteArray,
  n: Int,
  k: Int,
  tier3: Bool,
  certified: Bool,
  claimed_count: Int,
) -> ObserveCaseV1 {
  let preimage = cfi.uniform_field(n, cfi.wrap(payload))
  let source = cfi.field_source(field_index, preimage)
  let delivered = delivery(source.tx_id, field_index, preimage, tier3)
  let work_witness_cbor = vm_witness.encode_transaction_field_scan_witness(
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
    validation_context_cbor,
    field_index,
    k,
    0,
    if k == 0 {
      -1
    } else {
      n
    },
    0,
  )
  let pre = cfi.source_state(
    source,
    validation_context_cbor,
    validation_trace_v1.CanonicalDecode,
    7,
    work_witness_cbor,
  )
  let post = ValidationMachineStateV1 {
    ..pre,
    program_counter: 8,
    work_root: h_b,
  }
  let transition = ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let prepared = staging.prepare(
    staging.authenticate(
      prepared_of(pre, post, transition, NoAuxiliaryWitness),
      transition,
    ),
    bind_canonical_decode_item_source_v1(pre, transition),
  )
  let output: Data = staging.observe(
    prepared,
    CanonicalDecodeItemObservationV1 {
      item_count: claimed_count,
      item_length: bytearray.length(payload),
      item_order_is_valid: True,
    },
  )
  ObserveCaseV1 {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(prepared),
      },
    ),
    redeemer: ct.Continue(
      observe_validator.Observe {
        input_index: 0,
        output_index: 0,
        carriage: delivered.carriage,
        route: route_of(delivered, certified),
      },
    ),
    tx: continue_tx(output, delivered.door.reference_inputs),
  }
}

fn run_observe(case: ObserveCaseV1) -> Bool {
  observe_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    proof_item_hash,
    fixture.test_certificate_policy_id,
    cfi.grammar_certificate_policy_id,
    case.datum,
    case.redeemer,
    fixture.own_out_ref(),
    case.tx,
  )
}

fn observe_fixture_only(case: ObserveCaseV1) -> Bool {
  let redeemer: Data = case.redeemer
  and {
    list.length(case.tx.reference_inputs) > 0,
    case.datum != None,
    redeemer != builtin.i_data(0),
  }
}

fn observe_outputs_at_scale(certified: Bool) -> ObserveCaseV1 {
  observe_case(2, output_payload(), 761, 760, True, certified, 761)
}

fn observe_scripts_at_scale(certified: Bool) -> ObserveCaseV1 {
  observe_case(6, short_script_payload(), 3640, 3639, True, certified, 3640)
}

test certified_fixtures_sit_at_the_field_bound() {
  and {
    bytearray.length(cfi.uniform_field(761, cfi.wrap(output_payload()))) == 32726,
    bytearray.length(cfi.uniform_field(3640, cfi.wrap(short_script_payload()))) == 32763,
    bytearray.length(cfi.uniform_field(900, cfi.wrap(script_witness_payload()))) == 32403,
    bytearray.length(cfi.uniform_field(1724, cfi.wrap(redeemer_payload()))) == 32759,
    bytearray.length(cfi.uniform_field(910, cfi.wrap(mint_payload()))) == 32763,
  }
}

/// Item 760 of 761 field-2 outputs, carried by tier 3.
test observe_certified_item_760_of_761_outputs_at_scale() {
  run_observe(observe_outputs_at_scale(True))
}

test observe_certified_item_760_of_761_outputs_fixture_only() {
  observe_fixture_only(observe_outputs_at_scale(True))
}

/// Item 3,639 of 3,640 field-6 scripts, carried by tier 3.
test observe_certified_item_3639_of_3640_scripts_at_scale() {
  run_observe(observe_scripts_at_scale(True))
}

test observe_certified_item_3639_of_3640_scripts_fixture_only() {
  observe_fixture_only(observe_scripts_at_scale(True))
}

/// The walking route over the same 761 outputs: the cost the certified route
/// removes.
test observe_walking_item_760_of_761_outputs_at_scale() {
  run_observe(observe_outputs_at_scale(False))
}

test observe_walking_item_3639_of_3640_scripts_at_scale() {
  run_observe(observe_scripts_at_scale(False))
}

/// On a small field both routes accept the same observation.
test observe_routes_accept_the_same_observation() {
  and {
    run_observe(observe_case(2, output_payload(), 20, 19, False, False, 20)),
    run_observe(observe_case(2, output_payload(), 20, 19, False, True, 20)),
    run_observe(
      observe_case(6, short_script_payload(), 17, 16, False, False, 17),
    ),
    run_observe(
      observe_case(6, short_script_payload(), 17, 16, False, True, 17),
    ),
  }
}

test observe_walking_route_refuses_a_wrong_count() fail {
  run_observe(observe_case(2, output_payload(), 20, 19, False, False, 21))
}

test observe_certified_route_refuses_a_wrong_count() fail {
  run_observe(observe_case(2, output_payload(), 20, 19, False, True, 21))
}

// ---------------------------------------------------------------------------
// Script-sources stage-zero begin (field 6)
// ---------------------------------------------------------------------------

/// Stage-zero begin on source `k` of `n` script witnesses. `wrong_total`
/// claims one source more than the field has; `route_in_evidence` commits the
/// route inside the step's auxiliary.
fn stage_zero_case(
  n: Int,
  k: Int,
  tier3: Bool,
  certified: Bool,
  wrong_total: Bool,
  route_in_evidence: Bool,
) -> SpendCaseV1<stage_zero_validator.SpendRedeemer> {
  let payload = script_witness_payload()
  let preimage = cfi.uniform_field(n, cfi.wrap(payload))
  let source = cfi.field_source(6, preimage)
  let delivered = delivery(source.tx_id, 6, preimage, tier3)
  let route = route_of(delivered, certified)
  let sources = leaves(k)
  let control = ScriptSourcesControlV1 {
    ..script_sources_control(source, 0),
    source_count: sources.count,
    source_peaks: sources.peaks,
    inline_source_count: if k == 0 {
      0
    } else {
      n
    },
  }
  let total = if wrong_total {
    n + 1
  } else {
    n
  }
  let pending = InlineSourceHashControlV1 {
    version: 1,
    source_index: k,
    inline_source_count: total,
    language_tag: 3,
    payload_offset: 4,
    payload_length: 30,
    item_length: bytearray.length(payload),
    item_commitment: bounded_item_v1.from_bytes(6, k, payload),
    hash_control: blake2b_224_trace_v1.initial_control_v1(31),
  }
  let pending_control = ScriptSourcesControlV1 {
    ..control,
    inline_source_count: total,
    pending_source_cbor: vm_witness.encode_inline_source_hash_control_v1(
      pending,
    ),
  }
  let pre = script_sources_state(
    source,
    9,
    stage_zero_script_sources_work_witness(control),
  )
  let post = script_sources_state(
    source,
    10,
    stage_zero_script_sources_work_witness(pending_control),
  )
  let transition = ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_zero_script_sources_work_witness(control),
    claimed_successor: post,
  }
  let auxiliary: Data = TransactionFieldChunkWitness {
    field_index: 6,
    item_index: k,
    carriage: delivered.carriage,
  }
  let committed = if route_in_evidence {
    with_route_folded_in(auxiliary, route)
  } else {
    auxiliary
  }
  SpendCaseV1 {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(prepared_of(pre, post, transition, committed)),
      },
    ),
    redeemer: ct.Continue(
      stage_zero_validator.VerifyBegin {
        input_index: 0,
        output_index: 0,
        transition,
        field_index: 6,
        item_index: k,
        carriage: delivered.carriage,
        route,
      },
    ),
    tx: continue_tx(winning_resolution(), delivered.door.reference_inputs),
  }
}

fn run_stage_zero(case: SpendCaseV1<stage_zero_validator.SpendRedeemer>) -> Bool {
  stage_zero_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    cfi.grammar_certificate_policy_id,
    case.datum,
    case.redeemer,
    fixture.own_out_ref(),
    case.tx,
  )
}

/// Source 895 of 900: fifteen items past its waypoint, the most any read
/// takes.
test stage_zero_begin_certified_source_895_of_900_at_scale() {
  run_stage_zero(stage_zero_case(900, 895, True, True, False, False))
}

test stage_zero_begin_certified_source_895_of_900_fixture_only() {
  stage_zero_fixture_only(stage_zero_case(900, 895, True, True, False, False))
}

test stage_zero_begin_walking_source_895_of_900_at_scale() {
  run_stage_zero(stage_zero_case(900, 895, True, False, False, False))
}

test stage_zero_begin_routes_accept_the_same_successor() {
  and {
    run_stage_zero(stage_zero_case(3, 0, False, False, False, False)),
    run_stage_zero(stage_zero_case(3, 0, False, True, False, False)),
    run_stage_zero(stage_zero_case(20, 17, False, False, False, False)),
    run_stage_zero(stage_zero_case(20, 17, False, True, False, False)),
  }
}

test stage_zero_begin_walking_route_refuses_a_wrong_successor() fail {
  run_stage_zero(stage_zero_case(3, 0, False, False, True, False))
}

test stage_zero_begin_certified_route_refuses_a_wrong_successor() fail {
  run_stage_zero(stage_zero_case(3, 0, False, True, True, False))
}

test stage_zero_begin_refuses_the_route_in_its_evidence() fail {
  run_stage_zero(stage_zero_case(3, 0, False, True, False, True))
}

// ---------------------------------------------------------------------------
// Script-sources stage-one begin (field 8, certified route only)
// ---------------------------------------------------------------------------

/// One machine step as the walking reference machine reads it: the state
/// before it, the transition, the step's auxiliary, its decoded control and
/// the delivered field.
type StepPartsV1 {
  pre: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
  delivered: cfi.CertifiedDeliveryV1,
}

/// Stage-one begin on redeemer `k` of `n`.
fn stage_one_parts(
  n: Int,
  k: Int,
  tier3: Bool,
  wrong_total: Bool,
) -> StepPartsV1 {
  let payload = redeemer_payload()
  let preimage = cfi.uniform_field(n, cfi.wrap(payload))
  let source = cfi.field_source(8, preimage)
  let delivered = delivery(source.tx_id, 8, preimage, tier3)
  let redeemers = leaves(k)
  let control = ScriptSourcesControlV1 {
    ..script_sources_control(source, 1),
    redeemer_count: redeemers.count,
    redeemer_peaks: redeemers.peaks,
    redeemer_total_count: if k == 0 {
      0
    } else {
      n
    },
  }
  let total = if wrong_total {
    n + 1
  } else {
    n
  }
  let pending_hash = redeemer_item_proof_v1.initial_control_hash(
    redeemer_item_proof_v1.mode_data,
    k,
    total,
    bytearray.length(payload),
    bounded_item_v1.from_bytes(8, k, payload),
    -1,
    -1,
  )
  let work_witness_cbor = stage_zero_script_sources_work_witness(control)
  let pre = script_sources_state(source, 11, work_witness_cbor)
  let post = script_sources_state(
    source,
    12,
    vm_witness.encode_script_sources_redeemer_item_witness(
      ScriptSourcesControlV1 { ..control, redeemer_total_count: total },
      pending_hash,
    ),
  )
  StepPartsV1 {
    pre,
    transition: ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary: TransactionRedeemerItemBeginWitness {
      carriage: delivered.carriage,
    },
    control,
    delivered,
  }
}

/// Stage-one begin on redeemer `k` of `n`, read through the certificate.
fn stage_one_case(
  n: Int,
  k: Int,
  tier3: Bool,
  wrong_total: Bool,
  route_in_evidence: Bool,
) -> SpendCaseV1<stage_one_validator.SpendRedeemer> {
  let StepPartsV1 { pre, transition, auxiliary, delivered, .. } = stage_one_parts(
    n,
    k,
    tier3,
    wrong_total,
  )
  let post = transition.claimed_successor
  let auxiliary: Data = auxiliary
  let committed = if route_in_evidence {
    with_route_folded_in(auxiliary, route_of(delivered, True))
  } else {
    auxiliary
  }
  SpendCaseV1 {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(prepared_of(pre, post, transition, committed)),
      },
    ),
    redeemer: ct.Continue(
      stage_one_validator.VerifyRedeemer {
        input_index: 0,
        output_index: 0,
        transition,
        carriage: delivered.carriage,
        cert_ref_input_index: delivered.grammar_index,
      },
    ),
    tx: continue_tx(winning_resolution(), delivered.door.reference_inputs),
  }
}

fn run_stage_one(case: SpendCaseV1<stage_one_validator.SpendRedeemer>) -> Bool {
  stage_one_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    cfi.grammar_certificate_policy_id,
    case.datum,
    case.redeemer,
    fixture.own_out_ref(),
    case.tx,
  )
}

/// Redeemer 1,711 of 1,724: fifteen items past its waypoint.
test stage_one_begin_certified_redeemer_1711_of_1724_at_scale() {
  run_stage_one(stage_one_case(1724, 1711, True, False, False))
}

test stage_one_begin_certified_redeemer_1711_of_1724_fixture_only() {
  stage_one_fixture_only(stage_one_case(1724, 1711, True, False, False))
}

/// The walking reference machine (`script_sources_stage_one`) on the same
/// step: the existing route's answer, which the certified stage must equal.
fn stage_one_by_walking(n: Int, k: Int, wrong_total: Bool) -> Bool {
  let parts = stage_one_parts(n, k, False, wrong_total)
  early_stages.script_sources_stage_one(
    parts.pre,
    parts.transition,
    parts.auxiliary,
    parts.control,
    parts.delivered.door,
  )
}

/// On a small field the certified stage accepts exactly the successor the
/// walking reference machine accepts.
test stage_one_begin_routes_accept_the_same_successor() {
  and {
    stage_one_by_walking(2, 0, False),
    run_stage_one(stage_one_case(2, 0, False, False, False)),
    stage_one_by_walking(20, 17, False),
    run_stage_one(stage_one_case(20, 17, False, False, False)),
  }
}

test stage_one_begin_walking_route_refuses_a_wrong_successor() fail {
  stage_one_by_walking(2, 0, True)
}

test stage_one_begin_refuses_a_wrong_successor() fail {
  run_stage_one(stage_one_case(2, 0, False, True, False))
}

test stage_one_begin_refuses_the_certificate_index_in_its_evidence() fail {
  run_stage_one(stage_one_case(2, 0, False, False, True))
}

/// The walking reference machine over the same 1,724 redeemers: the cost the
/// certified stage no longer pays.
test stage_one_begin_walking_reference_redeemer_1711_of_1724_at_scale() {
  let parts = stage_one_parts(1724, 1711, True, False)
  early_stages.script_sources_stage_one(
    parts.pre,
    parts.transition,
    parts.auxiliary,
    parts.control,
    parts.delivered.door,
  )
}

/// The certificate is the stage's only route: without one the step is
/// refused.
test stage_one_begin_refuses_a_step_without_a_certificate() fail {
  let case = stage_one_case(2, 0, False, False, False)
  run_stage_one(
    SpendCaseV1 { ..case, tx: Transaction { ..case.tx, reference_inputs: [] } },
  )
}

// ---------------------------------------------------------------------------
// Script-sources middle yields: stage-four begin (field 2) and stage-six
// policy begin (field 5)
// ---------------------------------------------------------------------------

/// A transaction spending one dispatcher input whose prepared resolution
/// starts at `pre`, its action carrying `transition`, the auxiliary
/// `auxiliary` and the middle role `role`, beside `reference_inputs`.
fn dispatch(
  pre: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: Data,
  role: Int,
  reference_inputs: List<Input>,
) -> Transaction {
  let prepared = PreparedValidationResolutionStateV1 {
    version: 1,
    resolution: ValidationResolutionStateV1 {
      version: 1,
      pre_state: pre,
      operator_successor_hash: h_b,
      challenger_successor_hash: h_b,
    },
    evidence_hash: h_b,
  }
  let datum: Data = ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(prepared),
  }
  let zero: Data = 0
  let transition_data: Data = transition
  let role_data: Data = role
  let action = builtin.constr_data(
    0,
    [zero, zero, transition_data, auxiliary, role_data, zero],
  )
  let output_reference = OutputReference {
    transaction_id: h_b,
    output_index: 0,
  }
  Transaction {
    ..placeholder,
    inputs: [
      Input {
        output_reference,
        output: Output {
          address: address.from_script(dispatcher),
          value: assets.from_lovelace(2_000_000),
          datum: InlineDatum(datum),
          reference_script: None,
        },
      },
    ],
    reference_inputs,
    redeemers: [Pair(Spend(output_reference), builtin.constr_data(1, [action]))],
  }
}

type YieldCaseV1 {
  route: MachineFieldItemRouteV1,
  tx: Transaction,
}

fn yield_fixture_only(case: YieldCaseV1) -> Bool {
  let route: Data = case.route
  and {
    list.length(case.tx.reference_inputs) > 0,
    list.length(case.tx.redeemers) > 0,
    route != builtin.i_data(0),
  }
}

/// Stage-four begin on output `k` of `n`.
fn stage_four_case(
  n: Int,
  k: Int,
  tier3: Bool,
  certified: Bool,
  wrong_total: Bool,
) -> YieldCaseV1 {
  let payload = output_payload()
  let preimage = cfi.uniform_field(n, cfi.wrap(payload))
  let source = cfi.field_source(2, preimage)
  let delivered = delivery(source.tx_id, 2, preimage, tier3)
  let outputs = leaves(k)
  let control = ScriptSourcesControlV1 {
    ..script_sources_control(source, 4),
    output_count: outputs.count,
    output_peaks: outputs.peaks,
    output_total_count: if k == 0 {
      0
    } else {
      n
    },
  }
  let next = ScriptSourcesControlV1 {
    ..control,
    output_count: k + 1,
    output_peaks: validation_merkle_v1.append_leaf(
      k,
      outputs.peaks,
      script_proof_v1.output_item_leaf_hash(
        k,
        bytearray.length(payload),
        bounded_item_v1.from_bytes(2, k, payload),
      ),
    ),
    output_total_count: if wrong_total {
      n + 1
    } else {
      n
    },
  }
  let work_witness_cbor = stage_zero_script_sources_work_witness(control)
  let pre = script_sources_state(source, 20, work_witness_cbor)
  let transition = ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: script_sources_state(
      source,
      21,
      stage_zero_script_sources_work_witness(next),
    ),
  }
  let auxiliary: Data = TransactionRedeemerItemBeginWitness {
    carriage: delivered.carriage,
  }
  YieldCaseV1 {
    route: route_of(delivered, certified),
    tx: dispatch(pre, transition, auxiliary, 3, delivered.door.reference_inputs),
  }
}

fn run_stage_four(case: YieldCaseV1) -> Bool {
  middle_yields.stage_four_begin.withdraw(
    [dispatcher],
    fixture.test_certificate_policy_id,
    cfi.grammar_certificate_policy_id,
    case.route,
    address.Script(dispatcher),
    case.tx,
  )
}

test stage_four_begin_certified_output_760_of_761_at_scale() {
  run_stage_four(stage_four_case(761, 760, True, True, False))
}

test stage_four_begin_certified_output_760_of_761_fixture_only() {
  yield_fixture_only(stage_four_case(761, 760, True, True, False))
}

test stage_four_begin_walking_output_760_of_761_at_scale() {
  run_stage_four(stage_four_case(761, 760, True, False, False))
}

test stage_four_begin_routes_accept_the_same_successor() {
  and {
    run_stage_four(stage_four_case(3, 0, False, False, False)),
    run_stage_four(stage_four_case(3, 0, False, True, False)),
    run_stage_four(stage_four_case(20, 19, False, False, False)),
    run_stage_four(stage_four_case(20, 19, False, True, False)),
  }
}

test stage_four_begin_walking_route_refuses_a_wrong_successor() fail {
  run_stage_four(stage_four_case(3, 0, False, False, True))
}

test stage_four_begin_certified_route_refuses_a_wrong_successor() fail {
  run_stage_four(stage_four_case(3, 0, False, True, True))
}

/// Stage-six policy begin on mint entry `k` of `n`.
fn stage_six_parts(
  n: Int,
  k: Int,
  tier3: Bool,
  wrong_total: Bool,
) -> StepPartsV1 {
  let payload = mint_payload()
  let preimage = cfi.uniform_field(n, cfi.wrap(payload))
  let source = cfi.field_source(5, preimage)
  let delivered = delivery(source.tx_id, 5, preimage, tier3)
  let purposes = leaves(k)
  let assets_so_far = leaves(k)
  let fold = MintFoldControlV1 {
    ..vm_witness.empty_mint_fold_control(),
    policy_count: if k == 0 {
      -1
    } else {
      n
    },
    policy_cursor: k,
    previous_policy: if k == 0 {
      #""
    } else {
      cfi.repeat(#"00", 28)
    },
    asset_count: assets_so_far.count,
    asset_peaks: assets_so_far.peaks,
  }
  let control = ScriptSourcesControlV1 {
    ..script_sources_control(source, 6),
    purpose_count: purposes.count,
    purpose_peaks: purposes.peaks,
    mint_fold: fold,
  }
  let next = ScriptSourcesControlV1 {
    ..control,
    purpose_count: k + 1,
    purpose_peaks: validation_merkle_v1.append_leaf(
      k,
      purposes.peaks,
      script_proof_v1.purpose_leaf_hash(1, k, mint_policy, mint_policy),
    ),
    mint_fold: MintFoldControlV1 {
      ..fold,
      policy_count: if wrong_total {
        n + 1
      } else {
        n
      },
      active_policy: mint_policy,
      item_length: bytearray.length(payload),
      item_commitment: bounded_item_v1.from_bytes(5, k, payload),
      item_cursor: 32,
      assets_remaining: 1,
      policy_asset_cursor: 0,
      previous_asset: #"",
    },
  }
  let work_witness_cbor = stage_zero_script_sources_work_witness(control)
  let pre = script_sources_state(source, 30, work_witness_cbor)
  let transition = ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: script_sources_state(
      source,
      31,
      stage_zero_script_sources_work_witness(next),
    ),
  }
  StepPartsV1 {
    pre,
    transition,
    auxiliary: TransactionFieldChunkWitness {
      field_index: 5,
      item_index: k,
      carriage: delivered.carriage,
    },
    control,
    delivered,
  }
}

/// The stage-six yield's withdrawal on mint entry `k` of `n`, naming the
/// certified route or the walking one.
fn stage_six_case(
  n: Int,
  k: Int,
  tier3: Bool,
  certified: Bool,
  wrong_total: Bool,
) -> YieldCaseV1 {
  let StepPartsV1 { pre, transition, auxiliary, delivered, .. } = stage_six_parts(
    n,
    k,
    tier3,
    wrong_total,
  )
  let auxiliary: Data = auxiliary
  YieldCaseV1 {
    route: route_of(delivered, certified),
    tx: dispatch(pre, transition, auxiliary, 5, delivered.door.reference_inputs),
  }
}

/// The walking reference machine (`script_sources_stage_six`) on the same
/// step: the existing route's answer, which the certified yield must equal.
fn stage_six_by_walking(n: Int, k: Int, tier3: Bool, wrong_total: Bool) -> Bool {
  let parts = stage_six_parts(n, k, tier3, wrong_total)
  middle_stages.script_sources_stage_six(
    parts.pre,
    parts.transition,
    parts.auxiliary,
    parts.control,
    parts.delivered.door,
  )
}

fn run_stage_six(case: YieldCaseV1) -> Bool {
  middle_yields.stage_six_begin_policy.withdraw(
    [dispatcher],
    fixture.test_certificate_policy_id,
    cfi.grammar_certificate_policy_id,
    case.route,
    address.Script(dispatcher),
    case.tx,
  )
}

/// Mint entry 895 of 910: fifteen items past its waypoint.
test stage_six_begin_policy_certified_entry_895_of_910_at_scale() {
  run_stage_six(stage_six_case(910, 895, True, True, False))
}

test stage_six_begin_policy_certified_entry_895_of_910_fixture_only() {
  yield_fixture_only(stage_six_case(910, 895, True, True, False))
}

/// The walking reference machine over the same 910 entries: the cost the
/// certified yield no longer pays.
test stage_six_begin_policy_walking_reference_entry_895_of_910_at_scale() {
  stage_six_by_walking(910, 895, True, False)
}

/// On a small field the certified yield accepts exactly the successor the
/// walking reference machine accepts.
test stage_six_begin_policy_routes_accept_the_same_successor() {
  and {
    stage_six_by_walking(3, 0, False, False),
    run_stage_six(stage_six_case(3, 0, False, True, False)),
    stage_six_by_walking(20, 17, False, False),
    run_stage_six(stage_six_case(20, 17, False, True, False)),
  }
}

test stage_six_begin_policy_walking_route_refuses_a_wrong_successor() fail {
  stage_six_by_walking(3, 0, False, True)
}

/// The yield reads its policy item through the certificate alone: a
/// withdrawal naming the walking route is refused.
test stage_six_begin_policy_refuses_the_walking_route() fail {
  run_stage_six(stage_six_case(3, 0, False, False, False))
}

test stage_six_begin_policy_certified_route_refuses_a_wrong_successor() fail {
  run_stage_six(stage_six_case(3, 0, False, True, True))
}
