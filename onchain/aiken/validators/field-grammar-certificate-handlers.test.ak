//// Executable handler coverage for `validators/field-grammar-certificate.ak`
//// and the consumer reads of `midgard/field_grammar_certificate_v1`.
////
//// Every expected datum here is computed independently of the validator, from
//// the field's item lengths alone (`expected_certificate`): the item count,
//// each waypoint as a sum of enveloped item lengths, and the grammar
//// checkpoint's wire form and digest. The handlers require their output datum
//// byte for byte, so a passing honest vector is the proof that the on-chain
//// computation equals this one.
////
//// Fixture convention follows `field-preimage-certificate-handlers.test.ak`:
//// one honest builder per handler and one `fail` test per guard, each the
//// honest fixture with one thing changed. Cost rows are paired with a
//// `..._fixture_only` control that builds the same transaction without running
//// the handler.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{Value}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction,
}
use field_grammar_certificate as grammar_validator
use midgard/field_grammar_certificate_v1.{
  Complete, FieldGrammarCertificateV1, InProgress, bind_certified_field_grammar,
  certified_field_grammar_at, certified_field_item_at,
  certified_field_item_count, certified_field_item_extent,
  certified_field_waypoint_offset, field_grammar_certificate_asset_name,
  field_grammar_waypoint_interval,
}
use midgard/field_grammar_certification_v1.{
  Advance, FieldGrammarCertificateMintRedeemerV1,
  FieldGrammarCertificateSpendRedeemerV1, Open, Reclaim, Retire,
}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_witness_set_compact, encode_native_tx_body_compact,
  encode_native_tx_compact_v1, encode_native_tx_witness_set_compact,
  native_tx_id_for_version, validation_source_kind_v1, verify_tx_compact_cbor_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxCompact, NativeTxWitnessSetCompact,
  native_tx_version_v1,
}
use midgard/native_tx_carriage_v1.{
  field_preimage_certificate_v1, split_field_preimage,
}
use midgard/native_tx_field_access_v1.{
  Certified, FieldCarriageV1, Inline, Whole, encode_field_array_header,
  field_commitment, field_item_extent, field_preimage_certificate_asset_name,
}
use midgard/native_tx_machine_walk_v1.{
  certify_field_grammar, certify_field_grammar_batch,
  encode_field_grammar_checkpoint, open_field_grammar_certification,
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const zero_hash32: ByteArray =
  #"0000000000000000000000000000000000000000000000000000000000000000"

/// The field-preimage certificate policy, the validator's one parameter.
const preimage_certificate_policy: ByteArray =
  #"22222222222222222222222222222222222222222222222222222222"

/// The grammar certificate script hash: policy id and payment credential.
const grammar_policy: ByteArray =
  #"33333333333333333333333333333333333333333333333333333333"

const owner_hash: ByteArray =
  #"66666666666666666666666666666666666666666666666666666666"

const other_owner_hash: ByteArray =
  #"77777777777777777777777777777777777777777777777777777777"

const certificate_utxo_id: ByteArray =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const carriage_utxo_id: ByteArray =
  #"5555555555555555555555555555555555555555555555555555555555555555"

/// ASCII `MidgardFieldGrammarCheckpointV1`.
const grammar_checkpoint_domain: ByteArray =
  #"4d6964676172644669656c644772616d6d6172436865636b706f696e745631"

const deposit: Int = 5000000

/// One field of one transaction: its index, its preimage and its item count.
/// The wrapper offset of item `k` is `header_len + k · stride` when `stride`
/// is positive, and element `k` of `offsets` (which ends with the preimage
/// length) otherwise.
type Field {
  field_index: Int,
  preimage: ByteArray,
  count: Int,
  stride: Int,
  offsets: List<Int>,
}

fn repeat_bytes(bytes: ByteArray, n: Int) -> ByteArray {
  if n <= 0 {
    #""
  } else if n % 2 == 0 {
    let half = repeat_bytes(bytes, n / 2)
    bytearray.concat(half, half)
  } else {
    bytearray.concat(bytes, repeat_bytes(bytes, n - 1))
  }
}

/// `n` identical enveloped items. `wrapped` is one item with its §5.1 wrapper.
fn uniform_field(field_index: Int, n: Int, wrapped: ByteArray) -> Field {
  Field {
    field_index,
    preimage: bytearray.concat(
      encode_field_array_header(n),
      repeat_bytes(wrapped, n),
    ),
    count: n,
    stride: bytearray.length(wrapped),
    offsets: [],
  }
}

/// 43-byte enveloped items: `58 29` and 41 payload bytes.
fn outputs_field(n: Int) -> Field {
  uniform_field(2, n, bytearray.concat(#"5829", repeat_bytes(#"a1", 41)))
}

/// 9-byte enveloped items: `48` and 8 payload bytes.
fn scripts_field(n: Int) -> Field {
  uniform_field(6, n, bytearray.concat(#"48", repeat_bytes(#"a2", 8)))
}

fn wrap(payload: ByteArray) -> ByteArray {
  let length = bytearray.length(payload)
  if length <= 23 {
    bytearray.push(payload, 64 + length)
  } else if length <= 255 {
    bytearray.concat(#"58", bytearray.push(payload, length))
  } else {
    bytearray.concat(
      #"59",
      bytearray.concat(bytearray.from_int_big_endian(length, 2), payload),
    )
  }
}

/// Items of varying width, so every offset below is a sum, not a product:
/// payload `k` is `1 + (7k mod 40)` bytes, crossing the one- and two-byte
/// wrapper forms.
fn varied_field(field_index: Int, n: Int) -> Field {
  let wrapped = list.map(
    list.range(0, n - 1),
    fn(k) { wrap(repeat_bytes(#"c3", 1 + 7 * k % 40)) },
  )
  let header = encode_field_array_header(n)
  let Pair(_, offsets) = list.foldl(
    wrapped,
    Pair(bytearray.length(header), []),
    fn(item, Pair(at, reversed)) {
      Pair(at + bytearray.length(item), [at, ..reversed])
    },
  )
  let preimage = list.foldl(
    wrapped,
    header,
    fn(item, joined) { bytearray.concat(joined, item) },
  )
  Field {
    field_index,
    preimage,
    count: n,
    stride: 0,
    offsets: list.reverse([bytearray.length(preimage), ..offsets]),
  }
}

fn item_count(field: Field) -> Int {
  field.count
}

fn offset_of(field: Field, k: Int) -> Int {
  if field.stride > 0 {
    bytearray.length(field.preimage) - ( field.count - k ) * field.stride
  } else {
    expect Some(offset) = list.at(field.offsets, k)
    offset
  }
}

fn witness_set(field: Field) -> NativeTxWitnessSetCompact {
  let slot = fn(index) {
    if index == field.field_index {
      field_commitment(field.preimage)
    } else {
      zero_hash32
    }
  }
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash: slot(7),
    script_tx_wits_hash: slot(6),
    redeemer_tx_wits_hash: slot(8),
  }
}

fn body(field: Field) -> NativeTxBodyCompact {
  let slot = fn(index) {
    if index == field.field_index {
      field_commitment(field.preimage)
    } else {
      zero_hash32
    }
  }
  NativeTxBodyCompact {
    spend_inputs_hash: slot(0),
    reference_inputs_hash: slot(1),
    outputs_hash: slot(2),
    fee: 0,
    validity_interval_start: 0,
    validity_interval_end: 0,
    required_observers_hash: slot(3),
    required_signers_hash: slot(4),
    mint_hash: slot(5),
    script_integrity_hash: zero_hash32,
    auxiliary_data_hash: zero_hash32,
    network_id: 255,
  }
}

fn witness_set_cbor(field: Field) -> ByteArray {
  encode_native_tx_witness_set_compact(witness_set(field))
}

fn compact_cbor(field: Field) -> ByteArray {
  encode_native_tx_compact_v1(
    NativeTxCompact {
      body: body(field),
      witness_set_hash: blake2b_256(witness_set_cbor(field)),
      validity_code: 0,
    },
  )
}

fn tx_id(field: Field) -> ByteArray {
  native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(body(field)),
  )
}

/// Tier 1 carries the preimage in the redeemer. Tier 3 puts the §8.6
/// field-preimage certificate at reference input 0 and its chunks after it.
fn carriage(field: Field, tier3: Bool) -> Pair<FieldCarriageV1, List<Input>> {
  if tier3 {
    let chunks = split_field_preimage(field.preimage)
    Pair(
      Certified {
        cert_ref_input_index: 0,
        chunk_ref_input_indices: list.range(1, list.length(chunks)),
      },
      [preimage_certificate_input(field), ..chunk_inputs(chunks)],
    )
  } else {
    Pair(Inline { preimage: field.preimage }, [])
  }
}

fn preimage_certificate_input(field: Field) -> Input {
  let datum: Data = field_preimage_certificate_v1(
    owner_hash,
    tx_id(field),
    field.field_index,
    field.preimage,
  )
  Input {
    output_reference: OutputReference {
      transaction_id: carriage_utxo_id,
      output_index: 0,
    },
    output: Output {
      address: address.from_script(preimage_certificate_policy),
      value: assets.from_lovelace(2000000)
        |> assets.add(
            preimage_certificate_policy,
            field_preimage_certificate_asset_name,
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn chunk_inputs(chunks: List<ByteArray>) -> List<Input> {
  list.indexed_map(
    chunks,
    fn(index, chunk) {
      Input {
        output_reference: OutputReference {
          transaction_id: carriage_utxo_id,
          output_index: index + 1,
        },
        output: Output {
          address: address.from_verification_key(owner_hash),
          value: assets.from_lovelace(2000000),
          datum: InlineDatum(builtin.b_data(chunk)),
          reference_script: None,
        },
      }
    },
  )
}

// ---------------------------------------------------------------------------
// The independent mirror of the certificate computation
// ---------------------------------------------------------------------------

fn u24(value: Int) -> ByteArray {
  bytearray.concat(#"43", bytearray.from_int_big_endian(value, 3))
}

/// The grammar checkpoint's 87-byte wire form after `next` items.
fn checkpoint_bytes(field: Field, next: Int) -> ByteArray {
  #"875820"
    |> bytearray.concat(tx_id(field))
    |> bytearray.concat(#"41")
    |> bytearray.concat(bytearray.from_int_big_endian(field.field_index, 1))
    |> bytearray.concat(#"5820")
    |> bytearray.concat(field_commitment(field.preimage))
    |> bytearray.concat(u24(bytearray.length(field.preimage)))
    |> bytearray.concat(u24(item_count(field)))
    |> bytearray.concat(u24(next))
    |> bytearray.concat(u24(offset_of(field, next)))
}

fn checkpoint_hash(field: Field, next: Int) -> ByteArray {
  blake2b_256(
    bytearray.concat(grammar_checkpoint_domain, checkpoint_bytes(field, next)),
  )
}

/// Waypoint `k` is the wrapper offset of item `16k`, for every such item
/// before `next`.
fn waypoints(field: Field, next: Int) -> ByteArray {
  list.foldl(
    list.range(
      0,
      ( next + field_grammar_waypoint_interval - 1 ) / field_grammar_waypoint_interval - 1,
    ),
    #"",
    fn(k, joined) {
      bytearray.concat(
        joined,
        bytearray.from_int_big_endian(
          offset_of(field, k * field_grammar_waypoint_interval),
          2,
        ),
      )
    },
  )
}

/// The certificate after `next` items have been certified.
fn expected_certificate(
  field: Field,
  owner: ByteArray,
  next: Int,
) -> FieldGrammarCertificateV1 {
  if next == item_count(field) {
    Complete {
      owner,
      tx_id: tx_id(field),
      field_index: field.field_index,
      field_hash: field_commitment(field.preimage),
      total_length: bytearray.length(field.preimage),
      item_count: next,
      waypoints: waypoints(field, next),
    }
  } else {
    InProgress {
      owner,
      tx_id: tx_id(field),
      field_index: field.field_index,
      field_hash: field_commitment(field.preimage),
      total_length: bytearray.length(field.preimage),
      grammar_checkpoint_hash: checkpoint_hash(field, next),
      waypoints: waypoints(field, next),
    }
  }
}

/// A `Complete` datum's fields, so a test can change one of them.
type CompleteParts {
  owner: ByteArray,
  tx_id: ByteArray,
  field_index: Int,
  field_hash: ByteArray,
  total_length: Int,
  item_count: Int,
  waypoints: ByteArray,
}

fn complete_parts(field: Field) -> CompleteParts {
  CompleteParts {
    owner: owner_hash,
    tx_id: tx_id(field),
    field_index: field.field_index,
    field_hash: field_commitment(field.preimage),
    total_length: bytearray.length(field.preimage),
    item_count: item_count(field),
    waypoints: waypoints(field, item_count(field)),
  }
}

fn complete(parts: CompleteParts) -> FieldGrammarCertificateV1 {
  Complete {
    owner: parts.owner,
    tx_id: parts.tx_id,
    field_index: parts.field_index,
    field_hash: parts.field_hash,
    total_length: parts.total_length,
    item_count: parts.item_count,
    waypoints: parts.waypoints,
  }
}

/// An `InProgress` datum after `next` items, with its waypoints replaced.
fn in_progress_with_waypoints(
  field: Field,
  next: Int,
  replaced: ByteArray,
) -> FieldGrammarCertificateV1 {
  InProgress {
    owner: owner_hash,
    tx_id: tx_id(field),
    field_index: field.field_index,
    field_hash: field_commitment(field.preimage),
    total_length: bytearray.length(field.preimage),
    grammar_checkpoint_hash: checkpoint_hash(field, next),
    waypoints: replaced,
  }
}

// ---------------------------------------------------------------------------
// Transactions
// ---------------------------------------------------------------------------

fn certificate_output(
  certificate: FieldGrammarCertificateV1,
  value: Value,
) -> Output {
  let datum: Data = certificate
  Output {
    address: address.from_script(grammar_policy),
    value,
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn certificate_value(quantity: Int) -> Value {
  assets.from_lovelace(deposit)
    |> assets.add(
        grammar_policy,
        field_grammar_certificate_asset_name,
        quantity,
      )
}

fn certificate_reference(index: Int) -> OutputReference {
  OutputReference { transaction_id: certificate_utxo_id, output_index: index }
}

fn certificate_input(
  certificate: FieldGrammarCertificateV1,
  index: Int,
) -> Input {
  Input {
    output_reference: certificate_reference(index),
    output: certificate_output(certificate, certificate_value(1)),
  }
}

fn mint_one() -> Value {
  assets.from_asset(grammar_policy, field_grammar_certificate_asset_name, 1)
}

fn open_redeemer(
  field: Field,
  carriage: FieldCarriageV1,
  batch: Int,
) -> FieldGrammarCertificateMintRedeemerV1 {
  Open {
    source_kind: 0,
    compact_cbor: compact_cbor(field),
    witness_set_compact_cbor: witness_set_cbor(field),
    carriage,
    batch,
    output_index: 0,
  }
}

fn advance_redeemer(
  field: Field,
  carriage: FieldCarriageV1,
  from: Int,
  batch: Int,
) -> FieldGrammarCertificateSpendRedeemerV1 {
  Advance {
    source_kind: 0,
    compact_cbor: compact_cbor(field),
    witness_set_compact_cbor: witness_set_cbor(field),
    checkpoint: checkpoint_bytes(field, from),
    carriage,
    batch,
    output_index: 0,
  }
}

fn open_tx(
  outputs: List<Output>,
  mint: Value,
  reference_inputs: List<Input>,
) -> Transaction {
  Transaction { ..transaction.placeholder, outputs, mint, reference_inputs }
}

fn advance_tx(
  inputs: List<Input>,
  outputs: List<Output>,
  reference_inputs: List<Input>,
) -> Transaction {
  Transaction { ..transaction.placeholder, inputs, outputs, reference_inputs }
}

fn run_mint(
  redeemer: FieldGrammarCertificateMintRedeemerV1,
  tx: Transaction,
) -> Bool {
  grammar_validator.field_grammar_certificate.mint(
    preimage_certificate_policy,
    redeemer,
    grammar_policy,
    tx,
  )
}

fn run_spend(
  datum: Option<FieldGrammarCertificateV1>,
  redeemer: FieldGrammarCertificateSpendRedeemerV1,
  tx: Transaction,
) -> Bool {
  grammar_validator.field_grammar_certificate.spend(
    preimage_certificate_policy,
    datum,
    redeemer,
    certificate_reference(0),
    tx,
  )
}

/// One honest `Open` of `batch` items.
type OpenCase {
  redeemer: FieldGrammarCertificateMintRedeemerV1,
  tx: Transaction,
}

fn honest_open(field: Field, tier3: Bool, batch: Int) -> OpenCase {
  let Pair(carriage, reference_inputs) = carriage(field, tier3)
  let reached = min(batch, item_count(field))
  OpenCase {
    redeemer: open_redeemer(field, carriage, batch),
    tx: open_tx(
      [
        certificate_output(
          expected_certificate(field, owner_hash, reached),
          certificate_value(1),
        ),
      ],
      mint_one(),
      reference_inputs,
    ),
  }
}

fn open_with_output(field: Field, batch: Int, output: Output) -> Bool {
  let Pair(carriage, reference_inputs) = carriage(field, False)
  run_mint(
    open_redeemer(field, carriage, batch),
    open_tx([output], mint_one(), reference_inputs),
  )
}

/// One honest `Advance` of `batch` items from item `from`.
type AdvanceCase {
  datum: FieldGrammarCertificateV1,
  redeemer: FieldGrammarCertificateSpendRedeemerV1,
  tx: Transaction,
}

fn honest_advance(
  field: Field,
  tier3: Bool,
  from: Int,
  batch: Int,
) -> AdvanceCase {
  let Pair(carriage, reference_inputs) = carriage(field, tier3)
  let datum = expected_certificate(field, owner_hash, from)
  let reached = min(from + batch, item_count(field))
  AdvanceCase {
    datum,
    redeemer: advance_redeemer(field, carriage, from, batch),
    tx: advance_tx(
      [certificate_input(datum, 0)],
      [
        certificate_output(
          expected_certificate(field, owner_hash, reached),
          certificate_value(1),
        ),
      ],
      reference_inputs,
    ),
  }
}

fn run_advance_case(case: AdvanceCase) -> Bool {
  run_spend(Some(case.datum), case.redeemer, case.tx)
}

fn advance_with_outputs(
  field: Field,
  from: Int,
  batch: Int,
  outputs: List<Output>,
) -> Bool {
  let case = honest_advance(field, False, from, batch)
  run_spend(Some(case.datum), case.redeemer, Transaction { ..case.tx, outputs })
}

fn min(a: Int, b: Int) -> Int {
  if a < b {
    a
  } else {
    b
  }
}

/// Runs `Open` then `Advance`s over `schedule` (batch sizes), returning the
/// final certificate after checking every transaction.
fn certify_by_schedule(
  field: Field,
  tier3: Bool,
  owner: ByteArray,
  schedule: List<Int>,
) -> FieldGrammarCertificateV1 {
  expect [first, ..rest] = schedule
  let Pair(carriage, reference_inputs) = carriage(field, tier3)
  let opened = expected_certificate(field, owner, min(first, item_count(field)))
  expect
    run_mint(
      open_redeemer(field, carriage, first),
      open_tx(
        [certificate_output(opened, certificate_value(1))],
        mint_one(),
        reference_inputs,
      ),
    )
  let Pair(final, _) = list.foldl(
    rest,
    Pair(opened, first),
    fn(batch, Pair(datum, from)) {
      let reached = min(from + batch, item_count(field))
      let next = expected_certificate(field, owner, reached)
      expect
        run_spend(
          Some(datum),
          advance_redeemer(field, carriage, from, batch),
          advance_tx(
            [certificate_input(datum, 0)],
            [certificate_output(next, certificate_value(1))],
            reference_inputs,
          ),
        )
      Pair(next, reached)
    },
  )
  final
}

fn without_owner(
  certificate: FieldGrammarCertificateV1,
) -> FieldGrammarCertificateV1 {
  when certificate is {
    InProgress {
      tx_id,
      field_index,
      field_hash,
      total_length,
      grammar_checkpoint_hash,
      waypoints,
      ..
    } ->
      InProgress {
        owner: #"",
        tx_id,
        field_index,
        field_hash,
        total_length,
        grammar_checkpoint_hash,
        waypoints,
      }
    Complete {
      tx_id,
      field_index,
      field_hash,
      total_length,
      item_count,
      waypoints,
      ..
    } ->
      Complete {
        owner: #"",
        tx_id,
        field_index,
        field_hash,
        total_length,
        item_count,
        waypoints,
      }
  }
}

// ---------------------------------------------------------------------------
// Open
// ---------------------------------------------------------------------------

test field_grammar_certificate_handlers_open_completes_a_small_tier1_field() {
  let case = honest_open(outputs_field(40), False, 40)
  run_mint(case.redeemer, case.tx)
}

test field_grammar_certificate_handlers_open_completes_a_one_item_tier1_field() {
  let field = outputs_field(1)
  let case = honest_open(field, False, 1)
  and {
    run_mint(case.redeemer, case.tx),
    expected_certificate(field, owner_hash, 1) == Complete {
      owner: owner_hash,
      tx_id: tx_id(field),
      field_index: 2,
      field_hash: field_commitment(field.preimage),
      total_length: 44,
      item_count: 1,
      waypoints: #"0001",
    },
  }
}

test field_grammar_certificate_handlers_open_leaves_a_longer_field_in_progress() {
  let case = honest_open(varied_field(5, 70), False, 32)
  run_mint(case.redeemer, case.tx)
}

test field_grammar_certificate_handlers_open_certifies_a_witness_field() {
  let case = honest_open(scripts_field(100), False, 100)
  run_mint(case.redeemer, case.tx)
}

test field_grammar_certificate_handlers_open_certifies_a_redeemer_witness_field() {
  let case = honest_open(varied_field(8, 50), False, 50)
  run_mint(case.redeemer, case.tx)
}

/// A fixed-stride field has an arithmetic count and no grammar to certify.
test field_grammar_certificate_handlers_open_rejects_a_fixed_stride_field() fail {
  let field = uniform_field(
    0,
    3,
    bytearray.concat(#"5826", repeat_bytes(#"b4", 38)),
  )
  let case = honest_open(field, False, 3)
  run_mint(case.redeemer, case.tx)
}

test field_grammar_certificate_handlers_open_rejects_a_complete_datum_before_the_end() fail {
  let field = outputs_field(40)
  let early = complete(
    CompleteParts {
      ..complete_parts(field),
      item_count: 32,
      waypoints: waypoints(field, 32),
    },
  )
  open_with_output(field, 32, certificate_output(early, certificate_value(1)))
}

test field_grammar_certificate_handlers_open_rejects_a_forged_waypoint() fail {
  let field = varied_field(2, 40)
  let honest = complete_parts(field)
  let forged = complete(
    CompleteParts {
      ..honest,
      waypoints: bytearray.concat(
        bytearray.take(honest.waypoints, 2),
        bytearray.from_int_big_endian(offset_of(field, 16) + 1, 2),
      )
        |> bytearray.concat(bytearray.drop(honest.waypoints, 4)),
    },
  )
  open_with_output(field, 40, certificate_output(forged, certificate_value(1)))
}

/// The offsets of items 15, 30, … are real wrapper offsets of the field, but
/// not the protocol's grid.
test field_grammar_certificate_handlers_open_rejects_waypoints_off_the_grid() fail {
  let field = varied_field(2, 40)
  let off_grid = complete(
    CompleteParts {
      ..complete_parts(field),
      waypoints: bytearray.from_int_big_endian(offset_of(field, 0), 2)
        |> bytearray.concat(
            bytearray.from_int_big_endian(offset_of(field, 15), 2),
          )
        |> bytearray.concat(
            bytearray.from_int_big_endian(offset_of(field, 30), 2),
          ),
    },
  )
  open_with_output(
    field,
    40,
    certificate_output(off_grid, certificate_value(1)),
  )
}

test field_grammar_certificate_handlers_open_rejects_a_wrong_field_hash() fail {
  let field = outputs_field(20)
  open_with_output(
    field,
    20,
    certificate_output(
      complete(
        CompleteParts { ..complete_parts(field), field_hash: zero_hash32 },
      ),
      certificate_value(1),
    ),
  )
}

test field_grammar_certificate_handlers_open_rejects_a_wrong_tx_id() fail {
  let field = outputs_field(20)
  open_with_output(
    field,
    20,
    certificate_output(
      complete(CompleteParts { ..complete_parts(field), tx_id: zero_hash32 }),
      certificate_value(1),
    ),
  )
}

test field_grammar_certificate_handlers_open_rejects_a_wrong_total_length() fail {
  let field = outputs_field(20)
  let honest = complete_parts(field)
  open_with_output(
    field,
    20,
    certificate_output(
      complete(
        CompleteParts { ..honest, total_length: honest.total_length + 1 },
      ),
      certificate_value(1),
    ),
  )
}

/// Field 5 of this transaction commits to the empty-slot hash, so the door
/// refuses the preimage the datum's field index would have it read.
test field_grammar_certificate_handlers_open_rejects_a_wrong_field_index() fail {
  let field = outputs_field(20)
  open_with_output(
    field,
    20,
    certificate_output(
      complete(CompleteParts { ..complete_parts(field), field_index: 5 }),
      certificate_value(1),
    ),
  )
}

test field_grammar_certificate_handlers_open_rejects_a_wrong_item_count() fail {
  let field = outputs_field(20)
  open_with_output(
    field,
    20,
    certificate_output(
      complete(CompleteParts { ..complete_parts(field), item_count: 21 }),
      certificate_value(1),
    ),
  )
}

fn malformed_field(preimage: ByteArray) -> Field {
  Field { field_index: 2, preimage, count: 1, stride: 0, offsets: [0, 0] }
}

/// The items are canonical and the header declares their count, but one byte
/// follows the last item.
test field_grammar_certificate_handlers_open_rejects_trailing_bytes() fail {
  let honest = outputs_field(20)
  let field = malformed_field(bytearray.concat(honest.preimage, #"40"))
  let Pair(carriage, _) = carriage(field, False)
  run_mint(
    open_redeemer(field, carriage, 20),
    open_tx(
      [
        certificate_output(
          Complete {
            owner: owner_hash,
            tx_id: tx_id(field),
            field_index: 2,
            field_hash: field_commitment(field.preimage),
            total_length: bytearray.length(field.preimage),
            item_count: 20,
            waypoints: waypoints(honest, 20),
          },
          certificate_value(1),
        ),
      ],
      mint_one(),
      [],
    ),
  )
}

/// The header declares 21 items over 20.
test field_grammar_certificate_handlers_open_rejects_a_header_count_above_the_items() fail {
  let honest = outputs_field(20)
  let field = malformed_field(
    bytearray.concat(
      encode_field_array_header(21),
      bytearray.drop(honest.preimage, 1),
    ),
  )
  let Pair(carriage, _) = carriage(field, False)
  run_mint(
    open_redeemer(field, carriage, 21),
    open_tx(
      [
        certificate_output(
          Complete {
            owner: owner_hash,
            tx_id: tx_id(field),
            field_index: 2,
            field_hash: field_commitment(field.preimage),
            total_length: bytearray.length(field.preimage),
            item_count: 21,
            waypoints: waypoints(honest, 20),
          },
          certificate_value(1),
        ),
      ],
      mint_one(),
      [],
    ),
  )
}

/// The header declares 19 items over 20.
test field_grammar_certificate_handlers_open_rejects_a_header_count_below_the_items() fail {
  let honest = outputs_field(20)
  let field = malformed_field(
    bytearray.concat(
      encode_field_array_header(19),
      bytearray.drop(honest.preimage, 1),
    ),
  )
  let Pair(carriage, _) = carriage(field, False)
  run_mint(
    open_redeemer(field, carriage, 19),
    open_tx(
      [
        certificate_output(
          Complete {
            owner: owner_hash,
            tx_id: tx_id(field),
            field_index: 2,
            field_hash: field_commitment(field.preimage),
            total_length: bytearray.length(field.preimage),
            item_count: 19,
            waypoints: waypoints(honest, 19),
          },
          certificate_value(1),
        ),
      ],
      mint_one(),
      [],
    ),
  )
}

test field_grammar_certificate_handlers_open_rejects_a_missing_token() fail {
  let field = outputs_field(20)
  open_with_output(
    field,
    20,
    certificate_output(
      expected_certificate(field, owner_hash, 20),
      assets.from_lovelace(deposit),
    ),
  )
}

test field_grammar_certificate_handlers_open_rejects_two_tokens() fail {
  let field = outputs_field(20)
  let Pair(carriage, reference_inputs) = carriage(field, False)
  run_mint(
    open_redeemer(field, carriage, 20),
    open_tx(
      [
        certificate_output(
          expected_certificate(field, owner_hash, 20),
          certificate_value(2),
        ),
      ],
      assets.from_asset(grammar_policy, field_grammar_certificate_asset_name, 2),
      reference_inputs,
    ),
  )
}

/// A second certificate output carrying the second unit of a two-unit mint.
test field_grammar_certificate_handlers_open_rejects_a_second_certificate_in_one_transaction() fail {
  let field = outputs_field(20)
  let Pair(carriage, reference_inputs) = carriage(field, False)
  let output = certificate_output(
    expected_certificate(field, owner_hash, 20),
    certificate_value(1),
  )
  run_mint(
    open_redeemer(field, carriage, 20),
    open_tx(
      [output, output],
      assets.from_asset(grammar_policy, field_grammar_certificate_asset_name, 2),
      reference_inputs,
    ),
  )
}

test field_grammar_certificate_handlers_open_rejects_a_foreign_asset_name() fail {
  let field = outputs_field(20)
  let Pair(carriage, reference_inputs) = carriage(field, False)
  run_mint(
    open_redeemer(field, carriage, 20),
    open_tx(
      [
        certificate_output(
          expected_certificate(field, owner_hash, 20),
          certificate_value(1),
        ),
      ],
      assets.from_asset(grammar_policy, #"deadbeef", 1),
      reference_inputs,
    ),
  )
}

test field_grammar_certificate_handlers_open_rejects_a_certificate_at_a_key_address() fail {
  let field = outputs_field(20)
  open_with_output(
    field,
    20,
    Output {
      ..certificate_output(
        expected_certificate(field, owner_hash, 20),
        certificate_value(1),
      ),
      address: address.from_verification_key(owner_hash),
    },
  )
}

test field_grammar_certificate_handlers_open_rejects_a_stake_credential() fail {
  let field = outputs_field(20)
  open_with_output(
    field,
    20,
    Output {
      ..certificate_output(
        expected_certificate(field, owner_hash, 20),
        certificate_value(1),
      ),
      address: address.from_script(grammar_policy)
        |> address.with_delegation_key(owner_hash),
    },
  )
}

test field_grammar_certificate_handlers_open_rejects_a_reference_script() fail {
  let field = outputs_field(20)
  open_with_output(
    field,
    20,
    Output {
      ..certificate_output(
        expected_certificate(field, owner_hash, 20),
        certificate_value(1),
      ),
      reference_script: Some(grammar_policy),
    },
  )
}

/// Fewer than one waypoint interval, and the field is not finished.
test field_grammar_certificate_handlers_open_rejects_a_batch_below_the_interval() fail {
  let field = outputs_field(40)
  open_with_output(
    field,
    15,
    certificate_output(
      expected_certificate(field, owner_hash, 15),
      certificate_value(1),
    ),
  )
}

test field_grammar_certificate_handlers_open_rejects_a_short_owner() fail {
  let field = outputs_field(20)
  open_with_output(
    field,
    20,
    certificate_output(
      expected_certificate(field, #"66", 20),
      certificate_value(1),
    ),
  )
}

test field_grammar_certificate_handlers_open_certifies_a_tier3_field() {
  let case = honest_open(outputs_field(400), True, 64)
  run_mint(case.redeemer, case.tx)
}

/// One byte of the last chunk changed: the chunks no longer concatenate to the
/// bytes the field-preimage certificate and the field commitment name.
test field_grammar_certificate_handlers_open_rejects_tier3_chunks_that_disagree_with_the_preimage_certificate() fail {
  let field = outputs_field(400)
  let case = honest_open(field, True, 64)
  expect [chunk_0, chunk_1] = split_field_preimage(field.preimage)
  let tampered = bytearray.concat(
    bytearray.take(chunk_1, bytearray.length(chunk_1) - 1),
    #"ff",
  )
  run_mint(
    case.redeemer,
    Transaction {
      ..case.tx,
      reference_inputs: [
        preimage_certificate_input(field),
        ..chunk_inputs([chunk_0, tampered])
      ],
    },
  )
}

test field_grammar_certificate_handlers_retire_permits_a_burn() {
  run_mint(
    Retire,
    open_tx(
      [],
      assets.from_asset(
        grammar_policy,
        field_grammar_certificate_asset_name,
        -1,
      ),
      [],
    ),
  )
}

test field_grammar_certificate_handlers_retire_rejects_a_mint() fail {
  run_mint(Retire, open_tx([], mint_one(), []))
}

// ---------------------------------------------------------------------------
// Advance
// ---------------------------------------------------------------------------

test field_grammar_certificate_handlers_advance_continues_a_walk() {
  run_advance_case(honest_advance(varied_field(2, 70), False, 32, 16))
}

test field_grammar_certificate_handlers_advance_completes_a_walk() {
  run_advance_case(honest_advance(varied_field(2, 70), False, 32, 38))
}

/// A batch larger than what is left stops at the end.
test field_grammar_certificate_handlers_advance_completes_with_an_oversized_batch() {
  run_advance_case(honest_advance(varied_field(2, 70), False, 64, 100))
}

/// A batch that ends off the grid, and a following batch that starts there.
test field_grammar_certificate_handlers_advance_resumes_between_waypoints() {
  and {
    run_advance_case(honest_advance(varied_field(6, 70), False, 0, 21)),
    run_advance_case(honest_advance(varied_field(6, 70), False, 21, 17)),
  }
}

test field_grammar_certificate_handlers_advance_certifies_under_tier3() {
  run_advance_case(honest_advance(outputs_field(400), True, 64, 64))
}

/// Tier is a carriage detail: a walk opened under tier 3 advances under tier
/// 1 over the same bytes.
test field_grammar_certificate_handlers_advance_mixes_tiers() {
  run_advance_case(honest_advance(outputs_field(400), False, 64, 64))
}

/// Two owners, two batch schedules, one field: the certificates differ only
/// in `owner`.
test field_grammar_certificate_handlers_independent_builds_are_identical_apart_from_owner() {
  let field = varied_field(2, 90)
  let first = certify_by_schedule(
    field,
    False,
    owner_hash,
    [16, 16, 16, 16, 16, 10],
  )
  let second = certify_by_schedule(field, False, other_owner_hash, [37, 20, 33])
  and {
    first != second,
    without_owner(first) == without_owner(second),
    first == expected_certificate(field, owner_hash, 90),
  }
}

test field_grammar_certificate_handlers_advance_rejects_no_progress() fail {
  run_advance_case(honest_advance(varied_field(2, 70), False, 32, 0))
}

test field_grammar_certificate_handlers_advance_rejects_a_batch_below_the_interval() fail {
  run_advance_case(honest_advance(varied_field(2, 70), False, 32, 15))
}

test field_grammar_certificate_handlers_advance_rejects_a_complete_certificate() fail {
  let field = varied_field(2, 70)
  let complete = expected_certificate(field, owner_hash, 70)
  let case = honest_advance(field, False, 32, 38)
  run_spend(
    Some(complete),
    case.redeemer,
    Transaction {
      ..case.tx,
      inputs: [certificate_input(complete, 0)],
      outputs: [certificate_output(complete, certificate_value(1))],
    },
  )
}

test field_grammar_certificate_handlers_advance_rejects_a_checkpoint_the_datum_did_not_commit() fail {
  let field = varied_field(2, 70)
  let case = honest_advance(field, False, 32, 16)
  let Pair(carriage, _) = carriage(field, False)
  run_spend(
    Some(case.datum),
    Advance {
      source_kind: 0,
      compact_cbor: compact_cbor(field),
      witness_set_compact_cbor: witness_set_cbor(field),
      checkpoint: checkpoint_bytes(field, 33),
      carriage,
      batch: 16,
      output_index: 0,
    },
    case.tx,
  )
}

test field_grammar_certificate_handlers_advance_rejects_a_forged_waypoint() fail {
  let field = varied_field(2, 70)
  let forged = in_progress_with_waypoints(
    field,
    48,
    bytearray.concat(
      bytearray.take(waypoints(field, 48), 4),
      bytearray.from_int_big_endian(offset_of(field, 32) + 1, 2),
    ),
  )
  advance_with_outputs(
    field,
    32,
    16,
    [certificate_output(forged, certificate_value(1))],
  )
}

test field_grammar_certificate_handlers_advance_rejects_a_complete_datum_before_the_end() fail {
  let field = varied_field(2, 70)
  advance_with_outputs(
    field,
    32,
    16,
    [
      certificate_output(
        complete(
          CompleteParts {
            ..complete_parts(field),
            item_count: 48,
            waypoints: waypoints(field, 48),
          },
        ),
        certificate_value(1),
      ),
    ],
  )
}

test field_grammar_certificate_handlers_advance_rejects_a_changed_owner() fail {
  let field = varied_field(2, 70)
  advance_with_outputs(
    field,
    32,
    16,
    [
      certificate_output(
        expected_certificate(field, other_owner_hash, 48),
        certificate_value(1),
      ),
    ],
  )
}

test field_grammar_certificate_handlers_advance_rejects_a_missing_token() fail {
  let field = varied_field(2, 70)
  advance_with_outputs(
    field,
    32,
    16,
    [
      certificate_output(
        expected_certificate(field, owner_hash, 48),
        assets.from_lovelace(deposit),
      ),
      Output {
        address: address.from_verification_key(owner_hash),
        value: certificate_value(1),
        datum: NoDatum,
        reference_script: None,
      },
    ],
  )
}

/// Two identical certificates spent together, both advanced into one output:
/// the second token would leave over a datum nobody computed.
test field_grammar_certificate_handlers_advance_rejects_two_certificates_moving_together() fail {
  let field = varied_field(2, 70)
  let case = honest_advance(field, False, 32, 16)
  let next = certificate_output(
    expected_certificate(field, owner_hash, 48),
    certificate_value(1),
  )
  run_spend(
    Some(case.datum),
    case.redeemer,
    Transaction {
      ..case.tx,
      inputs: [
        certificate_input(case.datum, 0),
        certificate_input(case.datum, 1),
      ],
      outputs: [
        next,
        Output {
          address: address.from_verification_key(owner_hash),
          value: certificate_value(1),
          datum: NoDatum,
          reference_script: None,
        },
      ],
    },
  )
}

test field_grammar_certificate_handlers_advance_rejects_a_mint_of_the_policy() fail {
  let case = honest_advance(varied_field(2, 70), False, 32, 16)
  run_spend(
    Some(case.datum),
    case.redeemer,
    Transaction { ..case.tx, mint: mint_one() },
  )
}

test field_grammar_certificate_handlers_advance_rejects_a_smaller_deposit() fail {
  let field = varied_field(2, 70)
  advance_with_outputs(
    field,
    32,
    16,
    [
      certificate_output(
        expected_certificate(field, owner_hash, 48),
        assets.from_lovelace(deposit - 1)
          |> assets.add(grammar_policy, field_grammar_certificate_asset_name, 1),
      ),
    ],
  )
}

test field_grammar_certificate_handlers_advance_accepts_a_larger_deposit() {
  let field = varied_field(2, 70)
  advance_with_outputs(
    field,
    32,
    16,
    [
      certificate_output(
        expected_certificate(field, owner_hash, 48),
        assets.from_lovelace(deposit + 1)
          |> assets.add(grammar_policy, field_grammar_certificate_asset_name, 1),
      ),
    ],
  )
}

test field_grammar_certificate_handlers_advance_rejects_tier3_chunks_that_disagree_with_the_preimage_certificate() fail {
  let field = outputs_field(400)
  let case = honest_advance(field, True, 64, 64)
  expect [chunk_0, chunk_1] = split_field_preimage(field.preimage)
  let tampered = bytearray.concat(#"ff", bytearray.drop(chunk_0, 1))
  run_spend(
    Some(case.datum),
    case.redeemer,
    Transaction {
      ..case.tx,
      reference_inputs: [
        preimage_certificate_input(field),
        ..chunk_inputs([tampered, chunk_1])
      ],
    },
  )
}

// ---------------------------------------------------------------------------
// Reclaim
// ---------------------------------------------------------------------------

fn reclaim_tx(
  certificate: FieldGrammarCertificateV1,
  outputs: List<Output>,
  signers: List<ByteArray>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [certificate_input(certificate, 0)],
    outputs,
    mint: assets.from_asset(
      grammar_policy,
      field_grammar_certificate_asset_name,
      -1,
    ),
    extra_signatories: signers,
  }
}

fn reclaimable() -> FieldGrammarCertificateV1 {
  expected_certificate(varied_field(2, 40), owner_hash, 40)
}

test field_grammar_certificate_handlers_reclaim_burns_the_token_for_the_owner() {
  run_spend(
    Some(reclaimable()),
    Reclaim,
    reclaim_tx(reclaimable(), [], [owner_hash]),
  )
}

test field_grammar_certificate_handlers_reclaim_burns_an_in_progress_certificate() {
  let in_progress = expected_certificate(varied_field(2, 40), owner_hash, 16)
  run_spend(
    Some(in_progress),
    Reclaim,
    reclaim_tx(in_progress, [], [owner_hash]),
  )
}

test field_grammar_certificate_handlers_reclaim_rejects_a_non_owner() fail {
  run_spend(
    Some(reclaimable()),
    Reclaim,
    reclaim_tx(reclaimable(), [], [other_owner_hash]),
  )
}

test field_grammar_certificate_handlers_reclaim_rejects_a_surviving_token() fail {
  run_spend(
    Some(reclaimable()),
    Reclaim,
    Transaction {
      ..reclaim_tx(
        reclaimable(),
        [certificate_output(reclaimable(), certificate_value(1))],
        [owner_hash],
      ),
      mint: assets.zero,
    },
  )
}

test field_grammar_certificate_handlers_spend_rejects_a_missing_datum() fail {
  run_spend(None, Reclaim, reclaim_tx(reclaimable(), [], [owner_hash]))
}

// ---------------------------------------------------------------------------
// Consumer reads
// ---------------------------------------------------------------------------

fn consumer_field() -> Field {
  varied_field(2, 90)
}

fn consumer_view(field: Field) {
  Whole { bytes: field.preimage, count: item_count(field), stride: 0 }
}

fn consumer_certified(field: Field) {
  bind_certified_field_grammar(
    expected_certificate(field, owner_hash, item_count(field)),
    tx_id(field),
    field.field_index,
    field_commitment(field.preimage),
    bytearray.length(field.preimage),
  )
}

fn extent_agrees(field: Field, index: Int) -> Bool {
  let view = consumer_view(field)
  certified_field_item_extent(view, consumer_certified(field), index) == field_item_extent(
    view,
    index,
  )
}

test field_grammar_certificate_handlers_consumer_reads_the_count() {
  certified_field_item_count(consumer_certified(consumer_field())) == 90
}

test field_grammar_certificate_handlers_consumer_reads_the_first_item() {
  let field = consumer_field()
  and {
    extent_agrees(field, 0),
    certified_field_item_extent(
      consumer_view(field),
      consumer_certified(field),
      0,
    ) == Pair(offset_of(field, 0) + 1, 1),
  }
}

test field_grammar_certificate_handlers_consumer_reads_waypoint_items() {
  let field = consumer_field()
  let certified = consumer_certified(field)
  and {
    extent_agrees(field, 16),
    extent_agrees(field, 32),
    extent_agrees(field, 80),
    certified_field_waypoint_offset(certified, 5) == offset_of(field, 80),
  }
}

test field_grammar_certificate_handlers_consumer_reads_items_between_waypoints() {
  let field = consumer_field()
  and {
    extent_agrees(field, 1),
    extent_agrees(field, 17),
    extent_agrees(field, 31),
    extent_agrees(field, 47),
  }
}

test field_grammar_certificate_handlers_consumer_reads_the_last_item() {
  let field = consumer_field()
  and {
    extent_agrees(field, 89),
    certified_field_item_at(consumer_view(field), consumer_certified(field), 89) == repeat_bytes(
      #"c3",
      1 + 7 * 89 % 40,
    ),
  }
}

test field_grammar_certificate_handlers_consumer_refuses_an_index_past_the_count() fail {
  let field = consumer_field()
  certified_field_item_extent(
    consumer_view(field),
    consumer_certified(field),
    90,
  ) == Pair(0, 0)
}

test field_grammar_certificate_handlers_consumer_refuses_a_view_of_another_length() fail {
  let field = consumer_field()
  let other = varied_field(2, 91)
  certified_field_item_extent(
    consumer_view(other),
    consumer_certified(field),
    0,
  ) == Pair(0, 0)
}

test field_grammar_certificate_handlers_consumer_refuses_an_in_progress_certificate() fail {
  let field = consumer_field()
  expect _ = bind_certified_field_grammar(
    expected_certificate(field, owner_hash, 32),
    tx_id(field),
    2,
    field_commitment(field.preimage),
    bytearray.length(field.preimage),
  )
  True
}

test field_grammar_certificate_handlers_consumer_refuses_a_foreign_field_hash() fail {
  let field = consumer_field()
  expect _ = bind_certified_field_grammar(
    expected_certificate(field, owner_hash, 90),
    tx_id(field),
    2,
    zero_hash32,
    bytearray.length(field.preimage),
  )
  True
}

test field_grammar_certificate_handlers_consumer_refuses_a_foreign_field_index() fail {
  let field = consumer_field()
  expect _ = bind_certified_field_grammar(
    expected_certificate(field, owner_hash, 90),
    tx_id(field),
    5,
    field_commitment(field.preimage),
    bytearray.length(field.preimage),
  )
  True
}

test field_grammar_certificate_handlers_consumer_refuses_a_foreign_tx_id() fail {
  let field = consumer_field()
  expect _ = bind_certified_field_grammar(
    expected_certificate(field, owner_hash, 90),
    zero_hash32,
    2,
    field_commitment(field.preimage),
    bytearray.length(field.preimage),
  )
  True
}

test field_grammar_certificate_handlers_consumer_refuses_a_foreign_total_length() fail {
  let field = consumer_field()
  expect _ = bind_certified_field_grammar(
    expected_certificate(field, owner_hash, 90),
    tx_id(field),
    2,
    field_commitment(field.preimage),
    bytearray.length(field.preimage) + 1,
  )
  True
}

fn reference_certificate(value: Value, at_script: Bool) -> Input {
  let field = consumer_field()
  let output = certificate_output(
    expected_certificate(field, owner_hash, item_count(field)),
    value,
  )
  Input {
    output_reference: certificate_reference(0),
    output: if at_script {
      output
    } else {
      Output { ..output, address: address.from_verification_key(owner_hash) }
    },
  }
}

fn certified_from_reference(input: Input) {
  let field = consumer_field()
  certified_field_grammar_at(
    [input],
    0,
    grammar_policy,
    tx_id(field),
    2,
    field_commitment(field.preimage),
    bytearray.length(field.preimage),
  )
}

test field_grammar_certificate_handlers_consumer_finds_the_reference_input() {
  certified_field_item_count(
    certified_from_reference(reference_certificate(certificate_value(1), True)),
  ) == 90
}

test field_grammar_certificate_handlers_consumer_refuses_a_reference_input_without_the_token() fail {
  certified_field_item_count(
    certified_from_reference(
      reference_certificate(assets.from_lovelace(deposit), True),
    ),
  ) == 90
}

test field_grammar_certificate_handlers_consumer_refuses_a_reference_input_off_the_script() fail {
  certified_field_item_count(
    certified_from_reference(reference_certificate(certificate_value(1), False)),
  ) == 90
}

// ---------------------------------------------------------------------------
// Cost
// ---------------------------------------------------------------------------
//
// Batch sizes a builder uses: `wide_batch` items in general, and
// `narrow_batch` when every item of the batch has a one-byte wrapper (payloads
// of at most 23 bytes, the cheapest header to read). Each row is one
// transaction at the largest field its tier carries, paired with a control
// that builds the same transaction and does not run the handler.

const wide_batch: Int = 224

const narrow_batch: Int = 384

/// 761 items of 43 bytes: 32,726 bytes, tier 3.
fn tier3_outputs() -> Field {
  outputs_field(761)
}

/// 3,640 items of 9 bytes: 32,763 bytes, tier 3.
fn tier3_scripts() -> Field {
  scripts_field(3640)
}

/// 333 items of 43 bytes: 14,322 bytes, the largest such tier-1 preimage.
fn tier1_outputs() -> Field {
  outputs_field(333)
}

/// 1,592 items of 9 bytes: 14,331 bytes, the largest such tier-1 preimage.
fn tier1_scripts() -> Field {
  scripts_field(1592)
}

fn open_fixture_only(case: OpenCase) -> Bool {
  list.length(case.tx.outputs) == 1
}

fn advance_fixture_only(case: AdvanceCase) -> Bool {
  list.length(case.tx.inputs) == 1
}

test field_grammar_certificate_handlers_fixtures_sit_at_their_sizes() {
  and {
    bytearray.length(tier3_outputs().preimage) == 32726,
    bytearray.length(tier3_scripts().preimage) == 32763,
    bytearray.length(tier1_outputs().preimage) == 14322,
    bytearray.length(tier1_scripts().preimage) == 14331,
    list.length(split_field_preimage(tier3_outputs().preimage)) == 3,
    list.length(split_field_preimage(tier3_scripts().preimage)) == 3,
  }
}

test field_grammar_certificate_handlers_open_outputs_tier3_full_batch_fixture_only() {
  open_fixture_only(honest_open(tier3_outputs(), True, wide_batch))
}

test field_grammar_certificate_handlers_open_outputs_tier3_full_batch_cost() {
  let case = honest_open(tier3_outputs(), True, wide_batch)
  run_mint(case.redeemer, case.tx)
}

test field_grammar_certificate_handlers_advance_outputs_tier3_final_full_batch_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_outputs(), True, 761 - wide_batch, wide_batch),
  )
}

/// The completing batch: the largest datum the field produces.
test field_grammar_certificate_handlers_advance_outputs_tier3_final_full_batch_cost() {
  run_advance_case(
    honest_advance(tier3_outputs(), True, 761 - wide_batch, wide_batch),
  )
}

test field_grammar_certificate_handlers_open_scripts_tier3_full_batch_fixture_only() {
  open_fixture_only(honest_open(tier3_scripts(), True, narrow_batch))
}

test field_grammar_certificate_handlers_open_scripts_tier3_full_batch_cost() {
  let case = honest_open(tier3_scripts(), True, narrow_batch)
  run_mint(case.redeemer, case.tx)
}

test field_grammar_certificate_handlers_advance_scripts_tier3_final_full_batch_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_scripts(), True, 3640 - narrow_batch, narrow_batch),
  )
}

/// The completing batch: the largest datum the field produces.
test field_grammar_certificate_handlers_advance_scripts_tier3_final_full_batch_cost() {
  run_advance_case(
    honest_advance(tier3_scripts(), True, 3640 - narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_open_outputs_tier1_full_batch_fixture_only() {
  open_fixture_only(honest_open(tier1_outputs(), False, wide_batch))
}

test field_grammar_certificate_handlers_open_outputs_tier1_full_batch_cost() {
  let case = honest_open(tier1_outputs(), False, wide_batch)
  run_mint(case.redeemer, case.tx)
}

test field_grammar_certificate_handlers_advance_outputs_tier1_final_full_batch_fixture_only() {
  advance_fixture_only(
    honest_advance(tier1_outputs(), False, 333 - wide_batch, wide_batch),
  )
}

/// The completing batch: the largest datum the field produces.
test field_grammar_certificate_handlers_advance_outputs_tier1_final_full_batch_cost() {
  run_advance_case(
    honest_advance(tier1_outputs(), False, 333 - wide_batch, wide_batch),
  )
}

test field_grammar_certificate_handlers_open_scripts_tier1_full_batch_fixture_only() {
  open_fixture_only(honest_open(tier1_scripts(), False, narrow_batch))
}

test field_grammar_certificate_handlers_open_scripts_tier1_full_batch_cost() {
  let case = honest_open(tier1_scripts(), False, narrow_batch)
  run_mint(case.redeemer, case.tx)
}

test field_grammar_certificate_handlers_advance_scripts_tier1_final_full_batch_fixture_only() {
  advance_fixture_only(
    honest_advance(tier1_scripts(), False, 1592 - narrow_batch, narrow_batch),
  )
}

/// The completing batch: the largest datum the field produces.
test field_grammar_certificate_handlers_advance_scripts_tier1_final_full_batch_cost() {
  run_advance_case(
    honest_advance(tier1_scripts(), False, 1592 - narrow_batch, narrow_batch),
  )
}

fn reclaim_case() -> Transaction {
  reclaim_tx(reclaimable(), [], [owner_hash])
}

test field_grammar_certificate_handlers_reclaim_fixture_only() {
  let tx = reclaim_case()
  and {
    list.length(tx.inputs) == 1,
    tx.extra_signatories == [owner_hash],
  }
}

test field_grammar_certificate_handlers_reclaim_cost() {
  run_spend(Some(reclaimable()), Reclaim, reclaim_case())
}

// ---------------------------------------------------------------------------
// Whole certifications at the largest tier-3 fields
// ---------------------------------------------------------------------------

fn batches(count: Int, batch: Int) -> List<Int> {
  list.repeat(batch, ( count + batch - 1 ) / batch)
}

/// 761 outputs of 43 bytes certify in four transactions.
test field_grammar_certificate_handlers_certifies_761_outputs_under_tier3_in_four_transactions() {
  let field = tier3_outputs()
  let schedule = batches(761, wide_batch)
  and {
    list.length(schedule) == 4,
    certify_by_schedule(field, True, owner_hash, schedule) == expected_certificate(
      field,
      owner_hash,
      761,
    ),
  }
}

/// 3,640 scripts of 9 bytes certify in ten transactions.
test field_grammar_certificate_handlers_certifies_3640_scripts_under_tier3_in_ten_transactions() {
  let field = tier3_scripts()
  let schedule = batches(3640, narrow_batch)
  and {
    list.length(schedule) == 10,
    certify_by_schedule(field, True, owner_hash, schedule) == expected_certificate(
      field,
      owner_hash,
      3640,
    ),
  }
}

test field_grammar_certificate_handlers_outputs_tier3_transaction_1_fixture_only() {
  open_fixture_only(honest_open(tier3_outputs(), True, wide_batch))
}

test field_grammar_certificate_handlers_outputs_tier3_transaction_1_cost() {
  let case = honest_open(tier3_outputs(), True, wide_batch)
  run_mint(case.redeemer, case.tx)
}

test field_grammar_certificate_handlers_outputs_tier3_transaction_2_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_outputs(), True, 1 * wide_batch, wide_batch),
  )
}

test field_grammar_certificate_handlers_outputs_tier3_transaction_2_cost() {
  run_advance_case(
    honest_advance(tier3_outputs(), True, 1 * wide_batch, wide_batch),
  )
}

test field_grammar_certificate_handlers_outputs_tier3_transaction_3_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_outputs(), True, 2 * wide_batch, wide_batch),
  )
}

test field_grammar_certificate_handlers_outputs_tier3_transaction_3_cost() {
  run_advance_case(
    honest_advance(tier3_outputs(), True, 2 * wide_batch, wide_batch),
  )
}

test field_grammar_certificate_handlers_outputs_tier3_transaction_4_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_outputs(), True, 3 * wide_batch, wide_batch),
  )
}

test field_grammar_certificate_handlers_outputs_tier3_transaction_4_cost() {
  run_advance_case(
    honest_advance(tier3_outputs(), True, 3 * wide_batch, wide_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_1_fixture_only() {
  open_fixture_only(honest_open(tier3_scripts(), True, narrow_batch))
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_1_cost() {
  let case = honest_open(tier3_scripts(), True, narrow_batch)
  run_mint(case.redeemer, case.tx)
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_2_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_scripts(), True, 1 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_2_cost() {
  run_advance_case(
    honest_advance(tier3_scripts(), True, 1 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_3_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_scripts(), True, 2 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_3_cost() {
  run_advance_case(
    honest_advance(tier3_scripts(), True, 2 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_4_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_scripts(), True, 3 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_4_cost() {
  run_advance_case(
    honest_advance(tier3_scripts(), True, 3 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_5_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_scripts(), True, 4 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_5_cost() {
  run_advance_case(
    honest_advance(tier3_scripts(), True, 4 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_6_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_scripts(), True, 5 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_6_cost() {
  run_advance_case(
    honest_advance(tier3_scripts(), True, 5 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_7_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_scripts(), True, 6 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_7_cost() {
  run_advance_case(
    honest_advance(tier3_scripts(), True, 6 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_8_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_scripts(), True, 7 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_8_cost() {
  run_advance_case(
    honest_advance(tier3_scripts(), True, 7 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_9_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_scripts(), True, 8 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_9_cost() {
  run_advance_case(
    honest_advance(tier3_scripts(), True, 8 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_10_fixture_only() {
  advance_fixture_only(
    honest_advance(tier3_scripts(), True, 9 * narrow_batch, narrow_batch),
  )
}

test field_grammar_certificate_handlers_scripts_tier3_transaction_10_cost() {
  run_advance_case(
    honest_advance(tier3_scripts(), True, 9 * narrow_batch, narrow_batch),
  )
}

// ---------------------------------------------------------------------------
// Golden vectors shared with the off-chain mirror
// ---------------------------------------------------------------------------
//
// `demo/midgard-fault-proofs` recomputes these datums from the preimage of
// `varied_field(2, 40)`, the transaction id and the owner alone; both sides
// pin the same digests, so the two computations cannot drift apart.

const golden_tx_id: ByteArray =
  #"752b97ec14d1474f7eb9e94550874b87f5de218887a8a15c6fa21cafcf46f62f"

const golden_field_hash: ByteArray =
  #"bf0c0a2fd31be6b03dfefea796a476a46c60f84b61927c9e47c7c8a12f00e877"

/// The grammar checkpoint after 16 of the 40 items.
const golden_checkpoint_hash: ByteArray =
  #"dc00c1d7362c137bbe4822f55dbe3a4c287c5e1a113ad30d565833229d69c242"

/// `blake2b_256(serialise_data(datum))` of the certificate after 16 items.
const golden_in_progress_digest: ByteArray =
  #"a5e89f141e62d1f052c6c5c3ebc3570e60957f118fe5c3ff6d29025845d46a5c"

/// `blake2b_256(serialise_data(datum))` of the complete certificate.
const golden_complete_digest: ByteArray =
  #"722115e3e22a2b498e6128e9813951c5cfc2ec2babd51085598f8b35850debf5"

test field_grammar_certificate_handlers_datums_match_the_golden_vectors() {
  let field = varied_field(2, 40)
  let opened = expected_certificate(field, owner_hash, 16)
  let certified = certify_by_schedule(field, False, owner_hash, [16, 24])
  and {
    tx_id(field) == golden_tx_id,
    field_commitment(field.preimage) == golden_field_hash,
    checkpoint_hash(field, 16) == golden_checkpoint_hash,
    blake2b_256(builtin.serialise_data(opened)) == golden_in_progress_digest,
    blake2b_256(builtin.serialise_data(certified)) == golden_complete_digest,
  }
}

// ---------------------------------------------------------------------------
// The batch walk is the per-item walk
// ---------------------------------------------------------------------------
//
// The validator walks with `certify_field_grammar_batch`, which carries the
// position as two integers and rebuilds the checkpoint once. Each test below
// runs both walks from the same opened checkpoint and demands the same wire
// bytes after every split of the field into two batches, or that both fail.

fn open_inline(field: Field) {
  open_field_grammar_certification(
    verify_tx_compact_cbor_v1(
      validation_source_kind_v1(0),
      tx_id(field),
      compact_cbor(field),
    ),
    decode_native_tx_witness_set_compact(witness_set_cbor(field)),
    field.field_index,
    Inline { preimage: field.preimage },
    [],
    preimage_certificate_policy,
  )
}

fn walks_agree(field: Field, first: Int, second: Int) -> Bool {
  let Pair(view, start) = open_inline(field)
  let item_walk = certify_field_grammar(
    view,
    certify_field_grammar(view, start, first),
    second,
  )
  let batch_walk = certify_field_grammar_batch(
    view,
    certify_field_grammar_batch(view, start, first),
    second,
  )
  encode_field_grammar_checkpoint(item_walk) == encode_field_grammar_checkpoint(
    batch_walk,
  )
}

test field_grammar_certificate_handlers_grammar_batch_walk_agrees_with_the_item_walk_on_varied_items() {
  let field = varied_field(2, 40)
  list.all(
    [
      Pair(0, 40), Pair(1, 39), Pair(15, 7), Pair(16, 24), Pair(17, 100),
      Pair(39, 1), Pair(40, 3), Pair(0, 0),
    ],
    fn(split) { walks_agree(field, split.1st, split.2nd) },
  )
}

test field_grammar_certificate_handlers_grammar_batch_walk_agrees_with_the_item_walk_on_uniform_items() {
  and {
    walks_agree(outputs_field(50), 33, 17),
    walks_agree(scripts_field(70), 64, 64),
    walks_agree(varied_field(6, 1), 1, 0),
  }
}

fn batch_walk_completes(field: Field, budget: Int) -> Bool {
  let Pair(view, start) = open_inline(field)
  let reached = certify_field_grammar_batch(view, start, budget)
  encode_field_grammar_checkpoint(reached) != #""
}

fn item_walk_completes(field: Field, budget: Int) -> Bool {
  let Pair(view, start) = open_inline(field)
  let reached = certify_field_grammar(view, start, budget)
  encode_field_grammar_checkpoint(reached) != #""
}

fn trailing_byte_field() -> Field {
  malformed_field(bytearray.concat(outputs_field(20).preimage, #"40"))
}

fn recounted_field(declared: Int) -> Field {
  malformed_field(
    bytearray.concat(
      encode_field_array_header(declared),
      bytearray.drop(outputs_field(20).preimage, 1),
    ),
  )
}

test field_grammar_certificate_handlers_grammar_item_walk_rejects_trailing_bytes() fail {
  item_walk_completes(trailing_byte_field(), 20)
}

test field_grammar_certificate_handlers_grammar_batch_walk_rejects_trailing_bytes() fail {
  batch_walk_completes(trailing_byte_field(), 20)
}

test field_grammar_certificate_handlers_grammar_item_walk_rejects_a_header_count_below_the_items() fail {
  item_walk_completes(recounted_field(19), 19)
}

test field_grammar_certificate_handlers_grammar_batch_walk_rejects_a_header_count_below_the_items() fail {
  batch_walk_completes(recounted_field(19), 19)
}

test field_grammar_certificate_handlers_grammar_item_walk_rejects_a_header_count_above_the_items() fail {
  item_walk_completes(recounted_field(21), 21)
}

test field_grammar_certificate_handlers_grammar_batch_walk_rejects_a_header_count_above_the_items() fail {
  batch_walk_completes(recounted_field(21), 21)
}

test field_grammar_certificate_handlers_grammar_item_walk_rejects_a_negative_budget() fail {
  item_walk_completes(outputs_field(20), -1)
}

test field_grammar_certificate_handlers_grammar_batch_walk_rejects_a_negative_budget() fail {
  batch_walk_completes(outputs_field(20), -1)
}
