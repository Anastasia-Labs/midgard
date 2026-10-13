use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/utils
use midgard/fraud_proofs/native_tx/codec.{
  decode_canonical_int_at, decode_definite_array_header_at,
}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_field_preimage_lengths_v1, forced_tx_proof_commitment_v1,
  verify_forced_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_canonical_bytes_at, decode_canonical_map_header_at,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxFieldPreimageLengthsV1, NativeTxWitnessSetCompact,
  VerifiedTxSourceView,
}
use midgard/ledger_output_commitment_v1.{max_cardano_value_cbor_bytes}
use midgard/ledger_state.{
  ForcedInclusionTxV1, ForcedTxProofSourceV1, TxOrderEventV1, TxOrderId,
  TxOrderPayloadV1, unsafe_event_to_key_value_pair,
}
use midgard/mint_order.{mint_asset_is_valid, mint_policy_is_valid}
use midgard/native_tx_field_access_v1.{
  FieldCarriageV1, FieldViewV1, Whole, address_witness_stride,
  empty_field_commitment, field_header_len, hash28_stride, spend_input_stride,
}
use midgard/rejection_reason_v1.{OperatorVerdictV1}
use midgard/script_language_views_v1.{empty_script_integrity_hash}
use midgard/transition_trace
use midgard/user_events

pub type Datum =
  user_events.OptimisticDatum<TxOrderEventV1>

/// The tx-order minting policy's redeemer — the shared user-event mint
/// redeemer plus the §8 carriage of the order's material.
///
/// **Why a wrapper and not two more fields on `user_events.MintRedeemer`.**
/// That type is the deposit, withdrawal and witness policies' redeemer too, and
/// none of them has material to carry. Widening it would move three unrelated
/// frozen wire surfaces to serve one policy, so the carriage rides beside it
/// here instead and `event` is passed through untouched.
///
/// `material_carriage` is **positional over the order's non-empty fields, in
/// ascending field index** — not `(field_index, carriage)` pairs. §4 removed
/// field-index domain separation, so a redeemer-supplied index would have to be
/// checked against the slot it claims before it could be used, and the walk in
/// `verify_order_material` already knows that slot: it reads the nine
/// commitments in order and consumes one entry at each field whose commitment is
/// not `empty_field_commitment`. The index is therefore *derived* rather than
/// supplied, and the vector is required to be exhausted exactly, so neither a
/// missing nor a spare entry can spell the same order twice (§6.1).
///
/// A burn carries no material and must carry no vector — see
/// `material_carriage_matches_event`.
pub type MintRedeemer {
  event: user_events.MintRedeemer,
  material_carriage: List<FieldCarriageV1>,
}

/// Whether the redeemer's carriage vector is admissible for the event it rides
/// beside, *before* the vector's contents are looked at.
///
/// A burn reads no material, so it must carry **no** vector rather than carry
/// one nothing looks at: an unread wire field is where a second spelling of the
/// same transaction hides (§6.1). An `AuthenticateEvent` is unconstrained here —
/// its vector is exactly what `verify_order_material` goes on to consume and
/// exhaust, and pre-judging its length here would duplicate that walk's own
/// exhaustion rule in a second place.
///
/// This lives in the library rather than inline in the mint so it has a test
/// harness: the validator module has none, and a rule with no refuting negative
/// is a rule that can be deleted without anything going red.
pub fn material_carriage_matches_event(
  event: user_events.MintRedeemer,
  material_carriage: List<FieldCarriageV1>,
) -> Bool {
  when event is {
    user_events.BurnEventNFT { .. } -> list.is_empty(material_carriage)
    user_events.AuthenticateEvent { .. } -> True
  }
}

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  burn_redeemer_index: Int,
  membership_proof: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
  inclusion_proof_script_withdraw_redeemer_index: Int,
  validity_override: OperatorVerdictV1,
}

fn field_length(
  lengths: NativeTxFieldPreimageLengthsV1,
  field_index: Int,
) -> Int {
  when field_index is {
    0 -> lengths.spend_inputs
    1 -> lengths.reference_inputs
    2 -> lengths.outputs
    3 -> lengths.required_observers
    4 -> lengths.required_signers
    5 -> lengths.mint
    6 -> lengths.script_witnesses
    7 -> lengths.address_witnesses
    8 -> lengths.redeemers
    _ -> 0
  }
}

/// Everything the payload's own proof source establishes, before a single byte
/// of carriage is looked at: the transaction the order binds itself to, the
/// witness set paired with it, the nine committed field hashes in §2.5 order,
/// and the declared preimage lengths.
///
/// A record rather than a tuple because the door needs `verified` and
/// `witness_set` too — the compact structures are what §4's positional identity
/// extracts each field's expected commitment from — and a four-way `Pair` nest
/// at every recursion step would hide which of them the walk is reading.
type OrderMaterialDirectoryV1 {
  verified: VerifiedTxSourceView,
  witness_set: NativeTxWitnessSetCompact,
  commitments: List<ByteArray>,
  lengths: NativeTxFieldPreimageLengthsV1,
}

fn material_directory(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  source: ForcedTxProofSourceV1,
) -> OrderMaterialDirectoryV1 {
  let Pair(verified, witness_set) = verify_forced_tx_proof_source_v1(
    transaction_id,
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
  )
  expect
    forced_tx_proof_commitment_v1(
      source.compact_cbor,
      source.witness_set_compact_cbor,
      source.field_preimage_lengths_cbor,
    ) == transaction_commitment
  let lengths = decode_native_tx_field_preimage_lengths_v1(
    source.field_preimage_lengths_cbor,
  )
  let body = verified.tx_compact.body
  OrderMaterialDirectoryV1 {
    verified,
    witness_set,
    commitments: [
      body.spend_inputs_hash,
      body.reference_inputs_hash,
      body.outputs_hash,
      body.required_observers_hash,
      body.required_signers_hash,
      body.mint_hash,
      witness_set.script_tx_wits_hash,
      witness_set.addr_tx_wits_hash,
      witness_set.redeemer_tx_wits_hash,
    ],
    lengths,
  }
}

/// Walks §2.5's nine slots in order, opening the §8 door at each slot that
/// carries material and consuming one carriage entry there.
///
/// Returns the nine slots' authenticated views in §2.5 order, `None` for an
/// empty slot. The redeemer's vector must be **exhausted exactly**; that and
/// every other refusal aborts, because every refusal is a claim that turned out
/// to be false about bytes already in hand (§7.3, abort-never-clamp).
fn authenticate_material_fields(
  directory: OrderMaterialDirectoryV1,
  field_index: Int,
  material_carriage: List<FieldCarriageV1>,
  reference_inputs: List<Input>,
  field_preimage_certificate_policy_id: PolicyId,
) -> List<Option<FieldViewV1>> {
  if field_index >= native_tx_field_access_v1.field_count {
    // Exhaustion. A spare entry is not harmless: it would let two distinct
    // redeemers authenticate the same order, and one of them would name
    // reference inputs nothing in this walk ever read.
    expect list.is_empty(material_carriage)
    []
  } else {
    let commitment = directory.commitments |> list.expect_at(field_index)
    let declared_length = field_length(directory.lengths, field_index)
    if commitment == empty_field_commitment {
      // §5.1's empty field is the one-byte header `80`, so its declared length
      // is 1 and there is nothing to carry. Checked rather than skipped: the
      // declared vector is inside `transaction_commitment`, so a length that
      // disagrees with an empty commitment is an order that contradicts itself.
      expect declared_length == 1
      [
        None,
        ..authenticate_material_fields(
          directory,
          field_index + 1,
          material_carriage,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
      ]
    } else {
      // A field with material must be carried. The vector running short here is
      // the missing half of exhaustion above.
      expect [field_carriage, ..rest] = material_carriage
      let view = native_tx_field_access_v1.authenticated_whole_field_view(
        directory.verified,
        directory.witness_set,
        field_index,
        field_carriage,
        reference_inputs,
        field_preimage_certificate_policy_id,
      )
      // The declared §2.4 length and the authenticated preimage's own length are
      // two independently committed statements about the same field, and §4's
      // flat hash does not collapse them — the same disagreement the validation
      // machine renders as `reject_field_preimage_size`. Here it is the order
      // creator's own payload contradicting itself, so it fails closed instead.
      expect
        native_tx_field_access_v1.field_total_length(view) == declared_length
      [
        Some(view),
        ..authenticate_material_fields(
          directory,
          field_index + 1,
          rest,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
      ]
    }
  }
}

/// Verifies that the L1 forced-inclusion order's material is available under
/// `docs/spec/midgard-tx.md` §8 before the order NFT is authenticated.
///
/// For each of §2.5's nine slots whose committed hash is not
/// `empty_field_commitment`, the prover-chosen `FieldCarriageV1` at the matching
/// position of `material_carriage` is authenticated through the §8.8 field-access
/// door against that commitment. An order whose nine fields are all empty
/// consumes no carriage and is still admitted; an order carrying material is
/// admitted exactly when every field with material is carried and authenticates.
///
/// ### What "available" means here, and why this mint is the only reader
///
/// Durable availability is **L1 history plus digest addressing**, not a live
/// UTxO. This mint is the only on-chain reader of the material: the spend path
/// verifies settlement `forced_transactions_root` membership and never touches a
/// preimage, and a later dispute re-carries the bytes digest-checked under §8
/// (§8.7's mandatory content addressing — carriage is identified by digests,
/// never by `OutputReference`; the owner ruling's own text cites §8.5 for this,
/// which is the Custody subsection, and the citation is corrected here). So
/// once this mint has hashed the bytes against the commitment, they are permanent
/// L1 history, which is what the operator and the node's ingestion walk read.
/// That is why the order datum carries no carriage identity and why reclaim is an
/// ordinary key spend at any time after the mint, with no reclaim contract and no
/// time gate — §8.5's min-Ada-by-key-spend custody plus §8.7's
/// owner-discretionary, self-healing cleanup rule, both as written.
///
/// ### Tiers, and where the threshold is
///
/// All three §8 tiers are admissible, and which one a field uses is the order
/// creator's choice, not this function's:
///
///   * `Inline` — the preimage rides this transaction's own mint redeemer. There
///     is no on-chain threshold constant for it: the L1 16,384-byte transaction
///     budget is the gate, so the inline-versus-reference split is an off-chain
///     planning concern (`planTxOrderMaterialCarriageV1` in the SDK).
///   * `RawUtxo` / `Certified` — the preimage was published in *prior*
///     transactions as nothing-but-bytes inline datums at the creator's own
///     wallet address, chunked per §8.3/§8.4 when one publication cannot hold the
///     field, and is read here as reference inputs. Referenced datums do not
///     count against this transaction's size.
///
/// §8.4's lower bound (`total_length > K`) is enforced inside the door, so the
/// ladder stays a partition at the top end: a field that fits one publication
/// cannot be re-carried as a one-chunk "certificate".
///
/// ### Why the whole-materialising door entry point
///
/// `authenticated_whole_field_view` rather than `authenticated_field_view`. The
/// two are the same door — they share `field_door_prologue`, so the field-index
/// bound, §4's positional commitment extraction and the fields-6-to-8 witness-set
/// binding are identical — and they differ only in what tier 3 does. The lazy
/// form leaves a `Certified` field's chunk *bytes* unhashed until an accessor
/// touches one, authenticating the manifest but not the reference inputs the
/// redeemer named. This mint touches no items, so under the lazy form a tier-3
/// order would be authenticated without a single chunk ever being read: exactly
/// the availability claim this function exists to make would go unchecked. The
/// whole-materialising form concatenates the manifest's chunks and hashes them
/// once against §4's flat commitment, so wrong bytes are refused under every
/// carriage constructor.
///
/// ### What it costs, measured
///
/// Pinned by `scripts/tx-order-mint-exec-ledger-v1.json` against the tests in
/// this module's test file, because §8.11 publishes the claim and a published
/// cost claim with nothing asserting it can drift arbitrarily far.
///
/// The cost splits where `whole_view`'s §5.1 count-consistency check splits. For
/// the five fixed-stride fields it is §12.5's tier-2 per-step full-preimage
/// re-hash, bounded by §5.4's **per-field** byte bound: field 0 at 819 items
/// measures 79,774 memory units more than at one item, for 32,758 more bytes.
/// For the four variable-width fields (2, 5, 6, 8) it additionally pays
/// `walk_to_end` at one item head per item — ≈21,062 memory units each, which
/// reaches §3.3's basis near 537 items while the byte bound admits 16,382. On
/// those slots the item count is the operative bound, and the worst shape this
/// function admits measures 26× the basis; recorded on #594 for #580's
/// re-measurement, with the §8.11 erratum it needs.
///
/// Note also that the bound enforced here is **per-field**: `whole_view` compares
/// each opening's `total_length` against `max_transaction_aggregate_field_bytes`
/// and nothing below sums the nine, so §5.4's aggregate is a consensus property
/// of a valid L2 transaction rather than a guard this mint applies.
pub fn verify_order_material(
  payload: TxOrderPayloadV1,
  material_carriage: List<FieldCarriageV1>,
  reference_inputs: List<Input>,
  field_preimage_certificate_policy_id: PolicyId,
) -> OrderMaterialViewsV1 {
  let directory = material_directory(
    payload.tx_id,
    payload.transaction_commitment,
    payload.submitted_source,
  )
  OrderMaterialViewsV1 {
    body: directory.verified.tx_compact.body,
    witness_set: directory.witness_set,
    fields: authenticate_material_fields(
      directory,
      0,
      material_carriage,
      reference_inputs,
      field_preimage_certificate_policy_id,
    ),
  }
}

/// What `verify_order_material` hands the admission rules: the authenticated
/// compact body and witness set, and the nine fields' views in §2.5 order,
/// `None` for an empty field. Every view is `Whole`, because the mint opens
/// each field through the whole-materialising door.
pub type OrderMaterialViewsV1 {
  body: NativeTxBodyCompact,
  witness_set: NativeTxWitnessSetCompact,
  fields: List<Option<FieldViewV1>>,
}

// ---------------------------------------------------------------------------
// Forced-tx admission (plan rev 4 §A2, rules 1, 3, 4, 5, 6 and 7)
// ---------------------------------------------------------------------------

/// The absent validity bound in the compact body.
const absent_validity_bound: Int = -1

/// The body's absent network id.
const untagged_network_id: Int = 255

/// `blake2b_256(#"f6")`: the compact body's hash of an absent script-integrity
/// or auxiliary-data preimage.
const empty_null_hash: ByteArray = empty_script_integrity_hash

/// The head of a versioned script's `[language, payload]` pair, and the
/// language tag of a native script.
const versioned_script_array_head: Int = 0x82

const native_language_tag: Int = 0

/// The CBOR heads of a two- and a three-entry output map.
const two_entry_output_map_head: Int = 0xa2

const three_entry_output_map_head: Int = 0xa3

/// An output's datum key.
const datum_key: Int = 2

/// The CBOR head of a value, the pair `[coin, multi-asset]`.
const value_array_head: Int = 0x82

/// Fields 0 and 1: the `58 26` wrapper, then `82 58 20` before the 32-byte
/// transaction id. Byte 37 of the item is the fixed `19` output-index head.
const input_item_prefix: ByteArray = #"5826825820"

const input_item_index_head_offset: Int = 37

const input_item_index_head: Int = 0x19

/// Fields 3 and 4: the `58 1c` wrapper over a 28-byte hash.
const hash28_item_prefix: ByteArray = #"581c"

/// Field 7: the `58 65` wrapper, then `82 58 20` before the 32-byte key.
/// Bytes 37–38 of the item are the `58 40` head of the 64-byte signature.
const address_witness_item_prefix: ByteArray = #"5865825820"

const address_witness_signature_head_offset: Int = 37

const address_witness_signature_head: ByteArray = #"5840"

/// Whether the forced transaction an order carries is one the protocol admits
/// to the forced path. Called on the views `verify_order_material` returns,
/// after every field is authenticated.
///
/// The forced path is native-only and carries nothing the node cannot judge:
///
///   * Rule 1: native only. No redeemers, every field-6 script is native, and
///     no output carries a reference script. An output may carry a datum.
///   * Rule 3: at least one spend input, no auxiliary data, and not the shape
///     with a script-integrity hash, required observers and the absent network
///     id together.
///   * Rule 4: every item of fields 0, 1, 3, 4 and 7 carries its field's fixed
///     wrapper and payload prefix. The door's count arithmetic fixes each item's
///     width; this fixes the bytes the item decoders read before the payload.
///   * Rule 5: every output's value is at most `max_cardano_value_cbor_bytes`
///     bytes, measured as the value's own CBOR inside the output.
///   * Rule 6: every mint item passes the validation machine's stage-6 mint
///     fold checks (`midgard/mint_order`, read with the fold's own decoders),
///     so an admitted mint field never stalls the fold. A read the fold would
///     halt on halts the mint.
///   * Rule 7: the transaction has no validity upper bound.
///
/// Emptiness is read from the commitments, so an empty field costs nothing.
pub fn forced_tx_is_admissible_v1(views: OrderMaterialViewsV1) -> Bool {
  let OrderMaterialViewsV1 { body, witness_set, fields } = views
  expect
      [
        spend_inputs, reference_inputs, outputs, required_observers,
        required_signers, mint, script_witnesses, address_witnesses, _redeemers,
      ]
    = fields
  and {
    // Rule 7.
    has_no_validity_upper_bound(body)?,
    // Rule 3.
    has_a_spend_input(body)?,
    has_no_auxiliary_data(body)?,
    is_not_observed_on_untagged_network(body)?,
    // Rule 1.
    has_no_redeemers(witness_set)?,
    script_witnesses_are_native(script_witnesses)?,
    // Rules 1 and 5.
    outputs_are_admissible(outputs)?,
    // Rule 4.
    input_items_are_fixed(spend_inputs)?,
    input_items_are_fixed(reference_inputs)?,
    hash28_items_are_fixed(required_observers)?,
    hash28_items_are_fixed(required_signers)?,
    address_witness_items_are_fixed(address_witnesses)?,
    // Rule 6.
    mint_items_are_valid(mint)?,
  }
}

// The checks below are named so that a traced build's failure names the rule.
// `screenMidgardForcedTxAdmission` in midgard-core reports the same names, in
// the same order.

pub fn has_no_validity_upper_bound(body: NativeTxBodyCompact) -> Bool {
  body.validity_interval_end == absent_validity_bound
}

pub fn has_a_spend_input(body: NativeTxBodyCompact) -> Bool {
  body.spend_inputs_hash != empty_field_commitment
}

pub fn has_no_auxiliary_data(body: NativeTxBodyCompact) -> Bool {
  body.auxiliary_data_hash == empty_null_hash
}

/// Not a script-integrity hash, required observers and the absent network id
/// together.
pub fn is_not_observed_on_untagged_network(body: NativeTxBodyCompact) -> Bool {
  or {
    body.script_integrity_hash == empty_null_hash,
    body.required_observers_hash == empty_field_commitment,
    body.network_id != untagged_network_id,
  }
}

pub fn has_no_redeemers(witness_set: NativeTxWitnessSetCompact) -> Bool {
  witness_set.redeemer_tx_wits_hash == empty_field_commitment
}

/// The bytes of a field the mint opened. The whole-materialising door only
/// ever returns `Whole`.
fn field_bytes(view: FieldViewV1) -> ByteArray {
  expect Whole { bytes, .. } = view
  bytes
}

/// The width of the definite CBOR head at `offset`, which must have major type
/// `major`.
fn head_width(bytes: ByteArray, offset: Int, major: Int) -> Int {
  let additional = builtin.index_bytearray(bytes, offset) - major * 32
  if additional < 24 {
    expect additional >= 0
    1
  } else if additional == 24 {
    2
  } else if additional == 25 {
    3
  } else if additional == 26 {
    5
  } else {
    expect additional == 27
    9
  }
}

/// The argument of the CBOR head of width `width` at `offset`.
fn head_argument(bytes: ByteArray, offset: Int, width: Int) -> Int {
  if width == 1 {
    builtin.index_bytearray(bytes, offset) % 32
  } else if width == 2 {
    builtin.index_bytearray(bytes, offset + 1)
  } else {
    // `slice_bytearray` clamps at the end, so a head cut short is refused here.
    expect offset + width <= builtin.length_of_bytearray(bytes)
    builtin.bytearray_to_integer(
      True,
      builtin.slice_bytearray(offset + 1, width - 1, bytes),
    )
  }
}

/// Offset one past the byte string whose head is at `offset`. The one-byte
/// heads `40`–`57` and `58` take the short path; below `40` it reads another
/// major type by the same arithmetic, which the output scan refuses anyway.
fn skip_bytes(bytes: ByteArray, offset: Int) -> Int {
  let tag = builtin.index_bytearray(bytes, offset)
  if tag < 0x58 {
    offset + tag - 0x3f
  } else if tag == 0x58 {
    offset + 2 + builtin.index_bytearray(bytes, offset + 1)
  } else {
    let width = head_width(bytes, offset, 2)
    offset + width + head_argument(bytes, offset, width)
  }
}

/// Offset one past the unsigned integer whose head is at `offset`.
fn skip_unsigned(bytes: ByteArray, offset: Int) -> Int {
  if builtin.index_bytearray(bytes, offset) < 24 {
    offset + 1
  } else {
    offset + head_width(bytes, offset, 0)
  }
}

/// Offset one past `count` assets, each a name and a quantity.
fn skip_assets(bytes: ByteArray, offset: Int, count: Int) -> Int {
  if count == 0 {
    offset
  } else {
    skip_assets(
      bytes,
      skip_unsigned(bytes, skip_bytes(bytes, offset)),
      count - 1,
    )
  }
}

/// Offset one past `count` policies, each a policy id and its asset map.
fn skip_policies(bytes: ByteArray, offset: Int, count: Int) -> Int {
  if count == 0 {
    offset
  } else {
    let assets = skip_bytes(bytes, offset)
    let width = head_width(bytes, assets, 5)
    skip_policies(
      bytes,
      skip_assets(bytes, assets + width, head_argument(bytes, assets, width)),
      count - 1,
    )
  }
}

/// Offset one past the value `[coin, {policy => {name => quantity}}]` at
/// `offset`.
fn skip_value(bytes: ByteArray, offset: Int) -> Int {
  expect builtin.index_bytearray(bytes, offset) == value_array_head
  let policies = skip_unsigned(bytes, offset + 1)
  let width = head_width(bytes, policies, 5)
  skip_policies(bytes, policies + width, head_argument(bytes, policies, width))
}

/// Rules 1 and 5 on one output, the item payload `[start, end)`, the map
/// `{0: address, 1: value, ?2: datum, ?3: reference script}`.
///
/// Rule 1: the output carries no reference script. A two-entry output has
/// none; a three-entry output's third key is the datum's `2`; a four-entry
/// output always carries one.
///
/// Rule 5: the value's bytes. In a two-entry output the value runs to the end
/// of the item, so its size is read off the item without walking it.
///
/// Only the outputs the output scan accepts need a truthful verdict here; it
/// refuses every other shape as non-canonical, so the keys are read by
/// position rather than checked.
fn output_is_admissible(bytes: ByteArray, start: Int, end: Int) -> Bool {
  let map_head = builtin.index_bytearray(bytes, start)
  let value = skip_bytes(bytes, start + 2) + 1
  if map_head == two_entry_output_map_head {
    end - value <= max_cardano_value_cbor_bytes
  } else {
    and {
      map_head == three_entry_output_map_head,
      {
        let value_end = skip_value(bytes, value)
        and {
          value_end - value <= max_cardano_value_cbor_bytes,
          builtin.index_bytearray(bytes, value_end) == datum_key,
        }
      },
    }
  }
}

/// Each §5.1 item from `offset` to `end`, its payload `[start, item_end)`
/// handed to `admits`. The door's `walk_to_end` already proved every head a
/// canonical byte-string head of at most two argument bytes, and the walk to
/// land on `end`.
fn variable_items_admitted(
  bytes: ByteArray,
  offset: Int,
  end: Int,
  admits: fn(ByteArray, Int, Int) -> Bool,
) -> Bool {
  if offset == end {
    True
  } else {
    let tag = builtin.index_bytearray(bytes, offset)
    let start = if tag < 0x58 {
      offset + 1
    } else if tag == 0x58 {
      offset + 2
    } else {
      offset + 3
    }
    let item_end = skip_bytes(bytes, offset)
    and {
      admits(bytes, start, item_end),
      variable_items_admitted(bytes, item_end, end, admits),
    }
  }
}

fn variable_field_admitted(
  field: Option<FieldViewV1>,
  admits: fn(ByteArray, Int, Int) -> Bool,
) -> Bool {
  when field is {
    None -> True
    Some(view) -> {
      let bytes = field_bytes(view)
      variable_items_admitted(
        bytes,
        field_header_len(view),
        builtin.length_of_bytearray(bytes),
        admits,
      )
    }
  }
}

pub fn outputs_are_admissible(outputs: Option<FieldViewV1>) -> Bool {
  variable_field_admitted(outputs, output_is_admissible)
}

/// Rule 6 on the `remaining` assets of one mint item, from `offset` to the
/// item's `end`: each a canonical name and a canonical non-zero quantity, the
/// names strictly ascending, the last one ending exactly at the item's end.
fn mint_assets_are_valid(
  bytes: ByteArray,
  offset: Int,
  end: Int,
  remaining: Int,
  first: Bool,
  previous_asset: ByteArray,
) -> Bool {
  let Pair(quantity_offset, asset_name) = decode_canonical_bytes_at(
    bytes,
    offset,
  )
  let Pair(next, quantity) = decode_canonical_int_at(bytes, quantity_offset)
  and {
    mint_asset_is_valid(first, previous_asset, asset_name, quantity),
    if remaining == 1 {
      next == end
    } else {
      and {
        next < end,
        mint_assets_are_valid(
          bytes,
          next,
          end,
          remaining - 1,
          False,
          asset_name,
        ),
      }
    },
  }
}

/// Rule 6 on the mint items from `offset` to the field's `end`: the stage-6
/// mint fold's checks, through the decoders and the shared predicates
/// (`midgard/mint_order`) it uses, so an admitted mint field never stalls the
/// fold. Each item is `[policy_id, {asset_name => quantity}]`, policies
/// strictly ascending. The fold's item-count bound is not repeated: an item
/// that passes is at least 34 bytes, so a field holds far fewer.
fn mint_items_from(
  bytes: ByteArray,
  offset: Int,
  end: Int,
  first: Bool,
  previous_policy: ByteArray,
) -> Bool {
  if offset == end {
    True
  } else {
    let tag = builtin.index_bytearray(bytes, offset)
    let start = if tag < 0x58 {
      offset + 1
    } else if tag == 0x58 {
      offset + 2
    } else {
      offset + 3
    }
    let item_end = skip_bytes(bytes, offset)
    let Pair(policy_offset, item_count) = decode_definite_array_header_at(
      bytes,
      start,
    )
    expect item_count == 2
    let Pair(assets_header_offset, policy_id) = decode_canonical_bytes_at(
      bytes,
      policy_offset,
    )
    let Pair(assets_offset, asset_count) = decode_canonical_map_header_at(
      bytes,
      assets_header_offset,
    )
    and {
      mint_policy_is_valid(first, previous_policy, policy_id, asset_count),
      mint_assets_are_valid(
        bytes,
        assets_offset,
        item_end,
        asset_count,
        True,
        #"",
      ),
      mint_items_from(bytes, item_end, end, False, policy_id),
    }
  }
}

pub fn mint_items_are_valid(mint: Option<FieldViewV1>) -> Bool {
  when mint is {
    None -> True
    Some(view) -> {
      let bytes = field_bytes(view)
      mint_items_from(
        bytes,
        field_header_len(view),
        builtin.length_of_bytearray(bytes),
        True,
        #"",
      )
    }
  }
}

/// The field-6 item `[start, end)` is a native versioned script: the header
/// `versioned_script_header_v1` (the parser the witness-script decoding rule
/// binds) accepts with language 0, which is exactly `82 00`, then a minimal
/// byte-string head whose payload runs to the item's end. A field item is under
/// §5.4's 32,768 bytes, so only the one-, two- and three-byte heads can be
/// minimal. Every byte read before the length test passes lies inside the item.
fn script_is_native(bytes: ByteArray, start: Int, end: Int) -> Bool {
  and {
    builtin.index_bytearray(bytes, start) == versioned_script_array_head,
    builtin.index_bytearray(bytes, start + 1) == native_language_tag,
    {
      let head = start + 2
      let tag = builtin.index_bytearray(bytes, head)
      if tag < 0x58 {
        and {
          tag >= 0x40,
          head + tag - 0x3f == end,
        }
      } else if tag == 0x58 {
        let length = builtin.index_bytearray(bytes, head + 1)
        and {
          length >= 24,
          head + 2 + length == end,
        }
      } else if tag == 0x59 {
        let length = head_argument(bytes, head, 3)
        and {
          length >= 256,
          head + 3 + length == end,
        }
      } else {
        False
      }
    },
  }
}

pub fn script_witnesses_are_native(
  script_witnesses: Option<FieldViewV1>,
) -> Bool {
  variable_field_admitted(script_witnesses, script_is_native)
}

fn input_items_from(bytes: ByteArray, offset: Int, end: Int) -> Bool {
  if offset == end {
    True
  } else {
    and {
      builtin.slice_bytearray(offset, 5, bytes) == input_item_prefix,
      builtin.index_bytearray(bytes, offset + input_item_index_head_offset) == input_item_index_head,
      input_items_from(bytes, offset + spend_input_stride, end),
    }
  }
}

pub fn input_items_are_fixed(inputs: Option<FieldViewV1>) -> Bool {
  when inputs is {
    None -> True
    Some(view) -> {
      let bytes = field_bytes(view)
      input_items_from(bytes, field_header_len(view), bytearray.length(bytes))
    }
  }
}

fn hash28_items_from(bytes: ByteArray, offset: Int, end: Int) -> Bool {
  if offset == end {
    True
  } else {
    and {
      builtin.slice_bytearray(offset, 2, bytes) == hash28_item_prefix,
      hash28_items_from(bytes, offset + hash28_stride, end),
    }
  }
}

pub fn hash28_items_are_fixed(hashes: Option<FieldViewV1>) -> Bool {
  when hashes is {
    None -> True
    Some(view) -> {
      let bytes = field_bytes(view)
      hash28_items_from(bytes, field_header_len(view), bytearray.length(bytes))
    }
  }
}

fn address_witness_items_from(bytes: ByteArray, offset: Int, end: Int) -> Bool {
  if offset == end {
    True
  } else {
    and {
      builtin.slice_bytearray(offset, 5, bytes) == address_witness_item_prefix,
      builtin.slice_bytearray(
        offset + address_witness_signature_head_offset,
        2,
        bytes,
      ) == address_witness_signature_head,
      address_witness_items_from(bytes, offset + address_witness_stride, end),
    }
  }
}

pub fn address_witness_items_are_fixed(witnesses: Option<FieldViewV1>) -> Bool {
  when witnesses is {
    None -> True
    Some(view) -> {
      let bytes = field_bytes(view)
      address_witness_items_from(
        bytes,
        field_header_len(view),
        bytearray.length(bytes),
      )
    }
  }
}

pub fn forced_inclusion_key_value(
  tx_order_event_data: Data<TxOrderEventV1>,
  validity_override: OperatorVerdictV1,
  return: fn(TxOrderId, ForcedInclusionTxV1) -> result,
) -> result {
  let Pair(tx_order_event_id_data, tx_order_payload_data) = unsafe_event_to_key_value_pair(
    tx_order_event_data,
  )

  expect tx_order_id: TxOrderId = tx_order_event_id_data
  expect tx_order_payload: TxOrderPayloadV1 = tx_order_payload_data

  return(
    tx_order_id,
    ForcedInclusionTxV1 {
      tx_id: tx_order_payload.tx_id,
      submitted_source: tx_order_payload.submitted_source,
      verdict: validity_override,
    },
  )
}

pub fn get_datum(
  reference_inputs: List<Input>,
  tx_order_policy_id: PolicyId,
  tx_order_input_index: Int,
) -> Datum {
  expect
      Input {
        output: Output { datum: InlineDatum(tx_order_datum_data), .. },
        ..
      }
    = utils.get_authentic_input_with_policy_at(
    reference_inputs,
    tx_order_policy_id,
    tx_order_input_index,
  )
  expect tx_order_datum: Datum = tx_order_datum_data
  tx_order_datum
}
