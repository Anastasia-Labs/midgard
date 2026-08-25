use aiken/collection/list
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/utils
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_field_preimage_lengths_v1, native_tx_proof_commitment_v1,
  verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxFieldPreimageLengthsV1, NativeTxWitnessSetCompact,
  VerifiedMidgardNativeTxCompact,
}
use midgard/ledger_state.{
  ForcedInclusionTxV1, NativeTxProofSourceV1, TxOrderEventV1, TxOrderId,
  TxOrderPayloadV1, unsafe_event_to_key_value_pair,
}
use midgard/native_tx_field_access_v1.{FieldCarriageV1}
use midgard/rejection_reason_v1.{OperatorVerdictV1}
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
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  commitments: List<ByteArray>,
  lengths: NativeTxFieldPreimageLengthsV1,
}

fn material_directory(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  source: NativeTxProofSourceV1,
) -> OrderMaterialDirectoryV1 {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    transaction_id,
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
  )
  expect
    native_tx_proof_commitment_v1(
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
/// Returns whether the redeemer's vector was **exhausted exactly**. Every other
/// refusal aborts, because every other refusal is a claim that turned out to be
/// false about bytes already in hand (§7.3, abort-never-clamp).
fn authenticate_material_fields(
  directory: OrderMaterialDirectoryV1,
  field_index: Int,
  material_carriage: List<FieldCarriageV1>,
  reference_inputs: List<Input>,
  field_preimage_certificate_policy_id: PolicyId,
) -> Bool {
  if field_index >= native_tx_field_access_v1.field_count {
    // Exhaustion. A spare entry is not harmless: it would let two distinct
    // redeemers authenticate the same order, and one of them would name
    // reference inputs nothing in this walk ever read.
    list.is_empty(material_carriage)
  } else {
    let commitment = directory.commitments |> list.expect_at(field_index)
    let declared_length = field_length(directory.lengths, field_index)
    if commitment == native_tx_field_access_v1.empty_field_commitment {
      // §5.1's empty field is the one-byte header `80`, so its declared length
      // is 1 and there is nothing to carry. Checked rather than skipped: the
      // declared vector is inside `transaction_commitment`, so a length that
      // disagrees with an empty commitment is an order that contradicts itself.
      expect declared_length == 1
      authenticate_material_fields(
        directory,
        field_index + 1,
        material_carriage,
        reference_inputs,
        field_preimage_certificate_policy_id,
      )
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
      authenticate_material_fields(
        directory,
        field_index + 1,
        rest,
        reference_inputs,
        field_preimage_certificate_policy_id,
      )
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
) -> Bool {
  authenticate_material_fields(
    material_directory(
      payload.tx_id,
      payload.transaction_commitment,
      payload.source,
    ),
    0,
    material_carriage,
    reference_inputs,
    field_preimage_certificate_policy_id,
  )
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
      source: tx_order_payload.source,
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
