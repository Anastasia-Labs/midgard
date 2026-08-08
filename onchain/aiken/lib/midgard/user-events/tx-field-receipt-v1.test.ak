//// **This module's `PublishField` branch is dead, and these tests are the
//// record of it.**
////
//// `verify_publication` gates publication on
//// `native_transaction.verify_midgard_transaction_field_chunk_v1`, a retired
//// counted-scheme predicate. Under `docs/spec/midgard-tx.md` §4 a field
//// commitment is a flat `blake2b_256` over the preimage bytes, which has no
//// leaves for a per-item Merkle opening to be checked against — so that
//// predicate cannot be satisfied, and no field receipt can be minted.
////
//// This module had no test file, so nothing in the suite said any of that out
//// loud; a comment in `fraud-proofs/native-tx/transaction.ak` went as far as
//// asserting that no live path called the predicate at all, which was false.
//// The break is liveness, not soundness: publication fails closed, so no
//// receipt can be minted for a field that was never opened.
////
//// The rebind is not this ticket's. The receipt chain publishes *items and
//// chunks* under the counted scheme; the flat scheme publishes *whole
//// preimages* under the three §8 carriage tiers, so the replacement for this
//// chain is the §8.6 permissionless chunk-certificate validator and its
//// publish/heal tooling (#573, #574), on top of #570's machine walk core.
//// Whoever lands that must come back and delete this file — the last two
//// tests here turn red the moment the predicate becomes satisfiable again.

use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, encode_native_tx_compact_v1,
  encode_native_tx_field_preimage_lengths_v1,
  encode_native_tx_witness_set_compact, native_tx_id_for_version,
  native_tx_proof_commitment_v1, verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_address_witness}
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, NativeTxBodyCompact, NativeTxCompact,
  NativeTxFieldPreimageLengthsV1, NativeTxWitnessSetCompact,
  native_tx_version_v1,
}
use midgard/native_tx_field_access_v1.{
  Inline, authenticated_field_view, encode_field_preimage, field_item_at,
  field_item_count,
}
use midgard/validation_merkle_v1

const zero_hash: ByteArray =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const certificate_policy_id: ByteArray =
  #"22222222222222222222222222222222222222222222222222222222"

/// Field 7's §5.3 item: `82 ‖ 58 20 vkey(32) ‖ 58 40 signature(64)`.
fn address_witness_item() -> ByteArray {
  encode_midgard_address_witness(
    MidgardAddressWitness {
      verification_key: blake2b_256(#"01"),
      signature: bytearray.concat(blake2b_256(#"02"), blake2b_256(#"03")),
    },
  )
}

fn address_witness_preimage() -> ByteArray {
  encode_field_preimage([address_witness_item()])
}

/// A transaction whose field-7 slot carries the §4 flat commitment of exactly
/// one address witness. Everything the receipt mint forwards to the predicate
/// is derived from it.
fn source() -> NativeTxProofSourceCbor {
  let witness_set = NativeTxWitnessSetCompact {
    addr_tx_wits_hash: blake2b_256(address_witness_preimage()),
    script_tx_wits_hash: zero_hash,
    redeemer_tx_wits_hash: zero_hash,
  }
  let witness_set_cbor = encode_native_tx_witness_set_compact(witness_set)
  let compact = NativeTxCompact {
    body: NativeTxBodyCompact {
      spend_inputs_hash: zero_hash,
      reference_inputs_hash: zero_hash,
      outputs_hash: zero_hash,
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers_hash: zero_hash,
      required_signers_hash: zero_hash,
      mint_hash: zero_hash,
      script_integrity_hash: zero_hash,
      auxiliary_data_hash: zero_hash,
      network_id: 255,
    },
    witness_set_hash: blake2b_256(witness_set_cbor),
    validity_code: 0,
  }
  let compact_cbor = encode_native_tx_compact_v1(compact)
  let lengths_cbor = encode_native_tx_field_preimage_lengths_v1(
    NativeTxFieldPreimageLengthsV1 {
      spend_inputs: 1,
      reference_inputs: 1,
      outputs: 1,
      required_observers: 1,
      required_signers: 1,
      mint: 1,
      address_witnesses: bytearray.length(address_witness_preimage()),
      script_witnesses: 1,
      redeemers: 1,
    },
  )
  NativeTxProofSourceCbor {
    transaction_id: native_tx_id_for_version(
      native_tx_version_v1,
      encode_native_tx_body_compact(compact.body),
    ),
    transaction_commitment: native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_cbor,
      lengths_cbor,
    ),
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
  }
}

type NativeTxProofSourceCbor {
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_cbor: ByteArray,
  lengths_cbor: ByteArray,
}

/// The counted-scheme opening a publisher would build for that single item:
/// well formed in every internal respect, which is the point — nothing about
/// the proof is what makes publication fail.
fn counted_opening() -> Pair<
  bounded_collection_v1.ItemProofV1,
  bounded_item_v1.ChunkProofV1,
> {
  let item = address_witness_item()
  let item_leaf = bounded_item_v1.hash_chunk(7, 0, 0, item)
  let item_frontier = validation_merkle_v1.build_frontier([item_leaf])
  let item_commitment = bounded_item_v1.commitment(
    7,
    0,
    bytearray.length(item),
    item_frontier.peaks,
  )
  let collection_leaf = bounded_collection_v1.hash_item(
    7,
    0,
    bytearray.length(item),
    item_commitment,
  )
  let collection_frontier = validation_merkle_v1.build_frontier(
    [collection_leaf],
  )
  Pair(
    bounded_collection_v1.ItemProofV1 {
      version: bounded_collection_v1.version,
      field_index: 7,
      item_count: 1,
      item_index: 0,
      item_length: bytearray.length(item),
      item_commitment,
      frontier: collection_frontier.peaks,
      siblings: [],
    },
    bounded_item_v1.ChunkProofV1 {
      version: bounded_item_v1.version,
      field_index: 7,
      item_index: 0,
      total_length: bytearray.length(item),
      chunk_index: 0,
      chunk: item,
      frontier: item_frontier.peaks,
      siblings: [],
    },
  )
}

/// The opening is internally sound: its own two verifiers accept the item
/// commitment and the chunk. So when publication fails below, it is not
/// because the fixture is malformed.
test the_counted_opening_is_internally_well_formed() {
  let Pair(collection_proof, chunk_proof) = counted_opening()
  and {
    bounded_item_v1.verify_chunk(collection_proof.item_commitment, chunk_proof),
    collection_proof.item_length == chunk_proof.total_length,
    collection_proof.field_index == chunk_proof.field_index,
    collection_proof.item_index == chunk_proof.item_index,
  }
}

/// The root cause, isolated: a counted-scheme item proof is checked against a
/// counted-scheme Merkle root, and a §4 field commitment is not one.
test counted_item_proof_cannot_open_a_flat_field_commitment() {
  let Pair(collection_proof, _chunk_proof) = counted_opening()
  !bounded_collection_v1.verify_item(
    blake2b_256(address_witness_preimage()),
    collection_proof,
  )
}

/// The gate itself, called with exactly the arguments `verify_publication`
/// forwards at `tx-field-receipt-v1.ak:125`. It answers `False`, so the
/// `PublishField` branch of this module's `validate` can never return `True`.
test publish_field_gate_is_unsatisfiable() {
  let s = source()
  let Pair(collection_proof, chunk_proof) = counted_opening()
  !native_transaction.verify_midgard_transaction_field_chunk_v1(
    s.transaction_id,
    s.transaction_commitment,
    s.compact_cbor,
    s.witness_set_cbor,
    s.lengths_cbor,
    collection_proof,
    chunk_proof,
  )
}

/// And the capability itself is not lost — the very same field, on the very
/// same authenticated source, opens through the door. The receipt chain needs
/// rebinding, not resurrecting.
test the_same_field_still_opens_through_the_door() {
  let s = source()
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    s.transaction_id,
    s.compact_cbor,
    s.witness_set_cbor,
    s.lengths_cbor,
  )
  let view = authenticated_field_view(
    verified,
    witness_set,
    7,
    Inline { preimage: address_witness_preimage() },
    [],
    certificate_policy_id,
  )
  and {
    field_item_count(view) == 1,
    field_item_at(view, 0) == address_witness_item(),
  }
}
