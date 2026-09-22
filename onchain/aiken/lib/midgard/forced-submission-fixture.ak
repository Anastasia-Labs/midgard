//// Test construction of a new forced submission from existing native fixture material.

use midgard/fraud_proofs/field_opening_v1.{
  BodyFieldOpening, FieldOpeningV1, WitnessFieldOpening,
}
use midgard/fraud_proofs/native_tx/compact
use midgard/fraud_proofs/native_tx/transaction
use midgard/fraud_proofs/native_tx/types.{ForcedTxCompact, MidgardTransaction}
use midgard/ledger_state.{ForcedTxProofSourceV1, NativeTxProofSourceV1}

pub fn from_native_source(
  source: NativeTxProofSourceV1,
) -> ForcedTxProofSourceV1 {
  let native = compact.decode_native_tx_compact_v1(source.compact_cbor)
  ForcedTxProofSourceV1 {
    compact_cbor: compact.encode_forced_tx_compact_v1(
      ForcedTxCompact {
        body: native.body,
        witness_set_hash: native.witness_set_hash,
      },
    ),
    witness_set_compact_cbor: source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: source.field_preimage_lengths_cbor,
  }
}

/// Explicit fixture projection. Never used to admit obsolete protocol bytes.
pub fn from_native_compact(bytes: ByteArray) -> ByteArray {
  let native = compact.decode_native_tx_compact_v1(bytes)
  compact.encode_forced_tx_compact_v1(
    ForcedTxCompact {
      body: native.body,
      witness_set_hash: native.witness_set_hash,
    },
  )
}

pub fn from_native_opening(opening: FieldOpeningV1) -> FieldOpeningV1 {
  when opening is {
    BodyFieldOpening { native_tx_compact_cbor, carriage } ->
      BodyFieldOpening {
        native_tx_compact_cbor: from_native_compact(native_tx_compact_cbor),
        carriage,
      }
    WitnessFieldOpening { native_tx_compact_cbor, witness_set, carriage } ->
      WitnessFieldOpening {
        native_tx_compact_cbor: from_native_compact(native_tx_compact_cbor),
        witness_set,
        carriage,
      }
  }
}

pub fn from_native_transaction(tx: MidgardTransaction) -> ForcedTxProofSourceV1 {
  let source = transaction.midgard_transaction_proof_source_v1(tx)
  from_native_source(
    NativeTxProofSourceV1 {
      compact_cbor: source.compact_cbor,
      witness_set_compact_cbor: source.witness_set_compact_cbor,
      field_preimage_lengths_cbor: source.field_preimage_lengths_cbor,
    },
  )
}

pub fn commitment_of_native_transaction(tx: MidgardTransaction) -> ByteArray {
  let source = from_native_transaction(tx)
  compact.forced_tx_proof_commitment_v1(
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
  )
}
