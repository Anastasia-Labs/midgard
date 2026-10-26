//// Test fixtures for `executionNativeScriptInvalid`. Harness only: nothing
//// here is reachable from a validator.

use aiken/cbor
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference}
use midgard/bounded_item_v1.{ChunkProofV1}
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1} as dd_types
use midgard/fraud_proofs/execution_native_script_invalid/types.{EvalV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/fraud_proofs/native_tx/compact.{encode_forced_tx_compact_v1}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1 as descriptor
use midgard/rejection_reason_v1.{ExecutionNativeScriptFalse, RejectionReasonV1}
use midgard/rejection_reason_v1 as reasons
use midgard/validation_merkle_v1

pub fn recorded(kind: Int, index: Int) -> Option<RejectionReasonV1> {
  Some(ExecutionNativeScriptFalse { purpose_kind: kind, purpose_index: index })
}

/// `tx` committed as a forced transaction, under a subject that rejects it
/// with `reason` (or accepts it when `None`).
pub fn forced_bound(
  tx: native_types.MidgardTransaction,
  reason: Option<RejectionReasonV1>,
  pre_root: ByteArray,
) -> (BoundV1, fixture.NativeBlockFixtureV1) {
  let block = fixture.native_block_fixture_v1(tx)
  (
    BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction: if reason == None {
          0
        } else {
          1
        },
        source_kind: 1,
        transaction_id: block.tx_id,
        source_key: #"00",
        rejection_reason: reason,
      },
      compact_cbor: encode_forced_tx_compact_v1(
        native_types.ForcedTxCompact {
          body: block.compact.body,
          witness_set_hash: block.compact.witness_set_hash,
        },
      ),
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: pre_root,
    },
    block,
  )
}

// ## Native scripts

pub fn sig(key: ByteArray) -> ByteArray {
  bytearray.concat(#"8200581c", key)
}

pub fn after(slot: Int) -> ByteArray {
  bytearray.concat(#"8204", cbor.serialise(slot))
}

pub fn before(slot: Int) -> ByteArray {
  bytearray.concat(#"8205", cbor.serialise(slot))
}

fn children(items: List<ByteArray>) -> ByteArray {
  list.foldl(
    items,
    encode_definite_array_header(list.length(items)),
    fn(item, acc) { bytearray.concat(acc, item) },
  )
}

pub fn all(items: List<ByteArray>) -> ByteArray {
  bytearray.concat(#"8201", children(items))
}

pub fn any(items: List<ByteArray>) -> ByteArray {
  bytearray.concat(#"8202", children(items))
}

/// The committed versioned-script item `[0, bytes(payload)]`.
pub fn item(payload: ByteArray) -> ByteArray {
  bytearray.concat(#"8200", encode_definite_bytes(payload))
}

pub fn commitment_of(bytes: ByteArray) -> ByteArray {
  bounded_item_v1.from_bytes(6, 0, bytes)
}

/// The only chunk of a one-chunk item.
pub fn chunk_of(bytes: ByteArray) -> ChunkProofV1 {
  ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 6,
    item_index: 0,
    total_length: bytearray.length(bytes),
    chunk_index: 0,
    chunk: bytes,
    frontier: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: bounded_item_v1.hash_chunk(6, 0, 0, bytes),
      },
    ],
    siblings: [],
  }
}

// ## A key-locked spend

const spend_hash = #"33333333333333333333333333333333333333333333333333333333"

pub const root =
  #"4444444444444444444444444444444444444444444444444444444444444444"

pub fn spend_input(index: Int) {
  native_types.MidgardTxInput { tx_id: root, output_index: index }
}

/// Raw spend items 9, 1, 5: raw item two (output 5) has canonical spend rank
/// one. Its spent output is key-locked when `key`, script-locked otherwise.
pub fn key_spend_fixture(key: Bool, recorded_index: Int) {
  let inputs = [spend_input(9), spend_input(1), spend_input(5)]
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody { ..tx.body, inputs },
    },
  )
  let summary = DataSummaryV1 { root, cbor_length: 1, memory: 1 }
  let value = descriptor.encode(
    descriptor.LedgerOutputCommitmentV1 {
      version: 1,
      output_index: 5,
      total_length: 50,
      item_commitment: root,
      address: bytearray.concat(
        if key {
          #"60"
        } else {
          #"70"
        },
        spend_hash,
      ),
      lovelace: 5000000,
      asset_count: 0,
      asset_frontier_commitment: root,
      cardano_value_size: 5,
      reference_script_language: -1,
      reference_script_hash: #"",
      reference_script_total_length: 0,
      reference_script_item_commitment: #"",
      cardano_tx_out: summary,
      midgard_tx_out: summary,
      cardano_spend_datum: summary,
    },
  )
  let outref_key = ledger_outref_key(
    OutputReference { transaction_id: root, output_index: 5 },
  )
  let pre_root = mpf.root(mpf.insert(mpf.empty, outref_key, value, []))
  let bound = BoundV1 {
    subject: VerdictSubjectV1 {
      version: 1,
      direction: 1,
      source_kind: 1,
      transaction_id: block.tx_id,
      source_key: #"00",
      rejection_reason: Some(
        reasons.ExecutionNativeScriptFalse {
          purpose_kind: 0,
          purpose_index: recorded_index,
        },
      ),
    },
    compact_cbor: encode_forced_tx_compact_v1(
      native_types.ForcedTxCompact {
        body: block.compact.body,
        witness_set_hash: block.compact.witness_set_hash,
      },
    ),
    network_order_context: NetworkOrderContextV1 {
      body_network_mismatch: False,
    },
    event_pre_root: pre_root,
  }
  let opening = fixture.inline_body_opening(
    bound.compact_cbor,
    list.map(inputs, encode_midgard_tx_input),
  )
  (
    BoundIndexV1 { bound, source_index: source_index.initial_v1() },
    opening,
    value,
    outref_key,
  )
}

// ## A field-7 signer set

pub fn witness(byte: ByteArray) -> native_types.MidgardAddressWitness {
  native_types.MidgardAddressWitness {
    verification_key: fixture.repeated_bytes_v1(byte, 32),
    signature: fixture.repeated_bytes_v1(byte, 64),
  }
}

pub fn witnesses() -> List<native_types.MidgardAddressWitness> {
  [witness(#"01"), witness(#"02"), witness(#"03")]
}

/// A forced transaction whose field 7 commits `committed`; the opening
/// carries `opened`.
pub fn signers_fixture(
  committed: List<native_types.MidgardAddressWitness>,
  opened: List<native_types.MidgardAddressWitness>,
) -> (EvalV1, FieldOpeningV1) {
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      witness_set: native_types.MidgardTransactionWitnessSet {
        ..tx.witness_set,
        addr_tx_wits: committed,
      },
    },
  )
  let compact_cbor = encode_forced_tx_compact_v1(
    native_types.ForcedTxCompact {
      body: block.compact.body,
      witness_set_hash: block.compact.witness_set_hash,
    },
  )
  let bound = BoundV1 {
    subject: VerdictSubjectV1 {
      version: 1,
      direction: 1,
      source_kind: 1,
      transaction_id: block.tx_id,
      source_key: #"00",
      rejection_reason: Some(
        reasons.ExecutionNativeScriptFalse { purpose_kind: 0, purpose_index: 0 },
      ),
    },
    compact_cbor,
    network_order_context: NetworkOrderContextV1 {
      body_network_mismatch: False,
    },
    event_pre_root: root,
  }
  (
    EvalV1 {
      bound,
      coordinate: PurposeCoordinateV1 { kind: 0, index: 0 },
      item_commitment: root,
      validity_interval_start: -1,
      validity_interval_end: -1,
      signer_count: 0,
      signer_peaks: [],
      total_length: 0,
      cursor: 0,
      stack_root: #"",
      stack_depth: 0,
      pending: None,
    },
    fixture.inline_witness_opening(
      compact_cbor,
      block.witness_set_compact,
      list.map(opened, encode_midgard_address_witness),
    ),
  )
}
