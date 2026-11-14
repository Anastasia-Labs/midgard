use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/direct_script_derivation/types.{SourceBoundV1}
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/ledger_state.{HeaderV1}

fn bytes(tag: Int, length: Int) -> ByteArray {
  bytearray.from_int_big_endian(tag, length)
}

// Every field carries a distinct value, so any field reorder changes the
// encoding. The same value and digest are pinned off-chain in
// demo/midgard-sdk/tests/direct-script-derivation-source.test.ts.
test source_bound_encodes_as_off_chain() {
  let bound = SourceBoundV1 {
    subject: VerdictSubjectV1 {
      version: 1,
      direction: 2,
      source_kind: 3,
      transaction_id: bytes(4, 32),
      source_key: bytes(5, 32),
      rejection_reason: None,
    },
    header: HeaderV1 {
      prev_utxos_root: bytes(11, 32),
      utxos_root: bytes(12, 32),
      withdrawals_root: bytes(13, 32),
      forced_transactions_root: bytes(14, 32),
      transactions_root: bytes(15, 32),
      deposits_root: bytes(16, 32),
      transition_trace_root: bytes(17, 32),
      event_to_step_root: bytes(18, 32),
      validation_traces_root: bytes(19, 32),
      withdrawal_count: 20,
      forced_transaction_count: 21,
      l2_transaction_count: 22,
      deposit_count: 23,
      total_event_count: 24,
      transition_step_count: 25,
      validation_trace_count: 26,
      start_time: 27,
      end_time: 28,
      block_slot: 29,
      expected_network_id: 30,
      min_fee_a: 31,
      min_fee_b: 32,
      prev_header_hash: bytes(33, 28),
      operator_vkey: bytes(34, 28),
      protocol_version: 35,
    },
    compact_cbor: bytes(36, 3),
  }
  blake2b_256(cbor.serialise(bound)) == #"476753a9cdb14f3b018240c362ee53a5cc14a42143c526b2bace36fe0e062aed"
}
