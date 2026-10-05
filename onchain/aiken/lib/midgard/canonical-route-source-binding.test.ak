use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_field_preimage_lengths_v1, encode_native_tx_body_compact,
  encode_native_tx_field_preimage_lengths_v1, native_tx_id_for_version,
  verify_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  MidgardTxInput, NativeTxFieldPreimageLengthsV1, TxIsValid,
  native_tx_version_v1,
}
use midgard/validation_machine/canonical_decode
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_machine/witness.{encode_transaction_field_scan_witness}
use midgard/validation_trace_v1 as vt

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

fn transaction() -> MidgardTransaction {
  let empty_mint: Data = []
  MidgardTransaction {
    version: native_tx_version_v1,
    validity: TxIsValid,
    body: MidgardTransactionBody {
      inputs: [MidgardTxInput { tx_id: zero, output_index: 0 }],
      reference_inputs: [],
      outputs: [],
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers: [],
      required_signers: [],
      mint: empty_mint,
      script_integrity_hash: zero,
      auxiliary_data_hash: zero,
      network_id: 255,
    },
    witness_set: MidgardTransactionWitnessSet {
      addr_tx_wits: [],
      script_tx_wits: [],
      redeemer_tx_wits: [],
    },
  }
}

fn fixture(
  field_index: Int,
  substitute_lengths: Bool,
) -> Pair<vt.ValidationMachineStateV1, ValidationOneStepWitnessV1> {
  let tx = transaction()
  let source = native_transaction.midgard_transaction_proof_source_v1(tx)
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  let lengths = decode_native_tx_field_preimage_lengths_v1(
    source.field_preimage_lengths_cbor,
  )
  // Change a different field's length: the selected field and successor stay
  // exact, the body tx_id stays exact, and only the source commitment changes.
  let lengths_cbor = if substitute_lengths {
    encode_native_tx_field_preimage_lengths_v1(
      NativeTxFieldPreimageLengthsV1 {
        ..lengths,
        redeemers: lengths.redeemers + 1,
      },
    )
  } else {
    source.field_preimage_lengths_cbor
  }
  let Pair(verified, _) = verify_tx_proof_source_v1(
    vt.Normal,
    native_tx_id_for_version(
      native_tx_version_v1,
      encode_native_tx_body_compact(compact.body),
    ),
    source.compact_cbor,
    source.witness_set_compact_cbor,
    lengths_cbor,
  )
  let work = encode_transaction_field_scan_witness(
    source.compact_cbor,
    source.witness_set_compact_cbor,
    lengths_cbor,
    #"",
    field_index,
    0,
    0,
    -1,
    0,
  )
  let pre = vt.ValidationMachineStateV1 {
    machine_version: vt.machine_version,
    event_key_hash: zero,
    transaction_id: verified.tx_id,
    transaction_commitment: native_transaction.midgard_transaction_proof_commitment_v1(
      tx,
    ),
    validation_context_hash: vt.hash_validation_context(#""),
    source_kind: vt.Normal,
    prior_ledger_root: zero,
    phase: vt.CanonicalDecode,
    program_counter: 0,
    work_root: vt.hash_work_witness(vt.CanonicalDecode, 0, work),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: vt.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: zero,
  }
  let post_work = encode_transaction_field_scan_witness(
    source.compact_cbor,
    source.witness_set_compact_cbor,
    lengths_cbor,
    #"",
    field_index + 1,
    0,
    0,
    -1,
    0,
  )
  let post = vt.ValidationMachineStateV1 {
    ..pre,
    program_counter: 1,
    work_root: vt.hash_work_witness(vt.CanonicalDecode, 1, post_work),
  }
  Pair(
    pre,
    ValidationOneStepWitnessV1 {
      work_witness_cbor: work,
      claimed_successor: post,
    },
  )
}

// These call the exact production predicates; emulator controls separately
// cover the parameterized validator routes and their honest refusal.
test empty_accepts_authenticated_source() {
  let Pair(pre, witness) = fixture(1, False)
  canonical_decode.verify_canonical_decode_empty_semantics_v1(pre, witness)
}

test empty_refuses_same_body_with_different_source_commitment() {
  let Pair(pre, witness) = fixture(1, True)
  !canonical_decode.verify_canonical_decode_empty_semantics_v1(pre, witness)
}

test item_binds_authenticated_source() {
  let Pair(pre, witness) = fixture(0, False)
  let source = canonical_decode.bind_canonical_decode_item_source_v1(
    pre,
    witness,
  )
  source.expected_field_length > 1
}

test item_refuses_same_body_with_different_source_commitment() fail {
  let Pair(pre, witness) = fixture(0, True)
  let source = canonical_decode.bind_canonical_decode_item_source_v1(
    pre,
    witness,
  )
  source.expected_field_length > 1
}
