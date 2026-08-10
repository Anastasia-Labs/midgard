use aiken/cbor
use cardano/address
use cardano/transaction.{NoDatum, OutputReference}
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  MidgardTxInput, TxIsValid, native_tx_version_v1,
}
use midgard/ledger_state.{
  CekProgramMaterialDatumV1, ForcedInclusionTxV1, NativeTxProofSourceV1,
  TxOrderEventV1, TxOrderPayloadV1,
}
use midgard/transition_trace
use midgard/user_events
use midgard/user_events/tx_order_v1.{
  Datum, SpendRedeemer, forced_inclusion_key_value, verify_order_material,
}

const order_transaction_id =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const commitment_transaction_id =
  #"5555555555555555555555555555555555555555555555555555555555555555"

const input_transaction_id =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const signer = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

fn vector_order_id() -> OutputReference {
  OutputReference {
    transaction_id: #"3333333333333333333333333333333333333333333333333333333333333333",
    output_index: 4,
  }
}

fn vector_source() -> NativeTxProofSourceV1 {
  NativeTxProofSourceV1 {
    compact_cbor: #"01",
    witness_set_compact_cbor: #"0203",
    field_preimage_lengths_cbor: #"04",
  }
}

fn vector_payload() -> TxOrderPayloadV1 {
  TxOrderPayloadV1 {
    tx_id: order_transaction_id,
    transaction_commitment: commitment_transaction_id,
    source: vector_source(),
  }
}

fn vector_event() -> TxOrderEventV1 {
  TxOrderEventV1 { id: vector_order_id(), tx: vector_payload() }
}

fn vector_datum() -> Datum {
  user_events.OptimisticDatum {
    event: vector_event(),
    inclusion_time: 123,
    witness: #"66666666666666666666666666666666666666666666666666666666",
    refund_address: address.from_verification_key(
      #"77777777777777777777777777777777777777777777777777777777",
    ),
    refund_datum: NoDatum,
  }
}

fn vector_forced_inclusion() -> ForcedInclusionTxV1 {
  ForcedInclusionTxV1 {
    tx_id: order_transaction_id,
    source: vector_source(),
    operator_validity: ledger_state.FailedScript,
  }
}

fn vector_spend_redeemer() -> SpendRedeemer {
  SpendRedeemer {
    input_index: 0,
    output_index: 1,
    hub_ref_input_index: 2,
    settlement_ref_input_index: 3,
    burn_redeemer_index: 4,
    membership_proof: transition_trace.RootMembershipProof {
      domain: transition_trace.ForcedTransactionsV1RootDomain,
      root: zero,
      phas_root: #"1111111111111111111111111111111111111111111111111111111111111111",
      count: 1,
      key: cbor.serialise(vector_order_id()),
      value: cbor.serialise(vector_forced_inclusion()),
      proof: [],
    },
    inclusion_proof_script_withdraw_redeemer_index: 5,
    validity_override: ledger_state.FailedScript,
  }
}

fn empty_v1_transaction() -> MidgardTransaction {
  let empty_mint: Data = []
  MidgardTransaction {
    version: native_tx_version_v1,
    validity: TxIsValid,
    body: MidgardTransactionBody {
      inputs: [],
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

fn sample_v1_transaction(include_signer: Bool) -> MidgardTransaction {
  let empty = empty_v1_transaction()
  MidgardTransaction {
    ..empty,
    body: MidgardTransactionBody {
      ..empty.body,
      inputs: if include_signer {
        []
      } else {
        [MidgardTxInput { tx_id: input_transaction_id, output_index: 3 }]
      },
      required_signers: if include_signer {
        [signer]
      } else {
        []
      },
    },
  }
}

fn tx_id(tx: MidgardTransaction) -> ByteArray {
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
}

fn source(tx: MidgardTransaction) -> NativeTxProofSourceV1 {
  let proof_source = native_transaction.midgard_transaction_proof_source_v1(tx)
  NativeTxProofSourceV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
  }
}

fn commitment(tx: MidgardTransaction) -> ByteArray {
  native_transaction.midgard_transaction_proof_commitment_v1(tx)
}

fn payload(tx: MidgardTransaction) -> TxOrderPayloadV1 {
  TxOrderPayloadV1 {
    tx_id: tx_id(tx),
    transaction_commitment: commitment(tx),
    source: source(tx),
  }
}

test tx_order_v1_matches_the_canonical_typescript_abi_vectors() {
  and {
    cbor.serialise(vector_order_id()) == #"d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff",
    cbor.serialise(vector_payload()) == #"d8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffff",
    cbor.serialise(vector_event()) == #"d8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333304ffd8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffffff",
    cbor.serialise(vector_datum()) == #"d8799fd8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333304ffd8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffffff187b581c66666666666666666666666666666666666666666666666666666666d8799fd8799f581c77777777777777777777777777777777777777777777777777777777ffd87a80ffd87980ff",
    cbor.serialise(vector_forced_inclusion()) == #"d8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87c80ff",
    cbor.serialise(vector_spend_redeemer()) == #"d8799f0001020304d8799fd87a805820000000000000000000000000000000000000000000000000000000000000000058201111111111111111111111111111111111111111111111111111111111111111015827d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff5834d8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87c80ff80ff05d87c80ff",
  }
}

test forced_inclusion_key_value_matches_the_canonical_v1_vectors() {
  let key, value <- forced_inclusion_key_value(
    tx_order_event_data: vector_event(),
    validity_override: ledger_state.FailedScript,
  )
  and {
    cbor.serialise(key) == #"d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff",
    cbor.serialise(value) == #"d8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87c80ff",
  }
}

test tx_order_payload_rejects_an_unknown_outer_constructor() fail {
  expect Some(data) = cbor.deserialise(
    #"d87a9f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffff",
  )
  expect payload: TxOrderPayloadV1 = data
  payload.tx_id == order_transaction_id
}

test tx_order_payload_rejects_an_extra_field() fail {
  expect Some(data) = cbor.deserialise(
    #"d8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ff00ff",
  )
  expect payload: TxOrderPayloadV1 = data
  payload.tx_id == order_transaction_id
}

test cek_program_material_v1_matches_the_typed_blob_chunk_hash_vector() {
  let chunk = #"6d6174657269616c"
  let preimage = #"486d6174657269616c"
  let root = cek_proof_v1.hash_blob_chunk_v1(chunk)
  let datum = CekProgramMaterialDatumV1 { kind: 3, root, preimage }
  and {
    root == #"941de596141f044be570fb7b579b3fc520db7cacdbccd020cdb7618ba124380c",
    cbor.serialise(datum) == #"d8799f035820941de596141f044be570fb7b579b3fc520db7cacdbccd020cdb7618ba124380c49486d6174657269616cff",
  }
}

/// The one material state a forced order can be authenticated in today — a
/// strict narrowing of what the retired counted receipt chain admitted. That
/// chain's mint policy gated on a per-item opening that is unsatisfiable for any
/// payload whose commitments are the §4 flat hashes of real material, but
/// satisfiable by a payload declaring counted roots instead; this predicate drops
/// that declaring-payload path as well. See `verify_order_material`'s
/// `field_carriage_availability` note for the §8.6-certified re-expression and
/// the issue that owns it.
test empty_material_activates_the_order() {
  verify_order_material(payload(empty_v1_transaction()))
}

/// Fails closed on a body field (0 — spend inputs) and on a witness-set field
/// (4 — required signers), because the nine commitments are read from two
/// different committed structures and a check that only covered one of them
/// would pass a transaction whose material lives in the other.
test material_bearing_order_fails_closed_on_a_body_field() fail {
  verify_order_material(payload(sample_v1_transaction(False)))
}

test material_bearing_order_fails_closed_on_a_signer_field() fail {
  verify_order_material(payload(sample_v1_transaction(True)))
}
