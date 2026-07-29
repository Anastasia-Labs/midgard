use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/bounded_collection_v1.{ItemProofV1, hash_item}
use midgard/bounded_item_v1.{ChunkProofV1, from_bytes, hash_chunk}
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_tx_input,
  encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/preimages.{
  encode_address_witness_preimage, encode_script_witness_preimage,
}
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionWitnessSet, MidgardTxInput, MidgardVersionedScript,
  NativeCardanoScript, TxIsValid, native_tx_version_v1,
}
use midgard/ledger_state.{
  NativeTxProofSourceV1, TxFieldPreimageV1, TxFieldReceiptV1, TxOrderPayloadV1,
}
use midgard/user_events
use midgard/user_events/tx_order_v1.{
  field_fragment_burn_authorized, field_receipt_asset_name,
  field_receipt_burn_authorized, verify_order_receipts,
  verify_receipt_chain_link,
}
use midgard/validation_merkle_v1.{FrontierPeak}

const tx_order_policy_id =
  #"11111111111111111111111111111111111111111111111111111111"

const field_receipt_policy_id =
  #"22222222222222222222222222222222222222222222222222222222"

const receipt_script_hash =
  #"33333333333333333333333333333333333333333333333333333333"

const other_script_hash =
  #"77777777777777777777777777777777777777777777777777777777"

const order_transaction_id =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const field_transaction_id =
  #"5555555555555555555555555555555555555555555555555555555555555555"

const receipt_transaction_id =
  #"8888888888888888888888888888888888888888888888888888888888888888"

const input_transaction_id =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const signer = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

fn order_id() -> OutputReference {
  OutputReference { transaction_id: order_transaction_id, output_index: 7 }
}

fn field_reference(index: Int) -> OutputReference {
  OutputReference { transaction_id: field_transaction_id, output_index: index }
}

fn receipt_reference(index: Int) -> OutputReference {
  OutputReference {
    transaction_id: receipt_transaction_id,
    output_index: index,
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

fn sample_input() -> MidgardTxInput {
  MidgardTxInput { tx_id: input_transaction_id, output_index: 3 }
}

fn sample_v1_transaction(include_signer: Bool) -> MidgardTransaction {
  let empty = empty_v1_transaction()
  MidgardTransaction {
    ..empty,
    body: MidgardTransactionBody {
      ..empty.body,
      inputs: [sample_input()],
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

fn payload(
  tx: MidgardTransaction,
  terminal_receipt_reference: Option<OutputReference>,
) -> TxOrderPayloadV1 {
  TxOrderPayloadV1 {
    tx_id: tx_id(tx),
    transaction_commitment: commitment(tx),
    source: source(tx),
    terminal_receipt_reference,
  }
}

fn collection_proof(field_index: Int, item: ByteArray) -> ItemProofV1 {
  let item_commitment = from_bytes(field_index, 0, item)
  let leaf = hash_item(field_index, 0, bytearray.length(item), item_commitment)
  ItemProofV1 {
    version: 1,
    field_index,
    item_count: 1,
    item_index: 0,
    item_length: bytearray.length(item),
    item_commitment,
    frontier: [FrontierPeak { height: 0, hash: leaf }],
    siblings: [],
  }
}

fn input_receipt(
  tx: MidgardTransaction,
  predecessor_receipt_reference: Option<OutputReference>,
) -> TxFieldReceiptV1 {
  TxFieldReceiptV1 {
    field_receipt_policy_id,
    tx_order_policy_id,
    tx_order_id: order_id(),
    transaction_commitment: commitment(tx),
    collection_proof: collection_proof(
      0,
      encode_midgard_tx_input(sample_input()),
    ),
    chunk_index: 0,
    field_reference: field_reference(0),
    predecessor_receipt_reference,
    field_encoded_size: 39,
  }
}

fn signer_receipt(
  tx: MidgardTransaction,
  predecessor_receipt_reference: Option<OutputReference>,
) -> TxFieldReceiptV1 {
  TxFieldReceiptV1 {
    field_receipt_policy_id,
    tx_order_policy_id,
    tx_order_id: order_id(),
    transaction_commitment: commitment(tx),
    collection_proof: collection_proof(4, signer),
    chunk_index: 0,
    field_reference: field_reference(1),
    predecessor_receipt_reference,
    field_encoded_size: 31,
  }
}

fn receipt_input(
  reference: OutputReference,
  receipt: TxFieldReceiptV1,
  script_hash: ByteArray,
) -> Input {
  let proof = receipt.collection_proof
  let asset_name = field_receipt_asset_name(
    receipt.tx_order_policy_id,
    receipt.tx_order_id,
    receipt.transaction_commitment,
    proof.field_index,
    proof.item_index,
    receipt.chunk_index,
  )
  let receipt_data: Data = receipt
  Input {
    output_reference: reference,
    output: Output {
      address: address.from_script(script_hash),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(field_receipt_policy_id, asset_name, 1),
      datum: InlineDatum(receipt_data),
      reference_script: None,
    },
  }
}

test field_receipt_asset_name_matches_core_vector() {
  field_receipt_asset_name(tx_order_policy_id, order_id(), zero, 0, 0, 0) == #"7bc0ae911756007cc44a1e956fedc08d67af38ce19778405ab040faa77b60123"
}

test empty_material_activates_without_a_terminal_receipt() {
  let tx = empty_v1_transaction()
  verify_order_receipts(
    [],
    receipt_script_hash,
    field_receipt_policy_id,
    tx_order_policy_id,
    order_id(),
    payload(tx, None),
  )
}

test non_empty_material_without_a_terminal_receipt_fails_closed() fail {
  let tx = sample_v1_transaction(False)
  verify_order_receipts(
    [],
    receipt_script_hash,
    field_receipt_policy_id,
    tx_order_policy_id,
    order_id(),
    payload(tx, None),
  )
}

test first_receipt_and_terminal_receipt_authenticate_one_item_material() {
  let tx = sample_v1_transaction(False)
  let receipt = input_receipt(tx, None)
  and {
    verify_receipt_chain_link(
      tx_id(tx),
      commitment(tx),
      source(tx),
      field_receipt_policy_id,
      tx_order_policy_id,
      order_id(),
      None,
      receipt,
    ),
    verify_order_receipts(
      [receipt_input(receipt_reference(0), receipt, receipt_script_hash)],
      receipt_script_hash,
      field_receipt_policy_id,
      tx_order_policy_id,
      order_id(),
      payload(tx, Some(receipt_reference(0))),
    ),
  }
}

test later_non_empty_field_links_to_the_authenticated_predecessor() {
  let tx = sample_v1_transaction(True)
  let previous = input_receipt(tx, None)
  let current = signer_receipt(tx, Some(receipt_reference(0)))
  and {
    verify_receipt_chain_link(
      tx_id(tx),
      commitment(tx),
      source(tx),
      field_receipt_policy_id,
      tx_order_policy_id,
      order_id(),
      None,
      previous,
    ),
    verify_receipt_chain_link(
      tx_id(tx),
      commitment(tx),
      source(tx),
      field_receipt_policy_id,
      tx_order_policy_id,
      order_id(),
      Some(previous),
      current,
    ),
    verify_order_receipts(
      [receipt_input(receipt_reference(1), current, receipt_script_hash)],
      receipt_script_hash,
      field_receipt_policy_id,
      tx_order_policy_id,
      order_id(),
      payload(tx, Some(receipt_reference(1))),
    ),
  }
}

test script_field_six_links_to_byte_wrapped_address_field_seven() {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes: #"820400",
  }
  let address_witness = MidgardAddressWitness {
    verification_key: #"1111111111111111111111111111111111111111111111111111111111111111",
    signature: #"22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      addr_tx_wits: [address_witness],
      script_tx_wits: [script],
    },
  }
  let script_item = encode_midgard_versioned_script(script)
  let address_item = encode_midgard_address_witness(address_witness)
  let script_receipt = TxFieldReceiptV1 {
    field_receipt_policy_id,
    tx_order_policy_id,
    tx_order_id: order_id(),
    transaction_commitment: commitment(tx),
    collection_proof: collection_proof(6, script_item),
    chunk_index: 0,
    field_reference: field_reference(6),
    predecessor_receipt_reference: None,
    field_encoded_size: bytearray.length(
      encode_script_witness_preimage([script]),
    ),
  }
  let address_receipt = TxFieldReceiptV1 {
    field_receipt_policy_id,
    tx_order_policy_id,
    tx_order_id: order_id(),
    transaction_commitment: commitment(tx),
    collection_proof: collection_proof(7, address_item),
    chunk_index: 0,
    field_reference: field_reference(7),
    predecessor_receipt_reference: Some(receipt_reference(0)),
    field_encoded_size: bytearray.length(
      encode_address_witness_preimage([address_witness]),
    ),
  }
  and {
    verify_receipt_chain_link(
      tx_id(tx),
      commitment(tx),
      source(tx),
      field_receipt_policy_id,
      tx_order_policy_id,
      order_id(),
      None,
      script_receipt,
    ),
    verify_receipt_chain_link(
      tx_id(tx),
      commitment(tx),
      source(tx),
      field_receipt_policy_id,
      tx_order_policy_id,
      order_id(),
      Some(script_receipt),
      address_receipt,
    ),
  }
}

test later_field_without_its_predecessor_fails_closed() fail {
  let tx = sample_v1_transaction(True)
  verify_receipt_chain_link(
    tx_id(tx),
    commitment(tx),
    source(tx),
    field_receipt_policy_id,
    tx_order_policy_id,
    order_id(),
    None,
    signer_receipt(tx, None),
  )
}

test malformed_encoded_size_fails_closed() fail {
  let tx = sample_v1_transaction(False)
  let malformed = TxFieldReceiptV1 {
    ..input_receipt(tx, None),
    field_encoded_size: 42,
  }
  verify_receipt_chain_link(
    tx_id(tx),
    commitment(tx),
    source(tx),
    field_receipt_policy_id,
    tx_order_policy_id,
    order_id(),
    None,
    malformed,
  )
}

test terminal_receipt_locked_by_another_script_fails_closed() fail {
  let tx = sample_v1_transaction(False)
  let receipt = input_receipt(tx, None)
  verify_order_receipts(
    [receipt_input(receipt_reference(0), receipt, other_script_hash)],
    receipt_script_hash,
    field_receipt_policy_id,
    tx_order_policy_id,
    order_id(),
    payload(tx, Some(receipt_reference(0))),
  )
}

test fragment_spend_is_bound_to_exact_order_nft_burn() {
  let proof = collection_proof(0, encode_midgard_tx_input(sample_input()))
  let field = TxFieldPreimageV1 {
    field_receipt_policy_id,
    tx_order_policy_id,
    tx_order_id: order_id(),
    transaction_commitment: blake2b_256(#""),
    collection_proof: proof,
    proof: ChunkProofV1 {
      version: 1,
      field_index: 0,
      item_index: 0,
      total_length: proof.item_length,
      chunk_index: 0,
      chunk: encode_midgard_tx_input(sample_input()),
      frontier: [
        FrontierPeak {
          height: 0,
          hash: hash_chunk(0, 0, 0, encode_midgard_tx_input(sample_input())),
        },
      ],
      siblings: [],
    },
  }
  let nonce_asset_name = user_events.out_ref_to_nonce(field.tx_order_id)
  let receipt_asset_name = field_receipt_asset_name(
    field.tx_order_policy_id,
    field.tx_order_id,
    field.transaction_commitment,
    field.proof.field_index,
    field.proof.item_index,
    field.proof.chunk_index,
  )
  let complete_burn = assets.from_asset(
    tx_order_policy_id,
    nonce_asset_name,
    -1,
  )
    |> assets.add(field_receipt_policy_id, receipt_asset_name, -1)
  and {
    field_fragment_burn_authorized(field, complete_burn),
    !field_fragment_burn_authorized(
      field,
      assets.from_asset(tx_order_policy_id, nonce_asset_name, -1),
    ),
  }
}

test receipt_spend_is_bound_to_exact_order_and_receipt_burns() {
  let tx = sample_v1_transaction(False)
  let receipt = input_receipt(tx, None)
  let nonce_asset_name = user_events.out_ref_to_nonce(receipt.tx_order_id)
  let proof = receipt.collection_proof
  let receipt_asset_name = field_receipt_asset_name(
    receipt.tx_order_policy_id,
    receipt.tx_order_id,
    receipt.transaction_commitment,
    proof.field_index,
    proof.item_index,
    receipt.chunk_index,
  )
  let complete_burn = assets.from_asset(
    tx_order_policy_id,
    nonce_asset_name,
    -1,
  )
    |> assets.add(field_receipt_policy_id, receipt_asset_name, -1)
  and {
    field_receipt_burn_authorized(receipt, complete_burn),
    !field_receipt_burn_authorized(
      receipt,
      assets.from_asset(field_receipt_policy_id, receipt_asset_name, -1),
    ),
  }
}
