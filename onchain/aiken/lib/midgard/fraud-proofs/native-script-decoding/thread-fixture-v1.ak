//// Test fixtures for the decoding-fault thread family (#635, #633): crafted
//// reference-script items, resolved-output descriptors, single-leaf tries
//// for the committed-claim roots and the pre-state ledger, and L1 step
//// transactions. Test-only — never referenced by a validator.

use aiken/cbor
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address
use cardano/transaction.{InlineDatum, OutputReference, Transaction, placeholder}
use env
use midgard/bounded_item_v1
use midgard/cek_data_v1
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_output_commitment_v1.{LedgerOutputCommitmentV1}
use midgard/ledger_state
use midgard/rejection_reason_v1
use midgard/transition_trace
use midgard/validation_merkle_v1

pub const h28_y = #"99999999999999999999999999999999999999999999999999999999"

pub const signer_key_hash =
  #"99999999999999999999999999999999999999999999999999999999"

pub const h32_c =
  #"5555555555555555555555555555555555555555555555555555555555555555"

pub const h32_d =
  #"6666666666666666666666666666666666666666666666666666666666666666"

const order_key_tx_id =
  #"7777777777777777777777777777777777777777777777777777777777777777"

// ## Reference-script items
//
// Every item is the full versioned wrapper `[language, payload-bytes]` in
// canonical CBOR, exactly as committed by
// `reference_script_item_commitment` / `reference_script_total_length`.

/// `[0, h'8200581c<28-byte key>']` — a single canonical signature node. Scans
/// to the exact terminal in two primitive steps (token, finalize).
pub fn signature_script_item_v1() -> ByteArray {
  bytearray.concat(#"82005820", signature_node_bytes_v1())
}

/// The 32-byte canonical signature-node token.
pub fn signature_node_bytes_v1() -> ByteArray {
  bytearray.concat(#"8200581c", signer_key_hash)
}

/// `[0, h'8201828200581c<key>8200581c<key>']` — `all [sig, sig]`. Scans to
/// the terminal in six primitive steps (container token, two child tokens
/// with a frame step after each, finalize).
pub fn all_of_two_signatures_item_v1() -> ByteArray {
  let sig = signature_node_bytes_v1()
  let payload = #"820182"
    |> bytearray.concat(sig)
    |> bytearray.concat(sig)
  bytearray.concat(#"82005843", payload)
}

/// `[0, h'820700']` — well-formed wrapper, but the payload's single token
/// carries tag 7: the frozen token step refuses with `Invalid` (class 0).
pub fn malformed_payload_item_v1() -> ByteArray {
  #"820043820700"
}

/// `[1, h'0a']` — the wrapper itself is undecodable (language 1 is outside
/// {0, 3, 128}): `bind_machine_v1` reports `MachineBindMalformedV1`.
pub fn malformed_wrapper_item_v1() -> ByteArray {
  #"8201410a"
}

/// `[0, h'']` — tag-0 with an empty payload: the frozen machine's
/// `InvalidReferenceScript` precedent, `MachineBindMalformedV1` at bind.
pub fn empty_payload_item_v1() -> ByteArray {
  #"820040"
}

/// `[3, h'01020304']` — a (dummy) PlutusV3 wrapper: `MachineBindNonNativeV1`.
pub fn plutus_script_item_v1() -> ByteArray {
  #"82034401020304"
}

// ## Resolved-output descriptors

/// A well-formed `LedgerOutputCommitmentV1` whose reference-script fields
/// commit `item_bytes` under `language`. Fields this family never opens
/// carry inert canonical dummies (the address and zero-asset frontier are the
/// canonical cross-language vector's).
pub fn scan_descriptor_v1(
  output_index: Int,
  language: Int,
  item_bytes: ByteArray,
) -> LedgerOutputCommitmentV1 {
  LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index,
    total_length: 5000,
    item_commitment: h32_c,
    address: #"6011111111111111111111111111111111111111111111111111111111",
    lovelace: 5_000_000,
    asset_count: 0,
    asset_frontier_commitment: #"b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd",
    cardano_value_size: 5,
    reference_script_language: language,
    reference_script_hash: if language == -1 {
      #""
    } else {
      h28_y
    },
    reference_script_total_length: if language == -1 {
      0
    } else {
      bytearray.length(item_bytes)
    },
    reference_script_item_commitment: if language == -1 {
      #""
    } else {
      bounded_item_v1.from_bytes(
        ledger_output_commitment_v1.output_field_index,
        output_index,
        item_bytes,
      )
    },
    cardano_tx_out: cek_data_v1.DataSummaryV1 {
      root: h32_c,
      cbor_length: 101,
      memory: 202,
    },
    midgard_tx_out: cek_data_v1.DataSummaryV1 {
      root: h32_c,
      cbor_length: 103,
      memory: 204,
    },
    cardano_spend_datum: cek_data_v1.DataSummaryV1 {
      root: h32_c,
      cbor_length: 3,
      memory: 4,
    },
  }
}

/// The chunk-0 proof for an item that fits one chunk (every fixture item
/// does), in the reference-script commitment domain.
pub fn single_chunk_proof_v1(
  output_index: Int,
  item_bytes: ByteArray,
) -> bounded_item_v1.ChunkProofV1 {
  bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: ledger_output_commitment_v1.output_field_index,
    item_index: output_index,
    total_length: bytearray.length(item_bytes),
    chunk_index: 0,
    chunk: item_bytes,
    frontier: validation_merkle_v1.append_leaf(
      0,
      [],
      bounded_item_v1.hash_chunk(
        ledger_output_commitment_v1.output_field_index,
        output_index,
        0,
        item_bytes,
      ),
    ),
    siblings: [],
  }
}

// ## Single-leaf tries

/// The pre-state ledger holding exactly one resolved output: the accused
/// outpoint's canonical key mapped to the descriptor's canonical bytes.
pub fn single_leaf_ledger_root_v1(
  key_bytes: ByteArray,
  descriptor_cbor: ByteArray,
) -> ByteArray {
  mpf.root(mpf.insert(mpf.empty, key_bytes, descriptor_cbor, []))
}

// ## Subject transactions and forced leaves

/// The subject transaction whose field 0 / field 1 the bind step opens: the
/// shared binding fixture's canonical transaction (one spend input, one
/// reference input), with the leaf's validity scalar as required.
pub fn subject_tx_v1(
  validity: native_types.MidgardTxValidity,
) -> native_types.MidgardTransaction {
  native_types.MidgardTransaction { ..fixture.valid_native_tx_v1(), validity }
}

pub fn native_tx_id_of_v1(tx: native_types.MidgardTransaction) -> ByteArray {
  fixture.native_block_fixture_v1(tx).tx_id
}

pub fn proof_source_of_v1(
  tx: native_types.MidgardTransaction,
) -> ledger_state.NativeTxProofSourceV1 {
  let proof_source = native_transaction.midgard_transaction_proof_source_v1(tx)
  ledger_state.NativeTxProofSourceV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
  }
}

pub fn forced_order_key_v1() -> ledger_state.TxOrderId {
  OutputReference { transaction_id: order_key_tx_id, output_index: 0 }
}

pub fn forced_leaf_v1(
  tx: native_types.MidgardTransaction,
  verdict: rejection_reason_v1.OperatorVerdictV1,
) -> ledger_state.ForcedInclusionTxV1 {
  ledger_state.ForcedInclusionTxV1 {
    tx_id: native_tx_id_of_v1(tx),
    source: proof_source_of_v1(tx),
    verdict,
  }
}

pub fn forced_leaf_membership_v1(
  order_key: ledger_state.TxOrderId,
  leaf: ledger_state.ForcedInclusionTxV1,
) -> transition_trace.RootMembershipProof<
  ledger_state.TxOrderId,
  ledger_state.ForcedInclusionTxV1,
> {
  let key_bytes = cbor.serialise(order_key)
  let value_bytes = cbor.serialise(leaf)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.ForcedTransactionsV1RootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.ForcedTransactionsV1RootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: order_key,
    value: leaf,
    proof: [],
  }
}

// ## Committed-claim fixture (header + trace openings)

pub type ThreadClaimFixtureV1 {
  header: ledger_state.HeaderV1,
  event_to_step_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  transition_step_membership: transition_trace.RootMembershipProof<
    Int,
    ledger_state.TransitionStep,
  >,
}

/// A header whose transition-trace and event-to-step roots each hold exactly
/// the thread's event as step 0, with `pre_utxos_root` set to the given
/// pre-state ledger root. `forced_root` (already counted-committed) and its
/// count land in the header when the event is forced.
pub fn thread_claim_v1(
  event_key: ledger_state.EventKey,
  phase: ledger_state.TransitionPhase,
  prior_ledger_root: ByteArray,
  forced_root: Option<ByteArray>,
) -> ThreadClaimFixtureV1 {
  let step = ledger_state.TransitionStep {
    schema_version: ledger_state.transition_step_schema_version_v1,
    step_index: 0,
    event_key,
    phase,
    pre_utxos_root: prior_ledger_root,
    post_utxos_root: h32_d,
  }
  let event_to_step = ledger_state.EventToStepValue { step_index: 0, phase }
  let step_phas_root = mpf.root(
    mpf.insert(mpf.empty, cbor.serialise(0), cbor.serialise(step), []),
  )
  let event_phas_root = mpf.root(
    mpf.insert(
      mpf.empty,
      cbor.serialise(event_key),
      cbor.serialise(event_to_step),
      [],
    ),
  )
  let header = ledger_state.HeaderV1 {
    prev_utxos_root: h32_c,
    utxos_root: h32_d,
    withdrawals_root: env.empty_merkle_tree_root,
    forced_transactions_root: when forced_root is {
      Some(root) -> root
      None -> env.empty_merkle_tree_root
    },
    transactions_root: env.empty_merkle_tree_root,
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: transition_trace.commit_counted_root(
      transition_trace.TransitionTraceRootDomain,
      step_phas_root,
      1,
    ),
    event_to_step_root: transition_trace.commit_counted_root(
      transition_trace.EventToStepRootDomain,
      event_phas_root,
      1,
    ),
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count: 0,
    forced_transaction_count: when forced_root is {
      Some(_) -> 1
      None -> 0
    },
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 1,
    transition_step_count: 1,
    validation_trace_count: 0,
    start_time: 10,
    end_time: 20,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: h28_y,
    operator_vkey: h28_y,
    protocol_version: ledger_state.protocol_version_v1,
  }
  ThreadClaimFixtureV1 {
    header,
    event_to_step_membership: transition_trace.RootMembershipProof {
      domain: transition_trace.EventToStepRootDomain,
      root: header.event_to_step_root,
      phas_root: event_phas_root,
      count: 1,
      key: event_key,
      value: event_to_step,
      proof: [],
    },
    transition_step_membership: transition_trace.RootMembershipProof {
      domain: transition_trace.TransitionTraceRootDomain,
      root: header.transition_trace_root,
      phas_root: step_phas_root,
      count: 1,
      key: 0,
      value: step,
      proof: [],
    },
  }
}

/// The thread NFT asset name a header hashes to.
pub fn header_asset_name_v1(header: ledger_state.HeaderV1) -> ByteArray {
  let header_data: Data = header
  bytearray.concat(
    fixture.fraud_category_id,
    utils.serialise_and_hash_28(header_data),
  )
}

// ## Field openings

pub fn subject_spend_field_items_v1(
  tx: native_types.MidgardTransaction,
) -> List<ByteArray> {
  tx.body.inputs |> list.map(encode_midgard_tx_input)
}

pub fn subject_reference_field_items_v1(
  tx: native_types.MidgardTransaction,
) -> List<ByteArray> {
  tx.body.reference_inputs |> list.map(encode_midgard_tx_input)
}

// ## L1 step transactions

/// A mid-step L1 transaction whose thread output stays at the OWN step
/// address (step-03's self-loop), unlike the shared fixture's
/// `continue_mid_step_l1_tx_v1` which advances to the next step's address.
pub fn self_loop_l1_tx_v1(
  asset_name: ByteArray,
  output_state_data: Data,
) -> Transaction {
  let output_datum: ct.StepDatum<Data> = ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(output_state_data),
  }
  let output_datum_data: Data = output_datum
  Transaction {
    ..placeholder,
    inputs: [fixture.ct_thread_input(asset_name)],
    outputs: [
      transaction.Output {
        address: address.from_script(fixture.step_script_hash),
        value: fixture.ct_token_value(asset_name),
        datum: InlineDatum(output_datum_data),
        reference_script: None,
      },
    ],
  }
}
