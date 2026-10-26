use aiken/collection/list
use aiken/crypto.{blake2b_224}
use aiken/primitive/bytearray
use midgard/fixed_item_legality_v1.{fixed_item_is_legal_v1}
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/transaction as transaction
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionWitnessSet, MidgardTxInput,
}
use midgard/native_tx_field_access_v1 as access
use midgard/rejection_reason_v1.{FieldItemWidthIllegal}
use midgard/validation_machine/canonical_decode as canonical
use midgard/validation_machine/machine_types as machine
use midgard/validation_machine/witness as work
use midgard/validation_machine_tests/constants.{
  h_a, h_b, validation_context_cbor,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  empty_v1_transaction, exact_rejection_post, forced_pending_state,
  inline_field_door, submitted_source,
}
use midgard/validation_trace_v1 as vm_trace

/// The ordered lists a mode authors: spend/reference inputs and address
/// witnesses (see `order_step_via` for the modes).
fn order_lists(
  mode: Int,
) -> Pair<List<MidgardTxInput>, List<MidgardAddressWitness>> {
  let first = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let second = MidgardTxInput { tx_id: h_b, output_index: 0 }
  let inputs = if mode == 0 {
    [first, second]
  } else if mode == 1 {
    [second, first]
  } else {
    [first, first]
  }
  let signature = bytearray.concat(h_a, h_a)
  let address_a = MidgardAddressWitness { verification_key: h_a, signature }
  let address_b = MidgardAddressWitness { verification_key: h_b, signature }
  let ordered_addresses = if bytearray.compare(
    blake2b_224(h_a),
    blake2b_224(h_b),
  ) == Less {
    [address_a, address_b]
  } else {
    [address_b, address_a]
  }
  let address_a_late = MidgardAddressWitness {
    verification_key: h_a,
    signature: bytearray.concat(h_a, h_b),
  }
  let same_signer = if bytearray.compare(signature, address_a_late.signature) == Less {
    [address_a, address_a_late]
  } else {
    [address_a_late, address_a]
  }
  let addresses = if mode == 0 {
    ordered_addresses
  } else if mode == 1 {
    when ordered_addresses is {
      [a, b] -> [b, a]
      _ -> fail
    }
  } else if mode == 3 {
    same_signer
  } else if mode == 4 {
    when same_signer is {
      [a, b] -> [b, a]
      _ -> fail
    }
  } else {
    [address_a, address_a]
  }
  Pair(inputs, addresses)
}

fn order_step(field_index: Int, mode: Int, reject: Bool) -> Bool {
  order_step_via(field_index, mode, reject, False)
}

/// Modes: 0 sorted, 1 descending, 2 exact repeat; field 7 only: 3 one signer
/// with ascending item bytes, 4 one signer with descending item bytes.
fn order_step_via(
  field_index: Int,
  mode: Int,
  reject: Bool,
  chunk_route: Bool,
) -> Bool {
  let first = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let Pair(inputs, addresses) = order_lists(mode)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: if field_index == 0 {
        inputs
      } else {
        [first]
      },
      reference_inputs: if field_index == 1 {
        inputs
      } else {
        []
      },
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      addr_tx_wits: if field_index == 7 {
        addresses
      } else {
        []
      },
    },
  }
  let source = submitted_source(tx)
  let preimage = access.encode_field_preimage(
    if field_index == 7 {
      list.map(addresses, encode_midgard_address_witness)
    } else {
      list.map(inputs, encode_midgard_tx_input)
    },
  )
  let item_length = if field_index == 7 {
    access.address_witness_item_bytes
  } else {
    access.spend_input_item_bytes
  }
  let current = work.encode_transaction_field_scan_witness(
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
    validation_context_cbor,
    field_index,
    1,
    0,
    2,
    1 + 2 + item_length,
  )
  let tx_id = native_tx_id_for_version(
    1,
    encode_native_tx_body_compact(
      transaction.midgard_transaction_body_to_compact(tx.body),
    ),
  )
  let pre = forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    vm_trace.CanonicalDecode,
    40,
    current,
  )
  let next_work = work.encode_transaction_field_scan_witness(
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
    validation_context_cbor,
    field_index + 1,
    0,
    0,
    -1,
    0,
  )
  let accepted = vm_trace.ValidationMachineStateV1 {
    ..pre,
    program_counter: 41,
    work_root: vm_trace.hash_work_witness(
      vm_trace.CanonicalDecode,
      41,
      next_work,
    ),
  }
  let step = machine.ValidationOneStepWitnessV1 {
    work_witness_cbor: current,
    claimed_successor: if reject {
      exact_rejection_post(
        pre,
        FieldItemWidthIllegal { field_index, item_index: 1 },
      )
    } else {
      accepted
    },
  }
  if chunk_route {
    canonical.verify_canonical_decode_chunk_semantics_v1(
      pre,
      step,
      inline_field_door(),
      field_index,
      1,
      access.Inline { preimage },
    )
  } else {
    canonical.verify_canonical_decode_item_from_source_v1(
      pre,
      step,
      canonical.bind_canonical_decode_item_source_v1(pre, step),
      inline_field_door(),
      access.Inline { preimage },
    )
  }
}

test field_0_sorted_accepts() {
  order_step(0, 0, False)
}

test field_0_sorted_refuses_rejection() {
  !order_step(0, 0, True)
}

test field_0_descending_rejects() {
  order_step(0, 1, True)
}

test field_0_descending_refuses_acceptance() {
  !order_step(0, 1, False)
}

test field_0_duplicate_accepts() {
  order_step(0, 2, False)
}

test field_0_duplicate_refuses_rejection() {
  !order_step(0, 2, True)
}

test field_1_sorted_accepts() {
  order_step(1, 0, False)
}

test field_1_sorted_refuses_rejection() {
  !order_step(1, 0, True)
}

test field_1_descending_rejects() {
  order_step(1, 1, True)
}

test field_1_descending_refuses_acceptance() {
  !order_step(1, 1, False)
}

test field_1_duplicate_accepts() {
  order_step(1, 2, False)
}

test field_1_duplicate_refuses_rejection() {
  !order_step(1, 2, True)
}

test field_7_sorted_accepts() {
  order_step(7, 0, False)
}

test field_7_sorted_refuses_rejection() {
  !order_step(7, 0, True)
}

test field_7_descending_rejects() {
  order_step(7, 1, True)
}

test field_7_descending_refuses_acceptance() {
  !order_step(7, 1, False)
}

test field_7_duplicate_rejects() {
  order_step(7, 2, True)
}

test field_7_duplicate_refuses_acceptance() {
  !order_step(7, 2, False)
}

test field_7_one_signer_ascending_bytes_accepts() {
  order_step(7, 3, False)
}

test field_7_one_signer_ascending_bytes_refuses_rejection() {
  !order_step(7, 3, True)
}

test field_7_one_signer_descending_bytes_rejects() {
  order_step(7, 4, True)
}

test field_7_one_signer_descending_bytes_refuses_acceptance() {
  !order_step(7, 4, False)
}

// The one-step chunk route renders the same verdict as the item route.

test chunk_route_field_0_sorted_accepts() {
  order_step_via(0, 0, False, True)
}

test chunk_route_field_0_descending_rejects() {
  order_step_via(0, 1, True, True)
}

test chunk_route_field_0_descending_refuses_acceptance() {
  !order_step_via(0, 1, False, True)
}

test chunk_route_field_1_duplicate_accepts() {
  order_step_via(1, 2, False, True)
}

test chunk_route_field_7_duplicate_rejects() {
  order_step_via(7, 2, True, True)
}

test chunk_route_field_7_duplicate_refuses_acceptance() {
  !order_step_via(7, 2, False, True)
}

test chunk_route_field_7_sorted_refuses_rejection() {
  !order_step_via(7, 0, True, True)
}

// Fixed-item legality (the FIWI predicate) and CanonicalDecode give one verdict
// on the same physical pair.

fn wrapped(item: ByteArray) -> ByteArray {
  bytearray.push(bytearray.push(item, bytearray.length(item)), 0x58)
}

fn fixed_legality_matches_machine(field_index: Int, mode: Int) -> Bool {
  let Pair(inputs, addresses) = order_lists(mode)
  expect [previous,
        current] = if field_index == 7 {
    list.map(addresses, encode_midgard_address_witness)
  } else {
    list.map(inputs, encode_midgard_tx_input)
  }
  let legal = fixed_item_is_legal_v1(
    field_index,
    Some(wrapped(previous)),
    wrapped(current),
  )
  and {
    order_step(field_index, mode, !legal),
    !order_step(field_index, mode, legal),
  }
}

test fixed_legality_matches_machine_for_descending_witnesses() {
  fixed_legality_matches_machine(7, 1)
}

test fixed_legality_matches_machine_for_a_repeated_witness() {
  fixed_legality_matches_machine(7, 2)
}

test fixed_legality_matches_machine_for_descending_spend_inputs() {
  fixed_legality_matches_machine(0, 1)
}

test fixed_legality_matches_machine_for_sorted_witnesses() {
  fixed_legality_matches_machine(7, 0)
}

test fixed_legality_matches_machine_for_repeated_spend_inputs() {
  fixed_legality_matches_machine(0, 2)
}
