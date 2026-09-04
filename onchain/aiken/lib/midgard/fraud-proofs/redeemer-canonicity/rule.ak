//// Narrow total rule for `redeemerCanonicity` (`00000028`).

use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/canonical_cbor_scan_v1
use midgard/canonical_plutus_data_v1
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{RedeemerMalformed}

pub const redeemer_field_index: Int = 8

pub type BoundRedeemerV1 {
  subject: substrate.VerdictSubjectV1,
  witness_set_hash: ByteArray,
  redeemer_index: Int,
}

pub type TerminalStateV1 {
  bound: BoundRedeemerV1,
  canonical: Bool,
}

pub fn bind_redeemer_v1(
  subject: substrate.VerdictSubjectV1,
  witness_set_hash: ByteArray,
  redeemer_index: Int,
) -> BoundRedeemerV1 {
  expect bytearray.length(witness_set_hash) == 32
  expect substrate.subject_is_canonical_v1(subject)
  expect redeemer_index >= 0
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    Some(
      substrate.bind_exact_rejection_reason_v1(
        subject,
        RedeemerMalformed { redeemer_index },
      ),
    )
  } else {
    None
  }
  BoundRedeemerV1 { subject, witness_set_hash, redeemer_index }
}

fn supported_purpose(tag: Int) -> Bool {
  tag == 0 || tag == 1 || tag == 3 || tag == 6
}

/// Total exact field-8 item decoder: malformed adversarial bytes return False.
pub fn item_is_canonical_v1(item: ByteArray) -> Bool {
  when canonical_cbor_scan_v1.head_at_v1(item, 0, 4) is {
    None -> False
    Some(outer) ->
      if outer.value != 4 {
        False
      } else {
        when canonical_cbor_scan_v1.head_at_v1(item, outer.next_offset, 0) is {
          None -> False
          Some(purpose) ->
            if !supported_purpose(purpose.value) {
              False
            } else {
              when
                canonical_cbor_scan_v1.head_at_v1(item, purpose.next_offset, 0)
              is {
                None -> False
                Some(pointer) ->
                  when
                    canonical_cbor_scan_v1.head_at_v1(
                      item,
                      pointer.next_offset,
                      2,
                    )
                  is {
                    None -> False
                    Some(data) -> {
                      let data_end = data.next_offset + data.value
                      if data_end > bytearray.length(item) {
                        False
                      } else {
                        when
                          canonical_cbor_scan_v1.head_at_v1(item, data_end, 4)
                        is {
                          None -> False
                          Some(ex_units) ->
                            if ex_units.value != 2 {
                              False
                            } else {
                              when
                                canonical_cbor_scan_v1.head_at_v1(
                                  item,
                                  ex_units.next_offset,
                                  0,
                                )
                              is {
                                None -> False
                                Some(memory) ->
                                  when
                                    canonical_cbor_scan_v1.head_at_v1(
                                      item,
                                      memory.next_offset,
                                      0,
                                    )
                                  is {
                                    None -> False
                                    Some(steps) -> and {
                                        steps.next_offset == bytearray.length(
                                          item,
                                        ),
                                        canonical_plutus_data_v1.is_canonical_plutus_data_v1(
                                          builtin.slice_bytearray(
                                            data.next_offset,
                                            data.value,
                                            item,
                                          ),
                                        ),
                                      }
                                  }
                              }
                            }
                        }
                      }
                    }
                  }
              }
            }
        }
      }
  }
}

pub fn authenticate_item_v1(
  bound: BoundRedeemerV1,
  item: ByteArray,
) -> TerminalStateV1 {
  TerminalStateV1 { bound, canonical: item_is_canonical_v1(item) }
}

pub fn terminal_contradiction_v1(state: TerminalStateV1) -> Bool {
  substrate.terminal_contradiction_v1(state.bound.subject, !state.canonical)
}

pub fn encode_bound_v1(bound: BoundRedeemerV1) -> ByteArray {
  cbor.serialise(bound)
}

pub fn encode_terminal_v1(state: TerminalStateV1) -> ByteArray {
  cbor.serialise(state)
}
