//// ## `da-hash-preimage` — total source-leaf rule (GOAL_SPEC.md Q44)
////
//// A `transactions_root` leaf is `(key, Data(L2TransactionSourceV1))`.
//// The value must be the exact canonical record, its embedded `tx_id` must
//// equal the MPF key, its proof-source triple must be canonical and internally
//// bound, and its compact body must hash to the embedded `tx_id`.
////
//// This module deliberately never decodes an accusation through an `expect`.
//// `cbor.deserialise`, `choose_data`, exact list/constructor shapes and exact
//// re-encoding turn every malformed raw leaf into a verdict. The compact,
//// witness and length encoders are called only after all of their preconditions
//// have been guarded, so a malformed committed value remains provable instead
//// of aborting the validator.

use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, encode_native_tx_compact_v1,
  encode_native_tx_field_preimage_lengths_v1,
  encode_native_tx_witness_set_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxCompact, NativeTxFieldPreimageLengthsV1,
  NativeTxWitnessSetCompact, native_tx_version_v1,
}

pub type SourceEnvelopeV1 {
  embedded_tx_id: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
}

pub type CompactInspectionV1 {
  derived_tx_id: ByteArray,
  witness_set_hash: ByteArray,
}

/// Ordered, mutually exclusive result of adjudicating one authenticated raw
/// transactions-root leaf. `NoViolation` is carried through step 01 so the
/// final step can prove the valid-block negative without trusting the prover.
pub type VerdictV1 {
  MalformedSource
  KeyMismatch
  MalformedProofSource
  DerivedIdMismatch
  NoViolation
}

fn data_is_constructor(data: Data) -> Bool {
  builtin.choose_data(data, True, False, False, False, False)
}

fn data_is_list(data: Data) -> Bool {
  builtin.choose_data(data, False, False, True, False, False)
}

fn data_is_integer(data: Data) -> Bool {
  builtin.choose_data(data, False, False, False, True, False)
}

fn data_is_bytes(data: Data) -> Bool {
  builtin.choose_data(data, False, False, False, False, True)
}

fn constructor_data(data: Data) -> Option<Pair<Int, List<Data>>> {
  if data_is_constructor(data) {
    Some(builtin.un_constr_data(data))
  } else {
    None
  }
}

fn list_data(data: Data) -> Option<List<Data>> {
  if data_is_list(data) {
    Some(builtin.un_list_data(data))
  } else {
    None
  }
}

/// Total structural decoder for exact `Data(L2TransactionSourceV1)`.
pub fn inspect_source_envelope_v1(
  source_cbor: ByteArray,
) -> Option<SourceEnvelopeV1> {
  when cbor.deserialise(source_cbor) is {
    Some(source_data) ->
      when constructor_data(source_data) is {
        Some(Pair(0, [tx_id_data, proof_source_data])) ->
          when constructor_data(proof_source_data) is {
            Some(Pair(0, [compact_data, witness_data, lengths_data])) ->
              if and {
                data_is_bytes(tx_id_data),
                data_is_bytes(compact_data),
                data_is_bytes(witness_data),
                data_is_bytes(lengths_data),
              } {
                let tx_id = builtin.un_b_data(tx_id_data)
                let compact_cbor = builtin.un_b_data(compact_data)
                let witness_set_compact_cbor = builtin.un_b_data(witness_data)
                let field_preimage_lengths_cbor = builtin.un_b_data(
                  lengths_data,
                )
                if and {
                  bytearray.length(tx_id) == 32,
                  cbor.serialise(source_data) == source_cbor,
                } {
                  Some(
                    SourceEnvelopeV1 {
                      embedded_tx_id: tx_id,
                      compact_cbor,
                      witness_set_compact_cbor,
                      field_preimage_lengths_cbor,
                    },
                  )
                } else {
                  None
                }
              } else {
                None
              }
            _ -> None
          }
        _ -> None
      }
    None -> None
  }
}

fn inspect_compact_body_v1(body_data: Data) -> Option<NativeTxBodyCompact> {
  when list_data(body_data) is {
    Some([
      spend_inputs_hash_data, reference_inputs_hash_data, outputs_hash_data,
      fee_data, validity_interval_start_data, validity_interval_end_data,
      required_observers_hash_data, required_signers_hash_data, mint_hash_data,
      script_integrity_hash_data, auxiliary_data_hash_data, network_id_data,
    ]) ->
      if and {
        data_is_bytes(spend_inputs_hash_data),
        data_is_bytes(reference_inputs_hash_data),
        data_is_bytes(outputs_hash_data),
        data_is_integer(fee_data),
        data_is_integer(validity_interval_start_data),
        data_is_integer(validity_interval_end_data),
        data_is_bytes(required_observers_hash_data),
        data_is_bytes(required_signers_hash_data),
        data_is_bytes(mint_hash_data),
        data_is_bytes(script_integrity_hash_data),
        data_is_bytes(auxiliary_data_hash_data),
        data_is_integer(network_id_data),
      } {
        let spend_inputs_hash = builtin.un_b_data(spend_inputs_hash_data)
        let reference_inputs_hash = builtin.un_b_data(
          reference_inputs_hash_data,
        )
        let outputs_hash = builtin.un_b_data(outputs_hash_data)
        let fee = builtin.un_i_data(fee_data)
        let validity_interval_start = builtin.un_i_data(
          validity_interval_start_data,
        )
        let validity_interval_end = builtin.un_i_data(
          validity_interval_end_data,
        )
        let required_observers_hash = builtin.un_b_data(
          required_observers_hash_data,
        )
        let required_signers_hash = builtin.un_b_data(
          required_signers_hash_data,
        )
        let mint_hash = builtin.un_b_data(mint_hash_data)
        let script_integrity_hash = builtin.un_b_data(
          script_integrity_hash_data,
        )
        let auxiliary_data_hash = builtin.un_b_data(auxiliary_data_hash_data)
        let network_id = builtin.un_i_data(network_id_data)
        if and {
          bytearray.length(spend_inputs_hash) == 32,
          bytearray.length(reference_inputs_hash) == 32,
          bytearray.length(outputs_hash) == 32,
          fee >= 0,
          bytearray.length(required_observers_hash) == 32,
          bytearray.length(required_signers_hash) == 32,
          bytearray.length(mint_hash) == 32,
          bytearray.length(script_integrity_hash) == 32,
          bytearray.length(auxiliary_data_hash) == 32,
          network_id == 0 || network_id == 1 || network_id == 255,
        } {
          Some(
            NativeTxBodyCompact {
              spend_inputs_hash,
              reference_inputs_hash,
              outputs_hash,
              fee,
              validity_interval_start,
              validity_interval_end,
              required_observers_hash,
              required_signers_hash,
              mint_hash,
              script_integrity_hash,
              auxiliary_data_hash,
              network_id,
            },
          )
        } else {
          None
        }
      } else {
        None
      }
    _ -> None
  }
}

/// Total compact inspection. Exact native re-encoding proves the raw compact
/// bytes use the one canonical V1 wire before the body id is derived.
pub fn inspect_compact_v1(
  compact_cbor: ByteArray,
) -> Option<CompactInspectionV1> {
  when cbor.deserialise(compact_cbor) is {
    Some(compact_data) ->
      when list_data(compact_data) is {
        Some([version_data, body_data, witness_set_hash_data, validity_data]) ->
          if and {
            data_is_integer(version_data),
            data_is_bytes(witness_set_hash_data),
            data_is_integer(validity_data),
          } {
            let version = builtin.un_i_data(version_data)
            let witness_set_hash = builtin.un_b_data(witness_set_hash_data)
            let validity_code = builtin.un_i_data(validity_data)
            when inspect_compact_body_v1(body_data) is {
              Some(body) ->
                if and {
                  version == native_tx_version_v1,
                  bytearray.length(witness_set_hash) == 32,
                  validity_code == 0 || validity_code == 1,
                } {
                  let compact = NativeTxCompact {
                    body,
                    witness_set_hash,
                    validity_code,
                  }
                  let canonical_compact = encode_native_tx_compact_v1(compact)
                  if canonical_compact == compact_cbor {
                    Some(
                      CompactInspectionV1 {
                        derived_tx_id: native_tx_id_for_version(
                          native_tx_version_v1,
                          encode_native_tx_body_compact(body),
                        ),
                        witness_set_hash,
                      },
                    )
                  } else {
                    None
                  }
                } else {
                  None
                }
              None -> None
            }
          } else {
            None
          }
        _ -> None
      }
    None -> None
  }
}

pub fn witness_set_is_canonical_v1(witness_set_cbor: ByteArray) -> Bool {
  when cbor.deserialise(witness_set_cbor) is {
    Some(witness_set_data) ->
      when list_data(witness_set_data) is {
        Some([address_hash_data, script_hash_data, redeemer_hash_data]) ->
          if and {
            data_is_bytes(address_hash_data),
            data_is_bytes(script_hash_data),
            data_is_bytes(redeemer_hash_data),
          } {
            let address_hash = builtin.un_b_data(address_hash_data)
            let script_hash = builtin.un_b_data(script_hash_data)
            let redeemer_hash = builtin.un_b_data(redeemer_hash_data)
            if and {
              bytearray.length(address_hash) == 32,
              bytearray.length(script_hash) == 32,
              bytearray.length(redeemer_hash) == 32,
            } {
              encode_native_tx_witness_set_compact(
                NativeTxWitnessSetCompact {
                  addr_tx_wits_hash: address_hash,
                  script_tx_wits_hash: script_hash,
                  redeemer_tx_wits_hash: redeemer_hash,
                },
              ) == witness_set_cbor
            } else {
              False
            }
          } else {
            False
          }
        _ -> False
      }
    None -> False
  }
}

pub fn field_lengths_are_canonical_v1(lengths_cbor: ByteArray) -> Bool {
  when cbor.deserialise(lengths_cbor) is {
    Some(lengths_data) ->
      when list_data(lengths_data) is {
        Some([
          spend_inputs_data, reference_inputs_data, outputs_data,
          required_observers_data, required_signers_data, mint_data,
          script_witnesses_data, address_witnesses_data, redeemers_data,
        ]) ->
          if and {
            data_is_integer(spend_inputs_data),
            data_is_integer(reference_inputs_data),
            data_is_integer(outputs_data),
            data_is_integer(required_observers_data),
            data_is_integer(required_signers_data),
            data_is_integer(mint_data),
            data_is_integer(script_witnesses_data),
            data_is_integer(address_witnesses_data),
            data_is_integer(redeemers_data),
          } {
            let spend_inputs = builtin.un_i_data(spend_inputs_data)
            let reference_inputs = builtin.un_i_data(reference_inputs_data)
            let outputs = builtin.un_i_data(outputs_data)
            let required_observers = builtin.un_i_data(required_observers_data)
            let required_signers = builtin.un_i_data(required_signers_data)
            let mint = builtin.un_i_data(mint_data)
            let script_witnesses = builtin.un_i_data(script_witnesses_data)
            let address_witnesses = builtin.un_i_data(address_witnesses_data)
            let redeemers = builtin.un_i_data(redeemers_data)
            if and {
              spend_inputs >= 0,
              reference_inputs >= 0,
              outputs >= 0,
              required_observers >= 0,
              required_signers >= 0,
              mint >= 0,
              script_witnesses >= 0,
              address_witnesses >= 0,
              redeemers >= 0,
            } {
              encode_native_tx_field_preimage_lengths_v1(
                NativeTxFieldPreimageLengthsV1 {
                  spend_inputs,
                  reference_inputs,
                  outputs,
                  required_observers,
                  required_signers,
                  mint,
                  address_witnesses,
                  script_witnesses,
                  redeemers,
                },
              ) == lengths_cbor
            } else {
              False
            }
          } else {
            False
          }
        _ -> False
      }
    None -> False
  }
}

/// Total Q44 verdict. Ordering is deliberate and stable: outer source shape,
/// MPF-key agreement, proof-source integrity, then compact-body id agreement.
pub fn adjudicate_committed_source_leaf_v1(
  committed_tx_id: ByteArray,
  committed_leaf_value: ByteArray,
) -> VerdictV1 {
  when inspect_source_envelope_v1(committed_leaf_value) is {
    None -> MalformedSource
    Some(source) ->
      if source.embedded_tx_id != committed_tx_id {
        KeyMismatch
      } else {
        when inspect_compact_v1(source.compact_cbor) is {
          None -> MalformedProofSource
          Some(compact) ->
            if or {
              !witness_set_is_canonical_v1(source.witness_set_compact_cbor),
              blake2b_256(source.witness_set_compact_cbor) != compact.witness_set_hash,
              !field_lengths_are_canonical_v1(
                source.field_preimage_lengths_cbor,
              ),
            } {
              MalformedProofSource
            } else if compact.derived_tx_id != source.embedded_tx_id {
              DerivedIdMismatch
            } else {
              NoViolation
            }
        }
      }
  }
}

pub fn is_da_hash_preimage_violation_v1(verdict: VerdictV1) -> Bool {
  verdict != NoViolation
}
