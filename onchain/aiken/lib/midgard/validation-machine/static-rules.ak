use aiken/builtin
use aiken/cbor
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_field_preimage_lengths_v1, min_fee_lovelace_v1,
  native_tx_canonical_size_v1, native_tx_proof_commitment_v1,
  verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/types.{NativeTxBodyCompact}
use midgard/native_tx_field_access_v1
use midgard/rejection_reason_v1.{reject_min_fee, reject_network_id_mismatch}
use midgard/validation_machine/machine_types.{
  ValidationContextV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{
  decode_validation_context, rejected_successor_is_exact,
}
use midgard/validation_machine/witness.{
  empty_resolution_schedule_hash, encode_input_sets_scan_witness,
  encode_static_rules_witness,
}
use midgard/validation_trace_v1

fn static_rules_rejection(
  body: NativeTxBodyCompact,
  canonical_tx_size: Int,
  context: ValidationContextV1,
) -> Option<ByteArray> {
  if and {
    body.network_id != 255,
    body.network_id != context.expected_network_id,
  } {
    Some(reject_network_id_mismatch)
  } else if body.fee < min_fee_lovelace_v1(
    context.min_fee_a,
    context.min_fee_b,
    canonical_tx_size,
  ) {
    Some(reject_min_fee)
  } else {
    None
  }
}

pub fn verify_static_rules(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
      ]
    = builtin.un_list_data(data)
  let compact_cbor = builtin.un_b_data(compact_cbor_data)
  let witness_set_compact_cbor = builtin.un_b_data(
    witness_set_compact_cbor_data,
  )
  let field_preimage_lengths_cbor = builtin.un_b_data(
    field_preimage_lengths_cbor_data,
  )
  let context_cbor = builtin.un_b_data(context_cbor_data)
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    field_preimage_lengths_cbor,
  )
  let context = decode_validation_context(context_cbor)
  and {
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(context_cbor) == pre.validation_context_hash,
    witness.work_witness_cbor == encode_static_rules_witness(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
      context_cbor,
    ),
    when
      static_rules_rejection(
        verified.tx_compact.body,
        native_tx_canonical_size_v1(verified.tx_compact, field_lengths),
        context,
      )
    is {
      Some(rejection_code) ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          rejection_code,
        )
      None -> {
        let body = verified.tx_compact.body
        let spend_count = if body.spend_inputs_hash == native_tx_field_access_v1.empty_field_commitment {
          0
        } else {
          -1
        }
        let reference_count = if body.reference_inputs_hash == native_tx_field_access_v1.empty_field_commitment {
          0
        } else {
          -1
        }
        and {
          witness.claimed_successor.phase == validation_trace_v1.InputSets,
          witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
            validation_trace_v1.InputSets,
            pre.program_counter + 1,
            encode_input_sets_scan_witness(
              compact_cbor,
              witness_set_compact_cbor,
              field_preimage_lengths_cbor,
              context_cbor,
              spend_count,
              reference_count,
              0,
              0,
              #"",
              empty_resolution_schedule_hash(),
            ),
          ),
        }
      }
    },
  }
}
