use aiken/collection/list
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/utils
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_field_preimage_lengths_v1, native_tx_proof_commitment_v1,
  verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/types.{NativeTxFieldPreimageLengthsV1}
use midgard/ledger_state.{
  ForcedInclusionTxV1, MidgardTxValidity, NativeTxProofSourceV1, TxOrderEventV1,
  TxOrderId, TxOrderPayloadV1, unsafe_event_to_key_value_pair,
}
use midgard/native_tx_field_access_v1
use midgard/transition_trace
use midgard/user_events

pub type Datum =
  user_events.OptimisticDatum<TxOrderEventV1>

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  burn_redeemer_index: Int,
  membership_proof: transition_trace.RootMembershipProof<ByteArray, ByteArray>,
  inclusion_proof_script_withdraw_redeemer_index: Int,
  validity_override: MidgardTxValidity,
}

fn field_length(
  lengths: NativeTxFieldPreimageLengthsV1,
  field_index: Int,
) -> Int {
  when field_index is {
    0 -> lengths.spend_inputs
    1 -> lengths.reference_inputs
    2 -> lengths.outputs
    3 -> lengths.required_observers
    4 -> lengths.required_signers
    5 -> lengths.mint
    6 -> lengths.script_witnesses
    7 -> lengths.address_witnesses
    8 -> lengths.redeemers
    _ -> 0
  }
}

fn next_non_empty_field(
  commitments: List<ByteArray>,
  lengths: NativeTxFieldPreimageLengthsV1,
  field_index: Int,
) -> Int {
  if field_index >= 9 {
    9
  } else {
    let commitment = commitments |> list.expect_at(field_index)
    if commitment == native_tx_field_access_v1.empty_field_commitment {
      expect field_length(lengths, field_index) == 1
      next_non_empty_field(commitments, lengths, field_index + 1)
    } else {
      field_index
    }
  }
}

fn material_directory(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  source: NativeTxProofSourceV1,
) -> Pair<List<ByteArray>, NativeTxFieldPreimageLengthsV1> {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    transaction_id,
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
  )
  expect
    native_tx_proof_commitment_v1(
      source.compact_cbor,
      source.witness_set_compact_cbor,
      source.field_preimage_lengths_cbor,
    ) == transaction_commitment
  let lengths = decode_native_tx_field_preimage_lengths_v1(
    source.field_preimage_lengths_cbor,
  )
  let body = verified.tx_compact.body
  Pair(
    [
      body.spend_inputs_hash,
      body.reference_inputs_hash,
      body.outputs_hash,
      body.required_observers_hash,
      body.required_signers_hash,
      body.mint_hash,
      witness_set.script_tx_wits_hash,
      witness_set.addr_tx_wits_hash,
      witness_set.redeemer_tx_wits_hash,
    ],
    lengths,
  )
}

/// Verifies that the L1 forced-inclusion order's material is available under
/// `docs/spec/midgard-tx.md` §8 before the order NFT is authenticated.
///
/// **Today this admits exactly the canonically-empty transaction, which is
/// strictly narrower than its predecessor and deliberately so.** What stood here
/// was `verify_order_receipts`, which walked the counted per-item publication
/// receipt chain back from a `terminal_receipt_reference` in the payload. That
/// chain's mint policy gates its `PublishField` branch on
/// `native_tx/transaction.verify_midgard_transaction_field_chunk_v1`, which
/// checks a counted-scheme Merkle opening against the field commitment the
/// payload's own compact *declares*. It is therefore unsatisfiable for any
/// payload whose commitments are the §4 flat hashes of real material — but
/// **not** unsatisfiable outright: a payload declaring counted roots instead of
/// flat commitments could mint receipts and be authenticated, certifying
/// availability of material under a format §4 no longer defines. The all-empty
/// clause here closes that hole by construction rather than relying on the
/// opening being hard to satisfy. What is lost with the chain is only the
/// declaring-payload path, which was never a durable-availability guarantee.
///
/// ### `field_carriage_availability`
///
/// The §8 re-expression — per non-empty field, a §8.6 `FieldPreimageCertificateV1`
/// over §8.4 chunks, checked here through
/// `native_tx_field_access_v1.authenticated_field_view` — is **not** in this
/// function, because it cannot be wired from this lane: the §8.6 certificate
/// validator is not in the frozen blueprint and has no deployment role, and
/// §8.4's ladder is a partition that routes every preimage under 14,336 bytes to
/// tier-1 *redeemer* carriage, which does not outlive the transaction that
/// carried it and therefore cannot serve durable L1 availability at all. Both
/// are recorded on issue #587 as a Deviation and owned by the follow-up it
/// names. Until then this is fail-closed: an order carrying material cannot be
/// authenticated at all — not even by declaring counted-root commitments, which
/// is the one thing the retired chain would still have let through.
pub fn verify_order_material(payload: TxOrderPayloadV1) -> Bool {
  let Pair(commitments, lengths) = material_directory(
    payload.tx_id,
    payload.transaction_commitment,
    payload.source,
  )
  next_non_empty_field(commitments, lengths, 0) == 9
}

pub fn forced_inclusion_key_value(
  tx_order_event_data: Data<TxOrderEventV1>,
  validity_override: MidgardTxValidity,
  return: fn(TxOrderId, ForcedInclusionTxV1) -> result,
) -> result {
  let Pair(tx_order_event_id_data, tx_order_payload_data) = unsafe_event_to_key_value_pair(
    tx_order_event_data,
  )

  expect tx_order_id: TxOrderId = tx_order_event_id_data
  expect tx_order_payload: TxOrderPayloadV1 = tx_order_payload_data

  return(
    tx_order_id,
    ForcedInclusionTxV1 {
      tx_id: tx_order_payload.tx_id,
      source: tx_order_payload.source,
      operator_validity: validity_override,
    },
  )
}

pub fn get_datum(
  reference_inputs: List<Input>,
  tx_order_policy_id: PolicyId,
  tx_order_input_index: Int,
) -> Datum {
  expect
      Input {
        output: Output { datum: InlineDatum(tx_order_datum_data), .. },
        ..
      }
    = utils.get_authentic_input_with_policy_at(
    reference_inputs,
    tx_order_policy_id,
    tx_order_input_index,
  )
  expect tx_order_datum: Datum = tx_order_datum_data
  tx_order_datum
}
