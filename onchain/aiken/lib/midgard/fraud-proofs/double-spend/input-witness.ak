use aiken/collection/list
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/native_tx_field_access_v1

/// **The one surviving free-standing-hash consumer at the lib layer** (AC2),
/// and the reason it survives.
///
/// The double-spend family splits field-0 access across steps: an earlier step
/// extracts `spend_inputs_hash` positionally from the tx-id-verified compact
/// structure and forwards it in the computation thread's datum, and step-03 /
/// step-04 arrive holding that hash and nothing else. §4's positional-identity
/// invariant is therefore satisfied *at the entry point*, not here — which is
/// exactly the shape §4 sanctions, and exactly why this cannot be rebound to
/// `authenticated_field_view` without also carrying the compact structures
/// down the thread. That is the family's own rebind, owned by the Phase-5
/// lanes (#575–#578) on top of #570's machine walk core; until then the
/// narrowest honest export is this one function, and callers must obtain
/// `expected_spend_inputs_hash` positionally.
///
/// Note the flat scheme removed field-index domain separation, so a field-0
/// and a field-1 preimage with the same items now hash alike (§4). The
/// forwarded hash is what distinguishes them, so it must never come from a
/// redeemer.
pub fn verified_spend_inputs_at(
  reference_inputs: List<Input>,
  spend_inputs_ref_input_index: Int,
  expected_spend_inputs_hash: ByteArray,
) -> List<MidgardTxInput> {
  expect
      Some(Input {
        output: Output { datum: InlineDatum(spend_inputs_data), .. },
        ..
      })
    = reference_inputs |> list.at(spend_inputs_ref_input_index)

  expect spend_inputs: List<MidgardTxInput> = spend_inputs_data
  expect
    native_tx_field_access_v1.field_commitment_from_items(list.map(spend_inputs, encode_midgard_tx_input)) == expected_spend_inputs_hash

  spend_inputs
}
