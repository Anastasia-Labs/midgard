use aiken/collection/list
use aiken/crypto.{blake2b_256}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/fraud_proofs/native_tx/preimages
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}

pub fn verified_spend_inputs_at(
  reference_inputs: List<Input>,
  spend_inputs_ref_input_index: Int,
  expected_spend_inputs_hash: ByteArray,
) -> List<MidgardTxInput> {
  expect Some(Input {
    output: Output { datum: InlineDatum(spend_inputs_data), .. },
    ..
  }) = reference_inputs |> list.at(spend_inputs_ref_input_index)

  expect spend_inputs: List<MidgardTxInput> = spend_inputs_data
  expect
    blake2b_256(preimages.encode_input_preimage(spend_inputs)) == expected_spend_inputs_hash

  spend_inputs
}
