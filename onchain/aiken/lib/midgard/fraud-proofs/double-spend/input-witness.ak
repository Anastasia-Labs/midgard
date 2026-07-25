use aiken/collection/list
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/bounded_collection_v1
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}

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
    bounded_collection_v1.from_items(
      0,
      list.map(spend_inputs, encode_midgard_tx_input),
    ) == expected_spend_inputs_hash

  spend_inputs
}
