use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/fraud_proofs/native_tx/components.{encode_midgard_redeemer_witness}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  encode_redeemer_witness_preimage,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardExecutionUnits, MidgardRedeemerWitness, SpendRedeemer,
}

// Derived by the genuine Cardano boundary fixture in
// demo/midgard-validation/tests/ordered-collection-redeemer-boundary-v1.test.ts.
const maximum_cardano_spend_redeemer_count = 296

const maximum_cardano_spend_redeemer_preimage_bytes = 5053

const maximum_cardano_spend_redeemer_preimage_hash =
  #"680079f9aebb6ab20240bf0a4b46a9b607181843413e0cdfbb293942aebe3d0a"

const maximum_cardano_spend_redeemer_collection_commitment =
  #"07da3c8aea4dd252510b18f872268ea7b7d752fe9d6874f3321286ec6d8c4133"

fn maximum_cardano_spend_redeemers_from(
  pointer_index: Int,
) -> List<MidgardRedeemerWitness> {
  if pointer_index > maximum_cardano_spend_redeemer_count {
    []
  } else {
    [
      MidgardRedeemerWitness {
        purpose: SpendRedeemer,
        index: pointer_index,
        redeemer_cbor: #"d87980",
        execution_units: MidgardExecutionUnits { memory: 1601, steps: 316149 },
      },
      ..maximum_cardano_spend_redeemers_from(pointer_index + 1)
    ]
  }
}

fn redeemer_pointer_sequence_is_exact(
  redeemers: List<MidgardRedeemerWitness>,
  expected_pointer_index: Int,
) -> Bool {
  when redeemers is {
    [] -> expected_pointer_index == maximum_cardano_spend_redeemer_count + 1
    [redeemer, ..rest] -> and {
        redeemer.index == expected_pointer_index,
        redeemer_pointer_sequence_is_exact(rest, expected_pointer_index + 1),
      }
  }
}

test maximum_cardano_spend_redeemer_field_matches_typescript_terminal_commitment() {
  let redeemers = maximum_cardano_spend_redeemers_from(1)
  let preimage_cbor = encode_redeemer_witness_preimage(redeemers)
  let decoded = decode_midgard_tx_redeemer_witnesses_preimage_cbor(
    preimage_cbor,
  )
  let item_cbors = redeemers |> list.map(encode_midgard_redeemer_witness)
  expect Some(first) = list.at(decoded, 0)
  expect Some(last) = list.last(decoded)

  and {
    list.length(decoded) == maximum_cardano_spend_redeemer_count,
    decoded == redeemers,
    redeemer_pointer_sequence_is_exact(decoded, 1),
    first.index == 1,
    last.index == maximum_cardano_spend_redeemer_count,
    list.all(
      decoded,
      fn(redeemer) {
        and {
          redeemer.purpose == SpendRedeemer,
          redeemer.redeemer_cbor == #"d87980",
          redeemer.execution_units.memory == 1601,
          redeemer.execution_units.steps == 316149,
        }
      },
    ),
    bytearray.length(preimage_cbor) == maximum_cardano_spend_redeemer_preimage_bytes,
    blake2b_256(preimage_cbor) == maximum_cardano_spend_redeemer_preimage_hash,
    bounded_collection_v1.from_items(8, item_cbors) == maximum_cardano_spend_redeemer_collection_commitment,
  }
}
