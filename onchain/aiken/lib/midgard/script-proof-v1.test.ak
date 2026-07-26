use midgard/fraud_proofs/native_tx/types.{
  MidgardExecutionUnits, MidgardRedeemerWitness, MidgardVersionedScript,
  PlutusV3Script, SpendRedeemer,
}
use midgard/script_proof_v1

test script_proof_source_and_redeemer_vectors_are_stable() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: #"010203",
  }
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 2,
    redeemer_cbor: #"d87980",
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    2,
    script_proof_v1.versioned_script_hash(script),
    #"0102",
  )
  and {
    script_proof_v1.versioned_script_hash(script) == #"8b8c11dcad0af38c40d742ed155b4c938acc5507a0ecbcfcea36496a",
    source_leaf == #"3a1244e9f8fb6e6fc7ab5989f46f4b96e7baeff844bf18247d70329d0cd4ea39",
    redeemer_leaf == #"e42aed2342a26c9334ac80aea22c66b8f649cf6be5a5a4a70c6f33cbd8bda8ab",
    purpose_leaf == #"24c90c22834ab9ec656bee1db27d5421515010ec4c6b8928a63f65aecb5e367b",
    script_proof_v1.execution_leaf_hash(
      3,
      purpose_leaf,
      source_leaf,
      redeemer_leaf,
    ) == #"56be57a786433ade4b1480e0efe9768ea3b19f58f4e6f453af4765007764978d",
    script_proof_v1.output_leaf_hash(2, #"0102") == #"f9a3fa502da0ee4fe7048a78088ca4b89a72fe3aa0d313e12fbae7305e171727",
    script_proof_v1.output_descriptor_leaf_hash(2, #"820102") == #"7d1979d9a1af11ab5b13ceeb3ae750046f31ea6ed7776f99cb2d41b253e87fd4",
    script_proof_v1.context_item_leaf_hash(
      0,
      2,
      #"66b39a3f329165f6ab15f249df38d5e8bc99230853ce5c16976b4780af1ed029",
      176,
      218,
    ) == #"2758fe3ec7c263c1630eab8cb4bb7f431cd403838a25b46bfe3848b0481daef3",
  }
}
