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
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(redeemer)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    2,
    script_proof_v1.versioned_script_hash(script),
    #"0102",
  )
  and {
    script_proof_v1.versioned_script_hash(script) == #"8b8c11dcad0af38c40d742ed155b4c938acc5507a0ecbcfcea36496a",
    source_leaf == #"d1a1572b0d9583dc4be2105105a14004f7c62c0d6e5cea47e29d3110b894c80a",
    redeemer_leaf == #"1f0c84fb8982a20eac3f594ee864837af122e746d3365813222428b78f4555fc",
    purpose_leaf == #"24c90c22834ab9ec656bee1db27d5421515010ec4c6b8928a63f65aecb5e367b",
    script_proof_v1.execution_leaf_hash(
      3,
      purpose_leaf,
      source_leaf,
      redeemer_leaf,
    ) == #"1afbae568bdc11197a45ec838e96651e2985fd1311ba7f70d36ed9ae349a4ef5",
    script_proof_v1.output_leaf_hash(2, #"0102") == #"37c53f847e2610c1b9f1b514b4b086784720f68c425afab606afb8d9dde5d3b9",
    script_proof_v1.context_item_leaf_hash(
      0,
      2,
      #"66b39a3f329165f6ab15f249df38d5e8bc99230853ce5c16976b4780af1ed029",
      176,
      218,
    ) == #"2758fe3ec7c263c1630eab8cb4bb7f431cd403838a25b46bfe3848b0481daef3",
  }
}
