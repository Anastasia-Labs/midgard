use midgard/fraud_proofs/native_tx/types.{
  CertRedeemer, MidgardExecutionUnits, MidgardRedeemerWitness, MidgardV1Script,
  MidgardVersionedScript, MintRedeemer, PlutusV3Script, ProposeRedeemer,
  ReceiveRedeemer, RewardRedeemer, SpendRedeemer, VoteRedeemer,
}
use midgard/script_proof_v1
use midgard/validation_merkle_v1

test midgard_v1_script_tag_and_hash_prefix_vector_is_stable() {
  let script = MidgardVersionedScript {
    language: MidgardV1Script,
    script_bytes: #"010203",
  }
  and {
    script_proof_v1.language_tag(MidgardV1Script) == 128,
    script_proof_v1.versioned_script_hash(script) == #"760b621a49505853e1f4562d126e185f78483932825e0fb077a1ed80",
  }
}

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
    source_leaf == #"80339781c7437cf249ebf0d5ea03b26964f30bc857c53bd332f11d6cc7696775",
    redeemer_leaf == #"e42aed2342a26c9334ac80aea22c66b8f649cf6be5a5a4a70c6f33cbd8bda8ab",
    purpose_leaf == #"24c90c22834ab9ec656bee1db27d5421515010ec4c6b8928a63f65aecb5e367b",
    script_proof_v1.execution_leaf_hash(
      3,
      purpose_leaf,
      source_leaf,
      redeemer_leaf,
    ) == #"128c959c93e06cf97b245b8d281deb63278338460b81d522e8246ff30829b6ed",
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

test signer_leaf_and_seven_leaf_frontier_root_match_typescript() {
  let signer_leaf = script_proof_v1.signer_leaf_hash(
    #"11111111111111111111111111111111111111111111111111111111",
  )
  let frontier = validation_merkle_v1.build_frontier(
    [
      #"80339781c7437cf249ebf0d5ea03b26964f30bc857c53bd332f11d6cc7696775",
      #"e42aed2342a26c9334ac80aea22c66b8f649cf6be5a5a4a70c6f33cbd8bda8ab",
      #"24c90c22834ab9ec656bee1db27d5421515010ec4c6b8928a63f65aecb5e367b",
      #"128c959c93e06cf97b245b8d281deb63278338460b81d522e8246ff30829b6ed",
      signer_leaf,
      #"f9a3fa502da0ee4fe7048a78088ca4b89a72fe3aa0d313e12fbae7305e171727",
      #"2758fe3ec7c263c1630eab8cb4bb7f431cd403838a25b46bfe3848b0481daef3",
    ],
  )
  and {
    signer_leaf == #"9e4bab3a1b4ca49640fe5c54486aac6a1183fb7da45eec6b30d46382d8f3418b",
    frontier.count == 7,
    validation_merkle_v1.frontier_commitment(frontier.count, frontier.peaks) == #"aa064eed74ea954d127aaab072d1703e1ed5d9dee6ef4ff5ba506a7a0e9db1ba",
  }
}

test redeemer_purpose_tags_are_stable() {
  and {
    script_proof_v1.redeemer_purpose_tag(SpendRedeemer) == 0,
    script_proof_v1.redeemer_purpose_tag(MintRedeemer) == 1,
    script_proof_v1.redeemer_purpose_tag(CertRedeemer) == 2,
    script_proof_v1.redeemer_purpose_tag(RewardRedeemer) == 3,
    script_proof_v1.redeemer_purpose_tag(VoteRedeemer) == 4,
    script_proof_v1.redeemer_purpose_tag(ProposeRedeemer) == 5,
    script_proof_v1.redeemer_purpose_tag(ReceiveRedeemer) == 6,
  }
}
