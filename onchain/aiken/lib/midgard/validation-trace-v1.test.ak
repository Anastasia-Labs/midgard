use aiken/cbor
use midgard/validation_trace_v1

const expected_state_cbor =
  #"8f015820010101010101010101010101010101010101010101010101010101010101010158200202020202020202020202020202020202020202020202020202020202020202582003030303030303030303030303030303030303030303030303030303030303035820040404040404040404040404040404040404040404040404040404040404040401582005050505050505050505050505050505050505050505050505050505050505050b1158200606060606060606060606060606060606060606060606060606060606060606187b182d005820000000000000000000000000000000000000000000000000000000000000000058200707070707070707070707070707070707070707070707070707070707070707"

const expected_state_hash =
  #"fa9598fae21355bd529770b1c2c750ace65d721ada641bec6bd5f87a22c18088"

const second_state_hash =
  #"0909090909090909090909090909090909090909090909090909090909090909"

const expected_trace_root =
  #"c6760a9266746c67578026b6d44e533ae8390264d227a9649e6558a3d70970eb"

const expected_descriptor_cbor =
  #"8801015820c6760a9266746c67578026b6d44e533ae8390264d227a9649e6558a3d70970eb015820fa9598fae21355bd529770b1c2c750ace65d721ada641bec6bd5f87a22c18088582009090909090909090909090909090909090909090909090909090909090909090258200808080808080808080808080808080808080808080808080808080808080808"

fn fixture_state() -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: #"0101010101010101010101010101010101010101010101010101010101010101",
    transaction_id: #"0202020202020202020202020202020202020202020202020202020202020202",
    transaction_commitment: #"0303030303030303030303030303030303030303030303030303030303030303",
    validation_context_hash: #"0404040404040404040404040404040404040404040404040404040404040404",
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: #"0505050505050505050505050505050505050505050505050505050505050505",
    phase: validation_trace_v1.Cek,
    program_counter: 17,
    work_root: #"0606060606060606060606060606060606060606060606060606060606060606",
    execution_cpu: 123,
    execution_memory: 45,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
    ledger_delta_root: #"0707070707070707070707070707070707070707070707070707070707070707",
  }
}

fn fixture_descriptor() -> validation_trace_v1.ValidationTraceDescriptorV1 {
  validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: expected_trace_root,
    step_count: 1,
    initial_state_hash: expected_state_hash,
    terminal_state_hash: second_state_hash,
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: #"0808080808080808080808080808080808080808080808080808080808080808",
  }
}

test matches_typescript_machine_state_encoding_and_hash() {
  and {
    validation_trace_v1.machine_state_is_well_formed(fixture_state()),
    validation_trace_v1.encode_machine_state(fixture_state()) == expected_state_cbor,
    validation_trace_v1.hash_machine_state(fixture_state()) == expected_state_hash,
  }
}

test matches_typescript_descriptor_encoding() {
  and {
    validation_trace_v1.descriptor_is_well_formed(fixture_descriptor()),
    validation_trace_v1.encode_descriptor(fixture_descriptor()) == expected_descriptor_cbor,
  }
}

test rejects_wrong_machine_state_version_and_malformed_hash() {
  and {
    !validation_trace_v1.machine_state_is_well_formed(
      validation_trace_v1.ValidationMachineStateV1 {
        ..fixture_state(),
        machine_version: 2,
      },
    ),
    !validation_trace_v1.machine_state_is_well_formed(
      validation_trace_v1.ValidationMachineStateV1 {
        ..fixture_state(),
        event_key_hash: #"00",
      },
    ),
  }
}

test validation_phase_verdict_and_source_kind_tags_match_typescript() {
  and {
    cbor.serialise(validation_trace_v1.CanonicalDecode) == #"d87980",
    cbor.serialise(validation_trace_v1.CompactBinding) == #"d87a80",
    cbor.serialise(validation_trace_v1.StaticLedgerRules) == #"d87b80",
    cbor.serialise(validation_trace_v1.InputSets) == #"d87c80",
    cbor.serialise(validation_trace_v1.Signatures) == #"d87d80",
    cbor.serialise(validation_trace_v1.PhaseANativeScripts) == #"d87e80",
    cbor.serialise(validation_trace_v1.PhaseAScriptPreconditions) == #"d87f80",
    cbor.serialise(validation_trace_v1.ResolveInputs) == #"d9050080",
    cbor.serialise(validation_trace_v1.ScriptSources) == #"d9050180",
    cbor.serialise(validation_trace_v1.NativeScripts) == #"d9050280",
    cbor.serialise(validation_trace_v1.ScriptIntegrity) == #"d9050380",
    cbor.serialise(validation_trace_v1.Cek) == #"d9050480",
    cbor.serialise(validation_trace_v1.ValueAndMint) == #"d9050580",
    cbor.serialise(validation_trace_v1.LedgerDelta) == #"d9050680",
    cbor.serialise(validation_trace_v1.Terminal) == #"d9050780",
    cbor.serialise(validation_trace_v1.Pending) == #"d87980",
    cbor.serialise(validation_trace_v1.Accepted) == #"d87a80",
    cbor.serialise(validation_trace_v1.Rejected) == #"d87b80",
    cbor.serialise(validation_trace_v1.Normal) == #"d87980",
    cbor.serialise(validation_trace_v1.Forced) == #"d87a80",
  }
}

test verifies_both_members_of_two_state_trace() {
  let left_leaf = validation_trace_v1.trace_leaf_hash(expected_state_hash)
  let right_leaf = validation_trace_v1.trace_leaf_hash(second_state_hash)
  and {
    validation_trace_v1.verify_trace_proof(
      fixture_descriptor(),
      validation_trace_v1.ValidationTraceProof {
        state_index: 0,
        state_hash: expected_state_hash,
        siblings: [right_leaf],
      },
    ),
    validation_trace_v1.verify_trace_proof(
      fixture_descriptor(),
      validation_trace_v1.ValidationTraceProof {
        state_index: 1,
        state_hash: second_state_hash,
        siblings: [left_leaf],
      },
    ),
  }
}

test rejects_wrong_state_or_wrong_path_depth() {
  let right_leaf = validation_trace_v1.trace_leaf_hash(second_state_hash)
  and {
    !validation_trace_v1.verify_trace_proof(
      fixture_descriptor(),
      validation_trace_v1.ValidationTraceProof {
        state_index: 0,
        state_hash: second_state_hash,
        siblings: [right_leaf],
      },
    ),
    !validation_trace_v1.verify_trace_proof(
      fixture_descriptor(),
      validation_trace_v1.ValidationTraceProof {
        state_index: 0,
        state_hash: expected_state_hash,
        siblings: [],
      },
    ),
  }
}

test depth_is_exact_for_padded_state_count() {
  and {
    validation_trace_v1.trace_depth(0) == 0,
    validation_trace_v1.trace_depth(1) == 1,
    validation_trace_v1.trace_depth(4) == 3,
    validation_trace_v1.trace_depth(4_294_967_295) == 32,
  }
}

test work_context_and_delta_hashes_match_typescript() {
  and {
    validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      17,
      #"820142abcd",
    ) == #"36d4e5e57f9cbcca2fc621a5f9411251be1c31a39378089dff8d44e2caa8e2bc",
    validation_trace_v1.hash_validation_context(#"83010203") == #"e99abbeb7b56d9e4b72cbd1356b1dbacdc12f250dcda3ba4b86cea25ac3219bd",
    validation_trace_v1.hash_ledger_delta(
      #"81820058201111111111111111111111111111111111111111111111111111111111111111",
    ) == #"0fe3b46f56f2eb3137c35fd2f51be4fdd96043a811d20b75edc011d56c27ef44",
  }
}

test long_work_witness_hash_matches_typescript_and_aiken_byte_chunking() {
  validation_trace_v1.hash_work_witness(
    validation_trace_v1.CanonicalDecode,
    0,
    #"abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab",
  ) == #"c0e76d5f16d1f18c9e27ce0744c2e02e84a00ef55af81ce392137b2fdc9b8a53"
}
