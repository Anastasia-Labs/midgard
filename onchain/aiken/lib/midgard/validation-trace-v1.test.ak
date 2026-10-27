use aiken/cbor
use midgard/validation_trace_v1

const expected_state_cbor =
  #"8e015820010101010101010101010101010101010101010101010101010101010101010158200202020202020202020202020202020202020202020202020202020202020202582003030303030303030303030303030303030303030303030303030303030303035820040404040404040404040404040404040404040404040404040404040404040401582005050505050505050505050505050505050505050505050505050505050505050b1158200606060606060606060606060606060606060606060606060606060606060606187b182d0058200000000000000000000000000000000000000000000000000000000000000000"

const expected_state_hash =
  #"fae79f2d6059b1b07f78119d37a1a6a8004db1fb66faa5224f99a5581c2eb104"

const second_state_hash =
  #"0909090909090909090909090909090909090909090909090909090909090909"

const expected_trace_root =
  #"434f72181d374538569f6b5ac8ae5c587a993cf00a9fda7330333b5fb4ddb9d7"

const expected_descriptor_cbor =
  #"8801015820434f72181d374538569f6b5ac8ae5c587a993cf00a9fda7330333b5fb4ddb9d7015820fae79f2d6059b1b07f78119d37a1a6a8004db1fb66faa5224f99a5581c2eb104582009090909090909090909090909090909090909090909090909090909090909090258200808080808080808080808080808080808080808080808080808080808080808"

// Plutus Data the TypeScript SDK emits for the ValidationTraceDescriptorV1
// fixture of demo/midgard-node/tests/fixtures/transition-trace-abi.json.
// Rebound by demo/midgard-node/scripts/generate-transition-trace-abi-fixture.mjs;
// do not edit.
const typescript_descriptor_data_cbor =
  #"d8799f01025820a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1075820b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b25820c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3d87b805820d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4ff"

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

// The node commits descriptors as Plutus Data built from the SDK schema, and
// on-chain code decodes that Data. Comparing every field catches two fields of
// one type in a different order, which decoding into the type alone accepts.
test decodes_typescript_descriptor_data_field_by_field() {
  expect Some(data) = cbor.deserialise(typescript_descriptor_data_cbor)
  expect descriptor: validation_trace_v1.ValidationTraceDescriptorV1 = data
  and {
    descriptor.schema_version == 1,
    descriptor.machine_version == 2,
    descriptor.trace_root == #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1",
    descriptor.step_count == 7,
    descriptor.initial_state_hash == #"b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2",
    descriptor.terminal_state_hash == #"c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3",
    descriptor.verdict == validation_trace_v1.Rejected,
    descriptor.rejection_code_hash == #"d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4",
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
    cbor.serialise(validation_trace_v1.Terminal) == #"d9050680",
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

test work_and_context_hashes_match_typescript() {
  and {
    validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      17,
      #"820142abcd",
    ) == #"36d4e5e57f9cbcca2fc621a5f9411251be1c31a39378089dff8d44e2caa8e2bc",
    validation_trace_v1.hash_validation_context(#"83010203") == #"e99abbeb7b56d9e4b72cbd1356b1dbacdc12f250dcda3ba4b86cea25ac3219bd",
  }
}

test long_work_witness_hash_matches_typescript_and_aiken_byte_chunking() {
  validation_trace_v1.hash_work_witness(
    validation_trace_v1.CanonicalDecode,
    0,
    #"abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab",
  ) == #"c0e76d5f16d1f18c9e27ce0744c2e02e84a00ef55af81ce392137b2fdc9b8a53"
}
