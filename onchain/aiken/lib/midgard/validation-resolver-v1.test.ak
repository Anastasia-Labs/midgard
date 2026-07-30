use aiken/crypto.{ScriptHash}
use midgard/validation_machine_v1
use midgard/validation_resolution_v1
use midgard/validation_resolver_v1

fn phase_a_script_precondition_resolvers() -> List<ScriptHash> {
  [#"00", #"01"]
}

fn script_source_resolvers() -> List<ScriptHash> {
  [
    #"00", #"01", #"02", #"03", #"04", #"05", #"06", #"07", #"08", #"09", #"0a",
    #"0b", #"0c", #"0d", #"0e", #"0f", #"10", #"11", #"12", #"13", #"14", #"15",
    #"16", #"17", #"18", #"19", #"1a", #"1b",
  ]
}

fn every_resolver_routes(
  complete_group: List<ScriptHash>,
  remaining_group: List<ScriptHash>,
  expected_count: Int,
  index: Int,
) -> Bool {
  when remaining_group is {
    [] -> index == expected_count
    [resolver, ..rest] -> and {
        validation_resolver_v1.select_semantic_resolver(
          complete_group,
          expected_count,
          index,
        ) == Some(resolver),
        every_resolver_routes(complete_group, rest, expected_count, index + 1),
      }
  }
}

fn resolver_hash_is_distinct_from_remaining_group(
  resolver_hash: ScriptHash,
  remaining_group: List<ScriptHash>,
) -> Bool {
  when remaining_group is {
    [] -> True
    [candidate_hash, ..rest] -> and {
        candidate_hash != resolver_hash,
        resolver_hash_is_distinct_from_remaining_group(resolver_hash, rest),
      }
  }
}

fn every_resolver_is_isolated_from_hostile_action_substitution(
  remaining_group: List<ScriptHash>,
) -> Bool {
  when remaining_group is {
    [] -> True
    [resolver_hash, ..rest] -> and {
        resolver_hash_is_distinct_from_remaining_group(resolver_hash, rest),
        every_resolver_is_isolated_from_hostile_action_substitution(rest),
      }
  }
}

fn evidence_hash_for_auxiliary(
  auxiliary: validation_machine_v1.ValidationAuxiliaryWitnessV1,
) -> ByteArray {
  let transition_data: Data = #"00"
  let auxiliary_data: Data = auxiliary
  validation_resolution_v1.hash_one_step_evidence(
    transition_data,
    auxiliary_data,
  )
}

test deployed_phase_a_script_precondition_group_routes_every_resolver() {
  let resolvers = phase_a_script_precondition_resolvers()
  every_resolver_routes(
    resolvers,
    resolvers,
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    0,
  )
}

test deployed_script_source_group_routes_every_resolver() {
  let resolvers = script_source_resolvers()
  every_resolver_routes(
    resolvers,
    resolvers,
    validation_resolver_v1.script_sources_semantic_resolver_count,
    0,
  )
}

test phase_a_hostile_action_substitution_cannot_reuse_the_target_hash() {
  let resolvers = phase_a_script_precondition_resolvers()
  and {
    every_resolver_routes(
      resolvers,
      resolvers,
      validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
      0,
    ),
    every_resolver_is_isolated_from_hostile_action_substitution(resolvers),
  }
}

test script_sources_hostile_action_substitution_cannot_reuse_the_target_hash() {
  let resolvers = script_source_resolvers()
  and {
    every_resolver_routes(
      resolvers,
      resolvers,
      validation_resolver_v1.script_sources_semantic_resolver_count,
      0,
    ),
    every_resolver_is_isolated_from_hostile_action_substitution(resolvers),
  }
}

test hostile_cross_family_resolver_group_substitution_fails_closed() {
  let phase_a_resolvers = phase_a_script_precondition_resolvers()
  let script_source_resolvers = script_source_resolvers()
  and {
    validation_resolver_v1.select_semantic_resolver(
      phase_a_resolvers,
      validation_resolver_v1.script_sources_semantic_resolver_count,
      0,
    ) == None,
    validation_resolver_v1.select_semantic_resolver(
      script_source_resolvers,
      validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
      0,
    ) == None,
  }
}

test prepared_evidence_hash_rejects_auxiliary_action_substitution() {
  let no_auxiliary_hash = evidence_hash_for_auxiliary(
    validation_machine_v1.NoAuxiliaryWitness,
  )
  let field_preimage_hash = evidence_hash_for_auxiliary(
    validation_machine_v1.TransactionFieldPreimageWitness {
      preimage_cbor: #"00",
    },
  )
  let substituted_field_preimage_hash = evidence_hash_for_auxiliary(
    validation_machine_v1.TransactionFieldPreimageWitness {
      preimage_cbor: #"01",
    },
  )
  and {
    no_auxiliary_hash != field_preimage_hash,
    field_preimage_hash != substituted_field_preimage_hash,
    no_auxiliary_hash != substituted_field_preimage_hash,
  }
}

test stale_semantic_resolver_cardinalities_fail_closed() {
  let phase_a_resolvers = phase_a_script_precondition_resolvers()
  let script_source_resolvers = script_source_resolvers()
  and {
    validation_resolver_v1.select_semantic_resolver(phase_a_resolvers, 1, 0) == None,
    validation_resolver_v1.select_semantic_resolver(
      script_source_resolvers,
      14,
      0,
    ) == None,
  }
}

test semantic_resolver_selection_returns_the_exact_hash_and_rejects_bad_indices() {
  let resolvers = phase_a_script_precondition_resolvers()
  and {
    validation_resolver_v1.select_semantic_resolver(
      resolvers,
      validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
      0,
    ) == Some(#"00"),
    validation_resolver_v1.select_semantic_resolver(
      resolvers,
      validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
      2,
    ) == None,
    validation_resolver_v1.select_semantic_resolver(
      resolvers,
      validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
      -1,
    ) == None,
  }
}
