use aiken/crypto.{ScriptHash}
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
