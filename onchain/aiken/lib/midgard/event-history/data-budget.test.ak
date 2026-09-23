use aiken/builtin
use aiken/collection/list
use midgard/event_history/data_budget

test exact_node_limit_includes_containers_and_map_keys() {
  let data: Data = builtin.constr_data(
    0,
    [
      builtin.map_data(
        [Pair(builtin.i_data(1), builtin.list_data([builtin.b_data(#"abcd")]))],
      ),
    ],
  )
  and {
    data_budget.within_nodes(data, 5),
    !data_budget.within_nodes(data, 4),
  }
}

test wide_data_rejects_after_bounded_work() {
  let data: Data = list.repeat(0, 14_000)
  !data_budget.within_nodes(data, 1024)
}

test scalar_byte_length_does_not_count_as_data_nodes() {
  let data: Data = #"0102030405060708090a"
  and {
    data_budget.within_nodes(data, 1),
    !data_budget.within_nodes(data, 0),
  }
}

test exact_wide_boundary_is_accepted() {
  let data: Data = list.repeat(0, 1023)
  and {
    data_budget.within_nodes(data, 1024),
    !data_budget.within_nodes(data, 1023),
  }
}
