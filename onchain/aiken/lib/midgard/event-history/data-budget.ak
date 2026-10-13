//// A byte limit alone does not bound serialiseData cost: a list of tiny
//// integers has far more Data nodes than a bytestring of the same size. The
//// Data-node bound counts every node, containers and map keys included.
////
//// The count is one pass that threads a running total through each list and
//// allocates only a map's entries, so its cost per node is flat across nested,
//// flat and map shapes. It does not stop at the bound: a payload past the
//// bound is counted in full, and its admission is refused either at the bound
//// or by the transaction's budget.

use aiken/builtin

/// A map's keys and values, in order, as one list.
fn entries(pairs: Pairs<Data, Data>) -> List<Data> {
  when pairs is {
    [] -> []
    [Pair(key, value), ..rest] -> [key, value, ..entries(rest)]
  }
}

/// `count` plus the nodes of every item in `values`.
fn items(values: List<Data>, count: Int) -> Int {
  when values is {
    [] -> count
    [value, ..rest] ->
      items(
        rest,
        builtin.choose_data(
          value,
          items(builtin.unconstr_fields(value), count + 1),
          items(entries(builtin.un_map_data(value)), count + 1),
          items(builtin.un_list_data(value), count + 1),
          count + 1,
          count + 1,
        ),
      )
  }
}

pub fn within_nodes(data: Data, maximum: Int) -> Bool {
  items([data], 0) <= maximum
}
