//// A byte limit alone does not bound serialiseData cost: a list of tiny
//// integers has far more Data nodes than a bytestring of the same size.
//// Stop after the allowed nodes, before hashing attacker-controlled data.

use aiken/builtin

type Work {
  Values(List<Data>)
  Entries(Pairs<Data, Data>)
}

fn walk(work: List<Work>, remaining: Int) -> Bool {
  when work is {
    [] -> True
    [Values([]), ..rest] -> walk(rest, remaining)
    [Entries([]), ..rest] -> walk(rest, remaining)
    [Entries([Pair(key, value), ..entries]), ..rest] ->
      walk([Values([key, value]), Entries(entries), ..rest], remaining)
    [Values([data, ..values]), ..rest] ->
      if remaining <= 0 {
        False
      } else {
        let tail = [Values(values), ..rest]
        builtin.choose_data(
          data,
          {
            let Pair(_, fields) = builtin.un_constr_data(data)
            walk([Values(fields), ..tail], remaining - 1)
          },
          {
            walk([Entries(builtin.un_map_data(data)), ..tail], remaining - 1)
          },
          {
            walk([Values(builtin.un_list_data(data)), ..tail], remaining - 1)
          },
          walk(tail, remaining - 1),
          walk(tail, remaining - 1),
        )
      }
  }
}

pub fn within_nodes(data: Data, maximum: Int) -> Bool {
  walk([Values([data])], maximum)
}
