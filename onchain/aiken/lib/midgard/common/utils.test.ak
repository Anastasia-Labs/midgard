use aiken/interval.{after, before, between, everything}
use midgard/common/utils

test zip_foldl() {
  let list_a = [1, 2, 3]
  let list_b = [1, 2, 3, 4]
  let final_list = utils.zip_foldl(
    list_a,
    list_b,
    [],
    fn(a, b, acc) {
      let new_list = [(a, b), ..acc]
      new_list
    },
  )
  final_list == [(3, 3), (2, 2), (1, 1)]
}

test zip_foldr() {
  let list_a = [1, 2, 3]
  let list_b = [1, 2, 3, 4]
  let final_list = utils.zip_foldr(
    list_a,
    list_b,
    [],
    fn(a, b, acc) {
      let new_list = [(a, b), ..acc]
      new_list
    },
  )
  final_list == [(1, 1), (2, 2), (3, 3)]
}

// A tx-order's L1 validity range: closed, at most the event range long.

test bounded_validity_range_admits_the_maximum_length() {
  utils.get_inclusive_bounds_of_a_bounded_validity_range(
    between(1_000, 601_000),
    600_000,
  ) == (1_000, 601_000)
}

test bounded_validity_range_refuses_one_millisecond_over_the_maximum() fail {
  let (_, upper) = utils.get_inclusive_bounds_of_a_bounded_validity_range(
    between(1_000, 601_001),
    600_000,
  )
  upper == 601_001
}

test bounded_validity_range_refuses_an_open_lower_bound() fail {
  let (_, upper) = utils.get_inclusive_bounds_of_a_bounded_validity_range(
    before(601_000),
    600_000,
  )
  upper == 601_000
}

test bounded_validity_range_refuses_an_open_upper_bound() fail {
  let (lower, _) = utils.get_inclusive_bounds_of_a_bounded_validity_range(
    after(1_000),
    600_000,
  )
  lower == 1_000
}

test bounded_validity_range_refuses_an_unbounded_range() fail {
  let (lower, _) = utils.get_inclusive_bounds_of_a_bounded_validity_range(
    everything,
    600_000,
  )
  lower == 0
}
