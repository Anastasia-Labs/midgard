use aiken/interval.{Finite, Interval, IntervalBound, between}
use env
use midgard/state_queue

test commit_header_time_accepts_the_normalized_commit_upper_bound() {
  state_queue.commit_bound_header_time_is_valid(
    1_000,
    2_000,
    Interval {
      lower_bound: IntervalBound {
        bound_type: Finite(1_900),
        is_inclusive: True,
      },
      upper_bound: IntervalBound {
        bound_type: Finite(2_001),
        is_inclusive: False,
      },
    },
  )
}

test commit_header_time_rejects_a_backdated_end() {
  !state_queue.commit_bound_header_time_is_valid(
    1_000,
    1_500,
    between(1_900, 2_000),
  )
}

test commit_header_time_rejects_an_overlong_validity_range() fail {
  state_queue.commit_bound_header_time_is_valid(
    1,
    env.max_validity_range_length + 1,
    between(0, env.max_validity_range_length + 1),
  )
}

test commit_header_time_rejects_a_future_end() {
  !state_queue.commit_bound_header_time_is_valid(
    1_000,
    2_500,
    between(1_900, 2_000),
  )
}

test commit_header_time_rejects_a_non_advancing_event_interval() {
  !state_queue.commit_bound_header_time_is_valid(
    2_000,
    2_000,
    between(1_900, 2_000),
  )
}
