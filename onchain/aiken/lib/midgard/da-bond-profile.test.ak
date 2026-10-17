//// The env-bound tests apply each relation to the constants of the env this
//// check runs under; CI runs this module under every deployment profile. The
//// literal tests show each relation refusing a deliberately broken value.

use env
use midgard/da_bond_profile as profile

test env_challenge_window_is_ordered() {
  profile.challenge_window_is_ordered_v1(
    env.da_attestation_timeout_v1,
    env.da_challenge_window_ms_v1,
    env.block_maturity_duration_v1,
  )
}

test env_withdraw_delay_covers_challenge() {
  profile.withdraw_delay_covers_challenge_v1(
    env.da_bond_withdraw_delay_ms_v1,
    env.max_validity_range_length,
    env.da_challenge_window_ms_v1,
    env.full_response_window_ms_v1,
    env.da_slash_grace_ms_v1,
  )
}

test env_withdraw_delay_covers_head_removal() {
  profile.withdraw_delay_covers_head_removal_v1(
    env.da_bond_withdraw_delay_ms_v1,
    env.max_validity_range_length,
    env.da_challenge_window_ms_v1,
    env.full_response_window_ms_v1,
    env.block_maturity_duration_v1,
    env.da_slash_grace_ms_v1,
  )
}

test env_slash_penalty_is_bounded() {
  profile.slash_penalty_is_bounded_v1(
    env.da_slash_penalty_lovelace_v1,
    env.da_bond_lovelace_v1,
  )
}

test env_pool_amounts_are_positive() {
  profile.pool_amounts_are_positive_v1(
    env.da_bond_min_top_up_lovelace_v1,
    env.da_bond_pool_floor_lovelace_v1,
    env.challenge_record_lovelace_v1,
  )
}

test challenge_window_bounds_are_exact() {
  and {
    // Public figures: 1 h timeout, 3 d window, 7 d maturity.
    profile.challenge_window_is_ordered_v1(3_600_000, 259_200_000, 604_800_000),
    profile.challenge_window_is_ordered_v1(3_600_000, 3_600_001, 604_800_000),
    profile.challenge_window_is_ordered_v1(3_600_000, 604_800_000, 604_800_000),
  }
}

test challenge_window_equal_to_attestation_timeout_is_refused() {
  !profile.challenge_window_is_ordered_v1(600_000, 600_000, 900_000)
}

test challenge_window_shorter_than_attestation_timeout_is_refused() {
  !profile.challenge_window_is_ordered_v1(600_000, 599_999, 900_000)
}

test challenge_window_past_block_maturity_is_refused() {
  !profile.challenge_window_is_ordered_v1(600_000, 900_001, 900_000)
}

test withdraw_delay_spec_form_bound_is_exact() {
  and {
    // 480,000 + 720,000 + 880,000 + 300,000 = 2,380,000 (testing figures).
    profile.withdraw_delay_covers_challenge_v1(
      2_380_000,
      480_000,
      720_000,
      880_000,
      300_000,
    ),
    // 480,000 + 259,200,000 + 172,800,000 + 172,800,000 = 605,280,000.
    profile.withdraw_delay_covers_challenge_v1(
      605_280_000,
      480_000,
      259_200_000,
      172_800_000,
      172_800_000,
    ),
  }
}

test withdraw_delay_below_spec_form_is_refused() {
  !profile.withdraw_delay_covers_challenge_v1(
    2_379_999,
    480_000,
    720_000,
    880_000,
    300_000,
  )
}

test withdraw_delay_head_removal_bound_is_exact() {
  and {
    // 480,000 + max(432,000,000, 604,800,000) + 172,800,000 = 778,080,000.
    profile.withdraw_delay_covers_head_removal_v1(
      778_080_000,
      480_000,
      259_200_000,
      172_800_000,
      604_800_000,
      172_800_000,
    ),
    // 480,000 + max(1,600,000, 900,000) + 300,000 = 2,380,000.
    profile.withdraw_delay_covers_head_removal_v1(
      2_380_000,
      480_000,
      720_000,
      880_000,
      900_000,
      300_000,
    ),
  }
}

test withdraw_delay_below_head_removal_form_is_refused() {
  !profile.withdraw_delay_covers_head_removal_v1(
    778_079_999,
    480_000,
    259_200_000,
    172_800_000,
    604_800_000,
    172_800_000,
  )
}

/// The spec's own public delay meets the spec form but leaves no margin once
/// the challenged block must wait for the queue head, so the enforced form
/// refuses it.
test spec_public_withdraw_delay_is_refused_by_head_removal_form() {
  and {
    profile.withdraw_delay_covers_challenge_v1(
      605_280_000,
      480_000,
      259_200_000,
      172_800_000,
      172_800_000,
    ),
    !profile.withdraw_delay_covers_head_removal_v1(
      605_280_000,
      480_000,
      259_200_000,
      172_800_000,
      604_800_000,
      172_800_000,
    ),
  }
}

test withdraw_delay_head_removal_counts_the_challenge_path_when_it_dominates() {
  !profile.withdraw_delay_covers_head_removal_v1(
    2_379_999,
    480_000,
    720_000,
    880_000,
    900_000,
    300_000,
  )
}

test slash_penalty_bounds_are_exact() {
  and {
    profile.slash_penalty_is_bounded_v1(1, 500_000_000),
    profile.slash_penalty_is_bounded_v1(499_999_999, 500_000_000),
    profile.slash_penalty_is_bounded_v1(25_000_000_000, 100_000_000_000),
  }
}

test zero_slash_penalty_is_refused() {
  !profile.slash_penalty_is_bounded_v1(0, 500_000_000)
}

test negative_slash_penalty_is_refused() {
  !profile.slash_penalty_is_bounded_v1(-1, 500_000_000)
}

test slash_penalty_equal_to_bond_is_refused() {
  !profile.slash_penalty_is_bounded_v1(500_000_000, 500_000_000)
}

test slash_penalty_above_bond_is_refused() {
  !profile.slash_penalty_is_bounded_v1(500_000_001, 500_000_000)
}

test pool_amounts_at_one_lovelace_are_accepted() {
  profile.pool_amounts_are_positive_v1(1, 1, 1)
}

test zero_min_top_up_is_refused() {
  !profile.pool_amounts_are_positive_v1(0, 5_000_000, 27_000_000)
}

test zero_pool_floor_is_refused() {
  !profile.pool_amounts_are_positive_v1(5_000_000, 0, 27_000_000)
}

test zero_challenge_record_is_refused() {
  !profile.pool_amounts_are_positive_v1(5_000_000, 5_000_000, 0)
}
