use midgard/cek_cost_v1

test add_integer_budget_matches_pinned_v3() {
  cek_cost_v1.builtin_budget_v1(0, [1, 2]) == cek_cost_v1.BuiltinBudgetV1 {
    cpu: 101628,
    memory: 3,
  }
}

test sha2_budget_matches_pinned_v3() {
  cek_cost_v1.builtin_budget_v1(18, [32]) == cek_cost_v1.BuiltinBudgetV1 {
    cpu: 993468,
    memory: 4,
  }
}

test bls_miller_loop_budget_matches_pinned_v3() {
  cek_cost_v1.builtin_budget_v1(68, [48, 96]) == cek_cost_v1.BuiltinBudgetV1 {
    cpu: 254006273,
    memory: 72,
  }
}

test bls_final_verify_budget_matches_pinned_v3() {
  cek_cost_v1.builtin_budget_v1(70, [192, 192]) == cek_cost_v1.BuiltinBudgetV1 {
    cpu: 333849714,
    memory: 1,
  }
}

test bitwise_budget_uses_normalized_sizes() {
  cek_cost_v1.builtin_budget_v1(75, [1, 5, 5]) == cek_cost_v1.BuiltinBudgetV1 {
    cpu: 107406,
    memory: 5,
  }
}

test invalid_arity_fails_closed() fail {
  cek_cost_v1.builtin_budget_v1(0, [1]).cpu == 0
}

test unknown_tag_fails_closed() fail {
  cek_cost_v1.builtin_budget_v1(87, [1]).cpu == 0
}
