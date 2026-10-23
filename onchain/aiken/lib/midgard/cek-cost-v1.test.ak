use aiken/collection/list
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

test bitwise_budget_follows_the_pinned_v3_row() {
  cek_cost_v1.builtin_budget_v1(75, [1, 5, 5]) == cek_cost_v1.BuiltinBudgetV1 {
    cpu: 107406,
    memory: 5,
  }
}

// PV 11 division: divideInteger (3) and modInteger (6) cost the quadratic over
// (larger, smaller) word sizes on both sides of the diagonal; quotientInteger
// (4) and remainderInteger (5) are constant above it.
test division_budgets_follow_the_pv11_rows_on_both_sides_of_the_diagonal() {
  let quadratic = cek_cost_v1.BuiltinBudgetV1 { cpu: 176273, memory: 1 }
  let constant = cek_cost_v1.BuiltinBudgetV1 { cpu: 85848, memory: 1 }
  and {
    cek_cost_v1.builtin_budget_v1(3, [2, 10]) == quadratic,
    // modInteger's memory is the divisor's size.
    cek_cost_v1.builtin_budget_v1(6, [2, 10]) == cek_cost_v1.BuiltinBudgetV1 {
      cpu: 176273,
      memory: 10,
    },
    cek_cost_v1.builtin_budget_v1(4, [2, 10]) == constant,
    cek_cost_v1.builtin_budget_v1(5, [2, 10]).cpu == 85848,
    cek_cost_v1.builtin_budget_v1(3, [10, 2]).cpu == 176273,
    cek_cost_v1.builtin_budget_v1(4, [10, 2]).cpu == 176273,
  }
}

// multiplyInteger costs cpu over the product of the word sizes and memory
// over their sum.
test multiply_integer_budget_follows_the_product_of_the_sizes() {
  and {
    cek_cost_v1.builtin_budget_v1(2, [4, 3]) == cek_cost_v1.BuiltinBudgetV1 {
      cpu: 96662,
      memory: 7,
    },
    cek_cost_v1.builtin_budget_v1(2, [1, 1]) == cek_cost_v1.BuiltinBudgetV1 {
      cpu: 90953,
      memory: 2,
    },
  }
}

// equalsByteString is linear on the diagonal and constant off it.
test equals_bytestring_budget_follows_the_pv11_row() {
  and {
    cek_cost_v1.builtin_budget_v1(15, [4, 4]) == cek_cost_v1.BuiltinBudgetV1 {
      cpu: 29055,
      memory: 1,
    },
    cek_cost_v1.builtin_budget_v1(15, [4, 5]) == cek_cost_v1.BuiltinBudgetV1 {
      cpu: 30623,
      memory: 1,
    },
  }
}

test invalid_arity_fails_closed() fail {
  cek_cost_v1.builtin_budget_v1(0, [1]).cpu == 0
}

test unknown_tag_fails_closed() fail {
  cek_cost_v1.builtin_budget_v1(87, [1]).cpu == 0
}

test division_budget_is_the_division_row() {
  list.all(
    [3, 4, 5, 6],
    fn(tag) {
      list.all(
        [[4096, 1], [1, 4096], [1, 1], [0, 0], [300, 299], [7, 4000]],
        fn(sizes) {
          cek_cost_v1.division_budget_v1(tag, sizes) == cek_cost_v1.builtin_budget_v1(
            tag,
            sizes,
          )
        },
      )
    },
  )
}

test division_budget_refuses_another_builtin() fail {
  cek_cost_v1.division_budget_v1(2, [1, 1]).cpu > 0
}

test signature_budget_is_the_builtin_row() {
  list.all(
    [21, 53],
    fn(tag) {
      list.all(
        [[4, 8192, 8], [4, 1, 8], [1, 1, 1], [0, 0, 0], [8193, 1150, 9]],
        fn(sizes) {
          cek_cost_v1.signature_budget_v1(tag, sizes) == cek_cost_v1.builtin_budget_v1(
            tag,
            sizes,
          )
        },
      )
    },
  )
}

test signature_budget_refuses_another_builtin() fail {
  cek_cost_v1.signature_budget_v1(52, [5, 4, 8]).cpu > 0
}
