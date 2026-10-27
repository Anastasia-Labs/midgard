//// The protocol version 11 builtins (tags 87 to 100) on the direct route:
//// each success proven, each wrong result refused, each application Cardano
//// refuses unprovable as a success, the charge, the argument kinds, and the
//// failures the direct failure arm proves with what Cardano charges for them.

use aiken/builtin
use aiken/cbor
use midgard/cek_builtin_v1.{ConstantValue, RuntimeConstantValue}
use midgard/cek_constant_v1.{
  ArrayConstant, BlsG1Constant, BlsG2Constant, BooleanConstant, ConstantTypeV1,
  ConstantWitnessV1, DataConstant, IntegerConstant, ListConstant, ValueConstant,
}
use midgard/cek_cost_v1.{BuiltinBudgetV1}

fn witness(constant_type: ConstantTypeV1, payload: Data) -> ConstantWitnessV1 {
  ConstantWitnessV1 {
    type_cbor: cek_constant_v1.encode_constant_type_v1(constant_type),
    payload_cbor: cbor.serialise(payload),
  }
}

fn constant(
  constant_type: ConstantTypeV1,
  payload: Data,
) -> cek_builtin_v1.ValueWitnessV1 {
  ConstantValue(witness(constant_type, payload))
}

fn int(value: Int) -> cek_builtin_v1.ValueWitnessV1 {
  constant(IntegerConstant, builtin.i_data(value))
}

fn bytes(value: ByteArray) -> cek_builtin_v1.ValueWitnessV1 {
  constant(cek_constant_v1.ByteStringConstant, builtin.b_data(value))
}

fn bool(value: Bool) -> cek_builtin_v1.ValueWitnessV1 {
  constant(
    BooleanConstant,
    builtin.constr_data(
      if value {
        1
      } else {
        0
      },
      [],
    ),
  )
}

fn int_items(values: List<Int>) -> Data {
  when values is {
    [] -> builtin.list_data([])
    [value, ..rest] ->
      builtin.list_data(
        [builtin.i_data(value), ..builtin.un_list_data(int_items(rest))],
      )
  }
}

fn ints(values: List<Int>) -> cek_builtin_v1.ValueWitnessV1 {
  constant(ListConstant { element: IntegerConstant }, int_items(values))
}

fn int_array(values: List<Int>) -> cek_builtin_v1.ValueWitnessV1 {
  constant(ArrayConstant { element: IntegerConstant }, int_items(values))
}

const policy = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const token = #"bbbbbbbb"

const other_token = #"cccccccc"

/// `{policy: {token: quantity}}` as Data.
fn one_token(name: ByteArray, quantity: Int) -> Data {
  builtin.map_data(
    [
      Pair(
        builtin.b_data(policy),
        builtin.map_data([Pair(builtin.b_data(name), builtin.i_data(quantity))]),
      ),
    ],
  )
}

fn value(payload: Data) -> cek_builtin_v1.ValueWitnessV1 {
  constant(ValueConstant, payload)
}

fn proves(
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
  result: cek_builtin_v1.ValueWitnessV1,
) -> Bool {
  cek_builtin_v1.verify_direct_builtin_v1(tag, arguments, result)
}

const g1_generator =
  #"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"

const g2_generator =
  #"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"

test exp_mod_integer_proves_a_power_and_an_inverse() {
  and {
    proves(87, [int(3), int(5), int(7)], int(5)),
    proves(87, [int(3), int(-1), int(7)], int(5)),
    proves(87, [int(123), int(456), int(1)], int(0)),
    !proves(87, [int(3), int(5), int(7)], int(4)),
  }
}

test exp_mod_integer_cannot_prove_a_missing_inverse() fail {
  proves(87, [int(2), int(-1), int(4)], int(0))
}

test exp_mod_integer_cannot_prove_a_zero_modulus() fail {
  proves(87, [int(3), int(5), int(0)], int(0))
}

test drop_list_proves_every_count() {
  and {
    proves(88, [int(1), ints([5, 6, 7])], ints([6, 7])),
    proves(88, [int(-3), ints([5, 6, 7])], ints([5, 6, 7])),
    proves(88, [int(3), ints([5, 6, 7])], ints([])),
    proves(88, [int(9223372036854775808), ints([5, 6, 7])], ints([])),
    !proves(88, [int(1), ints([5, 6, 7])], ints([5, 6])),
    !proves(88, [int(1), ints([5, 6, 7])], int_array([6, 7])),
  }
}

test array_builtins_prove_on_the_committed_payload() {
  and {
    proves(89, [int_array([5, 6, 7])], int(3)),
    proves(89, [int_array([])], int(0)),
    !proves(89, [int_array([5, 6, 7])], int(2)),
    proves(90, [ints([5, 6])], int_array([5, 6])),
    !proves(90, [ints([5, 6])], ints([5, 6])),
    proves(91, [int_array([5, 6, 7]), int(2)], int(7)),
    proves(91, [int_array([5, 6, 7]), int(0)], int(5)),
    !proves(91, [int_array([5, 6, 7]), int(1)], int(5)),
  }
}

test index_array_cannot_prove_an_index_past_the_end() fail {
  proves(91, [int_array([5, 6, 7]), int(3)], int(7))
}

test index_array_cannot_prove_a_negative_index() fail {
  proves(91, [int_array([5, 6, 7]), int(-1)], int(5))
}

test multi_scalar_mul_proves_on_both_groups() {
  let g1 = builtin.bls12_381_g1_uncompress(g1_generator)
  let g2 = builtin.bls12_381_g2_uncompress(g2_generator)
  let g1_points = constant(
    ListConstant { element: BlsG1Constant },
    builtin.list_data(
      [builtin.b_data(g1_generator), builtin.b_data(g1_generator)],
    ),
  )
  let g2_points = constant(
    ListConstant { element: BlsG2Constant },
    builtin.list_data([builtin.b_data(g2_generator)]),
  )
  and {
    proves(
      92,
      [ints([2, 3]), g1_points],
      constant(
        BlsG1Constant,
        builtin.b_data(
          builtin.bls12_381_g1_compress(builtin.bls12_381_g1_scalar_mul(5, g1)),
        ),
      ),
    ),
    // The scalars and points zip to the shorter list.
    proves(
      93,
      [ints([4, 9]), g2_points],
      constant(
        BlsG2Constant,
        builtin.b_data(
          builtin.bls12_381_g2_compress(builtin.bls12_381_g2_scalar_mul(4, g2)),
        ),
      ),
    ),
    !proves(
      92,
      [ints([2, 3]), g1_points],
      constant(BlsG1Constant, builtin.b_data(g1_generator)),
    ),
  }
}

test insert_and_lookup_coin_prove() {
  let source = value(one_token(token, 1))
  let inserted = value(
    builtin.map_data(
      [
        Pair(
          builtin.b_data(policy),
          builtin.map_data(
            [
              Pair(builtin.b_data(token), builtin.i_data(1)),
              Pair(builtin.b_data(other_token), builtin.i_data(3)),
            ],
          ),
        ),
      ],
    ),
  )
  and {
    proves(94, [bytes(policy), bytes(other_token), int(3), source], inserted),
    proves(
      94,
      [bytes(policy), bytes(token), int(0), source],
      value(builtin.map_data([])),
    ),
    !proves(94, [bytes(policy), bytes(other_token), int(2), source], inserted),
    proves(95, [bytes(policy), bytes(token), source], int(1)),
    proves(95, [bytes(policy), bytes(other_token), source], int(0)),
    !proves(95, [bytes(policy), bytes(token), source], int(0)),
  }
}

test insert_coin_cannot_prove_a_33_byte_currency() fail {
  proves(
    94,
    [
      bytes(
        #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      ),
      bytes(token),
      int(1),
      value(builtin.map_data([])),
    ],
    value(builtin.map_data([])),
  )
}

test insert_coin_cannot_prove_a_quantity_past_128_bits() fail {
  proves(
    94,
    [
      bytes(policy),
      bytes(token),
      int(170141183460469231731687303715884105728),
      value(builtin.map_data([])),
    ],
    value(one_token(token, 170141183460469231731687303715884105728)),
  )
}

test union_contains_and_scale_prove() {
  and {
    proves(
      96,
      [value(one_token(token, 1)), value(one_token(token, -1))],
      value(builtin.map_data([])),
    ),
    proves(
      96,
      [value(one_token(token, 1)), value(one_token(token, 2))],
      value(one_token(token, 3)),
    ),
    proves(
      97,
      [value(one_token(token, 2)), value(one_token(token, 1))],
      bool(True),
    ),
    proves(
      97,
      [value(one_token(token, 1)), value(one_token(token, 2))],
      bool(False),
    ),
    !proves(
      97,
      [value(one_token(token, 2)), value(one_token(token, 1))],
      bool(False),
    ),
    proves(
      100,
      [int(2), value(one_token(token, 1))],
      value(one_token(token, 2)),
    ),
    proves(
      100,
      [int(0), value(one_token(token, 1))],
      value(builtin.map_data([])),
    ),
    !proves(
      100,
      [int(2), value(one_token(token, 1))],
      value(one_token(token, 1)),
    ),
  }
}

test value_contains_cannot_prove_a_negative_quantity() fail {
  proves(
    97,
    [value(one_token(token, -1)), value(builtin.map_data([]))],
    bool(True),
  )
}

test scale_value_cannot_prove_an_overflow() fail {
  proves(
    100,
    [int(2), value(one_token(token, 170141183460469231731687303715884105727))],
    value(one_token(token, 1)),
  )
}

test value_data_round_trips() {
  and {
    proves(
      98,
      [value(one_token(token, 1))],
      constant(DataConstant, one_token(token, 1)),
    ),
    !proves(
      98,
      [value(one_token(token, 1))],
      constant(DataConstant, one_token(token, 2)),
    ),
    proves(
      99,
      [constant(DataConstant, one_token(token, 1))],
      value(one_token(token, 1)),
    ),
    !proves(
      99,
      [constant(DataConstant, one_token(token, 1))],
      value(one_token(token, 2)),
    ),
  }
}

test un_value_data_cannot_prove_a_zero_quantity() fail {
  proves(
    99,
    [constant(DataConstant, one_token(token, 0))],
    value(builtin.map_data([])),
  )
}

test pv11_charge_on_the_sizes_cardano_measures() {
  and {
    // dropList is charged on the count's magnitude, saturated at 2^63 - 1.
    cek_builtin_v1.direct_builtin_budget_v1(
      88,
      [int(-9223372036854775809), ints([5])],
    ) == BuiltinBudgetV1 { cpu: 9223372036854775807, memory: 4 },
    // insertCoin on a Value of one currency of one token: depth 2.
    cek_builtin_v1.direct_builtin_budget_v1(
      94,
      [bytes(policy), bytes(token), int(1), value(one_token(token, 1))],
    ) == BuiltinBudgetV1 { cpu: 393750, memory: 87 },
    // unValueData on five Data nodes.
    cek_builtin_v1.direct_builtin_budget_v1(
      99,
      [constant(DataConstant, one_token(token, 1))],
    ) == BuiltinBudgetV1 { cpu: 480690, memory: 56 },
    // listToArray on the list's length.
    cek_builtin_v1.direct_builtin_budget_v1(90, [ints([5, 6, 7])]) == BuiltinBudgetV1 {
      cpu: 75514,
      memory: 10,
    },
  }
}

test pv11_argument_kinds() {
  let runtime = fn(constant_type, payload) {
    RuntimeConstantValue(witness(constant_type, payload))
  }
  let a_value = runtime(ValueConstant, one_token(token, 1))
  let a_data = runtime(DataConstant, one_token(token, 1))
  let an_int = runtime(IntegerConstant, builtin.i_data(1))
  let some_bytes = runtime(
    cek_constant_v1.ByteStringConstant,
    builtin.b_data(policy),
  )
  let g1_points = runtime(
    ListConstant { element: BlsG1Constant },
    builtin.list_data([builtin.b_data(g1_generator)]),
  )
  let scalars = runtime(
    ListConstant { element: IntegerConstant },
    int_items([1]),
  )
  and {
    cek_builtin_v1.builtin_arguments_are_well_typed_v1(
      94,
      [some_bytes, some_bytes, an_int, a_value],
    ),
    !cek_builtin_v1.builtin_arguments_are_well_typed_v1(
      94,
      [some_bytes, some_bytes, an_int, a_data],
    ),
    cek_builtin_v1.builtin_arguments_are_well_typed_v1(99, [a_data]),
    !cek_builtin_v1.builtin_arguments_are_well_typed_v1(99, [a_value]),
    cek_builtin_v1.builtin_arguments_are_well_typed_v1(92, [scalars,
        g1_points]),
    !cek_builtin_v1.builtin_arguments_are_well_typed_v1(
      93,
      [scalars, g1_points],
    ),
    !cek_builtin_v1.builtin_arguments_are_well_typed_v1(
      92,
      [g1_points, scalars],
    ),
    cek_builtin_v1.builtin_arguments_are_well_typed_v1(
      91,
      [
        runtime(ArrayConstant { element: IntegerConstant }, int_items([1])),
        an_int,
      ],
    ),
    !cek_builtin_v1.builtin_arguments_are_well_typed_v1(91, [scalars, an_int]),
  }
}

test pv11_failures_are_known_and_charged() {
  let missing_inverse = [int(2), int(-1), int(4)]
  let past_the_end = [int_array([5, 6, 7]), int(3)]
  and {
    cek_builtin_v1.known_builtin_failure_v1(87, missing_inverse),
    cek_builtin_v1.authenticated_builtin_failure_budget(87, missing_inverse) == cek_builtin_v1.direct_builtin_budget_v1(
      87,
      missing_inverse,
    ),
    cek_builtin_v1.known_builtin_failure_v1(91, past_the_end),
    cek_builtin_v1.authenticated_builtin_failure_budget(91, past_the_end) == cek_builtin_v1.direct_builtin_budget_v1(
      91,
      past_the_end,
    ),
  }
}

test index_array_outside_64_bits_fails_uncharged() {
  let arguments = [int_array([5, 6, 7]), int(18446744073709551616)]
  and {
    cek_builtin_v1.known_builtin_failure_v1(91, arguments),
    cek_builtin_v1.authenticated_builtin_failure_budget(91, arguments) == BuiltinBudgetV1 {
      cpu: 0,
      memory: 0,
    },
  }
}

test pv11_successes_are_not_known_failures() {
  let an_inverse = [int(3), int(-5), int(1000003)]
  let the_last_item = [int_array([5, 6, 7]), int(2)]
  and {
    !cek_builtin_v1.known_builtin_failure_v1(87, an_inverse),
    !cek_builtin_v1.known_builtin_failure_v1(87, [int(2), int(-1), int(1)]),
    !cek_builtin_v1.known_builtin_failure_v1(91, the_last_item),
    !cek_builtin_v1.known_builtin_failure_v1(88, [int(9), ints([5])]),
    !cek_builtin_v1.known_builtin_failure_v1(
      94,
      [bytes(#"00"), bytes(token), int(0), value(one_token(token, 1))],
    ),
  }
}
