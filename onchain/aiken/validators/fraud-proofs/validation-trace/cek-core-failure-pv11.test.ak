//// The failure route (arm 38, core group 14) of the protocol version 11
//// builtins (87 to 100): the budget hop and the semantics hop, which proves
//// the known failure, each beside the same step built without its spend.
//// Each failure is read at the largest shape both hops prove inside one L1
//// transaction's budget, at the next shape up on the semantics hop, and at
//// its consensus maximum: the direct bound (9,215 revealed bytes) for the
//// list, array, Value and multi-scalar failures, the 128-word modulus for
//// expModInteger with no inverse, whose greatest-common-divisor walk is
//// longest over consecutive Fibonacci numbers. A step no hop proves inside
//// the budget resolves for the defender.

use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use cardano/transaction.{Transaction}
use fraud_proofs/validation_trace/cek_core_failure_budget as failure_budget
use fraud_proofs/validation_trace/cek_core_failure_known as failure_known
use fraud_proofs/validation_trace/cek_core_pv11_budget as pv11_budget
use fraud_proofs/validation_trace/cek_core_pv11_semantics as pv11_semantics
use midgard/cek_builtin_v1.{ConstantValue, ValueWitnessV1}
use midgard/cek_constant_v1.{
  ArrayConstant, BlsG1Constant, BlsG2Constant, ByteStringConstant,
  ConstantTypeV1, ConstantWitnessV1, DataConstant, IntegerConstant, ListConstant,
  ValueConstant,
}
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1
use midgard/cek_semantic_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## The step

type FailureStep {
  datum: Option<chain.CoreDatum>,
  redeemer: chain.ArmRedeemer,
  transaction: Transaction,
}

/// The arm 38 step proving that builtin `tag` fails over `arguments`, at hop
/// `progress` (0 budget, 1 known failure).
fn failure_step(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  progress: Int,
) -> FailureStep {
  failure_step_in(14, tag, arguments, progress)
}

/// `failure_step` bound to core group `group`.
fn failure_step_in(
  group: Int,
  tag: Int,
  arguments: List<ValueWitnessV1>,
  progress: Int,
) -> FailureStep {
  let arguments_data: Data = arguments
  let witness = builtin.constr_data(38, [builtin.i_data(tag), arguments_data])
  let Pair(arguments_root, count) = cek_builtin_v1.arguments_root_v1(arguments)
  let budget = if tag > 86 {
    cek_builtin_v1.pv11_failure_budget_v1(tag, arguments)
  } else {
    cek_builtin_v1.authenticated_builtin_failure_budget(tag, arguments)
  }
  let state = machine.MachineStateV1 {
    mode: machine.mode_builtin,
    execution_index: 0,
    focus_root: cek_proof_v1.hash_builtin_value_v1(
      tag,
      0,
      count,
      arguments_root,
    ),
    environment_root: #"",
    continuation_root: #"",
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
  let bound = chain.CoreBound {
    prepared: builtin.constr_data(0, []),
    pre: state,
    post: machine.exact_state(
      state,
      machine.mode_halt_error,
      cek_proof_v1.hash_error_term_v1(),
      cek_proof_v1.empty_environment_root_v1,
      cek_proof_v1.empty_continuation_root_v1,
      machine.error_builtin_failure,
      budget.cpu,
      budget.memory,
    ),
    witness_hash: blake2b_256(cbor.serialise(witness)),
    arm: 38,
    group,
    progress,
    facts: builtin.constr_data(0, []),
  }
  let next: Data = chain.CoreBound { ..bound, progress: progress + 1 }
  FailureStep {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bound),
      },
    ),
    redeemer: ct.Continue(
      chain.VerifyArm { input_index: 0, output_index: 0, witness },
    ),
    transaction: Transaction {
      ..fixture.continue_mid_step_l1_tx_v1("cek-core-failure-pv11", next),
      extra_signatories: [fixture.fraud_prover_key_hash],
    },
  }
}

fn known(step: FailureStep) -> Bool {
  pv11_semantics.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

fn charged(step: FailureStep) -> Bool {
  pv11_budget.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

fn built(step: FailureStep) -> Bool {
  when step.datum is {
    Some(_) -> True
    None -> False
  }
}

// ## Constants

fn constant(constant_type: ConstantTypeV1, payload: Data) -> ValueWitnessV1 {
  ConstantValue(
    ConstantWitnessV1 {
      type_cbor: cek_constant_v1.encode_constant_type_v1(constant_type),
      payload_cbor: cbor.serialise(payload),
    },
  )
}

fn int(value: Int) -> ValueWitnessV1 {
  constant(IntegerConstant, builtin.i_data(value))
}

fn bytes(value: ByteArray) -> ValueWitnessV1 {
  constant(ByteStringConstant, builtin.b_data(value))
}

/// `count` one-byte integers (each below 24), from `index`.
fn small_int_items(index: Int, count: Int) -> List<Data> {
  if index >= count {
    []
  } else {
    [builtin.i_data(index % 24), ..small_int_items(index + 1, count)]
  }
}

/// The key of index `index`: the empty key, then every one-byte key.
fn key(index: Int) -> ByteArray {
  if index == 0 {
    #""
  } else {
    builtin.cons_bytearray(index - 1, #"")
  }
}

fn tokens(index: Int, last: Int, quantity: fn(Int) -> Int) -> Pairs<Data, Data> {
  if index >= last {
    []
  } else {
    [
      Pair(builtin.b_data(key(index % 257)), builtin.i_data(quantity(index))),
      ..tokens(index + 1, last, quantity)
    ]
  }
}

fn currencies(
  first: Int,
  count: Int,
  quantity: fn(Int) -> Int,
) -> Pairs<Data, Data> {
  if first >= count {
    []
  } else {
    let last = if first + 257 < count {
      first + 257
    } else {
      count
    }
    [
      Pair(
        builtin.b_data(key(first / 257)),
        builtin.map_data(tokens(first, last, quantity)),
      ),
      ..currencies(first + 257, count, quantity)
    ]
  }
}

/// The densest Value of `count` coins: currencies of 257 tokens each, keyed
/// by `key`, coin `index` holding `quantity(index)` (about 3 bytes a coin).
fn value_data(count: Int, quantity: fn(Int) -> Int) -> Data {
  builtin.map_data(currencies(0, count, quantity))
}

fn value(payload: Data) -> ValueWitnessV1 {
  constant(ValueConstant, payload)
}

/// One of every coin but the last of `count`, which holds `last`.
fn ones_then(count: Int, last: Int) -> fn(Int) -> Int {
  fn(index) {
    if index == count - 1 {
      last
    } else {
      1
    }
  }
}

// ## expModInteger (87) with no inverse: the longest walks

/// Fibonacci numbers `index - 1` and `index`.
fn fibonacci(index: Int) -> Pair<Int, Int> {
  do_fibonacci(index - 1, 0, 1)
}

fn do_fibonacci(steps: Int, previous: Int, current: Int) -> Pair<Int, Int> {
  if steps == 0 {
    Pair(previous, current)
  } else {
    do_fibonacci(steps - 1, current, previous + current)
  }
}

/// The inverse of twice Fibonacci number `index - 1` modulo twice Fibonacci
/// number `index`: their common divisor 2 takes the longest walk the
/// modulus' size allows.
fn no_inverse_step(index: Int, progress: Int) -> FailureStep {
  let Pair(previous, current) = fibonacci(index)
  failure_step(87, [int(2 * previous), int(-1), int(2 * current)], progress)
}

test exp_mod_no_inverse_known_at_56_words() {
  known(no_inverse_step(5161, 1))
}

test exp_mod_no_inverse_budget_at_56_words() {
  charged(no_inverse_step(5161, 0))
}

test exp_mod_no_inverse_fixture_only_at_56_words() {
  built(no_inverse_step(5161, 0))
}

test exp_mod_no_inverse_known_at_57_words() {
  known(no_inverse_step(5253, 1))
}

test exp_mod_no_inverse_fixture_only_at_57_words() {
  built(no_inverse_step(5253, 0))
}

/// Index 11,798 gives the largest such modulus under 2^8191, 128 words.
test exp_mod_no_inverse_known_at_128_words() {
  known(no_inverse_step(11798, 1))
}

test exp_mod_no_inverse_budget_at_128_words() {
  charged(no_inverse_step(11798, 0))
}

test exp_mod_no_inverse_fixture_only_at_128_words() {
  built(no_inverse_step(11798, 0))
}

// ## indexArray (91) past the end

/// Index `count` of an array of `count` one-byte integers; 9,210 fill the
/// direct bound.
fn index_array_step(count: Int, progress: Int) -> FailureStep {
  failure_step(
    91,
    [
      constant(
        ArrayConstant { element: IntegerConstant },
        builtin.list_data(small_int_items(0, count)),
      ),
      int(count),
    ],
    progress,
  )
}

test index_array_known_at_70_items() {
  known(index_array_step(70, 1))
}

test index_array_budget_at_70_items() {
  charged(index_array_step(70, 0))
}

test index_array_fixture_only_at_70_items() {
  built(index_array_step(70, 0))
}

test index_array_known_at_71_items() {
  known(index_array_step(71, 1))
}

test index_array_fixture_only_at_71_items() {
  built(index_array_step(71, 0))
}

test index_array_known_at_the_direct_bound() {
  known(index_array_step(9210, 1))
}

test index_array_budget_at_the_direct_bound() {
  charged(index_array_step(9210, 0))
}

test index_array_fixture_only_at_the_direct_bound() {
  built(index_array_step(9210, 0))
}

// ## Multi-scalar multiplication (92, 93) with the last scalar out of bound

const g1_generator =
  #"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"

const g2_generator =
  #"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"

/// 2^4095, the least scalar past the bound.
fn scalar_past_the_bound() -> Int {
  builtin.bytearray_to_integer(
    True,
    builtin.cons_bytearray(128, builtin.replicate_byte(511, 0)),
  )
}

/// `count` one-byte scalars then one past the bound, over one point.
fn msm_step(
  tag: Int,
  point_type: ConstantTypeV1,
  point: ByteArray,
  count: Int,
  progress: Int,
) -> FailureStep {
  failure_step(
    tag,
    [
      constant(
        ListConstant { element: IntegerConstant },
        builtin.list_data(
          list_append(
            small_int_items(0, count),
            [builtin.i_data(scalar_past_the_bound())],
          ),
        ),
      ),
      constant(
        ListConstant { element: point_type },
        builtin.list_data([builtin.b_data(point)]),
      ),
    ],
    progress,
  )
}

fn list_append(left: List<Data>, right: List<Data>) -> List<Data> {
  when left is {
    [] -> right
    [item, ..rest] -> [item, ..list_append(rest, right)]
  }
}

/// 8,630 one-byte scalars fill the direct bound.
fn g1_msm_step(count: Int, progress: Int) -> FailureStep {
  msm_step(92, BlsG1Constant, g1_generator, count, progress)
}

test g1_msm_known_at_60_scalars() {
  known(g1_msm_step(60, 1))
}

test g1_msm_budget_at_60_scalars() {
  charged(g1_msm_step(60, 0))
}

test g1_msm_fixture_only_at_60_scalars() {
  built(g1_msm_step(60, 0))
}

test g1_msm_known_at_61_scalars() {
  known(g1_msm_step(61, 1))
}

test g1_msm_fixture_only_at_61_scalars() {
  built(g1_msm_step(61, 0))
}

test g1_msm_known_at_the_direct_bound() {
  known(g1_msm_step(8630, 1))
}

test g1_msm_budget_at_the_direct_bound() {
  charged(g1_msm_step(8630, 0))
}

test g1_msm_fixture_only_at_the_direct_bound() {
  built(g1_msm_step(8630, 0))
}

/// 8,578 one-byte scalars fill the direct bound.
fn g2_msm_step(count: Int, progress: Int) -> FailureStep {
  msm_step(93, BlsG2Constant, g2_generator, count, progress)
}

test g2_msm_known_at_60_scalars() {
  known(g2_msm_step(60, 1))
}

test g2_msm_budget_at_60_scalars() {
  charged(g2_msm_step(60, 0))
}

test g2_msm_fixture_only_at_60_scalars() {
  built(g2_msm_step(60, 0))
}

test g2_msm_known_at_61_scalars() {
  known(g2_msm_step(61, 1))
}

test g2_msm_fixture_only_at_61_scalars() {
  built(g2_msm_step(61, 0))
}

test g2_msm_known_at_the_direct_bound() {
  known(g2_msm_step(8578, 1))
}

test g2_msm_budget_at_the_direct_bound() {
  charged(g2_msm_step(8578, 0))
}

test g2_msm_fixture_only_at_the_direct_bound() {
  built(g2_msm_step(8578, 0))
}

// ## The Value failures (94, 96, 97, 99, 100)

/// A 33-byte currency into a Value of `count` coins (3,000 at the direct
/// bound).
fn insert_coin_step(count: Int, progress: Int) -> FailureStep {
  failure_step(
    94,
    [
      bytes(builtin.replicate_byte(33, 0)),
      bytes(key(1)),
      int(1),
      value(value_data(count, ones_then(count, 1))),
    ],
    progress,
  )
}

test insert_coin_known_at_31_coins() {
  known(insert_coin_step(31, 1))
}

test insert_coin_budget_at_31_coins() {
  charged(insert_coin_step(31, 0))
}

test insert_coin_fixture_only_at_31_coins() {
  built(insert_coin_step(31, 0))
}

test insert_coin_known_at_32_coins() {
  known(insert_coin_step(32, 1))
}

test insert_coin_fixture_only_at_32_coins() {
  built(insert_coin_step(32, 0))
}

test insert_coin_known_at_the_direct_bound() {
  known(insert_coin_step(3000, 1))
}

test insert_coin_budget_at_the_direct_bound() {
  charged(insert_coin_step(3000, 0))
}

test insert_coin_fixture_only_at_the_direct_bound() {
  built(insert_coin_step(3000, 0))
}

/// Two Values of the same `count` coins whose last coins sum past the bound
/// (1,500 at the direct bound).
fn union_value_step(count: Int, progress: Int) -> FailureStep {
  let operand = value(
    value_data(count, ones_then(count, cek_constant_v1.max_value_quantity)),
  )
  failure_step(96, [operand, operand], progress)
}

test union_value_known_at_15_coins() {
  known(union_value_step(15, 1))
}

test union_value_budget_at_15_coins() {
  charged(union_value_step(15, 0))
}

test union_value_fixture_only_at_15_coins() {
  built(union_value_step(15, 0))
}

test union_value_known_at_16_coins() {
  known(union_value_step(16, 1))
}

test union_value_fixture_only_at_16_coins() {
  built(union_value_step(16, 0))
}

test union_value_known_at_the_direct_bound() {
  known(union_value_step(1500, 1))
}

test union_value_budget_at_the_direct_bound() {
  charged(union_value_step(1500, 0))
}

test union_value_fixture_only_at_the_direct_bound() {
  built(union_value_step(1500, 0))
}

/// Two Values of `count` coins, the second's last coin negative (1,500 at
/// the direct bound).
fn value_contains_step(count: Int, progress: Int) -> FailureStep {
  failure_step(
    97,
    [
      value(value_data(count, ones_then(count, 1))),
      value(value_data(count, ones_then(count, -1))),
    ],
    progress,
  )
}

test value_contains_known_at_15_coins() {
  known(value_contains_step(15, 1))
}

test value_contains_budget_at_15_coins() {
  charged(value_contains_step(15, 0))
}

test value_contains_fixture_only_at_15_coins() {
  built(value_contains_step(15, 0))
}

test value_contains_known_at_16_coins() {
  known(value_contains_step(16, 1))
}

test value_contains_fixture_only_at_16_coins() {
  built(value_contains_step(16, 0))
}

test value_contains_known_at_the_direct_bound() {
  known(value_contains_step(1500, 1))
}

test value_contains_budget_at_the_direct_bound() {
  charged(value_contains_step(1500, 0))
}

test value_contains_fixture_only_at_the_direct_bound() {
  built(value_contains_step(1500, 0))
}

/// Value Data of `count` coins whose last coin is zero (3,000 at the direct
/// bound).
fn un_value_data_step(count: Int, progress: Int) -> FailureStep {
  failure_step(
    99,
    [constant(DataConstant, value_data(count, ones_then(count, 0)))],
    progress,
  )
}

test un_value_data_known_at_37_coins() {
  known(un_value_data_step(37, 1))
}

test un_value_data_budget_at_37_coins() {
  charged(un_value_data_step(37, 0))
}

test un_value_data_fixture_only_at_37_coins() {
  built(un_value_data_step(37, 0))
}

test un_value_data_known_at_38_coins() {
  known(un_value_data_step(38, 1))
}

test un_value_data_fixture_only_at_38_coins() {
  built(un_value_data_step(38, 0))
}

test un_value_data_known_at_the_direct_bound() {
  known(un_value_data_step(3000, 1))
}

test un_value_data_budget_at_the_direct_bound() {
  charged(un_value_data_step(3000, 0))
}

test un_value_data_fixture_only_at_the_direct_bound() {
  built(un_value_data_step(3000, 0))
}

/// A Value of `count` coins doubled, its last coin at the bound (3,000 at
/// the direct bound).
fn scale_value_step(count: Int, progress: Int) -> FailureStep {
  failure_step(
    100,
    [
      int(2),
      value(
        value_data(count, ones_then(count, cek_constant_v1.max_value_quantity)),
      ),
    ],
    progress,
  )
}

test scale_value_known_at_32_coins() {
  known(scale_value_step(32, 1))
}

test scale_value_budget_at_32_coins() {
  charged(scale_value_step(32, 0))
}

test scale_value_fixture_only_at_32_coins() {
  built(scale_value_step(32, 0))
}

test scale_value_known_at_33_coins() {
  known(scale_value_step(33, 1))
}

test scale_value_fixture_only_at_33_coins() {
  built(scale_value_step(33, 0))
}

test scale_value_known_at_the_direct_bound() {
  known(scale_value_step(3000, 1))
}

test scale_value_budget_at_the_direct_bound() {
  charged(scale_value_step(3000, 0))
}

test scale_value_fixture_only_at_the_direct_bound() {
  built(scale_value_step(3000, 0))
}

// ## One route

// A protocol version 11 failure is proved in core group 14 alone. The known
// failure arm of group 9 refuses its tag (beside a division by zero both
// group 9 arms prove), and the protocol version 11 arms refuse any other
// group.

fn known_v3(step: FailureStep) -> Bool {
  failure_known.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

fn charged_v3(step: FailureStep) -> Bool {
  failure_budget.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

fn divide_by_zero_step(group: Int, progress: Int) -> FailureStep {
  failure_step_in(group, 4, [int(1), int(0)], progress)
}

fn small_index_array_step(group: Int, progress: Int) -> FailureStep {
  failure_step_in(
    group,
    91,
    [
      constant(
        ArrayConstant { element: IntegerConstant },
        builtin.list_data(small_int_items(0, 3)),
      ),
      int(3),
    ],
    progress,
  )
}

test known_arms_prove_a_division_by_zero() {
  and {
    known_v3(divide_by_zero_step(9, 0)),
    charged_v3(divide_by_zero_step(9, 1)),
  }
}

test pv11_arms_prove_a_small_index_array_failure() {
  and {
    charged(small_index_array_step(14, 0)),
    known(small_index_array_step(14, 1)),
  }
}

test known_failure_refuses_a_pv11_failure() fail {
  known_v3(small_index_array_step(9, 0))
}

test pv11_budget_refuses_group_9() fail {
  charged(small_index_array_step(9, 0))
}

test pv11_semantics_refuses_group_9() fail {
  known(small_index_array_step(9, 1))
}

test pv11_semantics_refuses_a_division_by_zero() fail {
  known(divide_by_zero_step(14, 1))
}
