//// The direct success route (arm 30, group 3) for the protocol version 11
//// builtins (87 to 100): the roots, budget and semantics hops, each beside
//// the same step built without its spend. Each builtin is read at the
//// largest shape every hop proves inside one L1 transaction's budget, at
//// the next shape up on the hop that bounds it, and at its consensus
//// maximum: the direct bound (9,215 revealed bytes) for the list, array,
//// Value and multi-scalar builtins, one L2 transaction's budget for
//// expModInteger. A step no hop proves inside the budget resolves for the
//// defender.

use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use cardano/transaction.{Transaction}
use fraud_proofs/validation_trace/cek_core_builtin_budget as builtin_budget
use fraud_proofs/validation_trace/cek_core_builtin_roots as builtin_roots
use fraud_proofs/validation_trace/cek_core_direct_scalar as direct_scalar
use midgard/cek_builtin_v1.{ConstantValue, ValueWitnessV1}
use midgard/cek_constant_v1.{
  ArrayConstant, BlsG1Constant, BlsG2Constant, BooleanConstant,
  ByteStringConstant, ConstantTypeV1, ConstantWitnessV1, DataConstant,
  IntegerConstant, ListConstant, ValueConstant,
}
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1
use midgard/cek_semantic_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## The step

type DirectStep {
  datum: Option<chain.CoreDatum>,
  redeemer: chain.ArmRedeemer,
  transaction: Transaction,
}

/// The arm 30 step proving that builtin `tag` over `arguments` returns
/// `result`, at hop `progress` (0 roots, 1 budget, 2 semantics). Every hop's
/// step carries the roots the first hop derives.
fn direct_step(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  progress: Int,
) -> DirectStep {
  let arguments_data: Data = arguments
  let result_data: Data = result
  let witness = builtin.constr_data(
    30,
    [builtin.i_data(tag), arguments_data, result_data],
  )
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let builtin_root = cek_proof_v1.hash_builtin_value_v1(
    tag,
    0,
    arguments_count,
    arguments_root,
  )
  let result_root = cek_builtin_v1.result_root_v1(result)
  let roots: Data = chain.BuiltinRoots {
    arguments_root,
    arguments_count,
    builtin_root,
    result_root,
  }
  let budget = cek_builtin_v1.direct_builtin_budget_v1(tag, arguments)
  let state = machine.MachineStateV1 {
    mode: machine.mode_builtin,
    execution_index: 0,
    focus_root: builtin_root,
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
      machine.mode_return,
      result_root,
      cek_proof_v1.empty_environment_root_v1,
      state.continuation_root,
      0,
      budget.cpu,
      budget.memory,
    ),
    witness_hash: blake2b_256(cbor.serialise(witness)),
    arm: 30,
    group: 3,
    progress,
    facts: roots,
  }
  let next: Data = chain.CoreBound { ..bound, progress: progress + 1 }
  DirectStep {
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
      ..fixture.continue_mid_step_l1_tx_v1("cek-core-direct-pv11", next),
      extra_signatories: [fixture.fraud_prover_key_hash],
    },
  }
}

fn roots(step: DirectStep) -> Bool {
  builtin_roots.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

fn budget(step: DirectStep) -> Bool {
  builtin_budget.main.spend(
    [fixture.next_step_script_hash],
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

fn semantics(step: DirectStep) -> Bool {
  direct_scalar.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

fn built(step: DirectStep) -> Bool {
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

/// `count` one-byte integers (each below 24).
fn small_int_items(index: Int, count: Int) -> List<Data> {
  if index >= count {
    []
  } else {
    [builtin.i_data(index % 24), ..small_int_items(index + 1, count)]
  }
}

fn int_list(items: List<Data>) -> ValueWitnessV1 {
  constant(ListConstant { element: IntegerConstant }, builtin.list_data(items))
}

fn int_array(items: List<Data>) -> ValueWitnessV1 {
  constant(ArrayConstant { element: IntegerConstant }, builtin.list_data(items))
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

fn ones(_index: Int) -> Int {
  1
}

fn twos(_index: Int) -> Int {
  2
}

// ## expModInteger (87): bounded by its computation

/// The largest integer of `words` 64-bit words with its top bit clear:
/// 2^(64 * words - 1) - 1, so 128 words is the modulus bound.
fn word_integer(words: Int) -> Int {
  builtin.bytearray_to_integer(
    True,
    builtin.cons_bytearray(127, builtin.replicate_byte(8 * words - 1, 255)),
  )
}

fn exp_mod_step(base: Int, exponent: Int, modulus: Int, progress: Int) {
  direct_step(
    87,
    [int(base), int(exponent), int(modulus)],
    int(builtin.exp_mod_integer(base, exponent, modulus)),
    progress,
  )
}

/// Exponent and modulus of `words` words, the base below the modulus.
fn exp_mod_equal_step(words: Int, progress: Int) -> DirectStep {
  let modulus = word_integer(words)
  exp_mod_step(modulus - 2, word_integer(words), modulus, progress)
}

/// Exponent and modulus of `words` words, the base at the 128-word bound.
fn exp_mod_wide_base_step(words: Int, progress: Int) -> DirectStep {
  exp_mod_step(
    word_integer(128),
    word_integer(words),
    word_integer(words),
    progress,
  )
}

/// The modulus at the 128-word bound, an exponent of `words` words.
fn exp_mod_wide_modulus_step(words: Int, progress: Int) -> DirectStep {
  let modulus = word_integer(128)
  exp_mod_step(modulus - 2, word_integer(words), modulus, progress)
}

test exp_mod_roots_at_50_words() {
  roots(exp_mod_equal_step(50, 0))
}

test exp_mod_budget_at_50_words() {
  budget(exp_mod_equal_step(50, 1))
}

test exp_mod_semantics_at_50_words() {
  semantics(exp_mod_equal_step(50, 2))
}

test exp_mod_fixture_only_at_50_words() {
  built(exp_mod_equal_step(50, 2))
}

test exp_mod_semantics_at_51_words() {
  semantics(exp_mod_equal_step(51, 2))
}

test exp_mod_fixture_only_at_51_words() {
  built(exp_mod_equal_step(51, 2))
}

/// The largest equal operands one transaction's budget admits.
test exp_mod_semantics_at_55_words() {
  semantics(exp_mod_equal_step(55, 2))
}

test exp_mod_fixture_only_at_55_words() {
  built(exp_mod_equal_step(55, 2))
}

test exp_mod_semantics_at_43_words_under_a_wide_base() {
  semantics(exp_mod_wide_base_step(43, 2))
}

test exp_mod_fixture_only_at_43_words_under_a_wide_base() {
  built(exp_mod_wide_base_step(43, 2))
}

test exp_mod_semantics_at_44_words_under_a_wide_base() {
  semantics(exp_mod_wide_base_step(44, 2))
}

test exp_mod_fixture_only_at_44_words_under_a_wide_base() {
  built(exp_mod_wide_base_step(44, 2))
}

test exp_mod_roots_at_8_words_under_a_wide_modulus() {
  roots(exp_mod_wide_modulus_step(8, 0))
}

test exp_mod_budget_at_8_words_under_a_wide_modulus() {
  budget(exp_mod_wide_modulus_step(8, 1))
}

test exp_mod_semantics_at_8_words_under_a_wide_modulus() {
  semantics(exp_mod_wide_modulus_step(8, 2))
}

test exp_mod_fixture_only_at_8_words_under_a_wide_modulus() {
  built(exp_mod_wide_modulus_step(8, 2))
}

test exp_mod_semantics_at_9_words_under_a_wide_modulus() {
  semantics(exp_mod_wide_modulus_step(9, 2))
}

test exp_mod_fixture_only_at_9_words_under_a_wide_modulus() {
  built(exp_mod_wide_modulus_step(9, 2))
}

/// The longest exponent one transaction's budget admits under the widest
/// modulus.
test exp_mod_semantics_at_11_words_under_a_wide_modulus() {
  semantics(exp_mod_wide_modulus_step(11, 2))
}

test exp_mod_fixture_only_at_11_words_under_a_wide_modulus() {
  built(exp_mod_wide_modulus_step(11, 2))
}

// ## dropList, lengthOfArray, listToArray, indexArray (88 to 91)

/// Dropping all but the last of `count` one-byte integers; 9,207 fill the
/// direct bound with the count and the result.
fn drop_list_step(count: Int, progress: Int) -> DirectStep {
  let items = small_int_items(0, count)
  direct_step(
    88,
    [int(count - 1), int_list(items)],
    int_list(builtin.drop_list(count - 1, items)),
    progress,
  )
}

test drop_list_roots_at_80_items() {
  roots(drop_list_step(80, 0))
}

test drop_list_budget_at_80_items() {
  budget(drop_list_step(80, 1))
}

test drop_list_semantics_at_80_items() {
  semantics(drop_list_step(80, 2))
}

test drop_list_fixture_only_at_80_items() {
  built(drop_list_step(80, 2))
}

test drop_list_roots_at_81_items() {
  roots(drop_list_step(81, 0))
}

test drop_list_fixture_only_at_81_items() {
  built(drop_list_step(81, 2))
}

test drop_list_roots_at_the_direct_bound() {
  roots(drop_list_step(9207, 0))
}

test drop_list_budget_at_the_direct_bound() {
  budget(drop_list_step(9207, 1))
}

test drop_list_semantics_at_the_direct_bound() {
  semantics(drop_list_step(9207, 2))
}

test drop_list_fixture_only_at_the_direct_bound() {
  built(drop_list_step(9207, 2))
}

/// An array of `count` one-byte integers and its length; 9,210 fill the
/// direct bound.
fn length_of_array_step(count: Int, progress: Int) -> DirectStep {
  direct_step(89, [int_array(small_int_items(0, count))], int(count), progress)
}

test length_of_array_roots_at_88_items() {
  roots(length_of_array_step(88, 0))
}

test length_of_array_budget_at_88_items() {
  budget(length_of_array_step(88, 1))
}

test length_of_array_semantics_at_88_items() {
  semantics(length_of_array_step(88, 2))
}

test length_of_array_fixture_only_at_88_items() {
  built(length_of_array_step(88, 2))
}

test length_of_array_roots_at_89_items() {
  roots(length_of_array_step(89, 0))
}

test length_of_array_fixture_only_at_89_items() {
  built(length_of_array_step(89, 2))
}

test length_of_array_roots_at_the_direct_bound() {
  roots(length_of_array_step(9210, 0))
}

test length_of_array_budget_at_the_direct_bound() {
  budget(length_of_array_step(9210, 1))
}

test length_of_array_semantics_at_the_direct_bound() {
  semantics(length_of_array_step(9210, 2))
}

test length_of_array_fixture_only_at_the_direct_bound() {
  built(length_of_array_step(9210, 2))
}

/// A list of `count` one-byte integers and the array of them; 4,605 fill
/// the direct bound.
fn list_to_array_step(count: Int, progress: Int) -> DirectStep {
  let items = small_int_items(0, count)
  direct_step(90, [int_list(items)], int_array(items), progress)
}

test list_to_array_roots_at_42_items() {
  roots(list_to_array_step(42, 0))
}

test list_to_array_budget_at_42_items() {
  budget(list_to_array_step(42, 1))
}

test list_to_array_semantics_at_42_items() {
  semantics(list_to_array_step(42, 2))
}

test list_to_array_fixture_only_at_42_items() {
  built(list_to_array_step(42, 2))
}

test list_to_array_roots_at_43_items() {
  roots(list_to_array_step(43, 0))
}

test list_to_array_fixture_only_at_43_items() {
  built(list_to_array_step(43, 2))
}

test list_to_array_roots_at_the_direct_bound() {
  roots(list_to_array_step(4605, 0))
}

test list_to_array_budget_at_the_direct_bound() {
  budget(list_to_array_step(4605, 1))
}

test list_to_array_semantics_at_the_direct_bound() {
  semantics(list_to_array_step(4605, 2))
}

test list_to_array_fixture_only_at_the_direct_bound() {
  built(list_to_array_step(4605, 2))
}

/// The last of an array of `count` one-byte integers; 9,209 fill the direct
/// bound.
fn index_array_step(count: Int, progress: Int) -> DirectStep {
  direct_step(
    91,
    [int_array(small_int_items(0, count)), int(count - 1)],
    int(( count - 1 ) % 24),
    progress,
  )
}

test index_array_roots_at_86_items() {
  roots(index_array_step(86, 0))
}

test index_array_budget_at_86_items() {
  budget(index_array_step(86, 1))
}

test index_array_semantics_at_86_items() {
  semantics(index_array_step(86, 2))
}

test index_array_fixture_only_at_86_items() {
  built(index_array_step(86, 2))
}

test index_array_roots_at_87_items() {
  roots(index_array_step(87, 0))
}

test index_array_fixture_only_at_87_items() {
  built(index_array_step(87, 2))
}

test index_array_roots_at_the_direct_bound() {
  roots(index_array_step(9209, 0))
}

test index_array_budget_at_the_direct_bound() {
  budget(index_array_step(9209, 1))
}

test index_array_semantics_at_the_direct_bound() {
  semantics(index_array_step(9209, 2))
}

test index_array_fixture_only_at_the_direct_bound() {
  built(index_array_step(9209, 2))
}

// ## Multi-scalar multiplication (92, 93): bounded by their computation

const g1_generator =
  #"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"

const g2_generator =
  #"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"

fn repeated(item: a, count: Int) -> List<a> {
  if count <= 0 {
    []
  } else {
    [item, ..repeated(item, count - 1)]
  }
}

/// `scalars` one-byte scalars over `points` copies of the G1 generator.
fn g1_msm_step(scalars: Int, points: Int, progress: Int) -> DirectStep {
  let scalar_items = small_int_items(1, scalars + 1)
  direct_step(
    92,
    [
      int_list(scalar_items),
      constant(
        ListConstant { element: BlsG1Constant },
        builtin.list_data(repeated(builtin.b_data(g1_generator), points)),
      ),
    ],
    constant(
      BlsG1Constant,
      builtin.b_data(
        builtin.bls12_381_g1_compress(
          builtin.bls12_381_g1_multi_scalar_mul(
            list_ints(scalar_items),
            point_list_g1(repeated(g1_generator, points)),
          ),
        ),
      ),
    ),
    progress,
  )
}

fn list_ints(items: List<Data>) -> List<Int> {
  when items is {
    [] -> []
    [item, ..rest] -> [builtin.un_i_data(item), ..list_ints(rest)]
  }
}

fn point_list_g1(points: List<ByteArray>) -> List<G1Element> {
  when points is {
    [] -> []
    [point, ..rest] ->
      [builtin.bls12_381_g1_uncompress(point), ..point_list_g1(rest)]
  }
}

fn point_list_g2(points: List<ByteArray>) -> List<G2Element> {
  when points is {
    [] -> []
    [point, ..rest] ->
      [builtin.bls12_381_g2_uncompress(point), ..point_list_g2(rest)]
  }
}

/// `scalars` one-byte scalars over `points` copies of the G2 generator.
fn g2_msm_step(scalars: Int, points: Int, progress: Int) -> DirectStep {
  let scalar_items = small_int_items(1, scalars + 1)
  direct_step(
    93,
    [
      int_list(scalar_items),
      constant(
        ListConstant { element: BlsG2Constant },
        builtin.list_data(repeated(builtin.b_data(g2_generator), points)),
      ),
    ],
    constant(
      BlsG2Constant,
      builtin.b_data(
        builtin.bls12_381_g2_compress(
          builtin.bls12_381_g2_multi_scalar_mul(
            list_ints(scalar_items),
            point_list_g2(repeated(g2_generator, points)),
          ),
        ),
      ),
    ),
    progress,
  )
}

test g1_msm_roots_at_37_points() {
  roots(g1_msm_step(37, 37, 0))
}

test g1_msm_budget_at_37_points() {
  budget(g1_msm_step(37, 37, 1))
}

test g1_msm_semantics_at_37_points() {
  semantics(g1_msm_step(37, 37, 2))
}

test g1_msm_fixture_only_at_37_points() {
  built(g1_msm_step(37, 37, 2))
}

test g1_msm_roots_at_38_points() {
  roots(g1_msm_step(38, 38, 0))
}

test g1_msm_fixture_only_at_38_points() {
  built(g1_msm_step(38, 38, 2))
}

test g1_msm_roots_at_80_scalars_over_one_point() {
  roots(g1_msm_step(80, 1, 0))
}

test g1_msm_budget_at_80_scalars_over_one_point() {
  budget(g1_msm_step(80, 1, 1))
}

test g1_msm_semantics_at_80_scalars_over_one_point() {
  semantics(g1_msm_step(80, 1, 2))
}

test g1_msm_fixture_only_at_80_scalars_over_one_point() {
  built(g1_msm_step(80, 1, 2))
}

test g1_msm_roots_at_81_scalars_over_one_point() {
  roots(g1_msm_step(81, 1, 0))
}

test g1_msm_fixture_only_at_81_scalars_over_one_point() {
  built(g1_msm_step(81, 1, 2))
}

/// 179 points fill the direct bound.
test g1_msm_roots_at_the_direct_bound() {
  roots(g1_msm_step(179, 179, 0))
}

test g1_msm_budget_at_the_direct_bound() {
  budget(g1_msm_step(179, 179, 1))
}

test g1_msm_semantics_at_the_direct_bound() {
  semantics(g1_msm_step(179, 179, 2))
}

test g1_msm_fixture_only_at_the_direct_bound() {
  built(g1_msm_step(179, 179, 2))
}

test g2_msm_roots_at_26_points() {
  roots(g2_msm_step(26, 26, 0))
}

test g2_msm_budget_at_26_points() {
  budget(g2_msm_step(26, 26, 1))
}

test g2_msm_semantics_at_26_points() {
  semantics(g2_msm_step(26, 26, 2))
}

test g2_msm_fixture_only_at_26_points() {
  built(g2_msm_step(26, 26, 2))
}

test g2_msm_semantics_at_27_points() {
  semantics(g2_msm_step(27, 27, 2))
}

test g2_msm_fixture_only_at_27_points() {
  built(g2_msm_step(27, 27, 2))
}

test g2_msm_roots_at_80_scalars_over_one_point() {
  roots(g2_msm_step(80, 1, 0))
}

test g2_msm_budget_at_80_scalars_over_one_point() {
  budget(g2_msm_step(80, 1, 1))
}

test g2_msm_semantics_at_80_scalars_over_one_point() {
  semantics(g2_msm_step(80, 1, 2))
}

test g2_msm_fixture_only_at_80_scalars_over_one_point() {
  built(g2_msm_step(80, 1, 2))
}

test g2_msm_roots_at_81_scalars_over_one_point() {
  roots(g2_msm_step(81, 1, 0))
}

test g2_msm_fixture_only_at_81_scalars_over_one_point() {
  built(g2_msm_step(81, 1, 2))
}

/// 88 points fill the direct bound.
test g2_msm_roots_at_the_direct_bound() {
  roots(g2_msm_step(88, 88, 0))
}

test g2_msm_budget_at_the_direct_bound() {
  budget(g2_msm_step(88, 88, 1))
}

test g2_msm_semantics_at_the_direct_bound() {
  semantics(g2_msm_step(88, 88, 2))
}

test g2_msm_fixture_only_at_the_direct_bound() {
  built(g2_msm_step(88, 88, 2))
}

// ## The Value builtins (94 to 100)

/// A new coin into a Value of `count` coins (1,500 at the direct bound).
fn insert_coin_step(count: Int, progress: Int) -> DirectStep {
  let source = value_data(count, ones)
  let currency = key(( count - 1 ) / 257)
  let token = key(( count - 1 ) % 257 + 1)
  direct_step(
    94,
    [
      constant(ByteStringConstant, builtin.b_data(currency)),
      constant(ByteStringConstant, builtin.b_data(token)),
      int(1),
      value(source),
    ],
    value(
      builtin.value_data(
        builtin.insert_value(currency, token, 1, builtin.un_value_data(source)),
      ),
    ),
    progress,
  )
}

test insert_coin_roots_at_21_coins() {
  roots(insert_coin_step(21, 0))
}

test insert_coin_budget_at_21_coins() {
  budget(insert_coin_step(21, 1))
}

test insert_coin_semantics_at_21_coins() {
  semantics(insert_coin_step(21, 2))
}

test insert_coin_fixture_only_at_21_coins() {
  built(insert_coin_step(21, 2))
}

test insert_coin_roots_at_22_coins() {
  roots(insert_coin_step(22, 0))
}

test insert_coin_fixture_only_at_22_coins() {
  built(insert_coin_step(22, 2))
}

test insert_coin_roots_at_the_direct_bound() {
  roots(insert_coin_step(1500, 0))
}

test insert_coin_budget_at_the_direct_bound() {
  budget(insert_coin_step(1500, 1))
}

test insert_coin_semantics_at_the_direct_bound() {
  semantics(insert_coin_step(1500, 2))
}

test insert_coin_fixture_only_at_the_direct_bound() {
  built(insert_coin_step(1500, 2))
}

/// The last coin of a Value of `count` coins (3,000 at the direct bound).
fn lookup_coin_step(count: Int, progress: Int) -> DirectStep {
  direct_step(
    95,
    [
      constant(ByteStringConstant, builtin.b_data(key(( count - 1 ) / 257))),
      constant(ByteStringConstant, builtin.b_data(key(( count - 1 ) % 257))),
      value(value_data(count, ones)),
    ],
    int(1),
    progress,
  )
}

test lookup_coin_roots_at_45_coins() {
  roots(lookup_coin_step(45, 0))
}

test lookup_coin_budget_at_45_coins() {
  budget(lookup_coin_step(45, 1))
}

test lookup_coin_semantics_at_45_coins() {
  semantics(lookup_coin_step(45, 2))
}

test lookup_coin_fixture_only_at_45_coins() {
  built(lookup_coin_step(45, 2))
}

test lookup_coin_roots_at_46_coins() {
  roots(lookup_coin_step(46, 0))
}

test lookup_coin_fixture_only_at_46_coins() {
  built(lookup_coin_step(46, 2))
}

test lookup_coin_roots_at_the_direct_bound() {
  roots(lookup_coin_step(3000, 0))
}

test lookup_coin_budget_at_the_direct_bound() {
  budget(lookup_coin_step(3000, 1))
}

test lookup_coin_semantics_at_the_direct_bound() {
  semantics(lookup_coin_step(3000, 2))
}

test lookup_coin_fixture_only_at_the_direct_bound() {
  built(lookup_coin_step(3000, 2))
}

/// Two Values of the same `count` coins and their sum (1,000 at the direct
/// bound).
fn union_value_step(count: Int, progress: Int) -> DirectStep {
  let operand = value_data(count, ones)
  direct_step(
    96,
    [value(operand), value(operand)],
    value(value_data(count, twos)),
    progress,
  )
}

test union_value_roots_at_14_coins() {
  roots(union_value_step(14, 0))
}

test union_value_budget_at_14_coins() {
  budget(union_value_step(14, 1))
}

test union_value_semantics_at_14_coins() {
  semantics(union_value_step(14, 2))
}

test union_value_fixture_only_at_14_coins() {
  built(union_value_step(14, 2))
}

test union_value_roots_at_15_coins() {
  roots(union_value_step(15, 0))
}

test union_value_fixture_only_at_15_coins() {
  built(union_value_step(15, 2))
}

test union_value_roots_at_the_direct_bound() {
  roots(union_value_step(1000, 0))
}

test union_value_budget_at_the_direct_bound() {
  budget(union_value_step(1000, 1))
}

test union_value_semantics_at_the_direct_bound() {
  semantics(union_value_step(1000, 2))
}

test union_value_fixture_only_at_the_direct_bound() {
  built(union_value_step(1000, 2))
}

/// A Value of `count` coins containing itself (1,500 at the direct bound).
fn value_contains_step(count: Int, progress: Int) -> DirectStep {
  let operand = value_data(count, ones)
  direct_step(
    97,
    [value(operand), value(operand)],
    constant(BooleanConstant, builtin.constr_data(1, [])),
    progress,
  )
}

test value_contains_roots_at_22_coins() {
  roots(value_contains_step(22, 0))
}

test value_contains_budget_at_22_coins() {
  budget(value_contains_step(22, 1))
}

test value_contains_semantics_at_22_coins() {
  semantics(value_contains_step(22, 2))
}

test value_contains_fixture_only_at_22_coins() {
  built(value_contains_step(22, 2))
}

test value_contains_roots_at_23_coins() {
  roots(value_contains_step(23, 0))
}

test value_contains_fixture_only_at_23_coins() {
  built(value_contains_step(23, 2))
}

test value_contains_roots_at_the_direct_bound() {
  roots(value_contains_step(1500, 0))
}

test value_contains_budget_at_the_direct_bound() {
  budget(value_contains_step(1500, 1))
}

test value_contains_semantics_at_the_direct_bound() {
  semantics(value_contains_step(1500, 2))
}

test value_contains_fixture_only_at_the_direct_bound() {
  built(value_contains_step(1500, 2))
}

/// A Value of `count` coins as Data (1,500 at the direct bound).
fn value_data_step(count: Int, progress: Int) -> DirectStep {
  let payload = value_data(count, ones)
  direct_step(98, [value(payload)], constant(DataConstant, payload), progress)
}

test value_data_roots_at_22_coins() {
  roots(value_data_step(22, 0))
}

test value_data_budget_at_22_coins() {
  budget(value_data_step(22, 1))
}

test value_data_semantics_at_22_coins() {
  semantics(value_data_step(22, 2))
}

test value_data_fixture_only_at_22_coins() {
  built(value_data_step(22, 2))
}

test value_data_roots_at_23_coins() {
  roots(value_data_step(23, 0))
}

test value_data_fixture_only_at_23_coins() {
  built(value_data_step(23, 2))
}

test value_data_roots_at_the_direct_bound() {
  roots(value_data_step(1500, 0))
}

test value_data_budget_at_the_direct_bound() {
  budget(value_data_step(1500, 1))
}

test value_data_semantics_at_the_direct_bound() {
  semantics(value_data_step(1500, 2))
}

test value_data_fixture_only_at_the_direct_bound() {
  built(value_data_step(1500, 2))
}

/// Data of a Value of `count` coins as the Value (1,500 at the direct
/// bound).
fn un_value_data_step(count: Int, progress: Int) -> DirectStep {
  let payload = value_data(count, ones)
  direct_step(99, [constant(DataConstant, payload)], value(payload), progress)
}

test un_value_data_roots_at_22_coins() {
  roots(un_value_data_step(22, 0))
}

test un_value_data_budget_at_22_coins() {
  budget(un_value_data_step(22, 1))
}

test un_value_data_semantics_at_22_coins() {
  semantics(un_value_data_step(22, 2))
}

test un_value_data_fixture_only_at_22_coins() {
  built(un_value_data_step(22, 2))
}

test un_value_data_roots_at_23_coins() {
  roots(un_value_data_step(23, 0))
}

test un_value_data_fixture_only_at_23_coins() {
  built(un_value_data_step(23, 2))
}

test un_value_data_roots_at_the_direct_bound() {
  roots(un_value_data_step(1500, 0))
}

test un_value_data_budget_at_the_direct_bound() {
  budget(un_value_data_step(1500, 1))
}

test un_value_data_semantics_at_the_direct_bound() {
  semantics(un_value_data_step(1500, 2))
}

test un_value_data_fixture_only_at_the_direct_bound() {
  built(un_value_data_step(1500, 2))
}

/// A Value of `count` coins doubled (1,500 at the direct bound).
fn scale_value_step(count: Int, progress: Int) -> DirectStep {
  direct_step(
    100,
    [int(2), value(value_data(count, ones))],
    value(value_data(count, twos)),
    progress,
  )
}

test scale_value_roots_at_22_coins() {
  roots(scale_value_step(22, 0))
}

test scale_value_budget_at_22_coins() {
  budget(scale_value_step(22, 1))
}

test scale_value_semantics_at_22_coins() {
  semantics(scale_value_step(22, 2))
}

test scale_value_fixture_only_at_22_coins() {
  built(scale_value_step(22, 2))
}

test scale_value_roots_at_23_coins() {
  roots(scale_value_step(23, 0))
}

test scale_value_fixture_only_at_23_coins() {
  built(scale_value_step(23, 2))
}

test scale_value_roots_at_the_direct_bound() {
  roots(scale_value_step(1500, 0))
}

test scale_value_budget_at_the_direct_bound() {
  budget(scale_value_step(1500, 1))
}

test scale_value_semantics_at_the_direct_bound() {
  semantics(scale_value_step(1500, 2))
}

test scale_value_fixture_only_at_the_direct_bound() {
  built(scale_value_step(1500, 2))
}
