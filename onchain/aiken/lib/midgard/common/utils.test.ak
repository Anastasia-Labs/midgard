use aiken/fuzz as aiken_fuzz
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{
  AssetName, PolicyId, Value, ada_asset_name, ada_policy_id, add, from_lovelace,
}
use cardano/transaction.{Input, NoDatum, Output, OutputReference, InlineDatum}
use midgard/common/types.{ValuePairs}
use midgard/common/utils
use aiken/crypto.{ ScriptHash }

test zip_foldl() {
  let list_a = [1, 2, 3]
  let list_b = [1, 2, 3, 4]
  let final_list =
    utils.zip_foldl(
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
  let final_list =
    utils.zip_foldr(
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

// ============================================================
// validate_mint
// ============================================================

const target_policy_id = #"01"

const target_asset_name = #"a0"

const invalid_policy_id = #"02"

const invalid_asset_name = #"b0"

fn value_with_valid_mint(target_quantity: Int) -> Value {
  assets.from_asset_list(
    [Pair(target_policy_id, [Pair(target_asset_name, target_quantity)])],
  )
}

fn value_with_invalid_mint(target_quantity: Int) -> Value {
  assets.from_asset_list(
    [
      Pair(invalid_policy_id, [Pair(target_asset_name, target_quantity)]),
      Pair(target_policy_id, [Pair(invalid_asset_name, target_quantity)]),
    ],
  )
}

test success__validate_mint(qty via aiken_fuzz.int_at_least(1)) {
  let v = value_with_valid_mint(qty)
  utils.validate_mint(v, target_policy_id, target_asset_name, qty)
}

test fail__validate_mint__token_mismatch(
  qty via aiken_fuzz.int_at_least(1),
) fail {
  let v = value_with_invalid_mint(qty)
  utils.validate_mint(v, target_policy_id, target_asset_name, qty)
}

// ============================================================
// authentic_value_has_tokens
// ============================================================

const auth_nft_policy_id = #"01"

const auth_nft_asset_name = #"a0"

const auth_token_policy_id = #"02"

const auth_token_asset_name = #"b0"

const baseline_lovelace = 2_000_000

const token_qty = 5

fn value_with_nft_and_tokens() -> Value {
  assets.from_asset_list(
    [
      Pair(ada_policy_id, [Pair(ada_asset_name, baseline_lovelace)]),
      Pair(auth_nft_policy_id, [Pair(auth_nft_asset_name, 1)]),
      Pair(auth_token_policy_id, [Pair(auth_token_asset_name, token_qty)]),
    ],
  )
}

fn value_without_nft() -> Value {
  assets.from_asset_list(
    [
      Pair(ada_policy_id, [Pair(ada_asset_name, baseline_lovelace)]),
      Pair(auth_token_policy_id, [Pair(auth_token_asset_name, token_qty)]),
    ],
  )
}

fn value_with_extra_token() -> Value {
  assets.from_asset_list(
    [
      Pair(ada_policy_id, [Pair(ada_asset_name, baseline_lovelace)]),
      Pair(auth_nft_policy_id, [Pair(auth_nft_asset_name, 1)]),
      Pair(
        auth_token_policy_id,
        [Pair(auth_token_asset_name, token_qty), Pair(#"c0", 1)],
      ),
    ],
  )
}

pub const valid_tokens: ValuePairs =
  [Pair(auth_token_policy_id, [Pair(auth_token_asset_name, token_qty)])]

pub const tokens_with_ada: ValuePairs =
  [
    Pair(ada_policy_id, [Pair(ada_asset_name, baseline_lovelace)]),
    Pair(auth_token_policy_id, [Pair(auth_token_asset_name, token_qty)]),
  ]

fn authentic_value_fuzzer() -> Fuzzer<(Int, Int)> {
  let lovelace <- aiken_fuzz.and_then(aiken_fuzz.int_at_least(2_000_000))
  let token_qty <- aiken_fuzz.and_then(aiken_fuzz.int_at_least(1))
  aiken_fuzz.constant((lovelace, token_qty))
}

test success__authentic_value_has_tokens(
  test_values via authentic_value_fuzzer(),
) {
  let (lovelace, token_qty) = test_values
  let value =
    assets.from_asset_list(
      [
        Pair(ada_policy_id, [Pair(ada_asset_name, lovelace)]),
        Pair(auth_nft_policy_id, [Pair(auth_nft_asset_name, 1)]),
        Pair(auth_token_policy_id, [Pair(auth_token_asset_name, token_qty)]),
      ],
    )
  let tokens: ValuePairs =
    [Pair(auth_token_policy_id, [Pair(auth_token_asset_name, token_qty)])]
  utils.authentic_value_has_tokens(
    value,
    auth_nft_policy_id,
    auth_nft_asset_name,
    tokens,
  )
}

test fail__authentic_value_has_tokens__missing_nft() {
  let value = value_without_nft()
  let tokens: ValuePairs =
    [Pair(auth_token_policy_id, [Pair(auth_token_asset_name, token_qty)])]
  !utils.authentic_value_has_tokens(
    value,
    auth_nft_policy_id,
    auth_nft_asset_name,
    tokens,
  )
}

test fail__authentic_value_has_tokens__ada_in_tokens() {
  let value = value_with_nft_and_tokens()
  let tokens_with_ada: ValuePairs =
    [
      Pair(ada_policy_id, [Pair(ada_asset_name, baseline_lovelace)]),
      Pair(auth_token_policy_id, [Pair(auth_token_asset_name, token_qty)]),
    ]
  !utils.authentic_value_has_tokens(
    value,
    auth_nft_policy_id,
    auth_nft_asset_name,
    tokens_with_ada,
  )
}

test fail__authentic_value_has_tokens__extra_token_in_value() {
  let value = value_with_extra_token()
  let tokens: ValuePairs =
    [Pair(auth_token_policy_id, [Pair(auth_token_asset_name, token_qty)])]
  !utils.authentic_value_has_tokens(
    value,
    auth_nft_policy_id,
    auth_nft_asset_name,
    tokens,
  )
}

// ============================================================
// compare_authentic_value_with_target
// ============================================================

const cmp_nft_policy_id = #"01"

const cmp_nft_asset_name = #"a0"

const cmp_token_policy_id = #"02"

const cmp_token_asset_name = #"b0"

const cmp_wrong_policy_id = #"03"

const cmp_wrong_asset_name = #"ff"

fn compare_value_fuzzer() -> Fuzzer<(Int, Int, Int)> {
  let lovelace <- aiken_fuzz.and_then(aiken_fuzz.int_at_least(2_000_000))
  let value_qty <- aiken_fuzz.and_then(aiken_fuzz.int_at_least(1))
  let target_qty <- aiken_fuzz.and_then(aiken_fuzz.int_at_least(1))
  aiken_fuzz.constant((lovelace, value_qty, target_qty))
}

test success__compare_authentic_value_with_target__predicate_holds(
  test_values via compare_value_fuzzer(),
) {
  let (lovelace, value_qty, _) = test_values

  let value =
    assets.from_asset_list(
      [
        Pair(ada_policy_id, [Pair(ada_asset_name, lovelace)]),
        Pair(cmp_nft_policy_id, [Pair(cmp_nft_asset_name, 1)]),
        Pair(cmp_token_policy_id, [Pair(cmp_token_asset_name, value_qty)]),
      ],
    )

  let target: ValuePairs =
    [
      Pair(ada_policy_id, [Pair(ada_asset_name, lovelace)]),
      Pair(cmp_token_policy_id, [Pair(cmp_token_asset_name, value_qty)]),
    ]

  utils.compare_authentic_value_with_target(
    value,
    cmp_nft_policy_id,
    cmp_nft_asset_name,
    target,
    fn(v, t) { v == t },
  )
}

test fail__compare_authentic_value_with_target__predicate_fails(
  test_values via compare_value_fuzzer(),
) {
  let (lovelace, value_qty, _) = test_values

  let value =
    assets.from_asset_list(
      [
        Pair(ada_policy_id, [Pair(ada_asset_name, lovelace)]),
        Pair(cmp_nft_policy_id, [Pair(cmp_nft_asset_name, 1)]),
        Pair(cmp_token_policy_id, [Pair(cmp_token_asset_name, value_qty)]),
      ],
    )

  let target: ValuePairs =
    [
      Pair(ada_policy_id, [Pair(ada_asset_name, lovelace)]),
      Pair(cmp_token_policy_id, [Pair(cmp_token_asset_name, value_qty + 1)]),
    ]

  !utils.compare_authentic_value_with_target(
    value,
    cmp_nft_policy_id,
    cmp_nft_asset_name,
    target,
    fn(v, t) { v > t },
  )
}

test fail__compare_authentic_value_with_target__policy_mismatch(
  test_values via compare_value_fuzzer(),
) fail {
  let (lovelace, value_qty, target_qty) = test_values

  let value =
    assets.from_asset_list(
      [
        Pair(ada_policy_id, [Pair(ada_asset_name, lovelace)]),
        Pair(cmp_nft_policy_id, [Pair(cmp_nft_asset_name, 1)]),
        Pair(cmp_token_policy_id, [Pair(cmp_token_asset_name, value_qty)]),
      ],
    )

  let target: ValuePairs =
    [Pair(cmp_wrong_policy_id, [Pair(cmp_token_asset_name, target_qty)])]

  expect
    utils.compare_authentic_value_with_target(
      value,
      cmp_nft_policy_id,
      cmp_nft_asset_name,
      target,
      fn(v, t) { v == t },
    )
  True
}

test fail__compare_authentic_value_with_target__asset_name_mismatch(
  test_values via compare_value_fuzzer(),
) fail {
  let (lovelace, value_qty, target_qty) = test_values

  let value =
    assets.from_asset_list(
      [
        Pair(ada_policy_id, [Pair(ada_asset_name, lovelace)]),
        Pair(cmp_nft_policy_id, [Pair(cmp_nft_asset_name, 1)]),
        Pair(cmp_token_policy_id, [Pair(cmp_token_asset_name, value_qty)]),
      ],
    )

  let target: ValuePairs =
    [Pair(cmp_token_policy_id, [Pair(cmp_wrong_asset_name, target_qty)])]

  expect
    utils.compare_authentic_value_with_target(
      value,
      cmp_nft_policy_id,
      cmp_nft_asset_name,
      target,
      fn(v, t) { v == t },
    )
  True
}

// ============================================================
// get_authentic_input_asset_name_with_policy_at
// ============================================================

fn authentic_input_asset_name_fuzzer() -> Fuzzer<(Input, PolicyId, AssetName)> {
  let policy_id <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let asset_name <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let tx_hash <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace <-
    aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))

  let value = add(from_lovelace(lovelace), policy_id, asset_name, 1)

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(policy_id), None),
        value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, policy_id, asset_name))
}

fn wrong_policy_input_fuzzer() -> Fuzzer<(Input, PolicyId, PolicyId, AssetName)> {
  let real_policy <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let wrong_policy <-
    aiken_fuzz.and_then(
      aiken_fuzz.bytearray_fixed(28)
        |> aiken_fuzz.such_that(fn(b) { b != real_policy }),
    )
  let asset_name <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let tx_hash <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace <-
    aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))

  let value = add(from_lovelace(lovelace), real_policy, asset_name, 1)

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(real_policy), None),
        value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, real_policy, wrong_policy, asset_name))
}

test success__get_authentic_input_asset_name_with_policy_at(
  test_values via authentic_input_asset_name_fuzzer(),
) {
  let (input, policy_id, asset_name) = test_values
  let result =
    utils.get_authentic_input_asset_name_with_policy_at([input], policy_id, 0)
  result == asset_name
}

test fail__get_authentic_input_asset_name_with_policy_at__index_out_of_bounds(
  test_values via authentic_input_asset_name_fuzzer(),
) fail {
  let (input, policy_id, _) = test_values
  expect _ =
    utils.get_authentic_input_asset_name_with_policy_at([input], policy_id, 1)
  True
}

test fail__get_authentic_input_asset_name_with_policy_at__invalid_value(
  test_values via authentic_input_asset_name_fuzzer(),
) fail {
  let (input, policy_id, _) = test_values

  let bad_input =
    Input {
      ..input,
      output: Output { ..input.output, value: from_lovelace(2_000_000) },
    }

  expect _ =
    utils.get_authentic_input_asset_name_with_policy_at(
      [bad_input],
      policy_id,
      0,
    )
  True
}

test fail__get_authentic_input_asset_name_with_policy_at__policy_mismatch(
  test_values via wrong_policy_input_fuzzer(),
) fail {
  let (input, _real_policy, wrong_policy, _) = test_values
  expect _ =
    utils.get_authentic_input_asset_name_with_policy_at(
      [input],
      wrong_policy,
      0,
    )
  True
}

// ============================================================
// get_authentic_input_and_asset_name_with_policy_at
// ============================================================

test success__get_authentic_input_and_asset_name_with_policy_at(
  test_values via authentic_input_asset_name_fuzzer(),
) {
  let (input, policy_id, asset_name) = test_values
  utils.get_authentic_input_and_asset_name_with_policy_at(
    [input],
    policy_id,
    0,
    fn(found_input, found_asset_name) {
      and {
        found_input == input,
        found_asset_name == asset_name,
      }
    },
  )
}

test fail__get_authentic_input_and_asset_name_with_policy_at__index_out_of_bounds(
  test_values via authentic_input_asset_name_fuzzer(),
) fail {
  let (input, policy_id, _) = test_values
  utils.get_authentic_input_and_asset_name_with_policy_at(
    [input],
    policy_id,
    1,
    fn(_, _) { True },
  )
}

test fail__get_authentic_input_and_asset_name_with_policy_at__invalid_value(
  test_values via authentic_input_asset_name_fuzzer(),
) fail {
  let (input, policy_id, _) = test_values
  let bad_input =
    Input {
      ..input,
      output: Output { ..input.output, value: from_lovelace(2_000_000) },
    }
  utils.get_authentic_input_and_asset_name_with_policy_at(
    [bad_input],
    policy_id,
    0,
    fn(_, _) { True },
  )
}

test fail__get_authentic_input_and_asset_name_with_policy_at__policy_mismatch(
  test_values via wrong_policy_input_fuzzer(),
) fail {
  let (input, _real_policy, wrong_policy, _) = test_values
  utils.get_authentic_input_and_asset_name_with_policy_at(
    [input],
    wrong_policy,
    0,
    fn(_, _) { True },
  )
}

// ============================================================
// get_authentic_input_datum_with_policy_at
// ============================================================

fn authentic_input_with_datum_fuzzer() -> Fuzzer<(Input, PolicyId, Data)> {
  let policy_id   <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let asset_name  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let tx_hash     <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace    <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))
  let datum_bytes <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))

  let value = add(from_lovelace(lovelace), policy_id, asset_name, 1)
  let datum_data: Data = datum_bytes

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(policy_id), None),
        value,
        datum: InlineDatum(datum_data),
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, policy_id, datum_data))
}

fn wrong_policy_with_datum_fuzzer() -> Fuzzer<(Input, PolicyId)> {
  let real_policy  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let wrong_policy <- aiken_fuzz.and_then(
    aiken_fuzz.bytearray_fixed(28) |> aiken_fuzz.such_that(fn(b) { b != real_policy }),
  )
  let asset_name   <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let tx_hash      <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace     <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))
  let datum_bytes  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))

  let value = add(from_lovelace(lovelace), real_policy, asset_name, 1)
  let datum_data: Data = datum_bytes

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(real_policy), None),
        value,
        datum: InlineDatum(datum_data),
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, wrong_policy))
}

test success__get_authentic_input_datum_with_policy_at(
  test_values via authentic_input_with_datum_fuzzer(),
) {
  let (input, policy_id, datum_data) = test_values
  let result =
    utils.get_authentic_input_datum_with_policy_at([input], policy_id, 0)
  result == datum_data
}

test fail__get_authentic_input_datum_with_policy_at__index_out_of_bounds(
  test_values via authentic_input_with_datum_fuzzer(),
) fail {
  let (input, policy_id, _) = test_values
  expect _: ByteArray =
    utils.get_authentic_input_datum_with_policy_at([input], policy_id, 1)
  True
}

test fail__get_authentic_input_datum_with_policy_at__no_inline_datum(
  test_values via authentic_input_with_datum_fuzzer(),
) fail {
  let (input, policy_id, _) = test_values

  let bad_input =
    Input {
      output_reference: input.output_reference,
      output: Output {
        address: input.output.address,
        value: input.output.value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  expect _: ByteArray =
    utils.get_authentic_input_datum_with_policy_at([bad_input], policy_id, 0)
  True
}

test fail__get_authentic_input_datum_with_policy_at__invalid_value(
  test_values via authentic_input_with_datum_fuzzer(),
) fail {
  let (input, policy_id, _) = test_values

  let bad_input =
    Input {
      output_reference: input.output_reference,
      output: Output {
        address: input.output.address,
        value: from_lovelace(2_000_000),
        datum: input.output.datum,
        reference_script: None,
      },
    }

  expect _: ByteArray =
    utils.get_authentic_input_datum_with_policy_at([bad_input], policy_id, 0)
  True
}

test fail__get_authentic_input_datum_with_policy_at__policy_mismatch(
  test_values via wrong_policy_with_datum_fuzzer(),
) fail {
  let (input, wrong_policy) = test_values
  expect _ : ByteArray =
    utils.get_authentic_input_datum_with_policy_at([input], wrong_policy, 0)
  True
}

// ============================================================
// get_authentic_input_datum_and_asset_name_with_policy_at
// ============================================================

test success__get_authentic_input_datum_and_asset_name_with_policy_at(
  test_values via authentic_input_with_datum_fuzzer(),
) {
  let (input, policy_id, datum_data) = test_values

  let (_, asset_name, _) =
    utils.get_single_asset_from_value_apart_from_ada(input.output.value)

  utils.get_authentic_input_datum_and_asset_name_with_policy_at(
    [input],
    policy_id,
    0,
    fn(found_asset_name, found_datum) {
      and {
        found_asset_name == asset_name,
        found_datum == datum_data,
      }
    },
  )
}

test fail__get_authentic_input_datum_and_asset_name_with_policy_at__index_out_of_bounds(
  test_values via authentic_input_with_datum_fuzzer(),
) fail {
  let (input, policy_id, _) = test_values
  utils.get_authentic_input_datum_and_asset_name_with_policy_at(
    [input],
    policy_id,
    1,
    fn(_, _) { True },
  )
}

test fail__get_authentic_input_datum_and_asset_name_with_policy_at__no_inline_datum(
  test_values via authentic_input_with_datum_fuzzer(),
) fail {
  let (input, policy_id, _) = test_values

  let bad_input =
    Input {
      output_reference: input.output_reference,
      output: Output {
        address: input.output.address,
        value: input.output.value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  utils.get_authentic_input_datum_and_asset_name_with_policy_at(
    [bad_input],
    policy_id,
    0,
    fn(_, _) { True },
  )
}

test fail__get_authentic_input_datum_and_asset_name_with_policy_at__invalid_value(
  test_values via authentic_input_with_datum_fuzzer(),
) fail {
  let (input, policy_id, _) = test_values

  let bad_input =
    Input {
      output_reference: input.output_reference,
      output: Output {
        address: input.output.address,
        value: from_lovelace(2_000_000),
        datum: input.output.datum,
        reference_script: None,
      },
    }

  utils.get_authentic_input_datum_and_asset_name_with_policy_at(
    [bad_input],
    policy_id,
    0,
    fn(_, _) { True },
  )
}

test fail__get_authentic_input_datum_and_asset_name_with_policy_at__policy_mismatch(
  test_values via wrong_policy_with_datum_fuzzer(),
) fail {
  let (input, wrong_policy) = test_values
  utils.get_authentic_input_datum_and_asset_name_with_policy_at(
    [input],
    wrong_policy,
    0,
    fn(_, _) { True },
  )
}

// ============================================================
// get_authentic_input_with_nft_at
// ============================================================

fn authentic_nft_input_fuzzer() -> Fuzzer<(Input, PolicyId, AssetName)> {
  let policy_id  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let asset_name <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let tx_hash    <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace   <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))

  let value = add(from_lovelace(lovelace), policy_id, asset_name, 1)

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(policy_id), None),
        value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, policy_id, asset_name))
}

fn wrong_nft_input_fuzzer() -> Fuzzer<(Input, PolicyId, AssetName, PolicyId, AssetName)> {
  let real_policy   <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let wrong_policy  <- aiken_fuzz.and_then(
    aiken_fuzz.bytearray_fixed(28) |> aiken_fuzz.such_that(fn(b) { b != real_policy }),
  )
  let real_name     <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let wrong_name    <- aiken_fuzz.and_then(
    aiken_fuzz.bytearray_fixed(32) |> aiken_fuzz.such_that(fn(b) { b != real_name }),
  )
  let tx_hash       <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace      <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))

  let value = add(from_lovelace(lovelace), real_policy, real_name, 1)

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(real_policy), None),
        value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, real_policy, real_name, wrong_policy, wrong_name))
}

fn wrong_quantity_nft_input_fuzzer() -> Fuzzer<(Input, PolicyId, AssetName)> {
  let policy_id  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let asset_name <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let tx_hash    <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace   <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))
  let qty        <- aiken_fuzz.and_then(aiken_fuzz.int_between(2, 1_000))

  let value = add(from_lovelace(lovelace), policy_id, asset_name, qty)

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(policy_id), None),
        value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, policy_id, asset_name))
}

test success__get_authentic_input_with_nft_at(
  test_values via authentic_nft_input_fuzzer(),
) {
  let (input, policy_id, asset_name) = test_values
  let result =
    utils.get_authentic_input_with_nft_at([input], policy_id, asset_name, 0)
  result == input
}

test fail__get_authentic_input_with_nft_at__index_out_of_bounds(
  test_values via authentic_nft_input_fuzzer(),
) fail {
  let (input, policy_id, asset_name) = test_values
  expect _ =
    utils.get_authentic_input_with_nft_at([input], policy_id, asset_name, 1)
  True
}

test fail__get_authentic_input_with_nft_at__wrong_policy(
  test_values via wrong_nft_input_fuzzer(),
) fail {
  let (input, _real_policy, real_name, wrong_policy, _) = test_values
  expect _ =
    utils.get_authentic_input_with_nft_at(
      [input],
      wrong_policy,
      real_name,
      0,
    )
  True
}

test fail__get_authentic_input_with_nft_at__wrong_asset_name(
  test_values via wrong_nft_input_fuzzer(),
) fail {
  let (input, real_policy, _real_name, _, wrong_name) = test_values
  expect _ =
    utils.get_authentic_input_with_nft_at(
      [input],
      real_policy,
      wrong_name,
      0,
    )
  True
}

test fail__get_authentic_input_with_nft_at__wrong_quantity(
  test_values via wrong_quantity_nft_input_fuzzer(),
) fail {
  let (input, policy_id, asset_name) = test_values
  expect _ =
    utils.get_authentic_input_with_nft_at(
      [input],
      policy_id,
      asset_name,
      0,
    )
  True
}

// ============================================================
// get_authentic_input_datum_with_nft_at
// ============================================================

fn authentic_nft_input_with_datum_fuzzer() -> Fuzzer<(Input, PolicyId, AssetName, Data)> {
  let policy_id   <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let asset_name  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let tx_hash     <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace    <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))
  let datum_bytes <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))

  let value = add(from_lovelace(lovelace), policy_id, asset_name, 1)
  let datum_data: Data = datum_bytes

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(policy_id), None),
        value,
        datum: InlineDatum(datum_data),
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, policy_id, asset_name, datum_data))
}

fn wrong_nft_with_datum_fuzzer() -> Fuzzer<(Input, PolicyId, AssetName, PolicyId, AssetName)> {
  let real_policy  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let wrong_policy <- aiken_fuzz.and_then(
    aiken_fuzz.bytearray_fixed(28) |> aiken_fuzz.such_that(fn(b) { b != real_policy }),
  )
  let real_name    <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let wrong_name   <- aiken_fuzz.and_then(
    aiken_fuzz.bytearray_fixed(32) |> aiken_fuzz.such_that(fn(b) { b != real_name }),
  )
  let tx_hash      <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace     <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))
  let datum_bytes  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))

  let value = add(from_lovelace(lovelace), real_policy, real_name, 1)
  let datum_data: Data = datum_bytes

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(real_policy), None),
        value,
        datum: InlineDatum(datum_data),
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, real_policy, real_name, wrong_policy, wrong_name))
}

fn wrong_quantity_nft_with_datum_fuzzer() -> Fuzzer<(Input, PolicyId, AssetName)> {
  let policy_id   <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let asset_name  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let tx_hash     <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace    <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))
  let datum_bytes <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let qty         <- aiken_fuzz.and_then(aiken_fuzz.int_between(2, 1_000))

  let value = add(from_lovelace(lovelace), policy_id, asset_name, qty)
  let datum_data: Data = datum_bytes

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(policy_id), None),
        value,
        datum: InlineDatum(datum_data),
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, policy_id, asset_name))
}


test success__get_authentic_input_datum_with_nft_at(
  test_values via authentic_nft_input_with_datum_fuzzer(),
) {
  let (input, policy_id, asset_name, datum_data) = test_values
  let result =
    utils.get_authentic_input_datum_with_nft_at(
      [input],
      policy_id,
      asset_name,
      0,
    )
  result == datum_data
}

test fail__get_authentic_input_datum_with_nft_at__index_out_of_bounds(
  test_values via authentic_nft_input_with_datum_fuzzer(),
) fail {
  let (input, policy_id, asset_name, _) = test_values
  expect _: ByteArray =
    utils.get_authentic_input_datum_with_nft_at(
      [input],
      policy_id,
      asset_name,
      1,
    )
  True
}

test fail__get_authentic_input_datum_with_nft_at__no_inline_datum(
  test_values via authentic_nft_input_with_datum_fuzzer(),
) fail {
  let (input, policy_id, asset_name, _) = test_values

  let bad_input =
    Input {
      output_reference: input.output_reference,
      output: Output {
        address: input.output.address,
        value: input.output.value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  expect _: ByteArray =
    utils.get_authentic_input_datum_with_nft_at(
      [bad_input],
      policy_id,
      asset_name,
      0,
    )
  True
}


test fail__get_authentic_input_datum_with_nft_at__wrong_policy(
  test_values via wrong_nft_with_datum_fuzzer(),
) fail {
  let (input, _real_policy, real_name, wrong_policy, _) = test_values
  expect _: ByteArray =
    utils.get_authentic_input_datum_with_nft_at(
      [input],
      wrong_policy,
      real_name,
      0,
    )
  True
}

test fail__get_authentic_input_datum_with_nft_at__wrong_asset_name(
  test_values via wrong_nft_with_datum_fuzzer(),
) fail {
  let (input, real_policy, _real_name, _, wrong_name) = test_values
  expect _: ByteArray =
    utils.get_authentic_input_datum_with_nft_at(
      [input],
      real_policy,
      wrong_name,
      0,
    )
  True
}

test fail__get_authentic_input_datum_with_nft_at__wrong_quantity(
  test_values via wrong_quantity_nft_with_datum_fuzzer(),
) fail {
  let (input, policy_id, asset_name) = test_values
  expect _: ByteArray =
    utils.get_authentic_input_datum_with_nft_at(
      [input],
      policy_id,
      asset_name,
      0,
    )
  True
}

// ============================================================
// get_authentic_input_of
// ============================================================

fn authentic_input_of_fuzzer() -> Fuzzer<(Input, ScriptHash, AssetName)> {
  let script_hash <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let token_name  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let tx_hash     <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace    <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))

  let value = add(from_lovelace(lovelace), script_hash, token_name, 1)

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(script_hash), None),
        value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, script_hash, token_name))
}

fn vk_credential_input_fuzzer() -> Fuzzer<(Input, ScriptHash, AssetName)> {
  let script_hash <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let pubkey_hash <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let token_name  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let tx_hash     <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace    <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))

  let value = add(from_lovelace(lovelace), script_hash, token_name, 1)

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(VerificationKey(pubkey_hash), None),
        value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, script_hash, token_name))
}

fn mismatched_script_hash_fuzzer() -> Fuzzer<(Input, ScriptHash, ScriptHash, AssetName)> {
  let real_script_hash  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let wrong_script_hash <- aiken_fuzz.and_then(
    aiken_fuzz.bytearray_fixed(28)
      |> aiken_fuzz.such_that(fn(b) { b != real_script_hash }),
  )
  let token_name        <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let tx_hash           <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace          <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))

  let value = add(from_lovelace(lovelace), real_script_hash, token_name, 1)

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(real_script_hash), None),
        value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, real_script_hash, wrong_script_hash, token_name))
}

fn wrong_token_name_fuzzer() -> Fuzzer<(Input, ScriptHash, AssetName, AssetName)> {
  let script_hash <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let token_name  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let wrong_name  <- aiken_fuzz.and_then(
    aiken_fuzz.bytearray_fixed(32)
      |> aiken_fuzz.such_that(fn(b) { b != token_name }),
  )
  let tx_hash     <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace    <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))

  let value = add(from_lovelace(lovelace), script_hash, token_name, 1)

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(script_hash), None),
        value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, script_hash, token_name, wrong_name))
}

fn wrong_quantity_input_of_fuzzer() -> Fuzzer<(Input, ScriptHash, AssetName)> {
  let script_hash <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(28))
  let token_name  <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let tx_hash     <- aiken_fuzz.and_then(aiken_fuzz.bytearray_fixed(32))
  let lovelace    <- aiken_fuzz.and_then(aiken_fuzz.int_between(2_000_000, 100_000_000))
  let qty         <- aiken_fuzz.and_then(aiken_fuzz.int_between(2, 1_000))

  let value = add(from_lovelace(lovelace), script_hash, token_name, qty)

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: tx_hash,
        output_index: 0,
      },
      output: Output {
        address: Address(Script(script_hash), None),
        value,
        datum: NoDatum,
        reference_script: None,
      },
    }

  aiken_fuzz.constant((input, script_hash, token_name))
}


test success__get_authentic_input_of(
  test_values via authentic_input_of_fuzzer(),
) {
  let (input, script_hash, token_name) = test_values
  let result =
    utils.get_authentic_input_of([input], script_hash, token_name, 0)
  result == input
}

test fail__get_authentic_input_of__index_out_of_bounds(
  test_values via authentic_input_of_fuzzer(),
) fail {
  let (input, script_hash, token_name) = test_values
  expect _ =
    utils.get_authentic_input_of([input], script_hash, token_name, 1)
  True
}

test fail__get_authentic_input_of__not_script_credential(
  test_values via vk_credential_input_fuzzer(),
) fail {
  let (input, script_hash, token_name) = test_values
  expect _ =
    utils.get_authentic_input_of([input], script_hash, token_name, 0)
  True
}

test fail__get_authentic_input_of__script_hash_mismatch(
  test_values via mismatched_script_hash_fuzzer(),
) fail {
  let (input, _real_script_hash, wrong_script_hash, token_name) = test_values
  expect _ =
    utils.get_authentic_input_of([input], wrong_script_hash, token_name, 0)
  True
}

test fail__get_authentic_input_of__wrong_token_name(
  test_values via wrong_token_name_fuzzer(),
) fail {
  let (input, script_hash, _real_name, wrong_name) = test_values
  expect _ =
    utils.get_authentic_input_of([input], script_hash, wrong_name, 0)
  True
}

test fail__get_authentic_input_of__wrong_quantity(
  test_values via wrong_quantity_input_of_fuzzer(),
) fail {
  let (input, script_hash, token_name) = test_values
  expect _ =
    utils.get_authentic_input_of([input], script_hash, token_name, 0)
  True
}