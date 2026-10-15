use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/math
use aiken/primitive/bytearray
use midgard/cek_constant_v1.{ConstantWitnessV1}
use midgard/cek_data_v1
use midgard/cek_proof_v1
use midgard/script_decode_v1.{
  Decoded, Frame, Machine, Refused, ScriptDecodeRecordV1, ScriptDecodeRunV1,
  decode_env_v1, empty_stack_root, initial_run_v1, is_terminal, record_of_v1,
  stack_root_of, step_v1, step_with_frames_v1,
}

// ## A Flat bit writer

type W =
  (Int, Int)

fn put(w: W, value: Int, width: Int) -> W {
  let (acc, n) = w
  (acc * math.pow2(width) + value, n + width)
}

/// A Flat natural (also a `Word64`): little-endian 7-bit groups.
fn nat(w: W, z: Int) -> W {
  if z < 128 {
    put(w, z, 8)
  } else {
    nat(put(w, 128 + z % 128, 8), z / 128)
  }
}

fn int(w: W, n: Int) -> W {
  nat(
    w,
    if n >= 0 {
      2 * n
    } else {
      -2 * n - 1
    },
  )
}

fn raw(w: W, bytes: List<Int>) -> W {
  list.foldl(bytes, w, fn(b, acc) { put(acc, b, 8) })
}

fn filler(w: W) -> W {
  let (_, n) = w
  put(w, 1, 8 - n % 8)
}

fn bytes_int(bytes: ByteArray) -> Int {
  if bytearray.length(bytes) == 0 {
    0
  } else {
    builtin.bytearray_to_integer(True, bytes)
  }
}

fn chunks(w: W, bytes: ByteArray) -> W {
  let length = bytearray.length(bytes)
  if length == 0 {
    put(w, 0, 8)
  } else {
    let k = if length > 255 {
      255
    } else {
      length
    }
    chunks(
      put(put(w, k, 8), bytes_int(bytearray.take(bytes, k)), 8 * k),
      bytearray.drop(bytes, k),
    )
  }
}

fn flat_bytes(w: W, bytes: ByteArray) -> W {
  chunks(filler(w), bytes)
}

fn to_bytes(w: W) -> ByteArray {
  let (acc, n) = w
  builtin.integer_to_bytearray(True, n / 8, acc)
}

fn finish(w: W) -> ByteArray {
  to_bytes(filler(w))
}

fn program(minor: Int, body: Term) -> ByteArray {
  (0, 0) |> nat(1) |> nat(minor) |> nat(0) |> enc(body) |> finish
}

// ## Terms and values

type Term {
  TVar { index: Int }
  TDelay { body: Term }
  TLam { body: Term }
  TApp { function: Term, argument: Term }
  TForce { body: Term }
  TErr
  TBuiltin { tag: Int }
  TConstr { tag: Int, fields: List<Term> }
  TCase { scrutinee: Term, branches: List<Term> }
  /// A constant with its Flat type tags (applications included).
  TCon { tags: List<Int>, value: Val }
  /// Raw bits where a term goes.
  TBits { value: Int, width: Int }
  /// A term tag followed by raw bytes.
  TRaw { tag: Int, bytes: List<Int> }
}

type Val {
  VInt { n: Int }
  VBytes { bytes: ByteArray }
  VBool { b: Bool }
  VUnit
  VList { items: List<Val> }
  VPair { first: Val, second: Val }
  /// A byte-string filler that ends one bit past a byte boundary.
  VMisaligned
}

fn enc(w: W, term: Term) -> W {
  when term is {
    TVar { index } -> nat(put(w, 0, 4), index)
    TDelay { body } -> enc(put(w, 1, 4), body)
    TLam { body } -> enc(put(w, 2, 4), body)
    TApp { function, argument } -> enc(enc(put(w, 3, 4), function), argument)
    TForce { body } -> enc(put(w, 5, 4), body)
    TErr -> put(w, 6, 4)
    TBuiltin { tag } -> put(put(w, 7, 4), tag, 7)
    TConstr { tag, fields } -> enc_list(nat(put(w, 8, 4), tag), fields)
    TCase { scrutinee, branches } ->
      enc_list(enc(put(w, 9, 4), scrutinee), branches)
    TCon { tags, value } -> enc_value(type_tags(put(w, 4, 4), tags), value)
    TBits { value, width } -> put(w, value, width)
    TRaw { tag, bytes } -> raw(put(w, tag, 4), bytes)
  }
}

fn enc_list(w: W, terms: List<Term>) -> W {
  when terms is {
    [] -> put(w, 0, 1)
    [term, ..rest] -> enc_list(enc(put(w, 1, 1), term), rest)
  }
}

fn enc_value(w: W, value: Val) -> W {
  when value is {
    VInt { n } -> int(w, n)
    VBytes { bytes } -> flat_bytes(w, bytes)
    VBool { b } ->
      put(
        w,
        if b {
          1
        } else {
          0
        },
        1,
      )
    VUnit -> w
    VList { items } -> enc_values(w, items)
    VPair { first, second } -> enc_value(enc_value(w, first), second)
    VMisaligned -> {
      let (_, n) = w
      chunks(put(w, 1, 8 - n % 8 + 1), #"")
    }
  }
}

fn enc_values(w: W, items: List<Val>) -> W {
  when items is {
    [] -> put(w, 0, 1)
    [item, ..rest] -> enc_values(enc_value(put(w, 1, 1), item), rest)
  }
}

fn type_tags(w: W, tags: List<Int>) -> W {
  when tags is {
    [] -> put(w, 0, 1)
    [tag, ..rest] -> type_tags(put(put(w, 1, 1), tag, 4), rest)
  }
}

fn var(index: Int) -> Term {
  TVar { index }
}

fn delay(body: Term) -> Term {
  TDelay { body }
}

fn lam(body: Term) -> Term {
  TLam { body }
}

fn app(function: Term, argument: Term) -> Term {
  TApp { function, argument }
}

fn force(body: Term) -> Term {
  TForce { body }
}

fn err() -> Term {
  TErr
}

fn builtin_term(tag: Int) -> Term {
  TBuiltin { tag }
}

fn constr(tag: Int, fields: List<Term>) -> Term {
  TConstr { tag, fields }
}

fn case_of(scrutinee: Term, branches: List<Term>) -> Term {
  TCase { scrutinee, branches }
}

fn con(tags: List<Int>, value: Val) -> Term {
  TCon { tags, value }
}

fn v_int(n: Int) -> Val {
  VInt { n }
}

fn v_bytes(bytes: ByteArray) -> Val {
  VBytes { bytes }
}

fn v_bool(b: Bool) -> Val {
  VBool { b }
}

fn v_unit() -> Val {
  VUnit
}

fn v_list(items: List<Val>) -> Val {
  VList { items }
}

fn v_pair(first: Val, second: Val) -> Val {
  VPair { first, second }
}

fn v_value(entries: List<(ByteArray, List<(ByteArray, Int)>)>) -> Val {
  v_list(
    list.map(
      entries,
      fn(entry) {
        let (currency, tokens) = entry
        v_pair(
          v_bytes(currency),
          v_list(
            list.map(
              tokens,
              fn(token) {
                let (name, quantity) = token
                v_pair(v_bytes(name), v_int(quantity))
              },
            ),
          ),
        )
      },
    ),
  )
}

fn value_data(entries: List<(ByteArray, List<(ByteArray, Int)>)>) -> Data {
  builtin.map_data(
    list.map(
      entries,
      fn(entry) {
        let (currency, tokens) = entry
        Pair(
          builtin.b_data(currency),
          builtin.map_data(
            list.map(
              tokens,
              fn(token) {
                let (name, quantity) = token
                Pair(builtin.b_data(name), builtin.i_data(quantity))
              },
            ),
          ),
        )
      },
    ),
  )
}

// ## Running the decoder

fn wrap(flat: ByteArray) -> ByteArray {
  let length = bytearray.length(flat)
  let head = if length < 24 {
    builtin.cons_bytearray(64 + length, #"")
  } else if length < 256 {
    builtin.cons_bytearray(88, builtin.cons_bytearray(length, #""))
  } else {
    builtin.cons_bytearray(89, builtin.integer_to_bytearray(True, 2, length))
  }
  bytearray.concat(head, flat)
}

fn run_script(script: ByteArray) -> ScriptDecodeRecordV1 {
  let run = initial_run_v1(#"aa", 3, script)
  let env = decode_env_v1(script, run)
  let machine = step_v1(
    env,
    run.machine,
    [],
    run.machine.stack_root,
    100_000_000,
  )
  expect Some(record) = record_of_v1(ScriptDecodeRunV1 { ..run, machine })
  record
}

fn decode(flat: ByteArray) -> ScriptDecodeRecordV1 {
  run_script(wrap(flat))
}

/// Walks a script over many steps of `fuel` ticks, each revealing only the
/// top `visible` frames, and checks each step against `step_v1`.
fn walk_script(
  script: ByteArray,
  fuel: Int,
  visible: Int,
) -> ScriptDecodeRecordV1 {
  let run = initial_run_v1(#"aa", 3, script)
  let env = decode_env_v1(script, run)
  let machine = walk_loop(env, run.machine, [], fuel, visible)
  expect Some(record) = record_of_v1(ScriptDecodeRunV1 { ..run, machine })
  record
}

fn walk_loop(
  env: script_decode_v1.DecodeEnv,
  machine: Machine,
  frames: List<Frame>,
  fuel: Int,
  visible: Int,
) -> Machine {
  if is_terminal(machine.control) {
    machine
  } else {
    let (next, after) = step_with_frames_v1(env, machine, frames, visible, fuel)
    expect next != machine
    expect
      step_v1(
        env,
        machine,
        list.take(frames, visible),
        stack_root_of(list.drop(frames, visible), empty_stack_root()),
        fuel,
      ) == next
    expect stack_root_of(after, empty_stack_root()) == next.stack_root
    walk_loop(env, next, after, fuel, visible)
  }
}

fn root_of(flat: ByteArray) -> ByteArray {
  expect Decoded { term_root, phase2_class, program_version, .. } = decode(flat)
  expect phase2_class == 0
  expect program_version >= 0
  term_root
}

fn phase2_of(flat: ByteArray) -> Int {
  expect Decoded { phase2_class, .. } = decode(flat)
  phase2_class
}

fn refusal(flat: ByteArray) -> (Int, Int) {
  expect Refused { class, bit_offset, .. } = decode(flat)
  (class, bit_offset)
}

fn refused(class: Int, bit_offset: Int) -> ScriptDecodeRecordV1 {
  Refused { script_hash: #"aa", language: 3, class, bit_offset }
}

fn class_of(flat: ByteArray) -> Int {
  refusal(flat).1st
}

// ## Expected roots

fn constant_root(type_cbor_tags: List<Int>, payload: Data) -> ByteArray {
  cek_proof_v1.hash_constant_term_v1(
    cek_constant_v1.constant_root_v1(
      ConstantWitnessV1 {
        type_cbor: cbor.serialise(type_cbor_tags),
        payload_cbor: cbor.serialise(payload),
      },
    ),
  )
}

fn sequence_root(roots: List<ByteArray>) -> ByteArray {
  when roots is {
    [] -> cek_proof_v1.empty_sequence_root_v1
    [head, ..rest] ->
      cek_proof_v1.hash_sequence_node_v1(
        head,
        sequence_root(rest),
        list.length(roots),
      )
  }
}

/// The same root from the semantic summary. The payload decoder behind
/// `constant_root_v1` does not take bignum or tag-102 payloads, so rows that
/// need them commit through the summary instead.
fn semantic_root(
  type_cbor_tags: List<Int>,
  payload: Data,
  memory: Int,
) -> ByteArray {
  cek_proof_v1.hash_constant_term_v1(
    cek_constant_v1.semantic_constant_root_v1(
      cbor.serialise(type_cbor_tags),
      cek_data_v1.semantic_data_summary_v1(payload),
      memory,
    ),
  )
}

fn int_root(n: Int) -> ByteArray {
  semantic_root([0], builtin.i_data(n), cek_data_v1.integer_memory_size_v1(n))
}

fn unit_data() -> Data {
  builtin.constr_data(0, [])
}

// ## Wrapper and framing

test script_decode_canonical_constant() {
  root_of(#"010000480081") == int_root(1)
}

test script_decode_refuses_non_bytestring_wrapper() {
  and {
    run_script(#"") == refused(1, 0),
    run_script(#"a0") == refused(1, 0),
    run_script(#"5f010000480081ff") == refused(1, 0),
    run_script(#"47010000480081") == refused(1, 0),
    run_script(#"1801") == refused(1, 0),
  }
}

test script_decode_accepts_non_minimal_wrapper_head() {
  expect Decoded { term_root, .. } = run_script(#"5806010000480081")
  term_root == int_root(1)
}

test script_decode_trailing_cbor_bytes_after_a_decoded_program() {
  and {
    run_script(bytearray.concat(wrap(#"010000480081"), #"00")) == refused(2, 56),
    // The Flat failure comes first in stream order.
    run_script(bytearray.concat(wrap(#"010000480083"), #"00")) == refused(4, 55),
  }
}

// Offsets count from the start of the script bytes, so the one-byte CBOR
// head adds 8 to each Flat position.
test script_decode_final_padding() {
  and {
    refusal(#"01000048008100") == (4, 56),
    refusal(#"010000480081ff") == (4, 56),
    refusal(#"0100004800810000") == (4, 56),
    refusal(#"010000480083") == (4, 55),
    refusal(#"0100004800bf") == (4, 51),
    refusal(#"0100004800a0") == (4, 51),
    refusal(#"01000048008103") == (4, 56),
    refusal(#"01000048008180") == (4, 56),
    root_of(#"01000048008001") == int_root(1),
    refusal(#"010000480080") == (3, 56),
    refusal(#"") == (3, 8),
    refusal(#"0100") == (3, 24),
  }
}

// ## Versions

test script_decode_version_classes() {
  let body = con([0], v_int(1))
  let version = fn(major, minor, patch) {
    decode(
      (0, 0) |> nat(major) |> nat(minor) |> nat(patch) |> enc(body) |> finish,
    )
  }
  expect Decoded { program_version: v100, phase2_class: p100, .. } = version(
    1,
    0,
    0,
  )
  expect Decoded { program_version: v110, phase2_class: p110, .. } = version(
    1,
    1,
    0,
  )
  expect Decoded { program_version: v200, phase2_class: p200, .. } = version(
    2,
    0,
    0,
  )
  expect Decoded { phase2_class: p120, .. } = version(1, 2, 0)
  expect Decoded { phase2_class: p101, .. } = version(1, 0, 1)
  expect Decoded { phase2_class: p000, .. } = version(0, 1, 0)
  expect Decoded { phase2_class: huge, program_version: vhuge, .. } = version(
    1,
    0,
    math.pow2(150),
  )
  // A non-minimal varint is the same natural.
  expect Decoded { program_version: vnm, phase2_class: pnm, .. } = decode(
    (0, 0) |> raw([129, 0]) |> nat(0) |> nat(0) |> enc(body) |> finish,
  )
  and {
    v100 == 0,
    p100 == 0,
    v110 == 1,
    p110 == 0,
    v200 == -1,
    p200 == 5,
    p120 == 5,
    p101 == 5,
    p000 == 5,
    huge == 5,
    vhuge == -1,
    vnm == 0,
    pnm == 0,
  }
}

// ## Terms

test script_decode_term_roots() {
  let e = cek_proof_v1.hash_error_term_v1()
  let b0 = cek_proof_v1.hash_builtin_term_v1(0)
  and {
    root_of(program(0, err())) == e,
    root_of(program(0, builtin_term(100))) == cek_proof_v1.hash_builtin_term_v1(
      100,
    ),
    root_of(program(0, lam(var(1)))) == cek_proof_v1.hash_lambda_term_v1(
      cek_proof_v1.hash_variable_term_v1(1),
    ),
    root_of(program(0, delay(force(err())))) == cek_proof_v1.hash_delay_term_v1(
      cek_proof_v1.hash_force_term_v1(e),
    ),
    root_of(program(0, app(lam(var(1)), builtin_term(0)))) == cek_proof_v1.hash_application_term_v1(
      cek_proof_v1.hash_lambda_term_v1(cek_proof_v1.hash_variable_term_v1(1)),
      b0,
    ),
    root_of(program(1, constr(0, []))) == cek_proof_v1.hash_constr_term_v1(
      0,
      0,
      cek_proof_v1.empty_sequence_root_v1,
    ),
    root_of(program(1, constr(5, [err(), builtin_term(0)]))) == cek_proof_v1.hash_constr_term_v1(
      5,
      2,
      sequence_root([e, b0]),
    ),
    root_of(program(1, case_of(err(), [builtin_term(0), err(), err()]))) == cek_proof_v1.hash_case_term_v1(
      e,
      3,
      sequence_root([b0, e, e]),
    ),
    root_of(program(1, case_of(err(), []))) == cek_proof_v1.hash_case_term_v1(
      e,
      0,
      cek_proof_v1.empty_sequence_root_v1,
    ),
  }
}

test script_decode_term_tag_classes() {
  and {
    refusal(program(0, TBits { value: 10, width: 4 })) == (6, 36),
    refusal(program(0, TBits { value: 15, width: 4 })) == (6, 36),
    refusal(program(0, constr(0, []))) == (7, 36),
    refusal(program(0, case_of(err(), []))) == (7, 36),
    refusal(program(0, builtin_term(101))) == (8, 43),
    refusal(program(0, builtin_term(127))) == (8, 43),
    refusal(program(0, TBits { value: 7, width: 4 })) == (3, 36),
    refusal(program(0, app(err(), TBits { value: 0, width: 0 }))) == (3, 44),
  }
}

test script_decode_constr_field_limit() {
  let fields = fn(count) { list.repeat(err(), count) }
  let (class, _) = refusal(program(1, constr(0, fields(1025))))
  and {
    root_of(program(1, constr(0, fields(1024)))) == cek_proof_v1.hash_constr_term_v1(
      0,
      1024,
      sequence_root(list.repeat(cek_proof_v1.hash_error_term_v1(), 1024)),
    ),
    class == 9,
    // The bound is checked after the fields, so an earlier failure wins.
    class_of(
      program(
        1,
        constr(0, list.concat(fields(1025), [TBits { value: 12, width: 4 }])),
      ),
    ) == 6,
  }
}

test script_decode_word64_bounds() {
  let word = fn(groups) { program(0, TRaw { tag: 0, bytes: groups }) }
  let nines = list.repeat(255, 9)
  expect Decoded { term_root, phase2_class, .. } = decode(
    word(list.concat(nines, [1])),
  )
  and {
    term_root == cek_proof_v1.hash_variable_term_v1(math.pow2(64) - 1),
    phase2_class == 11,
    refusal(word(list.concat(nines, [2]))) == (10, 116),
    refusal(word(list.concat(nines, [129]))) == (10, 116),
    class_of(program(1, TRaw { tag: 8, bytes: list.concat(nines, [2]) })) == 10,
    refusal(word([255, 255])) == (3, 52),
  }
}

test script_decode_open_terms() {
  and {
    phase2_of(program(0, var(1))) == 11,
    phase2_of(program(0, lam(var(1)))) == 0,
    phase2_of(program(0, lam(var(2)))) == 11,
    phase2_of(program(0, lam(var(0)))) == 11,
    phase2_of(program(0, lam(lam(var(2))))) == 0,
    phase2_of(program(0, app(lam(var(1)), var(1)))) == 11,
    phase2_of(program(0, delay(lam(force(var(1)))))) == 0,
    // Subtrees under constr and case are not scope-checked.
    phase2_of(program(1, constr(0, [var(7)]))) == 0,
    phase2_of(program(1, case_of(var(0), [var(3)]))) == 0,
    phase2_of(program(1, lam(constr(0, [lam(var(9))])))) == 0,
    phase2_of(program(1, app(constr(0, []), var(1)))) == 11,
  }
}

// ## Constant types

fn nested_list_tags(depth: Int) -> List<Int> {
  if depth == 0 {
    [0]
  } else {
    [7, 5, ..nested_list_tags(depth - 1)]
  }
}

fn nested_list_cbor(depth: Int) -> List<Int> {
  if depth == 0 {
    [0]
  } else {
    [5, ..nested_list_cbor(depth - 1)]
  }
}

test script_decode_constant_type_classes() {
  and {
    class_of(program(0, con([14], v_unit()))) == 12,
    class_of(program(0, con([15], v_unit()))) == 12,
    class_of(program(0, con([7, 5, 0, 0], v_list([])))) == 12,
    class_of(program(0, con([7, 7, 6, 0, 0, 0], v_unit()))) == 12,
    class_of(program(0, con([5, 0], v_unit()))) == 12,
    class_of(program(0, con([7, 6, 0, 0], v_unit()))) == 12,
    class_of(program(0, con([7, 7, 6, 0], v_unit()))) == 12,
    class_of(program(0, con([7], v_unit()))) == 12,
    class_of(program(0, con([], v_unit()))) == 12,
    class_of(program(0, con([7, 7, 7, 5, 0], v_unit()))) == 12,
    root_of(program(0, con(nested_list_tags(15), v_list([])))) == constant_root(
      nested_list_cbor(15),
      builtin.list_data([]),
    ),
    class_of(program(0, con(nested_list_tags(16), v_list([])))) == 13,
    // The kind check comes before the size check.
    class_of(
      program(0, con(list.concat(nested_list_tags(16), [0]), v_list([]))),
    ) == 12,
  }
}

test script_decode_bls_constants() {
  and {
    class_of(program(0, con([9], v_bytes(#"00")))) == 14,
    class_of(program(0, con([10], v_bytes(#"00")))) == 14,
    class_of(program(0, con([11], v_unit()))) == 14,
    root_of(program(0, con([7, 5, 9], v_list([])))) == constant_root(
      [5, 9],
      builtin.list_data([]),
    ),
    class_of(program(0, con([7, 5, 9], v_list([v_bytes(#"00")])))) == 14,
    class_of(program(0, con([7, 7, 6, 0, 10], v_pair(v_int(1), v_unit())))) == 14,
  }
}

// ## Constant values

test script_decode_integer_constants() {
  let ints = [
    0,
    1,
    -1,
    63,
    64,
    -64,
    -65,
    1000000,
    math.pow2(64),
    -math.pow2(64),
    math.pow2(139) - 1,
    math.pow2(139),
    -math.pow2(139) - 1,
    math.pow2(140),
    math.pow2(200),
    -math.pow2(200),
    math.pow2(511) + 12345,
    -math.pow2(512) - 1,
    math.pow2(1000) - 1,
    -math.pow2(4000),
  ]
  list.all(
    ints,
    fn(n) { root_of(program(0, con([0], v_int(n)))) == int_root(n) },
  )
}

test script_decode_bytes_strings_and_scalars() {
  let long = builtin.replicate_byte(700, 171)
  and {
    root_of(program(0, con([1], v_bytes(#"")))) == constant_root(
      [1],
      builtin.b_data(#""),
    ),
    root_of(program(0, con([1], v_bytes(#"abcd")))) == constant_root(
      [1],
      builtin.b_data(#"abcd"),
    ),
    root_of(program(0, con([1], v_bytes(long)))) == constant_root(
      [1],
      builtin.b_data(long),
    ),
    root_of(program(0, con([2], v_bytes(#"c3a948656c6c6f")))) == constant_root(
      [2],
      builtin.b_data(#"c3a948656c6c6f"),
    ),
    root_of(program(0, con([2], v_bytes(#"")))) == constant_root(
      [2],
      builtin.b_data(#""),
    ),
    class_of(program(0, con([2], v_bytes(#"ff")))) == 15,
    class_of(program(0, con([2], v_bytes(#"e282")))) == 15,
    root_of(program(0, con([3], v_unit()))) == constant_root([3], unit_data()),
    root_of(program(0, con([4], v_bool(True)))) == constant_root(
      [4],
      builtin.constr_data(1, []),
    ),
    root_of(program(0, con([4], v_bool(False)))) == constant_root(
      [4],
      builtin.constr_data(0, []),
    ),
  }
}

test script_decode_inner_filler_alignment() {
  // A byte-string filler that ends off a byte boundary.
  and {
    class_of(program(0, con([1], VMisaligned))) == 4,
    class_of(program(0, con([2], VMisaligned))) == 4,
    class_of(program(0, con([8], VMisaligned))) == 4,
  }
}

test script_decode_containers() {
  and {
    root_of(program(0, con([7, 5, 0], v_list([v_int(1), v_int(-2), v_int(3)])))) == constant_root(
      [5, 0],
      builtin.list_data(
        [builtin.i_data(1), builtin.i_data(-2), builtin.i_data(3)],
      ),
    ),
    root_of(program(0, con([7, 12, 0], v_list([v_int(1), v_int(2)])))) == constant_root(
      [12, 0],
      builtin.list_data([builtin.i_data(1), builtin.i_data(2)]),
    ),
    root_of(program(0, con([7, 7, 6, 0, 1], v_pair(v_int(5), v_bytes(#"ab"))))) == constant_root(
      [6, 0, 1],
      builtin.constr_data(0, [builtin.i_data(5), builtin.b_data(#"ab")]),
    ),
    root_of(
      program(
        0,
        con(
          [7, 5, 7, 7, 6, 2, 7, 5, 4],
          v_list(
            [
              v_pair(v_bytes(#"41"), v_list([v_bool(True), v_bool(False)])),
              v_pair(v_bytes(#""), v_list([])),
            ],
          ),
        ),
      ),
    ) == constant_root(
      [5, 6, 2, 5, 4],
      builtin.list_data(
        [
          builtin.constr_data(
            0,
            [
              builtin.b_data(#"41"),
              builtin.list_data(
                [builtin.constr_data(1, []), builtin.constr_data(0, [])],
              ),
            ],
          ),
          builtin.constr_data(0, [builtin.b_data(#""), builtin.list_data([])]),
        ],
      ),
    ),
  }
}

// ## Data constants

fn data_flat(cbor_bytes: ByteArray) -> ByteArray {
  program(0, con([8], v_bytes(cbor_bytes)))
}

fn data_root(data: Data) -> ByteArray {
  semantic_root([8], data, cek_data_v1.semantic_data_summary_v1(data).memory)
}

test script_decode_data_canonical_items() {
  let items = [
    builtin.i_data(1),
    builtin.i_data(-1000),
    builtin.i_data(math.pow2(64)),
    builtin.i_data(-math.pow2(300)),
    builtin.b_data(#""),
    builtin.b_data(builtin.replicate_byte(64, 7)),
    builtin.b_data(builtin.replicate_byte(300, 9)),
    builtin.list_data([]),
    builtin.map_data([]),
    builtin.map_data(
      [
        Pair(builtin.i_data(1), builtin.b_data(#"")),
        Pair(builtin.i_data(1), builtin.list_data([])),
      ],
    ),
    builtin.constr_data(0, []),
    builtin.constr_data(6, [builtin.i_data(1)]),
    builtin.constr_data(7, [builtin.i_data(1), builtin.b_data(#"ab")]),
    builtin.constr_data(127, []),
    builtin.constr_data(128, []),
    builtin.constr_data(
      math.pow2(64) - 1,
      [builtin.list_data([builtin.i_data(2)])],
    ),
    builtin.list_data(
      [
        builtin.constr_data(
          1,
          [builtin.map_data([Pair(builtin.b_data(#"aa"), unit_data())])],
        ),
        builtin.list_data([builtin.list_data([builtin.i_data(3)])]),
      ],
    ),
  ]
  list.all(
    items,
    fn(d) { root_of(data_flat(cbor.serialise(d))) == data_root(d) },
  )
}

test script_decode_data_accepts_non_canonical_encodings() {
  let one = data_root(builtin.i_data(1))
  and {
    root_of(data_flat(#"1801")) == one,
    root_of(data_flat(#"1b0000000000000001")) == one,
    root_of(data_flat(#"c24101")) == one,
    root_of(data_flat(#"c240")) == data_root(builtin.i_data(0)),
    root_of(data_flat(#"c340")) == data_root(builtin.i_data(-1)),
    root_of(data_flat(#"c25f4101ff")) == one,
    root_of(data_flat(#"c25f4041004101ff")) == one,
    root_of(data_flat(#"9f01ff")) == data_root(
      builtin.list_data([builtin.i_data(1)]),
    ),
    root_of(data_flat(#"9fff")) == data_root(builtin.list_data([])),
    root_of(data_flat(#"bf0102ff")) == data_root(
      builtin.map_data([Pair(builtin.i_data(1), builtin.i_data(2))]),
    ),
    root_of(data_flat(#"5f41014102ff")) == data_root(builtin.b_data(#"0102")),
    root_of(data_flat(#"5fff")) == data_root(builtin.b_data(#"")),
    root_of(data_flat(#"d866820080")) == data_root(builtin.constr_data(0, [])),
    root_of(data_flat(#"d8669f0080ff")) == data_root(builtin.constr_data(0, [])),
    root_of(data_flat(#"d8669f009f01ffff")) == data_root(
      builtin.constr_data(0, [builtin.i_data(1)]),
    ),
    root_of(data_flat(#"d8668218c880")) == data_root(
      builtin.constr_data(200, []),
    ),
    root_of(data_flat(#"d8799f01ff")) == data_root(
      builtin.constr_data(0, [builtin.i_data(1)]),
    ),
    root_of(data_flat(#"d9050080")) == data_root(builtin.constr_data(7, [])),
    root_of(data_flat(#"590001ab")) == data_root(builtin.b_data(#"ab")),
  }
}

test script_decode_data_refusals() {
  let invalid = fn(cbor_bytes) {
    let flat = data_flat(cbor_bytes)
    // InvalidData sits at the end of the Flat byte string, before the
    // final filler byte.
    refusal(flat) == (16, 8 * ( bytearray.length(wrap(flat)) - 1 ))
  }
  and {
    invalid(#""),
    invalid(#"6161"),
    invalid(#"0101"),
    invalid(#"ff"),
    invalid(#"f6"),
    invalid(#"1c"),
    invalid(#"1f"),
    invalid(#"3f"),
    invalid(#"d87a9f"),
    invalid(#"d8024101"),
    invalid(#"c201"),
    invalid(#"c2d87980"),
    invalid(#"df"),
    invalid(#"d86683008000"),
    invalid(#"d8669f008001"),
    invalid(#"d866820140"),
    invalid(#"d866822080"),
    invalid(#"d8668200a0"),
    invalid(#"d87801"),
    invalid(#"d87f01"),
    invalid(#"d9057980"),
    invalid(#"bf01ff"),
    invalid(#"8201ff"),
    invalid(#"a101"),
    invalid(#"5f01ff"),
    invalid(#"5f5f4101ffff"),
    invalid(bytearray.concat(#"5841", builtin.replicate_byte(65, 0))),
    invalid(bytearray.concat(#"c25841", builtin.replicate_byte(65, 1))),
    invalid(#"8301"),
  }
}

test script_decode_data_truncation_beats_invalid_data() {
  let truncated = fn(prefix: List<Int>) {
    fn(w) {
      let w = type_tags(put(w, 4, 4), [8])
      raw(filler(w), prefix)
    }
  }
  let head = (0, 0) |> nat(1) |> nat(0) |> nat(0)
  and {
    // The chunk claims five bytes and the Flat input ends first.
    class_of(to_bytes(truncated([5, 1, 1])(head))) == 3,
    // Invalid CBOR, then a chunk that overruns the input.
    class_of(to_bytes(truncated([1, 96, 9, 1])(head))) == 3,
    // No terminating chunk.
    class_of(to_bytes(truncated([1, 1])(head))) == 3,
  }
}

// ## Value constants

test script_decode_value_constants() {
  let accepted = fn(entries) {
    root_of(program(0, con([13], v_value(entries)))) == semantic_root(
      [13],
      value_data(entries),
      list.foldl(
        entries,
        0,
        fn(entry, total) { total + list.length(entry.2nd) },
      ),
    )
  }
  and {
    accepted([]),
    accepted([(#"", [(#"", 1)])]),
    accepted(
      [
        (#"01", [(#"", -5), (#"aa", math.pow2(127) - 1)]),
        (#"0101", [(#"00", -math.pow2(127))]),
        (builtin.replicate_byte(32, 255), [(builtin.replicate_byte(32, 1), 7)]),
      ],
    ),
  }
}

test script_decode_value_refusals() {
  let refused = fn(entries) {
    class_of(program(0, con([13], v_value(entries)))) == 17
  }
  and {
    refused([(#"", [(#"", 0)])]),
    refused([(#"02", [(#"", 1)]), (#"01", [(#"", 1)])]),
    refused([(#"01", [(#"", 1)]), (#"01", [(#"aa", 1)])]),
    refused([(#"01", [(#"bb", 1), (#"aa", 1)])]),
    refused([(#"01", [(#"aa", 1), (#"aa", 2)])]),
    refused([(#"01", [])]),
    refused([(builtin.replicate_byte(33, 0), [(#"", 1)])]),
    refused([(#"", [(builtin.replicate_byte(33, 0), 1)])]),
    refused([(#"", [(#"", math.pow2(127))])]),
    refused([(#"", [(#"", -math.pow2(127) - 1)])]),
    refused([(#"", [(#"", math.pow2(400))])]),
  }
}

// ## Multi-step walks

fn mixed_program() -> ByteArray {
  program(
    1,
    app(
      lam(
        constr(
          3,
          [
            var(1),
            con([0], v_int(-math.pow2(900))),
            con(
              [8],
              v_bytes(
                cbor.serialise(
                  builtin.list_data(
                    [
                      builtin.constr_data(
                        200,
                        [builtin.b_data(builtin.replicate_byte(150, 3))],
                      ),
                      builtin.map_data(
                        [Pair(builtin.i_data(math.pow2(70)), unit_data())],
                      ),
                    ],
                  ),
                ),
              ),
            ),
            con(
              [13],
              v_value([(#"01", [(#"aa", 3), (#"bb", -math.pow2(100))])]),
            ),
            con(
              [7, 7, 6, 2, 7, 5, 1],
              v_pair(v_bytes(#"e282ac"), v_list([v_bytes(#"00"), v_bytes(#"")])),
            ),
            case_of(err(), [delay(builtin_term(7)), force(err())]),
          ],
        ),
      ),
      con([4], v_bool(True)),
    ),
  )
}

test script_decode_walk_matches_one_step() {
  let script = wrap(mixed_program())
  let whole = run_script(script)
  expect Decoded { phase2_class, .. } = whole
  and {
    phase2_class == 0,
    walk_script(script, 1, 1) == whole,
    walk_script(script, 3, 2) == whole,
    walk_script(script, 50, 4) == whole,
  }
}

test script_decode_walk_matches_one_step_dense_and_deep() {
  let dense = wrap(program(1, constr(0, list.repeat(builtin_term(1), 100))))
  let deep = wrap(
    program(
      0,
      list.foldl(
        list.repeat(0, 60),
        app(err(), err()),
        fn(_, body) { lam(delay(body)) },
      ),
    ),
  )
  and {
    walk_script(dense, 7, 1) == run_script(dense),
    walk_script(deep, 5, 1) == run_script(deep),
  }
}

test script_decode_walk_matches_on_refusal() {
  let script = wrap(program(0, con([13], v_value([(#"01", [(#"", 0)])]))))
  walk_script(script, 1, 1) == run_script(script)
}

test script_decode_step_waits_for_a_hidden_frame() {
  let script = wrap(program(0, lam(var(1))))
  let run = initial_run_v1(#"aa", 3, script)
  let env = decode_env_v1(script, run)
  let (machine, frames) = step_with_frames_v1(env, run.machine, [], 0, 5)
  and {
    frames != [],
    step_v1(env, machine, [], machine.stack_root, 5) == machine,
    step_v1(env, machine, frames, empty_stack_root(), 5) != machine,
  }
}

test script_decode_step_rejects_a_wrong_reveal() fail {
  let script = wrap(program(0, lam(var(1))))
  let run = initial_run_v1(#"aa", 3, script)
  let env = decode_env_v1(script, run)
  let (machine, _) = step_with_frames_v1(env, run.machine, [], 0, 5)
  step_v1(env, machine, [script_decode_v1.FDelay], empty_stack_root(), 5) == machine
}
