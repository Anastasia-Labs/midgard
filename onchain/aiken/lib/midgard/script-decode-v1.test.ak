use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/fuzz
use aiken/math
use aiken/primitive/bytearray
use midgard/cbor_deserialise
use midgard/cek_constant_v1.{ConstantWitnessV1}
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1
use midgard/script_decode_bits_v1 as bits
use midgard/script_decode_bulk_v1 as bulk
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

/// A Data constant is the value of its first CBOR item; the bytes after it
/// are not decoded, valid CBOR or not. Cardano evaluates both programs below
/// to `(con data (I 0))`.
test script_decode_data_constant_is_its_first_item() {
  let zero = data_root(builtin.i_data(0))
  and {
    root_of(#"0101004c01020000000001") == zero,
    root_of(#"0101004c010200ff0001") == zero,
    root_of(data_flat(#"0101")) == data_root(builtin.i_data(1)),
    root_of(data_flat(#"d87980d87980")) == data_root(builtin.constr_data(0, [])),
    // The trailing bytes span several Flat chunks.
    root_of(
      data_flat(bytearray.concat(#"00", builtin.replicate_byte(600, 255))),
    ) == zero,
  }
}

/// Stand-alone Data (datums, redeemers) still refuses bytes after the item.
test script_decode_standalone_data_still_refuses_trailing_bytes() {
  and {
    cbor_deserialise.deserialise(#"0000") == None,
    cbor_deserialise.deserialise(#"00ff") == None,
    cbor_deserialise.deserialise(#"00") == Some(builtin.i_data(0)),
  }
}

/// The trailing chunks are still Flat: a chunk that overruns the input is
/// truncation.
test script_decode_data_trailing_chunks_are_still_framed() {
  let head = (0, 0) |> nat(1) |> nat(0) |> nat(0)
  let w = type_tags(put(head, 4, 4), [8])
  class_of(to_bytes(raw(filler(w), [1, 0, 5, 1]))) == 3
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
    refused([(#"01", [(#"aa", 1), (#"cc", 1), (#"bb", 1)])]),
    refused([(#"01", [])]),
    refused([(builtin.replicate_byte(33, 0), [(#"", 1)])]),
    refused([(#"", [(builtin.replicate_byte(33, 0), 1)])]),
    refused([(#"", [(#"", math.pow2(127))])]),
    refused([(#"", [(#"", -math.pow2(127) - 1)])]),
    refused([(#"", [(#"", math.pow2(400))])]),
  }
}

test script_decode_value_token_order_is_checked_in_a_list_of_values() {
  let ordered = v_value([(#"01", [(#"aa", 1), (#"bb", 1), (#"cc", 1)])])
  let unordered = v_value([(#"01", [(#"aa", 1), (#"cc", 1), (#"bb", 1)])])
  expect Refused { class, .. } = bulk_record(
    list_program([7, 5, 13], list.concat(list.repeat(ordered, 3), [unordered])),
  )
  expect Decoded { .. } = bulk_record(
    list_program([7, 5, 13], list.repeat(ordered, 4)),
  )
  class == 17
}

// ## Cardano differential rows (E3)
//
// Raw Flat programs with the result the Plutus V3 evaluator (plutus-core
// 1.71, protocol version 11) gives them through `deserialiseScript`, each
// wrapped in one CBOR byte string. These are the rows of the off-chain
// reader's vectors, `cek-program.cardano-flat.vectors.ts`.

/// Programs Cardano reads, each with the canonical Flat of the program it
/// read. Both decode, with no phase-2 class, to the same term root.
const cardano_accepted: List<(ByteArray, ByteArray)> =
  [
    // a Data filler running through a zero byte
    (#"0101004c000101000001", #"0101004c0101000001"),
    // serialiseData over a Data filler running through a zero byte
    (#"010100376698000101000001", #"0101003766980101000001"),
    // a byte-string filler running through a zero byte
    (#"0101004880010200550001", #"01010048810200550001"),
    // a byte-string filler with an extra zero byte
    (#"01010048800101aa0001", #"010100488101aa0001"),
    // a Data filler with an extra zero byte
    (#"010100376698000103c241010001", #"0101003766980101010001"),
    // Data bytes split into chunks of 1 and 2
    (#"0101003766980102c24101010001", #"0101003766980101010001"),
    // Data bytes split into chunks of 1, 1 and 1
    (#"0101003766980101c2014101010001", #"0101003766980101010001"),
    // a final pad of 0001
    (#"0101003766980103c24101000001", #"0101003766980101010001"),
    // a version number with a zero continuation group
    (#"810001004981", #"0101004981"),
    // builtin tag 100
    (#"0101007c81", #"0101007c81"),
    // builtin tag 33
    (#"0101007421", #"0101007421"),
    // constr in 1.1.0
    (#"0101008001", #"0101008001"),
    // constr tag 2^64 - 1
    (#"0101008ffffffffffffffffff011", #"0101008ffffffffffffffffff011"),
    // a bound variable
    (#"010100200101", #"010100200101"),
    // an empty integer list
    (#"0101004bd601", #"0101004bd601"),
    // a constant type of 31 tags
    (
      #"0101004bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6001",
      #"0101004bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6001",
    ),
    // an empty list of BLS12-381 G1 elements
    (#"0101004bd721", #"0101004bd721"),
    // a string starting with a byte-order mark
    (#"010100490104efbbbf610001", #"010100490104efbbbf610001"),
    // an integer and bool pair
    (#"0101004bded0a017", #"0101004bded0a017"),
  ]

/// Programs Cardano refuses to decode, with this decoder's class.
const cardano_refused: List<(ByteArray, Int)> =
  [
    // a 00 byte after the final pad
    (#"0101003766980103c24101000100", 4),
    // an ff byte after the final pad
    (#"0101003766980103c241010001ff", 4),
    // a final pad of 03
    (#"0101003766980103c241010003", 4),
    // a final pad of ff
    (#"0101003766980103c2410100ff", 4),
    // a final pad of 80
    (#"0101003766980103c241010080", 4),
    // a final pad ending inside a byte
    (#"0101007430", 4),
    // no final pad
    (#"0101003766980103c2410100", 3),
    // a byte-string filler ending in an early 1 bit
    (#"010100488401aa0001", 4),
    // term tag 10
    (#"010100a1", 6),
    // an unknown term tag after an unbound variable
    (#"01010032000b01", 6),
    // builtin tag 101
    (#"0101007ca1", 8),
    // builtin tag 105
    (#"0101007d21", 8),
    // builtin tag 106
    (#"0101007d41", 8),
    // constr in 1.0.0
    (#"0100008001", 7),
    // case in 1.0.0
    (#"010000980001", 7),
    // a constr tag over 64 bits
    (#"0101008ffffffffffffffffff021", 10),
    // a constr tag of 11 groups
    (#"010100880808080808080808081001", 10),
    // a variable index over 64 bits
    (#"01010020ffffffffffffffffff0201", 10),
    // a constant without type tags
    (#"01010041", 12),
    // a protoList constant
    (#"0101004a81", 12),
    // a type tag left over
    (#"010100484005", 12),
    // type tag 14
    (#"0101004f01", 12),
    // list applied to list
    (#"0101004bd6a1", 12),
    // a constant type of 33 tags
    (#"0101004bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5801", 13),
    // a BLS12-381 G1 constant
    (
      #"0101004c81300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001",
      14,
    ),
    // a string of ff
    (#"010100490101ff0001", 15),
    // a Data constant of ff
    (#"0101004c0101ff0001", 16),
    // a Value with a zero quantity
    (#"0101004ea10081000001", 17),
    // a Value with a 33-byte key
    (
      #"0101004ea1210000000000000000000000000000000000000000000000000000000000000000000001",
      17,
    ),
    // an integer array before a trailing byte
    (#"0101004bf20100", 4),
  ]

test script_decode_cardano_accepted_rows() {
  list.all(
    cardano_accepted,
    fn(row) {
      let (raw, twin) = row
      expect Decoded { term_root, phase2_class: 0, .. } = decode(raw)
      term_root == root_of(twin)
    },
  )
}

test script_decode_cardano_accepted_constr_of_1024_fields() {
  // `constr 0` over 1,024 unit constants is read; over 1,025 it is refused.
  let units = fn(count) { list.repeat(con([3], v_unit()), count) }
  and {
    root_of(program(1, constr(0, units(1024)))) != #"",
    class_of(program(1, constr(0, units(1025)))) == 9,
  }
}

test script_decode_cardano_refused_rows() {
  list.all(
    cardano_refused,
    fn(row) {
      let (raw, class) = row
      class_of(raw) == class
    },
  )
}

test script_decode_cardano_phase_two_rows() {
  and {
    // Cardano: DeBruijnError (FreeIndex 0) when the script runs.
    phase2_of(#"010100200001") == 11,
    // Cardano: PlutusCoreLanguageNotAvailableError when the script runs.
    phase2_of(#"8002000061") == 5,
    // Cardano runs these: an empty integer array, an empty Value and a
    // Value of one.
    phase2_of(#"0101004bf201") == 0,
    phase2_of(#"0101004e81") == 0,
    phase2_of(#"0101004ea10081000201") == 0,
  }
}

/// `(program 1.0.0 (con integer 1))` = `010000480081` with framing faults.
/// uplc at cfe16666e and Haskell agree on each row.
test script_decode_uplc_framing_rows() {
  and {
    class_of(#"01000048008100") == 4,
    class_of(#"010000480081ff") == 4,
    class_of(#"0100004800810000") == 4,
    class_of(#"01000048008103") == 4,
    class_of(#"01000048008180") == 4,
    class_of(#"010000480083") == 4,
    class_of(#"0100004800bf") == 4,
    class_of(#"0100004800a0") == 4,
    phase2_of(#"01000048008001") == 0,
    class_of(#"010000480080") == 3,
    class_of(#"0100004bd61001") == 12,
    class_of(#"0100004bded084000001") == 12,
    phase2_of(#"0100004bd601") == 0,
  }
}

// ## Constant lists decoded in bulk

test script_decode_bulk_summaries_match_cek_data() {
  let empty = cek_data_v1.empty_data_list_summary_v1()
  let small = cek_data_v1.semantic_data_summary_v1(builtin.i_data(5))
  let large = cek_data_v1.semantic_data_summary_v1(
    builtin.list_data(list.repeat(builtin.b_data(#"abcdef"), 40)),
  )
  let one = cek_data_v1.prepend_data_list_summary_v1(large, empty)
  let many = list.foldl(
    list.repeat(small, 30),
    one,
    fn(head, tail) { cek_data_v1.prepend_data_list_summary_v1(head, tail) },
  )
  let pair = fn(a, b) {
    cek_data_v1.small_constr_data_summary_v1(
      0,
      cek_data_v1.prepend_data_list_summary_v1(
        a,
        cek_data_v1.prepend_data_list_summary_v1(b, empty),
      ),
    )
  }
  let chunk = fn(n) { builtin.replicate_byte(n, 7) }
  and {
    list.all(
      [empty, one, many],
      fn(tail) {
        and {
          bulk.prepend_v1(small, tail) == cek_data_v1.prepend_data_list_summary_v1(
            small,
            tail,
          ),
          bulk.prepend_v1(large, tail) == cek_data_v1.prepend_data_list_summary_v1(
            large,
            tail,
          ),
          bulk.list_data_summary_v1(tail) == cek_data_v1.list_data_summary_v1(
            tail,
          ),
        }
      },
    ),
    bulk.pair_summary_v1(small, large) == pair(small, large),
    bulk.pair_summary_v1(large, small) == pair(large, small),
    bulk.pair_summary_v1(small, small) == pair(small, small),
    list.all(
      [1, 2, 31, 32, 33, 254, 255],
      fn(n) {
        bits.short_bytes_summary(chunk(n)) == bits.bytes_summary(
          n,
          bits.sink_root(bits.sink_push(bits.empty_sink(), chunk(n))),
        )
      },
    ),
  }
}

/// A constant list of `items` with Flat type tags `tags` (of the list type,
/// applications included).
fn list_program(tags: List<Int>, items: List<Val>) -> ByteArray {
  program(1, con(tags, v_list(items)))
}

/// Decodes in one step and over walks of a few fuels that reveal one frame
/// at a time.
fn bulk_record(flat: ByteArray) -> ScriptDecodeRecordV1 {
  let script = wrap(flat)
  let whole = run_script(script)
  expect walk_script(script, 1, 1) == whole
  expect walk_script(script, 3, 1) == whole
  expect walk_script(script, 40, 2) == whole
  whole
}

fn bulk_root(flat: ByteArray) -> ByteArray {
  expect Decoded { term_root, phase2_class: 0, .. } = bulk_record(flat)
  term_root
}

fn bool_data(b: Bool) -> Data {
  builtin.constr_data(
    if b {
      1
    } else {
      0
    },
    [],
  )
}

fn pair_data(first: Data, second: Data) -> Data {
  builtin.constr_data(0, [first, second])
}

/// A pair chain of `depth` pairs, each with a unit first, over `leaf`.
fn unit_chain_tags(depth: Int, leaf: List<Int>) -> List<Int> {
  if depth == 0 {
    leaf
  } else {
    [7, 7, 6, 3, ..unit_chain_tags(depth - 1, leaf)]
  }
}

fn unit_chain_val(depth: Int, leaf: Val) -> Val {
  if depth == 0 {
    leaf
  } else {
    v_pair(v_unit(), unit_chain_val(depth - 1, leaf))
  }
}

fn unit_chain_data(depth: Int, leaf: Data) -> Data {
  if depth == 0 {
    leaf
  } else {
    pair_data(unit_data(), unit_chain_data(depth - 1, leaf))
  }
}

fn without_applications(tags: List<Int>) -> List<Int> {
  list.filter(tags, fn(tag) { tag != 7 })
}

fn list_root(tags: List<Int>, items: List<Data>) -> ByteArray {
  constant_root(without_applications(tags), builtin.list_data(items))
}

test script_decode_bulk_zero_and_one_bit_elements() {
  let count = 300
  let flags = list.map(list.range(1, count), fn(i) { i % 3 == 0 })
  let chained = list.take(flags, 120)
  and {
    bulk_root(list_program([7, 5, 3], list.repeat(v_unit(), count))) == list_root(
      [7, 5, 3],
      list.repeat(unit_data(), count),
    ),
    bulk_root(list_program([7, 5, 4], list.map(flags, v_bool))) == list_root(
      [7, 5, 4],
      list.map(flags, bool_data),
    ),
    bulk_root(
      list_program(
        [7, 12, 7, 7, 6, 3, 3],
        list.repeat(v_pair(v_unit(), v_unit()), 200),
      ),
    ) == list_root(
      [7, 12, 7, 7, 6, 3, 3],
      list.repeat(pair_data(unit_data(), unit_data()), 200),
    ),
    bulk_root(
      list_program(
        [7, 5, ..unit_chain_tags(7, [4])],
        list.map(chained, fn(b) { unit_chain_val(7, v_bool(b)) }),
      ),
    ) == list_root(
      [7, 5, ..unit_chain_tags(7, [4])],
      list.map(chained, fn(b) { unit_chain_data(7, bool_data(b)) }),
    ),
  }
}

test script_decode_bulk_nested_lists() {
  let inner = fn(n) { v_list(list.repeat(v_unit(), n)) }
  let inner_data = fn(n) { builtin.list_data(list.repeat(unit_data(), n)) }
  let sizes = list.map(list.range(1, 120), fn(i) { i % 4 })
  and {
    bulk_root(list_program([7, 5, 7, 5, 3], list.map(sizes, inner))) == list_root(
      [7, 5, 7, 5, 3],
      list.map(sizes, inner_data),
    ),
    bulk_root(
      list_program(
        [7, 5, ..unit_chain_tags(6, [7, 5, 3])],
        list.map(sizes, fn(n) { unit_chain_val(6, inner(n)) }),
      ),
    ) == list_root(
      [7, 5, ..unit_chain_tags(6, [7, 5, 3])],
      list.map(sizes, fn(n) { unit_chain_data(6, inner_data(n)) }),
    ),
    // An array inside a list: its memory is its length.
    bulk_root(
      list_program([7, 5, 7, 12, 4], list.repeat(v_list([v_bool(True)]), 50)),
    ) == list_root(
      [7, 5, 7, 12, 4],
      list.repeat(builtin.list_data([bool_data(True)]), 50),
    ),
  }
}

test script_decode_bulk_integers_bytes_and_strings() {
  let ints = [
    0,
    1,
    -1,
    63,
    -64,
    1000000,
    math.pow2(64),
    -math.pow2(64),
    math.pow2(139) - 1,
    math.pow2(139),
    -math.pow2(139) - 1,
    math.pow2(200),
    5,
  ]
  let many = list.map(list.range(1, 150), fn(i) { i * 37 - 2000 })
  let byte_strings = [
    #"",
    #"00",
    builtin.replicate_byte(31, 9),
    builtin.replicate_byte(255, 1),
    builtin.replicate_byte(256, 2),
    builtin.replicate_byte(600, 3),
    #"ab",
  ]
  let strings = [
    #"",
    #"616263",
    #"c3a9",
    builtin.replicate_byte(255, 97),
    bytearray.concat(builtin.replicate_byte(84, 0), #"e282ac"),
    list.foldl(
      list.repeat(#"e282ac", 85),
      #"",
      fn(piece, acc) { bytearray.concat(acc, piece) },
    ),
    #"f09f9880",
  ]
  and {
    bulk_root(list_program([7, 5, 0], list.map(ints, v_int))) == list_root(
      [7, 5, 0],
      list.map(ints, builtin.i_data),
    ),
    bulk_root(list_program([7, 12, 0], list.map(many, v_int))) == list_root(
      [7, 12, 0],
      list.map(many, builtin.i_data),
    ),
    bulk_root(list_program([7, 5, 1], list.map(byte_strings, v_bytes))) == list_root(
      [7, 5, 1],
      list.map(byte_strings, builtin.b_data),
    ),
    bulk_root(list_program([7, 5, 2], list.map(strings, v_bytes))) == list_root(
      [7, 5, 2],
      list.map(strings, builtin.b_data),
    ),
    bulk_root(
      list_program(
        [7, 5, 7, 7, 6, 0, 7, 7, 6, 1, 4],
        list.map(
          many,
          fn(n) { v_pair(v_int(n), v_pair(v_bytes(#"cafe"), v_bool(n > 0))) },
        ),
      ),
    ) == list_root(
      [7, 5, 7, 7, 6, 0, 7, 7, 6, 1, 4],
      list.map(
        many,
        fn(n) {
          pair_data(
            builtin.i_data(n),
            pair_data(builtin.b_data(#"cafe"), bool_data(n > 0)),
          )
        },
      ),
    ),
  }
}

test script_decode_bulk_elements_that_decode_one_node_at_a_time() {
  let datum = builtin.list_data([builtin.i_data(1), builtin.b_data(#"aa")])
  let value = [(#"01", [(#"aa", 3), (#"bb", -4)])]
  and {
    bulk_root(
      list_program(
        [7, 5, 7, 7, 6, 8, 0],
        list.repeat(v_pair(v_bytes(cbor.serialise(datum)), v_int(2)), 20),
      ),
    ) == list_root(
      [7, 5, 7, 7, 6, 8, 0],
      list.repeat(pair_data(datum, builtin.i_data(2)), 20),
    ),
    bulk_root(
      list_program(
        [7, 5, 13],
        [v_value([]), v_value(value), v_value([]), v_value(value)],
      ),
    ) == list_root(
      [7, 5, 13],
      [value_data([]), value_data(value), value_data([]), value_data(value)],
    ),
    bulk_root(
      list_program(
        [7, 5, ..unit_chain_tags(7, [8])],
        list.repeat(unit_chain_val(7, v_bytes(cbor.serialise(datum))), 10),
      ),
    ) == list_root(
      [7, 5, ..unit_chain_tags(7, [8])],
      list.repeat(unit_chain_data(7, datum), 10),
    ),
  }
}

/// Bits up to the end of a list constant's type tags, inside
/// `(program 1.0.0 [(builtin 0) (con ...)])`, which leaves an odd bit count.
fn list_head(tags: List<Int>) -> W {
  (0, 0)
    |> nat(1)
    |> nat(0)
    |> nat(0)
    |> put(3, 4)
    |> put(7, 4)
    |> put(0, 7)
    |> put(4, 4)
    |> type_tags(tags)
}

/// `bit_offset` is from the start of the Flat bytes; the record's is from
/// the start of the script, after the CBOR head `wrap` puts on.
fn bulk_refusal(flat: ByteArray, class: Int, bit_offset: Int) -> Bool {
  let head = bytearray.length(wrap(flat)) - bytearray.length(flat)
  bulk_record(flat) == refused(class, 8 * head + bit_offset)
}

test script_decode_bulk_refusals_keep_their_offsets() {
  // 59 bits of head, 50 bools, then a cons bit with no bool after it.
  let bools = list.foldl(
    list.repeat(True, 50),
    list_head([7, 5, 4]),
    fn(b, w) {
      put(
        put(w, 1, 1),
        if b {
          1
        } else {
          0
        },
        1,
      )
    },
  )
  let truncated_bools = put(bools, 1, 1)
  // 59 bits of head and 101 unit elements: the list never ends.
  let units = list.foldl(
    list.repeat(0, 101),
    list_head([7, 5, 3]),
    fn(_, w) { put(w, 1, 1) },
  )
  let strings = fn(bad: ByteArray) {
    let before = list.foldl(
      [#"61", #"c3a9", #""],
      list_head([7, 5, 2]),
      fn(s, w) { flat_bytes(put(w, 1, 1), s) },
    )
    let invalid = flat_bytes(put(before, 1, 1), bad)
    (
      invalid
        |> put(1, 1)
        |> flat_bytes(#"62")
        |> put(0, 1)
        |> finish,
      invalid.2nd,
    )
  }
  let (bad_utf8, bad_utf8_end) = strings(#"ff")
  let (bad_tail, bad_tail_end) = strings(#"61e282")
  let ints = list.foldl(
    list.range(1, 44),
    list_head([7, 5, 0]),
    fn(i, w) { int(put(w, 1, 1), i) },
  )
  and {
    bulk_refusal(to_bytes(truncated_bools), 3, truncated_bools.2nd),
    bulk_refusal(to_bytes(units), 3, units.2nd),
    bulk_refusal(bad_utf8, 15, bad_utf8_end),
    bulk_refusal(bad_tail, 15, bad_tail_end),
    // A truncated integer: its continuation group has no successor.
    bulk_refusal(to_bytes(put(put(ints, 1, 1), 255, 8)), 3, ints.2nd + 9),
    // A byte string whose filler ends past the byte boundary.
    bulk_refusal(
      finish(
        enc_value(
          put(
            list.foldl(
              list.range(1, 30),
              list_head([7, 5, 1]),
              fn(_, w) { flat_bytes(put(w, 1, 1), #"aa") },
            ),
            1,
            1,
          ),
          VMisaligned,
        ),
      ),
      4,
      {
        let before = put(
          list.foldl(
            list.range(1, 30),
            list_head([7, 5, 1]),
            fn(_, w) { flat_bytes(put(w, 1, 1), #"aa") },
          ),
          1,
          1,
        )
        before.2nd + 8 - before.2nd % 8 + 1
      },
    ),
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

// ## Agreement with the CEK constant root
//
// A decoded program is evaluated by the CEK machine, whose ComputeConstant
// step recomputes a constant's value root with `constant_root_v1` and needs
// the term the decoder committed to be the constant term over that root. The
// rows below cover every constant type a program may hold.

/// The witness ComputeConstant opens for a constant of CBOR type `type_tags`.
fn constant_witness(type_tags: List<Int>, payload: Data) -> ConstantWitnessV1 {
  ConstantWitnessV1 {
    type_cbor: cbor.serialise(type_tags),
    payload_cbor: cbor.serialise(payload),
  }
}

/// True when the CEK machine accepts ComputeConstant with `witness` from the
/// evaluation start of `term_root`.
fn computes(term_root: ByteArray, witness: ConstantWitnessV1) -> Bool {
  let pre = cek_machine_v1.initial_state_v1(0, term_root)
  let post = cek_machine_v1.exact_state(
    pre,
    cek_machine_v1.mode_return,
    cek_constant_v1.constant_root_v1(witness),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    16000,
    100,
  )
  cek_machine_v1.verify_core_step_v1(
    pre,
    post,
    cek_machine_v1.ComputeConstant { value: witness },
  )
}

/// The constant `(con tags value)`, decoded as a whole program by `decoded`,
/// commits the CEK constant term over `constant_root_v1` of `payload`, and an
/// honest ComputeConstant step over the decoded term is accepted.
fn agrees_with(
  decoded: fn(ByteArray) -> ByteArray,
  tags: List<Int>,
  value: Val,
  payload: Data,
) -> Bool {
  let witness = constant_witness(without_applications(tags), payload)
  let term_root = decoded(program(1, con(tags, value)))
  and {
    term_root == cek_proof_v1.hash_constant_term_v1(
      cek_constant_v1.constant_root_v1(witness),
    ),
    computes(term_root, witness),
  }
}

fn agrees(tags: List<Int>, value: Val, payload: Data) -> Bool {
  agrees_with(root_of, tags, value, payload)
}

/// `agrees`, also checked over walks of a few fuels (the bulk list path).
fn agrees_walked(tags: List<Int>, value: Val, payload: Data) -> Bool {
  agrees_with(bulk_root, tags, value, payload)
}

fn utf8_repeat(piece: ByteArray, count: Int) -> ByteArray {
  list.foldl(
    list.repeat(piece, count),
    #"",
    fn(item, acc) { bytearray.concat(acc, item) },
  )
}

fn data_of(d: Data) -> Val {
  v_bytes(cbor.serialise(d))
}

test script_decode_constant_root_agreement_scalars() {
  let ints = [
    0,
    1,
    -1,
    63,
    64,
    -64,
    -65,
    math.pow2(63) - 1,
    math.pow2(64) - 1,
    math.pow2(64),
    -math.pow2(64),
    -math.pow2(64) - 1,
    math.pow2(139),
    math.pow2(200),
    -math.pow2(300),
    math.pow2(511) + 12345,
    math.pow2(512) - 1,
    -math.pow2(512),
  ]
  let byte_strings = [
    #"",
    #"abcd",
    builtin.replicate_byte(64, 7),
    builtin.replicate_byte(65, 8),
    builtin.replicate_byte(700, 171),
    builtin.replicate_byte(2000, 3),
  ]
  let strings = [
    #"",
    #"616263",
    #"c3a9",
    #"e697a5e69cace8aa9e",
    #"f09f9880",
    utf8_repeat(#"e282ac", 85),
    utf8_repeat(#"c3a9", 300),
  ]
  and {
    list.all(ints, fn(n) { agrees([0], v_int(n), builtin.i_data(n)) }),
    list.all(byte_strings, fn(b) { agrees([1], v_bytes(b), builtin.b_data(b)) }),
    list.all(strings, fn(s) { agrees([2], v_bytes(s), builtin.b_data(s)) }),
    agrees([3], v_unit(), unit_data()),
    agrees([4], v_bool(True), bool_data(True)),
    agrees([4], v_bool(False), bool_data(False)),
  }
}

test script_decode_constant_root_agreement_data() {
  let nested = list.foldl(
    list.range(1, 20),
    builtin.i_data(0),
    fn(i, inner) {
      builtin.constr_data(i, [inner, builtin.list_data([builtin.i_data(i)])])
    },
  )
  let items = [
    builtin.i_data(0),
    builtin.i_data(-1000),
    builtin.i_data(math.pow2(64)),
    builtin.i_data(-math.pow2(300)),
    builtin.i_data(math.pow2(512) - 1),
    builtin.b_data(#""),
    builtin.b_data(builtin.replicate_byte(64, 7)),
    builtin.b_data(builtin.replicate_byte(1000, 9)),
    builtin.list_data([]),
    builtin.list_data(list.map(list.range(1, 300), builtin.i_data)),
    builtin.map_data([]),
    builtin.map_data(
      list.map(
        list.range(1, 40),
        fn(i) { Pair(builtin.i_data(i), builtin.b_data(#"aa")) },
      ),
    ),
    builtin.constr_data(0, []),
    builtin.constr_data(6, [builtin.i_data(1)]),
    builtin.constr_data(7, [builtin.i_data(1), builtin.b_data(#"ab")]),
    builtin.constr_data(127, []),
    builtin.constr_data(128, []),
    builtin.constr_data(200, [builtin.map_data([])]),
    builtin.constr_data(math.pow2(64) - 1, [builtin.list_data([])]),
    nested,
  ]
  list.all(items, fn(d) { agrees([8], data_of(d), d) })
}

test script_decode_constant_root_agreement_lists() {
  let ints = list.map(
    list.range(1, 200),
    fn(i) {
      if i % 40 == 0 {
        math.pow2(300) + i
      } else {
        i * 37 - 2000
      }
    },
  )
  let byte_strings = list.map(
    list.range(1, 120),
    fn(i) { builtin.replicate_byte(i % 70, i) },
  )
  let strings = list.map(
    list.range(1, 100),
    fn(i) { utf8_repeat(#"c3a9", i % 9) },
  )
  let flags = list.map(list.range(1, 600), fn(i) { i % 3 == 0 })
  let datum = builtin.list_data([builtin.i_data(1), builtin.b_data(#"aa")])
  let value = [(#"01", [(#"aa", 3), (#"bb", -4)])]
  and {
    agrees_walked(
      [7, 5, 0],
      v_list(list.map(ints, v_int)),
      builtin.list_data(list.map(ints, builtin.i_data)),
    ),
    agrees_walked(
      [7, 5, 1],
      v_list(list.map(byte_strings, v_bytes)),
      builtin.list_data(list.map(byte_strings, builtin.b_data)),
    ),
    agrees_walked(
      [7, 5, 2],
      v_list(list.map(strings, v_bytes)),
      builtin.list_data(list.map(strings, builtin.b_data)),
    ),
    agrees_walked(
      [7, 5, 3],
      v_list(list.repeat(v_unit(), 600)),
      builtin.list_data(list.repeat(unit_data(), 600)),
    ),
    agrees_walked(
      [7, 5, 4],
      v_list(list.map(flags, v_bool)),
      builtin.list_data(list.map(flags, bool_data)),
    ),
    agrees_walked(
      [7, 5, 8],
      v_list(list.repeat(data_of(datum), 30)),
      builtin.list_data(list.repeat(datum, 30)),
    ),
    agrees_walked(
      [7, 5, 13],
      v_list([v_value([]), v_value(value), v_value(value)]),
      builtin.list_data([value_data([]), value_data(value), value_data(value)]),
    ),
    // Pairs with a fixed-width first, second, or both components.
    agrees_walked(
      [7, 5, 7, 7, 6, 3, 0],
      v_list(list.map(ints, fn(n) { v_pair(v_unit(), v_int(n)) })),
      builtin.list_data(
        list.map(ints, fn(n) { pair_data(unit_data(), builtin.i_data(n)) }),
      ),
    ),
    agrees_walked(
      [7, 5, 7, 7, 6, 1, 3],
      v_list(list.map(byte_strings, fn(b) { v_pair(v_bytes(b), v_unit()) })),
      builtin.list_data(
        list.map(
          byte_strings,
          fn(b) { pair_data(builtin.b_data(b), unit_data()) },
        ),
      ),
    ),
    agrees_walked(
      [7, 5, 7, 7, 6, 7, 7, 6, 3, 4, 3],
      v_list(
        list.map(
          list.take(flags, 300),
          fn(b) { v_pair(v_pair(v_unit(), v_bool(b)), v_unit()) },
        ),
      ),
      builtin.list_data(
        list.map(
          list.take(flags, 300),
          fn(b) { pair_data(pair_data(unit_data(), bool_data(b)), unit_data()) },
        ),
      ),
    ),
    agrees_walked(
      [7, 5, 7, 7, 6, 0, 1],
      v_list(list.map(ints, fn(n) { v_pair(v_int(n), v_bytes(#"cafe")) })),
      builtin.list_data(
        list.map(
          ints,
          fn(n) { pair_data(builtin.i_data(n), builtin.b_data(#"cafe")) },
        ),
      ),
    ),
    // Nested lists.
    agrees_walked(
      [7, 5, 7, 5, 7, 5, 0],
      v_list(
        list.map(
          list.range(0, 30),
          fn(i) {
            v_list(list.repeat(v_list(list.repeat(v_int(i), i % 3)), i % 4))
          },
        ),
      ),
      builtin.list_data(
        list.map(
          list.range(0, 30),
          fn(i) {
            builtin.list_data(
              list.repeat(
                builtin.list_data(list.repeat(builtin.i_data(i), i % 3)),
                i % 4,
              ),
            )
          },
        ),
      ),
    ),
    agrees_walked(
      [7, 5, 7, 5, 3],
      v_list(
        list.map(
          list.range(0, 80),
          fn(i) { v_list(list.repeat(v_unit(), i % 5)) },
        ),
      ),
      builtin.list_data(
        list.map(
          list.range(0, 80),
          fn(i) { builtin.list_data(list.repeat(unit_data(), i % 5)) },
        ),
      ),
    ),
  }
}

test script_decode_constant_root_agreement_empty_lists() {
  let element_types = [
    [0],
    [1],
    [2],
    [3],
    [4],
    [8],
    [9],
    [10],
    [11],
    [13],
    [7, 5, 0],
    [7, 12, 3],
    [7, 7, 6, 9, 10],
  ]
  list.all(
    element_types,
    fn(element) {
      and {
        agrees([7, 5, ..element], v_list([]), builtin.list_data([])),
        agrees([7, 12, ..element], v_list([]), builtin.list_data([])),
      }
    },
  )
}

test script_decode_constant_root_agreement_pairs_arrays_and_values() {
  let datum = builtin.constr_data(1, [builtin.b_data(#"aa")])
  let value = [
    (#"01", [(#"", -5), (#"aa", math.pow2(127) - 1)]),
    (#"0101", [(#"00", -math.pow2(127))]),
    (builtin.replicate_byte(32, 255), [(builtin.replicate_byte(32, 1), 7)]),
  ]
  let many = list.map(list.range(1, 200), fn(i) { i * 7 - 300 })
  and {
    agrees(
      [7, 7, 6, 0, 1],
      v_pair(v_int(5), v_bytes(#"ab")),
      pair_data(builtin.i_data(5), builtin.b_data(#"ab")),
    ),
    agrees(
      [7, 7, 6, 3, 3],
      v_pair(v_unit(), v_unit()),
      pair_data(unit_data(), unit_data()),
    ),
    agrees(
      [7, 7, 6, 7, 7, 6, 0, 3, 7, 5, 4],
      v_pair(v_pair(v_int(-9), v_unit()), v_list([v_bool(True), v_bool(False)])),
      pair_data(
        pair_data(builtin.i_data(-9), unit_data()),
        builtin.list_data([bool_data(True), bool_data(False)]),
      ),
    ),
    agrees(
      [7, 7, 6, 8, 2],
      v_pair(data_of(datum), v_bytes(#"c3a9")),
      pair_data(datum, builtin.b_data(#"c3a9")),
    ),
    agrees(
      [7, 7, 6, 13, 7, 7, 6, 4, 1],
      v_pair(v_value(value), v_pair(v_bool(True), v_bytes(#"00"))),
      pair_data(
        value_data(value),
        pair_data(bool_data(True), builtin.b_data(#"00")),
      ),
    ),
    agrees_walked(
      [7, 12, 0],
      v_list(list.map(many, v_int)),
      builtin.list_data(list.map(many, builtin.i_data)),
    ),
    agrees_walked(
      [7, 12, 1],
      v_list([v_bytes(#""), v_bytes(builtin.replicate_byte(100, 1))]),
      builtin.list_data(
        [builtin.b_data(#""), builtin.b_data(builtin.replicate_byte(100, 1))],
      ),
    ),
    agrees_walked(
      [7, 12, 3],
      v_list(list.repeat(v_unit(), 300)),
      builtin.list_data(list.repeat(unit_data(), 300)),
    ),
    agrees_walked(
      [7, 12, 7, 7, 6, 3, 0],
      v_list(list.map(many, fn(n) { v_pair(v_unit(), v_int(n)) })),
      builtin.list_data(
        list.map(many, fn(n) { pair_data(unit_data(), builtin.i_data(n)) }),
      ),
    ),
    agrees_walked(
      [7, 12, 7, 5, 0],
      v_list([v_list([v_int(1), v_int(2)]), v_list([])]),
      builtin.list_data(
        [
          builtin.list_data([builtin.i_data(1), builtin.i_data(2)]),
          builtin.list_data([]),
        ],
      ),
    ),
    agrees_walked(
      [7, 12, 7, 12, 4],
      v_list([v_list([v_bool(True)]), v_list([])]),
      builtin.list_data(
        [builtin.list_data([bool_data(True)]), builtin.list_data([])],
      ),
    ),
    agrees_walked(
      [7, 12, 8],
      v_list([data_of(datum), data_of(builtin.i_data(3))]),
      builtin.list_data([datum, builtin.i_data(3)]),
    ),
    agrees_walked(
      [7, 12, 13],
      v_list([v_value(value), v_value([])]),
      builtin.list_data([value_data(value), value_data([])]),
    ),
    agrees([13], v_value([]), value_data([])),
    agrees([13], v_value([(#"", [(#"", 1)])]), value_data([(#"", [(#"", 1)])])),
    agrees([13], v_value(value), value_data(value)),
  }
}

/// A constant inside a term commits the same constant term.
test script_decode_constant_root_agreement_inside_a_term() {
  let witness = constant_witness([5, 0], builtin.list_data([builtin.i_data(7)]))
  root_of(program(1, app(lam(var(1)), con([7, 5, 0], v_list([v_int(7)]))))) == cek_proof_v1.hash_application_term_v1(
    cek_proof_v1.hash_lambda_term_v1(cek_proof_v1.hash_variable_term_v1(1)),
    cek_proof_v1.hash_constant_term_v1(
      cek_constant_v1.constant_root_v1(witness),
    ),
  )
}

/// ComputeConstant over a decoded constant refuses a witness of another
/// value or another type with the same payload.
test script_decode_compute_constant_refuses_another_constant() {
  let ints = root_of(program(1, con([7, 5, 0], v_list([v_int(1), v_int(2)]))))
  let text = root_of(program(1, con([2], v_bytes(#"616263"))))
  let payload = builtin.list_data([builtin.i_data(1), builtin.i_data(2)])
  and {
    computes(ints, constant_witness([5, 0], payload)),
    !computes(ints, constant_witness([12, 0], payload)),
    !computes(
      ints,
      constant_witness([5, 0], builtin.list_data([builtin.i_data(1)])),
    ),
    computes(text, constant_witness([2], builtin.b_data(#"616263"))),
    !computes(text, constant_witness([1], builtin.b_data(#"616263"))),
  }
}

// ## Cost curves

/// The Flat bits of `count` list elements, each the `width`-bit `pattern`
/// with its cons bit, and the closing nil bit, padded with zeros to whole
/// bytes: the window, which holds one zero byte more, and the count of Flat
/// bits.
fn run_window(pattern: Int, width: Int, count: Int) -> (ByteArray, Int) {
  let (acc, n) = put(
    list.foldl(
      list.repeat(pattern, count),
      (0, 0),
      fn(p, w) { put(w, p, width) },
    ),
    0,
    1,
  )
  let pad = ( 8 - n % 8 ) % 8
  let flat_bits = n + pad
  (
    builtin.integer_to_bytearray(
      True,
      flat_bits / 8 + 1,
      acc * math.pow2(pad + 8),
    ),
    flat_bits,
  )
}

/// `[()]` under a cons bit.
fn unit_list_element() -> (cek_constant_v1.ConstantTypeV1, Int, Int) {
  (cek_constant_v1.ListConstant { element: cek_constant_v1.UnitConstant }, 6, 3)
}

/// `([()], [()])` under a cons bit.
fn list_pair_element() -> (cek_constant_v1.ConstantTypeV1, Int, Int) {
  let l = cek_constant_v1.ListConstant { element: cek_constant_v1.UnitConstant }
  (cek_constant_v1.PairConstant { first: l, second: l }, 26, 5)
}

type RunFixture {
  width: Int,
  window: ByteArray,
  flat_bits: Int,
  element: cek_constant_v1.ConstantTypeV1,
  count: Int,
}

fn run_fixture(
  element: (cek_constant_v1.ConstantTypeV1, Int, Int),
  count: Int,
) -> RunFixture {
  let (ty, pattern, width) = element
  let (window, flat_bits) = run_window(pattern, width, count)
  RunFixture { width, window, flat_bits, element: ty, count }
}

fn sample_unit_list_run(size: Int) -> Fuzzer<RunFixture> {
  fuzz.constant(run_fixture(unit_list_element(), 3 * size))
}

fn sample_list_pair_run(size: Int) -> Fuzzer<RunFixture> {
  fuzz.constant(run_fixture(list_pair_element(), size))
}

/// The position of the run's nil bit.
fn run_end(fixture: RunFixture) -> Int {
  fixture.count * fixture.width
}

fn scans_whole_run(fixture: RunFixture) -> Bool {
  let RunFixture { window, flat_bits, element, count, .. } = fixture
  let (scanned, nil, _, _) = bulk.scan_run_v1(
    window,
    flat_bits,
    bulk.scan_decoder_v1(window, flat_bits, element),
    0,
  )
  scanned == count && nil == run_end(fixture)
}

fn folds_whole_run(fixture: RunFixture) -> Bool {
  let RunFixture { window, flat_bits, element, count, .. } = fixture
  bulk.fold_run_v1(
    bulk.fold_decoder_v1(window, flat_bits, element),
    0,
    count,
    cek_data_v1.empty_data_list_summary_v1(),
  ).length == count
}

/// The bulk scan of a run of `[()]` elements over the run's length.
bench script_decode_scan_run_of_unit_lists(fixture via sample_unit_list_run) {
  scans_whole_run(fixture)
}

/// The bulk scan of a run of `([()], [()])` elements over the run's length.
bench script_decode_scan_run_of_list_pairs(fixture via sample_list_pair_run) {
  scans_whole_run(fixture)
}

/// The bulk fold of a run of `[()]` elements over the run's length.
bench script_decode_fold_run_of_unit_lists(fixture via sample_unit_list_run) {
  folds_whole_run(fixture)
}

/// The bulk fold of a run of `([()], [()])` elements over the run's length.
bench script_decode_fold_run_of_list_pairs(fixture via sample_list_pair_run) {
  folds_whole_run(fixture)
}

/// A script of a list of 600 six-pair unit chains over `[()]`, the shape whose
/// ticks cost most, and a tick count.
fn sample_unit_chain_ticks(size: Int) -> Fuzzer<(ByteArray, Int)> {
  let tags = [7, 5, ..unit_chain_tags(6, [7, 5, 3])]
  let item = unit_chain_val(6, v_list([v_unit()]))
  fuzz.constant((wrap(list_program(tags, list.repeat(item, 600))), size + 1))
}

/// A script of a list of 600 `([()], [()])` pairs and a tick count.
fn sample_list_pair_ticks(size: Int) -> Fuzzer<(ByteArray, Int)> {
  let tags = [7, 5, 7, 7, 6, 7, 5, 3, 7, 5, 3]
  let item = v_pair(v_list([v_unit()]), v_list([v_unit()]))
  fuzz.constant((wrap(list_program(tags, list.repeat(item, 600))), size + 1))
}

fn steps(fixture: (ByteArray, Int)) -> Bool {
  let (script, fuel) = fixture
  let run = initial_run_v1(#"aa", 3, script)
  let env = decode_env_v1(script, run)
  step_v1(env, run.machine, [], run.machine.stack_root, fuel) != run.machine
}

/// One step of the script decoder over its tick count.
bench script_decode_ticks_of_unit_chain_list(
  fixture via sample_unit_chain_ticks,
) {
  steps(fixture)
}

/// One step of the script decoder over its tick count.
bench script_decode_ticks_of_list_pair_list(fixture via sample_list_pair_ticks) {
  steps(fixture)
}

/// The Data constant of `depth` nested one-item lists over an integer.
fn nested_data(depth: Int) -> Data {
  if depth == 0 {
    builtin.i_data(1)
  } else {
    builtin.list_data([nested_data(depth - 1)])
  }
}

fn sample_nested_data(size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(wrap(data_flat(cbor.serialise(nested_data(8 * size)))))
}

/// The whole decode of a Data constant over its nesting depth.
bench script_decode_nested_data(script via sample_nested_data) {
  when run_script(script) is {
    Decoded { .. } -> True
    _ -> False
  }
}
