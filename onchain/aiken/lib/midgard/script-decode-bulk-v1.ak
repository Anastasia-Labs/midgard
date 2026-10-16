//// Bulk decoding of constant values for the script decode walk
//// (`script_decode_v1`).
////
//// A value whose bits allow it is decoded here in one call, with no machine
//// transition per node: a run of list elements, or a pair component. Any
//// value this module stops on is left to the walk's one-node ticks, which
//// decode it and raise every refusal, so no refusal class or offset comes
//// from here. A stop is never an error: the walk resumes at the same bit.
////
//// Work is metered in units. Each charge is sized so that a unit of folding
//// (decoding with summaries) costs about the same at every element type, and
//// a call stops before it runs out of units, so one tick does bounded work at
//// every element type.
////
//// A decoded value travels as its entry: its root followed by its canonical
//// CBOR length and its Data memory as `serialise_data` writes them, which is
//// how a list node commits its head. Its CBOR length, Data memory and
//// constant memory travel beside it as integers. Summaries are
//// byte-identical to the `cek_data_v1` functions named on each helper.

use aiken/builtin
use aiken/crypto.{blake2b_256}
use midgard/cek_constant_v1.{
  ArrayConstant, BooleanConstant, ByteStringConstant, ConstantTypeV1,
  IntegerConstant, ListConstant, PairConstant, StringConstant, UnitConstant,
  ValueConstant,
}
use midgard/cek_data_v1.{DataSequenceSummaryV1, DataSummaryV1}
use midgard/script_decode_bits_v1 as bits

/// The units one tick may spend on bulk decoding.
pub const units_per_tick = 512

/// Units charged per list element and per pair decoded component by
/// component.
const element_units = 2

pub const pair_units = 3

/// More units than any run spends: folding re-decodes a run that a scan
/// already accepted, so it is never metered.
const unmetered = 1000000000

/// The most leaves a pair's bit table may have.
const max_leaves = 4

// ## Summaries

fn ser(value: Int) -> ByteArray {
  builtin.serialise_data(builtin.i_data(value))
}

/// The list-node domain, the 7-field array head and the head-root byte
/// string head.
const list_node_head = #"4d69646761726443656b446174614c6973744e6f64655631875820"

/// The data-node domain, then a small constructor node's head up to its
/// fields root: 6 fields, constructor 0, 2 fields, a 32-byte string.
const pair_node_head = #"4d69646761726443656b446174614e6f64655631860000025820"

/// The data-node domain, then a list node's head: 5 fields, tag 3.
const list_data_head = #"4d69646761726443656b446174614e6f646556318503"

/// The tail of a one-item list node: its empty tail and length 1.
const last_item_tail: ByteArray =
  builtin.append_bytearray(
    #"5820",
    builtin.append_bytearray(cek_data_v1.empty_data_list_root_v1(), #"01"),
  )

const empty_list_root: ByteArray = cek_data_v1.empty_data_list_root_v1()

/// The entry of a summary.
pub fn entry_of_v1(summary: DataSummaryV1) -> ByteArray {
  builtin.append_bytearray(
    summary.root,
    builtin.append_bytearray(ser(summary.cbor_length), ser(summary.memory)),
  )
}

/// The summary of an entry with its CBOR length and Data memory.
pub fn summary_of_v1(entry: ByteArray, cbor: Int, data: Int) -> DataSummaryV1 {
  DataSummaryV1 {
    root: builtin.slice_bytearray(0, 32, entry),
    cbor_length: cbor,
    memory: data,
  }
}

/// The root of a list node: the head's entry over a tail root, where
/// `length`, `payload` and `data` already count the head.
fn prepend_root(
  head: ByteArray,
  tail: ByteArray,
  length: Int,
  payload: Int,
  data: Int,
) -> ByteArray {
  blake2b_256(
    builtin.append_bytearray(
      list_node_head,
      builtin.append_bytearray(
        head,
        builtin.append_bytearray(
          #"5820",
          builtin.append_bytearray(
            tail,
            builtin.append_bytearray(
              ser(length),
              builtin.append_bytearray(ser(payload), ser(data)),
            ),
          ),
        ),
      ),
    ),
  )
}

/// `cek_data_v1.prepend_data_list_summary_v1`.
pub fn prepend_v1(
  head: DataSummaryV1,
  tail: DataSequenceSummaryV1,
) -> DataSequenceSummaryV1 {
  let length = tail.length + 1
  let payload = head.cbor_length + tail.payload_cbor_length
  let memory = head.memory + tail.memory
  DataSequenceSummaryV1 {
    root: prepend_root(entry_of_v1(head), tail.root, length, payload, memory),
    length,
    payload_cbor_length: payload,
    memory,
  }
}

/// The entry of the pair of the values with entries `a` and `b`, whose CBOR
/// lengths sum to `payload` and whose Data memories sum to `data`: the
/// pair's CBOR length is `4 + payload` and its Data memory `4 + data`. This
/// is `cek_data_v1.small_constr_data_summary_v1` of constructor 0 over the
/// list `[a, b]`.
pub fn pair_entry_v1(
  a: ByteArray,
  b: ByteArray,
  payload: Int,
  data: Int,
) -> ByteArray {
  let inner = blake2b_256(
    builtin.append_bytearray(
      list_node_head,
      builtin.append_bytearray(
        b,
        builtin.append_bytearray(
          last_item_tail,
          builtin.slice_bytearray(32, 64, b),
        ),
      ),
    ),
  )
  let outer = blake2b_256(
    builtin.append_bytearray(
      list_node_head,
      builtin.append_bytearray(
        a,
        builtin.append_bytearray(
          #"5820",
          builtin.append_bytearray(
            inner,
            builtin.append_bytearray(
              #"02",
              builtin.append_bytearray(ser(payload), ser(data)),
            ),
          ),
        ),
      ),
    ),
  )
  let sizes = builtin.append_bytearray(ser(4 + payload), ser(4 + data))
  builtin.append_bytearray(
    blake2b_256(
      builtin.append_bytearray(
        pair_node_head,
        builtin.append_bytearray(outer, sizes),
      ),
    ),
    sizes,
  )
}

/// The summary of the pair `(a, b)`.
pub fn pair_summary_v1(a: DataSummaryV1, b: DataSummaryV1) -> DataSummaryV1 {
  let payload = a.cbor_length + b.cbor_length
  let data = a.memory + b.memory
  summary_of_v1(
    pair_entry_v1(entry_of_v1(a), entry_of_v1(b), payload, data),
    4 + payload,
    4 + data,
  )
}

/// The entry of a list Data value over a sequence with root `root` of
/// `length` items whose CBOR lengths sum to `payload` and Data memories to
/// `data`: `cek_data_v1.list_data_summary_v1`. Its CBOR length is
/// `list_cbor_length(length, payload)` and its Data memory `4 + data`.
fn list_entry(
  root: ByteArray,
  length: Int,
  payload: Int,
  data: Int,
) -> ByteArray {
  let sizes = builtin.append_bytearray(
    ser(list_cbor_length(length, payload)),
    ser(4 + data),
  )
  builtin.append_bytearray(
    blake2b_256(
      builtin.append_bytearray(
        list_data_head,
        builtin.append_bytearray(
          ser(length),
          builtin.append_bytearray(
            #"5820",
            builtin.append_bytearray(root, sizes),
          ),
        ),
      ),
    ),
    sizes,
  )
}

fn list_cbor_length(length: Int, payload: Int) -> Int {
  if length == 0 {
    1
  } else {
    2 + payload
  }
}

/// `cek_data_v1.list_data_summary_v1`.
pub fn list_data_summary_v1(items: DataSequenceSummaryV1) -> DataSummaryV1 {
  summary_of_v1(
    list_entry(
      items.root,
      items.length,
      items.payload_cbor_length,
      items.memory,
    ),
    list_cbor_length(items.length, items.payload_cbor_length),
    4 + items.memory,
  )
}

const empty_sequence: DataSequenceSummaryV1 =
  cek_data_v1.empty_data_list_summary_v1()

const unit_value: DataSummaryV1 =
  cek_data_v1.small_constr_data_summary_v1(0, empty_sequence)

const true_value: DataSummaryV1 =
  cek_data_v1.small_constr_data_summary_v1(1, empty_sequence)

const empty_list_value: DataSummaryV1 =
  cek_data_v1.list_data_summary_v1(empty_sequence)

const empty_value_value: DataSummaryV1 =
  cek_data_v1.map_data_summary_v1(cek_data_v1.empty_data_pair_summary_v1())

const empty_bytes_value: DataSummaryV1 =
  bits.bytes_summary(0, bits.sink_root(bits.empty_sink()))

const unit_entry: ByteArray = entry_of_v1(unit_value)

const true_entry: ByteArray = entry_of_v1(true_value)

const empty_list_entry: ByteArray = entry_of_v1(empty_list_value)

const empty_value_entry: ByteArray = entry_of_v1(empty_value_value)

const empty_bytes_entry: ByteArray = entry_of_v1(empty_bytes_value)

// ## Shapes

/// A constant type as the bulk decoder reads it.
pub type Shape {
  /// A value its bits so far fix: its entry, CBOR length, Data memory and
  /// constant memory.
  SFixed { entry: ByteArray, cbor: Int, data: Int, memory: Int }
  /// Reads one bit and goes on with `zero` or `one`.
  SBit { zero: Shape, one: Shape }
  /// Decodes `kind` from the bit where the shape began.
  SGen { kind: Kind }
}

pub type Kind {
  KInteger
  KBytes
  KString
  KList { element: Shape, array: Bool }
  /// `stops` when a component never decodes here.
  KPair { first: Shape, second: Shape, stops: Bool }
  /// A value of `inner` as a component of pairs whose other component no
  /// bits encode, the innermost pair first. The pairs are charged `units`
  /// and add `memory`. Without summaries `layers` is empty.
  KWrap { inner: Kind, layers: List<Layer>, units: Int, memory: Int }
  KStop
}

/// The other component of a pair a value is wrapped in.
pub type Layer {
  /// The other component comes first.
  LFirst { entry: ByteArray, cbor: Int, data: Int }
  /// The other component comes second.
  LSecond { entry: ByteArray, cbor: Int, data: Int }
}

fn fixed(summary: DataSummaryV1, entry: ByteArray, memory: Int, summaries: Bool) {
  if summaries {
    SFixed { entry, cbor: summary.cbor_length, data: summary.memory, memory }
  } else {
    SFixed { entry: #"", cbor: 0, data: 0, memory }
  }
}

/// The shape of `ty`. Without `summaries` the fixed values carry
/// placeholder entries and sizes, which only a scan, reading constant memory
/// alone, may use.
pub fn shape_v1(ty: ConstantTypeV1, summaries: Bool) -> Shape {
  when ty is {
    UnitConstant -> fixed(unit_value, unit_entry, 1, summaries)
    BooleanConstant ->
      SBit {
        zero: fixed(unit_value, unit_entry, 1, summaries),
        one: fixed(true_value, true_entry, 1, summaries),
      }
    IntegerConstant -> SGen { kind: KInteger }
    ByteStringConstant -> SGen { kind: KBytes }
    StringConstant -> SGen { kind: KString }
    ListConstant { element } ->
      SBit {
        zero: fixed(empty_list_value, empty_list_entry, 0, summaries),
        one: SGen {
          kind: KList { element: shape_v1(element, summaries), array: False },
        },
      }
    ArrayConstant { element } ->
      SBit {
        zero: fixed(empty_list_value, empty_list_entry, 0, summaries),
        one: SGen {
          kind: KList { element: shape_v1(element, summaries), array: True },
        },
      }
    ValueConstant ->
      SBit {
        zero: fixed(empty_value_value, empty_value_entry, 0, summaries),
        one: SGen { kind: KStop },
      }
    PairConstant { first, second } ->
      pair_shape(
        shape_v1(first, summaries),
        shape_v1(second, summaries),
        summaries,
      )
    _ -> SGen { kind: KStop }
  }
}

/// No value of the shape decodes here.
fn never(shape: Shape) -> Bool {
  when shape is {
    SGen { kind: KStop } -> True
    SGen{kind: KPair { stops, .. }} -> stops
    _ -> False
  }
}

fn leaves(shape: Shape) -> Int {
  when shape is {
    SBit { zero, one } -> leaves(zero) + leaves(one)
    _ -> 1
  }
}

fn has_fixed(shape: Shape) -> Bool {
  when shape is {
    SFixed { .. } -> True
    SBit { zero, one } -> has_fixed(zero) || has_fixed(one)
    SGen { .. } -> False
  }
}

/// The shape of a pair: the other component's table wrapped in the pair when
/// one component is fixed and the other can decode here, a bit table over its
/// components' tables when both have a fixed leaf and the table stays small,
/// and otherwise decoded component by component.
fn pair_shape(a: Shape, b: Shape, summaries: Bool) -> Shape {
  when a is {
    SFixed { entry, cbor, data, memory } ->
      if never(b) {
        SGen { kind: KPair { first: a, second: b, stops: True } }
      } else {
        wrap_shape(b, LFirst { entry, cbor, data }, memory, summaries)
      }
    _ ->
      when b is {
        SFixed { entry, cbor, data, memory } ->
          if never(a) {
            SGen { kind: KPair { first: a, second: b, stops: True } }
          } else {
            wrap_shape(a, LSecond { entry, cbor, data }, memory, summaries)
          }
        _ -> {
          let general = SGen {
            kind: KPair { first: a, second: b, stops: never(a) || never(b) },
          }
          if has_fixed(a) && has_fixed(b) && leaves(a) * leaves(b) <= max_leaves {
            graft_first(a, b, general, summaries)
          } else {
            general
          }
        }
      }
  }
}

/// The shape of a pair of a value of `shape` and a value no bits encode,
/// given as `layer` and its constant `memory`: the same bit table, each
/// leaf wrapped in the pair.
fn wrap_shape(shape: Shape, layer: Layer, memory: Int, summaries: Bool) -> Shape {
  when shape is {
    SFixed { entry, cbor, data, memory: inner_memory } ->
      if summaries {
        let (entry, cbor, data) = wrap_entry(layer, entry, cbor, data)
        SFixed { entry, cbor, data, memory: inner_memory + memory }
      } else {
        SFixed { entry: #"", cbor: 0, data: 0, memory: inner_memory + memory }
      }
    SBit { zero, one } ->
      SBit {
        zero: wrap_shape(zero, layer, memory, summaries),
        one: wrap_shape(one, layer, memory, summaries),
      }
    SGen { kind } ->
      SGen {
        kind: when kind is {
          KStop -> kind
          KPair { stops: True, .. } -> kind
          KWrap { inner, layers, units, memory: inner_memory } ->
            KWrap {
              inner,
              layers: if summaries {
                append_layer(layers, layer)
              } else {
                []
              },
              units: units + pair_units,
              memory: inner_memory + memory,
            }
          _ ->
            KWrap {
              inner: kind,
              layers: if summaries {
                [layer]
              } else {
                []
              },
              units: pair_units,
              memory,
            }
        },
      }
  }
}

fn append_layer(layers: List<Layer>, layer: Layer) -> List<Layer> {
  when layers is {
    [] -> [layer]
    [first, ..rest] -> [first, ..append_layer(rest, layer)]
  }
}

/// The entry, CBOR length and Data memory of the pair `layer` wraps a value
/// with this entry, CBOR length and Data memory in.
fn wrap_entry(
  layer: Layer,
  entry: ByteArray,
  cbor: Int,
  data: Int,
) -> (ByteArray, Int, Int) {
  when layer is {
    LFirst { entry: other, cbor: other_cbor, data: other_data } -> {
      let payload = other_cbor + cbor
      let data = other_data + data
      (pair_entry_v1(other, entry, payload, data), 4 + payload, 4 + data)
    }
    LSecond { entry: other, cbor: other_cbor, data: other_data } -> {
      let payload = cbor + other_cbor
      let data = data + other_data
      (pair_entry_v1(entry, other, payload, data), 4 + payload, 4 + data)
    }
  }
}

/// Wraps a decoded value's entry in `layers`, innermost first.
fn wrap_layers(
  layers: List<Layer>,
  entry: ByteArray,
  cbor: Int,
  data: Int,
  k: fn(ByteArray, Int, Int) -> r,
) -> r {
  when layers is {
    [] -> k(entry, cbor, data)
    [layer, ..rest] ->
      when layer is {
        LFirst { entry: other, cbor: other_cbor, data: other_data } -> {
          let payload = other_cbor + cbor
          let data = other_data + data
          wrap_layers(
            rest,
            pair_entry_v1(other, entry, payload, data),
            4 + payload,
            4 + data,
            k,
          )
        }
        LSecond { entry: other, cbor: other_cbor, data: other_data } -> {
          let payload = cbor + other_cbor
          let data = data + other_data
          wrap_layers(
            rest,
            pair_entry_v1(entry, other, payload, data),
            4 + payload,
            4 + data,
            k,
          )
        }
      }
  }
}

fn graft_first(a: Shape, b: Shape, general: Shape, summaries: Bool) -> Shape {
  when a is {
    SFixed { .. } -> graft_second(a, b, general, summaries)
    SBit { zero, one } ->
      SBit {
        zero: graft_first(zero, b, general, summaries),
        one: graft_first(one, b, general, summaries),
      }
    SGen { .. } -> general
  }
}

fn graft_second(a: Shape, b: Shape, general: Shape, summaries: Bool) -> Shape {
  when b is {
    SFixed { entry: b_entry, cbor: b_cbor, data: b_data, memory: b_memory } -> {
      expect
          SFixed {
            entry: a_entry,
            cbor: a_cbor,
            data: a_data,
            memory: a_memory,
          }
        = a
      if summaries {
        let payload = a_cbor + b_cbor
        let data = a_data + b_data
        SFixed {
          entry: pair_entry_v1(a_entry, b_entry, payload, data),
          cbor: 4 + payload,
          data: 4 + data,
          memory: a_memory + b_memory,
        }
      } else {
        SFixed { entry: #"", cbor: 0, data: 0, memory: a_memory + b_memory }
      }
    }
    SBit { zero, one } ->
      SBit {
        zero: graft_second(a, zero, general, summaries),
        one: graft_second(a, one, general, summaries),
      }
    SGen { .. } -> general
  }
}

// ## Decoding

/// The bit at `position`, below `flat_bits`, of a window holding the Flat
/// bytes and one zero byte after them.
fn bit(window: ByteArray, flat_bits: Int, position: Int) -> Bool {
  builtin.read_bit(window, flat_bits + 7 - position)
}

/// The byte starting at bit `position`.
fn byte_at(window: ByteArray, position: Int) -> Int {
  let index = position / 8
  let word = builtin.index_bytearray(window, index) * 256 + builtin.index_bytearray(
    window,
    index + 1,
  )
  word * builtin.index_bytearray(#[1, 2, 4, 8, 16, 32, 64, 128], position % 8) / 256 % 256
}

/// A compiled decoder of one value: from the bit where the value began, the
/// current bit (the same bit outside a shape's bit table) and the units
/// left, it goes to `ok` with the position after the value, its entry, CBOR
/// length, Data memory and constant memory (the first three placeholders
/// without summaries) and the units left, never negative; or to `stop` with
/// the units left when the value is not decoded here.
pub type Decoder<r> =
  fn(Int, Int, Int, fn(Int, ByteArray, Int, Int, Int, Int) -> r, fn(Int) -> r) ->
    r

/// Builds a decoder from two compiled ones. Every compiled decoder is built
/// through this call so that its parts are compiled once, when it is: a
/// part bound before a closure is otherwise moved into it by the compiler
/// and compiled again on every run.
fn join(
  a: Decoder<r>,
  b: Decoder<r>,
  build: fn(Decoder<r>, Decoder<r>) -> Decoder<r>,
) -> Decoder<r> {
  build(a, b)
}

fn stopped(
  _origin: Int,
  _position: Int,
  units: Int,
  _ok: fn(Int, ByteArray, Int, Int, Int, Int) -> r,
  stop: fn(Int) -> r,
) -> r {
  stop(units)
}

/// Compiles `shape` into a decoder over the window. A shape is compiled once
/// per tick and its decoder run on every value the tick decodes.
pub fn compile_v1(
  window: ByteArray,
  flat_bits: Int,
  shape: Shape,
  summaries: Bool,
) -> Decoder<r> {
  join(
    compile_node(window, flat_bits, shape, summaries),
    stopped,
    fn(node, _) {
      fn(origin, position, units, ok, stop) {
        if units < 0 {
          stop(units)
        } else {
          node(origin, position, units, ok, stop)
        }
      }
    },
  )
}

fn compile_node(
  window: ByteArray,
  flat_bits: Int,
  shape: Shape,
  summaries: Bool,
) -> Decoder<r> {
  when shape is {
    SFixed { entry, cbor, data, memory } ->
      join(
        stopped,
        stopped,
        fn(_, _) {
          fn(_origin, position, units, ok, _stop) {
            ok(position, entry, cbor, data, memory, units)
          }
        },
      )
    SBit { zero, one } ->
      join(
        compile_node(window, flat_bits, zero, summaries),
        compile_node(window, flat_bits, one, summaries),
        fn(zero, one) {
          fn(origin, position, units, ok, stop) {
            if position >= flat_bits {
              stop(units)
            } else if bit(window, flat_bits, position) {
              one(origin, position + 1, units, ok, stop)
            } else {
              zero(origin, position + 1, units, ok, stop)
            }
          }
        },
      )
    SGen { kind } -> compile_kind_v1(window, flat_bits, kind, summaries)
  }
}

/// Compiles a decoder of a value of `kind` at the bit where it begins.
pub fn compile_kind_v1(
  window: ByteArray,
  flat_bits: Int,
  kind: Kind,
  summaries: Bool,
) -> Decoder<r> {
  when kind is {
    KInteger ->
      fn(origin, _position, units, ok, stop) {
        natural(
          window,
          flat_bits,
          origin,
          0,
          0,
          1,
          fn(after, groups, acc) {
            let left = units - 5 - groups
            if left < 0 {
              stop(left)
            } else {
              let n = zigzag(acc)
              let memory = cek_data_v1.integer_memory_size_v1(n)
              if summaries {
                let summary = bits.integer_summary(n)
                ok(
                  after,
                  entry_of_v1(summary),
                  summary.cbor_length,
                  summary.memory,
                  memory,
                  left,
                )
              } else {
                ok(after, #"", 0, 0, memory, left)
              }
            }
          },
          fn() { stop(units) },
        )
      }
    KBytes ->
      fn(origin, _position, units, ok, stop) {
        byte_string(
          window,
          flat_bits,
          origin,
          False,
          units,
          summaries,
          ok,
          stop,
        )
      }
    KString ->
      fn(origin, _position, units, ok, stop) {
        byte_string(window, flat_bits, origin, True, units, summaries, ok, stop)
      }
    KList { element, array } ->
      join(
        compile_v1(window, flat_bits, element, summaries),
        stopped,
        fn(element, _) {
          fn(origin, _position, units, ok, stop) {
            items(
              window,
              flat_bits,
              element,
              origin,
              units - 1,
              summaries,
              fn(after, root, length, payload, data, memory, left) {
                ok(
                  after,
                  if summaries {
                    list_entry(root, length, payload, data)
                  } else {
                    #""
                  },
                  list_cbor_length(length, payload),
                  4 + data,
                  if array {
                    length
                  } else {
                    memory
                  },
                  left,
                )
              },
              stop,
            )
          }
        },
      )
    KPair { first, second, stops } ->
      if stops {
        stopped
      } else {
        join(
          compile_v1(window, flat_bits, first, summaries),
          compile_v1(window, flat_bits, second, summaries),
          fn(first, second) {
            if summaries {
              fn(origin, _position, units, ok, stop) {
                first(
                  origin,
                  origin,
                  units - pair_units,
                  fn(middle, a, a_cbor, a_data, a_memory, left) {
                    second(
                      middle,
                      middle,
                      left,
                      fn(after, b, b_cbor, b_data, b_memory, rest) {
                        let payload = a_cbor + b_cbor
                        let data = a_data + b_data
                        ok(
                          after,
                          pair_entry_v1(a, b, payload, data),
                          4 + payload,
                          4 + data,
                          a_memory + b_memory,
                          rest,
                        )
                      },
                      stop,
                    )
                  },
                  stop,
                )
              }
            } else {
              fn(origin, _position, units, ok, stop) {
                first(
                  origin,
                  origin,
                  units - pair_units,
                  fn(middle, _a, _a_cbor, _a_data, a_memory, left) {
                    second(
                      middle,
                      middle,
                      left,
                      fn(after, _b, _b_cbor, _b_data, b_memory, rest) {
                        ok(after, #"", 0, 0, a_memory + b_memory, rest)
                      },
                      stop,
                    )
                  },
                  stop,
                )
              }
            }
          },
        )
      }
    KWrap { inner, layers, units: layer_units, memory: layer_memory } ->
      join(
        compile_kind_v1(window, flat_bits, inner, summaries),
        stopped,
        fn(inner, _) {
          if summaries {
            fn(origin, position, units, ok, stop) {
              inner(
                origin,
                position,
                units - layer_units,
                fn(after, entry, cbor, data, memory, left) {
                  wrap_layers(
                    layers,
                    entry,
                    cbor,
                    data,
                    fn(entry, cbor, data) {
                      ok(after, entry, cbor, data, memory + layer_memory, left)
                    },
                  )
                },
                stop,
              )
            }
          } else {
            fn(origin, position, units, ok, stop) {
              inner(
                origin,
                position,
                units - layer_units,
                fn(after, _entry, _cbor, _data, memory, left) {
                  ok(after, #"", 0, 0, memory + layer_memory, left)
                },
                stop,
              )
            }
          }
        },
      )
    KStop -> stopped
  }
}

/// Follows the bits of `shape` from `position` to a leaf: `fixed` gets the
/// position after the bits read and the leaf's entry, CBOR length, Data
/// memory and constant memory; `general` gets the kind of a general leaf;
/// `truncated` is taken when the Flat bits end first.
pub fn leaf_v1(
  window: ByteArray,
  flat_bits: Int,
  shape: Shape,
  position: Int,
  fixed: fn(Int, ByteArray, Int, Int, Int) -> r,
  general: fn(Kind) -> r,
  truncated: fn() -> r,
) -> r {
  when shape is {
    SFixed { entry, cbor, data, memory } ->
      fixed(position, entry, cbor, data, memory)
    SBit { zero, one } ->
      if position >= flat_bits {
        truncated()
      } else if bit(window, flat_bits, position) {
        leaf_v1(window, flat_bits, one, position + 1, fixed, general, truncated)
      } else {
        leaf_v1(
          window,
          flat_bits,
          zero,
          position + 1,
          fixed,
          general,
          truncated,
        )
      }
    SGen { kind } -> general(kind)
  }
}

fn zigzag(z: Int) -> Int {
  if z % 2 == 0 {
    z / 2
  } else {
    -(z / 2) - 1
  }
}

/// A Flat natural of at most 20 groups, the ones the walk reads exactly.
fn natural(
  window: ByteArray,
  flat_bits: Int,
  position: Int,
  groups: Int,
  acc: Int,
  scale: Int,
  ok: fn(Int, Int, Int) -> r,
  stop: fn() -> r,
) -> r {
  if groups >= 20 || position + 8 > flat_bits {
    stop()
  } else {
    let v = byte_at(window, position)
    let acc = acc + v % 128 * scale
    if v >= 128 {
      natural(
        window,
        flat_bits,
        position + 8,
        groups + 1,
        acc,
        scale * 128,
        ok,
        stop,
      )
    } else {
      ok(position + 8, groups + 1, acc)
    }
  }
}

/// A byte string or string of at most one chunk: the filler, the chunk and
/// its empty successor.
fn byte_string(
  window: ByteArray,
  flat_bits: Int,
  position: Int,
  string: Bool,
  units: Int,
  summaries: Bool,
  ok: fn(Int, ByteArray, Int, Int, Int, Int) -> r,
  stop: fn(Int) -> r,
) -> r {
  let flat_length = flat_bits / 8
  when bits.next_set_bit(window, position, flat_length) is {
    None -> stop(units)
    Some(q) ->
      if ( q + 1 ) % 8 != 0 {
        stop(units)
      } else {
        let index = ( q + 1 ) / 8
        if index >= flat_length {
          stop(units)
        } else {
          let n = builtin.index_bytearray(window, index)
          if n == 0 {
            let left = units - 2
            if left < 0 {
              stop(left)
            } else {
              ok(
                q + 9,
                empty_bytes_entry,
                empty_bytes_value.cbor_length,
                empty_bytes_value.memory,
                if string {
                  0
                } else {
                  1
                },
                left,
              )
            }
          } else {
            let left = units - 10 - if string {
              n
            } else {
              n / 32
            }
            if left < 0 {
              stop(left)
            } else if index + 2 + n > flat_length {
              stop(left)
            } else if builtin.index_bytearray(window, index + 1 + n) != 0 {
              stop(left)
            } else {
              let chunk = builtin.slice_bytearray(index + 1, n, window)
              if string && bits.utf8_push(0, chunk) != 0 {
                stop(left)
              } else {
                let memory = if string {
                  n / 4
                } else {
                  cek_data_v1.bytes_memory_size_v1(n)
                }
                if summaries {
                  let summary = bits.short_bytes_summary(chunk)
                  ok(
                    q + 1 + 8 * ( n + 2 ),
                    entry_of_v1(summary),
                    summary.cbor_length,
                    summary.memory,
                    memory,
                    left,
                  )
                } else {
                  ok(q + 1 + 8 * ( n + 2 ), #"", 0, 0, memory, left)
                }
              }
            }
          }
        }
      }
  }
}

/// The elements of a list from the cons bit at `position` to its end:
/// `ok` gets the position after the end, the root of their sequence, their
/// count, the sums of their CBOR lengths, Data memories and constant
/// memories, and the units left.
fn items(
  window: ByteArray,
  flat_bits: Int,
  element: Decoder<r>,
  position: Int,
  units: Int,
  summaries: Bool,
  ok: fn(Int, ByteArray, Int, Int, Int, Int, Int) -> r,
  stop: fn(Int) -> r,
) -> r {
  if units < 0 {
    stop(units)
  } else if position >= flat_bits {
    stop(units)
  } else if bit(window, flat_bits, position) {
    element(
      position + 1,
      position + 1,
      units - element_units,
      fn(middle, head, cbor, data, memory, left) {
        items(
          window,
          flat_bits,
          element,
          middle,
          left,
          summaries,
          fn(after, root, length, payload, data_sum, memory_sum, rest) {
            let length = length + 1
            let payload = payload + cbor
            let data_sum = data_sum + data
            ok(
              after,
              if summaries {
                prepend_root(head, root, length, payload, data_sum)
              } else {
                root
              },
              length,
              payload,
              data_sum,
              memory_sum + memory,
              rest,
            )
          },
          stop,
        )
      },
      stop,
    )
  } else {
    ok(position + 1, empty_list_root, 0, 0, 0, 0, units)
  }
}

// ## Runs

/// The decoder a run scan uses for elements of type `element`. A Walk
/// compiles it once and runs it on every element of its ticks.
pub fn scan_decoder_v1(
  window: ByteArray,
  flat_bits: Int,
  element: ConstantTypeV1,
) -> Decoder<(Int, Int, Int, Int)> {
  compile_v1(window, flat_bits, shape_v1(element, False), False)
}

/// The decoder a run fold uses for elements of type `element`.
pub fn fold_decoder_v1(
  window: ByteArray,
  flat_bits: Int,
  element: ConstantTypeV1,
) -> Decoder<DataSequenceSummaryV1> {
  compile_v1(window, flat_bits, shape_v1(element, True), True)
}

/// The run of list elements from the cons bit at `position` that a tick
/// decodes with the scan decoder `element`: the longest whose elements all
/// decode here within `units_per_tick`, ending before a nil bit. Gives the
/// element count, the position after the run, the elements' constant memory
/// and the units the run spent.
pub fn scan_run_v1(
  window: ByteArray,
  flat_bits: Int,
  element: Decoder<(Int, Int, Int, Int)>,
  position: Int,
) -> (Int, Int, Int, Int) {
  run_loop(
    window,
    flat_bits,
    flat_bits + 7,
    element,
    position,
    units_per_tick,
    0,
    0,
  )
}

fn run_loop(
  window: ByteArray,
  flat_bits: Int,
  top: Int,
  element: Decoder<(Int, Int, Int, Int)>,
  position: Int,
  units: Int,
  count: Int,
  memory: Int,
) -> (Int, Int, Int, Int) {
  if position >= flat_bits {
    (count, position, memory, units_per_tick - units)
  } else if builtin.read_bit(window, top - position) {
    element(
      position + 1,
      position + 1,
      units - element_units,
      fn(after, _entry, _cbor, _data, value_memory, left) {
        run_loop(
          window,
          flat_bits,
          top,
          element,
          after,
          left,
          count + 1,
          memory + value_memory,
        )
      },
      fn(_left) { (count, position, memory, units_per_tick - units) },
    )
  } else {
    (count, position, memory, units_per_tick - units)
  }
}

/// Prepends the `count` elements of a scanned run, from the cons bit at
/// `position`, to `sequence`, last element first, with the fold decoder
/// `element`.
pub fn fold_run_v1(
  element: Decoder<DataSequenceSummaryV1>,
  position: Int,
  count: Int,
  sequence: DataSequenceSummaryV1,
) -> DataSequenceSummaryV1 {
  fold_loop(
    element,
    position,
    count,
    sequence,
    fn(root, length, payload, data) {
      DataSequenceSummaryV1 {
        root,
        length,
        payload_cbor_length: payload,
        memory: data,
      }
    },
  )
}

fn fold_loop(
  element: Decoder<DataSequenceSummaryV1>,
  position: Int,
  count: Int,
  sequence: DataSequenceSummaryV1,
  k: fn(ByteArray, Int, Int, Int) -> DataSequenceSummaryV1,
) -> DataSequenceSummaryV1 {
  if count == 0 {
    k(
      sequence.root,
      sequence.length,
      sequence.payload_cbor_length,
      sequence.memory,
    )
  } else {
    element(
      position + 1,
      position + 1,
      unmetered,
      fn(after, head, cbor, data, _memory, _left) {
        fold_loop(
          element,
          after,
          count - 1,
          sequence,
          fn(root, length, payload, data_sum) {
            let length = length + 1
            let payload = payload + cbor
            let data_sum = data_sum + data
            k(
              prepend_root(head, root, length, payload, data_sum),
              length,
              payload,
              data_sum,
            )
          },
        )
      },
      fn(_left) {
        fail @"a scanned run element no longer decodes"
      },
    )
  }
}
