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
  DataConstant, IntegerConstant, ListConstant, PairConstant, StringConstant,
  UnitConstant, ValueConstant,
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
pub const unmetered = 1000000000

/// The most leaves a pair's bit table may have.
const max_leaves = 4

// ## Summaries

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

/// The entry of a summary.
pub fn entry_of_v1(summary: DataSummaryV1) -> ByteArray {
  builtin.append_bytearray(
    summary.root,
    builtin.append_bytearray(
      builtin.serialise_data(builtin.i_data(summary.cbor_length)),
      builtin.serialise_data(builtin.i_data(summary.memory)),
    ),
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
/// `count` (the serialised length), `payload` and `data` already count the
/// head.
fn prepend_root(
  head: ByteArray,
  tail: ByteArray,
  count: ByteArray,
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
              count,
              builtin.append_bytearray(
                builtin.serialise_data(builtin.i_data(payload)),
                builtin.serialise_data(builtin.i_data(data)),
              ),
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
    root: prepend_root(
      entry_of_v1(head),
      tail.root,
      builtin.serialise_data(builtin.i_data(length)),
      payload,
      memory,
    ),
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
              builtin.append_bytearray(
                builtin.serialise_data(builtin.i_data(payload)),
                builtin.serialise_data(builtin.i_data(data)),
              ),
            ),
          ),
        ),
      ),
    ),
  )
  let sizes = builtin.append_bytearray(
    builtin.serialise_data(builtin.i_data(4 + payload)),
    builtin.serialise_data(builtin.i_data(4 + data)),
  )
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
/// items whose count serialises to `count` and whose Data memories sum to
/// `data`, the list's CBOR length being `cbor` (`list_cbor_length`):
/// `cek_data_v1.list_data_summary_v1`. Its Data memory is `4 + data`.
fn list_entry(
  root: ByteArray,
  count: ByteArray,
  cbor: Int,
  data: Int,
) -> ByteArray {
  let sizes = builtin.append_bytearray(
    builtin.serialise_data(builtin.i_data(cbor)),
    builtin.serialise_data(builtin.i_data(4 + data)),
  )
  builtin.append_bytearray(
    blake2b_256(
      builtin.append_bytearray(
        list_data_head,
        builtin.append_bytearray(
          count,
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
  let cbor = list_cbor_length(items.length, items.payload_cbor_length)
  summary_of_v1(
    list_entry(
      items.root,
      builtin.serialise_data(builtin.i_data(items.length)),
      cbor,
      items.memory,
    ),
    cbor,
    4 + items.memory,
  )
}

/// The data-node domain, then a byte string node's head up to its blob
/// root: 5 fields, tag 5, a 32-byte string.
const bytes_data_head = #"4d69646761726443656b446174614e6f6465563185055820"

/// The data-node domain, then an integer node's head up to its CBOR root:
/// 4 fields, tag 4, a 32-byte string.
const integer_data_head = #"4d69646761726443656b446174614e6f6465563184045820"

/// The data-node domain, then a map node's head: 5 fields, tag 2.
const map_data_head = #"4d69646761726443656b446174614e6f646556318502"

/// The pair-node domain, the 10-field array head and the key root's byte
/// string head.
const pair_node_list_head =
  #"4d69646761726443656b44617461506169724e6f646556318a5820"

const blob_chunk_domain = #"4d69646761726443656b426c6f624368756e6b5631"

const empty_pairs_root: ByteArray = cek_data_v1.empty_data_pair_root_v1()

/// The canonical CBOR of a byte string of at most 64 bytes: one definite
/// head, as `serialise_data` writes it at that length.
fn definite_bytes(bytes: ByteArray) -> ByteArray {
  builtin.serialise_data(builtin.b_data(bytes))
}

/// The entry, CBOR length and Data memory of a byte string of at most 64
/// bytes: `bits.short_bytes_summary`. Its CBOR is one definite head, so its
/// CBOR length is that of `definite_bytes`.
fn short_bytes_entry(bytes: ByteArray, k: fn(ByteArray, Int, Int) -> r) -> r {
  let length = builtin.length_of_bytearray(bytes)
  let encoded = definite_bytes(bytes)
  let cbor = builtin.length_of_bytearray(encoded)
  let data = 4 + cek_data_v1.bytes_memory_size_v1(length)
  let sizes = builtin.append_bytearray(
    builtin.serialise_data(builtin.i_data(cbor)),
    builtin.serialise_data(builtin.i_data(data)),
  )
  k(
    builtin.append_bytearray(
      blake2b_256(
        builtin.append_bytearray(
          bytes_data_head,
          builtin.append_bytearray(
            blake2b_256(builtin.append_bytearray(blob_chunk_domain, encoded)),
            builtin.append_bytearray(
              builtin.serialise_data(builtin.i_data(length)),
              sizes,
            ),
          ),
        ),
      ),
      sizes,
    ),
    cbor,
    data,
  )
}

/// The entry, CBOR length and Data memory of an integer whose canonical CBOR
/// is at most 64 bytes: `bits.integer_summary`.
fn integer_entry(value: Int, k: fn(ByteArray, Int, Int) -> r) -> r {
  let encoded = builtin.serialise_data(builtin.i_data(value))
  let cbor = builtin.length_of_bytearray(encoded)
  let data = 4 + cek_data_v1.integer_memory_size_v1(value)
  let sizes = builtin.append_bytearray(
    builtin.serialise_data(builtin.i_data(cbor)),
    builtin.serialise_data(builtin.i_data(data)),
  )
  k(
    builtin.append_bytearray(
      blake2b_256(
        builtin.append_bytearray(
          integer_data_head,
          builtin.append_bytearray(
            blake2b_256(
              builtin.append_bytearray(
                blob_chunk_domain,
                definite_bytes(encoded),
              ),
            ),
            sizes,
          ),
        ),
      ),
      sizes,
    ),
    cbor,
    data,
  )
}

/// The root of a map entry node: the key's and value's entries over a tail
/// root, where `length`, `payload` and `data` already count the entry.
/// `cek_data_v1.prepend_data_pair_summary_v1`.
fn prepend_pair_root(
  key: ByteArray,
  value: ByteArray,
  tail: ByteArray,
  length: Int,
  payload: Int,
  data: Int,
) -> ByteArray {
  blake2b_256(
    builtin.append_bytearray(
      pair_node_list_head,
      builtin.append_bytearray(
        key,
        builtin.append_bytearray(
          #"5820",
          builtin.append_bytearray(
            value,
            builtin.append_bytearray(
              #"5820",
              builtin.append_bytearray(
                tail,
                builtin.append_bytearray(
                  builtin.serialise_data(builtin.i_data(length)),
                  builtin.append_bytearray(
                    builtin.serialise_data(builtin.i_data(payload)),
                    builtin.serialise_data(builtin.i_data(data)),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    ),
  )
}

/// The CBOR length of a map of `length` entries whose CBOR lengths sum to
/// `payload`.
fn map_cbor_length(length: Int, payload: Int) -> Int {
  if length < 24 {
    1 + payload
  } else if length <= 255 {
    2 + payload
  } else if length <= 65535 {
    3 + payload
  } else {
    5 + payload
  }
}

/// The entry of a map Data value over entries with root `root`, `length`
/// of them, with CBOR length `cbor` and entries' Data memories summing to
/// `data`: `cek_data_v1.map_data_summary_v1`. Its Data memory is `4 + data`.
fn map_entry(root: ByteArray, length: Int, cbor: Int, data: Int) -> ByteArray {
  let sizes = builtin.append_bytearray(
    builtin.serialise_data(builtin.i_data(cbor)),
    builtin.serialise_data(builtin.i_data(4 + data)),
  )
  builtin.append_bytearray(
    blake2b_256(
      builtin.append_bytearray(
        map_data_head,
        builtin.append_bytearray(
          builtin.serialise_data(builtin.i_data(length)),
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
  KData
  /// A Value with at least one currency.
  KValue
  /// A list from its first cons bit, which the bit table it is the set
  /// leaf of has read.
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
    DataConstant -> SGen { kind: KData }
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
        one: SGen { kind: KValue },
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

/// Shapes with summaries, each with its list, array or pair type.
pub type Shapes =
  List<(ConstantTypeV1, Shape)>

/// `shape_v1(ty, True)`, built on the shapes `shapes` holds: those of the
/// list, array and pair types it holds are taken from it, and those of the
/// others built and added. Gives the shape and the shapes after it.
pub fn shape_in_v1(ty: ConstantTypeV1, shapes: Shapes) -> (Shape, Shapes) {
  when ty is {
    ListConstant { element } -> list_shape_in(ty, element, False, shapes)
    ArrayConstant { element } -> list_shape_in(ty, element, True, shapes)
    PairConstant { first, second } ->
      when shape_of(shapes, ty) is {
        Some(shape) -> (shape, shapes)
        None -> {
          let (a, shapes) = shape_in_v1(first, shapes)
          let (b, shapes) = shape_in_v1(second, shapes)
          let shape = pair_shape(a, b, True)
          (shape, [(ty, shape), ..shapes])
        }
      }
    _ -> (shape_v1(ty, True), shapes)
  }
}

fn list_shape_in(
  ty: ConstantTypeV1,
  element: ConstantTypeV1,
  array: Bool,
  shapes: Shapes,
) -> (Shape, Shapes) {
  when shape_of(shapes, ty) is {
    Some(shape) -> (shape, shapes)
    None -> {
      let (element, shapes) = shape_in_v1(element, shapes)
      let shape = SBit {
        zero: fixed(empty_list_value, empty_list_entry, 0, True),
        one: SGen { kind: KList { element, array } },
      }
      (shape, [(ty, shape), ..shapes])
    }
  }
}

fn shape_of(shapes: Shapes, ty: ConstantTypeV1) -> Option<Shape> {
  when shapes is {
    [] -> None
    [(known, shape), ..rest] ->
      if known == ty {
        Some(shape)
      } else {
        shape_of(rest, ty)
      }
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

/// Wraps a decoded value's entry in `layers`, innermost first. For a value
/// decoded once, where compiling the layers would not pay.
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

/// A compiled wrap: from a decoded value's entry, CBOR length and Data
/// memory, goes on with those of the value wrapped in a KWrap's pairs.
type Wrap<r> =
  fn(ByteArray, Int, Int, fn(ByteArray, Int, Int) -> r) -> r

/// Compiles `layers`, innermost first, into a wrap. Each pair's bytes that
/// do not depend on the wrapped value are computed here, once: for a value
/// that comes second, the head of the pair's outer list node; for a value
/// that comes first, everything after its entry in that node, including
/// the root of the one-item list holding the other component. The
/// outermost pair goes on with the wrap's continuation itself.
fn compile_layers(layers: List<Layer>) -> Wrap<r> {
  when layers is {
    [] ->
      fn(entry, cbor, data, k) { k(entry, cbor, data) }
    [LFirst { entry: other, cbor: other_cbor, data: other_data }, ..rest] ->
      hold(
        builtin.append_bytearray(
          list_node_head,
          builtin.append_bytearray(other, #"5820"),
        ),
        other_cbor,
        other_data,
        compile_layers(rest),
        when rest is {
          [] ->
            fn(prefix, other_cbor, other_data, _next) {
              fn(entry, cbor, data, k) {
                let payload = other_cbor + cbor
                let data = other_data + data
                let cbor = 4 + payload
                let memory = 4 + data
                let sizes = builtin.append_bytearray(
                  if cbor < 24 {
                    builtin.cons_bytearray(cbor, #"")
                  } else if cbor < 256 {
                    builtin.cons_bytearray(
                      24,
                      builtin.cons_bytearray(cbor, #""),
                    )
                  } else {
                    builtin.serialise_data(builtin.i_data(cbor))
                  },
                  builtin.serialise_data(builtin.i_data(memory)),
                )
                k(
                  builtin.append_bytearray(
                    blake2b_256(
                      builtin.append_bytearray(
                        pair_node_head,
                        builtin.append_bytearray(
                          blake2b_256(
                            builtin.append_bytearray(
                              prefix,
                              builtin.append_bytearray(
                                blake2b_256(
                                  builtin.append_bytearray(
                                    list_node_head,
                                    builtin.append_bytearray(
                                      entry,
                                      builtin.append_bytearray(
                                        last_item_tail,
                                        builtin.slice_bytearray(32, 64, entry),
                                      ),
                                    ),
                                  ),
                                ),
                                builtin.append_bytearray(
                                  #"02",
                                  builtin.append_bytearray(
                                    builtin.serialise_data(
                                      builtin.i_data(payload),
                                    ),
                                    builtin.serialise_data(builtin.i_data(data)),
                                  ),
                                ),
                              ),
                            ),
                          ),
                          sizes,
                        ),
                      ),
                    ),
                    sizes,
                  ),
                  cbor,
                  memory,
                )
              }
            }
          _ ->
            fn(prefix, other_cbor, other_data, next) {
              fn(entry, cbor, data, k) {
                let payload = other_cbor + cbor
                let data = other_data + data
                let cbor = 4 + payload
                let memory = 4 + data
                let sizes = builtin.append_bytearray(
                  if cbor < 24 {
                    builtin.cons_bytearray(cbor, #"")
                  } else if cbor < 256 {
                    builtin.cons_bytearray(
                      24,
                      builtin.cons_bytearray(cbor, #""),
                    )
                  } else {
                    builtin.serialise_data(builtin.i_data(cbor))
                  },
                  builtin.serialise_data(builtin.i_data(memory)),
                )
                next(
                  builtin.append_bytearray(
                    blake2b_256(
                      builtin.append_bytearray(
                        pair_node_head,
                        builtin.append_bytearray(
                          blake2b_256(
                            builtin.append_bytearray(
                              prefix,
                              builtin.append_bytearray(
                                blake2b_256(
                                  builtin.append_bytearray(
                                    list_node_head,
                                    builtin.append_bytearray(
                                      entry,
                                      builtin.append_bytearray(
                                        last_item_tail,
                                        builtin.slice_bytearray(32, 64, entry),
                                      ),
                                    ),
                                  ),
                                ),
                                builtin.append_bytearray(
                                  #"02",
                                  builtin.append_bytearray(
                                    builtin.serialise_data(
                                      builtin.i_data(payload),
                                    ),
                                    builtin.serialise_data(builtin.i_data(data)),
                                  ),
                                ),
                              ),
                            ),
                          ),
                          sizes,
                        ),
                      ),
                    ),
                    sizes,
                  ),
                  cbor,
                  memory,
                  k,
                )
              }
            }
        },
      )
    [LSecond { entry: other, cbor: other_cbor, data: other_data }, ..rest] ->
      hold(
        builtin.append_bytearray(
          #"5820",
          builtin.append_bytearray(
            blake2b_256(
              builtin.append_bytearray(
                list_node_head,
                builtin.append_bytearray(
                  other,
                  builtin.append_bytearray(
                    last_item_tail,
                    builtin.slice_bytearray(32, 64, other),
                  ),
                ),
              ),
            ),
            #"02",
          ),
        ),
        other_cbor,
        other_data,
        compile_layers(rest),
        when rest is {
          [] ->
            fn(suffix, other_cbor, other_data, _next) {
              fn(entry, cbor, data, k) {
                let payload = cbor + other_cbor
                let data = data + other_data
                let cbor = 4 + payload
                let memory = 4 + data
                let sizes = builtin.append_bytearray(
                  if cbor < 24 {
                    builtin.cons_bytearray(cbor, #"")
                  } else if cbor < 256 {
                    builtin.cons_bytearray(
                      24,
                      builtin.cons_bytearray(cbor, #""),
                    )
                  } else {
                    builtin.serialise_data(builtin.i_data(cbor))
                  },
                  builtin.serialise_data(builtin.i_data(memory)),
                )
                k(
                  builtin.append_bytearray(
                    blake2b_256(
                      builtin.append_bytearray(
                        pair_node_head,
                        builtin.append_bytearray(
                          blake2b_256(
                            builtin.append_bytearray(
                              list_node_head,
                              builtin.append_bytearray(
                                entry,
                                builtin.append_bytearray(
                                  suffix,
                                  builtin.append_bytearray(
                                    builtin.serialise_data(
                                      builtin.i_data(payload),
                                    ),
                                    builtin.serialise_data(builtin.i_data(data)),
                                  ),
                                ),
                              ),
                            ),
                          ),
                          sizes,
                        ),
                      ),
                    ),
                    sizes,
                  ),
                  cbor,
                  memory,
                )
              }
            }
          _ ->
            fn(suffix, other_cbor, other_data, next) {
              fn(entry, cbor, data, k) {
                let payload = cbor + other_cbor
                let data = data + other_data
                let cbor = 4 + payload
                let memory = 4 + data
                let sizes = builtin.append_bytearray(
                  if cbor < 24 {
                    builtin.cons_bytearray(cbor, #"")
                  } else if cbor < 256 {
                    builtin.cons_bytearray(
                      24,
                      builtin.cons_bytearray(cbor, #""),
                    )
                  } else {
                    builtin.serialise_data(builtin.i_data(cbor))
                  },
                  builtin.serialise_data(builtin.i_data(memory)),
                )
                next(
                  builtin.append_bytearray(
                    blake2b_256(
                      builtin.append_bytearray(
                        pair_node_head,
                        builtin.append_bytearray(
                          blake2b_256(
                            builtin.append_bytearray(
                              list_node_head,
                              builtin.append_bytearray(
                                entry,
                                builtin.append_bytearray(
                                  suffix,
                                  builtin.append_bytearray(
                                    builtin.serialise_data(
                                      builtin.i_data(payload),
                                    ),
                                    builtin.serialise_data(builtin.i_data(data)),
                                  ),
                                ),
                              ),
                            ),
                          ),
                          sizes,
                        ),
                      ),
                    ),
                    sizes,
                  ),
                  cbor,
                  memory,
                  k,
                )
              }
            }
        },
      )
  }
}

/// Builds a wrap from parts computed once, when it is compiled (see `join`).
fn hold(
  bytes: ByteArray,
  cbor: Int,
  data: Int,
  next: Wrap<r>,
  build: fn(ByteArray, Int, Int, Wrap<r>) -> Wrap<r>,
) -> Wrap<r> {
  build(bytes, cbor, data, next)
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

/// One step from a value down to where a decode of it stopped.
pub type Within {
  /// In a list, from its first cons bit: `count` elements decoded, with
  /// constant memory `memory`, spending `units`; stopped at or in the element
  /// whose cons bit is at `at`.
  WList { count: Int, memory: Int, units: Int, at: Int }
  /// In the first component of a pair whose components both have bits.
  WFirst
  /// In the second component of such a pair, from `middle`; the first has
  /// constant memory `memory`.
  WSecond { middle: Int, memory: Int }
}

/// A compiled decoder of one value: from the bit where the value began, the
/// current bit (the same bit outside a shape's bit table) and the units
/// left, it goes to `ok` with the position after the value, its entry, CBOR
/// length, Data memory and constant memory (the first three placeholders
/// without summaries) and the units left, never negative; or, when the value
/// is not decoded here, to `stop` with the steps from the value down to where
/// it stopped, outermost first. A metered decoder gives every step down it
/// took; one without `metered` stops only on a value no scan decoded.
pub type Decoder<r> =
  fn(
    Int,
    Int,
    Int,
    fn(Int, ByteArray, Int, Int, Int, Int) -> r,
    fn(List<Within>) -> r,
  ) ->
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

/// `join` for a decoder and a wrap.
fn join_wrap(
  a: Decoder<r>,
  w: Wrap<r>,
  build: fn(Decoder<r>, Wrap<r>) -> Decoder<r>,
) -> Decoder<r> {
  build(a, w)
}

fn stopped(
  _origin: Int,
  _position: Int,
  _units: Int,
  _ok: fn(Int, ByteArray, Int, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  stop([])
}

/// Compiles `shape` into a decoder over the window. A shape is compiled once
/// per tick and its decoder run on every value the tick decodes.
///
/// Its callers keep the units they give it from going negative.
///
/// A decoder compiled without `metered` is for a fold, with summaries, of a
/// run that a scan has already decoded within its units: it keeps no units,
/// and the units and constant memory it gives are not those of the value.
/// It reads only bits that scan read, all before `flat_bits`, so its list
/// and bit-table steps do not check for the end of the bits, and it skips
/// the checks of byte strings, strings and Values that the scan made (their
/// bounds, UTF-8, key order and quantities). The checks of Data and integers
/// stay.
pub fn compile_v1(
  window: ByteArray,
  flat_bits: Int,
  shape: Shape,
  summaries: Bool,
  metered: Bool,
) -> Decoder<r> {
  compile_node(window, flat_bits, flat_bits + 7, shape, summaries, metered)
}

fn compile_node(
  window: ByteArray,
  flat_bits: Int,
  top: Int,
  shape: Shape,
  summaries: Bool,
  metered: Bool,
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
        compile_node(window, flat_bits, top, zero, summaries, metered),
        compile_node(window, flat_bits, top, one, summaries, metered),
        fn(zero, one) { bit_table(window, flat_bits, top, metered, zero, one) },
      )
    SGen { kind } ->
      compile_kind_v1(window, flat_bits, top, kind, summaries, metered)
  }
}

/// The decoder that reads one bit and goes on with `zero` or `one`.
fn bit_table(
  window: ByteArray,
  flat_bits: Int,
  top: Int,
  metered: Bool,
  zero: Decoder<r>,
  one: Decoder<r>,
) -> Decoder<r> {
  if metered {
    fn(origin, position, units, ok, stop) {
      if position >= flat_bits {
        stop([])
      } else if builtin.read_bit(window, top - position) {
        one(origin, position + 1, units, ok, stop)
      } else {
        zero(origin, position + 1, units, ok, stop)
      }
    }
  } else {
    fn(origin, position, units, ok, stop) {
      if builtin.read_bit(window, top - position) {
        one(origin, position + 1, units, ok, stop)
      } else {
        zero(origin, position + 1, units, ok, stop)
      }
    }
  }
}

/// The decoder of a list or array from its first cons bit, whose elements
/// `element` decodes.
fn list_kind(
  window: ByteArray,
  flat_bits: Int,
  top: Int,
  element: Decoder<r>,
  array: Bool,
  summaries: Bool,
  metered: Bool,
) -> Decoder<r> {
  if !metered {
    fn(origin, _position, units, ok, stop) {
      fold_items_from(
        window,
        top,
        element,
        origin,
        units,
        fn(after, root, _length, count, payload, data, left) {
          let cbor = 2 + payload
          ok(
            after,
            list_entry(root, count, cbor, data),
            cbor,
            4 + data,
            0,
            left,
          )
        },
        stop,
      )
    }
  } else if summaries {
    fn(origin, _position, units, ok, stop) {
      let base = units - 1
      items_from(
        window,
        flat_bits,
        top,
        element,
        origin,
        base,
        0,
        0,
        fn(after, root, length, count, payload, data, memory, left) {
          let cbor = 2 + payload
          ok(
            after,
            list_entry(root, count, cbor, data),
            cbor,
            4 + data,
            if array {
              length
            } else {
              memory
            },
            left,
          )
        },
        fn(at, count, memory, now, path) {
          stop([WList { count, memory, units: base - now, at }, ..path])
        },
      )
    }
  } else {
    fn(origin, _position, units, ok, stop) {
      let base = units - 1
      scan_items_from(
        window,
        flat_bits,
        top,
        element,
        origin,
        base,
        0,
        0,
        fn(after, length, memory, left) {
          ok(
            after,
            #"",
            0,
            0,
            if array {
              length
            } else {
              memory
            },
            left,
          )
        },
        fn(at, count, memory, now, path) {
          stop([WList { count, memory, units: base - now, at }, ..path])
        },
      )
    }
  }
}

/// The decoder of a list or array whose elements `element` decodes, the
/// one `compile_v1` gives for its shape, built on the element decoder.
pub fn list_decoder_v1(
  window: ByteArray,
  flat_bits: Int,
  element: Decoder<r>,
  array: Bool,
  summaries: Bool,
  metered: Bool,
) -> Decoder<r> {
  list_decoder_at(
    window,
    flat_bits,
    flat_bits + 7,
    element,
    array,
    summaries,
    metered,
  )
}

fn list_decoder_at(
  window: ByteArray,
  flat_bits: Int,
  top: Int,
  element: Decoder<r>,
  array: Bool,
  summaries: Bool,
  metered: Bool,
) -> Decoder<r> {
  join(
    compile_node(
      window,
      flat_bits,
      top,
      fixed(empty_list_value, empty_list_entry, 0, summaries),
      summaries,
      metered,
    ),
    list_kind(window, flat_bits, top, element, array, summaries, metered),
    fn(zero, one) { bit_table(window, flat_bits, top, metered, zero, one) },
  )
}

/// Compiles a decoder of a value of `kind` at the bit where it begins.
pub fn compile_kind_v1(
  window: ByteArray,
  flat_bits: Int,
  top: Int,
  kind: Kind,
  summaries: Bool,
  metered: Bool,
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
              stop([])
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
          fn() { stop([]) },
        )
      }
    KBytes ->
      if metered {
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
      } else {
        fn(origin, _position, units, ok, _stop) {
          scanned_string(window, flat_bits, origin, False, units, ok)
        }
      }
    KString ->
      if metered {
        fn(origin, _position, units, ok, stop) {
          byte_string(
            window,
            flat_bits,
            origin,
            True,
            units,
            summaries,
            ok,
            stop,
          )
        }
      } else {
        fn(origin, _position, units, ok, _stop) {
          scanned_string(window, flat_bits, origin, True, units, ok)
        }
      }
    KData ->
      fn(origin, _position, units, ok, stop) {
        data_constant(window, flat_bits, origin, units, summaries, ok, stop)
      }
    KValue ->
      fn(origin, _position, units, ok, stop) {
        value_constant(
          window,
          flat_bits,
          origin,
          units,
          summaries,
          metered,
          ok,
          stop,
        )
      }
    KList { element, array } ->
      join(
        compile_node(window, flat_bits, top, element, summaries, metered),
        stopped,
        fn(element, _) {
          list_kind(window, flat_bits, top, element, array, summaries, metered)
        },
      )
    KPair { first, second, stops } ->
      if stops {
        stopped
      } else {
        join(
          compile_node(window, flat_bits, top, first, summaries, metered),
          compile_node(window, flat_bits, top, second, summaries, metered),
          fn(first, second) {
            if !metered {
              fn(origin, _position, units, ok, stop) {
                first(
                  origin,
                  origin,
                  units,
                  fn(middle, a, a_cbor, a_data, _a_memory, left) {
                    second(
                      middle,
                      middle,
                      left,
                      fn(after, b, b_cbor, b_data, _b_memory, rest) {
                        let payload = a_cbor + b_cbor
                        let data = a_data + b_data
                        ok(
                          after,
                          pair_entry_v1(a, b, payload, data),
                          4 + payload,
                          4 + data,
                          0,
                          rest,
                        )
                      },
                      stop,
                    )
                  },
                  stop,
                )
              }
            } else if summaries {
              fn(origin, _position, units, ok, stop) {
                if units < pair_units {
                  stop([])
                } else {
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
                        fn(path) {
                          stop([WSecond { middle, memory: a_memory }, ..path])
                        },
                      )
                    },
                    fn(path) { stop([WFirst, ..path]) },
                  )
                }
              }
            } else {
              fn(origin, _position, units, ok, stop) {
                if units < pair_units {
                  stop([])
                } else {
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
                        fn(path) {
                          stop([WSecond { middle, memory: a_memory }, ..path])
                        },
                      )
                    },
                    fn(path) { stop([WFirst, ..path]) },
                  )
                }
              }
            }
          },
        )
      }
    KWrap { inner, layers, units: layer_units, memory: layer_memory } ->
      join_wrap(
        compile_kind_v1(window, flat_bits, top, inner, summaries, metered),
        compile_layers(
          if summaries {
            layers
          } else {
            []
          },
        ),
        fn(inner, wrap) {
          if !metered {
            fn(origin, position, units, ok, stop) {
              inner(
                origin,
                position,
                units,
                fn(after, entry, cbor, data, memory, left) {
                  wrap(
                    entry,
                    cbor,
                    data,
                    fn(entry, cbor, data) {
                      ok(after, entry, cbor, data, memory, left)
                    },
                  )
                },
                stop,
              )
            }
          } else if summaries {
            fn(origin, position, units, ok, stop) {
              inner(
                origin,
                position,
                units - layer_units,
                fn(after, entry, cbor, data, memory, left) {
                  wrap(
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

/// Compiles a decoder of one value of `kind` with summaries, for a single
/// run: the pairs of a wrap are applied as they are, not compiled.
pub fn compile_once_v1(
  window: ByteArray,
  flat_bits: Int,
  kind: Kind,
) -> Decoder<r> {
  when kind is {
    KWrap { inner, layers, units: layer_units, memory: layer_memory } ->
      join(
        compile_kind_v1(window, flat_bits, flat_bits + 7, inner, True, True),
        stopped,
        fn(inner, _) {
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
        },
      )
    _ -> compile_kind_v1(window, flat_bits, flat_bits + 7, kind, True, True)
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

// ## Byte strings, Data and values
//
// Each decoder below accepts exactly the values the walk's one-node ticks
// accept, or stops. Only the total it charges matters, so a decoder may
// charge in any order, and it checks its units as it goes so that a value
// too large for the tick stops early.

/// Units per byte string or string, past those of its chunks.
const string_units = 10

/// Units per Flat chunk read or stepped over past a value's first.
const chunk_units = 1

/// Units per Data constant: its filler and its closing chunk.
const data_units = 4

/// Units per CBOR head or break read.
const head_units = 1

/// Units per integer item.
const integer_units = 6

/// Units per item of a Data list or constructor.
const item_units = 2

/// Units per entry of a Data map.
const entry_units = 4

/// Units per Data list, map or constructor node.
const node_units = 4

/// Units per currency and per token of a Value, past a quantity's groups.
const currency_units = 16

const token_units = 16

/// The most bytes a byte string decoded here may have: one blob chunk.
const max_string_bytes = 4095

const max_data_bytes_chunk = 64

const max_key_bytes = 32

const min_quantity = -170141183460469231731687303715884105728

const max_quantity = 170141183460469231731687303715884105727

const empty_pairs: DataSequenceSummaryV1 =
  cek_data_v1.empty_data_pair_summary_v1()

/// The byte index after the filler from bit `position`, or -1 when the
/// filler does not end on a byte boundary.
fn filler_end(window: ByteArray, flat_length: Int, position: Int) -> Int {
  when bits.next_set_bit(window, position, flat_length) is {
    None -> -1
    Some(q) ->
      if ( q + 1 ) % 8 == 0 {
        ( q + 1 ) / 8
      } else {
        -1
      }
  }
}

/// A byte string or string: the filler, its chunks and the empty chunk
/// ending them.
fn byte_string(
  window: ByteArray,
  flat_bits: Int,
  position: Int,
  string: Bool,
  units: Int,
  summaries: Bool,
  ok: fn(Int, ByteArray, Int, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  let flat_length = flat_bits / 8
  let index = filler_end(window, flat_length, position)
  if index < 0 || index >= flat_length {
    stop([])
  } else if builtin.index_bytearray(window, index) == 0 {
    let left = units - 2
    if left < 0 {
      stop([])
    } else {
      ok(
        8 * index + 8,
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
    chunks(
      window,
      flat_length,
      string,
      summaries,
      index,
      index,
      #"",
      0,
      0,
      units - string_units,
      ok,
      stop,
    )
  }
}

/// The chunks of a byte string from the chunk head at `i`, after `total`
/// bytes (kept in `acc` with summaries) and in UTF-8 state `utf8`.
fn chunks(
  window: ByteArray,
  flat_length: Int,
  string: Bool,
  summaries: Bool,
  first: Int,
  i: Int,
  acc: ByteArray,
  total: Int,
  utf8: Int,
  units: Int,
  ok: fn(Int, ByteArray, Int, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if i >= flat_length {
    stop([])
  } else {
    let n = builtin.index_bytearray(window, i)
    if n == 0 {
      if utf8 != 0 {
        stop([])
      } else {
        let memory = if string {
          total / 4
        } else {
          cek_data_v1.bytes_memory_size_v1(total)
        }
        if summaries {
          let summary = bits.short_bytes_summary(acc)
          ok(
            8 * i + 8,
            entry_of_v1(summary),
            summary.cbor_length,
            summary.memory,
            memory,
            units,
          )
        } else {
          ok(8 * i + 8, #"", 0, 0, memory, units)
        }
      }
    } else {
      let units = units - if i == first {
        0
      } else {
        chunk_units
      } - if string {
        n
      } else {
        n / 32
      }
      // A chunk running past the Flat bytes leaves the next chunk head past
      // them, where the check above stops.
      if units < 0 || total + n > max_string_bytes {
        stop([])
      } else if string {
        let chunk = builtin.slice_bytearray(i + 1, n, window)
        let utf8 = bits.utf8_push(utf8, chunk)
        if utf8 < 0 {
          stop([])
        } else {
          chunks(
            window,
            flat_length,
            string,
            summaries,
            first,
            i + 1 + n,
            if summaries {
              builtin.append_bytearray(acc, chunk)
            } else {
              acc
            },
            total + n,
            utf8,
            units,
            ok,
            stop,
          )
        }
      } else {
        chunks(
          window,
          flat_length,
          string,
          summaries,
          first,
          i + 1 + n,
          if summaries {
            builtin.append_bytearray(
              acc,
              builtin.slice_bytearray(i + 1, n, window),
            )
          } else {
            acc
          },
          total + n,
          0,
          units,
          ok,
          stop,
        )
      }
    }
  }
}

/// A byte string or string from bit `position` that a scan decoded: the
/// bytes and summary `byte_string` gives it, read without its checks. The
/// scan's `byte_string` checked the filler, the chunks against the Flat
/// bytes, the size bound and UTF-8, and the units are not metered here.
fn scanned_string(
  window: ByteArray,
  flat_bits: Int,
  position: Int,
  string: Bool,
  units: Int,
  ok: fn(Int, ByteArray, Int, Int, Int, Int) -> r,
) -> r {
  let flat_length = flat_bits / 8
  let index = filler_end(window, flat_length, position)
  if builtin.index_bytearray(window, index) == 0 {
    ok(
      8 * index + 8,
      empty_bytes_entry,
      empty_bytes_value.cbor_length,
      empty_bytes_value.memory,
      if string {
        0
      } else {
        1
      },
      units,
    )
  } else {
    scanned_chunks(window, string, index, #"", 0, units, ok)
  }
}

fn scanned_chunks(
  window: ByteArray,
  string: Bool,
  i: Int,
  acc: ByteArray,
  total: Int,
  units: Int,
  ok: fn(Int, ByteArray, Int, Int, Int, Int) -> r,
) -> r {
  let n = builtin.index_bytearray(window, i)
  if n == 0 {
    let summary = bits.short_bytes_summary(acc)
    ok(
      8 * i + 8,
      entry_of_v1(summary),
      summary.cbor_length,
      summary.memory,
      if string {
        total / 4
      } else {
        cek_data_v1.bytes_memory_size_v1(total)
      },
      units,
    )
  } else {
    scanned_chunks(
      window,
      string,
      i + 1 + n,
      builtin.append_bytearray(acc, builtin.slice_bytearray(i + 1, n, window)),
      total + n,
      units,
      ok,
    )
  }
}

// ### Data

/// A Data constant from bit `position`: the filler, the first CBOR item and
/// the Flat chunks after it up to the empty one.
fn data_constant(
  window: ByteArray,
  flat_bits: Int,
  position: Int,
  units: Int,
  summaries: Bool,
  ok: fn(Int, ByteArray, Int, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  let flat_length = flat_bits / 8
  let start = filler_end(window, flat_length, position)
  let units = units - data_units
  if start < 0 || units < 0 {
    stop([])
  } else {
    d_item(
      window,
      flat_length,
      summaries,
      start,
      start,
      units,
      fn(summary, _p, end, units) {
        d_trail(window, flat_length, end, summary, units, summaries, ok, stop)
      },
      fn(_p, _end, _units) { stop([]) },
      stop,
    )
  }
}

/// Steps over the Flat chunks from the chunk head at `p` to the empty one.
fn d_trail(
  window: ByteArray,
  flat_length: Int,
  p: Int,
  summary: DataSummaryV1,
  units: Int,
  summaries: Bool,
  ok: fn(Int, ByteArray, Int, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if p >= flat_length {
    stop([])
  } else {
    let n = builtin.index_bytearray(window, p)
    if n == 0 {
      if summaries {
        ok(
          8 * p + 8,
          entry_of_v1(summary),
          summary.cbor_length,
          summary.memory,
          summary.memory,
          units,
        )
      } else {
        ok(8 * p + 8, #"", 0, 0, summary.memory, units)
      }
    } else {
      let units = units - chunk_units
      if units < 0 {
        stop([])
      } else {
        d_trail(
          window,
          flat_length,
          p + 1 + n,
          summary,
          units,
          summaries,
          ok,
          stop,
        )
      }
    }
  }
}

/// From the CBOR byte `p` of the Flat chunk ending at `end`, opens the next
/// chunk when this one is spent.
fn d_open(
  window: ByteArray,
  flat_length: Int,
  p: Int,
  end: Int,
  units: Int,
  k: fn(Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if p < end {
    k(p, end, units)
  } else if p >= flat_length {
    stop([])
  } else {
    let n = builtin.index_bytearray(window, p)
    let units = units - chunk_units
    if n == 0 || p + 1 + n > flat_length || units < 0 {
      stop([])
    } else {
      k(p + 1, p + 1 + n, units)
    }
  }
}

fn d_byte(
  window: ByteArray,
  flat_length: Int,
  p: Int,
  end: Int,
  units: Int,
  k: fn(Int, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if p < end {
    k(builtin.index_bytearray(window, p), p + 1, end, units)
  } else {
    d_open(
      window,
      flat_length,
      p,
      end,
      units,
      fn(p, end, units) {
        k(builtin.index_bytearray(window, p), p + 1, end, units)
      },
      stop,
    )
  }
}

/// `count` CBOR bytes appended to `acc`, across Flat chunks.
fn d_take(
  window: ByteArray,
  flat_length: Int,
  count: Int,
  acc: ByteArray,
  p: Int,
  end: Int,
  units: Int,
  k: fn(ByteArray, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if count == 0 {
    k(acc, p, end, units)
  } else {
    d_open(
      window,
      flat_length,
      p,
      end,
      units,
      fn(p, end, units) {
        let m = if count < end - p {
          count
        } else {
          end - p
        }
        d_take(
          window,
          flat_length,
          count - m,
          builtin.append_bytearray(acc, builtin.slice_bytearray(p, m, window)),
          p + m,
          end,
          units,
          k,
          stop,
        )
      },
      stop,
    )
  }
}

fn d_uint(
  window: ByteArray,
  flat_length: Int,
  width: Int,
  acc: Int,
  p: Int,
  end: Int,
  units: Int,
  k: fn(Int, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if width == 0 {
    k(acc, p, end, units)
  } else {
    d_byte(
      window,
      flat_length,
      p,
      end,
      units,
      fn(byte, p, end, units) {
        d_uint(
          window,
          flat_length,
          width - 1,
          acc * 256 + byte,
          p,
          end,
          units,
          k,
          stop,
        )
      },
      stop,
    )
  }
}

/// The head with initial byte `initial`: its major type, additional
/// information and argument, -1 for additional information 31.
fn d_head(
  window: ByteArray,
  flat_length: Int,
  initial: Int,
  p: Int,
  end: Int,
  units: Int,
  k: fn(Int, Int, Int, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  let major = initial / 32
  let additional = initial % 32
  if additional < 24 {
    k(major, additional, additional, p, end, units)
  } else if additional == 31 {
    k(major, additional, -1, p, end, units)
  } else if additional > 27 {
    stop([])
  } else {
    d_uint(
      window,
      flat_length,
      builtin.index_bytearray(#[1, 2, 4, 8], additional - 24),
      0,
      p,
      end,
      units,
      fn(argument, p, end, units) {
        k(major, additional, argument, p, end, units)
      },
      stop,
    )
  }
}

fn d_read_head(
  window: ByteArray,
  flat_length: Int,
  p: Int,
  end: Int,
  units: Int,
  k: fn(Int, Int, Int, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  let units = units - head_units
  if units < 0 {
    stop([])
  } else {
    d_byte(
      window,
      flat_length,
      p,
      end,
      units,
      fn(initial, p, end, units) {
        d_head(window, flat_length, initial, p, end, units, k, stop)
      },
      stop,
    )
  }
}

/// The bytes of a byte string item of `length` bytes, or of chunks up to a
/// break when `length` is -1, at most `limit` of them.
fn d_string(
  window: ByteArray,
  flat_length: Int,
  length: Int,
  limit: Int,
  p: Int,
  end: Int,
  units: Int,
  k: fn(ByteArray, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if length >= 0 {
    d_take(window, flat_length, length, #"", p, end, units, k, stop)
  } else {
    d_pieces(window, flat_length, limit, #"", p, end, units, k, stop)
  }
}

fn d_pieces(
  window: ByteArray,
  flat_length: Int,
  limit: Int,
  acc: ByteArray,
  p: Int,
  end: Int,
  units: Int,
  k: fn(ByteArray, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  let units = units - head_units
  if units < 0 {
    stop([])
  } else {
    d_byte(
      window,
      flat_length,
      p,
      end,
      units,
      fn(initial, p, end, units) {
        if initial == 255 {
          k(acc, p, end, units)
        } else {
          d_head(
            window,
            flat_length,
            initial,
            p,
            end,
            units,
            fn(major, _additional, length, p, end, units) {
              if major != 2 || length < 0 || length > max_data_bytes_chunk || builtin.length_of_bytearray(
                acc,
              ) + length > limit {
                stop([])
              } else {
                d_take(
                  window,
                  flat_length,
                  length,
                  acc,
                  p,
                  end,
                  units,
                  fn(acc, p, end, units) {
                    d_pieces(
                      window,
                      flat_length,
                      limit,
                      acc,
                      p,
                      end,
                      units,
                      k,
                      stop,
                    )
                  },
                  stop,
                )
              }
            },
            stop,
          )
        }
      },
      stop,
    )
  }
}

fn d_integer(n: Int, summaries: Bool) -> DataSummaryV1 {
  if summaries {
    bits.integer_summary(n)
  } else {
    DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 4 + cek_data_v1.integer_memory_size_v1(n),
    }
  }
}

fn d_constr(
  constructor: Int,
  fields: DataSequenceSummaryV1,
  summaries: Bool,
) -> DataSummaryV1 {
  if summaries {
    bits.constr_summary(constructor, fields)
  } else {
    DataSummaryV1 { root: #"", cbor_length: 0, memory: 4 + fields.memory }
  }
}

/// One CBOR item: `k` gets its summary (only its memory without summaries),
/// `brk` is taken at a break.
fn d_item(
  window: ByteArray,
  flat_length: Int,
  summaries: Bool,
  p: Int,
  end: Int,
  units: Int,
  k: fn(DataSummaryV1, Int, Int, Int) -> r,
  brk: fn(Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  let units = units - head_units
  if units < 0 {
    stop([])
  } else {
    d_byte(
      window,
      flat_length,
      p,
      end,
      units,
      fn(initial, p, end, units) {
        if initial == 255 {
          brk(p, end, units)
        } else {
          d_head(
            window,
            flat_length,
            initial,
            p,
            end,
            units,
            fn(major, additional, argument, p, end, units) {
              d_body(
                window,
                flat_length,
                summaries,
                major,
                additional,
                argument,
                p,
                end,
                units,
                k,
                stop,
              )
            },
            stop,
          )
        }
      },
      stop,
    )
  }
}

fn d_body(
  window: ByteArray,
  flat_length: Int,
  summaries: Bool,
  major: Int,
  additional: Int,
  argument: Int,
  p: Int,
  end: Int,
  units: Int,
  k: fn(DataSummaryV1, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if major <= 1 {
    let units = units - integer_units
    if argument < 0 || units < 0 {
      stop([])
    } else if major == 0 {
      k(d_integer(argument, summaries), p, end, units)
    } else {
      k(d_integer(-1 - argument, summaries), p, end, units)
    }
  } else if major == 2 {
    if argument > max_data_bytes_chunk {
      stop([])
    } else {
      d_string(
        window,
        flat_length,
        argument,
        max_string_bytes,
        p,
        end,
        units,
        fn(bytes, p, end, units) {
          let length = builtin.length_of_bytearray(bytes)
          let units = units - string_units - length / 32
          if units < 0 {
            stop([])
          } else if summaries {
            k(bits.short_bytes_summary(bytes), p, end, units)
          } else {
            k(
              DataSummaryV1 {
                root: #"",
                cbor_length: 0,
                memory: 4 + cek_data_v1.bytes_memory_size_v1(length),
              },
              p,
              end,
              units,
            )
          }
        },
        stop,
      )
    }
  } else if major == 4 {
    d_items(
      window,
      flat_length,
      summaries,
      argument,
      0,
      p,
      end,
      units,
      fn(fields, p, end, units) {
        k(d_constr(-1, fields, summaries), p, end, units)
      },
      stop,
    )
  } else if major == 5 {
    d_entries(
      window,
      flat_length,
      summaries,
      argument,
      0,
      p,
      end,
      units,
      fn(entries, p, end, units) {
        if summaries {
          k(cek_data_v1.map_data_summary_v1(entries), p, end, units)
        } else {
          k(
            DataSummaryV1 {
              root: #"",
              cbor_length: 0,
              memory: 4 + entries.memory,
            },
            p,
            end,
            units,
          )
        }
      },
      stop,
    )
  } else if major == 6 {
    d_tagged(
      window,
      flat_length,
      summaries,
      additional,
      argument,
      p,
      end,
      units,
      k,
      stop,
    )
  } else {
    stop([])
  }
}

/// The items of a list from item `index` of `length` (-1 up to a break).
fn d_items(
  window: ByteArray,
  flat_length: Int,
  summaries: Bool,
  length: Int,
  index: Int,
  p: Int,
  end: Int,
  units: Int,
  k: fn(DataSequenceSummaryV1, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if index == length {
    let units = units - node_units
    if units < 0 {
      stop([])
    } else {
      k(empty_sequence, p, end, units)
    }
  } else {
    let units = units - item_units
    if units < 0 {
      stop([])
    } else {
      d_item(
        window,
        flat_length,
        summaries,
        p,
        end,
        units,
        fn(head, p, end, units) {
          d_items(
            window,
            flat_length,
            summaries,
            length,
            index + 1,
            p,
            end,
            units,
            fn(tail, p, end, units) {
              if summaries {
                k(prepend_v1(head, tail), p, end, units)
              } else {
                k(
                  DataSequenceSummaryV1 {
                    ..tail,
                    memory: tail.memory + head.memory,
                  },
                  p,
                  end,
                  units,
                )
              }
            },
            stop,
          )
        },
        fn(p, end, units) {
          let units = units - node_units
          if length >= 0 || units < 0 {
            stop([])
          } else {
            k(empty_sequence, p, end, units)
          }
        },
        stop,
      )
    }
  }
}

/// The entries of a map from entry `index` of `length` (-1 up to a break in
/// place of a key).
fn d_entries(
  window: ByteArray,
  flat_length: Int,
  summaries: Bool,
  length: Int,
  index: Int,
  p: Int,
  end: Int,
  units: Int,
  k: fn(DataSequenceSummaryV1, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if index == length {
    let units = units - node_units
    if units < 0 {
      stop([])
    } else {
      k(empty_pairs, p, end, units)
    }
  } else {
    let units = units - entry_units
    if units < 0 {
      stop([])
    } else {
      d_item(
        window,
        flat_length,
        summaries,
        p,
        end,
        units,
        fn(key, p, end, units) {
          d_item(
            window,
            flat_length,
            summaries,
            p,
            end,
            units,
            fn(value, p, end, units) {
              d_entries(
                window,
                flat_length,
                summaries,
                length,
                index + 1,
                p,
                end,
                units,
                fn(tail, p, end, units) {
                  if summaries {
                    k(
                      cek_data_v1.prepend_data_pair_summary_v1(key, value, tail),
                      p,
                      end,
                      units,
                    )
                  } else {
                    k(
                      DataSequenceSummaryV1 {
                        ..tail,
                        memory: tail.memory + key.memory + value.memory,
                      },
                      p,
                      end,
                      units,
                    )
                  }
                },
                stop,
              )
            },
            fn(_p, _end, _units) { stop([]) },
            stop,
          )
        },
        fn(p, end, units) {
          let units = units - node_units
          if length >= 0 || units < 0 {
            stop([])
          } else {
            k(empty_pairs, p, end, units)
          }
        },
        stop,
      )
    }
  }
}

/// The fields list of a constructor.
fn d_fields(
  window: ByteArray,
  flat_length: Int,
  summaries: Bool,
  constructor: Int,
  p: Int,
  end: Int,
  units: Int,
  k: fn(DataSummaryV1, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  d_read_head(
    window,
    flat_length,
    p,
    end,
    units,
    fn(major, _additional, length, p, end, units) {
      if major != 4 {
        stop([])
      } else {
        d_items(
          window,
          flat_length,
          summaries,
          length,
          0,
          p,
          end,
          units,
          fn(fields, p, end, units) {
            k(d_constr(constructor, fields, summaries), p, end, units)
          },
          stop,
        )
      }
    },
    stop,
  )
}

fn d_tagged(
  window: ByteArray,
  flat_length: Int,
  summaries: Bool,
  additional: Int,
  tag: Int,
  p: Int,
  end: Int,
  units: Int,
  k: fn(DataSummaryV1, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if tag < 0 {
    stop([])
  } else if additional == 2 || additional == 3 {
    d_read_head(
      window,
      flat_length,
      p,
      end,
      units,
      fn(major, _additional, length, p, end, units) {
        if major != 2 || length > max_data_bytes_chunk {
          stop([])
        } else {
          d_string(
            window,
            flat_length,
            length,
            max_data_bytes_chunk,
            p,
            end,
            units,
            fn(bytes, p, end, units) {
              let units = units - integer_units
              if units < 0 {
                stop([])
              } else {
                let m = builtin.bytearray_to_integer(True, bytes)
                if additional == 3 {
                  k(d_integer(-1 - m, summaries), p, end, units)
                } else {
                  k(d_integer(m, summaries), p, end, units)
                }
              }
            },
            stop,
          )
        }
      },
      stop,
    )
  } else if tag == 102 {
    d_read_head(
      window,
      flat_length,
      p,
      end,
      units,
      fn(major, _additional, length, p, end, units) {
        if major != 4 || length != 2 && length != -1 {
          stop([])
        } else {
          d_read_head(
            window,
            flat_length,
            p,
            end,
            units,
            fn(index_major, _additional, index, p, end, units) {
              if index_major != 0 || index < 0 {
                stop([])
              } else {
                d_fields(
                  window,
                  flat_length,
                  summaries,
                  index,
                  p,
                  end,
                  units,
                  fn(summary, p, end, units) {
                    if length == 2 {
                      k(summary, p, end, units)
                    } else {
                      let units = units - head_units
                      if units < 0 {
                        stop([])
                      } else {
                        d_byte(
                          window,
                          flat_length,
                          p,
                          end,
                          units,
                          fn(byte, p, end, units) {
                            if byte == 255 {
                              k(summary, p, end, units)
                            } else {
                              stop([])
                            }
                          },
                          stop,
                        )
                      }
                    }
                  },
                  stop,
                )
              }
            },
            stop,
          )
        }
      },
      stop,
    )
  } else if tag >= 121 && tag <= 127 {
    d_fields(window, flat_length, summaries, tag - 121, p, end, units, k, stop)
  } else if tag >= 1280 && tag <= 1400 {
    d_fields(window, flat_length, summaries, tag - 1273, p, end, units, k, stop)
  } else {
    stop([])
  }
}

// ### Values

/// A non-empty Value from its first cons bit at `position`.
fn value_constant(
  window: ByteArray,
  flat_bits: Int,
  position: Int,
  units: Int,
  summaries: Bool,
  check: Bool,
  ok: fn(Int, ByteArray, Int, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  v_currencies(
    window,
    flat_bits,
    summaries,
    check,
    #"",
    True,
    position,
    units,
    fn(root, count, payload, data, total, after, units) {
      if summaries {
        let cbor = map_cbor_length(count, payload)
        ok(
          after,
          map_entry(root, count, cbor, data),
          cbor,
          4 + data,
          total,
          units,
        )
      } else {
        ok(after, #"", 0, 0, total, units)
      }
    },
    stop,
  )
}

/// A Value key from bit `position`: at most 32 bytes over its chunks,
/// checked only with `check`.
fn v_key(
  window: ByteArray,
  flat_length: Int,
  check: Bool,
  position: Int,
  units: Int,
  k: fn(ByteArray, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  let index = filler_end(window, flat_length, position)
  if index < 0 {
    stop([])
  } else {
    v_chunks(window, flat_length, check, index, index, #"", units, k, stop)
  }
}

fn v_chunks(
  window: ByteArray,
  flat_length: Int,
  check: Bool,
  first: Int,
  i: Int,
  acc: ByteArray,
  units: Int,
  k: fn(ByteArray, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if i >= flat_length {
    stop([])
  } else {
    let n = builtin.index_bytearray(window, i)
    if n == 0 {
      k(acc, 8 * i + 8, units)
    } else {
      let units = if i == first {
        units
      } else {
        units - chunk_units
      }
      if check && (
        units < 0 || builtin.length_of_bytearray(acc) + n > max_key_bytes
      ) {
        stop([])
      } else {
        v_chunks(
          window,
          flat_length,
          check,
          first,
          i + 1 + n,
          builtin.append_bytearray(
            acc,
            builtin.slice_bytearray(i + 1, n, window),
          ),
          units,
          k,
          stop,
        )
      }
    }
  }
}

/// The currencies from the outer cons bit at `position`, after `previous`
/// unless `first`: `k` gets the root of their map entries, their count, the
/// sums of the entries' CBOR lengths and Data memories, their token count,
/// the position after them and the units left (the first four placeholders
/// without summaries). Without `check`, for a Value a scan decoded, it does
/// not check the key bounds, the key order or the quantities: the scan did.
fn v_currencies(
  window: ByteArray,
  flat_bits: Int,
  summaries: Bool,
  check: Bool,
  previous: ByteArray,
  first: Bool,
  position: Int,
  units: Int,
  k: fn(ByteArray, Int, Int, Int, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if position >= flat_bits {
    stop([])
  } else if !bit(window, flat_bits, position) {
    k(empty_pairs_root, 0, 0, 0, 0, position + 1, units)
  } else {
    let units = units - currency_units
    if units < 0 {
      stop([])
    } else {
      v_key(
        window,
        flat_bits / 8,
        check,
        position + 1,
        units,
        fn(currency, after, units) {
          if check && !first && !builtin.less_than_bytearray(previous, currency) {
            stop([])
          } else {
            v_tokens(
              window,
              flat_bits,
              summaries,
              check,
              #"",
              True,
              after,
              units,
              fn(tokens, count, tokens_payload, tokens_data, after, units) {
                v_currencies(
                  window,
                  flat_bits,
                  summaries,
                  check,
                  currency,
                  False,
                  after,
                  units,
                  fn(tail, length, payload, data, total, after, units) {
                    if summaries {
                      short_bytes_entry(
                        currency,
                        fn(key, key_cbor, key_data) {
                          let value_cbor = map_cbor_length(
                            count,
                            tokens_payload,
                          )
                          let length = length + 1
                          let payload = key_cbor + value_cbor + payload
                          let data = key_data + 4 + tokens_data + data
                          k(
                            prepend_pair_root(
                              key,
                              map_entry(tokens, count, value_cbor, tokens_data),
                              tail,
                              length,
                              payload,
                              data,
                            ),
                            length,
                            payload,
                            data,
                            total + count,
                            after,
                            units,
                          )
                        },
                      )
                    } else {
                      k(
                        tail,
                        length,
                        payload,
                        data,
                        total + count,
                        after,
                        units,
                      )
                    }
                  },
                  stop,
                )
              },
              stop,
            )
          }
        },
        stop,
      )
    }
  }
}

/// The tokens of one currency from the inner cons bit at `position`, after
/// `previous` unless `first`; a currency has at least one. `k` gets the root
/// of their map entries, their count, the sums of the entries' CBOR lengths
/// and Data memories, the position after them and the units left (the root
/// and sums placeholders without summaries).
fn v_tokens(
  window: ByteArray,
  flat_bits: Int,
  summaries: Bool,
  check: Bool,
  previous: ByteArray,
  first: Bool,
  position: Int,
  units: Int,
  k: fn(ByteArray, Int, Int, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  if position >= flat_bits {
    stop([])
  } else if !bit(window, flat_bits, position) {
    if first {
      stop([])
    } else {
      k(empty_pairs_root, 0, 0, 0, position + 1, units)
    }
  } else {
    let units = units - token_units
    if units < 0 {
      stop([])
    } else {
      v_key(
        window,
        flat_bits / 8,
        check,
        position + 1,
        units,
        fn(token, after, units) {
          if check && !first && !builtin.less_than_bytearray(previous, token) {
            stop([])
          } else {
            natural(
              window,
              flat_bits,
              after,
              0,
              0,
              1,
              fn(after, groups, acc) {
                let units = units - groups
                let n = zigzag(acc)
                if check && (
                  units < 0 || n == 0 || n < min_quantity || n > max_quantity
                ) {
                  stop([])
                } else {
                  v_tokens(
                    window,
                    flat_bits,
                    summaries,
                    check,
                    token,
                    False,
                    after,
                    units,
                    fn(tail, count, payload, data, after, units) {
                      if summaries {
                        short_bytes_entry(
                          token,
                          fn(key, key_cbor, key_data) {
                            integer_entry(
                              n,
                              fn(value, value_cbor, value_data) {
                                let count = count + 1
                                let payload = key_cbor + value_cbor + payload
                                let data = key_data + value_data + data
                                k(
                                  prepend_pair_root(
                                    key,
                                    value,
                                    tail,
                                    count,
                                    payload,
                                    data,
                                  ),
                                  count,
                                  payload,
                                  data,
                                  after,
                                  units,
                                )
                              },
                            )
                          },
                        )
                      } else {
                        k(tail, count + 1, payload, data, after, units)
                      }
                    },
                    stop,
                  )
                }
              },
              fn() { stop([]) },
            )
          }
        },
        stop,
      )
    }
  }
}

/// The elements of a list from its first cons bit at `position`, which the
/// list's bit table has read as set, to its end, with `units` to spend:
/// `ok` gets the position after the end, the root of their sequence, their
/// count and serialised count, the sums of their CBOR lengths, Data
/// memories and constant memories, and the units left. The last element's
/// sequence node is built from its entry, whose CBOR length and Data memory
/// are the sequence's. `count` elements with constant memory `before` come
/// before `position`. Where it stops, `level` gets the cons bit it stopped
/// at or in, the count and constant memory of the elements before that bit,
/// the units left there and the steps down from that element.
fn items_from(
  window: ByteArray,
  flat_bits: Int,
  top: Int,
  element: Decoder<r>,
  position: Int,
  units: Int,
  count: Int,
  before: Int,
  ok: fn(Int, ByteArray, Int, ByteArray, Int, Int, Int, Int) -> r,
  level: fn(Int, Int, Int, Int, List<Within>) -> r,
) -> r {
  if units < element_units {
    level(position, count, before, units, [])
  } else {
    element(
      position + 1,
      position + 1,
      units - element_units,
      fn(middle, head, cbor, data, memory, left) {
        if middle >= flat_bits {
          level(middle, count + 1, before + memory, left, [])
        } else if builtin.read_bit(window, top - middle) {
          items_from(
            window,
            flat_bits,
            top,
            element,
            middle,
            left,
            count + 1,
            before + memory,
            fn(after, root, length, _count, payload, data_sum, memory_sum, rest) {
              let length = length + 1
              let count = builtin.serialise_data(builtin.i_data(length))
              let payload = payload + cbor
              let data_sum = data_sum + data
              ok(
                after,
                prepend_root(head, root, count, payload, data_sum),
                length,
                count,
                payload,
                data_sum,
                memory_sum + memory,
                rest,
              )
            },
            level,
          )
        } else {
          ok(middle + 1, last_root(head), 1, #"01", cbor, data, memory, left)
        }
      },
      fn(path) { level(position, count, before, units, path) },
    )
  }
}

/// The root of the one-item sequence of the value with entry `head`.
fn last_root(head: ByteArray) -> ByteArray {
  blake2b_256(
    builtin.append_bytearray(
      list_node_head,
      builtin.append_bytearray(
        head,
        builtin.append_bytearray(
          last_item_tail,
          builtin.slice_bytearray(32, 64, head),
        ),
      ),
    ),
  )
}

/// `items_from` without summaries, with `length` elements and `memory`
/// constant memory before the cons bit at `position`: `ok` gets the
/// position after the end, the count of all the elements, their constant
/// memory and the units left; where it stops, `level` gets what
/// `items_from` gives it.
fn scan_items_from(
  window: ByteArray,
  flat_bits: Int,
  top: Int,
  element: Decoder<r>,
  position: Int,
  units: Int,
  length: Int,
  memory: Int,
  ok: fn(Int, Int, Int, Int) -> r,
  level: fn(Int, Int, Int, Int, List<Within>) -> r,
) -> r {
  if units < element_units {
    level(position, length, memory, units, [])
  } else {
    element(
      position + 1,
      position + 1,
      units - element_units,
      fn(middle, _head, _cbor, _data, value_memory, left) {
        if middle >= flat_bits {
          level(middle, length + 1, memory + value_memory, left, [])
        } else if builtin.read_bit(window, top - middle) {
          scan_items_from(
            window,
            flat_bits,
            top,
            element,
            middle,
            left,
            length + 1,
            memory + value_memory,
            ok,
            level,
          )
        } else {
          ok(middle + 1, length + 1, memory + value_memory, left)
        }
      },
      fn(path) { level(position, length, memory, units, path) },
    )
  }
}

/// `items_from` for a fold, which keeps no units and constant memory and
/// reads only bits a scan has read: `ok` gets the position after the end,
/// the root of the elements' sequence, their count and serialised count,
/// the sums of their CBOR lengths and Data memories, and the units.
fn fold_items_from(
  window: ByteArray,
  top: Int,
  element: Decoder<r>,
  position: Int,
  units: Int,
  ok: fn(Int, ByteArray, Int, ByteArray, Int, Int, Int) -> r,
  stop: fn(List<Within>) -> r,
) -> r {
  element(
    position + 1,
    position + 1,
    units,
    fn(middle, head, cbor, data, _memory, left) {
      if builtin.read_bit(window, top - middle) {
        fold_items_from(
          window,
          top,
          element,
          middle,
          left,
          fn(after, root, length, _count, payload, data_sum, rest) {
            let length = length + 1
            let count = builtin.serialise_data(builtin.i_data(length))
            let payload = payload + cbor
            let data_sum = data_sum + data
            ok(
              after,
              prepend_root(head, root, count, payload, data_sum),
              length,
              count,
              payload,
              data_sum,
              rest,
            )
          },
          stop,
        )
      } else {
        ok(middle + 1, last_root(head), 1, #"01", cbor, data, left)
      }
    },
    stop,
  )
}

// ## Runs

/// The decoder a run scan uses for elements whose shape with summaries is
/// `shape`. A Walk compiles it once and runs it on every element of its
/// ticks. A scan reads no summary, so the shape's entries and wraps go
/// unused: it decodes as on the shape without summaries.
pub fn scan_decoder_v1(
  window: ByteArray,
  flat_bits: Int,
  shape: Shape,
) -> Decoder<(Int, Int, Int, Int, List<Within>)> {
  compile_v1(window, flat_bits, shape, False, True)
}

/// The decoder a run fold uses for elements whose shape with summaries is
/// `shape`.
pub fn fold_decoder_v1(
  window: ByteArray,
  flat_bits: Int,
  shape: Shape,
) -> Decoder<DataSequenceSummaryV1> {
  compile_v1(window, flat_bits, shape, True, False)
}

/// The run of list elements from the cons bit at `position` that a tick
/// decodes with the scan decoder `element`: the longest whose elements all
/// decode here within `units_per_tick`, ending before a nil bit. Gives the
/// element count, the position after the run, the elements' constant memory,
/// the units the run spent, and, when an element stopped it, the steps down
/// from that element to where it stopped.
pub fn scan_run_v1(
  window: ByteArray,
  flat_bits: Int,
  element: Decoder<(Int, Int, Int, Int, List<Within>)>,
  position: Int,
) -> (Int, Int, Int, Int, List<Within>) {
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
  element: Decoder<(Int, Int, Int, Int, List<Within>)>,
  position: Int,
  units: Int,
  count: Int,
  memory: Int,
) -> (Int, Int, Int, Int, List<Within>) {
  if position >= flat_bits {
    (count, position, memory, units_per_tick - units, [])
  } else if !builtin.read_bit(window, top - position) {
    (count, position, memory, units_per_tick - units, [])
  } else if units < element_units {
    (count, position, memory, units_per_tick - units, [])
  } else {
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
      fn(path) { (count, position, memory, units_per_tick - units, path) },
    )
  }
}

/// Prepends the `count` elements of a scanned run, from the cons bit at
/// `position`, to `sequence`, last element first, with the fold decoder
/// `element`. The run is one a scan decoded, with its checks, within its
/// units, and the stack hash binds its start and count, so every element
/// decodes again here and the fold never stops.
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
              prepend_root(
                head,
                root,
                builtin.serialise_data(builtin.i_data(length)),
                payload,
                data_sum,
              ),
              length,
              payload,
              data_sum,
            )
          },
        )
      },
      fn(_path) {
        fail @"a scanned run element no longer decodes"
      },
    )
  }
}
