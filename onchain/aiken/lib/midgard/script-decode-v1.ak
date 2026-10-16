//// Cardano-exact decoding of a serialised Plutus script, walked over several
//// transactions.
////
//// The machine reads the CBOR-wrapped Flat program the way Cardano's
//// `deserialiseScript` does at protocol version 11 and ends in one of two
//// verdicts. `Decoded` carries the canonical CEK term root, the program
//// version and the phase-2 class (a version outside 1.0.0 and 1.1.0, or a
//// free variable outside constr and case). `Refused` carries the first
//// failure in stream order as a class code and the bit offset, from the start
//// of the script bytes, where it was raised.
////
//// Each tick does bounded work. Frames live on a hash-linked stack: a step
//// opens the frames it pops from revealed preimages and commits the frames it
//// leaves behind.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_224, blake2b_256}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/cek_constant_v1.{
  ArrayConstant, BlsG1Constant, BlsG2Constant, BlsMillerLoopResultConstant,
  BooleanConstant, ByteStringConstant, ConstantTypeV1, DataConstant,
  IntegerConstant, ListConstant, PairConstant, StringConstant, UnitConstant,
  ValueConstant,
}
use midgard/cek_data_v1.{DataSequenceSummaryV1, DataSummaryV1}
use midgard/cek_proof_v1
use midgard/script_decode_bits_v1.{BlobSink, MagSink} as bits
use midgard/script_decode_bulk_v1.{KPair, Shape} as bulk

// ## Classes

pub const class_cbor_not_bytes = 1

pub const class_cbor_trailing_bytes = 2

pub const class_flat_truncated = 3

pub const class_flat_bad_padding = 4

pub const class_version_unavailable = 5

pub const class_unknown_term_tag = 6

pub const class_constr_case_before_v110 = 7

pub const class_unavailable_builtin = 8

pub const class_constr_fields_over_limit = 9

pub const class_word_overflow = 10

pub const class_open_term = 11

pub const class_unknown_constant_type = 12

pub const class_constant_type_too_large = 13

pub const class_bls_constant = 14

pub const class_invalid_utf8 = 15

pub const class_invalid_data = 16

pub const class_non_canonical_value = 17

/// `phase2_class` of a script with no phase-2 failure.
pub const phase2_none = 0

pub const language_plutus_v3 = 3

pub const language_midgard_v1 = 128

const max_constr_fields = 1024

const max_type_tags = 32

const max_value_key_bytes = 32

const min_quantity = -170141183460469231731687303715884105728

const max_quantity = 170141183460469231731687303715884105727

/// Integer varint groups read per tick.
const nat_groups_per_tick = 10

/// Magnitude bytes repacked per tick.
const magnitude_bytes_per_tick = 32

const stack_domain = #"4d6964676172645363726970744465636f6465537461636b5631"

// ## Records

pub type ScriptDecodeRecordV1 {
  Decoded {
    script_hash: ByteArray,
    language: Int,
    program_version: Int,
    term_root: ByteArray,
    phase2_class: Int,
  }
  Refused { script_hash: ByteArray, language: Int, class: Int, bit_offset: Int }
}

/// The decode record at reference input `index`, or None for a negative
/// index. The record token of this family is named by the script hash the
/// record is for, and the family only mints it onto an unspendable output
/// carrying that record, so holding the token named by the record's own hash
/// authenticates the record.
pub fn record_at_v1(
  reference_inputs: List<Input>,
  index: Int,
  script_decode_policy_id: PolicyId,
) -> Option<ScriptDecodeRecordV1> {
  if index < 0 {
    None
  } else {
    expect
        Some(Input {
          output: Output { datum: InlineDatum(raw_record), value, .. },
          ..
        })
      = list.at(reference_inputs, index)
    expect record: ScriptDecodeRecordV1 = raw_record
    expect
      assets.quantity_of(
        value,
        script_decode_policy_id,
        record_script_hash_v1(record),
      ) == 1
    Some(record)
  }
}

/// The script hash a record is for.
pub fn record_script_hash_v1(record: ScriptDecodeRecordV1) -> ByteArray {
  when record is {
    Decoded { script_hash, .. } -> script_hash
    Refused { script_hash, .. } -> script_hash
  }
}

/// The class the decode record gives a Plutus script item: 0 when the record
/// is `Decoded`, its refusal class when `Refused`. The record must be the one
/// for the item's script hash, the hash of the language tag and the payload
/// running from `payload_offset` to the item end.
pub fn item_record_class_v1(
  item: ByteArray,
  payload_offset: Int,
  language_tag: Int,
  record: Option<ScriptDecodeRecordV1>,
) -> Int {
  expect Some(found) = record
  expect
    record_script_hash_v1(found) == blake2b_224(
      bytearray.push(bytearray.drop(item, payload_offset), language_tag),
    )
  when found is {
    Decoded { .. } -> 0
    Refused { class, .. } -> class
  }
}

// ## Stack frames

pub type Frame {
  FDelay
  FLambda
  FForce
  FApplyFunction
  FApplyArgument { function: ByteArray }
  FCaseScrutinee
  FConstrFields { tag: Int, count: Int }
  FCaseBranches { scrutinee: ByteArray, count: Int }
  FTermChild { hash: ByteArray }
  FConstant { type_hash: ByteArray }
  FValueList { element: ConstantTypeV1, array: Bool, count: Int, memory: Int }
  FValueItem { summary: DataSummaryV1 }
  FValuePairFirst { second: ConstantTypeV1 }
  FValuePairSecond { first: DataSummaryV1, memory: Int }
  FAssetOuter {
    count: Int,
    previous: Option<ByteArray>,
    invalid: Bool,
    total: Int,
  }
  FAssetInner {
    currency: ByteArray,
    count: Int,
    previous: Option<ByteArray>,
    invalid: Bool,
  }
  FPairEntry { key: DataSummaryV1, value: DataSummaryV1 }
  FDataRoot
  /// `remaining` is -1 for an indefinite list and `constructor` is -1 for a
  /// plain list.
  FDataList { remaining: Int, count: Int, constructor: Int, break_after: Bool }
  /// `remaining` is -1 for an indefinite map.
  FDataMap { remaining: Int, count: Int, key: Option<DataSummaryV1> }
  /// `count` list elements decoded in bulk, from the cons bit at `start`,
  /// whose scan spent `units`.
  FValueRun { start: Int, count: Int, units: Int }
}

// ## Control

pub type NatPurpose {
  NatVersion { index: Int }
  NatInteger
  NatQuantity { token: ByteArray }
}

pub type BytesPurpose {
  BytesPlain
  BytesString
  BytesCurrency
  BytesToken
  BytesData
}

pub type BytesAcc {
  AccBlob { sink: BlobSink, utf8: Int }
  AccKey { key: ByteArray, length: Int }
}

pub type Fold {
  pairs: Bool,
  remaining: Int,
  sequence: DataSequenceSummaryV1,
}

pub type ConstFoldNext {
  NextList { element: ConstantTypeV1, array: Bool, count: Int, memory: Int }
  NextAssetInner { currency: ByteArray, count: Int, invalid: Bool }
  NextAssetOuter { invalid: Bool, total: Int }
}

pub type DataFoldNext {
  NextDataList { constructor: Int, break_after: Bool }
  NextDataMap
}

/// A read position inside a Flat byte string: the byte `position` and the
/// bytes `left` in the current chunk (0 when a chunk header comes next).
pub type Source {
  position: Int,
  left: Int,
}

/// The CBOR reader inside a Data constant. `DBytes` and `DBignum` copy
/// `remaining` bytes of a definite string or chunk; with `remaining` at 0 an
/// indefinite one reads its next chunk head or its break. `DTrail` steps over
/// the Flat chunks after the first item and then returns its value.
pub type DataMode {
  DItem
  DBytes { sink: BlobSink, remaining: Int, indefinite: Bool }
  DBignum { magnitude: MagSink, remaining: Int, indefinite: Bool }
  DFold { fold: Fold, next: DataFoldNext }
  DSkim
  DTrail { summary: DataSummaryV1 }
}

pub type Control {
  CNat {
    purpose: NatPurpose,
    start: Int,
    groups: Int,
    acc: Int,
    scale: Int,
    big: Bool,
  }
  CMagnitude { start: Int, groups: Int, emitted: Int, magnitude: MagSink }
  CTerm
  CReturn { hash: ByteArray }
  CConsBit
  CConstrFold { tag: Int, remaining: Int, root: ByteArray, length: Int }
  CCaseFold {
    scrutinee: ByteArray,
    remaining: Int,
    root: ByteArray,
    length: Int,
  }
  /// The type tag list: `pending` symbols remain to parse, the top one
  /// `top` (0 a type, 1 an applied operator, 2 a pair awaiting arguments).
  CTypes { count: Int, pending: Int, top: Int, bad: Bool, tags: List<Int> }
  CValue { ty: ConstantTypeV1 }
  CValueReturn { summary: DataSummaryV1, memory: Int }
  CListBit { element: ConstantTypeV1 }
  CAssetOuterBit
  CAssetInnerBit
  CFiller { purpose: BytesPurpose }
  CBytes { purpose: BytesPurpose, acc: BytesAcc }
  CConstFold { fold: Fold, next: ConstFoldNext }
  CData { source: Source, mode: DataMode }
  CFinal { term_root: ByteArray }
  CDone { term_root: ByteArray }
  CRefused { class: Int, offset: Int }
}

/// `version` holds the program version as `9 * major + 3 * minor + patch`,
/// each component capped at 2. `stack_root` commits the frame stack.
pub type Machine {
  cursor: Int,
  version: Int,
  binders: Int,
  constr_case_depth: Int,
  open_term: Bool,
  stack_root: ByteArray,
  control: Control,
}

pub type ScriptDecodeRunV1 {
  script_hash: ByteArray,
  language: Int,
  flat_start: Int,
  flat_length: Int,
  byte_length: Int,
  machine: Machine,
}

pub type DecodeEnv {
  window: ByteArray,
  flat_length: Int,
  flat_bits: Int,
  base_bits: Int,
  trailing: Bool,
}

// ## Stack

pub fn empty_stack_root() -> ByteArray {
  blake2b_256(stack_domain)
}

pub fn push_stack_root(frame: Frame, below: ByteArray) -> ByteArray {
  stack_domain
    |> bytearray.concat(cbor.serialise(frame))
    |> bytearray.concat(below)
    |> blake2b_256
}

/// The root of a stack whose top frames are `frames` (top first) over `base`.
pub fn stack_root_of(frames: List<Frame>, base: ByteArray) -> ByteArray {
  when frames is {
    [] -> base
    [frame, ..rest] -> push_stack_root(frame, stack_root_of(rest, base))
  }
}

fn open_reveals(
  frames: List<Frame>,
  base: ByteArray,
) -> (List<Pair<Frame, ByteArray>>, ByteArray) {
  when frames is {
    [] -> ([], base)
    [frame, ..rest] -> {
      let (opened, below) = open_reveals(rest, base)
      ([Pair(frame, below), ..opened], push_stack_root(frame, below))
    }
  }
}

type Exec {
  cursor: Int,
  version: Int,
  binders: Int,
  depth: Int,
  open: Bool,
  mem: List<Frame>,
  revealed: List<Pair<Frame, ByteArray>>,
  hashed: ByteArray,
  control: Control,
}

fn pop(ex: Exec) -> Option<(Frame, Exec)> {
  when ex.mem is {
    [frame, ..rest] -> Some((frame, Exec { ..ex, mem: rest }))
    [] ->
      when ex.revealed is {
        [Pair(frame, below), ..rest] ->
          Some((frame, Exec { ..ex, revealed: rest, hashed: below }))
        [] -> None
      }
  }
}

fn push(ex: Exec, frame: Frame) -> Exec {
  Exec { ..ex, mem: [frame, ..ex.mem] }
}

fn stack_is_empty(ex: Exec) -> Bool {
  ex.mem == [] && ex.revealed == [] && ex.hashed == empty_stack_root()
}

fn to(ex: Exec, control: Control) -> Option<Exec> {
  Some(Exec { ..ex, control })
}

fn at(ex: Exec, cursor: Int, control: Control) -> Option<Exec> {
  Some(Exec { ..ex, cursor, control })
}

fn refuse(env: DecodeEnv, ex: Exec, class: Int, position: Int) -> Option<Exec> {
  Some(
    Exec { ..ex, control: CRefused { class, offset: env.base_bits + position } },
  )
}

fn have(env: DecodeEnv, position: Int, width: Int) -> Bool {
  position + width <= env.flat_bits
}

fn read(env: DecodeEnv, position: Int, width: Int) -> Int {
  bits.read_bits(env.window, position, width)
}

fn byte(env: DecodeEnv, index: Int) -> Int {
  builtin.index_bytearray(env.window, index)
}

// ## Wrapper and setup

pub type WrapperV1 {
  Wrapped { flat_start: Int, flat_length: Int }
  NotBytes
}

/// Cardano reads the script as one definite CBOR byte string (cborg
/// `decodeBytes`; a non-minimal length head is accepted).
pub fn parse_wrapper_v1(bytes: ByteArray) -> WrapperV1 {
  let length = bytearray.length(bytes)
  if length == 0 {
    NotBytes
  } else {
    let initial = builtin.index_bytearray(bytes, 0)
    let additional = initial % 32
    if initial / 32 != 2 || additional > 27 {
      NotBytes
    } else {
      let width = if additional < 24 {
        0
      } else if additional == 24 {
        1
      } else if additional == 25 {
        2
      } else if additional == 26 {
        4
      } else {
        8
      }
      if 1 + width > length {
        NotBytes
      } else {
        let flat_length = if width == 0 {
          additional
        } else {
          builtin.bytearray_to_integer(
            True,
            builtin.slice_bytearray(1, width, bytes),
          )
        }
        if 1 + width + flat_length > length {
          NotBytes
        } else {
          Wrapped { flat_start: 1 + width, flat_length }
        }
      }
    }
  }
}

pub fn initial_machine_v1() -> Machine {
  Machine {
    cursor: 0,
    version: 0,
    binders: 0,
    constr_case_depth: 0,
    open_term: False,
    stack_root: empty_stack_root(),
    control: CNat {
      purpose: NatVersion { index: 0 },
      start: 0,
      groups: 0,
      acc: 0,
      scale: 1,
      big: False,
    },
  }
}

/// The run a bind creates for `script_bytes`.
pub fn initial_run_v1(
  script_hash: ByteArray,
  language: Int,
  script_bytes: ByteArray,
) -> ScriptDecodeRunV1 {
  let byte_length = bytearray.length(script_bytes)
  when parse_wrapper_v1(script_bytes) is {
    Wrapped { flat_start, flat_length } ->
      ScriptDecodeRunV1 {
        script_hash,
        language,
        flat_start,
        flat_length,
        byte_length,
        machine: initial_machine_v1(),
      }
    NotBytes ->
      ScriptDecodeRunV1 {
        script_hash,
        language,
        flat_start: 0,
        flat_length: 0,
        byte_length,
        machine: Machine {
          ..initial_machine_v1(),
          control: CRefused { class: class_cbor_not_bytes, offset: 0 },
        },
      }
  }
}

pub fn decode_env_v1(
  script_bytes: ByteArray,
  run: ScriptDecodeRunV1,
) -> DecodeEnv {
  DecodeEnv {
    window: bytearray.concat(
      builtin.slice_bytearray(run.flat_start, run.flat_length, script_bytes),
      #"00",
    ),
    flat_length: run.flat_length,
    flat_bits: 8 * run.flat_length,
    base_bits: 8 * run.flat_start,
    trailing: run.flat_start + run.flat_length < run.byte_length,
  }
}

pub fn is_terminal(control: Control) -> Bool {
  when control is {
    CDone { .. } -> True
    CRefused { .. } -> True
    _ -> False
  }
}

/// The record of a finished run.
pub fn record_of_v1(run: ScriptDecodeRunV1) -> Option<ScriptDecodeRecordV1> {
  let machine = run.machine
  when machine.control is {
    CDone { term_root } -> {
      let major = machine.version / 9
      let minor = machine.version / 3 % 3
      let patch = machine.version % 3
      let available = major == 1 && minor <= 1 && patch == 0
      Some(
        Decoded {
          script_hash: run.script_hash,
          language: run.language,
          program_version: if available {
            minor
          } else {
            -1
          },
          term_root,
          phase2_class: if !available {
            class_version_unavailable
          } else if machine.open_term {
            class_open_term
          } else {
            phase2_none
          },
        },
      )
    }
    CRefused { class, offset } ->
      Some(
        Refused {
          script_hash: run.script_hash,
          language: run.language,
          class,
          bit_offset: offset,
        },
      )
    _ -> None
  }
}

// ## Step

/// Runs up to `fuel` ticks. `reveals` are the stack's top frames (top first)
/// over the root `base`; a tick that needs a frame below them is not taken.
pub fn step_v1(
  env: DecodeEnv,
  machine: Machine,
  reveals: List<Frame>,
  base: ByteArray,
  fuel: Int,
) -> Machine {
  let (revealed, root) = open_reveals(reveals, base)
  expect root == machine.stack_root
  let done = run_ticks(env, start_exec(machine, revealed), fuel)
  machine_of(done, stack_root_of(done.mem, done.hashed))
}

/// The prover's view of a step: the whole stack is `frames` (top first) and
/// the step reveals its top `visible` frames. Returns the machine `step_v1`
/// reaches and the whole stack after it.
pub fn step_with_frames_v1(
  env: DecodeEnv,
  machine: Machine,
  frames: List<Frame>,
  visible: Int,
  fuel: Int,
) -> (Machine, List<Frame>) {
  let hidden = list.drop(frames, visible)
  let (revealed, root) = open_reveals(
    list.take(frames, visible),
    stack_root_of(hidden, empty_stack_root()),
  )
  expect root == machine.stack_root
  let done = run_ticks(env, start_exec(machine, revealed), fuel)
  (
    machine_of(done, stack_root_of(done.mem, done.hashed)),
    list.concat(
      done.mem,
      list.concat(list.map(done.revealed, fn(pair) { pair.1st }), hidden),
    ),
  )
}

fn start_exec(machine: Machine, revealed: List<Pair<Frame, ByteArray>>) -> Exec {
  Exec {
    cursor: machine.cursor,
    version: machine.version,
    binders: machine.binders,
    depth: machine.constr_case_depth,
    open: machine.open_term,
    mem: [],
    revealed,
    hashed: machine.stack_root,
    control: machine.control,
  }
}

fn machine_of(done: Exec, stack_root: ByteArray) -> Machine {
  Machine {
    cursor: done.cursor,
    version: done.version,
    binders: done.binders,
    constr_case_depth: done.depth,
    open_term: done.open,
    stack_root,
    control: done.control,
  }
}

/// Runs up to `fuel` ticks. The bulk decoders for the list element type the
/// Walk starts scanning or folding are compiled once, for all its ticks.
fn run_ticks(env: DecodeEnv, ex: Exec, fuel: Int) -> Exec {
  let scan_ty = when ex.control is {
    CListBit { element } -> element
    _ -> UnitConstant
  }
  let fold_ty = when ex.control is {
    CConstFold { next: NextList { element, .. }, .. } -> element
    _ -> UnitConstant
  }
  run_compiled(
    env,
    ex,
    fuel,
    scan_ty,
    bulk.scan_decoder_v1(env.window, env.flat_bits, scan_ty),
    fold_ty,
    bulk.fold_decoder_v1(env.window, env.flat_bits, fold_ty),
  )
}

fn run_compiled(
  env: DecodeEnv,
  ex: Exec,
  fuel: Int,
  scan_ty: ConstantTypeV1,
  scan: ScanDecoder,
  fold_ty: ConstantTypeV1,
  fold: FoldDecoder,
) -> Exec {
  if fuel <= 0 || is_terminal(ex.control) {
    ex
  } else {
    when tick(env, ex, scan_ty, scan, fold_ty, fold) is {
      None -> ex
      Some(next) ->
        run_compiled(env, next, fuel - 1, scan_ty, scan, fold_ty, fold)
    }
  }
}

type ScanDecoder =
  bulk.Decoder<(Int, Int, Int, Int)>

type FoldDecoder =
  bulk.Decoder<DataSequenceSummaryV1>

fn tick(
  env: DecodeEnv,
  ex: Exec,
  scan_ty: ConstantTypeV1,
  scan: ScanDecoder,
  fold_ty: ConstantTypeV1,
  fold: FoldDecoder,
) -> Option<Exec> {
  when ex.control is {
    CTerm -> tick_term(env, ex)
    CReturn { hash } -> tick_return(ex, hash)
    CConsBit -> tick_cons_bit(env, ex)
    CConstrFold { tag, remaining, root, length } ->
      if remaining == 0 {
        to(
          Exec { ..ex, depth: ex.depth - 1 },
          CReturn { hash: cek_proof_v1.hash_constr_term_v1(tag, length, root) },
        )
      } else {
        when pop(ex) is {
          None -> None
          Some((frame, popped)) -> {
            expect FTermChild { hash } = frame
            to(
              popped,
              CConstrFold {
                tag,
                remaining: remaining - 1,
                root: cek_proof_v1.hash_sequence_node_v1(hash, root, length + 1),
                length: length + 1,
              },
            )
          }
        }
      }
    CCaseFold { scrutinee, remaining, root, length } ->
      if remaining == 0 {
        to(
          Exec { ..ex, depth: ex.depth - 1 },
          CReturn {
            hash: cek_proof_v1.hash_case_term_v1(scrutinee, length, root),
          },
        )
      } else {
        when pop(ex) is {
          None -> None
          Some((frame, popped)) -> {
            expect FTermChild { hash } = frame
            to(
              popped,
              CCaseFold {
                scrutinee,
                remaining: remaining - 1,
                root: cek_proof_v1.hash_sequence_node_v1(hash, root, length + 1),
                length: length + 1,
              },
            )
          }
        }
      }
    CNat { purpose, start, groups, acc, scale, big } ->
      tick_nat(env, ex, purpose, start, groups, acc, scale, big)
    CMagnitude { start, groups, emitted, magnitude } ->
      tick_magnitude(env, ex, start, groups, emitted, magnitude)
    CTypes { count, pending, top, bad, tags } ->
      tick_types(env, ex, count, pending, top, bad, tags)
    CValue { ty } -> tick_value(env, ex, ty)
    CValueReturn { summary, memory } ->
      tick_value_return(env, ex, summary, memory)
    CListBit { element } ->
      tick_list_bit(
        env,
        ex,
        element,
        if element == scan_ty {
          scan
        } else {
          bulk.scan_decoder_v1(env.window, env.flat_bits, element)
        },
      )
    CAssetOuterBit -> tick_asset_outer_bit(env, ex)
    CAssetInnerBit -> tick_asset_inner_bit(env, ex)
    CFiller { purpose } -> tick_filler(env, ex, purpose)
    CBytes { purpose, acc } -> tick_bytes(env, ex, purpose, acc)
    CConstFold { fold: folding, next } ->
      tick_const_fold(env, ex, folding, next, fold_ty, fold)
    CData { source, mode } -> tick_data(env, ex, source, mode)
    CFinal { term_root } -> tick_final(env, ex, term_root)
    _ -> None
  }
}

// ## Terms

type Word64 {
  WordOk { value: Int, next: Int }
  WordTruncated { at: Int }
  WordTooLarge { at: Int }
}

/// Flat's `Word64`: little-endian 7-bit groups, where a tenth group may only
/// carry the top bit and must end the word.
fn read_word64(
  env: DecodeEnv,
  position: Int,
  group: Int,
  acc: Int,
  scale: Int,
) -> Word64 {
  if !have(env, position, 8) {
    WordTruncated { at: position }
  } else {
    let v = read(env, position, 8)
    let payload = v % 128
    if group == 9 && ( v >= 128 || payload > 1 ) {
      WordTooLarge { at: position + 8 }
    } else if v >= 128 {
      read_word64(
        env,
        position + 8,
        group + 1,
        acc + payload * scale,
        scale * 128,
      )
    } else {
      WordOk { value: acc + payload * scale, next: position + 8 }
    }
  }
}

fn at_least_v110(version: Int) -> Bool {
  let major = version / 9
  major == 2 || major == 1 && version / 3 % 3 >= 1
}

fn tick_term(env: DecodeEnv, ex: Exec) -> Option<Exec> {
  let cursor = ex.cursor
  if !have(env, cursor, 4) {
    refuse(env, ex, class_flat_truncated, cursor)
  } else {
    let tag = read(env, cursor, 4)
    let c = cursor + 4
    if tag == 0 {
      when read_word64(env, c, 0, 0, 1) is {
        WordTruncated { at: p } -> refuse(env, ex, class_flat_truncated, p)
        WordTooLarge { at: p } -> refuse(env, ex, class_word_overflow, p)
        WordOk { value, next } -> {
          let free = value == 0 || value > ex.binders
          at(
            Exec { ..ex, open: ex.open || ex.depth == 0 && free },
            next,
            CReturn { hash: cek_proof_v1.hash_variable_term_v1(value) },
          )
        }
      }
    } else if tag == 1 {
      at(push(ex, FDelay), c, CTerm)
    } else if tag == 2 {
      at(Exec { ..push(ex, FLambda), binders: ex.binders + 1 }, c, CTerm)
    } else if tag == 3 {
      at(push(ex, FApplyFunction), c, CTerm)
    } else if tag == 4 {
      at(ex, c, CTypes { count: 0, pending: 1, top: 0, bad: False, tags: [] })
    } else if tag == 5 {
      at(push(ex, FForce), c, CTerm)
    } else if tag == 6 {
      at(ex, c, CReturn { hash: cek_proof_v1.hash_error_term_v1() })
    } else if tag == 7 {
      if !have(env, c, 7) {
        refuse(env, ex, class_flat_truncated, c)
      } else {
        let builtin_tag = read(env, c, 7)
        if builtin_tag > cek_proof_v1.max_builtin_tag {
          refuse(env, ex, class_unavailable_builtin, c + 7)
        } else {
          at(
            ex,
            c + 7,
            CReturn { hash: cek_proof_v1.hash_builtin_term_v1(builtin_tag) },
          )
        }
      }
    } else if tag == 8 {
      if !at_least_v110(ex.version) {
        refuse(env, ex, class_constr_case_before_v110, c)
      } else {
        when read_word64(env, c, 0, 0, 1) is {
          WordTruncated { at: p } -> refuse(env, ex, class_flat_truncated, p)
          WordTooLarge { at: p } -> refuse(env, ex, class_word_overflow, p)
          WordOk { value, next } ->
            at(
              Exec {
                ..push(ex, FConstrFields { tag: value, count: 0 }),
                depth: ex.depth + 1,
              },
              next,
              CConsBit,
            )
        }
      }
    } else if tag == 9 {
      if !at_least_v110(ex.version) {
        refuse(env, ex, class_constr_case_before_v110, c)
      } else {
        at(Exec { ..push(ex, FCaseScrutinee), depth: ex.depth + 1 }, c, CTerm)
      }
    } else {
      refuse(env, ex, class_unknown_term_tag, c)
    }
  }
}

fn tick_return(ex: Exec, hash: ByteArray) -> Option<Exec> {
  when pop(ex) is {
    None ->
      if stack_is_empty(ex) {
        to(ex, CFinal { term_root: hash })
      } else {
        None
      }
    Some((frame, popped)) ->
      when frame is {
        FDelay ->
          to(popped, CReturn { hash: cek_proof_v1.hash_delay_term_v1(hash) })
        FLambda ->
          to(
            Exec { ..popped, binders: popped.binders - 1 },
            CReturn { hash: cek_proof_v1.hash_lambda_term_v1(hash) },
          )
        FForce ->
          to(popped, CReturn { hash: cek_proof_v1.hash_force_term_v1(hash) })
        FApplyFunction ->
          to(push(popped, FApplyArgument { function: hash }), CTerm)
        FApplyArgument { function } ->
          to(
            popped,
            CReturn {
              hash: cek_proof_v1.hash_application_term_v1(function, hash),
            },
          )
        FCaseScrutinee ->
          to(
            push(popped, FCaseBranches { scrutinee: hash, count: 0 }),
            CConsBit,
          )
        FConstrFields { tag, count } ->
          to(
            push(
              push(popped, FTermChild { hash }),
              FConstrFields { tag, count: count + 1 },
            ),
            CConsBit,
          )
        FCaseBranches { scrutinee, count } ->
          to(
            push(
              push(popped, FTermChild { hash }),
              FCaseBranches { scrutinee, count: count + 1 },
            ),
            CConsBit,
          )
        _ -> fail @"term return under a non-term frame"
      }
  }
}

fn tick_cons_bit(env: DecodeEnv, ex: Exec) -> Option<Exec> {
  let cursor = ex.cursor
  if !have(env, cursor, 1) {
    refuse(env, ex, class_flat_truncated, cursor)
  } else if read(env, cursor, 1) == 1 {
    at(ex, cursor + 1, CTerm)
  } else {
    when pop(ex) is {
      None -> None
      Some((frame, popped)) ->
        when frame is {
          FConstrFields { tag, count } ->
            if count > max_constr_fields {
              refuse(env, ex, class_constr_fields_over_limit, cursor + 1)
            } else {
              at(
                popped,
                cursor + 1,
                CConstrFold {
                  tag,
                  remaining: count,
                  root: cek_proof_v1.empty_sequence_root_v1,
                  length: 0,
                },
              )
            }
          FCaseBranches { scrutinee, count } ->
            at(
              popped,
              cursor + 1,
              CCaseFold {
                scrutinee,
                remaining: count,
                root: cek_proof_v1.empty_sequence_root_v1,
                length: 0,
              },
            )
          _ -> fail @"term list bit under a non-list frame"
        }
    }
  }
}

fn tick_final(env: DecodeEnv, ex: Exec, term_root: ByteArray) -> Option<Exec> {
  when bits.next_set_bit(env.window, ex.cursor, env.flat_length) is {
    None -> refuse(env, ex, class_flat_truncated, env.flat_bits)
    Some(q) ->
      if q + 1 != env.flat_bits {
        refuse(env, ex, class_flat_bad_padding, q + 1)
      } else if env.trailing {
        refuse(env, ex, class_cbor_trailing_bytes, env.flat_bits)
      } else {
        at(ex, env.flat_bits, CDone { term_root })
      }
  }
}

// ## Naturals

fn zigzag(z: Int) -> Int {
  if z % 2 == 0 {
    z / 2
  } else {
    -(z / 2) - 1
  }
}

fn tick_nat(
  env: DecodeEnv,
  ex: Exec,
  purpose: NatPurpose,
  start: Int,
  groups: Int,
  acc: Int,
  scale: Int,
  big: Bool,
) -> Option<Exec> {
  nat_loop(
    env,
    ex,
    purpose,
    start,
    groups,
    acc,
    scale,
    big,
    nat_groups_per_tick,
  )
}

/// Exact below 20 groups (140 bits); a non-zero group past that sets `big`.
fn nat_loop(
  env: DecodeEnv,
  ex: Exec,
  purpose: NatPurpose,
  start: Int,
  groups: Int,
  acc: Int,
  scale: Int,
  big: Bool,
  budget: Int,
) -> Option<Exec> {
  let position = ex.cursor
  if budget == 0 {
    to(ex, CNat { purpose, start, groups, acc, scale, big })
  } else if !have(env, position, 8) {
    refuse(env, ex, class_flat_truncated, position)
  } else {
    let v = read(env, position, 8)
    let payload = v % 128
    let moved = Exec { ..ex, cursor: position + 8 }
    let exact = groups < 20
    let acc = if exact {
      acc + payload * scale
    } else {
      acc
    }
    let scale = if exact {
      scale * 128
    } else {
      scale
    }
    let big = big || !exact && payload > 0
    if v >= 128 {
      nat_loop(
        env,
        moved,
        purpose,
        start,
        groups + 1,
        acc,
        scale,
        big,
        budget - 1,
      )
    } else {
      nat_done(env, moved, purpose, start, groups + 1, acc, big)
    }
  }
}

fn nat_done(
  env: DecodeEnv,
  ex: Exec,
  purpose: NatPurpose,
  start: Int,
  groups: Int,
  acc: Int,
  big: Bool,
) -> Option<Exec> {
  when purpose is {
    NatVersion { index } -> {
      let component = if big || acc > 2 {
        2
      } else {
        acc
      }
      let weight = if index == 0 {
        9
      } else if index == 1 {
        3
      } else {
        1
      }
      let next = Exec { ..ex, version: ex.version + component * weight }
      if index == 2 {
        to(next, CTerm)
      } else {
        to(
          next,
          CNat {
            purpose: NatVersion { index: index + 1 },
            start: ex.cursor,
            groups: 0,
            acc: 0,
            scale: 1,
            big: False,
          },
        )
      }
    }
    NatInteger ->
      if big {
        to(
          ex,
          CMagnitude {
            start,
            groups,
            emitted: 0,
            magnitude: bits.mag_new(read(env, start + 1, 7) % 2 == 1),
          },
        )
      } else {
        let n = zigzag(acc)
        to(
          ex,
          CValueReturn {
            summary: bits.integer_summary(n),
            memory: cek_data_v1.integer_memory_size_v1(n),
          },
        )
      }
    NatQuantity { token } -> {
      let n = zigzag(acc)
      if big || n < min_quantity || n > max_quantity {
        refuse(env, ex, class_non_canonical_value, ex.cursor)
      } else {
        when pop(ex) is {
          None -> None
          Some((frame, popped)) -> {
            expect FAssetInner { currency, count, previous, invalid } = frame
            to(
              push(
                push(
                  popped,
                  FPairEntry {
                    key: bits.short_bytes_summary(token),
                    value: bits.integer_summary(n),
                  },
                ),
                FAssetInner {
                  currency,
                  count: count + 1,
                  previous,
                  invalid: invalid || n == 0,
                },
              ),
              CAssetInnerBit,
            )
          }
        }
      }
    }
  }
}

/// The 7-bit payload of group `index` of the natural at `start`, or 0 past
/// its `groups` groups.
fn group_payload(env: DecodeEnv, start: Int, groups: Int, index: Int) -> Int {
  if index >= groups {
    0
  } else {
    read(env, start + 8 * index + 1, 7)
  }
}

/// Byte `t` (from the least significant) of the magnitude `z >> 1`.
fn magnitude_byte(env: DecodeEnv, start: Int, groups: Int, t: Int) -> Int {
  let k = 8 * t + 1
  let i0 = k / 7
  let offset = k - 7 * i0
  let v = group_payload(env, start, groups, i0) + 128 * group_payload(
    env,
    start,
    groups,
    i0 + 1,
  )
  v / bits.pow2_small(offset) % 256
}

/// Bytes `from` to `to - 1` of the magnitude, most significant first, where
/// byte 0 is the most significant of `total`.
fn magnitude_bytes(
  env: DecodeEnv,
  start: Int,
  groups: Int,
  total: Int,
  from: Int,
  to: Int,
  tail: ByteArray,
) -> ByteArray {
  if to <= from {
    tail
  } else {
    let b = to - 1
    magnitude_bytes(
      env,
      start,
      groups,
      total,
      from,
      b,
      builtin.cons_bytearray(
        magnitude_byte(env, start, groups, total - 1 - b),
        tail,
      ),
    )
  }
}

fn tick_magnitude(
  env: DecodeEnv,
  ex: Exec,
  start: Int,
  groups: Int,
  emitted: Int,
  magnitude: MagSink,
) -> Option<Exec> {
  let total = ( 7 * groups - 1 + 7 ) / 8
  if emitted >= total {
    let (summary, words) = bits.mag_finish(magnitude)
    to(ex, CValueReturn { summary, memory: words })
  } else {
    let upto = if emitted + magnitude_bytes_per_tick < total {
      emitted + magnitude_bytes_per_tick
    } else {
      total
    }
    to(
      ex,
      CMagnitude {
        start,
        groups,
        emitted: upto,
        magnitude: bits.mag_push(
          magnitude,
          magnitude_bytes(env, start, groups, total, emitted, upto, #""),
        ),
      },
    )
  }
}

// ## Constant types

/// One type tag through the kind grammar: `pending` types remain, the next
/// symbol sits under `top` (0 a type, 1 one application, 2 two).
fn kind_step(pending: Int, top: Int, bad: Bool, tag: Int) -> (Int, Int, Bool) {
  if bad || pending == 0 {
    (pending, top, True)
  } else if tag == 7 {
    if top == 2 {
      (pending, top, True)
    } else {
      (pending, top + 1, False)
    }
  } else if tag == 5 || tag == 12 {
    if top == 1 {
      (pending, 0, False)
    } else {
      (pending, top, True)
    }
  } else if tag == 6 {
    if top == 2 {
      (pending + 1, 0, False)
    } else {
      (pending, top, True)
    }
  } else if tag == 14 || tag == 15 {
    (pending, top, True)
  } else if top == 0 {
    (pending - 1, 0, False)
  } else {
    (pending, top, True)
  }
}

fn parse_type(tags: List<Int>) -> (ConstantTypeV1, List<Int>) {
  expect [tag, ..rest] = tags
  if tag == 0 {
    (IntegerConstant, rest)
  } else if tag == 1 {
    (ByteStringConstant, rest)
  } else if tag == 2 {
    (StringConstant, rest)
  } else if tag == 3 {
    (UnitConstant, rest)
  } else if tag == 4 {
    (BooleanConstant, rest)
  } else if tag == 5 {
    let (element, after) = parse_type(rest)
    (ListConstant { element }, after)
  } else if tag == 6 {
    let (first, middle) = parse_type(rest)
    let (second, after) = parse_type(middle)
    (PairConstant { first, second }, after)
  } else if tag == 8 {
    (DataConstant, rest)
  } else if tag == 9 {
    (BlsG1Constant, rest)
  } else if tag == 10 {
    (BlsG2Constant, rest)
  } else if tag == 11 {
    (BlsMillerLoopResultConstant, rest)
  } else if tag == 12 {
    let (element, after) = parse_type(rest)
    (ArrayConstant { element }, after)
  } else {
    expect tag == 13
    (ValueConstant, rest)
  }
}

const type_tags_per_tick = 8

fn tick_types(
  env: DecodeEnv,
  ex: Exec,
  count: Int,
  pending: Int,
  top: Int,
  bad: Bool,
  tags: List<Int>,
) -> Option<Exec> {
  types_loop(
    env,
    ex.cursor,
    ex,
    count,
    pending,
    top,
    bad,
    tags,
    type_tags_per_tick,
  )
}

fn types_loop(
  env: DecodeEnv,
  c: Int,
  ex: Exec,
  count: Int,
  pending: Int,
  top: Int,
  bad: Bool,
  tags: List<Int>,
  budget: Int,
) -> Option<Exec> {
  if budget == 0 {
    at(ex, c, CTypes { count, pending, top, bad, tags })
  } else if !have(env, c, 1) {
    refuse(env, ex, class_flat_truncated, c)
  } else if read(env, c, 1) == 0 {
    let after = c + 1
    if bad || pending != 0 {
      refuse(env, ex, class_unknown_constant_type, after)
    } else if count > max_type_tags {
      refuse(env, ex, class_constant_type_too_large, after)
    } else {
      let ordered = list.reverse(tags)
      let (ty, _) = parse_type(ordered)
      at(
        push(
          ex,
          FConstant {
            type_hash: cek_proof_v1.hash_blob_chunk_v1(cbor.serialise(ordered)),
          },
        ),
        after,
        CValue { ty },
      )
    }
  } else if !have(env, c + 1, 4) {
    refuse(env, ex, class_flat_truncated, c + 1)
  } else {
    let tag = read(env, c + 1, 4)
    let (pending, top, bad) = kind_step(pending, top, bad, tag)
    let tags = if tag != 7 && count + 1 <= max_type_tags {
      [tag, ..tags]
    } else {
      tags
    }
    types_loop(env, c + 5, ex, count + 1, pending, top, bad, tags, budget - 1)
  }
}

// ## Constant values

fn unit_summary() -> DataSummaryV1 {
  cek_data_v1.small_constr_data_summary_v1(
    0,
    cek_data_v1.empty_data_list_summary_v1(),
  )
}

/// Decodes the value of `ty` in bulk where its bits allow, and otherwise as
/// one node, after decoding what it can of a pair's components.
fn tick_value(env: DecodeEnv, ex: Exec, ty: ConstantTypeV1) -> Option<Exec> {
  piece(
    env,
    ex,
    ex.cursor,
    ty,
    bulk.shape_v1(ty, True),
    bulk.units_per_tick,
    fn(held) { held },
    fn(after, entry, cbor, data, memory, left) {
      Some(
        climb(
          env,
          Exec { ..ex, cursor: after },
          bulk.summary_of_v1(entry, cbor, data),
          memory,
          left,
        ),
      )
    },
  )
}

/// Decodes the value of `ty` at `cursor` while `units` last. A value its
/// bits decode in bulk goes to `ok` with the position after it, its entry,
/// CBOR length, Data memory and constant memory, and the units left. A pair
/// is decoded component by component; the first value that does not decode
/// in bulk is decoded as one node, under the frames `pending` pushes.
fn piece(
  env: DecodeEnv,
  ex: Exec,
  cursor: Int,
  ty: ConstantTypeV1,
  shape: Shape,
  units: Int,
  pending: fn(Exec) -> Exec,
  ok: fn(Int, ByteArray, Int, Int, Int, Int) -> Option<Exec>,
) -> Option<Exec> {
  let node = fn() { node_value(env, pending(Exec { ..ex, cursor }), ty) }
  if units < 0 {
    node()
  } else {
    bulk.leaf_v1(
      env.window,
      env.flat_bits,
      shape,
      cursor,
      fn(after, entry, cbor, data, memory) {
        ok(after, entry, cbor, data, memory, units)
      },
      fn(kind) {
        when kind is {
          KPair { first: first_shape, second: second_shape, .. } -> {
            expect PairConstant { first, second } = ty
            piece(
              env,
              ex,
              cursor,
              first,
              first_shape,
              units - bulk.pair_units,
              fn(held) { push(pending(held), FValuePairFirst { second }) },
              fn(middle, a, a_cbor, a_data, a_memory, left) {
                piece(
                  env,
                  ex,
                  middle,
                  second,
                  second_shape,
                  left,
                  fn(held) {
                    push(
                      pending(held),
                      FValuePairSecond {
                        first: bulk.summary_of_v1(a, a_cbor, a_data),
                        memory: a_memory,
                      },
                    )
                  },
                  fn(after, b, b_cbor, b_data, b_memory, rest) {
                    let payload = a_cbor + b_cbor
                    let data = a_data + b_data
                    ok(
                      after,
                      bulk.pair_entry_v1(a, b, payload, data),
                      4 + payload,
                      4 + data,
                      a_memory + b_memory,
                      rest,
                    )
                  },
                )
              },
            )
          }
          _ -> {
            let general = bulk.compile_kind_v1(
              env.window,
              env.flat_bits,
              kind,
              True,
            )
            general(cursor, cursor, units, ok, fn(_left) { node() })
          }
        }
      },
      node,
    )
  }
}

/// One node of a constant value: a leaf, or the frame of a list, pair or
/// value.
fn node_value(env: DecodeEnv, ex: Exec, ty: ConstantTypeV1) -> Option<Exec> {
  let cursor = ex.cursor
  when ty is {
    IntegerConstant ->
      to(
        ex,
        CNat {
          purpose: NatInteger,
          start: cursor,
          groups: 0,
          acc: 0,
          scale: 1,
          big: False,
        },
      )
    ByteStringConstant -> to(ex, CFiller { purpose: BytesPlain })
    StringConstant -> to(ex, CFiller { purpose: BytesString })
    DataConstant -> to(ex, CFiller { purpose: BytesData })
    UnitConstant -> to(ex, CValueReturn { summary: unit_summary(), memory: 1 })
    BooleanConstant ->
      if !have(env, cursor, 1) {
        refuse(env, ex, class_flat_truncated, cursor)
      } else {
        at(
          ex,
          cursor + 1,
          CValueReturn {
            summary: cek_data_v1.small_constr_data_summary_v1(
              read(env, cursor, 1),
              cek_data_v1.empty_data_list_summary_v1(),
            ),
            memory: 1,
          },
        )
      }
    ListConstant { element } ->
      to(
        push(ex, FValueList { element, array: False, count: 0, memory: 0 }),
        CListBit { element },
      )
    ArrayConstant { element } ->
      to(
        push(ex, FValueList { element, array: True, count: 0, memory: 0 }),
        CListBit { element },
      )
    PairConstant { first, second } ->
      to(push(ex, FValuePairFirst { second }), CValue { ty: first })
    ValueConstant ->
      to(
        push(
          ex,
          FAssetOuter { count: 0, previous: None, invalid: False, total: 0 },
        ),
        CAssetOuterBit,
      )
    _ -> refuse(env, ex, class_bls_constant, cursor)
  }
}

fn tick_value_return(
  env: DecodeEnv,
  ex: Exec,
  summary: DataSummaryV1,
  memory: Int,
) -> Option<Exec> {
  when pop(ex) is {
    None -> None
    Some((frame, popped)) ->
      ascend(env, popped, frame, summary, memory, bulk.units_per_tick)
  }
}

/// Returns a decoded value to the frames above it while `units` last and the
/// frames are revealed.
fn climb(
  env: DecodeEnv,
  ex: Exec,
  summary: DataSummaryV1,
  memory: Int,
  units: Int,
) -> Exec {
  let held = Exec { ..ex, control: CValueReturn { summary, memory } }
  if units < 0 {
    held
  } else {
    when pop(ex) is {
      None -> held
      Some((frame, popped)) -> {
        expect Some(next) = ascend(env, popped, frame, summary, memory, units)
        next
      }
    }
  }
}

/// Returns a decoded value to `frame`.
fn ascend(
  env: DecodeEnv,
  ex: Exec,
  frame: Frame,
  summary: DataSummaryV1,
  memory: Int,
  units: Int,
) -> Option<Exec> {
  when frame is {
    FConstant { type_hash } ->
      to(
        ex,
        CReturn {
          hash: cek_proof_v1.hash_constant_term_v1(
            cek_proof_v1.hash_constant_value_v1(
              type_hash,
              summary.root,
              summary.cbor_length,
              summary.root,
              memory,
            ),
          ),
        },
      )
    FValueList { element, array, count, memory: total } ->
      to(
        push(
          push(ex, FValueItem { summary }),
          FValueList {
            element,
            array,
            count: count + 1,
            memory: total + memory,
          },
        ),
        CListBit { element },
      )
    FValuePairFirst { second } ->
      piece(
        env,
        ex,
        ex.cursor,
        second,
        bulk.shape_v1(second, True),
        units,
        fn(held) { push(held, FValuePairSecond { first: summary, memory }) },
        fn(after, entry, cbor, data, second_memory, left) {
          Some(
            climb(
              env,
              Exec { ..ex, cursor: after },
              bulk.pair_summary_v1(
                summary,
                bulk.summary_of_v1(entry, cbor, data),
              ),
              memory + second_memory,
              left - bulk.pair_units,
            ),
          )
        },
      )
    FValuePairSecond { first, memory: first_memory } ->
      Some(
        climb(
          env,
          ex,
          bulk.pair_summary_v1(first, summary),
          first_memory + memory,
          units - bulk.pair_units,
        ),
      )
    _ -> fail @"constant value under a non-constant frame"
  }
}

/// At a cons bit, decodes the run of elements that decode in bulk, or else
/// the next element as a value.
fn tick_list_bit(
  env: DecodeEnv,
  ex: Exec,
  element: ConstantTypeV1,
  scan: ScanDecoder,
) -> Option<Exec> {
  let cursor = ex.cursor
  if !have(env, cursor, 1) {
    refuse(env, ex, class_flat_truncated, cursor)
  } else if read(env, cursor, 1) == 1 {
    let (run, after, run_memory, spent) = bulk.scan_run_v1(
      env.window,
      env.flat_bits,
      scan,
      cursor,
    )
    if run == 0 {
      at(ex, cursor + 1, CValue { ty: element })
    } else {
      when pop(ex) is {
        None -> None
        Some((frame, popped)) -> {
          expect FValueList { array, count, memory, .. } = frame
          at(
            push(
              push(
                popped,
                FValueRun { start: cursor, count: run, units: spent },
              ),
              FValueList {
                element,
                array,
                count: count + run,
                memory: memory + run_memory,
              },
            ),
            after,
            CListBit { element },
          )
        }
      }
    }
  } else {
    when pop(ex) is {
      None -> None
      Some((frame, popped)) -> {
        expect FValueList { array, count, memory, .. } = frame
        at(
          popped,
          cursor + 1,
          CConstFold {
            fold: Fold {
              pairs: False,
              remaining: count,
              sequence: cek_data_v1.empty_data_list_summary_v1(),
            },
            next: NextList { element, array, count, memory },
          },
        )
      }
    }
  }
}

fn tick_asset_outer_bit(env: DecodeEnv, ex: Exec) -> Option<Exec> {
  let cursor = ex.cursor
  if !have(env, cursor, 1) {
    refuse(env, ex, class_flat_truncated, cursor)
  } else if read(env, cursor, 1) == 1 {
    at(ex, cursor + 1, CFiller { purpose: BytesCurrency })
  } else {
    when pop(ex) is {
      None -> None
      Some((frame, popped)) -> {
        expect FAssetOuter { count, invalid, total, .. } = frame
        at(
          popped,
          cursor + 1,
          CConstFold {
            fold: Fold {
              pairs: True,
              remaining: count,
              sequence: cek_data_v1.empty_data_pair_summary_v1(),
            },
            next: NextAssetOuter { invalid, total },
          },
        )
      }
    }
  }
}

fn tick_asset_inner_bit(env: DecodeEnv, ex: Exec) -> Option<Exec> {
  let cursor = ex.cursor
  if !have(env, cursor, 1) {
    refuse(env, ex, class_flat_truncated, cursor)
  } else if read(env, cursor, 1) == 1 {
    at(ex, cursor + 1, CFiller { purpose: BytesToken })
  } else {
    when pop(ex) is {
      None -> None
      Some((frame, popped)) -> {
        expect FAssetInner { currency, count, invalid, .. } = frame
        at(
          popped,
          cursor + 1,
          CConstFold {
            fold: Fold {
              pairs: True,
              remaining: count,
              sequence: cek_data_v1.empty_data_pair_summary_v1(),
            },
            next: NextAssetInner { currency, count, invalid },
          },
        )
      }
    }
  }
}

/// Folds one popped entry of a Data list or map into the sequence.
fn fold_one(fold: Fold, frame: Frame) -> Fold {
  if fold.pairs {
    expect FPairEntry { key, value } = frame
    Fold {
      ..fold,
      remaining: fold.remaining - 1,
      sequence: cek_data_v1.prepend_data_pair_summary_v1(
        key,
        value,
        fold.sequence,
      ),
    }
  } else {
    expect FValueItem { summary } = frame
    Fold {
      ..fold,
      remaining: fold.remaining - 1,
      sequence: bulk.prepend_v1(summary, fold.sequence),
    }
  }
}

/// The units folding `frame` costs.
fn entry_units(frame: Frame) -> Int {
  when frame is {
    FPairEntry { .. } -> fold_pair_units
    FValueItem { .. } -> fold_item_units
    FValueRun { units, .. } -> units
    _ -> fail @"a non-entry frame in a constant fold"
  }
}

/// Folds one entry into the sequence. The frame is revealed against the
/// committed stack hash, and the machine pushes pair entries only under a
/// map fold and items and runs only under a list fold, so the frame's kind
/// fixes the fold's.
fn fold_entry(
  env: DecodeEnv,
  fold: Fold,
  next: ConstFoldNext,
  frame: Frame,
  folder_ty: ConstantTypeV1,
  folder: FoldDecoder,
) -> Fold {
  when frame is {
    FPairEntry { key, value } ->
      Fold {
        ..fold,
        remaining: fold.remaining - 1,
        sequence: cek_data_v1.prepend_data_pair_summary_v1(
          key,
          value,
          fold.sequence,
        ),
      }
    FValueItem { summary } ->
      Fold {
        ..fold,
        remaining: fold.remaining - 1,
        sequence: bulk.prepend_v1(summary, fold.sequence),
      }
    FValueRun { start, count, .. } -> {
      expect NextList { element, .. } = next
      Fold {
        ..fold,
        remaining: fold.remaining - count,
        sequence: bulk.fold_run_v1(
          if element == folder_ty {
            folder
          } else {
            bulk.fold_decoder_v1(env.window, env.flat_bits, element)
          },
          start,
          count,
          fold.sequence,
        ),
      }
    }
    _ -> fail @"a non-entry frame in a constant fold"
  }
}

/// Units charged for folding one pair entry and one list item. Each entry is
/// a revealed frame, so its charge covers opening the frame as well as
/// folding it.
const fold_pair_units = 12

const fold_item_units = 8

/// Folds further entries while each fits in `units` and the frames are
/// revealed.
fn fold_more(
  env: DecodeEnv,
  ex: Exec,
  fold: Fold,
  next: ConstFoldNext,
  units: Int,
  folder_ty: ConstantTypeV1,
  folder: FoldDecoder,
) -> Exec {
  let held = Exec { ..ex, control: CConstFold { fold, next } }
  if fold.remaining == 0 {
    held
  } else {
    when pop(ex) is {
      None -> held
      Some((frame, popped)) -> {
        let cost = entry_units(frame)
        if cost > units {
          held
        } else {
          fold_more(
            env,
            popped,
            fold_entry(env, fold, next, frame, folder_ty, folder),
            next,
            units - cost,
            folder_ty,
            folder,
          )
        }
      }
    }
  }
}

fn tick_const_fold(
  env: DecodeEnv,
  ex: Exec,
  fold: Fold,
  next: ConstFoldNext,
  folder_ty: ConstantTypeV1,
  folder: FoldDecoder,
) -> Option<Exec> {
  if fold.remaining > 0 {
    when pop(ex) is {
      None -> None
      Some((frame, popped)) ->
        Some(
          fold_more(
            env,
            popped,
            fold_entry(env, fold, next, frame, folder_ty, folder),
            next,
            bulk.units_per_tick - entry_units(frame),
            folder_ty,
            folder,
          ),
        )
    }
  } else {
    when next is {
      NextList { array, count, memory, .. } ->
        to(
          ex,
          CValueReturn {
            summary: bulk.list_data_summary_v1(fold.sequence),
            memory: if array {
              count
            } else {
              memory
            },
          },
        )
      NextAssetInner { currency, count, invalid } ->
        when pop(ex) is {
          None -> None
          Some((frame, popped)) -> {
            expect
                FAssetOuter {
                  count: outer_count,
                  previous,
                  invalid: outer_invalid,
                  total,
                }
              = frame
            to(
              push(
                push(
                  popped,
                  FPairEntry {
                    key: bits.short_bytes_summary(currency),
                    value: cek_data_v1.map_data_summary_v1(fold.sequence),
                  },
                ),
                FAssetOuter {
                  count: outer_count + 1,
                  previous,
                  invalid: outer_invalid || invalid || count == 0,
                  total: total + count,
                },
              ),
              CAssetOuterBit,
            )
          }
        }
      NextAssetOuter { invalid, total } ->
        if invalid {
          refuse(env, ex, class_non_canonical_value, ex.cursor)
        } else {
          to(
            ex,
            CValueReturn {
              summary: cek_data_v1.map_data_summary_v1(fold.sequence),
              memory: total,
            },
          )
        }
    }
  }
}

// ## Flat byte strings

fn tick_filler(env: DecodeEnv, ex: Exec, purpose: BytesPurpose) -> Option<Exec> {
  when bits.next_set_bit(env.window, ex.cursor, env.flat_length) is {
    None -> refuse(env, ex, class_flat_truncated, env.flat_bits)
    Some(q) ->
      if ( q + 1 ) % 8 != 0 {
        refuse(env, ex, class_flat_bad_padding, q + 1)
      } else {
        when purpose is {
          BytesData ->
            at(
              push(ex, FDataRoot),
              q + 1,
              CData {
                source: Source { position: ( q + 1 ) / 8, left: 0 },
                mode: DItem,
              },
            )
          BytesCurrency ->
            at(
              ex,
              q + 1,
              CBytes { purpose, acc: AccKey { key: #"", length: 0 } },
            )
          BytesToken ->
            at(
              ex,
              q + 1,
              CBytes { purpose, acc: AccKey { key: #"", length: 0 } },
            )
          _ ->
            at(
              ex,
              q + 1,
              CBytes {
                purpose,
                acc: AccBlob { sink: bits.empty_sink(), utf8: 0 },
              },
            )
        }
      }
  }
}

fn key_follows(previous: Option<ByteArray>, key: ByteArray) -> Bool {
  when previous is {
    None -> True
    Some(prior) -> builtin.less_than_bytearray(prior, key)
  }
}

fn tick_bytes(
  env: DecodeEnv,
  ex: Exec,
  purpose: BytesPurpose,
  acc: BytesAcc,
) -> Option<Exec> {
  let cursor = ex.cursor
  let position = cursor / 8
  if position >= env.flat_length {
    refuse(env, ex, class_flat_truncated, cursor)
  } else {
    let n = byte(env, position)
    if n == 0 {
      bytes_done(env, ex, cursor + 8, purpose, acc)
    } else if position + 1 + n > env.flat_length {
      refuse(env, ex, class_flat_truncated, cursor)
    } else {
      let chunk = builtin.slice_bytearray(position + 1, n, env.window)
      let acc = when acc is {
        AccBlob { sink, utf8 } ->
          AccBlob {
            sink: bits.sink_push(sink, chunk),
            utf8: if purpose == BytesString && utf8 >= 0 {
              bits.utf8_push(utf8, chunk)
            } else {
              utf8
            },
          }
        AccKey { key, length } ->
          AccKey {
            key: if length + n <= max_value_key_bytes {
              bytearray.concat(key, chunk)
            } else {
              key
            },
            length: length + n,
          }
      }
      at(ex, cursor + 8 * ( 1 + n ), CBytes { purpose, acc })
    }
  }
}

fn bytes_done(
  env: DecodeEnv,
  ex: Exec,
  after: Int,
  purpose: BytesPurpose,
  acc: BytesAcc,
) -> Option<Exec> {
  when acc is {
    AccBlob { sink, utf8 } -> {
      let length = bits.sink_length(sink)
      if purpose == BytesString && utf8 != 0 {
        refuse(env, ex, class_invalid_utf8, after)
      } else {
        at(
          ex,
          after,
          CValueReturn {
            summary: bits.bytes_summary(length, bits.sink_root(sink)),
            memory: if purpose == BytesString {
              length / 4
            } else {
              cek_data_v1.bytes_memory_size_v1(length)
            },
          },
        )
      }
    }
    AccKey { key, length } ->
      if length > max_value_key_bytes {
        refuse(env, ex, class_non_canonical_value, after)
      } else {
        when pop(ex) is {
          None -> None
          Some((frame, popped)) ->
            if purpose == BytesCurrency {
              expect FAssetOuter { count, previous, invalid, total } = frame
              at(
                push(
                  push(
                    popped,
                    FAssetOuter {
                      count,
                      previous: Some(key),
                      invalid: invalid || !key_follows(previous, key),
                      total,
                    },
                  ),
                  FAssetInner {
                    currency: key,
                    count: 0,
                    previous: None,
                    invalid: False,
                  },
                ),
                after,
                CAssetInnerBit,
              )
            } else {
              expect FAssetInner { currency, count, previous, invalid } = frame
              at(
                push(
                  popped,
                  FAssetInner {
                    currency,
                    count,
                    previous: Some(key),
                    invalid: invalid || !key_follows(previous, key),
                  },
                ),
                after,
                CNat {
                  purpose: NatQuantity { token: key },
                  start: after,
                  groups: 0,
                  acc: 0,
                  scale: 1,
                  big: False,
                },
              )
            }
        }
      }
  }
}

// ## Data constants
//
// The Flat byte string is read in place, chunk by chunk, as the CBOR of one
// Plutus Data item under Cardano's `decodeData` rules. A CBOR failure skims to
// the end of the Flat byte string, so a truncated byte string still reports
// truncation; otherwise every failure is `InvalidData` at that end. As in
// Cardano, the constant is the value of the FIRST item: the bytes after it
// are stepped over undecoded, whatever they hold. (Stand-alone Data, such as
// datums and redeemers, is decoded elsewhere and still refuses them.)

type Got<a> {
  Got { value: a, source: Source }
  GotEnd { at: Int }
  GotTruncated { at: Int }
  GotInvalid
}

const skim_chunks_per_tick = 32

const cbor_break = 255

const max_data_bytes_chunk = 64

/// The bytes left in the current Flat chunk, opening the next one when the
/// current one is spent.
fn open_chunk(env: DecodeEnv, source: Source) -> Got<Int> {
  if source.left > 0 {
    Got { value: source.left, source }
  } else {
    let p = source.position
    if p >= env.flat_length {
      GotTruncated { at: p }
    } else {
      let n = byte(env, p)
      if n == 0 {
        GotEnd { at: p }
      } else if p + 1 + n > env.flat_length {
        GotTruncated { at: p }
      } else {
        Got { value: n, source: Source { position: p + 1, left: n } }
      }
    }
  }
}

fn take_byte(env: DecodeEnv, source: Source) -> Got<Int> {
  when open_chunk(env, source) is {
    Got { source: s, .. } ->
      Got {
        value: byte(env, s.position),
        source: Source { position: s.position + 1, left: s.left - 1 },
      }
    GotEnd { at } -> GotEnd { at }
    GotTruncated { at } -> GotTruncated { at }
    GotInvalid -> GotInvalid
  }
}

fn take_uint(env: DecodeEnv, source: Source, width: Int, acc: Int) -> Got<Int> {
  if width == 0 {
    Got { value: acc, source }
  } else {
    when take_byte(env, source) is {
      Got { value, source: s } ->
        take_uint(env, s, width - 1, acc * 256 + value)
      GotEnd { at } -> GotEnd { at }
      GotTruncated { at } -> GotTruncated { at }
      GotInvalid -> GotInvalid
    }
  }
}

/// A CBOR head after its initial byte: `(major, additional, argument)`, with
/// argument -1 for additional information 31.
fn head_from(
  env: DecodeEnv,
  initial: Int,
  source: Source,
) -> Got<(Int, Int, Int)> {
  let major = initial / 32
  let additional = initial % 32
  if additional < 24 {
    Got { value: (major, additional, additional), source }
  } else if additional == 31 {
    Got { value: (major, additional, -1), source }
  } else if additional > 27 {
    GotInvalid
  } else {
    let width = if additional == 24 {
      1
    } else if additional == 25 {
      2
    } else if additional == 26 {
      4
    } else {
      8
    }
    when take_uint(env, source, width, 0) is {
      Got { value, source: s } ->
        Got { value: (major, additional, value), source: s }
      GotEnd { at } -> GotEnd { at }
      GotTruncated { at } -> GotTruncated { at }
      GotInvalid -> GotInvalid
    }
  }
}

fn read_head(env: DecodeEnv, source: Source) -> Got<(Int, Int, Int)> {
  when take_byte(env, source) is {
    Got { value, source: s } -> head_from(env, value, s)
    GotEnd { at } -> GotEnd { at }
    GotTruncated { at } -> GotTruncated { at }
    GotInvalid -> GotInvalid
  }
}

fn skim(ex: Exec, origin: Source) -> Option<Exec> {
  to(ex, CData { source: origin, mode: DSkim })
}

fn data_stop(
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  got: Got<a>,
) -> Option<Exec> {
  when got is {
    GotEnd { at: p } -> refuse(env, ex, class_invalid_data, 8 * ( p + 1 ))
    GotTruncated { at: p } -> refuse(env, ex, class_flat_truncated, 8 * p)
    _ -> skim(ex, origin)
  }
}

fn skim_from(env: DecodeEnv, ex: Exec, p: Int, budget: Int) -> Option<Exec> {
  if budget == 0 {
    to(ex, CData { source: Source { position: p, left: 0 }, mode: DSkim })
  } else if p >= env.flat_length {
    refuse(env, ex, class_flat_truncated, 8 * p)
  } else {
    let n = byte(env, p)
    if n == 0 {
      refuse(env, ex, class_invalid_data, 8 * ( p + 1 ))
    } else if p + 1 + n > env.flat_length {
      refuse(env, ex, class_flat_truncated, 8 * p)
    } else {
      skim_from(env, ex, p + 1 + n, budget - 1)
    }
  }
}

/// Steps over the rest of the Flat byte string from the chunk head at `p`,
/// then returns `summary` as the constant's value.
fn trail_from(
  env: DecodeEnv,
  ex: Exec,
  p: Int,
  summary: DataSummaryV1,
  budget: Int,
) -> Option<Exec> {
  if budget == 0 {
    to(
      ex,
      CData {
        source: Source { position: p, left: 0 },
        mode: DTrail { summary },
      },
    )
  } else if p >= env.flat_length {
    refuse(env, ex, class_flat_truncated, 8 * p)
  } else {
    let n = byte(env, p)
    if n == 0 {
      at(ex, 8 * ( p + 1 ), CValueReturn { summary, memory: summary.memory })
    } else if p + 1 + n > env.flat_length {
      refuse(env, ex, class_flat_truncated, 8 * p)
    } else {
      trail_from(env, ex, p + 1 + n, summary, budget - 1)
    }
  }
}

/// Hands a finished item to the frame below it.
fn deliver(
  env: DecodeEnv,
  ex: Exec,
  source: Source,
  summary: DataSummaryV1,
) -> Option<Exec> {
  when pop(ex) is {
    None -> None
    Some((frame, popped)) ->
      when frame is {
        FDataRoot ->
          trail_from(
            env,
            popped,
            source.position + source.left,
            summary,
            skim_chunks_per_tick,
          )
        FDataList { remaining, count, constructor, break_after } -> {
          let pushed = push(popped, FValueItem { summary })
          if remaining == count + 1 {
            to(
              pushed,
              CData {
                source,
                mode: DFold {
                  fold: Fold {
                    pairs: False,
                    remaining: count + 1,
                    sequence: cek_data_v1.empty_data_list_summary_v1(),
                  },
                  next: NextDataList { constructor, break_after },
                },
              },
            )
          } else {
            to(
              push(
                pushed,
                FDataList {
                  remaining,
                  count: count + 1,
                  constructor,
                  break_after,
                },
              ),
              CData { source, mode: DItem },
            )
          }
        }
        FDataMap { remaining, count, key } ->
          when key is {
            None ->
              to(
                push(popped, FDataMap { remaining, count, key: Some(summary) }),
                CData { source, mode: DItem },
              )
            Some(k) -> {
              let pushed = push(popped, FPairEntry { key: k, value: summary })
              if remaining == count + 1 {
                to(
                  pushed,
                  CData {
                    source,
                    mode: DFold {
                      fold: Fold {
                        pairs: True,
                        remaining: count + 1,
                        sequence: cek_data_v1.empty_data_pair_summary_v1(),
                      },
                      next: NextDataMap,
                    },
                  },
                )
              } else {
                to(
                  push(
                    pushed,
                    FDataMap { remaining, count: count + 1, key: None },
                  ),
                  CData { source, mode: DItem },
                )
              }
            }
          }
        _ -> fail @"data item under a non-data frame"
      }
  }
}

/// Closes a list or constructor, reading the break after an indefinite tag
/// 102 array.
fn close_list(
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  source: Source,
  summary: DataSummaryV1,
  break_after: Bool,
) -> Option<Exec> {
  if !break_after {
    deliver(env, ex, source, summary)
  } else {
    when take_byte(env, source) is {
      Got { value, source: s } ->
        if value == cbor_break {
          deliver(env, ex, s, summary)
        } else {
          skim(ex, origin)
        }
      other -> data_stop(env, ex, origin, other)
    }
  }
}

fn open_list(
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  source: Source,
  argument: Int,
  constructor: Int,
  break_after: Bool,
) -> Option<Exec> {
  if argument == 0 {
    close_list(
      env,
      ex,
      origin,
      source,
      bits.constr_summary(constructor, cek_data_v1.empty_data_list_summary_v1()),
      break_after,
    )
  } else {
    to(
      push(
        ex,
        FDataList { remaining: argument, count: 0, constructor, break_after },
      ),
      CData { source, mode: DItem },
    )
  }
}

/// The fields list after a constructor tag.
fn open_fields(
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  source: Source,
  constructor: Int,
  break_after: Bool,
) -> Option<Exec> {
  when read_head(env, source) is {
    Got { value: (major, _, argument), source: s } ->
      if major != 4 {
        skim(ex, origin)
      } else {
        open_list(env, ex, origin, s, argument, constructor, break_after)
      }
    other -> data_stop(env, ex, origin, other)
  }
}

fn data_tagged(
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  additional: Int,
  tag: Int,
  source: Source,
) -> Option<Exec> {
  if tag < 0 {
    skim(ex, origin)
  } else if additional == 2 || additional == 3 {
    when read_head(env, source) is {
      Got { value: (major, _, length), source: s } ->
        if major != 2 || length > max_data_bytes_chunk {
          skim(ex, origin)
        } else {
          to(
            ex,
            CData {
              source: s,
              mode: DBignum {
                magnitude: bits.mag_new(additional == 3),
                remaining: if length < 0 {
                  0
                } else {
                  length
                },
                indefinite: length < 0,
              },
            },
          )
        }
      other -> data_stop(env, ex, origin, other)
    }
  } else if tag == 102 {
    when read_head(env, source) is {
      Got { value: (major, _, length), source: s } ->
        if major != 4 || length != 2 && length != -1 {
          skim(ex, origin)
        } else {
          when read_head(env, s) is {
            Got { value: (index_major, _, index), source: s2 } ->
              if index_major != 0 || index < 0 {
                skim(ex, origin)
              } else {
                open_fields(env, ex, origin, s2, index, length < 0)
              }
            other -> data_stop(env, ex, origin, other)
          }
        }
      other -> data_stop(env, ex, origin, other)
    }
  } else if tag >= 121 && tag <= 127 {
    open_fields(env, ex, origin, source, tag - 121, False)
  } else if tag >= 1280 && tag <= 1400 {
    open_fields(env, ex, origin, source, tag - 1273, False)
  } else {
    skim(ex, origin)
  }
}

fn data_head(
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  major: Int,
  additional: Int,
  argument: Int,
  source: Source,
) -> Option<Exec> {
  if major == 0 || major == 1 {
    if argument < 0 {
      skim(ex, origin)
    } else {
      deliver(
        env,
        ex,
        source,
        bits.integer_summary(
          if major == 0 {
            argument
          } else {
            -1 - argument
          },
        ),
      )
    }
  } else if major == 2 {
    if argument > max_data_bytes_chunk {
      skim(ex, origin)
    } else {
      to(
        ex,
        CData {
          source,
          mode: DBytes {
            sink: bits.empty_sink(),
            remaining: if argument < 0 {
              0
            } else {
              argument
            },
            indefinite: argument < 0,
          },
        },
      )
    }
  } else if major == 4 {
    open_list(env, ex, origin, source, argument, -1, False)
  } else if major == 5 {
    if argument == 0 {
      deliver(
        env,
        ex,
        source,
        cek_data_v1.map_data_summary_v1(
          cek_data_v1.empty_data_pair_summary_v1(),
        ),
      )
    } else {
      to(
        push(ex, FDataMap { remaining: argument, count: 0, key: None }),
        CData { source, mode: DItem },
      )
    }
  } else if major == 6 {
    data_tagged(env, ex, origin, additional, argument, source)
  } else {
    skim(ex, origin)
  }
}

fn data_break(ex: Exec, origin: Source, source: Source) -> Option<Exec> {
  when pop(ex) is {
    None -> None
    Some((frame, popped)) ->
      when frame is {
        FDataList { remaining, count, constructor, break_after } ->
          if remaining < 0 {
            to(
              popped,
              CData {
                source,
                mode: DFold {
                  fold: Fold {
                    pairs: False,
                    remaining: count,
                    sequence: cek_data_v1.empty_data_list_summary_v1(),
                  },
                  next: NextDataList { constructor, break_after },
                },
              },
            )
          } else {
            skim(ex, origin)
          }
        FDataMap { remaining, count, key } ->
          if remaining < 0 && key == None {
            to(
              popped,
              CData {
                source,
                mode: DFold {
                  fold: Fold {
                    pairs: True,
                    remaining: count,
                    sequence: cek_data_v1.empty_data_pair_summary_v1(),
                  },
                  next: NextDataMap,
                },
              },
            )
          } else {
            skim(ex, origin)
          }
        _ -> skim(ex, origin)
      }
  }
}

/// The next chunk head or break of an indefinite byte string.
fn next_chunk(
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  finish: fn(Source) -> Option<Exec>,
  resume: fn(Source, Int) -> Option<Exec>,
) -> Option<Exec> {
  when take_byte(env, origin) is {
    Got { value: initial, source: s } ->
      if initial == cbor_break {
        finish(s)
      } else {
        when head_from(env, initial, s) is {
          Got { value: (major, _, length), source: s2 } ->
            if major != 2 || length < 0 || length > max_data_bytes_chunk {
              skim(ex, origin)
            } else {
              resume(s2, length)
            }
          other -> data_stop(env, ex, origin, other)
        }
      }
    other -> data_stop(env, ex, origin, other)
  }
}

/// Up to `remaining` bytes from the current Flat chunk.
fn take_slice(env: DecodeEnv, source: Source, remaining: Int) -> Got<ByteArray> {
  when open_chunk(env, source) is {
    Got { value: left, source: s } -> {
      let k = if remaining < left {
        remaining
      } else {
        left
      }
      Got {
        value: builtin.slice_bytearray(s.position, k, env.window),
        source: Source { position: s.position + k, left: s.left - k },
      }
    }
    GotEnd { at } -> GotEnd { at }
    GotTruncated { at } -> GotTruncated { at }
    GotInvalid -> GotInvalid
  }
}

fn tick_data(
  env: DecodeEnv,
  ex: Exec,
  source: Source,
  mode: DataMode,
) -> Option<Exec> {
  when mode is {
    DItem ->
      when take_byte(env, source) is {
        Got { value: initial, source: s } ->
          if initial == cbor_break {
            data_break(ex, source, s)
          } else {
            when head_from(env, initial, s) is {
              Got { value: (major, additional, argument), source: s2 } ->
                data_head(env, ex, source, major, additional, argument, s2)
              other -> data_stop(env, ex, source, other)
            }
          }
        other -> data_stop(env, ex, source, other)
      }
    DBytes { sink, remaining, indefinite } ->
      if remaining > 0 {
        when take_slice(env, source, remaining) is {
          Got { value: piece, source: s } ->
            to(
              ex,
              CData {
                source: s,
                mode: DBytes {
                  sink: bits.sink_push(sink, piece),
                  remaining: remaining - bytearray.length(piece),
                  indefinite,
                },
              },
            )
          other -> data_stop(env, ex, source, other)
        }
      } else {
        let finish = fn(s) {
          deliver(
            env,
            ex,
            s,
            bits.bytes_summary(bits.sink_length(sink), bits.sink_root(sink)),
          )
        }
        if indefinite {
          next_chunk(
            env,
            ex,
            source,
            finish,
            fn(s, length) {
              to(
                ex,
                CData {
                  source: s,
                  mode: DBytes { sink, remaining: length, indefinite: True },
                },
              )
            },
          )
        } else {
          finish(source)
        }
      }
    DBignum { magnitude, remaining, indefinite } ->
      if remaining > 0 {
        when take_slice(env, source, remaining) is {
          Got { value: piece, source: s } ->
            to(
              ex,
              CData {
                source: s,
                mode: DBignum {
                  magnitude: bits.mag_push(magnitude, piece),
                  remaining: remaining - bytearray.length(piece),
                  indefinite,
                },
              },
            )
          other -> data_stop(env, ex, source, other)
        }
      } else {
        let finish = fn(s) {
          let (summary, _) = bits.mag_finish(magnitude)
          deliver(env, ex, s, summary)
        }
        if indefinite {
          next_chunk(
            env,
            ex,
            source,
            finish,
            fn(s, length) {
              to(
                ex,
                CData {
                  source: s,
                  mode: DBignum {
                    magnitude,
                    remaining: length,
                    indefinite: True,
                  },
                },
              )
            },
          )
        } else {
          finish(source)
        }
      }
    DFold { fold, next } ->
      if fold.remaining > 0 {
        when pop(ex) is {
          None -> None
          Some((frame, popped)) ->
            to(
              popped,
              CData {
                source,
                mode: DFold { fold: fold_one(fold, frame), next },
              },
            )
        }
      } else {
        when next is {
          NextDataList { constructor, break_after } ->
            close_list(
              env,
              ex,
              source,
              source,
              bits.constr_summary(constructor, fold.sequence),
              break_after,
            )
          NextDataMap ->
            deliver(
              env,
              ex,
              source,
              cek_data_v1.map_data_summary_v1(fold.sequence),
            )
        }
      }
    DSkim ->
      skim_from(env, ex, source.position + source.left, skim_chunks_per_tick)
    DTrail { summary } ->
      trail_from(env, ex, source.position, summary, skim_chunks_per_tick)
  }
}
