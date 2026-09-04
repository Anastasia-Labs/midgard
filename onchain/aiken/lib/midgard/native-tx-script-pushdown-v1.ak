//// The native-script checkpointable pushdown — `docs/spec/midgard-tx.md`
//// §11.3.
////
//// A field-6 item whose language tag is `NativeCardano` carries a **recursive**
//// script (§5.3). Every other intra-item structure in this format is flat
//// enough to walk with a cursor; this one is a tree, and the two things that
//// follow from that are the whole of this module.
////
////   * **The recursion is data, not call depth.** The traversal carries an
////     explicit frame stack, so "how deep is this script" is a number in a
////     structure rather than a shape of the evaluator. A recursive checker
////     cannot stop in the middle and cannot say where it got to; a pushdown
////     can do both.
////   * **A tree position is not one offset.** Resuming needs the pending
////     frames as well as the byte cursor, and §7.6 forbids carrying them
////     verbatim through thread state. They travel as a **hash chain**: the
////     checkpoint commits `stack_root`, the resuming step re-supplies the
////     frames, and the chain is recomputed and required to match. Thread state
////     stays one 32-byte digest whatever the script's shape, and the frames it
////     commits are four small scalars each — never script bytes.
////
//// **What a resume verifies** (§11.3). The script payload is re-supplied and
//// re-digested, so a cursor cannot be pointed at a different script; the
//// checkpoint hashes to the digest the previous step committed, so the
//// position is one a traversal actually reached; and the frame stack
//// re-derives to `stack_root`, so a prover cannot substitute a stack that
//// makes an unsatisfied script satisfied.
////
//// **Opaque, for the §10.6 reason.** `NativeScriptWalkV1`'s constructor and the
//// checkpoint decoder are both private and the commitment-taking resume is the
//// only resume exported, so outside this module a walk can only have been
//// opened over a digested script, stepped over its bytes, or resumed through
//// the digest. A caller that could write one could start the traversal
//// mid-script with a stack of its choosing — which is the same capability
//// #570 removed from `FieldWalkCheckpointV1`, in the shape this tree takes.
////
//// **Authenticate-once, per transaction** (§7.1). The payload is digested when
//// a walk is opened or resumed and not again: `native_script_run` checks it
//// once and the steps under it do not, so a traversal that takes a thousand
//// steps in one transaction pays one `blake2b_256` over the script, not a
//// thousand.
////
//// The verdict this computes is the one `midgard/native_script_v1` computes
//// recursively over materialised `Data`. That module stays the definition; this
//// one is how a dispute affords it.
////
//// **Equivalence includes the bounds, and that is the hard part.** Agreeing
//// about `all`/`any`/`n-of-k` on ordinary scripts is agreeing about the fold. A
//// pushdown can get the fold exactly right and still admit a script the
//// definition refuses, because it counts depth in *frames* where the definition
//// counts it in *nodes* — see `max_native_script_frames`. A disagreement of that
//// kind is worse than a wrong fold: it is a verdict for bytes the definition
//// says have none, and it only shows up at the bound.
////
//// **And the other direction costs a capability.** A bound this module invents
//// — one the definition does not have — turns a script the definition
//// *adjudicates* into one this module aborts on, and a dispute cannot be
//// resolved with an abort. `n-of-k`'s threshold is the place that nearly
//// happened: the script's bytes carry `n` with no upper bound of their own, and
//// `check_native_script` answers `Some(False)` for `at_least(33, [signature])`
//// like any other unmet threshold. So the threshold is **capped, not bounded**
//// — see `capped_required` — which keeps the wire scalar three bytes wide
//// without moving a single verdict.

use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/intra_item_bytes_v1.{head_at, slice_exact}
use midgard/native_script_v1.{
  max_native_script_depth, max_native_script_node_count,
}

// ---------------------------------------------------------------------------
// §11.3 node kinds and bounds
// ---------------------------------------------------------------------------

pub const signature_node: Int = 0

pub const all_node: Int = 1

pub const any_node: Int = 2

pub const at_least_node: Int = 3

pub const after_node: Int = 4

pub const before_node: Int = 5

/// A verdict that has not been folded into its parent yet. `0` is "no pending
/// verdict"; a presence flag beside a boolean would be the same three states in
/// two fields, so this is one field on purpose.
pub const pending_none: Int = 0

pub const pending_false: Int = 1

pub const pending_true: Int = 2

/// §11.3 wire form: `87` and seven elements, two of them 32-byte digests.
pub const native_script_cursor_bytes: Int = 87

/// The frame-stack bound, in **`max_native_script_depth`'s units**.
///
/// `midgard/native_script_v1` measures depth over *nodes* and counts a leaf as
/// depth 1, so a node read with `d` frames open is at depth `d + 1` and the
/// definition's `depth <= max_native_script_depth` is `d <= 15`. A frame is only
/// ever pushed with a child still to read — `open_compound` resolves a childless
/// compound on the spot and `fold_into_parent` re-pushes only while `remaining`
/// is positive — so the deepest node a stack of `d` frames commits to is at
/// depth `d + 2`, and the push that would reach depth 17 is the one that must
/// refuse.
///
/// Getting this off by one would not be a missing bound; it would be a
/// **divergence from the definition** in the permissive direction, which is the
/// one thing §11.3 promises this module does not do.
pub const max_native_script_frames: Int = max_native_script_depth - 1

/// The one spelling of "no child count can meet this threshold".
///
/// A frame's `required` is read in exactly one place — `satisfied >= required`
/// when the frame pops — and `satisfied` cannot exceed the frame's child count,
/// which `open_compound` holds to `max_native_script_node_count`. Every
/// threshold above that bound is therefore the same predicate, and this is the
/// representative of them all.
pub const unsatisfiable_required: Int = max_native_script_node_count + 1

/// Domain separation for the checkpoint commitment — ASCII
/// `MidgardNativeScriptWalkV1`. New surface: none of §4's prohibited
/// counted-scheme domains is reused.
const cursor_domain: ByteArray =
  #"4d6964676172644e617469766553637269707457616c6b5631"

/// Domain separation for the frame-stack hash chain — ASCII
/// `MidgardNativeScriptFrameV1`.
const frame_domain: ByteArray =
  #"4d6964676172644e61746976655363726970744672616d655631"

// ---------------------------------------------------------------------------
// Public state
// ---------------------------------------------------------------------------

/// One pending compound node: how many children are left, how many were
/// satisfied, and how many must be.
///
/// `required` is the uniform threshold §11.3 reduces the three compound kinds
/// to — `all` is "all of them", `any` is "one of them", `n-of-k` is "n" — so
/// the fold that pops a frame has one rule rather than three. `kind` rides
/// along because a resume has to reproduce the exact bytes the chain committed,
/// and because a frame that lost its kind could not be read back by a human
/// debugging a stuck thread.
pub type NativeScriptFrameV1 {
  kind: Int,
  remaining: Int,
  satisfied: Int,
  required: Int,
}

/// The signer set and validity interval the script is evaluated against —
/// the same three inputs `native_script_v1.check_native_script_data` takes,
/// with the same meanings.
pub type NativeScriptContextV1 {
  signers: List<ByteArray>,
  validity_interval_start: Int,
  validity_interval_end: Int,
}

/// A traversal in progress over one native script (§11.3).
///
/// Opaque: see this module's header. `frames` and `roots` are present only
/// because the traversal is running inside one transaction — what crosses a
/// transaction boundary is the head of `roots`, which commits them.
pub opaque type NativeScriptWalkV1 {
  /// `blake2b_256` of the script payload this walk is over. Re-derived at every
  /// open and every resume, so a cursor cannot be replayed against a different
  /// script.
  script_digest: ByteArray,
  script_length: Int,
  /// Byte offset of the next node head, or `script_length` once the last node
  /// has been read.
  offset: Int,
  /// The pending compound nodes, top first.
  frames: List<NativeScriptFrameV1>,
  /// `roots[i]` is the hash chain over `frames[i..]`, so the head is the stack
  /// root and a pop is dropping a cons cell rather than rehashing the stack.
  roots: List<ByteArray>,
  /// Nodes read so far, held to `max_native_script_node_count` so a script
  /// cannot be made expensive by being made big.
  nodes_visited: Int,
  /// `pending_none` / `pending_false` / `pending_true`.
  pending: Int,
}

// ---------------------------------------------------------------------------
// Opening
// ---------------------------------------------------------------------------

/// Open a traversal over the `NativeCardano` payload of an authenticated
/// field-6 item.
///
/// The payload is digested once here — the same authenticate-once shape §7.1
/// gives a field, one transaction one hash — and that digest is what every
/// resume re-establishes.
pub fn open_native_script_walk(payload: ByteArray) -> NativeScriptWalkV1 {
  let script_length = bytearray.length(payload)
  expect script_length > 0
  expect script_length <= 0xffffff
  NativeScriptWalkV1 {
    script_digest: blake2b_256(payload),
    script_length,
    offset: 0,
    frames: [],
    roots: [],
    nodes_visited: 0,
    pending: pending_none,
  }
}

fn empty_stack_root() -> ByteArray {
  blake2b_256(frame_domain)
}

fn stack_root(walk: NativeScriptWalkV1) -> ByteArray {
  when walk.roots is {
    [] -> empty_stack_root()
    [root, ..] -> root
  }
}

// ---------------------------------------------------------------------------
// Stepping
// ---------------------------------------------------------------------------

/// True once the traversal has a verdict: the stack is empty and a verdict is
/// waiting with nothing left to fold it into.
pub fn native_script_walk_is_complete(walk: NativeScriptWalkV1) -> Bool {
  and {
    walk.frames == [],
    walk.pending != pending_none,
  }
}

/// The verdict, or `None` while the traversal is unfinished.
///
/// A finished traversal must also have consumed the payload exactly: a script
/// with trailing bytes is not a script that evaluated to anything.
pub fn native_script_verdict(walk: NativeScriptWalkV1) -> Option<Bool> {
  if native_script_walk_is_complete(walk) {
    expect walk.offset == walk.script_length
    Some(walk.pending == pending_true)
  } else {
    None
  }
}

/// Perform at most `budget` steps and return where the traversal got to.
///
/// This is the whole of interruptibility, and it is the same arrangement
/// §10.4's `walk_fold` makes for the top-level walk: a step does what its
/// transaction can afford, commits the returned cursor's digest, and the next
/// transaction resumes from it. `budget` counts steps and MUST be
/// non-negative — the recursion stops at zero, so a negative budget would mean
/// "no limit" rather than "no steps".
///
/// The payload is digested once, here, rather than once per step.
pub fn native_script_run(
  walk: NativeScriptWalkV1,
  payload: ByteArray,
  context: NativeScriptContextV1,
  budget: Int,
) -> NativeScriptWalkV1 {
  native_script_run_with_signer_verdict(
    walk,
    payload,
    context.validity_interval_start,
    context.validity_interval_end,
    budget,
    fn(key_hash) { list.has(context.signers, key_hash) },
  )
}

/// The same canonical pushdown with signer membership supplied by an
/// authenticated predicate instead of a materialised signer list.
///
/// Fraud-proof families use this at maximum signer cardinality: their thread
/// commits the validation machine's signer frontier and each signature node is
/// answered by an exact membership/non-membership proof. The recursive
/// evaluator, cursor, bounds, interval rules and every non-signature node stay
/// identical to `native_script_run`; only the source of the membership bit is
/// abstracted.
pub fn native_script_run_with_signer_verdict(
  walk: NativeScriptWalkV1,
  payload: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  budget: Int,
  signer_is_present: fn(ByteArray) -> Bool,
) -> NativeScriptWalkV1 {
  expect budget >= 0
  expect blake2b_256(payload) == walk.script_digest
  run_steps_with_signer_verdict(
    walk,
    payload,
    validity_interval_start,
    validity_interval_end,
    signer_is_present,
    budget,
  )
}

fn run_steps_with_signer_verdict(
  walk: NativeScriptWalkV1,
  payload: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  signer_is_present: fn(ByteArray) -> Bool,
  budget: Int,
) -> NativeScriptWalkV1 {
  if or {
    budget == 0,
    native_script_walk_is_complete(walk),
  } {
    walk
  } else {
    run_steps_with_signer_verdict(
      native_script_step_with_signer_verdict(
        walk,
        payload,
        validity_interval_start,
        validity_interval_end,
        signer_is_present,
      ),
      payload,
      validity_interval_start,
      validity_interval_end,
      signer_is_present,
      budget - 1,
    )
  }
}

/// One step: either fold a finished subtree's verdict into its parent, or read
/// the next node.
///
/// Private because it does not re-establish the payload digest —
/// `native_script_run` does that once for the whole fold, and a step reachable
/// without it would be a step over bytes nobody checked.
fn native_script_step_with_signer_verdict(
  walk: NativeScriptWalkV1,
  payload: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  signer_is_present: fn(ByteArray) -> Bool,
) -> NativeScriptWalkV1 {
  if walk.pending != pending_none {
    fold_into_parent(walk)
  } else {
    read_node_with_signer_verdict(
      walk,
      payload,
      validity_interval_start,
      validity_interval_end,
      signer_is_present,
    )
  }
}

fn fold_into_parent(walk: NativeScriptWalkV1) -> NativeScriptWalkV1 {
  // A pending verdict with an empty stack is a complete walk, and `run_steps`
  // stops there, so there is always a parent to fold into here.
  expect [frame, ..rest_frames] = walk.frames
  expect [_root, ..rest_roots] = walk.roots
  let satisfied = if walk.pending == pending_true {
    frame.satisfied + 1
  } else {
    frame.satisfied
  }
  let remaining = frame.remaining - 1
  expect remaining >= 0
  let popped = NativeScriptWalkV1 {
    ..walk,
    frames: rest_frames,
    roots: rest_roots,
  }
  if remaining == 0 {
    NativeScriptWalkV1 {
      ..popped,
      pending: verdict_of(satisfied >= frame.required),
    }
  } else {
    push_frame(
      NativeScriptWalkV1 { ..popped, pending: pending_none },
      NativeScriptFrameV1 { ..frame, remaining, satisfied },
    )
  }
}

fn verdict_of(valid: Bool) -> Int {
  if valid {
    pending_true
  } else {
    pending_false
  }
}

fn push_frame(
  walk: NativeScriptWalkV1,
  frame: NativeScriptFrameV1,
) -> NativeScriptWalkV1 {
  // See `max_native_script_frames`: this is `depth <= max_native_script_depth`
  // written in frames, and the two spellings have to mean the same thing or the
  // pushdown admits a script the definition refuses.
  expect list.length(walk.frames) < max_native_script_frames
  NativeScriptWalkV1 {
    ..walk,
    frames: [frame, ..walk.frames],
    roots: [chain_frame(stack_root(walk), frame), ..walk.roots],
  }
}

/// The hash chain over a top-first frame list, folded bottom-to-top so that the
/// root of a stack is the root of the stack below it extended by the top frame.
fn chain_frame(below: ByteArray, frame: NativeScriptFrameV1) -> ByteArray {
  blake2b_256(
    frame_domain
      |> bytearray.concat(below)
      |> bytearray.concat(encode_frame(frame)),
  )
}

/// The fixed-width frame encoding. Its assertions are the frame's construction
/// domain, and they run again on every resume, because a resume re-derives the
/// chain from the frames it was handed.
pub fn encode_frame(frame: NativeScriptFrameV1) -> ByteArray {
  expect frame.kind >= all_node
  expect frame.kind <= at_least_node
  expect frame.remaining > 0
  expect frame.remaining <= max_native_script_node_count
  expect frame.satisfied >= 0
  expect frame.satisfied <= max_native_script_node_count
  expect frame.required >= 0
  expect frame.required <= unsatisfiable_required
  bytearray.from_int_big_endian(frame.kind, 1)
    |> bytearray.concat(bytearray.from_int_big_endian(frame.remaining, 3))
    |> bytearray.concat(bytearray.from_int_big_endian(frame.satisfied, 3))
    |> bytearray.concat(bytearray.from_int_big_endian(frame.required, 3))
}

// ---------------------------------------------------------------------------
// §11.3 node reading
// ---------------------------------------------------------------------------

fn read_node_with_signer_verdict(
  walk: NativeScriptWalkV1,
  payload: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  signer_is_present: fn(ByteArray) -> Bool,
) -> NativeScriptWalkV1 {
  let nodes_visited = walk.nodes_visited + 1
  expect nodes_visited <= max_native_script_node_count
  let Pair(after_array, arity) = head_at(payload, walk.offset, 4)
  let Pair(after_tag, tag) = head_at(payload, after_array, 0)
  // §11.3: `n-of-k` is the only three-element node; everything else is two.
  expect
    arity == if tag == at_least_node {
      3
    } else {
      2
    }
  let stepped = NativeScriptWalkV1 { ..walk, nodes_visited }
  if tag == signature_node {
    let Pair(key_offset, key_length) = head_at(payload, after_tag, 2)
    expect key_length == 28
    let key_hash = slice_exact(payload, key_offset, key_length)
    NativeScriptWalkV1 {
      ..stepped,
      offset: key_offset + key_length,
      pending: verdict_of(signer_is_present(key_hash)),
    }
  } else if tag == after_node {
    let Pair(next, slot) = head_at(payload, after_tag, 0)
    NativeScriptWalkV1 { ..stepped, offset: next, pending: verdict_of(and {
          validity_interval_start >= 0,
          validity_interval_start >= slot,
        }) }
  } else if tag == before_node {
    let Pair(next, slot) = head_at(payload, after_tag, 0)
    NativeScriptWalkV1 { ..stepped, offset: next, pending: verdict_of(and {
          validity_interval_end >= 0,
          validity_interval_end <= slot,
        }) }
  } else if tag == at_least_node {
    let Pair(after_required, required) = head_at(payload, after_tag, 0)
    let Pair(after_children, child_count) = head_at(payload, after_required, 4)
    open_compound(stepped, after_children, tag, child_count, required)
  } else {
    expect or {
        tag == all_node,
        tag == any_node,
      }
    let Pair(after_children, child_count) = head_at(payload, after_tag, 4)
    open_compound(
      stepped,
      after_children,
      tag,
      child_count,
      if tag == all_node {
        child_count
      } else {
        1
      },
    )
  }
}

/// A compound node either pushes a frame or, if it has no children, resolves on
/// the spot. Resolving on the spot is not an optimisation: a zero-child frame
/// would have `remaining == 0` before anything could decrement it, and the fold
/// that pops frames is written for the decrement.
///
/// `child_count` is bounded here because a frame carries it as a three-byte wire
/// scalar and because the definition's node bound makes any wider node a script
/// it refuses outright. `required` is **not**: see `capped_required`.
fn open_compound(
  walk: NativeScriptWalkV1,
  offset: Int,
  kind: Int,
  child_count: Int,
  required: Int,
) -> NativeScriptWalkV1 {
  expect child_count >= 0
  expect child_count <= max_native_script_node_count
  expect required >= 0
  let positioned = NativeScriptWalkV1 { ..walk, offset }
  if child_count == 0 {
    // `all` of nothing is satisfied; `any` of nothing is not; `0-of-0` is.
    NativeScriptWalkV1 { ..positioned, pending: verdict_of(0 >= required) }
  } else {
    push_frame(
      positioned,
      NativeScriptFrameV1 {
        kind,
        remaining: child_count,
        satisfied: 0,
        required: capped_required(required),
      },
    )
  }
}

/// The threshold a frame carries: the script's `n`, or `unsatisfiable_required`
/// when `n` is above anything a frame can satisfy.
///
/// **A cap, and deliberately not a bound.** `n-of-k` puts `n` in the script's
/// bytes and the definition puts no ceiling on it — `check_native_script`
/// answers `Some(valid: False)` for `at_least(33, [signature])` exactly as it
/// does for `at_least(2, [signature])`, and both are two nodes at depth two,
/// inside every bound it has. A pushdown that *refused* the first would be
/// unable to adjudicate a script the definition adjudicates, which in a fraud
/// proof is not a stricter check but a lost capability: a script nobody can
/// prove invalid.
///
/// **Why capping is verdict-preserving.** `required` is read once, as
/// `satisfied >= required` when the frame pops. `satisfied` starts at zero and
/// rises by at most one per child, and the frame's child count is held to
/// `max_native_script_node_count` just above, so `satisfied` never reaches
/// `unsatisfiable_required`. Every threshold at or above the cap is therefore
/// unmet for every reachable `satisfied`, and mapping them all onto one value
/// moves no verdict. What it buys is that the frame's wire scalar stays three
/// bytes wide whatever `n` the script names — up to the eight-byte CBOR uint
/// the head reader admits.
fn capped_required(required: Int) -> Int {
  if required > max_native_script_node_count {
    unsatisfiable_required
  } else {
    required
  }
}

// The §11.3 grammar is read with `midgard/intra_item_bytes_v1`'s `head_at` and
// `slice_exact` — the same minimal-width reader §11.1 and §11.2 use, because a
// script's arities and a value's group counts are the same CBOR head and §6.1
// wants one spelling for it, not two.

// ---------------------------------------------------------------------------
// §11.3 checkpoint wire form and commitment
// ---------------------------------------------------------------------------

/// The traversal's position as thread-carriable bytes: two 32-byte digests and
/// five fixed-width scalars, constant length whatever the script's shape.
///
/// The head is `87` because the array has seven elements. On-chain this
/// structure is only ever produced and consumed by the two functions below, so
/// an array header that disagreed with the element count would still round-trip
/// and the commitment would still be sound — but it would not be **CBOR**, and
/// an off-chain twin decoding these bytes would hit a premature end of input.
/// §7.6's carried state is a wire form, so it is held to being one.
pub fn encode_native_script_cursor(walk: NativeScriptWalkV1) -> ByteArray {
  let depth = list.length(walk.frames)
  expect bytearray.length(walk.script_digest) == 32
  expect walk.script_length > 0
  expect walk.script_length <= 0xffffff
  expect walk.offset >= 0
  expect walk.offset <= walk.script_length
  expect depth >= 0
  expect depth <= max_native_script_frames
  expect list.length(walk.roots) == depth
  expect walk.nodes_visited >= 0
  expect walk.nodes_visited <= max_native_script_node_count
  expect walk.pending >= pending_none
  expect walk.pending <= pending_true
  #"87"
    |> bytearray.concat(#"5820")
    |> bytearray.concat(walk.script_digest)
    |> bytearray.concat(#"5820")
    |> bytearray.concat(stack_root(walk))
    |> bytearray.concat(#"43")
    |> bytearray.concat(bytearray.from_int_big_endian(walk.script_length, 3))
    |> bytearray.concat(#"43")
    |> bytearray.concat(bytearray.from_int_big_endian(walk.offset, 3))
    |> bytearray.concat(#"43")
    |> bytearray.concat(bytearray.from_int_big_endian(depth, 3))
    |> bytearray.concat(#"43")
    |> bytearray.concat(bytearray.from_int_big_endian(walk.nodes_visited, 3))
    |> bytearray.concat(#"41")
    |> bytearray.concat(bytearray.from_int_big_endian(walk.pending, 1))
}

/// The 32-byte commitment a computation thread carries in place of the cursor.
pub fn native_script_cursor_hash(walk: NativeScriptWalkV1) -> ByteArray {
  blake2b_256(
    bytearray.concat(cursor_domain, encode_native_script_cursor(walk)),
  )
}

/// Fail-closed inverse of `encode_native_script_cursor`, with the frames the
/// caller re-supplies grafted back on.
///
/// **Deliberately private, for the §10.6 reason.** A public decoder is a public
/// constructor, and a caller holding one could start a traversal at a byte
/// offset of its choosing with a stack of its choosing. The only way through it
/// is `resume_native_script_walk_from_commitment`, which holds the result to a
/// digest the previous step committed.
///
/// The scalar bounds are not re-listed: re-encoding what was read and demanding
/// the input back is both the canonicity check (§6.1 — one admissible spelling)
/// and the range check, without a second reader of the same grammar to drift
/// from the first. That re-encode is also what checks the supplied frames,
/// because the wire form's stack-root element is computed from them.
fn decode_native_script_cursor(
  bytes: ByteArray,
  frames: List<NativeScriptFrameV1>,
) -> NativeScriptWalkV1 {
  expect bytearray.length(bytes) == native_script_cursor_bytes
  let walk = NativeScriptWalkV1 {
    script_digest: bytearray.slice(bytes, 3, 34),
    script_length: bytearray.to_int_big_endian(bytearray.slice(bytes, 70, 72)),
    offset: bytearray.to_int_big_endian(bytearray.slice(bytes, 74, 76)),
    nodes_visited: bytearray.to_int_big_endian(bytearray.slice(bytes, 82, 84)),
    pending: bytearray.at(bytes, 86),
    frames,
    roots: roots_of(frames),
  }
  expect encode_native_script_cursor(walk) == bytes
  walk
}

/// The chain roots for a top-first frame list: `roots[i]` covers `frames[i..]`.
fn roots_of(frames: List<NativeScriptFrameV1>) -> List<ByteArray> {
  when frames is {
    [] -> []
    [frame, ..rest] -> {
      let below = roots_of(rest)
      let below_root = when below is {
        [] -> empty_stack_root()
        [root, ..] -> root
      }
      [chain_frame(below_root, frame), ..below]
    }
  }
}

/// Resume a traversal from **thread state** — the form every real step uses
/// (§11.3).
///
/// Three independent things are established here, and the traversal is unsound
/// without any one of them:
///
///   1. the 87 carried bytes hash to `committed`, so the position is one a
///      previous step actually reached;
///   2. `payload` re-digests to the cursor's `script_digest` and has its
///      length, so the position belongs to *this* script;
///   3. `frames` re-derives to the committed stack root, so the pending
///      thresholds are the ones the traversal built rather than ones chosen to
///      make an unsatisfied script pass. This one is not a separate line below:
///      the wire form's stack-root element is computed from `frames`, so the
///      decoder's re-encode fails unless they agree.
pub fn resume_native_script_walk_from_commitment(
  committed: ByteArray,
  cursor_bytes: ByteArray,
  frames: List<NativeScriptFrameV1>,
  payload: ByteArray,
) -> NativeScriptWalkV1 {
  let walk = decode_native_script_cursor(cursor_bytes, frames)
  expect native_script_cursor_hash(walk) == committed
  expect blake2b_256(payload) == walk.script_digest
  expect bytearray.length(payload) == walk.script_length
  walk
}

/// The frames a step must hand the next one. Exposed because a thread carries
/// only the digest, and something has to re-supply the scalars that hash to it.
pub fn native_script_walk_frames(
  walk: NativeScriptWalkV1,
) -> List<NativeScriptFrameV1> {
  walk.frames
}

/// Nodes read so far — what a step reports when it says how far it got.
pub fn native_script_nodes_visited(walk: NativeScriptWalkV1) -> Int {
  walk.nodes_visited
}
