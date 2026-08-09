//// Seam vectors for the native-script checkpointable pushdown —
//// `docs/spec/midgard-tx.md` §11.3.
////
//// Everything is asserted at the module's public seam: `open_native_script_walk`,
//// `native_script_run`, `native_script_verdict`,
//// `resume_native_script_walk_from_commitment`, and the cursor wire form. No
//// test names a private helper or pins an intermediate offset.
////
//// Three claims, and how each is established:
////
////   * **The pushdown computes the recursive checker's verdict — including at
////     the bounds.** `pushdown_agrees_*` runs both over the same scripts.
////     `midgard/native_script_v1` stays the definition of what a native script
////     means; this module is only how a dispute affords it, so any disagreement
////     is this module's bug. Shallow agreement is not enough: a fold can be
////     right while the traversal admits a script the definition refuses, which
////     is a disagreement in the *permissive* direction, so the deepest
////     admissible script and the first inadmissible one are both vectors here.
////   * **It checkpoints and resumes.** `resume_*` stops a traversal mid-tree at
////     a budget, carries the position through the thread's 32-byte commitment
////     and the 87 bytes that hash to it, re-supplies the frames, and finishes.
////     The verdict must equal the uninterrupted one, and the resumed leg is
////     budgeted at exactly the work the first leg left — so a leg that had
////     restarted from zero would run out rather than quietly agreeing. The
////     three things a resume verifies are each shown to refuse on their own.
////   * **It fits the budget** (§11.4). A cost claim, so it is measured: the
////     `budget_*` rows are what §11.4 quotes, and their assertions only pin
////     that the two paths agree, so the numbers are comparable.
////
//// **How a forgery is spelled here.** `NativeScriptWalkV1` is opaque and its
//// decoder is private (§11.3), so no test can write a cursor literal — which
//// is the point, since that capability is exactly the one a prover must not
//// have either. Negative vectors forge the way a prover would: they take the
//// 87 carried bytes of an honest position and present them with the wrong
//// digest, the wrong frames, or the wrong script.

use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/native_script_v1.{
  max_native_script_depth, max_native_script_node_count,
}
use midgard/native_tx_script_pushdown_v1.{
  NativeScriptContextV1, NativeScriptFrameV1, encode_native_script_cursor,
  native_script_cursor_bytes, native_script_cursor_hash,
  native_script_nodes_visited, native_script_run, native_script_verdict,
  native_script_walk_frames, native_script_walk_is_complete,
  open_native_script_walk, resume_native_script_walk_from_commitment,
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const key_1: ByteArray =
  #"11111111111111111111111111111111111111111111111111111111"

const key_2: ByteArray =
  #"22222222222222222222222222222222222222222222222222222222"

const key_3: ByteArray =
  #"33333333333333333333333333333333333333333333333333333333"

fn signature(key_hash: ByteArray) -> ByteArray {
  #"8200581c" |> bytearray.concat(key_hash)
}

fn array_header(length: Int) -> ByteArray {
  expect length >= 0
  expect length <= 23
  bytearray.from_int_big_endian(128 + length, 1)
}

fn compound(tag: Int, children: List<ByteArray>) -> ByteArray {
  #"82"
    |> bytearray.concat(bytearray.from_int_big_endian(tag, 1))
    |> bytearray.concat(array_header(list.length(children)))
    |> bytearray.concat(list.foldr(children, #"", bytearray.concat))
}

fn all_of(children: List<ByteArray>) -> ByteArray {
  compound(1, children)
}

fn any_of(children: List<ByteArray>) -> ByteArray {
  compound(2, children)
}

/// `n-of-k`. `required` is spelled at its **minimal** CBOR width and carries no
/// ceiling of its own, because the definition gives it none: the threshold rows
/// below need `n` above the child count, above the node bound, and out at the
/// eight-byte form.
fn at_least(required: Int, children: List<ByteArray>) -> ByteArray {
  #"83"
    |> bytearray.concat(#"03")
    |> bytearray.concat(uint_bytes(required))
    |> bytearray.concat(array_header(list.length(children)))
    |> bytearray.concat(list.foldr(children, #"", bytearray.concat))
}

/// A CBOR unsigned integer at its minimal width — the only spelling §6.1 admits,
/// and the one the recursive checker's canonicity re-encode demands back.
fn uint_bytes(value: Int) -> ByteArray {
  expect value >= 0
  if value <= 23 {
    bytearray.from_int_big_endian(value, 1)
  } else if value <= 0xff {
    bytearray.concat(#"18", bytearray.from_int_big_endian(value, 1))
  } else if value <= 0xffff {
    bytearray.concat(#"19", bytearray.from_int_big_endian(value, 2))
  } else if value <= 0xffffffff {
    bytearray.concat(#"1a", bytearray.from_int_big_endian(value, 4))
  } else {
    expect value <= 0xffffffffffffffff
    bytearray.concat(#"1b", bytearray.from_int_big_endian(value, 8))
  }
}

fn after(slot: Int) -> ByteArray {
  bytearray.concat(#"8204", uint_bytes(slot))
}

fn before(slot: Int) -> ByteArray {
  bytearray.concat(#"8205", uint_bytes(slot))
}

/// Eight nodes, three levels, one of each compound kind and both timelocks —
/// enough shape that a resume in the middle is resuming a real stack rather
/// than a one-frame one.
fn nested_script() -> ByteArray {
  all_of(
    [
      any_of([signature(key_1), signature(key_2)]),
      at_least(2, [signature(key_1), signature(key_3), after(100)]),
    ],
  )
}

/// A different shape at the same depth, for the constant-width claim.
fn other_script() -> ByteArray {
  all_of(
    [
      any_of([signature(key_3), signature(key_2), before(200)]),
      at_least(1, [signature(key_2), after(50)]),
    ],
  )
}

/// `nested_script` with one signature's key changed: **the same length**, so a
/// resume that only compared lengths would take it.
fn twin_script() -> ByteArray {
  all_of(
    [
      any_of([signature(key_3), signature(key_2)]),
      at_least(2, [signature(key_1), signature(key_3), after(100)]),
    ],
  )
}

/// `count` nested single-child `all` nodes around `inner`.
///
/// The depth `midgard/native_script_v1` measures counts a **leaf** as 1, so
/// this is a script of depth `count + 1` in exactly `count + 1` nodes — the
/// shape that separates the depth bound from the node bound, because at
/// `count = 16` it is depth 17 with only 17 nodes, well inside the 32-node
/// bound.
fn nested_alls(count: Int, inner: ByteArray) -> ByteArray {
  if count <= 0 {
    inner
  } else {
    nested_alls(count - 1, all_of([inner]))
  }
}

/// `count` copies of `child`, which is how a script gets wide without getting
/// deep or exceeding any single child count.
fn repeated(count: Int, child: ByteArray) -> List<ByteArray> {
  list.map(list.range(1, count), fn(_i) { child })
}

fn context() -> NativeScriptContextV1 {
  NativeScriptContextV1 {
    signers: [key_1],
    validity_interval_start: 200,
    validity_interval_end: 300,
  }
}

/// Big enough to finish any fixture here in one call.
const unlimited: Int = 64

fn run_to_completion(script: ByteArray) -> Bool {
  let walk = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    unlimited,
  )
  expect Some(verdict) = native_script_verdict(walk)
  verdict
}

fn recursive_check(
  script: ByteArray,
) -> Option<native_script_v1.NativeScriptProofV1> {
  native_script_v1.check_native_script(
    script,
    context().validity_interval_start,
    context().validity_interval_end,
    context().signers,
  )
}

fn recursive_verdict(script: ByteArray) -> Bool {
  expect Some(proof) = recursive_check(script)
  proof.valid
}

/// The definition's own measurement of a script, taken before its bounds are
/// applied — so a vector can say *which* bound refuses rather than only that
/// one did.
fn recursive_shape(script: ByteArray) -> Pair<Int, Int> {
  expect Some(proof) = native_script_v1.inspect_native_script(
    script,
    context().validity_interval_start,
    context().validity_interval_end,
    context().signers,
  )
  Pair(proof.depth, proof.node_count)
}

// ---------------------------------------------------------------------------
// §11.3 the pushdown computes the recursive verdict
// ---------------------------------------------------------------------------

/// Both evaluators, same scripts, same answers. `midgard/native_script_v1` is
/// the definition; a disagreement here is this module's bug and nothing else's.
///
/// The shallow rows are the shapes; the deep ones are the bound. Agreement on
/// small scripts is agreement about the *fold*, and a fold can be right while
/// the traversal admits a script the definition refuses — which is a strictly
/// worse bug, because it is one in the permissive direction. So the deepest
/// script the definition admits is a row here too, and the first one it does not
/// has a refusal vector of its own below.
test pushdown_agrees_with_the_recursive_checker() {
  let agrees = fn(script: ByteArray) {
    run_to_completion(script) == recursive_verdict(script)
  }
  and {
    agrees(nested_script()),
    agrees(other_script()),
    agrees(signature(key_1)),
    agrees(signature(key_2)),
    agrees(after(100)),
    agrees(before(400)),
    agrees(all_of([])),
    agrees(any_of([])),
    agrees(at_least(0, [])),
    // Depth 1, 2, 15 and 16 — the last being the deepest script the definition
    // admits — on both verdicts, so the agreement is not agreement on `True`.
    agrees(nested_alls(0, signature(key_1))),
    agrees(nested_alls(1, signature(key_1))),
    agrees(nested_alls(14, signature(key_1))),
    agrees(nested_alls(15, signature(key_1))),
    agrees(nested_alls(15, signature(key_2))),
    // Wide as well as deep, at the node bound rather than under it.
    agrees(all_of(repeated(15, signature(key_1)))),
    agrees(any_of(repeated(15, signature(key_2)))),
    agrees(at_least(2, repeated(3, signature(key_1)))),
    // `n-of-k` with `n` **above** the child count, which the definition bounds
    // nowhere and evaluates like any other unmet threshold. Straddling the child
    // count, then the node bound, then out to the widest uint the head reader
    // reads — the last two only agree because the frame caps rather than refuses.
    agrees(at_least(1, [signature(key_1)])),
    agrees(at_least(2, [signature(key_1)])),
    agrees(at_least(4, repeated(3, signature(key_1)))),
    agrees(at_least(max_native_script_node_count, [signature(key_1)])),
    agrees(at_least(max_native_script_node_count + 1, [signature(key_1)])),
    agrees(
      at_least(max_native_script_node_count + 1, repeated(3, signature(key_1))),
    ),
    agrees(at_least(0xffff, [signature(key_1)])),
    agrees(at_least(0xffffffffffffffff, [signature(key_1)])),
    // The childless compounds at the same thresholds, which resolve without a
    // frame and so never meet the cap at all.
    agrees(at_least(1, [])),
    agrees(at_least(max_native_script_node_count + 1, [])),
  }
}

/// The divergence this vector exists to keep closed, pointing the *other* way
/// from the depth bound above.
///
/// `at_least(33, [signature])` is two nodes at depth two — inside every bound
/// `check_native_script` has — and it answers `Some(valid: False)`, because
/// nothing in the definition caps `n`. A pushdown that asserted `required ≤
/// max_native_script_node_count` on the frame **aborted** on these bytes
/// instead. That is not a stricter check: an abort is not a verdict, so a script
/// the definition calls invalid became one no dispute could resolve, which in a
/// fraud proof is a lost capability rather than a safety margin.
///
/// The cap that replaces it is verdict-preserving because `satisfied` cannot
/// reach the node bound's successor — the rows below pin exactly that, on both
/// sides of the cap and on both verdicts.
test the_pushdown_adjudicates_a_threshold_above_the_node_bound() {
  let one_child = at_least(max_native_script_node_count + 1, [signature(key_1)])
  let three_children = at_least(
    max_native_script_node_count + 1,
    repeated(3, signature(key_1)),
  )
  and {
    // The definition adjudicates it, and every bound it has is satisfied.
    recursive_shape(one_child) == Pair(2, 2),
    2 <= max_native_script_depth,
    2 <= max_native_script_node_count,
    recursive_check(one_child) != None,
    !recursive_verdict(one_child),
    // …and so does the pushdown, rather than refusing to answer.
    !run_to_completion(one_child),
    !run_to_completion(three_children),
    // The cap is only ever reached from above: one child satisfied is still one
    // short of a threshold of two, and a threshold of one is met.
    !run_to_completion(at_least(2, [signature(key_1)])),
    run_to_completion(at_least(1, [signature(key_1)])),
    // …and a capped threshold does not become satisfiable by having more
    // children satisfied under it, which is the failure mode a cap could
    // introduce: three satisfied children, threshold 33, still false.
    recursive_verdict(at_least(3, repeated(3, signature(key_1)))),
    !recursive_verdict(three_children),
  }
}

/// A capped threshold has to survive a **checkpoint**, not merely a traversal.
///
/// The frame the hash chain commits carries `required` as the cap left it, and
/// the resuming step re-derives the chain from frames it is handed. A cap
/// applied on one side of a resume and not the other would not produce a wrong
/// answer — it would produce no answer, because the re-derived stack root would
/// not match. So this is the vector that says the cap is part of the wire form
/// rather than a traversal-local convenience.
test resume_carries_a_capped_threshold() {
  let script = at_least(
    0xffffffffffffffff,
    [signature(key_1), signature(key_2), signature(key_3)],
  )
  let stopped = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    2,
  )
  expect !native_script_walk_is_complete(stopped)
  expect [top, ..] = native_script_walk_frames(stopped)
  let resumed = resume_native_script_walk_from_commitment(
    native_script_cursor_hash(stopped),
    encode_native_script_cursor(stopped),
    native_script_walk_frames(stopped),
    script,
  )
  and {
    // The frame really is carrying the cap rather than the script's `n`.
    top.required == max_native_script_node_count + 1,
    native_script_verdict(
      native_script_run(resumed, script, context(), unlimited),
    ) == Some(False),
    !recursive_verdict(script),
  }
}

/// The bound itself, in the definition's units, so the vectors below are known
/// to sit where they claim to. A leaf counts as depth 1, so `n` nested `all`
/// nodes around one signature is depth `n + 1` in `n + 1` nodes.
test the_depth_boundary_fixtures_are_where_they_claim_to_be() {
  and {
    recursive_shape(nested_alls(14, signature(key_1))) == Pair(15, 15),
    recursive_shape(nested_alls(15, signature(key_1))) == Pair(
      max_native_script_depth,
      16,
    ),
    // One past it: depth 17, and — this is what makes it a *depth* vector —
    // only 17 nodes, so the node bound is nowhere near refusing.
    recursive_shape(nested_alls(16, signature(key_1))) == Pair(17, 17),
    17 <= max_native_script_node_count,
    // …and the definition does refuse it, which is what the pushdown must match.
    recursive_check(nested_alls(15, signature(key_1))) != None,
    recursive_check(nested_alls(16, signature(key_1))) == None,
  }
}

/// The divergence this vector exists to keep closed. `check_native_script`
/// answers `None` on sixteen nested `all` nodes around a signature; a pushdown
/// whose frame bound admitted sixteen frames answered `Some(True)` for the same
/// bytes, which is a disagreement with the definition **in the permissive
/// direction** — the pushdown accepting a script §11.3 says it evaluates the
/// same way.
///
/// Every other bound is satisfied here: 17 nodes against a 32-node bound, and
/// every child count is 1.
test run_refuses_a_script_one_level_past_the_depth_bound() fail {
  let script = nested_alls(16, signature(key_1))
  expect _ = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    unlimited,
  )
  True
}

/// The other side of the same boundary, as an ordinary passing test: the
/// deepest script the definition admits is one the pushdown finishes. Without
/// this the bound above could be met by refusing one level too early, which is
/// the same divergence pointing the other way.
test the_deepest_admissible_script_still_traverses() {
  let script = nested_alls(15, signature(key_1))
  let walk = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    unlimited,
  )
  and {
    native_script_verdict(walk) == Some(True),
    native_script_nodes_visited(walk) == 16,
    // The same script with a signer nobody witnessed: a `False` verdict at the
    // bound, so the row above is not passing because everything deep says
    // `True`.
    !run_to_completion(nested_alls(15, signature(key_2))),
  }
}

/// The verdicts themselves, spelled out, so the agreement above is agreement on
/// something rather than on two identical mistakes.
test pushdown_verdicts_are_the_expected_ones() {
  and {
    run_to_completion(nested_script()),
    !run_to_completion(other_script()),
    run_to_completion(signature(key_1)),
    !run_to_completion(signature(key_2)),
    // `after(100)` against a validity start of 200: satisfied.
    run_to_completion(after(100)),
    // `before(400)` against a validity end of 300: satisfied.
    run_to_completion(before(400)),
    !run_to_completion(before(100)),
    // `all` of nothing is satisfied; `any` of nothing is not.
    run_to_completion(all_of([])),
    !run_to_completion(any_of([])),
  }
}

/// The traversal reads each of the fixture's eight nodes exactly once.
test pushdown_reads_each_node_once() {
  let script = nested_script()
  let walk = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    unlimited,
  )
  and {
    native_script_walk_is_complete(walk),
    native_script_nodes_visited(walk) == 8,
  }
}

// ---------------------------------------------------------------------------
// §11.3 checkpoint and resume
// ---------------------------------------------------------------------------

/// The whole of `nested_script`'s traversal is fifteen steps — eight node reads
/// and seven folds — and pinning that number is what lets the resume vector
/// below tell a resume from a restart. Pinned on both sides, so a step-count
/// drift shows up here as a change rather than quietly loosening the budget
/// that vector rests on.
const nested_script_steps: Int = 15

const interrupt_after: Int = 5

test the_nested_fixture_takes_exactly_fifteen_steps() {
  let script = nested_script()
  let exact = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    nested_script_steps,
  )
  let one_short = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    nested_script_steps - 1,
  )
  and {
    native_script_walk_is_complete(exact),
    !native_script_walk_is_complete(one_short),
  }
}

/// Stop at a budget, carry the position as a thread would — 87 bytes plus the
/// digest of them — and finish in what is, as far as this module can tell, a
/// different transaction. The answer must be the uninterrupted answer, and the
/// second leg must not re-read the first leg's nodes.
///
/// **How the second half of that is enforced.** The resumed leg is given a
/// budget of exactly the work the first leg left — ten of fifteen steps — and
/// one step fewer is shown not to finish. A leg that had restarted from zero
/// would need all fifteen and would run out, so it could not produce this
/// verdict at all. Counting nodes alone would not do it: `nodes_visited == 8`
/// is just as true of a traversal that read all eight from the beginning, which
/// is precisely the regression this is here to catch.
test resume_finishes_an_interrupted_traversal() {
  let script = nested_script()
  let stopped = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    interrupt_after,
  )
  expect !native_script_walk_is_complete(stopped)
  let carried = encode_native_script_cursor(stopped)
  let committed = native_script_cursor_hash(stopped)
  let resumed = resume_native_script_walk_from_commitment(
    committed,
    carried,
    native_script_walk_frames(stopped),
    script,
  )
  let remaining = nested_script_steps - interrupt_after
  let finished = native_script_run(resumed, script, context(), remaining)
  expect Some(verdict) = native_script_verdict(finished)
  // The budget really is tight — one step short of the remaining work does not
  // finish, so `remaining` is the work left rather than a generous ceiling.
  let starved = native_script_run(resumed, script, context(), remaining - 1)
  and {
    verdict == run_to_completion(script),
    bytearray.length(carried) == native_script_cursor_bytes,
    // The resumed leg pays only for the nodes the first leg did not read.
    native_script_nodes_visited(stopped) < 8,
    native_script_nodes_visited(finished) == 8,
    !native_script_walk_is_complete(starved),
  }
}

/// The same walk, interrupted at every budget from 1 to 12, always lands on the
/// same verdict. One interruption point could be a coincidence.
test resume_is_answer_preserving_at_every_budget() {
  let script = nested_script()
  let expected = run_to_completion(script)
  list.all(
    list.range(1, 12),
    fn(budget) {
      let stopped = native_script_run(
        open_native_script_walk(script),
        script,
        context(),
        budget,
      )
      if native_script_walk_is_complete(stopped) {
        native_script_verdict(stopped) == Some(expected)
      } else {
        let resumed = resume_native_script_walk_from_commitment(
          native_script_cursor_hash(stopped),
          encode_native_script_cursor(stopped),
          native_script_walk_frames(stopped),
          script,
        )
        native_script_verdict(
          native_script_run(resumed, script, context(), unlimited),
        ) == Some(expected)
      }
    },
  )
}

/// §7.6 made checkable. Two traversals of two scripts that share the fixture's
/// shape but differ in every byte the grammar leaves free serialise to
/// **the same length**, and the length is a constant of the format rather than
/// of the script.
test cursor_wire_form_is_constant_length() {
  let first = nested_script()
  let second = other_script()
  let first_cursor = encode_native_script_cursor(
    native_script_run(open_native_script_walk(first), first, context(), 4),
  )
  let second_cursor = encode_native_script_cursor(
    native_script_run(open_native_script_walk(second), second, context(), 4),
  )
  and {
    bytearray.length(first_cursor) == native_script_cursor_bytes,
    bytearray.length(second_cursor) == native_script_cursor_bytes,
    bytearray.length(first_cursor) == bytearray.length(second_cursor),
    // …and the fixtures really are different scripts, so the equality above is
    // about the wire form rather than about the fixture.
    first != second,
    first_cursor != second_cursor,
  }
}

/// The cursor is **CBOR**, not merely bytes this module agrees with itself
/// about.
///
/// On-chain the wire form is only ever written by `encode_native_script_cursor`
/// and read by the decoder beside it, so a header that disagreed with the
/// element count would round-trip perfectly and the commitment would still be
/// sound — the shipped form carried `88` over seven elements and nothing
/// on-chain noticed. A real CBOR reader does: it runs off the end looking for
/// the eighth. §7.6's carried state is a wire form, so an off-chain twin has to
/// be able to decode it, and `cbor.deserialise` is a decoder that will say so.
test cursor_wire_form_is_well_formed_cbor() {
  let script = nested_script()
  let cursor = encode_native_script_cursor(
    native_script_run(open_native_script_walk(script), script, context(), 5),
  )
  expect Some(decoded) = cbor.deserialise(cursor)
  expect elements: List<ByteArray> = decoded
  and {
    // Seven elements, and they are the seven this wire form declares: the two
    // 32-byte digests, four 3-byte scalars, and the one-byte `pending`. A
    // header that under-declared would decode a prefix and land here with the
    // wrong count; one that over-declares does not decode at all.
    list.length(elements) == 7,
    list.at(elements, 0) == Some(bytearray.slice(cursor, 3, 34)),
    list.at(elements, 1) == Some(bytearray.slice(cursor, 37, 68)),
    list.map(elements, bytearray.length) == [32, 32, 3, 3, 3, 3, 1],
  }
}

/// §11.3 resume check 1: the position must hash to the digest the thread
/// carries. Here the digest belongs to a position one step further on.
test resume_rejects_a_position_the_thread_did_not_commit() fail {
  let script = nested_script()
  let stopped = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    5,
  )
  let further = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    6,
  )
  expect _ = resume_native_script_walk_from_commitment(
    native_script_cursor_hash(further),
    encode_native_script_cursor(stopped),
    native_script_walk_frames(stopped),
    script,
  )
  True
}

/// §11.3 resume check 3: the frames must re-derive to the committed stack root.
/// The forged frame claims one more satisfied child than the traversal saw,
/// which is exactly the forgery that turns an unsatisfied `n-of-k` into a
/// satisfied one.
test resume_rejects_a_substituted_frame_stack() fail {
  let script = nested_script()
  let stopped = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    5,
  )
  expect [top, ..rest] = native_script_walk_frames(stopped)
  let forged = [
    NativeScriptFrameV1 { ..top, satisfied: top.satisfied + 1 },
    ..rest
  ]
  expect _ = resume_native_script_walk_from_commitment(
    native_script_cursor_hash(stopped),
    encode_native_script_cursor(stopped),
    forged,
    script,
  )
  True
}

/// The same check, from the other side: dropping a frame changes the depth the
/// wire form declares as well as the root.
test resume_rejects_a_shortened_frame_stack() fail {
  let script = nested_script()
  let stopped = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    5,
  )
  expect [_top, ..rest] = native_script_walk_frames(stopped)
  expect _ = resume_native_script_walk_from_commitment(
    native_script_cursor_hash(stopped),
    encode_native_script_cursor(stopped),
    rest,
    script,
  )
  True
}

/// §11.3 resume check 2: the payload must re-digest to the cursor's
/// `script_digest`, so a position cannot be replayed against another script.
test resume_rejects_a_different_script() fail {
  let script = nested_script()
  let stopped = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    5,
  )
  expect _ = resume_native_script_walk_from_commitment(
    native_script_cursor_hash(stopped),
    encode_native_script_cursor(stopped),
    native_script_walk_frames(stopped),
    other_script(),
  )
  True
}

/// The same check, isolated from the length comparison beside it: this script
/// has `nested_script`'s exact length and different bytes, so only the digest
/// can tell them apart. Without it a substituted script of matching shape would
/// resume, and the verdict would be about bytes nobody committed.
test resume_rejects_a_same_length_different_script() fail {
  let script = nested_script()
  let twin = twin_script()
  expect bytearray.length(twin) == bytearray.length(script)
  expect twin != script
  let stopped = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    5,
  )
  expect _ = resume_native_script_walk_from_commitment(
    native_script_cursor_hash(stopped),
    encode_native_script_cursor(stopped),
    native_script_walk_frames(stopped),
    twin,
  )
  True
}

/// The same binding on the ordinary path: `native_script_run` re-establishes
/// the digest once per transaction, so a walk cannot be pointed at other bytes
/// after it was opened.
test run_rejects_a_payload_the_walk_was_not_opened_on() fail {
  let walk = open_native_script_walk(nested_script())
  expect _ = native_script_run(walk, other_script(), context(), unlimited)
  True
}

/// §11.3: `budget` is a count of steps. A negative one would mean "no limit"
/// rather than "no steps", because the recursion stops at zero.
test run_refuses_a_negative_budget() fail {
  let script = nested_script()
  expect _ = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    -1,
  )
  True
}

/// A script with trailing bytes evaluated to nothing, so the verdict refuses
/// rather than reporting the prefix's answer.
test verdict_refuses_a_script_with_trailing_bytes() fail {
  let script = bytearray.concat(signature(key_1), #"00")
  expect _ = native_script_verdict(
    native_script_run(
      open_native_script_walk(script),
      script,
      context(),
      unlimited,
    ),
  )
  True
}

/// The node bound is enforced, so a script cannot be made expensive by being
/// made big. `max_native_script_node_count` is 32; this is 33 signatures under
/// one `any`.
///
/// **Composite.** A 33-child `any` trips the child-count bound in the node
/// reader on its way to the node bound, so this is the *ordinary* shape of the
/// attack rather than the isolating vector — `run_refuses_more_nodes_than_the_bound_without_a_wide_child`
/// below is that.
test run_refuses_more_nodes_than_the_bound() fail {
  let script = #"8202"
    |> bytearray.concat(#"9821")
    |> bytearray.concat(
        list.foldr(
          list.range(1, 33),
          #"",
          fn(_i, tail) { bytearray.concat(signature(key_2), tail) },
        ),
      )
  expect _ = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    128,
  )
  True
}

/// The node bound, on its own. `all` of sixteen `all`-of-two-signatures is
/// **49 nodes** with no child count above sixteen and a depth of three, so
/// neither the child-count bound nor the depth bound is anywhere near refusing:
/// what stops this script is the running node count and nothing else.
///
/// This is the vector the old §11.5 asserted could not exist. It claimed the
/// child-count bound "refuses at that node first", which is true of the wide
/// shape above and false in general — a script gets big by being *bushy*, not
/// only by having one enormous node.
test run_refuses_more_nodes_than_the_bound_without_a_wide_child() fail {
  let script = bushy_script()
  expect _ = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    256,
  )
  True
}

/// 1 + 16 + 32 = 49 nodes, every child count 16 or 2, depth 3.
fn bushy_script() -> ByteArray {
  all_of(repeated(16, all_of([signature(key_1), signature(key_1)])))
}

/// The fixture really does sit where the vector above says it does — inside
/// every bound except the one it is testing. Without this the refusal could be
/// coming from the child-count bound after all, which is exactly the mistake
/// the backstop rebuttal made.
test the_bushy_fixture_exceeds_only_the_node_bound() {
  and {
    recursive_shape(bushy_script()) == Pair(3, 49),
    3 <= max_native_script_depth,
    49 > max_native_script_node_count,
    // Every child count in it — 16 and 2 — is inside the same bound the node
    // count breaks, so the node reader's `child_count` check cannot be what
    // refuses.
    16 <= max_native_script_node_count,
    // …and the definition refuses it too, on the same bound.
    recursive_check(bushy_script()) == None,
  }
}

/// §11.3: a walk is opened over a script, and the empty byte string is not one.
/// Nothing downstream would notice — a zero-length payload has a digest and a
/// cursor like any other — so the refusal has to be here.
test open_refuses_an_empty_payload() fail {
  expect _ = open_native_script_walk(#"")
  True
}

/// §11.3: `n-of-k` is the only three-element node and every other kind is two,
/// so the array header and the tag have to agree. Here they do not: an
/// `n-of-k` wearing a two-element header, which is otherwise a perfectly
/// traversable script — required 1, one signature child, no trailing bytes.
test run_refuses_a_node_whose_arity_disagrees_with_its_tag() fail {
  let script = #"8203"
    |> bytearray.concat(#"01")
    |> bytearray.concat(array_header(1))
    |> bytearray.concat(signature(key_1))
  expect _ = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    unlimited,
  )
  True
}

/// §5.3: a signature node's argument is a 28-byte key hash. Twenty-seven bytes
/// is well-formed CBOR that consumes the payload exactly and would evaluate to
/// a perfectly ordinary `False`.
test run_refuses_a_signature_of_the_wrong_key_width() fail {
  let script = #"8200"
    |> bytearray.concat(#"581b")
    |> bytearray.concat(bytearray.slice(key_1, 0, 26))
  expect _ = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    unlimited,
  )
  True
}

/// §11.3 has six node kinds and tag `6` is not one of them. The vector gives it
/// **no children**, which is what makes it isolate this check rather than the
/// frame encoding's kind domain: a childless compound resolves on the spot, so
/// with this check gone the script would traverse to `False` instead of
/// refusing.
test run_refuses_an_unknown_node_tag() fail {
  let script = #"820680"
  expect _ = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    unlimited,
  )
  True
}

/// An unfinished traversal has no verdict, which is what makes "complete"
/// meaningful rather than decorative.
test an_unfinished_traversal_has_no_verdict() {
  let script = nested_script()
  let stopped = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    3,
  )
  and {
    !native_script_walk_is_complete(stopped),
    native_script_verdict(stopped) == None,
  }
}

// ---------------------------------------------------------------------------
// §11.4 budget rows
// ---------------------------------------------------------------------------

/// §11.4 row 6. The whole eight-node traversal in one transaction.
test budget_native_script_pushdown_traversal() {
  run_to_completion(nested_script())
}

/// §11.4 row 7, the control: the same verdict from the recursive checker, which
/// materialises the script as `Data` and re-encodes it to prove canonicity.
test budget_native_script_recursive_control() {
  recursive_verdict(nested_script())
}

/// §11.4 row 8. One interrupted leg plus one resumed leg — what the discipline
/// actually costs when a traversal does not fit, including the resume's own
/// digest checks.
test budget_native_script_checkpoint_and_resume() {
  let script = nested_script()
  let stopped = native_script_run(
    open_native_script_walk(script),
    script,
    context(),
    5,
  )
  let resumed = resume_native_script_walk_from_commitment(
    native_script_cursor_hash(stopped),
    encode_native_script_cursor(stopped),
    native_script_walk_frames(stopped),
    script,
  )
  native_script_verdict(
    native_script_run(resumed, script, context(), unlimited),
  ) == Some(True)
}
