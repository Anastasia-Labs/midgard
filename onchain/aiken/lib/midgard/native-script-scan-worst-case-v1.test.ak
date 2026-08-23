//// The cost curve behind the #633 ExUnits verdict: what one node of the
//// one-shot tag-0 structural predicate
//// (`native_script_scan_v1.payload_structure_is_canonical_v1`) costs, measured
//// at four cardinalities on both shapes, with the two independent predicates
//// decision 0005 A3 offers as alternatives measured on the same vectors.
////
//// The readings are not published here. They are pinned, taken through
//// `scripts/run-focused-check.mjs`, in
//// `scripts/native-script-scan-exec-ledger-v1.json` and judged by
//// `scripts/verify-native-script-scan-exec-ledger-v1.mjs`, which also
//// recomputes the node counts below from the fresh readings rather than
//// restating them.
////
//// **The verdict, at the frozen basis.** `GOAL_SPEC_EXECUTION_BASIS_V1` is
//// 13,200,000 memory units / 8,000,000,000 cpu units
//// (`scripts/exec-ledger-within-basis-v1.mjs`; GOAL_SPEC §3.3 retires the
//// 11,200,000 figure earlier benches used). Against it:
////
//// * the deep worst case -- 16,341 bytes, 5,447 nodes, the largest payload the
////   arm can ever see -- costs about 1.07 billion memory units, **81x over**
////   the basis with nothing else in the transaction;
//// * the basis is exhausted at **67 nodes** on the deep shape and **76** on the
////   wide one;
//// * `native_script_v1.inspect_native_script`, the ready predicate A3 names,
////   exhausts it at about **100** nodes;
//// * a bare `cbor.deserialise` of the payload -- no scan at all, just getting
////   the bytes into `Data` -- exhausts it at about **250**.
////
//// The last figure is the one that closes the question: no driver can be
//// cheaper than reading the payload, so the 16,384-node bound is out of reach
//// for any one-shot on-chain predicate, not just for this one. That bound is a
//// STAGED-machine bound precisely because one L1 transaction cannot carry the
//// scan.
////
//// **Reach, measured rather than argued.** Wiring the predicate into
//// `ledger_output_v1.parse_script_ref`'s tag-0 arm and diffing a scratch
//// blueprint against c98cd0bf moves three validators (six rows, each a
//// `.main.spend` / `.main.else` pair) out of 444 --
//// `fraud_proofs/transition_trace/accepted_transaction_v1`,
//// `fraud_proofs/transition_trace/deposit_v1`, and
//// `fraud_proofs/validation_trace/cek_context_step_semantic_v1`. The first two
//// are the pair decision 0005 A3 predicted. The third is NOT named in the
//// ruling and reaches the arm through
//// `validation_machine_v1` -> `script_context_v1.tx_out_data_v1` ->
//// `decode_canonical_output`; unlike `deposit_v1` (whose projected output is
//// built with `script_ref: None`, `transition-trace/proof.ak:901`, so its hash
//// moves and its behaviour does not) that is a live runtime reach, on a
//// validator that already spends its budget on a CEK step. It makes the budget
//// verdict strictly worse, not better.
////
//// The two reachable worst cases are in their own modules --
//// `native-script-scan-worst-case-deep-v1.test.ak` and
//// `native-script-scan-worst-case-wide-v1.test.ak` -- so a reading can be taken
//// from either one without the other being evaluated in the same process. Every
//// vector is built programmatically by
//// `midgard/native_script_payload_vectors_v1`.
////
//// **Memory.** Aiken evaluates a module's selected tests in PARALLEL. The deep
//// worst case peaks around 12.6 GB of host memory on its own, so a full
//// `aiken check` over the whole tree -- which evaluates all three of these
//// modules in one process -- needs `RAYON_NUM_THREADS=1` to stay under 16 GiB.
//// It passes there at a 12.5 GB peak. No single test in this family needs more
//// than that; the exhaustion is the evaluator's concurrency, not any one
//// vector.

use aiken/cbor
use aiken/primitive/bytearray
use midgard/native_script_payload_vectors_v1 as vectors
use midgard/native_script_scan_v1
use midgard/native_script_v1

// --- the predicate's own curve ---

test curve_wide_00064() {
  native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.wide_payload_n(64),
  )
}

test curve_wide_00256() {
  native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.wide_payload_n(256),
  )
}

test curve_wide_01024() {
  native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.wide_payload_n(1024),
  )
}

test curve_wide_04096() {
  native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.wide_payload_n(4096),
  )
}

test curve_deep_00064() {
  native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.deep_payload(195),
  )
}

test curve_deep_00256() {
  native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.deep_payload(771),
  )
}

test curve_deep_01024() {
  native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.deep_payload(3075),
  )
}

test curve_deep_04096() {
  native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.deep_payload(12291),
  )
}

// --- builder-only baselines, so the predicate's marginal cost is a difference ---

test worst_case_deep_builder_only() {
  bytearray.length(vectors.deep_payload(vectors.max_reachable_payload_bytes)) == 16341
}

test worst_case_wide_builder_only() {
  bytearray.length(vectors.wide_payload()) == 16340
}

// --- comparison arms: the two independent lower bounds ---

test cmp_inspect_wide_00064() {
  native_script_v1.inspect_native_script(vectors.wide_payload_n(64), 0, 0, []) != None
}

test cmp_inspect_wide_01024() {
  native_script_v1.inspect_native_script(vectors.wide_payload_n(1024), 0, 0, []) != None
}

test cmp_inspect_deep_01024() {
  native_script_v1.inspect_native_script(vectors.deep_payload(3075), 0, 0, []) != None
}

test cmp_cbor_wide_00064() {
  cbor.deserialise(vectors.wide_payload_n(64)) != None
}

test cmp_cbor_wide_01024() {
  cbor.deserialise(vectors.wide_payload_n(1024)) != None
}

// --- neutralisation selectors ---

/// Shape guard. Every curve row's cardinality is a number the ledger's
/// derivation divides by, so shrinking a vector to make a row cheaper has to
/// come through here first.
test curve_vectors_carry_the_node_counts_the_ledger_divides_by() {
  and {
    vectors.wide_payload_nodes(64) == 65,
    vectors.wide_payload_nodes(256) == 257,
    vectors.wide_payload_nodes(1024) == 1025,
    vectors.wide_payload_nodes(4096) == 4097,
    vectors.deep_payload_nodes(195) == 65,
    vectors.deep_payload_nodes(771) == 257,
    vectors.deep_payload_nodes(3075) == 1025,
    vectors.deep_payload_nodes(12291) == 4097,
    bytearray.length(vectors.wide_payload_n(64)) == 196,
    bytearray.length(vectors.deep_payload(195)) == 195,
  }
}

/// Discrimination guard. A predicate that returned `True` for anything would
/// price the same and mean nothing, and the comparison arms would be the same
/// kind of nothing. The scan predicate and `inspect_native_script` both refuse
/// `malformed_payload`, whose tag 7 is no native-script node; `cbor.deserialise`
/// does NOT -- `82 07 00` is perfectly good CBOR -- which is the point of the
/// third arm. It is a lower bound on any driver's cost, not a rival predicate,
/// so it is discriminated with bytes that are not CBOR at all.
test harness_predicates_refuse_a_malformed_payload() {
  and {
    !native_script_scan_v1.payload_structure_is_canonical_v1(
      vectors.malformed_payload,
    ),
    native_script_v1.inspect_native_script(vectors.malformed_payload, 0, 0, []) == None,
    cbor.deserialise(vectors.malformed_payload) != None,
    cbor.deserialise(#"82") == None,
  }
}
