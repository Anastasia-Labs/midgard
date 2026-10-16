//// Golden agreement between the typed ledger-output-proof datum-traversal
//// step and the routed narrow datum stage functions of the shared step
//// family, over the honest span/scalar claims a step dispatcher action
//// carries (see
//// docs/fault-proofs/decisions/0004-checkpointed-ledger-output-facts.md).
//// The chain below streams a large constructor with an eight-byte index
//// (`d866821b01000000000000009f01ff`), so a single traversal exercises
//// every datum stage role: head large-constructor, large-constructor
//// streaming, large-fields completion, head scalar, integer advance and
//// attach, close, list fold, frame finalize and the terminal finish
//// hand-off. Substitution attacks pin the fail-closed side: a shifted or
//// mutated span claim, a substituted scalar claim and a cross-family claim
//// swap are each refused at the exact predicate that binds them.

use aiken/builtin
use aiken/crypto
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cbor_deserialise
use midgard/cek_data_frame_v1 as frame
use midgard/cek_data_integer_v1 as integer_control
use midgard/cek_data_traverse_v1 as traverse
use midgard/cek_data_v1
use midgard/cek_source_blob_v1 as source_blob
use midgard/fraud_proofs/native_tx/codec.{slice_len}
use midgard/ledger_output_proof_datum as narrow_datum
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_span as span
use midgard/ledger_output_proof_stages as narrow
use midgard/ledger_output_proof_v1 as proof
use midgard/validation_merkle_v1 as merkle

/// `a3 00 address 01 empty-value 02 bytes(15)` where the datum item is a large
/// constructor (index 2^56, minimal eight-byte head) over one integer field.
const large_datum_output =
  #"a300581d7811111111111111111111111111111111111111111111111111111111018200a0024fd866821b01000000000000009f01ff"

fn one_chunk_material(
  output_index: Int,
  output: ByteArray,
) -> Pair<ByteArray, bounded_item_v1.ChunkProofV1> {
  let frontier = merkle.append_leaf(
    0,
    merkle.empty_frontier(),
    bounded_item_v1.hash_chunk(2, output_index, 0, output),
  )
  let commitment = bounded_item_v1.commitment(
    2,
    output_index,
    bytearray.length(output),
    frontier,
  )
  Pair(
    commitment,
    bounded_item_v1.ChunkProofV1 {
      version: 1,
      field_index: 2,
      item_index: output_index,
      total_length: bytearray.length(output),
      chunk_index: 0,
      chunk: output,
      frontier,
      siblings: [],
    },
  )
}

fn advanced(
  result: Option<proof.LedgerOutputProofStepResultV1>,
) -> proof.LedgerOutputProofControlV1 {
  expect Some(proof.LedgerOutputProofAdvanced { control }) = result
  control
}

/// Drive the typed proof to the datum-traversal stage: `NoWitness` where the
/// step accepts it, otherwise the single whole-output chunk proof.
fn until_datum(
  control: proof.LedgerOutputProofControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  fuel: Int,
) -> proof.LedgerOutputProofControlV1 {
  expect fuel > 0
  if control.stage == proof.stage_datum_traversal {
    control
  } else {
    let witness = when
      proof.step_v1(control, proof.LedgerOutputProofNoWitness)
    is {
      Some(_) -> proof.LedgerOutputProofNoWitness
      None ->
        proof.LedgerOutputProofChunks { chunk_proof, next_chunk_proof: None }
    }
    until_datum(
      advanced(proof.step_v1(control, witness)),
      chunk_proof,
      fuel - 1,
    )
  }
}

fn datum_ready(
  output: ByteArray,
) -> Pair<proof.LedgerOutputProofControlV1, bounded_item_v1.ChunkProofV1> {
  let Pair(commitment, chunk_proof) = one_chunk_material(0, output)
  let initial = proof.initial_control_v1(
    0,
    bytearray.length(output),
    commitment,
  )
  let at_datum = until_datum(initial, chunk_proof, 64)
  // The span-attach step records the window from the datum head's span to the
  // end of the output, which every datum-stage step below consumes.
  Pair(
    advanced(
      proof.step_v1(
        at_datum,
        proof.LedgerOutputProofSpanAttach {
          chunk_proof,
          next_chunk_proof: None,
        },
      ),
    ),
    chunk_proof,
  )
}

/// The bytes of the window `control` records, sliced out of `output`.
fn recorded_window(
  control: proof.LedgerOutputProofControlV1,
  output: ByteArray,
) -> ByteArray {
  expect Some(window) = control.span_window
  slice_len(output, window.start, window.length)
}

/// `recorded_window` with its first byte flipped.
fn mutated_window(
  control: proof.LedgerOutputProofControlV1,
  output: ByteArray,
) -> ByteArray {
  let window = recorded_window(control, output)
  bytearray.concat(#"00", slice_len(window, 1, bytearray.length(window) - 1))
}

fn active_datum(
  control: proof.LedgerOutputProofControlV1,
) -> traverse.DataTraverseControlV1 {
  expect Some(datum_control) = control.datum
  datum_control
}

/// The honest scalar claim: the traverse prefix plus the active scalar
/// sub-control, exactly the value `integer_scalar_claim_is_exact` /
/// `bytes_scalar_claim_is_exact` attest.
fn honest_scalar_claim(datum_control: traverse.DataTraverseControlV1) -> Data {
  let scalar_data: Data = if datum_control.stage == traverse.stage_integer {
    expect Some(scalar) = datum_control.integer
    let data: Data = scalar
    data
  } else {
    expect datum_control.stage == traverse.stage_bytes
    expect Some(scalar) = datum_control.bytes
    let data: Data = scalar
    data
  }
  builtin.constr_data(
    0,
    [
      builtin.i_data(datum_control.source_start),
      builtin.i_data(datum_control.source_length),
      builtin.i_data(datum_control.offset),
      builtin.b_data(datum_control.frame_root),
      scalar_data,
    ],
  )
}

/// Route one datum action to the narrow stage function of its role, exactly
/// the way the step-plan role table does.
fn routed(
  control_frame: raw.Frame,
  witness: Data,
  datum_control: traverse.DataTraverseControlV1,
  action: traverse.DataTraverseActionV1,
) -> Option<narrow.StepResult> {
  when action is {
    traverse.HeadScalar -> narrow_datum.head_scalar(control_frame, witness)
    traverse.HeadSequence -> narrow_datum.head_sequence(control_frame, witness)
    traverse.HeadMap { .. } -> narrow_datum.head_map(control_frame, witness)
    traverse.HeadLargeConstructor ->
      narrow_datum.head_large_constructor(control_frame, witness)
    traverse.AttachScalar { .. } ->
      if datum_control.stage == traverse.stage_integer {
        narrow_datum.attach_integer(
          control_frame,
          witness,
          honest_scalar_claim(datum_control),
        )
      } else {
        narrow_datum.attach_bytes(
          control_frame,
          witness,
          honest_scalar_claim(datum_control),
        )
      }
    traverse.NoAction ->
      if datum_control.stage == traverse.stage_integer {
        narrow_datum.advance_integer(
          control_frame,
          witness,
          honest_scalar_claim(datum_control),
        )
      } else if datum_control.stage == traverse.stage_bytes {
        narrow_datum.advance_bytes(
          control_frame,
          witness,
          honest_scalar_claim(datum_control),
        )
      } else if datum_control.stage == traverse.stage_large_constructor {
        narrow_datum.advance_large_constructor(control_frame, witness)
      } else if datum_control.stage == traverse.stage_large_fields {
        narrow_datum.advance_large_fields(control_frame, witness)
      } else {
        expect datum_control.stage == traverse.stage_close
        narrow_datum.close(control_frame, witness)
      }
    traverse.FoldList { .. } -> narrow_datum.fold_list(control_frame, witness)
    traverse.FoldMap { .. } -> narrow_datum.fold_map(control_frame, witness)
    traverse.FinalizeFrame { .. } ->
      narrow_datum.finalize_frame(control_frame, witness)
  }
}

/// One golden datum step: the typed monolith and the routed narrow function
/// advance to byte-identical successors under the honest witness and claims.
fn same_datum_step(
  control: proof.LedgerOutputProofControlV1,
  action: traverse.DataTraverseActionV1,
  _chunk_proof: bounded_item_v1.ChunkProofV1,
  output: ByteArray,
) -> proof.LedgerOutputProofControlV1 {
  let datum_control = active_datum(control)
  let typed_witness = proof.LedgerOutputProofDatum {
    action,
    window: when traverse.next_source_span_v1(datum_control) is {
      None -> None
      Some(_) -> Some(recorded_window(control, output))
    },
  }
  let next = advanced(proof.step_v1(control, typed_witness))
  let witness_data: Data = typed_witness
  let control_frame = raw.open(proof.encode_control_v1(control))
  expect Some(narrow.Advanced { control: raw_next }) = routed(
    control_frame,
    witness_data,
    datum_control,
    action,
  )
  expect raw.encode(raw_next) == proof.encode_control_v1(next)
  next
}

/// Golden `NoAction` steps while the datum control stays in `stage`.
fn same_no_action_while(
  control: proof.LedgerOutputProofControlV1,
  stage: Int,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  output: ByteArray,
  fuel: Int,
) -> proof.LedgerOutputProofControlV1 {
  expect fuel > 0
  if active_datum(control).stage == stage {
    same_no_action_while(
      same_datum_step(control, traverse.NoAction, chunk_proof, output),
      stage,
      chunk_proof,
      output,
      fuel - 1,
    )
  } else {
    control
  }
}

/// Golden integer `NoAction` advances until the active integer sub-control
/// reaches its terminal stage.
fn same_integer_advances(
  control: proof.LedgerOutputProofControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  output: ByteArray,
  fuel: Int,
) -> proof.LedgerOutputProofControlV1 {
  expect fuel > 0
  let datum_control = active_datum(control)
  expect datum_control.stage == traverse.stage_integer
  expect Some(integer) = datum_control.integer
  if integer.stage == integer_control.stage_terminal {
    control
  } else {
    same_integer_advances(
      same_datum_step(control, traverse.NoAction, chunk_proof, output),
      chunk_proof,
      output,
      fuel - 1,
    )
  }
}

test datum_family_golden_agreement_over_a_streamed_large_constructor() {
  let Pair(ready, chunk_proof) = datum_ready(large_datum_output)
  // Stage role 16: head large-constructor.
  let opened = same_datum_step(
    ready,
    traverse.HeadLargeConstructor,
    chunk_proof,
    large_datum_output,
  )
  // Stage role 20: large-constructor streaming NoAction steps, including the
  // terminal-integer completion into the large-fields stage.
  let large_fields = same_no_action_while(
    opened,
    traverse.stage_large_constructor,
    chunk_proof,
    large_datum_output,
    256,
  )
  let fields_control = active_datum(large_fields)
  expect fields_control.stage == traverse.stage_large_fields
  expect Some(streamed_integer) = fields_control.integer
  let initial_frame = frame.initial_large_constr_frame_v1(
    {
      expect Some(blob) = streamed_integer.blob
      expect Some(root) = source_blob.finalize_v1(blob)
      root
    },
    streamed_integer.source_length,
    streamed_integer.memory,
    fields_control.frame_root,
  )
  // Stage role 21: the large-fields completion opens the field frame.
  let fields_opened = same_datum_step(
    large_fields,
    traverse.NoAction,
    chunk_proof,
    large_datum_output,
  )
  expect active_datum(fields_opened).stage == traverse.stage_head
  // Stage role 4: head scalar.
  let field_opened = same_datum_step(
    fields_opened,
    traverse.HeadScalar,
    chunk_proof,
    large_datum_output,
  )
  // Stage role 5: integer advances to the terminal scalar.
  let field_terminal = same_integer_advances(
    field_opened,
    chunk_proof,
    large_datum_output,
    256,
  )
  expect Some(field_integer) = active_datum(field_terminal).integer
  expect Some(field_summary) = integer_control.finalize_v1(field_integer)
  // Stage role 7: attach the completed integer into the open frame.
  let field_attached = same_datum_step(
    field_terminal,
    traverse.AttachScalar { parent: Some(initial_frame) },
    chunk_proof,
    large_datum_output,
  )
  expect active_datum(field_attached).stage == traverse.stage_close
  // Stage role 22: the closing break byte.
  let closed = same_datum_step(
    field_attached,
    traverse.NoAction,
    chunk_proof,
    large_datum_output,
  )
  expect active_datum(closed).stage == traverse.stage_fold
  // Stage role 6: fold the attached child into the frame.
  expect Some(full_frame) = frame.append_child_v1(initial_frame, field_summary)
  let folded = same_datum_step(
    closed,
    traverse.FoldList {
      frame: full_frame,
      child_index: 0,
      child: field_summary,
      siblings: [],
    },
    chunk_proof,
    large_datum_output,
  )
  // Stage role 3: finalize the frame to the terminal traversal.
  expect Some(folded_frame) = frame.fold_list_child_v1(
    full_frame,
    0,
    field_summary,
    [],
  )
  let terminal = same_datum_step(
    folded,
    traverse.FinalizeFrame { frame: folded_frame, parent: None },
    chunk_proof,
    large_datum_output,
  )
  let terminal_datum = active_datum(terminal)
  expect terminal_datum.stage == traverse.stage_terminal
  // Stage role 19: the terminal finish hands the proof to the next stage.
  let finished = advanced(
    proof.step_v1(terminal, proof.LedgerOutputProofNoWitness),
  )
  expect Some(narrow.Advanced { control: raw_finished }) = narrow_datum.finish(
    raw.open(proof.encode_control_v1(terminal)),
    builtin.constr_data(0, []),
  )
  and {
    raw.encode(raw_finished) == proof.encode_control_v1(finished),
    // The datum-free reference script routes the finish to the terminal
    // proof stage.
    finished.stage == proof.stage_terminal,
    {
      expect Some(summary) = traverse.finalize_v1(terminal_datum)
      // The TypeScript twin's summary root for the datum item.
      summary.root == #"b5822d21df1af70ea902b57f6bebc7693e64cf2be6cb34804b6cb346e8baacfb"
    },
  }
}

/// Window bytes that do not hash to the recorded span-window digest are
/// refused by the consuming stage yield's binding clause.
test datum_window_with_mutated_bytes_fails() fail {
  let Pair(ready, _chunk_proof) = datum_ready(large_datum_output)
  let mutated = mutated_window(ready, large_datum_output)
  let witness_data: Data = proof.LedgerOutputProofDatum {
    action: traverse.HeadLargeConstructor,
    window: Some(mutated),
  }
  narrow_datum.head_large_constructor(
    raw.open(proof.encode_control_v1(ready)),
    witness_data,
  ) == None
}

/// A datum step whose control never went through the span-attach step has no
/// recorded window for the binding clause to accept.
test datum_window_without_span_attach_fails() fail {
  let Pair(commitment, chunk_proof) = one_chunk_material(0, large_datum_output)
  let initial = proof.initial_control_v1(
    0,
    bytearray.length(large_datum_output),
    commitment,
  )
  let unattached = until_datum(initial, chunk_proof, 64)
  let witness_data: Data = proof.LedgerOutputProofDatum {
    action: traverse.HeadLargeConstructor,
    window: Some(large_datum_output),
  }
  narrow_datum.head_large_constructor(
    raw.open(proof.encode_control_v1(unattached)),
    witness_data,
  ) == None
}

/// Mutated span bytes in a content claim are exactly what the span
/// attestation yield refuses: the authenticated span at the claimed
/// coordinates is not the mutated content.
test mutated_span_content_is_refused_by_the_span_attestation() {
  let Pair(commitment, chunk_proof) = one_chunk_material(0, large_datum_output)
  let Pair(ready, _proof) = datum_ready(large_datum_output)
  let datum_control = active_datum(ready)
  expect Some(span) = traverse.next_source_span_v1(datum_control)
  let honest = slice_len(large_datum_output, span.absolute_start, span.length)
  let mutated = bytearray.concat(
    #"00",
    slice_len(large_datum_output, span.absolute_start + 1, span.length - 1),
  )
  let witness = proof.LedgerOutputProofChunks {
    chunk_proof,
    next_chunk_proof: None,
  }
  let authenticated = proof.authenticated_output_span_for_output(
    0,
    bytearray.length(large_datum_output),
    commitment,
    span.absolute_start,
    span.length,
    witness,
  )
  and {
    honest != mutated,
    authenticated == Some(honest),
    authenticated != Some(mutated),
  }
}

/// Mutated window bytes in the large-fields completion witness fail the
/// stage function's window binding closed.
test large_fields_with_mutated_window_fails_closed() fail {
  let Pair(ready, chunk_proof) = datum_ready(large_datum_output)
  let opened = same_datum_step(
    ready,
    traverse.HeadLargeConstructor,
    chunk_proof,
    large_datum_output,
  )
  let large_fields = same_no_action_while(
    opened,
    traverse.stage_large_constructor,
    chunk_proof,
    large_datum_output,
    256,
  )
  let mutated = mutated_window(large_fields, large_datum_output)
  let witness_data: Data = proof.LedgerOutputProofDatum {
    action: traverse.NoAction,
    window: Some(mutated),
  }
  narrow_datum.advance_large_fields(
    raw.open(proof.encode_control_v1(large_fields)),
    witness_data,
  ) == None
}

/// The integer scalar attestation accepts exactly the honest claim and
/// refuses a claim whose prefix or scalar was substituted.
test integer_scalar_claim_binds_the_whole_claim() {
  let Pair(ready, chunk_proof) = datum_ready(large_datum_output)
  let opened = same_datum_step(
    ready,
    traverse.HeadLargeConstructor,
    chunk_proof,
    large_datum_output,
  )
  let large_fields = same_no_action_while(
    opened,
    traverse.stage_large_constructor,
    chunk_proof,
    large_datum_output,
    256,
  )
  let fields_opened = same_datum_step(
    large_fields,
    traverse.NoAction,
    chunk_proof,
    large_datum_output,
  )
  let field_opened = same_datum_step(
    fields_opened,
    traverse.HeadScalar,
    chunk_proof,
    large_datum_output,
  )
  let datum_control = active_datum(field_opened)
  expect datum_control.stage == traverse.stage_integer
  let control_frame = raw.open(proof.encode_control_v1(field_opened))
  let honest = honest_scalar_claim(datum_control)
  expect Pair(0, [start,
        length, offset, root, scalar]) = builtin.un_constr_data(honest)
  let shifted_offset = builtin.constr_data(
    0,
    [start, length, builtin.i_data(builtin.un_i_data(offset) + 1), root, scalar],
  )
  and {
    narrow_datum.integer_scalar_claim_is_exact(control_frame, honest),
    !narrow_datum.integer_scalar_claim_is_exact(control_frame, shifted_offset),
  }
}

/// A cross-family swap — the bytes-family attestation asked to certify an
/// integer-stage control — fails closed at the stage clause.
test bytes_scalar_attestation_refuses_an_integer_stage_control() fail {
  let Pair(ready, chunk_proof) = datum_ready(large_datum_output)
  let opened = same_datum_step(
    ready,
    traverse.HeadLargeConstructor,
    chunk_proof,
    large_datum_output,
  )
  let large_fields = same_no_action_while(
    opened,
    traverse.stage_large_constructor,
    chunk_proof,
    large_datum_output,
    256,
  )
  let fields_opened = same_datum_step(
    large_fields,
    traverse.NoAction,
    chunk_proof,
    large_datum_output,
  )
  let field_opened = same_datum_step(
    fields_opened,
    traverse.HeadScalar,
    chunk_proof,
    large_datum_output,
  )
  let datum_control = active_datum(field_opened)
  narrow_datum.bytes_scalar_claim_is_exact(
    raw.open(proof.encode_control_v1(field_opened)),
    honest_scalar_claim(datum_control),
  ) == False
}

/// The finish role refuses an active (non-terminal) datum traversal.
test finish_refuses_an_active_datum_traversal() fail {
  let Pair(ready, _chunk_proof) = datum_ready(large_datum_output)
  narrow_datum.finish(
    raw.open(proof.encode_control_v1(ready)),
    builtin.constr_data(0, []),
  ) == None
}

test datum_family_golden_agreement_over_a_streamed_chunked_integer() {
  let output = #"a300581d7811111111111111111111111111111111111111111111111111111111018200a0025847c25f5840010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004100ff"
  let Pair(ready, chunk_proof) = datum_ready(output)
  let opened = same_datum_step(ready, traverse.HeadScalar, chunk_proof, output)
  let terminal = same_integer_advances(opened, chunk_proof, output, 256)
  expect Some(integer) = active_datum(terminal).integer
  expect Some(summary) = integer_control.finalize_v1(integer)
  let attached = same_datum_step(
    terminal,
    traverse.AttachScalar { parent: None },
    chunk_proof,
    output,
  )
  and {
    summary.cbor_length == 71,
    summary.memory == 69,
    active_datum(attached).stage == traverse.stage_terminal,
  }
}

/// A tag-102 index spelled as a chunked bignum is not canonical Data: no head
/// action opens it on either path, and the typed step reports the output
/// invalid on the exact head window.
test chunked_constructor_alternative_is_unreadable_at_the_head() {
  let output = #"a300581d7811111111111111111111111111111111111111111111111111111111018200a002584bd86682c25f5840010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004100ff80"
  let Pair(ready, _chunk_proof) = datum_ready(output)
  let window = Some(recorded_window(ready, output))
  let head = proof.LedgerOutputProofDatum {
    action: traverse.HeadLargeConstructor,
    window,
  }
  let head_data: Data = head
  let no_action = proof.LedgerOutputProofDatum {
    action: traverse.NoAction,
    window,
  }
  and {
    proof.step_v1(ready, head) == None,
    narrow_datum.head_large_constructor(
      raw.open(proof.encode_control_v1(ready)),
      head_data,
    ) == None,
    proof.step_v1(ready, no_action) == Some(
      proof.LedgerOutputProofInvalidOutput,
    ),
  }
}

// An ordinary integer attestation must not authenticate a large-constructor
// control. Otherwise a caller can substitute ordinary role 5 or 7 and consume
// the constructor index as a child scalar, bypassing constructor grammar.
fn large_control_cannot_attest_as_integer(stage: Int) -> Bool {
  let output = #"a300581d7811111111111111111111111111111111111111111111111111111111018200a00248d8668218809f01ff"
  let Pair(ready, chunk_proof) = datum_ready(output)
  let opened = same_datum_step(
    ready,
    traverse.HeadLargeConstructor,
    chunk_proof,
    output,
  )
  let current = if stage == traverse.stage_large_constructor {
    opened
  } else {
    same_no_action_while(
      opened,
      traverse.stage_large_constructor,
      chunk_proof,
      output,
      256,
    )
  }
  let datum_control = active_datum(current)
  expect Some(scalar) = datum_control.integer
  let scalar_data: Data = scalar
  let claim = builtin.constr_data(
    0,
    [
      builtin.i_data(datum_control.source_start),
      builtin.i_data(datum_control.source_length),
      builtin.i_data(datum_control.offset),
      builtin.b_data(datum_control.frame_root),
      scalar_data,
    ],
  )
  narrow_datum.integer_scalar_claim_is_exact(
    raw.open(proof.encode_control_v1(current)),
    claim,
  )
}

test constructor_control_refuses_ordinary_integer_attestation() fail {
  large_control_cannot_attest_as_integer(traverse.stage_large_constructor)
}

test fields_control_refuses_ordinary_integer_attestation() fail {
  large_control_cannot_attest_as_integer(traverse.stage_large_fields)
}

// Outputs that hold an empty byte string. An empty span reads no output
// bytes, so its datum step takes no window and no span attach precedes it.
// Each row below is an honest output whose proof the TypeScript trace
// builder built; demo/midgard-validation/tests/ledger-output-proof-empty-span.test.ts
// pins the same witness-list and terminal hashes. A chunk-proof witness is
// written as its bare constructor (1 for chunks, 5 for a span attach) and is
// rebuilt here from the output's single chunk.

fn replay_witness(
  data: Data,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> proof.LedgerOutputProofWitnessV1 {
  let Pair(tag, fields) = builtin.un_constr_data(data)
  if and {
    tag == 1,
    fields == [],
  } {
    proof.LedgerOutputProofChunks { chunk_proof, next_chunk_proof: None }
  } else if and {
    tag == 5,
    fields == [],
  } {
    proof.LedgerOutputProofSpanAttach { chunk_proof, next_chunk_proof: None }
  } else {
    expect witness: proof.LedgerOutputProofWitnessV1 = data
    witness
  }
}

/// At a datum step over an empty span, neither the typed step nor the split
/// span-attach step admits a span attach, and the typed step refuses every
/// window.
fn empty_span_refusals(
  control: proof.LedgerOutputProofControlV1,
  action: traverse.DataTraverseActionV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  output: ByteArray,
) -> Bool {
  let attach = proof.LedgerOutputProofSpanAttach {
    chunk_proof,
    next_chunk_proof: None,
  }
  let refuses_window = fn(window) {
    proof.step_v1(
      control,
      proof.LedgerOutputProofDatum { action, window: Some(window) },
    ) == None
  }
  and {
    proof.demanded_span_v1(control) == None,
    proof.step_v1(control, attach) == None,
    refuses_window(#""),
    refuses_window(output),
    when control.span_window is {
      None -> True
      Some(_) -> refuses_window(recorded_window(control, output))
    },
  }
}

/// Whether the datum step at `control` reads an empty span.
fn reads_empty_span(control: proof.LedgerOutputProofControlV1) -> Bool {
  when traverse.next_source_span_v1(active_datum(control)) is {
    None -> False
    Some(source_span) -> source_span.length == 0
  }
}

/// One replayed step. A span attach and a datum step also run through their
/// split stage function, to the same successor. Returns the successor and
/// whether the step read an empty span.
fn replay_step(
  control: proof.LedgerOutputProofControlV1,
  witness: proof.LedgerOutputProofWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  output: ByteArray,
) -> (proof.LedgerOutputProofControlV1, Int) {
  let next = advanced(proof.step_v1(control, witness))
  let control_frame = raw.open(proof.encode_control_v1(control))
  when witness is {
    proof.LedgerOutputProofSpanAttach { .. } -> {
      expect Some(narrow.Advanced { control: raw_next }) = span.attach(
        control_frame,
        witness,
      )
      expect raw.encode(raw_next) == proof.encode_control_v1(next)
      (next, 0)
    }
    proof.LedgerOutputProofDatum { action, window } -> {
      let witness_data: Data = witness
      expect Some(narrow.Advanced { control: raw_next }) = routed(
        control_frame,
        witness_data,
        active_datum(control),
        action,
      )
      expect raw.encode(raw_next) == proof.encode_control_v1(next)
      if reads_empty_span(control) {
        expect window == None
        expect empty_span_refusals(control, action, chunk_proof, output)
        (next, 1)
      } else {
        (next, 0)
      }
    }
    _ -> (next, 0)
  }
}

fn replay(
  control: proof.LedgerOutputProofControlV1,
  witnesses: List<Data>,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  output: ByteArray,
  empty_steps: Int,
) -> (proof.LedgerOutputProofControlV1, Int) {
  when witnesses is {
    [] -> (control, empty_steps)
    [data, ..rest] -> {
      let (next, empty) = replay_step(
        control,
        replay_witness(data, chunk_proof),
        chunk_proof,
        output,
      )
      replay(next, rest, chunk_proof, output, empty_steps + empty)
    }
  }
}

/// The TypeScript-built proof of the one-chunk `output` replays to an exact
/// terminal whose control hash is `terminal_hash` and whose spend datum is
/// `datum`, reading `empty_steps` empty spans on the way.
fn replays(
  output: ByteArray,
  datum: ByteArray,
  witnesses: ByteArray,
  witnesses_hash: ByteArray,
  terminal_hash: ByteArray,
  empty_steps: Int,
) -> Bool {
  let Pair(commitment, chunk_proof) = one_chunk_material(0, output)
  let initial = proof.initial_control_v1(
    0,
    bytearray.length(output),
    commitment,
  )
  let witness_list <- cbor_deserialise.expect_deserialise(witnesses)
  // The decoder refuses an empty byte string at the very end of its input
  // (see cbor-deserialise.ak), so the datum is read as the head of a list.
  let datum_list <- cbor_deserialise.expect_deserialise(
    bytearray.concat(#"82", bytearray.concat(datum, #"00")),
  )
  expect [datum_data, _] = builtin.un_list_data(datum_list)
  let (terminal, empty) = replay(
    initial,
    builtin.un_list_data(witness_list),
    chunk_proof,
    output,
    0,
  )
  and {
    crypto.blake2b_256(witnesses) == witnesses_hash,
    empty == empty_steps,
    proof.terminal_is_exact_v1(terminal),
    crypto.blake2b_256(proof.encode_control_v1(terminal)) == terminal_hash,
    proof.cardano_spend_datum_summary_v1(terminal) == Some(
      cek_data_v1.semantic_data_summary_v1(builtin.constr_data(0, [datum_data])),
    ),
  }
}

/// The control of the first datum step that reads an empty span, and that
/// step's action.
fn at_empty_span(
  control: proof.LedgerOutputProofControlV1,
  witnesses: List<Data>,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> (proof.LedgerOutputProofControlV1, traverse.DataTraverseActionV1) {
  expect [data, ..rest] = witnesses
  let witness = replay_witness(data, chunk_proof)
  when witness is {
    proof.LedgerOutputProofDatum { action, .. } ->
      if reads_empty_span(control) {
        (control, action)
      } else {
        at_empty_span(
          advanced(proof.step_v1(control, witness)),
          rest,
          chunk_proof,
        )
      }
    _ ->
      at_empty_span(
        advanced(proof.step_v1(control, witness)),
        rest,
        chunk_proof,
      )
  }
}

/// The `[B""]` output, whose datum's empty byte string reads an empty span
/// at byte 45 of the 46-byte output.
const list_empty_output: ByteArray =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a002439f40ff"

/// The TypeScript-built witness list proving `list_empty_output`.
const list_empty_witnesses: ByteArray =
  #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87b80d8799f439f40ffffffd87c9fd87a80d8799f439f40ffffffd87c9fd87980d8799f439f40ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000008000d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87980d8799f439f40ffffffd87c9fd87f9fd8799f02004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffff00d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff80ffd87a80ffd87c9fd905019fd8799f02004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff01d8799f582016925ec20d2cfa48b7d727ce43aabab6f695dcc94463febd74e2c67631181800010105ffffd87a80ffd87a80ffd87980ff"

/// The first datum step of a one-chunk proof that reads an empty span: its
/// control and action.
fn first_empty_span_step(
  output: ByteArray,
  witnesses: ByteArray,
) -> (proof.LedgerOutputProofControlV1, traverse.DataTraverseActionV1) {
  let Pair(commitment, chunk_proof) = one_chunk_material(0, output)
  let initial = proof.initial_control_v1(
    0,
    bytearray.length(output),
    commitment,
  )
  let witness_list <- cbor_deserialise.expect_deserialise(witnesses)
  at_empty_span(initial, builtin.un_list_data(witness_list), chunk_proof)
}

/// The split datum step over `control`'s frame with `total_item` in place
/// of the encoded output length (frame bytes 4..5).
fn split_empty_span_step(
  control: proof.LedgerOutputProofControlV1,
  action: traverse.DataTraverseActionV1,
  total_item: ByteArray,
) -> Option<narrow.StepResult> {
  let encoded = proof.encode_control_v1(control)
  expect slice_len(encoded, 4, 2) == #"182e"
  let patched = bytearray.concat(
    slice_len(encoded, 0, 4),
    bytearray.concat(
      total_item,
      slice_len(encoded, 6, bytearray.length(encoded) - 6),
    ),
  )
  let witness_data: Data = proof.LedgerOutputProofDatum { action, window: None }
  routed(raw.open(patched), witness_data, active_datum(control), action)
}

/// The split datum step refuses a window over an empty span.
test split_datum_step_refuses_a_window_over_an_empty_span() fail {
  let (control, action) = first_empty_span_step(
    list_empty_output,
    list_empty_witnesses,
  )
  let witness_data: Data = proof.LedgerOutputProofDatum {
    action,
    window: Some(#""),
  }
  routed(
    raw.open(proof.encode_control_v1(control)),
    witness_data,
    active_datum(control),
    action,
  ) != None
}

/// The split datum step reads an empty span starting at the frame's output
/// length (45 here, the span's start) and at the honest length 46.
test split_empty_span_may_start_at_the_output_length() {
  let (control, action) = first_empty_span_step(
    list_empty_output,
    list_empty_witnesses,
  )
  expect Some(source_span) = traverse.next_source_span_v1(active_datum(control))
  and {
    source_span.length == 0,
    source_span.absolute_start == 45,
    split_empty_span_step(control, action, #"182e") != None,
    split_empty_span_step(control, action, #"182d") != None,
  }
}

/// The split datum step refuses an empty span starting past the frame's
/// output length.
test split_empty_span_refuses_a_start_past_the_output_length() fail {
  let (control, action) = first_empty_span_step(
    list_empty_output,
    list_empty_witnesses,
  )
  split_empty_span_step(control, action, #"182c") != None
}

/// An honest empty span starting exactly at the end of the 44-byte output
/// (the datum `B""` as the last field) is read by the typed and split steps.
test empty_span_starting_at_the_output_end_is_read() {
  let output = #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a0024140"
  let (control, action) = first_empty_span_step(
    output,
    #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87a80d8799f4140ffffd87c9fd87980d8799f4140ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980ff",
  )
  expect Some(source_span) = traverse.next_source_span_v1(active_datum(control))
  let witness = proof.LedgerOutputProofDatum { action, window: None }
  let witness_data: Data = witness
  and {
    source_span.length == 0,
    source_span.absolute_start == bytearray.length(output),
    control.total_length == bytearray.length(output),
    proof.step_v1(control, witness) != None,
    routed(
      raw.open(proof.encode_control_v1(control)),
      witness_data,
      active_datum(control),
      action,
    ) != None,
  }
}

/// The datum `B""` is the last field: its empty span starts at the end of the output.
test empty_byte_string_datum_ending_the_output_replays_the_typescript_trace() {
  replays(
    #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a0024140",
    #"40",
    #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87a80d8799f4140ffffd87c9fd87980d8799f4140ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980ff",
    #"8ec7e69b3ccc24ae8e1c0f7609cd07a0c6188f4cafcc9abbb2e7b4f34cec5586",
    #"9b9e60d6da19d5dc8cca51cb2aa0512b5e019cb3789087c458b243faa157a37f",
    1,
  )
}

/// The datum `[B""]`.
test list_holding_an_empty_byte_string_replays_the_typescript_trace() {
  replays(
    #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a002439f40ff",
    #"9f40ff",
    #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87b80d8799f439f40ffffffd87c9fd87a80d8799f439f40ffffffd87c9fd87980d8799f439f40ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000008000d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87980d8799f439f40ffffffd87c9fd87f9fd8799f02004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffff00d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff80ffd87a80ffd87c9fd905019fd8799f02004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff01d8799f582016925ec20d2cfa48b7d727ce43aabab6f695dcc94463febd74e2c67631181800010105ffffd87a80ffd87a80ffd87980ff",
    #"a3adbe1daaefc32482253e35f9a2f2cf1344525cc7af3cc37e9a2fdd0f40be3b",
    #"9f22dd78891a1e99a7eb9d65632a90db25ff6fc0d47d15691de984cd8baf311f",
    1,
  )
}

/// The datum `[B"", 1]`.
test list_starting_with_an_empty_byte_string_replays_the_typescript_trace() {
  replays(
    #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a002449f4001ff",
    #"9f4001ff",
    #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87b80d8799f449f4001ffffffd87c9fd87a80d8799f449f4001ffffffd87c9fd87980d8799f449f4001ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000008000d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87a80d8799f449f4001ffffffd87c9fd87980d8799f449f4001ffffffd87c9fd87980d8799f449f4001ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87980d8799f449f4001ffffffd87c9fd87f9fd8799f02004000004000029fd8799f015820e644b8b04fb1f16375b92127ea5b8cf59098fa2f7141805715d887aa8b2bbb15ffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffff01d8799f58206102804ee42d85218a31c7dda323bccc1ad4b31a9f3f3880449bdfca06a307dc0105ff9f58204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffffd87a80ffd87c9fd87f9fd8799f02004000004000029fd8799f015820e644b8b04fb1f16375b92127ea5b8cf59098fa2f7141805715d887aa8b2bbb15ffff01d8799f5820c66d34dc169dc55d9f05404a8d8b1e711f9e1e7fa38accdb2c01225262c4e763010105ffff00d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff9f58206a4eb2a14e5295bd2a48287c98e33b7948b9536ad5b9daef8a542fa898c49322ffffd87a80ffd87c9fd905019fd8799f02004000004000029fd8799f015820e644b8b04fb1f16375b92127ea5b8cf59098fa2f7141805715d887aa8b2bbb15ffff02d8799f58208fe7d531abef51c62880cb6f54137e5007a6743df67ba733ebee0a4bb9d7f8e002020affffd87a80ffd87a80ffd87980ff",
    #"1d7d0837c2643977fb81c14fe6236d2e320d256f473bf2f39d120d130f28950b",
    #"ea04b9559d06e85d313d0d065117cc6aacbbbd98e070359df8e9fbb9f31c8f61",
    1,
  )
}

/// The datum `Constr 0 [B""]`.
test constructor_holding_an_empty_byte_string_replays_the_typescript_trace() {
  replays(
    #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a00245d8799f40ff",
    #"d8799f40ff",
    #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87b80d8799f45d8799f40ffffffd87c9fd87a80d8799f45d8799f40ffffffd87c9fd87980d8799f45d8799f40ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f00004000004000008000d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87980d8799f45d8799f40ffffffd87c9fd87f9fd8799f00004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffff00d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff80ffd87a80ffd87c9fd905019fd8799f00004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff01d8799f582016925ec20d2cfa48b7d727ce43aabab6f695dcc94463febd74e2c67631181800010105ffffd87a80ffd87a80ffd87980ff",
    #"65c0e56cf73db6cc5c7c0f64aefc3588b29874c6a74558f4c4aff986cad76a4c",
    #"77b28cc72668a185fa2277aebdae336a00f90b239891af697c6eec7b51c25f33",
    1,
  )
}

/// The datum `Constr 0 [B"", 1]`.
test constructor_starting_with_an_empty_byte_string_replays_the_typescript_trace() {
  replays(
    #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a00246d8799f4001ff",
    #"d8799f4001ff",
    #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87b80d8799f46d8799f4001ffffffd87c9fd87a80d8799f46d8799f4001ffffffd87c9fd87980d8799f46d8799f4001ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f00004000004000008000d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87a80d8799f46d8799f4001ffffffd87c9fd87980d8799f46d8799f4001ffffffd87c9fd87980d8799f46d8799f4001ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f00004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87980d8799f46d8799f4001ffffffd87c9fd87f9fd8799f00004000004000029fd8799f015820e644b8b04fb1f16375b92127ea5b8cf59098fa2f7141805715d887aa8b2bbb15ffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffff01d8799f58206102804ee42d85218a31c7dda323bccc1ad4b31a9f3f3880449bdfca06a307dc0105ff9f58204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffffd87a80ffd87c9fd87f9fd8799f00004000004000029fd8799f015820e644b8b04fb1f16375b92127ea5b8cf59098fa2f7141805715d887aa8b2bbb15ffff01d8799f5820c66d34dc169dc55d9f05404a8d8b1e711f9e1e7fa38accdb2c01225262c4e763010105ffff00d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff9f58206a4eb2a14e5295bd2a48287c98e33b7948b9536ad5b9daef8a542fa898c49322ffffd87a80ffd87c9fd905019fd8799f00004000004000029fd8799f015820e644b8b04fb1f16375b92127ea5b8cf59098fa2f7141805715d887aa8b2bbb15ffff02d8799f58208fe7d531abef51c62880cb6f54137e5007a6743df67ba733ebee0a4bb9d7f8e002020affffd87a80ffd87a80ffd87980ff",
    #"bea77d37825b28cd24c71cf13bfbbf164b61328b14ce7c6da6e584bbadfb6e5f",
    #"9e865907b8a644d4520b6b6b66c15ba8c6cabce3dc8d5d1f6037aa521fe14fff",
    1,
  )
}

/// The datum `{B"": 0}`.
test map_with_an_empty_byte_string_key_replays_the_typescript_trace() {
  replays(
    #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a00243a14000",
    #"a14000",
    #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87c80d8799f43a14000ffffd87c9fd87a80d8799f43a14000ffffd87c9fd87980d8799f43a14000ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f03004000004002008000d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ffffffffd87a80ffd87c9fd87a80d8799f43a14000ffffd87c9fd87980d8799f43a14000ffffd87c9fd87980d8799f43a14000ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f03004000004002019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ffffffffd87a80ffd87c9fd905009fd8799f03004000004002029fd8799f01582069ba927ffb89db72ddfe802258c4a445bc04942bdf483ec0f963c675cdc51ce5ffff00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ffff00d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ffd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105ff9f5820ebfd10c4a2e729a2ded716392352fe681c9ebb85b93dec6698a7ebb223f9ea16ff9f58204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffffd87a80ffd87c9fd905019fd8799f03004000004002029fd8799f01582069ba927ffb89db72ddfe802258c4a445bc04942bdf483ec0f963c675cdc51ce5ffff01d8799f5820a43ad9958682ac4eb2811a3450133ba0addcde6e3107b4108824138c5660c19701020affffd87a80ffd87a80ffd87980ff",
    #"73a578ec81a5f213c7ae7eab2a93cf14ce44e99af083ab08730c806df5c853f3",
    #"dd557e32210bd0ad747fc4bcea3559c41baac486c98a2fddb3de57cfcf4afa58",
    1,
  )
}

/// The datum `{0: B""}`.
test map_with_an_empty_byte_string_value_replays_the_typescript_trace() {
  replays(
    #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a00243a10040",
    #"a10040",
    #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87c80d8799f43a10040ffffd87c9fd87a80d8799f43a10040ffffd87c9fd87980d8799f43a10040ffffd87c9fd87980d8799f43a10040ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f03004000004002008000d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ffffffffd87a80ffd87c9fd87a80d8799f43a10040ffffd87c9fd87980d8799f43a10040ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f03004000004002019fd8799f0058206a9194f205ef503230d073cded1b21eb8891ba6df37cb446dda7f703a00dd05bffff00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ffffffffd87a80ffd87c9fd905009fd8799f03004000004002029fd8799f015820a52bfa3178ba0b906d297cbbdcbce064369abb67dccc9a5fd94a98e40d89c984ffff00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ffff00d8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105ffd8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff9f582084a23d5356d95ebc0e196f044589d386f31e492462db23c29f883c49f37981beff9f58206a9194f205ef503230d073cded1b21eb8891ba6df37cb446dda7f703a00dd05bffffd87a80ffd87c9fd905019fd8799f03004000004002029fd8799f015820a52bfa3178ba0b906d297cbbdcbce064369abb67dccc9a5fd94a98e40d89c984ffff01d8799f582015eb547d20c86032bf253ac722b17b26d76fa4a23f499fddc96f4753eab636e001020affffd87a80ffd87a80ffd87980ff",
    #"4215dc224cea39d9395b06479b3274ee6c0c272714cede23dbe0267099e63792",
    #"4be44e4ab7c360d596fad6fda318ad08a40eac85d13011e034f9d8d81c43f373",
    1,
  )
}

/// The datum `B""`, then a ten-byte PlutusV3 reference script.
test empty_byte_string_datum_before_a_reference_script_replays_the_typescript_trace() {
  replays(
    #"a400581d781111111111111111111111111111111111111111111111111111111101821a001e8480a00241400382034a6b6b6b6b6b6b6b6b6b6b",
    #"40",
    #"9fd87a80d87a80d87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87a80d8799f4f400382034a6b6b6b6b6b6b6b6b6b6bffffd87c9fd87980d8799f4f400382034a6b6b6b6b6b6b6b6b6b6bffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980d87f9f4f400382034a6b6b6b6b6b6b6b6b6b6bffd87980d87f9f4f400382034a6b6b6b6b6b6b6b6b6b6bffd87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980ff",
    #"02d9097578a569235cc9cf6d253eb558234a8e3308fc557fcc47d0365b9b93fb",
    #"a233872d165bdefe6a4f5d0b32671ec758e4378900fa614101540c84c0867e07",
    1,
  )
}

/// A token with an empty asset name, and the datum `B""`.
test empty_asset_name_and_empty_byte_string_datum_replays_the_typescript_trace() {
  replays(
    #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a1581c55555555555555555555555555555555555555555555555555555555a14001024140",
    #"40",
    #"9fd87a80d87a80d87a80d87a80d87a80d87a80d87980d87980d87b9f00581c55555555555555555555555555555555555555555555555555555555400180d87a80ffd87980d87980d87980d87e80d87c9fd87a80d8799f4140ffffd87c9fd87980d8799f4140ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980ff",
    #"38d26f3e0c278baf8ac930fc85dcc02aec5ab4022dd4f3fb385990cc9333d8a4",
    #"b41d1323a9fc4c1775d9c3ac400fb4af1615e339bb415bcf0642ed458771588d",
    1,
  )
}

/// The datum `0`, then an empty PlutusV3 reference script.
test empty_reference_script_replays_the_typescript_trace() {
  replays(
    #"a400581d781111111111111111111111111111111111111111111111111111111101821a001e8480a002410003820340",
    #"00",
    #"9fd87a80d87a80d87a80d87a80d87a80d87980d87980d87980d87980d87e80d87c9fd87a80d8799f450003820340ffffd87c9fd87980d8799f450003820340ffffd87c9fd87980d8799f450003820340ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980d87f9f450003820340ffd87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980ff",
    #"f94cbc996375d18844219e49b747bcdf794147d536810931e121bda50f9c3c40",
    #"bb248dd4fad390dc849958ddbd2ca8503274b3f4356f6857f17fc09d4376df36",
    0,
  )
}

/// The datum `B"01"`: no empty span.
test one_byte_string_datum_replays_the_typescript_trace() {
  replays(
    #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a002424101",
    #"4101",
    #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87a80d8799f424101ffffd87c9fd87980d8799f424101ffffd87c9fd87980d8799f424101ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980ff",
    #"be1d3b4ae788dd5a68b99b4435b7ff461a5e729687b3a0dd6a10c809a96d5a9c",
    #"3520b30ab186c1614adc9456d20e57251a3cc24f73f1d10665f37f56f0fa5a6a",
    0,
  )
}
