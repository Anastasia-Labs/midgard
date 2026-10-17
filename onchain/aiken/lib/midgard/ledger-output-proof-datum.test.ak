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
use aiken/collection/list
use aiken/crypto
use aiken/primitive/bytearray
use midgard/blake2b_256_trace_v1 as blake256
use midgard/bounded_item_v1
use midgard/cbor_deserialise
use midgard/cek_blob_frontier_v1 as blob_frontier
use midgard/cek_data_bytes_v1 as bytes_control
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

/// The scalar attestation of the scalar stage `stage`'s family.
fn scalar_attestation(stage: Int) -> fn(raw.Frame, Data) -> Bool {
  if stage == traverse.stage_integer {
    narrow_datum.integer_scalar_claim_is_exact
  } else {
    expect stage == traverse.stage_bytes
    narrow_datum.bytes_scalar_claim_is_exact
  }
}

/// The scalar claim of the attestation stage: built from the split
/// frame-form read of the datum sub-control (not from the typed control),
/// and accepted by the scalar attestation of the control's family.
fn attested_scalar_claim(control_frame: raw.Frame) -> Data {
  let frame_control = narrow_datum.frame_datum_control(control_frame)
  let claim = honest_scalar_claim(frame_control)
  expect scalar_attestation(frame_control.stage)(control_frame, claim)
  claim
}

/// Route one datum action to the narrow stage function of its role, exactly
/// the way the step-plan role table does, with `claim` as the scalar claim of
/// the scalar attach and advance roles.
fn routed_with_claim(
  control_frame: raw.Frame,
  witness: Data,
  datum_control: traverse.DataTraverseControlV1,
  action: traverse.DataTraverseActionV1,
  claim: Data,
) -> Option<narrow.StepResult> {
  when action is {
    traverse.HeadScalar -> narrow_datum.head_scalar(control_frame, witness)
    traverse.HeadSequence -> narrow_datum.head_sequence(control_frame, witness)
    traverse.HeadMap { .. } -> narrow_datum.head_map(control_frame, witness)
    traverse.HeadLargeConstructor ->
      narrow_datum.head_large_constructor(control_frame, witness)
    traverse.AttachScalar { .. } ->
      if datum_control.stage == traverse.stage_integer {
        narrow_datum.attach_integer(control_frame, witness, claim)
      } else {
        narrow_datum.attach_bytes(control_frame, witness, claim)
      }
    traverse.NoAction ->
      if datum_control.stage == traverse.stage_integer {
        narrow_datum.advance_integer(control_frame, witness, claim)
      } else if datum_control.stage == traverse.stage_bytes {
        narrow_datum.advance_bytes(control_frame, witness, claim)
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

/// `routed_with_claim` with the claim of the attestation stage at the scalar
/// stages (see `attested_scalar_claim`).
fn routed(
  control_frame: raw.Frame,
  witness: Data,
  datum_control: traverse.DataTraverseControlV1,
  action: traverse.DataTraverseActionV1,
) -> Option<narrow.StepResult> {
  let scalar_role = when action is {
    traverse.AttachScalar { .. } -> True
    traverse.NoAction ->
      datum_control.stage == traverse.stage_integer || datum_control.stage == traverse.stage_bytes
    _ -> False
  }
  let claim = if scalar_role {
    attested_scalar_claim(control_frame)
  } else {
    builtin.constr_data(1, [])
  }
  routed_with_claim(control_frame, witness, datum_control, action, claim)
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
      summary.root == #"ce1eaec75e8bab1cd95372432d54684932757241ca27b0c31884904240283ee3"
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
    summary.memory == 13,
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

// Honest outputs whose proofs the TypeScript trace builder built; the rows
// below replay each proof step by step through the typed step and the split
// stage functions. demo/midgard-validation/tests/ledger-output-proof-empty-span.test.ts
// and ledger-output-proof-split-span.test.ts pin the same witness-list and
// terminal hashes. A chunk-proof witness is written as its constructor (1
// for chunks, 5 for a span attach), either bare (the output's single chunk)
// or over its chunk indices `[first, next or -1]`, and is rebuilt here from
// the output's chunk proofs. A datum witness whose window is the recorded
// window is written as constructor 9 over its action alone.

/// The output's chunk proof at `index`.
fn chunk_at(
  proofs: List<bounded_item_v1.ChunkProofV1>,
  index: Int,
) -> bounded_item_v1.ChunkProofV1 {
  expect Some(chunk_proof) = list.at(proofs, index)
  chunk_proof
}

/// The chunk proofs a chunk-proof witness placeholder names.
fn placeholder_chunks(
  fields: List<Data>,
  proofs: List<bounded_item_v1.ChunkProofV1>,
) -> (bounded_item_v1.ChunkProofV1, Option<bounded_item_v1.ChunkProofV1>) {
  when fields is {
    [] -> (chunk_at(proofs, 0), None)
    [first, next] -> {
      let next_index = builtin.un_i_data(next)
      (
        chunk_at(proofs, builtin.un_i_data(first)),
        if next_index == -1 {
          None
        } else {
          Some(chunk_at(proofs, next_index))
        },
      )
    }
    _ -> fail
  }
}

fn replay_witness(
  control: proof.LedgerOutputProofControlV1,
  data: Data,
  proofs: List<bounded_item_v1.ChunkProofV1>,
  output: ByteArray,
) -> proof.LedgerOutputProofWitnessV1 {
  let Pair(tag, fields) = builtin.un_constr_data(data)
  if tag == 1 {
    let (chunk_proof, next_chunk_proof) = placeholder_chunks(fields, proofs)
    proof.LedgerOutputProofChunks { chunk_proof, next_chunk_proof }
  } else if tag == 5 {
    let (chunk_proof, next_chunk_proof) = placeholder_chunks(fields, proofs)
    proof.LedgerOutputProofSpanAttach { chunk_proof, next_chunk_proof }
  } else if tag == 9 {
    expect [action_data] = fields
    expect action: traverse.DataTraverseActionV1 = action_data
    proof.LedgerOutputProofDatum {
      action,
      window: Some(recorded_window(control, output)),
    }
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
    span.attach(raw.open(proof.encode_control_v1(control)), attach) == None,
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

/// At the datum-traversal stage, the split frame-form read of the datum
/// sub-control is the typed datum sub-control (its result slot unread), and
/// the split span selection over the frame agrees with the typed one.
fn same_source_span(
  control: proof.LedgerOutputProofControlV1,
  control_frame: raw.Frame,
) -> Bool {
  if control.stage == proof.stage_datum_traversal {
    expect
      narrow_datum.frame_datum_control(control_frame) == traverse.DataTraverseControlV1 {
        ..active_datum(control),
        result: None,
      }
    narrow_datum.next_source_span(control_frame) == when
      traverse.next_source_span_v1(active_datum(control))
    is {
      None -> None
      Some(source_span) ->
        Some((source_span.absolute_start, source_span.length))
    }
  } else {
    True
  }
}

/// The active scalar's blob sub-control, if any.
fn active_blob(
  datum_control: traverse.DataTraverseControlV1,
) -> Option<source_blob.CekSourceBlobControlV1> {
  when (datum_control.integer, datum_control.bytes) is {
    (Some(integer), _) -> integer.blob
    (_, Some(bytes)) -> bytes.blob
    _ -> None
  }
}

/// A span attach at the datum-traversal stage, as
/// `100 * traverse stage + 10 * (blob present) + (blob hash present)`;
/// `-1` for any other step.
fn attach_shape(
  control: proof.LedgerOutputProofControlV1,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Int {
  when witness is {
    proof.LedgerOutputProofSpanAttach { .. } ->
      if control.stage == proof.stage_datum_traversal {
        let datum_control = active_datum(control)
        100 * datum_control.stage + when active_blob(datum_control) is {
          None -> 0
          Some(blob) ->
            when blob.active_hash is {
              None -> 10
              Some(_) -> 11
            }
        }
      } else {
        -1
      }
    _ -> -1
  }
}

/// At a scalar datum step, the honest scalar claim is attested against the
/// frame.
fn scalar_claim_attests(
  datum_control: traverse.DataTraverseControlV1,
  control_frame: raw.Frame,
) -> Bool {
  if datum_control.stage == traverse.stage_integer || datum_control.stage == traverse.stage_bytes {
    scalar_attestation(datum_control.stage)(
      control_frame,
      honest_scalar_claim(datum_control),
    )
  } else {
    True
  }
}

/// The step planner's scalar claim for step `index`, if `claims` (its
/// `[step index, claim]` pairs, in step order) has one, and the claims of the
/// later steps.
fn planner_claim(claims: List<Data>, index: Int) -> (Option<Data>, List<Data>) {
  when claims is {
    [] -> (None, [])
    [pair, ..rest] -> {
      expect [claim_index, claim] = builtin.un_list_data(pair)
      if builtin.un_i_data(claim_index) == index {
        (Some(claim), rest)
      } else {
        (None, claims)
      }
    }
  }
}

/// One replayed step. A span attach and a datum step also run through their
/// split stage function, to the same successor; at the datum-traversal
/// stage the split and typed span selections agree. A datum step with a
/// step planner's scalar claim takes it: the scalar attestation accepts it
/// and the split stage yield reaches the same successor under it. Returns
/// the successor and whether the step read an empty span.
fn replay_step(
  control: proof.LedgerOutputProofControlV1,
  witness: proof.LedgerOutputProofWitnessV1,
  proofs: List<bounded_item_v1.ChunkProofV1>,
  output: ByteArray,
  claim: Option<Data>,
) -> (proof.LedgerOutputProofControlV1, Int) {
  let next = advanced(proof.step_v1(control, witness))
  let control_frame = raw.open(proof.encode_control_v1(control))
  expect same_source_span(control, control_frame)
  when witness is {
    proof.LedgerOutputProofSpanAttach { .. } -> {
      expect claim == None
      expect Some(narrow.Advanced { control: raw_next }) = span.attach(
        control_frame,
        witness,
      )
      expect raw.encode(raw_next) == proof.encode_control_v1(next)
      (next, 0)
    }
    proof.LedgerOutputProofDatum { action, window } -> {
      let witness_data: Data = witness
      let datum_control = active_datum(control)
      let result = when claim is {
        None -> routed(control_frame, witness_data, datum_control, action)
        Some(planned) -> {
          expect scalar_attestation(datum_control.stage)(control_frame, planned)
          routed_with_claim(
            control_frame,
            witness_data,
            datum_control,
            action,
            planned,
          )
        }
      }
      expect Some(narrow.Advanced { control: raw_next }) = result
      expect raw.encode(raw_next) == proof.encode_control_v1(next)
      expect scalar_claim_attests(datum_control, control_frame)
      if reads_empty_span(control) {
        expect window == None
        expect empty_span_refusals(control, action, chunk_at(proofs, 0), output)
        (next, 1)
      } else {
        (next, 0)
      }
    }
    _ -> {
      expect claim == None
      (next, 0)
    }
  }
}

/// Replay `witnesses` from `control`, the first being step `index`, taking
/// the step planner's scalar `claims` at their steps: the terminal control,
/// the number of steps that read an empty span, and the shape of each
/// datum-stage span attach, in order. Every claim is taken.
fn replay(
  control: proof.LedgerOutputProofControlV1,
  witnesses: List<Data>,
  proofs: List<bounded_item_v1.ChunkProofV1>,
  output: ByteArray,
  empty_steps: Int,
  index: Int,
  claims: List<Data>,
) -> (proof.LedgerOutputProofControlV1, Int, List<Int>) {
  when witnesses is {
    [] -> {
      expect claims == []
      (control, empty_steps, [])
    }
    [data, ..rest] -> {
      let witness = replay_witness(control, data, proofs, output)
      let shape = attach_shape(control, witness)
      let (claim, later_claims) = planner_claim(claims, index)
      let (next, empty) = replay_step(control, witness, proofs, output, claim)
      let (terminal, total_empty, shapes) = replay(
        next,
        rest,
        proofs,
        output,
        empty_steps + empty,
        index + 1,
        later_claims,
      )
      (
        terminal,
        total_empty,
        if shape == -1 {
          shapes
        } else {
          [shape, ..shapes]
        },
      )
    }
  }
}

/// The commitment and the chunk proofs, in chunk order, of the one- or
/// two-chunk field 2 item `output` at index 0.
fn output_material(
  output: ByteArray,
) -> Pair<ByteArray, List<bounded_item_v1.ChunkProofV1>> {
  let total_length = bytearray.length(output)
  if bounded_item_v1.chunk_count(total_length) == 1 {
    let Pair(commitment, chunk_proof) = one_chunk_material(0, output)
    Pair(commitment, [chunk_proof])
  } else {
    expect bounded_item_v1.chunk_count(total_length) == 2
    let first = slice_len(output, 0, bounded_item_v1.chunk_bytes)
    let second = slice_len(
      output,
      bounded_item_v1.chunk_bytes,
      total_length - bounded_item_v1.chunk_bytes,
    )
    let first_leaf = bounded_item_v1.hash_chunk(2, 0, 0, first)
    let second_leaf = bounded_item_v1.hash_chunk(2, 0, 1, second)
    let frontier = merkle.build_frontier([first_leaf, second_leaf]).peaks
    let chunk_proof = fn(index, chunk, siblings) {
      bounded_item_v1.ChunkProofV1 {
        version: 1,
        field_index: 2,
        item_index: 0,
        total_length,
        chunk_index: index,
        chunk,
        frontier,
        siblings,
      }
    }
    Pair(
      bounded_item_v1.commitment(2, 0, total_length, frontier),
      [
        chunk_proof(0, first, [second_leaf]),
        chunk_proof(1, second, [first_leaf]),
      ],
    )
  }
}

/// Replay the TypeScript-built proof `witnesses` of `output` from its
/// initial control, taking the step planner's scalar claims
/// `planner_claims`.
fn replay_terminal(
  output: ByteArray,
  witnesses: ByteArray,
  planner_claims: ByteArray,
) -> (proof.LedgerOutputProofControlV1, Int, List<Int>) {
  let Pair(commitment, proofs) = output_material(output)
  let initial = proof.initial_control_v1(
    0,
    bytearray.length(output),
    commitment,
  )
  let witness_list <- cbor_deserialise.expect_deserialise(witnesses)
  let claims <- cbor_deserialise.expect_deserialise(planner_claims)
  replay(
    initial,
    builtin.un_list_data(witness_list),
    proofs,
    output,
    0,
    0,
    builtin.un_list_data(claims),
  )
}

/// The TypeScript-built proof of `output` replays to an exact terminal whose
/// control hash is `terminal_hash` and whose spend datum is `datum`, taking
/// the step planner's scalar claims `planner_claims` and reading
/// `empty_steps` empty spans on the way, with datum-stage span attaches (see
/// `attach_shape`) that satisfy `attaches`.
fn replays_where(
  output: ByteArray,
  datum: Data,
  witnesses: ByteArray,
  terminal_hash: ByteArray,
  planner_claims: ByteArray,
  empty_steps: Int,
  attaches: fn(List<Int>) -> Bool,
) -> Bool {
  let (terminal, empty, shapes) = replay_terminal(
    output,
    witnesses,
    planner_claims,
  )
  and {
    empty == empty_steps,
    attaches(shapes),
    proof.terminal_is_exact_v1(terminal),
    crypto.blake2b_256(proof.encode_control_v1(terminal)) == terminal_hash,
    proof.cardano_spend_datum_summary_v1(terminal) == Some(
      cek_data_v1.semantic_data_summary_v1(builtin.constr_data(0, [datum])),
    ),
  }
}

/// `replays_where` with the datum given by its CBOR, for any datum-stage
/// span attaches.
fn replays(
  output: ByteArray,
  datum: ByteArray,
  witnesses: ByteArray,
  terminal_hash: ByteArray,
  planner_claims: ByteArray,
  empty_steps: Int,
) -> Bool {
  // The decoder refuses an empty byte string at the very end of its input
  // (see cbor-deserialise.ak), so the datum is read as the head of a list.
  let datum_list <- cbor_deserialise.expect_deserialise(
    bytearray.concat(#"82", bytearray.concat(datum, #"00")),
  )
  expect [datum_data, _] = builtin.un_list_data(datum_list)
  replays_where(
    output,
    datum_data,
    witnesses,
    terminal_hash,
    planner_claims,
    empty_steps,
    fn(_) { True },
  )
}

// The scalar roles under substituted claims. At every integer or bytes
// attach or advance step of a TypeScript-built proof, the routed stage yield
// under the attested claim reaches the typed successor (see `routed`), and
// each claim that differs from it in one field is refused by the scalar
// attestation.

/// Each datum step of `output`'s TypeScript-built proof at the scalar stage
/// `stage`, attaching (`attach`) or advancing: its control and witness.
fn scalar_role_steps(
  output: ByteArray,
  witnesses: ByteArray,
  stage: Int,
  attach: Bool,
) -> List<(proof.LedgerOutputProofControlV1, proof.LedgerOutputProofWitnessV1)> {
  let Pair(commitment, proofs) = output_material(output)
  let initial = proof.initial_control_v1(
    0,
    bytearray.length(output),
    commitment,
  )
  let witness_list <- cbor_deserialise.expect_deserialise(witnesses)
  scalar_role_steps_from(
    initial,
    builtin.un_list_data(witness_list),
    proofs,
    output,
    stage,
    attach,
  )
}

fn scalar_role_steps_from(
  control: proof.LedgerOutputProofControlV1,
  witnesses: List<Data>,
  proofs: List<bounded_item_v1.ChunkProofV1>,
  output: ByteArray,
  stage: Int,
  attach: Bool,
) -> List<(proof.LedgerOutputProofControlV1, proof.LedgerOutputProofWitnessV1)> {
  when witnesses is {
    [] -> []
    [data, ..rest] -> {
      let witness = replay_witness(control, data, proofs, output)
      let later = scalar_role_steps_from(
        advanced(proof.step_v1(control, witness)),
        rest,
        proofs,
        output,
        stage,
        attach,
      )
      when witness is {
        proof.LedgerOutputProofDatum { action, .. } -> {
          let attaches = when action is {
            traverse.AttachScalar { .. } -> True
            _ -> False
          }
          if active_datum(control).stage == stage && attaches == attach {
            [(control, witness), ..later]
          } else {
            later
          }
        }
        _ -> later
      }
    }
  }
}

/// `datum_control` with its active scalar sub-control's extent shifted.
fn with_scalar_extent(
  datum_control: traverse.DataTraverseControlV1,
  start_shift: Int,
  length_shift: Int,
) -> traverse.DataTraverseControlV1 {
  when (datum_control.integer, datum_control.bytes) is {
    (Some(integer), _) ->
      traverse.DataTraverseControlV1 {
        ..datum_control,
        integer: Some(
          integer_control.CekDataIntegerControlV1 {
            ..integer,
            source_start: integer.source_start + start_shift,
            source_length: integer.source_length + length_shift,
          },
        ),
      }
    (_, Some(bytes)) ->
      traverse.DataTraverseControlV1 {
        ..datum_control,
        bytes: Some(
          bytes_control.CekDataBytesControlV1 {
            ..bytes,
            source_start: bytes.source_start + start_shift,
            source_length: bytes.source_length + length_shift,
          },
        ),
      }
    _ -> fail
  }
}

/// The claims that differ from the attested claim at `datum_control` in one
/// field: each prefix field, and the scalar sub-control's start or length.
fn substituted_claims(
  datum_control: traverse.DataTraverseControlV1,
) -> List<Data> {
  [
    traverse.DataTraverseControlV1 {
      ..datum_control,
      source_start: datum_control.source_start + 1,
    },
    traverse.DataTraverseControlV1 {
      ..datum_control,
      source_length: datum_control.source_length + 1,
    },
    traverse.DataTraverseControlV1 {
      ..datum_control,
      offset: datum_control.offset + 1,
    },
    traverse.DataTraverseControlV1 {
      ..datum_control,
      frame_root: if datum_control.frame_root == #"" {
        crypto.blake2b_256(#"")
      } else {
        #""
      },
    },
    with_scalar_extent(datum_control, 1, 0),
    with_scalar_extent(datum_control, 0, 1),
  ]
    |> list.map(honest_scalar_claim)
}

/// Whether `result` is the typed successor `next`.
fn reaches(
  result: Option<narrow.StepResult>,
  next: proof.LedgerOutputProofControlV1,
) -> Bool {
  when result is {
    Some(narrow.Advanced { control }) ->
      raw.encode(control) == proof.encode_control_v1(next)
    _ -> False
  }
}

/// At each `stage` scalar step of `output`'s proof, attaching (`attach`) or
/// advancing: the claim of the attestation stage is the typed control's, and
/// the routed stage yield under it reaches the typed successor; the scalar
/// attestation refuses every substituted claim; and under each substituted
/// claim at an index in `consumed` the routed stage yield does not reach the
/// typed successor.
fn scalar_claim_is_bound(
  output: ByteArray,
  witnesses: ByteArray,
  stage: Int,
  attach: Bool,
  consumed: List<Int>,
) -> Bool {
  let steps = scalar_role_steps(output, witnesses, stage, attach)
  let attests = scalar_attestation(stage)
  and {
    steps != [],
    list.all(
      steps,
      fn(step) {
        let (control, witness) = step
        expect proof.LedgerOutputProofDatum { action, .. } = witness
        let next = advanced(proof.step_v1(control, witness))
        let control_frame = raw.open(proof.encode_control_v1(control))
        let datum_control = active_datum(control)
        let witness_data: Data = witness
        let yields = fn(claim) {
          reaches(
            routed_with_claim(
              control_frame,
              witness_data,
              datum_control,
              action,
              claim,
            ),
            next,
          )
        }
        let claim = attested_scalar_claim(control_frame)
        let substituted = substituted_claims(datum_control)
        and {
          claim == honest_scalar_claim(datum_control),
          yields(claim),
          list.all(substituted, fn(other) { !attests(control_frame, other) }),
          list.all(
            consumed,
            fn(index) {
              expect Some(other) = list.at(substituted, index)
              !yields(other)
            },
          ),
        }
      },
    ),
  }
}

/// The integer attach step of the `[B"", 1]` proof consumes every field of
/// the claim.
test integer_attach_takes_the_attested_scalar_claim() {
  scalar_claim_is_bound(
    list_starting_with_empty_bytes_output,
    list_starting_with_empty_bytes_witnesses,
    traverse.stage_integer,
    True,
    [0, 1, 2, 3, 4, 5],
  )
}

/// The integer advance steps of the `[B"", 1]` proof consume the claimed
/// scalar sub-control.
test integer_advance_takes_the_attested_scalar_claim() {
  scalar_claim_is_bound(
    list_starting_with_empty_bytes_output,
    list_starting_with_empty_bytes_witnesses,
    traverse.stage_integer,
    False,
    [4, 5],
  )
}

/// The bytes attach steps of the `[B"", 1]` and `B"01"` proofs consume the
/// claim's prefix and the claimed scalar's length.
test bytes_attach_takes_the_attested_scalar_claim() {
  and {
    scalar_claim_is_bound(
      list_starting_with_empty_bytes_output,
      list_starting_with_empty_bytes_witnesses,
      traverse.stage_bytes,
      True,
      [0, 1, 2, 3, 5],
    ),
    scalar_claim_is_bound(
      one_byte_string_output,
      one_byte_string_witnesses,
      traverse.stage_bytes,
      True,
      [0, 1, 2, 3, 5],
    ),
  }
}

/// The bytes advance steps of the `[B"", 1]` and `B"01"` proofs, the empty
/// span's included, consume the claimed scalar's length.
test bytes_advance_takes_the_attested_scalar_claim() {
  and {
    scalar_claim_is_bound(
      list_starting_with_empty_bytes_output,
      list_starting_with_empty_bytes_witnesses,
      traverse.stage_bytes,
      False,
      [5],
    ),
    scalar_claim_is_bound(
      one_byte_string_output,
      one_byte_string_witnesses,
      traverse.stage_bytes,
      False,
      [5],
    ),
  }
}

/// The control of the first datum step that reads an empty span, and that
/// step's action.
fn at_empty_span(
  control: proof.LedgerOutputProofControlV1,
  witnesses: List<Data>,
  proofs: List<bounded_item_v1.ChunkProofV1>,
  output: ByteArray,
) -> (proof.LedgerOutputProofControlV1, traverse.DataTraverseActionV1) {
  expect [data, ..rest] = witnesses
  let witness = replay_witness(control, data, proofs, output)
  when witness is {
    proof.LedgerOutputProofDatum { action, .. } ->
      if reads_empty_span(control) {
        (control, action)
      } else {
        at_empty_span(
          advanced(proof.step_v1(control, witness)),
          rest,
          proofs,
          output,
        )
      }
    _ ->
      at_empty_span(
        advanced(proof.step_v1(control, witness)),
        rest,
        proofs,
        output,
      )
  }
}

/// The first datum step of a one-chunk proof that reads an empty span: its
/// control and action.
fn first_empty_span_step(
  output: ByteArray,
  witnesses: ByteArray,
) -> (proof.LedgerOutputProofControlV1, traverse.DataTraverseActionV1) {
  let Pair(commitment, proofs) = output_material(output)
  let initial = proof.initial_control_v1(
    0,
    bytearray.length(output),
    commitment,
  )
  let witness_list <- cbor_deserialise.expect_deserialise(witnesses)
  at_empty_span(initial, builtin.un_list_data(witness_list), proofs, output)
}

/// The split datum step refuses a window over an empty span.
test split_datum_step_refuses_a_window_over_an_empty_span() fail {
  let (control, action) = first_empty_span_step(
    list_holding_empty_bytes_output,
    list_holding_empty_bytes_witnesses,
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

/// The split datum step reads the honest empty span of `[B""]`, which starts
/// at 45, one byte before the end of the 46-byte output.
test split_datum_step_reads_an_empty_span_before_the_output_end() {
  let (control, action) = first_empty_span_step(
    list_holding_empty_bytes_output,
    list_holding_empty_bytes_witnesses,
  )
  expect Some(source_span) = traverse.next_source_span_v1(active_datum(control))
  let witness_data: Data = proof.LedgerOutputProofDatum { action, window: None }
  and {
    source_span.length == 0,
    source_span.absolute_start == 45,
    control.total_length == 46,
    routed(
      raw.open(proof.encode_control_v1(control)),
      witness_data,
      active_datum(control),
      action,
    ) != None,
  }
}

/// An honest empty span starting exactly at the end of the 44-byte output
/// (the datum `B""` as the last field) is read by the typed and split steps.
test empty_span_starting_at_the_output_end_is_read() {
  let output = empty_bytes_last_output
  let (control, action) = first_empty_span_step(
    output,
    empty_bytes_last_witnesses,
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

const empty_bytes_last_output =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a0024140"

const empty_bytes_last_witnesses =
  #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87a80d8799f4140ffffd87c9fd87980d8799f4140ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980ff"

const empty_bytes_last_terminal_hash =
  #"0c81d65a7ed0321f527d82e7930a93af662da3832792dd79469ae0dcb3133de0"

const empty_bytes_last_planner_claims =
  #"9f9f0bd8799f182b010040d8799f0100182b0100d87a80ffffff9f0cd8799f182b010040d8799f0101182b0100d8799fd8799f01000000d8799f000080ffd8799fd8799f01000016584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f181bd8799f182b010040d8799f0101182b0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffff9f181cd8799f182b010040d8799f0103182b0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffffff"

/// The datum `B""` is the last field: its empty span starts at the end of the output.
test empty_byte_string_datum_ending_the_output_replays_the_typescript_trace() {
  replays(
    empty_bytes_last_output,
    #"40",
    empty_bytes_last_witnesses,
    empty_bytes_last_terminal_hash,
    empty_bytes_last_planner_claims,
    1,
  )
}

const list_holding_empty_bytes_output =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a002439f40ff"

const list_holding_empty_bytes_witnesses =
  #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87b80d8799f439f40ffffffd87c9fd87a80d8799f439f40ffffffd87c9fd87980d8799f439f40ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000008000d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87980d8799f439f40ffffffd87c9fd87f9fd8799f02004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffff00d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff80ffd87a80ffd87c9fd905019fd8799f02004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff01d8799f582016925ec20d2cfa48b7d727ce43aabab6f695dcc94463febd74e2c67631181800010105ffffd87a80ffd87a80ffd87980ff"

const list_holding_empty_bytes_terminal_hash =
  #"f4ebf2cd5ab1b2ba1c7db74b8ca69893221818da614f0b9e6f0a7b19e8c3c9b3"

const list_holding_empty_bytes_planner_claims =
  #"9f9f0cd8799f182b030158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0100182c0100d87a80ffffff9f0dd8799f182b030158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0101182c0100d8799fd8799f01000000d8799f000080ffd8799fd8799f01000016584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f181cd8799f182b030158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0101182c0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffff9f181dd8799f182b030158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0103182c0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffffff"

/// The datum `[B""]`.
test list_holding_an_empty_byte_string_replays_the_typescript_trace() {
  replays(
    list_holding_empty_bytes_output,
    #"9f40ff",
    list_holding_empty_bytes_witnesses,
    list_holding_empty_bytes_terminal_hash,
    list_holding_empty_bytes_planner_claims,
    1,
  )
}

const list_starting_with_empty_bytes_output =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a002449f4001ff"

const list_starting_with_empty_bytes_witnesses =
  #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87b80d8799f449f4001ffffffd87c9fd87a80d8799f449f4001ffffffd87c9fd87980d8799f449f4001ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000008000d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87a80d8799f449f4001ffffffd87c9fd87980d8799f449f4001ffffffd87c9fd87980d8799f449f4001ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87980d8799f449f4001ffffffd87c9fd87f9fd8799f02004000004000029fd8799f015820e644b8b04fb1f16375b92127ea5b8cf59098fa2f7141805715d887aa8b2bbb15ffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffff01d8799f58206102804ee42d85218a31c7dda323bccc1ad4b31a9f3f3880449bdfca06a307dc0105ff9f58204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffffd87a80ffd87c9fd87f9fd8799f02004000004000029fd8799f015820e644b8b04fb1f16375b92127ea5b8cf59098fa2f7141805715d887aa8b2bbb15ffff01d8799f5820c66d34dc169dc55d9f05404a8d8b1e711f9e1e7fa38accdb2c01225262c4e763010105ffff00d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff9f58206a4eb2a14e5295bd2a48287c98e33b7948b9536ad5b9daef8a542fa898c49322ffffd87a80ffd87c9fd905019fd8799f02004000004000029fd8799f015820e644b8b04fb1f16375b92127ea5b8cf59098fa2f7141805715d887aa8b2bbb15ffff02d8799f58208fe7d531abef51c62880cb6f54137e5007a6743df67ba733ebee0a4bb9d7f8e002020affffd87a80ffd87a80ffd87980ff"

const list_starting_with_empty_bytes_terminal_hash =
  #"22f4a2fc513c4c12684e59b6f04de1467784dd6d6b99620b4f3010f95228cdb8"

const list_starting_with_empty_bytes_planner_claims =
  #"9f9f0cd8799f182b040158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0100182c0100d87a80ffffff9f0dd8799f182b040158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0101182c0100d8799fd8799f01000000d8799f000080ffd8799fd8799f01000016584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f181cd8799f182b040158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0101182c0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffff9f181dd8799f182b040158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0103182c0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffff9f181fd8799f182b04025820cc0257a7384f72c1a664aa1c1a230b1d48295e5d5549f3908d9e0b712fa88648d8799f0100182d0100d87a80ffffff9f1820d8799f182b04025820cc0257a7384f72c1a664aa1c1a230b1d48295e5d5549f3908d9e0b712fa88648d8799f0101182d0105d8799fd8799f0100182d01d8799f000080ffd8799fd8799f01000017584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f182fd8799f182b04025820cc0257a7384f72c1a664aa1c1a230b1d48295e5d5549f3908d9e0b712fa88648d8799f0101182d0105d8799fd8799f0101182d01d8799f01019fd8799f005820eb5fd47ef72521e3f86fe5eb13a464d7542ed021a5140ff4590b36d8432b002401ffffffd87a80ffffffffff9f1830d8799f182b04025820cc0257a7384f72c1a664aa1c1a230b1d48295e5d5549f3908d9e0b712fa88648d8799f0102182d0105d8799fd8799f0101182d01d8799f01019fd8799f005820eb5fd47ef72521e3f86fe5eb13a464d7542ed021a5140ff4590b36d8432b002401ffffffd87a80ffffffffffff"

/// The datum `[B"", 1]`.
test list_starting_with_an_empty_byte_string_replays_the_typescript_trace() {
  replays(
    list_starting_with_empty_bytes_output,
    #"9f4001ff",
    list_starting_with_empty_bytes_witnesses,
    list_starting_with_empty_bytes_terminal_hash,
    list_starting_with_empty_bytes_planner_claims,
    1,
  )
}

const constr_holding_empty_bytes_output =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a00245d8799f40ff"

const constr_holding_empty_bytes_witnesses =
  #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87b80d8799f45d8799f40ffffffd87c9fd87a80d8799f45d8799f40ffffffd87c9fd87980d8799f45d8799f40ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f00004000004000008000d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87980d8799f45d8799f40ffffffd87c9fd87f9fd8799f00004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffff00d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff80ffd87a80ffd87c9fd905019fd8799f00004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff01d8799f582016925ec20d2cfa48b7d727ce43aabab6f695dcc94463febd74e2c67631181800010105ffffd87a80ffd87a80ffd87980ff"

const constr_holding_empty_bytes_terminal_hash =
  #"6e3441de5c5c1bb195dbedc64d7160b98454508b6d01b9ee0e0d7ac88d790a9c"

const constr_holding_empty_bytes_planner_claims =
  #"9f9f0cd8799f182b05035820cd215fde0af7f804be5726273a2dfacd8088b852f9170b9d02183327d0c5f082d8799f0100182e0100d87a80ffffff9f0dd8799f182b05035820cd215fde0af7f804be5726273a2dfacd8088b852f9170b9d02183327d0c5f082d8799f0101182e0100d8799fd8799f01000000d8799f000080ffd8799fd8799f01000016584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f181cd8799f182b05035820cd215fde0af7f804be5726273a2dfacd8088b852f9170b9d02183327d0c5f082d8799f0101182e0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffff9f181dd8799f182b05035820cd215fde0af7f804be5726273a2dfacd8088b852f9170b9d02183327d0c5f082d8799f0103182e0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffffff"

/// The datum `Constr 0 [B""]`.
test constructor_holding_an_empty_byte_string_replays_the_typescript_trace() {
  replays(
    constr_holding_empty_bytes_output,
    #"d8799f40ff",
    constr_holding_empty_bytes_witnesses,
    constr_holding_empty_bytes_terminal_hash,
    constr_holding_empty_bytes_planner_claims,
    1,
  )
}

const constr_starting_with_empty_bytes_output =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a00246d8799f4001ff"

const constr_starting_with_empty_bytes_witnesses =
  #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87b80d8799f46d8799f4001ffffffd87c9fd87a80d8799f46d8799f4001ffffffd87c9fd87980d8799f46d8799f4001ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f00004000004000008000d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87a80d8799f46d8799f4001ffffffd87c9fd87980d8799f46d8799f4001ffffffd87c9fd87980d8799f46d8799f4001ffffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f00004000004000019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd87c9fd87980d8799f46d8799f4001ffffffd87c9fd87f9fd8799f00004000004000029fd8799f015820e644b8b04fb1f16375b92127ea5b8cf59098fa2f7141805715d887aa8b2bbb15ffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffff01d8799f58206102804ee42d85218a31c7dda323bccc1ad4b31a9f3f3880449bdfca06a307dc0105ff9f58204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffffd87a80ffd87c9fd87f9fd8799f00004000004000029fd8799f015820e644b8b04fb1f16375b92127ea5b8cf59098fa2f7141805715d887aa8b2bbb15ffff01d8799f5820c66d34dc169dc55d9f05404a8d8b1e711f9e1e7fa38accdb2c01225262c4e763010105ffff00d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff9f58206a4eb2a14e5295bd2a48287c98e33b7948b9536ad5b9daef8a542fa898c49322ffffd87a80ffd87c9fd905019fd8799f00004000004000029fd8799f015820e644b8b04fb1f16375b92127ea5b8cf59098fa2f7141805715d887aa8b2bbb15ffff02d8799f58208fe7d531abef51c62880cb6f54137e5007a6743df67ba733ebee0a4bb9d7f8e002020affffd87a80ffd87a80ffd87980ff"

const constr_starting_with_empty_bytes_terminal_hash =
  #"86bb3433c251b07df67e7e3ec827231a22ccae02ebd61c45b5b37f8451b74029"

const constr_starting_with_empty_bytes_planner_claims =
  #"9f9f0cd8799f182b06035820cd215fde0af7f804be5726273a2dfacd8088b852f9170b9d02183327d0c5f082d8799f0100182e0100d87a80ffffff9f0dd8799f182b06035820cd215fde0af7f804be5726273a2dfacd8088b852f9170b9d02183327d0c5f082d8799f0101182e0100d8799fd8799f01000000d8799f000080ffd8799fd8799f01000016584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f181cd8799f182b06035820cd215fde0af7f804be5726273a2dfacd8088b852f9170b9d02183327d0c5f082d8799f0101182e0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffff9f181dd8799f182b06035820cd215fde0af7f804be5726273a2dfacd8088b852f9170b9d02183327d0c5f082d8799f0103182e0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffff9f181fd8799f182b06045820a9f68596695906b87360ab98ae1b394ee0da715cf4eab0b975bdb88b47b9acc4d8799f0100182f0100d87a80ffffff9f1820d8799f182b06045820a9f68596695906b87360ab98ae1b394ee0da715cf4eab0b975bdb88b47b9acc4d8799f0101182f0105d8799fd8799f0100182f01d8799f000080ffd8799fd8799f01000017584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f182fd8799f182b06045820a9f68596695906b87360ab98ae1b394ee0da715cf4eab0b975bdb88b47b9acc4d8799f0101182f0105d8799fd8799f0101182f01d8799f01019fd8799f005820eb5fd47ef72521e3f86fe5eb13a464d7542ed021a5140ff4590b36d8432b002401ffffffd87a80ffffffffff9f1830d8799f182b06045820a9f68596695906b87360ab98ae1b394ee0da715cf4eab0b975bdb88b47b9acc4d8799f0102182f0105d8799fd8799f0101182f01d8799f01019fd8799f005820eb5fd47ef72521e3f86fe5eb13a464d7542ed021a5140ff4590b36d8432b002401ffffffd87a80ffffffffffff"

/// The datum `Constr 0 [B"", 1]`.
test constructor_starting_with_an_empty_byte_string_replays_the_typescript_trace() {
  replays(
    constr_starting_with_empty_bytes_output,
    #"d8799f4001ff",
    constr_starting_with_empty_bytes_witnesses,
    constr_starting_with_empty_bytes_terminal_hash,
    constr_starting_with_empty_bytes_planner_claims,
    1,
  )
}

const map_with_empty_bytes_key_output =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a00243a14000"

const map_with_empty_bytes_key_witnesses =
  #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87c80d8799f43a14000ffffd87c9fd87a80d8799f43a14000ffffd87c9fd87980d8799f43a14000ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f03004000004002008000d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ffffffffd87a80ffd87c9fd87a80d8799f43a14000ffffd87c9fd87980d8799f43a14000ffffd87c9fd87980d8799f43a14000ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f03004000004002019fd8799f0058204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffff00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ffffffffd87a80ffd87c9fd905009fd8799f03004000004002029fd8799f01582069ba927ffb89db72ddfe802258c4a445bc04942bdf483ec0f963c675cdc51ce5ffff00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ffff00d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ffd8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105ff9f5820ebfd10c4a2e729a2ded716392352fe681c9ebb85b93dec6698a7ebb223f9ea16ff9f58204e031d4f96ae82d7fad43d65ddb0f52be3182f9b33eaca92e11392ae380f07abffffd87a80ffd87c9fd905019fd8799f03004000004002029fd8799f01582069ba927ffb89db72ddfe802258c4a445bc04942bdf483ec0f963c675cdc51ce5ffff01d8799f5820a43ad9958682ac4eb2811a3450133ba0addcde6e3107b4108824138c5660c19701020affffd87a80ffd87a80ffd87980ff"

const map_with_empty_bytes_key_terminal_hash =
  #"451b49d73d74c602509ca14a55ac1261f2bcba5c68d03be526c44ac7ad08c4a3"

const map_with_empty_bytes_key_planner_claims =
  #"9f9f0cd8799f182b03015820260518ba81d70f59504e1a2880942f93e4a047fac1958c73cbe38ce00b7e7f93d8799f0100182c0100d87a80ffffff9f0dd8799f182b03015820260518ba81d70f59504e1a2880942f93e4a047fac1958c73cbe38ce00b7e7f93d8799f0101182c0100d8799fd8799f01000000d8799f000080ffd8799fd8799f01000016584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f181cd8799f182b03015820260518ba81d70f59504e1a2880942f93e4a047fac1958c73cbe38ce00b7e7f93d8799f0101182c0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffff9f181dd8799f182b03015820260518ba81d70f59504e1a2880942f93e4a047fac1958c73cbe38ce00b7e7f93d8799f0103182c0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffff9f181fd8799f182b030258202583b04d99310318fcdd671701af93f84517f20b16b257c7e42933a2e2c00c4cd8799f0100182d0100d87a80ffffff9f1820d8799f182b030258202583b04d99310318fcdd671701af93f84517f20b16b257c7e42933a2e2c00c4cd8799f0101182d0105d8799fd8799f0100182d01d8799f000080ffd8799fd8799f01000017584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f182fd8799f182b030258202583b04d99310318fcdd671701af93f84517f20b16b257c7e42933a2e2c00c4cd8799f0101182d0105d8799fd8799f0101182d01d8799f01019fd8799f0058205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df301ffffffd87a80ffffffffff9f1830d8799f182b030258202583b04d99310318fcdd671701af93f84517f20b16b257c7e42933a2e2c00c4cd8799f0102182d0105d8799fd8799f0101182d01d8799f01019fd8799f0058205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df301ffffffd87a80ffffffffffff"

/// The datum `{B"": 0}`.
test map_with_an_empty_byte_string_key_replays_the_typescript_trace() {
  replays(
    map_with_empty_bytes_key_output,
    #"a14000",
    map_with_empty_bytes_key_witnesses,
    map_with_empty_bytes_key_terminal_hash,
    map_with_empty_bytes_key_planner_claims,
    1,
  )
}

const map_with_empty_bytes_value_output =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a00243a10040"

const map_with_empty_bytes_value_witnesses =
  #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87c80d8799f43a10040ffffd87c9fd87a80d8799f43a10040ffffd87c9fd87980d8799f43a10040ffffd87c9fd87980d8799f43a10040ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f03004000004002008000d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ffffffffd87a80ffd87c9fd87a80d8799f43a10040ffffd87c9fd87980d8799f43a10040ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f03004000004002019fd8799f0058206a9194f205ef503230d073cded1b21eb8891ba6df37cb446dda7f703a00dd05bffff00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ffffffffd87a80ffd87c9fd905009fd8799f03004000004002029fd8799f015820a52bfa3178ba0b906d297cbbdcbce064369abb67dccc9a5fd94a98e40d89c984ffff00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ffff00d8799f58200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105ffd8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff9f582084a23d5356d95ebc0e196f044589d386f31e492462db23c29f883c49f37981beff9f58206a9194f205ef503230d073cded1b21eb8891ba6df37cb446dda7f703a00dd05bffffd87a80ffd87c9fd905019fd8799f03004000004002029fd8799f015820a52bfa3178ba0b906d297cbbdcbce064369abb67dccc9a5fd94a98e40d89c984ffff01d8799f582015eb547d20c86032bf253ac722b17b26d76fa4a23f499fddc96f4753eab636e001020affffd87a80ffd87a80ffd87980ff"

const map_with_empty_bytes_value_terminal_hash =
  #"86a0169a30ba7952c0f5f2f6251ac589ef1c69b182f014223245c0f2c90dcd5a"

const map_with_empty_bytes_value_planner_claims =
  #"9f9f0cd8799f182b03015820260518ba81d70f59504e1a2880942f93e4a047fac1958c73cbe38ce00b7e7f93d8799f0100182c0100d87a80ffffff9f0dd8799f182b03015820260518ba81d70f59504e1a2880942f93e4a047fac1958c73cbe38ce00b7e7f93d8799f0101182c0105d8799fd8799f0100182c01d8799f000080ffd8799fd8799f01000017584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f181cd8799f182b03015820260518ba81d70f59504e1a2880942f93e4a047fac1958c73cbe38ce00b7e7f93d8799f0101182c0105d8799fd8799f0101182c01d8799f01019fd8799f0058205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df301ffffffd87a80ffffffffff9f181dd8799f182b03015820260518ba81d70f59504e1a2880942f93e4a047fac1958c73cbe38ce00b7e7f93d8799f0102182c0105d8799fd8799f0101182c01d8799f01019fd8799f0058205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df301ffffffd87a80ffffffffff9f181fd8799f182b03025820e154b7c3e3dd45d90e419038d26fff72e2668726fc7c78078e56011d4365c081d8799f0100182d0100d87a80ffffff9f1820d8799f182b03025820e154b7c3e3dd45d90e419038d26fff72e2668726fc7c78078e56011d4365c081d8799f0101182d0100d8799fd8799f01000000d8799f000080ffd8799fd8799f01000016584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f182fd8799f182b03025820e154b7c3e3dd45d90e419038d26fff72e2668726fc7c78078e56011d4365c081d8799f0101182d0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffff9f1830d8799f182b03025820e154b7c3e3dd45d90e419038d26fff72e2668726fc7c78078e56011d4365c081d8799f0103182d0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffffff"

/// The datum `{0: B""}`.
test map_with_an_empty_byte_string_value_replays_the_typescript_trace() {
  replays(
    map_with_empty_bytes_value_output,
    #"a10040",
    map_with_empty_bytes_value_witnesses,
    map_with_empty_bytes_value_terminal_hash,
    map_with_empty_bytes_value_planner_claims,
    1,
  )
}

const empty_bytes_before_a_script_output =
  #"a400581d781111111111111111111111111111111111111111111111111111111101821a001e8480a00241400382034a6b6b6b6b6b6b6b6b6b6b"

const empty_bytes_before_a_script_witnesses =
  #"9fd87a80d87a80d87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87a80d8799f4f400382034a6b6b6b6b6b6b6b6b6b6bffffd87c9fd87980d8799f4f400382034a6b6b6b6b6b6b6b6b6b6bffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980d87f9f4f400382034a6b6b6b6b6b6b6b6b6b6bffd87980d87f9f4f400382034a6b6b6b6b6b6b6b6b6b6bffd87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980ff"

const empty_bytes_before_a_script_terminal_hash =
  #"2e13fa5859ee029e6e5208680dd69ce3a3da71db959c20e995fca5dde5bdcaa6"

const empty_bytes_before_a_script_planner_claims =
  #"9f9f0dd8799f182b010040d8799f0100182b0100d87a80ffffff9f0ed8799f182b010040d8799f0101182b0100d8799fd8799f01000000d8799f000080ffd8799fd8799f01000016584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f181dd8799f182b010040d8799f0101182b0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffff9f181ed8799f182b010040d8799f0103182b0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffffff"

/// The datum `B""`, then a ten-byte PlutusV3 reference script.
test empty_byte_string_datum_before_a_reference_script_replays_the_typescript_trace() {
  replays(
    empty_bytes_before_a_script_output,
    #"40",
    empty_bytes_before_a_script_witnesses,
    empty_bytes_before_a_script_terminal_hash,
    empty_bytes_before_a_script_planner_claims,
    1,
  )
}

const empty_asset_name_and_bytes_output =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a1581c55555555555555555555555555555555555555555555555555555555a14001024140"

const empty_asset_name_and_bytes_witnesses =
  #"9fd87a80d87a80d87a80d87a80d87a80d87a80d87980d87980d87b9f00581c55555555555555555555555555555555555555555555555555555555400180d87a80ffd87980d87980d87980d87e80d87c9fd87a80d8799f4140ffffd87c9fd87980d8799f4140ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980ff"

const empty_asset_name_and_bytes_terminal_hash =
  #"b7092ea0d2e58f6c6fc264092e76559d514e1f792d71eb7a9fe418b705186648"

const empty_asset_name_and_bytes_planner_claims =
  #"9f9f0ed8799f184c010040d8799f0100184c0100d87a80ffffff9f0fd8799f184c010040d8799f0101184c0100d8799fd8799f01000000d8799f000080ffd8799fd8799f01000016584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f181ed8799f184c010040d8799f0101184c0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffff9f181fd8799f184c010040d8799f0103184c0100d8799fd8799f01010000d8799f01009fd8799f0058205b5fb0c983697cff1977899896435e4e8f24cd6f761099da0c4a04a6a90e1c6c00ffffffd87a80ffffffffffff"

/// A token with an empty asset name, and the datum `B""`.
test empty_asset_name_and_empty_byte_string_datum_replays_the_typescript_trace() {
  replays(
    empty_asset_name_and_bytes_output,
    #"40",
    empty_asset_name_and_bytes_witnesses,
    empty_asset_name_and_bytes_terminal_hash,
    empty_asset_name_and_bytes_planner_claims,
    1,
  )
}

const empty_reference_script_output =
  #"a400581d781111111111111111111111111111111111111111111111111111111101821a001e8480a002410003820340"

const empty_reference_script_witnesses =
  #"9fd87a80d87a80d87a80d87a80d87a80d87980d87980d87980d87980d87e80d87c9fd87a80d8799f450003820340ffffd87c9fd87980d8799f450003820340ffffd87c9fd87980d8799f450003820340ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980d87f9f450003820340ffd87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980d87980ff"

const empty_reference_script_terminal_hash =
  #"1ddddfa46a9e27729f5897fe00970a24312fe82b5eb5fd086c314a65e3402163"

const empty_reference_script_planner_claims =
  #"9f9f0bd8799f182b010040d8799f0100182b0100d87a80ffffff9f0cd8799f182b010040d8799f0101182b0105d8799fd8799f0100182b01d8799f000080ffd8799fd8799f01000017584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f181bd8799f182b010040d8799f0101182b0105d8799fd8799f0101182b01d8799f01019fd8799f0058205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df301ffffffd87a80ffffffffff9f181cd8799f182b010040d8799f0102182b0105d8799fd8799f0101182b01d8799f01019fd8799f0058205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df301ffffffd87a80ffffffffffff"

/// The datum `0`, then an empty PlutusV3 reference script.
test empty_reference_script_replays_the_typescript_trace() {
  replays(
    empty_reference_script_output,
    #"00",
    empty_reference_script_witnesses,
    empty_reference_script_terminal_hash,
    empty_reference_script_planner_claims,
    0,
  )
}

const one_byte_string_output =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a002424101"

const one_byte_string_witnesses =
  #"9fd87a80d87a80d87a80d87a80d87980d87980d87980d87980d87980d87e80d87c9fd87a80d8799f424101ffffd87c9fd87980d8799f424101ffffd87c9fd87980d8799f424101ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980ff"

const one_byte_string_terminal_hash =
  #"4955dc6cb5ccb6ceedbc641f79a9a56b2b063bad7b5c5c0f7e3d7493c874819a"

const one_byte_string_planner_claims =
  #"9f9f0bd8799f182b020040d8799f0100182b0200d87a80ffffff9f0cd8799f182b020040d8799f0101182b0201d8799fd8799f01000001d8799f000080ffd8799fd8799f01000017584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f181bd8799f182b020040d8799f0101182b0201d8799fd8799f01010001d8799f01019fd8799f005820eb5fd47ef72521e3f86fe5eb13a464d7542ed021a5140ff4590b36d8432b002401ffffffd87a80ffffffffff9f181cd8799f182b020040d8799f0103182b0201d8799fd8799f01010001d8799f01019fd8799f005820eb5fd47ef72521e3f86fe5eb13a464d7542ed021a5140ff4590b36d8432b002401ffffffd87a80ffffffffffff"

/// The datum `B"01"`: no empty span.
test one_byte_string_datum_replays_the_typescript_trace() {
  replays(
    one_byte_string_output,
    #"4101",
    one_byte_string_witnesses,
    one_byte_string_terminal_hash,
    one_byte_string_planner_claims,
    0,
  )
}

// Two-chunk outputs whose datum is a 5000-byte chunked byte string (or an
// integer over one). The datum's scalar is read in source-blob chunks, so
// span attaches occur at the bytes or integer stage, both while the scalar
// is measured (no blob yet) and while its blob is hashed (a blob with an
// active hash), and the attach reads every level of the datum sub-control.

/// The CBOR chunks of an indefinite byte string of `length` bytes `fill`,
/// 64 bytes per chunk.
fn byte_string_chunks(length: Int, fill: Int) -> ByteArray {
  if length <= 0 {
    #""
  } else {
    let chunk = if length < 64 {
      length
    } else {
      64
    }
    let head = if chunk < 24 {
      builtin.cons_bytearray(0x40 + chunk, #"")
    } else {
      builtin.cons_bytearray(0x58, builtin.cons_bytearray(chunk, #""))
    }
    bytearray.concat(
      head,
      bytearray.concat(
        builtin.replicate_byte(chunk, fill),
        byte_string_chunks(length - chunk, fill),
      ),
    )
  }
}

/// The indefinite byte string of 5000 bytes `fill`.
fn five_kilobyte_bytes(fill: Int) -> ByteArray {
  bytearray.concat(
    #"5f",
    bytearray.concat(byte_string_chunks(5000, fill), #"ff"),
  )
}

/// `a3 00 address 01 value 02 bytes(datum)`, with the datum's
/// three-byte bytes head `datum_head`.
fn datum_output(datum_head: ByteArray, datum: ByteArray) -> ByteArray {
  bytearray.concat(
    #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a002",
    bytearray.concat(datum_head, datum),
  )
}

const large_bytes_datum_witnesses =
  #"9fd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0020ffd87a9f0120ffd87980d87980d87980d87980d87980d87e9f0001ffd905029fd87a80ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd87e9f0120ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd87e9f0001ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87e9f0120ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980ff"

const large_bytes_datum_terminal_hash =
  #"d4135281e19c7de5ba3ed1d3fdb50cf47aa56ceb409a18a8182f49158f81be50"

const large_bytes_datum_planner_claims =
  #"9f9f0cd8799f182d1914270040d8799f0102182d0100d87a80ffffff9f185ed8799f182d1914270040d8799f0101182d191427191388d8799fd8799f010000191388d8799f000080ffd8799fd8799f010000191017584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f19029fd8799f182d1914270040d8799f0101182d191427191388d8799fd8799f010100191388d8799f021913889fd8799f015820b5eb32b42f9bcf6ef9201e26576fd0dc9d482e09d8324203fb1600e6a7cbd0a8191388ffffffd87a80ffffffffff9f1902a0d8799f182d1914270040d8799f0103182d191427191388d8799fd8799f010100191388d8799f021913889fd8799f015820b5eb32b42f9bcf6ef9201e26576fd0dc9d482e09d8324203fb1600e6a7cbd0a8191388ffffffd87a80ffffffffffff"

/// The datum is a 5000-byte byte string: span attaches at the datum head,
/// at the bytes stage while measuring, and twice at the bytes stage with a
/// hashing blob.
test five_kilobyte_byte_string_datum_replays_the_typescript_trace() {
  let output = datum_output(#"591427", five_kilobyte_bytes(0xab))
  expect bytearray.length(output) == 5204
  replays_where(
    output,
    builtin.b_data(builtin.replicate_byte(5000, 0xab)),
    large_bytes_datum_witnesses,
    large_bytes_datum_terminal_hash,
    large_bytes_datum_planner_claims,
    0,
    fn(attaches) { attaches == [0, 200, 211, 211] },
  )
}

const large_integer_datum_witnesses =
  #"9fd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0020ffd87a9f0120ffd87980d87980d87980d87980d87980d87e9f0001ffd905029fd87a80ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd87e9f0120ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd87e9f0001ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87e9f0120ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980ff"

const large_integer_datum_terminal_hash =
  #"945fa49f6802ba295e7a4c60b58eb130aafdb0ff6faf454e853c9a0d250b7cd7"

const large_integer_datum_planner_claims =
  #"9f9f0cd8799f182d1914280040d8799f0103182d0200d87a80ffffff9f185ed8799f182d1914280040d8799f0101182d191428190275d8799fd8799f0100182d191428d8799f000080ffd8799fd8799f010000191017584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f1902add8799f182d1914280040d8799f0101182d191428190275d8799fd8799f0101182d191428d8799f021914289fd8799f015820130afe7bef03d587a0e69348e2fe780254207fa2210fe12dd20c25a4037f2d09191428ffffffd87a80ffffffffff9f1902aed8799f182d1914280040d8799f0102182d191428190275d8799fd8799f0101182d191428d8799f021914289fd8799f015820130afe7bef03d587a0e69348e2fe780254207fa2210fe12dd20c25a4037f2d09191428ffffffd87a80ffffffffffff"

/// The datum is a positive integer over a 5000-byte byte string: span
/// attaches at the datum head, at the integer stage while measuring, and
/// twice at the integer stage with a hashing blob.
test five_kilobyte_integer_datum_replays_the_typescript_trace() {
  let output = datum_output(
    #"591428",
    bytearray.concat(#"c2", five_kilobyte_bytes(1)),
  )
  expect bytearray.length(output) == 5205
  replays_where(
    output,
    builtin.i_data(
      builtin.bytearray_to_integer(True, builtin.replicate_byte(5000, 1)),
    ),
    large_integer_datum_witnesses,
    large_integer_datum_terminal_hash,
    large_integer_datum_planner_claims,
    0,
    fn(attaches) { attaches == [0, 100, 111, 111] },
  )
}

// An empty byte string after a chunked byte string, at the last byte of the
// first chunk, at the first byte of the second chunk and inside the second
// chunk: the split span selection agrees with the typed one at every datum
// step, and neither step attaches a window over the empty span.

/// The datum `[B"ab.." (length bytes), B""]` in its CBOR form.
fn list_ending_with_an_empty_byte_string(length: Int) -> ByteArray {
  bytearray.concat(
    #"9f5f",
    bytearray.concat(byte_string_chunks(length, 0xab), #"ff40ff"),
  )
}

fn replays_an_empty_byte_string_after(
  length: Int,
  datum_head: ByteArray,
  total: Int,
  witnesses: ByteArray,
  terminal_hash: ByteArray,
  planner_claims: ByteArray,
) -> Bool {
  let output = datum_output(
    datum_head,
    list_ending_with_an_empty_byte_string(length),
  )
  expect bytearray.length(output) == total
  replays_where(
    output,
    builtin.list_data(
      [
        builtin.b_data(builtin.replicate_byte(length, 0xab)),
        builtin.b_data(#""),
      ],
    ),
    witnesses,
    terminal_hash,
    planner_claims,
    1,
    fn(attaches) { attaches == [0] },
  )
}

const empty_bytes_ending_the_first_chunk_witnesses =
  #"9fd87a9f0020ffd87a9f0020ffd87a9f0020ffd87a9f0020ffd87980d87980d87980d87980d87980d87e9f0020ffd905029fd87b80ffd905029fd87a80ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000008000d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd905029fd87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000019fd8799f00582075a85a5c69153746625906b29a7f9ff020582a918dd513f80fe8fbb1298dc107ffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd905029fd87980ffd87c9fd87f9fd8799f02004000004000029fd8799f015820967b776e1b44fccd67eec9614fd3bd13e365a6568f2b7d1a3e74d85ba8cbe17cffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffff01d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff9f582075a85a5c69153746625906b29a7f9ff020582a918dd513f80fe8fbb1298dc107ffffd87a80ffd87c9fd87f9fd8799f02004000004000029fd8799f015820967b776e1b44fccd67eec9614fd3bd13e365a6568f2b7d1a3e74d85ba8cbe17cffff01d8799f582016925ec20d2cfa48b7d727ce43aabab6f695dcc94463febd74e2c67631181800010105ffff00d8799f582043faf06513e0dc0ca795c8b1bf96cbb66cf0cb870229dea29b0b541e51093585190fcf1901efff9f582084a23d5356d95ebc0e196f044589d386f31e492462db23c29f883c49f37981beffffd87a80ffd87c9fd905019fd8799f02004000004000029fd8799f015820967b776e1b44fccd67eec9614fd3bd13e365a6568f2b7d1a3e74d85ba8cbe17cffff02d8799f58204b263814dcb4858c41a0e7591c4f22296ed83518cdb8f58e2f7dfe09bec9a64f02190fd01901f4ffffd87a80ffd87a80ffd87980ff"

const empty_bytes_ending_the_first_chunk_terminal_hash =
  #"bfb94ce9c7174a2ca7b0a6b2f161e18aa6a783d9e0c5f60e672376318159816e"

const empty_bytes_ending_the_first_chunk_planner_claims =
  #"9f9f0cd8799f182d190fd20158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0102182e0100d87a80ffffff9f184bd8799f182d190fd20158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0101182e190fcf190f52d8799fd8799f010000190f52d8799f000080ffd8799fd8799f010000190f6a584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f1901fed8799f182d190fd20158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0101182e190fcf190f52d8799fd8799f010100190f52d8799f01190f529fd8799f005820b79345913ad14dfcf5c0b0cc8c6088f11212daa6bdcee04c3171e31ff79cc3a4190f52ffffffd87a80ffffffffff9f1901ffd8799f182d190fd20158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0103182e190fcf190f52d8799fd8799f010100190f52d8799f01190f529fd8799f005820b79345913ad14dfcf5c0b0cc8c6088f11212daa6bdcee04c3171e31ff79cc3a4190f52ffffffd87a80ffffffffffff"

/// The empty byte string ending the first chunk of the 4095-byte output.
test empty_byte_string_ending_the_first_chunk_replays_the_typescript_trace() {
  replays_an_empty_byte_string_after(
    3922,
    #"590fd2",
    4095,
    empty_bytes_ending_the_first_chunk_witnesses,
    empty_bytes_ending_the_first_chunk_terminal_hash,
    empty_bytes_ending_the_first_chunk_planner_claims,
  )
}

const empty_bytes_starting_the_second_chunk_witnesses =
  #"9fd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0020ffd87a9f0120ffd87980d87980d87980d87980d87980d87e9f0001ffd905029fd87b80ffd905029fd87a80ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000008000d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd905029fd87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000019fd8799f00582090553e0b33f534685b7cb1c4c9ea54892bf90cba8f794a3824e6c8d077405e21ffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd905029fd87980ffd87c9fd87f9fd8799f02004000004000029fd8799f015820eb5a55342b18baba38f5db9b6c498a4435e7346d54a83aa878748b9fe7979a49ffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffff01d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff9f582090553e0b33f534685b7cb1c4c9ea54892bf90cba8f794a3824e6c8d077405e21ffffd87a80ffd87c9fd87f9fd8799f02004000004000029fd8799f015820eb5a55342b18baba38f5db9b6c498a4435e7346d54a83aa878748b9fe7979a49ffff01d8799f582016925ec20d2cfa48b7d727ce43aabab6f695dcc94463febd74e2c67631181800010105ffff00d8799f5820966678d17aeff9b364b5762874990fbf85984de223df6c9c3007d4fffac2efbe190fd01901efff9f582084a23d5356d95ebc0e196f044589d386f31e492462db23c29f883c49f37981beffffd87a80ffd87c9fd905019fd8799f02004000004000029fd8799f015820eb5a55342b18baba38f5db9b6c498a4435e7346d54a83aa878748b9fe7979a49ffff02d8799f582040af5777c12b730efcfff0291768be745a21f36724700adda21de89a5cded63e02190fd11901f4ffffd87a80ffd87a80ffd87980ff"

const empty_bytes_starting_the_second_chunk_terminal_hash =
  #"35fe064678265e055ffa96ed0f2ccf05a7a7517fbbfa6800c95bd08e3cfe0017"

const empty_bytes_starting_the_second_chunk_planner_claims =
  #"9f9f0dd8799f182d190fd30158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0102182e0100d87a80ffffff9f184cd8799f182d190fd30158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0101182e190fd0190f53d8799fd8799f010000190f53d8799f000080ffd8799fd8799f010000190f6b584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f1901ffd8799f182d190fd30158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0101182e190fd0190f53d8799fd8799f010100190f53d8799f01190f539fd8799f005820f9826809229ff00afc27bda0ff91078e5193b5f62227e10ceebed0ca84628fb6190f53ffffffd87a80ffffffffff9f190200d8799f182d190fd30158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0103182e190fd0190f53d8799fd8799f010100190f53d8799f01190f539fd8799f005820f9826809229ff00afc27bda0ff91078e5193b5f62227e10ceebed0ca84628fb6190f53ffffffd87a80ffffffffffff"

/// The empty byte string whose content would start the second chunk of the 4096-byte output.
test empty_byte_string_starting_the_second_chunk_replays_the_typescript_trace() {
  replays_an_empty_byte_string_after(
    3923,
    #"590fd3",
    4096,
    empty_bytes_starting_the_second_chunk_witnesses,
    empty_bytes_starting_the_second_chunk_terminal_hash,
    empty_bytes_starting_the_second_chunk_planner_claims,
  )
}

const empty_bytes_inside_the_second_chunk_witnesses =
  #"9fd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0020ffd87a9f0120ffd87980d87980d87980d87980d87980d87e9f0001ffd905029fd87b80ffd905029fd87a80ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000008000d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd905029fd87a80ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd8799fd8799f02004000004000019fd8799f0058200f45411933c72a90d82aa5744de5c3b41189df07aca4cc85e205afb5f104347effff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffffffffd87a80ffd905029fd87980ffd87c9fd87f9fd8799f02004000004000029fd8799f0158200dbc05f54ee6fd140800aa6aaf2ed30945f20e435d3c2f5af35736b07533a833ffff00d8799f58208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000ffff01d8799f5820f3f2777df4485f9e3e4abff00c01e3036f171903ed0279565383a81c0e74929f0105ff9f58200f45411933c72a90d82aa5744de5c3b41189df07aca4cc85e205afb5f104347effffd87a80ffd87c9fd87f9fd8799f02004000004000029fd8799f0158200dbc05f54ee6fd140800aa6aaf2ed30945f20e435d3c2f5af35736b07533a833ffff01d8799f582016925ec20d2cfa48b7d727ce43aabab6f695dcc94463febd74e2c67631181800010105ffff00d8799f5820225214b6071b31ac26bb15cb50b0f401c3085ccf1270a1ac230039953db99fd3190fd11901efff9f582084a23d5356d95ebc0e196f044589d386f31e492462db23c29f883c49f37981beffffd87a80ffd87c9fd905019fd8799f02004000004000029fd8799f0158200dbc05f54ee6fd140800aa6aaf2ed30945f20e435d3c2f5af35736b07533a833ffff02d8799f58204f53a3ed1fbf0212c895eefa5e4c0d41f7f779cf8eea4667254ec867e4e7c54f02190fd21901f4ffffd87a80ffd87a80ffd87980ff"

const empty_bytes_inside_the_second_chunk_terminal_hash =
  #"c00dea2298bc1c6c08079ded420ad984c87f648c5e4aa957a935bdcb62ed2d54"

const empty_bytes_inside_the_second_chunk_planner_claims =
  #"9f9f0dd8799f182d190fd40158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0102182e0100d87a80ffffff9f184cd8799f182d190fd40158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0101182e190fd1190f54d8799fd8799f010000190f54d8799f000080ffd8799fd8799f010000190f6c584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f1901ffd8799f182d190fd40158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0101182e190fd1190f54d8799fd8799f010100190f54d8799f01190f549fd8799f00582027b4a1c6a0fdccde4aa13b04363597e8c3d54fbbf7226bf0fae3c82cdf261075190f54ffffffd87a80ffffffffff9f190200d8799f182d190fd40158201a63c468a0c551cce49c375c363b335f92d5a4e00e14a6c503dfb59ee3f6b549d8799f0103182e190fd1190f54d8799fd8799f010100190f54d8799f01190f549fd8799f00582027b4a1c6a0fdccde4aa13b04363597e8c3d54fbbf7226bf0fae3c82cdf261075190f54ffffffd87a80ffffffffffff"

/// The empty byte string inside the second chunk of the 4097-byte output.
test empty_byte_string_inside_the_second_chunk_replays_the_typescript_trace() {
  replays_an_empty_byte_string_after(
    3924,
    #"590fd4",
    4097,
    empty_bytes_inside_the_second_chunk_witnesses,
    empty_bytes_inside_the_second_chunk_terminal_hash,
    empty_bytes_inside_the_second_chunk_planner_claims,
  )
}

// The frame-form readers of the split span selection, over a datum
// sub-control with every level present: a scalar with a blob whose frontier
// holds two peaks and whose hash trace is active. The same frame with any
// level in the wrong form, with the wrong arity, with a wrong field type or
// with a wrong option tag is refused.

const frame_hash =
  blake256.Blake2b256TraceControlV1 {
    version: 1,
    stage: blake256.stage_ready,
    cursor: 128,
    total_length: 5000,
    chaining_value: #"0101010101010101010101010101010101010101010101010101010101010101",
    active_block: #"02020202",
    active_block_length: 4,
    working_value: #"0303030303030303",
    round: 3,
  }

const frame_peak_high =
  blob_frontier.CekBlobFrontierPeakV1 {
    height: 1,
    root: #"0404040404040404040404040404040404040404040404040404040404040404",
    byte_length: 128,
  }

const frame_peak_low =
  blob_frontier.CekBlobFrontierPeakV1 {
    height: 0,
    root: #"0505050505050505050505050505050505050505050505050505050505050505",
    byte_length: 64,
  }

const frame_blob =
  source_blob.CekSourceBlobControlV1 {
    version: 1,
    stage: source_blob.stage_active,
    source_start: 48,
    source_length: 5000,
    frontier: blob_frontier.CekBlobFrontierV1 {
      count: 3,
      byte_length: 192,
      peaks: [frame_peak_high, frame_peak_low],
    },
    active_hash: Some(frame_hash),
  }

const frame_integer =
  integer_control.CekDataIntegerControlV1 {
    version: 1,
    stage: integer_control.stage_blob,
    source_start: 45,
    source_length: 5160,
    memory: 0,
    blob: Some(frame_blob),
  }

const frame_bytes =
  bytes_control.CekDataBytesControlV1 {
    version: 1,
    stage: bytes_control.stage_blob,
    source_start: 45,
    source_length: 5159,
    bytes_length: 5000,
    blob: Some(frame_blob),
  }

const frame_root =
  #"0606060606060606060606060606060606060606060606060606060606060606"

fn some_data(value: Data) -> Data {
  builtin.constr_data(0, [value])
}

fn none_data() -> Data {
  builtin.constr_data(1, [])
}

fn wire_peak(peak: blob_frontier.CekBlobFrontierPeakV1) -> Data {
  builtin.list_data(
    [
      builtin.i_data(peak.height),
      builtin.b_data(peak.root),
      builtin.i_data(peak.byte_length),
    ],
  )
}

/// The frame-form frontier of `frame_blob` over `peaks`.
fn wire_frontier(peaks: List<Data>) -> Data {
  builtin.list_data(
    [
      builtin.i_data(1),
      builtin.i_data(frame_blob.frontier.count),
      builtin.i_data(frame_blob.frontier.byte_length),
      builtin.list_data(peaks),
    ],
  )
}

/// The frame-form `frame_blob` with `frontier` and `active_hash`.
fn wire_blob(frontier: Data, active_hash: Data) -> Data {
  builtin.list_data(
    [
      builtin.i_data(1),
      builtin.i_data(frame_blob.stage),
      builtin.i_data(frame_blob.source_start),
      builtin.i_data(frame_blob.source_length),
      frontier,
      active_hash,
    ],
  )
}

/// The frame-form `frame_blob` with `active_hash`.
fn wire_blob_hashing(active_hash: Data) -> Data {
  wire_blob(
    wire_frontier([wire_peak(frame_peak_high), wire_peak(frame_peak_low)]),
    active_hash,
  )
}

/// The frame-form `frame_blob` with `frontier`.
fn wire_blob_over(frontier: Data) -> Data {
  wire_blob(frontier, some_data(blake256.control_data_v1(frame_hash)))
}

/// The frame-form `frame_integer` with `blob`.
fn wire_integer(blob: Data) -> Data {
  builtin.list_data(
    [
      builtin.i_data(1),
      builtin.i_data(frame_integer.stage),
      builtin.i_data(frame_integer.source_start),
      builtin.i_data(frame_integer.source_length),
      builtin.i_data(frame_integer.memory),
      blob,
    ],
  )
}

/// The frame-form `frame_integer` with `blob` in place of its blob.
fn wire_integer_over(blob: Data) -> Data {
  wire_integer(some_data(blob))
}

/// A frame at the datum-traversal stage whose datum sub-control is at
/// `stage` with the scalar options `integer` and `bytes`.
fn datum_frame(stage: Int, integer: Data, bytes: Data) -> raw.Frame {
  raw_datum_frame(
    [
      builtin.i_data(1),
      builtin.i_data(stage),
      builtin.i_data(45),
      builtin.i_data(5160),
      builtin.i_data(0),
      builtin.b_data(frame_root),
      integer,
      bytes,
      none_data(),
    ],
  )
}

/// A frame at the datum-traversal stage whose datum sub-control is the list
/// `items`.
fn raw_datum_frame(items: List<Data>) -> raw.Frame {
  let initial = raw.open(
    proof.encode_control_v1(proof.initial_control_v1(0, 5205, frame_root)),
  )
  raw.replace(
    raw.replace(initial, 1, builtin.i_data(proof.stage_datum_traversal)),
    7,
    some_data(builtin.list_data(items)),
  )
}

/// A frame whose integer-stage datum sub-control holds `integer`.
fn integer_frame(integer: Data) -> raw.Frame {
  datum_frame(traverse.stage_integer, integer, none_data())
}

/// The split span selection over the integer-stage frame holding `integer`.
fn integer_span(integer: Data) -> Option<(Int, Int)> {
  narrow_datum.next_source_span(integer_frame(integer))
}

fn typed_span(
  datum_control: traverse.DataTraverseControlV1,
) -> Option<(Int, Int)> {
  when traverse.prevalidated_next_source_span(datum_control) is {
    None -> None
    Some(source_span) -> Some((source_span.absolute_start, source_span.length))
  }
}

fn typed_datum(
  stage: Int,
  integer: Option<integer_control.CekDataIntegerControlV1>,
  bytes: Option<bytes_control.CekDataBytesControlV1>,
) -> traverse.DataTraverseControlV1 {
  traverse.DataTraverseControlV1 {
    version: 1,
    stage,
    source_start: 45,
    source_length: 5160,
    offset: 0,
    frame_root,
    integer,
    bytes,
    result: None,
  }
}

test frame_readers_read_every_level_of_an_integer_sub_control() {
  let wire = wire_integer_over(
    wire_blob_hashing(some_data(blake256.control_data_v1(frame_hash))),
  )
  let frame = integer_frame(some_data(wire))
  let typed = typed_datum(traverse.stage_integer, Some(frame_integer), None)
  and {
    wire == integer_control.control_data_v1(frame_integer),
    narrow_datum.frame_datum_control(frame) == typed,
    typed_span(typed) != None,
    narrow_datum.next_source_span(frame) == typed_span(typed),
  }
}

test frame_readers_read_every_level_of_a_bytes_sub_control() {
  let wire = bytes_control.control_data_v1(frame_bytes)
  let frame = datum_frame(traverse.stage_bytes, none_data(), some_data(wire))
  let typed = typed_datum(traverse.stage_bytes, None, Some(frame_bytes))
  and {
    narrow_datum.frame_datum_control(frame) == typed,
    typed_span(typed) != None,
    narrow_datum.next_source_span(frame) == typed_span(typed),
  }
}

test frame_readers_read_a_scalar_without_a_blob() {
  let integer = integer_control.CekDataIntegerControlV1 {
    ..frame_integer,
    stage: integer_control.stage_measure,
    source_length: 3,
    blob: None,
  }
  let frame = integer_frame(some_data(integer_control.control_data_v1(integer)))
  let typed = typed_datum(traverse.stage_integer, Some(integer), None)
  and {
    narrow_datum.frame_datum_control(frame) == typed,
    typed_span(typed) != None,
    narrow_datum.next_source_span(frame) == typed_span(typed),
  }
}

test frame_readers_read_a_blob_without_an_active_hash() {
  let integer = integer_control.CekDataIntegerControlV1 {
    ..frame_integer,
    blob: Some(
      source_blob.CekSourceBlobControlV1 { ..frame_blob, active_hash: None },
    ),
  }
  let frame = integer_frame(some_data(integer_control.control_data_v1(integer)))
  let typed = typed_datum(traverse.stage_integer, Some(integer), None)
  and {
    narrow_datum.frame_datum_control(frame) == typed,
    typed_span(typed) == None,
    narrow_datum.next_source_span(frame) == typed_span(typed),
  }
}

/// The claimed (record upcast) form of `frame_integer` in the frame.
test frame_refuses_a_constructor_integer_sub_control() fail {
  let claimed: Data = frame_integer
  let span = integer_span(some_data(claimed))
  span == span
}

test frame_refuses_a_constructor_bytes_sub_control() fail {
  let claimed: Data = frame_bytes
  let span = narrow_datum.next_source_span(
    datum_frame(traverse.stage_bytes, none_data(), some_data(claimed)),
  )
  span == span
}

test frame_refuses_an_integer_sub_control_with_an_extra_field() fail {
  let items = builtin.un_list_data(
    integer_control.control_data_v1(frame_integer),
  )
  let span = integer_span(
    some_data(builtin.list_data(list.concat(items, [builtin.i_data(0)]))),
  )
  span == span
}

test frame_refuses_an_integer_sub_control_missing_a_field() fail {
  expect [_, ..rest] = builtin.un_list_data(
    integer_control.control_data_v1(frame_integer),
  )
  let span = integer_span(some_data(builtin.list_data(rest)))
  span == span
}

test frame_refuses_a_bytes_sub_control_with_an_extra_field() fail {
  let items = builtin.un_list_data(bytes_control.control_data_v1(frame_bytes))
  let span = narrow_datum.next_source_span(
    datum_frame(
      traverse.stage_bytes,
      none_data(),
      some_data(builtin.list_data(list.concat(items, [builtin.i_data(0)]))),
    ),
  )
  span == span
}

test frame_refuses_a_constructor_blob() fail {
  let claimed: Data = frame_blob
  let span = integer_span(some_data(wire_integer_over(claimed)))
  span == span
}

test frame_refuses_a_blob_with_an_extra_field() fail {
  let items = builtin.un_list_data(source_blob.control_data_v1(frame_blob))
  let span = integer_span(
    some_data(
      wire_integer_over(
        builtin.list_data(list.concat(items, [builtin.i_data(0)])),
      ),
    ),
  )
  span == span
}

/// The claimed (record upcast) frontier: `constr 0 [count, length, peaks]`.
test frame_refuses_a_constructor_frontier() fail {
  let claimed: Data = frame_blob.frontier
  let span = integer_span(some_data(wire_integer_over(wire_blob_over(claimed))))
  span == span
}

test frame_refuses_a_frontier_missing_its_version() fail {
  expect [_, ..rest] = builtin.un_list_data(
    wire_frontier([wire_peak(frame_peak_high)]),
  )
  let span = integer_span(
    some_data(wire_integer_over(wire_blob_over(builtin.list_data(rest)))),
  )
  span == span
}

test frame_refuses_a_frontier_with_another_version() fail {
  expect [_, ..rest] = builtin.un_list_data(
    wire_frontier([wire_peak(frame_peak_high)]),
  )
  let span = integer_span(
    some_data(
      wire_integer_over(
        wire_blob_over(builtin.list_data([builtin.i_data(2), ..rest])),
      ),
    ),
  )
  span == span
}

test frame_refuses_a_constructor_peak() fail {
  let claimed: Data = frame_peak_low
  let span = integer_span(
    some_data(
      wire_integer_over(
        wire_blob_over(wire_frontier([wire_peak(frame_peak_high), claimed])),
      ),
    ),
  )
  span == span
}

test frame_refuses_a_peak_with_an_extra_field() fail {
  let peak = builtin.list_data(
    list.concat(
      builtin.un_list_data(wire_peak(frame_peak_low)),
      [builtin.i_data(0)],
    ),
  )
  let span = integer_span(
    some_data(
      wire_integer_over(
        wire_blob_over(wire_frontier([wire_peak(frame_peak_high), peak])),
      ),
    ),
  )
  span == span
}

test frame_refuses_a_frontier_with_an_extra_field() fail {
  let frontier = builtin.list_data(
    list.concat(
      builtin.un_list_data(wire_frontier([wire_peak(frame_peak_high)])),
      [builtin.i_data(0)],
    ),
  )
  let span = integer_span(
    some_data(wire_integer_over(wire_blob_over(frontier))),
  )
  span == span
}

test frame_refuses_a_constructor_hash_trace() fail {
  let claimed: Data = frame_hash
  let span = integer_span(
    some_data(wire_integer_over(wire_blob_hashing(some_data(claimed)))),
  )
  span == span
}

test frame_refuses_a_hash_trace_with_an_extra_field() fail {
  let items = builtin.un_list_data(blake256.control_data_v1(frame_hash))
  let span = integer_span(
    some_data(
      wire_integer_over(
        wire_blob_hashing(
          some_data(builtin.list_data(list.concat(items, [builtin.i_data(0)]))),
        ),
      ),
    ),
  )
  span == span
}

test frame_refuses_a_present_option_without_a_value() fail {
  let span = integer_span(builtin.constr_data(0, []))
  span == span
}

test frame_refuses_an_absent_option_with_a_value() fail {
  let span = integer_span(
    builtin.constr_data(1, [integer_control.control_data_v1(frame_integer)]),
  )
  span == span
}

test frame_refuses_an_option_with_another_tag() fail {
  let span = integer_span(
    builtin.constr_data(2, [integer_control.control_data_v1(frame_integer)]),
  )
  span == span
}

test frame_refuses_a_blob_option_with_another_tag() fail {
  let span = integer_span(
    some_data(
      wire_integer(
        builtin.constr_data(2, [source_blob.control_data_v1(frame_blob)]),
      ),
    ),
  )
  span == span
}

test frame_refuses_a_hash_option_with_another_tag() fail {
  let span = integer_span(
    some_data(
      wire_integer_over(
        wire_blob_hashing(
          builtin.constr_data(2, [blake256.control_data_v1(frame_hash)]),
        ),
      ),
    ),
  )
  span == span
}

test frame_refuses_a_present_hash_option_without_a_value() fail {
  let span = integer_span(
    some_data(wire_integer_over(wire_blob_hashing(builtin.constr_data(0, [])))),
  )
  span == span
}

test frame_refuses_a_hash_option_with_two_values() fail {
  let hash = blake256.control_data_v1(frame_hash)
  let span = integer_span(
    some_data(
      wire_integer_over(wire_blob_hashing(builtin.constr_data(0, [hash, hash]))),
    ),
  )
  span == span
}

test frame_refuses_an_absent_hash_option_with_two_values() fail {
  let hash = blake256.control_data_v1(frame_hash)
  let span = integer_span(
    some_data(
      wire_integer_over(wire_blob_hashing(builtin.constr_data(1, [hash, hash]))),
    ),
  )
  span == span
}

test frame_refuses_a_scalar_stage_that_is_not_an_integer() fail {
  expect [version,
        _, ..rest] = builtin.un_list_data(
    integer_control.control_data_v1(frame_integer),
  )
  let span = integer_span(
    some_data(builtin.list_data([version, builtin.b_data(#"01"), ..rest])),
  )
  span == span
}

test frame_refuses_a_datum_sub_control_with_an_extra_field() fail {
  let span = narrow_datum.next_source_span(
    raw_datum_frame(
      [
        builtin.i_data(1),
        builtin.i_data(traverse.stage_head),
        builtin.i_data(45),
        builtin.i_data(5160),
        builtin.i_data(0),
        builtin.b_data(frame_root),
        none_data(),
        none_data(),
        none_data(),
        none_data(),
      ],
    ),
  )
  span == span
}

/// The same head-stage frame with every field in its frame form is read.
test frame_reads_a_head_stage_datum_sub_control() {
  let frame = raw_datum_frame(
    [
      builtin.i_data(1),
      builtin.i_data(traverse.stage_head),
      builtin.i_data(45),
      builtin.i_data(5160),
      builtin.i_data(0),
      builtin.b_data(frame_root),
      none_data(),
      none_data(),
      none_data(),
    ],
  )
  let typed = typed_datum(traverse.stage_head, None, None)
  and {
    narrow_datum.frame_datum_control(frame) == typed,
    narrow_datum.next_source_span(frame) == Some((45, 14)),
    typed_span(typed) == Some((45, 14)),
  }
}

/// The same head-stage datum sub-control under another option tag is refused.
test frame_refuses_a_datum_sub_control_under_another_tag() fail {
  let initial = raw.open(
    proof.encode_control_v1(proof.initial_control_v1(0, 5205, frame_root)),
  )
  let frame = raw.replace(
    raw.replace(initial, 1, builtin.i_data(proof.stage_datum_traversal)),
    7,
    builtin.constr_data(
      1,
      [
        builtin.list_data(
          [
            builtin.i_data(1),
            builtin.i_data(traverse.stage_head),
            builtin.i_data(45),
            builtin.i_data(5160),
            builtin.i_data(0),
            builtin.b_data(frame_root),
            none_data(),
            none_data(),
            none_data(),
          ],
        ),
      ],
    ),
  )
  let span = narrow_datum.next_source_span(frame)
  span == span
}

test frame_refuses_a_hash_cursor_that_is_not_an_integer() fail {
  expect [version,
        stage,
        _, ..rest] = builtin.un_list_data(blake256.control_data_v1(frame_hash))
  let span = integer_span(
    some_data(
      wire_integer_over(
        wire_blob_hashing(
          some_data(
            builtin.list_data([version, stage, builtin.b_data(#"00"), ..rest]),
          ),
        ),
      ),
    ),
  )
  span == span
}

test frame_refuses_a_frontier_length_that_is_not_an_integer() fail {
  let frontier = builtin.list_data(
    [
      builtin.i_data(1),
      builtin.i_data(frame_blob.frontier.count),
      builtin.b_data(#"00"),
      builtin.list_data([]),
    ],
  )
  let span = integer_span(
    some_data(wire_integer_over(wire_blob_over(frontier))),
  )
  span == span
}

test frame_refuses_a_source_start_that_is_not_an_integer() fail {
  let span = narrow_datum.next_source_span(
    raw_datum_frame(
      [
        builtin.i_data(1),
        builtin.i_data(traverse.stage_head),
        builtin.b_data(#"00"),
        builtin.i_data(5160),
        builtin.i_data(0),
        builtin.b_data(frame_root),
        none_data(),
        none_data(),
        none_data(),
      ],
    ),
  )
  span == span
}
