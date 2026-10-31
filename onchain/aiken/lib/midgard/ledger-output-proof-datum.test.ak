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
use midgard/ledger_output_scan_v1 as output_scan
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
  #"9fd87a80d87a80d87a80d87a80d87a80d87a80d87980d87980d87b9f581c55555555555555555555555555555555555555555555555555555555400180ffd87980d87980d87980d87e80d87c9fd87a80d8799f4140ffffd87c9fd87980d8799f4140ffffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980ff"

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

// Two-chunk outputs whose Cardano value is spelled with long asset names up
// to the 5,000-byte value cap, across the 4,095-byte chunk boundary: the
// TypeScript-built proof replays to the same terminal control, whose value
// size is the measured one.

/// The TypeScript-built proof of `output`, whose datum is the integer 0,
/// replays to an exact terminal whose control hash is `terminal_hash` and
/// whose Cardano value is `value_size` bytes.
fn replays_a_long_asset_name_value(
  output: ByteArray,
  value_size: Int,
  witnesses: ByteArray,
  terminal_hash: ByteArray,
  planner_claims: ByteArray,
) -> Bool {
  let (terminal, empty, _) = replay_terminal(output, witnesses, planner_claims)
  and {
    bounded_item_v1.chunk_count(bytearray.length(output)) == 2,
    empty == 0,
    proof.terminal_is_exact_v1(terminal),
    crypto.blake2b_256(proof.encode_control_v1(terminal)) == terminal_hash,
    terminal.output_scan.cardano_value_size == value_size,
    terminal.output_scan.asset_count == 142,
  }
}

const long_asset_names_value_at_the_cap_output =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a1581c55555555555555555555555555555555555555555555555555555555b88e582000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582001aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582002aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582003aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582004aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582005aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582006aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582007aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582008aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582009aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582010aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582011aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582012aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582013aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582014aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582015aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582016aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582017aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582018aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582019aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582020aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582021aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582022aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582023aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582024aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582025aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582026aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582027aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582028aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582029aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582030aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582031aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582032aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582033aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582034aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582035aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582036aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582037aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582038aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582039aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582040aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582041aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582042aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582043aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582044aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582045aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582046aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582047aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582048aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582049aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582050aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582051aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582052aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582053aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582054aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582055aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582056aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582057aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582058aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582059aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582060aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582061aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582062aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582063aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582064aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582065aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582066aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582067aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582068aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582069aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582070aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582071aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582072aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582073aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582074aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582075aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582076aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582077aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582078aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582079aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582080aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582081aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582082aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582083aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582084aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582085aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582086aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582087aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582088aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582089aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158208aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158208baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158208caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0157ffbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb1818024100"

const long_asset_names_value_at_the_cap_witnesses =
  #"9fd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87980d87980d87b9f581c5555555555555555555555555555555555555555555555555555555557ffbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb18189f582045c9d398507488094b23e2995a5b0c9da614df723c4f944e97c1865257af1bbeffffd87b9f581c5555555555555555555555555555555555555555555555555555555558208caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582082f57b0519feb1405dbee893f80f24232df19e284dc64d55908c98c2fab4d0faffffd87b9f581c5555555555555555555555555555555555555555555555555555555558208baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582024608065c83c2e3fe216befccba34be1a2d2bee691130a39fa83521e1685d37858203b4463fa4d79bcc7c86caa1fee8d3c8ecae4d40e944d0b33014ee95962e8ed75ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558208aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820fd31d707dddad5767e124677328eece3f9996a493fc37ea86e4e664e9c52d09158203b4463fa4d79bcc7c86caa1fee8d3c8ecae4d40e944d0b33014ee95962e8ed75ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582089aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582074c55a198b7b852d55a85314f643a109f870089f447b50314074313b6ffd1e755820a1624296e82ae340553fab2ae8a4bc6079df49efb05df9de7912aa9994f5d104ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582088aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820fedee166ca73d1639b0177286b52c24dbad2e9afa922e94ebd74130b1a3ad3385820a1624296e82ae340553fab2ae8a4bc6079df49efb05df9de7912aa9994f5d104ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582087aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582065f3ce2b70524be40809fcaa4144ac1d7acef64d1eb3fe4477ff136172b1241158208d281d2b430d114e07907b578ad20498dfa568c785eb7f5cc2c0f990fe09bdd7582072b1c1209c32f6704a10e72e8c502dd048337f762dc0b4892fbdec0ba6ade987ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582086aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582006b475cbad8b099ceb2dc2309482794aa7328ef78467fd5a2381d54631886c1a58208d281d2b430d114e07907b578ad20498dfa568c785eb7f5cc2c0f990fe09bdd7582072b1c1209c32f6704a10e72e8c502dd048337f762dc0b4892fbdec0ba6ade987ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582085aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820c64b816ed5aa1c6ec789cd0bcf555fdb057e8b5bd6d1f14aefbba585019c267a58202482c74fc58ab3e2d609f2892d729fbbf2cf04dfbd389ff60f4b8b12b01476e3582072b1c1209c32f6704a10e72e8c502dd048337f762dc0b4892fbdec0ba6ade987ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582084aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e1c1ca480488f84d4a04fa2b9d92e4176fb1ccf6a5bf635aced82628cd264a2358202482c74fc58ab3e2d609f2892d729fbbf2cf04dfbd389ff60f4b8b12b01476e3582072b1c1209c32f6704a10e72e8c502dd048337f762dc0b4892fbdec0ba6ade987ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582083aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582011ba7c21bfc0e1a488fe6b4e492efc67a11270ccddeca53ded16dcab7c3e140658209114ffb322c56e49ed0c365b11161e3ed7269db4bd9f8312d2bbe93769d6031758203b0175e1097be5b72df566388a8c4103f2677ebf9549d77708933c8ad64fa452ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582082aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582007e1b364060f0b01a79411e503b40ef65748c9d22ad4b0b4cbc03bdd68a23c5458209114ffb322c56e49ed0c365b11161e3ed7269db4bd9f8312d2bbe93769d6031758203b0175e1097be5b72df566388a8c4103f2677ebf9549d77708933c8ad64fa452ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582081aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582010087333e8c295990e5a85326d4fe7be025cd5821240d17437c7f39b3eb8736258209fcbd267588e8bc8faa68d9ae0c4ade56b481a24f357d00bdb246ba28bb6927a58203b0175e1097be5b72df566388a8c4103f2677ebf9549d77708933c8ad64fa452ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582080aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a68a8f2296b347e77010e6850c2c54779d0892143d9d5fbcc27251d2c27f707558209fcbd267588e8bc8faa68d9ae0c4ade56b481a24f357d00bdb246ba28bb6927a58203b0175e1097be5b72df566388a8c4103f2677ebf9549d77708933c8ad64fa452ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558207faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a133aa2cd144918d09a42ae053f4eb7c6f4125ea1e1cb4540bb52d500f9def025820cb08c87b28ec0cb64c3a648dc999bc8b1136f036e650e95001ccb1984ae042245820403533e61167f9f710baaa7621aa8d9f6cab4d8deb5a175eab5be0d04eab6a1e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558207eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58208486b579ba806d1129956cbe14ab7c5977df3c49888de44ebdf44c02cff0125f5820cb08c87b28ec0cb64c3a648dc999bc8b1136f036e650e95001ccb1984ae042245820403533e61167f9f710baaa7621aa8d9f6cab4d8deb5a175eab5be0d04eab6a1e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558207daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e2b4b12e1078ac2c6e079029ebfd0c3103a4a5336a4015400e2f271d9db97cc658208fa1bedc772e6174e9020b68612714ef9dfc3b0a17e5b45903f1900f7c1d5d455820403533e61167f9f710baaa7621aa8d9f6cab4d8deb5a175eab5be0d04eab6a1e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558207caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58201a7601b486ff3bcb253587a5d22b884b88f8c4716c62f177691318881596323258208fa1bedc772e6174e9020b68612714ef9dfc3b0a17e5b45903f1900f7c1d5d455820403533e61167f9f710baaa7621aa8d9f6cab4d8deb5a175eab5be0d04eab6a1e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558207baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820713cf97e55a285590bc09c1c4f766833e60493e4b4e70aebaec088bb3ad7a0c758203d2c7898e0e9295f316b6b6cd2a5deec4e8b87b1e87db4ff43e4b838e2511c115820cc77ab4130d33860bb1584bce5b5ce5103427bd6124e3181022f50d54fe6356e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558207aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a19108f0a189783e602c7b1e15c493dae98698cff76b08309b0f2977ad996a2758203d2c7898e0e9295f316b6b6cd2a5deec4e8b87b1e87db4ff43e4b838e2511c115820cc77ab4130d33860bb1584bce5b5ce5103427bd6124e3181022f50d54fe6356e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582079aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820413da02d299694732a040081e5c3080fe8a0a17170af41cf75ad76b7c3546e555820ac8adf9366203ff12787118bb8ab0b4051a9c027bcf758d2cc4eb3e2271194585820cc77ab4130d33860bb1584bce5b5ce5103427bd6124e3181022f50d54fe6356e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582078aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58202d2da448141c5dd5db0b682fd93d5f8d65076bdb6e389b91c97835eaebc8a17a5820ac8adf9366203ff12787118bb8ab0b4051a9c027bcf758d2cc4eb3e2271194585820cc77ab4130d33860bb1584bce5b5ce5103427bd6124e3181022f50d54fe6356e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582077aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58207cdf5e07392dc8ae8320171e7c30c79cd8fd77ae4aa6f52fdf07b62b8700e2e95820a88acf1e35ae61de0742470e870712d92d46e423dc5dbce32f3200b9667a4b13582047350b988f37e4ab80baff721a3015973dfc0226b596c8c83a2f40632dfc19db58208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582076aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820ecd6397275daa88bbae0f93b3efd3ca17df2092e18524f4371c96a77c0d69c355820a88acf1e35ae61de0742470e870712d92d46e423dc5dbce32f3200b9667a4b13582047350b988f37e4ab80baff721a3015973dfc0226b596c8c83a2f40632dfc19db58208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582075aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820dafdbcdf16ae62ffe630a16433186b5035857ceae4713de38d61e5c4acc65b1e58200019e04717dc9f703df6cd72ba8478fe1d3576c648a5ea0969867b370cfbf9c0582047350b988f37e4ab80baff721a3015973dfc0226b596c8c83a2f40632dfc19db58208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582074aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58208adc8ac8893508d48bd3690ef2d32d605c5c79da15b585476dbf0a90aab21c7358200019e04717dc9f703df6cd72ba8478fe1d3576c648a5ea0969867b370cfbf9c0582047350b988f37e4ab80baff721a3015973dfc0226b596c8c83a2f40632dfc19db58208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582073aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820edfce037574f30e11e9c14c6ba92c83aa0c343f69c7d492fa019fa52f5ba205c58203edd4bf8266271d12eb16017fb95f27a6d37f3d3e1f5d11a6172a2ef6a3e4dc65820fd7f135bf9418e191f5d5b248277049a177278d673fddcd8af7ed3a1061cdf1058208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582072aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58200c479590273ea6f83641c8fd2c64029b3aff7599beb2de33ccc94f33f0bc8c3558203edd4bf8266271d12eb16017fb95f27a6d37f3d3e1f5d11a6172a2ef6a3e4dc65820fd7f135bf9418e191f5d5b248277049a177278d673fddcd8af7ed3a1061cdf1058208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582071aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58202dbc19ab36466cb645b45852a1e61a954a579fbccd748dce7d9bf9190b85b6375820ed094b409f5aa2383b75c6d851b4ea85dff467e799ad3910c66a7a72a84706615820fd7f135bf9418e191f5d5b248277049a177278d673fddcd8af7ed3a1061cdf1058208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582070aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820cc01e10b48a7e83ea260540f3ba582ff7a6b0216346a6e3bf076da394612ae7c5820ed094b409f5aa2383b75c6d851b4ea85dff467e799ad3910c66a7a72a84706615820fd7f135bf9418e191f5d5b248277049a177278d673fddcd8af7ed3a1061cdf1058208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558206faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820f281727381c91dfbe61a027b6e2ed6a432c073b4129d726364b48f1b23dd542c582046e0cfcbfb74f39c4a7c163621072044801c85a6071617dc84f93417b437b08b58205e994bd8fc2f461ce50489966c089ae16f6b59d7df5724cb8a65a30e616d4ef65820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558206eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58200958edc45ba5135597b4a685cc831e0a5bb134e222441852c0cff0bc2aff927a582046e0cfcbfb74f39c4a7c163621072044801c85a6071617dc84f93417b437b08b58205e994bd8fc2f461ce50489966c089ae16f6b59d7df5724cb8a65a30e616d4ef65820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558206daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205231f0d99343cde017a19f3d372037db4d3343456e96de0e0c79fdcbc0b82c3558205eb3bd42e9fc62980fa7596deb11e8c9ac4d7987d4522d1d78583d21cef28ca558205e994bd8fc2f461ce50489966c089ae16f6b59d7df5724cb8a65a30e616d4ef65820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558206caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820f4b4722ad93bc02b4c764e4e7e91c909fc6ea81b3605405733e6a032d24bd94958205eb3bd42e9fc62980fa7596deb11e8c9ac4d7987d4522d1d78583d21cef28ca558205e994bd8fc2f461ce50489966c089ae16f6b59d7df5724cb8a65a30e616d4ef65820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558206baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820113d5d77d52484d7ce470304b84c9c61a75fd6cf3ab7a70f33569b742af89e8258209781fe3ecbfe06021b3c01050dfe250bc1d25a931600d4e560b5e172d1fe44b5582056cd5a2954619b08a955ce51b90cfa1c04858ba483595215abea502238a5819a5820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558206aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205e92462cfc8017d7ca452038b927244ba4c90b67dac1d354eb144b16a546e3bb58209781fe3ecbfe06021b3c01050dfe250bc1d25a931600d4e560b5e172d1fe44b5582056cd5a2954619b08a955ce51b90cfa1c04858ba483595215abea502238a5819a5820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582069aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820bd2f1dca9811b2ad7ce95979b4e4daddaa3ac866080d49d6a486ce4419104f8d582029b930d690c5a1340213250df7c70c7ae559b748638331b26a01a4a016b5a004582056cd5a2954619b08a955ce51b90cfa1c04858ba483595215abea502238a5819a5820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582068aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820bffa2b2d27be9a6490d5c70f1520935f9336bec8f61cd51b8d39ba0e1b9ea713582029b930d690c5a1340213250df7c70c7ae559b748638331b26a01a4a016b5a004582056cd5a2954619b08a955ce51b90cfa1c04858ba483595215abea502238a5819a5820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582067aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820ea57435a48cb40c618b3d8989ad77dce17603f9a25b652295693b23fee68984b5820e1c805c558c97a175d457ffea70f27aa1fac80392c5d1418afc2ced3ead99402582088b42804e05b3a97d6fd6528adda751703319516aac2daa3173f77f994287c345820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582066aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58201c340bb2b7db7936aef4b183ea228734fdbac3a014bdfbbc5611ffce5de406f25820e1c805c558c97a175d457ffea70f27aa1fac80392c5d1418afc2ced3ead99402582088b42804e05b3a97d6fd6528adda751703319516aac2daa3173f77f994287c345820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582065aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820f790f9e8df4133d965b0a5f3ef809ad4bdd81d5b4b6fafe2ec5fd0ec4b5ff74c5820fed54416d5027ca0ab065e8699d735850dc4450b0280466394a30063cba54674582088b42804e05b3a97d6fd6528adda751703319516aac2daa3173f77f994287c345820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582064aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e420e3b8849c64de525d75c8f8167124ed1097bcdd03547fdc2f6a1ee7ec0a175820fed54416d5027ca0ab065e8699d735850dc4450b0280466394a30063cba54674582088b42804e05b3a97d6fd6528adda751703319516aac2daa3173f77f994287c345820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582063aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58208f5962b204e82d53119ece79b892fce4f14a35a58dca22677cf0e1652c9c2e0f5820354ea3c0173971d53b1e8ae6b0a66c629e7487be2c7d051302857bda2b7a5fd95820918b262cab97e3b7700f93977916eabbc2a92a3bb4f613f5ea9f9cc2fcbc30a95820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582062aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58206f1432f9a7872e8a88da62bc901b9c30028a819c26af2e15cc3d99a8a2174fc95820354ea3c0173971d53b1e8ae6b0a66c629e7487be2c7d051302857bda2b7a5fd95820918b262cab97e3b7700f93977916eabbc2a92a3bb4f613f5ea9f9cc2fcbc30a95820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582061aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820070b5fbf6cab8aa0f0b961862d1c84c65ebf78b067816aaadb6440206bfeeff65820e3ec6325b08dd81d2bb2b9bcf31e43afc9f06a855c12d917c5e2ac3bb31a24945820918b262cab97e3b7700f93977916eabbc2a92a3bb4f613f5ea9f9cc2fcbc30a95820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582060aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582085afd37261d978c6a289e1d2378597cbaab50e1f09c928af5a79bebbea5d602d5820e3ec6325b08dd81d2bb2b9bcf31e43afc9f06a855c12d917c5e2ac3bb31a24945820918b262cab97e3b7700f93977916eabbc2a92a3bb4f613f5ea9f9cc2fcbc30a95820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558205faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820562d64f1ffbc176fa6e0c87d776d886d8fac8b0d1a6503f305775679e3109ef85820bbc2f2d207fd342a62e02e8f1ee93a4d95abd2fce85fe43acc54b34195790f38582086ce118745fb168ee3749aef4c33b4bbf2322547e569684943a91abd8f816c0b5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558205eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e5316ec9a8f2cb40e1e831226e3791d3c6380bb5c66ef13d91ad91063cfa75185820bbc2f2d207fd342a62e02e8f1ee93a4d95abd2fce85fe43acc54b34195790f38582086ce118745fb168ee3749aef4c33b4bbf2322547e569684943a91abd8f816c0b5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558205daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58207bebd2df9a0c5b20fcf780e705ed0c28fd3e170af544276dd28bc70d58d5d3175820ac98d5f145b6991e03d524adcd04e6cc2fbf6afcfddee7700f7123ad6d675479582086ce118745fb168ee3749aef4c33b4bbf2322547e569684943a91abd8f816c0b5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558205caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205b1657d1b47a04d26e0c68e1114869b130fcfd10fe570c2a357ad626f2ac19a95820ac98d5f145b6991e03d524adcd04e6cc2fbf6afcfddee7700f7123ad6d675479582086ce118745fb168ee3749aef4c33b4bbf2322547e569684943a91abd8f816c0b5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558205baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820883af77f7fa8cc2bc9283e3a798221e157ed580f65c5481da7e041ef58532285582098497cb052bd80238d0d60da4b4334dd7dca9af054a533d5cbace126b60a335658209fb12becef76b45025e8e9840ae12946c39f40058fd444bff9ac5e345121653c5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558205aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58207116ca68394550ce1155263ad9c09eb15a4316c93ba6328a256179496eacd387582098497cb052bd80238d0d60da4b4334dd7dca9af054a533d5cbace126b60a335658209fb12becef76b45025e8e9840ae12946c39f40058fd444bff9ac5e345121653c5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582059aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820ca080c5cbf959dfefe9eb3375880f80b2f45440c8e59041e0850f30cdd8b67e358208ab8e929189ff4135a171b0ca6f72a47cb56739dc95884e9f2ec6dd417e8200c58209fb12becef76b45025e8e9840ae12946c39f40058fd444bff9ac5e345121653c5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582058aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582064c044650e9d42a8b2b6b5c62c4a45c9e290185f80d7cf5ff309ae6ff0de5ae058208ab8e929189ff4135a171b0ca6f72a47cb56739dc95884e9f2ec6dd417e8200c58209fb12becef76b45025e8e9840ae12946c39f40058fd444bff9ac5e345121653c5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582057aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582057ef9c9fe898da3e4e593279a663986391bbe434cb2cfbf062dcaf7a4d016d1f5820412181d2c6c4e5a2131b5816c2097c0722b34676777eb5775707429d5fed07cd5820584174d84c3cc2f23da99976d8a9491d42a7f07f7eabe926a819039307cf217658201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582056aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582029a8804ab2ebd7e9fec2db1c10955b2d1e684a85ae43e5afb887488f9736fbac5820412181d2c6c4e5a2131b5816c2097c0722b34676777eb5775707429d5fed07cd5820584174d84c3cc2f23da99976d8a9491d42a7f07f7eabe926a819039307cf217658201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582055aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820d480fdf03a23a629830775813fb858cc8f6f0fc26ceb7acdf5bd7dad9b3407b25820e4a86b38732a723797f8b71b0eeb0b3053a314192c0435137b46fa3d7ceb6bb05820584174d84c3cc2f23da99976d8a9491d42a7f07f7eabe926a819039307cf217658201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582054aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58208b5687fc273064291829bb22a9b0c692af27237f89f1da9cb3dafb9c54aceac65820e4a86b38732a723797f8b71b0eeb0b3053a314192c0435137b46fa3d7ceb6bb05820584174d84c3cc2f23da99976d8a9491d42a7f07f7eabe926a819039307cf217658201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582053aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58207258a40c547529f431b3250b663fa3845dc07f4836bbc068272da6e0293c38c75820a5faef649594c5442e1263557050068674185385bb10a88aa842676980f20fce5820ba0aea8528d40fa58d5c4c86036c3f1456d00119fac0a59b808e76dee6c6753f58201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582052aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58203dba64778abe4645f0126702dde69afd7db40b6c3f7a159ea59ce3101caa9eb25820a5faef649594c5442e1263557050068674185385bb10a88aa842676980f20fce5820ba0aea8528d40fa58d5c4c86036c3f1456d00119fac0a59b808e76dee6c6753f58201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582051aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582049acd8ef97a3d59ed778ee9ec370be3a00bd6c47d392f7d5bc64ad1098733a8758208ed98f84898986c2fb0fe446b14f2637433126ae272e406f1543aeab7a2f9c135820ba0aea8528d40fa58d5c4c86036c3f1456d00119fac0a59b808e76dee6c6753f58201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582050aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a379d0dcb3acb4072b1fe5a274ee8778855107245f925e11f3f91b64f50f9a4058208ed98f84898986c2fb0fe446b14f2637433126ae272e406f1543aeab7a2f9c135820ba0aea8528d40fa58d5c4c86036c3f1456d00119fac0a59b808e76dee6c6753f58201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558204faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58203aa2604a38ae954692e51af8fc3574a17554a61c69465b0225d31f2e6a7d47755820f02d3c3cdf816d3e4827506b9515460e4a731f27ee9a03a684a309fba026b2ba5820c016a5f0c447d2454a8a7da31bede646bd58824d48f66fa37ca92779b016cc2758200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558204eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58206e39d854a200cf00085643d8eeae0315969c11c06571815089f3f9628a7e755d5820f02d3c3cdf816d3e4827506b9515460e4a731f27ee9a03a684a309fba026b2ba5820c016a5f0c447d2454a8a7da31bede646bd58824d48f66fa37ca92779b016cc2758200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558204daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820cb55d4100ba46e0097b8d777c34ff367f3be1691e777379328a13632cc3726fd5820e194dd68483a5f1f17b4558096b5789de24370ffc154ebfacbf2f4c3d0ed8b705820c016a5f0c447d2454a8a7da31bede646bd58824d48f66fa37ca92779b016cc2758200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558204caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820abc0f38fb42a5ffef8704db0c3e2cca3038ed781fe23b5fed8e40b25c8ab7de85820e194dd68483a5f1f17b4558096b5789de24370ffc154ebfacbf2f4c3d0ed8b705820c016a5f0c447d2454a8a7da31bede646bd58824d48f66fa37ca92779b016cc2758200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558204baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205391eb1dad7321f31d71f14728e4ee434116b2b3e38e9e10083572bea91f9855582096a41cc73e21163986fd83dfb87352f62d07c50f89e89134dca0bfceb5d04ebb582092dbb02333e59bcf1817218e561f2e014aebc770892fb0175af7951f6cab62ac58200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558204aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a77f0f92ac04bb421440c267878eb9390b484bc84c17572b8eb093de774868f3582096a41cc73e21163986fd83dfb87352f62d07c50f89e89134dca0bfceb5d04ebb582092dbb02333e59bcf1817218e561f2e014aebc770892fb0175af7951f6cab62ac58200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582049aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582011d1f6870e11bb5a568f18c7cf5c3c412abe7b53d9b2c7dbb612d6e71d12d69e5820a1c8b797db99e355d63f37ad756dd8a7c2e2a807173ae9b09cdc7e424bc19f30582092dbb02333e59bcf1817218e561f2e014aebc770892fb0175af7951f6cab62ac58200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582048aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820682df0f568e0c6376147850dbe182e16ad741aaa32f180a30ff9efc806d3de835820a1c8b797db99e355d63f37ad756dd8a7c2e2a807173ae9b09cdc7e424bc19f30582092dbb02333e59bcf1817218e561f2e014aebc770892fb0175af7951f6cab62ac58200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582047aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582048d5355ee71b3fdbaf4b2398228e0eadfab85c52b4be49220765d884a6bac2155820d7f0bcb4148119ddb3fefbcd9f5166bfec60fa35dcb4fcdb0a5da9ae93116b9258203cec65125954c81479a1698dd925e9d0de2e03d239521b4334aaf7c45ae6f4925820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582046aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820fd9e0fa49e99bd16fe832893be619e4e0900faa72332e95d98b07332c67de5e25820d7f0bcb4148119ddb3fefbcd9f5166bfec60fa35dcb4fcdb0a5da9ae93116b9258203cec65125954c81479a1698dd925e9d0de2e03d239521b4334aaf7c45ae6f4925820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582045aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820ee91dca42abfd53edad99f56b7d28f980875d04fc301b936b46bfb461b12dd5458204c0bc7de7f8fbdb6fb843c660a763b47b0448b7fd51c459148963df85e05f2ab58203cec65125954c81479a1698dd925e9d0de2e03d239521b4334aaf7c45ae6f4925820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582044aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205496d0fcef2327deba20afa76857d7a54ee840365d8bd4ad9c0f4d0f1a995e3958204c0bc7de7f8fbdb6fb843c660a763b47b0448b7fd51c459148963df85e05f2ab58203cec65125954c81479a1698dd925e9d0de2e03d239521b4334aaf7c45ae6f4925820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582043aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58201c556b47d66f9c91ecc5136b5c5c26f7058b1de40bda02cd994de652999df6305820f9023ebd9d123ed68ff644fd8b3cc105b87eac9297aef9c097ba1ce6c8056e4058205901d771069dc05eefefaf46ea7c028fadadeb484f8ac87c39d8388372a54e445820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582042aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820fec9906a376b2d55cecc757a4db348597e7a8c927e003a7b62a286f3185f9bb15820f9023ebd9d123ed68ff644fd8b3cc105b87eac9297aef9c097ba1ce6c8056e4058205901d771069dc05eefefaf46ea7c028fadadeb484f8ac87c39d8388372a54e445820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582041aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58203db0d9e1d0355725faa68f7f776f499221a31a4b9a10250154f42456b4a27efc58200e0bf82bbf1116cc8db013888fd47e972c8914d964f68afa0819b565fefdc24c58205901d771069dc05eefefaf46ea7c028fadadeb484f8ac87c39d8388372a54e445820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582040aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820546cb234a0ed7d8f669b938f6fa7759727e5bc088caefa29b4211abc5ed29cb258200e0bf82bbf1116cc8db013888fd47e972c8914d964f68afa0819b565fefdc24c58205901d771069dc05eefefaf46ea7c028fadadeb484f8ac87c39d8388372a54e445820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558203faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582016b5bfe248f4a5b7d7b6e1d83cedd7475d4082fa9682d52f60b44718d5a213c35820335d454d8ffe3bcd09060eb39beeb566613bbf55f7dc8481f49419d486590c2f58209b9c096d1a9166ad7c41decfc11738ad621c0b6b76bf9cb8c0e5ed020c9386b958207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558203eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582073ca2f3463515ae5e5f37f8688320d2d2a7c5f08143ee430616e4400b61d87af5820335d454d8ffe3bcd09060eb39beeb566613bbf55f7dc8481f49419d486590c2f58209b9c096d1a9166ad7c41decfc11738ad621c0b6b76bf9cb8c0e5ed020c9386b958207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558203daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58204a8cb9e4d508b704a6821720a27ce8e29700832a0fbaa5835f1b6fb1123d498f58204e41c0f986f40428fa2e8f543817e2c860f8665740cbd9f2432698670755ccf858209b9c096d1a9166ad7c41decfc11738ad621c0b6b76bf9cb8c0e5ed020c9386b958207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558203caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582066092cbefce1e436587617e3721a3dadcb982358397c6fde0d3634848789ae0058204e41c0f986f40428fa2e8f543817e2c860f8665740cbd9f2432698670755ccf858209b9c096d1a9166ad7c41decfc11738ad621c0b6b76bf9cb8c0e5ed020c9386b958207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558203baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582018d9b4ae92e7223e97c1d3e13a7d0e3ec7b123778d93e22f060780c0f9bd993d5820f27265cd8fea583071b49cd5e0786a395a33b02d22bdb4f421d5b451b377f9925820392d366d5a4c3072b68879cca22f3bedf95c6ff33d3c8b944783f49b967fd8c358207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558203aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58209314a00fda28ac31e9f77708dc0ed954fe5bd452ac8510b88a5b09d115dd3a775820f27265cd8fea583071b49cd5e0786a395a33b02d22bdb4f421d5b451b377f9925820392d366d5a4c3072b68879cca22f3bedf95c6ff33d3c8b944783f49b967fd8c358207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582039aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582061f601a13ef2d962f773950318c479f266975b1d5aefa5fc18eca1a31f6793cf582049c49e310c12d730b4ef9486c5d3f83bf67e85a4ddda6c982fc75a44f36e69ad5820392d366d5a4c3072b68879cca22f3bedf95c6ff33d3c8b944783f49b967fd8c358207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582038aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820c4277f6eb73f2a7ac2dc354dbb9b0766fd6551da9765360bb3cfd2ca75b79ec2582049c49e310c12d730b4ef9486c5d3f83bf67e85a4ddda6c982fc75a44f36e69ad5820392d366d5a4c3072b68879cca22f3bedf95c6ff33d3c8b944783f49b967fd8c358207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582037aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820b97c91b4d6605e75d28a7cdd9d1929232e2b3c8843887524823f48feaa2cfcfe5820ac4cd1e3c54834f69008699d7b68668525c04ce44819c012b431f086aafb5c29582054015f083a7fa272802093c1b7ddf540fb4a6f9b8d0cba23115fa2a513293c6c5820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582036aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820d7097553b192dcb4b9ce6c44b24e09d9c460cb387965d7c3972cb8610ae7d1e15820ac4cd1e3c54834f69008699d7b68668525c04ce44819c012b431f086aafb5c29582054015f083a7fa272802093c1b7ddf540fb4a6f9b8d0cba23115fa2a513293c6c5820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582035aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820cb0b43ecf5572626537ad22975374bfa0cb183576d7e77435ed95b3031249d5d582061dac83d1b555f8a3715f2d148031e686f5ad1282064c69415ac89c75ad8059a582054015f083a7fa272802093c1b7ddf540fb4a6f9b8d0cba23115fa2a513293c6c5820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582034aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58206133dc2e4c7d3c58876782e0fd4b7d5a6f3b3215dc8fa3777df0359e3e0ec914582061dac83d1b555f8a3715f2d148031e686f5ad1282064c69415ac89c75ad8059a582054015f083a7fa272802093c1b7ddf540fb4a6f9b8d0cba23115fa2a513293c6c5820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582033aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58204dcf1b0c9915f080f3960bf47e3c8d6bf1ffcc28d0a05166b5ec0ba9b35e408c5820f236bcf06de73c39adb961c01668885c0beba85c77dde6db0a70c247ab8bf665582098caa8dcb61e799d9a079e1c301a2f6c54e9b666820378c9e6a660460aa5d2115820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582032aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58203d014d6d86914389d56bc17aa425f31593812e157db20c235a69395184b19bc35820f236bcf06de73c39adb961c01668885c0beba85c77dde6db0a70c247ab8bf665582098caa8dcb61e799d9a079e1c301a2f6c54e9b666820378c9e6a660460aa5d2115820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582031aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a9de7a715934909893d32c85ec28cc2adce1a0ca612d20738f553f8581031e5958208cf58549b665f2cf1a21e04fb15036baa16efa3d5062c9ad234078b387261597582098caa8dcb61e799d9a079e1c301a2f6c54e9b666820378c9e6a660460aa5d2115820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582030aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205157a89f8e911e9876b814c526ef82612758f5fdb0b48fb114847e02da90e15e58208cf58549b665f2cf1a21e04fb15036baa16efa3d5062c9ad234078b387261597582098caa8dcb61e799d9a079e1c301a2f6c54e9b666820378c9e6a660460aa5d2115820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558202faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58201d050efecc3796fe3435fa27d4b85a34a4079f82d3fbd1b4aff436c1812ecb0d5820cc40251bdcf608d3ad39b832e737f3e7d3b424b68d84a01f9ffaf017b529e2e65820253bbe9faa550cd4ba8595a69c95ea676c977c462c1bbf7c8d82fb18306079115820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558202eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820110db1f93e83f1a51bbc27dbc37c4946d8b0a3f7655b729f2e4e21c013a66ca25820cc40251bdcf608d3ad39b832e737f3e7d3b424b68d84a01f9ffaf017b529e2e65820253bbe9faa550cd4ba8595a69c95ea676c977c462c1bbf7c8d82fb18306079115820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558202daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e9a9548863cb0147ac1e414f4ccb3768d706d3b9d611b04b2a0a5914323d3f6f5820b457129ae670aa9aa27cf9b39dbddd49e7130f38d4f941f42e8e884411f308895820253bbe9faa550cd4ba8595a69c95ea676c977c462c1bbf7c8d82fb18306079115820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558202caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58201da9810919ffb4d1c39b7af037f33d965bae9d561762bdba5658f9e3b30016e05820b457129ae670aa9aa27cf9b39dbddd49e7130f38d4f941f42e8e884411f308895820253bbe9faa550cd4ba8595a69c95ea676c977c462c1bbf7c8d82fb18306079115820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558202baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58209894978b9027e477c59b1d6f2c6707da84fe908170722c8f44f4e4899b84cdd458209ad598e8d65189fe295b14a4315e712c27a09ee26b4bb6e9bd4c8cdf37a11903582096eebf45c93060b647db372a09d6d0c0098e4f902ca4b6eae3cc7a9ff84aab615820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558202aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820f47b906a5b36e4bda0570272c5857a0b7e99becef6e0dadd93efba4da9e68dbc58209ad598e8d65189fe295b14a4315e712c27a09ee26b4bb6e9bd4c8cdf37a11903582096eebf45c93060b647db372a09d6d0c0098e4f902ca4b6eae3cc7a9ff84aab615820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582029aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58202de6fcd34afe982f4bbd30987ef2ce50b9f5f29e2a6b779c604cbca8dd6fad2d582026675d90c3a2e73987738d5329e4a98e22cc30cb0dd31f9155b1dafd57ff01cd582096eebf45c93060b647db372a09d6d0c0098e4f902ca4b6eae3cc7a9ff84aab615820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582028aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205ae95dfe6c055e20df4f9b2df1c74d8e46cddbc1f50f3d2f37d2fa8ed400a059582026675d90c3a2e73987738d5329e4a98e22cc30cb0dd31f9155b1dafd57ff01cd582096eebf45c93060b647db372a09d6d0c0098e4f902ca4b6eae3cc7a9ff84aab615820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582027aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58204b7e3245e1280ff39b4e8dc34976b62dc0c3f601379ac3fb7dd1a561c75e7d845820885ef2ac8682ca90a65323354f88794c6b1c167fa3e8155882efb30466fce7f85820eaa47b95c99d419ec6e8970a2d321fafda691d4afc50953d0763b1f29e7daad75820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582026aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820dd00bf2f687527044bffe4782f713f4f4b858d93a62665dbfae48906c8582d8d5820885ef2ac8682ca90a65323354f88794c6b1c167fa3e8155882efb30466fce7f85820eaa47b95c99d419ec6e8970a2d321fafda691d4afc50953d0763b1f29e7daad75820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582025aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e9ab9a2a35fdd0edf4892ae979a1b62637b079d51ed7ec20162a105c2c47ebe95820043dfc36fb094fdf09c7e182c624ff9c3dc3dfac76d2c3a3b787ded9f9e0745c5820eaa47b95c99d419ec6e8970a2d321fafda691d4afc50953d0763b1f29e7daad75820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582024aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58202fa554c85c6576134a7f58de11741db47d66b613265eace7bbdc0bca8bbd56125820043dfc36fb094fdf09c7e182c624ff9c3dc3dfac76d2c3a3b787ded9f9e0745c5820eaa47b95c99d419ec6e8970a2d321fafda691d4afc50953d0763b1f29e7daad75820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582023aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582064a384c778fb3c54307fd8d449279d7e80c2da819888e38be04fbd1fc88e9b4f5820d2dfd31f6511727dc9237f10781c6aa7b43f2708c3041dec4b32bb140d4638f358209400c93d35bc620c5454317e15c10e95cadf6090a26ce12c661a5d2aa31fdb575820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582022aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582063ac8dfa5dd7b27b143395caf8c34f2f98d478fb5a7a34b5c99a71f202dc96145820d2dfd31f6511727dc9237f10781c6aa7b43f2708c3041dec4b32bb140d4638f358209400c93d35bc620c5454317e15c10e95cadf6090a26ce12c661a5d2aa31fdb575820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582021aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582054e3e6b66396db9b8e76d7e7ed4c1c27d38cd3efb1081571b9b7d7d5862e69535820419dc471fb004af56d4780237dca9c87516773fb2d8a8c1e75db93ea47db278558209400c93d35bc620c5454317e15c10e95cadf6090a26ce12c661a5d2aa31fdb575820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582020aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820b2979c3d8c3cd5fcc1b8bbc2113fbbbd581a9e08961b33240f817f85616027e85820419dc471fb004af56d4780237dca9c87516773fb2d8a8c1e75db93ea47db278558209400c93d35bc620c5454317e15c10e95cadf6090a26ce12c661a5d2aa31fdb575820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558201faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582056c85c5072b042e8d01437409f5669cbdf9bfb91e15a6ca6d0e1beddb0ef2e545820e8ec7e0efe9dc0da2cb49acfbfd2ebe49c1e121cda1a6cc07834b25262414f3f5820d8e8879b6f6b06869c2acad5f581c2ba9bd8175d621a4dcf946237f5753d030b582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558201eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820da44d384b6e7f4140311a567d6d3a90aa82c8bef30c899470a5e57530fd2b3e65820e8ec7e0efe9dc0da2cb49acfbfd2ebe49c1e121cda1a6cc07834b25262414f3f5820d8e8879b6f6b06869c2acad5f581c2ba9bd8175d621a4dcf946237f5753d030b582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558201daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820d6d4abe3c412099787be34942a46245fe7dd17c3ac0e4a305d941bc5c8ede962582018a4673f5eb2ccd8bcd3850f43b279ddd4975fe13bff9bd2fd9f29de3fb606e05820d8e8879b6f6b06869c2acad5f581c2ba9bd8175d621a4dcf946237f5753d030b582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558201caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58200281f8d3082b9facb5c3ad971461c6fd1f7ee88ab3d4c8bdd3a00d041d320041582018a4673f5eb2ccd8bcd3850f43b279ddd4975fe13bff9bd2fd9f29de3fb606e05820d8e8879b6f6b06869c2acad5f581c2ba9bd8175d621a4dcf946237f5753d030b582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558201baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820b9e0bcc11387b06f64773676d75b5df0849f7825ff8dc4030ec4e370e9942b0558208ef971953c493ded4d51bde336f9ce67bf304c6bd7cbd57d69f436cfeafe5618582088028fb1c4ebec6667a624cf95fc540a9a41d0be6c0790fec123dccc8f5fd55f582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558201aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582097448ec2c444688b60409cf46f6605ddd15427f4f1d1bff768ed2b5e5dbd895058208ef971953c493ded4d51bde336f9ce67bf304c6bd7cbd57d69f436cfeafe5618582088028fb1c4ebec6667a624cf95fc540a9a41d0be6c0790fec123dccc8f5fd55f582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582019aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820200a427e73ceec75a5ad05635a8a001df9c64aff492ccf6c7d5a95cd90a00c525820a4d621d142917f3169bd752970ebf640af6cca995ac9c77b91f42ba7b3f23822582088028fb1c4ebec6667a624cf95fc540a9a41d0be6c0790fec123dccc8f5fd55f582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582018aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820bb34a5a02ca0fa70232862dd98806dace6e8db4eb5ee6b221d59eca201b227325820a4d621d142917f3169bd752970ebf640af6cca995ac9c77b91f42ba7b3f23822582088028fb1c4ebec6667a624cf95fc540a9a41d0be6c0790fec123dccc8f5fd55f582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582017aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820b8f73ce3124a9e888466928d8ced28bcb3baefd9030bd42f2f82421a91c2c859582040bac1358526418b784af9e754a32059d4745310feb7868c54260b47a2c61eb15820a90eafbf94596034d4283abc0ac03fcee000ed1ff923506b054dd05a7d20b423582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582016aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582088f8c8b20107062376debf364f6df82be79eeeed1ae98b1be65169592ffe3310582040bac1358526418b784af9e754a32059d4745310feb7868c54260b47a2c61eb15820a90eafbf94596034d4283abc0ac03fcee000ed1ff923506b054dd05a7d20b423582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582015aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820495336a1c19d5627c4ae7901e1fc6cb5b3a3c2d7da3347a29f3d819d0e83c31e5820e95ec57ab63fd9cfdb39a1e778ce5f78fe1d753df37402f3559c558bf5bf723a5820a90eafbf94596034d4283abc0ac03fcee000ed1ff923506b054dd05a7d20b423582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582014aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582002202bc54829f001aa57b9d1d788976871f70901a201c912e3bb3a9353ee0fd25820e95ec57ab63fd9cfdb39a1e778ce5f78fe1d753df37402f3559c558bf5bf723a5820a90eafbf94596034d4283abc0ac03fcee000ed1ff923506b054dd05a7d20b423582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582013aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58201d661f2c950abcd5adf5128ab700c8a10a4cea7e3e4227f3726a3310ca67b5c358205a0f41e8b24cb6e0a739b3d5384b45d61f52b077c9bebe27c00f73e564196124582061159164439e788ad8b9efb72f520f1bc5626b540401350dd996f614aff423a8582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582012aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820b9fa08cc6e9d6b887365d1623775c0153f2d1367ef66c22a4ed1b66ac2c0264158205a0f41e8b24cb6e0a739b3d5384b45d61f52b077c9bebe27c00f73e564196124582061159164439e788ad8b9efb72f520f1bc5626b540401350dd996f614aff423a8582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582011aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582085adef99dfae1212ce9e13fdc3718b17e73133cb4770c1978d1e99c819376c47582016d45c2e27611fc92ab2ff06a2557184459d39f9cec25ea61b13212a1036081c582061159164439e788ad8b9efb72f520f1bc5626b540401350dd996f614aff423a8582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582010aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a0a1f1fd460f5c36b0d5522484584f23a7268080ff943cbc3fc3a6fd300fbdcb582016d45c2e27611fc92ab2ff06a2557184459d39f9cec25ea61b13212a1036081c582061159164439e788ad8b9efb72f520f1bc5626b540401350dd996f614aff423a8582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558200faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582005fcf47431f9e54346c393d710ce784e15368a85838c80841d4df39b078e60145820abe0454cbbfbfb9e5c28f1ba6feaa59f165db4ae844c4bc5231e36730b473d5958203a1418de5a8d9101158b6428080e7571b823c7e2f6dc1012d9a86a2aec34da185820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558200eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e8c492fb587cb0c63d8fda04007d86e557774a31d226b933b48d0763c58bccb85820abe0454cbbfbfb9e5c28f1ba6feaa59f165db4ae844c4bc5231e36730b473d5958203a1418de5a8d9101158b6428080e7571b823c7e2f6dc1012d9a86a2aec34da185820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558200daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820b474cb6782b9fa5156b8672ffa4c3b9544af67f3ac297260977163cd759782195820d0e8bbfd69c744fbe1195b8d97957a2966545de6341d1beb82d125ac0bc8ff5658203a1418de5a8d9101158b6428080e7571b823c7e2f6dc1012d9a86a2aec34da185820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558200caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820477c388f19a6b9e353f9d9fa9ab5b1df610f6e5e81e2815a1a9817db3d76f6105820d0e8bbfd69c744fbe1195b8d97957a2966545de6341d1beb82d125ac0bc8ff5658203a1418de5a8d9101158b6428080e7571b823c7e2f6dc1012d9a86a2aec34da185820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558200baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582002cba8412003b4db80b1918c4460648674e3221977a42a4729e4f3f6a194b79b5820c9df1f625af5118d082020004128f29ea59abd7072377d1295204e1f9c0263c55820788eca5bb2626c475f6c72b83376c23917f6df616f55dd975d725eabeb7c8ddf5820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558200aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820347afe5763e85ab2387fd0590573fd93e5f64834a4ad8d16d997722f3e8af4d55820c9df1f625af5118d082020004128f29ea59abd7072377d1295204e1f9c0263c55820788eca5bb2626c475f6c72b83376c23917f6df616f55dd975d725eabeb7c8ddf5820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582009aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58206177c164493a0224a115cae3d1079b8e9c7fb617ce02e53d2b659471847a90ed58204f6416aab910d504ce300253f1473b13e37271a377d275a94bc853c4a293bbb15820788eca5bb2626c475f6c72b83376c23917f6df616f55dd975d725eabeb7c8ddf5820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582008aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582068113bd609b97fef93cd9971fd2b03f8e0bc0639b43803e04753cb8c61a6659458204f6416aab910d504ce300253f1473b13e37271a377d275a94bc853c4a293bbb15820788eca5bb2626c475f6c72b83376c23917f6df616f55dd975d725eabeb7c8ddf5820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582007aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58206a4f1f1d86a0831cbe484590350b7d6ca9da63b615f1662120ad07c1b96d7b6b5820acb35704a2237fef99e5629a22298b76dd249aeb768d84c7a4f293fbf9a090ff5820bb4b232688e4221eb55ac081ab9afae8afdcb78f650d43e2b6f1bb35ff18ee5358204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582006aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58206e1f79f70fa198d18c9cffa92f58ca2fd72392f8a077679155154c6a0f7fc99f5820acb35704a2237fef99e5629a22298b76dd249aeb768d84c7a4f293fbf9a090ff5820bb4b232688e4221eb55ac081ab9afae8afdcb78f650d43e2b6f1bb35ff18ee5358204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582005aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820266603acb42e667ac5f08f1d1fca1b548c65869e9af637665f49fe5e7fe686485820709ab97898eed844206279c8f267ffaa13ed65c828363a44317de7ed638a54e35820bb4b232688e4221eb55ac081ab9afae8afdcb78f650d43e2b6f1bb35ff18ee5358204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582004aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58209671dba386de4806723a00ce544f277f443c01e0c0dba6a279bfd0d5103b1d3b5820709ab97898eed844206279c8f267ffaa13ed65c828363a44317de7ed638a54e35820bb4b232688e4221eb55ac081ab9afae8afdcb78f650d43e2b6f1bb35ff18ee5358204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582003aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820db94dca20f9591537af643f2b22930f332d5d757e43661bee4e91cf3d495807a58202bf5666f69d0cb39a110a35ad13e046e55bbab075691c96102c4dfce0150a741582019b70478585f0bf117d7b95c766699f14639d62477053a656fbe72fdd308054258204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582002aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58204298c16ee439c9f5c16e96cfb55bcc0de02e3b227123d018b9ea449ca62f1ff358202bf5666f69d0cb39a110a35ad13e046e55bbab075691c96102c4dfce0150a741582019b70478585f0bf117d7b95c766699f14639d62477053a656fbe72fdd308054258204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582001aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582024383e249731b2ea1acf26f669c461d8f14a4158d17e2e44fbecbffc465543a358209545e9c75d21c526687d4aaf5e11cac6562a94584610c6a65110be78de6734a6582019b70478585f0bf117d7b95c766699f14639d62477053a656fbe72fdd308054258204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820025a790431a864be2f6d7285146f6a7916a9735180124f90ca3258c8175dbeb158209545e9c75d21c526687d4aaf5e11cac6562a94584610c6a65110be78de6734a6582019b70478585f0bf117d7b95c766699f14639d62477053a656fbe72fdd308054258204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87980d87980d87980d87e9f0120ffd905029fd87a80ffd905029fd87980ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980ff"

const long_asset_names_value_at_the_cap_terminal_hash =
  #"700afe4940b690af37697705f73738022cfa5c471da15a63f5f0da18ac87c18e"

const long_asset_names_value_at_the_cap_planner_claims =
  #"9f9f190128d8799f1913ac010040d8799f01001913ac0100d87a80ffffff9f190129d8799f1913ac010040d8799f01011913ac0105d8799fd8799f01001913ac01d8799f000080ffd8799fd8799f01000017584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f190138d8799f1913ac010040d8799f01011913ac0105d8799fd8799f01011913ac01d8799f01019fd8799f0058205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df301ffffffd87a80ffffffffff9f190139d8799f1913ac010040d8799f01021913ac0105d8799fd8799f01011913ac01d8799f01019fd8799f0058205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df301ffffffd87a80ffffffffffff"

test value_of_5000_bytes_of_long_asset_names_replays_the_typescript_trace() {
  replays_a_long_asset_name_value(
    long_asset_names_value_at_the_cap_output,
    5000,
    long_asset_names_value_at_the_cap_witnesses,
    long_asset_names_value_at_the_cap_terminal_hash,
    long_asset_names_value_at_the_cap_planner_claims,
  )
}

const long_asset_names_value_under_the_cap_output =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a1581c55555555555555555555555555555555555555555555555555555555b88e582000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582001aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582002aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582003aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582004aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582005aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582006aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582007aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582008aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582009aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582010aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582011aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582012aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582013aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582014aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582015aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582016aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582017aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582018aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582019aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582020aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582021aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582022aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582023aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582024aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582025aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582026aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582027aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582028aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582029aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582030aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582031aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582032aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582033aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582034aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582035aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582036aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582037aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582038aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582039aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582040aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582041aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582042aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582043aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582044aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582045aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582046aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582047aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582048aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582049aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582050aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582051aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582052aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582053aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582054aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582055aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582056aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582057aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582058aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582059aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582060aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582061aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582062aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582063aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582064aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582065aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582066aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582067aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582068aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582069aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582070aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582071aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582072aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582073aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582074aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582075aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582076aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582077aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582078aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582079aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582080aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582081aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582082aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582083aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582084aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582085aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582086aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582087aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582088aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582089aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158208aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158208baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158208caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0157ffbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb17024100"

const long_asset_names_value_under_the_cap_witnesses =
  #"9fd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0001ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87a9f0120ffd87980d87980d87b9f581c5555555555555555555555555555555555555555555555555555555557ffbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb179f582045c9d398507488094b23e2995a5b0c9da614df723c4f944e97c1865257af1bbeffffd87b9f581c5555555555555555555555555555555555555555555555555555555558208caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820fa0273439dd39449e40aa99eaa7e61b8536ad2da97d4f89e8b0c3eb50669cf81ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558208baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582024608065c83c2e3fe216befccba34be1a2d2bee691130a39fa83521e1685d37858203b4463fa4d79bcc7c86caa1fee8d3c8ecae4d40e944d0b33014ee95962e8ed75ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558208aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820fd31d707dddad5767e124677328eece3f9996a493fc37ea86e4e664e9c52d09158203b4463fa4d79bcc7c86caa1fee8d3c8ecae4d40e944d0b33014ee95962e8ed75ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582089aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582074c55a198b7b852d55a85314f643a109f870089f447b50314074313b6ffd1e755820a1624296e82ae340553fab2ae8a4bc6079df49efb05df9de7912aa9994f5d104ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582088aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820fedee166ca73d1639b0177286b52c24dbad2e9afa922e94ebd74130b1a3ad3385820a1624296e82ae340553fab2ae8a4bc6079df49efb05df9de7912aa9994f5d104ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582087aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582065f3ce2b70524be40809fcaa4144ac1d7acef64d1eb3fe4477ff136172b1241158208d281d2b430d114e07907b578ad20498dfa568c785eb7f5cc2c0f990fe09bdd7582072b1c1209c32f6704a10e72e8c502dd048337f762dc0b4892fbdec0ba6ade987ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582086aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582006b475cbad8b099ceb2dc2309482794aa7328ef78467fd5a2381d54631886c1a58208d281d2b430d114e07907b578ad20498dfa568c785eb7f5cc2c0f990fe09bdd7582072b1c1209c32f6704a10e72e8c502dd048337f762dc0b4892fbdec0ba6ade987ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582085aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820c64b816ed5aa1c6ec789cd0bcf555fdb057e8b5bd6d1f14aefbba585019c267a58202482c74fc58ab3e2d609f2892d729fbbf2cf04dfbd389ff60f4b8b12b01476e3582072b1c1209c32f6704a10e72e8c502dd048337f762dc0b4892fbdec0ba6ade987ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582084aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e1c1ca480488f84d4a04fa2b9d92e4176fb1ccf6a5bf635aced82628cd264a2358202482c74fc58ab3e2d609f2892d729fbbf2cf04dfbd389ff60f4b8b12b01476e3582072b1c1209c32f6704a10e72e8c502dd048337f762dc0b4892fbdec0ba6ade987ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582083aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582011ba7c21bfc0e1a488fe6b4e492efc67a11270ccddeca53ded16dcab7c3e140658209114ffb322c56e49ed0c365b11161e3ed7269db4bd9f8312d2bbe93769d6031758203b0175e1097be5b72df566388a8c4103f2677ebf9549d77708933c8ad64fa452ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582082aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582007e1b364060f0b01a79411e503b40ef65748c9d22ad4b0b4cbc03bdd68a23c5458209114ffb322c56e49ed0c365b11161e3ed7269db4bd9f8312d2bbe93769d6031758203b0175e1097be5b72df566388a8c4103f2677ebf9549d77708933c8ad64fa452ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582081aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582010087333e8c295990e5a85326d4fe7be025cd5821240d17437c7f39b3eb8736258209fcbd267588e8bc8faa68d9ae0c4ade56b481a24f357d00bdb246ba28bb6927a58203b0175e1097be5b72df566388a8c4103f2677ebf9549d77708933c8ad64fa452ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582080aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a68a8f2296b347e77010e6850c2c54779d0892143d9d5fbcc27251d2c27f707558209fcbd267588e8bc8faa68d9ae0c4ade56b481a24f357d00bdb246ba28bb6927a58203b0175e1097be5b72df566388a8c4103f2677ebf9549d77708933c8ad64fa452ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558207faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a133aa2cd144918d09a42ae053f4eb7c6f4125ea1e1cb4540bb52d500f9def025820cb08c87b28ec0cb64c3a648dc999bc8b1136f036e650e95001ccb1984ae042245820403533e61167f9f710baaa7621aa8d9f6cab4d8deb5a175eab5be0d04eab6a1e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558207eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58208486b579ba806d1129956cbe14ab7c5977df3c49888de44ebdf44c02cff0125f5820cb08c87b28ec0cb64c3a648dc999bc8b1136f036e650e95001ccb1984ae042245820403533e61167f9f710baaa7621aa8d9f6cab4d8deb5a175eab5be0d04eab6a1e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558207daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e2b4b12e1078ac2c6e079029ebfd0c3103a4a5336a4015400e2f271d9db97cc658208fa1bedc772e6174e9020b68612714ef9dfc3b0a17e5b45903f1900f7c1d5d455820403533e61167f9f710baaa7621aa8d9f6cab4d8deb5a175eab5be0d04eab6a1e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558207caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58201a7601b486ff3bcb253587a5d22b884b88f8c4716c62f177691318881596323258208fa1bedc772e6174e9020b68612714ef9dfc3b0a17e5b45903f1900f7c1d5d455820403533e61167f9f710baaa7621aa8d9f6cab4d8deb5a175eab5be0d04eab6a1e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558207baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820713cf97e55a285590bc09c1c4f766833e60493e4b4e70aebaec088bb3ad7a0c758203d2c7898e0e9295f316b6b6cd2a5deec4e8b87b1e87db4ff43e4b838e2511c115820cc77ab4130d33860bb1584bce5b5ce5103427bd6124e3181022f50d54fe6356e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558207aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a19108f0a189783e602c7b1e15c493dae98698cff76b08309b0f2977ad996a2758203d2c7898e0e9295f316b6b6cd2a5deec4e8b87b1e87db4ff43e4b838e2511c115820cc77ab4130d33860bb1584bce5b5ce5103427bd6124e3181022f50d54fe6356e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582079aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820413da02d299694732a040081e5c3080fe8a0a17170af41cf75ad76b7c3546e555820ac8adf9366203ff12787118bb8ab0b4051a9c027bcf758d2cc4eb3e2271194585820cc77ab4130d33860bb1584bce5b5ce5103427bd6124e3181022f50d54fe6356e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582078aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58202d2da448141c5dd5db0b682fd93d5f8d65076bdb6e389b91c97835eaebc8a17a5820ac8adf9366203ff12787118bb8ab0b4051a9c027bcf758d2cc4eb3e2271194585820cc77ab4130d33860bb1584bce5b5ce5103427bd6124e3181022f50d54fe6356e58201fcc58b32e639dcc82de095cf2aa23bf5cfa177dd30c737e5e288cc0f3a4ed6658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582077aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58207cdf5e07392dc8ae8320171e7c30c79cd8fd77ae4aa6f52fdf07b62b8700e2e95820a88acf1e35ae61de0742470e870712d92d46e423dc5dbce32f3200b9667a4b13582047350b988f37e4ab80baff721a3015973dfc0226b596c8c83a2f40632dfc19db58208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582076aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820ecd6397275daa88bbae0f93b3efd3ca17df2092e18524f4371c96a77c0d69c355820a88acf1e35ae61de0742470e870712d92d46e423dc5dbce32f3200b9667a4b13582047350b988f37e4ab80baff721a3015973dfc0226b596c8c83a2f40632dfc19db58208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582075aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820dafdbcdf16ae62ffe630a16433186b5035857ceae4713de38d61e5c4acc65b1e58200019e04717dc9f703df6cd72ba8478fe1d3576c648a5ea0969867b370cfbf9c0582047350b988f37e4ab80baff721a3015973dfc0226b596c8c83a2f40632dfc19db58208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582074aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58208adc8ac8893508d48bd3690ef2d32d605c5c79da15b585476dbf0a90aab21c7358200019e04717dc9f703df6cd72ba8478fe1d3576c648a5ea0969867b370cfbf9c0582047350b988f37e4ab80baff721a3015973dfc0226b596c8c83a2f40632dfc19db58208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582073aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820edfce037574f30e11e9c14c6ba92c83aa0c343f69c7d492fa019fa52f5ba205c58203edd4bf8266271d12eb16017fb95f27a6d37f3d3e1f5d11a6172a2ef6a3e4dc65820fd7f135bf9418e191f5d5b248277049a177278d673fddcd8af7ed3a1061cdf1058208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582072aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58200c479590273ea6f83641c8fd2c64029b3aff7599beb2de33ccc94f33f0bc8c3558203edd4bf8266271d12eb16017fb95f27a6d37f3d3e1f5d11a6172a2ef6a3e4dc65820fd7f135bf9418e191f5d5b248277049a177278d673fddcd8af7ed3a1061cdf1058208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582071aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58202dbc19ab36466cb645b45852a1e61a954a579fbccd748dce7d9bf9190b85b6375820ed094b409f5aa2383b75c6d851b4ea85dff467e799ad3910c66a7a72a84706615820fd7f135bf9418e191f5d5b248277049a177278d673fddcd8af7ed3a1061cdf1058208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582070aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820cc01e10b48a7e83ea260540f3ba582ff7a6b0216346a6e3bf076da394612ae7c5820ed094b409f5aa2383b75c6d851b4ea85dff467e799ad3910c66a7a72a84706615820fd7f135bf9418e191f5d5b248277049a177278d673fddcd8af7ed3a1061cdf1058208b9e7fc11999b5aab27495c315b9b640d3fc65a0e363f9fb162eed8f28a7266658206d931f2c0a3f09ef2e718a88dea20e8510a5196b5f90912efccab879c61a49e458203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558206faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820f281727381c91dfbe61a027b6e2ed6a432c073b4129d726364b48f1b23dd542c582046e0cfcbfb74f39c4a7c163621072044801c85a6071617dc84f93417b437b08b58205e994bd8fc2f461ce50489966c089ae16f6b59d7df5724cb8a65a30e616d4ef65820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558206eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58200958edc45ba5135597b4a685cc831e0a5bb134e222441852c0cff0bc2aff927a582046e0cfcbfb74f39c4a7c163621072044801c85a6071617dc84f93417b437b08b58205e994bd8fc2f461ce50489966c089ae16f6b59d7df5724cb8a65a30e616d4ef65820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558206daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205231f0d99343cde017a19f3d372037db4d3343456e96de0e0c79fdcbc0b82c3558205eb3bd42e9fc62980fa7596deb11e8c9ac4d7987d4522d1d78583d21cef28ca558205e994bd8fc2f461ce50489966c089ae16f6b59d7df5724cb8a65a30e616d4ef65820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558206caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820f4b4722ad93bc02b4c764e4e7e91c909fc6ea81b3605405733e6a032d24bd94958205eb3bd42e9fc62980fa7596deb11e8c9ac4d7987d4522d1d78583d21cef28ca558205e994bd8fc2f461ce50489966c089ae16f6b59d7df5724cb8a65a30e616d4ef65820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558206baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820113d5d77d52484d7ce470304b84c9c61a75fd6cf3ab7a70f33569b742af89e8258209781fe3ecbfe06021b3c01050dfe250bc1d25a931600d4e560b5e172d1fe44b5582056cd5a2954619b08a955ce51b90cfa1c04858ba483595215abea502238a5819a5820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558206aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205e92462cfc8017d7ca452038b927244ba4c90b67dac1d354eb144b16a546e3bb58209781fe3ecbfe06021b3c01050dfe250bc1d25a931600d4e560b5e172d1fe44b5582056cd5a2954619b08a955ce51b90cfa1c04858ba483595215abea502238a5819a5820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582069aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820bd2f1dca9811b2ad7ce95979b4e4daddaa3ac866080d49d6a486ce4419104f8d582029b930d690c5a1340213250df7c70c7ae559b748638331b26a01a4a016b5a004582056cd5a2954619b08a955ce51b90cfa1c04858ba483595215abea502238a5819a5820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582068aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820bffa2b2d27be9a6490d5c70f1520935f9336bec8f61cd51b8d39ba0e1b9ea713582029b930d690c5a1340213250df7c70c7ae559b748638331b26a01a4a016b5a004582056cd5a2954619b08a955ce51b90cfa1c04858ba483595215abea502238a5819a5820dabaf3baaba93f672616ccc843f33708e75b867633061b65034e3b0fbb8e44a858204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582067aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820ea57435a48cb40c618b3d8989ad77dce17603f9a25b652295693b23fee68984b5820e1c805c558c97a175d457ffea70f27aa1fac80392c5d1418afc2ced3ead99402582088b42804e05b3a97d6fd6528adda751703319516aac2daa3173f77f994287c345820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582066aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58201c340bb2b7db7936aef4b183ea228734fdbac3a014bdfbbc5611ffce5de406f25820e1c805c558c97a175d457ffea70f27aa1fac80392c5d1418afc2ced3ead99402582088b42804e05b3a97d6fd6528adda751703319516aac2daa3173f77f994287c345820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582065aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820f790f9e8df4133d965b0a5f3ef809ad4bdd81d5b4b6fafe2ec5fd0ec4b5ff74c5820fed54416d5027ca0ab065e8699d735850dc4450b0280466394a30063cba54674582088b42804e05b3a97d6fd6528adda751703319516aac2daa3173f77f994287c345820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582064aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e420e3b8849c64de525d75c8f8167124ed1097bcdd03547fdc2f6a1ee7ec0a175820fed54416d5027ca0ab065e8699d735850dc4450b0280466394a30063cba54674582088b42804e05b3a97d6fd6528adda751703319516aac2daa3173f77f994287c345820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582063aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58208f5962b204e82d53119ece79b892fce4f14a35a58dca22677cf0e1652c9c2e0f5820354ea3c0173971d53b1e8ae6b0a66c629e7487be2c7d051302857bda2b7a5fd95820918b262cab97e3b7700f93977916eabbc2a92a3bb4f613f5ea9f9cc2fcbc30a95820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582062aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58206f1432f9a7872e8a88da62bc901b9c30028a819c26af2e15cc3d99a8a2174fc95820354ea3c0173971d53b1e8ae6b0a66c629e7487be2c7d051302857bda2b7a5fd95820918b262cab97e3b7700f93977916eabbc2a92a3bb4f613f5ea9f9cc2fcbc30a95820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582061aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820070b5fbf6cab8aa0f0b961862d1c84c65ebf78b067816aaadb6440206bfeeff65820e3ec6325b08dd81d2bb2b9bcf31e43afc9f06a855c12d917c5e2ac3bb31a24945820918b262cab97e3b7700f93977916eabbc2a92a3bb4f613f5ea9f9cc2fcbc30a95820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582060aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582085afd37261d978c6a289e1d2378597cbaab50e1f09c928af5a79bebbea5d602d5820e3ec6325b08dd81d2bb2b9bcf31e43afc9f06a855c12d917c5e2ac3bb31a24945820918b262cab97e3b7700f93977916eabbc2a92a3bb4f613f5ea9f9cc2fcbc30a95820ec3a4180d174b406f7bdd6d00dbeea76010668e1484ee0b4464bdb8c39ed615658204da366bc46c14f24a534af3c59110aee6e5b04c79680d625c019f453353dbd2258203834e80feec42b578a43dc88f797c91eee606fc4a2c8c3c86c1fb00e1defd28c582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558205faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820562d64f1ffbc176fa6e0c87d776d886d8fac8b0d1a6503f305775679e3109ef85820bbc2f2d207fd342a62e02e8f1ee93a4d95abd2fce85fe43acc54b34195790f38582086ce118745fb168ee3749aef4c33b4bbf2322547e569684943a91abd8f816c0b5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558205eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e5316ec9a8f2cb40e1e831226e3791d3c6380bb5c66ef13d91ad91063cfa75185820bbc2f2d207fd342a62e02e8f1ee93a4d95abd2fce85fe43acc54b34195790f38582086ce118745fb168ee3749aef4c33b4bbf2322547e569684943a91abd8f816c0b5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558205daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58207bebd2df9a0c5b20fcf780e705ed0c28fd3e170af544276dd28bc70d58d5d3175820ac98d5f145b6991e03d524adcd04e6cc2fbf6afcfddee7700f7123ad6d675479582086ce118745fb168ee3749aef4c33b4bbf2322547e569684943a91abd8f816c0b5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558205caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205b1657d1b47a04d26e0c68e1114869b130fcfd10fe570c2a357ad626f2ac19a95820ac98d5f145b6991e03d524adcd04e6cc2fbf6afcfddee7700f7123ad6d675479582086ce118745fb168ee3749aef4c33b4bbf2322547e569684943a91abd8f816c0b5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558205baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820883af77f7fa8cc2bc9283e3a798221e157ed580f65c5481da7e041ef58532285582098497cb052bd80238d0d60da4b4334dd7dca9af054a533d5cbace126b60a335658209fb12becef76b45025e8e9840ae12946c39f40058fd444bff9ac5e345121653c5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558205aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58207116ca68394550ce1155263ad9c09eb15a4316c93ba6328a256179496eacd387582098497cb052bd80238d0d60da4b4334dd7dca9af054a533d5cbace126b60a335658209fb12becef76b45025e8e9840ae12946c39f40058fd444bff9ac5e345121653c5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582059aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820ca080c5cbf959dfefe9eb3375880f80b2f45440c8e59041e0850f30cdd8b67e358208ab8e929189ff4135a171b0ca6f72a47cb56739dc95884e9f2ec6dd417e8200c58209fb12becef76b45025e8e9840ae12946c39f40058fd444bff9ac5e345121653c5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582058aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582064c044650e9d42a8b2b6b5c62c4a45c9e290185f80d7cf5ff309ae6ff0de5ae058208ab8e929189ff4135a171b0ca6f72a47cb56739dc95884e9f2ec6dd417e8200c58209fb12becef76b45025e8e9840ae12946c39f40058fd444bff9ac5e345121653c5820afc59b9f2ec9c91a5b5c32977adbb419f21a18066b174c3e1fbef413d7abf4b5582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582057aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582057ef9c9fe898da3e4e593279a663986391bbe434cb2cfbf062dcaf7a4d016d1f5820412181d2c6c4e5a2131b5816c2097c0722b34676777eb5775707429d5fed07cd5820584174d84c3cc2f23da99976d8a9491d42a7f07f7eabe926a819039307cf217658201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582056aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582029a8804ab2ebd7e9fec2db1c10955b2d1e684a85ae43e5afb887488f9736fbac5820412181d2c6c4e5a2131b5816c2097c0722b34676777eb5775707429d5fed07cd5820584174d84c3cc2f23da99976d8a9491d42a7f07f7eabe926a819039307cf217658201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582055aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820d480fdf03a23a629830775813fb858cc8f6f0fc26ceb7acdf5bd7dad9b3407b25820e4a86b38732a723797f8b71b0eeb0b3053a314192c0435137b46fa3d7ceb6bb05820584174d84c3cc2f23da99976d8a9491d42a7f07f7eabe926a819039307cf217658201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582054aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58208b5687fc273064291829bb22a9b0c692af27237f89f1da9cb3dafb9c54aceac65820e4a86b38732a723797f8b71b0eeb0b3053a314192c0435137b46fa3d7ceb6bb05820584174d84c3cc2f23da99976d8a9491d42a7f07f7eabe926a819039307cf217658201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582053aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58207258a40c547529f431b3250b663fa3845dc07f4836bbc068272da6e0293c38c75820a5faef649594c5442e1263557050068674185385bb10a88aa842676980f20fce5820ba0aea8528d40fa58d5c4c86036c3f1456d00119fac0a59b808e76dee6c6753f58201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582052aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58203dba64778abe4645f0126702dde69afd7db40b6c3f7a159ea59ce3101caa9eb25820a5faef649594c5442e1263557050068674185385bb10a88aa842676980f20fce5820ba0aea8528d40fa58d5c4c86036c3f1456d00119fac0a59b808e76dee6c6753f58201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582051aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582049acd8ef97a3d59ed778ee9ec370be3a00bd6c47d392f7d5bc64ad1098733a8758208ed98f84898986c2fb0fe446b14f2637433126ae272e406f1543aeab7a2f9c135820ba0aea8528d40fa58d5c4c86036c3f1456d00119fac0a59b808e76dee6c6753f58201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582050aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a379d0dcb3acb4072b1fe5a274ee8778855107245f925e11f3f91b64f50f9a4058208ed98f84898986c2fb0fe446b14f2637433126ae272e406f1543aeab7a2f9c135820ba0aea8528d40fa58d5c4c86036c3f1456d00119fac0a59b808e76dee6c6753f58201149f9fcaa1bd1b66b0c03efb7ce92379bcd4b8c56285c757a38458b0958c1ce582065c2ce668e727332fdc6aeb8994b162efd7ffd134c08913ed110b2d1f533e8ff58204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558204faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58203aa2604a38ae954692e51af8fc3574a17554a61c69465b0225d31f2e6a7d47755820f02d3c3cdf816d3e4827506b9515460e4a731f27ee9a03a684a309fba026b2ba5820c016a5f0c447d2454a8a7da31bede646bd58824d48f66fa37ca92779b016cc2758200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558204eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58206e39d854a200cf00085643d8eeae0315969c11c06571815089f3f9628a7e755d5820f02d3c3cdf816d3e4827506b9515460e4a731f27ee9a03a684a309fba026b2ba5820c016a5f0c447d2454a8a7da31bede646bd58824d48f66fa37ca92779b016cc2758200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558204daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820cb55d4100ba46e0097b8d777c34ff367f3be1691e777379328a13632cc3726fd5820e194dd68483a5f1f17b4558096b5789de24370ffc154ebfacbf2f4c3d0ed8b705820c016a5f0c447d2454a8a7da31bede646bd58824d48f66fa37ca92779b016cc2758200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558204caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820abc0f38fb42a5ffef8704db0c3e2cca3038ed781fe23b5fed8e40b25c8ab7de85820e194dd68483a5f1f17b4558096b5789de24370ffc154ebfacbf2f4c3d0ed8b705820c016a5f0c447d2454a8a7da31bede646bd58824d48f66fa37ca92779b016cc2758200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558204baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205391eb1dad7321f31d71f14728e4ee434116b2b3e38e9e10083572bea91f9855582096a41cc73e21163986fd83dfb87352f62d07c50f89e89134dca0bfceb5d04ebb582092dbb02333e59bcf1817218e561f2e014aebc770892fb0175af7951f6cab62ac58200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558204aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a77f0f92ac04bb421440c267878eb9390b484bc84c17572b8eb093de774868f3582096a41cc73e21163986fd83dfb87352f62d07c50f89e89134dca0bfceb5d04ebb582092dbb02333e59bcf1817218e561f2e014aebc770892fb0175af7951f6cab62ac58200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582049aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582011d1f6870e11bb5a568f18c7cf5c3c412abe7b53d9b2c7dbb612d6e71d12d69e5820a1c8b797db99e355d63f37ad756dd8a7c2e2a807173ae9b09cdc7e424bc19f30582092dbb02333e59bcf1817218e561f2e014aebc770892fb0175af7951f6cab62ac58200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582048aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820682df0f568e0c6376147850dbe182e16ad741aaa32f180a30ff9efc806d3de835820a1c8b797db99e355d63f37ad756dd8a7c2e2a807173ae9b09cdc7e424bc19f30582092dbb02333e59bcf1817218e561f2e014aebc770892fb0175af7951f6cab62ac58200661eb7a860f008f9ce2113e1a7ccf3cee8ace7f1b8fe7f1f5d6fb57d4f46af55820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582047aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582048d5355ee71b3fdbaf4b2398228e0eadfab85c52b4be49220765d884a6bac2155820d7f0bcb4148119ddb3fefbcd9f5166bfec60fa35dcb4fcdb0a5da9ae93116b9258203cec65125954c81479a1698dd925e9d0de2e03d239521b4334aaf7c45ae6f4925820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582046aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820fd9e0fa49e99bd16fe832893be619e4e0900faa72332e95d98b07332c67de5e25820d7f0bcb4148119ddb3fefbcd9f5166bfec60fa35dcb4fcdb0a5da9ae93116b9258203cec65125954c81479a1698dd925e9d0de2e03d239521b4334aaf7c45ae6f4925820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582045aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820ee91dca42abfd53edad99f56b7d28f980875d04fc301b936b46bfb461b12dd5458204c0bc7de7f8fbdb6fb843c660a763b47b0448b7fd51c459148963df85e05f2ab58203cec65125954c81479a1698dd925e9d0de2e03d239521b4334aaf7c45ae6f4925820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582044aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205496d0fcef2327deba20afa76857d7a54ee840365d8bd4ad9c0f4d0f1a995e3958204c0bc7de7f8fbdb6fb843c660a763b47b0448b7fd51c459148963df85e05f2ab58203cec65125954c81479a1698dd925e9d0de2e03d239521b4334aaf7c45ae6f4925820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582043aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58201c556b47d66f9c91ecc5136b5c5c26f7058b1de40bda02cd994de652999df6305820f9023ebd9d123ed68ff644fd8b3cc105b87eac9297aef9c097ba1ce6c8056e4058205901d771069dc05eefefaf46ea7c028fadadeb484f8ac87c39d8388372a54e445820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582042aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820fec9906a376b2d55cecc757a4db348597e7a8c927e003a7b62a286f3185f9bb15820f9023ebd9d123ed68ff644fd8b3cc105b87eac9297aef9c097ba1ce6c8056e4058205901d771069dc05eefefaf46ea7c028fadadeb484f8ac87c39d8388372a54e445820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582041aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58203db0d9e1d0355725faa68f7f776f499221a31a4b9a10250154f42456b4a27efc58200e0bf82bbf1116cc8db013888fd47e972c8914d964f68afa0819b565fefdc24c58205901d771069dc05eefefaf46ea7c028fadadeb484f8ac87c39d8388372a54e445820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582040aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820546cb234a0ed7d8f669b938f6fa7759727e5bc088caefa29b4211abc5ed29cb258200e0bf82bbf1116cc8db013888fd47e972c8914d964f68afa0819b565fefdc24c58205901d771069dc05eefefaf46ea7c028fadadeb484f8ac87c39d8388372a54e445820330122b4d43dd68a7f7b2f322d6508c8afaa2a5f3fe1a7bce93f2519753a60ef5820cc690c9ba2fc369b01fe00a44aee49f81afbb419f9c0cee756da2513366c4c3658204fd665e045a972be8be3384b5c7b22d874609b18fbb2ba4cbb48fa2a80d934b2582049896b0b979dd22c2650f66b658418a70da96119e023e3da4c62b2791eaf9300ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558203faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582016b5bfe248f4a5b7d7b6e1d83cedd7475d4082fa9682d52f60b44718d5a213c35820335d454d8ffe3bcd09060eb39beeb566613bbf55f7dc8481f49419d486590c2f58209b9c096d1a9166ad7c41decfc11738ad621c0b6b76bf9cb8c0e5ed020c9386b958207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558203eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582073ca2f3463515ae5e5f37f8688320d2d2a7c5f08143ee430616e4400b61d87af5820335d454d8ffe3bcd09060eb39beeb566613bbf55f7dc8481f49419d486590c2f58209b9c096d1a9166ad7c41decfc11738ad621c0b6b76bf9cb8c0e5ed020c9386b958207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558203daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58204a8cb9e4d508b704a6821720a27ce8e29700832a0fbaa5835f1b6fb1123d498f58204e41c0f986f40428fa2e8f543817e2c860f8665740cbd9f2432698670755ccf858209b9c096d1a9166ad7c41decfc11738ad621c0b6b76bf9cb8c0e5ed020c9386b958207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558203caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582066092cbefce1e436587617e3721a3dadcb982358397c6fde0d3634848789ae0058204e41c0f986f40428fa2e8f543817e2c860f8665740cbd9f2432698670755ccf858209b9c096d1a9166ad7c41decfc11738ad621c0b6b76bf9cb8c0e5ed020c9386b958207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558203baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582018d9b4ae92e7223e97c1d3e13a7d0e3ec7b123778d93e22f060780c0f9bd993d5820f27265cd8fea583071b49cd5e0786a395a33b02d22bdb4f421d5b451b377f9925820392d366d5a4c3072b68879cca22f3bedf95c6ff33d3c8b944783f49b967fd8c358207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558203aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58209314a00fda28ac31e9f77708dc0ed954fe5bd452ac8510b88a5b09d115dd3a775820f27265cd8fea583071b49cd5e0786a395a33b02d22bdb4f421d5b451b377f9925820392d366d5a4c3072b68879cca22f3bedf95c6ff33d3c8b944783f49b967fd8c358207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582039aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582061f601a13ef2d962f773950318c479f266975b1d5aefa5fc18eca1a31f6793cf582049c49e310c12d730b4ef9486c5d3f83bf67e85a4ddda6c982fc75a44f36e69ad5820392d366d5a4c3072b68879cca22f3bedf95c6ff33d3c8b944783f49b967fd8c358207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582038aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820c4277f6eb73f2a7ac2dc354dbb9b0766fd6551da9765360bb3cfd2ca75b79ec2582049c49e310c12d730b4ef9486c5d3f83bf67e85a4ddda6c982fc75a44f36e69ad5820392d366d5a4c3072b68879cca22f3bedf95c6ff33d3c8b944783f49b967fd8c358207256548380851812d32c3a32107d7beb50d70b7266abdf51114836b77e5cc0a4582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582037aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820b97c91b4d6605e75d28a7cdd9d1929232e2b3c8843887524823f48feaa2cfcfe5820ac4cd1e3c54834f69008699d7b68668525c04ce44819c012b431f086aafb5c29582054015f083a7fa272802093c1b7ddf540fb4a6f9b8d0cba23115fa2a513293c6c5820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582036aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820d7097553b192dcb4b9ce6c44b24e09d9c460cb387965d7c3972cb8610ae7d1e15820ac4cd1e3c54834f69008699d7b68668525c04ce44819c012b431f086aafb5c29582054015f083a7fa272802093c1b7ddf540fb4a6f9b8d0cba23115fa2a513293c6c5820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582035aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820cb0b43ecf5572626537ad22975374bfa0cb183576d7e77435ed95b3031249d5d582061dac83d1b555f8a3715f2d148031e686f5ad1282064c69415ac89c75ad8059a582054015f083a7fa272802093c1b7ddf540fb4a6f9b8d0cba23115fa2a513293c6c5820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582034aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58206133dc2e4c7d3c58876782e0fd4b7d5a6f3b3215dc8fa3777df0359e3e0ec914582061dac83d1b555f8a3715f2d148031e686f5ad1282064c69415ac89c75ad8059a582054015f083a7fa272802093c1b7ddf540fb4a6f9b8d0cba23115fa2a513293c6c5820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582033aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58204dcf1b0c9915f080f3960bf47e3c8d6bf1ffcc28d0a05166b5ec0ba9b35e408c5820f236bcf06de73c39adb961c01668885c0beba85c77dde6db0a70c247ab8bf665582098caa8dcb61e799d9a079e1c301a2f6c54e9b666820378c9e6a660460aa5d2115820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582032aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58203d014d6d86914389d56bc17aa425f31593812e157db20c235a69395184b19bc35820f236bcf06de73c39adb961c01668885c0beba85c77dde6db0a70c247ab8bf665582098caa8dcb61e799d9a079e1c301a2f6c54e9b666820378c9e6a660460aa5d2115820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582031aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a9de7a715934909893d32c85ec28cc2adce1a0ca612d20738f553f8581031e5958208cf58549b665f2cf1a21e04fb15036baa16efa3d5062c9ad234078b387261597582098caa8dcb61e799d9a079e1c301a2f6c54e9b666820378c9e6a660460aa5d2115820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582030aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205157a89f8e911e9876b814c526ef82612758f5fdb0b48fb114847e02da90e15e58208cf58549b665f2cf1a21e04fb15036baa16efa3d5062c9ad234078b387261597582098caa8dcb61e799d9a079e1c301a2f6c54e9b666820378c9e6a660460aa5d2115820c0f105b7862bf050b10c07b60daa340c49d6312d9634c94f88a8d5e3030cf764582056644025a7b8cf1c0af9e3b5a5fad9ee22d9b46330cc58efe85c0469cb89a24e582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558202faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58201d050efecc3796fe3435fa27d4b85a34a4079f82d3fbd1b4aff436c1812ecb0d5820cc40251bdcf608d3ad39b832e737f3e7d3b424b68d84a01f9ffaf017b529e2e65820253bbe9faa550cd4ba8595a69c95ea676c977c462c1bbf7c8d82fb18306079115820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558202eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820110db1f93e83f1a51bbc27dbc37c4946d8b0a3f7655b729f2e4e21c013a66ca25820cc40251bdcf608d3ad39b832e737f3e7d3b424b68d84a01f9ffaf017b529e2e65820253bbe9faa550cd4ba8595a69c95ea676c977c462c1bbf7c8d82fb18306079115820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558202daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e9a9548863cb0147ac1e414f4ccb3768d706d3b9d611b04b2a0a5914323d3f6f5820b457129ae670aa9aa27cf9b39dbddd49e7130f38d4f941f42e8e884411f308895820253bbe9faa550cd4ba8595a69c95ea676c977c462c1bbf7c8d82fb18306079115820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558202caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58201da9810919ffb4d1c39b7af037f33d965bae9d561762bdba5658f9e3b30016e05820b457129ae670aa9aa27cf9b39dbddd49e7130f38d4f941f42e8e884411f308895820253bbe9faa550cd4ba8595a69c95ea676c977c462c1bbf7c8d82fb18306079115820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558202baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58209894978b9027e477c59b1d6f2c6707da84fe908170722c8f44f4e4899b84cdd458209ad598e8d65189fe295b14a4315e712c27a09ee26b4bb6e9bd4c8cdf37a11903582096eebf45c93060b647db372a09d6d0c0098e4f902ca4b6eae3cc7a9ff84aab615820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558202aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820f47b906a5b36e4bda0570272c5857a0b7e99becef6e0dadd93efba4da9e68dbc58209ad598e8d65189fe295b14a4315e712c27a09ee26b4bb6e9bd4c8cdf37a11903582096eebf45c93060b647db372a09d6d0c0098e4f902ca4b6eae3cc7a9ff84aab615820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582029aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58202de6fcd34afe982f4bbd30987ef2ce50b9f5f29e2a6b779c604cbca8dd6fad2d582026675d90c3a2e73987738d5329e4a98e22cc30cb0dd31f9155b1dafd57ff01cd582096eebf45c93060b647db372a09d6d0c0098e4f902ca4b6eae3cc7a9ff84aab615820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582028aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58205ae95dfe6c055e20df4f9b2df1c74d8e46cddbc1f50f3d2f37d2fa8ed400a059582026675d90c3a2e73987738d5329e4a98e22cc30cb0dd31f9155b1dafd57ff01cd582096eebf45c93060b647db372a09d6d0c0098e4f902ca4b6eae3cc7a9ff84aab615820969608b6a857d18555873acf19a71b40561d2ed80d59e7c415830328dc85e4955820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582027aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58204b7e3245e1280ff39b4e8dc34976b62dc0c3f601379ac3fb7dd1a561c75e7d845820885ef2ac8682ca90a65323354f88794c6b1c167fa3e8155882efb30466fce7f85820eaa47b95c99d419ec6e8970a2d321fafda691d4afc50953d0763b1f29e7daad75820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582026aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820dd00bf2f687527044bffe4782f713f4f4b858d93a62665dbfae48906c8582d8d5820885ef2ac8682ca90a65323354f88794c6b1c167fa3e8155882efb30466fce7f85820eaa47b95c99d419ec6e8970a2d321fafda691d4afc50953d0763b1f29e7daad75820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582025aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e9ab9a2a35fdd0edf4892ae979a1b62637b079d51ed7ec20162a105c2c47ebe95820043dfc36fb094fdf09c7e182c624ff9c3dc3dfac76d2c3a3b787ded9f9e0745c5820eaa47b95c99d419ec6e8970a2d321fafda691d4afc50953d0763b1f29e7daad75820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582024aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58202fa554c85c6576134a7f58de11741db47d66b613265eace7bbdc0bca8bbd56125820043dfc36fb094fdf09c7e182c624ff9c3dc3dfac76d2c3a3b787ded9f9e0745c5820eaa47b95c99d419ec6e8970a2d321fafda691d4afc50953d0763b1f29e7daad75820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582023aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582064a384c778fb3c54307fd8d449279d7e80c2da819888e38be04fbd1fc88e9b4f5820d2dfd31f6511727dc9237f10781c6aa7b43f2708c3041dec4b32bb140d4638f358209400c93d35bc620c5454317e15c10e95cadf6090a26ce12c661a5d2aa31fdb575820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582022aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582063ac8dfa5dd7b27b143395caf8c34f2f98d478fb5a7a34b5c99a71f202dc96145820d2dfd31f6511727dc9237f10781c6aa7b43f2708c3041dec4b32bb140d4638f358209400c93d35bc620c5454317e15c10e95cadf6090a26ce12c661a5d2aa31fdb575820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582021aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582054e3e6b66396db9b8e76d7e7ed4c1c27d38cd3efb1081571b9b7d7d5862e69535820419dc471fb004af56d4780237dca9c87516773fb2d8a8c1e75db93ea47db278558209400c93d35bc620c5454317e15c10e95cadf6090a26ce12c661a5d2aa31fdb575820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582020aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820b2979c3d8c3cd5fcc1b8bbc2113fbbbd581a9e08961b33240f817f85616027e85820419dc471fb004af56d4780237dca9c87516773fb2d8a8c1e75db93ea47db278558209400c93d35bc620c5454317e15c10e95cadf6090a26ce12c661a5d2aa31fdb575820f7c5709543639d0ded253f7be22a78616546acda722f662f749949b0509b73af5820123205a9ece3d63bca980b209ce0b072206bb748e2b69d27e6a3388eb3f4fdd1582073c86985994683082d7d0b0b0ba2662b7e1d753ae43e617a492d0addda4d2d5e5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558201faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582056c85c5072b042e8d01437409f5669cbdf9bfb91e15a6ca6d0e1beddb0ef2e545820e8ec7e0efe9dc0da2cb49acfbfd2ebe49c1e121cda1a6cc07834b25262414f3f5820d8e8879b6f6b06869c2acad5f581c2ba9bd8175d621a4dcf946237f5753d030b582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558201eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820da44d384b6e7f4140311a567d6d3a90aa82c8bef30c899470a5e57530fd2b3e65820e8ec7e0efe9dc0da2cb49acfbfd2ebe49c1e121cda1a6cc07834b25262414f3f5820d8e8879b6f6b06869c2acad5f581c2ba9bd8175d621a4dcf946237f5753d030b582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558201daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820d6d4abe3c412099787be34942a46245fe7dd17c3ac0e4a305d941bc5c8ede962582018a4673f5eb2ccd8bcd3850f43b279ddd4975fe13bff9bd2fd9f29de3fb606e05820d8e8879b6f6b06869c2acad5f581c2ba9bd8175d621a4dcf946237f5753d030b582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558201caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58200281f8d3082b9facb5c3ad971461c6fd1f7ee88ab3d4c8bdd3a00d041d320041582018a4673f5eb2ccd8bcd3850f43b279ddd4975fe13bff9bd2fd9f29de3fb606e05820d8e8879b6f6b06869c2acad5f581c2ba9bd8175d621a4dcf946237f5753d030b582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558201baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820b9e0bcc11387b06f64773676d75b5df0849f7825ff8dc4030ec4e370e9942b0558208ef971953c493ded4d51bde336f9ce67bf304c6bd7cbd57d69f436cfeafe5618582088028fb1c4ebec6667a624cf95fc540a9a41d0be6c0790fec123dccc8f5fd55f582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558201aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582097448ec2c444688b60409cf46f6605ddd15427f4f1d1bff768ed2b5e5dbd895058208ef971953c493ded4d51bde336f9ce67bf304c6bd7cbd57d69f436cfeafe5618582088028fb1c4ebec6667a624cf95fc540a9a41d0be6c0790fec123dccc8f5fd55f582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582019aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820200a427e73ceec75a5ad05635a8a001df9c64aff492ccf6c7d5a95cd90a00c525820a4d621d142917f3169bd752970ebf640af6cca995ac9c77b91f42ba7b3f23822582088028fb1c4ebec6667a624cf95fc540a9a41d0be6c0790fec123dccc8f5fd55f582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582018aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820bb34a5a02ca0fa70232862dd98806dace6e8db4eb5ee6b221d59eca201b227325820a4d621d142917f3169bd752970ebf640af6cca995ac9c77b91f42ba7b3f23822582088028fb1c4ebec6667a624cf95fc540a9a41d0be6c0790fec123dccc8f5fd55f582096a5c576dda762fbfb09c6afefef9e8e47ceb31f2f64c245d0818f9f305af47058202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582017aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820b8f73ce3124a9e888466928d8ced28bcb3baefd9030bd42f2f82421a91c2c859582040bac1358526418b784af9e754a32059d4745310feb7868c54260b47a2c61eb15820a90eafbf94596034d4283abc0ac03fcee000ed1ff923506b054dd05a7d20b423582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582016aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582088f8c8b20107062376debf364f6df82be79eeeed1ae98b1be65169592ffe3310582040bac1358526418b784af9e754a32059d4745310feb7868c54260b47a2c61eb15820a90eafbf94596034d4283abc0ac03fcee000ed1ff923506b054dd05a7d20b423582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582015aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820495336a1c19d5627c4ae7901e1fc6cb5b3a3c2d7da3347a29f3d819d0e83c31e5820e95ec57ab63fd9cfdb39a1e778ce5f78fe1d753df37402f3559c558bf5bf723a5820a90eafbf94596034d4283abc0ac03fcee000ed1ff923506b054dd05a7d20b423582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582014aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582002202bc54829f001aa57b9d1d788976871f70901a201c912e3bb3a9353ee0fd25820e95ec57ab63fd9cfdb39a1e778ce5f78fe1d753df37402f3559c558bf5bf723a5820a90eafbf94596034d4283abc0ac03fcee000ed1ff923506b054dd05a7d20b423582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582013aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58201d661f2c950abcd5adf5128ab700c8a10a4cea7e3e4227f3726a3310ca67b5c358205a0f41e8b24cb6e0a739b3d5384b45d61f52b077c9bebe27c00f73e564196124582061159164439e788ad8b9efb72f520f1bc5626b540401350dd996f614aff423a8582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582012aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820b9fa08cc6e9d6b887365d1623775c0153f2d1367ef66c22a4ed1b66ac2c0264158205a0f41e8b24cb6e0a739b3d5384b45d61f52b077c9bebe27c00f73e564196124582061159164439e788ad8b9efb72f520f1bc5626b540401350dd996f614aff423a8582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582011aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582085adef99dfae1212ce9e13fdc3718b17e73133cb4770c1978d1e99c819376c47582016d45c2e27611fc92ab2ff06a2557184459d39f9cec25ea61b13212a1036081c582061159164439e788ad8b9efb72f520f1bc5626b540401350dd996f614aff423a8582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582010aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820a0a1f1fd460f5c36b0d5522484584f23a7268080ff943cbc3fc3a6fd300fbdcb582016d45c2e27611fc92ab2ff06a2557184459d39f9cec25ea61b13212a1036081c582061159164439e788ad8b9efb72f520f1bc5626b540401350dd996f614aff423a8582092b56f2c768520b066d6fecc9f9e3769c4aac00aa229b86bdfa7a3233378a5a458202f8b42a37be844c79130a5d86dd1f70bb7543da8f2a813dd3262fd1e0fbc67cf58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558200faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582005fcf47431f9e54346c393d710ce784e15368a85838c80841d4df39b078e60145820abe0454cbbfbfb9e5c28f1ba6feaa59f165db4ae844c4bc5231e36730b473d5958203a1418de5a8d9101158b6428080e7571b823c7e2f6dc1012d9a86a2aec34da185820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558200eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820e8c492fb587cb0c63d8fda04007d86e557774a31d226b933b48d0763c58bccb85820abe0454cbbfbfb9e5c28f1ba6feaa59f165db4ae844c4bc5231e36730b473d5958203a1418de5a8d9101158b6428080e7571b823c7e2f6dc1012d9a86a2aec34da185820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558200daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820b474cb6782b9fa5156b8672ffa4c3b9544af67f3ac297260977163cd759782195820d0e8bbfd69c744fbe1195b8d97957a2966545de6341d1beb82d125ac0bc8ff5658203a1418de5a8d9101158b6428080e7571b823c7e2f6dc1012d9a86a2aec34da185820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558200caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820477c388f19a6b9e353f9d9fa9ab5b1df610f6e5e81e2815a1a9817db3d76f6105820d0e8bbfd69c744fbe1195b8d97957a2966545de6341d1beb82d125ac0bc8ff5658203a1418de5a8d9101158b6428080e7571b823c7e2f6dc1012d9a86a2aec34da185820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558200baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582002cba8412003b4db80b1918c4460648674e3221977a42a4729e4f3f6a194b79b5820c9df1f625af5118d082020004128f29ea59abd7072377d1295204e1f9c0263c55820788eca5bb2626c475f6c72b83376c23917f6df616f55dd975d725eabeb7c8ddf5820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c5555555555555555555555555555555555555555555555555555555558200aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820347afe5763e85ab2387fd0590573fd93e5f64834a4ad8d16d997722f3e8af4d55820c9df1f625af5118d082020004128f29ea59abd7072377d1295204e1f9c0263c55820788eca5bb2626c475f6c72b83376c23917f6df616f55dd975d725eabeb7c8ddf5820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582009aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58206177c164493a0224a115cae3d1079b8e9c7fb617ce02e53d2b659471847a90ed58204f6416aab910d504ce300253f1473b13e37271a377d275a94bc853c4a293bbb15820788eca5bb2626c475f6c72b83376c23917f6df616f55dd975d725eabeb7c8ddf5820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582008aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582068113bd609b97fef93cd9971fd2b03f8e0bc0639b43803e04753cb8c61a6659458204f6416aab910d504ce300253f1473b13e37271a377d275a94bc853c4a293bbb15820788eca5bb2626c475f6c72b83376c23917f6df616f55dd975d725eabeb7c8ddf5820aee1938a550ade46f8c5bda00b0344dcf8113fecb6b9d6925e5617aabab2621c5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582007aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58206a4f1f1d86a0831cbe484590350b7d6ca9da63b615f1662120ad07c1b96d7b6b5820acb35704a2237fef99e5629a22298b76dd249aeb768d84c7a4f293fbf9a090ff5820bb4b232688e4221eb55ac081ab9afae8afdcb78f650d43e2b6f1bb35ff18ee5358204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582006aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58206e1f79f70fa198d18c9cffa92f58ca2fd72392f8a077679155154c6a0f7fc99f5820acb35704a2237fef99e5629a22298b76dd249aeb768d84c7a4f293fbf9a090ff5820bb4b232688e4221eb55ac081ab9afae8afdcb78f650d43e2b6f1bb35ff18ee5358204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582005aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820266603acb42e667ac5f08f1d1fca1b548c65869e9af637665f49fe5e7fe686485820709ab97898eed844206279c8f267ffaa13ed65c828363a44317de7ed638a54e35820bb4b232688e4221eb55ac081ab9afae8afdcb78f650d43e2b6f1bb35ff18ee5358204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582004aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58209671dba386de4806723a00ce544f277f443c01e0c0dba6a279bfd0d5103b1d3b5820709ab97898eed844206279c8f267ffaa13ed65c828363a44317de7ed638a54e35820bb4b232688e4221eb55ac081ab9afae8afdcb78f650d43e2b6f1bb35ff18ee5358204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582003aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820db94dca20f9591537af643f2b22930f332d5d757e43661bee4e91cf3d495807a58202bf5666f69d0cb39a110a35ad13e046e55bbab075691c96102c4dfce0150a741582019b70478585f0bf117d7b95c766699f14639d62477053a656fbe72fdd308054258204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582002aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f58204298c16ee439c9f5c16e96cfb55bcc0de02e3b227123d018b9ea449ca62f1ff358202bf5666f69d0cb39a110a35ad13e046e55bbab075691c96102c4dfce0150a741582019b70478585f0bf117d7b95c766699f14639d62477053a656fbe72fdd308054258204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582001aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f582024383e249731b2ea1acf26f669c461d8f14a4158d17e2e44fbecbffc465543a358209545e9c75d21c526687d4aaf5e11cac6562a94584610c6a65110be78de6734a6582019b70478585f0bf117d7b95c766699f14639d62477053a656fbe72fdd308054258204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87b9f581c55555555555555555555555555555555555555555555555555555555582000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa019f5820025a790431a864be2f6d7285146f6a7916a9735180124f90ca3258c8175dbeb158209545e9c75d21c526687d4aaf5e11cac6562a94584610c6a65110be78de6734a6582019b70478585f0bf117d7b95c766699f14639d62477053a656fbe72fdd308054258204bce873a76e13b634c329f50f8c7cf59eaa2f63164944884b7c60d31a2a651ae5820bad7c90265fd209c5c015c42dd6adf72935abe3e1571229b057746ac57f48cfb58201a8e3337704bf714bb49a1212d35b4ff12b782a5fac2cedc64acf81db432480d5820d1d01b52d374d290086cb3d2a22e0f314d05904d76fb12cf11443b66cea98002ffffd87980d87980d87980d87e9f0120ffd905029fd87a80ffd905029fd87980ffd905029fd87980ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87980d87a80ffd87c9fd87e9fd87a80ffd87a80ffd87980ff"

const long_asset_names_value_under_the_cap_terminal_hash =
  #"c6a3a8ba617af6f437c0a334f72d2971fe2e1a7322740676dbdcdcccd70801a5"

const long_asset_names_value_under_the_cap_planner_claims =
  #"9f9f190128d8799f1913ab010040d8799f01001913ab0100d87a80ffffff9f190129d8799f1913ab010040d8799f01011913ab0105d8799fd8799f01001913ab01d8799f000080ffd8799fd8799f01000017584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffffffffff9f190138d8799f1913ab010040d8799f01011913ab0105d8799fd8799f01011913ab01d8799f01019fd8799f0058205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df301ffffffd87a80ffffffffff9f190139d8799f1913ab010040d8799f01021913ab0105d8799fd8799f01011913ab01d8799f01019fd8799f0058205fb8c7d0d530210e8dc0ab900c98c42577fe756e39777c1e516c187f89e55df301ffffffd87a80ffffffffffff"

test value_of_4999_bytes_of_long_asset_names_replays_the_typescript_trace() {
  replays_a_long_asset_name_value(
    long_asset_names_value_under_the_cap_output,
    4999,
    long_asset_names_value_under_the_cap_witnesses,
    long_asset_names_value_under_the_cap_terminal_hash,
    long_asset_names_value_under_the_cap_planner_claims,
  )
}

// One byte over the cap, the TypeScript output scan refuses the output and
// the proof builder refuses it with it. The on-chain scan refuses the same
// output after the same number of steps, at the same control.

/// The output scan of `output` from `control`, `taken` steps in, up to its
/// first refused step: that step's control and the number of steps before
/// it, or `None` when the scan reaches its terminal.
fn scan_to_refusal(
  control: output_scan.LedgerOutputScanControlV1,
  output: ByteArray,
  taken: Int,
) -> Option<(output_scan.LedgerOutputScanControlV1, Int)> {
  let total_length = bytearray.length(output)
  expect taken <= total_length + 32
  when output_scan.finish_v1(control, total_length) is {
    Some(_) -> None
    None -> {
      let chunk_start = control.cursor - control.cursor % 4095
      let span = if control.stage <= output_scan.stage_optional_field {
        8190
      } else {
        4095
      }
      let window = bytearray.slice(output, chunk_start, chunk_start + span - 1)
      when
        output_scan.step_v1(
          control,
          total_length,
          window,
          control.cursor - chunk_start,
        )
      is {
        None -> Some((control, taken))
        Some(next) -> scan_to_refusal(next, output, taken + 1)
      }
    }
  }
}

const long_asset_names_value_over_the_cap_output =
  #"a300581d781111111111111111111111111111111111111111111111111111111101821a001e8480a1581c55555555555555555555555555555555555555555555555555555555b88e582000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582001aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582002aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582003aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582004aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582005aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582006aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582007aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582008aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582009aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158200faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582010aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582011aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582012aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582013aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582014aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582015aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582016aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582017aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582018aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582019aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158201faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582020aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582021aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582022aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582023aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582024aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582025aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582026aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582027aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582028aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582029aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158202faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582030aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582031aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582032aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582033aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582034aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582035aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582036aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582037aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582038aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582039aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158203faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582040aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582041aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582042aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582043aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582044aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582045aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582046aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582047aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582048aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582049aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158204faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582050aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582051aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582052aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582053aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582054aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582055aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582056aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582057aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582058aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582059aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158205faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582060aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582061aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582062aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582063aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582064aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582065aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582066aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582067aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582068aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582069aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158206faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582070aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582071aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582072aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582073aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582074aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582075aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582076aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582077aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582078aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582079aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207daaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207eaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158207faaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582080aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582081aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582082aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582083aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582084aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582085aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582086aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582087aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582088aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01582089aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158208aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158208baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0158208caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0157ffbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb190100024100"

const long_asset_names_value_over_the_cap_accepted_steps = 144

const long_asset_names_value_over_the_cap_refused_control_hash =
  #"975b898d8826baadb8b9a5d6bd6e53a9bce1bc8292cbe2c3a6f83e4e62547851"

test value_of_5001_bytes_of_long_asset_names_is_refused_where_the_typescript_scan_refuses_it() {
  expect Some((refused, taken)) = scan_to_refusal(
    output_scan.initial_control_v1(),
    long_asset_names_value_over_the_cap_output,
    0,
  )
  and {
    bounded_item_v1.chunk_count(
      bytearray.length(long_asset_names_value_over_the_cap_output),
    ) == 2,
    taken == long_asset_names_value_over_the_cap_accepted_steps,
    crypto.blake2b_256(output_scan.encode_control_v1(refused)) == long_asset_names_value_over_the_cap_refused_control_hash,
  }
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
