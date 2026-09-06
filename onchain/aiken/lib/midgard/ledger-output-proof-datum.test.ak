//// Golden agreement between the typed ledger-output-proof datum-traversal
//// step and the routed narrow datum stage functions of the shared step
//// family, over the honest span/scalar claims a step dispatcher action
//// carries (§5.1 of
//// docs/fault-proofs/size-plans/ledger-output-proof-shared-implementation.md).
//// The chain below streams a bignum-tagged large constructor
//// (`d86682c2490100000000000000009f01ff`), so a single traversal exercises
//// every datum stage role: head large-constructor, large-constructor
//// streaming, large-fields completion, head scalar, integer advance and
//// attach, close, list fold, frame finalize and the terminal finish
//// hand-off. Substitution attacks pin the fail-closed side: a shifted or
//// mutated span claim, a substituted scalar claim and a cross-family claim
//// swap are each refused at the exact predicate that binds them.

use aiken/builtin
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_frame_v1 as frame
use midgard/cek_data_integer_v1 as integer_control
use midgard/cek_data_traverse_v1 as traverse
use midgard/cek_source_blob_v1 as source_blob
use midgard/fraud_proofs/native_tx/codec.{slice_len}
use midgard/ledger_output_proof_datum as narrow_datum
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_stages as narrow
use midgard/ledger_output_proof_v1 as proof
use midgard/validation_merkle_v1 as merkle

/// `a3 00 address 01 empty-value 02 bytes(17)` where the datum item is the
/// large-constructor source of
/// `cek_data_traverse_v1.test.streams_large_constructor_before_fields_and_matches_typescript`.
const large_datum_output =
  #"a300581d7811111111111111111111111111111111111111111111111111111111018200a00251d86682c2490100000000000000009f01ff"

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
  Pair(until_datum(initial, chunk_proof, 64), chunk_proof)
}

fn active_datum(
  control: proof.LedgerOutputProofControlV1,
) -> traverse.DataTraverseControlV1 {
  expect Some(datum_control) = control.datum
  datum_control
}

/// The honest span claim of the shared dispatcher action: the content mode
/// (`constr 2 [start, length, bytes]`) carrying exactly the span the datum
/// control demands, or the empty claim (`constr 1 []`) when it demands none.
fn honest_span_claim(
  datum_control: traverse.DataTraverseControlV1,
  output: ByteArray,
) -> Data {
  when traverse.next_source_span_v1(datum_control) is {
    None -> builtin.constr_data(1, [])
    Some(span) ->
      builtin.constr_data(
        2,
        [
          builtin.i_data(span.absolute_start),
          builtin.i_data(span.length),
          builtin.b_data(slice_len(output, span.absolute_start, span.length)),
        ],
      )
  }
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
  output: ByteArray,
) -> Option<narrow.StepResult> {
  let span = honest_span_claim(datum_control, output)
  when action is {
    traverse.HeadScalar { .. } ->
      narrow_datum.head_scalar(control_frame, witness, span)
    traverse.HeadSequence { .. } ->
      narrow_datum.head_sequence(control_frame, witness, span)
    traverse.HeadMap { .. } ->
      narrow_datum.head_map(control_frame, witness, span)
    traverse.HeadLargeConstructor { .. } ->
      narrow_datum.head_large_constructor(control_frame, witness, span)
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
          span,
        )
      } else if datum_control.stage == traverse.stage_bytes {
        narrow_datum.advance_bytes(
          control_frame,
          witness,
          honest_scalar_claim(datum_control),
          span,
        )
      } else if datum_control.stage == traverse.stage_large_constructor {
        narrow_datum.advance_large_constructor(control_frame, witness, span)
      } else if datum_control.stage == traverse.stage_large_fields {
        narrow_datum.advance_large_fields(control_frame, witness, span)
      } else {
        expect datum_control.stage == traverse.stage_close
        narrow_datum.close(control_frame, witness, span)
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
  chunk_proof: bounded_item_v1.ChunkProofV1,
  output: ByteArray,
) -> proof.LedgerOutputProofControlV1 {
  let datum_control = active_datum(control)
  let typed_witness = proof.LedgerOutputProofDatum {
    action,
    chunk_proof: when traverse.next_source_span_v1(datum_control) is {
      None -> None
      Some(_) -> Some(chunk_proof)
    },
    next_chunk_proof: None,
  }
  let next = advanced(proof.step_v1(control, typed_witness))
  let witness_data: Data = typed_witness
  let control_frame = raw.open(proof.encode_control_v1(control))
  expect Some(narrow.Advanced { control: raw_next }) = routed(
    control_frame,
    witness_data,
    datum_control,
    action,
    output,
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
    traverse.HeadLargeConstructor {
      constructor_cbor_length: 11,
      expected_children: 1,
    },
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
    1,
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
    traverse.HeadScalar { item_length: 1 },
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
      summary.root == #"844cdd8ac8dc97d87e4ed149da121054504365b523034a804a12c014d55c2c44"
    },
  }
}

/// A span claim whose start is shifted off the span the control demands is
/// refused by the consuming stage yield's exactness clause.
test datum_span_claim_with_shifted_start_fails() fail {
  let Pair(ready, _chunk_proof) = datum_ready(large_datum_output)
  let datum_control = active_datum(ready)
  expect Some(span) = traverse.next_source_span_v1(datum_control)
  let shifted = builtin.constr_data(
    2,
    [
      builtin.i_data(span.absolute_start + 1),
      builtin.i_data(span.length),
      builtin.b_data(
        slice_len(large_datum_output, span.absolute_start, span.length),
      ),
    ],
  )
  let witness_data: Data = proof.LedgerOutputProofDatum {
    action: traverse.HeadLargeConstructor {
      constructor_cbor_length: 11,
      expected_children: 1,
    },
    chunk_proof: None,
    next_chunk_proof: None,
  }
  narrow_datum.head_large_constructor(
    raw.open(proof.encode_control_v1(ready)),
    witness_data,
    shifted,
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

/// A mutated header byte in the large-fields completion claim fails the
/// stage function closed even before the span attestation refuses it.
test large_fields_with_mutated_header_byte_fails_closed() {
  let Pair(ready, chunk_proof) = datum_ready(large_datum_output)
  let opened = same_datum_step(
    ready,
    traverse.HeadLargeConstructor {
      constructor_cbor_length: 11,
      expected_children: 1,
    },
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
  let datum_control = active_datum(large_fields)
  expect Some(span) = traverse.next_source_span_v1(datum_control)
  let mutated = builtin.constr_data(
    2,
    [
      builtin.i_data(span.absolute_start),
      builtin.i_data(span.length),
      builtin.b_data(#"00"),
    ],
  )
  let witness_data: Data = proof.LedgerOutputProofDatum {
    action: traverse.NoAction,
    chunk_proof: None,
    next_chunk_proof: None,
  }
  narrow_datum.advance_large_fields(
    raw.open(proof.encode_control_v1(large_fields)),
    witness_data,
    mutated,
  ) == None
}

/// The integer scalar attestation accepts exactly the honest claim and
/// refuses a claim whose prefix or scalar was substituted.
test integer_scalar_claim_binds_the_whole_claim() {
  let Pair(ready, chunk_proof) = datum_ready(large_datum_output)
  let opened = same_datum_step(
    ready,
    traverse.HeadLargeConstructor {
      constructor_cbor_length: 11,
      expected_children: 1,
    },
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
    traverse.HeadScalar { item_length: 1 },
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
    traverse.HeadLargeConstructor {
      constructor_cbor_length: 11,
      expected_children: 1,
    },
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
    traverse.HeadScalar { item_length: 1 },
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
