use aiken/primitive/bytearray
use cardano/transaction.{OutputReference}
use midgard/bounded_item_v1.{ChunkProofV1}
use midgard/canonical_plutus_data_stream_v1.{
  StreamCanonical, StreamFrameV1, StreamNonCanonical, StreamPaused,
} as stream
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_output_commitment_v1.{LedgerOutputCommitmentV1}
use midgard/ledger_output_scan_v1.{LedgerOutputScanControlV1}
use midgard/rejection_reason_v1.{InputSpentOutputNonCanonical}

pub type BoundInputV1 {
  subject: substrate.VerdictSubjectV1,
  source_kind: Int,
  input_index: Int,
  prior_root: ByteArray,
}

pub type AuthenticatedOutRefV1 {
  subject: substrate.VerdictSubjectV1,
  prior_root: ByteArray,
  out_ref: OutputReference,
}

pub type CanonicalVerdictV1 {
  subject: substrate.VerdictSubjectV1,
  output_is_non_canonical: Bool,
}

/// The inline datum of a resolved output whose envelope is canonical, read as
/// canonical Data a bounded number of nodes per transaction. `item_commitment`
/// is the descriptor's commitment to the output's bytes, which also binds the
/// output index and total length; `datum_start` and `datum_end` bound the
/// datum within the output; `cursor` is the output offset of the next node;
/// `stack_root` is the hash chain of the open containers
/// (`canonical_plutus_data_stream_v1.stack_root_v1`).
pub type DatumVerdictStateV1 {
  subject: substrate.VerdictSubjectV1,
  item_commitment: ByteArray,
  datum_start: Int,
  datum_end: Int,
  cursor: Int,
  stack_root: ByteArray,
}

/// One datum step's result: read on, or hand the verdict to the terminal.
pub type DatumStepV1 {
  ContinueDatum { state: DatumVerdictStateV1 }
  FinishDatum { verdict: CanonicalVerdictV1 }
}

pub fn bind_input_v1(
  subject: substrate.VerdictSubjectV1,
  source_kind: Int,
  input_index: Int,
  prior_root: ByteArray,
) -> BoundInputV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect source_kind == 0 || source_kind == 1
  expect input_index >= 0
  expect bytearray.length(prior_root) == 32
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    let expected = InputSpentOutputNonCanonical { source_kind, input_index }
    Some(substrate.bind_exact_rejection_reason_v1(subject, expected))
  } else {
    None
  }
  BoundInputV1 { subject, source_kind, input_index, prior_root }
}

pub fn authenticate_out_ref_v1(
  bound: BoundInputV1,
  out_ref: OutputReference,
) -> AuthenticatedOutRefV1 {
  expect bytearray.length(out_ref.transaction_id) == 32
  expect out_ref.output_index >= 0
  AuthenticatedOutRefV1 {
    subject: bound.subject,
    prior_root: bound.prior_root,
    out_ref,
  }
}

/// A chunk proof is admitted only against the descriptor's own item
/// commitment, output index and total length: no other item, index or length
/// can stand in for the resolved output the ledger committed. The descriptor
/// is already well-formed: step 04 reads it through
/// `ledger_output_commitment_v1.decode_canonical_schema`, which ends by
/// checking `descriptor_is_well_formed`.
pub fn valid_chunk_v1(
  descriptor: LedgerOutputCommitmentV1,
  proof: ChunkProofV1,
) -> Bool {
  and {
    proof.field_index == ledger_output_commitment_v1.output_field_index,
    proof.item_index == descriptor.output_index,
    proof.total_length == descriptor.total_length,
    bounded_item_v1.verify_chunk(descriptor.item_commitment, proof),
  }
}

/// One resumable reconstruction transition. The authenticated control names
/// the chunk it reads (`cursor / chunk_bytes`); the prover supplies exactly
/// that chunk and, when one follows, its successor, so a bounded token that
/// crosses the chunk boundary is readable. `Some(control)` continues the
/// self-loop; `None` is the shared engine's structural verdict that the
/// output is not canonical at the cursor.
///
/// A control that `finish_v1` can already close is never advanced: the only
/// honest move there is `finalize_canonical_v1`. Without this guard the
/// engine's window-bounds refusal at `cursor == total_length` would be
/// indistinguishable from a malformed token and a prover could record a
/// canonical output as non-canonical.
pub fn advance_reconstruction_v1(
  descriptor: LedgerOutputCommitmentV1,
  control: LedgerOutputScanControlV1,
  chunk_proof: ChunkProofV1,
  next_chunk_proof: Option<ChunkProofV1>,
) -> Option<LedgerOutputScanControlV1> {
  expect
    ledger_output_scan_v1.finish_v1(control, descriptor.total_length) == None
  expect valid_chunk_v1(descriptor, chunk_proof)
  let expected_chunk_index = control.cursor / bounded_item_v1.chunk_bytes
  expect chunk_proof.chunk_index == expected_chunk_index
  let has_next = chunk_proof.chunk_index + 1 < bounded_item_v1.chunk_count(
    descriptor.total_length,
  )
  expect
    if has_next {
      next_chunk_proof != None
    } else {
      next_chunk_proof == None
    }
  let window = when next_chunk_proof is {
    None -> chunk_proof.chunk
    Some(next_proof) -> {
      expect and {
          next_proof.chunk_index == chunk_proof.chunk_index + 1,
          valid_chunk_v1(descriptor, next_proof),
        }
      bytearray.concat(chunk_proof.chunk, next_proof.chunk)
    }
  }
  ledger_output_scan_v1.step_v1(
    control,
    descriptor.total_length,
    window,
    control.cursor - chunk_proof.chunk_index * bounded_item_v1.chunk_bytes,
  )
}

/// The canonical terminal: the zero-byte closing edge succeeds and the
/// resulting terminal control accounts for the descriptor's exact length.
/// Any earlier control, and any control left over by trailing bytes, is
/// `None`.
pub fn finalize_canonical_v1(
  descriptor: LedgerOutputCommitmentV1,
  control: LedgerOutputScanControlV1,
) -> LedgerOutputScanControlV1 {
  expect Some(terminal) = ledger_output_scan_v1.finish_v1(
    control,
    descriptor.total_length,
  )
  expect
    ledger_output_scan_v1.terminal_is_exact_v1(
      terminal,
      descriptor.total_length,
    )
  terminal
}

/// After a canonical envelope: an output with an inline datum opens the datum
/// step at the datum's first byte; any other output is canonical.
pub fn open_datum_v1(
  subject: substrate.VerdictSubjectV1,
  descriptor: LedgerOutputCommitmentV1,
  terminal: LedgerOutputScanControlV1,
) -> DatumStepV1 {
  if terminal.datum_offset >= 0 {
    ContinueDatum(
      DatumVerdictStateV1 {
        subject,
        item_commitment: descriptor.item_commitment,
        datum_start: terminal.datum_offset,
        datum_end: terminal.datum_offset + terminal.datum_length,
        cursor: terminal.datum_offset,
        stack_root: stream.empty_stack_root,
      },
    )
  } else {
    FinishDatum(CanonicalVerdictV1 { subject, output_is_non_canonical: False })
  }
}

/// Reads up to `node_budget` nodes of the datum over the output chunk holding
/// the cursor, followed by the next chunk while the datum continues past it.
/// Both chunks are admitted against the stored item commitment, whose preimage
/// binds the field, output index and total length. `revealed` and `tail_root`
/// open the stored container stack.
pub fn advance_datum_v1(
  state: DatumVerdictStateV1,
  chunk_proof: ChunkProofV1,
  next_chunk_proof: Option<ChunkProofV1>,
  revealed: List<StreamFrameV1>,
  tail_root: ByteArray,
  node_budget: Int,
) -> DatumStepV1 {
  let DatumVerdictStateV1 { subject, item_commitment, datum_end, cursor, .. } = state
  expect stream.stack_root_v1(revealed, tail_root) == state.stack_root
  expect bounded_item_v1.verify_chunk(item_commitment, chunk_proof)
  let chunk_index = min(cursor, chunk_proof.total_length - 1) / bounded_item_v1.chunk_bytes
  expect chunk_proof.chunk_index == chunk_index
  let base = chunk_index * bounded_item_v1.chunk_bytes
  let current_end = base + bytearray.length(chunk_proof.chunk)
  let window = when next_chunk_proof is {
    None -> {
      expect datum_end <= current_end
      chunk_proof.chunk
    }
    Some(next_proof) -> {
      expect and {
          datum_end > current_end,
          next_proof.chunk_index == chunk_index + 1,
          bounded_item_v1.verify_chunk(item_commitment, next_proof),
        }
      bytearray.concat(chunk_proof.chunk, next_proof.chunk)
    }
  }
  let full = base + bytearray.length(window) >= datum_end
  let bytes = if full {
    bytearray.take(window, datum_end - base)
  } else {
    window
  }
  when
    stream.run_v1(
      bytes,
      full,
      state.datum_start - base,
      cursor - base,
      revealed,
      tail_root,
      node_budget,
    )
  is {
    StreamCanonical ->
      FinishDatum(
        CanonicalVerdictV1 { subject, output_is_non_canonical: False },
      )
    StreamNonCanonical ->
      FinishDatum(CanonicalVerdictV1 { subject, output_is_non_canonical: True })
    StreamPaused { offset, frames, .. } ->
      ContinueDatum(
        DatumVerdictStateV1 {
          ..state,
          cursor: base + offset,
          stack_root: stream.stack_root_v1(frames, tail_root),
        },
      )
  }
}

fn min(left: Int, right: Int) -> Int {
  if left < right {
    left
  } else {
    right
  }
}

pub fn terminal_contradiction_v1(verdict: CanonicalVerdictV1) -> Bool {
  substrate.terminal_contradiction_v1(
    verdict.subject,
    verdict.output_is_non_canonical,
  )
}
