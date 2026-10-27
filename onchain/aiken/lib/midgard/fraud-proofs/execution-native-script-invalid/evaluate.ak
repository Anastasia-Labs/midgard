//// Paged evaluation of one committed native source. Each page reads a
//// consecutive run of authenticated chunks of the source item and walks
//// tokens in pre-order; open containers live on a hash-linked frame stack, so
//// a page carries only the frames it pops. Signature leaves are decided
//// against the signer frontier built from the address-witness field (field
//// 7), whose canonical order ascends by signer hash, and timelock leaves
//// against the transaction's validity interval. The semantics are the producer's: `after` holds when
//// the interval start is present and at or after the slot, `before` when the
//// interval end is present and at or before it.

use aiken/builtin
use aiken/primitive/bytearray
use midgard/bounded_item_v1.{ChunkProofV1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/execution_native_script_invalid/signers.{
  signer_leaf_v1,
}
use midgard/fraud_proofs/execution_native_script_invalid/types.{
  EvalV1, Evaluated, FinalV1,
}
use midgard/native_script_scan_v1.{
  NativeFrameComplete, NativeFramePending, NativeScriptFrameV1, after_node,
  apply_child_v1, before_node, empty_container_result_v1, frame_for_token_v1,
  hash_frame_v1, signature_node, token_at_v1, versioned_script_header_v1,
}
use midgard/validation_merkle_v1.{FrontierPeak}

/// A signer frontier leaf and its membership path.
pub type SignerWitnessV1 {
  key_hash: ByteArray,
  siblings: List<ByteArray>,
}

/// One signature leaf's answer, consumed in leaf order. `SignerPresent`
/// proves the leaf's key at a frontier position. `SignerAbsent` names the
/// position the key would take and proves its neighbours: the signer below
/// (none at position 0) and the signer above (none at the frontier's end).
pub type SignerQueryV1 {
  SignerPresent { position: Int, siblings: List<ByteArray> }
  SignerAbsent {
    position: Int,
    below: Option<SignerWitnessV1>,
    above: Option<SignerWitnessV1>,
  }
}

pub type PageV1 {
  input_index: Int,
  output_index: Int,
  chunks: List<ChunkProofV1>,
  frames: List<NativeScriptFrameV1>,
  signers: List<SignerQueryV1>,
  budget: Int,
}

pub type PagedV1 {
  Paged { state: EvalV1 }
  Finished { state: FinalV1 }
}

pub type Datum =
  ct.StepDatum<EvalV1>

pub type SpendRedeemer =
  ct.StepRedeemer<PageV1>

type EnvV1 {
  window: ByteArray,
  base: Int,
  signer_count: Int,
  signer_peaks: List<FrontierPeak>,
  validity_interval_start: Int,
  validity_interval_end: Int,
}

fn signer_at_v1(env: EnvV1, position: Int, witness: SignerWitnessV1) -> Bool {
  validation_merkle_v1.verify_membership(
    env.signer_count,
    env.signer_peaks,
    position,
    signer_leaf_v1(witness.key_hash),
    witness.siblings,
  )
}

fn signer_holds_v1(
  env: EnvV1,
  query: SignerQueryV1,
  key_hash: ByteArray,
) -> Bool {
  when query is {
    SignerPresent { position, siblings } -> {
      expect signer_at_v1(env, position, SignerWitnessV1 { key_hash, siblings })
      True
    }
    SignerAbsent { position, below, above } -> {
      expect and {
          when below is {
            None -> position == 0
            Some(witness) -> and {
                signer_at_v1(env, position - 1, witness),
                builtin.less_than_bytearray(witness.key_hash, key_hash),
              }
          },
          when above is {
            None -> position == env.signer_count
            Some(witness) -> and {
                signer_at_v1(env, position, witness),
                builtin.less_than_bytearray(key_hash, witness.key_hash),
              }
          },
        }
      False
    }
  }
}

fn concat_chunks_v1(
  commitment: ByteArray,
  expected_index: Int,
  window: ByteArray,
  chunks: List<ChunkProofV1>,
) -> ByteArray {
  when chunks is {
    [] -> window
    [proof, ..rest] -> {
      expect and {
          proof.chunk_index == expected_index,
          bounded_item_v1.verify_chunk(commitment, proof),
        }
      concat_chunks_v1(
        commitment,
        expected_index + 1,
        bytearray.concat(window, proof.chunk),
        rest,
      )
    }
  }
}

/// The page window and its absolute offset. Every chunk is checked against
/// the item commitment, which also binds the item length each proof carries.
fn window_v1(
  commitment: ByteArray,
  chunks: List<ChunkProofV1>,
) -> Pair<Int, ByteArray> {
  when chunks is {
    [] -> Pair(0, #"")
    [first, ..] ->
      Pair(
        first.chunk_index * bounded_item_v1.chunk_bytes,
        concat_chunks_v1(commitment, first.chunk_index, #"", chunks),
      )
  }
}

type WalkV1 {
  cursor: Int,
  pending: Option<Bool>,
  local: List<NativeScriptFrameV1>,
  local_depth: Int,
  root: ByteArray,
  depth: Int,
}

fn walk_v1(
  env: EnvV1,
  cursor: Int,
  pending: Option<Bool>,
  local: List<NativeScriptFrameV1>,
  local_depth: Int,
  root: ByteArray,
  depth: Int,
  frames: List<NativeScriptFrameV1>,
  queries: List<SignerQueryV1>,
  budget: Int,
) -> WalkV1 {
  let done = when pending is {
    Some(_) -> local_depth == 0 && depth == 0
    None -> False
  }
  if done || budget == 0 {
    WalkV1 { cursor, pending, local, local_depth, root, depth }
  } else {
    when pending is {
      Some(valid) -> {
        // Pop the top frame: one this page pushed, else the next carried
        // frame, which must be the one the stack root commits.
        let (top, below, below_depth, below_root, below_depth_base, rest_frames)
          = when local is {
          [frame, ..rest] -> (frame, rest, local_depth - 1, root, depth, frames)
          [] -> {
            expect [frame, ..rest] = frames
            expect hash_frame_v1(frame) == root
            (frame, [], 0, frame.tail, depth - 1, rest)
          }
        }
        expect Some(applied) = apply_child_v1(top, valid)
        when applied is {
          NativeFramePending { frame } ->
            walk_v1(
              env,
              cursor,
              None,
              [frame, ..below],
              below_depth + 1,
              below_root,
              below_depth_base,
              rest_frames,
              queries,
              budget - 1,
            )
          NativeFrameComplete { valid: parent_valid, .. } ->
            walk_v1(
              env,
              cursor,
              Some(parent_valid),
              below,
              below_depth,
              below_root,
              below_depth_base,
              rest_frames,
              queries,
              budget - 1,
            )
        }
      }
      None -> {
        expect Some(token) = token_at_v1(env.window, cursor - env.base, cursor)
        if token.kind == signature_node {
          expect [query, ..rest_queries] = queries
          walk_v1(
            env,
            token.next_offset,
            Some(signer_holds_v1(env, query, token.key_hash)),
            local,
            local_depth,
            root,
            depth,
            frames,
            rest_queries,
            budget - 1,
          )
        } else if token.kind == after_node {
          walk_v1(
            env,
            token.next_offset,
            Some(
              env.validity_interval_start >= 0 && env.validity_interval_start >= token.slot,
            ),
            local,
            local_depth,
            root,
            depth,
            frames,
            queries,
            budget - 1,
          )
        } else if token.kind == before_node {
          walk_v1(
            env,
            token.next_offset,
            Some(
              env.validity_interval_end >= 0 && env.validity_interval_end <= token.slot,
            ),
            local,
            local_depth,
            root,
            depth,
            frames,
            queries,
            budget - 1,
          )
        } else {
          when empty_container_result_v1(token) is {
            Some(result) ->
              walk_v1(
                env,
                token.next_offset,
                Some(result),
                local,
                local_depth,
                root,
                depth,
                frames,
                queries,
                budget - 1,
              )
            None -> {
              expect Some(frame) = frame_for_token_v1(token, #"")
              walk_v1(
                env,
                token.next_offset,
                None,
                [frame, ..local],
                local_depth + 1,
                root,
                depth,
                frames,
                queries,
                budget - 1,
              )
            }
          }
        }
      }
    }
  }
}

/// One page. The first page reads the versioned-script header from chunk 0
/// and starts at the payload. A page ends after `budget` token reads and
/// frame pops, or when the root result is known; the root must then end
/// exactly at the item's end.
pub fn page_v1(state: EvalV1, page: PageV1) -> PagedV1 {
  expect page.budget > 0
  let Pair(base, window) = window_v1(state.item_commitment, page.chunks)
  let started = if state.cursor == 0 {
    expect [first, ..] = page.chunks
    expect first.chunk_index == 0
    expect Some(header) = versioned_script_header_v1(window, first.total_length)
    expect header.language_tag == 0
    EvalV1 {
      ..state,
      total_length: first.total_length,
      cursor: header.payload_offset,
    }
  } else {
    state
  }
  let env = EnvV1 {
    window,
    base,
    signer_count: state.signer_count,
    signer_peaks: state.signer_peaks,
    validity_interval_start: state.validity_interval_start,
    validity_interval_end: state.validity_interval_end,
  }
  let walked = walk_v1(
    env,
    started.cursor,
    started.pending,
    [],
    0,
    started.stack_root,
    started.stack_depth,
    page.frames,
    page.signers,
    page.budget,
  )
  let root = list_foldr_frames_v1(walked.local, walked.root)
  let depth = walked.depth + walked.local_depth
  let paged = Paged {
    state: EvalV1 {
      ..started,
      cursor: walked.cursor,
      stack_root: root,
      stack_depth: depth,
      pending: walked.pending,
    },
  }
  when walked.pending is {
    Some(satisfied) ->
      if depth == 0 {
        expect walked.cursor == started.total_length
        Finished {
          state: FinalV1 {
            bound: state.bound,
            fact: Evaluated { coordinate: state.coordinate, satisfied },
          },
        }
      } else {
        paged
      }
    None -> paged
  }
}

/// Re-links the frames a page left open (top first) onto the carried root.
fn list_foldr_frames_v1(
  frames: List<NativeScriptFrameV1>,
  root: ByteArray,
) -> ByteArray {
  when frames is {
    [] -> root
    [frame, ..rest] ->
      hash_frame_v1(
        NativeScriptFrameV1 { ..frame, tail: list_foldr_frames_v1(rest, root) },
      )
  }
}
