use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/transaction.{InlineDatum, Input}
use midgard/native_tx_field_access_v1.{
  Chunked, FieldViewV1, ProvisionalWhole, Whole, chunk_bytes_k,
}

pub type ChunkCarriage {
  InlineChunks { chunks: List<ByteArray> }
  ReferencedChunks { reference_indices: List<Int> }
}

fn hashes(bytes: ByteArray, offset: Int) -> List<ByteArray> {
  if offset == bytearray.length(bytes) {
    []
  } else {
    let remaining = bytearray.length(bytes) - offset
    let size = if remaining < chunk_bytes_k {
      remaining
    } else {
      chunk_bytes_k
    }
    let chunk = bytearray.take(bytearray.drop(bytes, offset), size)
    [blake2b_256(chunk), ..hashes(bytes, offset + size)]
  }
}

/// Called only after the native field door authenticates the view.
pub fn authenticated_chunk_hashes(view: FieldViewV1) -> List<ByteArray> {
  when view is {
    Whole { bytes, .. } -> hashes(bytes, 0)
    ProvisionalWhole { bytes, .. } -> hashes(bytes, 0)
    Chunked { chunk_digests, .. } -> chunk_digests
  }
}

fn lengths_are_exact(chunks: List<ByteArray>, remaining: Int) -> Bool {
  when chunks is {
    [] -> remaining == 0
    [chunk, ..rest] -> {
      let size = if remaining < chunk_bytes_k {
        remaining
      } else {
        chunk_bytes_k
      }
      remaining > 0 && bytearray.length(chunk) == size && lengths_are_exact(
        rest,
        remaining - size,
      )
    }
  }
}

/// The manifest is inherited from authenticated thread state. Chunk contents
/// remain untrusted until the canonical slice accessor checks their hashes.
pub fn open(
  carriage: ChunkCarriage,
  reference_inputs: List<Input>,
  digests: List<ByteArray>,
  total_length: Int,
) -> FieldViewV1 {
  let chunks = when carriage is {
    InlineChunks { chunks } -> chunks
    ReferencedChunks { reference_indices } ->
      list.map(
        reference_indices,
        fn(index) {
          expect index >= 0
          expect Some(input) = list.at(reference_inputs, index)
          expect InlineDatum(data) = input.output.datum
          builtin.un_b_data(data)
        },
      )
  }
  expect list.length(chunks) == list.length(digests)
  expect lengths_are_exact(chunks, total_length)
  Chunked { chunks, chunk_digests: digests, count: 0, stride: 0 }
}
