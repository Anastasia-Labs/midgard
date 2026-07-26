use aiken/cbor
use aiken/primitive/bytearray
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/validation_merkle_v1

pub const version: Int = 1

pub type CekBlobFrontierPeakV1 {
  height: Int,
  root: ByteArray,
  byte_length: Int,
}

/// Peaks are ordered from the rightmost/smallest subtree to the
/// leftmost/largest subtree. Only the first peak may contain a partial final
/// leaf.
pub type CekBlobFrontierV1 {
  count: Int,
  byte_length: Int,
  peaks: List<CekBlobFrontierPeakV1>,
}

fn power_of_two(height: Int) -> Int {
  expect height >= 0
  if height == 0 {
    1
  } else {
    2 * power_of_two(height - 1)
  }
}

fn peaks_are_well_formed(
  remaining: Int,
  prior_height: Int,
  peaks: List<CekBlobFrontierPeakV1>,
  first: Bool,
) -> Bool {
  when peaks is {
    [] -> remaining == 0
    [peak, ..rest] -> {
      let leaves = power_of_two(peak.height)
      let maximum = leaves * cek_proof_v1.max_blob_chunk_bytes
      let minimum = if first {
        ( leaves - 1 ) * cek_proof_v1.max_blob_chunk_bytes
      } else {
        maximum
      }
      and {
        peak.height >= 0,
        peak.height <= 31,
        peak.height > prior_height,
        bytearray.length(peak.root) == 32,
        peak.byte_length >= minimum,
        peak.byte_length <= maximum,
        remaining % ( 2 * leaves ) >= leaves,
        peaks_are_well_formed(
          remaining - leaves,
          peak.height,
          rest,
          False,
        ),
      }
    }
  }
}

fn peak_lengths_sum(
  peaks: List<CekBlobFrontierPeakV1>,
) -> Int {
  when peaks is {
    [] -> 0
    [peak, ..rest] -> peak.byte_length + peak_lengths_sum(rest)
  }
}

pub fn frontier_is_well_formed(
  frontier: CekBlobFrontierV1,
) -> Bool {
  let minimum = if frontier.count == 0 {
    0
  } else {
    ( frontier.count - 1 ) * cek_proof_v1.max_blob_chunk_bytes
  }
  let maximum = frontier.count * cek_proof_v1.max_blob_chunk_bytes
  and {
    frontier.count >= 0,
    frontier.count <= validation_merkle_v1.maximum_leaf_count,
    frontier.byte_length >= minimum,
    frontier.byte_length <= maximum,
    peak_lengths_sum(frontier.peaks) == frontier.byte_length,
    peaks_are_well_formed(
      frontier.count,
      -1,
      frontier.peaks,
      True,
    ),
  }
}

pub fn empty_frontier_v1() -> CekBlobFrontierV1 {
  CekBlobFrontierV1 { count: 0, byte_length: 0, peaks: [] }
}

fn merge_peak(
  peaks: List<CekBlobFrontierPeakV1>,
  appended: CekBlobFrontierPeakV1,
) -> List<CekBlobFrontierPeakV1> {
  when peaks is {
    [] -> [appended]
    [left, ..rest] ->
      if left.height == appended.height {
        let byte_length = left.byte_length + appended.byte_length
        merge_peak(
          rest,
          CekBlobFrontierPeakV1 {
            height: appended.height + 1,
            root: cek_proof_v1.hash_blob_branch_v1(
              left.root,
              appended.root,
              byte_length,
            ),
            byte_length,
          },
        )
      } else {
        [appended, left, ..rest]
      }
  }
}

pub fn append_chunk_root_v1(
  frontier: CekBlobFrontierV1,
  chunk_root: ByteArray,
  chunk_length: Int,
) -> Option<CekBlobFrontierV1> {
  if and {
    frontier_is_well_formed(frontier),
    frontier.count < validation_merkle_v1.maximum_leaf_count,
    bytearray.length(chunk_root) == 32,
    chunk_length >= 0,
    chunk_length <= cek_proof_v1.max_blob_chunk_bytes,
    if frontier.count == 0 {
      frontier.byte_length == 0
    } else {
      frontier.byte_length
        == frontier.count * cek_proof_v1.max_blob_chunk_bytes
    },
  } {
    let next = CekBlobFrontierV1 {
      count: frontier.count + 1,
      byte_length: frontier.byte_length + chunk_length,
      peaks: merge_peak(
        frontier.peaks,
        CekBlobFrontierPeakV1 {
          height: 0,
          root: chunk_root,
          byte_length: chunk_length,
        },
      ),
    }
    if frontier_is_well_formed(next) {
      Some(next)
    } else {
      None
    }
  } else {
    None
  }
}

pub fn append_chunk_v1(
  frontier: CekBlobFrontierV1,
  chunk: ByteArray,
) -> Option<CekBlobFrontierV1> {
  append_chunk_root_v1(
    frontier,
    cek_proof_v1.hash_blob_chunk_v1(chunk),
    bytearray.length(chunk),
  )
}

fn aggregate_peaks(
  aggregate: CekBlobFrontierPeakV1,
  peaks: List<CekBlobFrontierPeakV1>,
) -> CekBlobFrontierPeakV1 {
  when peaks is {
    [] -> aggregate
    [left, ..rest] -> {
      let byte_length = left.byte_length + aggregate.byte_length
      aggregate_peaks(
        CekBlobFrontierPeakV1 {
          height: left.height + 1,
          root: cek_proof_v1.hash_blob_branch_v1(
            left.root,
            aggregate.root,
            byte_length,
          ),
          byte_length,
        },
        rest,
      )
    }
  }
}

pub fn finalize_v1(
  frontier: CekBlobFrontierV1,
) -> Option<ByteArray> {
  if !frontier_is_well_formed(frontier) {
    None
  } else {
    when frontier.peaks is {
      [] -> None
      [right, ..left] -> Some(aggregate_peaks(right, left).root)
    }
  }
}

fn encode_peak(peak: CekBlobFrontierPeakV1) -> ByteArray {
  encode_definite_array_header(3)
    |> bytearray.concat(cbor.serialise(peak.height))
    |> bytearray.concat(encode_definite_bytes(peak.root))
    |> bytearray.concat(cbor.serialise(peak.byte_length))
}

fn peak_count(peaks: List<CekBlobFrontierPeakV1>) -> Int {
  when peaks is {
    [] -> 0
    [_, ..rest] -> 1 + peak_count(rest)
  }
}

fn encode_peak_items(
  peaks: List<CekBlobFrontierPeakV1>,
) -> ByteArray {
  when peaks is {
    [] -> #""
    [peak, ..rest] ->
      encode_peak(peak)
        |> bytearray.concat(encode_peak_items(rest))
  }
}

fn encode_peaks(peaks: List<CekBlobFrontierPeakV1>) -> ByteArray {
  encode_definite_array_header(peak_count(peaks))
    |> bytearray.concat(encode_peak_items(peaks))
}

pub fn encode_frontier_v1(
  frontier: CekBlobFrontierV1,
) -> ByteArray {
  expect frontier_is_well_formed(frontier)
  encode_definite_array_header(4)
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(frontier.count))
    |> bytearray.concat(cbor.serialise(frontier.byte_length))
    |> bytearray.concat(encode_peaks(frontier.peaks))
}

fn append_all(
  remaining: List<ByteArray>,
  frontier: CekBlobFrontierV1,
) -> Option<CekBlobFrontierV1> {
  when remaining is {
    [] -> Some(frontier)
    [chunk, ..rest] ->
      when append_chunk_v1(frontier, chunk) is {
        None -> None
        Some(next) -> append_all(rest, next)
      }
  }
}

pub fn root_from_chunks_v1(
  chunks: List<ByteArray>,
) -> Option<ByteArray> {
  when append_all(chunks, empty_frontier_v1()) is {
    None -> None
    Some(frontier) -> finalize_v1(frontier)
  }
}
