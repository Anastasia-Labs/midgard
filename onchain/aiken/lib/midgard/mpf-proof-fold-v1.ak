use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, nibbles, suffix}
use aiken/merkle_patricia_forestry/merkling.{
  merkle_16, null_hash, sparse_merkle_16,
}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/validation_merkle_v1

pub const proof_fold_version: Int = 1

const digest_byte_count: Int = 32

const path_nibble_count: Int = 64

const frame_domain = #"4d6964676172644d706650726f6f664672616d655631"

pub type ProofFrameV1 {
  version: Int,
  frame_index: Int,
  cursor: Int,
  next_cursor: Int,
  step: mpf.ProofStep,
}

pub type ProofDescriptorV1 {
  version: Int,
  frame_count: Int,
  terminal_cursor: Int,
  peaks: List<validation_merkle_v1.FrontierPeak>,
}

pub type ProofFoldControlV1 {
  next_frame_index: Int,
  expected_next_cursor: Int,
  including_root: ByteArray,
  excluding_root: ByteArray,
}

fn encode_frame_prefix(frame: ProofFrameV1, kind: Int, item_count: Int) {
  encode_definite_array_header(item_count)
    |> bytearray.concat(cbor.serialise(frame.version))
    |> bytearray.concat(cbor.serialise(frame.frame_index))
    |> bytearray.concat(cbor.serialise(frame.cursor))
    |> bytearray.concat(cbor.serialise(frame.next_cursor))
    |> bytearray.concat(cbor.serialise(kind))
}

pub fn encode_proof_frame_v1(frame: ProofFrameV1) -> ByteArray {
  when frame.step is {
    mpf.Branch { skip, neighbors } ->
      encode_frame_prefix(frame, 0, 7)
        |> bytearray.concat(cbor.serialise(skip))
        |> bytearray.concat(encode_definite_bytes(neighbors))
    mpf.Fork { skip, neighbor } ->
      encode_frame_prefix(frame, 1, 9)
        |> bytearray.concat(cbor.serialise(skip))
        |> bytearray.concat(cbor.serialise(neighbor.nibble))
        |> bytearray.concat(encode_definite_bytes(neighbor.prefix))
        |> bytearray.concat(encode_definite_bytes(neighbor.root))
    mpf.Leaf { skip, key, value } ->
      encode_frame_prefix(frame, 2, 8)
        |> bytearray.concat(cbor.serialise(skip))
        |> bytearray.concat(encode_definite_bytes(key))
        |> bytearray.concat(encode_definite_bytes(value))
  }
}

pub fn proof_frame_leaf_hash_v1(frame: ProofFrameV1) -> ByteArray {
  frame_domain
    |> bytearray.concat(encode_proof_frame_v1(frame))
    |> blake2b_256
}

pub fn descriptor_is_well_formed_v1(descriptor: ProofDescriptorV1) -> Bool {
  and {
    descriptor.version == proof_fold_version,
    descriptor.frame_count >= 0,
    descriptor.frame_count <= path_nibble_count,
    descriptor.terminal_cursor >= 0,
    descriptor.terminal_cursor <= path_nibble_count,
    validation_merkle_v1.frontier_is_well_formed(
      descriptor.frame_count,
      descriptor.peaks,
    ),
    if descriptor.frame_count == 0 {
      descriptor.terminal_cursor == 0
    } else {
      descriptor.terminal_cursor > 0
    },
  }
}

pub fn encode_proof_descriptor_v1(descriptor: ProofDescriptorV1) -> ByteArray {
  expect descriptor_is_well_formed_v1(descriptor)
  encode_definite_array_header(4)
    |> bytearray.concat(cbor.serialise(descriptor.version))
    |> bytearray.concat(cbor.serialise(descriptor.frame_count))
    |> bytearray.concat(cbor.serialise(descriptor.terminal_cursor))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(descriptor.peaks))
}

fn neighbor_is_well_formed(neighbor: mpf.Neighbor) -> Bool {
  and {
    neighbor.nibble >= 0,
    neighbor.nibble < 16,
    bytearray.length(neighbor.prefix) <= digest_byte_count,
    bytearray.length(neighbor.root) == digest_byte_count,
  }
}

fn frame_is_well_formed(path: ByteArray, frame: ProofFrameV1) -> Bool {
  let skip = when frame.step is {
    mpf.Branch { skip, .. } -> skip
    mpf.Fork { skip, .. } -> skip
    mpf.Leaf { skip, .. } -> skip
  }
  and {
    frame.version == proof_fold_version,
    frame.frame_index >= 0,
    frame.frame_index < path_nibble_count,
    frame.cursor >= 0,
    skip >= 0,
    frame.next_cursor == frame.cursor + 1 + skip,
    frame.next_cursor <= path_nibble_count,
    when frame.step is {
      mpf.Branch { neighbors, .. } ->
        bytearray.length(neighbors) == 4 * digest_byte_count
      mpf.Fork { neighbor, .. } -> and {
          neighbor_is_well_formed(neighbor),
          nibble(path, frame.next_cursor - 1) != neighbor.nibble,
        }
      mpf.Leaf { key, value, .. } -> and {
          bytearray.length(key) == digest_byte_count,
          bytearray.length(value) == digest_byte_count,
          nibble(path, frame.next_cursor - 1) != nibble(
            key,
            frame.next_cursor - 1,
          ),
        }
    },
  }
}

fn do_branch(
  path: ByteArray,
  frame: ProofFrameV1,
  child_root: ByteArray,
  neighbors: ByteArray,
) -> ByteArray {
  let branch = nibble(path, frame.next_cursor - 1)
  let prefix = nibbles(path, frame.cursor, frame.next_cursor - 1)
  combine(
    prefix,
    merkle_16(
      branch,
      child_root,
      bytearray.slice(neighbors, 0, 31),
      bytearray.slice(neighbors, 32, 63),
      bytearray.slice(neighbors, 64, 95),
      bytearray.slice(neighbors, 96, 127),
    ),
  )
}

fn do_fork(
  path: ByteArray,
  frame: ProofFrameV1,
  child_root: ByteArray,
  neighbor_nibble: Int,
  neighbor_prefix: ByteArray,
  neighbor_root: ByteArray,
) -> ByteArray {
  let branch = nibble(path, frame.next_cursor - 1)
  let prefix = nibbles(path, frame.cursor, frame.next_cursor - 1)
  combine(
    prefix,
    sparse_merkle_16(
      branch,
      child_root,
      neighbor_nibble,
      combine(neighbor_prefix, neighbor_root),
    ),
  )
}

fn fold_including_frame(
  path: ByteArray,
  frame: ProofFrameV1,
  child_root: ByteArray,
) -> ByteArray {
  when frame.step is {
    mpf.Branch { neighbors, .. } ->
      do_branch(path, frame, child_root, neighbors)
    mpf.Fork { neighbor, .. } ->
      do_fork(
        path,
        frame,
        child_root,
        neighbor.nibble,
        neighbor.prefix,
        neighbor.root,
      )
    mpf.Leaf { key, value, .. } ->
      do_fork(
        path,
        frame,
        child_root,
        nibble(key, frame.next_cursor - 1),
        suffix(key, frame.next_cursor),
        value,
      )
  }
}

fn fold_excluding_frame(
  path: ByteArray,
  frame: ProofFrameV1,
  child_root: ByteArray,
  is_terminal_frame: Bool,
) -> ByteArray {
  when frame.step is {
    mpf.Branch { neighbors, .. } ->
      do_branch(path, frame, child_root, neighbors)
    mpf.Fork { neighbor, .. } ->
      if is_terminal_frame {
        combine(bytearray.push(neighbor.prefix, neighbor.nibble), neighbor.root)
      } else {
        do_fork(
          path,
          frame,
          child_root,
          neighbor.nibble,
          neighbor.prefix,
          neighbor.root,
        )
      }
    mpf.Leaf { key, value, .. } ->
      if is_terminal_frame {
        combine(suffix(key, frame.cursor), value)
      } else {
        do_fork(
          path,
          frame,
          child_root,
          nibble(key, frame.cursor),
          suffix(key, frame.next_cursor),
          value,
        )
      }
  }
}

pub fn initial_fold_control_v1(
  key: ByteArray,
  value: ByteArray,
  descriptor: ProofDescriptorV1,
) -> Option<ProofFoldControlV1> {
  if descriptor_is_well_formed_v1(descriptor) {
    let path = blake2b_256(key)
    Some(
      ProofFoldControlV1 {
        next_frame_index: descriptor.frame_count - 1,
        expected_next_cursor: descriptor.terminal_cursor,
        including_root: combine(
          suffix(path, descriptor.terminal_cursor),
          blake2b_256(value),
        ),
        excluding_root: null_hash,
      },
    )
  } else {
    None
  }
}

pub fn fold_proof_frame_v1(
  key: ByteArray,
  descriptor: ProofDescriptorV1,
  control: ProofFoldControlV1,
  frame: ProofFrameV1,
  siblings: List<ByteArray>,
) -> Option<ProofFoldControlV1> {
  let path = blake2b_256(key)
  if and {
    descriptor_is_well_formed_v1(descriptor),
    bytearray.length(control.including_root) == digest_byte_count,
    bytearray.length(control.excluding_root) == digest_byte_count,
    control.next_frame_index >= 0,
    control.next_frame_index < descriptor.frame_count,
    frame.frame_index == control.next_frame_index,
    frame.next_cursor == control.expected_next_cursor,
    frame_is_well_formed(path, frame),
    validation_merkle_v1.verify_membership(
      descriptor.frame_count,
      descriptor.peaks,
      frame.frame_index,
      proof_frame_leaf_hash_v1(frame),
      siblings,
    ),
  } {
    Some(
      ProofFoldControlV1 {
        next_frame_index: control.next_frame_index - 1,
        expected_next_cursor: frame.cursor,
        including_root: fold_including_frame(
          path,
          frame,
          control.including_root,
        ),
        excluding_root: fold_excluding_frame(
          path,
          frame,
          control.excluding_root,
          frame.frame_index == descriptor.frame_count - 1,
        ),
      },
    )
  } else {
    None
  }
}

pub fn fold_is_complete_v1(control: ProofFoldControlV1) -> Bool {
  and {
    control.next_frame_index == -1,
    control.expected_next_cursor == 0,
    bytearray.length(control.including_root) == digest_byte_count,
    bytearray.length(control.excluding_root) == digest_byte_count,
  }
}
