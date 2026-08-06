use aiken/builtin.{slice_bytearray}
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, nibbles, suffix}
use aiken/merkle_patricia_forestry/merkling.{merkle_16, null_hash}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets
use cardano/transaction.{
  DatumHash, InlineDatum, Input, NoDatum, Output, OutputReference,
}
use midgard/mpf_chunked_proof_v1.{
  FinalizeProofRedeemer, Membership, NonMembership, ProofChallengeDatum,
  ProofChunkDatum,
}

// ## Deterministic identities

const proof_owner = #"11111111111111111111111111111111111111111111111111111111"

const challenged_header_hash =
  #"22222222222222222222222222222222222222222222222222222222"

const target_key =
  #"3333333333333333333333333333333333333333333333333333333333333333"

const absent_neighbour_key =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const target_value_hash =
  #"5555555555555555555555555555555555555555555555555555555555555555"

const absent_neighbour_value_hash =
  #"6666666666666666666666666666666666666666666666666666666666666666"

const chunk_holder_script_hash =
  #"77777777777777777777777777777777777777777777777777777777"

fn target_path() -> ByteArray {
  blake2b_256(target_key)
}

// ## Synthetic proof construction
//
// The on-chain library has no prover, so the fixtures build proofs directly and
// derive the root they prove by mirroring the library's own fold. Neighbour
// digests are arbitrary but deterministic, which is exactly the adversarial
// shape issue #541 describes: a ground sibling at every level forces the widest
// `Branch` step, and the resulting root is a genuine root of a trie that holds
// the target at that depth.

fn branch_neighbors(seed: Int) -> ByteArray {
  let a = blake2b_256(cbor.serialise(seed))
  let b = blake2b_256(a)
  let c = blake2b_256(b)
  let d = blake2b_256(c)
  a
    |> bytearray.concat(b)
    |> bytearray.concat(c)
    |> bytearray.concat(d)
}

fn branch_step(seed: Int) -> mpf.ProofStep {
  mpf.Branch { skip: 0, neighbors: branch_neighbors(seed) }
}

fn branch_steps(count: Int) -> mpf.Proof {
  if count <= 0 {
    []
  } else {
    list.push(branch_steps(count - 1), branch_step(count - 1))
  }
}

/// The divergent terminal leaf of a canonical absence witness: a different key
/// occupies the slot the target path would take.
fn divergent_terminal_leaf() -> mpf.ProofStep {
  mpf.Leaf {
    skip: 0,
    key: blake2b_256(absent_neighbour_key),
    value: absent_neighbour_value_hash,
  }
}

fn fold_branch(
  path: ByteArray,
  cursor: Int,
  next_cursor: Int,
  child_root: ByteArray,
  neighbors: ByteArray,
) -> ByteArray {
  combine(
    nibbles(path, cursor, next_cursor - 1),
    merkle_16(
      nibble(path, next_cursor - 1),
      child_root,
      slice_bytearray(0, 32, neighbors),
      slice_bytearray(32, 32, neighbors),
      slice_bytearray(64, 32, neighbors),
      slice_bytearray(96, 32, neighbors),
    ),
  )
}

/// Independent reconstruction of the root a membership proof proves.
fn membership_root(cursor: Int, steps: mpf.Proof) -> ByteArray {
  let path = target_path()
  when steps is {
    [] -> combine(suffix(path, cursor), target_value_hash)
    [mpf.Branch { skip, neighbors }, ..rest] -> {
      let next_cursor = cursor + 1 + skip
      let child_root = membership_root(next_cursor, rest)
      fold_branch(path, cursor, next_cursor, child_root, neighbors)
    }
    _ -> null_hash
  }
}

/// Independent reconstruction of the root an absence witness proves, for the
/// `Branch`-prefixed, divergent-terminal-leaf shape used by these fixtures.
fn non_membership_root(cursor: Int, steps: mpf.Proof) -> ByteArray {
  let path = target_path()
  when steps is {
    [mpf.Leaf { key, value, .. }] -> combine(suffix(key, cursor), value)
    [mpf.Branch { skip, neighbors }, ..rest] -> {
      let next_cursor = cursor + 1 + skip
      let child_root = non_membership_root(next_cursor, rest)
      fold_branch(path, cursor, next_cursor, child_root, neighbors)
    }
    _ -> null_hash
  }
}

// ## Transaction-shaped fixtures

fn chunk_output(datum_data: Data) -> Output {
  Output {
    address: address.from_script(chunk_holder_script_hash),
    value: assets.from_lovelace(2_000_000),
    datum: InlineDatum(datum_data),
    reference_script: None,
  }
}

fn chunk_input_from_output(index: Int, output: Output) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: blake2b_256(cbor.serialise(index)),
      output_index: index,
    },
    output,
  }
}

fn chunk_input(index: Int, steps: mpf.Proof) -> Input {
  let chunk_datum: Data = ProofChunkDatum { proof_steps: steps }
  chunk_input_from_output(index, chunk_output(chunk_datum))
}

/// Splits a proof into publication chunks of exactly
/// `maximum_chunk_proof_step_count` steps (plus a final partial chunk).
fn publish(steps: mpf.Proof) -> List<Input> {
  do_publish(steps, 0)
}

fn do_publish(steps: mpf.Proof, index: Int) -> List<Input> {
  if steps == [] {
    []
  } else {
    let head_steps = list.take(
      steps,
      mpf_chunked_proof_v1.maximum_chunk_proof_step_count,
    )
    let tail_steps = list.drop(
      steps,
      mpf_chunked_proof_v1.maximum_chunk_proof_step_count,
    )
    list.push(do_publish(tail_steps, index + 1), chunk_input(index, head_steps))
  }
}

fn indices_of(reference_inputs: List<Input>) -> List<Int> {
  list.indexed_map(reference_inputs, fn(index, _input) { index })
}

fn membership_challenge(expected_root: ByteArray) -> ProofChallengeDatum {
  ProofChallengeDatum {
    proof_owner,
    challenged_header_hash,
    target_key,
    target_value_hash,
    expected_root,
    expected_leaf_count: 1_000,
    mode: Membership,
  }
}

fn non_membership_challenge(expected_root: ByteArray) -> ProofChallengeDatum {
  ProofChallengeDatum {
    proof_owner,
    challenged_header_hash,
    target_key,
    target_value_hash,
    expected_root,
    expected_leaf_count: 1_000,
    mode: NonMembership,
  }
}

fn finalize(indices: List<Int>) -> FinalizeProofRedeemer {
  FinalizeProofRedeemer { ordered_chunk_reference_input_indices: indices }
}

fn verify(
  reference_inputs: List<Input>,
  challenge: ProofChallengeDatum,
  indices: List<Int>,
) -> Bool {
  mpf_chunked_proof_v1.verify_published_proof(
    reference_inputs,
    challenge,
    finalize(indices),
  )
}

// ## Honest acceptance

test mpf_chunked_proof_v1_verifies_a_single_chunk_membership_proof() {
  let steps = branch_steps(3)
  let reference_inputs = publish(steps)
  and {
    list.length(reference_inputs) == 1,
    verify(
      reference_inputs,
      membership_challenge(membership_root(0, steps)),
      indices_of(reference_inputs),
    ),
  }
}

/// The #541 max case: a depth-32 all-`Branch` proof — the deepest a 2^128
/// adversary can grind — split across two publication UTxOs and verified
/// atomically from a finalizing transaction that carries only two indices.
test mpf_chunked_proof_v1_verifies_the_depth_32_adversarial_membership_proof() {
  let steps = branch_steps(32)
  let reference_inputs = publish(steps)
  and {
    list.length(reference_inputs) == 2,
    verify(
      reference_inputs,
      membership_challenge(membership_root(0, steps)),
      indices_of(reference_inputs),
    ),
  }
}

test mpf_chunked_proof_v1_verifies_a_chunked_non_membership_proof() {
  let steps = list.concat(branch_steps(17), [divergent_terminal_leaf()])
  let reference_inputs = publish(steps)
  and {
    list.length(reference_inputs) == 2,
    verify(
      reference_inputs,
      non_membership_challenge(non_membership_root(0, steps)),
      indices_of(reference_inputs),
    ),
  }
}

// ## Size and depth arithmetic (issue #541 envelope defeat)

test mpf_chunked_proof_v1_pins_the_chunk_and_depth_arithmetic() {
  let widest_chunk = ProofChunkDatum {
    proof_steps: branch_steps(
      mpf_chunked_proof_v1.maximum_chunk_proof_step_count,
    ),
  }
  let widest_chunk_bytes = bytearray.length(cbor.serialise(widest_chunk))
  and {
    // A `Branch` step is the 139-byte shape the Q1x fixture measured; a full
    // chunk is 16 of them plus the list and constructor headers.
    widest_chunk_bytes == 2_230,
    widest_chunk_bytes <= mpf_chunked_proof_v1.maximum_chunk_datum_byte_count,
    // ... which is ~14% of the 16,173-byte usable inline-datum payload of the
    // preserved 16,384-byte envelope, so one publication transaction can carry
    // several full chunks.
    widest_chunk_bytes * 7 <= 16_173,
    mpf_chunked_proof_v1.chunk_datum_is_well_formed(widest_chunk),
    // Depth 32 (the 2^128 adversary's reach) needs two chunks ...
    list.length(publish(branch_steps(32))) == 2,
    // ... and the protocol-wide 64-step ceiling needs four, well inside the
    // eight-chunk reference-input bound of a finalizing transaction.
    mpf_chunked_proof_v1.maximum_total_proof_step_count == 64,
    list.length(publish(branch_steps(64))) == 4,
    4 <= mpf_chunked_proof_v1.maximum_chunk_count,
  }
}

// ## Adversarial rejection, one guard per test

test mpf_chunked_proof_v1_rejects_a_missing_step() {
  let steps = branch_steps(32)
  let expected_root = membership_root(0, steps)
  let truncated = list.take(steps, 31)
  let reference_inputs = publish(truncated)
  !verify(
    reference_inputs,
    membership_challenge(expected_root),
    indices_of(reference_inputs),
  )
}

test mpf_chunked_proof_v1_rejects_reordered_chunks() {
  let steps = branch_steps(32)
  let reference_inputs = publish(steps)
  !verify(
    reference_inputs,
    membership_challenge(membership_root(0, steps)),
    [1, 0],
  )
}

test mpf_chunked_proof_v1_rejects_a_duplicated_chunk_index() {
  let steps = branch_steps(32)
  let reference_inputs = publish(steps)
  !verify(
    reference_inputs,
    membership_challenge(membership_root(0, steps)),
    [0, 1, 1],
  )
}

test mpf_chunked_proof_v1_rejects_a_substituted_step() {
  let steps = branch_steps(32)
  let expected_root = membership_root(0, steps)
  // Right shape, wrong neighbour digests, at the last step of the last chunk.
  let substituted = list.concat(list.take(steps, 31), [branch_step(999)])
  let reference_inputs = publish(substituted)
  !verify(
    reference_inputs,
    membership_challenge(expected_root),
    indices_of(reference_inputs),
  )
}

test mpf_chunked_proof_v1_rejects_steps_trailing_the_absence_terminal() {
  let steps = list.concat(branch_steps(17), [divergent_terminal_leaf()])
  let expected_root = non_membership_root(0, steps)
  let reference_inputs = list.concat(
    publish(steps),
    [chunk_input(9, branch_steps(1))],
  )
  !verify(
    reference_inputs,
    non_membership_challenge(expected_root),
    indices_of(reference_inputs),
  )
}

test mpf_chunked_proof_v1_rejects_a_non_inline_chunk_datum() {
  let steps = branch_steps(3)
  let expected_root = membership_root(0, steps)
  let chunk_datum: Data = ProofChunkDatum { proof_steps: steps }
  let hashed = chunk_input_from_output(
    0,
    Output {
      ..chunk_output(chunk_datum),
      datum: DatumHash(blake2b_256(cbor.serialise(chunk_datum))),
    },
  )
  and {
    !verify([hashed], membership_challenge(expected_root), [0]),
    !verify(
      [
        chunk_input_from_output(
          0,
          Output { ..chunk_output(chunk_datum), datum: NoDatum },
        ),
      ],
      membership_challenge(expected_root),
      [0],
    ),
  }
}

test mpf_chunked_proof_v1_rejects_a_foreign_inline_chunk_datum() fail {
  let steps = branch_steps(3)
  let foreign: Data = target_key
  verify(
    [chunk_input_from_output(0, chunk_output(foreign))],
    membership_challenge(membership_root(0, steps)),
    [0],
  )
}

test mpf_chunked_proof_v1_rejects_an_empty_chunk() {
  let steps = branch_steps(3)
  let expected_root = membership_root(0, steps)
  !verify(
    [chunk_input(0, []), chunk_input(1, steps)],
    membership_challenge(expected_root),
    [0, 1],
  )
}

test mpf_chunked_proof_v1_rejects_an_oversized_chunk() {
  let steps = branch_steps(17)
  let expected_root = membership_root(0, steps)
  and {
    !mpf_chunked_proof_v1.chunk_datum_is_well_formed(
      ProofChunkDatum { proof_steps: steps },
    ),
    !verify([chunk_input(0, steps)], membership_challenge(expected_root), [0]),
  }
}

test mpf_chunked_proof_v1_rejects_more_steps_than_the_protocol_bound() {
  // 80 steps is past the 64-nibble MPF path, so no root exists that such a
  // proof could prove: the total-step bound must reject before the walk.
  let steps = branch_steps(80)
  let reference_inputs = publish(steps)
  and {
    list.length(reference_inputs) == 5,
    list.length(steps) > mpf_chunked_proof_v1.maximum_total_proof_step_count,
    !verify(
      reference_inputs,
      membership_challenge(membership_root(0, branch_steps(3))),
      indices_of(reference_inputs),
    ),
  }
}

test mpf_chunked_proof_v1_rejects_more_chunks_than_the_chunk_bound() {
  let single_step_chunks = list.map(
    list.range(0, 8),
    fn(index) { chunk_input(index, branch_steps(1)) },
  )
  and {
    list.length(single_step_chunks) == 9,
    !mpf_chunked_proof_v1.chunk_indices_are_well_formed(
      indices_of(single_step_chunks),
      9,
    ),
    !verify(
      single_step_chunks,
      membership_challenge(membership_root(0, branch_steps(1))),
      indices_of(single_step_chunks),
    ),
  }
}

test mpf_chunked_proof_v1_rejects_an_empty_chunk_order_against_a_deep_root() {
  let steps = branch_steps(3)
  and {
    // The zero-step proof is legitimate for a one-leaf trie ...
    verify([], membership_challenge(membership_root(0, [])), []),
    // ... and proves nothing about any deeper root.
    !verify(publish(steps), membership_challenge(membership_root(0, steps)), []),
  }
}

test mpf_chunked_proof_v1_rejects_an_out_of_range_chunk_index() {
  let steps = branch_steps(3)
  let expected_root = membership_root(0, steps)
  let reference_inputs = publish(steps)
  and {
    !verify(reference_inputs, membership_challenge(expected_root), [1]),
    !verify(reference_inputs, membership_challenge(expected_root), [-1]),
  }
}

test mpf_chunked_proof_v1_rejects_a_root_that_is_not_the_challenged_root() {
  let steps = branch_steps(32)
  let reference_inputs = publish(steps)
  !verify(
    reference_inputs,
    membership_challenge(membership_root(0, branch_steps(31))),
    indices_of(reference_inputs),
  )
}

test mpf_chunked_proof_v1_rejects_a_wrong_target_key() {
  let steps = branch_steps(32)
  let challenge = ProofChallengeDatum {
    ..membership_challenge(membership_root(0, steps)),
    target_key: absent_neighbour_key,
  }
  let reference_inputs = publish(steps)
  !verify(reference_inputs, challenge, indices_of(reference_inputs))
}

test mpf_chunked_proof_v1_rejects_a_wrong_target_value_hash() {
  let steps = branch_steps(32)
  let challenge = ProofChallengeDatum {
    ..membership_challenge(membership_root(0, steps)),
    target_value_hash: absent_neighbour_value_hash,
  }
  let reference_inputs = publish(steps)
  !verify(reference_inputs, challenge, indices_of(reference_inputs))
}

test mpf_chunked_proof_v1_rejects_a_present_key_as_non_membership() {
  let steps = branch_steps(17)
  let membership_steps = list.concat(steps, [])
  let reference_inputs = publish(membership_steps)
  // The very proof that establishes membership must not pass as an absence
  // witness against the same root: its walk has no divergent terminal.
  !verify(
    reference_inputs,
    non_membership_challenge(membership_root(0, membership_steps)),
    indices_of(reference_inputs),
  )
}

test mpf_chunked_proof_v1_rejects_non_protocol_width_hash_fields() {
  let steps = branch_steps(3)
  let expected_root = membership_root(0, steps)
  let reference_inputs = publish(steps)
  let honest = membership_challenge(expected_root)
  and {
    mpf_chunked_proof_v1.challenge_datum_is_well_formed(honest),
    !mpf_chunked_proof_v1.challenge_datum_is_well_formed(
      ProofChallengeDatum { ..honest, proof_owner: target_key },
    ),
    !mpf_chunked_proof_v1.challenge_datum_is_well_formed(
      ProofChallengeDatum { ..honest, challenged_header_hash: target_key },
    ),
    !mpf_chunked_proof_v1.challenge_datum_is_well_formed(
      ProofChallengeDatum { ..honest, target_key: proof_owner },
    ),
    !mpf_chunked_proof_v1.challenge_datum_is_well_formed(
      ProofChallengeDatum { ..honest, target_value_hash: proof_owner },
    ),
    !mpf_chunked_proof_v1.challenge_datum_is_well_formed(
      ProofChallengeDatum { ..honest, expected_root: proof_owner },
    ),
    !verify(
      reference_inputs,
      ProofChallengeDatum { ..honest, target_key: proof_owner },
      indices_of(reference_inputs),
    ),
  }
}

test mpf_chunked_proof_v1_rejects_membership_against_an_empty_trie() {
  let steps = branch_steps(3)
  let expected_root = membership_root(0, steps)
  let reference_inputs = publish(steps)
  let challenge = ProofChallengeDatum {
    ..membership_challenge(expected_root),
    expected_leaf_count: 0,
  }
  and {
    !mpf_chunked_proof_v1.challenge_datum_is_well_formed(challenge),
    !verify(reference_inputs, challenge, indices_of(reference_inputs)),
    !mpf_chunked_proof_v1.challenge_datum_is_well_formed(
      ProofChallengeDatum { ..challenge, expected_leaf_count: -1 },
    ),
  }
}
