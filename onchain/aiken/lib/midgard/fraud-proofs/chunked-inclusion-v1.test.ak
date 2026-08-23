//// Guards for the fault-proof step seam onto published-chunk carriage
//// (issue #545). One attributed test per named invariant of
//// `midgard/fraud_proofs/chunked_inclusion_v1`.
////
//// The library has no prover, so proofs are synthesised and the root they
//// prove is reconstructed by mirroring the MPF fold — the same discipline
//// `midgard/mpf_chunked_proof_v1.test` uses, and the same fixtures the family
//// selectors drive, so a bound proved here is the bound the steps enforce.

use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, suffix}
use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, OutputReference}
use env
use midgard/fraud_proofs/chunked_inclusion_v1.{PublishedProofCarriage}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/mpf_chunked_proof_v1

const target_key =
  #"3131313131313131313131313131313131313131313131313131313131313131"

const target_value = #"a1b2c3d4"

const absent_leaf_value_hash =
  #"6666666666666666666666666666666666666666666666666666666666666666"

fn carriage(indices: List<Int>) -> PublishedProofCarriage {
  PublishedProofCarriage { ordered_chunk_reference_input_indices: indices }
}

fn membership(
  reference_inputs: List<Input>,
  root: ByteArray,
  indices: List<Int>,
) -> Bool {
  chunked_inclusion_v1.published_chunk_membership(
    reference_inputs,
    carriage(indices),
    root,
    target_key,
    target_value,
  )
}

fn non_membership(
  reference_inputs: List<Input>,
  root: ByteArray,
  indices: List<Int>,
) -> Bool {
  chunked_inclusion_v1.published_chunk_non_membership(
    reference_inputs,
    carriage(indices),
    root,
    target_key,
  )
}

fn deep_proof(levels: Int) -> mpf.Proof {
  fixture.adversarial_branch_steps(levels)
}

fn deep_root(steps: mpf.Proof) -> ByteArray {
  fixture.adversarial_membership_root(target_key, target_value, steps)
}

fn publish(steps: mpf.Proof) -> List<Input> {
  fixture.proof_chunk_reference_inputs(steps)
}

fn all_indices(reference_inputs: List<Input>) -> List<Int> {
  list.indexed_map(reference_inputs, fn(index, _input) { index })
}

fn absence_witness() -> (ByteArray, mpf.Proof) {
  let leaf_path = blake2b_256(#"a15e17")
  (
    combine(suffix(leaf_path, 0), absent_leaf_value_hash),
    [mpf.Leaf { skip: 0, key: leaf_path, value: absent_leaf_value_hash }],
  )
}

// ## Honest acceptance

/// A 22-level proof — past the depth the redeemer-carried route can fit inside
/// the 16,384-byte envelope — is reassembled from two publication UTxOs and
/// verifies against the root it proves.
test chunked_inclusion_v1_verifies_a_deep_published_membership_proof() {
  let steps = deep_proof(22)
  let published = publish(steps)
  and {
    list.length(published) == 2,
    membership(published, deep_root(steps), all_indices(published)),
  }
}

/// The 64-step protocol ceiling is reachable, and needs four chunks.
test chunked_inclusion_v1_verifies_the_protocol_maximum_step_count() {
  let steps = deep_proof(mpf_chunked_proof_v1.maximum_total_proof_step_count)
  let published = publish(steps)
  and {
    list.length(published) == 4,
    membership(published, deep_root(steps), all_indices(published)),
  }
}

/// A canonical absence witness carried by a published chunk.
test chunked_inclusion_v1_verifies_a_published_non_membership_proof() {
  let (root, witness) = absence_witness()
  let published = publish(witness)
  and {
    // The witness really is a divergence at the target's first nibble.
    nibble(blake2b_256(target_key), 0) != nibble(blake2b_256(#"a15e17"), 0),
    non_membership(published, root, [0]),
  }
}

// ## Named rejections

/// A missing chunk changes the reconstructed root.
test chunked_inclusion_v1_rejects_a_withheld_chunk() {
  let steps = deep_proof(22)
  let published = publish(steps)
  !membership(published, deep_root(steps), [0])
}

/// Chunk order is proof order: swapping two chunks is a different proof.
test chunked_inclusion_v1_rejects_reordered_chunks() {
  let steps = deep_proof(22)
  let published = publish(steps)
  !membership(published, deep_root(steps), [1, 0])
}

/// Duplicates are rejected by name rather than left to fail the walk.
test chunked_inclusion_v1_rejects_a_duplicated_chunk_index() {
  let steps = deep_proof(22)
  let published = publish(steps)
  !membership(published, deep_root(steps), [0, 0])
}

/// An index outside the reference-input list fails closed.
test chunked_inclusion_v1_rejects_an_out_of_range_chunk_index() {
  let steps = deep_proof(22)
  let published = publish(steps)
  !membership(published, deep_root(steps), [0, 9])
}

/// More chunks than the publication bound allows.
test chunked_inclusion_v1_rejects_more_chunks_than_the_chunk_bound() {
  let steps = deep_proof(22)
  let published = publish(steps)
  !membership(
    published,
    deep_root(steps),
    list.range(0, mpf_chunked_proof_v1.maximum_chunk_count),
  )
}

/// The value is bound: the same proof proves nothing about another value.
test chunked_inclusion_v1_rejects_a_substituted_value() {
  let steps = deep_proof(22)
  let published = publish(steps)
  !chunked_inclusion_v1.published_chunk_membership(
    published,
    carriage(all_indices(published)),
    deep_root(steps),
    target_key,
    #"d4c3b2a1",
  )
}

/// The key is bound.
test chunked_inclusion_v1_rejects_a_substituted_key() {
  let steps = deep_proof(22)
  let published = publish(steps)
  !chunked_inclusion_v1.published_chunk_membership(
    published,
    carriage(all_indices(published)),
    deep_root(steps),
    #"3232323232323232323232323232323232323232323232323232323232323232",
    target_value,
  )
}

/// The root is bound.
test chunked_inclusion_v1_rejects_a_substituted_root() {
  let steps = deep_proof(22)
  let published = publish(steps)
  !membership(published, blake2b_256(#"0badc0de"), all_indices(published))
}

/// A referenced UTxO whose datum is not inline carries no proof.
test chunked_inclusion_v1_rejects_a_non_inline_chunk_datum() {
  let steps = deep_proof(4)
  let bare = Input {
    output_reference: OutputReference {
      transaction_id: blake2b_256(#"c0de"),
      output_index: 0,
    },
    output: Output {
      address: address.from_script(fixture.step_script_hash),
      value: assets.from_lovelace(2_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
  !membership([bare], deep_root(steps), [0])
}

/// An empty chunk is rejected by the publication bound rather than treated as
/// a zero-step proof.
test chunked_inclusion_v1_rejects_an_empty_chunk() {
  let empty_chunk_data: Data = mpf_chunked_proof_v1.ProofChunkDatum {
    proof_steps: [],
  }
  let empty_chunk = Input {
    output_reference: OutputReference {
      transaction_id: blake2b_256(#"c0de"),
      output_index: 0,
    },
    output: Output {
      address: address.from_script(fixture.step_script_hash),
      value: assets.from_lovelace(2_000_000),
      datum: InlineDatum(empty_chunk_data),
      reference_script: None,
    },
  }
  !membership([empty_chunk], deep_root(deep_proof(0)), [0])
}

/// Naming no chunk at all leaves the zero-step proof, which cannot reconstruct
/// a deep root.
test chunked_inclusion_v1_rejects_an_empty_chunk_order_against_a_deep_root() {
  let steps = deep_proof(22)
  !membership(publish(steps), deep_root(steps), [])
}

/// A key that the trie really holds cannot be proved absent.
test chunked_inclusion_v1_rejects_a_present_key_as_non_membership() {
  let steps = deep_proof(22)
  let published = publish(steps)
  !non_membership(published, deep_root(steps), all_indices(published))
}

// ## Empty-trie sentinel translation (#625)
//
// The three selectors `validators/pexcludes.ak` carries for the direct route,
// re-run through this route's terminal. The sentinel positive is the
// route-parity gap this module's header used to record as open: until #625
// `mpf_proof_v1.does_not_have` compared against the untranslated root, so a
// first block over a genesis confirmed state was unprovable through
// published-chunk carriage while the direct route proved it. Both arms of the
// translation carry a positive, because a positive on only one of them leaves
// the other free to be mutated silently.

/// A root the prover offers no proof for.
const unproven_root =
  #"1111111111111111111111111111111111111111111111111111111111111111"

/// A genuinely populated trie, borrowed verbatim from
/// `validators/pexcludes.ak`: the MPF library's own 29-element "fruits"
/// fixture with `apple[uid: 58]` removed, i.e. its `without_apple` root, and
/// the three-step absence witness the library's own `example_insert` selector
/// proves is its real pair.
const fruits_root =
  #"cb7812785c2f6b56e3dba69923e37625e94953257915d25f1041f43cefd5cb62"

/// The key absent from `fruits_root`.
const fruits_absent_key = "apple[uid: 58]"

const fruits_absence_proof: mpf.Proof =
  [
    mpf.Branch {
      skip: 0,
      neighbors: #"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c3490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d79519b8cdfbd053e5a86cf28a781debae71638cd77f85aad4b88869373d9dcfd",
    },
    mpf.Leaf {
      skip: 0,
      key: #"5cddcd30a0a388cf6feb3fd6e112c96e9daf23e3a9c8a334e7044650471aaa9e",
      value: #"f429821ddf89c9df3c7fbb5aa6fadb6c246d75ceede53173ce59d70dde375d14",
    },
    mpf.Leaf {
      skip: 0,
      key: #"5e7ccfedd44c90423b191ecca1eb21dfbac865d561bace8c0f3e94ae7edf4440",
      value: #"7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911",
    },
  ]

/// The sentinel arm. An empty Midgard trie commits `env.empty_merkle_tree_root`
/// and can produce only the zero-step witness, which on this route is a
/// carriage naming no chunk at all.
test chunked_inclusion_v1_verifies_non_membership_against_the_empty_sentinel() {
  non_membership([], env.empty_merkle_tree_root, [])
}

/// The production arm: a populated trie reached through `mpf.from_root`, with
/// a real three-step absence witness published as one chunk.
test chunked_inclusion_v1_verifies_non_membership_over_a_populated_trie() {
  let published = publish(fruits_absence_proof)
  and {
    list.length(published) == 1,
    chunked_inclusion_v1.published_chunk_non_membership(
      published,
      carriage([0]),
      fruits_root,
      fruits_absent_key,
    ),
  }
}

/// The translation stays a translation: an unproven root is still refused.
test chunked_inclusion_v1_rejects_an_unproven_root_as_non_membership() {
  !non_membership([], unproven_root, [])
}
