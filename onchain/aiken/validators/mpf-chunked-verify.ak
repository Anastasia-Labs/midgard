//// Merkelized verifier for published-chunk MPF proof carriage (issue #545).
////
//// ## Why this is a withdraw script and not step-local code
////
//// The proof walk is the expensive part of the carriage, in script bytes as
//// well as in execution units. Compiled into every fault-proof step that can
//// open a trie, it added ~3.7 kB to each step's spending script — which is
//// carried by the step transaction whichever route the prover takes, so it
//// would have made the *redeemer-carried* route markedly more exhaustible
//// (finding Q1X-F5's measured ceiling fell from branch level 21..23 to 8..9).
//// Remediating one route by degrading the other is not a remediation.
////
//// So the walk lives here, in one merkelized validator shared by every family,
//// exactly as `phas.ak` and `pexcludes.ak` already host the single-transaction
//// walks. A step on the direct route does not attach this script at all and is
//// byte-for-byte the size it was; a step on the chunked route attaches it in
//// place of `phas`/`pexcludes` and carries no proof.
////
//// ## What it proves
////
//// The claim in the redeemer names a root, a key, a terminal and the order of
//// the publication UTxOs. This validator reassembles the proof from those
//// UTxOs' inline datums — reading `tx.reference_inputs`, which it has and the
//// delegating step also has — and runs the complete walk. It authenticates
//// nothing about the claim itself: binding the root to a challenged header and
//// the key to the step's own evidence is the delegating step's job, and the
//// step does it by requiring this exact claim in this exact redeemer.

use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, suffix}
use cardano/address.{Credential}
use cardano/transaction.{Input, Transaction}
use midgard/fraud_proofs/chunked_inclusion_v1.{
  ChunkedProofClaim, Membership, NonMembership, PublishedProofCarriage,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

validator verify {
  withdraw(
    redeemer: ChunkedProofClaim,
    _own_credential: Credential,
    tx: Transaction,
  ) {
    let Transaction { reference_inputs, .. } = tx
    let
        ChunkedProofClaim {
          mode,
          merkle_root,
          key_bytes,
          value_hash,
          ordered_chunk_reference_input_indices,
        }
      = redeemer
    let carriage = PublishedProofCarriage {
      ordered_chunk_reference_input_indices,
    }
    when mode is {
      Membership ->
        chunked_inclusion_v1.published_chunk_membership_by_digest(
          reference_inputs,
          carriage,
          merkle_root,
          key_bytes,
          value_hash,
        )
      NonMembership ->
        chunked_inclusion_v1.published_chunk_non_membership(
          reference_inputs,
          carriage,
          merkle_root,
          key_bytes,
        )
    }
  }

  else(_) {
    fail
  }
}

// ## Selectors
//
// The walk's own invariants are covered guard by guard in
// `midgard/fraud_proofs/chunked_inclusion_v1.test`, which drives the very
// functions this validator delegates to. What is exercised here is the
// validator's own surface: the two terminals it dispatches on, at the depth the
// carriage exists for, over publication UTxOs shaped exactly as the fixture
// builds them.

const target_key =
  #"3131313131313131313131313131313131313131313131313131313131313131"

const target_value = #"a1b2c3d4"

const absent_leaf_value_hash =
  #"6666666666666666666666666666666666666666666666666666666666666666"

fn run(claim: ChunkedProofClaim, chunks: List<Input>) -> Bool {
  verify.withdraw(
    claim,
    address.Script(#"90909090909090909090909090909090909090909090909090909090"),
    Transaction { ..transaction.placeholder, reference_inputs: chunks },
  )
}

fn deep_membership_claim(
  merkle_root: ByteArray,
  indices: List<Int>,
) -> ChunkedProofClaim {
  ChunkedProofClaim {
    mode: Membership,
    merkle_root,
    key_bytes: target_key,
    value_hash: blake2b_256(target_value),
    ordered_chunk_reference_input_indices: indices,
  }
}

fn deep_proof() -> mpf.Proof {
  fixture.adversarial_branch_steps(22)
}

fn deep_root() -> ByteArray {
  fixture.adversarial_membership_root(target_key, target_value, deep_proof())
}

/// Honest acceptance: a 22-level proof — past the depth the redeemer-carried
/// route can fit inside the 16,384-byte envelope — reassembled from two
/// publication UTxOs.
test mpf_chunked_verify_accepts_a_deep_published_membership_proof() {
  let chunks = fixture.proof_chunk_reference_inputs(deep_proof())
  and {
    list.length(chunks) == 2,
    run(deep_membership_claim(deep_root(), [0, 1]), chunks),
  }
}

/// A withheld chunk changes the reconstructed root.
test mpf_chunked_verify_rejects_a_withheld_chunk() {
  let chunks = fixture.proof_chunk_reference_inputs(deep_proof())
  !run(deep_membership_claim(deep_root(), [0]), chunks)
}

/// Chunk order is proof order.
test mpf_chunked_verify_rejects_reordered_chunks() {
  let chunks = fixture.proof_chunk_reference_inputs(deep_proof())
  !run(deep_membership_claim(deep_root(), [1, 0]), chunks)
}

/// The claimed root is what the walk must reconstruct.
test mpf_chunked_verify_rejects_a_substituted_root() {
  let chunks = fixture.proof_chunk_reference_inputs(deep_proof())
  !run(deep_membership_claim(blake2b_256(#"0badc0de"), [0, 1]), chunks)
}

/// The claimed value digest is bound.
test mpf_chunked_verify_rejects_a_substituted_value_digest() {
  let chunks = fixture.proof_chunk_reference_inputs(deep_proof())
  !run(
    ChunkedProofClaim {
      ..deep_membership_claim(deep_root(), [0, 1]),
      value_hash: blake2b_256(#"d4c3b2a1"),
    },
    chunks,
  )
}

/// A canonical absence witness, dispatched through the non-membership terminal.
test mpf_chunked_verify_accepts_a_published_absence_witness() {
  let leaf_path = blake2b_256(#"a15e17")
  let witness = [
    mpf.Leaf { skip: 0, key: leaf_path, value: absent_leaf_value_hash },
  ]
  and {
    nibble(blake2b_256(target_key), 0) != nibble(leaf_path, 0),
    run(
      ChunkedProofClaim {
        mode: NonMembership,
        merkle_root: combine(suffix(leaf_path, 0), absent_leaf_value_hash),
        key_bytes: target_key,
        value_hash: chunked_inclusion_v1.absent_value_hash,
        ordered_chunk_reference_input_indices: [0],
      },
      fixture.proof_chunk_reference_inputs(witness),
    ),
  }
}

/// A key the trie really holds cannot be proved absent through this validator
/// either.
test mpf_chunked_verify_rejects_a_present_key_as_absent() {
  let chunks = fixture.proof_chunk_reference_inputs(deep_proof())
  !run(
    ChunkedProofClaim {
      ..deep_membership_claim(deep_root(), [0, 1]),
      mode: NonMembership,
      value_hash: chunked_inclusion_v1.absent_value_hash,
    },
    chunks,
  )
}
