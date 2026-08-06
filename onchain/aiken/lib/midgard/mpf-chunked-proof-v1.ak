//// Published proof chunks with atomic final verification (issue #541).
////
//// ## Why this exists
////
//// A fault-proof step that carries its MPF membership proof inside the final
//// transaction's redeemer pays ~276 signed-transaction bytes per proof level.
//// Against the preserved 16,384-byte envelope that exhausts at branch level
//// 21..23 depending on the family, so an adversary who grinds sibling keys at
//// 2^4 work per level can push a proof past the envelope for as little as
//// 2^84 work, and a 2^128 adversary reaches level 32. The proof no longer
//// fits; the honest prover is censored.
////
//// This module removes the proof from the final transaction entirely. A large
//// proof is *published* across one or more bounded UTxOs whose inline datum
//// carries nothing but a list of proof steps. The finalizing transaction
//// references those UTxOs, names their order with indices into
//// `tx.reference_inputs`, concatenates their steps and verifies the complete
//// proof atomically. The final transaction's marginal cost per proof level
//// falls to zero: it carries one small integer and one reference input per
//// chunk.
////
//// ## Why publication needs no validator
////
//// A chunk UTxO is inert data at an arbitrary address. It holds no protocol
//// token, authorises nothing, and is never mutated. The finalizing validator
//// trusts a chunk's *content* only, never its provenance: a chunk whose steps
//// do not reconstruct the challenged root simply fails verification. Because
//// there is no mutable intermediate proof state, there is nothing an adversary
//// can poison between publication and finalization — the entire proof is
//// checked in one transaction or not at all. Publishing chunks and submitting
//// the finalizing transaction are therefore both permissionless, while the
//// reward stays bound to the challenge's `proof_owner`.
////
//// ## Size derivation (all bounds pinned by tests in the sibling test module)
////
//// - A `Branch` step is the largest step shape and serialises to 139..140
////   bytes of Plutus data (139 measured by the Q1x fixture; +1 when `skip`
////   needs a two-byte integer). `Fork` (<= 74) and `Leaf` (<= 73) are cheaper.
//// - `maximum_chunk_proof_step_count = 16` therefore bounds a chunk datum at
////   16 * 140 + 6 = 2,246 bytes, which `maximum_chunk_datum_byte_count`
////   rounds to 2,304. That is ~14% of the 16,384-byte envelope (measured
////   usable inline-datum payload: 16,173 bytes), so a publication transaction
////   can carry up to six full chunks and still fit.
//// - `maximum_total_proof_step_count` is the protocol-wide 64 (an MPF path is
////   64 nibbles, so no well-formed proof can be deeper). 64 steps therefore
////   need at most 4 chunks; the depth-32 proof a 2^128 adversary can grind
////   needs 2.
//// - `maximum_chunk_count = 8` keeps the finalizing transaction's
////   reference-input list small while tolerating partially packed chunks.

use aiken/cbor
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/ledger_state
use midgard/mpf_proof_v1
use midgard/transition_trace

// ## Bounds

/// Width of every Blake2b-256 field carried by a challenge datum.
pub const digest_byte_count: Int = 32

/// Width of a Midgard V1 header hash (`ledger_state.HeaderHash` is `H28`, a
/// Blake2b-224 digest). The specification called for 32 bytes on every hash
/// field; the header hash is the one field whose width is fixed by the
/// existing state queue, so it is bound to the real 28-byte width instead of
/// a value no honest prover could ever supply.
pub const header_hash_byte_count: Int = 28

/// Maximum number of proof steps a single publication UTxO may carry.
pub const maximum_chunk_proof_step_count: Int = 16

/// Maximum serialised size of a single publication UTxO's inline datum.
pub const maximum_chunk_datum_byte_count: Int = 2_304

/// Protocol-wide ceiling on the steps of one proof, however it is carried.
pub const maximum_total_proof_step_count: Int =
  mpf_proof_v1.maximum_proof_step_count

/// Maximum number of publication UTxOs one finalizing transaction may consume.
pub const maximum_chunk_count: Int = 8

// ## Types

/// The repo's canonical MPF step representation. Chunked carriage deliberately
/// introduces no parallel encoding: these are the very same steps that no
/// longer fit into a single transaction's redeemer.
pub type MpfProofStep =
  mpf.ProofStep

pub type ProofMode {
  Membership
  NonMembership
}

/// The challenge thread's state. Authenticated once, at initialization, against
/// the challenged block header; never mutated afterwards.
pub type ProofChallengeDatum {
  proof_owner: VerificationKeyHash,
  challenged_header_hash: ByteArray,
  target_key: ByteArray,
  target_value_hash: ByteArray,
  expected_root: ByteArray,
  expected_leaf_count: Int,
  mode: ProofMode,
}

/// A published proof chunk. Nothing but steps: no owner, no ordering claim, no
/// link to a challenge. Order comes from the finalizing redeemer.
pub type ProofChunkDatum {
  proof_steps: List<MpfProofStep>,
}

/// Names the publication UTxOs and their proof order as indices into
/// `tx.reference_inputs`.
pub type FinalizeProofRedeemer {
  ordered_chunk_reference_input_indices: List<Int>,
}

/// Minting redeemer of the challenge thread token.
pub type ChallengeMintRedeemer {
  /// Open a unique challenge thread. The token's asset name is the nonce
  /// derived from the consumed output reference at `nonce_input_index`.
  InitChallenge {
    nonce_input_index: Int,
    challenge_output_index: Int,
    hub_oracle_ref_input_index: Int,
    challenged_block_ref_input_index: Int,
    challenged_root_domain: transition_trace.RootDomain,
  }
  /// Retire a challenge thread. Only reachable while the thread's own UTxO is
  /// being spent, so the spending handler carries the real conditions.
  BurnChallenge { challenge_input_index: Int }
}

// ## Header binding

/// The `(counted root, leaf count)` pair a challenged header commits for one
/// root domain. `commit_counted_root` already binds a domain's raw MPF root to
/// its leaf count, so a challenge's `expected_root`/`expected_leaf_count` pair
/// is authenticated against the header by reproducing that commitment.
pub fn header_counted_commitment(
  header: ledger_state.HeaderV1,
  domain: transition_trace.RootDomain,
) -> (ByteArray, Int) {
  when domain is {
    transition_trace.WithdrawalsRootDomain ->
      (header.withdrawals_root, header.withdrawal_count)
    transition_trace.ForcedTransactionsV1RootDomain ->
      (header.forced_transactions_root, header.forced_transaction_count)
    transition_trace.TransactionsV1RootDomain ->
      (header.transactions_root, header.l2_transaction_count)
    transition_trace.DepositsRootDomain ->
      (header.deposits_root, header.deposit_count)
    transition_trace.TransitionTraceRootDomain ->
      (header.transition_trace_root, header.transition_step_count)
    transition_trace.EventToStepRootDomain ->
      (header.event_to_step_root, header.total_event_count)
    transition_trace.ValidationTracesRootDomain ->
      (header.validation_traces_root, header.validation_trace_count)
  }
}

/// The challenge's target commitment is exactly what the challenged header
/// commits for the named domain.
pub fn challenge_matches_header(
  challenge: ProofChallengeDatum,
  header: ledger_state.HeaderV1,
  header_hash: ByteArray,
  domain: transition_trace.RootDomain,
) -> Bool {
  let (counted_root, leaf_count) = header_counted_commitment(header, domain)
  and {
    challenge.challenged_header_hash == header_hash,
    transition_trace.verify_root_count_proof(
      transition_trace.RootCountProof {
        domain,
        root: counted_root,
        phas_root: challenge.expected_root,
        count: challenge.expected_leaf_count,
      },
      domain,
      counted_root,
      leaf_count,
    ),
  }
}

// ## Named invariants

/// Every hash field has its exact protocol width, and a membership challenge
/// names a non-empty trie (nothing is a member of an empty trie).
pub fn challenge_datum_is_well_formed(challenge: ProofChallengeDatum) -> Bool {
  and {
    bytearray.length(challenge.proof_owner) == header_hash_byte_count,
    bytearray.length(challenge.challenged_header_hash) == header_hash_byte_count,
    bytearray.length(challenge.target_key) == digest_byte_count,
    bytearray.length(challenge.target_value_hash) == digest_byte_count,
    bytearray.length(challenge.expected_root) == digest_byte_count,
    challenge.expected_leaf_count >= 0,
    when challenge.mode is {
      Membership -> challenge.expected_leaf_count > 0
      NonMembership -> True
    },
  }
}

/// A publication UTxO carries at least one step, no more than the per-chunk
/// step bound, and serialises within the per-chunk byte bound.
pub fn chunk_datum_is_well_formed(chunk: ProofChunkDatum) -> Bool {
  let step_count = list.length(chunk.proof_steps)
  and {
    step_count >= 1,
    step_count <= maximum_chunk_proof_step_count,
    bytearray.length(cbor.serialise(chunk)) <= maximum_chunk_datum_byte_count,
  }
}

fn indices_are_distinct(indices: List<Int>) -> Bool {
  when indices is {
    [] -> True
    [index, ..rest] -> and {
        !list.has(rest, index),
        indices_are_distinct(rest),
      }
  }
}

/// The chunk order is bounded, in range and free of duplicates. Duplicates are
/// rejected by name rather than left to fail the proof walk.
///
/// An empty order is allowed: the zero-step proof is the legitimate shape for a
/// one-leaf trie, and like every other shape it still has to reconstruct the
/// challenged root.
pub fn chunk_indices_are_well_formed(
  indices: List<Int>,
  reference_input_count: Int,
) -> Bool {
  let chunk_count = list.length(indices)
  and {
    chunk_count <= maximum_chunk_count,
    list.all(indices, fn(index) { index >= 0 && index < reference_input_count }),
    indices_are_distinct(indices),
  }
}

/// Strictly decodes the inline datum of one referenced publication UTxO.
///
/// Fails closed (`None`) on an out-of-range index and on any datum that is not
/// inline. A datum that is inline but is not a `ProofChunkDatum` aborts the
/// script through the strict `expect`, which is the same rejection.
pub fn chunk_datum_at(
  reference_inputs: List<Input>,
  index: Int,
) -> Option<ProofChunkDatum> {
  when list.at(reference_inputs, index) is {
    Some(Input { output: Output { datum: InlineDatum(chunk_data), .. }, .. }) -> {
      expect chunk: ProofChunkDatum = chunk_data
      if chunk_datum_is_well_formed(chunk) {
        Some(chunk)
      } else {
        None
      }
    }
    _ -> None
  }
}

/// Selects the named publication UTxOs and concatenates their steps in the
/// order the redeemer gave.
pub fn concatenate_published_steps(
  reference_inputs: List<Input>,
  indices: List<Int>,
) -> Option<mpf.Proof> {
  when indices is {
    [] -> Some([])
    [index, ..rest] ->
      when chunk_datum_at(reference_inputs, index) is {
        None -> None
        Some(ProofChunkDatum { proof_steps }) ->
          when concatenate_published_steps(reference_inputs, rest) is {
            None -> None
            Some(remaining_steps) ->
              Some(list.concat(proof_steps, remaining_steps))
          }
      }
  }
}

/// Runs the complete MPF walk to the terminal the challenge's mode demands.
///
/// Both arms are total and fail-closed: they reject unless every step is
/// consumed and the exact membership terminal (target key at the challenged
/// value digest) or the canonical non-membership absence witness is reached.
/// A missing, reordered, substituted, duplicated or trailing step changes the
/// reconstructed root and is rejected here.
pub fn proof_reaches_challenged_terminal(
  challenge: ProofChallengeDatum,
  proof: mpf.Proof,
) -> Bool {
  when challenge.mode is {
    Membership ->
      mpf_proof_v1.has_value_hash(
        challenge.expected_root,
        challenge.target_key,
        challenge.target_value_hash,
        proof,
      )
    NonMembership ->
      mpf_proof_v1.does_not_have(
        challenge.expected_root,
        challenge.target_key,
        proof,
      )
  }
}

/// The full published-chunk verification: every named invariant, in order.
pub fn verify_published_proof(
  reference_inputs: List<Input>,
  challenge: ProofChallengeDatum,
  redeemer: FinalizeProofRedeemer,
) -> Bool {
  let FinalizeProofRedeemer { ordered_chunk_reference_input_indices: indices } = redeemer
  and {
    challenge_datum_is_well_formed(challenge),
    chunk_indices_are_well_formed(indices, list.length(reference_inputs)),
    when concatenate_published_steps(reference_inputs, indices) is {
      None -> False
      Some(proof) -> and {
          mpf_proof_v1.proof_has_at_most_steps(
            proof,
            maximum_total_proof_step_count,
          ),
          proof_reaches_challenged_terminal(challenge, proof),
        }
    },
  }
}
