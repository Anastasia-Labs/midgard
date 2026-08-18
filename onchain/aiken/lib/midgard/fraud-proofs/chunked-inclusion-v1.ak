//// Published-chunk carriage of a fault-proof step's MPF opening (issue #545).
////
//// ## Why this exists
////
//// The four foundational proof families (double-spend, no-input,
//// invalid-range, zero-input) open the challenged block's `transactions_root`
//// — and, for no-input, its `prev_utxos_root` and `transactions_root` again —
//// by carrying the MPF proof inside the step transaction itself. That route
//// pays ~276 complete-signed-transaction bytes per forced branch level, so the
//// preserved 16,384-byte envelope is exhausted at branch level 21..23 and an
//// adversary grinding sibling keys at 2^4 work per level can censor the honest
//// prover for as little as 2^84 work (`Q1X-F5`).
////
//// `midgard/mpf_chunked_proof_v1` already removes a proof from the
//// transaction that verifies it: the same canonical `mpf.ProofStep` list is
//// published across bounded, inert UTxOs and the verifying transaction names
//// their order as indices into `tx.reference_inputs`. This module is the one
//// place where that mechanism meets a fault-proof step. It is deliberately
//// thin at the interface and complete underneath: two predicates, membership
//// and non-membership, each taking the carriage the step redeemer carried and
//// the commitment the step already authenticated.
////
//// ## What a step gains, and what it does not
////
//// The step's marginal cost per proof level falls to one small integer and one
//// reference input. Nothing else about a step changes: the root is still the
//// one the challenged header commits, the key is still the step's own
//// canonical encoding, and the terminal is still the exact membership or
//// canonical-absence witness. A chunk whose steps do not reconstruct that root
//// simply fails here, which is why publication needs no validator (see the
//// module header of `midgard/mpf_chunked_proof_v1`).
////
//// ## Why the direct route stays
////
//// Small proofs — every non-adversarial block — are cheaper carried in the
//// redeemer than published: the direct route needs one transaction where this
//// one needs two. Both routes end in a verified opening against the same
//// authenticated root, so which one a prover takes is normally the prover's
//// business and neither weakens the other.
////
//// ## Where the two routes are **not** interchangeable today (#606/#608)
////
//// Stated here rather than left implied, because the parity claim above used
//// to be unqualified and is not:
////
//// **Non-membership over an empty Midgard trie is provable on the direct
//// route and not on this one.** Midgard commits `env.empty_merkle_tree_root =
//// blake2b_256("")` for an empty MPF, while the library reconstructs its empty
//// trie as `merkling.null_hash` (32 zero bytes), so the sentinel has to be
//// translated before the reconstructed root is compared. #606's rider (owner
//// ruling on #608, 2026-08-15) authorised that translation at exactly one
//// placement — `validators/pexcludes.ak`, the merkelized verifier the *direct*
//// route withdraws through. The published-chunk route terminates instead in
//// `midgard/mpf_proof_v1.does_not_have` (`lib/midgard/mpf-proof-v1.ak:328-344`,
//// reached from `delegated_chunk_non_membership` below and from
//// `validators/mpf-chunked-verify.ak`), which still compares
//// `do_excluding(path, 0, proof)` against the caller's root with no sentinel
//// branch — so for `root == env.empty_merkle_tree_root` it returns `False` for
//// the empty proof, which is the only witness an empty ledger can produce.
////
//// Consequence, liveness only and narrow: a `no-input` or
//// `no-reference-input` step-03 whose `prev_utxos_root` is the empty sentinel
//// — i.e. a first block over a genesis confirmed state — cannot be proved
//// through published-chunk carriage. Nothing false is accepted either way.
//// The residual is **unrepaired and out of #606's authorised scope**: this is
//// a library used by validators, so translating it here would move
//// `plutus.json` and the applied-identity cascade beyond the five-validator
//// movement the rider authorised. The repair is DECIDED (owner approval
//// 2026-08-17) and executes inside #510's single freeze-event regeneration,
//// together with the matching `insert_root` fold-in. #582 is NOT this
//// repair's gate — that earlier claim was self-referential, tracing back to
//// this module's own defect note.

use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{Input, Redeemer, ScriptPurpose}
use midgard/common/utils.{get_unique_withdraw_redeemer}
use midgard/mpf_chunked_proof_v1
use midgard/mpf_proof_v1

/// Names the publication UTxOs carrying one opening, and their proof order, as
/// indices into `tx.reference_inputs`.
///
/// This is `mpf_chunked_proof_v1.FinalizeProofRedeemer` in a step redeemer's
/// clothing: the same single field, so the wire encoding of a carriage is the
/// wire encoding the challenge thread already uses.
pub type PublishedProofCarriage {
  ordered_chunk_reference_input_indices: List<Int>,
}

/// Which terminal a claim demands.
pub type ProofTerminal {
  Membership
  NonMembership
}

/// The redeemer of the merkelized `mpf_chunked_verify` withdraw validator, and
/// the exact value a delegating step requires to be present in it.
///
/// Nothing here is trusted by the verifier: the root, key and terminal are the
/// step's own authenticated evidence, and the step is what refuses a claim that
/// is not its own.
pub type ChunkedProofClaim {
  mode: ProofTerminal,
  merkle_root: ByteArray,
  key_bytes: ByteArray,
  value_hash: ByteArray,
  ordered_chunk_reference_input_indices: List<Int>,
}

/// The 32-byte digest a non-membership claim carries in the `value_hash` slot,
/// which the terminal does not use. Fixed so the claim has one encoding.
pub const absent_value_hash: ByteArray =
  #"0000000000000000000000000000000000000000000000000000000000000000"

/// Delegated membership: the step requires the merkelized verifier to have been
/// invoked in this transaction, exactly once, on exactly this claim.
///
/// The step contributes the binding (an authenticated root, its own canonical
/// key, the value it has already decoded, and the chunk order its redeemer
/// named); the verifier contributes the walk. This is the same division of
/// labour, and the same `get_unique_withdraw_redeemer` discipline, that
/// `plutarch_phas_raw` already uses for the redeemer-carried route.
pub fn delegated_chunk_membership(
  verifier_script_hash: ScriptHash,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  carriage: PublishedProofCarriage,
  merkle_root: ByteArray,
  key_bytes: ByteArray,
  value_bytes: ByteArray,
) -> Bool {
  let PublishedProofCarriage { ordered_chunk_reference_input_indices } = carriage
  expect claim: ChunkedProofClaim = get_unique_withdraw_redeemer(
    redeemers,
    verifier_script_hash,
  )
  claim == ChunkedProofClaim {
    mode: Membership,
    merkle_root,
    key_bytes,
    value_hash: blake2b_256(value_bytes),
    ordered_chunk_reference_input_indices,
  }
}

/// Delegated non-membership. The `value_hash` slot is the fixed
/// `absent_value_hash`, because an absence claim has no value.
pub fn delegated_chunk_non_membership(
  verifier_script_hash: ScriptHash,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  carriage: PublishedProofCarriage,
  merkle_root: ByteArray,
  key_bytes: ByteArray,
) -> Bool {
  let PublishedProofCarriage { ordered_chunk_reference_input_indices } = carriage
  expect claim: ChunkedProofClaim = get_unique_withdraw_redeemer(
    redeemers,
    verifier_script_hash,
  )
  claim == ChunkedProofClaim {
    mode: NonMembership,
    merkle_root,
    key_bytes,
    value_hash: absent_value_hash,
    ordered_chunk_reference_input_indices,
  }
}

/// Reassembles the published proof, or fails closed.
///
/// Every bound the publication mechanism names is applied here and nowhere
/// else: bounded, in-range, duplicate-free indices; a strictly decoded, well
/// formed chunk datum per index; and the protocol-wide 64-step ceiling on the
/// concatenation.
fn published_proof(
  reference_inputs: List<Input>,
  carriage: PublishedProofCarriage,
) -> Option<mpf.Proof> {
  let PublishedProofCarriage { ordered_chunk_reference_input_indices: indices }
    = carriage
  if mpf_chunked_proof_v1.chunk_indices_are_well_formed(
    indices,
    list.length(reference_inputs),
  ) {
    when
      mpf_chunked_proof_v1.concatenate_published_steps(
        reference_inputs,
        indices,
      )
    is {
      None -> None
      Some(proof) ->
        if mpf_proof_v1.proof_has_at_most_steps(
          proof,
          mpf_chunked_proof_v1.maximum_total_proof_step_count,
        ) {
          Some(proof)
        } else {
          None
        }
    }
  } else {
    None
  }
}

/// Membership of `key_bytes -> value_bytes` under `merkle_root`, proved by the
/// published chunks the carriage names.
///
/// The value's preimage is already in the step transaction (a step cannot act
/// on a transaction it has not decoded), so it is hashed here and the walk runs
/// against the digest. Equivalent to `mpf_proof_v1.has` on the same proof.
pub fn published_chunk_membership(
  reference_inputs: List<Input>,
  carriage: PublishedProofCarriage,
  merkle_root: ByteArray,
  key_bytes: ByteArray,
  value_bytes: ByteArray,
) -> Bool {
  published_chunk_membership_by_digest(
    reference_inputs,
    carriage,
    merkle_root,
    key_bytes,
    blake2b_256(value_bytes),
  )
}

/// The same membership check taking the value's digest, which is what a
/// delegated claim carries: the merkelized verifier never sees the preimage.
pub fn published_chunk_membership_by_digest(
  reference_inputs: List<Input>,
  carriage: PublishedProofCarriage,
  merkle_root: ByteArray,
  key_bytes: ByteArray,
  value_hash: ByteArray,
) -> Bool {
  when published_proof(reference_inputs, carriage) is {
    None -> False
    Some(proof) ->
      mpf_proof_v1.has_value_hash(merkle_root, key_bytes, value_hash, proof)
  }
}

/// Absence of `key_bytes` under `merkle_root`, proved by the published chunks
/// the carriage names. The canonical absence terminal is required, exactly as
/// on the direct route.
pub fn published_chunk_non_membership(
  reference_inputs: List<Input>,
  carriage: PublishedProofCarriage,
  merkle_root: ByteArray,
  key_bytes: ByteArray,
) -> Bool {
  when published_proof(reference_inputs, carriage) is {
    None -> False
    Some(proof) -> mpf_proof_v1.does_not_have(merkle_root, key_bytes, proof)
  }
}
