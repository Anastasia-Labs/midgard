//// Kind-0 ledger witnesses at the protocol's 64-nibble path ceiling.
////
//// Keys are 32-byte digests, so no ledger path is longer than 64 nibbles and
//// no proof has more than 64 steps, whoever chose the keys. An all-`Branch`
//// ladder is the widest step at every level. These tests apply one delete and
//// one insert witness at depths up to 64; the roots and proofs are module
//// constants, so a test's budget is the witness application alone.
////
//// An insert witness carries one proof. Its paired fold authenticates the
//// key's absence from the predecessor root and derives the successor, so the
//// tests below also pin what that single proof refuses.

use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use env
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/transition_trace/proof.{
  LedgerDeleteWitness, LedgerInsertWitness, apply_delete_witness,
  apply_insert_witness,
}

const key =
  #"825820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa190000"

const value =
  #"a2005839016b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b"

const delete_4 = fixture.adversarial_branch_steps(4)

const delete_16 = fixture.adversarial_branch_steps(16)

const delete_32 = fixture.adversarial_branch_steps(32)

const delete_64 = fixture.adversarial_branch_steps(64)

const insert_4 = fixture.adversarial_non_membership_steps(3)

const insert_48 = fixture.adversarial_non_membership_steps(47)

const insert_32 = fixture.adversarial_non_membership_steps(31)

const insert_64 = fixture.adversarial_non_membership_steps(63)

const delete_root_4 = fixture.adversarial_membership_root(key, value, delete_4)

const delete_root_16 =
  fixture.adversarial_membership_root(key, value, delete_16)

const delete_root_32 =
  fixture.adversarial_membership_root(key, value, delete_32)

const delete_root_64 =
  fixture.adversarial_membership_root(key, value, delete_64)

const insert_root_4 = fixture.adversarial_non_membership_root(key, insert_4)

const insert_root_48 = fixture.adversarial_non_membership_root(key, insert_48)

const insert_root_32 = fixture.adversarial_non_membership_root(key, insert_32)

const insert_root_64 = fixture.adversarial_non_membership_root(key, insert_64)

fn deleted(root: ByteArray, steps) -> ByteArray {
  apply_delete_witness(
    root,
    LedgerDeleteWitness { key, value, opening: #"", delete_proof: steps },
  )
}

fn inserted(root: ByteArray, steps) -> ByteArray {
  apply_insert_witness(
    root,
    LedgerInsertWitness { key, value, insert_proof: steps },
  )
}

fn inserted_as(
  root: ByteArray,
  inserted_key: ByteArray,
  inserted_value: ByteArray,
  steps,
) -> ByteArray {
  apply_insert_witness(
    root,
    LedgerInsertWitness {
      key: inserted_key,
      value: inserted_value,
      insert_proof: steps,
    },
  )
}

test delete_at_depth_4() {
  deleted(delete_root_4, delete_4) != delete_root_4
}

test delete_at_depth_16() {
  deleted(delete_root_16, delete_16) != delete_root_16
}

test delete_at_depth_32() {
  deleted(delete_root_32, delete_32) != delete_root_32
}

test delete_at_depth_64() {
  deleted(delete_root_64, delete_64) != delete_root_64
}

test insert_at_depth_4() {
  inserted(insert_root_4, insert_4) != insert_root_4
}

test insert_at_depth_48() {
  inserted(insert_root_48, insert_48) != insert_root_48
}

test insert_at_depth_32() {
  inserted(insert_root_32, insert_32) != insert_root_32
}

test insert_at_depth_64() {
  inserted(insert_root_64, insert_64) != insert_root_64
}

test digest_key_paths_are_64_nibbles() {
  blake2b_256(key) != key
}

const other_key =
  #"825820bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb190000"

const other_value =
  #"a2005839016c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c"

// (1) A key already in the ledger cannot be inserted again, neither with the
// proof of its own membership nor with an absence-shaped proof.

test insert_of_a_present_key_with_its_membership_proof_is_refused() fail {
  inserted_as(delete_root_4, key, other_value, delete_4) != delete_root_4
}

test insert_of_a_present_key_with_an_absence_proof_is_refused() fail {
  inserted_as(delete_root_4, key, value, insert_4) != delete_root_4
}

test insert_of_a_present_key_at_depth_64_is_refused() fail {
  inserted_as(delete_root_64, key, value, delete_64) != delete_root_64
}

// (2) The proof must authenticate the root it is applied to.

test insert_proof_of_another_root_is_refused() fail {
  inserted(insert_root_4, insert_64) != insert_root_4
}

test insert_proof_against_a_deeper_root_is_refused() fail {
  inserted(insert_root_64, insert_32) != insert_root_64
}

test insert_into_the_empty_ledger_with_a_non_empty_proof_is_refused() fail {
  inserted(env.empty_merkle_tree_root, insert_4) != env.empty_merkle_tree_root
}

// (3) One pre-state and one proof: the successor for the proven key verifies,
// the successor for any other key does not.

test insert_successor_for_the_proven_key_verifies() {
  let successor = inserted(insert_root_64, insert_64)
  successor != insert_root_64 && successor == inserted(
    insert_root_64,
    insert_64,
  )
}

test insert_successor_for_another_key_is_refused() fail {
  inserted_as(insert_root_64, other_key, value, insert_64) != insert_root_64
}

test insert_successors_differ_by_value() {
  inserted_as(insert_root_32, key, value, insert_32) != inserted_as(
    insert_root_32,
    key,
    other_value,
    insert_32,
  )
}

// (4) The depth-64 insert witness, as carried in a step redeemer, and its
// application budget (`insert_at_depth_64` above).

const insert_witness_64_bytes =
  bytearray.length(
    cbor.serialise(LedgerInsertWitness { key, value, insert_proof: insert_64 }),
  )

test insert_witness_at_depth_64_fits_a_step_transaction() {
  trace @"insert witness bytes at depth 64": insert_witness_64_bytes
  insert_witness_64_bytes <= 9_000
}
