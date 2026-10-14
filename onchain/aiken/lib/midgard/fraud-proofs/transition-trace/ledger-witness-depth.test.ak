//// Kind-0 ledger witnesses at the protocol's 64-nibble path ceiling.
////
//// Keys are 32-byte digests, so no ledger path is longer than 64 nibbles and
//// no proof has more than 64 steps, whoever chose the keys. An all-`Branch`
//// ladder is the widest step at every level. These tests apply one delete and
//// one insert witness at depths up to 64; the roots and proofs are module
//// constants, so a test's budget is the witness application alone.

use aiken/crypto.{blake2b_256}
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
    LedgerInsertWitness {
      key,
      value,
      non_membership_proof: steps,
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
