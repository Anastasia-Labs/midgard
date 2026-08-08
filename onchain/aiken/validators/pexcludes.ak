use aiken/builtin
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Credential}
use cardano/transaction.{Transaction}

/// Merkelized validator backing `midgard/common/utils.plutarch_pexcludes` and
/// `plutarch_pexcludes_raw`.
///
/// The exclusion redeemer is `[root, key, proof]` (no value, unlike the
/// membership counterpart in `phas.ak`). It proves the `key` is absent from the
/// trie rooted at `root`: `mpf.insert` asserts `excluding(key, proof) == root`,
/// failing if the key is already present or the proof is invalid. The resulting
/// root is irrelevant — a successful insert is itself the non-membership
/// witness, so the inserted value is left empty.
validator exclusion {
  withdraw(redeemer: Data, _own_credential: Credential, _tx: Transaction) {
    expect [root_data, key_data, proof_data] = builtin.un_list_data(redeemer)
    expect root: ByteArray = root_data
    expect key: ByteArray = key_data
    expect proof: mpf.Proof = proof_data

    expect _excluded = mpf.insert(mpf.from_root(root), key, "", proof)

    True
  }

  else(_) {
    fail
  }
}

// ## Selectors
//
// The proof walk itself is the mpf library's, and is covered there. What is
// exercised here is this validator's own surface: that a redeemer of the right
// *shape* is not by itself a non-membership witness. The assertion carrying
// that property lives inside `mpf.insert`, so it only reaches the compiled
// script if the call is bound with `expect` — `let` discards it and Aiken
// removes the call entirely.

const test_key =
  #"3131313131313131313131313131313131313131313131313131313131313131"

/// A root the prover offers no proof for. `excluding(key, [])` is the empty
/// trie's root, which is not this, so `mpf.insert` must reject it.
const unproven_root =
  #"1111111111111111111111111111111111111111111111111111111111111111"

fn redeemer_of(root: ByteArray, key: ByteArray, proof: mpf.Proof) -> Data {
  let proof_data: Data = proof
  let fields: List<Data> =
    [builtin.b_data(root), builtin.b_data(key), proof_data]
  let redeemer: Data = fields
  redeemer
}

fn run(redeemer: Data) -> Bool {
  exclusion.withdraw(
    redeemer,
    address.Script(#"90909090909090909090909090909090909090909090909090909090"),
    transaction.placeholder,
  )
}

/// Honest acceptance: the key is genuinely absent from the empty trie, and the
/// empty proof is the witness to that. Guards the fix against over-rejecting.
test exclusion_accepts_genuine_non_membership() {
  run(redeemer_of(mpf.root(mpf.empty), test_key, []))
}

/// Regression for the discarded-binding defect. A well-shaped redeemer whose
/// proof does not reconstruct the claimed root must be rejected. While
/// `mpf.insert` was bound with `let _excluded`, its assertion was compiled
/// away and this returned `True` — non-membership was never proven.
test exclusion_rejects_unproven_root() fail {
  run(redeemer_of(unproven_root, test_key, []))
}
