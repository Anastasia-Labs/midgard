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

    let _excluded = mpf.insert(mpf.from_root(root), key, "", proof)

    True
  }

  else(_) {
    fail
  }
}
