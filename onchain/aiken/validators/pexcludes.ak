use aiken/builtin
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use env

/// Merkelized validator backing `midgard/common/utils.plutarch_pexcludes` and
/// `plutarch_pexcludes_raw`.
///
/// The exclusion redeemer is `[root, key, proof]` (no value, unlike the
/// membership counterpart in `phas.ak`). It proves the `key` is absent from the
/// trie rooted at `root`: `mpf.insert` asserts `excluding(key, proof) == root`,
/// failing if the key is already present or the proof is invalid. The resulting
/// root is irrelevant — a successful insert is itself the non-membership
/// witness, so the inserted value is left empty.
///
/// The root is translated through the Midgard empty-trie sentinel before it
/// reaches the MPF library (#606, from #608's classification). Midgard commits
/// `env.empty_merkle_tree_root = blake2b_256("")` for every empty MPF —
/// `state-queue.ak` forces a first block over a genesis confirmed state to
/// carry exactly that value as `prev_utxos_root` — while the library
/// reconstructs its empty trie as its own `null_hash` (32 zero bytes). Without
/// the translation every non-membership claim against an empty ledger is
/// unsatisfiable and the validator crashes instead of answering; the same
/// adapter already guards the transition-trace path
/// (`transition-trace/proof.ak`, `ledger_trie`).
validator exclusion {
  withdraw(redeemer: Data, _own_credential: Credential, _tx: Transaction) {
    expect [root_data, key_data, proof_data] = builtin.un_list_data(redeemer)
    expect root: ByteArray = root_data
    expect key: ByteArray = key_data
    expect proof: mpf.Proof = proof_data

    let trie =
      if root == env.empty_merkle_tree_root {
        mpf.empty
      } else {
        mpf.from_root(root)
      }
    expect _excluded = mpf.insert(trie, key, "", proof)

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
  let fields: List<Data> = [
    builtin.b_data(root),
    builtin.b_data(key),
    proof_data,
  ]
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

/// Honest acceptance, stated against the root a Midgard header actually
/// carries: `env.empty_merkle_tree_root` is the canonical empty-trie sentinel
/// (`blake2b_256("")`), forced onto a first block over a genesis confirmed
/// state by `state-queue.ak`, and the empty proof is the only witness an empty
/// ledger can produce. This is the first-block empty-ledger path #608 found
/// dead; it also guards the sentinel translation against over-rejecting.
test exclusion_accepts_genuine_non_membership() {
  run(redeemer_of(env.empty_merkle_tree_root, test_key, []))
}

/// Regression for the discarded-binding defect. A well-shaped redeemer whose
/// proof does not reconstruct the claimed root must be rejected. While
/// `mpf.insert` was bound with `let _excluded`, its assertion was compiled
/// away and this returned `True` — non-membership was never proven.
test exclusion_rejects_unproven_root() fail {
  run(redeemer_of(unproven_root, test_key, []))
}
