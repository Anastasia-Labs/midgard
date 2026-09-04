use aiken/builtin
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{nibble}
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use env
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}

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

    let trie = if root == env.empty_merkle_tree_root {
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
//
// Since #606 the surface has two arms, and **both** carry a positive, because
// a positive on only one of them leaves the other free to be mutated silently:
//
//   * `exclusion_accepts_genuine_non_membership` — the sentinel arm, an empty
//     Midgard ledger, which is the first-block path #608 found dead.
//   * `exclusion_accepts_non_membership_over_a_populated_trie` — the
//     `mpf.from_root` arm with a real multi-step proof, which is the arm every
//     block after the first takes.
//
// and one negative, `exclusion_rejects_unproven_root`, which keeps the
// sentinel translation from becoming a way to wave any root through.

const test_key =
  #"3131313131313131313131313131313131313131313131313131313131313131"

/// A root the prover offers no proof for. `excluding(key, [])` is the empty
/// trie's root, which is not this, so `mpf.insert` must reject it.
const unproven_root =
  #"1111111111111111111111111111111111111111111111111111111111111111"

/// A genuinely populated trie: the MPF library's own 29-element "fruits"
/// fixture with `apple[uid: 58]` removed, i.e. its `without_apple` root
/// (`build/packages/aiken-lang-merkle-patricia-forestry/lib/aiken/`
/// `merkle-patricia-forestry.tests.ak`). Borrowed rather than synthesised
/// because a genuine multi-step absence witness cannot be written by hand —
/// and the library's own `example_insert` selector proves this root and the
/// proof below are a real pair, so nothing here rests on this module's
/// arithmetic.
///
/// Key shape is irrelevant to what is being covered: this validator never
/// inspects the key, it hashes whatever ByteArray the redeemer names, so a
/// Midgard `encode_midgard_tx_input` key and a fruit name walk identically.
const populated_root =
  #"cb7812785c2f6b56e3dba69923e37625e94953257915d25f1041f43cefd5cb62"

/// The key absent from `populated_root`.
const absent_key = "apple[uid: 58]"

/// Three steps — a branch and two leaf neighbours — so the walk this proves is
/// the real recursion and not the empty-proof base case the sentinel arm
/// exercises.
const populated_absence_proof: mpf.Proof =
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

/// Honest acceptance on the **production** arm: a populated trie reached
/// through `mpf.from_root`, with a real three-step absence proof.
///
/// Every block after a genesis one carries a non-empty `prev_utxos_root`, so
/// this is the arm a real conviction walks; the sentinel arm above is the
/// first-block corner. Without this selector the `else` branch had no positive
/// at all after #606 re-pointed the acceptance test at the sentinel, and a
/// mutation that broke every production non-membership proof — collapsing
/// `mpf.from_root(root)` to `mpf.empty`, say — would have been silent under
/// `aiken check` while both remaining selectors stayed green.
test exclusion_accepts_non_membership_over_a_populated_trie() {
  run(redeemer_of(populated_root, absent_key, populated_absence_proof))
}

/// Regression for the discarded-binding defect. A well-shaped redeemer whose
/// proof does not reconstruct the claimed root must be rejected. While
/// `mpf.insert` was bound with `let _excluded`, its assertion was compiled
/// away and this returned `True` — non-membership was never proven.
test exclusion_rejects_unproven_root() fail {
  run(redeemer_of(unproven_root, test_key, []))
}

// ## No-input fixture witnesses (#582)
//
// The no-input step-03/step-04 selectors attach delegated `pexcludes` claims
// but never execute this validator — the blind spot that let the original
// dead non-membership check survive. These selectors close it: each one runs
// the withdraw handler on the *exact* redeemer entry the shared fixture
// builder produces for a step selector, so a fixture drifting into a witness
// shape production would refuse turns a test red here.

fn run_entry(
  entry: Pair<transaction.ScriptPurpose, transaction.Redeemer>,
) -> Bool {
  let Pair(_, redeemer) = entry
  run(redeemer)
}

/// The step-04 positive claim: the fixture block's genuine single-leaf raw
/// transactions root, the absent native tx id, and the real committed-leaf
/// absence witness. Accepted, so the fixture is witness-faithful.
test exclusion_accepts_the_no_input_step_04_fixture_claim() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  and {
    nibble(blake2b_256(fixture.absent_native_tx_id), 0) != nibble(
      blake2b_256(block.tx_id),
      0,
    ),
    run_entry(
      fixture.pexcludes_withdraw_redeemer_entry(
        block.transactions_phas_root,
        fixture.absent_native_tx_id,
        fixture.transactions_phas_absence_witness(block),
      ),
    ),
  }
}

/// The step-03 genesis-arm claim: the empty-trie sentinel root under the
/// canonical `MidgardTxInput` CBOR key, witnessed by the empty proof.
test exclusion_accepts_the_no_input_step_03_genesis_claim() {
  run_entry(
    fixture.pexcludes_withdraw_redeemer_entry(
      env.empty_merkle_tree_root,
      encode_midgard_tx_input(fixture.absent_midgard_tx_input()),
      [],
    ),
  )
}

/// The step-03 populated-arm claim: the shared single-leaf initial-ledger
/// trie with the real absence witness that reconstructs it.
test exclusion_accepts_the_no_input_step_03_populated_claim() {
  let key = encode_midgard_tx_input(fixture.absent_midgard_tx_input())
  let (root, witness) = fixture.absence_witness()
  and {
    nibble(blake2b_256(key), 0) != nibble(
      fixture.absence_witness_leaf_path(),
      0,
    ),
    run_entry(fixture.pexcludes_withdraw_redeemer_entry(root, key, witness)),
  }
}

/// The retired fixture shape (#582): an empty proof against a non-empty root
/// is not a non-membership witness, and the step fixtures carried exactly
/// this before the re-pin. It must stay rejected, so the suite catches a
/// regression in either direction.
test exclusion_rejects_the_retired_empty_proof_fixture_shape() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_entry(
    fixture.pexcludes_withdraw_redeemer_entry(
      block.transactions_phas_root,
      fixture.absent_native_tx_id,
      [],
    ),
  )
}
