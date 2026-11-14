# GOAL_SPEC digest

A one-page map of [GOAL_SPEC.md](GOAL_SPEC.md), the authoritative execution
specification for the Canonical V1 Goal. This page only routes; where the two
differ, the full specification wins. An agent acting as the Goal's active
parent still follows the full-read rule in §4.1 of the specification.

## The Goal in three outcomes

1. **G1, Canonical V1 capability:** Canonical V1 is independently verifiable
   on Cardano L1 at the Cardano transaction-capability floor.
2. **G2, state correction:** every enabled fund-safety or state-correction
   rule has a sound, reachable, operational fault-proof path.
3. **G4, autonomous watcher:** an independent production watcher reconstructs,
   detects, proves and completes correction without trusting an operator's
   private state.

The Goal is complete only when every `AC-*` criterion in §12 passes at
`releaseCommit` and the direct verification in §13 proves it.

## Where to read, by task

| Task                                                 | Sections                          |
| ---------------------------------------------------- | --------------------------------- |
| Any Goal work: authority, scope, invariants          | §0, §1, §2, §3                    |
| Starting or resuming a session; commits; the PR      | §4 (§4.1 first turn, §4.4 the PR) |
| Parallel workers and serialization-sensitive files   | §5                                |
| Ordering work and integration gates                  | §6, §11                           |
| Shared foundation packages                           | §7                                |
| Proof-item capability, resolvers, forced execution   | §8 (G1)                           |
| A proof family's closure or the correction lifecycle | §9 (G2)                           |
| The watcher package, indexing, DA, actuation         | §10 (G4)                          |
| Deciding whether something is done                   | §12, then §15                     |
| Which commands prove it; live evidence and storage   | §13                               |
| Something outside the repository blocks progress     | §14                               |

## Facts that are easy to miss

- The pull request base is `tx-validation`, confirmed by the owner on
  2026-09-25 (§4.4).
- The compact-tx per-field commitments are flat blake2b-256 hashes defined in
  `docs/spec/midgard-tx.md` (the 2026-08-08 amendment in §0).
- Aiken runs use the fork pinned in `.github/workflows/aiken-ci.yml`;
  `aiken.toml` records only the base version (§13.2).
- Graph navigation (Graphify) is owner-local and optional (§0.1).
- Readiness is tracked in `docs/public_testnet_readiness.md`.
