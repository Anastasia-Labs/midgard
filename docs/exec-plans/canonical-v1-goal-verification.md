# Canonical V1 Goal verification

This document describes the repository commands required by `GOAL_SPEC.md`
§13. The executable command graph is
`docs/exec-plans/evidence/canonical-v1-goal-verification-plan-v1.json`; prose
does not override that plan, the closure manifest, or `GOAL_SPEC.md`.

Run commands from the repository root. The declared Node/pnpm environment is
the non-login Nix shell for `demo`; Aiken must be the exact compiler declared
by `onchain/aiken/aiken.toml`.

## Local commands

- `pnpm --dir demo run goal:verify:static` checks the protected dirty baseline,
  active workspace command policy, forbidden legacy and whole-item bindings,
  registry and current-truth artifacts, generated documentation, Git diff,
  compiler and blueprint identity, Aiken format/check/build, and serialized
  workspace format/lint/typecheck/build.
- `pnpm --dir demo run goal:verify:capability` runs the local CG1–CG5 evidence
  producers and consumers, including retained-DA/data-breadth suites and
  separately guarded exact Aiken boundary selectors.
- `pnpm --dir demo run goal:verify:fault-proofs` runs QG1/QG2 reconciliation,
  proof tooling, SDK, and node correction suites.
- `pnpm --dir demo run goal:verify:watcher` runs the watcher dependency,
  build/type/lint/format, and WG1 test surfaces.
- `pnpm --dir demo run goal:verify:local` runs the preceding four commands
  serially and stops at the first nonzero result.

Each phase ends by requiring its exact closure-manifest acceptance-criterion
group to be `PASS` with bound file evidence. A passing inventory verifier does
not turn an open criterion into a pass.

## State-changing Preprod acceptance

`pnpm --dir demo run goal:accept:testnet` is the only state-changing Goal
command. It requires:

- explicit `MIDGARD_GOAL_ACCEPT_TESTNET=YES`;
- `NETWORK=preprod`, `L1_PROVIDER=Kupmios`, and no provider failover;
- explicit local Kupo/Ogmios, wallet, DA submitter, and PostgreSQL environment
  names required by the E2E acceptance skill;
- a current passing E2E runbook preflight; and
- the exact C80–C87, Q57/QG3, and W45–W46/WG2 orchestrator.

It rejects Mainnet and refuses to substitute a narrower legacy acceptance
flow. Secrets and raw runtime state are never closure artifacts; the
orchestrator must retain immutable redacted evidence and hashes.

## Evidence and release commands

- `pnpm --dir demo run goal:verify:evidence` verifies the canonical closure
  manifest without submitting transactions. It requires final revision and
  release identity, parameter/blueprint/validator/deployment/fixture bindings,
  successful command results, all 35 exact `AC-*` entries at `PASS`, a clean
  secret scan, baseline-relative cleanliness, and a reproduced SHA-256 release
  digest.
- `pnpm --dir demo run goal:verify:all` runs `goal:verify:local` and then
  `goal:verify:evidence`. Missing, stale, mismatched, incomplete, or
  wrong-revision Preprod evidence is a failure.

The in-progress closure manifest is intentionally valid as a schema and
current-tree inventory while release mode remains nonzero. It must never be
marked `BOUND` or populated with `PASS` criteria until final-tree evidence
exists.
