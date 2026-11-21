# Midgard Agent Guide

Always-loaded context only: project facts the code cannot tell you and
repo-wide guardrails. Directory rules go in a nested `AGENTS.md`, task guidance
in `docs/agents/*` or a skill. Blind spot: the line budget caps length, not
relevance. [ci: repo-tools-ci/Check agent rule enforcement tags]

Each rule ends with one tag naming what enforces it; the vocabulary is in
[docs/agents/README.md](docs/agents/README.md).

## Project Context

Midgard is an optimistic rollup for Cardano: Plutus V3/Aiken validators on
chain, a TypeScript node, watcher and SDK off chain, and a technical
specification. It is production L2 infrastructure.

Two kinds of failure get opposite treatment:

- **Protocol failure**: an invalid state transition accepted, a fraud left
  unproven, funds or a bond lost, liveness broken. Not traded for anything.
- **Operational cost**: a wasted fee, a transaction built again, a read
  repeated after a restart. Accepted; it justifies no mechanism.

Tradeoff order: correctness, safety, liveness, simplicity, performance,
convenience.

## Engineering Principles

- **Complexity carries the burden of proof.** A guard, retry, hold, stored
  record, status, alert, config knob or layer stays only when it names a
  realistic trigger and a protocol failure it prevents that nothing else
  already prevents. "Cheap to keep", "defence in depth" and "just in case"
  justify nothing; a check that repeats one the ledger, a validator or an
  earlier layer makes is a defect. Read `docs/agents/production-l2.md` before
  adding any such mechanism. [review]
- **Prefer the change that deletes.** The best feature deepens an existing
  module and leaves less code than before; a change that adds a layer says
  what it could not reuse. Report net lines added and removed. [review]
- Grow the system in layers: the smallest version that works end to end
  first, each capability on top of a product that already works. [review]
- Prefer an established, maintained library, or a bump of a pinned
  dependency to the release that fixes the problem, over local code or a
  compatibility shim. [review]
- Clear names, direct control flow, abstractions justified by current
  behavior. Many human developers work here; keeping the code easy for them
  is a top priority. [review]

## Repo Shape

- `onchain/aiken`: Plutus V3 contracts; read `onchain/aiken/AGENTS.md` for work
  in this tree.
- `demo`: pnpm TypeScript workspace (SDKs, node, watcher, CLI, tests, e2e
  tooling); read `demo/AGENTS.md` for work in this tree.
- `technical-spec`: protocol specification, built by the root `Makefile`. Its
  `Lean4Midgard` submodule is not initialised in a normal clone; nothing
  builds it.
- `offchain`: Haskell library for operator-directory transactions. No CI
  workflow builds or tests it.
- `onchain/plutarch`: legacy reference; the Aiken tree is the implementation.
- `docs-site`: documentation site with its own pnpm version and CI.
- `docs/agents`: guidance by topic; open only the relevant doc.

Toolchain versions are in `docs/toolchain.md`.

## Always-On Rules

- Strict behavior is the default. Demo, benchmark, migration, or compatibility
  shortcuts must be explicit, isolated, and unavailable by default. [review]
- Before mainnet launch, undeployed versions have no compatibility contract:
  replace obsolete schemas and APIs in place, and preserve or migrate only
  versions that actually shipped. This does not authorize resetting an
  existing deployment; read `docs/agents/state-reset.md` before resetting
  durable state or redeploying. [review]
- Preserve user work: check dirty state, do not clean or revert unrelated
  changes. [review]
- Before finalizing changes, run the required checks for each kind of change
  you made, listed in [docs/agents/verification.md](docs/agents/verification.md),
  and report exactly what ran. Blind spot: the pre-push hook runs only
  preflight's fast slice, only where hooks are installed, and nothing checks
  what you report. [hook: pre-push]

## When Relevant

- Validator changes or deployment parameter application: read
  `docs/agents/contracts.md` before editing contracts, builders, or fixtures.
- Naming or renaming: read `docs/agents/naming-and-versioning.md`.
- Before the first commit in a clone, and whenever a hook stops firing or
  after entering the `offchain` Nix shell: run `bash .githooks/install`. The
  session-start doctor reports a broken hook but blocks nothing.
  [script: scripts/doctor.mjs]
