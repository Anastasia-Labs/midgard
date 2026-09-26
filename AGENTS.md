# Midgard Agent Guide

Always-loaded context only. Keep this file narrow: project facts the code
cannot tell you, repo-wide guardrails, and pitfalls observed across sessions.
Put directory-specific rules in nested `AGENTS.md` files and shared task guidance
in `docs/agents/*` or a skill. Blind spot: the line budget caps length, not
relevance. [script: scripts/agents/check-enforcement-tags.mjs]

Each rule ends with one tag naming what enforces it; the vocabulary is in
[docs/agents/README.md](docs/agents/README.md).

## Project Context

Midgard is an optimistic rollup for Cardano. It runs across on-chain
Cardano validators and off-chain node/runtime services, with Plutus V3/Aiken
contracts, TypeScript SDK/demo/e2e packages, and a technical specification.

Treat it as production-grade L2 infrastructure. Faulty state transitions,
timing assumptions, resets, or compatibility shortcuts can corrupt protocol
state, break liveness, or put funds at risk.

Tradeoff order: correctness, safety, liveness, performance, convenience.

## Engineering Principles

- Grow the system in layers. Start from the smallest version that works end to
  end, and add each new capability on top of a product that already works.
  Never trade a working product for unfinished complexity. [review]
- Prefer established, well-maintained libraries when they reduce overall
  complexity or improve reliability. [review]
- Write clean code, with a focus on maintainability, clear names, direct
  control flow, and abstractions justified by current behavior. There are many
  human developers working on the codebase, and keeping it easy for them to
  work on is a top priority. [review]
- Ship architecture that supports the intended deployment. Temporary diagnostic
  instrumentation follows `docs/agents/production-l2.md`. [review]

## Repo Shape

- `onchain/aiken`: Plutus V3 contracts; read `onchain/aiken/AGENTS.md` for work
  in this tree.
- `demo`: pnpm TypeScript workspace for SDKs, node/runtime, manager/CLI, tests,
  benchmarks, and e2e tooling; read `demo/AGENTS.md` for work in this tree.
- `technical-spec`: protocol specification built through the root `Makefile`.
  `technical-spec/Lean4Midgard` is a git submodule (`rnd-midgard`) that is not
  initialised in a normal clone and that nothing builds.
- `offchain`: Haskell (Cabal, Nix flake) library `midgard-offchain` that builds
  the operator-directory transactions (registered, active and retired operator
  lists, scheduler, initialisation), with its own specs. No CI workflow builds
  or tests it.
- `onchain/plutarch`: legacy Plutarch Merkle Patricia Forestry helpers kept for
  reference; the Aiken tree is the on-chain implementation.
- `docs-site`: Fumadocs/Next.js documentation site with its own pnpm version;
  Docs Site CI checks its links, builds it and typechecks it.
- `docs/agents`: progressive guidance; open only the relevant domain doc.

Use the declared repo toolchain: pnpm/Node in `demo`, Aiken in
`onchain/aiken`, and `make` for the spec.

## Always-On Rules

- Strict behavior is the default. Demo, benchmark, migration, or compatibility
  shortcuts must be explicit, isolated, and unavailable by default. [review]
- Before mainnet launch, undeployed versions have no compatibility contract:
  replace obsolete schemas and APIs in place. Keep versioning seams for
  post-launch upgrades; preserve or migrate only versions that actually shipped.
  This does not authorize resetting an existing deployment. Before resetting
  durable state or redeploying, read `docs/agents/state-reset.md`. [review]
- Prefer bumping a pinned dependency to a release that fixes the problem over
  writing a local compatibility shim or workaround for the old version.
  [review]
- Preserve user work: check dirty state, do not clean or revert unrelated
  changes. [review]
- Before finalizing changes, run the required checks for each kind of change
  you made, listed in [docs/agents/verification.md](docs/agents/verification.md),
  and the narrow checks that prove touched behavior. Report exactly what ran; a
  smoke test does not replace a required acceptance check. [review]

## When Relevant

- Safety or architecture tradeoffs: read `docs/agents/production-l2.md` before
  choosing behavior that affects protocol integrity or recovery.
- Validator changes or deployment parameter application: read
  `docs/agents/contracts.md` before editing contracts, builders, or fixtures.
- Naming or renaming: read `docs/agents/naming-and-versioning.md`.
- Before the first commit in a clone: ensure repository hooks are installed
  with `bash .githooks/install`; every worktree then runs its own branch's copy.
  Required verification still applies when hooks skip a file or tool. Blind
  spot: with `core.fileMode=false`, git shows no change when a hook file loses
  its executable bit, and then skips that hook without a word; re-running the
  installer restores the bit. Entering the `offchain` Nix shell can repoint
  `core.hooksPath`; re-run the installer after it. [review]
