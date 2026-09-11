# Midgard Agent Guide

Always-loaded context only. Keep this file narrow: project facts the code
cannot tell you, repo-wide guardrails, and pitfalls observed across sessions.
Put directory-specific rules in nested `AGENTS.md` files and shared task guidance
in `docs/agents/*` or a skill.

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
  Never trade a working product for unfinished complexity.
- Prefer established, well-maintained libraries when they reduce overall
  complexity or improve reliability.
- Write maintainable code with clear names, direct control flow, and abstractions
  justified by current behavior.
- Ship architecture that supports the intended deployment. Temporary diagnostic
  instrumentation follows `docs/agents/production-l2.md`.

## Repo Shape

- `onchain/aiken`: Plutus V3 contracts; read `onchain/aiken/AGENTS.md` for work
  in this tree.
- `demo`: pnpm TypeScript workspace for SDKs, node/runtime, manager/CLI, tests,
  benchmarks, and e2e tooling; read `demo/AGENTS.md` for work in this tree.
- `technical-spec`: protocol specification built through the root `Makefile`.
- `docs/agents`: progressive guidance; open only the relevant domain doc.

Use the declared repo toolchain: pnpm/Node in `demo`, Aiken in
`onchain/aiken`, and `make` for the spec.

## Always-On Rules

- Strict behavior is the default. Demo, benchmark, migration, or compatibility
  shortcuts must be explicit, isolated, and unavailable by default.
- Before mainnet launch, undeployed versions have no compatibility contract:
  replace obsolete schemas and APIs in place. Keep versioning seams for
  post-launch upgrades; preserve or migrate only versions that actually shipped.
  This does not authorize resetting an existing deployment. Before resetting
  durable state or redeploying, read `docs/agents/state-reset.md`.
- Follow the current task and its named plans, review docs, commands, and
  verification requirements. Reconcile stale guidance with the user's current
  instructions and the deployment target. Keep changes inside the agreed scope.
- Preserve user work: check dirty state, do not clean or revert unrelated
  changes or generated artifacts.
- Before finalizing changes, run the named required checks and the narrow checks
  that prove touched behavior. Report exactly what ran; a smoke test does not
  replace a required acceptance check.
- When a path is explicitly protected, search only individually named tracked
  files. Shell wildcards expand before tool-level exclusions, so a later
  `--glob` or ignore rule does not protect an argument the shell already added.

## When Relevant

- Safety or architecture tradeoffs: read `docs/agents/production-l2.md` before
  choosing behavior that affects protocol integrity or recovery.
- Validator changes or deployment parameter application: read
  `docs/agents/contracts.md` before editing contracts, builders, or fixtures.
- Naming or renaming: read `docs/agents/naming-and-versioning.md`.
- Before the first commit in a clone: ensure repository hooks are installed
  with `bash .githooks/install`. Required verification still applies when hooks
  skip a file or tool.
