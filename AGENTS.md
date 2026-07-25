# Midgard Agent Guide

Always-loaded context only. Keep this file narrow: project facts the code
cannot tell you, repo-wide guardrails, and pitfalls observed across sessions.
Move task-triggered rules to `docs/agents/*` or a skill.

## Project Context

Midgard is Cardano's first optimistic rollup protocol. It runs across on-chain
Cardano validators and off-chain node/runtime services, with Plutus V3/Aiken
contracts, TypeScript SDK/demo/e2e packages, and a technical specification.

Treat it as production-grade L2 infrastructure. Faulty state transitions,
timing assumptions, resets, or compatibility shortcuts can corrupt protocol
state, break liveness, or put funds at risk.

## North Star

Build a strict, auditable, production-ready rollup. Prefer solutions that
preserve protocol semantics, deterministic operation, explicit recovery paths,
and evidence another engineer can verify.

Tradeoff order: correctness, safety, liveness, performance, convenience.

## Repo Shape

- `onchain/aiken`: Plutus V3 contracts.
- `demo`: pnpm TypeScript workspace for SDKs, node/runtime, manager/CLI, tests,
  benchmarks, and e2e tooling.
- `technical-spec`: protocol specification built through the root `Makefile`.
- `docs/agents`: progressive guidance; open only the relevant domain doc.

Use the declared repo toolchain: pnpm/Node in `demo`, Aiken in
`onchain/aiken`, and `make` for the spec. Demo, preprod, and e2e deployment
work defaults to the Aiken `testnet` environment unless the task explicitly
targets another environment.

## Always-On Rules

- Strict behavior is the default. Demo, benchmark, migration, or compatibility
  shortcuts must be explicit, isolated, and unavailable by default.
- Before mainnet launch, undeployed versions have no compatibility contract:
  replace V1 and database schemas in place, remove obsolete branches, and
  wipe/redeploy development state instead of adding compatibility layers or
  migrations. Keep versioning seams for post-launch upgrades, but preserve or
  migrate only versions that actually shipped.
- Named plan docs, review docs, commands, and verification surfaces are the
  source of truth before improvising.
- Preserve user work: check dirty state, do not clean or revert unrelated
  changes, and keep patches scoped to the request.
- Before finalizing changes, run the narrow checks that prove the touched
  behavior and report exactly what ran.

## Observed Pitfalls

- Plan work has drifted into nearby reliability fixes. Stay inside the named
  boundary.
- Dirty worktrees and generated artifacts have been mistaken for cleanup
  targets. Leave unrelated state alone.
- Loose smoke tests have replaced plan-requested checks. Run named checks
  first.
- Demo or benchmark behavior has leaked into defaults. Keep production
  semantics strict.

## When Relevant

Open `docs/agents/production-l2.md`, `state-reset.md`,
`transaction-finalization.md`, `midgard-node.md`, or `README.md` only when the
task enters that domain.
