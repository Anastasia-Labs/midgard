# TypeScript Workspace

These rules apply to the packages under `demo`. Paths below are relative to the
repository root.

## Package Boundaries

- Import workspace packages by name (`@al-ft/midgard-core/hex`,
  `da-committee-node/config`), never through `../<package>/src` or `dist`;
  add a workspace dependency and an `exports` entry for a missing subpath.
  Blind spot: `no-restricted-imports` sees only static `import`/`export`
  declarations, so a dynamic `import()`, a `require()` or a path handed to a
  subprocess passes the lint. [eslint: no-restricted-imports]
- Follow declared exports and package build prerequisites when running compiled
  entrypoints or subprocesses; source-based checks do not verify built
  artifacts. [review]

## When Relevant

- L1 transaction construction, evaluation, or submission: read
  `docs/agents/transaction-finalization.md` before changing or running that flow.
- Operator runtime or CLI placement: read `demo/midgard-node/AGENTS.md` before
  changing the node or adding operational commands.
- Live acceptance, deployment, or recovery diagnosis: use
  `.agents/skills/midgard-e2e-acceptance/SKILL.md` before operating services.
- Devnet setup or configuration: read `demo/midgard-node-tools/devnet/AGENTS.md`.
