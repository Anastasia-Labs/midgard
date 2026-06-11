# Midgard Agent Guide

Midgard is Cardano's first optimistic rollup protocol, containing on-chain
contracts, off-chain SDKs, demo node/runtime packages, and the technical
specification.

## Essentials

- Treat this repository as a production-grade L2: prioritize correctness,
  safety, liveness, performance, then convenience.
- Default to strict, auditable behavior; do not make benchmark, demo, or
  compatibility shortcuts the default path.
- Use `pnpm` for the TypeScript workspace in `demo/`
  (`pnpm@9.15.4`, Node.js `>=18`).
- Common demo workspace commands:
  - Build: `cd demo && pnpm run build`
  - Typecheck: `cd demo && pnpm run typecheck`
  - Test: `cd demo && pnpm run test`
  - Lint: `cd demo && pnpm run lint`
  - Format check: `cd demo && pnpm run format-check`
- Build Aiken contracts from `onchain/aiken` with `aiken build`.
- Build the technical specification with `make spec`.

## Detailed Guidance

- [Production L2 principles](docs/agents/production-l2.md)
- [State reset and redeploy rules](docs/agents/state-reset.md)
- [Transaction finalization](docs/agents/transaction-finalization.md)
- [Midgard node compatibility](docs/agents/midgard-node.md)
- [Progressive disclosure structure](docs/agents/README.md)
- [Instructions flagged for deletion or consolidation](docs/agents/deletion-candidates.md)
