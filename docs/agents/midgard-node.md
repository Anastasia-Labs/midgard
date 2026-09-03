# Midgard Node Compatibility

This file applies to `demo/midgard-node`.

Backward compatibility is never a goal for `demo/midgard-node`. Midgard has not
launched, so prefer the current canonical production design over support for
older in-repo behavior.

Do not add:

- Compatibility modes
- Fallback paths
- Alias lookups
- Dual-ID behavior
- Legacy-format support
- Operator toggles intended to preserve old behavior
- Migration shims, unless the user explicitly asks for an isolated migration
  tool

Tests should prove the intended current canonical behavior, not compatibility
with abandoned pre-launch shapes.

## Test tooling lives in `demo/midgard-node-tools`

The e2e step runner and service supervisor, run finalizer, state-correction
acceptance, stress-wallet and corpus tooling, bounded L2 stress harness, and
the Phase 4 local-devnet acceptance gate (plus its devnet assets) are a
separate package with its own binary, `midgard-node-tools`. Do not register a
test, demo, or benchmark command in `demo/midgard-node/src/index.ts`; add it
to the tooling package instead.

The tooling package compiles `midgard-node` from source: the node's `exports`
map carries only the `midgard-source` condition (`midgard-node/<subpath>` and
`midgard-node/tests/<subpath>`), the tooling bundle inlines what it imports,
and the node publishes no per-module dist. Neither package has a `@/` alias;
use relative specifiers inside a package and `midgard-node/<subpath>` from the
tooling package. The operator suite's one reach back into the tooling package
(`database.test.ts`) is a dev-only workspace cycle, like validation and
fault-proofs.
