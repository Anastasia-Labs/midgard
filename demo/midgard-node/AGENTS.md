# Operator Runtime

These rules apply to `demo/midgard-node`. Paths below are relative to the
repository root.

## Command Placement

Keep the operator binary focused on operating the node. Add test, demo,
benchmark, and acceptance commands to `demo/midgard-node-tools`, which has its
own binary. Do not register them in `demo/midgard-node/src/index.ts`.

## Source Consumers

The tooling package bundles imports from the node's source exports. Use relative
imports within the node and its declared `midgard-node/<subpath>` exports from
consumers. Node subpaths do not provide per-module `dist` entrypoints; preserve
source consumption when changing these package boundaries.
