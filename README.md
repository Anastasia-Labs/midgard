<p align="center">
  <img width="150px" src="technical-spec/images/midgard-icon-green.png" align="center" alt="Midgard Logo"/>
  <h1 align="center">Midgard</h1>
  <p align="center">Cardano's first optimistic rollup protocol</p>
</p>

Midgard contains the on-chain protocol, the off-chain SDK, demo node/runtime
implementations, and the LaTeX technical specification in a single repository.

## Repository Map

- `onchain/aiken`: primary Aiken validators, minting policies, and shared
  protocol libraries.
- `onchain/plutarch`: Plutarch Merkle membership/exclusion validators and their
  shared proof utilities.
- `demo/midgard-sdk`: off-chain TypeScript SDK for building Midgard
  transactions and decoding protocol data.
- `demo/midgard-node`: demo node runtime, HTTP server, worker loops, and
  integration tests.
- `demo/midgard-node-tools`: end-to-end, stress, and acceptance tooling that
  drives a node from the outside, kept out of the operator binary.
- `demo/midgard-watcher`: independent verifier and challenger service.
- `technical-spec`: normative protocol design target and diagrams. Its
  conformance status is tracked separately from the implementation.
- `docs`: [contributor references, decisions, and active plans](./docs/README.md).
  Release acceptance lives in `docs/public_testnet_readiness.md`.
- `docs-site`: documentation site covering the SDK, node, watchers, fault
  proofs, and on-chain validators.

## Documentation Guide

- [Contribution guidelines](./CONTRIBUTING.md): repository workflow, hooks, and
  review expectations.
- [Technical specification guide](./technical-spec/README.md): how to build and
  work on the LaTeX spec.
- [Midgard node guide](./demo/midgard-node/README.md): runtime setup,
  operational workflows, stress scripts, and test entrypoints.
- [Midgard SDK guide](./demo/midgard-sdk/README.md): packaging, conventions,
  and SDK module layout.
- [Documentation site](./docs-site/README.md): the full guides and reference,
  runnable locally with `pnpm --dir docs-site dev`. CI checks selected command,
  fiber, transaction-status, and inventory facts against their source files.
  These checks do not validate every prose claim or code example; review the
  affected documentation when implementation behavior changes.
- [Documentation policy](./docs/DOCUMENTATION_POLICY.md): source-of-truth,
  evidence, security-claim, and plan-lifecycle rules.
- [Canonical V1 consensus profile](./docs/consensus-profile-v1.md): exact
  versions, feature gates, immutable size bounds, and enforcement ownership.

## Build the On-Chain Code

Use the Aiken fork pinned in [.github/workflows/aiken-ci.yml](./.github/workflows/aiken-ci.yml).
The upstream version number alone does not identify the repository compiler.
With that compiler on `PATH`, build the demo/preprod environment:

```sh
cd onchain/aiken
aiken build --env testnet
```

## Technical Specification

Run:

```sh
make spec
```

Then open `technical-spec/midgard.pdf`.

## Demo Packages

The demo packages form one pnpm workspace under `demo/`:

```sh
cd demo
pnpm install
pnpm build
```

## Contributor guidelines

All contributors must enable the project's standardized git hooks:

```
bash .githooks/install
```

Take a look at the [contribution guidelines](./CONTRIBUTING.md) for more details.
