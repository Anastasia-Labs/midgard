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
- `onchain/plutarch`: Plutarch models, validators, and test helpers that mirror
  important protocol logic.
- `demo/midgard-sdk`: off-chain TypeScript SDK for building Midgard
  transactions and decoding protocol data.
- `demo/midgard-node`: demo node runtime, HTTP server, worker loops, stress
  scripts, and integration tests.
- `demo/midgard-manager`: CLI and transaction-generator tooling for operator
  workflows and demos.
- `technical-spec`: normative protocol design target and diagrams. Its
  conformance status is tracked separately from the implementation.
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
  runnable locally with `pnpm dev`. CI checks the pages against the source
  files they describe, so a PR that changes a documented CLI command, fiber,
  or transaction status also updates the matching page; the check's error
  message names both files.
- [Documentation policy](./docs/DOCUMENTATION_POLICY.md): source-of-truth,
  evidence, security-claim, and plan-lifecycle rules.
- [Canonical V1 consensus profile](./docs/consensus-profile-v1.md): exact
  versions, feature gates, immutable size bounds, and enforcement ownership.

## Build the On-Chain Code

Install [Aiken](https://aiken-lang.org/) and run:

```sh
cd onchain/aiken
aiken build
```

## Technical Specification

Run:

```sh
make spec
```

Then open `technical-spec/midgard.pdf`.

## Demo Packages

The demo packages are built independently inside `demo/`.

- `demo/midgard-sdk`: `pnpm install && pnpm build`
- `demo/midgard-node`: `pnpm install && pnpm build`
- `demo/midgard-manager`: `pnpm install && pnpm build`

## Contributor guidelines

All contributors must enable the project's standardized git hooks:

```
make enable-git-hooks
```

Take a look at the [contribution guidelines](./CONTRIBUTING.md) for more details.
