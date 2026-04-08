# Midgard Off-Chain SDK

TypeScript SDK for building, decoding, and authenticating Midgard off-chain
state and transactions.

This package is the shared protocol-facing library used by `midgard-node`,
tests, and other local tooling in this repository. It wraps Lucid Evolution
types and transaction builders with Midgard-specific data schemas, state-machine
helpers, and transaction-construction utilities.

## What This Package Is For

Use `@al-ft/midgard-sdk` when you need to:

- build Midgard protocol transactions off chain,
- decode or authenticate Midgard state UTxOs,
- work with typed datums, redeemers, and protocol parameters,
- share one consistent off-chain protocol surface across node/runtime code and
  tests.

The package is not just a bag of helpers. It is the repository's canonical
off-chain protocol interface.

## Public Surface

The package's public exports are assembled in
[`src/index.ts`](./src/index.ts). At the top level it exposes:

- foundational helpers from `common`, `cbor`, `constants`,
  `protocol-parameters`, `ledger-state`, and `linked-list`,
- operator lifecycle modules: `registered-operators`, `active-operators`,
  `retired-operators`,
- state-machine modules: `hub-oracle`, `scheduler`, `state-queue`,
  `initialization`,
- user-event modules: `user-events/deposit`, `user-events/withdrawal`,
  `user-events/tx-order`,
- fraud-proof modules: `fraud-proof/catalogue`,
  `fraud-proof/computation-threads`, and `fraud-proof/tokens`.

For callers inside this repository, the common import style is:

```ts
import * as SDK from "@al-ft/midgard-sdk";
```

## API Conventions

The SDK follows a few conventions that matter when adding or consuming APIs:

- Functions suffixed with `Program` return `Effect.Effect<...>` values.
- Promise-style wrappers usually delegate to those Effect programs rather than
  duplicating logic.
- Names are intended to be globally unique even when modules are organized by
  protocol area.
- Transaction builders often expose "incomplete" variants that return a
  `TxBuilder` before `.complete(...)`, plus higher-level helpers that produce a
  `TxSignBuilder`.

This is deliberate. The codebase needs deterministic, composable transaction
fragments first, and convenience wrappers second.

## Build and Packaging

### Local Development

If you are working inside this repository:

```sh
corepack enable
cd demo/midgard-sdk
pnpm install
pnpm build
```

`pnpm build` produces:

- `dist/index.js`
- `dist/index.d.ts`

The package is built as both ESM and CommonJS, with declarations emitted from
the single `src/index.ts` entrypoint.

### Local Tarball Consumption

When another local package needs a packaged SDK artifact:

```sh
cd demo/midgard-sdk
pnpm repack
```

`pnpm repack` runs a fresh build and then creates a tarball such as:

```text
al-ft-midgard-sdk-0.1.0.tgz
```

You can then reference it from another project's `package.json`:

```json
{
  "dependencies": {
    "@al-ft/midgard-sdk": "file:~/path/to/midgard-sdk/al-ft-midgard-sdk-0.1.0.tgz"
  }
}
```

## Repository Integration

`midgard-node` depends on this package directly and also syncs the generated
`dist/index.d.ts` surface into its own generated type shim during build. In
practice this means:

- `src/index.ts` is the public contract that downstream packages rely on,
- adding a new module is not enough; it must also be exported from
  `src/index.ts`,
- breaking export names or changing transaction-builder conventions has
  immediate downstream impact on the node and tests.

## Module Map

### Foundational Modules

- `common.ts`: shared types, authentication helpers, Effect adapters, and
  shared errors.
- `cbor.ts`: CBOR encoding/decoding helpers for Midgard data.
- `constants.ts`: protocol constants and initial state values.
- `protocol-parameters.ts`: off-chain protocol parameter accessors and timing
  helpers.
- `ledger-state.ts`: typed ledger-state data structures and decoders.
- `linked-list.ts`: generic linked-list/node helpers used by Midgard state
  machines.

### Protocol State Machines

- `hub-oracle.ts`: hub-oracle UTxO helpers and transaction builders.
- `scheduler.ts`: scheduler datum/redeemer helpers and scheduler transaction
  fragments.
- `state-queue.ts`: state-queue authentication, traversal, and mutation
  builders.
- `initialization.ts`: protocol bootstrap and one-shot initialization builders.

### Operator Lifecycle

- `registered-operators.ts`: registration-state helpers and transaction
  builders.
- `active-operators.ts`: active-operator state and lifecycle builders.
- `retired-operators.ts`: retired-operator state and retirement-flow helpers.

### User Events

- `user-events/deposit.ts`: deposit event typing and builders.
- `user-events/withdrawal.ts`: withdrawal event typing and builders.
- `user-events/tx-order.ts`: transaction-order event typing and builders.

### Fraud Proofs

- `fraud-proof/catalogue.ts`: fraud-proof catalogue initialization and category
  mutation builders.
- `fraud-proof/computation-threads.ts`: computation-thread lifecycle helpers.
- `fraud-proof/tokens.ts`: fraud-proof token mint/burn helpers.

## Example Usage

For simple utility-style use, you can call exported helpers directly:

```ts
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

const digest = await Effect.runPromise(
  SDK.hashHexWithBlake2b256("deadbeef"),
);
```

For transaction builders, the typical pattern is to work with a `Program`
variant and decide at the call site how to run it:

```ts
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

const tx = await Effect.runPromise(
  SDK.unsignedInitializationTxProgram(lucid, initParams),
);
```

## Development Commands

```sh
pnpm build
pnpm test
pnpm repack
pnpm clean
pnpm reset
```

Use:

- `build` when iterating on the public package surface,
- `test` for SDK-local verification,
- `repack` when another package needs a fresh tarball,
- `clean`/`reset` when rebuilding from a known-empty state.

## Notes for Contributors

- Prefer adding new APIs in the narrowest relevant module and then exporting
  them from `src/index.ts` only if they are intended to be public.
- Keep `Program` naming for `Effect`-returning builders so callers can predict
  execution style.
- Preserve globally unique export names where possible; this repository often
  imports the SDK as a namespace.
- Changes here should be evaluated as protocol-surface changes, not just local
  helper edits.
