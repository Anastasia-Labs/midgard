# Midgard Off-Chain SDK

Midgard Typescript library for building operator and watcher transactions.

## How to use

1. If you haven't already, enable `corepack` to be able to use `pnpm`
   (`corepack` is shipped with Node.js from version 14.19.0):

```sh
corepack enable
```

2. Install the dependencies:

```sh
cd demo/midgard-sdk
pnpm install
```

3. Bundle the package into a tarball:

```sh
pnpm repack
```

4. In `package.json` file of your project, add `midgard-sdk` as a dependency:

```json
{
  "dependencies": {
    "@al-ft/midgard-sdk": "file:~/path/to/midgard-sdk/al-ft-midgard-sdk-0.1.0.tgz"
  }
}
```

## Structure of the Package

The modules themselves are organized vertically. However, all names are globally
unique. For `midgard-node`, the SDK is used as such:

```ts
import * as SDK from "@al-ft/midgard-sdk";
```

Midgard SDK itself is built with `effect`, but it doesn't impose this on
dependent packages. Having said that, it does offect function variants for
projects that use `effect` as well.

The convention is the same as Lucid Evolution, i.e. functions that return
`Effect` blueprints are postfixed with `Program` (e.g.
`incompleteInitTxProgram`).

Note that this SDK is under ongoing development and these conventions may not
hold for every single function yet.
