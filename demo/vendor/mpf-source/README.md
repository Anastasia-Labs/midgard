# MPF package source and receipt

The workspace installs `../mpf-1.3.1-midgard.1.tgz` with a portable pnpm
override. This is the existing local fixing package, preserved byte for byte.
It is not an npm release. The published incremental-runtime branch remains at
`b139785f7c118854f9824c26cc443e2df95cb50e`; the fixing commit is local.

`off-chain-source.tgz` contains the genuine source from local commit
`a0f8c2410ec07d9b45b02539817e980a17845c5f`, including the MPL-2.0 license,
tests and original Yarn lock. `upstream.patch` records its changes against the
published base. `package-lock.json` adds an exact npm build dependency lock;
the upstream package source is otherwise unchanged. `provenance.json` records
source hashes and the toolchain. `package-receipt.json` records the existing
tarball integrity and every package member hash.

The fix marks odd-cursor leaf suffixes with `0x10`, separating their hash
preimages from branch nodes. It changes roots containing such leaves. The
corresponding on-chain change and deployment acceptance belong to the combined
Midgard contract candidate; installing this package does not migrate durable
state or authorize a redeploy.

With Node 22.22.2 and npm 10.9.7 on PATH, run:

```sh
node demo/vendor/mpf-source/reproduce.mjs
```

The script checks preserved source and package hashes, installs locked build
dependencies, rebuilds in a fresh temporary directory, and compares every repacked member
with the installed package receipt. All members reproduced in a fresh build.
The compressed archive itself differs with npm packaging metadata; the
preserved installed tarball remains protected by its SHA256 and pnpm SHA512.
Ordinary workspace installation requires neither the build tools nor this
rebuild step.

The source's CBOR, helper and incremental-runtime tests can be run with
`node node_modules/ava/entrypoints/cli.mjs tests/cbor.test.js
tests/helpers.test.js tests/midgard-runtime.test.js --concurrency=1` from
`off-chain/` after extracting the source archive. The upstream trie suite also calls Aiken and is a separate gate.

Remove the override only when a published immutable fixing dependency passes
the unchanged installed cross-language and deployment acceptance gates.
