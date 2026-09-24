# Pinned UPLC evaluator

`uplc-0.2.23-midgard.1.tgz` replaces `@lucid-evolution/uplc@0.2.23`
through the workspace's version-specific pnpm override. Its public package
metadata, JavaScript, declarations and MIT license are unchanged. The Node and
bundler WASM targets are rebuilt separately, with the UPLC Apache license and a
modified-source notice included. This is a local dependency, not an npm release.

The insertion-only [Rust patch](uplc-source/constant-cost.patch) avoids computing
argument sizes when both components of an existing builtin cost model are
constant. The configured costs, runtime validation, budget charging and original
nonconstant fallback remain unchanged. This removes expensive recursive sizing
whose result those constant models discard; it does not increase ledger limits.

The [package receipt](uplc-source/package-receipt.json) records every member and
the tarball integrity. The [build provenance](uplc-source/provenance.json) pins
the upstream source, compiler executables, locks and both final WASM hashes.
`uplc-0.2.23-midgard.1-source.tgz` contains the original npm/crate archives, full
patched source, wrapper sources and locks, build/package recipes, licenses, and
regression harnesses. Its [receipt](uplc-source/source-receipt.json) inventories
the archive. Extract into a fresh environment and follow its `README.md` and
`reproduce.sh`; ordinary dependency installation requires neither Rust nor Clang.

Exact binary reproduction requires the source paths documented in that archive.
A fresh Cargo target at those paths reproduced both binaries. Relocating the
source changed the binaries despite file-path remapping. Do not claim
path-independent reproduction. The build uses Clang 19.1.1, while the published
package used 21.1.8; the rebuilt baseline is not byte-identical to that release.

Before integration, the same 25 requests were replayed through published,
rebuilt baseline and patched modules for both targets: all returned redeemer
CBOR, execution units and errors matched. Native checks also compared 2,268
cost evaluations and 270 full-machine/budget/error cases. Bundler-target replay
used its generated imports in Node; it is not a deployed browser test. Installed
workspace acceptance remains a separate requirement from this comparison.

Remove this override only after an upstream release provides equivalent
behavior and passes the unchanged installed acceptance gates. Avoid pnpm text
patches for binary WASM payloads.
