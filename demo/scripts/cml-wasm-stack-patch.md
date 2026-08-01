# C26 Step 2 — the CML wasm shadow-stack patch

**What is patched:** two integers inside the vendored
`cardano_multiplatform_lib_bg.wasm` of
`@anastasia-labs/cardano-multiplatform-lib-nodejs@6.2.0-1` and its sibling
`@anastasia-labs/cardano-multiplatform-lib-browser@6.2.0-1`.

**Who applies it:** `demo/package.json` → `"postinstall": "node
scripts/patch-cml-wasm-stack.mjs"`. Nothing else in the repo depends on the
patch having been applied except the maximum-depth suites listed below.

**What it is not:** it is not a source patch, not a `pnpm patch` entry, and not
a fork. No code, no data segment, no export, and no import is modified. The
lockfile is untouched — the patch operates on the extracted `node_modules`
copies after install, and is re-applied on every install.

---

## 1. Why

Canonical Midgard V1 admits a unary Plutus Data nesting depth of **4,043**
inside an exactly **16,384-byte** signed Cardano transaction. That number is
derived from Cardano transaction capacity; it is not a Midgard cap. C26
requires that the maximum be genuinely exercised, including real
`Emulator.submitTx` / `awaitTx` admission.

CML's CBOR decoders are recursive descent and the shipped binary uses the
wasm-bindgen default **1 MiB shadow stack** (~688 bytes per Plutus Data
nesting level). Measured ceilings on the stock binary:

| CML entry point                                 | max OK depth | first failing depth |
| ----------------------------------------------- | ------------ | ------------------- |
| `PlutusData.from_cbor_hex` / `from_cbor_bytes`  | 1,522        | 1,523               |
| `TransactionOutput.from_cbor_hex`               | 1,517        | 1,518               |
| `Transaction.from_cbor_hex` / `from_cbor_bytes` | 1,502        | 1,503               |

Above those depths V8 reports `RuntimeError: memory access out of bounds`, and
the trap **permanently poisons the `WebAssembly.Instance`** for the rest of the
process — even a subsequent `PlutusData.from_cbor_hex("d87980")` throws.

The binding exposes no way to configure this: no global is exported, and the
nodejs glue instantiates synchronously at module load from a fixed path
(`new WebAssembly.Instance(wasmModule, …)`). The binary must therefore be
rewritten, or the maximum stays unreachable.

Background, measurements and the alternatives that were rejected:
`docs/exec-plans/evidence/c26-cml-investigation.md`.

## 2. The exact edit

Both edits are visible with any wasm section dumper; the script's own
`parseWasmSections` is ~40 lines and does nothing else.

| #   | Section       | Stock bytes                  | Patched bytes                | Meaning                                                                                        |
| --- | ------------- | ---------------------------- | ---------------------------- | ---------------------------------------------------------------------------------------------- |
| 1   | memory (id 5) | `01 00 15`                   | `01 00 95 02`                | `count=1`, `flags=0x00` (no maximum), `initial` **21 → 277 pages** (unsigned LEB128)           |
| 2   | global (id 6) | `01 7f 01 41 80 80 c0 00 0b` | `01 7f 01 41 80 80 d4 08 0b` | `global[0]`: `mut i32`, `i32.const` **1048576 → 18153472** (signed LEB128, 4 bytes either way) |

`global[0]` is the wasm-bindgen `__stack_pointer`; the shadow stack grows
**downward** from it. 21 pages = 1,376,256 bytes; 277 pages = 18,153,472 bytes,
which is exactly the new stack-pointer value — i.e. the stack now starts at the
top of initial memory and has **16 MiB** (256 pages) of freshly added,
otherwise unreachable space below it.

Only the memory section's payload changes length (3 → 4 bytes), so the file
grows by exactly **one byte** and every other section is copied verbatim. wasm
sections are length-prefixed and self-locating, so no offset elsewhere needs
adjusting.

```
stock    sha256 91b38c8e0ad609862186620e2fc07a1919740112c819d13884d029a0a0481b6e   3,191,423 bytes
patched  sha256 cd96b005edaaabc4239f61857f92c322b04cc967363917aaf2ff17ea20313435   3,191,424 bytes
```

### Why it cannot corrupt the heap

- The stock **data section** spans `0x100000`–`0x141d0d`: all static data sits
  _above_ the old 1 MiB shadow stack and _below_ the old initial memory end.
  The relocation neither overlaps nor moves it.
- Rust's dlmalloc on `wasm32` only ever hands out pages it obtained from
  `memory.grow`. Pages present at instantiation — which is exactly what raising
  `initial` produces — are never returned by the allocator, so the relocated
  stack cannot collide with the heap.
- The old `[0, 0x100000)` region simply becomes dead space.
- Cost: ~16.6 MiB of initial linear memory per CML instance.

## 3. The second limit: `--stack-size`

V8 executes wasm frames on the **real machine stack**, so deep recursion
consumes two budgets. With the wasm patched but the default V8 stack, depth
4,043 fails with `RangeError: Maximum call stack size exceeded`. Measured on
this tree:

| configuration                     | `PlutusData.from_cbor_hex` @ 4,043             |
| --------------------------------- | ---------------------------------------------- |
| stock wasm, default V8 stack      | `RuntimeError: memory access out of bounds`    |
| stock wasm, `--stack-size=2000`   | `RuntimeError: memory access out of bounds`    |
| patched wasm, default V8 stack    | `RangeError: Maximum call stack size exceeded` |
| patched wasm, `--stack-size=1200` | `RangeError`                                   |
| patched wasm, `--stack-size=1400` | **OK, ~10 ms**                                 |
| patched wasm, `--stack-size=2000` | **OK, ~7 ms**                                  |

**How the flag is scoped.** It is _not_ set globally: there is no
`NODE_OPTIONS`, no vitest pool option, and no change to how any other suite
runs. Max-depth operations are dispatched to a short-lived child process by
`demo/midgard-validation/tests/helpers/cml-max-depth-runner-v1.ts`, which spawns
`node --stack-size=2000
demo/midgard-validation/tests/helpers/cml-max-depth-child-v1.mjs`. Two reasons:

1. `--stack-size` cannot be changed inside a running worker; and
2. a CML trap poisons the instance for the whole process, so the stock-CML
   control cases (which are _meant_ to trap) must not share a worker with real
   assertions.

`--stack-size=2000` (≈1.95 MiB) is safely below the 8 MiB default `ulimit -s`
of the main thread.

## 4. The hash pin, and how it fails closed

The pin is the reason this is acceptable rather than a silent supply-chain
rewrite. `runCmlWasmStackPatch` classifies every candidate by sha256 _before_
anything is written:

| observed sha256                     | action                                                                                                                       |
| ----------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `91b38c8e…` (stock)                 | patch it; then re-hash the produced bytes and abort unless they are `cd96b005…`; write; re-read and re-hash the file on disk |
| `cd96b005…` (patched)               | leave alone — idempotent, safe to re-run                                                                                     |
| anything else, at version `6.2.0-1` | **hard failure**, non-zero exit, nothing written                                                                             |
| any other CML version (`6.0.2-x`)   | reported and skipped — not on the deep-Plutus-Data path                                                                      |
| **no `6.2.0-1` copy found at all**  | **hard failure** — the pinned dependency moved and the max-depth suites would otherwise silently lose their premise          |

A hard failure prints a `CML WASM PATCH REFUSED` block naming the file, the
observed digest and both expected digests, and exits non-zero — which fails the
install.

Both failure modes are exercised as tests, not just asserted in prose:
`demo/midgard-validation/tests/cml-wasm-stack-patch-v1.test.ts` builds a fake
tree containing a valid-magic but unrecognised wasm at version `6.2.0-1`,
confirms the refusal, and confirms the impostor file is byte-unchanged
afterwards; a second control does the same for an empty tree.

**On a CML upgrade** the patch must fail — that is intended. Re-run the C26
measurements, then update the pin in `patch-cml-wasm-stack.mjs` _and_ this
document together. Do not loosen the check.

## 5. Hardlink safety (pnpm store)

pnpm hardlinks package files from the global content-addressable store; link
counts of 43 were observed on this tree. Writing in place would silently
corrupt the store **and every other project on the machine**. The script
therefore always writes a fresh temp file in the target's directory and
`rename()`s it over the target, which breaks the hardlink.

Verified after applying: the store object
(`~/.local/share/pnpm/store/v3/files/0b/46dc…`) is still `91b38c8e…`,
3,191,423 bytes, with its link count merely decremented.

## 6. Verifying, auditing and reverting

```bash
# report the state of every CML copy in the tree; exits non-zero on a bad pin
pnpm --dir demo run patch:cml-wasm:check

# (re-)apply — this is what postinstall runs; idempotent
pnpm --dir demo run patch:cml-wasm

# restore the stock bytes everywhere
pnpm --dir demo run patch:cml-wasm:revert

# materialise a standalone, loadable copy of the STOCK package for comparison
node demo/scripts/patch-cml-wasm-stack.mjs --emit-stock-package /tmp/stock-cml

# machine-readable output for any of the above
node demo/scripts/patch-cml-wasm-stack.mjs --check --json
```

Reverting is a complete undo: the rewrite is exactly invertible, and
`--revert` re-derives the stock bytes and refuses unless they hash to
`91b38c8e…`. `pnpm install --ignore-scripts` also leaves the tree stock.
Removing the patch entirely = delete the `postinstall` line from
`demo/package.json`, run `patch:cml-wasm:revert`, and delete this script, its
documentation, and `cml-wasm-stack-patch-v1.test.ts`; the only functional loss
is maximum-depth CML admission (Plutus Data deeper than ~1,502 in a
transaction).

Each patched package directory also gets a
`.midgard-cml-wasm-stack-patch-v1.json` receipt recording what was done, when,
and how to revert. It lives inside `node_modules` and is not committed.

## 7. Equivalence evidence (owner condition #2)

`demo/midgard-validation/tests/helpers/cml-wasm-differential-scenarios.cjs`
runs a fixed, side-effect-free exercise of CML and prints a canonical JSON
document. It is executed twice with identical Node flags — once against the
installed (patched) package, once against the stock package materialised by
`--emit-stock-package` — and the two outputs are compared byte for byte.

Eight scenario groups, 51,121 bytes of output:

1. Ed25519 — private-key derivation, bech32, public key, key hash, signing,
   verification, and rejection of a tampered message.
2. Addresses — enterprise/base/reward construction, bech32 round trip, raw
   bytes, network id.
3. Plutus Data — 13 shapes: unit, positive/negative ints, chunked and
   definite byte strings, bignums, list, map, alternate constructor tag family,
   and nesting at depths 8 / 300 / 1,400. Each yields kind, CBOR round trip,
   canonical CBOR, `hash_plutus_data`, and detailed-schema JSON length.
4. `Value` — multi-asset CBOR round trip, coin, policy count, `checked_add`,
   `min_ada_required`.
5. Native scripts — pubkey and timelock scripts, `script_all`, hashes, CBOR,
   JSON.
6. Whole transaction — an 800-deep inline datum: CBOR round-trip identity,
   `hash_transaction`, body CBOR, extracted datum and its hash,
   `min_no_script_fee`.
7. `BigInteger` — five boundary values including full u64 and 256-bit.
8. Allocation churn — 4,000 iterations of constructing and encoding
   `ConstrPlutusData`, accumulating a digest.

Result: **byte-identical**, sha256
`b5c38e22fea338f58262e50dae3d3576e98d6da7068f5f064d83901a13d1b8ca`, across
three runs — the real installed binary _before_ patching, the reverted binary
_after_ patching, and the patched binary. Re-checked on every test run by
`cml-wasm-stack-patch-v1.test.ts`.

## 8. Files

| Path                                                                        | Role                                                                      |
| --------------------------------------------------------------------------- | ------------------------------------------------------------------------- |
| `demo/scripts/patch-cml-wasm-stack.mjs`                                     | the patcher (hash-pinned, idempotent, reversible)                         |
| `demo/scripts/cml-wasm-stack-patch.md`                                      | this document                                                             |
| `demo/package.json`                                                         | `postinstall` + `patch:cml-wasm*` scripts                                 |
| `demo/midgard-validation/tests/cml-wasm-stack-patch-v1.test.ts`             | pin, invertibility, byte-identity, ceiling, and both fail-closed controls |
| `demo/midgard-validation/tests/helpers/cml-wasm-differential-scenarios.cjs` | the deterministic exercise compared across binaries                       |
| `demo/midgard-validation/tests/helpers/cml-max-depth-child-v1.mjs`          | out-of-process CML/emulator runner                                        |
| `demo/midgard-validation/tests/helpers/cml-max-depth-runner-v1.ts`          | typed front end that applies `--stack-size`                               |
| `demo/midgard-validation/tests/helpers/unary-depth-candidate-v1.ts`         | shared raw (CML-free) maximum-depth candidate builder                     |
| `demo/midgard-validation/tests/plutus-data-unary-depth-boundary-v1.test.ts` | genuine emulator admission at the exact maximum                           |

## 9. Residual risk

- This is a third-party binary rewrite. It is contained by the hash pin, the
  byte-identity differential, the exact invertibility of the rewrite, and the
  fact that only two integers change — but it is still a rewrite, and any CML
  upgrade requires re-measuring rather than re-pinning blindly.
- The reasoning about dlmalloc never allocating pre-`memory.grow` pages is an
  argument about Rust's `wasm32` allocator, not a proof about this binary. The
  differential (including 4,000 iterations of allocation churn and a full
  transaction build) is the empirical counterweight.
- Every CML instance now reserves ~16.6 MiB of initial linear memory instead of
  ~1.3 MiB. Processes that create many instances pay that per instance.
- `demo/midgard-manager` is not a pnpm workspace member and carries its own
  store. It is patched when the patcher runs from `demo/`, but a bare
  `pnpm install` executed _inside_ `demo/midgard-manager` will restore stock
  bytes there until `pnpm --dir demo run patch:cml-wasm` is run again.
- Upstream fix worth filing: build CML with
  `-C link-arg=-zstack-size=…`, which removes the need for this patch entirely.
