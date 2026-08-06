# C26 CML/WASM unary-depth trap — root cause and proposal — 2026-07-29

Authority:

- `GOAL_SPEC.md` §8.2 row C26;
- `GOAL_PROGRESS.md` row C26 (`PARTIAL`), read-heavy investigation lease.

Scope: investigation record. No production `src/**` change, no
`package.json`/lockfile change, no `GOAL_SPEC.md`/`GOAL_PROGRESS.md` change
was made for the investigation itself. The temporary probing suites used to
collect the measurements were deliberately non-evidence scaffolding and have
been removed. The permanent evidence surfaces are the validation boundary,
CML patch, and midgard-core well-formedness/retained-data suites.

## 1. Summary

C26 is `PARTIAL` because of **two independent, unrelated defects**, both in
third-party code, neither of which is a Cardano protocol limit:

| # | Defect | Component | Symptom | Blocks |
| - | ------ | --------- | ------- | ------ |
| A | 1 MiB wasm shadow stack, recursive descent decoder | `@anastasia-labs/cardano-multiplatform-lib-nodejs@6.2.0-1` | `RuntimeError: memory access out of bounds` above depth 1,522 | maximum **emulator admission** |
| B | Host-recursive `Data.from` with per-node WASM allocation | `@lucid-evolution/plutus@0.1.35` (via `@lucid-evolution/lucid@0.6.0`) | trap below 1,522; superlinear time and GiB-scale RSS above ~1,000 | maximum **retained reconstruction** |

Defect B is reached from Midgard production code through exactly one call:
`Data.from(source.toString("hex"))` in
`demo/midgard-core/src/codec/datum.ts:36`. Its return value is **discarded** —
it is used purely as a "is this decodable Plutus Data" probe. Replacing that
one probe with an iterative repo-native check closes the entire Midgard-side
half of C26 with **stock, unpatched CML** and with **byte-identical** output.

Measured, end to end, at the exact C26 maximum (depth 4,043, signed 16,384
bytes): full canonical decode + consensus validation + exact datum identity +
proof source + commitment + all six chunk proofs verified + terminal
reconstruction fold equal to canonical, in **296 ms / 381 MiB peak**. Today
the same path fails outright.

Neither fix introduces a depth cap. The admitted maximum continues to derive
from Cardano transaction capacity, satisfying C26's acceptance wording.

## 2. Root cause A — CML wasm shadow-stack exhaustion

### Exact dependency and entry points

Package: `@anastasia-labs/cardano-multiplatform-lib-nodejs@6.2.0-1`
(resolved through `@lucid-evolution/lucid@0.6.0`; the sibling
`-browser@6.2.0-1` is also installed and carries the same binary layout).
Binary: `cardano_multiplatform_lib_bg.wasm`,
`sha256:91b38c8e0ad609862186620e2fc07a1919740112c819d13884d029a0a0481b6e`.

Trapping entry points, with the exact maximum passing depth found by binary
search (fresh child process per probe, because a trap poisons the instance):

| CML entry point | max OK depth | first failing depth |
| --------------- | ------------ | ------------------- |
| `CML.PlutusData.from_cbor_hex` / `from_cbor_bytes` | 1,522 | 1,523 |
| `CML.TransactionOutput.from_cbor_hex` | 1,517 | 1,518 |
| `CML.Transaction.from_cbor_hex` / `from_cbor_bytes` | 1,502 | 1,503 |

The ~20-level spread is the extra Cardano framing (tx → body → outputs →
output → datum option → tag 24) above the Plutus Data value itself. The
inline datum inside tag 24 is parsed **eagerly**, so a deep datum traps the
whole transaction parse.

### It is stack exhaustion, not heap growth

Static analysis of the binary:

- memory section: `initial = 21 pages` (1.3125 MiB), **no maximum**;
- global section: `global[0]` is `mut i32`, `i32.const 1048576` — the
  wasm-bindgen `__stack_pointer`, i.e. a **1 MiB shadow stack** in
  `[0, 0x100000)` growing **downward**;
- data section: 307 segments spanning `0x100000` to `0x141d0d` — all static
  data sits immediately **above** the shadow stack; the dlmalloc heap starts
  above that;
- no global is exported, so the stack pointer cannot be adjusted at runtime.

1,048,576 bytes / 1,523 frames ≈ **688 bytes per recursion level**. The
decoder recurses once per Plutus Data node, the stack pointer walks down past
zero, and the resulting out-of-range linear-memory access is reported by V8 as
`memory access out of bounds`. Heap growth is not involved: the memory has no
maximum and `memory.grow` is never the failing operation.

Consequence for test design: after a trap the `WebAssembly.Instance` is
unrecoverable. Verified — a subsequent `CML.PlutusData.from_cbor_hex("d87980")`
(depth 1, needs no recursion budget) also throws `memory access out of
bounds`. Any in-process probing of this limit corrupts every later assertion
in the same worker, which is why the historical probes forked per probe.

### A second, distinct limit hides behind the first

Once the shadow stack is enlarged, a **different** limit appears: V8's native
frame limit, reported as `RangeError: Maximum call stack size exceeded`. WASM
frames in V8 execute on the real machine stack, so deep recursion consumes
both budgets. Attribution with a 16 MiB shadow stack, depth 4,043:

| Configuration | `PlutusData.from_cbor_hex` @4,043 |
| ------------- | --------------------------------- |
| stock wasm, default V8 stack | `RuntimeError: memory access out of bounds` (shadow stack) |
| patched wasm, default V8 stack | `RangeError: Maximum call stack size exceeded` (V8 frames) |
| patched wasm, `--stack-size=1200` | `RangeError` |
| patched wasm, `--stack-size=1400` | **OK, 5 ms** |
| patched wasm, `--stack-size=2000` | **OK, 4 ms** |

Both budgets must be raised together. Any proposal that raises only one is
incomplete — this is the trap the current `PARTIAL` note did not distinguish.

## 3. Root cause B — lucid `Data.from` is the real production blocker

`demo/midgard-core/src/codec/datum.ts` validates every inline datum with two
independent gates:

```ts
try {
  Data.from(source.toString("hex"));      // gate 1: CML decodability probe
} catch (cause) {
  return fail("Invalid PlutusData datum CBOR", String(cause));
}
// ... gate 2: canonicity, via the repo's own iterative normalizer
canonical = aikenSerialisedPlutusDataCborPreservingMapOrder(...);
if (!canonical.equals(source)) return fail("PlutusData datum is not canonical");
```

Gate 1's result is never used. Its implementation
(`@lucid-evolution/plutus@0.1.35`, `from()` → inner `deserialize()`) is a
**host-recursive** JS function that walks the CML object graph, allocating a
fresh CML wrapper per node via `l.get(i)` and never releasing them.

Measured cost of gate 1 versus gate 2 (patched wasm so deep depths are
reachable at all):

| depth | CML parse | `Data.from` | `Data.from` 2nd call | repo normalizer | RSS |
| ----- | --------- | ----------- | -------------------- | --------------- | --- |
| 256   | 2 ms  | 7 ms   | 5 ms   | 3 ms  | 273 MiB |
| 512   | 1 ms  | 17 ms  | 29 ms  | 6 ms  | 312 MiB |
| 1,024 | 5 ms  | 148 ms | 160 ms | 16 ms | 460 MiB |
| 1,500 | 2 ms  | 340 ms | 367 ms | 8 ms  | 781 MiB |
| 2,048 | 2 ms  | 645 ms | 656 ms | 11 ms | 1,375 MiB |

`Data.from` is roughly **quadratic** (2× depth ≈ 4× time) and leaks linearly.
CML's own parse is ~2–5 ms flat. The repo's normalizer is linear and ~10 ms.

This is why the currently passing depth-1,024 witness is already slow, and why
depth 4,043 is hopeless even with the wasm patched: the production conversion
calls `decodeMidgardDatum` several times per transaction, and one instrumented
`encodeMidgardTxOutput` at depth 4,043 took **80,713 ms**. A full-path run at
4,043 with patched wasm but unmodified `Data.from` was killed after **8+
minutes of CPU at 2.9 GiB RSS** without completing.

The measured `Invalid PlutusData datum CBOR` failure at depth 4,043 with stock
CML is gate 1 trapping — the trap is being reported as a Midgard schema error,
which is why the underlying cause was not obvious from the test output.

## 4. Workaround experiments

### (a) Iterative pre-normalization in TypeScript — WORKS, recommended

The repo already owns a fully iterative Plutus Data CBOR parser and encoder
(`demo/midgard-core/src/plutus-data-cbor.ts`: `parseCborNode` and
`encodeCborNodeWithDefiniteMaps` both use explicit frame/visit stacks, no
recursion). Only the redundant `Data.from` probe is recursive.

Gate 1 is **not** simply deletable. Differential testing shows the repo
normalizer alone accepts inputs CML rejects — out-of-range constructor tags
(120, 128, 1,000, 1,401) and tag 30 round-trip byte-identically through the
normalizer, so the canonicity gate passes them. The probe must be *replaced*
by an equivalent iterative Plutus-Data well-formedness check, not removed.

The investigation prototype (the proposed production logic verbatim) validates:
constructor tags in `121..127 ∪ 1280..1400 ∪ {102}`
with correct array framing, bignum tags 2/3 wrapping definite byte strings,
byte chunks ≤ 64 bytes, no major type 7, exactly one value, no trailing bytes.

Differential results against CML/lucid `Data.from`:

- raw gate versus `Data.from`: 4,061-case corpus, **4,058 agreements**, 3
  divergences — all three are cases where the iterative gate is *stricter*
  (`0000` and `d8799f0102ffff` trailing data, `5fff` empty indefinite bytes),
  and in all three the existing canonicity gate already rejects, so the
  composite verdict is unchanged;
- **composite** verdict (`gate && canonicity`) versus current composite
  (`Data.from && canonicity`): 6,048-case corpus, **6,048 agreements, 0
  divergences**.

Cost of the replacement gate: **4 ms at depth 4,043**, 2 ms at depth 16,000,
versus ~5,000 ms and gigabytes for `Data.from`.

End-to-end proof with **stock, unpatched CML**:

| depth | gate | result |
| ----- | ---- | ------ |
| 1,024 | today's `Data.from` | OK, sha256 `ae9f29c7…`, 564 ms, 621 MiB |
| 1,024 | iterative | OK, sha256 `ae9f29c7…` **identical**, 65 ms, 290 MiB |
| 4,043 | today's `Data.from` | **FAILS** `Invalid PlutusData datum CBOR` |
| 4,043 | iterative | **OK**, 296 ms, 381 MiB, 6 reveal steps, terminal reconstruction equals canonical, `validateMidgardConsensusV1Tx` returns `null` |

The depth-1,024 sha256 match is the equivalence evidence: the change is
byte-identical at the depth the suite already covers.

### (b) Bypassing CML with the repo's own codecs — WORKS for the Midgard half only

Confirmed: the Midgard-side path (canonical decode, proof source, chunk
proofs, terminal fold) needs no CML at all once gate 1 is iterative. But the
Cardano-side entry `cardanoTxBytesToMidgardNativeTxCanonicalCborV1` calls
`CML.Transaction.from_cbor_bytes` (`native-cardano-conversion.ts:25`), which
still traps at 4,043 with stock CML (`Invalid Cardano transaction bytes`).

A "hoist" variant was tested — locate the tag-24 datum in the raw bytes, swap
in a shallow placeholder before CML sees them, convert, then splice the real
datum back through repo codecs. It produced byte-identical canonical output at
depth 1,024 (sha256 `006323b1…` matching the direct path) and works at 4,043
*if* gate 1 is also iterative. It is rejected as a primary path: it makes the
production conversion aware of a synthetic placeholder substitution, which is
exactly the kind of shortcut C26/CG2 exist to forbid.

### (c) WASM stack/memory configuration — WORKS, needed only for emulator admission

The binding exposes no configuration: no global export, and the glue
instantiates synchronously from a fixed path
(`new WebAssembly.Instance(wasmModule, ...)` at module load). The binary must
therefore be rewritten. A 60-line patcher does it without touching code or
relocating data:

1. raise memory `initial` from 21 to 277 pages, so the initial region contains
   16 MiB above the old initial end;
2. set `global[0]` (`__stack_pointer`) to the top of that region
   (`18153472`).

This is safe because Rust's dlmalloc on `wasm32` only ever hands out pages it
obtained from `memory.grow`; pre-grown pages between the data end and the
initial memory end are never allocated, so the relocated stack cannot collide
with the heap. The old `[0, 1 MiB)` region simply becomes dead. Result:
`sha256:cd96b005edaaabc4239f61857f92c322b04cc967363917aaf2ff17ea20313435`,
3,191,424 bytes (+1 byte), cost ~16.6 MiB initial memory per instance.

Correctness was not assumed. A differential smoke test ran 8 scenarios
(Ed25519 derive/sign/verify, address bech32 round-trip, 11 Plutus Data shapes
including depth 300/1,400, multi-asset `Value` CBOR, native script build and
hash, full transaction build with an 800-deep datum plus hash and
`min_no_script_fee`, `BigInteger`, and a 4,000-iteration allocation-churn
stress) against stock and patched binaries in the same process: **8/8
byte-identical, 0 mismatches**.

With patched wasm plus `--stack-size >= 1400`, every CML operation at depth
4,043 and 4,044 completes in **under 30 ms**:

| operation | @4,043 | @4,044 |
| --------- | ------ | ------ |
| `PlutusData.from_cbor_hex` | 25 ms | 7 ms |
| `PlutusData` round-trip `to_cbor_hex` | 15 ms | 4 ms |
| `PlutusData.to_canonical_cbor_hex` | 15 ms | 3 ms |
| `hash_plutus_data` | 4 ms | 6 ms |
| `Transaction.from_cbor_hex` | 14 ms | 6 ms |
| `Transaction` round-trip `to_cbor_hex` | 23 ms | 11 ms |
| `…outputs().get(0).datum().as_datum()` | 19 ms | 8 ms |
| `hash_transaction` | 8 ms | 4 ms |

**Genuine emulator admission at the exact maximum, reproduced 4/4 runs:** a
signed 16,384-byte depth-4,043 candidate submitted through the real
`lucid-evolution` `Emulator.submitTx` / `awaitTx`:

```
{"depth":4043,"signedBytes":16384,"withinMaxTxSize":true,
 "submitted":true,"confirmed":true,"emulatorReturnedExactDatum":true}
```

Stock wasm on the same candidate: `RuntimeError: memory access out of bounds`
at `AddressFinalization`. Note that adjacent depth 4,044 (16,388 bytes) is
*also* accepted by the emulator — the emulator does not enforce `maxTxSize`,
so adjacent rejection must keep deriving from the exact signed byte count, as
the existing test already does. That is a property of the emulator, not a
regression from the patch.

Combined (patched wasm + iterative gate + `--stack-size=2000`), the **real
production entry point** `cardanoTxBytesToMidgardNativeTxCanonicalCborV1` on
the genuine signed 16,384-byte candidate:

```
{"depth":4043,"signedBytes":16384,"canonicalBytes":16470,
 "consensusValidation":null,"datumExact":true,"revealStepCount":6,
 "reconstructedEqualsCanonical":true,"totalMs":336,"peakRssMiB":431}
```

### (d) Chunked/streaming decode — not needed, not recommended

CML exposes no incremental Plutus Data decoder, so this would mean a
repo-native Cardano transaction decoder replacing `CML.Transaction`. That is
far larger than (a)+(c) and, per C26/§3.2, a bounded/incremental fallback
needs a measured necessity artifact. Since (a)+(c) reach the full derived
capacity with the complete-item path intact, no such artifact exists. Rejected.

### (e) Bounded-admission cap — not required

Recommended against. Both blocking limits are third-party configuration and
one redundant validation call, not protocol properties. For the record, the
capacity that *is* safely admissible end to end today, unmodified, is depth
**1,502** (`Transaction.from_cbor_hex` ceiling, ~6,009 datum bytes) — but
`Data.from`'s quadratic cost makes anything above ~1,024 impractical in
practice, which is precisely why the current witness sits at 1,024. A cap
would enshrine an arbitrary recursion limit as protocol capacity, contradicting
C26's acceptance wording, and would need owner sign-off. It is not needed.

## 5. Recommendation

**Primary: (a) + (c), in that order.** (a) is repo-owned, closes the larger
half, and is independently valuable (it removes a quadratic, leaking call from
every datum decode in production). (c) is a scoped third-party fix needed only
for genuine emulator admission.

### Step 1 — iterative Plutus Data gate (repo-owned)

Effort: **~4–6 hours** including tests.

- `demo/midgard-core/src/plutus-data-cbor.ts` — add
  `assertMidgardPlutusDataWellFormedV1(bytes: Buffer): void`, the iterative
  validator prototyped during the investigation. One
  correction to carry over: allow a zero-chunk indefinite byte string (`5fff`)
  so the gate mirrors CML exactly and the canonicity gate stays the only
  rejecter. Export it from the package index.
- `demo/midgard-core/src/codec/datum.ts` — in `decodeMidgardDatum`, replace
  the `Data.from(source.toString("hex"))` probe (lines 34–38) with the new
  assertion, preserving the exact `"Invalid PlutusData datum CBOR"` message
  and `MidgardTxCodecErrorCodes.InvalidFieldType`. Drop the now-unused
  `@lucid-evolution/lucid` import.
- `demo/midgard-core/src/codec/native-redeemer.ts` — same pattern applies to
  `CML.PlutusData.from_cbor_bytes` at line 77; audit and convert for redeemer
  Data (C20-8 shares this ceiling, so this is likely a prerequisite for the
  redeemer-side maximum too).
- Tests: retain the differential corpus in
  `demo/midgard-core/tests/plutus-data-wellformed-v1.test.ts` as the
  semantic-equivalence evidence, together with the depth-1,024 sha256
  equality regression guard.
- Then extend `plutus-data-unary-depth-boundary-v1.test.ts` to run retained
  reconstruction at depth 4,043 rather than 1,024.

Risk: low. Verdict equivalence is established on 6,048 cases with zero
composite divergences, and canonical output is byte-identical at depth 1,024.

### Step 2 — CML shadow-stack relocation (third-party)

Effort: **~3–5 hours** plus review of the vendoring approach.

- Add a deterministic patcher script (e.g.
  `demo/scripts/patch-cml-wasm-stack.mjs`) implementing §4(c), asserting the
  input `sha256` `91b38c8e…` and the output `sha256` `cd96b005…` so the
  rewrite is verifiable and fails closed on a CML upgrade.
- Wire it as a `postinstall` for both
  `@anastasia-labs/cardano-multiplatform-lib-nodejs` and `-browser` at
  `6.2.0-1`. A `pnpm patch` entry is a poor fit here — the payload is a 3 MiB
  binary. **This touches `demo/package.json`, so it is outside the
  investigation lease and needs parent approval.**
- Add `--stack-size=2000` to the vitest/Node options for the suites that
  exercise maximum depth (measured floor is 1,400; 2,000 leaves headroom).
- Carry the 8-scenario differential smoke test into the repo so the patched
  binary is proven equivalent on every install.

Risk: moderate but contained — it is a third-party binary rewrite, mitigated
by hash pinning, the differential smoke test, and the fact that it only moves
an initial stack-pointer value.

Preferred upstream follow-up: file the 1 MiB shadow-stack limit against
`Anastasia-Labs/cardano-multiplatform-lib` (a `-C link-arg=-zstack-size=…`
build flag fixes it at source) and the quadratic recursive `deserialize`
against `lucid-evolution`. Both are worth reporting regardless of the local fix.

### Fallback

If Step 2 is rejected, ship Step 1 alone. That closes maximum **retained
reconstruction** at the full derived depth 4,043 with stock CML — the larger
and more protocol-relevant half — and leaves only genuine *emulator admission*
short of maximum. C26 would stay `PARTIAL`, but the residual gap would be
narrowed to a single documented third-party ceiling (depth 1,502 for
`CML.Transaction.from_cbor_hex`) with a measured, reproducible fix on the
shelf, rather than the current unexplained trap. A depth cap remains
unnecessary and is not recommended in either case.

## 6. Reproduction

```bash
# permanent validation boundary and CML patch suites
pnpm --dir demo/midgard-validation test -- plutus-data-unary-depth
pnpm --dir demo/midgard-validation test -- cml-wasm-stack-patch-v1
# ^ Retired 2026-08-05: CML 6.2.0-2 fixed the shadow stack at source
#   (16 MiB via -zstack-size, upstream CML PR #6), so the patcher and
#   cml-wasm-stack-patch-v1.test.ts were removed with the #543 bump.
#   The active successor suite is:
pnpm --dir demo/midgard-validation test -- cml-wasm-shadow-stack-v1

# permanent midgard-core gate, retained datum, and redeemer suites
pnpm --dir demo/midgard-core test -- plutus-data-wellformed-v1 plutus-data-deep-datum-retained-v1 native-redeemer-deep-data-v1
```

The measurements were verified on Node v24.13.1 and pinned Node v22.22.2 /
pnpm 9.15.9. The permanent suites above are the active evidence surfaces;
there are no investigation suites in the default validation glob.
`tsc --noEmit` was clean for `demo/midgard-validation`. Note:
`pnpm --dir demo/midgard-validation lint`
fails in this environment before reading any file (`Cannot find module
'/home/home/.../eslint/bin/eslint.js'`); `eslint --version` fails identically,
so the breakage is pre-existing and unrelated to these files.

The one-off harnesses used for the numbers above (wasm patcher, differential
smoke test, staged timing probes, emulator admission runner) were written to a
session scratchpad rather than the repo. The wasm patcher is the only one
Step 2 needs, and §4(c) specifies it completely.
