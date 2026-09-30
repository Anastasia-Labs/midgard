# Off-chain module-size refactor

Measured and verified on 2026-09-30 UTC against
`dae1120a5611819af953ab539f848b5cdf745fdb`. Changes are local; no commit,
push, deployment or state reset was performed.

## Outcome and scope

ESLint defaults to **500 physical lines**, counting blank lines and comments.
Every retained demo TS/JS overage has a file-specific cap and explanation in
[module-size-exceptions.json](../../demo/module-size-exceptions.json). The cap
validator requires the recorded size to equal the actual size, so growth and
obsolete waivers both require an explicit update. There is no blanket test
exception.

The production TypeScript inventory falls from **159 to 21 files above 1,000
lines**, and from **45 to 4 above 2,000 lines**. The largest production module
falls from 4,033 to 3,964 lines. The remaining large functions/classes are
deliberate exceptions; this change does not claim that every module is below
500 lines.

[metrics.json](metrics.json) records the exact before/after counts and date.
The baseline production denominator is 1,846 under the documented definition,
rather than the supplied table's 1,801; its 159/45 oversized counts agree with
that table. Production excludes test/spec/bench filenames, test/e2e/benchmark
directories, and explicitly registered test facets. All counts exclude
`onchain/`; the independently developed `full-stack` source directory is also
excluded. The final percentages are about **0.62% / 0.12%**, compared with
**8.61% / 2.44%** at this baseline. Keeping the original denominator fixed gives
**1.14% / 0.22%**, so the improvement is not solely a larger file count. Both
measures are below the supplied Lodestar figures of 1.6% / 0.5%.

Reproduce the inventory from the repository root:

```sh
node demo/scripts/module-sizes.mjs --base HEAD \
  --exclude-prefix demo/midgard-node-tools/src/full-stack/ --json
```

The [extraction map](extractions.json) lists every changed TS/JS original and
its replacement parts. In total, **713 TS/JS originals and four Go/Rust
originals** were divided. Existing entrypoints retain their public exports;
implementation facets live alongside their original files. Writable bindings
remain with their writers, test hook/registration order is preserved, and
hoisted-mock boundaries are retained. Existing collation/clock lint waivers
were relocated with their original sites and reasons, without adding waivers.

No Aiken or Plutarch source was edited. On-chain verification selected by the
full preflight ran read-only checks and a disposable blueprint build. The
existing untracked working-tree blueprint was preserved.

## Retained modules and reasons

[retained-modules.json](retained-modules.json) is the complete file-by-file
inventory, including the exceptions outside demo's ESLint scope. It includes
366 exact TS/JS caps, four agent-tooling JS modules, one Rust ownership module
and one SQL migration. Repeated reason patterns describe the actual retained
boundary, rather than exempting an entire directory:

- A single state owner retains private lifecycle, fencing, database or cache
  invariants. Independent helpers have been moved; separating its methods
  requires changing state ownership.
- A large function retains staged local state and error/cleanup order.
  Reducing it further requires a function-body refactor with its own transition
  and failure-path verification, rather than a declaration move.
- Test registrations retain suite-local hooks, mutable fixtures or hoisted
  mocks. Independent top-level helpers/suites were moved; breaking an inner
  registration requires a dedicated fixture/harness interface.
- Declarative catalogues and module augmentations retain one complete schema
  or role universe. Public barrels retain one export surface.
- Entrypoint-sensitive code retains its process/loading identity. In
  particular, the 602-line SQLite journal and 598-line auxiliary-witness fixture
  are loaded directly as TypeScript by Node strip-types. Splitting them into
  source files referenced with `.js` breaks those existing loading contracts.

The four production modules above 2,000 lines deserve a separate design task:

| Module                                                                 | Lines | Reason for retaining its implementation boundary                                                                                                                                                                                                                          |
| ---------------------------------------------------------------------- | ----: | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `validation-machine/trace-builder-prepare.prepare-validation-trace.ts` | 3,964 | The preparation function owns source commitments, resolution scheduling, ledger deltas and witness/CEK frontiers in one evolving local context. Helper declarations were extracted. A further split needs an explicit context/failure interface and witness-order checks. |
| `validation-machine/trace-builder-complete.ts`                         | 2,850 | Completion appends ordered continuation witnesses and control checks over accumulated proof state. Its local state and rejection/terminal order must remain shared.                                                                                                       |
| `cek-executor.structural-executor.ts`                                  | 2,567 | One executor owns the CEK heap, continuation/control state and execution budgets. Splitting its methods requires a separately verified machine-state interface.                                                                                                           |
| `validation-dispute/submit/semantic-resolution.ts`                     | 2,398 | One submit workflow coordinates witness planning, UTxO carriage, funding and submission stages. Splitting the function changes cancellation/failure handoffs and needs lifecycle tests.                                                                                   |

The 611-line Rust `owner/compact_index.rs` retains the `FullIndex` compaction,
authenticated closure, proof arena and child-index cache boundary. Other
native CLI/RPC/runtime/hash/input/output concerns were separated.

The 1,875-line `0001_initial_schema.sql` remains intact: it is an existing
ordered atomic migration with a durable migration identity/checksum. Splitting
an applied migration for a line-count target changes upgrade semantics.

These are reasons for the present scope, not claims that later subdivision is
impossible. Further reduction must preserve each named invariant rather than
introduce arbitrary numeric fragments.

## Regression review and repairs

Independent first and second reviews compared complete original declarations,
public exports, import provenance, registration order, mutable ownership and
resource paths. Structural checking covered the mapped files; it was not an
exhaustive manual semantic audit of every line. The review lenses and DA,
state-queue, user-event and fraud-proof invariant references were applied.

| Finding                                               | Evidence and repair                                                                                                                                                                                                                                                                                                                                  | Final review verdict |
| ----------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------- |
| SDK ESM initialization cycle                          | Imports erased by the original TypeScript compiler had become eager imports. Removing those imports restores SDK startup. A scratch copy reinstating them throws `ReferenceError: Cannot access 'POSIXTimeSchema' before initialization`; the fixed root loads with the same 2,705 exports as HEAD. Core/validation/fault-proof ESM roots also load. | CLOSED               |
| Source guards read only facades                       | DA transport, provider diagnostics, router assertions, family/runbook readers now enumerate implementation siblings. The helper has inclusion/exclusion/missing-entry tests; a scratch forbidden HTTP token in a sibling is refused.                                                                                                                 | CLOSED               |
| Journey source pin omitted facets                     | The session pins all session and runner siblings before launch, reuse and restart. Three mutation cases turn red when the old partial pin is restored, then pass with the complete pin.                                                                                                                                                              | CLOSED               |
| Scenario registry lost extracted titles               | The full fault-proof suite reported 15 missing scenario references. The reader now follows runtime import/export edges through candidate facets. Existing mapping checks pass again.                                                                                                                                                                 | CLOSED               |
| Filename-only scenario reading accepted orphan titles | A new nearest-suite case removes the runtime import and includes a type-only orphan. It fails under filename-only discovery (`['registered','orphan']` versus `['registered']`) and passes with AST-based reachability. All six registry checks pass.                                                                                                | CLOSED               |

The final provider review independently matched 105 declarations, 45 public
exports and 160 imported bindings, found an acyclic runtime graph, and verified
one shared authority registry. Native review matched 107 Rust function bodies
and 65 Go production/test function bodies; private visibility, WASM attributes,
wire ordering and bounds remain intact.

| Review lens                     | Pass 1 / final pass result                                                                       |
| ------------------------------- | ------------------------------------------------------------------------------------------------ |
| Parameter trust                 | Application and validation bodies preserved; no changed parameter application.                   |
| Always-succeeds scripts         | No deployment bypass or validator arm introduced.                                                |
| Decoders/compiler               | Decoder bodies preserved; source moves do not change encoding domains.                           |
| Value conservation              | Economic builders and accounting bodies preserved.                                               |
| Anchoring                       | Commitment/source checks preserved; journey source pin gap closed.                               |
| Reference scripts               | Publication and authenticated reference selection preserved.                                     |
| Both polarities                 | Original registrations retained; scenario reachability is checked.                               |
| Gates that cannot fail          | Facade-reading/pinning/scenario gaps found and closed with red evidence.                         |
| Execution/size budgets          | No added consensus computation or transaction shape.                                             |
| TypeScript/Aiken twins          | Generator computations preserved; registered helper inputs and golden checks cover the moves.    |
| Ledger facts                    | Ordering/indexing/authentication computations preserved.                                         |
| Replacement paths retain guards | SDK startup and source-reader/pinning regressions closed; no displaced finding in final reviews. |

Pre-existing partial invariants, including SQ5, UE7 and FP10, were not closed
or widened by this refactor. The registry's title parser still cannot prove
that a test body reaches its claimed validator refusal; that limitation
predates this change.

## Verification ledger

All runs below are dated 2026-09-30 UTC. Node 22.22.2 and demo pnpm 9.15.4 were
used; docs-site corrective runs used its declared pnpm 10.11.0. Initial suites
ran concurrently on the shared host; timeout failures were rerun with fewer
workers without changing timeouts. Detailed compact observations and commands
are in [verification.json](verification.json).

[preflight-results.json](preflight-results.json) preserves the original full
preflight result: **44 checks passed, eight failed**, exit 1. It is not a green
preflight result. Corrective runs and remaining failures are distinguished in
the verification ledger; the full preflight was not repeated wholesale.

The workspace typecheck and build checks pass. Final ESLint checks pass over
all 3,159 owned demo files, with no warnings. Core: 704 passed;
validation: 469 passed; watcher: 1,589 passed/3 skipped; committee: 732
passed/1 skipped; SDK: 876 passed; Lucid: 175 passed; node-tools: 292 passed.
All three manually required transaction-preparation commands completed with
exit 0. Repository tooling: 197 passed; skill tooling: 189 passed. Rust: nine
passed; native-owner Node tests: 24 passed. Go package tests pass. The size
boundary test accepts exactly 500 lines and rejects line 501.

The fault-proof full run passed 4,754 tests, failed the scenario registry and
skipped four tests. Its repaired registry passed all six checks separately;
the complete 34-minute suite was not repeated after that test-only fix.

The PostgreSQL node full run passed 2,717 tests, failed three, skipped five,
and reported one todo. A corrective run passed all 12 ledger-repair cases and
eight migration cases; its two migration failures remain. The broad
watcher-journey run passed 401, failed four and skipped 106; all four failures
reproduced against untouched HEAD journey sources with the same dependencies.

## Remaining failures and limits

- Two migration assertions expect a single fresh migration/version 1, while
  unchanged HEAD declares versions 1 and 2. Both also fail with untouched HEAD
  test sources in a scratch run; the manifest and migration SQL were not
  changed to satisfy stale expectations.
- Two settlement journey rows expect seven-day maturity but observe the
  configured 900,000-ms maturity. Two timing rows expect an allowance differing
  by 29,970 ms. All four reproduce on HEAD journey sources. Their protocol
  assumptions were not changed as part of module cleanup.
- One ledger-repair admission claim assertion failed during the concurrent
  full node run and passed in the isolated 12-case rerun. Its root cause was
  not established, so this report does not claim it was proven pre-existing.
- The initial formatting/typecheck failures were in concurrently developed
  `full-stack` files. This refactor preserved that work. The final typecheck
  passes; the final formatting result is recorded in the ledger.
- Live process-devnet acceptance, skipped opt-in deployment journeys and
  production release acceptance were not run. No live deployment/reset was
  needed to verify declaration moves. Skipped cases are not counted as passes.
- Structural move proofs exclude imports, export surfaces and evaluation
  order. Independent composition reviews and runtime/test checks supplement
  them. Explicit type annotations, cosmetic parentheses and the documented
  source-guard/pinning repairs are the reported non-identical declarations.
