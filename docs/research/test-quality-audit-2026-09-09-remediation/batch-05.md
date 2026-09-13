# Batch 05 report — 10 files in demo/midgard-node

Branch `colll78/canonical-v1-watcher-l1-source-checkpoint`, working on top of the
existing uncommitted tree. Nothing was stashed, reset, checked out, committed or
pushed. No `.hs` file was touched.

All ten files are **done**. Every batch file is green, `npx tsc --noEmit` for the
package is clean, and eslint/prettier are clean over every file touched.

## Environment note (differs from the brief's assumption)

The scratch Postgres on 127.0.0.1:5433 **was reachable** for this batch, so the
node vitest `globalSetup` provisioned its per-worker shards normally and the
DB-backed work in `migration-runner.test.ts` actually ran (10/10 green) instead
of being reported NOT RUN. Two mutation runs used a throwaway shard prefix
(`MIDGARD_TEST_DATABASE_PREFIX=midgard_mut1|midgard_mut2`); those ten scratch
databases were dropped afterwards.

The Aiken work below needs the pinned fork: the local default `aiken` is v1.1.19
while `onchain/aiken/aiken.toml` pins `compiler = "v1.1.23"`, and the stock
binary exits 1 with **no diagnostics at all**. The fork at
`/home/gumbo/.aiken/bin/aiken-fork` (`aiken v1.1.23+5adf783`) is the one that
works, and its `fmt` output differs from v1.1.19's — that matters for the
generated golden below.

## Note on the failure-evidence records below

For files 1-7, 9 and 10 the mutations were run earlier in this session and the
_failing assertion_ is identified by the assertion expression it fired on; only
files 2 and 8 (and the two scheduler cases in file 3) carry the runner's verbatim
message text, which I captured directly. Nothing below is a mutation that was not
actually run, but where a line is not in `code quotes` from the runner it is my
identification of the assertion, not a transcript.

## Production / non-test files touched (called out per COMMON.md)

| file                                                                           | change                                                                                                     | why                                                                                                                                  |
| ------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| `demo/midgard-node/scripts/lib/deadline-batched-schedule.mjs`                  | **new** — extracted `scheduledStartCountDue` + `runDeadlineBatchedSchedule` with injectable `now`/`sleep`  | batch-05 row 3 `by_construction` asks for the scheduler to be exported and tested with a fake clock instead of grepped               |
| `demo/midgard-node/scripts/throughput-valid-stress.mjs`                        | scheduler block replaced by an import of the new module                                                    | same row; behaviour unchanged                                                                                                        |
| `demo/midgard-node/scripts/generate-transition-trace-abi-fixture.mjs`          | **new** generator with `--check`                                                                           | batch-05 row 8 `by_construction`                                                                                                     |
| `demo/midgard-node/package.json`                                               | `fixtures:transition-trace-abi` / `:check` replace the two `UPDATE_TRANSITION_TRACE_ABI_FIXTURE=1` scripts | same row                                                                                                                             |
| `.github/workflows/midgard-node-ci.yml`                                        | one new step, `Check transition-trace ABI golden vectors`                                                  | row 8 says "with a --check step wired into CI"; the row's `recommend` is `strengthen`, not `wire-into-ci`, so flagging it explicitly |
| `onchain/aiken/validators/fraud-proofs/transition-trace/abi-v1-golden.test.ak` | **new, generated** — 85 Aiken tests consuming the golden bytes                                             | same row ("have an Aiken golden consume the same bytes")                                                                             |
| `demo/midgard-node/vitest.config.ts`                                           | one comment line added to the DB-touching inventory                                                        | `migration-runner.test.ts` now touches the DB                                                                                        |

No production `src/` file was modified. Every file mutated for failure evidence
was restored byte-for-byte (verified with `diff -q <backup> <file>`, not
`git diff`, because the tree carries unrelated uncommitted edits):

```
OK  demo/midgard-node/src/fibers/da-publication-trigger.ts
OK  demo/midgard-node/src/database/migrations/runner.ts
OK  demo/midgard-node/src/database/migrations/sql/0001_initial_schema.sql
OK  demo/midgard-node/src/services/midgard-contracts.ts
OK  demo/midgard-node/src/transactions/reference-scripts.ts
OK  demo/midgard-node/src/workers/commit-block-header/da-payload.ts
OK  demo/midgard-core/src/deployment-manifest-identity.ts
OK  demo/midgard-sdk/src/ledger-state.ts
OK  demo/midgard-node/scripts/lib/deadline-batched-schedule.mjs
OK  onchain/aiken/lib/midgard/ledger-state.ak
```

---

## 1. tests/commit-submission-publication-order.test.ts — rewrite

**Contract.** DA publication must start only after the L1 control-plane permit is
released, and must not block local mutation completion.

**What changed.** The file is rewritten around
`runAfterL1ControlPlaneRelease` (`src/fibers/da-publication-trigger.ts`).

- Deleted the source-grep ordering "proof" (it read the `.ts` of
  block-commitment / speculative-commit-builder and asserted substring order),
  the `durablePublicationBacklog === 4` tautology (the test's own `Effect.sync`
  set it), the readiness-reason tautology, and the duplicated `path` loop that
  ran the identical test twice. `evaluateReadiness` is no longer imported.
- The ordering is now observed at runtime: one `trace: string[]` receives
  `mutation-start`, `mutation-end`, `publish-start`, `publish-end` from the real
  continuation, and the assertion is on the interleaving.
- The permit probe is kept and sharpened: while publication is blocked on a dead
  peer, `l1ControlPlane.withPermitsIfAvailable(1)` must succeed, and the caller
  fiber must not have settled (`Fiber.poll` is `None`).
- The published header hash is captured, so a wrong hash is caught, not just a
  publish/no-publish signal.
- Two new negatives: no finalized header hash ⇒ no publish; a failing L1 effect
  ⇒ no publish **and** the permit is released.

**Failure evidence.**

| mutation (`src/fibers/da-publication-trigger.ts`)                                | failing assertion                                                                                                     |
| -------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| publish the wrong header hash (hard-coded constant instead of the projected one) | `expect(publishedHeaderHashes).toEqual([FINALIZED_HEADER_HASH])`                                                      |
| move the publication call inside the L1 permit                                   | the `withPermitsIfAvailable` probe returns `None` — `expect(Option.getOrUndefined(permit)).toBe("reacquired")`        |
| publish even when the projection yields `undefined`                              | `expect(publishedHeaderHashes).toEqual([])` in "does not publish when the L1 result carries no finalized header hash" |

An earlier attempt (`yield* Effect.fork(publish(headerHash))`) hung the file to
its 420s timeout with empty output; it was killed, reverted from `/tmp/dpt.bak`
and replaced by the wrong-header-hash mutation above, which fails precisely.

**Verification.** `npx vitest run tests/commit-submission-publication-order.test.ts`
green (in the group-A run below); eslint / prettier / tsc clean.

---

## 2. tests/migration-runner.test.ts — rewrite

**Contract.** The SQL statement splitter handles quoting/comments/dollar-quoting,
and the single fresh-install migration's constraints exist.

**What changed.**

- The circular checksum/manifest assertions (recomputing sha256 with the same
  formula `src/database/migrations/index.ts` uses) are gone.
- The nine `toContain`/`toMatch` assertions over the raw DDL text of
  `MIGRATIONS[0].sql` — a source-text oracle — are replaced by a **DB-backed**
  `describe("applied fresh-install schema")` that drops and recreates `public`,
  runs `MigrationRunner.migrate` through `provideDatabaseLayers` + `BatchSql`,
  and asks Postgres itself:
  - `pg_class.relpersistence = 'u'` must equal exactly `["mempool_tx_deltas"]`
    (only the rebuildable delta cache may lose its WAL);
  - all ten `REQUIRED_CHECKS` constraints must exist, and each named semantic
    fragment must appear in `pg_get_constraintdef(...)` — Postgres's own
    normalized rendering, not the file text.
- A splitter property test replaces the text pinning:
  `splitSqlStatements(statements.join(";\n") + ";")` must equal `statements`
  (idempotence), with `> 100` statements and no empty statement.
- `vitest.config.ts` gained one line in the DB-touching inventory comment.

**Failure evidence** (fresh shard prefix so the migration checksum row matches
the mutated file — without that the runner's own checksum guard fires first and
masks the schema assertion):

| mutation (`src/database/migrations/sql/0001_initial_schema.sql`)                    | failing assertion                                                        |
| ----------------------------------------------------------------------------------- | ------------------------------------------------------------------------ |
| `CREATE UNLOGGED TABLE public.mempool_tx_deltas` → `CREATE TABLE`                   | `expected [] to deeply equal [ 'mempool_tx_deltas' ]`                    |
| delete the `foreign_tip_reconciliations_verified_da_nonempty_check` constraint line | `expected [ Array(1) ] to deeply equal []` (the missing-constraint list) |

Both reverted (`diff -q /tmp/schema.bak …` clean) and re-run green.

**Verification.** `npx vitest run tests/migration-runner.test.ts` → 10 passed.

---

## 3. tests/benchmark-regression.test.mjs — strengthen

**Contract.** Benchmark-harness contracts: Stage-A window and starvation gates
fail closed; corpus manifest/row/index parsing accepts only the canonical shape;
Phase 1 formal binding refuses hostile overrides and identity drift; the Class A
regression gate bootstraps below five entries and fails >10 % drops.

**What changed.**

- The three tests that grepped ~40 substrings and `indexOf` orderings out of
  `scripts/throughput-valid-stress.mjs` are gone. The scheduler they were
  guarding is now real code: `scripts/lib/deadline-batched-schedule.mjs` exports
  `scheduledStartCountDue` and `runDeadlineBatchedSchedule` with injectable
  `now`/`sleep`, and `throughput-valid-stress.mjs` imports it.
- New `describe("deadline-batched open-loop scheduler")` with a `makeFakeClock`
  and five behavioural tests (due-count schedule across an interval boundary,
  batched dispatch, no busy-wait, deadline catch-up, termination).
- The `dependencies.undici === "^7.25.0"` pin is dropped (hand-maintained range,
  no behavioural contract).
- The eight Phase-1 binding mutations no longer use bare `.toThrow()`: they are
  named `{name, mutate, expected}` cases carrying the exact refusal regex from
  `parsePhase1FormalBindingDocument`.

**Failure evidence.**

| mutation                                                                                                                                                    | failing assertion                                           |
| ----------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------- |
| replace the deadline-batched dispatch with `await sleep(Math.max(1, Math.ceil(intervalMs)))` per dispatch (the exact regression the greps existed to catch) | `expected 200 to be less than or equal to 25` (sleep count) |
| off-by-one in `scheduledStartCountDue` (`Math.floor` → `Math.ceil`)                                                                                         | the due-count sequence assertion `[0,1,1,2,4,5,5]`          |

Noted honestly: the `&& !dispatchedAny` mutation does **not** redden the batching
test, because it removes batching _and_ the sleeps together; the sleep-per-
dispatch mutation above is the representative one.

A first attempt at the due-count expectation was wrong on my side
(`[0,1,1,2,5,5,5]`; 139 ms is 3.9 intervals, so 4 are due) and was corrected to
`[0,1,1,2,4,5,5]` with a comment.

**Verification.** `npx vitest run tests/benchmark-regression.test.mjs` → 40 passed.

---

## 4. tests/midgard-contracts.test.ts — strengthen

**Contract.** Real (non-placeholder) validators resolve for every protocol role,
and a deployment manifest that disagrees with config or has been tampered with is
refused.

**What changed.** The ~25 `not.toEqual(placeholder)` weak assertions and the
hand-copied title constants are replaced by inventory-wide checks over a new
helper `tests/helpers/script-inventory.ts` (`collectScriptInventory` walks the
`MidgardValidators` tree collecting `{path, kind, cbor, script, declaredHash}`
for every spending/minting/withdrawal slot; 609 entries measured):

```ts
expect(
  inventory
    .filter(({ cbor }) => placeholderCbors.has(cbor))
    .map(scriptInventoryId)
    .sort(),
).toEqual(EXPECTED_STAND_INS); // ["escapeHatch:minting","escapeHatch:spending","hubOracle:spending","referenceScriptAuth:minting"]
expect(
  inventory
    .filter(
      (e) =>
        scriptInventoryId(e) !== "hubOracle:spending" &&
        validatorToScriptHash(e.script) !== e.declaredHash,
    )
    .map(scriptInventoryId),
).toEqual([]);
```

So the test now asserts _exactly which_ slots may be stand-ins (an accidental
fallback anywhere else fails) and that every declared hash equals the hash of the
script actually resolved. `hubOracle:spending` is excluded from the hash equality
by design: `hubOracle.spendingScriptHash = hubOracleMint.policyId`
(`src/services/midgard-contracts.ts` L983-991).

**Failure evidence.**

| mutation (`src/services/midgard-contracts.ts`)         | failing assertion                                                        |
| ------------------------------------------------------ | ------------------------------------------------------------------------ |
| point one real role at the always-succeeds placeholder | the stand-in set assertion — the extra id appears in the actual array    |
| return a declared hash from a neighbouring role        | the hash-equality filter returns a non-empty array against `toEqual([])` |

Restored from `/tmp/mc.bak` / `/tmp/mc2.bak` / `/tmp/mc3.bak`; a misleading
`git diff --stat` (134/167 lines) after the revert turned out to be the
pre-existing uncommitted diff versus HEAD, which is why `diff -q` is the criterion
used throughout this report.

**Verification.** `npx vitest run tests/midgard-contracts.test.ts` → 3 passed (12.4 s).

---

## 5. tests/harmonic-uplc-contract-eval.test.ts — strengthen

**Contract.** The real fraud-proof-catalogue minting policy evaluates to a CEK
constant under a ledger-shaped script context.

**What changed.** The lone `result instanceof CEKConst` (script did not error)
gets its missing negative half. New helpers:

```ts
const evaluateScriptContext = (cbor, ctx) =>
  Machine.eval(new Application(parseUPLC(fromHex(cbor), "cbor").body, dataConst(ctx))).result;
const expectScriptContextAccepted = (...) => expect(evaluateScriptContext(...)).toBeInstanceOf(CEKConst);
const expectScriptContextRefused  = (...) => expect(evaluateScriptContext(...)).not.toBeInstanceOf(CEKConst);
```

plus `genesisMint(contracts)` (the accepted coupled genesis mint) and four
`it.each` refusal cases mirroring the Aiken-side immutability negatives
(standalone catalogue re-mint without the hub-oracle NFT, duplicate catalogue
token, wrong policy, wrong asset name). The documented skip for the upstream
builtin bug is left exactly as it was — it names its re-enable condition.

**Failure evidence.** With the policy's `quantity_of(mint, hub_oracle_script_hash,
hub.asset_name) == 1` conjunct forced true (evaluating a policy variant that drops
the coupling), the refusal cases flip: `expect(result).not.toBeInstanceOf(CEKConst)`
fails on the standalone re-mint case while the accept case stays green — i.e. the
pair discriminates, which the single accept assertion could not.

**Verification.** green in group A (1 test skipped is the documented upstream-bug skip).

---

## 6. tests/network-id-forced-step-deployment.test.ts — wire-into-ci

## 7. tests/missing-signature-forced-deployment.test.ts — wire-into-ci

**Contract (6).** network-id forced step and forced scan each get their own
manifest role, a distinct applied script hash outside the linear steps, and their
own reference-script publication target. **(7)** the same for each
missing-signature auxiliary script (forcedStep / forcedSigner / forcedWitness).

**What changed (both files).**

- `describe.skipIf(...)` is gone. A `beforeAll` asserts `existsSync(blueprintPath)`
  and that the blueprint carries the required validator titles, failing closed
  with a message naming `aiken build --env testnet` — so a clean checkout without
  the (gitignored) blueprint **fails**, it does not silently run one constant
  comparison.
- The hand-transcribed copy of `DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE`
  is deleted. Roles are derived from the production map by a new shared helper
  `tests/helpers/forced-script-deployment.ts`
  (`auxiliaryManifestRoles(contractPrefix)` → `{role, contract, field}`;
  `expectBlueprintCarriesValidators(...)`).
- New test in each file: "exposes exactly the forced fields the manifest roles
  name" — cardinality and distinctness of the derived roles, their applied hashes
  distinct from each other and outside the linear steps
  (`LINEAR_STEP_COUNT` 2 and 4 respectively).

**CI.** No workflow edit was needed: `midgard-node-ci.yml` already runs
`aiken build --env testnet` before the node suite, so removing the skip guard is
what makes these run in a required lane. That is why the only workflow edit in
this batch is the one for file 8.

**Failure evidence.** Renaming one derived role's contract field in the
production map (`src/services/deployment-manifest.ts` side of the role table)
makes `auxiliaryManifestRoles` return a role whose applied hash collides with a
linear step; the new test fails on the distinctness assertion. Removing the
blueprint makes the `beforeAll` fail with the `aiken build --env testnet`
message instead of skipping (verified by pointing `MIDGARD_REAL_BLUEPRINT_PATH`
at a non-existent file).

**Verification.** both green in group A.

---

## 8. tests/sdk-abi-fixtures.test.ts — strengthen

**Contract.** SDK TypeScript schemas, constants and hashes stay aligned with the
canonical Aiken blueprint ABI and the transition-trace CBOR encoding.

This is the file the row asked to turn into a real two-implementation contract.
Three changes.

### 8a. The golden is no longer written by the test that asserts against it

The in-test branch `if (process.env.UPDATE_TRANSITION_TRACE_ABI_FIXTURE === "1")
{ … writeFileSync(transitionTraceAbiGoldenPath, …); Object.assign(…) }` is gone.
What remains is an **emit mode** that only ever writes to a scratch path the
generator hands it (`MIDGARD_TRANSITION_TRACE_ABI_EMIT_PATH`) and asserts
nothing; a normal run has no code path that can rewrite
`tests/fixtures/transition-trace-abi.json`. Missing golden ⇒ the whole file fails
(verified: hiding the file gives `Error: ENOENT … transition-trace-abi.json` and
`Test Files 1 failed`), so rule 14 holds.

### 8b. A checked-in generator, a CI `--check`, and an Aiken golden that consumes the same bytes

`scripts/generate-transition-trace-abi-fixture.mjs`:

- default mode re-encodes the fixtures by running the ABI test in emit mode
  against a temp path, copies the result over the golden, and regenerates the
  Aiken golden;
- `--check` regenerates the Aiken golden **in memory from the checked-in JSON**
  and fails if the checked-in `.ak` differs. It is a pure file transform: no
  build, no database, no Aiken binary (see below), so it is cheap in CI;
- an unmapped schema is a hard failure (`AIKEN_TYPE_BY_SCHEMA` must list every
  schema the golden carries), so a new fixture family cannot silently escape the
  Aiken side. `DaPayloadBody` is explicitly mapped to `null` — it is an off-chain
  envelope with no on-chain counterpart. 85 of the 86 fixtures are consumed.

`onchain/aiken/validators/fraud-proofs/transition-trace/abi-v1-golden.test.ak`
(generated, 902 lines) decodes each fixture into its on-chain type
(`ledger_state.HeaderV1`, `ledger_state.ForcedInclusionTxV1`,
`ledger_state.EventKey`, `ledger_state.EventToStepValue`,
`ledger_state.TransitionStep`, `proof.TransitionFault`,
`proof.TransitionFaultProof`, `route_v1.SpendRedeemer`,
`final_v1.SpendRedeemer`) and re-serialises it byte for byte.

CI: one new step in `.github/workflows/midgard-node-ci.yml`,
`Check transition-trace ABI golden vectors`, placed before `Typecheck Midgard
node`. The three-link chain is: JSON golden ↔ SDK encoder (the node test),
JSON golden ↔ Aiken golden (`--check`), Aiken golden ↔ Aiken types
(`aiken check`).

**A real finding surfaced by building the channel.** On the first `aiken check`,
12 of 85 tests failed — all and only the fixtures carrying a `MidgardValue`
(withdrawal-bearing faults). The cause is not an ABI disagreement: lucid emits
Plutus Data **maps** with indefinite-length headers (`bf … ff`) while Aiken
re-serialises maps with definite headers (`a1 …`). Same value, different bytes.
So byte equality after an Aiken round trip is a property lucid does not provide
for map-carrying values. Rather than paper over it, the generator detects it
structurally (a minimal CBOR walk — never a raw byte scan, so a `0xbf` inside a
byte string cannot be mistaken for a map header) and emits, for those fixtures,
a decode plus an Aiken-side round-trip-stability assertion with the reason in a
comment; every map-free fixture keeps the strict `cbor.serialise(value) == bytes`
check. Any _other_ future divergence still reddens `aiken check` — fail-closed.

Also worth recording: the generator formats the `.ak` itself (80-column rules
replicated) instead of shelling out to `aiken fmt`, so `--check` is hermetic.
This was necessary because v1.1.19 and the pinned fork v1.1.23 format the same
source differently; the checked-in file matches the **fork**
(`aiken-fork fmt --check` clean).

### 8c. The baseline header hash is derived, not transcribed

`expect(baselineHash).toBe("964baf9a89b4c4aa99d8cb6f1b365af9fa951a4d8043a93c5da993c1")`
is gone. The test now uses `headerFixture` (the value behind the golden's
`HeaderV1` entry) and:

```ts
const goldenHeaderCborHex = transitionTraceAbiGolden.fixtures.HeaderV1!.cborHex;
expect(Data.to(headerFixture, SDK.Header)).toBe(goldenHeaderCborHex);
const expectedBaselineHash = Buffer.from(
  blake2b(Buffer.from(goldenHeaderCborHex, "hex"), { dkLen: 28 }),
).toString("hex");
expect(baselineHash).toBe(expectedBaselineHash);
```

i.e. the preimage comes from the checked-in golden (the same bytes the Aiken
golden decodes) and the digest is recomputed with `@noble/hashes` rather than
with `SDK.hashBlockHeader`'s own helper.

The 13-mutation sensitivity set is replaced by an exhaustive one:
`Record<keyof SDK.Header, SDK.Header | null>` — 24 varied fields plus
`protocolVersion: null` — so **adding a header field without deciding whether it
is committed fails to compile**, with a runtime key-set assertion as well. Each
mutated digest must differ from the baseline _and_ from every other mutated
digest (no two fields may share a commitment slot). The `null` entry is paired
with the missing reject case:

```ts
expect(() =>
  SDK.hashBlockHeader({
    ...headerFixture,
    protocolVersion: headerFixture.protocolVersion + 1n,
  }),
).toThrow(/protocol version/i);
```

### 8d. Documented the Aiken `pub const` regex channel

`aikenIntegerConst` keeps its regex read of `env/testnet.ak` /
`lib/midgard/ledger-state.ak` — the compiler emits none of those constants into
`plutus.json`, so no other channel exists — and now says so, and says why it is
fail-closed (missing declaration ⇒ failed lookup; anything but a product of
decimal literals ⇒ failed shape check, never a silent coercion).

**Failure evidence.**

| mutation                                                                                                                                                   | failing assertion                                                                                                                                                                                |
| ---------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `demo/midgard-sdk/src/ledger-state.ts`: `hashHexWithBlake2b(…, 28)` → `32`                                                                                 | `expected '57265554746b27cd9c2b662e36e0561441e5b…' to be '8c8f1b118c318d55342dd5a38ec1357e2645e…'`                                                                                               |
| same file: `Data.to(header, Header)` → `Data.to({ ...header, minFeeB: 0n }, Header)`                                                                       | `header field minFeeB is not committed: expected '8c8f1b…' not to be '8c8f1b…'`                                                                                                                  |
| same file: delete the `protocolVersion !== MIDGARD_PROTOCOL_VERSION` guard in `encodeHeaderCbor`                                                           | `expected [Function] to throw an error`                                                                                                                                                          |
| `onchain/aiken/lib/midgard/ledger-state.ak`: swap the field order of `EventToStepValue` (`step_index`/`phase`) — compiles fine, labels are used everywhere | `aiken-fork check` 25 of 85 golden tests fail, e.g. `transition_trace_abi_event_to_step_value: × program failed`                                                                                 |
| flip one hex digit in the generated `.ak`                                                                                                                  | `--check` fails: `generated artifact is stale: onchain/aiken/validators/fraud-proofs/transition-trace/abi-v1-golden.test.ak; run pnpm --dir demo/midgard-node run fixtures:transition-trace-abi` |
| hide `tests/fixtures/transition-trace-abi.json`                                                                                                            | `Error: ENOENT …` / `Test Files 1 failed` (fail-closed, not skip)                                                                                                                                |

All four production mutations reverted byte-for-byte and re-run green.

**Verification.**
`npx vitest run tests/sdk-abi-fixtures.test.ts` → 8 passed;
`node scripts/generate-transition-trace-abi-fixture.mjs --check` → clean;
`~/.aiken/bin/aiken-fork check -m 'fraud_proofs/transition_trace/abi_v1_golden'`
→ `{'total': 85, 'passed': 85, 'failed': 0}`;
`~/.aiken/bin/aiken-fork fmt --check <golden>` → clean.

**Not done (scope call, flagged).** The row also notes "many cases (L1064 onward)
assert only Data round-trip which per rule 4 does not establish wire
compatibility". Those cases are left as they are: the wire-compatibility gap they
represent is now covered for the transition-trace family by the golden + Aiken
channel above, and converting the remaining ~40 round-trip cases into golden
vectors is a larger fixture-design task than this row's `strengthen` warrants.

---

## 9. tests/contract-deployment-info.test.ts — strengthen

**Contract.** Deployment-manifest build/parse/reconstruct: role-token admission
refusals (duplicate live UTxO, wrong script, non-unit quantity, no scriptRef,
bundled roles); manifest id stability across rebuilds; tamper and schema/network
rejection; validators reconstructed from manifest contract bytes.

**What changed.**

- The `identity.digest === computeDeploymentManifestJsonDigest(identity.snapshot)`
  recompute and the `consensusProfile` / `validationDispute` self-comparisons
  (the builder copies those fields from `MIDGARD_CONSENSUS_PROFILE`) are removed.
- The file ran entirely on `AlwaysSucceedsContract`, where every fault-proof step
  shares one script, so the 8 `missingNativeScriptTx` step hashes and 8
  `transitionTrace` final hashes could not discriminate the step-index-to-role
  wiring at all. A new 600 s test, "discriminates step-index-to-role wiring
  against the real blueprint", uses `loadRealMidgardContractsForTest` and
  `expectOrderedDistinctWiring(label, appliedHashes, manifestHashes)`, which
  asserts **pairwise distinctness before** ordered equality
  (`missingNativeScriptTx.steps` ×8, `transitionTrace.finals` ×8,
  `networkId.steps` ×2).

**Failure evidence.** Swapping step indices 2↔3 for `category === "missingNativeScriptTx"`
in the manifest wiring makes exactly the new test fail on the ordered-equality
assertion while the AlwaysSucceeds tests stay green — which is precisely the
discrimination the row said was missing. (A first, over-broad version of this
mutation swapped indices for _all_ categories and broke four other tests by
producing non-existent contract names for 2-step chains; it was narrowed.)

**Verification.** `npx vitest run tests/contract-deployment-info.test.ts` → 20 passed (14 s).

---

## 10. tests/da-payload.test.ts — derive-oracle

**Contract.** The DA payload builder emits a canonical V1 payload whose roots,
counts, header and header hash match the pending-finalization journal; it refuses
a journal whose recomputed roots disagree; the backfill path reports
backfilled/skipped/status-excluded journals correctly.

**What changed.**

- `expect(insert.payload_sha256).toBe(SDK.daPayloadHashHex(insert.payload_cbor))`
  (the same function the builder used, so it could not fail) is replaced by an
  independent digest: `import { createHash } from "node:crypto"` and
  `createHash("sha256").update(Buffer.from(payload_cbor, "hex")).digest("hex")`,
  for both the identity and the zstd insert. Added
  `expect(zstdInsert.payload_sha256).not.toEqual(insert.payload_sha256)` so the
  two envelope encodings cannot be confused.
- The self-sorted key-list assertion (`keys` equals a sorted copy of itself) is
  replaced by an independently constructed expectation: the fixture now carries
  three utxo entries, and `expectedUtxoEntries` / `descendingUtxoInputs` are built
  from the _fixture inputs_, so `expect(payload.block_body.utxos).toEqual(expectedUtxoEntries)`
  fails if the builder emits input order rather than canonical order.

**Failure evidence.**

| mutation (`src/workers/commit-block-header/da-payload.ts`)     | failing assertion                                                                                                 |
| -------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| drop the canonical sort of the utxo entries (emit input order) | `expect(payload.block_body.utxos).toEqual(expectedUtxoEntries)` — the three entries come back in descending order |
| compute `payload_sha256` over the pre-envelope bytes           | the `node:crypto` digest comparison                                                                               |

Reverted (`diff -q /tmp/dap.bak …` clean) and re-run green.

**Verification.** green in group A.

---

## Verification summary (all from `demo/midgard-node`)

```
npx tsc --noEmit                                         → 0 errors
npx eslint <16 touched files> --max-warnings=0           → clean
npx prettier --check <16 touched files> package.json \
   ../../.github/workflows/midgard-node-ci.yml           → clean
node scripts/generate-transition-trace-abi-fixture.mjs --check  → clean

npx vitest run tests/commit-submission-publication-order.test.ts \
  tests/harmonic-uplc-contract-eval.test.ts \
  tests/network-id-forced-step-deployment.test.ts \
  tests/missing-signature-forced-deployment.test.ts \
  tests/da-payload.test.ts
   → Test Files 5 passed (5) | Tests 22 passed | 1 skipped (23)

npx vitest run tests/migration-runner.test.ts tests/midgard-contracts.test.ts \
  tests/sdk-abi-fixtures.test.ts tests/contract-deployment-info.test.ts \
  tests/benchmark-regression.test.mjs
   → Test Files 5 passed (5) | Tests 81 passed (81)

(onchain/aiken) ~/.aiken/bin/aiken-fork check -m 'fraud_proofs/transition_trace/abi_v1_golden'
   → 85 total, 85 passed, 0 failed
(onchain/aiken) ~/.aiken/bin/aiken-fork fmt --check <generated golden> → clean
```

Never more than two vitest processes at once; no whole-workspace command was run.

## Reds

None in this batch. During one intermediate `npx tsc --noEmit` run, four errors
appeared in `demo/midgard-fault-proofs/src/workflow/funding-reservation-permit.ts`
(`TS2305 readFraudSlashFundingAuthority`, three `TS7006`); they were gone on the
next run of the same command a few minutes later, so they were a transient
mid-edit state of a sibling package (other batch agents are working in
`demo/midgard-fault-proofs`), not caused by anything in this batch. None of the
known pre-existing reds (stale blueprint digest pins, watcher settlement
malformed_state, direct-frontier-exact refusal, fault-proof.test.ts stale
allowlist) is in this batch's file set.

## Rulings received

None. No escalation was needed: every decision was settled by the batch row's
`by_construction`, COMMON.md, or an observable property of the code.
