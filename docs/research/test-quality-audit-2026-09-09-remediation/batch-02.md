# Batch 02 report — demo/midgard-fault-proofs (9 files)

Repo: /home/gumbo/midgard-hub/midgard, branch `colll78/canonical-v1-watcher-l1-source-checkpoint`.
All work is uncommitted, on top of the existing working tree. Nothing stashed/reset/committed/pushed.
No `.hs` file touched. **No production `src/` file was left changed** — every mutation used for
failure evidence was reverted and byte-compared against a pre-mutation backup (`cmp -s`, all CLEAN).

## Verification commands (run from `demo/midgard-fault-proofs`)

| command                                                        | result                        |
| -------------------------------------------------------------- | ----------------------------- |
| `npx tsc --noEmit` (whole package)                             | green                         |
| `npx eslint <the 9 files> --max-warnings=0`                    | green                         |
| `npx prettier --check <the 9 files>`                           | green                         |
| `npx vitest run <the 9 files>` (`MIDGARD_FAULT_PROOF_FORKS=4`) | **9 files / 51 tests passed** |

The package's vitest config has no `globalSetup`, so no Postgres/Docker scratch config was needed.
`onchain/aiken/plutus.json` is present locally (22 MB, dated 2026-09-08) and is fresh enough that all
five publication-fit families and the script-integrity size budget resolve against it.
All nine files are already inside the required lane: `package.json` `"test": "vitest run"` with
`include: ["./tests/**/*.test.{ts,tsx}"]`. No CI/package.json edit was needed.

---

## 1. `demo/midgard-fault-proofs/tests/script-integrity-hash-mismatch.test.ts`

**Contract.** `scriptIntegrityHashMismatch` evidence closes only genuine accepted/forced integrity
contradictions; refuses bitmap/hash/reason substitution; the runner refuses category and
callback-authority substitution.

**Pre-existing state.** Two of the row's three `by_construction` items were already applied in the
working tree by an earlier pass (`git diff HEAD` on this file shows the `[14692;11817;1603;5401;1957]`
size pin and the whole tautological `keys`-array test already deleted). I did not redo those.

**What I changed.**

- Gave the four language-view vectors real provenance instead of the unsourced
  "authoritative Cardano language-view vectors" claim. They are cross-implementation vectors: the
  on-chain Aiken test `onchain/aiken/lib/midgard/script-language-views-v1.test.ak`
  (`script_integrity_language_view_vectors_match_typescript`) asserts exactly these four byte strings
  for bitmaps 0..3 over the same all-`0x11` redeemer hash, and
  `demo/midgard-core/tests/script-language-views.test.ts` asserts them for the codec. Added that
  citation as a block comment and renamed the case to
  `"derives the on-chain expected hash for every language bitmap"` — the claim it actually supports
  is that the family's bitmap→language wiring reproduces the second implementation's answer. Added a
  per-bitmap assertion message.
- Replaced `expect(CONFIG_KEYS).not.toEqual(expect.arrayContaining(["evidence","actuator","verdict","submit"]))`
  with a per-key `not.toContain` loop. The `not.arrayContaining` form is satisfied by omitting any
  _one_ of the four, so it did not state the "none of these may appear" claim (rule 4).

**Failure evidence.**

- Mutation: `src/script-integrity-hash-mismatch/family.ts`, `languagesForIntegrityBitmap`,
  `bitmap >= 2` → `bitmap >= 3`.
  Failing assertion: `derives the on-chain expected hash for every language bitmap` →
  `bitmap 2: expected '01f4b788593d4f70de2a45c2e1e87088bfbdf…' to be '71201d25ea11e4104eda108782a7d67b37b4a…'`.
- Mutation: `src/script-integrity-hash-mismatch/manifest-workflow.ts`, added `"verdict"` to
  `SCRIPT_INTEGRITY_HASH_MISMATCH_CONFIG_KEYS`.
  Failing assertion: `exposes a strict shared-runtime loader and refuses category substitution` →
  `expected [ 'manifest', 'blueprintJson', …(9) ] to not include 'verdict'`.
  (The pre-existing `not.arrayContaining` form did **not** fail on this mutation.)
  Both reverted; `cmp -s` against backups CLEAN.

---

## 2. `demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-phase-a-item.test.ts`

**Contract.** Forged phase-A native/foreign item successors run to award and removal on the emulator;
a dishonest challenger and an opposite-yield claim are refused at semantic resolution; a cancelled
prepared item can be re-attempted; every submitted transaction stays inside the exec-unit budget.

**What I changed.**

- **Exec-unit ceilings now always run.** 13.2M memory / 8G CPU moved out of
  `if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return` and out of `afterAll` entirely: they are
  asserted inside `onSubmittedTransaction`, on _every_ submitted transaction of _every_ scenario, with
  a label naming the scenario, attempt and transaction kind. Previously they never executed in CI
  (§14 fail-open).
- **Deleted the ledger side effects.** The `/tmp/nip-phase-a-item-measurements.json` write and the
  `docs/fault-proofs/size-plans/validation-trace-phase-a-native-item-fit-ledger.json` writer are gone,
  along with the `VanRossemFitMeasurement` row accumulation and the `readFileSync`/`createHash`/
  `fileURLToPath` imports that fed them.
- **Discovery/outcome gate replaced, not deleted.** The old `expect(completed).toBe(6)` was _not_
  stale as the row assumed — `completed++` only runs on scenarios that return, and 4 of the 10
  invocations are refusals, so 6 was correct but silently under-stated the file. It is now
  `expect({ started, completed }).toEqual({ started: 10, completed: 6 })`, which pins both the number
  of scenarios discovered and the accept/reject split (§5: an always-refusing implementation cannot
  satisfy it; §14: a scenario that stops being generated fails the file).
- **Positive cases discriminate.** All six `expect(txHash).toHaveLength(64)` /
  `expect(removal?.transactions.length).toBeGreaterThan(0)` pairs replaced by one
  `expectAwardedAndRemoved(result)` helper that asserts: award `txHash` matches `/^[0-9a-f]{64}$/`;
  removal ran; **every** removal transaction's `removedHeaderHash` equals `result.setup.headerHash`
  (`removedHeaderHash` is read back off the state-queue node's own asset name by
  `requireStateQueueHeaderHash`, so it is an L1-derived value, not an echo of the submitted argument);
  `removal.fraudProofOutRef` matches `/^[0-9a-f]{64}#\d+$/` (the located minted proof unit); and
  `fraudCategory`/`fraudCategoryId` are `"validationTraceDispute"` / `"00000006"` — the id taken from
  `demo/midgard-sdk/src/fraud-proof/catalogue.ts`, stated as a literal rather than read back from the
  runner. The cancellation case additionally asserts `cancelled.removal` is undefined.

**Deviation from `by_construction` (flagged).** The row said "move the ledger writer to a bench script
with `--check`". I deleted it instead. Rule 15 record: protected contract = exec-unit fit, which is now
enforced by the always-on ceilings; existing consumer = **none** (the target ledger JSON is not checked
in and no code, script, doc or CI job references that path — verified by repo-wide grep, the only hit
was this test file); replacement protection = the ceilings. Building a new bench script for a ledger
nothing reads would have added the waste rule 15 asks to remove. The repo's live replacement for this
pattern (`tests/support/measured-fit-ledger.ts` + `verifyMeasuredFitLedger`) exists and is being
adopted elsewhere in this working tree; if the owner wants this family's fragment produced, that is
the mechanism to wire in, not a bespoke writer.

**Failure evidence** (all with `-t "proves the native phase-A item successor"`; the outcome gate also
fails under a filtered run, as documented in the file — the intended assertion is quoted below):

- Mutation: `src/remove-fraudulent-block.ts`,
  `const removedHeaderHash = await requireStateQueueHeaderHash(removed)` →
  `` `00${(await requireStateQueueHeaderHash(removed)).slice(2)}` ``.
  Failing assertion (line 101): `expect(removal.transactions.map((tx) => tx.removedHeaderHash)).toEqual(...)`
  → `expected [ Array(1) ] to deeply equal [ Array(1) ]`. The replaced
  `transactions.length > 0` check passed under this mutation.
- Mutation: `src/remove-fraudulent-block.ts`, `fraudCategoryId: contracts.fraudCategoryId` →
  `fraudCategoryId: "00000007"`.
  Failing assertion: `expected '00000007' to be '00000006'`.
- Controlled fault for the exec-unit gate: `tests/support/emulator/measurement.ts`,
  `executionMemory += exUnits.mem()` → `* 4n` (an over-budget build, injected at the measurement
  boundary rather than by making a validator genuinely cost more).
  Failing assertion: `proves the native phase-A item successor through permanent proof and removal/attempt-0/lifecycle memory units: expected 14237320 to be less than or equal to 13200000`.
  This is the §14 defect being fixed: with the env gate in place this assertion could not run at all.
  The real measured figure is ~3.56M memory units, well inside the ceiling.
  All three reverted; `cmp -s` CLEAN.

**Runtime.** 104 s for the file (9 tests), unchanged by these edits.

---

## 3. `demo/midgard-fault-proofs/tests/spend-input-signer-missing-authenticated-workflow.test.ts`

**Contract.** The `spendInputSignerMissing` runner surface is callback-free; reference out-refs must
match the finalized manifest identity for their role; stage actions must match the authenticated
raw-L1 stage.

**What I changed.**

- Deleted `expect(Object.keys(runner).sort()).toEqual(["runOrResume","runnerVersion"])` and
  `expect(Object.values(MANIFEST_CONTRACTS)).toEqual([…9 hand-copied strings…])`.
- Callback-freedom is now stated as a _property_: `runOrResume` is the surface's only callable member
  (`Object.entries(runner).filter(([, m]) => typeof m === "function").map(([n]) => n)` equals
  `["runOrResume"]`). A rename cannot fail it; an added hook must. I first tried the row's suggested
  `expectTypeOf(...).returns.toEqualTypeOf<WorkflowAdapterRunner>()` and **rejected it as
  tautological (rule 2)**: the factory's declared return type _is_ `WorkflowAdapterRunner`, so both
  sides of the type assertion move together. Worth recording: `Object.freeze({...})` defeats
  TypeScript's excess-property check, so an extra `verdict` member on the runner compiles cleanly —
  the runtime property check is genuinely load-bearing here.
- Added a behavioural refusal for the runner: a foreign `category` rejects with
  `/category mismatch: unusedRedeemer/` **and** does not call `loadRuntimeConfig` (§5 — the refusal
  contract also prohibits the load side effect).
- Replaced the circular accept case. The manifest identity is now an explicit literal table
  (`MANIFEST_IDENTITY`: contract name → published UTxO) instead of being zipped from `supplied` in an
  order the test hand-wrote, so a production change that re-pointed a role at another contract's
  publication has to disagree with the table.
- Manifest completeness is now behavioural instead of transcribed: `it.each` over the nine contract
  names withholds exactly one from the binding and requires
  `finalized manifest has no published reference-script identity for <name>`.
- Added two causal negatives the file did not have, each holding all other validity requirements:
  a step02/step03 **role swap** (right UTxOs, wrong roles) and a **script substitution** at the right
  out-ref (which can only be caught by the script-hash check — different error text). Kept the
  original `outputIndex: 99` case. Kept the raw-L1 stage-resolver case unchanged.
- One small transcription remains, deliberately: `names every role the production table exposes`
  keeps `MANIFEST_IDENTITY` honest about the role set, so a new production role cannot appear without
  gaining the refusal coverage above. It is stated as that claim, not as a surface pin.

Test count on this file: 3 → 17.

**Failure evidence.**

- Mutation: `src/spend-input-signer-missing/authenticated-workflow.ts`, `step02` bound with
  `contractName: names.step03`.
  Failing assertion: `accepts the finalized manifest identity for every role` →
  `fraudProofSpendInputSignerMissingStep03 reference UTxO differs from finalized manifest identity`.
  **This is the mutant the deleted `Object.values(...)` transcription could not catch.**
- Mutation: `src/workflow/deployment-manifest-binding.ts`, `requireManifestBoundReferenceScriptUtxo`,
  the `expected === undefined` throw → `return utxo`.
  Failing assertions: all nine `refuses a manifest that publishes no identity for <name>` cases.
- Mutation: same family file, added `verdict: () => undefined` to the frozen runner object.
  Failing assertion: `exposes no callable member besides the single drive method` →
  `expected [ 'verdict', 'runOrResume' ] to deeply equal [ 'runOrResume' ]`.
  All reverted; `cmp -s` CLEAN.

---

## 4. `demo/midgard-fault-proofs/tests/spend-input-witness.test.ts`

**Contract.** A 180-item spend-input witness decodes from canonical §5.3 CBOR; the surviving min-Ada
helper prices a §8.5 nothing-but-bytes inline-datum output at the ledger minimum; protocol parameters
resolve from Lucid's construction-time cache before a provider round trip. This module's three exported
helpers had **no other test anywhere in the repo** (verified by grep), so the negatives added here are
new coverage, not duplication.

**What I changed.**

- **Replaced the `> 5_000_000n` floor with an independent exact oracle.** The Babbage/Conway rule
  (CIP-55) is `coinsPerUtxoByte * (160 + |serialized output|)`; the function under test searches for
  the fixpoint of `CML.min_ada_required`. The test now computes the ledger formula directly from
  `output.to_cbor_bytes().length` and asserts (a) **sufficiency** — the returned coin equals the
  formula for the output that carries it (32,915,470 lovelace at the default `coinsPerUtxoByte` of 4310) — and (b) **minimality** — one lovelace less does not fund the output it would sit in. So the
  fixpoint is pinned as the least sufficient value, not as "some large number".
- **Removed the Emulator + Lucid spin-up** (the row's note). `PROTOCOL_PARAMETERS_DEFAULT` supplies
  `coinsPerUtxoByte` and a fixed `credentialToAddress("Preprod", keyHashToCredential("ab"×28))`
  supplies the address, whose bytes are load-bearing for the size and are now stated in the test.
  File runtime dropped from ~30 s (emulator) to ~2.8 s.
- **`resolveProtocolParameters` keeps its coverage at the right scope** (§13) rather than losing it
  with the emulator: three focused cases — configured parameters win _and the provider is never
  called_ (the caching behaviour the old comment described but never checked), provider fallback when
  Lucid carries none, and a refusal when neither is configured.
- **`toHaveLength(180)` no longer echoes the construction.** The decoded list is asserted against the
  exact 180 `{tx_id, output_index}` pairs the scenario numbered, which a decoder that dropped,
  reordered, truncated, or mis-offset items cannot reproduce.
- **Added the missing refusal (§5).** Starting from a demonstrated-valid item, exactly one thing is
  changed: the output index re-encoded in CBOR's shortest form (`01` instead of the §5.3 fixed
  `19 <uint16>`), then a 3-element array of the same total length — both required to fail with the
  specific `test.inputs[0] is not valid Midgard §5.3 TxOutRef CBOR` category — paired with the
  untouched item still decoding to its exact value.

**Failure evidence.**

- Mutation: `src/spend-input-witness.ts`, `CML.min_ada_required(..., coinsPerUtxoByte)` →
  `coinsPerUtxoByte - 1n`.
  Failing assertion: `prices a §8.5 nothing-but-bytes witness datum at the exact ledger minimum` →
  `expected 32907833n to be 32915470n`. **The replaced `> 5_000_000n` floor passed under this
  mutation.**
- Mutation: same file, `requireCanonicalInputCbor`'s catch clause replaced by a lenient
  `input = { txId: new Uint8Array(32), outputIndex: 0 }`.
  Failing assertion: `refuses a spend-input item that is not the canonical 38-byte §5.3 form` →
  `expected [Function] to throw error matching /test\.inputs\[0\] is not valid Midga…/u but got
'test.inputs[0] must be canonical Midg…'` — i.e. the test discriminates _which_ refusal route fired.
- Mutation: same file, `output_index: BigInt(input.outputIndex)` → `BigInt(index)`.
  Failing assertion: `decodes a high-cardinality witness into its exact §5.3 out-refs` →
  `expected [ Array(180) ] to deeply equal [ Array(180) ]`.
- Mutation: same file, `resolveProtocolParameters`'s `config.protocolParameters !== undefined`
  early return deleted.
  Failing assertion: `prefers the parameters Lucid was constructed with over a provider round trip`.
  All reverted; `git diff` on `src/spend-input-witness.ts` empty.

---

## 5–9. The five publication-fit files

- `demo/midgard-fault-proofs/tests/submit-init-emulator-mint-declared-asset-limit-publication.test.ts`
- `demo/midgard-fault-proofs/tests/execution-native-script-invalid-publication-fit.test.ts`
- `demo/midgard-fault-proofs/tests/missing-script-source-publication-fit.test.ts`
- `demo/midgard-fault-proofs/tests/execution-source-script-decoding-publication-fit.test.ts`
- `demo/midgard-fault-proofs/tests/receive-purpose-language-publication-fit.test.ts`

**Contract (each).** Every fully applied validator of the family publishes as a signed reference-script
UTxO under the 15,872-byte reliability reserve.

**What I changed (uniformly).**

- **`describe.runIf(hasFamily)` → fail-closed discovery.** Each file now computes `missingTitles` and
  opens with a required case, e.g.
  `it("finds every declared missingScriptSource validator in the deployed blueprint", () => expect(missingTitles).toEqual([]))`.
  A blueprint missing the family now fails the file instead of skipping it green (§14).
- **Closed a second fail-open the row did not name.** Each file looped over `steps.entries()` with no
  cardinality check, so an empty or short applied-script array would have produced _zero_ budget
  assertions and a green pass. Each now asserts the count its own title states — 4 / 6 logical + 7
  accepted-prelude / 6 / 5 / 3 — as a reviewed literal, not as `TITLES.length`.
- `execution-native-script-invalid`: dropped `rawSizes`, computed only to be logged.
- `missing-script-source`: dropped the per-script sha256 `digests` array (computed only to be
  `console.info`'d, no acceptance criterion) and moved the stray `import { createHash }` that sat
  _after_ the `describe` block — the import is now unnecessary and gone.
- `mint-declared-asset-limit`: left the `createMeasuredFitRecorder` call as-is. It is the working
  tree's established, env-gated, fresh-fragment-disciplined ledger mechanism (already used by ~20
  sibling files) and is not a bare test side effect; the row's "no assertion in this file" is answered
  by the two assertions added above.
- Diagnostic `console.info` of signed sizes kept where the row did not object; it is labelled
  diagnostic, not an oracle.

**Failure evidence.**

- Mutation (all five, one blueprint title per family renamed in `src/<family>/contracts.ts` so the
  blueprint no longer carries it, e.g. `fraud_proofs/missing_script_source/step_01.main.spend` →
  `…/step_99.main.spend`; for `execution-native-script-invalid`,
  `accepted_reconstruction_init.main.spend` → `accepted_reconstruction_zz.main.spend`):
  **all five `finds every declared … validator in the deployed blueprint` cases failed** (10 tests
  failed across the 5 files).
  Control for the same fault against the pre-change file: `git show HEAD:…/receive-purpose-language-publication-fit.test.ts`
  run under the identical mutation reported `Test Files 1 skipped (1) / Tests 1 skipped (1)` — a
  green run. That is the §14 fail-open being closed.
- Mutation: `src/execution-source-script-decoding/contracts.ts`,
  `return [step01, step02, step03, step04, step05]` → `return [step01, step02, step03, step04]`.
  Failing assertion: `publishes all five applied scripts below the reliability reserve` →
  `expected [ { …(5) }, … ] to have a length of 5 but got 4`. (The size loop itself stayed green —
  four scripts, four passing budget checks.) Shared evidence for the same claim in the other four
  files, which are structurally identical.
  All reverted; `cmp -s` against backups CLEAN for all five `contracts.ts`.

**Observed measurements from the green run** (all within the 15,872-byte reserve):
mint-declared-asset-limit max 2,214 B; execution-native-script-invalid
`[15575,15805,5317,14404,10435,10380,3068,14605,10389,9532,12048,11513,14181]`;
execution-source-script-decoding `[15032,15730,6868,12194,2990]`;
missing-script-source `[15117,10238,11112,2322,5522,2672]`.

---

## Nothing skipped

All nine files were edited and executed. No pre-existing red was encountered in any of them; none of
the known reds (stale blueprint digest pins, watcher settlement `malformed_state`,
direct-frontier-exact refusal, `fault-proof.test.ts` stale allowlist) touch this batch.

## src edits

**None retained.** Eleven production files and one test-support file were temporarily mutated for
failure evidence and restored byte-for-byte:
`src/spend-input-witness.ts`, `src/spend-input-signer-missing/authenticated-workflow.ts`,
`src/workflow/deployment-manifest-binding.ts`, `src/workflow/adapters.ts`,
`src/script-integrity-hash-mismatch/family.ts`, `src/script-integrity-hash-mismatch/manifest-workflow.ts`,
`src/remove-fraudulent-block.ts`, the five `src/<family>/contracts.ts`, and
`tests/support/emulator/measurement.ts`.

## Escalations

None raised. The one judgement call not settled by the brief — deleting the phase-A item ledger writer
rather than relocating it to a `--check` bench script — is flagged in section 2 above with the rule-15
record (no consumer, no checked-in ledger, replacement protection in place) rather than escalated,
since it removes a non-asserting side effect and preserves the protected contract. If the owner wants
the fragment produced, the mechanism is `tests/support/measured-fit-ledger.ts`.
