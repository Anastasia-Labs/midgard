# Batch 10 report

All 8 brief rows executed. No `.hs` files touched. No stash/reset/checkout/commit.
Every production file mutated for failure evidence was `cp`-backed-up to the
scratchpad BEFORE mutation and restored from that copy, verified with `cmp`
(never `git show HEAD:` / `git checkout` / `git restore`).

Backups live in
`<scratchpad>/backup/`.

## src / shared-file edits (call-outs)

1. **`demo/eslint.config.mjs`** — SHARED FILE, edited.
   Two blocks appended before the `midgard-node/**` block, replacing static
   source-text greps that a prior wave deleted from `lucid-midgard` tests, so the
   gates survive as lint rather than as fake tests:
   - `lucid-midgard/src/**/*.ts`: `no-restricted-syntax` banning
     `.orDie/.orDieWith/.catchAllDefect/.catchAllCause/.catchCause` member access
     and any `unsafeRun*` identifier.
   - `lucid-midgard/examples/**/*.ts`: `no-restricted-syntax` banning the
     provider identifiers `Blockfrost|Maestro|Kupmios|Koios|Emulator`.
     A `no-restricted-imports` rule on `@lucid-evolution/*` was tried and dropped:
     `examples/usage.ts` legitimately imports `CML`, which is not a provider.
     Verified: `npx eslint src examples --max-warnings=0` green in
     `demo/lucid-midgard`; `npx prettier --check eslint.config.mjs` green in `demo`.
2. **No other `src/` file was modified.** All production mutations were temporary
   failure-evidence mutations, restored byte-exact.

## Test-infra file added

`demo/lucid-midgard/tests/fixtures/native-tx-fixture-spec.ts`
— an independent (non-circular) reference model for native-tx conformance
fixtures. Shares only RFC-7693 blake2b with production; re-derives the nine §5
field commitments, the compact witness-set hash, the compact-body layout, the
domain-separated tx id (`"MidgardNativeTxBodyV1" ‖ cbor(version) ‖ compact_body`),
§5.3 out-ref list canonicity, ascending mint policy ids, and strictly ascending
redeemer pointers. No other batch owns this file.

---

## 1. demo/da-committee-node/tests/availability-responder-reference-scripts.test.ts

Contract: the readiness gate binds each of nine responder role names to exactly
one deployed, authenticated reference script.

Changes: `EXPECTED_ROLES` written out locally (not read from the production role
table). Accept test now asserts the exact key set and, per role, that
`{txHash, outputIndex, scriptRef, authToken}` equals that role's own deployment
contract — a permuted or collapsed mapping now fails. Existing negative tightened
from a bare pattern to `/Unregistered availability-challenge bond withdrawal/`.
Two new causal negatives:

- swap only the role-scoped auth NFTs of `utxos[7]`/`utxos[8]` →
  `/unauthenticated state-queue minting/`;
- `utxos[1].scriptRef = deployment.stateQueue.spend.script`, outref and NFT still
  correct → `/unauthenticated availability-challenge spending/`.

Failure evidence: three mutations of `src/availability/reference-scripts.ts`
(role-key derivation, auth-unit role scoping, script-bytes comparison); each
tripped the corresponding new assertion. Restored from
`backup/reference-scripts.ts`, `cmp` clean.

Verification (from `demo/da-committee-node`): `npx tsc --noEmit` (0),
`npx eslint tests/availability-responder-reference-scripts.test.ts --max-warnings=0` (0),
`npx prettier --check …` (green), `npx vitest run …` → 4 passed.

## 2. demo/lucid-midgard/tests/safe-program.test.ts

Contract: `completeSafe` / `completeProgram` / `chainSafe` / `chainProgram`
produce identical, fully-determined transactions and structured refusals.

Changes: three tx-id pins with a provenance comment explaining why the scenarios
are fully determined; exact `toEqual` metadata records (completeSafe: fee 0n,
inputCount 1, outputCount 1, txByteLength 208, feeIterations 0, balanced false,
estimatedSignedTxByteLength 312; completeProgram: outputCount 2, txByteLength 279,
feeIterations 1, balanced true, changeOutputIndex 1, walletInputSource
"provider"); exact error JSON
`{name:"BuilderInvariantError",code:"BUILDER_INVARIANT",message:"Cannot complete a transaction with no spend inputs",detail:null}`
on both APIs; `expect(counters).toEqual({protocolParameters:1,utxos:1})` for
laziness; exact chained derived/wallet outputs keyed by the chain tx id; and
cross-checks against the bytes themselves (`txHex.length/2 === 208`,
`tx.body.fee === 0n`, `tx.body.networkId === 0n`).

Note: `MidgardNativeTxFull` exposes `body` + `compact`, not `transactionBody`;
the byte-level cross-checks were written against `tx.body` accordingly.

Failure evidence: mutations of `src/balancing.ts` / `src/builder.ts`. M3 (forcing
a change output when change is zero) did NOT trip anything, because the
`completeSafe` accept case never takes the balancing path; it was replaced with
M4 (metadata `txByteLength` reporting a wrong length), which tripped the exact
metadata `toEqual`. Restored from `backup/balancing.ts`, `backup/builder.ts`,
`cmp` clean.

## 3. demo/lucid-midgard/tests/documentation-examples.test.ts

**Brief premise correction (recorded in the file):** the row's `why_low` states
the example results are deterministic. They are not — `makeExampleContext()`
calls `CML.PrivateKey.generate_ed25519()` per invocation, so ids differ every
run. Verified by running the examples twice. Exact id pinning is therefore
impossible and was not attempted.

Changes: `expect(composed[1]).toBe(composed[0])` (import round-trip must yield
the _same_ id — replaces `toHaveLength(2)`), four distinct ids, no all-zero id,
exact status sequence `["queued","accepted","rejected","E_EXAMPLE","committed"]`,
exact error codes.

Failure evidence: mutations of `src/index.ts` (id derivation) and
`examples/usage.ts` (status ordering) each tripped the new assertions; restored
from `backup/lm-index.ts` and `backup/usage.ts`, `cmp` clean.

## 4. demo/lucid-midgard/tests/native-high-cardinality-fixture.test.ts

Changes: kept the exact 13-entry redeemer-pointer list, dropped the weak
`toHaveLength` / `toHaveLength(64)` shape checks, and added
`expectNativeTxFixtureFacetsSatisfySpec` (with `sortedInputs: true`) against both
the rebuilt facets and the checked-in file — so the fixture is now checked
against the specification, not only against the codec that produced it.

## 5. demo/lucid-midgard/tests/native-size-balanced-fixture.test.ts

Changes: removed the `SIZE_BALANCED_COUNTS` / `SIZE_BALANCED_PARAMETERS` imports
(circular oracle) and declared them locally: spendInputs 48, referenceInputs 32,
outputs 48, mintPolicies 24, spendRedeemers 8, mintRedeemers 24, observerRedeemers
18, receiveRedeemers 18, totalRedeemers 68, requiredSigners 17, addrWitnesses 17,
scriptWitnesses 68, `DECLARED_SIZE_BAND {min:16_000,max:16_256}`,
`DECLARED_MAX_FEE 10_000_000n`, and the eight spend-redeemer pointers `0:40`…`0:47`.
Added the spec check to both the rebuilt and the checked-in fixture.

Two modelling corrections made while building the spec helper:

- a §5.3 out-ref item is 40 bytes (the `5826` head is 2 bytes over 38 content
  bytes), not 39;
- ascending input order is NOT a canonical-decoding requirement (the
  size-balanced fixture puts its 40 key-witnessed inputs before its 8
  script-witnessed ones and decodes fine), so ascending order is opt-in
  (`requireAscending` / `sortedInputs`) and asserted only for the
  LucidMidgard-built high-cardinality fixture.

Failure evidence for 4+5: mutations of `src/core-native.ts` and
`src/core-native-witness.ts` (field-commitment ordering; witness-set framing)
tripped the spec assertions in both fixture suites; restored from
`backup/core-native.ts`, `backup/core-native-witness.ts`, `cmp` clean.

## 6. demo/midgard-node-tools/tests/e2e-release-finality-policy.test.ts

Rewritten, 17 tests. Local `RELEASE_POLICY = {confirmationDepth: 30,
automaticRecoveryMaxDepth: 2160, deepRollbackPolicy:
"automated_rewind_replay_incident-v1"}` written out instead of imported, plus a
separate test pinning the production constant to it. Accept test uses `toEqual`,
asserts `Object.isFrozen(parsed)` and non-aliasing (mutating the input after
parsing cannot reach the parsed policy). The six bare `toThrow()` rejections were
replaced with a 14-row `it.each` table, each row violating exactly one
requirement and naming its refusal regex: boundaries 29/31 and 2159/2161/30,
the numeric string `"30"`, an aliased `deep_rollback_policy`, a missing field, an
extra `sourceMode`, `null`, an array, a JSON string, and a `-v2` policy id. Plus a
field-name-override test (`"release identity l1Finality.confirmationDepth must be
exactly 30"`).

Failure evidence: mutations of `src/commands/e2e-release-finality-policy.ts`
(depth comparison widened to `>=`; exact-keys check dropped) each tripped the
specific rows; restored from `backup/e2e-release-finality-policy.ts`, `cmp` clean.

## 7. demo/midgard-node-tools/tests/pipelined-commit-process-harness.test.ts

Per the row's `by_construction`, the cases now name the **harness** contract, not
the commit-pipeline contract. A file-level doc comment states explicitly that the
supervised children are stub scripts, that the node's lease/journal decisions are
NOT executed here (that needs the real `midgard-node` build in the acceptance
lane), and that what is real is the harness's own decisions.

13 tests, each naming one harness decision, with the stub scripts as controlled
inputs that drive it — including into its refusals:

- one-shot arm file: SIGKILL at the checkpoint marker, exactly one attempt, arm
  consumed (`access(armFile)` rejects with ENOENT);
- re-arming over a surviving arm file rejects with `code: "EEXIST"`;
- terminating at the _wrong_ checkpoint →
  `/Expected exactly one supervised checkpoint termination for … checkpoint=speculative_mid_build; observed 0/`;
- journal contention: the arm-file race winner is the SIGKILLed journal holder
  (marker equality on both sides, log attribution checked);
- survivor with no lease-busy line → "Journal-kill survivor did not record
  state-queue lease contention";
- survivor with no recovery line → "…did not execute unsubmitted-journal
  recovery after lease expiry";
- normal contention: one submitted winner + one T2-invalidated loser; and the
  database single-active-journal refusal accepted as the T7 loser's evidence;
- both processes submitting → `"Expected one submitted winner and one invalidated
loser; observed submitted=2,invalidated=0"`;
- a 4-row `it.each` over the pre-spawn spec guard (shared MPF store, differing
  Postgres identity, missing `timeoutMs`, `timeoutMs <= 2 × lease TTL`), each row
  keeping every other requirement satisfied.

Failure evidence — mutations of
`demo/midgard-node-tools/src/e2e/pipelined-commit-process-harness.ts`
(backed up to `backup/pipelined-commit-process-harness.ts`, restored with `cmp`
after each round):

- M1 `assertCheckpointTermination`: `checkpointAttempts.length !== 1` → `> 1`
  → 2 failed: "refuses to re-arm…" and "fails closed when the supervised process
  terminates at the wrong checkpoint"
  (`expected … to throw error matching /Expected exactly one supervised check…/
but got 'Expected checkpoint process to exit v…'`).
- M2 normal contention: `submitted.length !== 1 || invalidated.length !== 1` →
  `submitted.length < 1` → 1 failed: "refuses a normal-contention run in which
  nobody lost".
- M3 journal contention: lease-busy guard disabled → 1 failed: "refuses a
  journal-contention result whose survivor never recorded lease contention"
  (`promise resolved "{ winnerNodeId: 'node-a', …(5) }" instead of rejecting`).
- M4 spec guard: `timeoutMs! <= ttl * 2` → `<= 0` → 1 failed: "rejects contention
  specs 'timing out before two lease TTLs have…'".

## 8. demo/midgard-node-tools/devnet/phase4-process/tests/assets.test.mjs

A prior wave had already deleted ~14 of the source-text-grep tests (870 → 256
lines). This batch finished the row.

Removed: the residual source greps at L116-121 (`parse_kupo_checkpoint` /
`mostRecentCheckpoint` / `kupo-health.json` matched against script text) and the
now-unused `read()` helper.

Added, all executing the real scripts (10 tests total in the file now):

- **capture-snapshot, positive**: builds a throwaway checkout with the real
  `common.sh` + `capture-snapshot.sh`, a real unix socket for
  `grant_cardano_socket_access`, and stub `docker`/`curl` on PATH; the run is
  stopped at the PHAS preflight by a stub exiting 91, so "accepted the payload
  and moved on" is a distinct observable outcome. Asserts status 91 and that
  `snapshots/matched-v1/kupo-health.json` holds the served payload verbatim.
- **capture-snapshot, negative**: a labeled `kupo_most_recent_checkpoint{network="devnet"}`
  payload → non-zero, not 91, stderr matches
  `/exactly one unlabeled finite nonnegative integer/`, and no
  `snapshot-identity.json` is written.
- **reset fail-closed guards, executed** (these precede the first container, so
  no Docker is needed): missing `SHA256SUMS` → "matched snapshot is incomplete";
  missing identity → "matched snapshot identity is missing"; intact files whose
  digest-of-the-digest-list disagrees → "snapshot-set checksum mismatch";
  `MIDGARD_PHASE4_SNAPSHOT_DIR` redirect → "snapshot override is not authorized
  for the run-scoped matched snapshot"; missing scenario label →
  "MIDGARD_PHASE4_SCENARIO_LABEL is required".

Failure evidence — mutations of the phase-4 shell assets (backups
`backup/phase4-common.sh`, `backup/phase4-reset.sh`,
`backup/phase4-capture-snapshot.sh`; each restored with `cmp` clean):

- N1 `common.sh` `parse_kupo_checkpoint`: strict sample regex disabled
  (`if (0)`) → 2 failed: the existing parser suite and "snapshot capture refuses
  a Kupo payload the strict parser rejects".
- N2 `capture-snapshot.sh`: pipe curl straight into `parse_kupo_checkpoint`
  instead of persisting `kupo-health.json` → 1 failed: "snapshot capture reads
  the Kupo checkpoint through the strict parser".
- N3 `reset.sh`: snapshot-override guard replaced by `true` → 1 failed: "reset
  refuses a caller-redirected snapshot directory".
- N4 `reset.sh`: snapshot-set checksum comparison replaced by `true` → 1 failed:
  the "snapshot-set checksum mismatch" row.

CI wiring: already wired, nothing to add. `demo/midgard-node-tools/package.json`
`test:phase4:devnet-assets` runs `node --test devnet/phase4-process/tests/assets.test.mjs …`
and is chained from `"test"`, which `.github/workflows/midgard-node-ci.yml:347`
invokes via `pnpm --dir demo/midgard-node-tools test`.

Skipped deliberately: an executing check of `reset.sh`'s own
`parse_kupo_checkpoint` call sites (reset.sh:162, :228). Reaching them requires a
fully valid restored matched snapshot plus Docker restores; the grep that
approximated it was deleted rather than kept, and the parser contract itself plus
capture-snapshot's routing are now covered by execution. Also skipped: a
"checkpoint never converges" capture case — the loop is 120 × 1s.

---

## Verification commands run green (per package, from the package dir)

`demo/da-committee-node`

- `npx tsc --noEmit` → 0 (batch 09 was editing other files in this package concurrently; no errors surfaced)
- `npx eslint tests/availability-responder-reference-scripts.test.ts --max-warnings=0` → 0
- `npx prettier --check tests/availability-responder-reference-scripts.test.ts` → green
- `npx vitest run tests/availability-responder-reference-scripts.test.ts` → 4 passed

`demo/lucid-midgard`

- `npx tsc --noEmit` → 0
- `npx eslint tests/safe-program.test.ts tests/documentation-examples.test.ts tests/native-high-cardinality-fixture.test.ts tests/native-size-balanced-fixture.test.ts tests/fixtures/native-tx-fixture-spec.ts --max-warnings=0` → 0
- `npx eslint src examples --max-warnings=0` → 0 (validates the new shared eslint.config.mjs blocks)
- `npx prettier --check` on the same five files → green
- `npx vitest run tests/safe-program.test.ts tests/documentation-examples.test.ts tests/native-high-cardinality-fixture.test.ts tests/native-size-balanced-fixture.test.ts` → 4 files / 6 tests passed

`demo/midgard-node-tools`

- `npx tsc --noEmit` → 0
- `npx eslint tests/pipelined-commit-process-harness.test.ts tests/e2e-release-finality-policy.test.ts devnet/phase4-process/tests/assets.test.mjs --max-warnings=0` → 0
- `npx prettier --check` on the same three files → green
- `npx vitest run tests/pipelined-commit-process-harness.test.ts tests/e2e-release-finality-policy.test.ts` → 2 files / 30 tests passed
- `node --test devnet/phase4-process/tests/assets.test.mjs` → 10 passed / 0 failed

`demo`

- `npx prettier --check eslint.config.mjs` → green

Never ran whole-workspace commands; never more than 2 vitest processes at once.

## Rulings received

None. No escalation was required.
