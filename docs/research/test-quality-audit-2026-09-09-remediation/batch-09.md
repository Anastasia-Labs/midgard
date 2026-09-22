# Batch 09 report — 9 files (demo/midgard-watcher, demo/da-committee-node)

All 9 files done. No file skipped. Two production edits, both called out below
(`demo/eslint.config.mjs`, and nothing else). Every failure-evidence mutation was
applied to a file first copied byte-exactly into
`/tmp/.../scratchpad/backups/`, restored from that copy, and verified with `cmp`
(each printed RESTORED). `git show HEAD:` / `git checkout` / `git restore` were
never used.

## PRODUCTION EDITS

1. `demo/eslint.config.mjs` — added a block for
   `midgard-watcher/src/**/*.ts` with `no-restricted-imports` banning
   `midgard-node`, `midgard-node-tools`, `da-committee-node` (and their
   `**/…/**` forms). This replaces the source-text grep deleted from
   `tests/runtime/scaffold.test.ts` (testing-best-practices §7 forbids source
   greps as runtime proof; the file already carried precedent rules citing the
   same doc). Verified: watcher `src` lints clean, and a probe file importing
   `"midgard-node"` is rejected by the rule.
2. No `src/` behaviour was changed anywhere. All other src touches were
   temporary mutations, each restored and `cmp`-verified.

## demo/midgard-watcher/tests/verification/missing-signature-detector.test.ts

- Contract: classify a replay divergence into MissingWitness /
  UnknownVkeyPreimage / PresentButInvalid / NotAFault, journal every
  classification in order, honour the default-on kill switch.
- Changes: rewritten, 23 tests. Replaced the production-derived hash oracle
  with CPython-checked blake2b-224 vectors (`VKEY_A = "11".repeat(32)` →
  `HASH_A = 8cf0020fd6584f7b130db5ca0229c51f934821a2eb07c1df512d8aca`).
  Replaced the `estimatedThreadTxCount: 5` transcription with a reference
  model computed from the exported constant:
  `referenceThreadTxCount(n) = 4 + max(1, ceil(n / MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE))`,
  with cases either side of the 32-witness batch boundary so the divisor is
  under test. Replaced `journaled` vs `detections.map(...)` (output vs output)
  with an independently written expected journal. Added the committedL2Vkeys
  precedence and operatorSuppliedVkey last-resort cases the old test name
  claimed but never exercised, plus full-record assertions (headerHash, txId,
  committedWitnessSetHash pass-through, `resolvedVkey: null` cases).
- Failure evidence: mutated `src/verification/missing-signature-detector.ts`
  line 160 (batch divisor / base term of the estimate) — the boundary cases
  failed on `estimatedThreadTxCount`; vkey-source precedence mutation flipped
  the resolution order and failed the precedence cases. Restored + cmp OK.

## demo/midgard-watcher/tests/fault-proofs/fault-proof-application.test.ts

- Contract: install a production workflow runner for every catalogue category,
  preflight each category's reference roster read-only at startup, fail closed
  on unknown infrastructure fields, duplicate history providers, missing
  secrets, uninstalled categories and a foreign deployment fingerprint.
- Changes: rewritten, 1478 → ~460 lines. The ~830-line nested-ternary
  transcription of every category's reference-script key list is gone. In its
  place the roster is checked by derived invariants: the fixture resolver is
  inverted (`contractNameForOutRef`, which throws on a malformed outRef) and
  the roster's outRefs are mapped back to contract names and compared, sorted,
  against the names production actually resolved; per-category rules that carry
  a real failure mode replaced the table (universal keys
  `computationThreadMint`/`fraudProofMint`; `phasMembershipWithdraw` present
  iff the category is not in the documented exception set; step keys ordinal-
  contiguous via `/^step(\d{2})(?:[A-Z]\w*)?$/u`; transitionTrace compared
  against its own exported constant, as the pre-existing L400-404 branch did).
- Failure evidence: dropping a required reference key from a workflow's roster
  in `src/fault-proofs/` made the derived roster/name comparison fail for that
  category; restored + cmp OK.

## demo/midgard-watcher/tests/runtime/fault-proof-launch-scope.test.ts

- Contract: `assertWatcherFaultProofLaunchScope` admits only the exact
  canonical fault-proof catalogue in canonical order.
- Changes: rewritten, 8 tests. The positive case now feeds the watcher's real
  `WATCHER_INSTALLED_WORKFLOW_CATEGORIES` (the value `watcher-runtime.ts` L552
  actually passes) rather than echoing the SDK constant the production function
  compares against, so the installed map is judged against the independent SDK
  catalogue. Seven named rejection cases (truncated, extended, reordered,
  duplicated, unknown member, empty, single-swap).
- Failure evidence: removing one category from
  `WATCHER_INSTALLED_WORKFLOW_CATEGORIES` failed the positive case; restored +
  cmp OK.

## demo/midgard-watcher/tests/funding/prover-funding-permit-mint.test.ts

- Contract: prover funding permit minting refuses an unbranded authority
  factory and a runner not installed by its canonical fixed-category workflow
  factory.
- Changes: added the missing admitted mint path (an always-rejecting mint no
  longer passes the file): a measured funding profile is built and signed with
  CML, admitted through the real
  `assertWatcherProverFundingAuthorityFactory` / `isAdmittedWorkflowRunner`
  registries, and the mint progresses. Both refusals gained
  absence-of-side-effect assertions (no permit minted, no reservation written).
- Failure evidence: relaxing the authority-brand check admitted the unbranded
  factory and failed the refusal's no-side-effect assertion; restored + cmp OK.

## demo/midgard-watcher/tests/runtime/scaffold.test.ts

- Contract: CLI argument parsing and command scaffolding; replay closes after
  durable catch-up; start refuses readiness when proof supervision is blocked
  or a deadline is at risk; an early clean runtime exit is a liveness failure;
  the authority command never constructs a watcher.
- Changes: deleted the package.json manifest pin (config transcription, no
  runtime behaviour) and the src-concatenation grep (§7). The import boundary
  it protected moved to the eslint rule above. Weak presence checks became
  parsed-JSON record assertions; added 7 causal readiness refusals and the full
  CLI parse matrix. 26 tests.
- Failure evidence: making `runWatcher` return cleanly on an at-risk deadline
  failed the corresponding readiness refusal; restored + cmp OK.

## demo/midgard-watcher/tests/l1/native-stream-initial-acknowledgement.test.ts

- Contract: the supervisor forwards the helper's initial intersection
  acknowledgement exactly once, ahead of ordinary forward frames, without its
  ordering guard terminating the stream.
- Changes: the tip is now specified by the test (`fixture.setNativeTip(...)`
  from the fixture's own block registry) instead of being read back out of
  production's `watcherNativeChainSyncAuthorityDetails`, and `schemaVersion`
  is asserted against the exported
  `WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION` instead of being copied from
  `events[1]`. events[0] and events[1] are now full `toEqual` frames (kind,
  point, schemaVersion, tip; blockType, prevHash from `parentPoint`, slot,
  blockNo, rawBlockCbor). Added the liveness half the row asked for: a further
  block is appended and must arrive as a third forward frame, with the kind
  sequence pinned to `[roll_backward, roll_forward, roll_forward]`.
- Failure evidence (2 mutations, `src/l1/native-chain-sync.ts`):
  1. `if (event.kind === "roll_backward") continue;` before the onEvent
     dispatch → `tests/l1/native-stream-initial-acknowledgement.test.ts:51`
     `AssertionError: expected 1 to be 2` (the acknowledgement is dropped).
  2. `current = Object.freeze({ hash: current?.hash ?? event.blockHash, slot, blockNo })`
     (forward frames stop advancing the ordering guard's cursor) → line 82
     `AssertionError: expected 2 to be 3` (the stream terminates on the
     appended block instead of forwarding it).
     Both restored from `backups/native-chain-sync.ts.orig`, cmp OK, re-run green.

## demo/da-committee-node/tests/reference-scripts.test.ts

- Contract: DA attestation reference-script resolution authenticates role NFTs,
  script refs and reward-account registration from deployment info.
- Changes (8 tests, was 5):
  - The identity-mapping assertions (L19-28, L35-41) are gone. Role→contract is
    now stated once as `REFERENCE_SCRIPT_ROLE_CONTRACTS` (the published shape of
    `DaAttestationReferenceScripts`) and every one of the six roles is checked,
    with the resolved UTxO's `scriptRef` independently re-hashed by
    `validatorToScriptHash` and required to equal that contract's hash, an
    exhaustive key-set assertion, and six distinct resolved outRefs. (Script
    hashes are NOT distinct — DA attestation mint and spend share one script;
    the test says so.)
  - The provider now returns the UTxOs reversed, so order-independence is a
    property under test.
  - `daAttestationValidatorsFromDeployment` is no longer compared to the fields
    it copies: the validator set must be internally coherent under lucid's own
    derivations (`mintingPolicyToId`, `validatorToScriptHash`,
    `validatorToAddress("Preview", …)`), all 15 roles are enumerated (5
    authenticated + 5 availability-challenge yields + 5 state-queue yields, key
    sets asserted), the ten yield scripts must be ten distinct hashes, and a
    separate case inverts deployment scriptHash→contract key to prove each role
    is wired to the deployment entry that carries its name (mint and spend
    indexed separately because of the shared DA attestation script).
  - New refusal: a reference UTxO absent from the chain, asserting the exact
    `missing state queue minting reference script UTxO at <txHash>#<idx>`.
  - New refusal: role NFT present in quantity 2 (`!== 1n`, not merely absent).
  - The two existing refusals now assert exact messages with independently
    recomputed values (`validatorToRewardAddress("Preview", bond.script)`;
    expected/actual script hashes).
- Failure evidence (4 mutations, all restored + cmp OK):
  1. `src/l1/reference-scripts.ts`: swap `daAttestationMinting: resolved[3]` /
     `daAttestationSpending: resolved[2]` → `reference-scripts.test.ts:70`
     `AssertionError: expected { role: 'daAttestationMinting', …(2) } to deeply
equal { role: 'daAttestationMinting', …(2) }`.
  2. `src/l1/reference-scripts.ts`: `byOutRef.get(refLabel) ?? [...byOutRef.values()][0]`
     → line 91 `expected [Function] to throw error matching /missing state queue
minting referenc…/u but got 'state queue minting reference script …'`.
  3. `src/l1/deployment.ts`: `daParamsGovernor: authenticatedValidatorFromDeployment(deployment.daAttestation)`
     → line 224 `expected { mint: 'daAttestationMint', …(1) } to deeply equal
{ mint: 'daParamsGovernorMint', …(1) }`.
  4. `src/l1/deployment.ts`: `spendingScriptHash: contract.policyId` →
     line 151 `expected { role: 'stateQueue', …(5) } to deeply equal
{ role: 'stateQueue', …(5) }`.

## demo/da-committee-node/tests/coordinator-factory.test.ts

- Contract: the on-chain coordinator factory fails closed without an L1
  submitter key or a canonical chain reader, and does no reference-script work
  before wallet preflight succeeds.
- Changes (7 tests, was 4):
  - The brittle exact call transcript
    `["lucid","select","preflight:…","reference-scripts"]` is replaced by the
    two ordering relations it exists to protect (`lucid < select < preflight <
reference-scripts`, preflight called exactly once), so an added internal
    step no longer fails it while a reordering still does.
  - Added real argument assertions instead of pure interaction counting: the
    SAME lucid instance must reach select, preflight and reference-script
    resolution (a second client would carry no selected wallet), the provider
    URL / network / key source / deployment must be the configured ones, and
    the preflight options must equal the full record derived from config.
  - Added `Object.hasOwn(options, "autoFundKeySource") === false` when the
    operator configured none (the conditional spread has a failure mode now).
  - Added the causal negative for preflight being skipped only when disabled
    (assert `assertL1SubmitterWalletPreflight` never ran and construction still
    succeeded).
  - Added the real (unstubbed) production decision the file never exercised:
    the DA attestation policy-id cross-check, refused with its exact message,
    paired with the otherwise-identical accepting configuration.
  - Constructed results are asserted `toBeInstanceOf(OnChainLifecycleCoordinator)`
    instead of `toBeDefined()`.
- by_construction note: the row suggests requiring a typed preflight result as
  an argument to `fetchDaAttestationReferenceScripts`. I did NOT make that src
  change: preflight is optional (`config.l1SubmitterPreflight.enabled` may be
  false), so a required preflight-result parameter would be wrong for the
  legitimate disabled path, and the resolver has other callers. Reported rather
  than guessed.
- Failure evidence (4 mutations to `src/coordinator/factory.ts`, restored + cmp OK):
  1. Reference-script resolution moved above preflight →
     `coordinator-factory.test.ts:143` `AssertionError: expected 3 to be less
than 2`, and `:73` `expected true to be false`.
  2. Unconditional `autoFundKeySource` (conditional spread removed) → `:190`
     `expected true to be false`.
  3. `if (false && contractDaAttestationPolicyId !== config.daAttestationPolicyId)`
     → `:239` `promise resolved "OnChainLifecycleCoordinator{ …(5) }" instead
of rejecting`.
  4. `if (true)` for the preflight guard → `:220` `expected true to be false`.

## demo/da-committee-node/tests/public-retained-da-store.test.ts

- Contract: the public retained-DA Postgres store opens only under a
  SELECT-only role and can never issue DML against the retained-evidence
  tables.
- Changes:
  - DELETED the mock-verifies-mock test ("surfaces the read-only transaction
    rejection when DML is attempted"), whose fake client implemented the very
    25006 rejection it then asserted (rubric 8).
  - ADDED a real-PostgreSQL suite (5 tests) that crosses the boundary the row
    said was never crossed. It creates its own database and login role
    (`midgard_public_reader_<rand>`), the two exposed tables and a row, grants
    only SELECT, and opens `PostgresPublicRetainedDaStore` through its REAL
    default `pg.Pool` factory:
    - opens on the SELECT-only login and reads the record PostgreSQL actually
      stores (plus an absent-key `undefined`);
    - `GRANT DELETE ON watcher_da_payloads` → `open()` is refused, and after
      `REVOKE` it opens again (the grant/revoke is the cause) — this is the
      assertion that finally puts the probe's `has_table_privilege(... 'DELETE')`
      SQL under test; the revoke runs in a `finally` so a failure cannot leak
      state into later cases;
    - `GRANT pg_read_all_data` → refused, revoked → admitted;
    - a real `DELETE` inside `BEGIN READ ONLY` is rejected by the server with
      SQLSTATE `25006` even for the superuser, and the row count is unchanged;
    - the reader role's own `DELETE` attempt is rejected with `42501`.
  - Fails closed per rule 14: no skip path. Connection parameters come from
    `POSTGRES_HOST/PORT/USER/PASSWORD/DB` with `127.0.0.1:5432` defaults,
    matching the `postgres` service and env of the `midgard-node-ci` job that
    already runs `pnpm --dir demo/da-committee-node test`. Locally the scratch
    cluster is on 5433, so local runs need
    `POSTGRES_PORT=5433 POSTGRES_DB=postgres`.
  - The existing mock-based tests are kept (they still cover the store's own
    query text, BEGIN READ ONLY framing, release/end accounting and record
    validation), so nothing was lost.
- Failure evidence (3 mutations to `src/store/public-retained-da.ts`, restored +
  cmp OK):
  1. Removed the `OR has_table_privilege(current_user, 'watcher_da_payloads',
'DELETE')` clause from the probe SQL → `public-retained-da-store.test.ts:363`
     `AssertionError: promise resolved "PostgresPublicRetainedDaStore{ …(1) }"
instead of rejecting`. This is exactly the defect the old mock-only file
     could not see.
  2. Removed `access.broad_role_membership ||` from the refusal condition →
     the real pg_read_all_data case failed at `:363` (and the fake-based
     privileged-attributes case at `:178`).
  3. `BEGIN READ ONLY` → `BEGIN` → `:139` and `:223`
     `AssertionError: expected [] to have a length of 3 but got +0`.

## Verification

demo/midgard-watcher (package dir):

- `npx tsc --noEmit` → exit 0.
- `npx eslint <6 test files> ../eslint.config.mjs --max-warnings=0` → clean.
- `npx prettier --check <same>` → all files use Prettier code style.
- `npx vitest run <6 test files>` → **6 files passed, 65 tests passed**.

demo/da-committee-node (package dir):

- `npx tsc --noEmit` → exit 0.
- `npx eslint tests/public-retained-da-store.test.ts tests/coordinator-factory.test.ts tests/reference-scripts.test.ts --max-warnings=0` → clean.
- `npx prettier --check <same>` → clean.
- `POSTGRES_PORT=5433 POSTGRES_DB=postgres npx vitest run <same 3>` →
  **3 files passed, 27 tests passed**.

No reds, pre-existing or otherwise, were encountered in the files of this
batch. No whole-workspace command was run; never more than one vitest process
at a time.
