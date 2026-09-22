# Batch 06 report — demo/midgard-node (9 files)

Repo: /home/gumbo/midgard-hub/midgard, branch colll78/canonical-v1-watcher-l1-source-checkpoint.
Worked on top of the existing uncommitted working tree. No commit, no stash, no reset.
No `.hs` file touched. **No intentional production `src/` edit** — every src mutation was
for failure evidence and was reverted byte-for-byte (verified with `git diff` on the path).

Note on the brief: several rows cite line numbers/tests that no longer exist at working-tree
state (the tree already carries uncommitted deletions of the source-text-oracle tests the
brief was generated against). Where a row's contract had been deleted rather than fixed, I
restored the contract _behaviorally_ instead of re-adding a source-grep test.

---

## 1. tests/database-pool-timeout.test.ts

Contract: every database pool role gets a connect timeout, and the value is a reviewed
operational band rather than a self-pin.

Changes: replaced the circular `toBe(10_000)` self-pin with

- a static exhaustiveness gate over the role union
  (`Exclude<DatabasePoolRole, (typeof DATABASE_POOL_ROLES)[number]>` collapsed to `never`),
  so a new pool role fails to compile until it is covered;
- a role-invariance assertion (all roles share one timeout);
- a justified band `MIN_REASONABLE_CONNECT_TIMEOUT_MS = 3_000` /
  `MAX_REASONABLE_CONNECT_TIMEOUT_MS = 30_000`.

Failure evidence: in `src/services/database.ts`, dropped the connect timeout from one role's
pool options. Failing assertion: the role-invariance / band assertion for that role
(`expected undefined to be greater than or equal to 3000`). Reverted; `git diff` empty; green.

Result: 5 tests green.

## 2. tests/protocol-info.test.ts

Contract: `/protocol-info` advertises the whole operator-visible payload.

Changes: the single-field spot checks became one `toStrictEqual` over the whole advertised
object (apiVersion, midgardNativeTxVersion, deploymentMarker, network, decimal-string slot and
fees, `maxSubmitTxCborBytes`, `validation.localValidationIsAuthoritative: false`, both
script-language lists with canonical tags 2 / 0x80), plus `expect(consensusProfile).toBe(
MIDGARD_CONSENSUS_PROFILE)`. The configured submit cap is an independent constant
(`CONFIGURED_SUBMIT_CAP_BYTES = 123_456`), not the compiled bound, so the advertised value
cannot be right by construction. Added a bigint-slot case and an accept-at-bound /
reject-one-above pair against the compiled V1 maximum.

Failure evidence: in `src/commands/protocol-info.ts`, changed
`localValidationIsAuthoritative` to `true`. Failing assertion: the `toStrictEqual` payload
comparison, diffing exactly that field. Reverted; green.

Result: 9 tests green.

## 3. tests/fraud-proof-catalogue.test.ts

Contract: the published fraud-proof catalogue's MPF root and per-category membership proofs
are internally consistent, and the wire category IDs are stable.

Changes: the oracle is now the independent verifier
`verifyDeploymentManifestFraudProofCatalogueIdentity` from
`@al-ft/midgard-core/deployment-manifest-identity` — it rebuilds the root from
(categoryId, scriptHash) with its own implementation and folds every `membershipProofCbor`
back to it. Nothing re-derives an expected value with the builder under test. Four tests:
registration/order against the declared validator record + tail-ID pins
(`00000005` zeroInput, `00000006` validationTraceDispute); accept; reject on a tampered
scriptHash (`/fraud-proof catalogue root mismatch/u`); reject on a swapped-in proof from
another category (`/zeroInput\.membershipProofCbor does not prove membership/u`).

Failure evidence: in `src/transactions/initialization.ts`, swapped two entries in the
catalogue ordering. Failing assertion: the wire-order comparison against
`FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` and the `00000005`/`zeroInput` tail pin.
Reverted; the file's remaining diff is pre-existing working-tree change, unrelated. Green.

Result: 4 tests green.

## 4. tests/atomic-write-order.test.ts

Contract: durable atomic write ordering — data synced before rename, parent directory synced
after rename, temp file never renamed after a failed write, temp file cleaned up.

Changes: the transcript-equality oracle (which pinned the exact syscall sequence, i.e. the
implementation strategy) was replaced by `relativeOrder(call)` positional comparisons that
assert only the two ordering relations the contract states. A header comment records that the
content/permission/replacement/cleanup claims are asserted against a real filesystem in
`tests/atomic-write.test.ts`, and that no crash-injection harness exists in this suite, so the
syscall boundary is the observation point.

Failure evidence: in `src/files/atomic-write.ts`, moved the file `sync()` to after `rename`.
Failing assertion: `expect(relativeOrder("sync:file")).toBeLessThan(relativeOrder("rename"))`.
Reverted; `git diff` empty; green.

Known gap (reported, not fixed): nothing in this package injects a crash between rename and
directory sync, so genuine post-crash durability is not proven — only the ordering the
implementation issues.

Result: 3 tests green.

## 5. tests/pipeline-status-route.test.ts

Contract: `GET /pipeline-status` reports counts by journal status and the oldest _active_
pending-block-finalization, with age derived from the stored row.

Changes: the test previously asserted the route's own tuple against itself. Now:

- "active" is derived independently, by excluding the two terminal statuses from the status
  enum (`EXPECTED_ACTIVE_STATUSES`), and the route's exported tuple is compared to that;
- the route's _real_ SQL runs against the per-worker Postgres shard. A `journalRow(...)`
  fixture satisfies every NOT NULL column and CHECK constraint (format_version 1,
  replay_kind `ledger_delta_v1`, consensus_profile_id `midgard-consensus-v1`,
  `expected_validation_traces_root` = the empty-tree root at count 0, ledger delta jsonb);
- the per-status cases are driven by `EXPECTED_ACTIVE_STATUSES`, not by the route's tuple, so
  removing a status from the route fails as "a journal the route stops reporting" rather than
  silently generating fewer cases;
- `runAgainstShard` clears the table with `DELETE` (not `TRUNCATE`: seven child tables carry
  FKs) and provides the shard SqlClient.

Failure evidence: in `src/commands/listen-router.ts`, removed
`observed_waiting_stability` from `PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES`.
Failing assertions: the tuple-vs-derived-set equality, and
`reports an active observed_waiting_stability journal rather than nothing` with
`expected null to match object { headerHash: '1111…', status: 'observed_waiting_stability' }`.
Reverted; the file's remaining diff is pre-existing, unrelated. Green.

Flake hardening: one 9-file run showed `ageMs` = 3445 against a 5 s lower bound (a ~1.5 s
backwards wall-clock step inside the worker; the row's `createdAt` matched exactly, so the
row and column were right). The age anchor is now 10 minutes with a two-sided
`[600_000, 660_000)` band — still catches a wrong column, wrong row, or wrong sign, but
cannot be moved by a scheduling or clock hiccup. Re-run clean repeatedly.

Also changed: `demo/midgard-node/vitest.config.ts` — added
`*   tests/pipeline-status-route.test.ts` to the DB-touching inventory comment (comment only;
this file is also edited by batch 05, so expect a trivial merge there).

Result: 9 tests green.

## 6. tests/block-commitment-provider-evidence-preflight.test.ts

Contract: the block-commitment action must not reach the state-queue mutation lease when
there is no commit work, or while a local finalization awaits a confirmed recovery block.

Changes: dropped the exact-call-count assertions (which pinned how many times a collaborator
was consulted — implementation strategy) in favour of the gate itself. Added a hoisted
`mempoolState` count and `runAction(prepareGlobals)`, plus two new behavioral tests asserting
that `tryWithLeaseMock` and `fetchStateQueueSnapshotProgramMock` are **never** called:

- "never reaches the mutation lease while local finalization awaits a confirmed recovery block"
- "…when no commit work exists at all".

Failure evidence: in `src/fibers/block-commitment.ts`, removed the
local-finalization-pending guard so the action proceeded to acquire the lease. Failing
assertion: `expect(tryWithLeaseMock).not.toHaveBeenCalled()` in the first new test
(`expected "tryWithLease" to not be called at all, but was called 1 times`). Reverted;
`git diff` empty; green.

Result: 6 tests green.

## 7. tests/commit-block-header-worker-output.test.ts

Contract: for each commit-worker output type, the scratch MPF roots are kept or rolled back
according to that output's meaning.

Changes: replaced the per-type ad-hoc cases with an exhaustive
`Record<WorkerOutput["type"], { output, preserve }>` table — a new output variant fails to
compile until it declares its root-transaction meaning — driven through a single
`runRootTransaction(output)` helper that returns the _observed_ values
(`{ result, ledgerKept, transactionsKept }`) so vitest prints expected vs observed instead of
"→ undefined" from an assertion buried inside an Effect.

Failure evidence: in `src/workers/commit-block-header.ts`, made the failure output keep the
scratch roots. Failing assertion: the table row for `FailureOutput`
(`expected { …, ledgerKept: true, transactionsKept: true } to strictly equal
{ …, ledgerKept: false, transactionsKept: false }`). Reverted; `git diff` empty; green.

Result: 17 tests green.

## 8. tests/commit-worker-failure-lease-classification.test.ts

Contract (existing, kept unchanged — 7 tests): a typed commit-worker failure fails closed for
the mutation lease whenever durable journal evidence exists, when the evidence lookup itself
fails, and when there is no lease token; non-failure output is not classified.

Contract restored (new): **the durable journal row is written before the commit is signed and
submitted, and a failed journal write means nothing is ever signed or submitted.** The brief's
row proposed either a structural token or observing the journal row against Postgres; the
prior test asserted this by grepping the source, and that test had already been deleted from
the working tree. I implemented the behavioral observation: the _real_
`submitDepositOnlyCommit` and `submitTxBackedCommit` programs from
`src/workers/commit-block-header/submission.ts` run end to end, with only the collaborators
that would need L1, a live wallet, or a full ledger replaced. A hoisted `submissionTrace`
records `"prepare-journal"` (from `PendingBlockFinalizationsDB.preparePendingSubmission`) and
`"sign-and-submit"` (from the built tx's `signAndSubmitProgram`). Four new tests, both commit
paths × both polarities:

- `expect(submissionTrace.calls).toEqual(["prepare-journal", "sign-and-submit"])` plus
  `markSubmitted` called once;
- with `failJournalPreparation`, `expect(submissionTrace.calls).toEqual(["prepare-journal"])`
  and the program ends `Left`.

To reach the real ordering the harness needed real inputs, not stubs:

- `resolveDepositsRoot` returns `Option.some(...)` with a non-empty
  `includedDepositEventIds`, so the deposit-only path passes the "Nothing to commit" early
  return in `submission.ts` instead of short-circuiting;
- `processedMempoolTxs` carries a genuine canonical V1 transaction taken from
  `tests/fixtures/transaction-root-v1.generated.json`, so the production pre-submit DA sizing
  really decodes transaction bytes (a `Buffer.from("tx")` stub died in
  `encodeTransactionRootValue` with `MidgardTxCodecError E_CBOR_DECODE`);
- `NEW_HEADER` is a structurally complete V1 header, so
  `SDK.daPayloadEncodedSizeFromUtxoAggregate` sizes real field values rather than dying on an
  empty stub.

Failure evidence: in `src/workers/commit-block-header/submission.ts`, inserted
`yield* signAndSubmitProgram;` immediately before **both**
`PendingBlockFinalizationsDB.preparePendingSubmission(` call sites (lines ~956 and ~1486), so
submission happens before the journal write. All four new assertions failed:

```
× deposit-only commit writes the durable journal row before signing and submitting
  → expected [ 'sign-and-submit', …(2) ] to deeply equal [ Array(2) ]
× tx-backed  commit writes the durable journal row before signing and submitting
  → expected [ 'sign-and-submit', …(2) ] to deeply equal [ Array(2) ]
× deposit-only commit never signs or submits when the journal row cannot be written
  → expected [ Array(2) ] to deeply equal [ 'prepare-journal' ]
× tx-backed  commit never signs or submits when the journal row cannot be written
  → expected [ Array(2) ] to deeply equal [ 'prepare-journal' ]
```

Reverted from a byte-for-byte backup; `git diff --stat` on the path empty; re-ran green.

Result: 11 tests green (7 pre-existing + 4 new).

## 9. tests/emulator-submit-slot-snapshot.test.ts

Row recommendation was `keep`. Untouched; re-run as part of the batch (3 tests green).

---

## Verification (run from demo/midgard-node)

```
npx tsc --noEmit                                   # clean
npx prettier --check <9 batch files>               # All matched files use Prettier code style!
npx eslint <9 batch files> --max-warnings=0        # clean
npx vitest run <9 batch files>                     # Test Files 9 passed (9) | Tests 67 passed (67)
```

`pnpm typecheck` was deliberately not used (known tsup --clean sibling-dist race).
Never more than 2 vitest processes at once; no whole-workspace command was run.

## Files changed

- demo/midgard-node/tests/database-pool-timeout.test.ts
- demo/midgard-node/tests/protocol-info.test.ts
- demo/midgard-node/tests/fraud-proof-catalogue.test.ts
- demo/midgard-node/tests/atomic-write-order.test.ts
- demo/midgard-node/tests/pipeline-status-route.test.ts
- demo/midgard-node/tests/block-commitment-provider-evidence-preflight.test.ts
- demo/midgard-node/tests/commit-block-header-worker-output.test.ts
- demo/midgard-node/tests/commit-worker-failure-lease-classification.test.ts
- demo/midgard-node/vitest.config.ts (inventory comment line only)

`tests/emulator-submit-slot-snapshot.test.ts` unchanged.

## Escalations / rulings

None requested, none received.

## Pre-existing reds

None of the four known pre-existing reds (stale blueprint digest pins, watcher settlement
malformed_state, direct-frontier-exact refusal, fault-proof.test.ts stale allowlist) fall in
this batch; no new red observed. The scratch Postgres on 127.0.0.1:5433 was reachable
throughout; no global-setup bypass was needed.
