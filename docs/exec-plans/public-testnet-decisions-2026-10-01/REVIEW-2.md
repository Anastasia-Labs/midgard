# Independent review, pass 2

Date: 2026-10-01. Base: `3027be19cb3bc740f83af0e155e34ab38c7cf894`.
Input: the complete scoped working-tree diff and new files, final source and
callers, the review lenses, DA/fraud-proof invariants, and the finding traces in
`REVIEW-1.md`. No author rationale was an input. This reviewer made no
implementation edits and spawned no agents. Source-owner coordination was used
to avoid testing temporary mutations in this shared checkout.

Both first-pass findings are CLOSED after the additional collateral-capacity
fix. No confirmed remaining defect was found in this scope. This is independent
review evidence, not final acceptance of the combined working-tree change.

## Scope and invariants

- A7: all changed/new watcher funding files; availability runtime and parameter
  refresh; production funding startup and watcher-runtime integration;
  fault-proof funding permit admission, begin, refresh/recovery, signing helpers
  and their changed/new tests. The attack surface includes historical signed
  reconciliation before current limits, fresh collateral selection under current
  parameters while retaining the original immutable lease basis, and
  authenticated parameter/capacity history across restart.
- A4: rollback durable commit, observation, canonical progress and retention;
  trusted-head store and authenticated retention floor; new retention tests and
  the related durable-reference and snapshot verification callers.
- Read `references/lenses.md`, `invariants-da.md` and
  `invariants-fraud-proofs.md` from the reviewing-consensus-changes skill. No
  validator, script-application ABI, canonical codec twin or golden channel
  changes are in this scope. DA fees, exact outputs and immutable signed intent
  remain relevant through the existing SDK operation boundary.
- Archive retirement, public-DA transport/payload changes, node/full-stack work
  and decision prose are reviewed separately and are outside this report.

## First-pass verdicts

### F1 — CLOSED

Historical signed recovery no longer admits its leases against the current
collateral-input limit. Permit creation and `parseStateSnapshot` use the
authenticated recovery capacity; `readWorkflowFundingRecovery` does not demand
already-spent inputs. Confirmation applies the exact durable transition before
any current submission guard. New action begin and ready-to-submit still check
the current count, and the actual signed-body guard checks the current count,
collateral percentage, value and conservation.

Source: `demo/midgard-fault-proofs/src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.ts:307`,
`funding-reservation-permit.reconcile-workflow-funding-submission-handoff.ts:244`,
`funding-reservation-permit.read-workflow-funding-recovery.ts:232`,
`funding-reservation-permit.apply-transition.ts:48`,
`funding-reservation-permit.begin-workflow-funding-reservation-action.ts:59`,
`funding-reservation-permit.read-workflow-funding-recovery.ts:196`, and
`funding-reservation-permit.assert-runtime-transaction-bound.ts:253` in the same
directory. The production orchestrator observes/reconciles and confirms at
`orchestrator.run-admitted-fraud-proof-workflow.ts:551`; rebroadcast calls the
current submission guard at `:469`.

Regression: `tests/funding-parameter-update.test.ts` prepares an exact signed
two-collateral intent, reopens under cap one, recovers identical signed bytes,
refuses rebroadcast, confirms the durable transition and refuses a fresh build
without usable current funding. Independently observed: 7 collected and passed,
exit 0; the final expanded file independently passed 12/12, including
nonparameter authority-substitution controls for the capacity policy. This is a
permit/lifecycle regression, not an L1 inclusion experiment.

### F2 — CLOSED

The original stale-sizing path is removed: the factory computes a separate
current selection calculation, while the reservation ID, original policy and
original basis remain fixed. Idle refresh passes that calculation to the
planner; the planner reads current collateral value requirements and the current
input cap. A separate authenticated maximum-capacity snapshot allows later
recovery of leases selected under a past higher cap.

Source: `demo/midgard-watcher/src/funding/prover-funding-authority.create-watcher-prover-funding-authority-factory.ts:283`,
`:305`, `:316`; `prover-funding-authority.create-watcher-prover-funding-authority.ts:304`;
`prover-funding-reservation.plan-watcher-prover-funding-reservation.ts:146`;
`prover-funding-parameter-history.ts:52` and `:97`; and the fault-proof permit's
recovery capacity at `funding-reservation-permit.create-workflow-funding-reservation-permit.ts:307`.

Regression: `tests/funding/prover-funding-calculation-reuse.test.ts:314` checks
the current 3-billion-lovelace slash-collateral bound and proves that idle
selection chooses the adequate larger coin while reservation ID, original
policy, original basis and signed journal prefix stay fixed. Independently
observed: all 8 tests in that file passed. The additional
`tests/funding/parameter-capacity-recovery.test.ts:25` reproduces `1 -> 3 -> 1`,
including return to the exact original parameter digest, two freshly selected
collateral inputs, a real CML signed intermediate intent, restart, refusal when
authenticated capacity is missing, identical signed recovery, workflow journal
reconciliation/confirmation, and fresh one-input rotation. Independently
observed after the fixture corrections: 1 collected and passed, exit 0. Expanded
history checks also passed (1 collected), including capacity tamper, wrong key,
identity substitution, original/capacity row swap, historical-authority write
refusal and monotonic cap retention. These tests exercise production funding
and durable lifecycle boundaries; they do not prove L1 inclusion of a complete
installed proof/slash transaction.

## Additional paths found during this pass

The initial F2 fix moved the shortage to an increased-capacity path. An original
cap-one reservation followed by a cap-three/current-collateral increase, with
two individually insufficient but jointly sufficient pure-ADA coins, was still
restricted by `Math.min(originalCap, currentCap)`. The selector deterministically
threw even though the wallet could meet the current requirement. Changing only
that minimum would have caused recovery to reject the new two-coin snapshot
against the original cap.

The follow-up fix adds one authenticated maximum-capacity row per reservation
and uses its admitted policy only for recovery snapshot parsing. Original policy
and basis identity remain exact; current selection and transaction admission use
current limits. The HMAC payload includes its original/capacity role and exact
reservation, deployment, policy and basis. Capacity updates are admitted only
from local runtime authority and SQL-updated only to a strictly larger cap.

A further `1 -> 3 -> 1` composition needed to refresh idle leases even when
current parameters return exactly to the immutable original snapshot: digest
inequality alone could not trigger that refresh. The final begin condition at
`funding-reservation-permit.begin-workflow-funding-reservation-action.ts:99`
also refreshes when the active collateral count exceeds the current cap. Its
refresh still requires authenticated resolution of every signed journal attempt
before idle leases can be released. The final cap-cycle regression passed with
that condition. Both displaced paths are closed in the reviewed final source.

These are collateral recovery/continuation liveness paths (major), not an
established false slash or value-redirection attack. They are part of the F2
fix-batch review, not a new fault-proof family coverage gap.

## Lens coverage

1. Parameter trust — Clean at the validator boundary. Runtime refresh retains
   admitted loopback authority. Historical parameter and capacity snapshots are
   authenticated evidence for recovery; they cannot perform a fresh live query.
   Stable identity checks preserve deployment, wallet, runner/category,
   economics, reference scripts and prior contract custody roles.
2. Always-succeeds scripts — Clean: no validator arm, script application arity,
   bare blueprint deployment or yield handshake changed.
3. Decoders/pinned compiler — Clean for the scoped new persistence: canonical
   history and floors are MAC-checked, identities are exact and protocol
   snapshots pass the shared parser. No new Aiken decoder was introduced. No
   Aiken build/check was run by this reviewer.
4. Value conservation — F2 and its capacity variants concern sufficient live
   collateral. Actual signed bodies still enforce pure-ADA wallet collateral,
   exact declared/returned value, current percentage/cap and release floor.
   Durable signed bytes and actual output derivation are unchanged.
5. Anchoring — Clean: historical basis re-admission reconstructs the exact
   original policy/basis. Capacity history authenticates its role and immutable
   reservation identity. The floor authenticates a prior record and its hash;
   the first retained suffix record must hash-link to that record.
6. Reference scripts — Clean: reference rosters and the runtime prohibition on
   inline executable witnesses remain in force. Availability retry wraps only
   unsigned construction; the SDK signs afterwards.
7. Both polarities — Clean: F1 has historical recovery plus current refusal
   controls. F2 now has the single-large-coin continuation and cap-growth/cycle
   counterpart, including missing-capacity refusal before signed recovery.
8. Gates that can fail — Observed runs collected nonzero counts. History
   wrong-key/tamper/identity checks and floor tamper/missing/replay checks assert
   observable refusals. Owner mutation logs inspected below turn the targeted
   historical admission, stale sizing and capacity-growth regressions red.
   This reviewer performed no source mutation.
9. Execution/size budgets — No new on-chain work or execution ledger changes.
   Ordinary observation retention is age/depth-based, preserves durable
   references and every observation of retained block hashes, and pins all
   evidence during unresolved work. No production-size or actual transaction
   budget experiment was run.
10. TypeScript/Aiken twins — Clean: no canonical codec or proof-computation twin
    changed within this scope.
11. Ledger facts — Clean: signed transition derivation reads actual body
    inputs/outputs. Historical inclusion/expiry is observed before rebroadcast
    limits in availability reconciliation. Refreshed unsigned availability
    selection must retain the action and checks revocation before retry/signing.
12. Replacement guards — Clean in final source: F1 and F2 are closed, including
    the cap-growth/cycle displacement found in this pass. Old custody,
    reference, identity, CAS and monotonic successor/hash checks survive. The
    new floor is fsynced before any summarized record is removed; snapshot
    compaction preserves all remaining durable references and drops incomplete
    consistency entries together.

## Independently observed verification

All commands ran from the repository root on 2026-10-01. Test commands used:

```sh
PATH=/tmp/midgard-toolchain-20261001:$PATH
MIDGARD_REAL_BLUEPRINT_PATH=/tmp/midgard-decisions-blueprint-20261001/onchain/aiken/plutus.json
MIDGARD_TEST_DATABASE_PREFIX=midgard_review2_20261001
```

Watcher runs additionally used `MALLOC_MMAP_THRESHOLD_=131072` and
`MIDGARD_WATCHER_FORKS=2`. Other review/implementation suites and the parent's
preflight were active in the shared checkout. No builds were run while the
parent preflight owned build outputs.

- `node scripts/doctor.mjs` with the requested PATH/database prefix: exit 1.
  Sandbox EPERM blocked Aiken/Postgres/process probes. Node 22.22.2, installed
  dependencies, fresh blueprint stamp and executable repository hooks were
  observed. This was not a compiler/Postgres verification pass.
- `pnpm --dir demo/midgard-fault-proofs exec vitest run tests/funding-parameter-update.test.ts --reporter=default`:
  7 collected and passed, exit 0.
- `pnpm --dir demo/midgard-watcher exec vitest run tests/funding/protocol-parameter-history.test.ts tests/funding/protocol-parameter-retry.test.ts tests/l1/rollback-retention.test.ts tests/runtime/trusted-head-authority-retention.test.ts --reporter=default`:
  11 collected; 8 passed and 3 failed, exit 1. All three trusted-head failures
  were EROFS at `mkdtemp('/var/tmp/midgard-trusted-head-*')`, before the code
  under review ran. History 1, retry 3 and rollback retention 4 passed.
- `pnpm --dir demo/midgard-watcher exec vitest run tests/runtime/trusted-head-authority-retention.test.ts --reporter=default`:
  rerun with escalation for the existing `/var/tmp` fixtures; 3 collected and
  passed, exit 0. Direct successor after compaction/restart, old-head refusal,
  tampered/missing/replayed floor refusal and simulated interrupted cleanup were
  observed.
- `pnpm --dir demo/midgard-watcher exec vitest run tests/funding/prover-funding.test.ts tests/availability/runtime.test.ts --reporter=default`:
  21 collected and passed (4 funding, 17 availability), exit 0.
- `pnpm --dir demo/midgard-watcher exec vitest run tests/funding/protocol-parameter-history.test.ts tests/funding/prover-funding-calculation-reuse.test.ts tests/funding/parameter-capacity-recovery.test.ts --reporter=default`:
  10 collected; history 1 and calculation/recovery 8 passed, while the new cycle
  test failed before its target behavior because its helper had not yet emitted
  the requested 600% collateral percentage (750-million versus 3-billion bound).
  Source was being finalized; this is not evidence against the final helper.
- `pnpm --dir demo/midgard-fault-proofs exec vitest run tests/funding-parameter-update.test.ts --reporter=default`:
  after the final capacity implementation, 7 collected and passed, exit 0.
- `pnpm --dir demo/midgard-watcher exec vitest run tests/funding/parameter-capacity-recovery.test.ts --reporter=default`:
  after final fixture corrections, 1 collected and passed, exit 0. This run
  observed the complete capacity-cycle/restart/reconciliation/refresh behavior.
- `pnpm --dir demo/midgard-fault-proofs exec vitest run tests/funding-parameter-update.test.ts --reporter=default`:
  final expanded capacity-policy substitution controls; 12 collected and passed,
  exit 0. Credential, contract custody, economics, references and fixed category
  remain pinned for both original and capacity policies.

## Mutation evidence inspected

The funding owner ran the mutation checks; this reviewer read their actual
output and independently ran the final green regressions above. The reviewer
did not rerun these mutations or attest the isolation method used for the first
three logs.

- `/tmp/a7-review-f1-red.log`: original unconditional current-cap admission;
  7 executed, 6 passed and the exact historical signed recovery regression
  failed at `assertSnapshotInputBounds`, exit nonzero.
- `/tmp/a7-review-f2-shortage-red.log`: test-controlled historical selection
  calculation substitution; 1 executed, 1 failed, 7 skipped. It chose the
  smaller historical coin instead of the independently expected adequate
  `cd...#0` coin. The log also has an unrelated duplicate-property warning
  from an intermediate edit; that warning was not the test failure.
- `/tmp/a7-capacity-growth-red.log`: original cap-one restriction during
  current-cap-three repricing; 1 executed, 1 failed, 8 skipped. The selected
  target failed at fresh action begin with funding unavailable.
- `/tmp/a7-overcount-guard-red.log`: isolated copied begin adapter with only
  the over-current-count refresh condition removed, plus a copied final cycle
  test importing that adapter. The current count refusal stayed in place.
  `pnpm --dir demo/midgard-watcher exec vitest run --config .a7-overcount-scratch/vitest.config.ts .a7-overcount-scratch/cycle.test.ts`
  collected 1 test, which failed at the final begin's
  `assertCurrentFundingCollateralLimit` after the digest returned to the
  original value; exit 1. This reviewer read the log and preserved scratch
  copies at `/tmp/a7-overcount-scratch/successful-{vitest.config,begin,cycle.test}.ts`.
  The final source green cycle was independently observed above. Two owner
  scratch configuration attempts that stopped before collection are not counted
  as mutation-test evidence.

## Residual risks and limits

No confirmed remaining scoped defect is recorded. No new semantic-family
coverage-gap entry in `docs/fault-proofs/remaining-gaps.md` is proposed.

This reviewer did not run full package suites, lint/typecheck, preflight,
SDK/node/emulator transaction-preparation acceptance, live L1 parameter updates,
Aiken/golden/execution-ledger checks, or forced process/filesystem crashes at
each fsync. The retention tests use synthetic observations and a small suffix;
they do not prove production-size retention. Durable non-observation records
are retained by their owning operations; this change deliberately keeps their
referenced observations, so it is not proof that all durable storage is bounded
under every completed/unresolved workload. The floor remains within the
existing operationally independent single-owner trusted-head threat model.
The wider required gates are the combined change's final-verification work;
this independent pass ran the scoped regressions without competing for build
outputs. It did not operate a live L1/devnet or inject physical crash failures,
so the source/fixture evidence is not claimed as that acceptance evidence.
