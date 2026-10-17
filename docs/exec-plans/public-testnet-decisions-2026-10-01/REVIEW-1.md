# Independent review, pass 1

Date: 2026-10-01. Base: `3027be19cb3bc740f83af0e155e34ab38c7cf894`.
Input was the scoped working-tree diff, new files, final files and their callers.
No author rationale or earlier review findings were supplied. No implementation
files were edited.

## Scope and invariants

- A7: changed/new watcher `src/funding/*.ts`; availability
  `runtime.create-watcher-availability-runtime.ts` and
  `runtime.protocol-parameter-refresh.ts`; runtime
  `watcher-prover-funding-runtime.ts` and
  `watcher-runtime.create-watcher-runtime.ts`; fault-proofs
  `funding-reservation-permit.create-workflow-funding-reservation-permit.ts`;
  changed/new funding tests and their changed helpers.
- A4: rollback durable commit, observation, canonical progress and retention;
  trusted-head store and retention floor; the two new retention test files.
- Read `references/lenses.md`, `invariants-da.md` and
  `invariants-fraud-proofs.md` in the consensus-review skill. No validator,
  blueprint application or canonical codec changed in this scope.
- Relevant gates: focused watcher funding/recovery, availability and retention
  tests; focused fault-proofs runtime funding tests; package lint/typecheck and
  repository preflight; transaction-preparation SDK/node/emulator checks for
  final acceptance of the transaction-selection changes. There is no changed
  golden channel or execution ledger to regenerate in this scope.

## Ranked findings

### [F1] CONFIRMED — major

Where: `demo/midgard-fault-proofs/src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.ts:253`, `:294`;
`demo/midgard-fault-proofs/src/workflow/funding-reservation-permit.reconcile-workflow-funding-submission-handoff.ts:234`;
`demo/midgard-watcher/src/fault-proofs/fault-proof-execution.ts:305`.

Defect: A reduced live collateral-input limit prevents reconciliation of an
existing signed attempt, including a transaction already confirmed under the
old limit, because permit creation applies the new limit to the entire old
lease before the reconciliation runner can execute.

Trace:

1. An ordinary wallet with no single sufficiently large pure-ADA coin gets two
   collateral leases under the old cap of three; the production selector permits
   this at
   `demo/midgard-watcher/src/funding/prover-funding-reservation.plan-watcher-prover-funding-reservation.ts:48`.
2. A proof step is signed, its exact intent and leases remain durable, and the
   transaction confirms before an L1 update reduces the cap to one. A process
   interruption can leave its confirmation unrecorded locally. This is the
   already supported recovery starting state used by the funding fixture.
3. On resume, the factory reconstructs the original policy and calculation,
   while passing the current policy to the permit, at
   `demo/midgard-watcher/src/funding/prover-funding-authority.create-watcher-prover-funding-authority-factory.ts:240`
   and `:278`.
4. Permit creation takes `maximumCollateralInputs` from the current policy at
   `funding-reservation-permit.create-workflow-funding-reservation-permit.ts:253`
   and unconditionally calls the snapshot bound check at `:294`. The check
   counts two historical leases and throws at
   `funding-reservation-permit.reconcile-workflow-funding-submission-handoff.ts:238`.
5. `fault-proof-execution.ts:305` must mint that permit before invoking
   `application.runOrResume` at `:317`. Consequently no runner can observe the
   confirmation, expire or abandon the exact old intent, or release/refresh its
   leases. Repeated resume requests encounter the same immutable state.

Reach: A malicious operator can leave a real fault needing a proof while an
honest watcher hits this ordinary L1-update/crash ordering. The old signed
transaction and wallet leases are valid under their historical policy; the
failure happens after their recovery identity passes admission. Counting
historical leases against a new transaction's cap is unnecessary for recording
a past confirmation and also blocks reconciliation-only authority.

Lens: 12, replacement/recovery paths keep the recovery guards without making
the recovery itself unreachable; 7, both polarities across a parameter update.

Evidence: The new test `tests/funding-parameter-update.test.ts` explicitly
asserts unconditional rejection of a two-collateral snapshot after the cap
becomes one. That test passed in the run below. It does not exercise signed
intent reconciliation. The complete caller chain above was read; no emulator
scenario for this exact ordering was run. Severity is major because the proven
outcome is recovery liveness loss; no false slash was established.

### [F2] PLAUSIBLE — major

Where: `demo/midgard-watcher/src/funding/prover-funding-authority.create-watcher-prover-funding-authority-factory.ts:240`, `:261`;
`demo/midgard-watcher/src/funding/prover-funding-authority.create-watcher-prover-funding-authority.ts:302`.

Defect: After an existing workflow reconciles its old signed attempt, a live
increase in required collateral can leave its future steps permanently using
the old collateral sizing even when the wallet contains adequate new coins.

Trace: An admitted old reservation has a small collateral coin; fees or the
collateral percentage rise -> the factory intentionally recovers the old
parameter snapshot and computes `calculation` from `reservationPolicy` at
`:261` -> that same historical calculation is passed to the authority at
`:284` -> next-action selection exposes the old collateral leases at
`demo/midgard-fault-proofs/src/workflow/funding-reservation-permit.begin-workflow-funding-reservation-action.ts:110`
-> a current transaction needs more collateral than their total -> the current
policy's percentage/value checks refuse it at
`demo/midgard-fault-proofs/src/workflow/funding-reservation-permit.assert-runtime-transaction-bound.ts:256`.
Even when idle input refresh becomes reachable, it plans from
`input.calculation` at `prover-funding-authority.create-watcher-prover-funding-authority.ts:304`;
the selector chooses the smallest coin meeting the old requirement at
`prover-funding-reservation.plan-watcher-prover-funding-reservation.ts:38`.
Wallet top-ups alone therefore need not cure the shortage.

Reach: Existing workflows and historical snapshots are expressly admitted by
this change. The old policy/basis identity must remain fixed for signed intent
recovery; the same identity currently also fixes every subsequent idle
collateral selection. Current unspent leases do not trigger `refreshIdle`
merely because collateral requirements changed (`begin-workflow-funding-reservation-action.ts:83`).

Lens: 12, parameter-update continuation; 4, collateral remains sufficient for
the live transaction without changing historical intent identity.

Missing: Run an actual installed proof step after a supported live parameter
increase, with collateral deliberately sufficient for the old transaction and
insufficient for the new one, plus an adequate unleased larger wallet coin.
Measure the transaction's actual fee/collateral requirement and demonstrate that
the production builder cannot recover by selecting the larger coin. This was
not reproduced, so it remains PLAUSIBLE rather than a measured budget finding.

## Lens coverage

1. Parameter trust — Clean for the scoped SDK/validator boundary: no validator
   parameters or application doors changed. Live queries are loopback-only;
   recovered snapshots require an admitted signed identity or authenticated
   reservation-bound history.
2. Always-succeeds scripts — Clean: no script deployment, arity, validator arm
   or yield handshake changed.
3. Decoders and pinned compiler — Clean within scope: historical JSON is
   canonical, HMAC-checked, reservation-bound and parsed through the existing
   protocol-parameter parser; retention floors require exact fields, canonical
   bytes and valid record/floor MACs. No new Aiken decoding was introduced.
4. Value conservation — Finding F2 concerns live collateral sufficiency;
   unchanged signed-body accounting and exact lease output derivation were
   read, with no reachable value-redirection defect found.
5. Anchoring, not preimage — Clean: history binds deployment, reservation,
   policy and basis before re-admission; the floor binds its authenticated
   record hash and the retained suffix checks its first prior-record hash.
6. Reference scripts — Clean: the bridge preserves the reference-script
   roster, and the runtime signed-transaction guard still forbids inline
   executable witnesses and authenticates governed reference scripts.
7. Both polarities — Finding F1: parameter-update tests cover accepting fee
   changes and rejecting authority substitution, but not historical signed
   reconciliation when a cap decreases.
8. Gates that cannot fail — Clean on the tests run: nonzero collected counts;
   wrong-key, tampered-history, cap-reduction, floor-tamper and missing-floor
   checks assert independently observable refusals. No mutation red-check was
   performed because this pass made no fixes.
9. Execution and size budgets — No confirmed new budget defect: no on-chain
   work/ledger changes; retained observations are selected by recovery depth
   and exact references, and pending work preserves its evidence. F2's actual
   collateral shortage still needs the measurement named above.
10. TypeScript/Aiken twins — Clean: no canonical codec, proof computation twin
    or generated golden changed in this scope.
11. Ledger facts — Clean: existing signed transition derivation reads actual
    body inputs/outputs; availability retries only unsigned builds, reselects
    resources and preserves the action, while included/expired signed intent
    reconciliation is performed before checking current rebroadcast limits.
12. Replacement guards — Findings F1/F2. Otherwise paired guards survived:
    credential/deployment/runner/economics/references remain stable, contract
    changes remain additive, original lease policy and basis remain exact;
    rollback compaction keeps every durable chain-point reference and all
    observations sharing retained block hashes; incomplete consistency entries
    are dropped together; the floor is fsynced before summarized files are
    deleted and suffix successor/hash/CAS checks remain present.

## Verification evidence

All commands ran on 2026-10-01 from the repository root. Other work was active
in the shared checkout. `PINNED_PNPM` below denotes the exact executable
`/home/gumbo/.nvm/versions/node/v22.22.2/bin/node /home/gumbo/.cache/node/corepack/v1/pnpm/9.15.4/bin/pnpm.cjs`;
each test command set
`PATH=/home/gumbo/.nvm/versions/node/v22.22.2/bin:$PATH`.

- `node scripts/doctor.mjs`: exit 1; sandbox EPERM prevented the compiler,
  Postgres and child-process probes; blueprint stamp was fresh and dependencies
  existed. The default shell's Node 24 differed from CI. Tests below used Node
  22.22.2 and pnpm 9.15.4 explicitly.
- Initial unpinned `pnpm --dir demo/midgard-watcher exec vitest run ...` and
  `pnpm --dir demo/midgard-fault-proofs exec vitest run ...`: both exit 1 before
  collection because the shim selected pnpm 11.18.0. They are not test results.
- `env MALLOC_MMAP_THRESHOLD_=131072 PINNED_PNPM --dir demo/midgard-watcher exec vitest run tests/funding/protocol-parameter-history.test.ts tests/funding/protocol-parameter-retry.test.ts tests/funding/prover-funding-calculation-reuse.test.ts tests/l1/rollback-retention.test.ts tests/runtime/trusted-head-authority-retention.test.ts --reporter=default`:
  18 collected, 15 passed, 3 failed; exit 1. All three failures were fixture
  `mkdtemp('/var/tmp/midgard-trusted-head-*')` EROFS before the tests exercised
  the trusted-head code. History 1, retry 3, calculation/recovery 7, and rollback
  retention 4 passed.
- `env MALLOC_MMAP_THRESHOLD_=131072 PINNED_PNPM --dir demo/midgard-watcher exec vitest run tests/runtime/trusted-head-authority-retention.test.ts --reporter=default`:
  rerun with sandbox escalation for the existing `/var/tmp` fixtures; 3
  collected and passed, exit 0.
- `PINNED_PNPM --dir demo/midgard-fault-proofs exec vitest run tests/funding-parameter-update.test.ts --reporter=default`:
  7 collected and passed, exit 0.
- `env MALLOC_MMAP_THRESHOLD_=131072 PINNED_PNPM --dir demo/midgard-watcher exec vitest run tests/funding/prover-funding.test.ts tests/availability/runtime.test.ts --reporter=default`:
  21 collected and passed (funding 4, availability 17), exit 0.

## Residual risks and limits

F1 remains open. F2 needs the concrete builder/collateral experiment above.
Neither is a new semantic fault-proof-family coverage gap, so no entry in the
family `remaining-gaps.md` register is proposed by this pass.

No fixes, scratch regressions or red-checks were made. No full package suites,
package lint/typecheck, preflight, SDK/node/emulator transaction-preparation
acceptance, live L1 update experiment, forced filesystem crash at each fsync,
or Aiken compiler/golden/execution-ledger gates were run. This is the independent
review pass, not final acceptance. Unrelated full-stack, node and public-DA
changes were outside the supplied scope. The retention tests use a small
configured suffix and synthetic observation records; their results do not
prove production-size retention under every unresolved proof shape.
