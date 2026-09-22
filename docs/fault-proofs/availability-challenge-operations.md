# Availability challenge operations

Status: implemented, with local verification recorded below. Live release
acceptance remains outstanding. The publication evidence in
[the size plan](size-plans/availability-challenge.md) establishes the contracts
and registered emulator lifecycle. This plan closes the operational boundary;
it does not change the protocol, bonds, deadlines, or reference roles.

## Execution design

1. Add reusable SDK builders for open, ordered chunk publication, tranche
   settlement, answered close, and timeout with unavailable queue removal.
   Timeout must also prune descendants and finish removal under the existing
   correction lock. Builders authenticate script units, datums, reference roles,
   exact carrier outrefs and frozen commitments. Explicit reserved funding and
   collateral replace test-wallet selection. Evaluate locally, enforce deployed
   fee ceilings, and retain production validity backoff.
2. Share an intent-first operation executor between CLI, watcher and responder.
   A durable SQLite journal binds deployment, actor, action and exact inputs to
   immutable signed CBOR before submission. Atomic resource reservations and
   fenced leases prevent conflicting work. Restart reconciles the same tx hash,
   inputs and canonical inclusion; ambiguous submission never creates a new
   transaction. Keep reservations until finality or proven expiration with all
   inputs still unspent. Rollback reopens reconciliation and blocks mutation when
   canonical evidence is uncertain.
3. Wire the accountable responder into the committee process. Discover live
   challenges, load retained payloads and verify the entire frozen commitment
   before publishing the next required chunk. Rotation does not change the bond
   beneficiary. Publication is permissionless; a responding wallet need not be
   the original signer. Resume from authenticated tranche/carrier state, then
   settle and close fully answered challenges. Missing or mismatched payloads
   remain visible failures and never produce invented bytes.
4. Wire independent watcher actuation before fault classification. An attested
   header whose public retrieval fails becomes an availability challenge
   candidate, not a healthy header or a fabricated fault proof. Reconcile live
   challenge state and durable intents, reserve the full configured challenge
   funding, open, settle completed or expired tranches, close answered challenges
   or remove an unavailable header. Preserve native chain provenance and revoke
   observations on rollback.
5. Expose operational CLI actions and status/recovery with an explicitly isolated
   actor wallet and durable journal path. Commands and process actors invoke the
   same builders and executor. Readiness checks authenticate the release manifest,
   live reference scripts and registered withdrawals.
6. Separate deployed capability from retention authority. A manifest with the
   required contracts is no longer reported as missing. It is not evidence that
   a particular header has no active challenge. Retention requires authenticated
   finalized per-header terminal state and revokes that authority on rollback;
   absent or uncertain observations retain data.

## Acceptance

- Run real signed SDK-builder emulator flows using the existing protocol-11
  mainnet limits: full response, no response after attestation, partial response,
  answered close, timeout and descendant removal. Keep existing publication and
  failure-polarity scenarios passing.
- Exercise persistent restart before submit, ambiguous submit, already-included
  transaction, repeated requests, concurrent actors, expired intent and rollback.
- Verify insufficient funding, overlapping collateral, missing references,
  corrupted retained bytes and premature timeout refuse before submission.
- Prove watcher public-fetch failure actuates a challenge and committee retained
  payload service answers through the shared production builders/executor.
- Run narrow package tests, typechecks, formatting and lint for touched paths;
  record exact results and remaining release acceptance limits here.

Ownership: SDK builders and emulator integration; watcher intake and actuation;
committee responder integration; shared operation journal/executor, node commands,
capability and retention authority. Preserve unrelated work in the dirty tree.

## Delivered behavior

The SDK exports all seven transaction builders, authenticated live snapshots,
payload reconstruction, exact opening-funding preparation and the shared signed
operation executor. The [CLI guide](../../demo/midgard-node/docs/availability-challenge-commands.md)
describes the command group; the [committee guide](../../demo/da-committee-node/docs/availability-responder.md)
describes dedicated responder keys and durable storage. The watcher drives its
independent actor from public retrieval failure and finalized native observations.
It can recover publication bytes from authenticated L1 history.

The journal distinguishes canonical inclusion from finality so response chains
can advance without waiting the finality depth after every chunk. It preserves
resources until finality, shares collateral only between the same actor's
compatible operations, and reconciles causal descendants before ancestors. A
canonical child proves inclusion of its ancestors; a finalized child becomes the
persistent audit anchor. Uncertain evidence pauses work. Positive loss of a
finalized anchor persists an incident halt; deleting the journal is not recovery.
Ordinary rollback and expired orphan chains reconcile without replacement bytes.

An opened challenge owns the actor wallet's future capital until the terminal
transaction is finalized. Actor processes must share one journal. Wallet funding
excludes resources reserved under any deployment. Both watcher and CLI check the
remaining live queue suffix before timeout; the transaction references that
queue's tail to prevent appends from invalidating the checked removal budget.
Initial timeout keeps the wallet reserved while descendants remain to be pruned.
Dedicated enterprise actor wallets provide exact opening inputs; responder fees
use the protected on-chain challenge shares.

Capability reports distinguish missing deployments from deployed but unobserved
availability state. Neither classification authorizes payload deletion. Node and
committee collectors authenticate exact consumed queue outputs and ordered
transitions, persist per-header terminal evidence, and revalidate it with finality
and retention deadlines. Rollback revokes authority and serializes against
pruning. Generic terminal status, missing UTxOs and an attestation alone retain
bytes.

The locked [Lucid dependency patch](../../demo/patches/README.md) resolves its
explicit-fee delayed-redeemer bootstrap failure. Resolved transactions retain real
local evaluation and fee checks. Lifecycle limits apply to the final signed CBOR
and aggregate execution units with a 20% execution reserve. The separate
512-byte publication reserve remains enforced on reference-script publication;
it is not subtracted again from a fully signed response transaction.

## Verification record

Checks ran with Node 22.22.2 and the declared pnpm 9.15.4 toolchain. The unchanged
testnet blueprint SHA-256 is
`eaf17c21a8fe433f80815a588f73ca39c29f39e40af16649ef897ec0c632b375`.
No live Cardano transaction was submitted.

- Core journal: four tests, including a killed writer, independent SQLite
  connections, immutable intent, cross-deployment reservations, workflow capital
  ownership and persistent incident halt.
- Signed operation recovery: six tests, including ambiguous submission/restart,
  canonical inclusion, expiry, rollback generation, orphan chains, ancestor
  recovery, finalized-anchor audit and signed-envelope refusal.
- Production SDK lifecycle: five passed. Happy response and close, zero-response timeout,
  two-descendant pruning and final removal, partial-carrier settlement and timeout.
  Negative cases include wrong references, insufficient fees/funding, premature
  deadline and mismatched carrier. The bounded maximum case opens 64 MiB across
  16 tranches and 19 outputs, then publishes two chunks through the shared executor.
  Its 15,923-byte signed response crosses the former overly strict reserve check
  while satisfying the ledger cap and 20% execution reserve. Ambiguous submission
  and journal restart recover the same transaction without rebuilding or
  rebroadcasting; authenticated progress resumes at byte offset 28,040.
- Committee signed responder lifecycle: a real attestation and challenge followed
  by permissionless publication, SQLite reopen between chunks, settlement and
  exact refund of the frozen original bond beneficiary.
- Existing four real contract lifecycles: all passed, including 16-tranche opening
  and settlement and all 301 chunks of a complete two-tranche payload. Together
  with the six recovery, four SDK and one responder tests, the initial combined
  run passed 15 tests.
- Publication/readiness, mainnet parameters, atomic initialization and full
  reference roster: 13 tests passed across five files, including all eight
  initialization tests and signed submission of all 513 runtime targets.
- Committee responder/config/reference checks: 47 passed. Native retention,
  provider, scanner, replay and watcher regression: 92 passed, one existing
  unrelated opt-in signature-republication test skipped.
- Node retention: 23 passed against isolated databases using
  `MIDGARD_TEST_DATABASE_PREFIX=midgard_availability_retention_20260908`,
  `MIDGARD_NODE_TEST_FORKS=1`, and `MIDGARD_SKIP_NATIVE_BUILD=1`. Shared development
  databases were not reset. Initial-schema changes are applied in place for this
  undeployed version.
- Watcher integration: 66 focused tests and 13 raw-source tests passed, covering
  failed public retrieval, native observation and rollback, retained payload
  fallback and actuation.
- CLI command/source funding checks: eight passed. Watcher action/funding checks:
  eight passed. Package typechecks and scoped ESLint/Prettier checks passed.
- Final core and SDK builds, complete node bundle and worker declaration builds,
  and bundled `availability-challenge --help` / `open --help` all passed.

Primary emulator checks, from the repository root with the declared toolchain:

```sh
MIDGARD_SKIP_DB_TESTS=1 NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run \
  tests/availability-challenge-operation.test.ts \
  tests/availability-challenge-sdk-lifecycle.test.ts \
  tests/availability-challenge-responder-lifecycle.test.ts \
  tests/availability-challenge-lifecycle.test.ts
pnpm --dir demo/midgard-core exec vitest run tests/availability-operation-journal.test.ts
MIDGARD_SKIP_DB_TESTS=1 NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run \
  tests/availability-challenge-publication-admission.test.ts \
  tests/availability-challenge-readiness.test.ts \
  tests/mainnet-protocol-parameters.test.ts \
  tests/initialization-emulator.test.ts \
  tests/scratch-cg1-publication-fit.test.ts
```

These local results establish implementation behavior, not acceptance of a live
deployment. Release acceptance still needs independent live actors and retained
payload service, actual publication/registration, withholding and partial-response
timeout exercises, and recovery under the target deployment's chain/finality
configuration. See [release acceptance](execution-plan.md).
