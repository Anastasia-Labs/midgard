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
   transaction. Reservations are released only once the signed transaction's
   validity has passed: an unlanded intent then expires on evidence that it
   cannot land (one normal input spent and another still unspent, a normal
   input spent by a valid transaction of another hash, verified from its raw
   bytes, or every input back unspent), and a confirmed one frees its inputs
   once the canonical tip is past that validity. Otherwise a record keeps them
   until it is pruned beyond `automaticRecoveryMaxDepth` (2160). Confirmation
   depth never releases them. Absence of our transaction is never proof.
   Rollback reopens reconciliation and holds that intent while canonical
   evidence is uncertain.
3. Wire the accountable responder into the committee process. Discover live
   challenges, load retained payloads and verify the entire frozen commitment
   before publishing the next required chunk. Rotation changes neither the
   pooled DA bond nor the challenge record, and a Close refunds the challenger
   the record names. Publication is permissionless; a responding wallet need not be
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

A lost challenge slashes the pooled DA bond, which backs every attestation of
the committee. The [DA bond pool guide](../../demo/midgard-node/docs/da-bond-commands.md)
covers how `init` first funds the pool, the permissionless top-up, the owner
quorum's two-step withdrawal through the offline multi-signer flow, and the
watcher alerts and committee readiness reasons raised while the pool is short
or withdrawing.

A withheld block can be timed out only once it is the queue head. Its Timeout
slashes the pool once, removes that head and prunes every descendant without a
further slash, so the committee's liability is one DA bond per withholding
episode, not one per withheld block. Every Apply needs a full bond of backing,
so no block applied before a slash meets the slashed pool; a Timeout meets a
partly funded pool only when it lands late, after the owners' CompleteWithdraw.
While the queue head is `Challenged`, the state queue refuses every Append, as
it does while the head is `Unattested` past its attestation timeout. Block
production therefore stalls until the head's challenge is closed or times out,
which is why the responder answers promptly and the watcher times out
unanswered challenges as soon as they are due.

The journal distinguishes canonical inclusion from finality so response chains
can advance without waiting the finality depth after every chunk. It preserves
resources until the transaction can no longer land, shares collateral only
between the same actor's compatible operations, and reconciles causal
descendants before ancestors. A canonical child proves inclusion of its
ancestors; a confirmed child becomes the audit anchor. Confirmed records and
their workflow rows are retired, not deleted, and pruned only beyond the
manifest's `automaticRecoveryMaxDepth` (2160). Uncertain evidence holds the
affected intent: until fresh evidence resolves it, readiness fails with its
reason and the same actor signs no new transaction, while its other intents
still reconcile on every pass. The committee responder reports such a hold as
`held` and fails readiness with `availability_operation_held:<tx>: <reason>`.
A responder drain that fails is retried at most three times on a 5 s, 10 s,
10 s backoff (`AVAILABILITY_RESPONDER_RETRY_POLICY`); once that burst is spent
readiness fails with `availability_responder_retries_exhausted:<error>` and the
responder keeps draining on its poll interval until a drain no longer fails. A confirmed intent the chain no longer
carries is rewound to pending and its identical signed bytes are rebroadcast.
Ordinary rollback and expired orphan chains reconcile without replacement bytes.
A `halt` row left by an older journal is cleared on open with one log line.

Each opened challenge is its own journal workflow, keyed by actor, deployment and
header. The workflow ends when the actor's own terminal transaction is finalized
or its own Open expires. A terminal step landed by someone else (the committee's
Close, another watcher's Timeout, a prune or removal of the header) releases the
row once a finalized, verified transaction burns the header's queue node or
closes the header's challenge. The watcher checks this on every reconciliation
for each of its rows in any deployment: from its own confirmed Open it walks the
header's queue node from spend to spend, each spend proven by the consuming
transaction's raw bytes at the finality depth, until a transaction burns the
node or spends the challenge record while minting nothing under the queue
policy. Consuming the record alone never releases the row: a Timeout with a
descendant consumes it while the header's removal chain still needs the
wallet's reserve. Released rows are reported under `workflowReleased`; a failed
check keeps the row, is reported under `workflowReleaseDeferred` and is retried
on the next reconciliation. A release stays reversible until its terminal
transaction exceeds `automaticRecoveryMaxDepth`: the journal keeps the evidence,
each reconciliation verifies it again, and a terminal transaction that is no
longer canonical makes the row live again, reported under
`workflowReleaseDeferred`. One actor may run challenges on several headers of
one deployment at once. While any of them is live or its terminal step remains within the rollback
recovery horizon, the journal refuses every step for another deployment, because the wallet's removal reserve is computed
from one deployment's queue, and it refuses a new challenger-coin preparation
for a header whose own Open landed. The watcher reports each refused step under
`workflowRefused`, naming the deployment and header of the live workflow that
blocks it. A challenge still live in a deployment nobody drives any more keeps
refusing other deployments until someone terminates it. Actor processes must
share one journal; it migrates a schema-1 journal (one
workflow per actor) in place. A provisional terminal permits progress in the
same deployment while keeping the capital guard against another deployment;
a failed terminal re-verification keeps that guard. A verified foreign
terminal beyond recovery depth clears the capital guard even while its Open
history remains. An Open proved never landed after TTL expiry also clears
its capital guard without deleting its historical progress. Confirmed
ancestor history is pruned independently using authenticated block heights,
so continuous activity does not retain the entire ancestry. Expired progress
is kept through 2160 blocks after its first authenticated post-expiry boundary
and while an unresolved child still needs it. Older journals start this
conservative retention clock on their first authenticated observation.
Wallet funding excludes resources reserved under
any deployment. Both watcher and CLI check the
remaining live queue suffix before timeout; the transaction references that
queue's tail to prevent appends from invalidating the checked removal budget.
Initial timeout keeps the wallet reserved while descendants remain to be pruned.
Dedicated enterprise actor wallets provide exact opening inputs; responder fees
use the protected on-chain challenge shares.

A pending intent whose inputs another transaction consumed first, such as a
Timeout another watcher landed on the same header, or a pool top-up that spent
the pool input, expires once the intent's validity has passed. Reconciliation
needs positive evidence: one normal input spent while another is still unspent
at the same point, or a normal input consumed by another valid canonical
transaction at confirmation depth, verified by reading that transaction back.
The expiry releases the intent's reservations, so the actor proceeds to its next
step instead of waiting on the lost intent. Missing inputs alone, or a spent
collateral input, never expire an intent.

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

This record predates the pooled DA bond (#685), which replaced the bond each
attestation used to lock; its responder-lifecycle refund wording describes that
earlier design.

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
