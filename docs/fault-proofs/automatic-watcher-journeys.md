# Automatic watcher journeys

<!-- doc-links:run-relative -->

Status: Active

The acceptance target is a fresh, isolated Cardano devnet with real Cardano
node, Kupo, Ogmios, native chain sync, public libp2p DA retrieval, and the normal
watcher launcher. The source catalogue supplies the family inventory. Each
family must be detected from a committed L1 header and public DA, complete its
installed workflow automatically, retain its permanent proof token, remove the
faulty header with the configured slash/reward, and classify a subsequent honest
commitment as healthy. A family counts only after these observations pass.

## Preflights

The run must not reuse or reset the checkout's persistent Preprod services.
Successful lower-layer or emulator tests do not count as this devnet acceptance.

Before the watcher starts, a binding preflight runs the production configuration
loaders for every installed family against the finalized manifest and actual
reference UTxOs. Its application exposes readiness checks and resource cleanup
only. `workflow-binding-preflight.test.ts` runs this check independently of the
watcher launcher; its per-family receipts do not imply live proof completion.

For each family, the runner uses the session's actual parsed configuration
and verified deployment identity to retrieve both staged payloads through the
production public DA runtime. Exact bytes and authenticated peer identities must
match. `public-da-preflight.json` records payload hashes, durations, and failed
requests. The standalone `retained-da-live.test.ts` uses this same preflight for
cheap diagnosis without restarting the watcher or submitting transactions.

`retained-da-lifecycle-live.test.ts` checks the shared public DA transport with
the actual runtime configuration and retained payloads. Set `MIDGARD_WATCHER_DA_CONTINUOUS_SECONDS=720` for the lifecycle test's bounded
continuous-retrieval variant; its evidence is not proof evidence.
Set `MIDGARD_WATCHER_DA_CONCURRENT_LEASES=8` to exercise the actual owner's full
request concurrency; the default is two leases.

## Shared journey session

`journey.test.ts` lazily opens one `JourneySession` from `journey-session.ts` for
the selected sequential families and closes it in the suite's `afterAll` hook.
The session owns the deployment context, independent native recorder, retained
DA server, authenticated history archives and installed watcher. Later families
reuse these running services. The session creates the watcher's rollback,
prover and availability secrets under the run's `secrets/` directory only while
the shared runtime is fresh; once the runtime exists, a missing secret is
refused rather than regenerated.
The native recorder replays from origin once per session; a new process still
authenticates its history and resumes the existing durable runtime. A standalone
runner call owns and closes its own session.

The watcher starts against the retained journey head before staging when one
exists, or against the first staged target otherwise. Its original readiness
header is a startup binding; subsequent classification observes the current
queue. The watcher keeps no record of its workflows: the correction wait reads
each family's progress and correction from the chain, and the watcher's own
verdicts from its `/v1/status`.
Fixture wallets keep their existing Base addresses, while prover wallets use
Enterprise addresses for funding and keep proof materials at the Enterprise
address of the native script `sig(payment key)`, which no fixture wallet
uses. Address-scoped selection isolates their UTxOs even where they share a
payment key; the session requires no wallet pause API.

Session artifacts live under `work/journeys/sessions/session-*`: process
configuration, binding preflight, `start.log`, restart records, session
`timings.ndjson`, and the shared `native-chain.ndjson`. The watcher's decision
journals and availability state remain in `work/journeys/runtime`.
Each family keeps its own staging, correction, successor, result and timing
artifacts. Its `session.json` identifies the session, process, configuration,
binding receipt and native evidence path. Results and finalized stamps point to the actual recorder
path.

Session reuse checks pinned configuration, deployment and executable inputs,
including compiled watcher and fault-proof artifacts. A failed-closed watcher
can restart within the existing bounded policy using the same bindings and live
services; unrelated exits or changed identities fail the session. Initialization
and family failures close owned services, and cleanup attempts every owner even
if one close operation fails. A normal family handoff leaves the services alive;
the last selected family still waits for outstanding finalized stamps before
suite cleanup.

A proof can consume the attested queue output before the DA fixture
publisher's post-submit live-UTxO lookup, so the publisher checks the apply
transaction returned by the independent native recorder. It verifies the exact signed body hash, the ledger's valid branch,
one output at the expected queue address carrying its token, and the expected
header and attested commitment hash in the datum. This authenticated receipt remains checkable
after the output is spent. Locally signed bytes alone do not establish inclusion.
Callers without the native receipt reader retain the existing live-output check.

## Shared configuration and verification

The common Phase 4 generator applies the verified Preprod profile before genesis
hashing. The snapshot records its query time and canonical source tip. Both the
CLI protocol-parameter representation (used to render genesis) and Ogmios
representation (used for an exact live comparison) are retained. Refresh the
profile explicitly after target-network changes and generate a fresh chain.

`devnet/watcher-journeys/configuration.test.ts` checks the running node without
publishing contracts. `deployment.test.ts` runs the same gate before funding or
publication. Local credentials, network magic, start time, initial funds and
initial delegation remain isolated test-network state.

## Devnet consensus timing

The watcher acceptance generator uses verified Preprod consensus timing:
`slotLength=1`, `activeSlotsCoeff=0.05`, `epochLength=432000`, and
`securityParam=2160`. Byron's `protocolConsts.k` is also 2160. All genesis
changes are applied and their configuration hashes recomputed before startup.
The deployment check refuses a run with different values.

Blocks are probabilistic, with approximately 20 seconds between blocks; an epoch
lasts five days. A 30-block confirmation takes approximately ten minutes. This is an acceptance rule, separate
from the chain's consensus security parameter.

Changing timing requires a fresh run directory and chain. Stop the previous
watcher Compose project, generate the replacement, and deploy its protocol state
afresh; never reuse a previous chain's database or deployment receipts.

## Devnet cost models

The watcher devnet pins Cardano node 11.1.3 by image digest. This is a
prerelease used only by this isolated acceptance environment. Node 11.0.1
silently retains the original Plutus V1/V3 cost models when `extraConfig`
supplies replacements ([ledger issue 5896](https://github.com/IntersectMBO/cardano-ledger/issues/5896));
the [11.1.0 release](https://github.com/IntersectMBO/cardano-node/releases/tag/11.1.0)
includes the fix.

The original 251-entry V3 model caused the real DA attestation's
`count_set_bits` call to exhaust both local and node evaluation budgets. The
replacement models in
`demo/midgard-node-tools/devnet/preprod/configuration.json`
were read from the local Preprod node at protocol version 11. They are supplied
through the supported Alonzo `extraConfig.costModels` field; the original
era-specific genesis fields retain their required historical lengths. Before
funding or publication, the deployment test compares every live model entry
against this pinned input, as well as the 16,384-byte transaction limit and
17,500,000-memory / 10,000,000,000-step transaction limits and
77,500,000-memory / 20,000,000,000-step block limits. The complete live Ogmios
protocol-parameter object must equal the recorded Preprod object, including
economic and governance parameters; selected-field checks are insufficient.

The fixture wallets use the same Scalus local evaluator as the watcher availability
runtime. With the corrected live cost models, node evaluation accepts the DA
signature transaction at 318,448 memory and 248,474,366 CPU units; Lucid's
default Aiken evaluator still reports a budget overflow for that transaction.
Local evaluation remains enabled and the actual node validates every submission.

Each run retains its genesis, deployment receipts, checkpoints, native block
stream, watcher journals, and stage timings. A failed run is stopped and retained,
not overwritten with a different genesis.

## Bounded reference publication

Run the builder/emulator checks and the small real-node publication probes before
starting a complete deployment. The shared publisher records exact signed bytes,
inputs, outputs, dependencies and role assignments before submission. Children
spend the designated predecessor change output; confirmed reference outputs and
role tokens remain reserved. Pending transaction count and signed bytes both
apply backpressure. Local evaluation receives the exact unconfirmed inputs.

The run's `work/publication-schedule.json` binds those limits and the measured
preparation rate, chain capacity estimate and bounded confirmation allowance.
Its companion evidence records how those measurements were obtained. The
planned workload determines the minting deadline. Publisher-signed policies
proceed after canonical publication and the role-token audit, trusting the
publisher key until expiry. Historical time-only policies still require canonical
expiry: check the node tip and its exact indexed checkpoint, never just a local
clock or elapsed timer. Report publication duration and readiness audit separately.
On restart, reopen the same authority and signed journal; reconcile canonical
transactions before extending it. An expired or invalid parent requires all
recorded descendants to be resolved; it does not authorize a replacement
publication identity.

Separate small real-node probes cover a lost acknowledgement after node
acceptance, byte-bound backpressure, and expiry of an accepted parent with a
pending child. These publication checks establish neither automatic watcher
journeys nor workflow interruption recovery. Initialization, the authority-expiry
wait and final deployment uniqueness verification remain distinct stages.

## Pooled DA bond journey

The pooled DA bond journey drives the committee's one shared bond pool through
the six steps of spec #685 on the process devnet. The step logic lives in
`devnet/watcher-journeys/da-bond-pool-journey.ts`. The live adapter is
`da-bond-pool-live-port.ts` and the live test is
`da-bond-pool-journey-live.test.ts`. The same driver also runs against the
emulator, but that run is a dry run: its report says `emulator` and never counts
as devnet evidence.

| Step | What it shows                                                                                                                                             |
| ---- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1    | B1 (withheld) is attested against a Bonded pool that backs one bond but not two; the Apply leaves the pool untouched.                                     |
| 3    | B1 is opened, left unanswered past its response deadline, settled and timed out; the Timeout slashes the pool and removes B1.                             |
| 4    | B2 is committed, and its Apply is refused `pool-under-backed`; the watcher and committee both flag the short pool.                                        |
| 5    | A top-up restores the backing, the alerts clear, and B2's Apply lands within `da_attestation_timeout_ms` of its end time.                                 |
| 2    | B3 (served) is attested, opened, answered from its payload, settled and closed; the pool is unchanged.                                                    |
| 6    | A withdrawal begins, B4's Apply is refused `pool-withdrawing`, the withdrawal is cancelled and B4 attests; a second withdrawal completes after the delay. |

The steps run in the order 1, 3, 4, 5, 2, 6 because each one sets up the pool
for the next. Step 1 needs a pool that backs fewer than two bonds, so step 3's
Timeout leaves it short for step 4. Step 5's top-up both answers step 4 and
restores the backing that B3's attestation in step 2 needs. The report still has one section
per spec step, in spec order.

B1 is removed before B2 is committed because the state queue refuses an Append
while its head is Challenged. B1 is the head from its Open until its Timeout
removes it. The Timeout burns the head's node itself when the head has no
descendant; otherwise the adapter follows it with the removal the challenge
command plans. The journey therefore needs a state queue that holds only its
root when it starts, so that B1 becomes the head; the adapter refuses anything
else.

For the Timeout, the adapter reads the fee and outputs from the landed
transaction and checks that `fee = fee_part + c` with `c` at most
`max_timeout_fee`, and that `fee = penalty` on a full pool. It also checks that
the pool output keeps its datum and only the pool NFT. There must be exactly one
output to the challenger, worth
`remaining - c + challenge_record_lovelace + payout`.

### Process evidence

The live test runs the driver with `requireProcessEvidence: true`, so two kinds
of process evidence are required, and a missing one fails its step:

- Committee process evidence, at step 1, from step 3 on, and on the node's
  restart before step 6: the verbatim `/readyz` answer of one real
  `da-committee-node` process (its HTTP status and body; the pool reasons come
  from the body) and the `da_bond_pool_*` events on that process's stderr,
  each tied to its pid. The first answer after each start must carry no pool
  reason and no event. At step 3 the same process's stderr must also carry an
  `availability_responder` line that reports B1's challenge `unavailable`.
  No responder line for B1, on stderr or stdout, may report anything else: an
  action's report (on stdout) names it, and a `failed` line (on stderr) may be
  a failed action.
- A real `midgard-node da-bond` CLI chain for each pool transaction, from step 5
  on: a `status` before; then either `top-up`, or
  `withdraw <step> --build-unsigned`, one `witness` per key and `assemble`;
  then a `status` after that reads the new pool output.

The live adapter produces both. It runs the committee node from
`demo/da-committee-node/dist/index.js` (see
[No watcher or committee daemon](#no-watcher-or-committee-daemon)) and each
`da-bond` command as a `node demo/midgard-node/dist/index.js da-bond` process.
Before each observation it waits, within a bound, until the node has read the
pool after the preceding action and its reasons agree with the adapter's own
pool read. The bound is ten of the node's 2-second polls plus twice the ideal
time for the release confirmation depth
(`2 x depth x slotLength / activeSlotsCoeff`, read from the run's Shelley
genesis and deployment manifest): 140 s on `local-devnet-testing`, whose depth
is 3. A spawn, stop or read that fails fails its step; nothing falls back
to an in-process composition. The `da-bond` CLI admits the devnet's `Custom`
network and derives its slot mapping from the local Ogmios, as the node does.

### Running it

The journey needs a freshly deployed run directory on the
`local-devnet-testing` profile. The checkout compiles `preprod-testing`, so
select the local profile first, as the
[deployments README](../../config/deployments/README.md#live-testing-profiles)
describes:

1. Run `pnpm --dir demo deployment:build local-devnet-testing`, which compiles
   and binds that profile's blueprint, then rebuild the off-chain packages with
   `pnpm --dir demo build`.
2. Generate the run directory with `devnet/watcher-journeys/scripts/generate.sh`
   and start its services. Run `configuration.test.ts` and `deployment.test.ts`
   as for the other journeys, with `MIDGARD_DEPLOYMENT_PROFILE=local-devnet-testing`
   and `NETWORK=Custom` set for every step, including the journey below.
3. Afterwards, restore the checkout's selection with
   `pnpm --dir demo deployment:build preprod-testing` and rebuild again.

The journey deployment writes no DA libp2p runtime for the committee node, so
the adapter produces one before the journey's first transaction, through the
same command an operator runs after init:

- fresh libp2p keys under `secrets/` (mode 0600, never overwritten): one per
  DA committee member of the deployment
  (`da-bond-pool-libp2p-committee-<signer index>.key`), one producer key and
  one public retained-DA key;
- the committee-target runtime manifest at
  `deploymentInfo/da-runtime-manifest.json`, written by
  `node demo/midgard-node/dist/index.js da-libp2p-generate-manifest --target committee --profile host`
  with the members' signer indexes, DA verification keys and threshold read
  from `deploymentInfo/manifest.json`. The libp2p ports are the generator's
  defaults plus this checkout's offset from `scripts/lib/worktree-identity.mjs`,
  the offset `generate.sh` adds to the devnet's own ports.

The node loads member 0's libp2p key and no DA signing key. The adapter records
the generator's arguments (key sources, not key bytes), its exit code, the
manifest's sha256 and the observer's peer id in `committee/runtime.json`. It
then checks the node's environment with the node's own configuration loader and
peer check. A key or manifest that already exists, a failed generator run, or
a refused configuration stops the journey
(`DaBondPoolCommitteeUnavailableError`). Because the keys are never
overwritten, a second full run on the same run directory is refused before any
transaction; each run needs a freshly deployed run directory.
`MIDGARD_DA_BOND_JOURNEY_RESUME_AFTER=5` reuses `committee/runtime.json` to
resume a run that passed steps 1, 3, 4 and 5 into steps 2 and 6; a resumed run
is a smoke check and never journey evidence. A refused
configuration comes after the challenger funding transaction may have landed.

The node's database comes from the devnet Postgres settings that the
generated `run.env` already holds (`MIDGARD_PHASE4_POSTGRES_*` and
`MIDGARD_PHASE4_COMPOSE_PROJECT`); the adapter creates a fresh database there
for the node.

It also needs `demo/da-committee-node` and `demo/midgard-node` built, which
step 1 does. The adapter creates the node's two submitter keys under
`secrets/` (mode 0600) and funds them once from the availability account.

Put the run directory at a real path with no symlink in it, short enough that
the node socket path stays within 108 bytes: the native reward-account query
refuses identity paths that traverse a symlink. Then, from
`demo/midgard-node-tools`:

```sh
MIDGARD_DEPLOYMENT_PROFILE=local-devnet-testing NETWORK=Custom \
MIDGARD_WATCHER_JOURNEY_RUN_DIR=/abs/run \
MIDGARD_DA_BOND_JOURNEY_REPORT_PATH=/abs/report.md \
pnpm exec vitest run --config vitest.watcher-journeys.config.ts \
  devnet/watcher-journeys/da-bond-pool-journey-live.test.ts
```

The test is skipped when `MIDGARD_WATCHER_JOURNEY_RUN_DIR` is unset.
`MIDGARD_DA_BOND_JOURNEY_REPORT_PATH` is optional. Expect about 90 minutes: the
withdraw delay (39 minutes 40 seconds on the local-devnet profile) and B1's
response window (14 minutes 40 seconds, the small-payload window) dominate. Each dependent step also waits for one block after inclusion
(`JOURNEY_ACTION_DEPTH`), roughly 20 seconds on this devnet. The test's own
timeout is three hours.

Kupo must match `*`, because the Open recovers its commitment and the challenge
snapshots read outputs at every address. The adapter refuses a narrower pattern.

### No watcher or committee daemon

Nothing else may act on the chain while the journey runs. A watcher would
contest B1 and B3 itself, and a DA committee node that holds the payloads would
answer the withheld B1. The adapter reads the process table, including each
process's environment, and refuses any watcher CLI or committee node process
that names a path inside the run directory in its arguments or environment.
That includes the shared journey session's watcher, so stop that session first,
or run this journey on its own run directory. The adapter fails closed if it
cannot read the process table or a daemon's environment.

The only daemon admitted is the adapter's own committee node, and it is
admitted by pid, never by command line. The adapter checks the process table
before it spawns the node, after each spawn, before each step and before each
Open. That node:

- runs from `dist/index.js`, unmodified, with L1 submission and its submitter
  preflight on;
- has no DA signer key (`DA_SIGNER_INDEX` and every `DA_SIGNER_KEY_SOURCE*` are
  unset) and no auto-fund key, and never receives a journey payload, so it
  cannot attest, answer B1 or Apply;
- submits with two fresh keys, distinct from each other and from every
  operational key. Their addresses must hold the same UTxOs before each start
  and after each stop, which shows it spent nothing;
- has its own availability journal, database and API port. The port is derived
  from the worktree path, so two worktrees do not collide.

The node starts before step 1. It stops before step 2's commit, because its
payload-free settle and Close would otherwise race B3. It restarts before step
6 and stops again at the end of step 6. Each stop must exit 0 on SIGTERM, leave
no daemon behind and find both submitter addresses unchanged for one node poll
plus the finality lag after the exit, so a last-tick submission cannot land
unseen, or the step it ended fails. The live test also requires the node to be
stopped when the journey passes. If the journey fails before that, the node gets SIGTERM, then
SIGKILL after a bound.

No operator keeps the scheduler alive during the long waits, and none needs to.
Inactivity strikes and the scheduler shift act only through strike and advance
transactions, a strike needs an undelivered user event, and nothing in a
journey devnet submits either. The state queue's
commit checks only that the scheduler's operator is the committer.

### Signers and the challenger wallet

The journey signs with the run's own keys from `secrets/journey-accounts.json`:

- the operator commits, and the operator and cosigner sign the DA attestation;
- the availability account pays for the top-up, funds the challenger and pays
  the withdrawal fees;
- the withdrawal quorum is `update_threshold` of the DA params owners, which on
  a journey deployment are the operator and the cosigner. If the run lacks a
  seed phrase or an owner key, the adapter raises
  `DaBondJourneySigningMaterialError`, which names each missing item.

The challenger is a separate key, created on first use in
`secrets/da-bond-pool-challenger.seed` (mode 0600) and reused afterwards. The
adapter refuses it if it matches the operator, publisher, cosigner, availability
or reference-script deployer key. The availability account funds it with:

- one exact Open coin per challenge (two):
  `challenger_bond + challenge_record_lovelace + max_open_fee`;
- one collateral coin covering the Timeout collateral bound (G9): the ledger's
  collateral percentage of `penalty + max_timeout_fee`, plus a 5 ADA margin;
- one operating coin, 250 ADA by default, for the removal fee and the challenge
  command's capital checks.

The funding transaction and plan are recorded in `challenger.json`.

### Where the results land

Everything goes under `work/journeys/da-bond-pool/` in the run directory:

- `da-bond-pool-journey.json`: the stage ledger, with every transaction id by
  label, the observations and the assertion results. It is written before the
  test passes or fails, so a failed run keeps its partial ledger.
- `timings.ndjson`: stage and wait timings.
- `availability-journal.sqlite`: the challenge operation journal.
- One file per committed block, the Timeout evidence, the served payload and
  `challenger.json`.
- `cli/`: one `NNN-<label>.json` per da-bond CLI chain, with each process's
  argument vector, exit code, stdout, stderr and redacted environment, and one
  `<time>-<label>/` directory per withdrawal step holding its
  `unsigned.json` and each owner's `witness-<role>.json`.
- `committee/`: the committee node's `committee.json`, one `NNN-<kind>.json`
  record per start, observation and stop, the node's
  `committee-<pid>.stdout.log` and `committee-<pid>.stderr.log`, its
  `availability-journal.sqlite` and its `chain-sync-cursor.json`.

When `MIDGARD_DA_BOND_JOURNEY_REPORT_PATH` is set, the test also writes the
rendered report there. Its adapter is `live-devnet`, and it names the run
directory, the deployment manifest, the network magic and the git head.

## Recovery and maturity

Exercise follower outages while the producer continues, then replay the retained
canonical intersection through subsequent live blocks. Native follower recovery
is a lower-layer gate; workflow recovery additionally needs proof initialization,
intermediate-step and lost-acknowledgement interruptions through the common
runner, followed by independent token, correction and economics checks.

Native node-to-client streaming must survive waiting for a block and consumer
backpressure. The pinned muxer's segment deadline starts when the first byte
arrives; incomplete headers and payloads remain bounded. Exact-point queries
retain their absolute operation deadline. Native failures after startup retain
the operation, helper identity and bounded stderr cause, while revoking the
failed helper's authority. The real-node regression pauses the stdout consumer
for 130 seconds, then checks ordered catch-up.

Each family's timing plan (`journey-timing.journey-timing-for-plan.ts`) budgets
staging, the honest successor and healthy processing from the action depth and
the devnet cadence; no plan counts proof transactions. The correction wait
(`verifyJourneyCorrection`) ends only when the chain holds the correction or the
watcher's own rule gives up on it, and the family's time limit
(`journeyFamilyDeadline`) does not run while it waits. The watcher's verdicts
come from the `l1Readiness` holds of `/v1/status`: a hold by which the watcher
gives up (`fault_proof_counterparty_deadline_passed`) fails the wait at once; a
pausing hold (funding, replay, quarantine or an L1 refusal) fails it once it
persists for 30 minutes (`JOURNEY_PAUSE_BOUND_MS`) with no transaction of ours
landing. A transaction of ours is any output at the prover's address created
since the fault's header. `validationTraceDispute` additionally waits out its
undefended case's defense deadline. These are finite test deadlines, not
finality guarantees.

Fault staging, the proof chain, the honest successor commitment, and healthy
processing advance on authenticated inclusion. The watcher reads its progress
from the chain and its node's mempool on every pass and remembers nothing it
sent.
Rollback invalidates cached authority; the worker re-observes current state
before retrying or rebuilding. The chain index supplies recovery state; no
per-family inverse workflow is required.

The harness persists `pending-evidence-stamp.json` before its inclusion-based
`result.json`. The shared watcher continues across families and restart. At each
family boundary the harness reads each pending request's correction from the
chain again (proof token, removal and slash) and stamps it once its own native
recorder authenticates it at the release depth (`l1Finality.confirmationDepth`): the recorder
must have replayed enough canonical blocks to establish the proof-token, removal,
and honest-successor depths. Only the last selected family waits for all
remaining stamps before stopping services. `finalized-evidence-stamp.json`
records the release policy digest, the correction the chain held, stamp time,
and independent native evidence path. A reauthenticated correction may have a
different inclusion point after rollback.
A completed proof can resume its unfinished honest-successor stage
against the authenticated current retained head when it has no prepared successor.
The original fault and removal evidence stay unchanged, and a separate
`successor-predecessor.json` records the resumed continuation. Existing prepared
or signed successor attempts keep their predecessor until reconciled.

All families run sequentially in one Vitest process, while anchor observation
overlaps subsequent proof execution. Report active execution and anchoring
latency separately. A provisional result is insufficient for finalized release
evidence until its stamp exists.

Test the authority transition through the actual follower callbacks: history
advance, authenticated queue removal, and the workflow's next read of the chain.
Removal may restrict authority to checking the exact landed transaction and
terminal result. It must not authorize another submission. A real rollback or
lost canonical authority still revokes it. Restart tests must pass through the
supervisor queue as well as the workflow runner; a queue job marked finished
does not establish that its correction landed.

A frozen standalone producer has the actual consensus forecast horizon derived
from `3*k/f` (129,600 one-second slots for this profile). Phase 4 refuses a restore
at or beyond that boundary before replacing durable state. This is distinct from
an ordinary follower outage on a continuing canonical chain. There is no
72-hour snapshot-reuse guarantee.
