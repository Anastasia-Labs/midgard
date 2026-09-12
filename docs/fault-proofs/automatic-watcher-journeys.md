# Automatic watcher journeys

Status: In progress.

The acceptance target is a fresh, isolated Cardano devnet with real Cardano
node, Kupo, Ogmios, native chain sync, public libp2p DA retrieval, and the normal
watcher launcher. The source catalogue supplies the family inventory. Each
family must be detected from a committed L1 header and public DA, complete its
installed workflow automatically, retain its permanent proof token, remove the
faulty header with the configured slash/reward, and classify a subsequent honest
commitment as healthy. A family counts only after these observations pass.

The existing `transitionTrace` journey supplies emulator evidence. Its retained
run took 2,896.92 seconds: deployment 19.6s, startup 57.9s, proof/correction
34.0s, and successor catch-up 2,760.1s. These measurements motivate profiling
catch-up; they do not establish the real devnet's performance.

## Work sequence

1. Provision an independent run directory, network magic, genesis, wallets and
   service ports using the repository's pinned Cardano/Kupo/Ogmios images.
2. Admit the custom chain explicitly, binding its network magic and slot clock
   to the actual genesis used by the native node. Retain normal transaction
   limits, local evaluation, 30-block finality and rollback checks.
3. Establish a real-node baseline for the existing journey. Capture stage
   durations and profiles before changing the measured expensive path.
4. Share deployment and fixture construction where their identities permit it;
   retain independent protocol state and evidence for each family. Use the
   production builders and existing canonical fault material, without mocked
   classifier decisions, proof results, chain receipts, DA responses or clocks.
5. Run all 53 non-interactive catalogue entries and repair defects reached by
   the real flow. The installed interactive `validationTraceDispute` family is
   outside this acceptance scope.
6. Retain a per-family result index with source revision/dirty diff, compiler,
   blueprint, deployment identity, timings, workflow journals and independent
   chain observations. Record failures and incomplete cases explicitly.

The run must not reuse or reset the checkout's persistent Preprod services.
Successful lower-layer or emulator tests do not count as this devnet acceptance.

A retained deployment also retains its catalogue and blueprint identity. When
concurrent work adds an uninstalled family, verify the retained run from an
isolated source tree with its recorded catalogue. Preserve the concurrent work
and record the isolated source hashes; do not relax deployment admission to
load incompatible receipts.

Before the watcher starts, a binding preflight runs the production configuration
loaders for every installed family against the finalized manifest and actual
reference UTxOs. Its application exposes readiness checks and resource cleanup
only. `workflow-binding-preflight.test.ts` runs this check independently of the
watcher launcher; its per-family receipts do not imply live proof completion.

Before launching the watcher, the runner uses its actual parsed configuration
and verified deployment identity to retrieve both staged payloads through the
production public DA runtime. Exact bytes and authenticated peer identities must
match. `public-da-preflight.json` records payload hashes, durations, and failed
requests. The standalone `retained-da-live.test.ts` uses this same preflight for
cheap diagnosis without restarting the watcher or submitting transactions.

The watcher application shares one public DA transport across classification
and workflow runtime leases. Each lease can cancel its own requests; the owner
bounds concurrent requests and queued work and closes after the proof supervisor.
This avoids creating a new libp2p connection for each classification, which can
exceed the retained DA server's connection admission rate. Server limits and
authenticated retention-absence checks remain enforced.
`retained-da-lifecycle-live.test.ts` checks this lifecycle with the actual runtime
configuration and retained payloads: 256 exact fetches across 128 leases,
including 330 seconds idle. A passing transport probe does not establish a
completed proof journey.

Public DA retrieval may repeat an identical request once on a fresh stream after
a recognized stream closure, remote reset or connection closure. Both attempts share the original
deadline, cancellation, peer and protocol identity. Partial responses are
discarded, and each complete response still passes normal authentication and
frame checks. Submission protocols and validation failures are never retried.
Set `MIDGARD_WATCHER_DA_CONTINUOUS_SECONDS=720` for the lifecycle test's bounded
continuous-retrieval variant; its evidence remains separate from proof journals.
Set `MIDGARD_WATCHER_DA_CONCURRENT_LEASES=8` to exercise the actual owner's full
request concurrency; the default remains two leases.

Both public DA endpoints serve the standard libp2p ping control protocol on
existing authenticated connections. Two bounded inbound control streams allow
heartbeat and asynchronous stream closure to coexist with the separate DA
request limit. The watcher still refuses inbound connections and exposes no
inbound DA handlers. Connection monitoring stays enabled: a zero inbound Yamux
stream budget resets heartbeats before negotiation and makes the monitor abort
otherwise healthy connections. The retained listener keeps its DA protocol
allowlist, aggregate admission bounds and outbound dialing restrictions.

The read-only `reference-acquisition-live.test.ts` checks every retained
reference output against its canonical bytes through the actual Kupo/Ogmios
configuration. Ogmios acquisition allows four physical sessions per endpoint;
closing connections retain their capacity until closure is observed.
`l1-events-live.test.ts` also captures the real trace event history across an
actual producer head advance. A typed Kupo head change discards the entire
unpublished snapshot and retries from a fresh release-final boundary, at most
three attempts. Partial reads never carry into another attempt. Explicit
canonical disagreement and other acquisition errors remain distinct failures.

`observation-lifecycle-live.test.ts` holds a real finalized observation for 330
seconds before resolving its block. It records native, Kupo and Ogmios transport
liveness separately, covering the connection age that short backfill tests miss.
It does not submit transactions or change the retained watcher database.
Local query authority renews its endpoint connections every 30 seconds while the
old connection and native authority are still live. Each service owns at most
one current connection and one renewal candidate; it waits for physical closure
before another renewal. Lost or revoked authority is never revived. A closed
transport and an endpoint mismatch have separate diagnostics.

`transaction-recovery-live.test.ts` uses the production signed-intent reader to
check a never-submitted expired transaction, a valid transaction absent from the
real mempool, and a recorded transaction already included on chain. It signs
with an independent plain-Ada publisher input but submits nothing. The recorded
inclusion case remains reusable after the workflow clears its pending funding
state. These source checks do not replace full workflow recovery acceptance.

The CLI emits structured startup phase records before its operations endpoint is
available. Pending records report elapsed time every 30 seconds; completed and
failed records retain stage duration. These records always report
`productionReady: false`; they describe startup activity, not a readiness result.
Journey stage timing also retains failures so a failed start does not disappear
from the timing record.

Availability failures and recovery emit bounded `availability_status` records
into the same CLI log. They retain the authenticated observation identity and
reconciliation duration, including failures caught internally while proof
processing continues. Repeated identical failures are deduplicated; these
diagnostics never assert whole-watcher readiness.

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
lasts five days. Acceptance retains its existing 30-block confirmation rule,
which now takes approximately ten minutes. This is an acceptance rule, separate
from the chain's consensus security parameter.

Changing timing requires a fresh run directory and chain. Stop the previous
watcher Compose project, generate the replacement, and deploy its protocol state
afresh; never reuse a previous chain's database or deployment receipts.

## Devnet cost models

The watcher devnet pins Cardano node 11.1.0 by image digest. This is a
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
stream, watcher journals, stage timings, and watcher CPU profile. A failed run
is stopped and retained, not overwritten with a different genesis.

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
planned workload determines authority expiry. Authority closure repeatedly checks
the canonical node tip and its indexed checkpoint; a local clock or an elapsed
timer alone cannot establish expiry. Report publication duration
separately from the subsequent wait for authority expiry and uniqueness checks.
On restart, reopen the same authority and signed journal; reconcile canonical
transactions before extending it. An expired or invalid parent requires all
recorded descendants to be resolved; it does not authorize a replacement
publication identity.

The September 11, 2026 isolated-node run published all 517 roles through 444
transactions (4,842,075 signed bytes) in 18m23.151s of recorded journal time,
including an interruption
with eight pending transactions and restart. The peak was eight unconfirmed
transactions and 98,502 signed bytes against bounds of eight and 100,000.
Every transaction confirmed; the journal retained its original signed records,
with no duplicated roles, reused inputs, spent reference outputs or unresolved
outcomes. Publication finished with 34m44.857s remaining in its authority
window. Independently, the first and last publications were included at slots
1590 and 2687: 1,097 one-second slots apart, with 2,085 slots remaining before
authority expiry. Canonical closure and all 517 unique retained roles were
verified at slot 4863, beyond expiry slot 4772. The run retains
`work/full-publication-evidence.json`, `work/publication-closure.json` and
`work/publication-transactions.ndjson` for independent inspection.

Separate small real-node probes cover a lost acknowledgement after node
acceptance, byte-bound backpressure, and expiry of an accepted parent with a
pending child. These publication results establish neither automatic watcher
journeys nor workflow interruption recovery. Initialization, the authority-expiry
wait and final deployment uniqueness verification remain distinct stages.

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
for 130 seconds, then checks ordered catch-up; the old helper failed this case
with a socket read timeout.

The deterministic single-deposit `transitionTrace` fixture has fifteen dependent
transactions, including its preimages, checkpoint folds, proof token and removal.
The fixture checks the real projected output against the audited plan: one
41-byte output, three scan primitives and two value primitives. Its timing helper
reads the retained genesis and verified deployment manifest, and checks the
runtime's authenticated finality depth. Thirty actual blocks at one-second slots
and `f=0.05` average ten minutes per confirmation gate; thirty minutes for the
whole proof and two hours for the whole journey are insufficient.

The trace harness allows twice the expected confirmation time, plus explicit
transaction preparation, RPC, startup and successor allowances. This gives
337.5 minutes for correction and 415.5 minutes for the complete retained journey
under the captured configuration. These are finite test deadlines, not finality
guarantees. They do not change transaction validity, consensus or confirmation
depth. Other family timing plans remain separate work. Poll deadlines use a
monotonic clock, and a resumed attempt preserves its entire pre-launch journal
prefix while treating newly recorded failures as failures of the current attempt.

Test the authority transition through the actual follower callbacks: history
advance, authenticated queue removal, and subsequent workflow reconciliation.
Removal may restrict authority to checking the exact recorded transaction and
terminal result. It must not authorize another submission. A real rollback or
lost canonical authority still revokes reconciliation. Restart tests must pass
through the supervisor queue as well as the workflow runner; a queue job marked
finished does not establish that its transaction journal completed.

Preparation and completion each cross a SQLite reservation and an immutable
directory journal. Crash tests must close and reopen both stores between those
writes. The durable preparation handoff retains the exact signed transaction,
action and expected journal prefix. The completion handoff retains the verified
terminal result with the funding release, so recovery cannot reopen released
inputs. An expiry/absence result needs the same interruption analysis before
discarding pending transaction context.

The real-node `transaction-recovery-live.test.ts` gate reads a never-submitted
expired transaction, valid transactions with and without expiry absent from the
node's mempool, and an already included recorded workflow transaction. It retains
the exact signed transaction for rechecking after workflow completion and uses the runtime's admitted
source and signs diagnostic intents without submitting them. This establishes
the adapter's expiry and rebroadcast eligibility decisions; actual rebroadcast,
crash recovery and complete proof evidence remain separate gates.

A frozen standalone producer has the actual consensus forecast horizon derived
from `3*k/f` (129,600 one-second slots for this profile). Phase 4 refuses a restore
at or beyond that boundary before replacing durable state. This is distinct from
an ordinary follower outage on a continuing canonical chain. There is no
72-hour snapshot-reuse guarantee.

The `crossBlockDuplicateEvent` journey requires a real prior settlement. Stage
its honest deposit history early by adopting the independently verified
`transitionTrace` successor and its exact genuine deposit. The preparation gate
checks the completed trace result, native commitment, retained payload, deposit
role and inclusion interval before recording the source header's end time plus
seven days. Run `history-preparation.test.ts` with
`MIDGARD_WATCHER_JOURNEY_PREPARE_HISTORY=1` immediately after the trace passes.
Resume the existing SDK merge path when that actual maturity has elapsed and a
later healthy queue head exists.
Keep this family time-gated until the settlement exists; a prior queued header
or a fabricated settlement cannot satisfy it. Other families can advance the
same queue while that prerequisite matures.
