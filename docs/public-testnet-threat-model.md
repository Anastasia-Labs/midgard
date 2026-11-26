# Public testnet threat model

This describes the intended stage-0 public
Preprod deployment; it does not establish deployment or acceptance completion.
The public profile and authenticated release manifest determine an instance.
See [SECURITY.md](../SECURITY.md) for private vulnerability reporting.

## System, assets and deployment boundary

Midgard is an optimistic rollup on Cardano. The first public testnet is the stage-0
Preprod deployment of the
[launch model](public_testnet_readiness.md#the-launch-model), with curated
operators and permissionless fraud provers and DA challengers. The proposed release profile is
`preprod-public`; an emulator or testing profile cannot establish public release
readiness. An instance is identified by its approved genesis, authenticated
manifest, profile digest, pinned blueprint and deployed script/reference UTxOs.

Assets include L1 deposits and withdrawals, the L2 ledger and roots, operator
bonds and proof rewards, DA bonds, retained payloads, canonical follower history,
intent and settlement journals, signing keys, API/admin credentials, and service
availability. Preprod is a test network; corrupted state, credential disclosure and
honest-operator conviction remain meaningful.

The covered boundary includes Aiken validators, SDK and Lucid builders, native
ledger and follower processes, node and workers, committee transport/storage,
watcher and proof tooling, operator CLI, public/admin HTTP, deployment and
release tooling. Reference-only code and third-party services are not claimed
to be deployed testnet controls. A vulnerability in their integration or dependency
use is still reportable; no blanket dependency or trusted-role exclusion applies.

## Roles and assumptions

| Role or source                    | Authority and trust                                                    | Inputs remain untrusted / required boundary                                                                                                                                        |
| --------------------------------- | ---------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Permissionless user               | Own transaction signatures and owned funds                             | Transaction CBOR, witnesses, datums, scripts, event shape, payload size and timing are adversarial; bounded total admission and truthful verdicts are required.                    |
| Curated operator                  | Production liveness and verdicts outside enabled proof coverage        | Operator membership does not authenticate arbitrary roots, deposits, forced inputs, replay evidence or another deployment. Enabled proof and value-conservation rules still apply. |
| Permissionless fraud prover       | Any key; every watcher submits the proofs it finds                     | Only authenticated, correctly parameterized proof stages can slash or remove. An honest block must survive adversarial claims from any key.                                        |
| Permissionless DA challenger      | Any key that funds the challenger bond opens an availability challenge | The bond funds the committee's publication; signature, deployment, deadline, bond and subject rules apply.                                                                         |
| DA committee                      | Publishes and retains the exact attested payload, and responds in time | An attestation is availability evidence, not ledger validity. Payload commitments, signer/threshold binding, peer identity, quotas and retention deadlines must be checked.        |
| Node/watcher/follower processes   | Execute configured operations using durable local state                | Cross-process messages and native answers require identity/version/schema checks; crash/restart must preserve obligations and rollbacks must invalidate orphan evidence.           |
| L1 Cardano chain                  | Canonical ledger and finality under the selected genesis parameters    | A provider response is an observation. Provider identity, canonicality, tip freshness and configured confirmation rules must hold before dependent actions.                        |
| External content/provider service | Retrieves candidate bytes and answers requests                         | Timeouts, truncation, malformed values, conflicting tips, rate limits and wrong-chain data cannot authorize changes; retrieved content needs independent commitment checks.        |
| Administrator                     | Configures ingress, secrets, storage and operator procedures           | Admin actions require the applicable authentication and attribution; admin possession is not permission to bypass validators or reset durable obligations.                         |
| Release authority                 | Approves the release manifest, revision, image and profile             | Release signatures do not replace independent rebuilds or script/hash/profile checks. A changed key/profile creates a new deployment identity.                                     |

Distinct public key hashes do not prove independent human custody, hardware
isolation, quorum, funding or operational availability. Release readiness must
establish custody, access policy, backup/recovery and incident ownership.

## Security properties

1. Value and authenticated ledger roots are conserved by each supported state
   transition. Each event, verdict, proof and successor binds the correct
   deployment, source, root, ordering and stage.
2. Every supported user input has bounded parsing/evaluation and a truthful
   admitted or refused result. It cannot indefinitely halt block production,
   wedge recovery, or convict/eject an honest operator.
3. Only a valid proof of the authenticated fault may remove a block or slash a
   bond. Both a real faulty control and an honest adversarial control are needed
   in validation evidence.
4. The operator allow-list applies at registration. Computation-thread Init and
   DA challenge Open are permissionless. No alternate builder, catalogue path or
   administrative interface may bypass these entry points.
5. DA acceptance binds exact payload bytes to the header. Read failure is not a
   negative verdict, and an attestation does not stand in for execution evidence.
6. Durable replay/intent/settlement state survives restart and canonical rollback
   without fabricating, losing, duplicating or prematurely resolving obligations.
7. Missing identity, stale evidence, schema mismatch or unavailable authenticated
   material holds the dependent action visibly rather than using a permissive
   fallback. Unrelated eligible work should continue where the protocol permits.
8. Secrets and private transaction material are not leaked through logs, errors,
   diagnostics, caches, container images or public response bodies.

## Runtime, APIs and operational DoS

Public HTTP includes transaction submission and evaluation, transaction/UTxO and
address history reads and block/protocol/status endpoints.
Admin/operational interfaces and committee/watcher diagnostic APIs form distinct
boundaries. Required public deployment controls are TLS ingress, protected
internal infrastructure, bounded bodies/queries/pages, request and byte quotas,
slow-request deadlines, bounded concurrency/queues and redacted errors/logs.
These are release requirements; this document does not establish that a
candidate deployment meets each of them.

Submission currently reserves count and byte-weighted ingress permits and checks
bounded bodies, envelope/media type and durable backlog. `/tx/evaluate` shares
the bounded body reader and limits concurrency relative to the validation pool;
it returns advisory budgets and must not consume the capacity needed for
admission. Every route class has a per-client request-rate limit; reads, batch
status and evaluation also have a global limit and submission a per-/24 or /48
limit, and a public profile refuses to start with rate limits disabled. These bounds do not establish proxy
limits, slow-client handling or load-test throughput.

Read amplification can arise from address scans, large outref batches, repeated
missing-object lookups, full payload retrieval, predecessor chains, log/status
queries and provider fanout. Enforce cardinality and response-byte bounds before
work starts, paginate deterministically, cap recursive depth/parallel requests,
verify bytes once per authenticated operation, and bound caches. A small request
must not cause unbounded DB, network, hash or replay work.

Spam and mempool churn include repeated valid submissions, duplicates, conflicting
spends, invalid descendants, rejected envelopes and expensive CEK evaluation.
Durable admission accounting, deduplication, validation pool fairness, bounded
mempool/backlog and deterministic reject/rebase behavior must keep forced/L1
obligations progressing. Advisory evaluation cannot imply admission or execution.
Operational DoS includes disk exhaustion, failed writes, unbounded journals,
worker crash loops, stale locks, resource-starved signing, dependency outages,
provider throttling and repeated rollback. Each needs a bounded stop/hold,
readiness reason and a recovery/runbook; tests must include restart and load.

## L1, provider and incident assumptions

Cardano finality is probabilistic. The configured genesis, slot map, chain-growth
and depth assumptions bound which events are eligible. A rollback beyond the
selected confirmation model, sustained chain failure, compromised local chain
source or unanimous dishonest committee can exceed the advertised protection.
These assumptions do not excuse an implementation accepting a wrong genesis,
wrong deployment, stale/conflicting tip, unauthenticated response or orphan fact.

External providers can drop, rate-limit, reorder, replay or falsify answers.
Authenticate inclusion and identity on the actual route/session; content must
hash to the expected transaction or DA commitment. Treat conflicting providers
as visible uncertainty rather than inventing canonicality. Deadline/retry policy
must not continually renew the same unavailable obligation or stall independent
work. Provider availability affects liveness; it must never redefine validity.

Signing/admin/release compromise can censor, withhold data, issue permitted
commands or approve malicious software according to the compromised role. Report
unexpected effects even if a trusted role is required. Deployment operations
must establish key custody and incident handling. Restore procedures must
preserve identity and durable obligations;
changing the fixed operator allow-list requires another deployment, not a
runtime edit.

A suspected active compromise should trigger private security contact, preservation
of revision/profile/manifest/log/chain evidence, isolation of compromised runtime
credentials, suspension of the affected operation and a reviewed recovery plan.
No universal protocol pause or unilateral on-chain reversal is assumed. Retained
DA must not be deleted and durable stores must not be reset during triage. The
node and watcher ship alert rules with runbooks; each deployment must still
establish alert delivery, incident ownership and drills.

## Stage-0 limitations

- Curated operators, fixed for the testnet's life; permissionless fraud provers and DA
  challengers. Watchers submit the proofs they find automatically. A watcher
  that reaches a proof after its latest safe start still runs it until the
  block matures, and raises a late-start alert; at maturity it holds the
  objective visibly, and no command at this release re-drives it.
- Native-only forced transactions. No reference-script outputs, Plutus witnesses,
  redeemers, auxiliary data or L2 validity upper bound; canonical and noncanonical inline datums
  remain permitted at L1 admission. Funds needing Plutus or
  a native script expiry branch have no forced exit on this testnet.
- DA payloads carry no validation-trace states. The nine trace-relative
  families prove their faults directly, and the public profile enables all
  nine; it excludes no category. Plutus verdicts are
  challengeable through the interactive dispute game (`validationTraceDispute`)
  for every execution step that fits in a single L1 transaction. Faults in
  excluded categories, wrong verdicts that depend on a step that does not fit,
  fabricated or substituted forced transactions (which can censor a valid one)
  and the other
  [open fault-proof gaps](fault-proofs/remaining-gaps.md) retain
  curated-operator trust. A wrongful forced rejection is challengeable under
  every rejection reason.
- Strikes are permissionless and rotate operator shifts. They can still rotate
  shifts during catch-up and slow its progress. The public ejection threshold
  is set out of reach, so strikes do not provide an operator ejection mechanism.
- A new node or watcher takes the ledger at the confirmed state from a node
  of the deployment, checked against the UTxO root on L1, and replays the
  blocks above it from retained payloads. It needs one node of the deployment
  to serve that ledger and the payload history above it to be available. The
  public profile configures 120-day payload retention; its authenticated
  manifest and release acceptance establish the actual instance's
  configuration and retrieval behavior.
- The public profile uses seven-day block maturity. Event processing and maturity
  mean an L1 withdrawal request is not an immediate payout. An unsigned withdrawal
  order can block an output reference (issue #819).
- Rejections committed for forced transactions carry typed rejection reasons;
  the node's transaction status API reports its own diagnostic rejection codes.
- Escape hatch and settlement claims are unsupported. Split cadence, hourly
  commitments, further DA trim, permissionless operators and full proof
  coverage come with the regenesis.
- The regenesis must be separately announced; a future date is a plan, not a
  guarantee that this candidate is deployed or complete.

Documented limits are not blanket finding exclusions. New bypasses, unexpected
honest conviction, loss/freeze, user/environment-triggerable liveness failures,
unbounded work or secret disclosure remain reportable. Disabled families do
not create a claim that every invalid state can be proven. A finding in code kept
for the regenesis should identify current reachability and future relevance separately.
