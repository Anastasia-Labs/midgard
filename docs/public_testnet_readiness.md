# Public Testnet Readiness

Status: no-go. Reviewed 2026-11-05 against the integration branch at
`2ad2f8418`.

The first public testnet, P1, is a stage-0 network on Cardano Preprod, run from
the `preprod-public` deployment profile. Its launch date follows its scope: the
full P1 scope ships and the dates move (owner, 2026-10-25). A regenesis, P2, is
planned for December 2026 and lifts the P1 limits listed below.

This checklist owns the launch decision. It lists the implementation work that
must merge before launch and the release gates that need evidence from the
release revision. Proof coverage is tracked in the
[fault-proof catalogue](fault-proofs/catalogue-status.md),
[coverage matrix](fault-proofs/coverage-matrix.md) and
[testing status](fault-proofs/testing-status.md). Tags such as A1 or R6 are the
release plan's ticket ids.

Each item carries one status:

- **Done**: merged into the integration branch; the pointer names the merge or
  commit. Done code still needs the evidence under
  [Release verification](#release-verification).
- **In progress**: part of the work is merged; the entry names what remains.
- **Open**: no implementation or evidence yet.
- **Unverified**: this review found no evidence either way.

Close a release gate only with evidence from the release revision: owner or
reviewer, exact command and environment, artifact location, result and known
limitations. Focused tests, local emulator runs and runs on an earlier revision
do not close a gate.

## The P1 model

P1 launches the way other optimistic rollups launched their first public
networks: curated block producers, permissionless challengers and a planned
regenesis.

| Role                    | P1             | Enforcement                                                                             |
| ----------------------- | -------------- | --------------------------------------------------------------------------------------- |
| Operators               | Curated        | Operator allow-list, checked at registration                                            |
| Fault-proof provers     | Permissionless | Any signing key initializes a proof's computation thread                                |
| DA availability callers | Permissionless | Any key that posts the challenger bond opens an availability challenge                  |
| Users                   | Permissionless | L2 transactions (including Plutus), deposits, withdrawals and native-only forced orders |

P1 has these limits, and the security claim (R1) must state each one:

- Forced transactions are native-only. They carry no redeemers, no non-native
  witness scripts, no reference scripts on outputs, no auxiliary data and no L2
  validity upper bound. Inline datums are allowed. Funds at a Plutus script, or
  reachable only through a native script's expiry branch, have no forced exit
  in P1; they move through normal L2 transactions.
- The L1 order of a forced transaction carries a closed validity range of at
  most 10 minutes (A2).
- DA payloads carry neither validation-trace states nor the UTxO set. The
  nine fault-proof families that read those states are rebuilt without them
  and all in the P1 catalogue; `excluded_fraud_proof_categories` in
  [`preprod-public.yaml`](../config/deployments/preprod-public.yaml) is empty.
  The Plutus execution dispute (`validationTraceDispute`) is in the catalogue
  too (owner, 2026-10-21); `mainnet` still excludes it.
- Every party derives ledger state by replaying blocks from genesis. Payloads
  are retained for 120 days, and P1 has no state snapshot.
- Inactivity strikes skip an operator's shift but never eject the operator.
- Every watcher submits the proofs it finds with its own key.
- The operator allow-list is fixed for P1's life. A new operator, or an
  operator's new key, waits for P2.
- The escape hatch and settlement claims are not supported.
- L2 Plutus budgets follow Cardano's protocol-version-11 rules with the pinned
  PlutusV3 cost model. The node's `POST /tx/evaluate` returns the budget a
  transaction needs.
- The Plutus dispute game proves every execution step that fits in one L1
  transaction; a step that does not fit is documented as unprovable.

## Schedule

The `preprod-public` allow-list keys are set (`d07f2fda7`). The remaining steps
run in this order. They carry no dates.

| Step | Milestone                                                                                                                                               | Status      |
| ---- | ------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------- |
| 1    | Operator-2 joins a fresh fast-timer `preprod-testing` twin before its first block and shares block production with operator-1                           | In progress |
| 2    | Remaining fixes from this checklist, then the validator freeze, one golden regeneration, and fit rows and emulator lifecycles over the frozen blueprint | In progress |
| 3    | Rehearsal funding                                                                                                                                       | Open        |
| 4    | Full rehearsal on the `preprod-rehearsal` profile with every drill (R6)                                                                                 | Open        |
| 5    | Go/no-go (owner), around day 5 of the rehearsal                                                                                                         | Open        |
| 6    | Fresh `preprod-public` deployment and its unattended run (R6)                                                                                           | Open        |
| 7    | Tag (R11) and SDK publish (R10) (owner)                                                                                                                 | Open        |

A contract change after the freeze needs an owner decision. Nothing is cut from
P1 to hold a date.

## Implementation before launch

### Contracts

Every contract item merges before the validator freeze.

| Item                                                               | Status | Pointer and remaining work                                                                                                                                                                                                                      |
| ------------------------------------------------------------------ | ------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Forced-transaction admission at the tx-order mint (A1)             | Done   | Merge `b06aca902`. `forced_tx_is_admissible_v1` in `onchain/aiken/lib/midgard/user-events/tx-order-v1.ak`; the SDK and node share `screenMidgardForcedTxAdmission` and a golden corpus with Aiken                                               |
| Closed 10-minute validity range on tx orders (A2)                  | Done   | `faee6e018`. The tx-order mint requires a closed L1 validity range of at most `max_event_validity_range_ms` (10 minutes on every profile)                                                                                                       |
| Operator allow-list, P1 catalogue and P1 parameters (C1)           | Done   | Merges `63e24132e`, `bf82caf27`; keys `d07f2fda7`. Curated emulator tests in both polarities. Provers and DA challengers are permissionless (PL)                                                                                                |
| Long-gap catch-up after an all-operator outage (C2)                | Done   | Merges `19faf4948`, `43cc2c76a` (commit time in the state-queue node), `fa0435b97` (retention clocks) and `db675593c` (fixed commit clocks); the catch-up planner builds frames from authenticated clocks (`a1e6619fe`). The outage drill is R6 |
| No empty blocks (C3)                                               | Done   | `39cde8b6d`, merged in `43cc2c76a`; the block planner commits nothing when no event is due (`a1e6619fe`)                                                                                                                                        |
| Asset names compare as plain bytes (C5)                            | Done   | `51ec22412`, `113fa9369`                                                                                                                                                                                                                        |
| Datum canonicity fault proof (DC)                                  | Done   | Merges `78943b3c9`, `a81655e75`                                                                                                                                                                                                                 |
| Minimum-ADA and network-id routes for accepted forced transactions | Done   | Merge `5bdcafbec`: both families read committed output bytes and route accepted forced transactions through the watcher workflow                                                                                                                |

### Node, SDK, committee and watcher

| Item                                                                                         | Status | Pointer and remaining work                                                                                                                                                                                                                                                             |
| -------------------------------------------------------------------------------------------- | ------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Placeholder validation descriptors; no trace states in DA (B1)                               | Done   | Merge `e065e47d7`; `026761a97` pins the refusal of outputs with no descriptor; merge `12b0b5350` removes the retained-witness readers from the installed proof scope                                                                                                                   |
| Every admitted forced transaction gets a verdict (B2)                                        | Done   | Merge `e23b8a456`: every mint-admitted forced order gets a forced verdict, checked over signed orders with changed fee and validity                                                                                                                                                    |
| Normal L2 Plutus (P)                                                                         | Done   | Merges `1fa289e40`, `eac653de0` and `7a9c84a87`: verdict mode on admission and the commit path, per-transaction cap, per-block budget, verdicts on the worker pool, `POST /tx/evaluate`, the Schnorr builtin and `evaluate()` in lucid-midgard. Per-IP limits on `/tx/evaluate` are R7 |
| Evict normal transactions that cannot be committed (B2-8)                                    | Done   | Merge `830fd20cb`                                                                                                                                                                                                                                                                      |
| Deposit datum decoding in the L1 follower (B3)                                               | Done   | `3c6af325f`, merged in `3654a21a5`                                                                                                                                                                                                                                                     |
| Withdrawal payability and settlement retries (B4, #820, #821)                                | Done   | Merges `a97f50266`, `3654a21a5`: the prover sizes the payout from the committed bytes (`1c93ed3e8`), and a failing status read backs off (`64bd913cc`)                                                                                                                                 |
| DA payloads carry no UTxO set (DS)                                                           | Done   | Payload and producers (`bb888bb2a`); fault-proof ledgers by replay (`f49a882cc`); shared node and watcher replay and the watcher's replayed ledger (`5190e0235`); stack replayed-ledger journeys in node CI (`f15f327b6`); 120-day retention on the public profile (`91538258f`)       |
| Unattended-liveness musts (B6, #696)                                                         | Done   | Merges `66cb1b6dc`, `aa18a9264`. The final unattended run is R6                                                                                                                                                                                                                        |
| The operator watchdog holds takeovers while the queue is progressing or cannot progress (B7) | Done   | `ff9dde981`, merged in `3654a21a5`                                                                                                                                                                                                                                                     |
| L1 reads for deployment, operator and user commands (owner, 2026-11-03 and 2026-11-04)       | Done   | Deployment commands read their own outputs and the wallet straight from the Cardano node (`3fe2bcc97`); operator commands read the running node's index (`4fe49bfb7`); deposits and withdrawals go through `midgard-user` (`8242971b4`)                                                |

## Release verification

### Security claim and threat model (R1, R2)

- **Security claim.** In progress. Publish one stage-0 claim in the
  announcement and the SDK, operator and challenger docs: the P1 model and
  every limit above, the enabled fault-proof families, challenge windows, trust
  assumptions, and the P2 sunset and regenesis notice, stating what P1
  disables (owner, 2026-10-12, two-stage launch). The
  [claim](public-testnet-security-claim.md) is in the tree (`b5d5ea700`).
- **Curated operators.** In progress. The allow-list is enforced on-chain at
  registration and off-chain by the SDK and node (C1); proof threads and
  availability challenges are permissionless (PL). The claim must still
  document the operator-halt and fund-recovery assumptions.
- **Unsupported scope.** In progress. The security claim marks the escape
  hatch and settlement claims unsupported; the manifest is Unverified.
- **Published catalogue.** In progress. `preprod-public` enables every
  category and excludes none; computation-thread initialization refuses a
  category the catalogue does not hold (C1). The claim and the manifest must
  list the enabled families.
- **Threat model and disclosure policy.** Done (`b5d5ea700`). The
  [threat model](public-testnet-threat-model.md) covers each role, runtime,
  APIs, key, admin and L1 provider compromise, spam, denial of service and
  incidents; [`SECURITY.md`](../SECURITY.md) gives the reporting channel,
  scope, out-of-scope list and emergency contact.

### Contract release and deployment identity (R3, R4, R5)

- **Reproducible blueprint.** Open. Rebuild the release blueprint with the fork
  compiler pinned by `aiken.toml`; CI's rebuild must equal it. Record the source
  revision, compiler, flags, `aiken.lock`, blueprint, script hashes, protocol
  parameters and rule bundle.
- **Signed manifest.** In progress. Publish a manifest with script hashes and
  parameters, signed by one release key, and say how anyone verifies it. The
  stack signs the release for the deployment it finalized and checks it as
  public listen does (`3a1cbf5d5`, `392964f03`,
  [deployment-release-tooling.md](deployment-release-tooling.md)). No public
  release is signed yet.
- **Script registry.** In progress. The deploy tooling classifies every
  validator as deployed, excluded or internal and fails on an unclassified one
  (`56f329a57`). Unverified: every enabled proof family has its full step chain
  and parameter links.
- **Reference-script records.** Unverified. Every required reference script
  has a non-null record from the deploy tooling: address, outref, script hash,
  CBOR hash, lovelace and publisher transaction.
- **Parameter review.** In progress.
  [Public-testnet parameters](public-testnet-parameters.md) lists every
  `preprod-public` value with its unit, profile line and rationale, checked in
  CI, plus the values set outside the profile. Release tooling must refuse
  placeholder keys and unexplained zero economics.
  - Settled: the 10-minute event validity range (A2, `faee6e018`); 120-day
    payload retention (`91538258f`); the public operator and DA bond figures
    (owner, 2026-10-22); no excluded category; DA slash grace 2 d and pool
    floor 5 ADA (owner, 2026-10-28); the unused dispute response window
    removed (`67f390618`); a 2,500 ADA rehearsal challenger bond (owner,
    2026-10-31, `21b21cb07`).
  - Open: the block ex-units budget, which defaults to 33,000,000 memory and
    20,000,000,000 steps (`demo/midgard-node/src/services/config.block-ex-units-budget.ts`)
    and is to be re-measured on an idle machine.
  - The `mainnet` profile still carries a placeholder operator key. It is not
    a P1 target.
- **Pooled DA bond.** Done ([decision record](midgard/decisions/da-committee-bond-pool.md),
  #686–#691). An attestation applies only while one bonded pool holds at least
  one DA bond; a timeout slashes the pool, and a top-up after a slash is manual
  (owner, 2026-11-04, `964ce21b9`).
- **Store identity.** In progress. The node's database binds to its
  deployment before any startup write (`e3ebd17ee`); the L1 follower store
  refuses another origin. Open: bind every Postgres, MPF and DA store with a
  marker holding the network, one-shot outref, policy ids, manifest hash and
  schema version, failing before serving on a mismatch. Wiping local state
  requires a fresh on-chain deployment.
- **Fee and collateral.** Open. Audit fee and collateral selection across
  protocol transactions: pure-ADA inputs owned by the intended wallet, above
  configured minimums, never protocol or reference-script UTxOs. Fix only what
  the journey trips on.
- **Canonical admission.** Open. Verify canonical native-CBOR admission,
  locally derived transaction ids and durable payload hashes. Same-id,
  different-bytes submissions fail on every insert and replay path.

### Public runtime (R7)

- **Public profile.** In progress. Expose only intended ingress, through TLS.
  Keep Postgres, the observability stack, the Cardano node, and Kupo and Ogmios
  where tooling runs them, internal or authenticated. Disable anonymous Grafana
  admin. Remove Docker socket and host filesystem mounts. Verify the rendered
  port bindings. The public stack keeps metrics, DA ports and Postgres on the
  loopback and serves HTTPS from digest-pinned images (`8f6c8f948`).
- **Fail before binding.** In progress. Refuse to bind HTTP on missing public
  settings, blank admin keys, demo seeds, default database credentials, mutable
  images, exposed internal services, missing identity or placeholder economics.
  Define image policy, sizing and retention capacity. The public stack checks
  every public input before it spends (`8f6c8f948`). The owner-set production
  hardware floor from the
  [economics decision](midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md)
  is accepted and carried here verbatim:

  | Role                                         | Floor                                                                                                             |
  | -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
  | midgard-node (operator)                      | ≥ 32 GiB RAM, ≥ 16 vCPU (2026 gaming-PC class), NVMe storage                                                      |
  | DA committee node, midgard-watcher, Postgres | sized from C74/C86 measured usage plus ≥ 2× headroom; the §5.1 ceilings are containment caps, not recommendations |

  C86 bounded-stress results refine the non-node role sizing but cannot lower
  the node floor.

- **Secrets.** In progress. Use mounted secret files and prove that startup
  errors and logs redact them; node, Postgres and journey secrets now come from
  private files (`3a1cbf5d5`, `48020abb8`). Separate operator,
  reference-script, admin and release keys. Never take seeds from arguments.
- **Action map.** Open. Classify every route and command as public, operator
  only, admin or internal, and test the route graph. Protect `/init`,
  `/commit`, `/merge`, `/stateQueue`, `/logBlocksDB` and `/logGlobals`.
- **Request limits.** In progress. Bound request bodies, content types, per-IP
  and global rates, and pagination at the proxy and in the app, including
  `/tx/evaluate`, and prove the bounds with a load test. The app limits every
  route per client (an IPv4 address or IPv6 /56) in fixed-memory hashed
  buckets, `/submit` per transaction and per /24 or /48 as well, and the other
  public routes globally, with stricter classes for `/tx/evaluate` and batch
  status and separate health and admin classes (`9b6994157`, `10fcf75c8`,
  #880); the load test is open.
- **Log redaction.** Open. Public routes never log raw CBOR or request bodies.
  Test redaction of seeds, keys, credentials, transaction CBOR and addresses.

### Fresh deployment, journeys and drills (R6)

These run first in a full rehearsal on `preprod-rehearsal` (`preprod-public`
with testing economics, `7ac572868`), then on the fresh `preprod-public`
deployment (owner, 2026-10-29). Status: Open. So far only the fast-timer
`preprod-testing` twin (`4e5cb615f`) has run: a full wallet journey with
automatic merge and finality, three cycles on 2026-11-04. That covers the
wallet steps of the clean acceptance sequence and a fast-timer payout, not
onboarding on the release revision, restarts, the journey, drills, crash
boundaries, the unattended run, history readiness or a seven-day payout.

- **Clean acceptance sequence.** One command sequence from a fresh on-chain
  deployment and empty local state: operator onboarding, deposit, L2 transfers,
  commit, confirmation, merge, withdrawal, reserve and payout, and restart.
  Verify database, chain and API state without manual edits, and retain
  transaction hashes, queue state, balances, readiness and logs.
- **Journey.** One forced order of each admitted shape, plus refused shapes;
  deposits; normal L2 transactions including Plutus; a strike drill; one live
  fault-proof family run with manual submission. Use the
  [challenger runbook](fault-proofs/challenger-runbook.md), the
  [manual recovery runbook](fault-proofs/manual-recovery-runbook.md) and the
  [automatic watcher journeys](fault-proofs/automatic-watcher-journeys.md)
  harness.
- **Payout.** A live withdrawal pays out after the seven-day maturity, first
  on the rehearsal.
- **Drills.** Restart Cardano; lose public retained-DA retrieval; stop every
  operator for at least one hour, then let them catch up (C2). Readiness is
  held and then recovers, with no manual commit, merge or store reset. Fault
  drills run on a subset of families, then all, with operator-2 faulty. Record
  every actor's durable state and pending signed attempts across recovery. No
  role reads Kupo or Ogmios (#814), so there are no Kupo or Ogmios drills.
- **Crash and restart boundaries.** Restart after projection, admission,
  submission, confirmation, merge and payout. Cover a validation lease restart
  after rejections persist but before acceptances; commit, confirmation and
  merge crash boundaries with persistent database and MPF state; a submitted
  header recovered before its local marker; merge confirmation recovered before
  local job start. No duplicate blocks, lost mempool transactions or
  divergence.
- **Unattended run.** Complete the unattended lifecycle program (#696) on the
  rehearsal and on the fresh deployment. The availability
  response budget is checked against both testing and both public profiles by
  `demo/da-committee-node/tests/availability-response-budget.test.ts`.
- **History readiness.** Verify readiness against the full configured recovery
  window and the follower's chain. A follower intervention
  (`rollback_beyond_k`, `intersection_outside_history`, `store_integrity`,
  `origin_mismatch`) is cleared only by the operator's explicit
  `midgard-l1-follower reset --to-origin` and a replay from the origin; nothing
  clears it on a timer.

These capabilities are built; R6 supplies their release evidence:

- Forced submissions use the
  [immutable-submission format](midgard/decisions/forced-inclusion-submission-verdict.md),
  and the tx-order mint enforces the P1 admission rules (A1).
- `/healthz` reports liveness and `/readyz` readiness, with named hold reasons.
- Each role (node, watcher, DA committee) reads L1 only through one ChainSync
  follower on its operator's own Cardano node. A rollback rewinds the
  follower's facts and every role recomputes; one deeper than k holds
  readiness with the process up.
- Deposit, withdrawal and commit observations are anchored to follower chain
  points; L2 validity ranges use POSIX milliseconds from chain time.
- Withdrawal payability uses the on-chain payout rule (#820).
- There is no operator rekey: an operator retires and a new key registers
  ([operator lifecycle](../docs-site/content/docs/operators/node/operator-lifecycle.mdx)),
  which in P1 waits for P2.
- Each block header is bound to its DA commitment, which the DA committee
  attests and serves.

### Alerts, runbooks and documentation (R8, R9)

- **Alerts.** In progress. Page on commit lag, catch-up mode, a watcher
  replay-root mismatch, forced-admission divergence, follower refusals, stuck
  settlements, and expose readiness, worker staleness, admission depth,
  provider lag, database saturation, disk and payout state. Node and watcher
  alert rules ship (`700811aca`, `cf9303980`); alert delivery is open.
- **Runbooks.** In progress. The [operational runbooks](../docs-site/content/docs/operators/node/operational-runbooks.mdx)
  cover commit lag, catch-up, replay-root mismatch, forced-admission divergence,
  follower refusals, stuck settlements and proof deadlines, linked from each
  alert rule (`0b29bce56`). Recovery drills and release validation are open.
- **User and operator docs.** In progress. The docs describe
  [deposit/withdrawal milestones](../docs-site/content/docs/operators/node/deposits-withdrawals.mdx),
  [forced-transaction limits and the missing L2 upper bound](../docs-site/content/docs/sdk/midgard-sdk/user-events.mdx),
  [`evaluate()`](../docs-site/content/docs/sdk/lucid-midgard/validation-simulation.mdx),
  deposits and withdrawals through `midgard-user` (`a5e30ef78`), the
  [curated lifecycle](../docs-site/content/docs/operators/node/operator-lifecycle.mdx)
  and [transaction status transitions](../docs-site/content/docs/sdk/lucid-midgard/submission-observability.mdx).
  Check them against the final deployed revision and complete the release
  journeys before closing R9.

### SDK and release artifacts (R10, R11)

- **SDK exports.** In progress. Audit the public exports. Test the packed
  tarball's ESM import, CommonJS require and NodeNext types against a public
  API gate. The packages pack and install alone (`7c3eafb98`, `cd456dc6b`).
- **Invalid UTxOs.** Done (`33e5a1f8e`). The SDK fails on invalid UTxOs or
  partial wallet state instead of discarding data.
- **Versioned publish.** Open. Publish versioned packages from CI. Reject
  `latest`, local file dependencies and unresolved workspace specifiers.
- **Frozen build.** Open. Run frozen install, builds, typechecks and tests for
  every package, plus the pinned Aiken checks, on the tagged release
  candidate. Retain exact commands and results.
- **Tag and digests.** Open. Build public artifacts from a tagged revision with
  immutable SHAs and digests for actions, images and release inputs. Release
  CI fails on mutable selections; every workflow still runs on `ubuntu-latest`.

### Not yet assigned

These requirements have no release ticket yet. Status: Open.

- Backlog limits are concurrency-safe with a documented tolerance, and
  admission distinguishes permanent validation rejects from retryable
  infrastructure failures.
- Duplicate operator registration, activation or retirement cannot lock
  scheduler advance, retirement or inactivity handling.

## After launch

These are planned after launch and are not P1 release gates:

- Late join (#868). A node with an empty database builds its ledger only from
  genesis, so an operator joins a deployment before its first block, and a
  lost node database cannot be rebuilt. Rebuilding the ledger from retained
  DA history lifts both limits.

- SIGTERM drain that releases leases and leaves durable jobs recoverable.
- Versioned proof bundles that do not depend on node internals.
- Public retrieval APIs for bundles, witnesses and family metadata.
- A machine-readable map from reject code to family, script hashes and DA
  needs.
- SDK cancellation, timeout classification and typed provider error codes.
- SBOMs, vulnerability and license scans, signed images and publish
  provenance.
- Audit trails for admission, commitments, merges, payouts and admin actions.
- A data classification and retention policy for chain, DA, index, log,
  support and backup data.
- Support identifiers, diagnostic exports and a public status and explorer
  surface.
- Encrypted offsite backup and restore drills with RPO and RTO targets.
