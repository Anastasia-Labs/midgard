# Public Testnet Readiness

Status: no-go. Reviewed 2026-10-13 against the integration branch at
`a81655e75`.

The first public testnet, P1, is a stage-0 network on Cardano Preprod, run from
the `preprod-public` deployment profile. Its launch is committed for 2026-11-07,
with 2026-10-31 as the stretch target. A regenesis, P2, is planned for December
2026 and lifts the P1 limits listed below.

This checklist owns the launch decision. It lists the implementation work that
must merge before launch and the release gates that need evidence from the
release revision. Proof coverage is tracked in the
[fault-proof catalogue](fault-proofs/catalogue-status.md),
[coverage matrix](fault-proofs/coverage-matrix.md) and
[testing status](fault-proofs/testing-status.md). Tags such as A1 or R6 are the
release plan's ticket ids.

Each item carries one status:

- **Done**: merged into the integration branch. The pointer names the merge or
  commit. Done code still needs the release evidence under
  [Release verification](#release-verification).
- **In progress**: part of the work exists, on a lane branch or merged. The
  entry names what remains.
- **Open**: no implementation or evidence yet.

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
- DA payloads carry neither validation-trace states nor the UTxO set. Of the
  nine fault-proof families that read validation-trace states, `missingRedeemer`
  is rebuilt without them and is in the P1 catalogue (owner, 2026-10-25); the
  other eight are not yet (`excluded_fraud_proof_categories` in
  [`preprod-public.yaml`](../config/deployments/preprod-public.yaml)) and join
  as each is rebuilt. The
  Plutus execution dispute (`validationTraceDispute`) is in it (owner,
  2026-10-21); `mainnet` still excludes it.
- Every party derives ledger state by replaying blocks from genesis. Payloads
  are retained for 120 days, and P1 has no state snapshot.
- Inactivity strikes skip an operator's shift but never eject the operator.
- Every watcher submits the proofs it finds with its own key.
- The operator allow-list is fixed for P1's life. A new operator, or an
  operator's new key, waits for P2.
- The escape hatch and settlement claims are not supported.
- L2 Plutus budgets use Midgard's P1 cost accounting. The node's
  `POST /tx/evaluate` returns the budget a transaction needs.
- Some node limits stay until P2: the Plutus Data size caps, the refusal of
  very large datum integers, and the refusal of native scripts supplied only
  through a reference input.

## Schedule

| Date       | Milestone                                                                                                                               | Status             |
| ---------- | --------------------------------------------------------------------------------------------------------------------------------------- | ------------------ |
| 2026-10-23 | Allow-list key hashes for `preprod-public` (owner input)                                                                                | Done (`d07f2fda7`) |
| 2026-10-26 | Validator freeze: every Aiken change merged, blueprint frozen                                                                           | Open               |
| 2026-10-27 | One golden regeneration; fit rows at the P1 maximum shapes; emulator lifecycles in both polarities over the frozen blueprint            | Open               |
| 2026-10-28 | Release candidate: frozen build and tag (R11), manifest (R3), devnet journey on `preprod-public` parameters, unattended run starts (R6) | Open               |
| 2026-10-29 | Fresh `preprod-public` deployment (R6); a second unattended run starts on it                                                            | Open               |
| 2026-10-30 | Drills on the fresh deployment (R6)                                                                                                     | Open               |
| 2026-10-31 | Stretch launch: security claim and user docs published (R1, R9), SDK published (R10), status page linked                                | Open               |
| 2026-11-05 | First live withdrawal payout matures, seven days after the deployment                                                                   | Open               |
| 2026-11-07 | Committed launch                                                                                                                        | Open               |

A contract change after the freeze needs an owner decision. A freeze later than
2026-11-02 moves the launch day for day. If the freeze slips, scope is cut in
this order and nowhere else:

1. the transaction evaluation endpoint (users then declare budgets by hand);
2. the watcher's persisted replay checkpoint (the watcher then replays from
   genesis on every restart);
3. R5.

## Implementation before launch

### Contracts

Every contract item merges before the 2026-10-26 freeze.

| Item                                                               | Status      | Pointer and remaining work                                                                                                                                                                                               |
| ------------------------------------------------------------------ | ----------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Forced-transaction admission at the tx-order mint (A1)             | Done        | Merge `b06aca902`. `forced_tx_is_admissible_v1` in `onchain/aiken/lib/midgard/user-events/tx-order-v1.ak`; the SDK and node share `screenMidgardForcedTxAdmission` and a golden corpus with Aiken                        |
| Closed 10-minute validity range on tx orders (A2)                  | In progress | Not committed. No profile carries `max_event_validity_range_ms` yet                                                                                                                                                      |
| Operator allow-list, P1 catalogue and P1 parameters (C1)           | Done        | Merges `63e24132e`, `bf82caf27`; keys `d07f2fda7`. Curated emulator tests in both polarities. Provers and DA challengers are permissionless (PL)                                                                         |
| Long-gap catch-up after an all-operator outage (C2)                | In progress | Commit time in the state-queue node merged (`19faf4948`); retention clocks merged (`fa0435b97`). Two follow-ups on `p1/c2a-onchain` are not merged. The remaining commit-clock readers and the catch-up planner are open |
| No empty blocks (C3)                                               | In progress | `6419a5fb2` and `39cde8b6d` on `p1/c2a-onchain`, not merged. The matching block-planner change is open                                                                                                                   |
| Asset names compare as plain bytes (C5)                            | Done        | `51ec22412`, `113fa9369`                                                                                                                                                                                                 |
| Datum canonicity fault proof (DC)                                  | Done        | Merges `78943b3c9`, `a81655e75`                                                                                                                                                                                          |
| Minimum-ADA and network-id routes for accepted forced transactions | In progress | `b742bb756`, `f0ddcfa5b`, `a40099c60` on `p1/pm-post-modes`, not merged; watcher workflow wiring open                                                                                                                    |

### Node, SDK, committee and watcher

| Item                                                                                         | Status      | Pointer and remaining work                                                                                                                                                                                                                                                             |
| -------------------------------------------------------------------------------------------- | ----------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Placeholder validation descriptors; no trace states in DA (B1)                               | In progress | Merge `e065e47d7`; `026761a97` pins the refusal of outputs with no descriptor. Removal of the retained-witness readers and the excluded families' off-chain code is on `p1/b1-rest`, not merged                                                                                        |
| Every admitted forced transaction gets a verdict (B2)                                        | In progress | Not committed                                                                                                                                                                                                                                                                          |
| Normal L2 Plutus (P)                                                                         | Done        | Merges `1fa289e40`, `eac653de0` and `7a9c84a87`: verdict mode on admission and the commit path, per-transaction cap, per-block budget, verdicts on the worker pool, `POST /tx/evaluate`, the Schnorr builtin and `evaluate()` in lucid-midgard. Per-IP limits on `/tx/evaluate` are R7 |
| Evict normal transactions that cannot be committed (B2-8)                                    | In progress | Not committed                                                                                                                                                                                                                                                                          |
| Deposit datum decoding in the L1 follower (B3)                                               | In progress | Not committed                                                                                                                                                                                                                                                                          |
| Withdrawal payability and settlement retries (B4, #820, #821)                                | In progress | Merge `a97f50266`. Two follow-ups on `p1/l6-next`, not merged: the prover sizes the payout from the committed bytes (`1c93ed3e8`), and a failing status read backs off (`64bd913cc`)                                                                                                   |
| DA payloads carry no UTxO set (DS)                                                           | In progress | Payload and producers merged (`bb888bb2a`); fault-proof ledgers by replay merged (`f49a882cc`). Shared block replay and the watcher's replayed ledger are on `p1/ds3-watcher-ledger`, not merged. Open: watcher production wiring, the node-tools watcher journeys, 120-day retention  |
| Unattended-liveness musts (B6, #696)                                                         | Done        | Merges `66cb1b6dc`, `aa18a9264`. The final unattended run is R6                                                                                                                                                                                                                        |
| The operator watchdog holds takeovers while the queue is progressing or cannot progress (B7) | In progress | `ff9dde981` on `p1/l6-next`, not merged                                                                                                                                                                                                                                                |

## Release verification

### Security claim and threat model (R1, R2)

- **Security claim.** Open. Publish one stage-0 claim in the announcement and
  the SDK, operator and challenger docs: the P1 model and every limit above,
  the enabled fault-proof families, challenge windows, trust assumptions, and
  the P2 sunset and regenesis notice. The owner ruled a two-stage launch on
  2026-10-12, so P1 does not accept the full canonical V1 surface; the claim
  states what P1 disables instead. A draft exists and awaits review.
- **Curated operators.** In progress. The operator allow-list is enforced
  on-chain at operator registration and off-chain by the SDK and node (C1).
  Computation-thread initialization and availability challenge Open are
  permissionless (PL). The claim
  must still document the operator-halt and fund-recovery assumptions.
- **Unsupported scope.** Open. Mark the escape hatch and settlement claims
  unsupported in the public docs and the manifest.
- **Published catalogue.** In progress. `preprod-public` excludes eight
  families not yet rebuilt without validation-trace states, and computation-thread
  initialization refuses an excluded category (C1). The claim and the manifest
  must list the enabled families.
- **Threat model and disclosure policy.** In progress. Publish the trust model
  per role, the threat model (runtime, APIs, key and admin compromise, L1
  provider compromise, spam, operational denial of service, incident
  assumptions) and `SECURITY.md` with the reporting channel, scope,
  out-of-scope list and emergency contact. `SECURITY.md` is not yet in this
  tree.

### Contract release and deployment identity (R3, R4, R5)

- **Reproducible blueprint.** Open. Rebuild the release blueprint with the fork
  compiler pinned by `aiken.toml`; CI's rebuild must equal it. Record the source
  revision, compiler, flags, `aiken.lock`, blueprint, script hashes, protocol
  parameters and rule bundle.
- **Signed manifest.** Open. Publish a manifest with script hashes and
  parameters, signed by one release key, and say how anyone verifies it. No
  manifest-signing tooling exists yet.
- **Script registry.** Open. The deploy tooling classifies every validator as
  deployed, excluded or internal and fails on an unclassified one. Every
  enabled proof family has its full step chain and parameter links.
- **Reference-script records.** Open. Every required reference script has a
  non-null record from the deploy tooling: address, outref, script hash, CBOR
  hash, lovelace and publisher transaction.
- **Parameter review.** In progress. Publish every P1 parameter with its
  rationale: the operator allow-list, excluded families, event validity range, maturity,
  negligence timeout, shift length, `max_inactivity_strikes`,
  `registration_ms`, the block ex-units budget and bonds. Release tooling must
  refuse placeholder keys and unexplained zero economics.
  - Set in `preprod-public`: `registration_ms` 30,000;
    `max_inactivity_strikes` 1,000,000; the operator allow-list; the excluded families.
  - Open: the 10-minute event validity range (A2); 120-day payload retention
    (DS); the block ex-units budget, which defaults to 33,000,000 memory and
    20,000,000,000 steps (`demo/midgard-node/src/services/config.block-ex-units-budget.ts`)
    and is to be re-measured on an idle machine; the owner's confirmation of
    the public operator and DA bond figures.
  - The `mainnet` profile still carries a placeholder operator key. It is not
    a P1 target.
- **Pooled DA bond.** Done ([decision record](midgard/decisions/da-committee-bond-pool.md),
  #686–#691). An attestation applies only while one bonded pool holds at least
  one DA bond of backing; an availability timeout slashes the pool. Its public
  figures are part of the parameter review.
- **Store identity.** Open. Bind every Postgres, MPF and DA store to the
  deployment with a marker holding the network, one-shot outref, policy ids,
  manifest hash and schema version. A missing or mismatched identity fails
  before serving. Wiping local state requires a fresh on-chain deployment.
- **Fee and collateral.** Open. Audit fee and collateral selection across
  protocol transactions: pure-ADA inputs owned by the intended wallet, above
  configured minimums, never protocol or reference-script UTxOs. Fix only what
  the journey trips on.
- **Canonical admission.** Open. Verify canonical native-CBOR admission,
  locally derived transaction ids and durable payload hashes. Same-id,
  different-bytes submissions fail on every insert and replay path.

### Public runtime (R7)

- **Public profile.** Open. Expose only intended ingress, through TLS. Keep
  Postgres, the observability stack, the Cardano node, and Kupo and Ogmios
  where tooling runs them, internal or authenticated. Disable anonymous Grafana
  admin. Remove Docker socket and host filesystem mounts. Verify the rendered
  port bindings.
- **Fail before binding.** Open. Refuse to bind HTTP on missing public
  settings, blank admin keys, demo seeds, default database credentials, mutable
  images, exposed internal services, missing identity or placeholder economics.
  Define image policy, sizing and retention capacity. The owner-set production
  hardware floor from the
  [economics decision](midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md)
  is accepted and carried here verbatim:

  | Role                                         | Floor                                                                                                             |
  | -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
  | midgard-node (operator)                      | ≥ 32 GiB RAM, ≥ 16 vCPU (2026 gaming-PC class), NVMe storage                                                      |
  | DA committee node, midgard-watcher, Postgres | sized from C74/C86 measured usage plus ≥ 2× headroom; the §5.1 ceilings are containment caps, not recommendations |

  C86 bounded-stress results refine the non-node role sizing but cannot lower
  the node floor.

- **Secrets.** Open. Use mounted secret files and prove that startup errors and
  logs redact them. Separate operator, reference-script, admin and release
  keys. Never take seed phrases from process arguments.
- **Action map.** Open. Classify every route and command as public, operator
  only, admin or internal, and test the route graph. Protect `/init`,
  `/commit`, `/merge`, `/stateQueue`, `/logBlocksDB` and `/logGlobals`.
- **Request limits.** In progress. Bound request bodies, content types, per-IP
  and global rates, and pagination at the proxy and in the app, including
  `/tx/evaluate`, and prove the bounds with a load test. The app now limits
  every route per client (IPv6 by /64) and every public route globally, with
  stricter classes for `/tx/evaluate`, `/submit` and batch status and separate
  health and admin classes; the load test is open.
- **Log redaction.** Open. Public routes never log raw CBOR or request bodies.
  Test redaction of seeds, keys, credentials, transaction CBOR and addresses.

### Fresh deployment, journeys and drills (R6)

All of these run on the release candidate and on the fresh `preprod-public`
deployment. Status: Open.

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
- **Payout.** A live withdrawal pays out after the seven-day maturity, on
  2026-11-05 for a 2026-10-29 deployment. Before launch, payout is evidenced on
  the emulator and on a `local-devnet-testing` devnet.
- **Drills.** Restart Cardano; lose public retained-DA retrieval; stop every
  operator for at least one hour, then let them catch up (C2). Readiness is
  held and then recovers, with no manual commit, merge, reset or follower store
  reset. Record every actor's durable state and pending signed attempts across
  recovery. Since the L1 follower landed (#814), no role reads Kupo or Ogmios,
  so there are no Kupo or Ogmios drills.
- **Crash and restart boundaries.** Restart after projection, admission,
  submission, confirmation, merge and payout. Cover a validation lease restart
  after rejections persist but before acceptances; commit, confirmation and
  merge crash boundaries with persistent database and MPF state; a submitted
  header recovered before its local marker; merge confirmation recovered before
  local job start. No duplicate blocks, lost mempool transactions or
  divergence.
- **Unattended run.** Complete the unattended lifecycle program (#696) on the
  release candidate devnet and on the fresh deployment. The availability
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
- `/healthz` reports liveness and `/readyz` reports readiness, with named hold
  reasons.
- Each role (node, watcher, DA committee) reads L1 only through one ChainSync
  follower on its operator's own Cardano node. A rollback rewinds the
  follower's facts and every role recomputes from them; a rollback deeper than
  k holds readiness with the process up.
- Deposit, withdrawal and commit observations are anchored to follower chain
  points, and L2 validity ranges use POSIX milliseconds from chain time.
- Withdrawal payability is classified with the same rule as the on-chain
  payout check (#820).
- There is no operator rekey: an operator retires and a new key registers
  ([operator lifecycle](../docs-site/content/docs/operators/node/operator-lifecycle.mdx)).
  In P1 a new key waits for P2.
- Each block header is bound to its DA commitment, which the DA committee
  attests and serves.

### Alerts, runbooks and documentation (R8, R9)

- **Alerts.** In progress. Page on commit lag, catch-up mode, a watcher
  replay-root mismatch, forced-admission divergence, follower refusals, stuck
  settlements, and expose readiness, worker staleness, admission depth,
  provider lag, database saturation, disk and payout state. The node's
  `/readyz` names stuck settlement jobs in its details. Alert delivery is
  open.
- **Runbooks.** Open. One runbook per alert, plus operator setup, deployment,
  upgrade and recovery, and the user wallet, deposit, submit, status and
  withdrawal flows.
- **User and operator docs.** Open. Document deposits, withdrawals, the P1
  forced-transaction limits with guidance on the missing validity upper bound,
  `evaluate()`, the curated operator lifecycle and status transitions. The
  [HTTP API page](../docs-site/content/docs/operators/node/server-http-api.mdx)
  already documents `/tx/evaluate`.

### SDK and release artifacts (R10, R11)

- **SDK exports.** Open. Audit the public exports. Test the packed tarball's
  ESM import, CommonJS require and NodeNext types against a public API gate.
- **Invalid UTxOs.** Open. The SDK fails explicitly on invalid UTxOs or
  partial wallet state instead of discarding data.
- **Versioned publish.** Open. Publish versioned packages from CI. Reject
  `latest`, local file dependencies and unresolved workspace specifiers.
- **Frozen build.** Open. Run frozen install, builds, typechecks and tests for
  every package, plus the pinned Aiken checks, on the tagged release
  candidate. Retain exact commands and results.
- **Tag and digests.** Open. Build public artifacts from a tagged revision with
  immutable SHAs and digests for actions, images and release inputs. Release
  CI fails on mutable selections such as `ubuntu-latest`.

### Not yet assigned

These requirements have no release ticket yet. Status: Open.

- Backlog limits are concurrency-safe with a documented tolerance, and
  admission distinguishes permanent validation rejects from retryable
  infrastructure failures.
- Duplicate operator registration, activation or retirement cannot lock
  scheduler advance, retirement or inactivity handling.

## After launch

These are planned after launch and are not P1 release gates:

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
