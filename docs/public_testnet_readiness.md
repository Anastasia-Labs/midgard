# Public Testnet Readiness Checklist

Status: Active — no-go for an open public testnet.

Last full readiness review: 2026-09-01. Consolidated: 2026-09-07. Updated
2026-09-12 for the forced-submission merge and watcher journey status.
This edit does not rerun launch acceptance or close any release gate.

This is the release acceptance checklist for an externally reachable deployment.
It covers adversarial safety, deployment identity, recovery, user and operator
lifecycles, public challenger access, and operations. Each requirement appears
once below; implementation inventories belong in their maintained domain docs.

The honest-path pipeline and watcher source catalogue exist. Source installation,
unit tests, and local emulator success do not establish public readiness. Use the
[fault-proof catalogue](fault-proofs/catalogue-status.md),
[coverage matrix](fault-proofs/coverage-matrix.md), and
[testing status](fault-proofs/testing-status.md) for current coverage.

Close a gate only with evidence from the release revision: owner/reviewer, exact
command and environment, artifact location, result, and known limitations.
Previously implemented portions of a combined gate still need release verification.

## Public claim and protocol scope

- [ ] Publish one security claim across announcements, SDK, operator, and challenger
      docs: supported features, challenge windows, trust assumptions, and limitations.
      Accept the entire canonical V1 surface, including tx-order, script transactions,
      mint/burn, observers, redeemers, reference scripts, and withdrawal categories.
      Configuration cannot disable normative features to bypass acceptance.
- [ ] Choose permissionless or explicitly curated operator participation. Curated
      access uses deliberate controls; permissionless access cannot depend on key
      ordering accidents. Document operator-halt and fund-recovery assumptions.
- [ ] For genuinely non-canonical scope, either accept the escape-hatch lifecycle
      (real mint/spend validators, initialization, trigger, reduced bonds, grace and
      penalties, CLI, recovery, tests) or explicitly mark it unsupported in public
      docs and the manifest. Likewise accept settlement resolution claims or declare
      them unsupported: claims, deposit/withdrawal/tx-order disproof, economic slashing,
      matured removal, restart, valid-claim maturity, and no-slash tests are required.
- [ ] Forced (tx-order) submissions use the immutable-submission format from the
      [forced-submission decision](midgard/decisions/forced-inclusion-submission-verdict.md):
      the L1 order carries no validity field and `OperatorVerdictV1` is the only
      committed operator claim. Its encoding is bound into the consensus profile
      as `forcedTransactionSourceEncoding`, so a manifest published before it
      fails deployment identity and must be republished. The format is merged
      and locally verified; it is not deployed or live-accepted anywhere.
- [ ] Publish a threat model covering runtime, APIs, key/admin compromise, L1
      provider compromise and rate limits, spam, read amplification, mempool behavior,
      operational DoS, and incident assumptions. Publish `SECURITY.md` with scope,
      reporting/encryption options, acknowledgement/fix targets, safe harbor,
      exclusions, and an emergency contact.

## Deployment identity and contract release

- [ ] Reproduce the testnet blueprint with the compiler pinned by `aiken.toml`
      and CI. Record source revision, compiler/fork, flags, `aiken.lock`, blueprint,
      script bytes/hashes, protocol parameters, Cardano era/protocol version, runtime
      rule bundle, and release hashes. Enforce local version/rebuild checks as well
      as CI checks; old prose hash pins do not authorize deployment.
- [ ] Publish a signed manifest and complete parameterization graph: blueprint
      title/purpose, unapplied hash, ordered typed JSON/Data-CBOR parameters,
      dependency links, applied CBOR/hash/policy id, and catalogue root. Define signing
      authority, threshold, independent verification, rotation, and revocation.
- [ ] Generate one canonical script/reference registry. Classify every validator
      as deployed, excluded, or internal/test-only; fail on unclassified entries.
      Every included proof category has a complete ordered chain, step hashes,
      parameter links, membership CBOR, and reference requirements. Verify hub oracle,
      state queue, scheduler, operator lists, reserve, payout, and proof chains against
      the manifest at startup.
- [ ] Required public reference scripts have non-null records: address, outref,
      script hash, CBOR hash, lovelace, publisher tx hash, and verification time.
      The registry covers initialization, operator lifecycle, state queue, user
      events, reserve/payout, settlement, and every proof category.
- [ ] Review each public parameter's rationale and redeploy impact: registration
      and maturity durations, bond, slash/inactivity penalties, prover reward,
      reserve/outbox values, and required protocol assets. Isolate demo parameters;
      zero economics require an explicit signed rationale. Reject empty delegated
      hashes/placeholders unless a permitted excluded feature is provably unreachable.
      Require live value-conservation evidence, not merely nonzero constants.
- [ ] Bind every Postgres, MPF/local, and DA store to the deployment. Persist a
      genesis marker containing network, one-shot outref, policy ids, manifest hash,
      and schema version. Fail before serving on missing/mismatched identity or
      manifest read/write/verification failure. Test readiness identity checks and
      reset coupling: wiping local DB/MPF state requires a fresh on-chain deployment.

## Public runtime, ingress, and key custody

- [ ] Provide a distinct public profile exposing only intended ingress through
      TLS. Keep Postgres, Grafana, Prometheus, Loki, Tempo, cAdvisor, Ogmios, Kupo, and
      Cardano services internal or authenticated; disable anonymous Grafana admin.
      Remove Docker socket/host filesystem mounts or use hardened collectors.
      Verify rendered compose/deployment port bindings.
- [ ] Fail before HTTP binding on missing public settings, blank admin keys,
      demo seeds, default DB credentials, mutable images, exposed internal services,
      missing identity, or unapproved placeholder economics. Define image and Mithril
      snapshot policy, CPU/memory/disk/DB/indexer sizing, and retention capacity.
      The owner-set production hardware floor from the
      [economics decision](midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md)
      is accepted and carried here verbatim:

      | Role                                         | Floor                                                                                                             |
      | -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
      | midgard-node (operator)                      | ≥ 32 GiB RAM, ≥ 16 vCPU (2026 gaming-PC class), NVMe storage                                                      |
      | DA committee node, midgard-watcher, Postgres | sized from C74/C86 measured usage plus ≥ 2× headroom; the §5.1 ceilings are containment caps, not recommendations |

      C86 bounded-stress results refine the non-node role sizing but cannot
      lower the node floor.

- [ ] Use mounted secrets/secret files and prove startup errors/logs redact them.
      Separate operator, merge, reference-script, admin, provider, and release key
      owners, balances, privileges, signer isolation, and rotation/revocation.
      Decide hot versus external/HSM/manual signing. Disable seed phrases in process
      arguments and shell history; public user commands never select operational
      wallets by default and reject that custody mistake before submission.
- [ ] Define an authoritative action map: public HTTP/CLI, operator-only CLI,
      admin HTTP, or internal. Test the route graph. Protect `/init`, `/commit`,
      `/merge`, `/stateQueue`, `/stateQueueMutationLease`, `/logBlocksDB`, and
      `/logGlobals`. Admin mutations use POST, scoped attributable signed or mTLS
      identities, key ids, rotation/revocation, replay protection, and idempotency.
- [ ] Bound request bodies/content types at proxy and app, per-IP/global rates,
      admission concurrency, and read query cost. Paginate tx/UTxO/block/status/batch
      APIs with stable cursors, result and response-byte limits. Load tests prove
      bounded memory, DB growth, and bandwidth under spam and large accounts/blocks.
      Document CORS, timeout/abort, structured errors, and retry behavior for
      200/202/409/413/415/422/429/503 and provider failures.
- [ ] Distinguish `/healthz` liveness from `/readyz` readiness. Readiness checks
      provider freshness, Kupo coverage, Ogmios connection, node sync, deployment
      identity, successful first iterations of required workers, and recovery state.
      A temporary provider failure alone must not trigger restart loops.
- [ ] Test SIGTERM drain: immediately become non-ready, stop admission and new
      commit/merge work, finish or time out handlers, cancel loops, safely release
      renewable leases, close DB/MPF/metrics, flush logs, and exit within stop grace.
      Leave durable jobs recoverable. Provide container healthchecks.

## L1 authority, finality, time, and funding

- [ ] Select exactly one source mode without inference or silent fallback.
      `local_node` uses one watcher-operated Cardano full node and chain-sync as
      authority; its Ogmios/Kupo/db-sync services are not independent quorum members.
      Validate their network and compatible canonical chain point. In
      `external_providers`, require at least two operationally independent operators
      agreeing on network and compatible chain points; quarantine disagreement or
      lost independence. Decode actual chain bytes deterministically, and propagate
      rollback through every index without replaying Cardano validator semantics.
- [ ] Apply source authority/agreement and configured finality depth to commit,
      merge, deposit, withdrawal, reserve/payout, scheduler, operator, proof, and init
      observations. Persist tx/block hash, slot/block number when available, source
      mode/identity, observed depth, and threshold. Test rollback before threshold
      (pending/quarantine) and after finalization (incident and explicit recovery).
- [ ] Anchor deposit/withdrawal events and commit barriers to stable indexed
      chain points. Test appearance, disappearance, reappearance at another point,
      and conflicting same-event payloads before projection/finalization. Readiness
      exposes network, era, tip hash/slot, indexed-through point, and maximum drift;
      logs retain observation authority and provenance.
- [ ] Use chain time for validity windows with one audited slot/time conversion
      module across all lifecycle builders. Bound local-clock skew and fail/degrade
      readiness on skew or unavailable conversion. Test stale slots, epoch boundaries,
      upper-bound inclusivity, and public isolation from Custom/emulator fallbacks.
- [ ] Centralize fee/collateral selection across protocol transactions. Inputs
      must be pure ADA, owned by the intended wallet, above configured minimums,
      unconsumed locally, and neither protocol/reference-script nor datum-bearing
      UTxOs. Verify collateral return, total/max collateral inputs, fee funding, and
      min-UTxO after balancing. Expose balances, fragmentation, refill thresholds,
      and recovery procedures.

## Admission, validation, and durable recovery

- [ ] Verify canonical native-CBOR admission, locally derived and response-checked
      tx ids, durable bytes/payload hashes, and status transitions. Same-id different
      bytes must fail across every insert/replay path. Every conflict-ignore
      transition needs exact-count checks or same-payload reconciliation.
- [ ] Make backlog limits concurrency-safe with a documented bounded tolerance.
      Validate using deterministic public-verifier data; distinguish permanent
      validation rejects from retryable infrastructure failures. Preserve reason
      metrics and tie adversarial Phase A/B fixtures to proof-data expectations.
- [ ] Test validation lease/batch restart, including a crash after rejections
      persist but before acceptances: rejects remain terminal, candidates retry or
      accept exactly once, and spent inputs never resurrect.
- [ ] Exercise commit/confirmation/merge crash boundaries with persistent DB/MPF:
      no duplicate blocks, lost mempool txs, unbounded finalization, or divergence.
      Verify transaction-root reset/replay after DB commit and exact canonical payload
      transitions between mempool, processed, latest, and confirmed tables.
- [ ] Recover a submitted header before its local submitted marker, including
      deposit-only/user-event-only commits. Resolve by canonical header hash; do not
      build a competitor on the same base. Preserve the pending journal and finalize
      or abandon deterministically. Projection assignment and pending-status updates
      must be atomic or replay-safe.
- [ ] Recover merge confirmation before local job start and local DB commit before
      job completion. Detect queue advancement, prove completed effects or replay
      safely, and reconcile without manual DB edits. Preserve canonical DA payloads
      and verify their roots/counts before completing finalization.
- [ ] Expose lease/pending-finalization state and ages, mutation job ids/ages,
      processed depth, mempool overlap, missing immutable payload counts, unresolved
      submission age, merge failures, and queue length. Document manual recovery for
      pending finalization, leases, scheduler, merge, and intentionally abandoned
      commits, including how public watchers interpret them.

## Users, withdrawals, and operators

- [ ] Deposit build responses provide unsigned CBOR, event id, nonce outref/unit,
      auth/expected event units, valid-to, and expected inclusion/settlement timing.
      Document wallet funding, nonce selection, validity/retry, and public status
      discovery. Test duplicates, late deposits, malformed datums, and insufficient
      funding; monitor fetch/projection lag, divergence, and consumed-event conflicts.
- [ ] Declare whether withdrawal orders use L1 CLI/API, L2 submission, or both.
      Provide public build/status and external-wallet unsigned-CBOR/event metadata,
      or explicitly document local CLI custody limitations. Use shared production
      L1 submission/recovery, local UPLC evaluation, script-data-hash repair, timeout,
      and confirmation behavior.
- [ ] Classify exact L1 payout payability for `l2_value`, `l1_address`, and
      `l1_datum`. Persist diagnostic evidence; route `UnpayableWithdrawalValue` to
      invalid refund. Test min-ADA, token bundles, datum/address, and multi-asset
      edges. Wire production invalid-refund submission for the public lifecycle.
- [ ] Accept deposit reserve absorption, payout initialization, reserve funding,
      conclusion, and withdrawal finality from a clean public/preprod deployment.
      Expose payout phase, shortfall, next action, stuck/expired/invalid states, and
      alerts. Document reserve funding, batching, fee recovery, latency, and maturity.
      Make inclusion/settlement proof artifacts verifiable externally or explicitly
      describe privileged proof-resolution access. Complete tx-order lifecycle
      coverage under the canonical scope gate above.
- [ ] Provide register/activate/deactivate/deregister/status and bond/slash docs.
      Permissionless activation must support empty/head/middle/tail insertion and
      stale-anchor retry; curated activation must use explicit access controls.
      Preflight per-wallet funding for bond, lifecycle, scheduler, commit/merge,
      reference scripts, collateral, and recovery. Define Sybil-resistant faucet or
      manual funding that still permits legitimate onboarding.
- [ ] Enforce missed commitments and neglected deposit/withdrawal/tx-order events
      through watchdog/scheduler takeover and inactivity strikes. Test no-event and
      neglected-event inactivity, single-operator strike limit, multi-operator
      takeover, and partial-slash retirement. Expose operator/shift age, missed age,
      strike count, next takeover time, and last takeover tx.
- [ ] Prevent or slash duplicate registration/activation/retirement combinations;
      provide removal tooling and prove duplicates cannot lock scheduler advance,
      rewind, retirement, or inactivity slashing. Verify deterministic slash/reward
      flows for active, retired, registered, duplicate, bad-state, bad-settlement,
      and partially inactivity-slashed operators.
- [ ] Accept safe rekey preserving bonds/scheduler semantics or explicitly require
      retire, unlock, recover, and re-register. Test behavior during bond holds,
      pending shifts/commitments, and retirement. Document compromised-current-key
      response: stop signing, prevent unsafe commits, preserve audit state, recover
      or slash.

## Fraud proofs and public data availability

- [ ] Complete current signed publication/lifecycle checks and the availability
      challenge size plan in the [execution plan](fault-proofs/execution-plan.md).
      Every canonical family needs deployment-bound emulator, watcher, and preprod
      evidence; a first-family milestone cannot close the complete launch gate.
      Availability reference publication and real contract lifecycles now pass the
      pinned mainnet protocol-11 size/execution gates; see the
      [measured scope and remaining operational work](fault-proofs/size-plans/availability-challenge.md).
- [ ] With real deployed validators and public data, accept invalid-block
      detection, init/steps/conclusion, permanent proof-token mint, fraudulent block
      removal, and operator/slashing effects. Prove valid blocks cannot be challenged.
      Cover size/budget bounds and maximum shapes, cancellation, retry, and resume.
- [ ] Bind every append/header to verifiable L1-visible DA commitment/attestation
      covering full tx payloads, opened preimages, proof metadata, and member counts.
      Independent committee/storage nodes validate the exact header/payload relation,
      attest it, and serve retained data independently of the producer.
- [ ] Persist versioned proof bundles independent of transient node internals:
      header, root role/schema/root/count, canonical key/value CBOR, membership and
      applicable non-membership/deletion CBOR, field preimages, source payload hash,
      and verifier ABI. Store exact committed tx-root proofs and verify recomputation
      after restart.
- [ ] Provide stable public retrieval APIs or artifact exports for bundles, root
      members, witnesses, family/hash metadata, pagination, and retention guarantees.
      A raw payload alone is not challenger-grade retrieval. External challengers
      must reconstruct and submit without privileged DB access.
- [ ] Accept a single watcher process per supported workflow: watch headers, fetch
      public evidence, detect invalidity, simulate, submit, resume, and observe final
      resolution. Persist attempt id, thread UTxO, submitted hashes, confirmation,
      next action, and safe recovery semantics. Verify the
      [challenger](fault-proofs/challenger-runbook.md) and
      [manual recovery](fault-proofs/manual-recovery-runbook.md) runbooks externally.
      The [automatic watcher journeys](fault-proofs/automatic-watcher-journeys.md)
      harness is the acceptance vehicle for this gate on a real devnet. As of
      2026-09-12 one of the 54 non-interactive families (`transitionTrace`) has a
      completed live journey on the merged code's 55-family deployment; the rest
      are locally verified, running, blocked on owner rulings, or unrun. Watcher
      installation covers all 55 source categories; installation is not
      acceptance.
- [ ] Expose machine-readable rule/reject-code to family, script hashes, DA needs,
      and supported/disabled/unsupported status. Status cannot excuse missing
      normative coverage. Public tooling must not rely on incompatible-output or
      old-layout flags. Monitor proof-data publication/retrieval lag.

## SDK and public clients

- [ ] Verify intentional public exports and no accidental internal/test symbols.
      Test packed tarball ESM import, CJS require, and NodeNext types, plus a public
      API contract gate. Publish packages through the release channel below.
- [ ] Fail explicitly on invalid UTxOs/partial wallet state rather than silently
      discarding data. Keep protocol-info fallback explicit and unavailable by
      default for normal public use.
- [ ] Test cancellation throughout await/status/protocol calls, hung fetch and
      body reads, body failures, timeout classification, and user abort for protocol
      info, UTxO, submit, and status. Publish typed provider codes, retryability, and
      expected 400/409/413/415/422/429/503/5xx payload contracts.
- [ ] Document terminal/nonterminal status transitions, default await targets,
      `pending_commit`, and `awaiting_local_recovery`. Provide runnable browser/public
      endpoint examples for deposits, submit, withdrawal, polling, abort, and safe
      retries using published package versions.

## Release verification

- [ ] Run frozen install, builds, typechecks, and tests for core, SDK, Lucid,
      validation, node, fault proofs, DA, and watcher plus the pinned Aiken checks,
      native MPF, and relevant throughput gates. Retain exact commands/results.
- [ ] Provide one clean acceptance command sequence: fresh on-chain deployment
      and local state, operator onboarding, deposit, L2 transfers, commit, confirm,
      merge, withdrawal/reserve/payout, and restart. Verify DB/chain/API state without
      manual edits; retain tx hashes, queue state, balances, health/readiness, logs,
      and DB artifacts. Compose smoke must verify startup and safe port exposure.
- [ ] Accept persistent restart after projection, admission, submission,
      confirmation, merge, and payout; apply the crash cases above. Set pass/fail
      thresholds for spam, valid/invalid throughput, provider throttling, DB
      saturation, and API latency. Retain live proof acceptance evidence.
- [ ] Build public artifacts from a tagged revision with immutable SHAs/digests
      for actions, base/service/compose images, and release inputs. Fail release CI
      on mutable runner/image/action selections, including `ubuntu-latest` and tags.
- [ ] Generate CycloneDX/SPDX SBOMs for images, npm packages, and contract bundles.
      Scan vulnerabilities/licenses; block untriaged high/critical or prohibited
      licenses. Sign images by digest, publish npm provenance and contract
      in-toto/SLSA attestations; verify source, builder, locks, and command.
- [ ] Define coordinated versions or an explicit dependency graph, registry,
      `publishConfig`, CI publishing, and post-publish registry smoke tests. Reject
      `latest`, local file dependencies, unapproved runtime ranges, unresolved
      workspace specifiers, and locks from unapproved package-manager versions.

## Operations, retention, and public support

- [ ] Alert and dashboard readiness, stale workers, admission age/depth/rejects,
      provider lag, DB saturation, disk, unresolved finalization, merge failures,
      deposit/withdrawal/payout state, DA/proof lag, and public error spikes.
- [ ] Retain audit trails for admission, validation, commitments, merge, payouts,
      and admin actions. Enforce the current minimum DA availability window; pruning
      cannot remove data needed for proofs, recovery, payouts, or support.
- [ ] Classify on-chain/DA/canonical tx/index/log/support/backup data as immutable,
      retained, or deletable. State that public on-chain/DA data cannot be deleted.
      Define status minimum/terminal retention, pruning/expiry responses,
      `retainedUntil`, support windows, and consistent DB/log/backup policy.
- [ ] Remove raw CBOR/body logging on public routes; hash/truncate addresses and
      query values unless necessary. Test redaction of seeds, keys, credentials,
      requests, provider responses, tx CBOR, addresses, and support identifiers.
      Define log access audits, deletion/anonymization, and legal holds without
      corrupting proof reconstruction or recovery.
- [ ] Provide stable response correlation ids, support identifiers and safe
      diagnostic exports (tx/event/header/request ids, status history, log spans).
      Define severity, ownership, escalation, and response targets. Publish a public
      status/explorer surface for health, limitations, confirmed/merged blocks,
      user/payout status, incidents, maintenance, and resolution updates.
- [ ] Drill encrypted offsite backup/restore of Postgres and MPF on a fresh host,
      PITR/WAL archiving, manifest consistency, provider/indexer rebuild, key recovery,
      and failover. Set RPO/RTO and the exact stop-and-redeploy boundary.
- [ ] Publish operator setup/deploy/onboarding/upgrade/recovery and user
      wallet/protocol-info/deposit/submit/status/withdrawal runbooks. Include provider
      and DB outage, invalid block, stuck queue/payout, scheduler, backup, reset,
      and incident communication procedures with impact and resolution criteria.

Launch remains no-go until these gates have release-bound acceptance evidence
and the public claims match the accepted deployment.
