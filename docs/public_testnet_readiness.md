# Public Testnet Readiness Checklist

Last reviewed: 2026-09-01 against the current working tree.

Scope: this checklist reviews the current Midgard repository state for an externally reachable public testnet deployment. It treats Midgard as a production-grade L2, so "public testnet ready" includes adversarial safety, deterministic deployment identity, restart/recovery behavior, public client ergonomics, monitoring, and explicit runbooks. It is stricter than "the local happy path works."

Review method: repository-wide source review against the current worktree plus targeted verification of node operations, submission/admission, deposits and withdrawals, reserve/payout, contracts/deployment, data availability, fault proofs, SDK/provider behavior, and CI/acceptance coverage.

## Executive Decision

Current decision: no-go for an open public testnet.

- [ ] Launch gate: public testnet is ready as an unqualified public endpoint.
- [x] The core happy-path L2 pipeline is materially implemented: node startup, schema migrations, protocol initialization, operator registration/activation, `/submit`, admission/validation, mempool processing, deposit projection, block commitment, confirmation, merge, reserve/payout builders, and SDK/provider primitives.
- [ ] Public readiness blockers remain: complete fault-proof emulator/watcher acceptance, permissionless DA/retrieval guarantees, production timing/economics, L1 finality and rollback policy, ingress/ops hardening, clean public acceptance, and operator-halt/fund-recovery behavior.
- [ ] Public messaging must not claim full fault-proof readiness until all enabled families are installed and the proof bundle/data availability surfaces below are accepted.

## Core Feature Matrix

| Area                                | Status                            | Readiness judgement                                                                                                                                                                                                                                                                                                        |
| ----------------------------------- | --------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Native transaction submit/admission | Mostly implemented                | `/submit` accepts canonical Midgard-native CBOR and durable admission exists. Needs concurrency cap hardening and public DoS controls.                                                                                                                                                                                     |
| Phase A/B validation                | Mostly implemented                | Shared validation is broad and used by node admission. Needs public adversarial fixture coverage tied to proof generation.                                                                                                                                                                                                 |
| Deposits                            | Mostly implemented                | SDK/node builders and projection flow exist. Public `/deposit/build` needs richer metadata and runbook hardening.                                                                                                                                                                                                          |
| Withdrawals                         | Partial                           | SDK and CLI paths, L1 withdrawal-order ingestion, projection/classification, withdrawal roots, local status lookup, and payout handoff exist. Public HTTP build/status parity, shared L1 submit/finality behavior, exact payability, and docs remain blockers.                                                             |
| Reserve/payout                      | Partial                           | Builders and CLI flows exist for deposit absorption, valid-withdrawal payout init/funding/conclusion, plus SDK-level invalid-withdrawal refund construction. Node-level invalid-refund submission, public operator runbooks, and public/preprod acceptance remain blockers.                                                |
| Commit/confirm/merge                | Mostly implemented                | State queue, pending finalization journal, leases, DA-attestation-gated merge, and merge worker exist. Needs restart/recovery acceptance and stronger crash-boundary tests.                                                                                                                                                |
| Operator lifecycle                  | Partial                           | Register/activate builders and tests exist. Public CLI/API/runbook coverage for status, register, activate, deactivate/deregister, and monitoring is incomplete.                                                                                                                                                           |
| Fault proofs and DA                 | Partial, not public-testnet ready | All 32 catalogue validator families and their off-chain family modules exist. The watcher installs 25/32 categories. The three final families now pass under Van Rossem limits; Q58 publication, fixture-drifted inspection/dispute suites, public proof-data retrieval, and preprod challenge acceptance remain blockers. |
| Contract deployment                 | Mostly implemented                | Atomic init, canonical V1 deployment manifest identity, real blueprint loading, reference-script records, DA binding, and fail-closed startup verification exist. Manifest provenance/parameter coverage, signing/release policy, and realistic public-testnet parameters remain.                                          |
| Node operations                     | Partial                           | Docker, migrations, readiness, DA payload retention guards, metrics, and logs exist. Defaults and compose exposure are not public-hardened.                                                                                                                                                                                |
| SDK/provider                        | Partial                           | Provider submit, protocol-info parsing, DA transport V1 envelope codecs, and `DaPayloadV1` payload codecs exist. Packaging, abort/timeout behavior, and public docs need hardening.                                                                                                                                        |
| CI/acceptance                       | Partial                           | Primary Aiken, core, SDK, validation, fault-proof, DA, Lucid, node, watcher, native-MPF, and offline throughput checks run in CI. A clean public deploy, rollback/restart matrix, and preprod fraud-proof challenge remain outside the gate.                                                                               |

## Launch Blockers

- [x] Define and enforce a deployment fingerprint for current node and DA attachment.

  - Acceptance: every node stores and verifies the canonical V1 manifest identity containing network id, one-shot out-ref, contract bytes/hashes, reference-script records, and fraud-proof catalogue metadata.
  - Acceptance: startup fails closed when local durable state belongs to a different deployment fingerprint.
  - Evidence: `demo/midgard-node/src/commands/contract-deployment-info.ts` builds and verifies `midgard-deployment-manifest-v1`; `demo/midgard-node/src/commands/listen-startup.ts` refuses an existing deployment without a matching finalized manifest; DA runtime/store configuration binds to its manifest ID.

- [ ] Extend deployment identity into complete signed release provenance.

  - Acceptance: include Aiken compiler, `aiken.lock`, blueprint, protocol-parameter, Cardano era/protocol-version, runtime rule-bundle, and release hashes.
  - Acceptance: define manifest signing authority, threshold, rotation/revocation, and independent verification; reconcile identity across every durable node/MPF/DA store.

- [ ] Complete the public fraud-proof/proof-data-availability milestone.

  - Prerequisite: deploy only the current generated testnet identity. The
    blueprint on the working tree (built 2026-09-01 with `v1.1.23+5adf783`)
    contains 567 validators and has SHA-256
    `597c38912123f7f2c167bb73b61c3b37be44cd274be506538ee9bd4437711c96`,
    reproducible byte-for-byte from the working tree with the pinned fork
    (`~/.aiken/versions/v1.1.23-org-5adf7837`). The 32-category source
    catalogue (`00000000`–`0000001f`) derives root
    `85ecf82f70e409621d5324c54ae8e2deedbb7c37698e28ba7d76481c17bb6e90`,
    pinned by the inspection suite; that suite currently fails on
    deployment-fixture drift after the reference-script role-NFT change, so
    the pin is not re-verified against this blueprint. Do not deploy an older
    blueprint or catalogue identity as a substitute.
  - Acceptance: at least the intended public-testnet fraud-proof family is fully end-to-end on preprod from invalid block fixture to computation thread steps to fraudulent block removal.
  - Blocker: 51 compiled scripts exceed the 16,384-byte reference-script publication limit on the reproducible blueprint: 47 `validationTraceDispute` resolver bodies, both `transitionTrace` finals, `withdrawalMistag` step 03, and the availability challenge. Their emulator lifecycles pass only through a raised-limit oversized publication path in the test harness; production publication refuses them. Those three families cannot be deployed as compiled.
  - Blocker: the Q58 availability-challenge spending and minting roles currently apply the same 20,017-byte script (19,927-byte raw blueprint body), which alone exceeds the 16,384-byte L1 limit. Signed reference-script publications last measured 20,524 and 20,522 bytes. Production publication fails before funding selection; an authenticated split/yield redesign and redeployment are required before post-attestation withholding has an on-chain remedy.
  - Acceptance: proof bundles persist schema versions, payload hashes, root role, member count, membership/non-membership/deletion witnesses, and all inputs needed by public challengers.
  - Acceptance: public endpoints or documented data exports allow an external watcher to reconstruct and submit the proof without privileged local DB access.
  - Evidence: `demo/midgard-node/src/database/daPayloads.ts`, `demo/midgard-node/src/workers/commit-block-header/da-payload.ts`, `demo/midgard-sdk/src/da-payload.ts`, `demo/da-committee-node/src/da/payload.ts`, `demo/da-committee-node/src/da/libp2p/payload-protocols.ts`, and `demo/midgard-node/docs/TRANSITION_TRACE_COMMITMENTS.md` now provide a versioned node-produced `DaPayloadV1` keyed by header hash, with transition trace roots/counts and DA committee validation/retention paths, but not typed proof witnesses, public retrieval guarantees, or challenger-grade proof bundle retrieval.
  - Evidence: `docs/fault-proofs/testing-status.md`,
    `docs/fault-proofs/coverage-matrix.md`, and
    `docs/fault-proofs/catalogue-status.md` record the current local proof
    coverage and the remaining watcher, proof-data, and live/preprod gaps.

- [ ] Harden public ingress and operational exposure.

  - Acceptance: public API is behind TLS and rate limiting; admin endpoints are not internet-exposed; Prometheus, Loki, Grafana, cAdvisor, Tempo, Postgres, Ogmios, Kupo, and Cardano node ports are internal-only or authenticated.
  - Acceptance: `ADMIN_API_KEY` is mandatory for any admin route in public profiles, with a boot-time failure for unsafe public config.
  - Evidence: `demo/midgard-node/docker-compose.yaml` exposes multiple operational ports and enables anonymous Grafana admin access; this is acceptable for local/demo, not public.

- [ ] Define and enforce L1 finality, rollback, and provider-consistency policy.

  - Acceptance: protocol-affecting L1 observations finalize only after the selected source mode's authority/agreement rule and configured depth are satisfied.
  - Acceptance: every finalized L1 observation records chain point, source mode, chain-authority/provider identity, observed depth, and finality threshold.
  - Acceptance: rollback before threshold quarantines pending local state; rollback after local finalization is treated as an incident with an explicit recovery path.
  - Evidence: `demo/midgard-node-tools/src/commands/e2e-release-finality-policy-v1.ts` and the deployment manifest bind a release finality policy (confirmation depth 30, automatic recovery max depth 2160, deep-rollback incident policy), and `STATE_QUEUE_CORRECTION_FINALITY_DEPTH` must equal that depth; it is applied to state-queue correction and DA terminal outcomes, not to commit, merge, deposit, or withdrawal confirmation.

- [ ] Define public key custody and admin authority model.

  - Acceptance: operator, merge, reference-script, admin, provider, and release-signing keys have separate owners, rotation/revocation procedures, least-privilege balances, and an explicit hot-signing versus external-signer decision.
  - Acceptance: state-changing admin operations use attributable, replay-protected identities rather than a single static shared header.

- [ ] Close operator liveness, Sybil, and incentive gaps for the chosen participation model.

  - Acceptance: public testnet either supports permissionless operators with arbitrary ordered activation, inactivity/slash enforcement, duplicate removal, rewards, and key-compromise recovery, or clearly declares a curated-operator model with matching controls.

- [ ] Add a clean public-testnet acceptance gate.

  - Acceptance: a single documented command sequence starts from clean local state and a fresh on-chain deploy, registers/activates an operator, builds and projects a deposit, submits L2 transfers, commits, confirms, merges, processes withdrawal/reserve/payout, restarts the node, and verifies final DB/chain/API state.
  - Acceptance: the gate records tx hashes, state queue state, balances, health/readiness responses, logs, and DB verification artifacts.
  - Acceptance: it never combines wiped local state with an old on-chain deployment.

- [x] Align package-level CI with the public gate's relevant code surfaces.

  - Acceptance: CI runs frozen install, builds, typechecks, and tests the relevant workspace packages, including `midgard-core`, `lucid-midgard`, `midgard-sdk`, `midgard-validation`, `midgard-node`, and `midgard-fault-proofs`.
  - Acceptance: Aiken CI uses the same compiler version required by `onchain/aiken/aiken.toml`.
  - Evidence: `.github/workflows/aiken-ci.yml` and `.github/workflows/midgard-node-ci.yml` use the patched Aiken fork `v1.1.23+5adf783` (`Anastasia-Labs/aiken`, tag `midgard-5adf7837`), matching the `v1.1.23` release `onchain/aiken/aiken.toml` declares; node CI builds/tests core, validation, SDK, fault-proof tooling, DA committee, Lucid, node, native MPF, and offline throughput gates; `.github/workflows/midgard-watcher-ci.yml` covers the watcher package.
  - Remaining hardening: CI still lacks a clean public deployment, rollback/restart matrix, and preprod fraud-proof acceptance.

- [ ] Review and accept public-testnet economics and isolate demo parameters.

  - Acceptance: public-testnet protocol constants explicitly document registration duration, maturity duration, bond, slash penalty, prover reward, reserve/outbox parameters, and any intentionally zero-valued testnet choices.
  - Acceptance: public-testnet config is isolated from local/demo constants.
  - Evidence: canonical V1 fixes block maturity at seven days in `onchain/aiken/lib/midgard/ledger-state.ak`. The compiled testnet profile sets a 900 ADA required bond, 500 ADA slash penalty, 400 ADA prover reward, and 100 ADA inactivity penalty; the default profile sets 100,000/25,000/75,000/10,000 ADA respectively. These values are non-zero but are not yet supported by public economic analysis and live balance-conservation acceptance.

- [ ] Add public release, support, and data-retention posture.
  - Acceptance: public artifacts have signed provenance/SBOMs, public packages are published through a real release channel, SECURITY.md exists, user-facing support/status surfaces exist, and retention/deletion policy is documented without weakening auditability or fraud-proof reconstruction.

## Implementation Waves

### Wave 0: Public Launch Boundary

- [ ] Decide the first public-testnet security claim.
  - Acceptance: the public announcement, README, operator docs, and challenger docs use the same language for supported proof families, unsupported features, challenge window assumptions, and operator trust assumptions.
- [ ] Decide the public API surface.
  - Acceptance: every public route is listed as public, admin, or internal; only public routes are internet-routable in the public compose/deployment profile.
- [ ] Decide whether public testnet is "operator-run public endpoint" or "permissionless operator participation."
  - Acceptance: operator registration/activation/deregistration docs and access control match the chosen model.
- [ ] Publish a public-testnet threat model and SECURITY.md.
- [ ] Complete and accept every canonical V1 feature, including tx-order and
      script-bearing L2 transactions; separately identify any genuinely
      non-canonical protocol family that is outside the first public-testnet claim.

### Wave 1: Fail-Closed Deployment Identity

- [x] Add a deployment manifest generator and verifier.
- [ ] Persist the deployment fingerprint in Postgres and MPF/local state metadata.
- [x] Make listener startup fail before serving when the fingerprint is missing or mismatched.
- [x] Align Aiken compiler pinning and primary CI artifact rebuild checks.
- [ ] Split local/demo env templates from public-testnet env templates.

### Wave 2: Public Ingress And Runtime Safety

- [ ] Add a public compose/profile or deployment manifest that exposes only the intended API through ingress.
- [ ] Add node container healthchecks, stop grace periods, and graceful shutdown.
- [ ] Add provider/indexer freshness readiness.
- [ ] Add app/proxy rate limits and admission backpressure tests.
- [ ] Add alert rules and retention policy for public operations.
- [ ] Replace static admin header auth with scoped, attributable, replay-protected admin identities.
- [ ] Add pagination/result-size limits for every public read endpoint.

### Wave 3: Transaction Pipeline Recovery Evidence

- [ ] Add crash-boundary tests for durable admission, validation leases, mempool insertion, commit submission, pending finalization, MPF reset, confirmation recovery, and merge.
- [ ] Make every silent `ON CONFLICT DO NOTHING` transition either exact-count checked or same-payload reconciled.
- [ ] Make durable admission backlog caps concurrency-safe.
- [ ] Add public metrics for validation rejection reasons and recovery states.
- [ ] Add L1 finality/reorg tests for all protocol-affecting L1 tx observations.
- [ ] Make deposit/withdrawal ingestion chain-point anchored and rollback-aware.

### Wave 4: User And Operator Lifecycle Completeness

- [ ] Return deposit metadata needed by public clients.
- [ ] Harden withdrawal submission through the shared production L1 submit helper.
- [ ] Expose withdrawal build/status through the chosen public surface.
- [ ] Wire invalid-withdrawal refund submission through node CLI/API if the public lifecycle includes invalid withdrawal handling.
- [ ] Document and add public/preprod acceptance for deposit -> L2 submit -> withdrawal -> merge -> reserve/payout end to end.
  - Evidence: emulator coverage exists for deposit, reserve absorption, withdrawal commitment, settlement proof resolution, payout funding, and payout conclusion, but this is not yet a clean public/preprod acceptance gate.
- [ ] Add explicit operator lifecycle commands/status docs for register, activate, deactivate/deregister, and bond/slash state.
- [ ] Implement exact withdrawal payability classification and production invalid-withdrawal refund routing.
- [ ] Implement or explicitly exclude settlement resolution-claim flows.
- [ ] Add operator funding/faucet, inactivity enforcement, duplicate-operator handling, reward/slash, and key-rotation runbooks.

### Wave 5: Challenger And Fraud-Proof Milestone

- [ ] Define the first public fraud-proof milestone and prove it end to end on preprod.
- [ ] Persist typed proof bundles and public DA artifacts.
- [x] Persist and serve canonical `DaPayloadV1` block-body payloads for locally finalized blocks.
- [ ] Add proof bundle APIs or artifact export commands.
- [ ] Add watcher/challenger runbook and adversarial fixtures.
- [x] Maintain a closed proof-family coverage matrix before claiming broad fault-proof security.
  - Evidence: `docs/fault-proofs/coverage-matrix.md` maps the current 32-category codebase and separates implemented validators from remaining emulator-sweep, watcher, DA, economics, and acceptance gates.

### Wave 6: SDK/Public Client Release

- [ ] Fix `@al-ft/midgard-sdk` package exports for ESM/CJS/types.
- [ ] Curate public versus internal SDK exports.
- [ ] Add provider timeout/abort support.
- [ ] Add public testnet examples and status/error contract docs.
- [ ] Add package provenance and version pinning docs.

### Wave 7: Release, Support, And Public Operations

- [ ] Pin release inputs by immutable digest or commit SHA.
- [ ] Generate SBOMs, vulnerability/license scan reports, and signed attestations.
- [ ] Publish JS packages from CI through the chosen registry.
- [ ] Add public status/explorer, incident communications, support correlation IDs, and user-safe diagnostic export.
- [ ] Define public data retention/deletion policy and disaster recovery RPO/RTO.

## Source Evidence Index

These are the main code-backed reasons for the no-go decision. They should be kept current as items are fixed.

| Evidence                                                                                                                                                                                                                                                                                                                                                                                                                                                       | Readiness impact                                                                                                                                                                            |
| -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `demo/midgard-node/docker-compose.yaml` exposes API, metrics, Postgres, Prometheus, Loki, cAdvisor, Grafana, and Tempo host ports; Grafana anonymous role is Admin.                                                                                                                                                                                                                                                                                            | Public profile must hide or authenticate internal services before public launch.                                                                                                            |
| `demo/midgard-node/docker-compose.kupmios.yaml` defaults Mithril, Ogmios, and Kupo images/snapshots to `latest` and exposes provider ports.                                                                                                                                                                                                                                                                                                                    | Public deployment must pin images/digests and restrict provider ports.                                                                                                                      |
| `demo/midgard-node/.env.example` contains blank `ADMIN_API_KEY`, default Postgres credentials, `latest` image tags, retention `0`, and test/demo seed phrases. Positive retention below the DA minimum now fails config, but there is still no separate public env profile or public retention policy.                                                                                                                                                         | Public env template must reject demo/default secrets and document retention/DA availability choices explicitly.                                                                             |
| `demo/midgard-node/src/services/config.ts` defaults min fees to `0`, admin key to empty, hub-oracle one-shot index to `-1`, Postgres credentials to `postgres`, and MPF paths to relative local paths.                                                                                                                                                                                                                                                         | Public strict mode must require explicit production values and durable absolute paths.                                                                                                      |
| `demo/midgard-node/src/commands/listen-router.ts` readiness checks worker heartbeat timestamps, DB health, durable-admission backlog/age, unresolved submissions, unfinished local mutation jobs, state-queue mutation lease inspection, pending-finalization summaries, and a hub-oracle query, but not provider tip freshness, Kupo coverage, deployment fingerprint, or first successful worker loop.                                                       | `/readyz` is useful but not sufficient for public traffic routing.                                                                                                                          |
| `demo/midgard-node/src/commands/listen-router.ts` keeps `/init`, `/commit`, `/merge`, `/stateQueue`, `/stateQueueMutationLease`, `/logBlocksDB`, and `/logGlobals` as HTTP admin routes.                                                                                                                                                                                                                                                                       | Admin routes must be private/authenticated and not exposed via public ingress.                                                                                                              |
| `demo/midgard-node/src/commands/listen-router.ts` exposes `/deposit/build` and `/deposit-status`, but no `/withdrawal/build` or `/withdrawal-status` HTTP route.                                                                                                                                                                                                                                                                                               | Withdrawal support exists through SDK/CLI/node internals, but public API parity is incomplete.                                                                                              |
| `demo/midgard-node/src/database/txAdmissions.ts` checks backlog with `COUNT(*)` before insert inside the transaction.                                                                                                                                                                                                                                                                                                                                          | Concurrent public submissions can overshoot the configured cap unless admission is globally serialized or otherwise bounded.                                                                |
| `demo/midgard-node/src/workers/utils/commit-submission.ts` finalizes DB state and then resets the transactions MPF root outside the SQL transaction.                                                                                                                                                                                                                                                                                                           | Recovery may be valid, but public readiness needs explicit crash-boundary tests and alerts.                                                                                                 |
| `demo/midgard-node/src/workers/utils/commit-submission.ts` transfers skipped submissions by inserting processed mempool rows and then clearing mempool rows in separate steps.                                                                                                                                                                                                                                                                                 | Crash-boundary behavior must be tested or made atomic to avoid ambiguous duplicate material.                                                                                                |
| `.github/workflows/aiken-ci.yml` and `.github/workflows/midgard-node-ci.yml` use the patched Aiken fork `v1.1.23+5adf783` (`Anastasia-Labs/aiken`, tag `midgard-5adf7837`), matching the `v1.1.23` release `onchain/aiken/aiken.toml` declares, and both pin the fork by rev; but the workflows still use mutable runner/action references for everything else.                                                                                                | Compiler-version alignment is fixed, but release CI is not yet supply-chain pinned.                                                                                                         |
| `.github/workflows/midgard-node-ci.yml` builds/tests core, validation, SDK, fault-proof, DA committee, Lucid, node, native MPF, and offline throughput surfaces, but not a clean public deployment, rollback/restart matrix, or preprod challenge.                                                                                                                                                                                                             | Package CI is broad; system acceptance is not yet a public-testnet release gate.                                                                                                            |
| `onchain/aiken/env/testnet.ak` has non-zero bond/slash/reward values but defines no outbox identifiers at all; its delegated Plutarch hashes are fully pinned (PHAS `1fc59ff5…`, pexcludes `03adaadf…`, both matching the deployed Aiken-native scripts).                                                                                                                                                                                                      | Public-testnet economic parameters still need independent review and live acceptance.                                                                                                       |
| Canonical V1 fixes block maturity at seven days. The default and testnet environments compile non-zero bond, slash, prover-reward, and inactivity-penalty values.                                                                                                                                                                                                                                                                                              | Public economic analysis and exact live balance-conservation acceptance remain blockers.                                                                                                    |
| `docs/fault-proofs/testing-status.md` records emulator coverage but no current publishable preprod end-to-end challenge/removal acceptance.                                                                                                                                                                                                                                                                                                                    | Fraud-proof readiness is not yet public-testnet ready.                                                                                                                                      |
| `docs/fault-proofs/coverage-matrix.md` and `catalogue-status.md` record 32 implemented validator families, three final families now green under Van Rossem limits, and the remaining fixture-drift, seven watcher-installation, proof-data, economics, and live/preprod gaps.                                                                                                                                                                                  | Public security claims must be narrowed and enabled features gated until operational acceptance is complete.                                                                                |
| `demo/midgard-node/src/database/pendingBlockFinalizations.ts` persists pending block payload members and `demo/midgard-node/src/database/daPayloads.ts` persists canonical `DaPayloadV1` payload CBOR plus roots and counts, but neither stores full typed proof-bundle schemas, root roles, membership/non-membership/deletion witnesses, opened field preimages, or verifier ABI versions.                                                                   | External challengers need proof witnesses and stable schemas, not only node-local block-body payloads.                                                                                      |
| `demo/da-committee-node/src/da/payload.ts` validates canonical `DaPayloadV1` bytes and `demo/da-committee-node/src/da/libp2p/payload-protocols.ts` serves payload, metadata, and attestation exchange over libp2p rather than HTTP.                                                                                                                                                                                                                            | The DA committee path is materially stronger than node-local payload production, but public availability guarantees and challenger APIs still need to be specified and accepted end to end. |
| `demo/midgard-node/src/transactions/da-attestation.ts` can mint/sign/apply DA attestations for state-queue headers, and `demo/midgard-node/src/transactions/state-queue/merge-to-confirmed-state.ts` skips merge until the queued block carries the expected DA attestation policy id.                                                                                                                                                                         | Merge is gated on DA attestation, but public committee operation, payload retrieval guarantees, and watcher/challenger integration remain undefined.                                        |
| `demo/midgard-fault-proofs` provides manifest-bound production runner factories for 25 categories, and `demo/midgard-watcher` installs the same 25.                                                                                                                                                                                                                                                                                                            | Production challenger infrastructure is real but not catalogue-complete or accepted end to end.                                                                                             |
| `demo/midgard-sdk/package.json` lacks an `exports` map while building ESM and CJS under `"type": "module"`.                                                                                                                                                                                                                                                                                                                                                    | Public CJS consumers can resolve the wrong entrypoint.                                                                                                                                      |
| `demo/lucid-midgard/src/provider/transport.ts` calls fetch without managed timeout/abort support.                                                                                                                                                                                                                                                                                                                                                              | Public clients can hang on provider calls despite `awaitTx` accepting a signal at a higher layer.                                                                                           |
| `demo/midgard-node/src/transactions/utils.ts` and `demo/midgard-node/src/workers/confirm-block-commitments.ts` treat `awaitTxConfirmation` visibility (timeout and poll interval, no depth) as confirmation, and block confirmation advances local state when the state-queue UTxO is observed; the manifest-bound release finality depth (30) is enforced only for state-queue correction and DA terminal outcomes.                                           | Public readiness needs explicit L1 finality/depth and rollback policy.                                                                                                                      |
| Deposit and withdrawal DB rows store event tx hashes/output indexes but not observed block hash, slot, depth, or provider source.                                                                                                                                                                                                                                                                                                                              | User-event ingestion is not rollback-aware enough for public operation.                                                                                                                     |
| `demo/midgard-node/src/services/config.ts` accepts only `L1_PROVIDER=Kupmios`, and `demo/midgard-node/src/services/lucid.ts` runs preflight checks against that single route; the node has no `external_providers` mode, multi-provider agreement, or same-tip gate.                                                                                                                                                                                           | State decisions can mix provider views unless consistency checks are added.                                                                                                                 |
| User-event builders and commit window selection use local `Date.now()` for validity windows.                                                                                                                                                                                                                                                                                                                                                                   | Public readiness needs a chain-time authority and clock-skew policy.                                                                                                                        |
| Node config and Lucid service derive long-running operator/merge/reference wallets from seed phrases.                                                                                                                                                                                                                                                                                                                                                          | Public readiness needs a clear signer boundary and key-custody model.                                                                                                                       |
| CLI commands accept seed phrases directly as command-line arguments.                                                                                                                                                                                                                                                                                                                                                                                           | Public/operator workflows must avoid secrets in shell history, process args, and logs.                                                                                                      |
| Admin routes use one static `x-midgard-admin-key`, and mutation routes such as `/init`, `/commit`, and `/merge` are GET endpoints.                                                                                                                                                                                                                                                                                                                             | Public admin control needs attributable identities, rotation, replay protection, and POST/idempotency semantics.                                                                            |
| Public read endpoints include unbounded address-history, UTxO, and block transaction queries.                                                                                                                                                                                                                                                                                                                                                                  | Public APIs need pagination, result caps, and query cost budgets.                                                                                                                           |
| No `SECURITY.md` or visible vulnerability disclosure process exists.                                                                                                                                                                                                                                                                                                                                                                                           | Public testnet needs a security reporting and active-exploitation escalation process.                                                                                                       |
| `demo/midgard-node/Dockerfile` uses `node:22`, CI uses `ubuntu-latest`/tagged actions, and no SBOM/signing scan exists.                                                                                                                                                                                                                                                                                                                                        | Public release artifacts are not yet supply-chain hardened.                                                                                                                                 |
| SDK/node docs still describe local tarball packing and manual lockfile SHA updates.                                                                                                                                                                                                                                                                                                                                                                            | Public packages need a real registry release channel and post-publish smoke tests.                                                                                                          |
| Operator activation currently only appends after the active tail in some paths.                                                                                                                                                                                                                                                                                                                                                                                | Permissionless operator participation is not robust for arbitrary keys unless curated participation is declared.                                                                            |
| On-chain scheduler/inactivity/slashing capabilities are not fully wired as operator services.                                                                                                                                                                                                                                                                                                                                                                  | Public liveness needs watchdog/takeover/slashing flows, not only happy-path scheduling.                                                                                                     |
| Active-operator comments document a duplicate active/retired scheduler-lock edge case.                                                                                                                                                                                                                                                                                                                                                                         | Public readiness needs duplicate-operator/Sybil cleanup or explicit exclusion.                                                                                                              |
| Escape hatch is specified, but no validator exists under `onchain/aiken/validators`, and `demo/midgard-sdk/src/escape-hatch.ts` holds only the asset-name constant.                                                                                                                                                                                                                                                                                            | Public limitations must state whether escape hatch liveness recovery is supported.                                                                                                          |
| Settlement resolution-claim disprove/slashing builders are incomplete.                                                                                                                                                                                                                                                                                                                                                                                         | Settlement claim lifecycle must be completed or excluded from public scope.                                                                                                                 |
| `demo/midgard-node/src/database/withdrawals.ts`, `demo/midgard-node/src/fibers/fetch-and-insert-withdrawal-utxos.ts`, `demo/midgard-node/src/workers/utils/mpf.ts`, `demo/midgard-node/src/commands/withdrawal-status.ts`, and `demo/midgard-node/src/workers/commit-block-header/event-roots.ts` implement withdrawal event persistence, ingestion, classification, L2 ledger deletion for valid withdrawals, local status, and withdrawal-root construction. | The readiness gap is no longer basic withdrawal support; it is public API/docs, finality/reorg safety, exact payability, and production refund/payout operations.                           |
| `UnpayableWithdrawalValue` exists in the `withdrawal_utxos` validity enum and the SDK ledger-state schema, but `demo/midgard-node/src/workers/utils/mpf/withdrawal-classification.ts` never produces it; classification lacks exact Cardano L1 payability/min-ADA evidence.                                                                                                                                                                                    | Valid withdrawals may be marked payable before the L1 output is actually constructible.                                                                                                     |
| Logs include raw tx CBOR and full user query values in public route paths.                                                                                                                                                                                                                                                                                                                                                                                     | Public logging needs privacy-safe redaction and retention/deletion policy.                                                                                                                  |

## Deployment And Contracts

- [ ] Pin and verify the exact Aiken compiler version in CI, local docs, and build scripts.
  - Evidence: primary CI workflows now use the patched Aiken fork `v1.1.23+5adf783` (`Anastasia-Labs/aiken`, tag `midgard-5adf7837`), matching the `v1.1.23` release `onchain/aiken/aiken.toml` declares; local install docs and reproducible artifact verification still need enforcement.
- [ ] Publish a reproducible contract build artifact bundle.
  - Acceptance: bundle includes blueprint, script CBOR/hashes, source commit, `aiken.lock`, compiler version, and a machine-verifiable hash.
- [ ] Define a canonical script catalogue generated from `plutus.json`.
  - Acceptance: every public-testnet validator is classified as `deployed`, `excluded`, or `internal/test-only`; unclassified blueprint validators fail CI.
  - Acceptance: each catalogue row includes title, purpose, raw blueprint hash, applied parameters, final script hash/policy id, and reference-script requirement.
- [ ] Require a signed deployment manifest for public testnet.
  - Acceptance: operators can compare the deployed hub oracle, state queue, scheduler, operator lists, reserve, payout, and fraud-proof catalogue against the manifest.
- [ ] Make contract deployment info fail-closed for public profiles.
  - Acceptance: startup refuses to serve if it cannot write/read/verify the deployment manifest in public-testnet mode.
- [ ] Record the full parameterization graph in the deployment manifest.
  - Acceptance: each script entry includes blueprint title, un-applied compiled-code hash, ordered applied params as typed JSON/Data CBOR, final script CBOR hash, final script hash, and dependency links.
- [ ] Make fraud-proof catalogue membership match deployed proof chains.
  - Acceptance: each included category maps to a complete ordered chain with all step hashes, parameter links, membership proof CBOR, and reference-script requirements.
- [ ] Replace default or placeholder public-testnet protocol parameters with explicit public-testnet values.
- [ ] Fail public-testnet builds on empty delegated script hashes and placeholder outbox values.
  - Acceptance: public env validation fails if any reachable delegated validator hash, outbox policy/address, or required protocol asset is empty/zero unless an explicit disabled-feature manifest entry proves it is unreachable.
- [ ] Document and test the full redeploy/reset procedure.
  - Acceptance: the runbook states that local DB/MPF resets require a fresh on-chain redeploy, and the acceptance script enforces this.
- [ ] Verify reference scripts at startup against the manifest, not only against expected script hashes discovered at runtime.
- [ ] Replace scattered reference-script target lists with a registry keyed by manifest entries.
  - Acceptance: one canonical registry declares every script's reference-script requirement per flow: init, operator lifecycle, state queue, deposits, withdrawals, payout/reserve, settlement, and each proof category.
- [ ] Make reference-script UTxO identity non-null for public-required scripts.
  - Acceptance: public manifest rejects null refs for required scripts and records ref address, out-ref, script hash, script CBOR hash, lovelace, publisher tx hash, and verification timestamp.
- [ ] Add a public-testnet contract parameter review record.
  - Acceptance: each public parameter has a rationale and a rollback/redeploy impact note.

## Node Configuration And Startup

- [ ] Create a separate public compose/deployment profile.
  - Acceptance: `docker compose -f docker-compose.public-testnet.yaml config` exposes only the intended public ingress port; Postgres, metrics, logs, tracing, Ogmios, Kupo, and Cardano node ports have no host bindings.
- [ ] Remove Docker socket and host filesystem mounts from the public observability profile.
  - Acceptance: public observability either omits those collectors or uses a hardened collector setup without Docker socket access.
- [ ] Add a `PUBLIC_TESTNET_PROFILE` or equivalent strict mode.
  - Acceptance: strict mode rejects blank admin keys, demo seeds, default Postgres credentials, `latest` image tags, exposed internal services, zero/placeholder economics unless explicitly allowlisted, and missing deployment fingerprint.
- [ ] Move public secrets out of `.env`/`env_file` and prove they are not logged.
  - Acceptance: public profile uses Docker secrets or mounted secret files; startup redacts seed/API/DB/admin values; tests assert config errors never include secret material.
- [ ] Make unsafe defaults impossible in public mode.
  - Acceptance: node fails before binding HTTP if required public settings are missing.
- [ ] Verify Kupo/Ogmios/Cardano tip freshness in readiness.
  - Acceptance: `/readyz` reports provider tip age, Kupo coverage, Ogmios connection, and Cardano node sync health.
- [ ] Add deployment-fingerprint checks to readiness.
- [x] Add active state-queue mutation lease visibility to readiness.
  - Evidence: `/readyz` includes `stateQueueMutationLease` with active lease, pending finalizations, and recent leases.
- [ ] Ensure worker heartbeat readiness cannot pass before each required worker has completed at least one successful iteration.
- [ ] Add node container healthcheck and operational health policy.
  - Acceptance: container restart/liveness uses `/healthz`, ingress/load balancer readiness uses `/readyz`, and the runbook states not to restart solely on temporary provider/readiness failures.
- [ ] Add graceful shutdown handling.
  - Acceptance: the node stops admission/commit/merge loops cleanly, releases renewable leases, flushes logs/metrics, and leaves no orphaned local mutation job.
- [ ] Implement shutdown admission drain semantics.
  - Acceptance: on SIGTERM, node immediately returns non-ready, stops accepting `/submit`, lets in-flight handlers finish or times out, cancels periodic loops, closes metrics/DB/MPF resources, and exits within the configured stop grace period.
- [ ] Document resource sizing for public testnet.
  - Acceptance: CPU, memory, disk, DB, Cardano node, Kupo, Ogmios, and retention requirements are stated with minimum and recommended values.
- [ ] Pin Docker image tags and Mithril snapshot policy.
  - Acceptance: no public profile uses `latest` for node dependencies.
- [ ] Increase log retention and audit retention for public deployments.
- [ ] Enforce reset/redeploy coupling with a machine-readable local genesis marker.
  - Acceptance: first successful deploy writes `{network, oneShotOutRef, policyIds, manifestHash, schemaVersion}` into Postgres and MPF metadata; missing or mismatched marker fails closed unless an explicit redeploy command creates a new marker from a fresh on-chain deployment.
- [x] Reject positive retention windows shorter than the current DA payload availability minimum.
  - Evidence: `demo/midgard-node/src/database/retention-policy.ts` requires `RETENTION_DAYS=0` or at least 8 days, and config loading applies that validator.

## API, Ingress, And Security

- [ ] Define one authoritative public route/action map.
  - Acceptance: every lifecycle action is classified as public HTTP, public CLI, operator-only CLI, admin HTTP, or internal-only; tests assert the route graph matches that contract.
- [ ] Put `/submit`, `/deposit/build`, status, and UTxO APIs behind documented rate limits.
- [ ] Enforce request body size and content-type limits at the proxy and application layer.
- [ ] Add per-IP and global admission backpressure behavior.
  - Acceptance: load tests show bounded memory and durable-admission DB growth under spam.
- [ ] Keep admin routes private or strongly authenticated.
  - Acceptance: `/init`, `/commit`, `/merge`, `/stateQueue`, `/stateQueueMutationLease`, `/logBlocksDB`, and `/logGlobals` are not publicly reachable.
- [ ] Add CORS policy for public clients.
- [ ] Add structured error response contracts for all public endpoints.
- [ ] Add timeout/abort handling for long public requests.
- [ ] Document retry semantics for `202`, `200`, `409`, `413`, `415`, `422`, `429`, `503`, and provider failures.
- [ ] Add abuse monitoring for admission rejects, duplicate submissions, bad CBOR, oversized requests, and validation failures.
- [ ] Remove operator-seed defaults from public user commands.
  - Acceptance: public user commands never default to operator or reference-script wallets; command startup fails if a user command resolves to an operational wallet.

## Security, Threat Model, And Key Custody

- [ ] Publish a public-testnet threat model.
  - Acceptance: threat model covers off-chain runtime, public APIs, operator key compromise, admin compromise, L1 provider compromise/rate limits, submission spam, read amplification, public mempool behavior, operational DoS, and incident-response assumptions.
- [ ] Add `SECURITY.md`.
  - Acceptance: document includes public-testnet scope, reporting contact, encryption/PGP option, acknowledgement/fix timelines, safe-harbor language, known exclusions, and emergency contact path for active exploitation.
- [ ] Define public-testnet key custody requirements.
  - Acceptance: operator, merge, reference-script, admin, provider, and release keys have separate owners, rotation plans, least-privilege balances, signer isolation, and an explicit hot in-process signing versus external/HSM/manual-signing decision.
- [ ] Remove or hard-disable command-line seed phrase arguments from public-testnet/operator workflows.
  - Acceptance: public workflows use secret files, environment variable names, stdin no-echo, or external signers; secrets never appear in shell history, process args, logs, or runbooks.
- [ ] Replace public-testnet admin auth with scoped, attributable admin identities.
  - Acceptance: admin requests are signed or mTLS-authenticated with key ids, rotation/revocation, replay protection, and audit subject.
  - Acceptance: state-changing admin routes use `POST` with idempotency/replay controls, not `GET`.
- [ ] Add query cost controls to public read APIs.
  - Acceptance: `/txs`, `/utxos`, `/block`, batch out-ref lookups, and status/explorer APIs have pagination, maximum result counts, maximum response bytes, stable cursors, and DB/memory/bandwidth tests for large accounts and blocks.

## L1 Finality, Rollbacks, And Provider Consistency

- [ ] Select exactly one explicit L1 source mode: `local_node` or `external_providers`.
  - Acceptance: configuration never infers a mode from endpoint count and never falls back or mixes authority models after startup.
  - Acceptance (`local_node`): one watcher-operated Cardano full node is the chain-consensus authority and chain-sync supplies roll-forward and rollback events.
  - Acceptance (`local_node`): Ogmios, Kupo/Kupmios, and db-sync may query the same local node, but never count as independent providers or create a quorum requirement.
  - Acceptance (`local_node`): every query/index result is proven to use the authority node's network and a compatible canonical chain point; stale or mismatched results fail closed.
  - Acceptance (`external_providers`): at least two operationally independent provider operators/endpoints agree on network and compatible chain points before any protocol decision.
  - Acceptance (`external_providers`): disagreement or loss of independence quarantines protocol decisions.
  - Acceptance (both modes): canonical rollbacks propagate through every watcher index, and accepted state is decoded deterministically from actual node-derived transaction/output/datum bytes without replaying Cardano validator semantics.
- [ ] Define and enforce an L1 finality policy for every protocol-affecting L1 transaction.
  - Acceptance: commit, merge, deposit, withdrawal, reserve/payout, scheduler, operator lifecycle, proof, and initialization flows finalize only after the selected source mode's authority/agreement rule and configured depth are satisfied.
  - Acceptance: each finalized L1 observation records tx hash, block hash, slot, block number if available, source mode, chain-authority/provider identity, observed depth, and finality threshold.
  - Acceptance: reorg below the threshold leaves local state pending or quarantined; reorg after local finalization is detected as an incident and has an explicit recovery runbook.
  - Acceptance: public-testnet config states finality depth/settlement assumptions and tests cover rollback before and after the threshold.
- [ ] Make deposit and withdrawal ingestion rollback-aware.
  - Acceptance: every ingested L1 event records block hash, slot, provider/indexer source, and observed depth.
  - Acceptance: commit-time event barriers only accept events visible through a stable indexed tip, not merely present in the current UTxO query.
  - Acceptance: if a previously ingested but unfinalized event disappears or moves due to rollback/indexer correction, the node invalidates or quarantines it before projection/finalization.
  - Acceptance: tests simulate event appearance, rollback disappearance, reappearance at a different chain point, and conflicting same-event payloads.
- [ ] Add source-mode consistency gates for public-testnet L1 reads and confirmations.
  - Acceptance: in `local_node`, Cardano-node chain-sync is authoritative and Kupo, Ogmios, or db-sync results are accepted only after same-network, compatible-canonical-chain-point validation against that node.
  - Acceptance: in `external_providers`, protocol decisions require at least two operationally independent provider operators/endpoints to agree on network and compatible chain points.
  - Acceptance: fallback or diagnostic sources never silently change the configured authority model; state-changing decisions fail closed on stale, mismatched, or disagreeing views.
  - Acceptance: Kupo/Ogmios readiness includes network id, era, tip hash/slot, Kupo indexed-through point, Ogmios node tip, and maximum allowed drift.
  - Acceptance: logs/DB records include source mode and exact chain-authority/provider identity for each L1 observation used to finalize or project protocol state.
- [ ] Define a public-testnet slot/time authority and clock-skew policy.
  - Acceptance: public chain-window construction uses provider/Cardano-node chain time as the authority, with local wall clock only after bounded skew validation.
  - Acceptance: startup/readiness fails or degrades when local clock skew exceeds the configured bound or when slot/time conversion is unavailable for the active network.
  - Acceptance: deposit, withdrawal, scheduler, operator lifecycle, commit, and merge validity windows share one audited slot/time conversion module.
  - Acceptance: tests cover clock skew, stale provider slot, epoch boundary, validity upper-bound inclusivity, and Custom/emulator fallback isolation from public profiles.
- [ ] Centralize L1 fee and collateral input policy for all public-testnet protocol transactions.
  - Acceptance: commit, merge, scheduler, operator lifecycle, initialization, deposit, withdrawal, reserve, payout, and proof tx builders use one policy for fee and collateral inputs.
  - Acceptance: selected fee/collateral UTxOs are pure ADA, owned by the intended wallet, not protocol state, not reference-script UTxOs, not datum-bearing, not already locally consumed, and above configured min ADA thresholds.
  - Acceptance: builders verify collateral inputs, collateral return, total collateral, max collateral inputs, fee funding, and min-UTxO behavior after final balancing.
  - Acceptance: operator runbooks and readiness expose fee/collateral wallet balances, fragmentation state, refill thresholds, and failure recovery.

## Submission, Admission, And Validation

- [x] `/submit` accepts canonical Midgard-native transaction CBOR rather than the full canonical+compact envelope.
- [x] The provider computes the submitted tx id locally and checks the node response tx id.
- [x] Durable admission records tx id, canonical bytes, payload hash, and status transitions.
- [ ] Close the durable-admission backlog race.
  - Acceptance: concurrent inserts cannot exceed the configured max pending backlog beyond a documented bounded tolerance.
- [ ] Make canonical tx payload conflict checks strict across all DB paths.
  - Acceptance: duplicate ids with different bytes are rejected consistently wherever tx rows are inserted or replayed.
- [ ] Add public validation rejection metrics by reason.
- [ ] Add adversarial validation fixtures tied to proof categories.
  - Acceptance: invalid transactions rejected by Phase A/B have corresponding proof data expectations where public challenge support is claimed.
- [ ] Confirm validation uses only deterministic data available to public verifiers.
- [ ] Document which validation failures are permanent versus retryable infrastructure failures.
- [ ] Add stress gates for admission processor restart during validation batch claims.
- [ ] Add mixed-batch crash recovery tests.
  - Acceptance: if the process dies after rejections persist but before accepted txs persist, rejected txs remain terminal, accepted candidates are retried or accepted exactly once, and no spent input is resurrected.

## Deposits

- [x] SDK/node deposit builders exist and use hub-oracle/reference-script aware construction.
- [x] Deposit projection and deposit catch-up workers exist.
- [ ] Return public deposit metadata from `/deposit/build`.
  - Acceptance: response includes deposit event id, nonce out-ref/unit, auth unit, valid-to, expected event unit, expected settlement/inclusion timing, and unsigned tx CBOR.
- [ ] Add public docs for wallet funding, nonce selection, validity interval, and retry behavior.
- [ ] Add acceptance coverage for duplicate deposit submission, late deposit, malformed event datum, and insufficient funding UTxOs.
- [ ] Add monitoring for deposit-fetch lag, projection lag, projected/confirmed divergence, and consumed deposit conflicts.
- [ ] Define how external users discover deposit status without internal DB access.
- [ ] Ensure public deposit commands do not default to operator wallets.
  - Acceptance: deposit CLI examples use user-provided seed/env names and never operational wallet env vars as defaults.

## Withdrawals, Reserve, And Payout

- [x] L1 withdrawal-order submission, ingestion, local status, projection/classification, committed withdrawal roots, and valid-withdrawal payout handoff exist.
  - Evidence: `submit-withdrawal`, `fetch-withdrawals-once`, and `withdrawal-status` CLI commands are wired; `withdrawal_utxos` persists withdrawal rows; commit-time ingestion barriers reconcile visible withdrawal UTxOs; block commitment computes `withdrawalsRoot`; valid withdrawals remove the spent L2 ledger UTxO and can feed reserve/payout commands.
- [ ] Centralize withdrawal submit/finalization through the production submit helper.
  - Acceptance: withdrawal L1 submission uses the same recovery, local UPLC evaluation, script-data hash repair, timeout, and confirmation behavior as other production L1 submissions.
  - Evidence: `demo/midgard-node/src/transactions/submit-withdrawal.ts` contains a direct sign/complete/submit path that should be reconciled with the shared production transaction submission path.
- [ ] Provide a public withdrawal build/status surface if withdrawals are intended for public users through node APIs.
  - Evidence: the HTTP router exposes `/deposit/build` and `/deposit-status`, but withdrawal build/status is currently CLI/local-node only.
- [ ] Add withdrawal external-wallet build parity or explicitly reject it as unsupported.
  - Acceptance: either `/withdrawal/build` returns unsigned CBOR plus event metadata for external signing, or public docs state withdrawals require local CLI custody and are not a wallet API.
- [ ] Decide and document whether users submit withdrawal orders through L2 `/submit`, L1 CLI/API, or both.
- [ ] Wire invalid-withdrawal refund submission into node CLI/API if it is part of the public lifecycle.
  - Evidence: `@al-ft/midgard-sdk` exports `buildRefundInvalidWithdrawalTxProgram`, but the node command surface currently wires deposit absorption, valid payout initialization/funding/conclusion, reserve inspection, and payout status rather than an invalid-refund submit command.
- [ ] Add clean public/preprod end-to-end reserve/payout acceptance.
  - Acceptance: deposit reserve absorption, payout initialization, adding reserve funds, payout conclusion, and withdrawal finality are verified from clean deployment.
  - Evidence: the `demo/midgard-node/tests/deposit-flow-emulator-*.test.ts`
    suites cover the representative emulator lifecycle through payout
    conclusion, but public readiness still needs the same lifecycle from a
    clean deployment with public/preprod services.
- [ ] Add payout liveness monitoring.
  - Acceptance: stuck payout, insufficient reserve liquidity, expired withdrawal, and invalid withdrawal states are visible.
- [ ] Add operator runbook for reserve funding, payout batching, fee funding, and failure recovery.
- [ ] Document user-visible withdrawal latency and maturity assumptions.
- [ ] Turn reserve/payout inspection into an operational contract.
  - Acceptance: payout status exposes phase, reserve shortfall, next required operator action, and alertable metrics.
- [ ] Make settlement proof resolution externally consumable or explicitly operator-local.
  - Acceptance: public users/operators can verify deposit/withdrawal inclusion from documented API/artifacts, or the runbook states proof resolution requires privileged node DB access.
- [ ] Decide the tx-order public status and enforce it in exports/docs.
  - Acceptance: tx-order is either documented as unsupported/internal and removed from public examples, or node gets full build/fetch/status/settlement/payout lifecycle coverage for it.

## Operator Onboarding, Liveness, And Incentives

- [ ] Make operator activation match the chosen public participation model.
  - Acceptance: if permissionless, activation can insert any eligible operator key into the active set at the correct ordered anchor, not only append after the current tail.
  - Acceptance: tests cover activation into empty, head, middle, and tail active-set positions, plus stale-anchor retry behavior.
  - Acceptance: if curated, registration/activation is explicitly gated by an allowlist or approval mechanism instead of accidental key-order constraints.
- [ ] Define public-testnet operator funding and faucet policy.
  - Acceptance: docs/tooling state minimum ADA for bond, registration, activation, scheduler refresh, commits, merges, reference scripts, collateral, and recovery transactions.
  - Acceptance: public faucet or manual funding flow prevents one actor from cheaply creating many operators while still allowing legitimate onboarding.
  - Acceptance: onboarding preflight fails with actionable per-wallet shortfall diagnostics before submitting any lifecycle transaction.
- [ ] Implement public-testnet missed-commitment enforcement.
  - Acceptance: an operator/watchdog can detect missed commitments and neglected deposits/withdrawals/tx-orders, build skipped-operator scheduler transactions, increment inactivity strikes, and continue block production.
  - Acceptance: tests cover no-event inactivity, neglected user event inactivity, single-operator strike limit, multi-operator takeover, and partial-slash retirement.
  - Acceptance: metrics expose current operator, shift age, missed commitment age, strike count, next eligible takeover time, and last successful takeover tx.
- [ ] Close duplicate-operator Sybil and scheduler-lock paths.
  - Acceptance: duplicate registration/activation/retirement states are impossible or slashable from every operator set combination.
  - Acceptance: an automated duplicate sweeper or public transaction builder can remove duplicates before they affect scheduler liveness.
  - Acceptance: tests prove duplicate registered/active/retired combinations cannot permanently block scheduler advance, rewind, retirement, or inactivity slashing.
- [ ] Make bond slashing and prover rewards explicit and testable.
  - Acceptance: public parameters define required bond, slashing penalty, inactivity penalty, and prover reward with non-placeholder values or a signed zero-economics rationale.
  - Acceptance: slashing transactions have deterministic value-flow checks or a documented prover-controlled reward construction that external challengers can verify.
  - Acceptance: tests cover active, retired, registered, duplicate, bad-state, bad-settlement, and partially inactivity-slashed operators.
- [ ] Define operator key rotation and compromise response.
  - Acceptance: either implement safe rotate/rekey lifecycle preserving bond holds and scheduler correctness, or document that rotation requires retire, wait for bond unlock, recover, and re-register.
  - Acceptance: compromised active/current operator runbook states how to stop signing, prevent unsafe commits, preserve audit state, and recover or slash as appropriate.
  - Acceptance: tests cover rotation or explicit no-rotation behavior while bond holds, pending shifts, pending commitments, and retired status exist.

## Commit, Confirmation, Merge, And Recovery

- [x] Commit, confirmation, pending finalization journal, local finalization recovery, and merge workers exist.
- [x] State-queue mutation leases exist to prevent overlapping mutation workers.
- [x] Locally finalized blocks persist canonical DA payload records keyed by header hash.
  - Evidence: local finalization builds `DaPayloadV1`, recomputes committed roots/counts, and upserts `da_payloads` before marking the pending finalization complete.
- [ ] Add crash-boundary acceptance tests.
  - Acceptance: restart at each boundary leaves no duplicate committed blocks, no lost mempool transactions, no unbounded local finalization pending state, and no MPF/DB divergence.
- [ ] Make split DB/MPF boundaries explicitly tested.
  - Acceptance: transaction-root MPF reset/replay after DB commit is deterministic and verified by recovery tests.
- [ ] Add exact-count checks for every DB transition that moves canonical tx payloads between mempool, processed, latest, and confirmed tables.
- [ ] Add commit crash test for "submitted on-chain, not yet marked submitted."
  - Acceptance: restart resolves the canonical header by header hash, does not build a competing block on the same base, and eventually finalizes or abandons deterministically.
- [ ] Add submit-recovery parity for deposit-only/user-event-only commits.
  - Acceptance: if a deposit-only commit submit errors after reaching L1, recovery finds the header and preserves the pending journal instead of abandoning a canonical block.
- [ ] Make confirmed-pending observation atomic or explicitly recoverable.
  - Acceptance: crash after deposit/withdrawal projection assignment but before pending journal status update is replay-safe and produces the same final status.
- [ ] Add merge crash test for "merge tx confirmed, local merge job not started."
  - Acceptance: restart detects on-chain queue advancement and reconciles confirmed ledger/blocks without manual DB edits.
- [ ] Add merge crash test for "local merge DB transaction committed, job not completed."
  - Acceptance: recovery can prove whether job effects are complete and either mark complete or safely replay.
- [ ] Add public metrics and alerts for unresolved block submission age, local finalization pending age, merge failure count, and state queue length.
- [ ] Expand `/readyz` with concrete recovery-state diagnostics.
  - Acceptance: readiness extends the current lease and pending-finalization summaries with pending-finalization age, local mutation job ids/ages, processed-mempool depth, mempool/processed overlap count, and BlocksDB-to-ImmutableDB missing payload count.
- [ ] Document manual recovery procedures for pending finalizations, mutation leases, stuck scheduler refresh, and failed merge.
- [ ] Require restart acceptance with persistent Postgres and MPF state before public launch.
- [ ] Add a runbook for intentionally abandoned commits and how public watchers should interpret them.

## Fraud Proofs And Proof Data Availability

- [ ] Close the public fraud-proof coverage matrix for the fixed canonical V1
      consensus surface.
  - Acceptance: every canonical V1 transaction and event feature, including
    script-bearing transactions, mint/burn, observers, redeemers, reference
    scripts, and withdrawal categories, maps to complete proof and acceptance
    evidence. Configuration or deployment manifests cannot disable these
    normative V1 features to bypass the gate.
- [x] Define and expose the current node-produced DA payload shape.
  - Evidence: `DaPayloadV1` includes header hash, header CBOR semantics, sorted UTxO, withdrawal, forced-transaction, L2 transaction, deposit, transition-trace, event-to-step entries, and member counts; the node exposes no HTTP payload route; `demo/midgard-node/src/da/libp2p-producer.ts` serves `payload-by-header` and `payload-chunk` requests over libp2p, and DA committee nodes validate and exchange the payload over the same protocols.
- [ ] Bind public DA to committed headers, L1-visible attestations, and public retrieval guarantees.
  - Acceptance: every public-testnet block header or state-queue append has a verifiable DA commitment or attestation covering full tx payloads, opened field preimages, proof bundle metadata, and transition/proof member counts.
  - Acceptance: the public DA committee/storage layer validates the node-produced payload, signs or otherwise attests the exact header/payload relationship, and serves it independently of the producer node.
- [ ] Define the public-testnet fraud-proof scope explicitly.
  - Acceptance: docs say exactly which proof families are supported on public testnet and which are not.
- [ ] Complete the double-spend proof path end-to-end on preprod if it is the first public fraud-proof milestone.
  - Acceptance: generate invalid block, publish proof data, submit init/steps/conclusion, mint proof token, remove fraudulent block, and verify operator/slashing effects where applicable.
- [ ] Implement proof bundle persistence independent of transient node internals.
  - Acceptance: challengers can reconstruct proof transactions from committed public data and the bundle schema.
- [ ] Define a versioned proof-bundle schema.
  - Acceptance: `ProofBundleV1` includes header hash, root role, root schema version, root, member count, canonical key/value CBOR, membership proof CBOR, optional non-membership/deletion proof, opened field preimages, source payload hash, and verifier ABI version.
- [ ] Add public proof bundle APIs or artifact exports.
- [ ] Publish proof-bundle retrieval APIs beyond `/block`, `/tx`, and raw `DaPayloadV1`.
  - Acceptance: external challengers can fetch block proof bundles, tx root members, membership witnesses, proof families, hashes, pagination, and retention guarantees through stable schemas.
- [x] Add watcher/challenger runbook.
  - Evidence: `docs/fault-proofs/challenger-runbook.md` and
    `docs/fault-proofs/manual-recovery-runbook.md` define fail-closed detection,
    evidence, funding, submission, reconciliation, rollback, and escalation
    procedures for the currently supported workflow surface.
  - Acceptance: an external party can detect an invalid block, fetch data, build the proof, submit transactions, and observe final resolution.
- [x] Implement the manifest-bound watcher/challenger daemon foundation.
  - Evidence: the production watcher application installs 25 catalogue categories with retained-DA/L1 authority, durable runner admission, funding reservations, and reconciliation.
- [ ] Complete and accept the watcher flow for every enabled category.
  - Acceptance: a public challenger can run one process that watches state-queue headers, fetches DA/proof bundles, detects any enabled invalidity, simulates proof transactions, submits steps with retry/resume, and records final resolution. The current application still omits seven categories.
- [ ] Remove or isolate compatibility flags in public fault-proof tooling.
  - Acceptance: no public flow relies on `allowIncompatibleOutput` or old-layout compatibility assumptions.
- [ ] Make proof-family support machine-readable and enforced.
  - Acceptance: `/protocol-info` or `/proof-families` returns each validation rule/reject code with `supported | disabled | unsupported`, proof family id, script hashes, DA requirements, and public-testnet status.
- [ ] Add adversarial fixtures for every claimed proof family.
- [ ] Add public adversarial fixtures that use real validators, not always-succeeds scaffolding.
  - Acceptance: gated emulator/preprod fixtures create invalid committed headers with real deployed validators, publish the same proof bundles an external challenger sees, and prove challenge success plus valid-block non-challenge failure.
- [ ] Add negative tests proving valid blocks cannot be challenged by supported proof paths.
- [ ] Add proof transaction size/budget gates.
- [ ] Add readiness metric for proof-data publication lag.
- [ ] Persist and expose transaction-root proofs at commit time.
  - Acceptance: node stores and serves the exact membership proof for each committed tx root member as committed, with root recomputation checks after restart.
- [ ] Add challenger resume/state model.
  - Acceptance: challenger state stores proof attempt id, current thread UTxO, submitted tx hashes, confirmation status, next action, and safe retry semantics after process restart.

## Protocol Scope Gaps

- [ ] Implement or explicitly exclude the escape hatch from public testnet scope.
  - Acceptance: either real escape-hatch mint/spend validators, initialization, trigger transaction, reduced-bond registration semantics, grace/penalty handling, CLI/runbook, and tests exist; or public-testnet docs and manifest mark escape hatch unsupported and explain liveness assumptions when operators stop committing.
- [ ] Complete the settlement resolution-claim lifecycle or mark settlement resolution claims unsupported.
  - Acceptance: public/operator flows can attach resolution claims, disprove fraudulent claims with deposit/withdrawal/tx-order membership evidence, slash the claimant using canonical protocol economics, remove matured settlements, and recover after restart.
  - Acceptance: tests cover valid claim maturity, false-claim slashing, and no-slash valid claims.
- [ ] Implement withdrawal exact-payability classification.
  - Acceptance: classification simulates/validates the exact Cardano L1 payout output for `l2_value`, `l1_address`, and `l1_datum`.
  - Acceptance: unpayable values are tagged `UnpayableWithdrawalValue`, routed to invalid-withdrawal refund, persisted with diagnostic evidence, and covered by tests for min-ADA, token-bundle, datum, address, and multi-asset edge cases.

## SDK And Client Readiness

- [ ] Add a stable public exports map for `@al-ft/midgard-sdk`.
  - Acceptance: ESM and CJS consumers resolve the intended entrypoints and types.
- [ ] Add SDK package smoke tests against packed tarballs.
  - Acceptance: `import`, `require`, and TypeScript `moduleResolution: nodenext` smoke tests pass against the packed package.
- [ ] Add an SDK public API snapshot gate.
  - Acceptance: CI fails on accidental new/removed top-level SDK exports and verifies no test/internal names leak.
- [ ] Remove test-only or internal exports from public SDK surfaces, or move them under explicitly named internal paths.
- [ ] Add strict diagnostic modes where SDK currently drops invalid UTxOs or proceeds with partial wallet state.
- [ ] Add timeout/abort support to provider requests.
  - Acceptance: `awaitTx` and all nested status/protocol calls can be cancelled cleanly.
- [ ] Add transport timeout tests.
  - Acceptance: tests cover hung fetch, body read hang/failure, timeout classification, and user abort for protocol-info, UTxO, submit, and tx-status requests.
- [ ] Document protocol-info fallback as non-default and unsafe for normal public use.
- [ ] Publish stable provider error contracts.
  - Acceptance: docs define every public `ProviderError.code`, retryability rule, and expected node error JSON for 400/409/413/415/422/429/503/5xx.
- [ ] Document and test tx status semantics.
  - Acceptance: docs include a state machine/table, terminal versus non-terminal states, default `awaitTx` targets, and recommended handling for `pending_commit` and `awaiting_local_recovery`.
- [ ] Add browser/client examples for submit, deposit, withdrawal, status, and retry behavior.
- [ ] Replace in-memory-only examples with public endpoint examples.
  - Acceptance: runnable examples cover real node endpoint setup, submit/status polling, abort/timeout, safe error handling, and package install from the public testnet release channel.
- [ ] Add typed error classes and examples for common public failures.
- [ ] Publish package provenance/versioning policy for public testnet SDK builds.

## Observability And Operations

- [ ] Add alerting rules.
  - Acceptance: alerts cover `/readyz` failure, stale workers, admission backlog depth/age, provider lag, DB saturation, disk pressure, local finalization pending, unresolved block submission, merge failures, deposit/withdrawal lag, proof-data lag, and public error-rate spikes.
- [ ] Disable anonymous admin access for Grafana in public profiles.
- [ ] Keep Loki/Tempo/Prometheus internal or authenticated.
- [ ] Increase log retention and preserve audit trails for tx admission, validation decisions, commits, merges, withdrawals, payouts, and admin actions.
- [ ] Define log redaction policy for seeds, keys, API keys, wallet addresses where appropriate, and provider credentials.
- [ ] Add dashboards for operator health, queue health, L1 provider health, deposits, withdrawals, fraud-proof readiness, and DB state.
- [ ] Add incident runbooks for provider outage, DB outage, invalid block detection, stuck state queue, stuck payout, and redeploy/reset.
- [ ] Add backup/restore procedure and drill for Postgres plus MPF/local state.

## CI, Tests, And Acceptance Gates

- [x] Run build/typecheck/test coverage for public-testnet relevant packages in CI.
  - Acceptance: `pnpm --dir demo install --frozen-lockfile`, builds, typechecks, and tests are run for core, SDK, lucid-midgard, validation, node, and fault proofs.
- [x] Align Aiken compiler versions for the primary contract project, CI workflows, and real checked-in blueprint.
  - Evidence: `onchain/aiken/aiken.toml`, `.github/workflows/aiken-ci.yml`, `.github/workflows/midgard-node-ci.yml`, and the generated `onchain/aiken/plutus.json` reflect the patched Aiken fork `v1.1.23+5adf783` (`Anastasia-Labs/aiken`, tag `midgard-5adf7837`), whose `v1.1.23` release matches the `aiken.toml` declaration.
- [ ] Document and enforce local Aiken install/version checks and artifact rebuild checks outside CI.
- [ ] Add a Docker compose smoke gate.
  - Acceptance: migration service exits successfully, node starts, `/healthz` and `/readyz` pass, and unsafe public ports are not exposed in public profile.
- [ ] Add clean public-testnet e2e acceptance.
  - Acceptance: fresh on-chain deployment plus clean local state completes the full user/operator lifecycle without manual DB edits.
- [ ] Add persistent-state restart acceptance.
  - Acceptance: restart after deposit projection, after mempool admission, after commit submission, after confirmation, during merge, and during payout is safe and deterministic.
- [ ] Add fraud-proof public milestone acceptance.
- [ ] Add load/stress thresholds.
  - Acceptance: admission spam, valid tx throughput, invalid tx throughput, provider rate limiting, DB connection saturation, and API latency have pass/fail thresholds.
- [ ] Add release artifact verification.
  - Acceptance: public images/packages are built from a tagged commit and their hashes are recorded.

## Release Engineering And Supply Chain

- [ ] Pin all public-testnet release inputs by immutable digest or commit SHA.
  - Acceptance: GitHub Actions, container base images, CI service images, compose images, and release build actions use commit SHAs or image digests.
  - Acceptance: CI fails on `ubuntu-latest`, action tags, mutable image tags, or unpinned release inputs in public release workflows.
- [ ] Generate and gate SBOMs for every public artifact.
  - Acceptance: public container images, npm packages, and contract artifact bundles produce CycloneDX/SPDX SBOMs.
  - Acceptance: CI scans SBOMs and images for vulnerabilities and license policy violations; releases fail on untriaged critical/high findings or prohibited licenses.
- [ ] Sign and attest all public-testnet artifacts.
  - Acceptance: container images are signed by digest, npm packages use registry provenance, contract bundles have signed in-toto/SLSA provenance, and operators can verify artifact digest, source commit, builder identity, dependency lock hash, and build command before deployment.
- [ ] Define the public package release channel.
  - Acceptance: all public JS packages are versioned together or by an explicit dependency graph, published from CI to the chosen registry, include `publishConfig` and provenance, replace local tarball install docs, and have post-publish smoke tests installing by registry version.
- [ ] Enforce release dependency specifier hygiene.
  - Acceptance: CI fails public-testnet release builds on `latest`, local `file:` dependencies, unapproved semver ranges in runtime dependencies, unresolved `workspace:*` in packed packages, or lockfiles not regenerated by the approved package manager/version.

## Public Data, Support, And Compliance

- [ ] Publish a public data retention and deletion policy.
  - Acceptance: classify on-chain data, L2 canonical tx data, public index rows, logs, support records, and backups as immutable/public, operationally retained, or deletable.
  - Acceptance: policy states that on-chain/public DA data cannot be deleted; off-chain log/support deletion or anonymization must not corrupt auditability, fraud-proof reconstruction, or state recovery.
- [ ] Define public status retention guarantees.
  - Acceptance: `/submit`, `/tx-status`, `/deposit-status`, withdrawal status, and explorer APIs document minimum retention, terminal-state retention, pruned/expired responses, `retainedUntil`, and support escalation windows.
  - Acceptance: retention applies consistently across DB tables, logs, and backups; pruning never removes data still needed for recovery, fraud proofs, payouts, or user support.
- [ ] Make public logging privacy-safe and deletion-aware.
  - Acceptance: remove raw tx CBOR/body logging from public routes; hash or truncate user addresses/query values unless explicitly needed.
  - Acceptance: automated log redaction tests cover tx CBOR, addresses, request bodies, provider responses, and support IDs.
  - Acceptance: public Loki/log storage has retention, deletion/legal-hold procedure, and access audit logs.
- [ ] Add a public support workflow.
  - Acceptance: every public API response includes a stable request/correlation id.
  - Acceptance: user docs say exactly which identifiers to provide for submit/deposit/withdrawal/payout issues.
  - Acceptance: operators have a safe diagnostic export that redacts private material while including tx id, deposit/withdrawal event id, block/header, request id, status history, and relevant log spans.
  - Acceptance: support severity, ownership, escalation, and response targets are documented.
- [ ] Launch a public status/explorer and incident communications surface.
  - Acceptance: public users can view network health, current limitations, degraded components, latest confirmed/merged block, tx/deposit/withdrawal status, payout status, known incidents, maintenance windows, and post-incident updates without Grafana/admin access.
  - Acceptance: incident templates define severity, user impact, mitigation, rollback/redeploy status, and resolution criteria.
- [ ] Define public-testnet disaster recovery objectives and game days.
  - Acceptance: document RPO/RTO, PITR/WAL archiving, encrypted offsite backups, restore into a fresh host, MPF/Postgres/deployment-manifest consistency checks, provider/indexer rebuild procedure, operator key custody recovery, and failover decision tree.
  - Acceptance: runbook states the exact point where recovery must stop and perform a clean on-chain redeploy/reset instead of restoring local state.

## Documentation And Runbooks

- [ ] Publish a public-testnet operator runbook.
  - Acceptance: setup, config, deploy, register, activate, monitor, commit/merge behavior, restart, backup, upgrade, reset/redeploy, and incident recovery are covered.
- [ ] Publish a public-testnet user runbook.
  - Acceptance: connect wallet, get protocol info, build/submit deposit, submit L2 transaction, check status, withdraw, and understand failure/retry modes.
- [ ] Publish a challenger/watcher runbook.
  - Acceptance: observe blocks, fetch proof data, run proof tooling, submit proof steps, remove fraudulent block, and monitor outcome.
- [ ] Publish API documentation.
  - Acceptance: endpoint request/response schemas, error codes, idempotency, retry semantics, rate limits, and auth boundaries are specified.
- [ ] Publish deployment manifest and contract verification instructions.
- [ ] Document public-testnet limitations.
  - Acceptance: any intentionally unsupported proof families, economics, payout modes, or API surfaces are stated plainly.
- [ ] Document upgrade procedure and compatibility policy.
  - Acceptance: since pre-launch legacy support is not a goal, upgrades should favor explicit redeploy/migration plans over hidden compatibility shims.

## Go / No-Go Checklist

Public testnet can move to go only when all of the following are complete:

- [ ] A clean public-testnet deployment manifest exists and startup verifies it fail-closed.
- [ ] Public ingress is hardened and internal observability/admin surfaces are not internet-exposed.
- [ ] L1 finality, rollback, provider consistency, and clock-skew policies are implemented and tested.
- [ ] Key custody, admin authorization, seed handling, and vulnerability disclosure are production-ready.
- [ ] Operator onboarding, funding, activation, inactivity enforcement, duplicate handling, slashing/rewards, and key-compromise response match the chosen participation model.
- [ ] The full clean deployment acceptance test passes and produces artifacts.
- [ ] Persistent-state restart/recovery acceptance passes.
- [ ] Fraud-proof/proof-data-availability scope is implemented, tested, and documented.
- [ ] Canonical V1 tx-order, withdrawal exact-payability, and all other
      normative protocol-scope gaps are implemented and accepted. Any genuinely
      separate protocol family omitted from the first public testnet is named
      explicitly without disabling canonical V1 behavior.
- [ ] Public SDK/provider packages resolve correctly and have documented examples.
- [ ] Withdrawal/reserve/payout public lifecycle is wired, tested, and documented.
- [ ] CI gates the package, contract, and acceptance checks relevant to public testnet.
- [ ] Release artifacts are pinned, scanned, signed, attested, and published through the selected public channel.
- [ ] Monitoring dashboards and alerts are installed and exercised.
- [ ] Public status/explorer, incident communications, support workflow, retention/deletion policy, and disaster recovery objectives are in place.
- [ ] Operator, user, and challenger runbooks are published.
- [ ] Public-testnet limitations are explicit and do not misrepresent security guarantees.

## Current Answer To "Do We Have All Core Features Implemented?"

No, not for a public testnet definition that includes adversarial safety and public operation. The core honest-path L2 transaction pipeline and all 32 planned fault-proof validator families are present. The remaining pieces are not cosmetic: public testnet readiness requires complete emulator and watcher coverage, public proof-data availability, a current preprod challenge/removal acceptance artifact, reviewed economics with live conservation evidence, hardened ingress, restart/recovery evidence, and complete lifecycle runbooks for users, operators, and challengers.

For a controlled private preprod demonstration, the repo appears much closer: the node can initialize, accept canonical transactions, project deposits, commit blocks, confirm/merge state, and exercise reserve/payout flows when run with the expected local/preprod setup. That should not be presented as public testnet readiness until the blocker checklist above is closed.
