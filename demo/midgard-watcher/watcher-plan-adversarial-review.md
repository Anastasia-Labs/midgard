# Adversarial Review Of Watcher Implementation Plan

Status: Historical review input. Its findings were incorporated into
`midgard-watcher-architecture.md`; it is not a current implementation plan.

Last reviewed: 2026-07-29

Reviewer: historical adversarial production-readiness review

Reviewed files:

- `demo/midgard-watcher/midgard-watcher-architecture.md`
- the former watcher implementation plan, which is not retained as a current
  repository artifact

Additional context consulted by reviewer:

- `public_testnet_readiness.md`
- `demo/midgard-node/docs/PREPROD_DOUBLE_SPEND_FAULT_PROOF_GAP_REPORT.md`
- `demo/midgard-node/docs/FAULT_PROOF_DECISION_RECOMMENDATIONS.md`
- `technical-spec`
- `demo/midgard-validation/src`
- `demo/midgard-node/src/workers/utils/mpf.ts`
- `onchain/aiken/lib/midgard`

## Authoritative L1-Source Clarification

The original finding below is superseded where it treated two providers as an
unconditional acceptance requirement. The watcher retains two explicit,
mutually exclusive vocabulary modes, but the current wire configuration
selects only `external_providers`:

- `local_node`: deferred until a peer-authenticated native adapter exists.
  The retired pathname-based authority constructor and route are not
  selectable; pure local-mode state vocabulary remains for that future adapter.
- `external_providers`: at least two operationally independent providers must
  agree on network and a compatible chain point.

The prelaunch retirement has no compatibility alias. `start` and `replay`
remain unwired, transport-free scaffolds that exit `78`; W10 operational wire
binding and W14 live provenance remain open.

Both modes fail closed on mismatched or stale evidence and propagate
rollbacks. Transient non-agreement and same-point content mismatch preserve
finalized state while quarantining the current decision. An agreed canonical replacement opens a
durable incident, and W13 automatically proves and rewinds the exact branch
within Cardano's fixed `k = 2160` bound before replay resumes; it never
requires manual database surgery. W14 consumes canonical node-derived bytes
and indexes accepted state, including W13-authorized recovery; it does not
duplicate Cardano state-queue validator semantics.

## Verdict

No-go as a production-ready plan in its first draft.
The watcher docs identify many right launch gates, but several were not concrete, testable, or aligned tightly enough with the current Midgard repo.
The Midgard readiness doc is already explicit that public testnet is no-go because fraud-proof/proof-data availability, deterministic deployment identity, finality/rollback, CI, and acceptance gates remain blockers.

## Findings

### 1. Critical: State-transition proofs were not a first-class launch gate

The first plan verified final `utxos_root`, but did not require a committed transition trace, step index, exact-once coverage, or per-step challenge path.
Local transaction faults are insufficient because an operator can commit individually valid transactions with a wrong post-state root.

Recommendation:

- Add a milestone before verifier/proof implementation for `transition_trace_root`, `transition_trace_index_root`, step schema, exact-once challenges, ordering challenges, and Aiken/TypeScript conformance/budget tests.

Disposition:

- Incorporated into Task 0.4 and referenced from root reconstruction and critical path.

### 2. Critical: DA was under-specified for independent challengers

The first DA task defined a payload, but did not require deterministic state-queue header reconstruction from public payload bytes, non-circular header/payload binding, certificate contents, or a rule that missing DA blocks append/merge or are directly slashable.
The technical spec's DA section remains aspirational Leios/Mithril design.

Recommendation:

- Make DA security mode, retention, permissionless retrieval, `challenge_window + finality + proof_margin`, and missing-DA challenge semantics launch blockers.

Disposition:

- Incorporated into Task 0.6 and strengthened Task 4.1.

### 3. Critical: Consensus feature gating was missing

The plan could exclude features from the security claim, but that is not enough if the deployed protocol still admits unprovable features.
Current validation accepts or evaluates mint, Plutus/MidgardV1 scripts, observers, redeemers, protected receives, budgets, and value preservation.

Recommendation:

- Add a manifest-enforced feature matrix: every enabled ledger feature must have end-to-end proof support, or be rejected/disabled in consensus.

Disposition:

- Incorporated into Task 0.5, manifest criteria, and block decision criteria.

### 4. Critical: L1 finality and provider consistency were not strong enough

The first plan accepted an ambiguous "at least one provider, with a path to
multiple" policy and did not distinguish a watcher-operated full node from
externally operated providers.

Recommendation:

- Require an explicit `local_node | external_providers` source discriminator.
  The prelaunch wire parser currently accepts only `external_providers`, which
  requires at least two
  operationally independent providers to agree on network and compatible chain
  point. `local_node` remains deferred: a native adapter must authenticate the
  connected peer before that route can be selected. Both modes retain explicit
  finality, non-terminal quarantine for transient evidence, and automated W13
  recovery from an agreed finalized rollback within Cardano `k = 2160`.

Disposition:

- Incorporated into W01 and W10–W14 with the authoritative source-mode
  discriminator.

### 5. High: Spec, implementation, and proof roots required one conformance rule

The original review found divergent transition order and root representations.
The current TeX block spec and node agree on withdrawals, forced transaction
orders, normal L2 transactions, then deposits; same-block deposit spending is
rejected. Aiken proof binding to the current typed/counted roots is still
incomplete.

Recommendation:

- Keep Milestone 0 stronger than "document exists": require generated
  TypeScript/Aiken/live-block golden vectors and clean redeploy criteria so
  transition order and roots cannot drift again.

Disposition:

- Incorporated into Tasks 0.2 and 0.3.

### 6. High: Double-spend E2E can pass without proving acceptedness

Phase A rejects non-`TxIsValid`.
The current native compact verifier checks that the validity code is well formed, but does not necessarily assert that it equals `TxIsValid`.

Recommendation:

- Task 7.3 must require `TxIsValid` assertions in all accepted-transaction proof helpers and mutation tests with invalid validity codes.

Disposition:

- Incorporated into Tasks 5.3 and 7.3.

### 7. High: Acceptance gate was too narrow

The first Task 10.2 required only one proof-covered invalid block.
That proves a demo path, not production coverage.

Recommendation:

- Require E2E emulator tests for every claimed proof family, gated preprod tests for all launch-scope families, negative tests proving valid blocks cannot be challenged, and artifacts from public DA/proof bundles.

Disposition:

- Incorporated into Task 10.2.

### 8. High: Fraudulent-header removal concurrency was under-specified

The first plan covered tail/non-tail tests, but not live state-queue races, refetch-after-each-confirmation, successor pruning order, or per-removed-block slashing under concurrent commits/merges.

Recommendation:

- Add adversarial removal orchestration tests against changing topology, stale UTxOs, concurrent append/merge attempts, and active/retired/already-slashed operators.

Disposition:

- Incorporated into Task 8.3.

### 9. High: Protocol economics and timing were not production-planned

Current parameters still include TODOs, very short `maturity_duration`, and zero slash/reward values in testnet settings.

Recommendation:

- Add a quantitative proof-deadline model: worst-case proof steps, L1 confirmation depth, retries, fee congestion, DA fetch, and rollback margin must fit inside maturity with measurable slack.

Disposition:

- Incorporated into Task 0.7.

### 10. Medium: Manifest trust and provenance were incomplete

The manifest task listed many fields, but not a full script parameterization graph, DA mode, feature gates, release commit, signer trust root, threshold/rotation policy, or manifest signing authority.

Recommendation:

- Treat manifest generation, signing, verification, and key custody as a dedicated release-security milestone.

Disposition:

- Incorporated into Tasks 1.1 and 1.3.

## Reviewer Summary

The strongest immediate correction is to move proof coverage, DA/header binding, transition trace design, consensus feature gates, and finality/provider policy into Milestone 0/1.
Building a broad verifier before those are frozen risks producing a watcher that can detect real faults it cannot prove or stop.
