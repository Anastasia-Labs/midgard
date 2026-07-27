# Canonical V1 Goal Progress

## Baseline

- Starting revision: `d5f36df25a9a1696e4df857e01aa81d2f0b6ef96`.
- Starting branch: `codex/tx-validation-capability-checkpoint`.
- Specification authoring revision: `d5f36df25a9a1696e4df857e01aa81d2f0b6ef96`.
- Graphify indexed revision: `320ed869262dba7f4aac5627f1bd9efa0b5618a6`
  (stale; navigation hints only).
- Starting dirty state (`git status --porcelain=v1 --branch`):
  - ` M onchain/aiken/lib/midgard/cek-data-traverse-v1.ak`
  - ` M onchain/aiken/lib/midgard/redeemer-item-proof-v1.ak`
  - `?? GOAL_SPEC.md`
  - `?? onchain/aiken/lib/midgard/script-sources-redeemer-normalization-v1.ak`
  - `?? onchain/aiken/lib/midgard/script-sources-redeemer-normalization-v1.test.ak`
  - `?? onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-envelope-v1.ak`
  - `?? onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-finalize-frame-executor-v1.ak`
  - `?? onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-fold-map-executor-v1.ak`
  - `?? onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-outer-normalizer-v1.ak`
  - `?? onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-traversal-normalizer-v1.ak`
- Protected pre-Goal paths and starting SHA-256:
  - `GOAL_SPEC.md` — `18cb46a4c2dd0ec6eb0e605044e47a760f51975ecceb23d63b65ce67593e8e19`;
    authoritative external pre-Goal state, parent-owned.
  - `onchain/aiken/lib/midgard/cek-data-traverse-v1.ak` —
    `0788e9de32d5e5353007aa6e95fb47bf3bca729a929d337d1653b0234b20841f`.
  - `onchain/aiken/lib/midgard/redeemer-item-proof-v1.ak` —
    `d20490b06e548a0112ecd3d36187fc0de1b6c6c1c0bbb1a9d3ecdc9d984221c3`.
  - `onchain/aiken/lib/midgard/script-sources-redeemer-normalization-v1.ak` —
    `afe358b315db698111b0935633888a71a9c962456ba475b8a33da908ea3f4a57`.
  - `onchain/aiken/lib/midgard/script-sources-redeemer-normalization-v1.test.ak` —
    `5bec5dbf14b919f08697c9502dbb6b2e9f072332ad4fefde4c19a2565db6fa2b`.
  - `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-envelope-v1.ak` —
    `86fb46064d984086429b591c2b2457ee03dd33bb161030e2f4f55392dc134114`.
  - `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-finalize-frame-executor-v1.ak` —
    `ac9981696cee44fe137e48964a3796beb57ca2f076d6aadeb1fae230ceff1c6f`.
  - `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-fold-map-executor-v1.ak` —
    `10c00875852f0405041a2039033af55790e8f42f75ba92f97cbbc64dae299608`.
  - `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-outer-normalizer-v1.ak` —
    `581a87203def04a2e998d3fec0df9372745743ebc819c61556a2b9618b967f4b`.
  - `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-traversal-normalizer-v1.ak` —
    `08781adad00701104ee2ab652e19f5c6540c76ca4268ea29f6d8711152fd99bc`.
- Dirty-path ownership: all protected Aiken bytes are an intentional,
  uncommitted stage-one redeemer feasibility checkpoint from prior Codex task
  `019f8ca7-e935-7730-89d4-b46b7bf1e3cd`. They are not Goal-owned unless the
  user explicitly hands that checkpoint to this Goal after review. No task may
  edit, stage, commit, regenerate over, or claim these paths.
- Historical checkpoint context, not current-tree PASS evidence: five
  parameterized scripts previously built at no more than 12,500 raw bytes
  (Finalize 9,335 bytes; traversal normalizer had the tightest 614-byte
  margin); 12 Finalize/common-ABI focused guards passed; six of nine FoldMap
  regressions passed through
  `fold_map_executor_rebind_rejects_wrong_family_identity_and_action`. The
  next route selector was interrupted and has no result. Every historical
  command requires final-tree replay.
- Sensitive pre-existing local path: `demo/midgard-node/.env` exists and is
  never a Goal edit or evidence artifact. Only variable presence was
  inventoried; no credential value is recorded here.
- Toolchain observed at the baseline:
  - Git `2.43.0`; Nix `2.32.0`; GNU Make `4.3`; Docker `29.2.0`;
    `cardano-cli 11.0.0.0`.
  - Host Node `v24.13.1`, host pnpm `10.18.3`.
  - `nix develop ./demo --command bash -c 'node --version && pnpm --version'`
    resolves Node `v22.22.2` and pnpm `9.15.9`.
  - `demo/package.json` declares Node `>=22.16.0` and pnpm `9.15.4`.
  - Host Aiken is `v1.1.22+39d6b04`; `onchain/aiken/aiken.toml` declares
    compiler `v1.1.21`. The mismatch must fail the final compiler identity
    gate until the pinned compiler is used.
  - Root `nix develop --command ...` currently fails because the repository
    root has no `flake.nix`; the existing demo flake succeeds when selected
    explicitly. F40 must provide the required final-tree repository command.
  - No live Aiken or focused-check process remained after the provenance
    handoff; the process search matched only its own sandbox wrapper.
- External configuration and credential availability (presence only; funding,
  authorization, freshness, and provider operation are not yet proven):
  - `demo/midgard-node/.env` configures `NETWORK=Preprod`,
    `L1_PROVIDER=Kupmios`, and `RUN_GENESIS_ON_STARTUP=false`.
  - Set: primary and fallback Blockfrost keys, operator and merge seed
    phrases, reference-script seed phrase, user seed phrase, and DA libp2p key
    source.
  - Absent: `DA_L1_SUBMITTER_KEY_SOURCE`, required before a live DA signer can
    submit L1 transactions.
  - No Midgard/Kupmios acceptance topology is running. Existing Docker
    containers belong to unrelated Signal and Cardano-mainnet projects and are
    protected from this Goal.

## Criterion ledger

| Criterion | Status | Exact final-tree evidence |
| --- | --- | --- |
| AC-00 | IN_PROGRESS | Ledger initialized; all downstream evidence remains open. |
| AC-01 | IN_PROGRESS | Protected baseline and hashes recorded; Goal commits and final relative-clean proof remain open. |
| AC-02 | TODO | F01/F02 and final ABI gate required. |
| AC-03 | TODO | Final release identity and digest required. |
| AC-C10 | TODO | CG1 required. |
| AC-C20 | TODO | CG2 full P2 matrix required. |
| AC-C21 | TODO | Whole-item production searches and ABI tests required. |
| AC-C30 | TODO | CG3 resolver sweep required. |
| AC-C31 | TODO | Enabled semantic surface proof required. |
| AC-C40 | TODO | CG4 classification/forced sweep required. |
| AC-C50 | TODO | CG5 release evidence required. |
| AC-C60 | TODO | CG6 fresh target-testnet acceptance required. |
| AC-Q10 | TODO | QG1 total coverage required. |
| AC-Q11 | TODO | Atomic closure for every family required. |
| AC-Q12 | TODO | Native V1 binding and cross-language equivalence required. |
| AC-Q13 | TODO | Catalogue/deployment exactness required. |
| AC-Q14 | TODO | Unified resumable public-evidence workflow required. |
| AC-Q15 | TODO | Correction topology lifecycle matrix required. |
| AC-Q16 | TODO | Non-placeholder economics and duplicate-reward prevention required. |
| AC-Q17 | TODO | Retention and bond-backed availability lifecycle required. |
| AC-Q18 | TODO | QG1, QG2, and QG3 required. |
| AC-W10 | TODO | Production watcher package and gates required. |
| AC-W11 | TODO | Public authenticated trust-boundary proof required. |
| AC-W12 | TODO | Provider/finality/rollback adversarial evidence required. |
| AC-W13 | TODO | Deterministic reconstruction/replay evidence required. |
| AC-W14 | TODO | Canonical decision totality evidence required. |
| AC-W15 | TODO | Total deterministic family adapters required. |
| AC-W16 | TODO | Durable actuation/reconciliation evidence required. |
| AC-W17 | TODO | Offline byte-identical replay evidence required. |
| AC-W18 | TODO | Operations, API, metrics, alerts, and runbooks required. |
| AC-W19 | TODO | WG1 and WG2 acceptance required. |
| AC-X10 | TODO | Enabled-feature/proof/watcher totality required. |
| AC-X11 | TODO | Measured end-to-end maturity margin required. |
| AC-X12 | TODO | One-revision reproducible evidence required. |
| AC-X13 | TODO | Final anti-shortcut evidence audit required. |

## Task queue

| Task | Dependencies | Owner | Leased paths | Status | Commit | Focused verification |
| --- | --- | --- | --- | --- | --- | --- |
| F00 | none | parent | `GOAL_PROGRESS.md` only; all baseline dirty paths protected | PASS | pending Goal checkpoint | Revision/branch/status, SHA-256 inventory, tools, credential setness, graph staleness, and process absence recorded above. |
| F01 | F00 | `/root/f01_feature_inventory` | read-only repository inventory; no writes | IN_PROGRESS | n/a | Source paths and machine-readable feature inventory gap report. |
| F02 | F00 | `/root/f02_abi_registry` | read-only repository ABI audit; no writes | IN_PROGRESS | n/a | Exact format/tag/arity/legacy gap report. |
| F03 | F00 | parent | `GOAL_PROGRESS.md`; read-only provider/runbook/source inspection | PASS | pending Goal checkpoint | Preprod/local-Kupmios submission route, independent-provider gap, effective/future parameter commands, chain-point query, finality gap, credentials, and safe preflight commands identified. |
| F10 | F01–F02 | unassigned | none until first-wave integration | BLOCKED | n/a | Revalidate P0/P1/P2 current claims. |
| F20 | F01–F02 | unassigned | none until first-wave integration | BLOCKED | n/a | Reconcile every proof row/catalogue/tool/test. |
| F30 | F00–F02 | parent after F02 | none until first-wave integration | BLOCKED | n/a | Public-data-only watcher dependency/source map. |

## Decisions

- Canonical V1 and all required enabled features follow GOAL_SPEC.md §3.1;
  no compatibility, feature-disable, proof-convenience, or weaker-testnet
  shortcut is permitted.
- Pre-existing dirty Aiken checkpoint bytes remain protected despite their
  relevance to P2. Provenance does not grant implementation ownership.
- Historical tests and size observations are orientation only until replayed
  against the final source, pinned compiler, generated blueprint, parameter
  snapshot, and release identity.
- F03 authority preflight:
  - Target acceptance network is Preprod. The state-changing node route is
    local Kupmios, with configured local Ogmios/Kupo endpoints, as required by
    the E2E skill.
  - The current manifest builder calls the configured provider's
    `getProtocolParameters()` and hashes that snapshot
    (`demo/midgard-node/src/commands/contract-deployment-info.ts:637-685`),
    but it does not bind effective versus pending parameters, provider
    identities, or the source chain point.
  - `cardano-cli 11.0.0.0` provides the required read-only primitives:
    `latest query protocol-parameters`, `latest query future-pparams`, and
    `latest query tip`, each against an explicit socket/network, with
    immutable-tip selection available for future parameters and tip. The
    repository has no wrapper that captures all three into canonical release
    evidence yet; C70/F40 must add it.
  - Blockfrost primary/fallback credentials are configured, but two keys for
    one provider are not two independent watcher sources. No second
    provider-neutral watcher adapter or same-chain-point policy exists.
  - Canonical block maturity is seven days in the consensus profile/Aiken
    ledger constants. A release-bound Cardano confirmation depth,
    two-provider compatibility rule, pre-depth rollback behavior, and
    post-finality incident policy exist only in watcher architecture prose,
    not production source.
  - Live credential gaps remain `DA_L1_SUBMITTER_KEY_SOURCE`, proof of funded
    wallet/collateral, proof of local Preprod provider synchronization, and a
    second independent watcher provider. These affect P6 only and do not
    block local work.
- Current-truth reconciliation before implementation assignment:
  - The live P2 matrix has only `General byte blobs/chunks` and nested output
    `Value` at full-row `PASS`. Ordered fields, Data breadth/depth, script
    envelopes/program material, and incremental CBOR scans remain
    `PARTIAL`/`OPEN`. Its recorded focused results are historical until
    final-tree replay, and the protected redeemer checkpoint cannot be
    credited or edited.
  - Fault-proof status documents are stale (the catalogue report was audited
    at `269bf6b3`). Current source registers exactly six categories:
    `doubleSpend`, `nonExistentInput`, `nonExistentInputNoIndex`,
    `invalidRange`, `transitionTrace`, and `validationTraceDispute`.
    Numerous compiled legacy family directories remain unregistered and
    untooled; the Q23–Q49 launch-scope families are not implemented as
    independently closed catalogue families.
  - The current `min-fee/step-02.ak` still computes minimum fee as literal
    zero, while TypeScript Phase A uses `minFeeA * canonicalTxSize + minFeeB`.
    No matrix claim can promote Q20/C49.
  - The fault-proof CLI exposes separate manual chains for double-spend,
    invalid-range, non-existent-input, and validation disputes plus removal;
    it has no one-command, journaled, all-family public-evidence workflow.
  - `demo/midgard-watcher` contains only two Markdown files and is absent from
    `demo/pnpm-workspace.yaml`. `demo/da-committee-node/package.json`
    currently uses the package/bin identity `midgard-watcher`; therefore the
    independent production watcher required by W00 does not exist.

## Validation ledger

| Command | Revision/artifact identity | Result | Count/duration |
| --- | --- | --- | --- |
| `sed` bounded reads covering all 1,060 lines of `GOAL_SPEC.md` | baseline SHA-256 `18cb46...8e19` | PASS; full authoritative specification read | 1,060 lines |
| `git rev-parse HEAD`; `git branch --show-current`; `git status --porcelain=v1 --branch` | starting tree | PASS; values recorded in Baseline | n/a |
| `sha256sum` over every starting dirty path | starting tree | PASS; ten hashes recorded | 10 paths |
| host tool version probes | starting environment | PASS with declared Aiken mismatch recorded | 8 tools |
| `nix develop --command bash -c 'node --version && pnpm --version'` | repository root | FAIL; no root flake | 0 tools resolved |
| `nix develop ./demo --command bash -c 'node --version && pnpm --version'` | `demo/flake.lock` at baseline tree | PASS; Node `v22.22.2`, pnpm `9.15.9` | 2 versions |
| redacted `.env` setness inventory | pre-existing local configuration | PASS; values not read into ledger | 8 required credential names |
| `docker ps --format '{{json .}}'` | local Docker daemon | PASS; no Midgard acceptance topology; unrelated projects observed and protected | 4 unrelated containers |
| process search for Aiken/focused runner | current host | PASS; no actual build/test process remained | 0 relevant processes |
| `node .agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.mjs` | baseline skill/finalizer sources | PASS | 17 referenced commands, 11 required steps, 9 transaction labels |
| Cardano CLI query-help inspection | `cardano-cli 11.0.0.0` | PASS; effective, future, and tip primitives identified without network mutation | 3 commands |
| source reconciliation of P2 matrix, fault catalogue/CLI/min-fee, workspace, and watcher paths | starting tree | PASS as before-state inventory; no acceptance criterion promoted | 4 material gap clusters |

## Current next action

Integrate the read-only F01/F02 findings, expand the durable task queue, then
launch only dependency-ready F10/F20 while the parent performs F30.

## Blockers

None proven. Missing `DA_L1_SUBMITTER_KEY_SOURCE`, provider operation, wallet
funding/collateral, and a second watcher provider are F03 preflight gaps, not
current blockers; all local work remains ready.
