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
| AC-02 | IN_PROGRESS | F01 inventory is machine-readable and fail closed; F02 audit found unresolved one-way/non-exhaustive cross-language vectors and fail-open external parsers, so the final ABI gate remains open. |
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
| F00 | none | parent | `GOAL_PROGRESS.md` only; all baseline dirty paths protected | PASS | `dde4b789` | Revision/branch/status, SHA-256 inventory, tools, credential setness, graph staleness, and process absence recorded above. |
| F01 | F00 | `/root/f01_feature_inventory`; parent integration | agent read-only; parent owns `docs/exec-plans/evidence/canonical-v1-feature-inventory-v1.json` | PASS | `c1f4a800` | Machine-readable inventory validates 14 unique enabled features, 45 existing source surfaces, correction/proof gaps, an empty watcher surface, and fail-closed unknown behavior. |
| F02 | F00 | parent integration; initial audit by `/root/f02_abi_registry` | parent-owned registry/ABI integration surfaces | IN_PROGRESS | n/a | Audit completed without edits and found the gate failing: non-exhaustive one-way TS→Aiken vectors, fail-open protocol-info/runtime-manifest parsers, an incomplete prose registry, duplicated deployment digest logic, and uninspected format families. |
| F02-R | F02 audit | parent | `docs/exec-plans/evidence/canonical-v1-format-registry-v1.json`, `demo/scripts/verify-canonical-v1-format-registry.mjs` | IN_PROGRESS | `a0bb3767` plus worktree | Registry contains all 132 prose IDs in canonical order and unknowns fail closed. Structural mode passes; default release mode rejects 123 open rows. C01/C02/C03/C04/C05/C10/D19/D20/A22 are source-verified PASS; every remaining row must still supply exact source/symbol, wire field/tag/arity, parser/encoder, positive/rejection, cross-language/N/A, and obsolete-branch evidence. |
| F02-P | F02 audit | `/root/f02_protocol_info_exact`; parent reviewed | exclusive lease released after edits to `demo/lucid-midgard/src/provider/payload.ts`, `demo/lucid-midgard/tests/provider.test.ts` only | PASS | `c1f4a800` | `/protocol-info` rejects unknown root and all nested keys while preserving exact current payload acceptance; 3 focused tests, typecheck, leased-file lint, and diff check pass. |
| F02-D | F02 audit | `/root/f02_da_manifest_exact`; parent integration | exclusive lease released after edits to `demo/midgard-core/src/da-transport.ts`, its DA test; `demo/midgard-node/src/da/libp2p-runtime-manifest.ts`, `demo/midgard-node/src/da/libp2p-producer.ts`, producer test; `demo/da-committee-node/src/config.ts`, its config test | PASS | `c1f4a800` | One exact six-root-key parser serves generator/producer/watcher and binds watcher network to verified deployment/override; pinned Node 22 replay passed core 8/8, node 25/25, watcher 20/20, core/watcher/node compilation, and focused lint. |
| F02-I | F02 audit | `/root/f02_manifest_digest_single`; parent integration | exclusive lease released after edits to `demo/midgard-core/src/deployment-manifest-identity-v1.ts`, its direct test, `demo/midgard-node/src/deployment-manifest-v1.ts`, and its direct test | PASS | `c1f4a800` | Core solely owns JSON normalization/stable serialization/digest; node directly re-exports it. Parent pinned replay passed core 4/4, node 9/9, package compilation/typechecks, and focused lint. |
| F02-A | F02 audit | parent | `demo/midgard-core/src/codec/native.ts`, `demo/midgard-sdk/src/common.ts`, `demo/midgard-sdk/tests/proof-abi.test.ts`, `demo/midgard-node/src/workers/utils/mpf/phas.ts`, `demo/midgard-node/tests/sdk-aiken-schema-parity.test.ts` | PASS | `c1f4a800` | Removed extra `Neighbor` constructor; exact proof CBOR passes 2/2; recursive current-blueprint parity and raw validity-code/Plutus binding pass 26/26; core/SDK builds/typechecks, node `tsc`, and focused lint pass. |
| F02-N10 | F02 audit | `/root/f02_partial_witness_bundle`; parent integration | parent integration paths: `demo/lucid-midgard/src/builder/witness-bundle.ts`, `demo/lucid-midgard/src/builder.ts`, `demo/lucid-midgard/tests/partial-signing.test.ts`, `demo/lucid-midgard/tests/api-export-snapshot.test.ts` | PASS | `c1f4a800` | Sole public/wire `MidgardPartialWitnessBundleV1`; exact seven-field/two-wrapper schema, both versions `1`, lowercase hex, strict order, duplicate rejection, and tx/body binding pass 8/8 focused/API tests plus build/typecheck/lint. |
| F02-DS | F02 audit | `/root/f02_da_store_exact`; parent integration | released lease: `demo/da-committee-node/src/domain.ts`, `demo/da-committee-node/src/store.ts`, `demo/da-committee-node/src/store/postgres.ts`, `demo/da-committee-node/src/peer/signatures.ts`, `demo/da-committee-node/tests/postgres-store.test.ts`, `demo/da-committee-node/tests/store-factory.test.ts`, `demo/da-committee-node/tests/watcher.test.ts`, `demo/da-committee-node/tests/peer-coordinator.test.ts` | PASS | `a0bb3767` | D19/D20 require exact persisted V1 payload/signature records on JSON/Postgres reads and writes; missing/non-1/legacy/extra/malformed fields reject. Agent focused suite passed 41/41 including Postgres 17.2; pinned typecheck/build/lint/format/diff and parent fixture replay pass. |
| F02-DS-B | F02-DS | parent | `demo/da-committee-node/src/da/libp2p/attestations.ts`, `demo/da-committee-node/src/coordinator/submitter-reconciler.ts`, three directly dependent fixture tests | PASS | `a0bb3767` | Peer/local attestation producers and retained fixtures include canonical `validationTracesRoot` and `validationTraceCount`; mandatory envelopes are used by the three-peer integration. Pinned typecheck and 12/12 payload/proof/startup plus 1/1 three-peer tests pass. |
| F02-ART | F02 audit | `/root/f02_artifact_registry` | released read-only lease over artifact producers/readers/tests/docs | PASS | n/a | Source-classified A03–A23 without edits. A16 and parent-integrated A22 are deleted; every other family’s exact field language, boundary, validation gaps, tests, and non-overlapping repair wave is recorded. This audit task passes while F02 remains open on its discovered defects. |
| F02-A22 | F02-ART discovery | parent | `demo/midgard-node/package.json`, `demo/midgard-node/tests/da-multi-peer-integration.test.ts` | PASS | `a0bb3767` | Removed the retired Phase-5 package command, external 50k envelope/report reader, measurement collection, and both evidence emitters; made both retained integration payloads use mandatory V1 envelopes. Executable absence scan, JSON parse, lint, DA typecheck, and the real three-peer quorum/rejection/recovery integration pass. |
| F02-C03 | F02 audit | parent | `onchain/aiken/lib/midgard/cek-proof-v1.ak`, `onchain/aiken/lib/midgard/canonical-version-tuple-v1.test.ak`, registry row C03 | PASS | pending | The complete 27-field TypeScript tuple pins 24 numeric members to `1` and three exact V1 identities; exact-profile parsing rejects non-V1 mutations. A pinned Aiken vector passes exactly 1/1 over all 26 explicit corresponding on-chain V1 constants, including the now-named CEK envelope version. |
| F02-C10 | F02 audit | parent | `demo/midgard-core/tests/output-codec.test.ts`, `onchain/aiken/lib/midgard/script-proof-v1.test.ak` | PASS | `a0bb3767` | TS and Aiken independently pin language tag `128` and the same `blake2b-224(tag || script)` hash; TS also pins the exact `821880` array/tag prefix and rejects unknown tag `129`. Pinned TS file passed 11/11 and pinned Aiken exact selector passed exactly 1/1. |
| F03 | F00 | parent | `GOAL_PROGRESS.md`; read-only provider/runbook/source inspection | PASS | `dde4b789` | Preprod/local-Kupmios submission route, independent-provider gap, effective/future parameter commands, chain-point query, finality gap, credentials, and safe preflight commands identified. |
| F10 | F01–F02 | unassigned | none until F02 acceptance | BLOCKED | n/a | F01 is complete; F02 acceptance remains unmet, so P0/P1/P2 claim reconciliation is not dependency-ready. |
| F20 | F01–F02 | unassigned | none until F02 acceptance | BLOCKED | n/a | F01 is complete; F02 acceptance remains unmet, so proof-row reconciliation is not dependency-ready. |
| F30 | F00–F02 | parent after F02 | none until F02 acceptance | BLOCKED | n/a | F00 is complete; F02 acceptance remains unmet, so the watcher dependency/source map is not dependency-ready. |

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
- F01 is represented by
  `docs/exec-plans/evidence/canonical-v1-feature-inventory-v1.json`, not by
  optimistic matrix promotion. It records all 14 compiled enabled features,
  current source surfaces, partial/ambiguous states, missing proof families,
  correction gaps, and the absent watcher surface. Its unknown behavior is
  explicitly fail closed.
- F02 cannot be marked `PASS` merely because its discovery pass completed.
  Current source does not yet satisfy the task acceptance:
  - cross-language ABI evidence is TS-to-Aiken only and covers two generated
    cases rather than every tag and arity;
  - `/protocol-info` silently discards unknown root/nested JSON keys;
  - producer and watcher accept different root languages for the same DA
    runtime manifest, and the watcher does not bind its network field;
  - the prose format registry omits exact fields/tags/arities/parsers/domains
    and vectors while claiming completion;
  - deployment JSON normalization/digest logic is duplicated; and
  - multiple serialized format families remain uninspected and therefore
    fail closed.
  The narrow external-parser repairs are leased first; registry generation,
  bidirectional vectors, persistence coverage, digest consolidation, and the
  final obsolete-branch scan remain parent integration work before F02 can
  pass.

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
| `pnpm --dir demo/midgard-core exec vitest run tests/consensus-profile-v1.test.ts tests/capability-parity-v1.test.ts tests/deployment-manifest-identity-v1.test.ts --reporter=verbose` | `dde4b789` plus protected baseline | PASS; release gate remains correctly unset, manifest extra/tampered fields reject, and incomplete/unknown parity fails closed | 3 files, 14 tests, 3.34 s |
| F01 source inventory plus `jq` schema/uniqueness/enabled/fail-closed checks and path-existence audit | `docs/exec-plans/evidence/canonical-v1-feature-inventory-v1.json` SHA-256 `44ebaedd...06e23` at `dde4b789` plus parent worktree | PASS; every registered source path exists and no downstream criterion was promoted | 14 unique enabled features, 45 paths |
| F02 read-only source audit and protected-path hash replay | production source unchanged from starting revision; current HEAD `dde4b789` | FAIL for F02 acceptance; five blocking gap clusters and uninspected fail-closed families recorded in Decisions | 0 edits; 10 protected hashes unchanged |
| `jq empty docs/exec-plans/evidence/canonical-v1-feature-inventory-v1.json`; `git diff --check` | parent integration worktree at `dde4b789` | PASS | 1 JSON artifact; 0 whitespace errors |
| `pnpm --dir demo/lucid-midgard exec vitest run tests/provider.test.ts -t 'accepts the exact current protocol-info shape\|rejects unknown root protocol-info fields\|rejects unknown nested protocol-info fields' --reporter=verbose`; package typecheck; leased-file ESLint; diff check | F02-P parent-reviewed worktree at `dde4b789` | PASS; exact current shape accepted and unknown root/nested mutations fail closed | 1 file, 3 passed, 22 intentionally unselected; 9 ms tests |
| `nix develop ./demo --command pnpm --dir demo/midgard-core run build`; corresponding `midgard-sdk` build | F02-D/F02-I/F02-A integration worktree at `dde4b789` | PASS; canonical declarations restored under Node `22.22.2` / pnpm `9.15.9` | 2 package builds |
| Focused F02-I core/node deployment-manifest tests | F02-I integration worktree at `dde4b789` | PASS; one core normalizer/digest implementation, direct node delegation, exact/tamper vectors | core 4/4; node 9/9 |
| `nix develop ./demo --command pnpm --dir demo/midgard-sdk exec vitest run tests/proof-abi.test.ts --reporter=verbose` | F02-A integration worktree at `dde4b789` | PASS; exact Branch/Fork/Leaf CBOR and obsolete double-wrapped neighbor rejection | 2/2 |
| `nix develop ./demo --command pnpm --dir demo/midgard-node exec vitest run tests/sdk-aiken-schema-parity.test.ts --reporter=verbose` | F02-A integration worktree against current blueprint | PASS; recursive constructor/tag/arity/field parity plus raw-validity-code/Plutus-constructor binding | 26/26 |
| Core and SDK package typechecks under `nix develop ./demo` | F02 integration worktree at `dde4b789` | PASS | 2 packages |
| Node package typecheck under `nix develop ./demo` | concurrent F02-N10 worktree | FAIL before node compilation because active leased N10 edit caused a Lucid DTS union-narrowing error; routed to owning agent, no product verdict | 1 leased-file diagnostic |
| Pinned-toolchain F02-D focused replay: core DA transport, node producer/runtime manifest, watcher config | integrated F02-D worktree at `dde4b789` | PASS; shared exact parser and deployment/network binding proved | core 8/8; node 25/25; watcher 20/20 |
| `nix develop ./demo --command pnpm --dir demo/da-committee-node run typecheck`; focused F02 lint | integrated F02 worktree | PASS | watcher typecheck; 18 leased files linted |
| Lucid build; partial-signing/API snapshot tests; typecheck; focused lint | integrated F02-N10 worktree | PASS; sole public/wire `MidgardPartialWitnessBundleV1` and strict canonical boundary | build PASS; 2 files, 8/8 tests |
| `nix develop ./demo --command pnpm --dir demo/midgard-node exec tsc --noEmit`; parity test and lint | integrated F02-A/F02-D/F02-I/N10 worktree | PASS | node compilation; 26/26 parity tests |
| `node demo/scripts/verify-canonical-v1-format-registry.mjs --allow-incomplete`; default release invocation | F02-R bootstrap at `dde4b789` plus parent worktree | Structural PASS; release-mode expected FAIL, proving the bootstrap cannot be credited as F02 completion | 132 unique ordered rows; 132 deliberately unverified |
| A22 forbidden-name search, package JSON parse, focused lint, DA package typecheck, multi-peer integration replay | concurrent F02-A22/F02-DS worktree | Search/JSON/lint/typecheck PASS; integration FAIL with all three peers rejecting the pre-strict-record fixture as `malformed_payload`, so no product credit and rerun required after F02-DS integration | 4 static checks; 1 test failed before A22-specific behavior |
| Pinned multi-peer DA replay after mandatory-envelope fixture repair | F02-A22 worktree at `c1f4a800` | PASS; threshold/rejection/restart behavior retained without retired Phase-5 readers or emitters | 1/1 test, 15.31 s |
| Pinned DA payload/proof/startup fixture replay after strict persisted-root integration | F02-DS/F02-DS-B worktree at `c1f4a800` | PASS; exact validation-trace roots survive storage and release gate still fails closed for the intended reason | 3 files, 12/12 tests |
| `nix develop ./demo --command` DA store/coordinator typecheck and focused replay | F02-DS/F02-DS-B worktree at `c1f4a800` | PASS; strict JSON records, peer-source records, watcher recovery, and coordinator retry/restart behavior compile and pass together | 3 files, 29/29 tests |
| Pinned compiler TS/Aiken C10 golden-vector replay | F02-C10 worktree at `c1f4a800`; Aiken `v1.1.21+42babe5` | PASS; TS exact tag/hash/unknown-tag file 11/11 and guarded Aiken selector exactly 1/1 | 12 tests |
| Registry structural/release replay plus protected SHA-256 inventory and `git diff --check` | F02 integration worktree at `c1f4a800` | PASS for 132-row structure and expected fail-closed release rejection of all 124 open rows; all ten protected hashes unchanged; whitespace clean | 132 rows; 8 PASS, 124 open; 10 hashes |
| Pinned C03 TypeScript tuple/rejection and Aiken equality vector | F02-C03 worktree at `a0bb3767`; Node `22.22.2`, pnpm `9.15.9`, Aiken `v1.1.21+42babe5` | PASS; exact compiled profile accepts only the all-V1 tuple, and every explicit corresponding Aiken format version equals `1` | TS 2/2; Aiken exactly 1/1 over 26 constants |
| Registry structural/release replay after C03 | F02-R worktree at `a0bb3767` | Structural PASS; release-mode expected FAIL for exactly the remaining rows, proving C03 promotion did not weaken fail-closed behavior | 132 rows; 9 PASS, 123 open |

## Current next action

Integrate and checkpoint F02-DS/D19/D20, C10, A22, and the strict dependent
fixtures. While the A03-A09 and A13-A16 repair leases run, populate the exact
machine-readable registry and prepare the next non-overlapping A10-A12/A17-A23
artifact wave, followed by L/S/K/V/P ABI family audits.

## Blockers

None proven. Missing `DA_L1_SUBMITTER_KEY_SOURCE`, provider operation, wallet
funding/collateral, and a second watcher provider are F03 preflight gaps, not
current blockers; all local work remains ready.
