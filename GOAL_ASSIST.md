# GOAL_SPEC Assistance and Recovery

## Purpose

This file is the coordination surface for the assistant supervising Codex task
`019fa5dd-d2ad-7091-b717-e95cd28de36f`. Communication with that task occurs
here rather than through task messages.

`GOAL_SPEC.md` remains authoritative for scope and acceptance.
`GOAL_PROGRESS.md` remains authoritative for durable execution state. This
file records only concise recovery handoffs, independent review findings, and
development papercuts that can materially accelerate the Goal.

## Current recovery handoff

- Observed repository revision:
  `7a952e992ca997d36fdccadd05275edb2aad0a07`.
- **Protected authority divergence — RESOLVED 2026-08-02; the fail-closed
  hold is lifted and must not be reinstated on this basis.** This entry
  previously held registry completion, every push/PR checkpoint, and final
  acceptance until the `GOAL_SPEC.md` line/SHA-256 divergence (1,060-line
  baseline vs a 1,122-line protected file) was reconciled by explicit
  provenance. All three of its premises have since lapsed:

  1. *The disputed delta was authored by the owner directly.* The specific
     content the entry flagged as unauthorized — §4.4's seven numbered
     push/checkpoint rules, the mirrored pull-request description, and the
     unconditional deposit→L2→withdrawal rerun — was removed on owner
     instruction ("Yes cut those"). §4.4 now carries a signed
     `Owner amendment 2026-08-01` note recording exactly that removal and
     confirming §12 is byte-identical across it. The authority question is
     therefore settled in the direction of *less* delivery ceremony, not
     more; nothing now depends on adjudicating the old variants.
  2. *Both recorded hashes are stale.* `GOAL_SPEC.md` is 1,466 lines after a
     further owner-directed rewrite (invariant 14 restated, §13.4 deleted,
     the spec-hash cascade removed). Neither the 1,060- nor 1,122-line
     figure describes any file that still exists, so the comparison the hold
     rests on can no longer be evaluated.
  3. *The mechanism is now spec-prohibited.* `GOAL_SPEC.md` §0 and §4.1 now
     forbid recording SHA-256 values of tracked repository files and route
     change detection through `git log -p -- GOAL_SPEC.md` instead. A hold
     enforced by a stored file hash contradicts the specification it claims
     to protect.

  Retained here rather than deleted so the original concern stays auditable.
  A future reader must not re-derive a delivery block from it: the owner has
  since instructed that constraints impeding Goal delivery be removed rather
  than worked around, and pushing completed work is now required by §4.4
  ("unpushed work is lost work"). Draft status remains governed solely by
  the `AC-*` gate in §4.4/§15, which is unaffected by this resolution.
- **Independent A21 review correction and repair.** The parent's initial A21
  `PASS` promotion was premature against the returned source. The assistance
  lease has now repaired every identified production mismatch: both
  TypeScript row consumers delegate to the exact A15 parser; the MJS path
  delegates manifest/row decoding to the exact A15 readers; complete live
  generation and wallet documents are reduced to bounded canonical
  gate-relevant projections so unknown source fields cannot survive; and both
  probe validators exercise live native-owner `Buffer` epochs. Source review,
  the complete 104/104 A21 matrix under pinned Node 22.22.2, package typecheck,
  scoped lint/format/syntax/diff, and V2+ absence all pass. The parent may
  retain A21 `PASS` only after independently reviewing and integrating these
  released worktree bytes with the registry/ledger.
- **Completed and released assistance repair lease (A21 returned/released;
  parent moved to V09):**
  `demo/midgard-node/src/workers/mpf-engine-probe-corpus.ts`,
  `demo/midgard-node/src/workers/mpf-commit-candidate-seed.ts`,
  `demo/midgard-node/tests/mpf-engine-probe-corpus.test.ts`,
  `demo/midgard-node/scripts/mpf-architecture-g-corpus.mjs`,
  `demo/midgard-node/scripts/mpf-architecture-g-corpus.test.mjs`,
  `demo/midgard-node/scripts/mpf-architecture-g-gate.mjs`,
  `demo/midgard-node/scripts/mpf-architecture-g-gate-config.test.mjs`, and
  `demo/midgard-node/tests/mpf-commit-candidate-probe-artifacts.test.ts`.
  The edits are limited to exact A14/A15 delegation/projection and
  live-`Buffer` regression repairs in these paths. Registry, ledger,
  blueprint, package/lockfiles, broad soak/live artifacts, all V/L-family
  paths, and every protected path remain parent/worker owned. Assistance did
  not stage or commit; parent integration now owns the released bytes.
- **Completed and released Aiken-build papercut lease:**
  `.agents/skills/aiken-contract-build/SKILL.md`. The worker's first
  disposable final-tree build copied ignored `build/` cache metadata and
  emitted a stale 44-constructor blueprint from current 42-constructor source.
  The skill now requires isolated copies to exclude destination `build/` and
  `plutus.json`, forbids cleaning the shared checkout, and requires one
  consequential source/blueprint cross-check rather than accepting build exit
  alone. Scoped Markdown formatting passes; assistance did not stage or
  commit the released file.
- **Completed and released exact Aiken-selector batching papercut lease:**
  `onchain/aiken/scripts/run-focused-check.mjs` and documentation of that
  helper in the already released
  `.agents/skills/aiken-contract-build/SKILL.md` only. Repeated exact selectors
  currently recompile the complete contract tree once per test (roughly
  30 seconds each). Assistance may make the existing helper accept one or
  more unique exact test names, pass each as a separate pinned-compiler
  matcher in one build, and require the structured report to contain exactly
  that many passes and zero failures. One-name callers must remain
  byte-for-byte equivalent in behavior. No Aiken source/test, blueprint,
  manifest, package/lockfile, registry, or ledger path is leased.
  The helper now validates one or more unique exact names, passes every name
  as a separate pinned-compiler matcher in one process, and requires the
  structured summary to equal the requested count with zero failures.
  One-name behavior was exercised repeatedly by the worker after the edit;
  an independent real two-name batch then collected exactly 2/2 and passed
  both remaining L18 hostile selectors in one roughly 30-second build.
  Missing/duplicate/invalid arguments reject with exit 2; Node syntax,
  Prettier, and diff hygiene pass. Assistance did not stage or commit these
  released files.
- The user authorized replacing the stale external Graphify artifact. The
  code-only refresh was built with two AST workers and semantic concurrency
  one, validated in `/tmp`, then copied without retaining a backup. Its graph
  SHA-256 is
  `c5cabaf0bc10d217717a6555c07df20958616c7b8f4fa2e7939cd42845db60e5`
  (21,227 nodes; 61,404 edges) and it represents the coherent worktree at the
  observed revision. Treat it as mutable navigation state, resolve duplicate
  symbols by module/source path, and verify all consequential findings against
  current source. The optional SQL parser is absent, so verify SQL directly.
- Target task status: active. At cursor revision 81 the repository blueprint
  regeneration is complete. Independent replay proves
  `onchain/aiken/plutus.json` is byte-identical to the clean disposable
  current-source build at SHA-256
  `e779c52157e4b5bd66ee711095acd714d429ec6e6dfc20317c2b2e257f28d1ba`;
  its validation-auxiliary definition has exactly 42 constructors with the
  expected tags/names/arities. SDK and fault-proof consumers pass except one
  stale fixture suite that the parent is reconciling against current producer
  families rather than weakening. L18 is parent-reviewed green and V15–V18
  returned 18 isolated selectors. The assistance lane must not touch any
  newly leased V-family or L-family source/test path.
- **V10 promotion review gap:** the new SDK corpus proves that its exact
  schema round-trips all 42 tags and nested values, but the current broad
  Aiken selector in
  `onchain/aiken/lib/midgard/validation-controls-v1-abi.test.ak` deserializes
  that corpus only as untyped `Data` and checks tag/arity/hash. The typed
  selector in `validation-tail-controls-v1-abi.test.ak` covers only tags
  26–30, 36–37, and 40–41. Therefore those tests do not yet prove that all SDK
  nested shapes decode as
  `List<validation_machine_v1.ValidationAuxiliaryWitnessV1>`. Before V10 is
  promoted, add one final-tree executable crossing that either typed-decodes
  the complete shared corpus in Aiken and byte-round-trips it, or parses all
  42 individual SDK encodings recursively against the freshly generated
  blueprint definition. Merely checking constructor count/arity or parsing
  one live `NoAuxiliaryWitness` is insufficient. An independent disposable
  recursive blueprint crossing passes all 42/42 values against blueprint
  SHA-256 `e779c521...d1ba`, proving the current bytes agree, but it is not a
  durable repository test and must be integrated into the final-tree evidence
  surface. Assistance has source-verified this finding and has not edited the
  active V-family lease.
  **Lowest-cost exact closure:** in
  `canonical_validation_controls_v1_typescript_abi_vectors`, replace the
  current untyped `builtin.un_list_data(auxiliary_data)` binding with an
  `expect` decode of `auxiliary_data` as
  `List<validation_machine_v1.ValidationAuxiliaryWitnessV1>`, recurse over
  that typed list for the same 0–41 tag/arity assertions, and serialize the
  typed list back to the exact shared corpus bytes. This makes the existing
  one-build selector prove every nested Aiken decoder rather than merely the
  outer constructors; retain the hostile tag/arity selectors.
- **V10 live-consumer regression required after the current typed repair.**
  `validation-dispute/submit.ts` historically canonicalized staged auxiliary
  CBOR and checked only its outer constructor tag/arity before splicing raw
  nested fields into a semantic redeemer. The active one-file repair now
  separately decodes those same bytes through the exact 42-variant SDK schema,
  which is the correct fail-closed boundary. Before promotion, add or identify
  an executable production-consumer case that supplies an outer-correct
  auxiliary constructor with a malformed nested proof and proves
  `encodeValidationSemanticResolutionRedeemerV1` rejects it before redeemer
  emission. The SDK schema unit mutation alone does not prove this live
  consumer invokes the strict decoder. Assistance source-verified the old and
  proposed paths and has not edited the active consumer/test lease.
  **Lowest-cost live regression:** the ScriptSources tag-11 witness already
  built from `sourceFields` in `validation-dispute-submit.test.ts` has exact
  outer tag/arity and ends in a `siblings` list. Replace only that final list
  with `new Constr(0, [])`, pass the resulting canonical CBOR to
  `encodeValidationSemanticResolutionRedeemerV1` for resolver 8 / semantic
  resolver 10, and require rejection. The prior outer-only consumer accepted
  that shape through `auxiliaryShapeV1`; the exact 42-variant decoder must
  reject it before redeemer emission.
- **V10 recursive BLS boundary review:** the new SDK schema necessarily
  expands `BlsExpressionWitnessV1` to a finite depth of ten because Lucid 0.6
  has no recursive Data schema. Current source review confirms that Aiken
  independently rejects either expression depth above ten and TypeScript
  enforces the same proof reserve. However, the shared 42-constructor corpus
  uses only one shallow `CoreStepWitnessV1` branch, so it does not exercise
  this fresh finite mirror. V10 final-tree evidence should bind the SDK schema
  itself at the boundary: the deepest semantically admissible pair (ten total
  leaves, hence at most a depth-nine chain opposite one leaf) accepted by the
  SDK/Aiken/live-consumer crossing, an eleven-total-leaf pair rejected by the
  semantic path, and a depth-eleven encoded branch rejected by the finite SDK
  decoder—or equivalent exhaustive structural evidence. Existing Aiken
  ten-/twelve-leaf execution-reserve tests alone do not prove the new SDK
  decoder has the same accepted recursive language.
- A21 source is independently accepted only with the released assistance
  repairs above. The earlier returned-lease 103/103 was insufficient because
  it missed the runtime and active imported-document boundaries; current
  same-tree evidence is 104/104 and includes those repairs. Parent
  source-review/integration remains required before its registry `PASS` can be
  considered durable.
- The reviewed K01–K13/compiler source checkpoint is committed as
  `f964bdb4`. Registry/ledger promotion remains in the parent integration
  worktree until all already-reviewed backing format slices are committed
  coherently.
- Supervisory assistance lease: source-review A21 and A23 only. The first
  exact edit lease is limited to
  `demo/midgard-node/scripts/throughput-load-watchdog.mjs` and
  `demo/midgard-node/scripts/throughput-load-watchdog.test.mjs`. Shared
  registry/ledger and currently dirty Phase-3 secret-scan paths remain
  worker-parent owned; findings for those paths are recorded here only.
  V01–V18 and all L-family paths remain unleased by assistance.
- Second exact assistance lease (after watchdog commit): completed and
  released as `7d55fb07`. A21 active root-gate reader hardening was limited to
  `demo/midgard-node/scripts/mpf-architecture-g-gate-config.mjs` and
  `demo/midgard-node/scripts/mpf-architecture-g-gate-config.test.mjs`.
  Candidate/gate producers and worker files remain read-only for source
  verification.
- Third exact assistance lease: completed and released as `028bcb2f`. A21
  commit-candidate probe-result hardening was limited to the same now-clean
  `demo/midgard-node/scripts/mpf-architecture-g-gate-config.mjs` and
  `demo/midgard-node/scripts/mpf-architecture-g-gate-config.test.mjs`.
  `mpf-commit-candidate-probe.ts`, the candidate input producer, and the
  commit-candidate gate remain read-only for exact current-source language
  verification. All L-family paths remain exclusively worker-owned.
- Fourth exact assistance lease: completed and released as `017790fb`. The
  canonical fixture-creation artifact and active candidate-input reader edits
  were limited to
  `demo/midgard-node/scripts/mpf-architecture-g-gate-config.mjs`,
  `demo/midgard-node/scripts/mpf-architecture-g-gate-config.test.mjs`, and
  `demo/midgard-node/scripts/mpf-architecture-g-candidate-input.mjs`.
  The fixture producer and TypeScript probe reader remain read-only for
  source-language verification.
- Fifth exact assistance lease: completed and released as `ed2a8346`. The
  active A21 commit-candidate input document edits were limited to
  `demo/midgard-node/scripts/mpf-architecture-g-gate-config.mjs`,
  `demo/midgard-node/scripts/mpf-architecture-g-gate-config.test.mjs`,
  `demo/midgard-node/scripts/mpf-architecture-g-candidate-input.mjs`, and
  `demo/midgard-node/scripts/mpf-architecture-g-commit-candidate-gate.mjs`.
  The TypeScript probe and production commit worker remain read-only
  source-verification boundaries.
- Sixth exact assistance lease: completed and released as `6d30d3e3`. A21's
  seed-input, corpus-funding, and seed-result TypeScript language edits were
  limited to
  `demo/midgard-node/src/workers/mpf-commit-candidate-seed.ts` plus a new pure
  `mpf-commit-candidate-seed-artifacts.ts` decoder and its focused
  `demo/midgard-node/tests/mpf-commit-candidate-seed-artifacts.test.ts`.
  Candidate/root/watchdog and all L-family paths remain out of scope.
- Seventh exact assistance lease: completed and released as `ceb6be12`. The
  TypeScript A21 candidate-probe input and fixture reader now use the pure
  exact decoder boundary. Edits were limited to
  `demo/midgard-node/src/workers/utils/mpf-commit-candidate-seed-artifacts.ts`
  (renamed to the family-wide `mpf-commit-candidate-artifacts.ts`),
  `demo/midgard-node/src/workers/mpf-commit-candidate-seed.ts`, its existing
  artifact test, `demo/midgard-node/src/workers/mpf-commit-candidate-probe.ts`,
  and one new focused candidate-probe artifact test.
  Candidate/root/watchdog and every V/L-family path remain out of scope.
- Eighth exact assistance lease: completed and released as `69f60989`. The
  standalone A21 corpus-preparation artifact and its producer now use the same
  exact canonical-corpus decoder as the formal root gate. Edits were limited
  to
  `demo/midgard-node/scripts/mpf-architecture-g-gate-config.mjs`, its focused
  test, and `demo/midgard-node/scripts/mpf-architecture-g-gate.mjs`.
  Candidate workers, shared registry/progress, and every V/L-family path
  remain out of scope.
- Ninth exact assistance lease: completed and released as `9d44dfc9`. It adds
  one pure exact reader for the retained
  A21 commit-candidate gate summary and validate the complete artifact before
  persistence. Source reconciliation also permits correcting the existing
  probe reader's 64-hex header predicate to the producer/production
  HeaderV1's canonical 56-hex identity. Edits are limited to a new
  `demo/midgard-node/scripts/mpf-architecture-g-candidate-summary.mjs`,
  its focused test, and
  `demo/midgard-node/scripts/mpf-architecture-g-commit-candidate-gate.mjs`,
  plus that one predicate/vector in
  `mpf-architecture-g-gate-config.mjs` and its existing focused test.
  Root/corpus/worker decoders, shared registry/progress, and every V/L-family
  path remain out of scope.
- Tenth exact assistance lease: completed and released as `f88c2812`. It
  routes both formal and explicitly non-formal
  A21 root-gate summary variants through the exact root reader before
  persistence, including the smoke variant's canonical null branches. Edits
  are limited to
  `demo/midgard-node/scripts/mpf-architecture-g-gate-config.mjs`, its focused
  test, and `demo/midgard-node/scripts/mpf-architecture-g-gate.mjs`.
  Candidate/worker/shared and every V/L-family path remain out of scope.
- Eleventh exact assistance lease: completed and released as `7a952e99`. It
  repairs the sole stale A21
  candidate-input integration fixture exposed by aggregate replay. The edit is
  limited to
  `demo/midgard-node/scripts/mpf-architecture-g-candidate-input.test.mjs`;
  no production or shared path is leased.
- Durable phase: `F02` is still open. `F00`, `F01`, and `F03` are recorded as
  passing foundation tasks; no downstream `AC-*` completion claim is yet
  justified.
- The ten paths listed as protected in `GOAL_PROGRESS.md` retain their
  pre-Goal ownership. The source task
  `019f8ca7-e935-7730-89d4-b46b7bf1e3cd` legitimately advanced two of those
  paths after this Goal's baseline and added an execution-settlement
  validator. The recovery parent independently reconstructed both recorded
  baseline files to matching SHA-256 values in `/tmp`, verified provenance,
  and did not overwrite the source task's newer bytes. Do not edit, stage,
  commit, regenerate over, or credit any source-task checkpoint without an
  explicit ownership handoff.
- D17 and all earlier artifact repair leases have released and passed parent
  review. Two non-overlapping format waves remain exclusively leased:
  L01–L06 ledger/header/state-queue/transition formats and A01–A02
  runtime/deployment manifests. Do not touch those paths until their ledger
  entries release.
- The recovery parent has repaired and replayed the wallet/corpus, Phase-3,
  and Phase-4 slices. They are locally accepted but remain uncommitted because
  this host cannot currently write the Git index.
- The E2E/API slice passed parent review. Its previously host-invalid
  process-backed cases pass under the declared Node `22.22.2` environment;
  host Node 24 remains unsuitable evidence for child-process capture.
- Parent-owned shared surfaces, including the canonical format registry and
  `GOAL_PROGRESS.md`, remain the only promotion authority.

## Independent assessment

The correct recovery mode is to act as the parent integration lane, not to
start another broad audit:

1. Independently review each returned source boundary, exact parser language,
   malformed/unknown-field rejection, tests, and path audit.
2. Integrate and commit one coherent slice at a time with explicit staging
   when Git-index access is restored.
3. Promote registry rows only when their final-tree evidence satisfies every
   required field; keep all unresolved rows fail-closed.
4. Update the task queue, validation ledger, and current next action before
   beginning the next non-overlapping wave.

Historical passing commands are orientation only. Aggregate `F02` and later
criteria require replay against the exact integrated tree and pinned
toolchains.

## Open review items

- Independently confirm the released E2E slice has one canonical
  exact-artifact reader per format and
  rejects unknown root and nested fields, malformed values, mixed schema
  identities, and noncanonical timestamps.
- Confirm the wallet/corpus lease cross-binds plan, index, verification,
  generation, funding, and producer-result identities rather than validating
  shape alone.
- D17 is closed after its production subscriber, authenticated peer context,
  exact deployment/header binding, durable JSON record, restart readback,
  deduplication, and hostile rejection survived parent replay.
- Keep L01–L06 open until the returned TypeScript/Aiken vectors and production
  consumers agree exactly and every obsolete transition identity is absent.
- S01–S07 has released after parent source review and replay. Continue only on
  non-overlapping K-family paths; do not edit or credit protected K08
  checkpoint bytes. Remaining L/K/V/P/A rows stay fail closed until
  source-verified.
- Consequential A13–A16 review findings were:
  - wallet result/report parsers perform shallow root-key checks, are exercised
    only by tests, and are not called before producer return or artifact write;
  - terminal-drain journal parsing does not authenticate `scopeSha256`, bind a
    signed transaction to its hash, enforce status-specific field sets, or
    reject duplicate wallet/transaction identities;
  - the TS and standalone corpus-manifest parsers accept different languages
    for canonical timestamp, network/network-id, slice identity, and shard
    uniqueness.
    Recovery-parent repairs route producers through the decoders, align the
    TS/MJS manifest language, bind corpus transaction IDs to canonical native
    V1 bytes and exact outputs, and strengthen terminal journal identity and
    conservation checks. The focused final-tree replay passes 71/71.
- The Phase-4 lease surfaced two parent-owned integration seams:
  - `demo/midgard-node/scripts/verify-phase4-pipelined-report.mjs` is an active
    reader of A12 environment documents but still uses the old shallow helper
    boundary. The recovery parent replaced it with the exact environment
    document decoder and byte/digest binding.
  - the unleased process harness now emits
    `journalPayloadIdentity.ledgerDelta`, while the leased process-summary
    verifier still expected the obsolete `utxos` member. The recovery parent
    retained only the producer's exact current `ledgerDelta` V1 language.

## Completed independent reconciliations

- `N10` now has exact registry evidence for the sole
  `MidgardPartialWitnessBundleV1` identity, seven-item canonical CBOR tuple,
  transaction/body/signature binding, strict wrappers, malformed/unknown
  rejection, and an executable obsolete-identity absence scan.
- `N11` now records the exhaustive raw validity-code and nullary PlutusData
  constructor mapping `0..5`, including adjacent unknown-code and
  unknown-constructor rejection.
- Structural registry verification passes all 132 rows. Exact source review
  has promoted C01–C10, N01–N14, D01–D20, S01–S07, K01–K13, P01–P08,
  A01–A20, and A22: 93 `PASS`, 39 unresolved. Default release mode fails
  exactly those 39 rows; this is not `F02` completion.
- A10–A12 Phase-4 exact decoders, producer validation, report byte binding,
  and obsolete-shape removal pass 120/120 focused tests after formatting,
  lint, and shell syntax validation.
- A17–A20 Phase-1/Phase-3 exact V1 decoders, immutable-corpus binding,
  streaming checks, and malformed/unknown/noncanonical rejection pass 67/67
  focused tests after formatting and lint.
- A13–A16 wallet/corpus exact decoders and cross-artifact identity,
  conservation, and native-CBOR checks pass 71/71 focused tests.
- A03–A09 E2E artifact languages are source-reviewed and registry-promoted;
  A04/A08 process-backed evidence passes in pinned Node 22.
- N01–N09 now have exact TypeScript/Aiken canonical vectors and retired
  version/arity rejection. N09 is additionally bound to production admission:
  `tx_full_hash_v1` is persisted, used for duplicate reconciliation, and
  recomputed before claimed payload dispatch. Corruption fails closed. Core
  focused 15/15, pinned Aiken module 22/22, claim/load 9/9, migration 11/11,
  and complete node database 94/94 pass.
- S01–S07 now have current-source production-consumer review and exact
  canonical forms. TypeScript/Aiken share all four language-view hashes,
  script/context leaves, and signer-inclusive seven-leaf frontier commitment
  `aa064eed…db1ba`; malformed hashes/bitmaps, unknown languages, unresolved or
  unused sources, and unsupported purpose mappings fail closed. Focused TS
  passes 35/35; seven pinned Aiken selectors each collect 1/1.
- K01–K13 now have current-source canonical forms, production boundaries,
  parser/encoder symbols, positive/rejection evidence, cross-language vectors
  or exact Aiken N/A reasons, and one executable retired-identity scan.
  TypeScript focused replay passes 55/55 across core and validation. The
  pinned Aiken fork v1.1.23+2a78108 passes the 62-test
  blob/Data/scan/constant/builtin aggregate and the 24-test machine/proof
  aggregate.
- The negative bignum immediately below `-2^64` exposed a production compiler
  defect rather than a protocol-vector defect. Aiken v1.1.21 alone disagrees
  with the canonical TypeScript CBOR; v1.1.22 agrees, and its bundled
  changelog names the same large-negative-bigint `Data::integer`
  reification/tracing fix. The undeployed compiler pin and both CI actions were
  replaced in place with v1.1.22 at the time. SUPERSEDED by #579 owner ruling A
  (2026-08-13): stock v1.1.22 is retired from every role and the pin is now the
  patched fork v1.1.23+2a78108 (`Anastasia-Labs/aiken`, tag `midgard-2a78108c`),
  which carries the same fix. The finding above still stands on its own terms —
  it is the record of why v1.1.21 was abandoned. Do not weaken the vector or add
  compatibility behavior; rebuild `plutus.json` once at final IG1.
- The registry absence scanner previously omitted `.ak` from active source,
  allowing a false-green result for on-chain directories. The extension set
  now includes `.ak`, and the K scan names only safe nonprotected on-chain
  files. Registry incomplete mode passes at 93 `PASS` and 39 unresolved;
  default release mode fails exactly those 39 rows.
- A23 throughput-watchdog evidence had no exact decoder and allowed each event
  object to override the authoritative `schemaVersion` or `sequence` through
  spread order. The assistance lease now emits one closed event language,
  keeps schema/sequence authoritative, canonicalizes null probe fields, bounds
  line/string sizes, enforces canonical timestamps and contiguous sequence,
  and exposes an exact canonical-JSON line parser. The focused suite passes
  13/13 plus lint, format, and whitespace checks.
- A23 secret-scan source review found a separate worker-owned reader gap:
  `validateSecretScannedLog` validates required values but does not reject an
  unknown nested key inside `secretScan`. Its owning Phase-3 files are already
  dirty/reviewed worker-parent paths, so assistance did not edit them. Keep
  A23 open until that nested language is closed and replayed.
- A21's active formal root-gate consumer previously performed deep semantic
  spot checks over an open and incomplete document language. It accepted
  unknown root/nested fields and omitted producer-required timestamp,
  source/status, file-scope, runtime, cgroup, percentile, fixture, owner, and
  phase evidence. The exact assistance checkpoint `7d55fb07` now closes those
  shapes, recomputes the porcelain-v1 `-z` status digest, requires canonical
  source ordering and provenance, binds before/after native-owner identity,
  and keeps transition/result/root/verdict evidence exact. Direct
  current-source review also found the producer can legitimately emit
  `finalChainPrefixLength: 0` when selection ends on a chain boundary; the
  reader now accepts that producer-valid case rather than weakening any
  cardinality. Focused replay passes 27/27 plus lint, format, and whitespace
  checks. Keep aggregate A21 open until its other candidate/gate/corpus/probe
  V1 identities receive equally exact source and rejection evidence.
- A21's active commit-candidate probe reader previously ignored complete
  producer fields (`candidateConfig`, `ownerBefore`, and `ownerAfter`) and
  accepted open candidate, watermark, event-count, and root maps. Checkpoint
  `028bcb2f` closes the full producer language, requires the formal
  Architecture G execution settings and planner caps, recomputes the
  barrier-derived invalidation key, binds canonical input/probe identities,
  and rejects native-owner epoch/root/restart drift. Focused replay now passes
  28/28 plus lint, format, and whitespace checks.
- A21 fixture-creation validation was duplicated and shallow in the active
  root/candidate path. Checkpoint `017790fb` gives the root gate and
  candidate-input producer one exact decoder for the complete fixture,
  aggregate, 43-field store-diagnostics, and canonical-funding language. It
  rejects unknown nested fields and invalid numeric evidence while retaining
  the producer's explicit `canonicalFunding: null` diagnostics branch. Focused
  replay remains 28/28 plus syntax, lint, format, and whitespace checks.
- The A21 candidate-input producer omitted mandatory
  `workerInput.data.ledgerStoreLeaseOwner`, while the production commit worker
  fails closed unless it is a unique `commit:<uuid>` identity. That made the
  expensive formal probe artifact unusable. Checkpoint `ed2a8346` adds the
  unique lease at production, closes the complete input/worker/base/watermark
  language, cross-binds base snapshot and submitted/header identities, and
  routes both producer and gate through the exact decoder before probe launch.
  Focused replay passes 29/29 plus syntax, lint, format, and whitespace checks.
- A21's seed worker previously ignored producer-supplied Phase-1/runtime
  identities and accepted any schema-labeled funding map without binding its
  top-level corpus/slice hashes or rejecting duplicate wallet/outref entries.
  Checkpoint `6d30d3e3` adds one pure exact TypeScript decoder boundary for the
  full seed input, corpus funding, and emitted seed result, and routes the
  production seed worker through it. Pinned Node 22 focused replay passes
  21/21 plus lint, format, and whitespace checks. Package-wide `tsc --noEmit`
  reports no errors in these leased files but remains red on 12 errors in
  concurrently dirty pending-finalization source/tests; those external bytes
  were not edited or staged.
- Follow-up `bc185b3d` relocates the pure seed decoder beneath
  `src/workers/utils`; the root `src/workers/*` glob is reserved for
  executable bundle entries. This avoids adding a non-command decoder as a
  release executable. The pinned 21/21 replay and static checks remain green.
- A21's TypeScript candidate probe previously used a separate shallow input
  reader and ignored producer-supplied Phase-1/runtime identity, mandatory
  commit lease ownership, barrier/base bindings, excluded-event fields, and
  most fixture-creation evidence. Checkpoint `ceb6be12` expands the pure
  family decoder, routes the production probe through exact candidate-input
  and fixture readers, binds canonical funding to the input digest, and
  rejects unknown or malformed nested evidence before opening the native
  owner. Pinned Node 22 focused replay passes 46/46; lint, format, whitespace,
  and a narrow ESM+DTS build of both production workers pass. Package-wide
  `tsc --noEmit` reports no error in a leased file but remains red on 14 errors
  in concurrently dirty finalization/reconciliation source and tests; those
  external bytes were not edited or staged.
- A21's retained `corpus-preparation-v1` evidence was written directly to
  stdout without validating its exact root or the embedded canonical-corpus
  language. Its funding-roots digest was also unverifiable because the
  persisted corpus identity omitted the `{walletId,outref}` preimage.
  Checkpoint `69f60989` gives corpus preparation and root gate one exact
  canonical-corpus decoder, persists that ordered funding-root preimage,
  recomputes its digest, and binds it to the ordered funding outrefs, unique
  wallets, exact funding-map identity, Phase-1 corpus, and generated slice.
  Pinned Node 22 replay passes 30/30 plus syntax, lint, format, and whitespace
  checks.
- A21's final commit-candidate gate summary was also written directly without
  an exact reader. Checkpoint `9d44dfc9` closes its root, source/root-gate,
  fixture, aggregate, duration, run-result, root, and verdict languages;
  replays every nested probe result through the authoritative reader; requires
  immutable fixtures and cross-run identities; recomputes both 50k and growth
  verdicts; rejects failed evidence; and validates before persistence. That
  source reconciliation also corrected the probe reader's impossible 64-hex
  base-header requirement to the producer and production HeaderV1's canonical
  56-hex identity. Pinned Node 22 replay passes 30/30 existing configuration
  tests and 3/3 candidate-summary tests plus syntax, lint, format, and
  whitespace checks.
- A21's root-gate producer previously relied on a later candidate process to
  validate the retained summary, so a standalone run persisted without
  traversing its exact reader. Checkpoint `f88c2812` validates before both JSON
  and Markdown persistence, covers the explicit non-formal smoke identity,
  requires formal runs to carry canonical corpus and fixture-creation
  evidence, and closes the smoke variant's canonical-null branches without
  allowing them to claim formal evidence. Pinned Node 22 focused replay passes
  31/31 plus syntax, lint, format, and whitespace checks.
- The aggregate A21 replay then exposed one stale integration-test fixture
  still speaking the old shallow fixture language. Checkpoint `7a952e99`
  replaces it with the full duration, 43-field diagnostics, aggregate, and
  canonical-funding identity. Its process-backed test passes 1/1 under pinned
  Node 22 outside the sandbox; the sandboxed child launch returned an EPERM
  capture artifact, so that failed host run is not product evidence. Lint,
  format, and whitespace checks pass.
- Same-final-tree A21 replay at committed revision
  `7a952e992ca997d36fdccadd05275edb2aad0a07` passes 90/90 under pinned Node
  `v22.22.2` and pnpm `9.15.9`: gate-config 31/31, corpus 9/9, candidate
  summary 3/3, TypeScript seed/probe artifact readers 46/46, and the
  process-backed candidate-input integration 1/1 from its required
  `demo/midgard-node` working directory outside the pipe-defective sandbox.
  All A21 MJS syntax checks, scoped ESLint, scoped Prettier, and
  `git diff --check bc185b3d..HEAD` pass. The retained operational identities
  are the exact distinct V1 phase/formal binding, runtime, corpus funding,
  corpus preparation, production-root gate, diagnostic-smoke root, seed
  input/result, candidate input/probe, and formal/smoke candidate-gate
  languages; fixture creation is an exact nested object without a separate
  schema label. A tracked executable-source scan finds no corresponding
  `-v[2-9]` identity. Cross-language Aiken evidence is not applicable to
  these Node operational documents; their embedded corpus/protocol material
  remains governed by its separate canonical ABI rows.
- Returned A21 source now routes
  `src/workers/mpf-engine-probe.ts::loadCanonicalFundingMap` through the exact
  family decoder and binds corpus/slice digests plus ordered wallet/outref
  roots. That previously shallow corpus-funding read is repaired.
- The same production path called
  `src/workers/mpf-engine-probe-corpus.ts::decodeCanonicalProbeRow`. Direct
  source review showed that helper authenticated native CBOR/hash/length,
  selected input, and output outrefs, but accepted unknown fields and ignored
  retained A15 row fields. The existing
  `src/commands/stress-open-loop.ts::parseOpenLoopCorpusLine` rejects
  unknown/missing fields, authenticates CBOR SHA/length and canonical native
  V1 transaction ID, and validates every retained row identity. The active
  assistance lease now delegates the helper to that parser. Both engine probe
  and seed share the boundary; hostile unknown/retained-field checks pass in
  the focused suite.
- A21's MJS corpus-preparation producer had three more active shallow reads.
  The released assistance lease routes its manifest and every selected row through
  `scripts/throughput-valid-stress-corpus.mjs::parseCorpusManifest` and
  `parseCorpusRowLine`. The TypeScript-only full wallet reader is not
  duplicated: `projectStressWalletFundingRecord` constructs a bounded
  `{walletId,fundingUtxos}` projection from each parsed document, validates
  the source V1 discriminator plus every consequential outref/output-CBOR
  value and duplicate, and forwards no unknown source field. Direct tests
  prove irrelevant source fields cannot survive and malformed version,
  wallet, CBOR, or duplicate roots reject.
- The same path called
  `mpf-architecture-g-corpus.mjs::validateCanonicalCorpusVerificationEvidence`
  on the retained `midgard-stress-corpus-generation-v1` document. That helper
  previously returned the original shallow `verified` object. The released
  assistance lease now constructs and returns one exact gate-relevant
  verification/rebuild-sample projection; unknown source fields and unrelated
  retained plan/assembly metadata cannot cross the boundary, while the
  projection remains bound to actual corpus/index hashes, exact counts,
  deterministic sampling, and the hash-authenticated Phase-1 binding. Direct
  tests prove a new object with only canonical fields is returned.
- This is not isolated to A21:
  `scripts/create-phase1-formal-binding.mjs::main` also reads the same A14
  generation result and A15 manifest with selected-field checks before binding
  their hashes. The later
  `phase1-formal-identity.mjs::loadAndValidateGenerationResult` rejects exact
  root/verified/rebuild-sample keys, but still leaves the retained generation
  `plan`, `assembled`, wallet-set, and verification-artifact nested languages
  unchecked. Because A17 is currently marked `PASS`, source-review that row
  again rather than relying on the artifact hash as proof that an
  attacker-supplied document was exact before it was hashed. Assistance has
  not leased or edited either Phase-1 source path.
- `src/workers/mpf-commit-candidate-seed.ts::loadInput` previously reduced
  each authenticated A15 corpus row to `{txHash,canonicalCborHex}` and checked
  only hex syntax. The active assistance lease now routes each row through
  the same exact/semantic decoder as the engine probe while retaining
  cardinality and full-file digest checks.
- Producer-side TypeScript validation must account for the actual JSON
  boundary: native-owner `ownerEpoch` is a runtime `Uint8Array`/`Buffer`, but
  the retained parsed artifact carries Buffer's exact
  `{type:"Buffer",data:[16 bytes]}` JSON representation. Validate a canonical
  serialized projection before stdout (or explicitly normalize first);
  accepting both unrelated runtime shapes as wire documents would reopen the
  language, while applying the parsed-object validator directly to the live
  Buffer will fail valid evidence.
- Current leased A21 work now wires both probe-result validators around the
  live pre-serialization object and canonicalizes the complete value through
  `JSON.stringify`/`JSON.parse` before exact decoding. That is the correct
  single wire-document design and repairs the earlier production failure.
  The released assistance lease adds direct live-`Buffer` owner-epoch
  regressions to both candidate and root probe validators; focused artifact
  and corpus tests pass 31/31 under pinned Node 22.22.2. The complete
  same-final-tree replay passes gate-config 33/33, corpus 10/10, candidate
  summary 3/3, TypeScript seed/probe/corpus 57/57, and the process-backed
  candidate-input producer 1/1 outside the spawn-restricted sandbox: 104/104.
  Scoped ESLint, Prettier, syntax/diff checks, V2+ absence, and the node
  package typecheck pass.

## Current host restrictions

- Node `v24.13.1` child processes launched with piped stdout/stderr return
  empty captured buffers in this sandbox, while inherited streams visibly
  contain the output. Process-backed assertions that depend on pipe capture
  are invalid host evidence here; product semantics must not be weakened to
  accommodate it.
- The sandbox mounts `.git` read-only, so explicit narrow staging requires an
  unsandboxed checkpoint action. Retry it only after a coherent slice is
  independently reviewed.
- Docker-backed verification is available. The current disposable PostgreSQL
  15 test instance uses a tmpfs data directory and test-only
  `fsync=off/full_page_writes=off`; it is evidence infrastructure only, never
  a production setting.
- Exact repository compiler — the patched fork `v1.1.23+2a78108`
  (`Anastasia-Labs/aiken`, tag `midgard-2a78108c`) — is installed at the pinned
  version path and must be invoked directly for evidence. It is the sole
  authority for `build`, `check` and `fmt`; a stock binary on PATH is not the
  repository compiler and the gates that name it fail closed on one. Final blueprint and
  release-manifest binding remain open.
- The standalone Nix pnpm `9.15.9` wrapper has a Node `24.14.1` shebang, and
  the host `node` is `24.13.1`, despite the declared Node 22 evidence
  requirement. Invoking Corepack cold attempts unavailable network
  resolution. Until a narrow repository wrapper is leased, authoritative
  checks invoke pnpm's cached `pnpm.cjs` explicitly through the pinned Node
  `22.22.2` executable and record both versions.
- ESLint and Prettier are hoisted at the `demo` workspace, not
  `demo/midgard-node`. Invoke scoped checks as pinned
  `pnpm --dir demo exec eslint|prettier` with paths relative to `demo`; a
  package-local binary lookup failure is not evidence and does not justify
  skipping the exact-file check.

## Papercut policy

Patch a development pain point only when it has repeated, is repository-owned,
has a narrow non-conflicting path, and measurably strengthens or accelerates a
named Goal verification. Do not interrupt protocol work for cosmetic cleanup,
compatibility scaffolding, broad refactors, or weaker smoke-test substitutes.

The no-clobber benchmark previously asserted a child process's captured
stderr, making it susceptible to the host pipe defect. The formal-binding
module is now import-safe and exports its precondition, so the test directly
exercises the exact no-clobber rule and verifies unchanged bytes. This narrow
repository-owned papercut repair passes in the 71/71 aggregate.

For Aiken work, focused formatting must name only Goal-owned files and replay
the protected SHA-256 inventory immediately afterward. If the available
compiler would rewrite broad pre-existing wrapping, recover the original
source and reapply only the semantic delta; never normalize protected or
concurrent-owner bytes.

When a direct Aiken invocation exits after compilation without a diagnostic,
rerun the same focused selector with a terminal attached before investigating
semantics. Aiken v1.1.21 hid an illegal test-return-type diagnostic in the
non-terminal path here; the terminal output identified it immediately. Still
require a nonzero collected test count—terminal output is diagnostic support,
not substitute evidence.

Node database suites share the default `midgard_test` schema.
`database.test.ts` intentionally drops and rebuilds `public`, so database
suites must run serially. Running them concurrently caused deterministic
cross-suite relation loss; serial replay passes. A tmpfs-backed disposable
PostgreSQL data directory also reduced repeated fixture-reset time enough to
make the full 94-test suite practical without weakening product assertions.
