# Authenticated event history implementation log

Status: In progress; ABI not frozen; no acceptance or gap closure claimed.

## Baseline and preservation

- Starting source: `1b53eafd8c0c88c36232869b23eb8aaf48f7f8c2`.
- Existing user edits: `event-history-design.md`, `event-history-research.md`,
  and `remaining-gaps.md` in this directory. Preserve these edits.
- Architecture: two sorted authenticated UTxO lists, separately bound to deposit
  and withdrawal policies. Preserve nonce `OutputReference` event IDs and their
  full `blake2b_256(serialise(id))` keys.
- Required guidance read: root/onchain/demo/node AGENTS, production L2,
  contracts, transaction finalization, state reset, withdraw-zero, naming,
  design/research/gaps/testing status, Aiken build and E2E acceptance skills.
- Updated authorization (2026-09-22): redeploy Midgard whenever necessary for
  testing. Follow coupled local/on-chain state reset rules and preserve unrelated
  work. No deployment was changed in the latest fixture checkpoint.

## Ordered plan and remaining gates

1. Resolve eligibility, authorized retirement, proof completion versus merge,
   and adversarial witness capture. Document predicates and executable models.
2. Establish full-width root/node authentication, original asset accounting,
   permissionless filler promotion and exact reclaim authorization. Build a
   complete isolated admission/proof/retirement slice, both polarities.
3. Measure maximum inline predecessor and new node, external publication and
   admission, promotion/refund, unlink and reclaim under unchanged production
   limits. Only then freeze shared ABI and apply parameters.
4. Migrate deposit and withdrawal lifecycles, initialization, queue/settlement,
   all fabricated and affected transition proofs including staged openings,
   SDK schemas/builders, node ingestion/selection/projection, watcher indexing,
   public retrieval, persistence/restart/L1 rollback/L2 correction, generated
   artifacts, manifests/catalogue, deployment tools and documentation.
5. Run acceptance matrix: positive/negative, arbitrary IDs/content/timing/reuse,
   maximum shapes, races, pointer churn, restart/rollback and shared consumers.
6. Run pinned build/checks, deployed-parameter emulator scenarios, installed
   replay/fit, affected package suites/typechecks, lower-layer transaction gates,
   docs links, and live acceptance. Bind evidence to exact source, compiler,
   blueprint, parameters and deployment. Only verified gaps may close.

## Decisions and findings

- Admission eligibility remains admission validity upper bound plus event wait;
  publication alone creates no event. External admission must reference an
  existing retention output and hash its complete datum including reclaim auth.
- Pointer changes preserve immutable facts and original Value. Filler/storage
  funding and structural tokens must be excluded from deposited assets.
- Current queue merges at `header.end_time + seven days`, and opening an
  ordinary proof does not freeze merge. Retirement must authenticate that its
  inclusion interval has finalized, not just find any settlement membership.
- Current settlement datum contains event roots but no interval. Therefore
  membership alone cannot establish the retirement timing predicate when a
  malicious later header reuses an ID. Resolve this before adopting a schema.
- Permissionless retries alone cannot guarantee witness capture under targeted
  predecessor churn. A bounded capture opportunity must be enforced or supported
  by explicit inclusion assumptions before broad proof migration.

## Checks and evidence

| Command / inspection                       | Result                                                                                                                |
| ------------------------------------------ | --------------------------------------------------------------------------------------------------------------------- |
| `git status --short`, `git rev-parse HEAD` | Baseline and three user edits recorded above.                                                                         |
| `aiken --version`                          | PATH has **wrong compiler**, `v1.1.19+e525483`; not used for builds.                                                  |
| CI pin inspection                          | Required fork `Anastasia-Labs/aiken`, revision `5adf7837cbddb5d329fd51d9c0cd73f561eaf95c`, version `v1.1.23+5adf783`. |
| Toolchain inspection                       | Rust 1.94.1 and pinned fork source cached; locating/building correct executable.                                      |
| Node/pnpm inspection                       | Shell Node 24.13.1/pnpm 10.18.3; repository declares pnpm 9.15.4; Node 22.22.2 available.                             |

All implementation, fit, integration and live gates remain open. This log is a
progress record, not authority for L1 event facts or a release receipt.

## Readiness work, first implementation pass

- Added isolated Aiken primitives and refusal tests under
  `onchain/aiken/lib/midgard/event-history/`: full-width ordering and exact
  policy/address authentication, predecessor Value/payload preservation,
  temporal predicates, existing-reference external-data authentication, complete
  storage datum hash, and exact signer/zero-withdrawal owner authorization.
  These helpers are not yet integrated validators or a frozen ABI.
- Candidate churn mitigation: every mutation creates a bounded observation
  protection interval and cannot spend a node before its prior protection ends.
  A targeted spend therefore gives the challenger a newly protected witness.
  This trades same-gap write throughput for a capture opportunity. Duration,
  inclusion assumptions, full mutation enforcement and fit remain unverified.
- Capture lower bound must be strictly after the challenged header end. Capture
  and terminal upper bounds must precede earliest merge; an open thread does not
  extend merge. Retirement additionally requires authenticated finalized end >=
  actual event eligibility, independently of membership in a settlement tree.
- Installed pinned compiler successfully using the exact CI cargo command and
  Rust 1.94.1; `/home/gumbo/.aiken-fork/bin/aiken --version` now matches
  `aiken v1.1.23+5adf783`. Shell default compiler was not replaced.
- Initial primitive checks failed (formatter caught missing `test` keywords;
  typechecking caught pipeline/equality precedence). Fixed those defects without
  modifying any existing check. Compilation/test verification ongoing.
- `node .agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.mjs`:
  passed, 17 command references / 22 recovery drills.
- `pnpm --dir docs-site run check:links`: passed, 281 Markdown/MDX files.
- Read-only live prerequisite inspection: `docker ps --format ...` fails because
  the Docker daemon is unavailable. Existing deployment manifests are present;
  no deployment or durable state has been changed. This is not yet the complete
  live diagnosis; identity and provider checks remain required.
- Raw compiler install and initial diagnostic output retained in
  `artifacts/event-history/` (ignored local evidence). Check summaries must bind
  exact source hashes before being treated as acceptance evidence.

## Primitive and retention slice checkpoint

- Structural mutation helpers now cover one-shot empty-root initialization,
  gap insertion, exact-key replacement and adjacent removal. They require the
  exact consumed list-input set, exact mint/burn, preserved predecessor Value
  and immutable payload, and renewed observation protection on outputs. These
  are structural helpers: semantic event admission/promotion/retirement remains
  a required caller obligation and is not implemented yet.
- Funding helpers separate declared structural ADA from actual order assets and
  enforce fixed-size plain-key filler refunds with no owner signer requirement.
  Minimum funding, inline size and complete operation fit remain open.
- 74 primitive tests passed after initialization/funding additions. The earlier
  60-case tree built in a clean isolated copy with pinned compiler and testnet
  environment. Blueprint SHA-256 was
  `04790ae612c2478aa089a7f08089f11e43638cf007a2f562fa8b445f4ee25f6e`
  (1,149 entries), identical to the recorded pre-change blueprint; production
  validators were unchanged in that snapshot. Source/config hashes and logs are
  in `artifacts/event-history/source-identity.json`. Later source additions
  supersede that snapshot; it is not final-tree acceptance.
- Added role-authenticated gap/exact-filler absence and the separate
  `user-events/history-data` spending validator: absence AND exact owner
  authorization, no owner-only escape or same-transaction retirement/reclaim.
  Added SDK schemas and strict deployment parameter application, plus applied
  emulator cases. Their fresh build/emulator verification is still pending.
- The first role test run collected 82 cases: 81 passed, one failed. The failing
  `root_cannot_claim_an_event_role` caught lazy `let` evaluation omitting role
  checks on the gap branch. Changed the implementation to force `decode` with
  `expect`; assertions/checks were not weakened. Diagnostic JSON is retained.
- SDK typecheck passed before the role fix (which changes only Aiken). Fault-proof
  typecheck and the final role test run are in progress.
- Read-only environment checks also report `systemctl is-active docker` as
  inactive, and connection refusals at local Kupo 1442 and Ogmios 1337. Existing
  manifest schema is `midgard-deployment-manifest-v2`, ID
  `bbdfca85031de1c9adea89239f51180c1ba804e926f9c66c2b11294b666f7ea5`.
  This has not been matched to new source or treated as deployment authority.
  No services, environments, manifests or durable state were reset/redeployed.

### Consumer trace awaiting migration

| Surface                         | Concrete consumers / required migration                                                                                                                                                                                                                            |
| ------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Event admission                 | `user-events.ak`, deposit/withdrawal mint/spend, SDK `user-events/{contracts,internals,deposit,withdrawal}.ts`; remove only these events' witness certificates after replacement safety is proven.                                                                 |
| Initialization and authority    | SDK/node initialization, hub fields or explicit deployment-bound policy/address parameters, applied blueprint and manifests; initialize both unique roots.                                                                                                         |
| Finality/settlement             | Queue merge, settlement Spawn/get_datum/valid_event_inclusion, deposit absorption, withdrawal refund and payout initialization, SDK settlement/reserve-payout; authenticate actual finalized frontier before unlink.                                               |
| Fabricated families             | Aiken step 02/03/04 and validators; SDK family schemas/parameter maps; prepare/submit/workflow files for both kinds; capture facts and remove exact-unspent-nonce dependence.                                                                                      |
| Transition traces               | `proof.ak`, `deposit-source.ak`, deposit-yields/final-yield and all summary/value/yield continuations; SDK codecs and fault-proof replay/workflow/public L1 capture. Retain original actual Value and submitted withdrawal body separately from operator validity. |
| Watcher indexing                | `user-event-indexer.ts` currently separately scans consumed/created events and rejects repeated active/terminal event IDs. Add atomic pointer-continuation recognition, canonical list/payload indexing and corresponding rollback transitions.                    |
| Node ingestion                  | `fetch-and-insert-{deposit,withdrawal}-utxos.ts`, projection/selection and reserve-payout commands; keep continuation out-refs current without creating duplicate L2 events.                                                                                       |
| Persistence and public evidence | Watcher history/archive, retained replay authority, restart/reconciliation/pruning and native rollback; archived bytes remain preimages, not L1 authority.                                                                                                         |
| Tooling/tests                   | Deployment catalogue/manifests, node-tools devnet history fixtures, ABI vectors, installed replay/fit, live state-correction journeys and shared forced-order regressions.                                                                                         |

A broader path inventory (93 candidate source files, not an exhaustive symbol
call graph) is retained at `artifacts/event-history/consumer-path-inventory.txt`.
All migration rows remain open. NIFP-01/02/03 status is unchanged.

## Applied retention and immutable-facts checkpoint

- The final root-role fix uses an explicit Boolean `expect` in the absence
  function. Merely changing the helper binding from `let` to `expect` did not
  force all checks. The unchanged negative assertion now passes; 82/82 focused
  cases passed after the final fix.
- A second clean isolated pinned testnet build added only the retention
  validator's two blueprint entries. All 1,149 pre-existing compiled scripts
  remained byte-identical. Blueprint SHA-256:
  `f2457e3854575bd3d008618c850a53e56e71b51ef1b3b36d4bd0eb135e9620e8`.
  Source/compiler/config/SDK/emulator identity is recorded in
  `artifacts/event-history/retention-source-identity.json`.
- Applied retention emulator: 9/9 cases passed using unchanged production
  protocol parameters and local UPLC evaluation. Cases cover authenticated
  gap/filler absence, false presence, wrong policy/gap, missing signer, exact
  zero-withdrawal script owner authorization, and a separately published
  10,000-byte payload. Fixture list tokens use a native policy; this is NOT
  admission/retirement or complete list acceptance.
- Root reclaim: 2,103 signed bytes, 259,269 lovelace fee, 132,378 memory,
  49,115,164 CPU. Filler reclaim: 2,103 bytes, 260,723 fee, 148,691 memory,
  56,226,396 CPU. Script-owner reclaim: 2,139 bytes, 265,399 fee, 132,514 memory,
  50,426,633 CPU. The large publication (including fixture list creation/native
  mint) was 10,825 bytes, 636,301 fee; its reclaim matched root reclaim costs.
  Raw transactions, applied identities and parameters are retained in
  `artifacts/event-history/retention-emulator-second/retention-emulator.json`.
  No safe inline bound or maximum external admission fit follows from this.
- First emulator attempt exhausted memory while Vitest compared two 22 MB
  Buffers; replacing its assertion with exact `Buffer.equals` preserved the
  assertion and allowed verification. No production/check limits changed.
- SDK suite passed 66 files / 576 tests. SDK and fault-proof package typechecks
  passed. Scoped ESLint, Prettier and `git diff --check` passed at that snapshot.
- Added immutable order facts for both kinds: admission consumes the original
  nonce, binds eligibility to actual upper bound + wait, opens inline data by
  serialized size or references the actual retained output, and derives assets
  from actual Value. Withdrawal structural ADA is zero to preserve existing
  payout/refund funding. Deposit structural ADA is explicitly excluded.
- Presence capture retains payload and original-Value commitments without
  retaining pointer output references. Later reopening authenticates immutable
  preimages; operator withdrawal validity is separate from submitted body and
  signature. Focused tests reached 100/100 before semantic composition work.
- Semantic admission/promotion/reclamation composition is in progress. Proposed
  standalone filler reclaim requires recorded key approval plus exact refund;
  equal-key promotion requires exact refund with no filler-owner approval.
  A candidate finalized-retirement check combines normal settlement membership
  with authenticated current confirmed-root end >= actual eligibility, avoiding
  an invented settlement interval. This still needs executable integration.

Reproduction uses Node 22.22.2 and the Corepack-cached pnpm 9.15.4 executable:

```sh
MIDGARD_AIKEN_ENV=testnet PATH=/home/gumbo/.aiken-fork/bin:$PATH \
  node scripts/guard-focused-selector.mjs midgard/event_history
# cwd: onchain/aiken
PATH=/home/gumbo/.nvm/versions/node/v22.22.2/bin:$PATH \
MIDGARD_REAL_BLUEPRINT_PATH="$PWD/artifacts/event-history/aiken-retention-build/plutus.json" \
MIDGARD_EVENT_HISTORY_EVIDENCE_DIR="$PWD/artifacts/event-history/retention-emulator-second" \
node /home/gumbo/.cache/node/corepack/v1/pnpm/9.15.4/bin/pnpm.cjs \
  --dir demo/midgard-fault-proofs exec vitest run \
  tests/submit-init-emulator-event-history-retention.test.ts
# cwd: repository root; use a fresh evidence directory for a new run
```

The production list validator, both complete lifecycles, proof/consumer migration,
inline/maximum-operation fit, adversarial inclusion bounds, final deadlines,
restart/rollback and live acceptance remain incomplete. No ABI is frozen and no
fault-proof gap is closed by this checkpoint.

## List observer and applied admission checkpoint

- Semantic composition reached 128/128 focused cases: exact consumed-set
  admission/promotion/reclaim, finalized-frontier plus settlement retirement,
  original deposit assets, invalid-withdrawal refund and valid payout shape.
  Unit settlement cases check PHAS delegation; they do not execute PHAS.
- Added the multi-purpose `user-events/history` validator. Mint/spend yield to
  its exact zero withdrawal; the observer checks one full mutation including
  all consumed list inputs. Each kind has one reward account. Initialization
  consumes a configured one-shot nonce; later operations authenticate the hub.
  No production caller has switched to this validator yet.
- Resolved parameter circularity without adding hub fields: retention is now
  parameterized by hub identity and kind, and authenticates that kind's list
  policy/address through the hub on reclaim. Its applied address is therefore
  available before applying the list validator. SDK applies both together.
  This supersedes the earlier retention parameter interface and measurements;
  the prior source-bound evidence remains valid only for its recorded build.
- Fresh isolated pinned testnet build succeeded. Blueprint SHA-256:
  `9c79b89820459034821e41c1e810a8e6ddb0313e772cb5d1f8088325359adc38`.
  Unapplied list script 14,244 bytes; retention 2,772 bytes. Source inventory:
  `artifacts/event-history/history-source-identity.json`. Latest SDK/test hashes
  still need attaching before treating the eventual test run as bound evidence.
- Updated hub-bound retention emulator passed 9/9. Root reclaim: 3,329 signed
  bytes, fee 336,485, memory 415,870, CPU 145,014,498. Filler reclaim: 3,329 bytes,
  fee 337,939, memory 432,183, CPU 152,125,730. Full records/parameters are in
  `artifacts/event-history/retention-hub-emulator/retention-emulator.json`.
- Applied list tests use an explicitly provisional 512-byte inline limit and
  2-second observation duration, NOT accepted production bounds. Script plus
  fixture hub publication fit at 15,782 signed bytes. Actual initialization,
  filler insertion, inline deposit/withdrawal promotion and external deposit
  promotion passed under unchanged limits. External withdrawal promotion fails
  local UPLC evaluation and is under diagnosis; the suite is NOT passing.
- The emulator rejected registration plus first zero withdrawal in one
  transaction. Setup now separately registers the observer before use.
  Promotion also caught a fixture using a wallet base address for a refund that
  must target the fixed plain key address; the fixture now honors that rule.
- Snapshot measurements from passing cases: initialization 664 signed bytes;
  filler insertion 992; inline deposit promotion 1,045 / memory 1,267,874 /
  CPU 479,544,903; inline withdrawal promotion 1,312 / memory 1,395,398 /
  CPU 554,787,935; external deposit promotion 981 / memory 1,280,142 /
  CPU 608,026,758. All raw signed transactions retained under
  `artifacts/event-history/list-emulator-fifth/`. These are specific fixtures,
  not maximum-size or complete-lifecycle acceptance.
- SDK and fault-proof typechecks passed after adding the provisional schemas.
  No existing environments changed. Complete consumer/proof migration,
  adversarial availability, deadline/correction serialization, maximum fit,
  persistence/restart/rollback, installed replay and live gates remain open.

### External withdrawal defect resolved; continuation/public query verified

- Isolated the external-withdrawal failure to complete-datum hashing: Lucid
  encodes non-empty nested Value maps indefinitely; Aiken `serialiseData` uses
  definite maps. The same decoded datum produced different hashes. Reused
  `aikenSerialisedPlutusDataCborPreservingMapOrder` for publication and hashing,
  preserving map pair order and authenticating all metadata. Added cross-language
  fixture assertions for a withdrawal with a 4,000-byte destination datum.
  Payload decoding, normalized bytes and normalized hash now agree. Focused
  Aiken suite: **131 collected / 131 passed**.
- Normalized applied emulator run: **13/13 passed** across retention and all four
  inline/external deposit/withdrawal admission journeys. A later run extended
  all four journeys with an actual order predecessor pointer continuation;
  **4/4 passed**, preserving original payload/Value and retained-data reference.
- Added strict public SDK witness retrieval from canonical list/retention UTxOs.
  It authenticates full token keys, exact address and node role/ordering, selects
  a unique strict-gap/equal-filler/presence witness, opens existing retained data
  by complete normalized datum hash, and checks event ID and inline bound.
  Archives are not consulted. The applied journeys verify fresh out-ref lookup
  after a pointer rewrite. Watcher/node persistence consumers are NOT migrated.
- Build inventory comparison reports no source changes to the compiled list
  snapshot. Consumer hashes, blueprint/compiler binding and evidence paths are
  recorded in `artifacts/event-history/history-slice-evidence-identity.json`;
  refresh consumer hashes after final formatting/checks. No production ABI or
  timing/size recipe is frozen.
- Diagnostic attempts retained: verbose build is 33,646 bytes and was not
  published or accepted as fit evidence; no limits were raised. Two scratch
  selector attempts collected zero cases and are explicitly invalid evidence.
  The repository exact-test guard diagnosed the actual hash mismatch, and the
  subsequent guarded 131-case run proves the corrected fixture assertions.

### Latest verification and next implementation gates

- `history-slice-checked.log`: applied retention/list emulator **13/13 passed**,
  including all four pointer-continuation/public-retrieval journeys. Assertions
  remain against unchanged production transaction/execution limits.
- `sdk-history-slice-tests.log`: **66 files / 576 tests passed**. SDK and
  fault-proof typechecks passed after the query addition. Scoped ESLint passed
  after import ordering and `const` fixes.
- Ran the requested `pnpm --dir docs-site run check:links` from the repository
  root successfully (283 Markdown/MDX files). An earlier attempt incorrectly
  forced demo's pnpm 9 onto the separate docs pnpm 10 workspace and failed
  before running the link checker; both logs are retained, the successful one
  is `docs-links-history-slice-correct-toolchain.log`.
- Next funding gate: measure actual retirement outputs for large withdrawal
  destination/refund data and deposit structural-ADA separation. An admission
  that fits does not establish that its locked ADA can fund the later payout or
  refund output. Current applied tests stop after admission/continuation and
  must not be described as completed withdrawal lifecycles.
- Next deadline gate: a terminal token merely issued before merge still leaves
  a race before separate correction. Investigate atomic terminal issuance plus
  authenticated queue correction state, with explicit merge/append behavior
  only after a completed proof. Opening a proof must not freeze merge. This is
  a candidate requiring implementation and race tests, not a settled ABI.
- Next work remains applied settlement/refund/payout/unlink/reclaim and maximum
  fit, then automatic SDK lifecycle/retries, authenticated fabricated/transition
  proofs and staged reopening, watcher/node/persistence/rollback migration,
  deployment identities, installed replay, and live acceptance. Existing live
  deployment has not been reset/redeployed. No acceptance gap is closed yet.

## Applied retirement and size-driven observer split (in progress)

- Previous goal turn classification: progress (implemented source changes and
  source-bound Aiken/emulator/package evidence), not a wait or blocker.
- Extended applied journeys through deposit absorption/unlink, valid withdrawal
  payout initialization/unlink, invalid withdrawal refund/unlink, and separate
  external-data reclamation. `retirement-emulator-third.log`: **10/10 passed**.
  Hub/finality/settlement/payout mint authorities remain fixture native policies;
  the actual history/retention scripts and PHAS membership validator executed.
  Queue merge, the production payout mint/spend and final user payout are NOT
  established by those tests.
- Large withdrawal destination data required **19,403,620 lovelace** to fund its
  future payout output (inline fixture 1,603,320). The new retirement fixture
  computes that requirement before funding admission, while preserving the
  admission-only assertions. Production builders still need this calculation
  and an enforced admission/future-operation funding rule.
- Prior-layout measurements: deposit unlink 1,472 signed bytes, 1,797,670 memory,
  694,463,187 CPU; external deposit unlink 10,024 bytes, 1,869,526 memory,
  975,907,258 CPU. Inline withdrawal payout initialization 2,093 bytes,
  1,917,722 memory, 774,447,113 CPU; external 14,777 bytes, 2,013,560 memory,
  1,077,120,436 CPU. Refund inline/external 1,847/10,401 bytes. Subsequent data
  reclaims 3,329 bytes. Raw signed transactions/fees/parameters are retained in
  `artifacts/event-history/retirement-emulator-third/list-emulator.json`.
- The 14,777-byte case exposed duplicated leaf bytes: both the history witness
  and generic PHAS redeemer carried data already authenticated from an input.
  Changed the provisional retirement witness to just count, trie root and path;
  root/domain/leaf are derived from authenticated state and checked directly.
  To keep scripts publishable, retirement is now a separate rewarding observer,
  parameter-bound by the list validator. Both exact zero withdrawals are
  required; the list arm binds the same hub reference, and retirement still
  validates the entire consumed list-input set and exact mint/burn.
- Parameter order remains acyclic: hub identity/kind -> retention -> retirement
  observer -> list -> hub datum values. No additional hub fields or alternative
  history architecture. Initialization registers each required reward account
  before first use. Production deployment integration remains open.
- Unit fixtures now build actual singleton trie roots and execute membership,
  replacing the provisional fake root/redeemer-only setup. Kept content/finality
  refusal coverage and added direct verification without a redeemer-carried
  leaf. Guarded suite **132/132 passed**. Fresh isolated pinned build and adapted
  applied retirement scenarios are currently being verified; prior measurements
  do not apply to this new observer layout.

## Payload complexity and direct-retirement checkpoint

- Prior continuation produced real source/evidence progress; the goal remains
  active. Production consumers and proof families are still unmigrated.
- Fresh direct-membership build (before payload bounds) SHA-256
  `855648901dc646d37c5aa72bdc2dccd691dfa2b02c854cde90e3406ab0f6217a`:
  132 guarded Aiken tests and 19 applied emulator scenarios passed. Main list
  script was 10,085 bytes; retirement observer 12,140 bytes. Direct membership
  removes duplicated event bytes from the retirement witness.
- The maximum-shape experiment passed 14 cases but failed both 14,000-element
  integer-list admissions: byte limits alone did not prevent hashing from
  exhausting CPU. Original failures remain in `history-max-shapes-first.log`.
  Added an explicit deployment parameter limiting total Plutus Data nodes,
  counted before hashing, alongside total canonical payload bytes. Containers,
  map keys and values all count. Wide containers are traversed incrementally.
- Pinned compiler initially panicked on delayed recursive `choose_data` arms;
  used the existing repository convention of direct lazy branches. No compiler,
  evaluator, production resource limits or guard scripts changed. The first
  guarded complexity suite passed 136/136; two additional admission-bound tests
  are being checked after correcting a fixture `Option<Data>` annotation.
- Fresh isolated budget build SHA-256
  `78bab9b371aef9d0bff4f77fdc7b52f0e0f6901c0ac2faaf9ec9a878da428fa3`:
  main script 10,536 bytes, retirement observer 12,564 bytes. Parameter arity and
  names verified against source. Identity manifest:
  `artifacts/event-history/history-budget-source-identity.json`.
- Applied suite **27/27 passed** against that artifact with unchanged Cardano
  size/execution assertions (`history-budget-emulator-second.log`). First run
  had three 5-second runner timeouts; rerun explicitly used Vitest
  `--testTimeout 60000` for the new dense-data scenarios. This changes runner
  wall-clock allowance, not transaction limits or assertions. Hub/finality/
  settlement/payout-mint remain fixture authorities, not production acceptance.
- Exact 512-byte inline payloads complete both retirements. External 14,000-byte
  byte-string withdrawal datum completes payout initialization at **15,852
  signed bytes**, **2,981,305 memory**, **2,372,932,729 CPU**, fee **1,551,824
  lovelace**. Its 532-byte margin does NOT establish fit for a maximum-depth
  membership path. The 15,000-byte payload recipe remains provisional.
- Exact 1,024-node payloads also retire: withdrawal **2,405 bytes**, **13,541,627
  memory**, **7,734,685,062 CPU**, fee **1,956,069 lovelace**. This narrow memory
  margin still requires worst-shape/path measurements. Over-budget published
  data is refused without consuming the nonce or filler, then reclaimed through
  authenticated equal-filler absence and exact owner authorization. Reclaim is
  3,329 bytes and about 432k memory/152M CPU; no permanent receipt remains.
- Added SDK storage selection using canonical Aiken bytes and identical node
  counting before publication; five SDK tests pass. Actual automatic publication,
  insertion/retry, retirement and all production consumer wiring remain open.
- Required remaining gates: worst data shapes/proof paths, future-operation
  funding, authenticated completed-fraud fencing versus merge, and bounded
  adversarial pointer churn. Existing correction singleton already fences
  append/merge while Locked; terminal event-dependent proof issuance must
  acquire it atomically without blocking concurrent valid earlier-target proof
  completion or abandoning availability-challenge cleanup. No ABI freeze yet.

## Maximum paths, predecessors, and SDK builders checkpoint

- This continuation made implementation and verification progress. **Goal remains
  active; no ABI freeze or gap closure.** No production validator was replaced,
  no existing environment was changed, and the three pre-existing user document
  edits remain untouched.
- Added widest 64-step MPF membership paths using the repository's synthetic
  proof helper. These are accepted real MPF proofs against fixture-published
  roots; they measure carriage/evaluation, not the cost of grinding such keys.
  The prior 1,024-node withdrawal failed retirement memory at this depth
  (`history-deep-emulator-first.log`). A 512-node/6,000-byte candidate passed
  against a root predecessor, then failed with an actual 512-byte inline Order
  predecessor: **16,546 bytes > 16,384** (`history-predecessor-emulator-diagnostic.log`).
- Candidate recipe is now **512 inline bytes / 5,000 total payload bytes / 512
  Data nodes**. These are applied validator parameters, not raised ledger
  limits. Prior 14,000-byte/1,024-node singleton positives retain their original
  assertions under an explicit exploratory recipe and diagnostic test names;
  they do not establish deployable bounds. Added refusals/reclamation under the
  smaller candidate and full combined-bound/path/predecessor positives.
- Both combined maxima pass with a genuine admitted predecessor and 64-level
  path. Withdrawal retirement: **15,581 signed bytes, 11,204,388 memory,
  5,607,705,297 CPU, 2,247,599 lovelace fee**. Deposit retirement: **10,747
  bytes, 10,922,428 memory, 5,461,380,063 CPU, 2,003,640 fee**. Measurements are
  fixture-specific; maximum asset/address/data shapes and production payout
  policy/reference overhead still require verification before accepting a recipe.
- Added `history-build.ts` SDK builders for separate publication and current
  authenticated admission (strict gap insertion or equal-filler promotion).
  They bind the actual hub/list script, keep publication funding separate from
  the event nonce, authenticate the actual existing external-data UTxO, derive
  slot-aligned inclusion/protection bounds, reject spend/reference overlap,
  preserve predecessor facts/value, and refund a promoted filler without its
  signature. All completions use local evaluation and explicit funding inputs.
  The applied journeys now exercise these SDK builders. The maximum-predecessor
  fixture also exercises genuine direct Order insertion, not a fake mint.
- Automatic storage selection is implemented in `prepareEventHistoryPayload`.
  Lifecycle orchestration, contention retry/reconciliation and durable submission
  journals are **not** implemented yet; publication/admission are still separate
  builder calls in the fixture. SDK retirement/refund builders and production
  initialization also remain to migrate.
- Final verification at this checkpoint:
  - Pinned guarded Aiken selector: **138 collected, 138 passed**
    (`payload-budget-check-fourth.log`). Two extra admission-bound tests extend
    the 136-test source snapshot used for the last clean build.
  - Pinned isolated testnet build: passed; blueprint
    `78bab9b371aef9d0bff4f77fdc7b52f0e0f6901c0ac2faaf9ec9a878da428fa3`.
    All **1,149** prior validator entries retain identical compiled code.
    This is a preservation check, not installed-family replay acceptance.
  - Both applied emulator files: **39/39 passed, no filtered/skipped cases**
    (`history-builders-final.log`), with explicit `--testTimeout 60000`.
  - SDK suite: **67 files / 581 tests passed** (`history-builders-sdk-suite.log`).
  - SDK and fault-proofs package typechecks: passed
    (`history-builders-typecheck-second.log`).
  - Scoped ESLint, formatting and `git diff --check`: passed.
  - Exact root command `pnpm --dir docs-site run check:links`: passed,
    **283 Markdown/MDX files** (`history-builders-docs-links.log`).
- Evidence manifest `artifacts/event-history/history-builders-evidence-identity.json`
  binds compiler, blueprint, current slice source and evidence hashes; each
  signed-transaction record contains parameters, script/policy identities,
  bytes, fees and execution budgets. The clean compiler snapshot identity is
  `history-budget-source-identity.json`. These are local emulator deployments,
  not acceptance against the existing live manifest.

### Immediate next gates (before broad consumer migration)

1. **Future-operation funding:** current fixtures compute payout minimum ADA,
   but admission does not yet enforce closure for minimally funded links,
   future pointer growth, reserve output minimums, large refund destinations,
   and valid withdrawal payout initialization. Resolve this without changing
   original deposit Value, including filler funds in deposits, or inventing an
   early refund. Existing exact-value continuation/output rules make this a
   real safety/liveness gate, not just SDK convenience.
2. **Deadline versus merge:** implement atomic completed-event-fraud acquisition
   of the existing correction singleton, with authenticated pending-header
   deadline and safe handling of an earlier completed fault while another
   correction holds the singleton. Preserve DA timeout/bond cleanup and all
   shared correction consumers. Proof initiation must not silently freeze merge.
3. **Targeted churn:** apply repeated competing predecessor mutations and witness
   capture transactions, measure retries/inclusion assumptions and choose a
   defensible protection duration. The 2-second fixture duration remains only a
   test recipe. Retained facts must survive later pointer changes and retirement.
4. Finish lifecycle SDK orchestration/retries, then migrate all fabricated and
   transition-trace evidence/staged reopening, queue/settlement/refund/payout,
   watcher/node ingestion/projection, restart/L1 rollback/L2 correction, public
   retrieval and deployment/artifact/catalogue identities. Run installed-family
   replay/fit and required package/live acceptance after that integration.

Latest applied-suite reproduction (repository root):

```sh
PATH=/home/gumbo/.nvm/versions/node/v22.22.2/bin:$PATH \
MIDGARD_REAL_BLUEPRINT_PATH="$PWD/artifacts/event-history/aiken-history-budget-build/plutus.json" \
MIDGARD_EVENT_HISTORY_EVIDENCE_DIR="$PWD/artifacts/event-history/history-builders-final" \
node /home/gumbo/.cache/node/corepack/v1/pnpm/9.15.4/bin/pnpm.cjs \
  --dir demo/midgard-fault-proofs exec vitest run \
  tests/submit-init-emulator-event-history-list.test.ts \
  tests/submit-init-emulator-event-history-retention.test.ts --testTimeout 60000
```

Use a fresh evidence directory for subsequent runs. Live acceptance remains
incomplete: the previously inspected Docker/Kupo/Ogmios prerequisites were
unavailable and the existing manifest is not bound to this source. Do not reset
or redeploy it. Independent implementation gates above remain substantial.

### Funding closure and completed-fraud queue marker (in progress)

- Admission now checks a conservative minimum-output bound under the compiled
  `coins_per_utxo_byte` (4310): ledger entry overhead 160 bytes, 96 bytes for a
  maximum Shelley address and output framing, canonical Plutus Value/datum bytes,
  and an additional 80 bytes when a distinct full-width payout NFT will be added.
  No output in this calculation carries a reference script. Original deposit
  Value must independently fund its reserve output; structural/filler funds are
  not counted. Withdrawal funds must cover the exact refund datum and initial
  payout, fit within the requested target, and the target must fund the completed
  payout. Links reserve for a full successor key and the largest permitted
  protection timestamp (signed 64-bit nonnegative); exact-value continuation is
  preserved. SDK admission applies the same checks.
- SDK CML comparisons cover 84 address/value/datum combinations (including a
  maximum pointer address, ten full-width assets and large/nested datums), twelve
  added-NFT cases, and successor/timestamp growth. Funding/payload tests: 8 passed.
  Pinned focused Aiken history tests: 142 passed. The clean funding build is
  `artifacts/event-history/aiken-history-funding-build/plutus.json`;
  `history-funding-build.log` records success. Existing applied scenarios: 39
  passed (`history-funding-emulator-first.log`); six additional genuine script
  refusals passed (`history-funding-refusals-first.log`). A combined 45-case run
  and a new source/evidence identity remain due. Both affected package typechecks
  passed at the funding-only checkpoint. These measurements do not establish a
  live deployment or complete the retirement/SDK lifecycle acceptance matrix.
- **Deadline design revision:** use a per-header `proven_fraud` queue marker,
  written atomically by completed proof minting, instead of acquiring the global
  correction singleton in that transaction. Consuming the challenged queue input
  invalidates a prepared merge; the continued marked node cannot merge. Proof
  initiation never sets the marker. Independent headers avoid singleton contention
  and earlier honest headers can still merge. Existing finite suffix correction
  and its lock remain responsible for removal and operator/DA cleanup. No new
  event receipt is introduced. The selected marker names an existing permanent
  fraud-proof NFT. This supersedes immediate gate 2's pending-singleton proposal.
- Initial implementation adds the optional marker to the queue datum, a spending
  arm bound to the trusted fraud-proof policy, exact header/link/status preservation,
  actual mint quantity one and a finite terminal window strictly before maturity.
  Merge rejects marked nodes; append initializes an unmarked node; DA status changes
  preserve the marker. SDK schema, parameter application and fresh fixtures are
  being migrated. **Atomic terminal proof coupling, already-marked reference
  handling, runtime/indexer handling and adversarial applied tests are not yet
  implemented. The deadline gate remains open and ABI is not frozen.**
- The prior claim of identical compiled existing validators applies only to the
  earlier funding/history checkpoints. Queue schema changes necessarily invalidate
  that identity; all shared consumers and applied-parameter evidence must be rerun.

### Queue marker checkpoint and shared-consumer verification

- `require_completed_fraud_witness` now defines the event-history terminal seam:
  authenticate the exact challenged queue NFT/header, require its new output marker
  to equal the terminal proof identity, or reference an already marked header.
  The reference path accepts a different proven category for the same header and
  does not rewrite the node. Both paths reject terminal windows at/after maturity.
  Event-history-dependent terminal steps must call this helper in their migration;
  **none call it yet**. Unrelated proof families retain their existing terminal
  semantics; this work does not claim their independent deadline gaps closed.
- The marker spend is parameterized by the trusted fraud-proof policy and requires
  actual quantity-one minting. It preserves header, link, DA status, address and
  locked ADA, with permissionless ADA top-up allowed for the larger datum. Root
  and append behavior cannot fabricate a marker. DA status changes preserve it.
  Existing finite correction/pruning still removes marked invalid headers.
- SDK queue schemas, constructors, spend parameter application, DA continuation,
  ABI vectors and affected fixtures were updated. Merge builders refuse marked
  nodes. Node merge readiness and CLI diagnostics explicitly report
  `skipped_oldest_block_proven_fraud` and required state correction; the marker is
  included in semantic candidate identity. Watcher marker-transaction indexing,
  durable projection/restart/rollback and correction scheduling are **still due**.
- Pinned checks: all **62** `state-queue-removal.test` tests passed, including
  **26** completed-fraud tests; all **6** `midgard/ledger-state.test` ABI cases
  passed. `history-marker-queue-check-all.log` and
  `history-marker-ledger-check.log` retain exact-selector reports. A subsequent
  focused marker run after minimizing unrelated formatting also passed **26/26**.
- Applied marker scenario file: **11/11 passed**
  (`history-marker-applied-second.log`). It uses the real four-parameter queue
  spending validator and fixture native queue/proof issuers. Nine script refusals
  include foreign/missing mint, wrong header, rewrite/clear, link/status/address
  mutation and input redirection. A competing prepared spend loses the consumed
  input. This is **not** the production terminal-policy/merge race acceptance.
  A successful marker transaction measured **5350 signed bytes, 358036 memory,
  163665428 CPU, 427904 lovelace fee**; no production limits were increased.
- Combined applied funding/history run passed **45/45**
  (`history-funding-combined.log`). The marker build retains byte-identical history
  validators. Its queue schema changes **148 existing compiled entries**, including
  header-opening consumers. The list is retained in
  `history-marker-compiled-diff.json`; installed-family replay/fit is mandatory
  before release, and is **not** replaced by this comparison.
- Clean marker blueprint at this checkpoint:
  `938e3fb3d57d4a53406beeecb261b2671c882030333ededfac0cbd86c66d500d`.
  Verified four spend parameters and **3961 unapplied script bytes**. The local
  ignored `onchain/aiken/plutus.json` was refreshed because recipe tests read it
  directly; its prior bytes were preserved as
  `pre-marker-blueprint-04790ae612c2478aa089a7f08089f11e43638cf007a2f562fa8b445f4ee25f6e.json`.
  No environment was reset or redeployed. Initial recipe failures remain logged.
- Exact applied registry assertions were retained and repinned for the new ABI:
  full bundle `81594f78bda0635a3de0d08f4416be86d6dc05a3ff313f4b0e68595880484dce`,
  queue/correction subset `0127a10a7407d6be49c1386a069f07246d1c20923f51d2d1f3adbf0addaaf694`.
  An independent source-mode recipe invocation records **613** applied script
  entries in `history-marker-registry-identity.json`. Stand-in inventory, parameter
  arity/shape and hash assertions remain in force. SDK recipe tests **29/29** and
  node registry tests **8/8** passed after repinning.
- SDK, fault-proofs, node, watcher, DA committee and node-tools typechecks passed
  at this checkpoint. Exact root `pnpm --dir docs-site run check:links` passed
  **283 files**. Scoped ESLint passed. The final clean source build, package-suite
  reruns and complete evidence manifest are being recorded below.

Final checkpoint results (2026-09-22):

- SDK suite: **68 files / 584 tests passed** (`history-marker-sdk-suite-final.log`).
  Focused node checks: **7 files / 57 tests passed**
  (`history-marker-node-tests-final.log`). Affected watcher fixtures/indexing and
  replay tests: **6 files / 63 tests passed**
  (`history-marker-watcher-tests-first.log`). Those exercise existing behavior
  with the new datum, not new marker ingestion or authenticated event history.
- Final isolated compiler snapshot, including the terminal witness helper:
  `history-marker-final-source-identity.json`; blueprint SHA256
  `414bd2321984cba135b57866e50f96a385305d7c6190553c3595c3555bf962ba`.
  Compared with the earlier marker build, only the commit-yield spend/else pair
  changes compiled bytes (the two independent append guards now run in the final
  source order). The queue marker spend and all history programs are identical.
  Local generated blueprint was refreshed again. Final queue module: **62/62**.
  SDK queue builders/recipes: **47/47**; node registry: **8/8** on this artifact.
- Final registry identities are
  `2d6c61f00f483c48cf799c9b580a88ce65cad52137e6de792c6f502b0b6a4fe6`
  (full) and
  `7ea695e5bf105e14c6e1bbce64f93f31afdc337ab1c9ab8ccac401b7d62ffd64`
  (queue/correction), recorded independently in
  `history-marker-final-registry-identity.json`. Earlier identities above remain
  historical checkpoints, not final deployment claims. Only test-fixture
  whitespace changed after the final compiler snapshot.
- Read-only live prerequisite recheck at 07:58 UTC: Docker daemon unavailable;
  Kupo `127.0.0.1:1442/health` and Ogmios `127.0.0.1:1337/health` both refused
  connections. Evidence: `history-marker-live-prerequisites.json`. There was no
  reset/redeployment/service mutation. Existing deployment/source identity still
  does not establish acceptance for this implementation. Live acceptance remains
  incomplete; independent proof/lifecycle integration work continues.

Commands used for this checkpoint (repository root; Node22/pnpm9 paths as above):

```sh
MIDGARD_AIKEN_ENV=testnet PATH=/home/gumbo/.aiken-fork/bin:$PATH \
  node onchain/aiken/scripts/guard-focused-selector.mjs completed_fraud
# All 62 exact names were read from the source module and passed together to:
node onchain/aiken/scripts/run-focused-check.mjs state-queue-removal.test <62-exact-test-names>
node onchain/aiken/scripts/run-focused-check.mjs midgard/ledger-state.test <6-exact-test-names>
# Builds used a clean copy containing only lib/, validators/, env/, aiken.toml,
# and aiken.lock; the command in each copied project was:
/home/gumbo/.aiken-fork/bin/aiken build --env testnet
pnpm --dir demo/midgard-sdk test
pnpm --dir demo/midgard-sdk exec vitest run tests/state-queue.test.ts tests/state-queue-contracts.test.ts
pnpm --dir demo/midgard-node exec vitest run tests/merge-readiness.test.ts tests/merge-maturity-preflight.test.ts tests/merge-maturity-builder.test.ts tests/state-queue-topology.test.ts tests/commit-block-header-state-queue-tail.test.ts tests/midgard-contracts.test.ts tests/sdk-abi-fixtures.test.ts
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-completed-fraud-marker.test.ts --testTimeout 60000
pnpm --dir docs-site run check:links
```

Next implementation priority: migrate both fabricated-event families from their
old exact-unspent-nonce/live-event evidence into bounded authenticated history
capture and reopening. Their step-01 hub/header opening already returns the hub;
carry its authenticated queue policy through immutable step state, and require
`require_completed_fraud_witness` at terminal step-04. Capture original deposit
Value, keep withdrawal body/signature distinct from the operator validity verdict,
apply capture/terminal deadlines and real predecessor protection, and update the
SDK submission/staged reopening path with applied positive/negative tests before
extending the same seam to every affected transition-trace consumer. The completed
marker helper alone is not a closed deadline or end-to-end acceptance gate.

SDK artifact check: `pnpm --dir demo/midgard-sdk run build` passed ESM, CJS
and declarations (`history-marker-sdk-build.log`). Repeating the registry recipe
through the built SDK, without source-mode resolution, produced an **identical
613-entry registry** (`history-marker-built-registry-identity.json`). Checkpoint
source/compiler/blueprint/parameter/evidence hashes and the complete exact Aiken
argument arrays are in `history-marker-evidence-identity.json`. The log retains
initial failures and later remedies; no acceptance assertion was weakened.

## Paired fabricated-history migration (2026-09-22, in progress)

- Both Aiken families now capture sorted-list absence (authenticated strict gap or
  equal-key filler) or immutable Order commitments. The exact-unspent-nonce arm
  has been removed from these validators. Current absence also covers an ID that
  was legitimately retired; it does not claim that the nonce never existed.
- Step 01 carries the authentic hub's queue policy through steps 02–04. Capture
  and reopening require a finite range after the challenged header end and before
  earliest merge. Terminal issuance requires an atomic completed-fraud marker
  output or an authenticated previously marked reference for this same header.
  Opening a thread still does not lock merge. Other terminal families have not
  been migrated and are not covered by this claim.
- Present-event evidence retains policy, kind, ID, inclusion time, full payload
  hash and original L1 Value hash. Step 03 reopens payload plus original Value;
  it does not need the old pointer UTxO. Withdrawal body/signature fidelity remains
  separate from the operator's validity verdict. A new third fault constructor
  classifies ineligible timing even when content matches; existing absence and
  content-mismatch constructor indices remain 0 and 1. Event IDs are unchanged.
- Step 02 now applies retention address and explicit measured byte/Data-node
  bounds. SDK family schemas, immutable commitment/opening helpers, queue-marker
  witness and parameter-application builders have been migrated. Full-contract
  construction requires explicit history bounds; no production default has been
  introduced. Deployment/runtime configuration and submission/staged consumers
  still need migration. The local blueprint is not yet refreshed for this slice.
- Pinned testnet Aiken checks: deposit family **16/16**, withdrawal **18/18**;
  expanded terminal cases bring both families to **46/46**. Separate history
  evidence module **22/22** covers arbitrary IDs against an authenticated empty
  root, equal filler, live-nonce rejection, Order non-absence, spent-reference
  rejection, deadlines, pointer continuation, timing and forged original Value.
  Atomic marker-output unit fixtures do not substitute for actual queue-spend
  plus terminal-policy emulator transactions.
- SDK paired byte twins **20/20**. Handoff/opening literals were measured from
  pinned Aiken testnet output in `history-fabricated-evidence-22.log`, preserving
  all previous assertions and extending them. SDK typecheck passed after explicit
  bounds were added to affected SDK builder fixtures. Initial compilation/type
  errors and import-sort lint failures are retained in diagnostic logs. A clean
  pinned testnet build is underway from a source-only copied project; its source
  hash map is `history-fabricated-build-source.json`.
- Root `pnpm --dir docs-site run check:links`: **283 files passed**
  (`history-fabricated-docs-links.log`). No deployment/service mutation occurred.

Remaining immediate gates: finish paired proof construction/submission and staged
reopening, bind bounds to deployment/manifests, apply new scripts, exercise real
four-step proofs with genuine list admissions and atomic queue marking, measure
maximum proof fit/fees/budgets and merge/contention races, then migrate all affected
transition-trace consumers. Automatic user lifecycles/retries, production queue
settlement integration, watcher/node projection, public evidence, restart/rollback,
L2 correction, installed-family replay/fit and live acceptance remain incomplete.
The two-second observation protection remains a fixture value, not a verified
production capture guarantee. ABI is not frozen and no overall readiness gap is
closed by these unit results.

Clean-build remedy: the initial copied-project build failed at blueprint emission
because opaque `cardano/assets.Value` appeared in step-03's public redeemer. The
opening now uses `Data` at that boundary; `order_facts.opens` checks the exact
serialized bytes against the hash of the genuine Value captured from L1. It does
not rebuild, reorder or normalize a prover's map before comparing that hash.
The SDK retains its stricter Value-shaped encoder, with identical wire bytes.
Both family selectors passed **46/46** after this change; malformed-Data opening
negatives were added. The initial failed build and diagnostic remain retained.

Verification checkpoint after the boundary remedy:

- Clean pinned testnet build passed. Blueprint SHA-256:
  `13b26e1d656979399fa28b3cac40d2bfa3ac24612f59b3f4cb6422e6007cc5c3`.
  The ignored local blueprint was refreshed after checking its old hash and
  retaining `history-pre-fabricated-marker-plutus.json`. Exactly the eight
  fabricated-family steps changed compiled code (16 blueprint entries including
  else aliases); every other validator is byte-identical to the marker build.
  Unapplied step sizes are recorded in `history-fabricated-blueprint-identity.json`;
  they are not transaction fit/fee/execution measurements. The pinned formatter's
  eight trailing-whitespace lines were stripped afterward; the copied build
  source and the whitespace-only differences remain identifiable in evidence.
- Aiken history suite **166/166**, including **24** new fabricated-evidence
  cases, and paired family selectors **46/46** passed. New malformed raw-Value
  openings fail authentication. Existing SDK byte assertions remain intact.
- Full SDK suite **68 files / 588 tests passed**. SDK typecheck, whole-package
  lint, ESM/CJS/declaration build and final `git diff --check` passed.
- Downstream checks deliberately expose unfinished work: fault-proofs typecheck
  failed with **35 errors in 19 files**; node typecheck failed with **22 errors in
  14 files**, including imported fault-proofs sources. These are missing history
  bounds/queue-policy handoffs and old evidence/opening/terminal schemas. Logs:
  `history-fabricated-fault-proofs-typecheck-pending.log` and
  `history-fabricated-node-typecheck-pending.log`. They are remaining migration
  gates, not waived checks. No installed-family replay or new-family applied
  emulator pass is claimed.
- Read-only live prerequisite recheck at **12:50 UTC**: Docker daemon unavailable;
  Kupo 1442 and Ogmios 1337 refused connections. No reset/redeploy/start was
  attempted. The previous deployment does not match this new blueprint. Live
  acceptance remains incomplete, independently of the source integration work.

Commands for this checkpoint (repository root unless stated; Node22/pnpm9):

```sh
MIDGARD_AIKEN_ENV=testnet PATH=/home/gumbo/.aiken-fork/bin:$PATH node onchain/aiken/scripts/guard-focused-selector.mjs fraud_proofs/fabricated_
MIDGARD_AIKEN_ENV=testnet PATH=/home/gumbo/.aiken-fork/bin:$PATH node onchain/aiken/scripts/guard-focused-selector.mjs midgard/event_history
# All 24 exact source test names were passed together to:
node onchain/aiken/scripts/run-focused-check.mjs midgard/event-history/proof-evidence.test <24-exact-test-names>
# In the fresh source-only copied Aiken project:
/home/gumbo/.aiken-fork/bin/aiken build --env testnet
pnpm --dir demo/midgard-sdk test
pnpm --dir demo/midgard-sdk run typecheck
pnpm --dir demo/midgard-sdk run lint
pnpm --dir demo/midgard-sdk run build
pnpm --dir demo/midgard-fault-proofs run typecheck # FAIL: migration pending
pnpm --dir demo/midgard-node run typecheck # FAIL: migration pending
pnpm --dir docs-site run check:links
git diff --check
```

Design-update reconciliation: the preserved user-owned design document changed
since the marker checkpoint (current file mtime 07:30:34 CDT). Its SHA-256 changed
from `2bc015cff870aa518c0c8d6383afb83a83e88dbd9d5acddba92a101b9ca16850` to
`f1462fde3efa114fed8bfb352eebb0b5c54bbaaaae75b8916fa5f9854093c3c4`.
This task did not write that file. The complete current design was reread;
research and remaining-gaps still match their prior hashes.

The current design explicitly removes deposit/withdrawal per-order stake
registration and replaces its double-satisfaction dependency with complete input
accounting and exclusive output claims, including cross-kind transactions. The
new history path already omits per-order witness fields/certificates; legacy
production deposit/withdrawal and shared forced-order consumers are not yet
migrated. Existing history mutations validate their complete per-policy input
sets. **Cross-kind output claim disjointness remains unimplemented/unverified**:
identical filler/structural refund targets or overlapping settlement/refund
obligations must not share one output across the two policies. A zero-withdrawal
observer must authenticate the other participating kind's declared operation and
claims, not trust a redeemer-selected subset. Positive distinct-output and
negative reused-output combinations need applied measurement with the same
production limits. This gate now precedes further broad proof/submission
migration; the current isolated slice is not ready for deployment. Preserve the
forced-order witness machinery until its remaining consumers no longer need it.

## Cross-kind output ownership (2026-09-22)

Previous goal turn classification: progress (paired proof validators/SDK codecs,
clean build and tests, plus discovery of the updated output-ownership gate).

- Added `event-history/output-claims.ak`. The observer derives claims from the
  executed operation: continuation/new nodes, filler refunds, retirement funds
  and structural refunds. All claimed indexes must exist and be distinct across
  both deployed policies. Unclaimed wallet change remains allowed. The other
  kind's claims are read only from its exact zero-withdrawal redeemer; absent
  invocation cannot hide consumed list inputs or that policy's mint/burn.
- `RetireOrder` now carries its three output-index fields (optional structural
  refund). The list observer checks them against the exact deployed retirement
  observer's witness, so a caller cannot advertise a smaller claim set. Complete
  per-policy input accounting still comes from the existing mutation checks;
  each spending invocation still binds its index to `own_ref`. No per-order
  certificate, shared mutable claim UTxO, extra receipt or new observer was added.
- Added typed SDK operation/observer schemas and retirement-claim derivation;
  admission uses the same typed schema. Applied fixtures now register distinct
  deposit/withdrawal list policies instead of aliasing both hub entries to the
  tested kind. Shared forced-order certificate machinery remains unchanged.
- Pinned Aiken focused ownership tests **17/17**, complete history tests
  **183/183**, and clean source-only testnet build passed. Blueprint SHA-256:
  `3d04859765f10a154e7bfbc2a7daeb6063e2d216754d1cf39cd5981c8af2fb19`.
  The applied list script's unapplied size is 12,376 bytes; reference publication
  is exercised under the existing L1 limits.
- New applied ownership suite **11/11** with both genuine list policies. It
  initializes both lists, inserts fillers, promotes genuine events without a
  filler-owner required signer, reclaims fillers and retires a deposit plus an
  invalid withdrawal to the same destination with distinct outputs. Refusal cases
  cover shared refunds, continuation/payment reuse, missing other withdrawal,
  wrong `own_ref` index, hidden filler input, shared retirement funds, omitted
  structural-refund claims, and structural-refund/payment reuse. Finality and
  settlement issuers remain explicit native fixtures; this is not production
  queue/settlement or end-to-end proof acceptance.
- Existing lifecycle/retention suites **45/45** still pass, including exact
  512-byte inline predecessor, combined 5,000-byte/512-node payload bounds and
  64-level settlement paths. Full SDK suite **68 files / 588 tests**, SDK build,
  and touched-file lint passed. The initial applied positive failed because the
  test paid a base address instead of the specified enterprise refund address;
  correcting that fixture made the positive and all negatives pass. The failure
  remains retained, and none of the refusal assertions were weakened.

Representative paired-operation measurements (signed transaction, unchanged
protocol limits; full CBOR and recipes in `history-output-claims-applied.json`):

| Operation | Bytes | Memory | CPU steps | Fee (lovelace) |
| --- | ---: | ---: | ---: | ---: |
| Initialize both lists | 938 | 689,746 | 280,274,487 | 634,026 |
| Insert two fillers | 1,558 | 2,485,818 | 915,817,443 | 810,762 |
| Reclaim with distinct refunds | 1,295 | 2,967,496 | 1,055,049,444 | 837,021 |
| Promote both fillers | 1,815 | 4,020,118 | 1,667,468,618 | 964,793 |
| Deposit absorption + withdrawal refund | 1,673 | 5,864,299 | 2,227,823,297 | 1,561,626 |

The one-operation-per-kind output-ownership slice is now implemented and tested.
This does not close overall funds/proof/lifecycle readiness: original Value must
still reach every L2 projection/proof consumer, legacy event certificates must be
removed from production deposit/withdrawal flows, and the deployment/runtime ABI
migration remains incomplete. The next unresolved prerequisite is measured
challenge capture under adversarial pointer churn; two-second fixture protection
is not a production inclusion guarantee. Keep ABI unfrozen while resolving that
and completing the paired proof submission/deployment/staged consumers, then all
remaining transition, watcher/node, recovery, installed replay and live gates.

Measurement rerun with explicit evidence output retained **45/45** lifecycle
results under `artifacts/event-history/history-claims-lifecycles/`. The largest
candidate (excluding exploratory 15k/1024 recipes) remains withdrawal settlement
and unlink: **15,587 signed bytes**, **797-byte margin**, **11,401,442 memory**,
**5,680,358,002 CPU steps**, **2,292,522 lovelace fee**. Full transaction bytes,
compiled blueprint identity, parameters and per-operation measurements are saved.
The fault-proofs typecheck still reports exactly the prior **35 errors / 19 files**
from the pending fabricated/deployment migration; the new ownership scenarios
introduce no additional type errors. SDK typecheck passed.

Commands (repository root; pinned Node22/pnpm9 paths retained in command logs):

```sh
MIDGARD_AIKEN_ENV=testnet PATH=/home/gumbo/.aiken-fork/bin:$PATH node onchain/aiken/scripts/guard-focused-selector.mjs midgard/event_history/output_claims
MIDGARD_AIKEN_ENV=testnet PATH=/home/gumbo/.aiken-fork/bin:$PATH node onchain/aiken/scripts/guard-focused-selector.mjs midgard/event_history
# Fresh source-only project: /home/gumbo/.aiken-fork/bin/aiken build --env testnet
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-event-history-output-claims.test.ts --testTimeout 60000
MIDGARD_EVENT_HISTORY_EVIDENCE_DIR=/home/gumbo/midgard-hub/midgard/artifacts/event-history/history-claims-lifecycles pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-event-history-list.test.ts tests/submit-init-emulator-event-history-retention.test.ts --testTimeout 60000
pnpm --dir demo/midgard-sdk test
pnpm --dir demo/midgard-sdk run typecheck
pnpm --dir demo/midgard-sdk run build
pnpm --dir demo/midgard-fault-proofs run typecheck # incomplete migration: 35 errors
pnpm --dir docs-site run check:links
git diff --check
```

No live service, deployment or durable environment was modified in this turn.
The prior verified live prerequisites remain incomplete; independent source work
continues, so the goal is active rather than blocked or complete.

## Bounded challenge capture under pointer churn (2026-09-22)

Previous goal turn classification: progress (cross-kind output ownership applied
and measured). This turn adds the client capture mechanism and an adversarial
applied slice for both fabricated families. The overall goal remains active.

### Conditional availability argument and implementation

- A mutation included at time `t` creates every replacement node with protection
  ending at `transaction inclusive upper + D`, hence no earlier than `t + D`.
  The actual list observer refuses another spend before that bound. An insertion
  dividing an absence gap produces a new authenticated predecessor; an insertion
  after an Order preserves its facts and Value at a new output reference.
- Let `V` bound replacement visibility and `S` bound construction, signing,
  propagation and inclusion. Require `D > V + S + one slot`. Given those
  assumptions and a current authoritative view, a challenger observing the
  replacement can reference it before any subsequent mutation is legal. An old
  unprotected node can be tried optimistically; a conflicting mutation creates
  the protected opportunity. Repeated retries alone do not establish this bound.
- Added SDK `history-capture.ts`: explicit timing budgets, protection-inequality
  validation, validity planning with a 60-second lower-bound backoff constrained
  by the accused interval, slot slack, and a reserved budget for every remaining
  proof stage before merge. It refuses an insufficient deadline both before and
  after provider reads. The bounded retry loop re-fetches L1 witnesses only after
  an explicitly classified reference conflict. Validation errors, ambiguous
  submissions and confirmed-output visibility failures propagate for
  reconciliation; they do not trigger blind resubmission.
- Candidate applied recipe: `D = 120 seconds`, `V = 20 seconds`, `S = 40 seconds`,
  one-second slots, and 80 seconds reserved for subsequent proof stages. These
  are **explicit simulated inclusion assumptions, not measured live guarantees**.
  The two-second lifecycle fixture fails the inequality and is not promoted to a
  production default. A deployment still needs measured visibility/inclusion,
  adequate challenger funding/fees and a fair-inclusion assumption. L1 censorship
  or an unbounded provider delay defeats a finite deadline; no merge freeze or
  deadline extension was introduced to hide that limitation.
- The planner/retry API is exercised by the applied slice; production paired
  submitters and deployment configuration still need to consume it. ABI remains
  unfrozen. The mechanism now has a conditional argument plus targeted applied
  evidence; the live parameter/availability gate remains incomplete.

### Applied evidence

Extracted the existing genuine two-list setup/promotion fixture into
`tests/support/emulator/history-pair.ts`, preserving its assertions and original
40-second waits for the two-second recipe. Longer protection is an explicit
fixture parameter. Both real list policies and real fabricated steps 02/03 run
with locally evaluated transactions and unchanged L1 limits. Hub authority and
initial computation-thread issuance are **native fixtures**. The terminal output
uses a fixture destination; these tests do not execute production step 01,
step 04, catalogue issuance, queue marking or final settlement.

New capture suite: **8/8**. Across each family's arbitrary-ID absence and genuine
Order presence cases, three targeted mutations invalidate selected witnesses.
The first spends an unprotected witness before submission. The next two
intentionally exceed the declared inclusion budget, wait out protection and
replace the selected pointer again: their expired submissions also have a
provably spent reference while fee/thread inputs remain unspent. This explicitly
exercises failure outside the assumptions. The fourth attempt references the
fresh protected replacement; an immediate second mutation is refused and the
capture succeeds. There are **12 retained stale attempts** across four cases.

After successful capture, another legal pointer mutation spends the old witness.
Stage 03 still succeeds using the captured commitment and serialized opening,
with no live history reference. A substituted original Value fails for both
families. This is opening serialization/reuse, **not process restart acceptance**.
Additional applied cases refuse fraud against honest eligible events after
pointer continuation, including an operator-only withdrawal validity difference;
they also refuse capture and classification ranges overlapping the merge deadline.
The unchanged ownership suite passes **11/11** after fixture extraction.

Representative signed transactions from `history-capture-applied.json` (native
fixture CT authority; ordinary payloads, not maximum-payload proof fit):

| Operation | Bytes | Memory | CPU steps | Fee (lovelace) |
| --- | ---: | ---: | ---: | ---: |
| Deposit absence capture | 876 | 601,155 | 217,234,419 | 356,336 |
| Deposit Order capture | 1,030 | 961,580 | 389,306,111 | 396,315 |
| Withdrawal absence capture | 876 | 598,951 | 216,601,498 | 356,523 |
| Withdrawal Order capture | 1,030 | 1,304,320 | 561,582,992 | 428,872 |
| Deposit retained-Order classification | 881 | 258,661 | 116,431,604 | 275,781 |
| Withdrawal retained-Order classification | 1,113 | 314,989 | 155,205,418 | 292,905 |

Targeted pointer mutation examples cost 493,115 lovelace / 1,093 bytes for a
deposit gap and 509,809 lovelace / 1,479 bytes after a withdrawal Order. Full
signed CBOR, recipes, applied policy identities, protocol parameters and stale
attempts are retained. These costs do not establish a censorship-resistance or
production throughput guarantee.

### Checks and remaining gates

- SDK full suite **69 files / 595 tests**, including seven capture timing/retry
  tests; SDK typecheck/build and touched-file lint passed.
- Applied capture **8/8** and existing ownership **11/11** passed. Initial new
  fixture failures exposed tuple-shaped CT redeemers, the withdrawal content-hash
  field name, the emulator's exact expired-range error, and a nonexistent generic
  invalid-withdrawal enum. Fixed the fixture encodings and used the actual
  `NonExistentWithdrawalUtxo` verdict. Original refusal assertions remain intact;
  obsolete or unsupported matcher syntax was replaced with equivalent assertions.
  Intermediate failure logs are retained.
- Fault-proofs typecheck is still **incomplete: 35 errors / 19 files**, exactly
  the existing paired submitter/deployment/test migration inventory. The new
  capture/fixture files have no remaining type errors. This is not a waived gate.
- Aiken source did not change. All **1,182** source/config hashes match the prior
  clean source-only pinned testnet build; blueprint remains
  `3d04859765f10a154e7bfbc2a7daeb6063e2d216754d1cf39cd5981c8af2fb19`.
  Compiler version `aiken v1.1.23+5adf783` matches the CI revision pin
  `5adf7837cbddb5d329fd51d9c0cd73f561eaf95c`. Reused its previously recorded
  **183/183** history tests and clean-build evidence, rather than claiming a new
  compile. Applied scripts in this turn are derived from that same blueprint.
- Docs link check passed **283 Markdown/MDX files**. The first invocation found
  the shell's pnpm 11 shim instead of docs-site's declared pnpm 10.11.0; running
  the installed matching executable passed without changing package metadata or
  weakening the check. `git diff --check` passed.
- Live skill runbook currency check passed. Read-only prerequisite checks still
  find Docker daemon unavailable, Kupo 1442 refused and Ogmios 1337 refused.
  No services, durable environment, deployment or existing identities were
  changed. Source/runtime migration and a matching deployment identity are also
  still necessary before this history flow can pass live acceptance. Independent
  work remains available, so the goal is active rather than externally blocked.

Commands (from repository root; exact outputs under `artifacts/event-history/`):

```sh
# Node22 PATH; explicit installed pnpm9 executable for demo workspace
pnpm --dir demo/midgard-sdk test
pnpm --dir demo/midgard-sdk run typecheck
pnpm --dir demo/midgard-sdk run build
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-event-history-capture.test.ts --testTimeout 60000
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-event-history-output-claims.test.ts --testTimeout 60000
pnpm --dir demo/midgard-fault-proofs run typecheck # 35 errors; incomplete
# Touched-file eslint in each affected package, --max-warnings=0
# Installed pnpm10.11.0 executable, as declared by docs-site
pnpm --dir docs-site run check:links
node .agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.mjs
docker info --format '{{json .ServerVersion}}'
curl --max-time 5 --silent --show-error http://localhost:1442/health
curl --max-time 5 --silent --show-error http://localhost:1337/health
git diff --check
```

Next independent work: connect both production fabricated submitters, preparation
and staged reopening to authenticated list witnesses, complete payload/original
Value openings, reserved proof deadlines and atomic terminal queue marking;
propagate explicit measured history parameters through deployment manifests and
all contract/catalogue builders. Then migrate all transition-trace and user
lifecycle consumers, watcher/node/persistence/rollback/correction paths, installed
replay and maximum proof fit, and live acceptance. No unrelated proof gap is
closed by this slice. The user's design/research/remaining-gaps files remain
byte-for-byte unchanged from the previous checkpoint.

## Production retained-content submission (2026-09-22)

Previous goal turn classification: progress (capture timing/retry implementation,
adversarial applied scenarios and identity-bound evidence). This turn connects
both **production stage-three submitters** to the migrated history commitment.
It does not complete the production preparation, stage-two capture, stage-four
queue marking, or whole-family workflow migration.

- Replaced legacy `RetainedEventDatum` handling in both stage-three modules with
  complete `RetainedEventData { payload, original_assets }` openings. `openingCbor`
  is the new artifact/API field; datum-only files are explicitly refused. Absence
  accepts only no-content. Presence checks the kind, committed identity, payload
  hash and original Value hash against the authentic thread's captured commitment.
  Map wire forms are normalized by the existing Plutus commitment implementation.
- Classification now mirrors Aiken: the captured inclusion time outside
  `(header_start, header_end]` establishes `Ineligible*Event` even when content
  also differs. Eligible matching content is refused. Eligible body/content
  substitutions retain both comparison hashes. Withdrawal comparison still
  excludes the operator's validity verdict; the full opening itself must match
  the captured payload, including its admission fields.
- Both real submitters construct finite validity ranges after the accused
  interval and strictly before earliest merge. A shared validity helper keeps
  the 60-second lower-bound backoff, respects the protocol lower bound with slot
  slack and refuses an unusable deadline. This is transaction safety, not an
  additional inclusion guarantee: workflow capture planning must still reserve
  time for all remaining stages. Emulator clocks are explicitly injected.
- The applied churn suite now submits stage three through the **actual production
  submitters**, after the old history reference has been spent. The fixture CT
  asset names use the actual family category/header layout. Existing direct
  on-chain negative cases remain, with additional production preflight refusals
  for honest content and expired proof windows. Hub and initial CT issuance are
  still native fixtures and terminal output is a fixture destination; this does
  not establish catalogue/init/stage-one/stage-four/queue-removal acceptance.
- Updated only the older suites' affected stage-three fixtures: retained payload
  plus original Value, carried queue policy, and the exact current Aiken-measured
  header/state goldens already verified by the SDK wire twins. Preserved exact
  byte assertions and refusal assertions. The obsolete attempt to alter an
  opening's inclusion timestamp is replaced by original-Value tampering: the new
  opening has no claimant-selected inclusion-time field; time comes from the
  captured commitment. Additional focused tests cover both timing boundaries,
  content mismatch plus timing error, identity/kind/value tampering, pairing and
  definite/indefinite map encodings.

Checks:

- New production opening/validity tests: **15/15**.
- Applied capture/production stage-three submission: **8/8**.
- Existing fabricated deposit/withdrawal suites: **41/41**, after updating their
  affected stage-three fixtures. Their remaining legacy preparation/stage-two
  assertions are explicitly **not** evidence for the new list model. They must
  migrate with those implementations, including arbitrary nonexistent IDs and
  retired-ID reuse; a green legacy expectation does not close that requirement.
- Touched-file eslint and `git diff --check` passed. Docs links passed **283**
  Markdown/MDX files.
- Fault-proofs typecheck: **31 errors / 17 files**, still incomplete. Both
  stage-three production modules and the new applied/opening tests are clean.
  Remaining errors identify preparation, stages 01/02/04, runtime/parameter
  application and older emulator fixtures. The intermediate 43-error inventory
  and four failing legacy test results are retained alongside the final results.
- No SDK or Aiken implementation changed in this turn. All **1,182** Aiken
  source/config hashes still match the prior pinned clean testnet build and
  blueprint `3d04859765f10a154e7bfbc2a7daeb6063e2d216754d1cf39cd5981c8af2fb19`.
  Prior SDK/Aiken suite/build evidence applies to their unchanged source; it is
  not reported as a new run. No live state or deployment was changed.

Measured signed **production stage-three** transactions (unchanged L1 limits):

| Family/opening | Bytes | Memory | CPU steps | Fee (lovelace) |
| --- | ---: | ---: | ---: | ---: |
| Deposit absence | 796 | 178,346 | 64,580,555 | 263,756 |
| Deposit retained Order | 900 | 258,661 | 116,431,604 | 277,101 |
| Withdrawal absence | 796 | 181,214 | 65,472,802 | 264,856 |
| Withdrawal retained Order | 1,125 | 314,989 | 155,205,418 | 294,225 |

These ordinary-payload measurements include real signer/layout/fee construction
and local UPLC evaluation; maximum-payload and installed-family fit remain open.
Full signed bytes are archived in this checkpoint's applied evidence snapshot.

Commands (root; pinned Node22/pnpm9 for demo, pnpm10.11.0 for docs-site):

```sh
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/fabricated-history-opening.test.ts
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-event-history-capture.test.ts --testTimeout 60000
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/fabricated-deposit.test.ts tests/fabricated-withdrawal.test.ts
pnpm --dir demo/midgard-fault-proofs run typecheck # 31 errors; incomplete migration
# Touched-file prettier/eslint; eslint --max-warnings=0
pnpm --dir docs-site run check:links
git diff --check
```

Next work is the shared production witness/preparation/artifact boundary, not
another isolated validator: replace `LiveOutputReference`/exact-nonce evidence
with authenticated list witnesses, emit `openingCbor` containing complete payload
and Value, and migrate both workflow adapters. In particular, current adapters
still pass the obsolete `eventDatumCbor` field and re-admit current live events
before every stage. After capture, they must instead reopen against the authentic
thread's retained commitment, so a journal restart or pointer change cannot force
an already captured proof back onto a live history reference. Then connect
stage-two capture/retries and explicit deployment parameters, carried queue policy
at stage one and atomic terminal queue marking at stage four. All remaining
lifecycle, transition, watcher/node, recovery, maximum-fit, installed replay and
live gates remain part of the unchanged goal. No gap is closed by a passing test
whose implementation still encodes the obsolete evidence model.

## Preparation and durable opening migration — 2026-09-22

Both fabricated-family preparers now authenticate raw hub/list/retention outputs
through the shared public history witness verifier. Arbitrary absent IDs need no
nonce lookup. Present observations commit payload and original Value separately;
ineligible genuine events produce timing faults and eligible matching events are
refused. Explicit applied payload bounds are enforced before capture.

Both workflow evidence authorities now persist complete `openingCbor` and history
reference hints. Pre-capture readmission fetches current public history, tolerates
pointer-only changes, and refuses changed semantic facts pending re-preparation.
After capture, integrity-only journal admission does not query live history; the
production stage-three submitter checks the opening against the authenticated L1
thread commitment. This is not archive authority. Automatic regeneration after
filler promotion and workflow conflict recovery remain gates. SDK family chains
carry their applied retention address and payload bounds for the consumers.

Verification (Node 22.22.2 / pnpm 9.15.4 for demo):

```sh
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-history-preparation.test.ts tests/submit-init-emulator-event-history-capture.test.ts tests/submit-init-emulator-event-history-output-claims.test.ts --testTimeout 60000
# 25/25 passed; final preparation-only rerun after budget guards: 6/6
pnpm --dir demo/midgard-sdk test # 69 files, 595 tests passed
pnpm --dir demo/midgard-sdk run typecheck # passed
pnpm --dir demo/midgard-sdk run build # passed
pnpm --dir demo/midgard-fault-proofs run typecheck # 59 errors / 17 files
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/fabricated-deposit.test.ts tests/fabricated-withdrawal.test.ts
# 23 failed / 18 passed: obsolete preparation/witness fixtures remain to migrate
pnpm --dir docs-site run check:links # pnpm 10.11.0; 283 files passed
# touched-file eslint --max-warnings=0 passed (empty verified log)
```

New applied scenarios cover both kinds: arbitrary absence without nonce reads,
authentication failures, original funds, timing errors, honest-event refusal,
pointer churn, journal reopening during public-history outage, equal-key filler
promotion invalidating pre-capture semantics, and bounds. These are fixture-hub
preparation/capture scenarios, not complete installed-family or DA acceptance.
Logs use `artifacts/event-history/history-preparation-*`; applied snapshots are
archived under `artifacts/event-history/preparation-checkpoint-20260922T142319Z/`.
No Aiken source or deployment changed in this slice. Remaining source type errors
include stages 01/02/04, explicit runtime bounds and older consumer fixtures.
Do not interpret the new green scenarios as resolving the red family suites or
full lifecycle, transition, watcher/node, rollback, fit and live gates.

### Updated deployment authorization — 2026-09-22

The user superseded the prior deployment restriction with: “Redeploy midgard
whenever necessary to test this.” Necessary redeploys are now authorized. Preserve
unrelated work and follow the coupled local/on-chain state rules; do not reuse
old protocol state with a fresh local database. Readiness work remains independent
of live infrastructure availability, and redeploy is deferred until the migrated
consumers and deployment identity can support meaningful acceptance.

## Production history capture submission — 2026-09-22

Replaced both production stage-two submitters' exact-unspent-nonce/legacy event
datum paths. They now discover the current authentic hub and sorted-list witness,
authenticate full-key gaps/fillers or Orders, check the carried and configured
state-queue policy, and reference the actual existing retained-data UTxO for
external Orders. The stage-three thread retains complete immutable commitments,
including original funds; the submit result exposes the full `openingCbor` and
actual history reference. An old event pointer is only an ignored discovery hint.
The old exported legacy-event authentication helpers are removed.

Workflow and CLI contract adapters carry the family's applied history environment.
Workflows bind the prepared opening to the fresh capture, refusing semantic
changes instead of forwarding an opening that the next stage cannot use.
Stage-one handoff derivation now carries the authenticated queue policy, and
production stages one/two set finite validity after header end and before merge.
The first-stage change has pure counted-root/handoff coverage in this checkpoint;
its complete installed transaction path remains an acceptance gate.

**Safety versus availability:** these production submitters perform one current
capture attempt with finite validity. They do not yet connect the SDK's declared
visibility/inclusion/remaining-proof budgets or authoritative conflict retry to
the durable workflow. The adversarial test's explicit 120s protection / 20s
visibility / 40s submission / 80s remaining assumptions are not production
defaults or live inclusion guarantees. Artifact regeneration after semantic
change, retry/reconciliation after contention, output visibility and restart/
rollback remain open integration work. No liveness gate is closed by these tests.

Checks (Node22.22.2 / pnpm9.15.4; docs pnpm10.11.0):

```sh
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-event-history-capture.test.ts tests/submit-init-emulator-history-preparation.test.ts tests/submit-init-emulator-event-history-output-claims.test.ts tests/fabricated-history-opening.test.ts --testTimeout 60000
# 4 files / 48 tests passed
pnpm --dir demo/midgard-fault-proofs run typecheck
# 66 errors / 13 files: incomplete runtime/terminal/fixture migration
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/fabricated-deposit.test.ts tests/fabricated-withdrawal.test.ts
# 25 failed / 16 passed: legacy witness fixtures and removed legacy helper assertions
# touched-file eslint --max-warnings=0: passed
pnpm --dir docs-site run check:links # 283 Markdown/MDX files passed
git diff --check # passed
```

The 48 passing checks include six real production capture paths: each family's
arbitrary-ID absence, inline Order and separately prepublished external Order.
Tests spend the old pointer first, verify fresh capture, refuse the wrong semantic
arm/opening/queue policy, refuse expired proof windows, and refuse unavailable
external data. External cases assert that the signed transaction references the
actual retention UTxO, then run the production retained-opening classification.
The external fixture uses 2,000-byte user data; this is not maximum-fit evidence.
The existing three-conflict-per-path applied mutation scenarios remain intact.
All evaluation uses local UPLC and unchanged protocol size/execution limits.

Measured complete signed production stage-two transactions, final regression:

| Family / witness | Bytes | Memory | CPU steps | Fee (lovelace) |
| --- | ---: | ---: | ---: | ---: |
| Deposit absence | 904 | 601,155 | 217,234,419 | 357,700 |
| Deposit inline | 1,055 | 967,316 | 391,090,605 | 398,006 |
| Deposit external | 1,092 | 1,036,300 | 593,434,617 | 418,248 |
| Withdrawal absence | 904 | 629,714 | 228,795,349 | 360,541 |
| Withdrawal inline | 1,055 | 1,307,188 | 562,475,239 | 430,334 |
| Withdrawal external | 1,092 | 1,381,746 | 766,754,256 | 451,036 |

Applied snapshots: `artifacts/event-history/production-capture-checkpoint-20260922T143528Z/`.
Logs: `artifacts/event-history/history-production-capture-*`. These scenarios use
real history and proof validators but a native fixture hub/initial CT issuer and
fixture terminal address. They do not claim full catalogue/CT/terminal acceptance.
All 1,182 Aiken source/config hashes still match the pinned clean testnet build
and blueprint `3d04859765f10a154e7bfbc2a7daeb6063e2d216754d1cf39cd5981c8af2fb19`.
Aiken and SDK implementation did not change in this capture slice; prior pinned
build and SDK595 evidence applies to unchanged sources, not a new test run.
User-owned design/research/remaining-gaps files retain their recorded hashes.

Next: migrate obsolete family fixtures without removing their authentication,
count/root, exact-byte, signature/body and rejection assertions; wire explicit
deployment bounds through runtime/inspection; implement terminal queue marking
and its deadline/lease/recovery path. Then run full installed-family lifecycle,
replay and fit, connect production retries, and continue all transition, watcher,
node, user lifecycle, persistence/rollback and live gates. Redeployment is now
authorized when necessary. No deployment or durable environment changed in this
checkpoint; the previously observed live infrastructure blocker was not retested
here. The full end-to-end goal remains incomplete.

## Legacy family fixture migration — 2026-09-22

The two existing fabricated-family unit suites now use authentic raw hub/list
outputs and complete retained openings. Removed their obsolete exact-unspent-nonce
and legacy marker authentication expectations. Authenticated gaps cover arbitrary
and historically consumed IDs, and mocks throw if nonce liveness is queried.
Governed spending-address tests still exercise both staked and unstaked addresses,
including refusal at a mint-policy-derived address. Missing history/hub NFTs,
wrong list address, foreign policy, mismatched identity and changed authenticated
content still refuse. Equal-key fillers authenticate absence and contribute no
captured original funds.

Preserved counted-root/cardinality, leaf selection, DA provenance, honest eligible
event refusal, withdrawal body/signature versus operator-validity distinctions,
prover ownership, artifact schema/digest and exact Aiken stage-four bytes. Whole
legacy datum hashes are replaced by history commitment/payload/original-Value
assertions. Timing tests now positively establish `Ineligible*Event` at both
window boundaries, as the selected architecture requires. Recomputed artifact
digests cannot authorize fabricated semantic facts: current public history is
checked again before capture. These synthetic unit fixtures do not claim to
establish an admission transaction or finalized frontier. Applied admission/
capture/retirement evidence remains separately scoped.

Commands (Node22.22.2 / pnpm9.15.4):

```sh
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/fabricated-deposit.test.ts tests/fabricated-withdrawal.test.ts tests/fabricated-history-opening.test.ts
# 3 files / 60 tests passed (43 family tests + 17 opening/handoff tests)
pnpm --dir demo/midgard-fault-proofs run typecheck
# 40 errors / 11 files; down from 66 / 13. Both migrated unit files are clean.
pnpm --dir demo/midgard-fault-proofs exec eslint tests/fabricated-deposit.test.ts tests/fabricated-withdrawal.test.ts --max-warnings=0
# passed
git diff --check # passed
```

Logs: `artifacts/event-history/history-family-fixtures-*`. The initial syntax
error, intermediate 31/41 pass and final green results remain recorded rather
than erased. No source validator, SDK implementation, production submitter or
applied parameters changed in this fixture-only checkpoint; prior applied
measurements remain evidence for their previously bound source, not rerun claims.

Remaining compiler gates: runtime/inspection explicit bounds (3 source errors),
terminal completed-fraud witness (2 source errors), full-family lifecycle
fixtures, catalogue replay configuration and shared contract fixtures. All
transition/node/watcher/lifecycle/retry/restart/rollback/installed-fit/live gates
remain. The previous 25 unit failures are resolved, but this does not establish
the complete family lifecycle or close broader event-history acceptance.

## Explicit history parameters and deployment identity — 2026-09-22

Both fabricated-family first-step manifest entries now carry required
`eventHistoryBounds` (canonical decimal strings for `inlineLimitBytes`,
`maxPayloadBytes`, `maxPayloadNodes`) and `eventHistoryRetentionAddress`.
The shared finalized-manifest parser and node's strict parser enforce the same
required fields. Whole-manifest identity includes this metadata. Focused family
resolution reapplies explicit bounds, checks all installed step hashes and
catalogue membership, and compares the retention address derived by that same
parameter application. Full catalogue inspection also checks both families'
retention addresses and requires their common SDK construction bounds to agree.
Missing bounds never silently select defaults. Syntax validation alone is not
proof of measured safety or correct blueprint application.

The node manifest writer records metadata from the constructed families, and
manifest restoration reconstructs their bigint bounds/address. Fresh real
construction requires explicit environment settings:

```sh
MIDGARD_EVENT_HISTORY_INLINE_LIMIT_BYTES
MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_BYTES
MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_NODES
```

These are required positive integer strings; inline cannot exceed total payload
bytes. No production defaults were introduced. Isolated always-succeeds fixtures
carry explicit 512/5000/512 test metadata. Real blueprint reapplication tests use
those same previously measured candidate values, not an assertion that every
required subsequent operation already fits. This is an undeployed schema
replacement and remains provisional; it does not freeze the ABI or authorize an
unmeasured production parameter set.

Tests reject omitted parameters, valid-looking substituted parameters that change
applied hashes, and a substituted retention address. Manifest tests bind both
metadata fields to identity, including refusal after recomputing an identity for
missing required fields. Writer/restoration tests assert both family round trips.
The full registry golden is preserved with canonical bigint normalization; its
new digest is `e5d05d35e13019de51cf0e8af3ce6c60d440afc82f086d1dff57b3ab78122283`.
The separate queue/correction golden remains unchanged. This digest change
includes current proof recipes and explicit history metadata; the diagnostic
failure and old expected value are retained in the logs.

Commands (Node22.22.2 / pnpm9.15.4):

```sh
pnpm --dir demo/midgard-core run build
# passed; dist source digest 67930691374e1a49cff4277749d4f68d9a9155f659ef2b0d3a222421b92e8ebc
pnpm --dir demo/midgard-core run typecheck
# passed
pnpm --dir demo/midgard-core test
# 52 files / 579 tests passed
pnpm --dir demo/midgard-core exec vitest run tests/deployment-manifest-identity.test.ts
# 24 passed, rerun after adding retention-address identity/omission assertions
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/history-deployment-bounds.test.ts tests/deployment-manifest-binding.test.ts
# 2 files / 17 passed (8 applied family resolution + 9 existing manifest binding)
pnpm --dir demo/midgard-node exec vitest run tests/midgard-contracts.test.ts tests/deployment-manifest.test.ts tests/contract-deployment-info.test.ts tests/harmonic-uplc-contract-eval.test.ts
# 4 files / 49 passed, 1 pre-existing skipped hub-oracle evaluation case
pnpm --dir demo/midgard-node exec vitest run tests/midgard-contracts.test.ts tests/deployment-manifest.test.ts tests/contract-deployment-info.test.ts
# 3 files / 44 passed after final metadata writer simplification
pnpm --dir demo/midgard-fault-proofs run typecheck
# INCOMPLETE: 37 errors / 9 files, down from 40 / 11
pnpm --dir demo/midgard-node run typecheck
# INCOMPLETE: 10 errors / 4 imported fault-proof files
```

The remaining source errors are the two terminal submitters' missing completed
fraud witness; remaining fixture errors concern full-family lifecycles, catalogue
replay, and shared emulator construction. No checks were disabled or relaxed.
The existing skipped hub-oracle test was not introduced or edited here and is not
counted as passing. No new L1 transaction-size/fee/execution measurements were
produced by these metadata checks. Prior applied measurements remain bound to
their earlier source, compiler and fixture parameter identities.

Logs: `artifacts/event-history/history-bounds-*`; failed intermediate typechecks,
import-format check, and golden diagnostic remain available. Node list validator
construction and user lifecycle ingestion are still legacy and must be migrated
before deployment readiness. No environment was reset or redeployed in this
checkpoint. Next work is terminal queue marking with finite deadline, mutation
lease and recovery; then complete family/installed replay/fit, production retries,
all transition consumers, node/watcher/user lifecycles, restart/rollback and live
acceptance. The end-to-end goal remains incomplete.

Final checkpoint verification: ESLint passed for all 15 metadata source/fixture
files; `git diff --check` passed; root command `pnpm --dir docs-site run check:links`
(pnpm10.11.0) passed across 283 Markdown/MDX files. All 1,182 Aiken source/config
hashes still match the pinned clean testnet build, and the three user-owned
proposal/gap documents retain their recorded hashes. Evidence identity is
`artifacts/event-history/history-bounds-evidence-identity.json`. Aiken and SDK
implementation did not change in this checkpoint, so their prior build/applied
results are referenced rather than claimed as fresh runs.

## Terminal submitter queue coupling — 2026-09-22

Previous goal turn classified as progress: explicit deployment metadata changed
production loading and produced parameter/identity evidence. This turn implements
both production step-04 submitters' missing completed-fraud witness. They check
the carried queue policy against deployment, authenticate a fresh public hub and
current queue NFT/full address/inline datum/header hash, and use the governed
published queue spending script. Newly completed fraud consumes that node and
preserves its header, DA state, link, address and complete Value while setting
`proven_fraud` in the same transaction as CT burning and proof minting. An already
marked node for the same header is instead referenced; its original marker is
preserved even when another category produces the later proof. Both branches use
finite validity after the challenged interval and before maturity. No mint,
spending or merge rule was relaxed; no on-chain source changed in this slice.

CLI resolution now fetches the CT mint, proof mint and queue spend reference
scripts for step 04 and forwards them to submission. The two manifest-bound
families declare queue spend as an additional required witness. The general
shared mint/withdraw witness interface remains unchanged, so unrelated families
are not forced to acquire this new witness. The family application registry's
exact-role assertions were migrated to distinguish the terminal queue witness
from the complete removal set; all existing role coverage assertions remain.

Both workflow ports acquire a queue mutation lease before terminal capture,
renew it before the durable signed-body boundary, and return it with the captured
transaction. The shared adapter requires/journals/resumes that exact lease for
these two terminal stages, using its existing transaction recovery path. New
unit cases cover both family names, missing lease refusal, durable identity and
restart resume without releasing an unresolved lease. This is adapter-level
recovery evidence, not complete node restart or L1 rollback acceptance.

The applied queue harness exercises the actual production queue helper with the
real queue spending validator, newly recorded and previously recorded branches,
wrong identity/policy/reference-script and expired deadline refusal, and a race:
a competing marker spends the old node, the stale transaction refuses, and a
fresh lookup returns the existing marker. Existing nine mutation-negative cases
and the competing-spend test remain. Fixture hub/queue/proof issuance uses native
policies: these tests do not prove CT or terminal validator coupling. In
particular the previously-recorded helper-only transaction executes no Plutus
script; its measurement must not be represented as terminal proof fit.

The first new helper run exposed a fixture before-genesis validity conversion:
its header was constructed at emulator startup, before the production 60-second
backoff had a representable slot. The fixture now advances 100 slots before
constructing the header. Production backoff and validator rules were unchanged;
initial failure and detailed provider diagnostics remain recorded.

Commands (Node22.22.2 / pnpm9.15.4):

```sh
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-completed-fraud-marker.test.ts tests/linear-family-adapter.test.ts tests/fabricated-deposit.test.ts tests/fabricated-withdrawal.test.ts tests/family-application-registry.test.ts
# 400 passed / 3 registry fixture failures; applied queue15, adapter14, families43 pass
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/family-application-registry.test.ts
# 331 passed after explicit terminal-reference fixture migration
pnpm --dir demo/midgard-fault-proofs run typecheck
# INCOMPLETE: 35 errors / 7 legacy fixture files; production source errors resolved
```

ESLint passed for the 12 changed source/test files, and `git diff --check` passed.
Logs: `artifacts/event-history/history-terminal-*`. Applied helper evidence:
`artifacts/event-history/terminal-queue-checkpoint/completed-fraud-marker.json`
(45 records, including fixture setup and refusals). Measurements are complete
signed transactions using unchanged emulator production limits:

| Queue-helper branch | Signed bytes | Memory | CPU steps | Fee lovelace |
| --- | ---: | ---: | ---: | ---: |
| Newly recorded, real queue spend/native proof issuer | 1,285 | 358,498 | 163,838,386 | 310,584 |
| Previously recorded, reference/native proof issuer only | 449 | 0 | 0 | 179,757 |

These are scoped queue-helper measurements, not combined four-stage proof or
installed terminal-fit measurements. Next: execute both production terminal
submitters with actual CT/proof/terminal validators, migrate the full family
fixtures and shared contract construction, then run installed replay/fit and
merge races. Complete durable retry/rollback validation and all remaining
transition, node, watcher, user lifecycle, deployment and live acceptance gates
remain. No deployment or durable environment changed, and no live infrastructure
probe was repeated. The goal remains active and incomplete.

Checkpoint evidence identity:
`artifacts/event-history/history-terminal-evidence-identity.json`. Verified all
1,182 Aiken source/config hashes against the pinned clean testnet build and the
three preserved proposal/gap document hashes. Root `pnpm --dir docs-site run
check:links` (pnpm10.11.0) passed across 283 files. No fresh Aiken build/test is
claimed for this off-chain-only checkpoint.

## 2026-09-22 — complete fabricated-family emulator migration (in progress)

Both fabricated families now execute actual history admission followed by the
four production step submitters, permanent proof mint, atomic queue fraud marker,
and fraudulent-block removal. Each runs inline and separately prepublished
external payloads. The eight focused cases also retain the pure retained-DA
handoff assertions and honest eligible-content refusal, including a substituted
membership rejected on chain. Initialization of the computation thread still
uses the focused emulator adapter; catalogue governance uses the existing
fixture policy. This is not installed-workflow replay, production initialization,
or live acceptance evidence.

The new family history fixture deploys and registers both actual list scripts,
initializes their roots, reserves actual nonce inputs, and admits against the
existing hub before committing the challenged header. External data is published
and confirmed before admission; admission references the actual retained UTxO.
The old IDs remain unchanged. Deposit original Value excludes structural ADA.
The withdrawal lifecycle fixture now includes 50 ADA in its target body so it can
fund the actual admission/payout rules; the original token-only Aiken golden
remains in the pure handoff test with its existing assertions. No validator or
production limit changed in this checkpoint.

Shared setup accepts an optional admission hook before header commitment. Its
initialization/appointment lower validity bounds use the emulator's current time
with the production-style 60-second backoff, allowing a future header after real
admission. Header commitment reads already-published state-queue mint and active
operator spend scripts. This resolves an observed 16,399-byte complete setup
transaction against the unchanged 16,384-byte limit. These shared fixture edits
are undergoing the full package suite; failures remain open.

Shared builder fixtures now pass explicit history bounds, and removal deployment
fixtures carry each applied family's bounds and retention address. This removes
the remaining package type errors without implicit production defaults.

Commands (Node22.22.2 / pnpm9.15.4 unless specified):

```sh
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-fabricated-deposit.test.ts tests/submit-init-emulator-fabricated-withdrawal.test.ts
# final rerun: 2 files / 8 tests passed
pnpm --dir demo/midgard-fault-proofs run typecheck
# passed
pnpm --dir demo/midgard-fault-proofs run build
# passed
pnpm --dir demo/midgard-fault-proofs test
# RUNNING; shared-family and history size failures detected, not accepted
pnpm --dir docs-site run check:links
# pnpm10.11.0, repo root: passed, 283 files
```

ESLint for the 12 changed fixture files and `git diff --check` passed. Logs are
`artifacts/event-history/history-full-family-*`,
`history-full-families-final.log`, and `history-fault-proofs-full-suite.log`.
Final focused evidence lives in `full-family-final-checkpoint/full-deposit-history.json`
and `full-withdrawal-history.json` under that artifact directory: 35 records per
family, complete signed CBOR, fees, execution measurements, and applied recipe,
policy, catalogue, and protocol-parameter identities. Full-suite failures are
being diagnosed separately; no assertion or limit has been weakened.

Both terminal transactions are 1,620 signed bytes. Final-run deposit terminal
memory/CPU is 824,976 / 360,268,404 inline and 829,400 / 361,614,550 external;
withdrawal is 829,400 / 361,614,550 inline and 825,838 / 360,496,061 external.
External content-opening transactions are 2,985 bytes (deposit) and 2,844 bytes
(withdrawal). These are actual four-stage-family measurements, not maximum-shape
or installed-catalogue acceptance. The 512/5,000/512 candidate bounds remain
unfrozen. Remaining gates include shared-suite failures, all transition/staged
consumers, full user lifecycle orchestration/retries, node/watcher projection,
restart/rollback/correction, installed replay/fit, and live deployment acceptance.
No deployment or durable environment was reset in this checkpoint.

Shared-suite diagnosis follow-up: a separately reproduced signer lifecycle failed
because successor header construction carried 16,642 bytes against the unchanged
16,384-byte limit. The shared successor fixture now references its already
published queue-mint and active-operator scripts, matching the initial-header
fix. The same forced signer lifecycle now passes. A 12-file affected-family
rerun is in progress; coverage assertions are unchanged. Full-suite history size
failures report the existing 5-second test timeout; they require a clean rerun
without concurrent heavy suites before any conclusion about behavior. Neither
protocol budgets nor test deadlines have been raised.

All 23 production-builder identity mismatches were traced to the changed
`StateQueueNode` decoding in the shared queue authenticator used by proof
validators. Their fixed full-byte/identity/adapter digests were migrated to the
pinned testnet blueprint, with compiler/blueprint provenance recorded next to the
baselines. No equality, duplicate-title, or under-application assertion was
removed. `production-builder-parity.test.ts` and
`family-application-registry.test.ts` now pass all 398 tests. Diagnostic old/new
hashes are retained in `history-builder-parity-failures.json`; the original
failure log and migrated result log are both retained. Full-family fee/byte/CPU
measurements are also summarized in `history-full-family-measurements.csv`.

The affected shared rerun completed with 94 passes and 2 size failures across 12
files. Both remaining failures came from the value-preservation fixture's own
successor builder, which also embedded the queue/active-operator scripts. That
builder now uses its published references. A follow-up covering all four
value-preservation suites and the removal-lease suite passed all 10 tests across
5 files. The first 12-file log remains a failed diagnostic run; it is not relabeled
as a pass. Latest package typecheck and changed shared-fixture ESLint passed.
The original full package run is still completing; a final updated-source run
remains required. `history-full-family-evidence-identity.json` records the focused
milestone and explicitly lists that pending gate. No new Aiken build is claimed:
all 1,182 source/config hashes match the pinned build and all three preserved
proposal/gap documents retain their original hashes.

Timeout clarification: inspection of the earlier recorded commands confirms the
same dense-data cases already required `--testTimeout 60000` (see the earlier
budget and claims checkpoints). Prior successful durations were approximately
6 seconds for the 1,024-node withdrawal and 12–13 seconds for over-budget payload
rejection/reclamation. The default 5-second failure is therefore not explained
solely by current load. The final run uses that already-documented 60-second
runner allowance; protocol execution/size limits and all assertions remain
unchanged. The failed default-timeout attempts remain evidence and will not be
reported as passing. Node package typecheck now also passes after the imported
fault-proof fixtures were migrated.

The final list/retention rerun completed: 45/45 tests passed using the previously
documented `--testTimeout 60000`, in 87.01 seconds. Evidence is retained under
`artifacts/event-history/history-list-final-checkpoint/`; log:
`history-list-final-checkpoint.log`. This reruns actual list initialization,
admission, filler promotion, continuation, settlement/refund unlink and external
reclamation (with the fixture-authority limitations recorded in that evidence).
The temporary diagnostic reporter was removed from the package. Both package
typechecks remain clean; protocol size, execution and funding assertions were
unchanged throughout this diagnosis.

Next implementation boundary remains transition traces. The outstanding authority
reads are in `transition-trace/proof.ak` (deposit original Value and omission/window
references), `deposit-source.ak` / `deposit-yields.ak` (staged reopening pinned to a
live out-ref), and the SDK/TypeScript phase schemas and submitters. The public
`transition-trace/l1-events.ts` collector still decodes legacy deposit/withdrawal
datums, including root/filler/pointer classification concerns. Migrate these as
one coherent chain with authenticated immutable event commitments and original
Value, while preserving forced-order branches. No transition-trace or watcher
readiness gate is closed by this fabricated-family checkpoint.

The original default full-package run finished: 377 files passed, 20 failed,
4 pre-existing skipped files; 4,503 tests passed, 93 failed, 4 pre-existing skipped
(23m41.58s). The failures comprise the diagnosed inline-script successor builders,
23 obsolete builder byte identities, three dense-data wall-time limits, and one
additional suite-level fit-ledger identity failure. The latter is the pinned
forced-window ledger still naming blueprint
`04790ae612c2478aa089a7f08089f11e43638cf007a2f562fa8b445f4ee25f6e`.
Regenerate that artifact through its existing measurement mode, then run its
normal assertions in the final suite. This does not close the transition event
migration: these are shared forced-order regression scenarios. Original full
log remains `history-fault-proofs-full-suite.log`; updated-source full acceptance
is still pending. Existing skips were neither added nor modified.

Forced-window fit ledger was regenerated from all six actual emulator scenarios
using its existing `MIDGARD_WRITE_FIT_LEDGER=1` mode; the following full run uses
normal assertion mode. Updated-source full package run is now active:

```sh
pnpm --dir demo/midgard-fault-proofs run test --testTimeout 60000
```

Log: `artifacts/event-history/history-fault-proofs-full-suite-final.log`.
Execution session: 8511 (do not restart merely because a heavy case is quiet).
Pre-run source identity: `history-full-suite-final-source.json`, 4,432 files,
including the current pinned blueprint/compiler identity. Compare these hashes
after completion; concurrent unrelated DA-package edits were observed and
preserved, so an aggregate dirty-source manifest does not imply ownership of
those changes. The initial shorthand `pnpm test --testTimeout` was rejected by
pnpm option parsing before tests ran; corrected to `pnpm run test --testTimeout`.
Its invocation error is retained separately. Builder export checks passed for
both import and require entrypoints. Full-suite acceptance is still pending,
and the goal remains active.

## 2026-09-22 — transition staged-opening work (isolated prototype)

Previous goal turn was progress: complete fabricated-family emulator paths,
shared fixture fixes, typechecks and recorded evidence. The final full package
rerun (session 8511) remains live and has reported no failures so far. Its pinned
blueprint is unchanged while transition work proceeds in a disposable source
copy, `artifacts/event-history/aiken-transition-history-work`, created without
`build/` or `plutus.json` as required by the Aiken build skill.

The prototype adds `order_facts.capture_open` and authenticated Value reopening.
It captures the exact payload and original assets alongside the immutable
commitment, and reconstructs only the Value whose complete encoding hashes to
that commitment. The staged deposit source no longer reopens a live out-ref:
its state contains the captured commitment and its continuation carries the
hash-authenticated opening. The projection yield authenticates the current list
and actual retained data, with explicit retention/bound parameters. Native-asset
counting no longer subtracts the NFT from an already stripped original Value.
Value and summary yields reopen the same retained facts. A separate original-
asset projection helper avoids double subtraction; the legacy direct proof
helpers remain unmigrated and must be replaced in the next part of this work.

Pinned Aiken guarded history checks in the isolated copy passed 190/190, then
193/193 after wiring the staged source. New cases cover original Value excluding
structural ADA/NFT, pointer/out-ref churn, substituted quantities/payload,
external capture, staged reopening without live references, missing opening,
and rejection of the retired out-ref receipt shape. An initial fixture
`Option<Int>` versus `Option<Data>` compiler error was corrected with an explicit
Data annotation. Logs: `history-transition-opening-check-third.log` and
`history-transition-staged-opening-check.log`. A follow-up corrects the negative
receipt fixture to encode the exact old receipt, and requires a fresh check.

These edits are not yet copied to the source tree or represented as deployment
acceptance. Parameter application, SDK schemas/builders, applied scenarios,
omission/window branches, public indexing and terminal deadline/queue marking
still need coherent migration. The current full-package run proves only the
preceding fabricated-family checkpoint. No ABI or readiness gate is frozen.

The updated full fault-proof package run completed successfully: 397 files /
4,596 tests passed, four pre-existing files/tests skipped, zero failures
(23m21.83s). Its exact command was `pnpm --dir demo/midgard-fault-proofs run test
--testTimeout 60000`. Final result and source comparison:
`history-full-suite-final-result.json`. Fault-proof, SDK, core and Aiken sources
and the current blueprint stayed fixed during the run. Four unrelated DA-package
files changed concurrently and were preserved; the transition workflow fit ledger
was regenerated by the tests. The four skips require `MIDGARD_FINALIZED_RUN_DIR`
and real finalized-manifest / retained-removal / projected-removal artifacts;
those live-artifact checks remain unverified, not waived.

The isolated staged prototype now passes 195 history and 104 transition-trace
regressions under the pinned compiler. Fresh clean build identity:
`history-transition-raw-build-source.json` (1,183 source/config files), blueprint
`0689f6ed9b6e64c013bcadc517f3058770559bac3974e6d5f4fcecd46a0fb3a8`.
The generated projection parameter sequence and the shared Args schema were
checked against source. Args carry `Option<Data>` and the deposit yield then
requires the exact typed opening and its commitment. This delays the decoder
instead of duplicating it in unrelated L2 dispatchers: accepted-transaction
validator shrank from 15,869 to 14,715 unapplied bytes; deposit dispatcher from
11,421 to 10,653. Deposit value yield is 15,194 and projection yield 14,220 bytes.
These are unapplied sizes, not publication or execution-fit acceptance. Size
measurements: `history-transition-raw-build-measurements.json`. Malformed deferred
openings and withdrawal-kind substitution are explicitly rejected by new tests.

Continue integration in the isolated workspace:
`artifacts/event-history/transition-history-workspace`. It contains the staged
Aiken source and matching fresh blueprint, a snapshot of demo sources, copied
package-local workspace dependency links and dist outputs, and a shared external
dependency store. Source snapshot: `history-transition-workspace-source.json`.
Do not mutate the prior build directories or treat their blueprints as matching
later edits. The saved `transition-staged-opening.patch` applies cleanly to the
shared tree, but is intentionally not applied until matching SDK parameter
application, schemas, submitters and fixtures work together. No active services
or deployments were changed. Next: finish the SDK and staged submission path,
then the direct omission/window and terminal deadline/queue-marker migration;
run applied inline/external, honest, churn, restart and maximum-shape scenarios
before integrating the coherent change into the shared workspace.

Temporary-workspace relocation: the repository link checker also walked the
copied docs beneath the artifact directory, exposing links whose companion
repository docs were intentionally not copied. The initial check failed and is
retained in `history-transition-prototype-docs-links.log`. The isolated workspace
has therefore moved outside the repository to
`/home/gumbo/midgard-hub/event-history-transition-workspace`; this is the active
integration location, superseding the path above. Its relative workspace module
links were verified after moving. No checker, assertion, or project doc link was
changed. Root `pnpm --dir docs-site run check:links` then passed across 286 files
(`history-transition-prototype-docs-links-final.log`). Prototype source/evidence
identity: `history-transition-staged-evidence-identity.json`.

### Transition SDK integration checkpoint (isolated workspace)

Continued in `/home/gumbo/midgard-hub/event-history-transition-workspace`.
The shared repository HEAD is now `409dfdaa6181941b32b1e833adfd630cc5580c67`
from concurrent work; this task has not committed or reset anything. The source
snapshot remains the baseline for integrating only this task's isolated delta.

The transition builder now requires explicit history bounds, derives both
retention addresses, and applies the deposit address plus three bounds to the
projection yield's six-parameter ABI. The shared Args schema adds the external
reference index and deferred raw opening; `EventHistoryOpening` carries the typed
payload and original Value. Manifest identity, parsing, writing and restoration
carry both addresses on the transition entry. Runtime resolution requires exact
reapplied hashes for every semantic yield: bounds affect the projection yield,
so checking only the route/catalogue hash would not bind these parameters.

Staged deposit preparation now has a serializable commitment/opening pair.
Reopening checks kind, policy, exact checkpoint commitment, original Value and
source event. Yield data derives asset counts/indexes and projected outputs from
original funds, without structural ADA or list NFT. Submission captures a fresh
public Order plus actual external UTxO at phase zero, then supplies retained
openings at phases 7/8/10 and drops the live references for later stages including
the terminal transaction. Capture uses a finite window after the accused header.
The recursive convenience submitter carries its opening across stages. Durable
workflow preparation/journal recovery is **not yet wired**; resumed deposit
stages without the persisted opening refuse. The old projection entrypoint is
still used by unmigrated ingestion/replay consumers and remains an explicit
migration gate, not an accepted compatibility layer.

Validation so far (all from the isolated workspace, pinned blueprint
`0689f6ed9b6e64c013bcadc517f3058770559bac3974e6d5f4fcecd46a0fb3a8`):

- `pnpm --dir demo/midgard-sdk run test tests/fault-proof.test.ts`: 30 passed,
  including exact applied projection bytes and retention-address derivation.
- `pnpm --dir demo/midgard-fault-proofs run test
  tests/history-deployment-bounds.test.ts tests/transition-history-opening.test.ts
  tests/submit-init.test.ts`: 36 passed. Covers substituted/omitted bounds and
  retention metadata, staged original Value, structural/NFT/quantity/content
  substitutions, wrong checkpoint time/policy, later ID reuse and legacy receipts.
  Initial run's two regex assertion failures were corrected to match singular
  and plural address error messages; all original assertions remain.
- `pnpm --dir demo/midgard-core run test
  tests/deployment-manifest-identity.test.ts`: 25 passed, including each transition
  retention address and bounds changing the complete manifest identity.
- SDK, fault-proof and node typechecks passed after updating the explicit
  always-succeeds scaffold metadata. Final formatting/lint and typecheck reruns
  are tracked in the evidence identity, not inferred from these earlier passes.
- Shared accepted-claim emulator regression: `pnpm --dir
  demo/midgard-fault-proofs run test
  tests/submit-init-emulator-transition-trace-final.test.ts -t claim
  --testTimeout 60000`: 2 passed (fraud removal and honest rejection), 17 cases
  excluded by the explicit filter. The first `-t 'kind=claim'` selected zero
  because Vitest quotes string values; that run is retained and is not evidence.
  Reran with `TRANSITION_TRACE_FIT_LEDGER_PATH` to retain measurements. Ledger
  `history-transition-shared-claim-fit.json` has 152 rows; maxima are 15,755
  signed bytes, 5,483,092 memory and 1,644,500,053 CPU. These preserve a shared L2
  consumer under the new Args ABI; they do not establish deposit/withdrawal fit.

Next gates: actual history admission in transition fixtures (replace legacy
`DepositDatum` and short event asset names), applied inline/external staged
projection with predecessor churn and restart, public raw snapshot and replay
migration, durable workflow opening persistence, direct omission/window and
withdrawal-body distinctions, terminal deadlines/queue marking, and the wider
node/lifecycle/rollback/installed/live acceptance matrix. Existing maximum datum
and 5,000-byte Value cases must be migrated without reducing assertions or
raising production limits. No new Aiken edit/build was made in this checkpoint;
the prior prototype's source/build evidence remains applicable only to its
recorded source. Nothing from this isolated transition slice is merged into the
shared implementation yet, and the ABI remains unfrozen.

Final checks for this isolated integration checkpoint: the full
`pnpm --dir demo/midgard-validation run test` suite passed 56 files / 462 tests
in 253.66 seconds. Core/SDK/validation/fault-proof builds passed; built ESM
exports for the opening schema, transition builder, original-funds projector
and final submitter were loaded successfully. Final core/SDK/fault-proof/node
typechecks and changed-file ESLint passed. Post-format focused reruns retained
30 SDK builder, 36 integration, and 25 manifest passing tests. Root
`pnpm --dir docs-site run check:links` passed 286 files; `git diff --check`
passed. The 22-file TypeScript delta has no overlap against the shared source
baseline, and `git apply --check
artifacts/event-history/history-transition-sdk-integration.patch` passed; the
patch remains unapplied. Compiler, 1,183 Aiken source/config files, 3,028
TypeScript/config files and the three user-owned design/research/gaps documents
were hash-verified by `write-transition-integration-identity.py`. Evidence:
`history-transition-sdk-integration-identity.json`. The deterministic applied
SDK identity includes exact scripts and parameters; the disposable shared-claim
fit ledger is explicitly not live or installed-family acceptance and does not
retain fees or wallet/deployment identities. Those remain required in the actual
history lifecycle scenarios. No test or production limit was weakened.


### Applied staged deposit reopening and terminal merge binding (isolated)

The transition workspace remains `/home/gumbo/midgard-hub/event-history-transition-workspace`;
its changes have not been integrated into the shared working tree. Root HEAD was
`409dfdaa6181941b32b1e833adfd630cc5580c67` when this checkpoint began. Concurrent
DA changes and the user-owned design/research/remaining-gaps documents are preserved.

Added a production-route staged deposit emulator matrix: inline and separately
prepublished external payloads, honest/fraudulent transitions, and ADA-only or
three native assets (including empty and leading-zero asset names). Admission
locks 25 ADA, of which 20 ADA is the authenticated original deposit and 5 ADA is
list structure funding. The tests authenticate phase-0 capture, JSON round-trip
the opening, spend the original Order through permissionless successor insertion,
and continue the proof after its exact outRef is gone. A changed original Value
is rejected by the deployed yield, even when client validation is bypassed at
redeemer encoding. Honest eligible events cannot mint proof tokens. This is
serialized preimage reopening, **not** yet durable process-restart acceptance.

The real route exposed the initial phase-6 preparation requirement: preimages can
be fetched for planning before phase 0; phase 0 always refreshes and authenticates
the actual Order and external data UTxO. Later phases use the checkpoint commitment
and opening without live event references. Fixed this without turning preliminary
planning into L1 authority. Restricted new test funding selection to ordinary
wallet inputs after it initially consumed published proof chunks.

Pre-terminal evidence: `history-transition-applied-identity.json`,
`history-transition-applied-source.json`, `transition-applied-final/`, and
`history-transition-applied-measurements.csv`. Five files / 36 tests passed;
548 unique transition transactions retain CBOR, fees and execution measurements.
Maxima were 15,755 signed bytes, 6,471,117 memory, and 2,527,782,188 CPU. FP build,
typecheck and changed-file lint passed. Failed fixture-import/funding attempts
remain in the earlier logs and are not counted as passes.

Terminal deposit finalization now requires `completed_fraud_witness` and a hub
parameter. It authenticates the governed queue node, matches the completed proof
to the header, and requires a finite interval strictly before maturity. The
submitter records the marker atomically with proof minting, or references an
already marked node. A started thread does not freeze merge. Shared accepted
proof dispatchers carry a deferred optional raw field and do not inherit deposit
marker semantics. Updated the SDK parameter application, redeemer schema and
exact applied-byte expectation together.

Used the Aiken build skill and pinned compiler
`v1.1.23+5adf783` / revision `5adf7837cbddb5d329fd51d9c0cd73f561eaf95c`.
The final disposable build copied the exact 1,183 source/config files without
`build/` or `plutus.json`; source identity is
`history-transition-terminal-final-build-source.json`. Incidental formatter
changes outside the three intended Aiken files were restored before this build.
Blueprint SHA-256:
`329b0f34b28949954101787f9049ec62668e9008bf40908d5d59bb19c853aec9`.
Verified five deposit-final parameters and eleven shared Args fields against
source. The unapplied deposit dispatcher is 12,826 bytes. This is not installed
maximum-size acceptance and does not freeze the candidate payload bounds.

Commands/results for this terminal checkpoint (Node 22.22.2, pnpm 9.15.4):

- `aiken build --env testnet` from the clean disposable contract tree: passed.
- `MIDGARD_AIKEN_ENV=testnet node scripts/guard-focused-selector.mjs
  midgard/event_history midgard/fraud_proofs/transition_trace`: 195 + 104 passed,
  zero failures; `history-transition-terminal-final-guard.log`.
- `pnpm --dir demo/midgard-sdk run build`: passed.
- `pnpm --dir demo/midgard-sdk run test tests/fault-proof.test.ts`: 30 passed.
- `pnpm --dir demo/midgard-fault-proofs run typecheck`: passed.
- `pnpm --dir demo/midgard-fault-proofs run test
  tests/submit-init-emulator-transition-history.test.ts
  tests/submit-init-emulator-fabricated-deposit.test.ts
  tests/submit-init-emulator-fabricated-withdrawal.test.ts
  tests/transition-history-opening.test.ts tests/history-deployment-bounds.test.ts
  --testTimeout 60000`: 36 passed in 42.75 seconds. Includes refusal of an omitted
  terminal witness, an unmarked queue after refusal, and the exact proof marker
  after success followed by fraudulent-block removal.
- `TRANSITION_TRACE_FIT_LEDGER_PATH=... pnpm --dir demo/midgard-fault-proofs run
  test tests/submit-init-emulator-transition-trace-final.test.ts -t claim
  --testTimeout 60000`: 2 passed; 17 cases explicitly excluded by the filter.
  This checks the shared accepted final under the new ABI, not the unmigrated
  deposit maximum fixtures.

Still open: durable transition workflow opening persistence and terminal queue mutation
leases/reference roles; direct omission/window and withdrawal-body proof migration;
public L1 indexing and replay; original-Value ingestion/selection; maximum-size
fixture migration and installed-family fit; automatic user lifecycles/contention;
restart/L1 rollback/L2 correction; live acceptance. No live environment was reset
or redeployed during this checkpoint. The full goal and ABI readiness remain open.


Terminal checkpoint final verification: the eight applied cases also reject a
short finite validity interval crossing earliest merge maturity after bypassing
client clock planning. All eight passed (`history-transition-terminal-deadline-tests.log`),
followed by FP typecheck, build, and five-file ESLint passing. Root
`pnpm --dir docs-site run check:links` passed 287 Markdown/MDX files and
`git diff --check` passed. Exact evidence is bound in
`history-transition-terminal-identity.json` to the compiler, blueprint, 1,183
Aiken source/config files, 3,029 TypeScript/config files, exact emulator deployment
recipes/manifests and protocol parameters. The final staged-run ledger retains
548 unique transactions (CBOR and fees): maxima 15,769 signed bytes, 6,481,637
memory and 2,530,629,697 CPU. This adds atomic queue marking within unchanged
production transaction limits, but does not establish maximum event payload or
installed-family fit. The final 25-file SDK/test delta applies cleanly to the
shared baseline and has no concurrent-edit overlaps; it remains unapplied at
`history-transition-terminal-sdk-integration.patch`. All protected user docs
still match their recorded hashes. The next implementation slice is durable
transition workflow/public L1 replay migration, including marker transaction
leases; no claim of restart, rollback, live or complete end-to-end readiness.


### Durable terminal queue lease wiring (isolated continuation)

The preceding turn was progress: it changed terminal proof authority and produced
source-bound positive/negative applied evidence. Root HEAD remains
`409dfdaa6181941b32b1e833adfd630cc5580c67`. This continuation works in the same
isolated transition workspace and does not integrate its unfinished migration
into the shared tree.

The transition workflow now declares and authenticates `stateQueueSpend` as an
auxiliary reference script before Init. Its action refinement reopens the exact
observed deposit computation-thread outRef, verifies address, full token, owner,
kind and frozen proof commitment, and derives the queue-mutation lease requirement
only for phase 5. Capture rechecks that decision, acquires and renews the lease
before building/at the signed boundary, retains it with the captured transaction,
and fails it on capture error. Earlier deposit stages and shared accepted proof
stages do not acquire the queue lease. Cursor recovery now recognizes this
specific transition terminal action alongside its existing removal actions and
persists/resumes the same lease identity.

Tests cover nonterminal/terminal checkpoint selection; changed address, missing
or duplicated NFT, foreign token, missing datum/reference script, wrong owner,
proof commitment and kind; fresh-adapter pending renewal and confirmed release;
and rejection of missing journal/capture lease or altered action metadata. The
catalogue-roster assertion was updated only for transition's newly required queue
spending script. The initial roster failure remains in
`history-transition-lease-tests.log`; assertions for every other removal role and
family remain intact.

Commands/results (isolated workspace, Node 22.22.2 and pnpm 9.15.4):

- `pnpm --dir demo/midgard-fault-proofs run test
  tests/cursor-family-adapter.test.ts tests/transition-workflow-checkpoint.test.ts
  tests/family-application-registry.test.ts`: 3 files / 354 tests passed in 6.88s;
  `history-transition-lease-final-tests.log`.
- `pnpm --dir demo/midgard-fault-proofs run test
  tests/family-assembly-lifecycle.test.ts tests/linear-family-adapter.test.ts
  tests/cursor-family-state.test.ts`: 3 files / 31 tests passed in 4.88s;
  `history-transition-lease-shared-tests.log`. Includes shared rollback and durable
  lease behavior; this is not deposit lifecycle rollback acceptance.
- `pnpm --dir demo/midgard-fault-proofs run typecheck`: passed;
  `history-transition-lease-final-typecheck.log`.
- Changed-file ESLint: passed after import formatting; no checks or production
  limits changed.

No Aiken source, compiler or deployment parameter changed in this slice. The
previous terminal blueprint identity still applies. This wiring closes the
missing transition terminal lease implementation, **not** full durable workflow
acceptance: public L1 event capture/replay still decodes legacy deposit/withdrawal
datums, the workflow opening is not yet populated/persisted, and restart recovery
still requires a coherent migration away from exact event outRef identity.
Those are the next implementation gates, followed by direct omission/window and
withdrawal-body proofs, maximum-size/installed checks, remaining node/user
consumers, rollback/correction and live acceptance. The goal remains active.


Final lease-slice checks: FP build passed; root
`pnpm --dir docs-site run check:links` passed 287 files; `git diff --check` and
`git apply --check artifacts/event-history/history-transition-lease-integration.patch`
passed. No patch was applied. `history-transition-lease-identity.json` binds
1,183 unchanged Aiken files and 3,031 TypeScript/config files, the previous applied
blueprint/measurement evidence, current test/build logs, and protected user-doc
hashes. All 30 patch paths match the shared source baseline; no concurrent edits
would be overwritten by that delta. The lease test slice adds no new deployment
or maximum-size measurement claim. Public event capture/replay and durable opening
persistence remain the immediate next dependency for full workflow acceptance.


### Public authenticated Order capture and original-funds replay (isolated)

Previous turn classified as progress (terminal lease implementation and recovery
checks). Shared HEAD still `409dfdaa6181941b32b1e833adfd630cc5580c67`; work remains
in `/home/gumbo/midgard-hub/event-history-transition-workspace`.

Public raw L1 capture now requests explicit deposit/withdrawal retention-address
coverage from the reapplied transition deployment parameters. It authenticates
full-width history nodes, filters roots/fillers from events, resolves inline or
actual existing external data, and captures complete immutable commitments plus
original funds. Original Value and payloads are retained as CBOR strings rather
than mutable Map objects in the frozen admitted handle. The raw snapshot still
provides L1 authority; these strings alone do not. Relevant external output
references are included in the evidence projection.

Transition replay reads inclusion time and submitted bodies from those admitted
commitments/openings and projects only original deposit funds. It no longer
redecodes DepositDatum/WithdrawalOrderDatum or projects the full structural list
output. Existing forced-transaction raw decoding and caller-dependent withdrawal
validity distinctions remain. The workflow now populates `depositOpening` for
staged deposit proofs and persists it in the artifact. Artifact readmission
compares the immutable opening while allowing its diagnostic Order outRef to
change; direct proofs retain exact reference binding until their ABI migration.
On-chain reopening still checks its own checkpoint commitment. Recovery after
retirement/removal, and stable decision identity across every pointer churn path,
remain unverified and require the installed workflow migration.

Added raw-capture tests for both kinds, inline/external, full-key authentication,
root/filler filtering, missing retained outputs, and original funds (25 ADA in
an Order yields 20 ADA plus native assets; 5 ADA structure and a separate 9 ADA
filler never enter deposit projection). Added journal tests for serialized
openings, pointer continuation, substituted commitment/payload, later ID reuse,
missing/extended opening shape, unchanged direct-proof binding, and altered
frozen proof/raw evidence. These are raw-admission and artifact tests, not new
applied L1 admission or full process-restart acceptance. Synthetic raw fixture
parameters are labeled as such.

Checks completed (Node 22.22.2 / pnpm 9.15.4):

- `pnpm --dir demo/midgard-fault-proofs run test
  tests/transition-trace-l1-evidence.test.ts
  tests/transition-trace-event-authority.test.ts
  tests/transition-workflow-artifact.test.ts
  tests/catalogue-retained-classification.test.ts
  tests/typed-reason-retained-classification.test.ts
  tests/cursor-family-adapter.test.ts`: 6 files / 184 tests passed, 20.64s;
  `history-transition-public-final-tests.log`.
- `pnpm --dir demo/midgard-fault-proofs run typecheck`: passed;
  `history-transition-public-final-typecheck.log`.
- Changed 13-file ESLint passed. Initial raw fixture failures exposed canonical
  datum/output CBOR mismatch and an incorrect nested InlineDatum fixture shape;
  corrected the fixtures, preserving all admission checks and assertions.

Installed gate attempted, not passed:
`pnpm --dir demo/midgard-fault-proofs run test
 tests/transition-trace-installed-lifecycle.test.ts --testTimeout 120000`.
The first attempt found the newly required stateQueueSpend missing from the
fixture roster. Added the actual already-published queue UTxO and complete
resolved contract parameters. The latest attempt now reaches history decoding;
all five cases still create obsolete DepositDatum/short-name event outputs and
fail there. The suite's exact five-case completion assertion remains unchanged.
The maximum-deposit-datum case remains 12,000 bytes and has not been reduced or
skipped. Logs: `history-transition-public-installed-tests.log`,
`history-transition-public-installed-second.log`, and
`history-transition-public-tests-fourth.log`. No installed fit ledger is claimed
from these failing runs.

Next work: migrate `tests/support/transition-trace-retained.ts` and installed
fixtures to real list initialization/admission plus prepublished retention data,
including the 12,000-byte case and an explicitly measured payload bound (current
512/5000/512 candidate is still unfrozen and insufficient for that payload).
Then prove full journal restart, pointer churn, terminal lease/marker, removal,
retirement and rollback through the installed path. Other direct transition
proofs, watcher/node/user consumers and live acceptance remain open. No live
reset/redeployment, production limit change or Aiken source change occurred here.


Public-capture checkpoint final checks: FP build passed; root docs links passed
287 files; `git diff --check` and the full isolated patch application check
passed. `history-transition-public-identity.json` binds 1,183 unchanged Aiken
files, 3,033 TypeScript/config files, compiler/blueprint identity, passed checks
and failed installed runs. The 42-file transition delta has no shared-baseline
overlaps and remains unapplied at `history-transition-public-integration.patch`.
No new applied transaction fit claim is made by the raw-fixture tests; prior
applied measurements remain scoped to their recorded source. The installed
suite is explicitly incomplete and must retain its existing maximum-case and
five-case completion assertions during migration.


### Installed history workflow, 12,000-byte payload and pointer churn

The prior turn was progress: public capture/replay changed and the installed
suite exposed obsolete fixtures. Shared HEAD remains
`409dfdaa6181941b32b1e833adfd630cc5580c67`. This continuation stays in the isolated
transition workspace; no shared implementation integration or live redeployment.

Migrated the retained deposit fixture to explicit event content and original
Value, using the production original-funds projector. The installed suite now
initializes the real deposit/withdrawal lists, admits the genuine deposit, and
prepublishes larger payloads through the retention builder. It locks 95 ADA,
asserts exactly 90 ADA of original funds, and keeps structural ADA separate.
Both accepted-transaction cases retain an actual unrelated eligible-at-an-earlier-
window history Order; roots/fillers are not counted as events.

The existing five-case completion assertion and 12,000-byte datum are unchanged.
The emulator applies an explicit experimental history configuration of
512 inline bytes / 14,000 payload bytes / 512 data nodes consistently to list and
proof validators. This is a test parameter recipe, **not** a production limit
increase or frozen ABI. The entire 14,000-byte/512-node frontier and other maximum
Value/shape combinations still require measurement before accepting that bound.
The 12,000-byte payload itself now completes through the installed proof workflow.

Real admission waits into the header window, which exposed fixture sequencing:
scheduler appointment must occur before header start. The existing
`beforeHeaderCommit` hook now runs after appointment, before header commitment.
For the two-header fixture, deposit eligibility is selected one slot into the
second header window so admission/confirmation fit before the first header's
commit deadline. No scheduler, commit or admission validity check was relaxed.
Earlier failed ordering attempts remain in
`history-transition-installed-history-{first,second}.log`.

Initial migrated run: all five installed cases passed in 71.34s
(`history-transition-installed-history-third.log`). Extended the fraudulent
inline and maximum-data cases to spend the captured Order by permissionless
successor-filler insertion, assert the old outRef is gone, then recreate the
workflow constructor and reopen the on-disk journal at every subsequent boundary.
The same original commitment completes without a live original pointer. All five
extended cases passed in 74.41s (`history-transition-installed-history-churn.log`).
The captured evidence has 514 unique accepted transactions, exact CBOR/fees,
applied history recipes, transition scripts, manifest information, compiler
blueprint hash and protocol parameters in
`transition-installed-history-churn/installed-transition-history.json`.
Maxima: 15,769 signed bytes, 5,274,199 memory, 2,761,945,253 CPU. These are emulator
measurements under the existing production limits; governance/deployment binding
fixtures remain explicit and do not constitute live deployment acceptance.

The final run additionally models a separate resumable mutation-lease service
across workflow reconstruction, checks that terminal leases are released without
failure, and reruns direct staged transition and both fabricated-family scenarios
against the shared setup change. Final results and identity follow below.
Remaining gates include full frontier sizing; retired-ID/retired-Order workflow
recovery and L1 rollback/L2 correction; direct omission/window and withdrawal-body
proof migration; every remaining watcher/node/user lifecycle consumer; broader
required suites; and live acceptance. No global readiness or unrelated gap closure
is claimed from these installed cases.

### Installed history final source verification (2026-09-22)

Post-formatting verification passed all 21 tests in four files, including the five installed transition cases, pointer churn after checkpoint capture, resumable terminal leases, and both fabricated proof families. The installed fixture admits real history Orders, separately prepublishes the 12,000-byte payload, and retains original 90 ADA accounting separately from 95 ADA locked funds. Existing completion assertions and production Cardano limits remain unchanged. Experimental history bounds 512/14000/512 are **not frozen**; the complete maximum payload/node/Value frontier remains open.

Commands (Node 22.22.2, pnpm 9.15.4; isolated workspace unless stated):

- `MIDGARD_EVENT_HISTORY_EVIDENCE_DIR=/home/gumbo/midgard-hub/midgard/artifacts/event-history/transition-installed-history-verified pnpm --dir demo/midgard-fault-proofs run test tests/transition-trace-installed-lifecycle.test.ts tests/submit-init-emulator-transition-history.test.ts tests/submit-init-emulator-fabricated-deposit.test.ts tests/submit-init-emulator-fabricated-withdrawal.test.ts --testTimeout 120000`: 4 files, 21 tests passed, 76.95 s.
- `pnpm --dir demo/midgard-fault-proofs run typecheck`: passed.
- Targeted ESLint on the five changed installed-fixture/helper files: passed after import sorting; behavior tests above reran after that change.
- Root `pnpm --dir docs-site run check:links` (pnpm 10.11.0): 287 Markdown/MDX files passed.
- Root `git apply --check artifacts/event-history/history-transition-installed-history-integration.patch` and `git diff --check`: passed. Patch remains unapplied.

Evidence: `artifacts/event-history/history-transition-installed-history-identity.json`, verified test/typecheck logs, `history-transition-installed-history-verified-fit.json`, transaction measurements CSV and `transition-installed-history-verified/*.json`. Verified 3,033 TypeScript/source files and 1,183 Aiken build inputs unchanged against their recorded snapshots, pinned compiler binary, blueprint `329b0f34b28949954101787f9049ec62668e9008bf40908d5d59bb19c853aec9`, and all three protected design/research/gaps documents. No new Aiken compilation was necessary for this test-only checkpoint; prior clean pinned build identity is retained.

The installed evidence records 514 unique transaction CBORs with applied deployment/manifests and parameters. Observed independent maxima: 15,769 signed bytes, 5,278,267 execution memory, 2,764,621,994 execution steps. Per-transaction fees are retained in CSV; the 500,000,000-lovelace maximum includes setup transactions and is not a user lifecycle fee estimate. No live reset/redeployment occurred.

Remaining: direct timed/legacy transition proof migration, retired-ID/retired-Order recovery, complete size frontier, watcher/node ingestion and both automatic user lifecycles, rollback/L2 correction, settlement/refund/pruning, coherent root integration, required broad checks and live acceptance. The goal remains active and acceptance incomplete. Next: trace omission/out-of-window evidence and all shared consumers before changing their ABI.

#### Next direct-timing migration: traced consumers and decision constraints

Read-only trace confirms `transition-trace/l1-event-v1.ak` is the production caller of `validate_l1_event_fault_proof`; it still consumes the shared four-field `final-v1.Args` and supplies only reference inputs, so it cannot authenticate actual finite capture validity or terminal queue marking. `proof.ak` timed deposit/withdrawal helpers still decode legacy optimistic datums. Their witness indices are frozen into the routed proof by `workflow-proof.ts`; pointer churn between route and final therefore remains a live gate.

The coherent next change must update these together: Aiken timed witnesses/helpers and the dedicated final validator/redeemer; shared monolithic proof test callers; SDK witness schemas, l1-event parameter application and manifest/runtime identity; direct submission reference layout and finite validity/terminal queue lease handling; workflow proof persistence/retry; proof construction and emulator fixtures. Keep unrelated structural/accepted final schemas unchanged where a dedicated l1-event redeemer can express the additional evidence. Resolve current node and optional actual retained-data reference indices from the final transaction context rather than embedding a mutable pointer in the routed semantic proof. Authenticate history policy/address, full ID key, actual data, eligibility, and withdrawal body/signature separately from operator validity. Do not introduce archive authority or a hash-only external opening shortcut. Tests must retain forced-transaction timing behavior and all existing negative assertions while migrating deposit/withdrawal fixtures.

Specific Aiken callers traced: `proof.test.ak`, `structural-na-q47-event-window-variants.test.ak`, `proof-exunits-frontier.test.ak`; the monolithic deposit transition helper remains a separate legacy consumer. SDK definitions reside in `demo/midgard-sdk/src/fraud-proof/transition-trace.ts`; `submit.ts` currently uses the same generic final serializer at two call sites, so changing shared arity globally would affect other proof families. This is an implementation plan, not a new frozen ABI or a verified closure.

The cumulative isolated TypeScript integration patch contains 45 paths (38 existing and 7 new); all 45 were checked against the original root baseline and have no concurrent overlap. It remains unapplied pending coherent migration.

### Direct timing semantic migration (2026-09-22, in progress)

Previous goal turn classified as progress: final installed tests/source identity/transaction evidence completed and the direct timing consumers were traced. Continued in the isolated transition workspace; root dirty state was inspected and no unrelated source was changed.

Added `onchain/aiken/lib/midgard/fraud-proofs/transition-trace/timed-history.ak` and its focused test module. The semantic API accepts transaction-local history references separately from source-root proofs. It authenticates a current Order and existing optional retention UTxO through `order_facts.capture_open`, uses finite after-header/before-maturity capture bounds, and binds full identity and kind. Deposit timing requires exact deposit info; withdrawal timing authenticates body and signature while preserving the operator's verdict in the source leaf. Omissions require authenticated nonmembership in the correct source domain. This module is not yet wired into the production final validator; no ABI or full direct-route completion is claimed.

First compilation caught a generic test helper that needed explicit serialized value bytes. First executable run collected 23 tests, passed 22, and correctly rejected the supposed positive pointer fixture's invalid successor order. Corrected the positive fixture to a greater full-width key and added a distinct malformed-link rejection test; no production predicate or existing check was weakened. Final focused guard collected and passed 31 tests. Added checks include wrong policy/address/kind/ID, exact NFT quantity/full width, filler rejection, actual external datum/hash/address, missing/consumed/same-transaction publication rejection, byte bounds, eligible honest negatives, header boundaries, operator validity distinction, pointer-only continuation and later-ID-reuse timing. This is semantic fixture evidence, not admission/retirement lifecycle evidence.

A fresh pinned testnet build is now running from `artifacts/event-history/aiken-transition-timed-history-final-build`, created without `build/` or `plutus.json`. The 1,183 prior Aiken sources are unchanged; the final snapshot contains 1,185 source files. Next: finish build/source identity and regression guards, then migrate the dedicated final validator, SDK parameter/redeemer schema and transaction-context reference/lease handling together. Direct legacy proof helpers/fixtures remain an explicit gate until that integration is verified.

#### Timing predicate checkpoint results

- Pinned `/home/gumbo/.aiken-fork/bin/aiken build --env testnet` in the clean final disposable build: passed. Blueprint SHA-256 remains `329b0f34b28949954101787f9049ec62668e9008bf40908d5d59bb19c853aec9`; direct `l1-event-v1` remains four parameters because the new module is not wired yet. This confirms no deployed ABI change, not completion of migration.
- `MIDGARD_AIKEN_BIN=/home/gumbo/.aiken-fork/bin/aiken MIDGARD_AIKEN_ENV=testnet node scripts/guard-focused-selector.mjs midgard/event_history midgard/fraud_proofs/transition_trace`: 195/195 history tests and 135/135 transition tests passed. The latter includes the 31 new timing tests; totals must not double-count them.
- `aiken check --env testnet -m 'midgard/fraud_proofs/transition_trace/timed_history.{..}' --plain-numbers`: 31/31 passed, structured report retained and checked with the existing `evaluateSelectorReport`, including exact nonzero count. This extra focused run retained the execution measurements the summary-only guard does not preserve.
- Root `pnpm --dir docs-site run check:links` (Node 22.22.2 / pnpm 10.11.0): 289 Markdown/MDX files passed. The count increased with unrelated concurrent work.
- Targeted `aiken fmt` touched only the two new files. Root `git apply --check artifacts/event-history/history-transition-timed-history.patch` and `git diff --check`: passed. The two-file patch is unapplied and supplements the previous isolated transition changes; it is not a complete root integration patch.

Final source/compiler/blueprint/check identity and hashes: `artifacts/event-history/history-transition-timed-history-identity.json`. Raw structured predicate measurements and CSV are retained beside it. Maximum per-test execution: 3,914,088 memory and 1,740,938,304 steps; some tests invoke several predicates, so these are **not** deployed transaction budget or fee estimates. No production limits were changed. The 1,185 Aiken source hashes, pinned compiler binary and protected design/research/gaps documents were reverified. TypeScript code was unchanged in this checkpoint; prior package evidence remains scoped to its previous identity.

Next required implementation is unchanged: wire these predicates into the dedicated final validator, remove frozen timing witness pointers from the routed semantic proof, update SDK parameter/redeemer application and current-reference resolution, and require finite terminal queue marking with lease recovery. Update every old direct/monolithic caller and migrate both deposit/withdrawal emulator fixtures with positive/negative cases. Direct proof carriage for maximum withdrawal body sizes also needs measured fit; semantic payload bounds do not establish transaction fit. No direct-route, maximum-size, retired-ID lifecycle, rollback or live-acceptance gap is closed by this checkpoint. Goal active, no external blocker.

### Production timed-final wiring (2026-09-22, in progress)

Previous goal turn was progress (new timing predicates, 31 tests, clean build and 330 regression checks). Continued in the isolated workspace. The direct final now has a dedicated `final-l1-event` redeemer with transaction-local event reference indices and a required completed-fraud witness; deployment applies both retention addresses and all three payload bounds. Submission resolves current deposit/withdrawal Orders and actual retained data, and forced-event indices are also resolved from the actual transaction to account for queue references. The workflow plans/acquires a queue mutation lease at step_08. SDK explicit parameter-order expectations were updated without removing their equality assertion.

Publication gate remains under active measurement. First candidate compiled to 18,960 unapplied bytes: unpublishable. Separating the unchanged forced timing branches from legacy deposit/withdrawal decoding reduced it to 17,700 bytes: still unpublishable. No transaction/production limit was changed. A third clean candidate separates `order_facts.capture_payload` (full L1 metadata/content authentication) from the original-Value commitment retained by staged deposit proofs. The direct timing path does not need that Value commitment; staged capture still derives/hashes original funds. Shared authentication, external existing-UTxO checks and deadlines remain common. This must pass both fit and shared proof regressions before acceptance.

New applied emulator cases cover both history kinds, inline/external data, omission, outside-window claims, eligible-event negatives, pointer churn between route and final, wrong reference indices and terminal marking. These cases are written but have not yet run: compilation/parameter publication fit is the immediate prerequisite. Current semantic proof schemas still carry obsolete pointer fields; the new final does not use them. Removing those fields and migrating every old helper/fixture is an explicit remaining gate, not a compatibility contract or frozen ABI. Forced timing semantics remain unchanged; only actual reference layout and terminal deadline/marking are updated.

Current sources and diagnostic candidates: `history-transition-timed-route-{first,second,third}-source.json` and matching disposable build directories/logs. The first and second candidates are rejected fit evidence, not passing acceptance. Third clean build is running; next action is inspect its size and execute applied scenarios if publishable, otherwise partition the semantic script without weakening predicates.

#### Timed-final publication result and required partition

Third clean build passed with blueprint `3d924df6d4369cc568c78b886d8b86e7833bb7c5f714c9d3cbeaeecaf82f0605`; the direct final is still **16,982 unapplied bytes**, exceeding the unchanged 16,384-byte transaction limit before parameter/application/publication overhead. It cannot be deployed. The isolated blueprint now points to this current candidate; do not use it as successful deployed evidence. The three size results (18,960 → 17,700 → 16,982) are failed publication gates, not accepted limits.

The shared capture refactor passed 195 history and 135 transition checks on the third source snapshot. Fault-proof typecheck passed after current-reference and SDK schema changes. Targeted lint on eight changed TypeScript files passed. The SDK explicit parameter-order test passed (1 selected test; 29 filtered by `-t`, not a full SDK suite). No applied emulator scenario was run against the unpublishable candidate; the 12 new inline/external deposit/withdrawal cases remain unexecuted. No production limit, assertion or check implementation was weakened. The original-Value path is retained and covered by existing Aiken history/transition regression checks; deployed shared-family replay must rerun after the publishable partition.

Next required action: split the direct final using the existing authenticated zero-withdrawal yield pattern. Keep finalization/cancellation and completed-fraud queue marking in the spending dispatcher; move the timing predicate to a rewarding validator pinned to that dispatcher. The rewarding validator must bind exactly one dispatcher input and its exact Continue redeemer, authenticate its computation-thread asset/header envelope, then use the same hub/event reference indices. The dispatcher must authenticate the yield reference NFT under the deployment reference policy and require its exact zero withdrawal. This is one final transaction, not an added protocol wait or merge freeze. Both scripts must individually fit actual reference publication after parameter application.

Trace/update for that split: dedicated final-l1-event Args/Datum (raw proof data at dispatcher boundary), new rewarding validator; SDK family builder, titles, yield record and role-to-NFT mapping; fault-proof yield-reference/deployment metadata and final transaction withdrawal/reference layout; published/registered reward fixtures; node deployment manifest enumeration, contract reconstruction, deployment-info output, reference publication and explicit always-succeeds fixtures. Existing `final_yield.dispatch` demonstrates the singleton input/exact Spend redeemer binding, but its staged Datum schema cannot be reused for this direct inline proof. Preserve exact target/invocation/transition binding from `docs/agents/withdraw-zero-yielding.md`.

Current route still carries obsolete semantic witness pointer fields, deliberately ignored by the new transaction-local reference path; remove these fields and legacy helper/fixture consumers before any ABI freeze. New tests use deliberately stale legacy indices/names to expose accidental dependence during migration; that fixture representation must also disappear with the obsolete fields. Workflow artifacts retain raw snapshots but omit a mutable eventOutRef identity for timed deposit/withdrawal proofs; forced submission identity remains on its existing domain. Direct workflow restart/lease and public proof/carriage tests must verify this behavior, not just typecheck it.

Final candidate evidence: `artifacts/event-history/history-transition-timed-route-identity.json` binds the current 1,186 Aiken and 3,034 TypeScript/source files, pinned compiler, blueprint and all check logs. Post-lint fault-proof typecheck and the selected SDK parameter-order check passed. Root `pnpm --dir docs-site run check:links` passed for **292** Markdown/MDX files; root `git diff --check` passed. The protected design/research/gaps document hashes and root HEAD `409dfdaa6181941b32b1e833adfd630cc5580c67` were reverified. This candidate remains unpublishable and unapplied; the new forced timing entry has compilation evidence but no applied behavior evidence. No live environment was reset or redeployed. No external blocker: continue with the authenticated yield split.


### Publishable direct timing handoff (2026-09-22, in progress)

Continued implementation in the isolated transition workspace; root dirty state remains extensive and unrelated work is preserved. The previous direct timing candidate could not be published (16,982 bytes before the publication transaction). Split the final into a spending dispatcher plus authenticated zero-withdrawal timing validators. The dispatcher authenticates the reference-policy NFT role selected from the actual carried proof, requires the corresponding zero withdrawal, finalizes the computation thread, and requires the exact completed-fraud queue marker. Each rewarding validator binds the singleton dispatcher input and its exact Continue redeemer, header, computation-thread token and transaction-local event references. History and forced timing are separate targets because one combined target remained oversized. No production limits or protocol waits changed.

Rejected partition measurements are retained: first combined yield 23,141 bytes (full proof decoding), second combined yield 19,126 bytes (selective witness decoding). The third clean pinned testnet build produced blueprint `d8657aa788061e979ae0ab66c305cd803ded309de27e285c480b38aa73021109`: unapplied dispatcher 5,338 bytes/5 parameters; history yield 13,146 bytes/8 parameters; forced timing yield 13,163 bytes/3 parameters. All reference-script publications succeeded in actual applied-parameter emulator transactions. Source snapshot/build log: `history-transition-timing-yield-third-source.json` and `history-transition-timing-yield-third-build.log` under `artifacts/event-history`.

Updated isolated SDK parameter application/redeemers/reference roles, proof final construction and zero withdrawals, yield publication/registration fixtures, core manifest identities and node reconstruction/deployment enumeration/publication. Direct route preflight now compares canonical Plutus Data encodings using the existing core helper: equivalent definite/indefinite CBOR previously failed a raw-byte comparison. Semantic fields remain bound. Fixed the new withdrawal timing fixture to use the existing committed-withdrawal encoder (the production reconstruction already does), and funded external withdrawal admission sufficiently for existing future-operation checks; no validator assertions changed.

Checks so far (Node 22.22.2/pnpm 9.15.4, isolated workspace):

- `pnpm --dir demo/midgard-fault-proofs run test tests/submit-init-emulator-transition-timing-history.test.ts --testTimeout 120000`: 12/12 passed, 36.37 s. Both kinds, inline/external, omission/out-of-window positives and honest eligible negatives; actual admission, prepublished external data, predecessor churn, stale caller references, corrupted actual event index, terminal marker. Evidence `transition-timing-yields-third/transition-timing-history.json`; log `history-transition-timing-yields-third-tests.log`.
- `pnpm --dir demo/midgard-node run typecheck`: passed after completing the core token-name manifest map. Log `history-transition-timing-yields-node-typecheck-second.log`.
- SDK selected validator parameter-order test: 1 passed, 29 unselected, with exact applied CBOR assertions for dispatcher and both yields. Log `history-transition-timing-yields-sdk-parameters.log`.
- `MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs run test tests/submit-init-emulator-transition-trace-subvariants.test.ts -t 'authenticates a late rejected order' --testTimeout 180000`: 2 passed, 4 unselected, 12.20 s; preserves exact forced-submission/verdict authentication and removal. The isolated generated forced-window fit ledger was regenerated through its existing supported path; no check changed. Log `history-transition-timing-forced-first-tests.log`.

Added negative applied yield-reference substitution and 19 focused dispatcher/role-selection unit cases. New shared applied run covers timed history plus installed/staged transition and both fabricated families; checks are in progress. Current implementation is still unapplied to root. No live reset/redeployment occurred. Remaining gates include obsolete timed witness/schema/helper removal, maximum direct proof carriage, installed direct timing restart/rollback, full history/user/node lifecycle migration, coherent integration and full/live acceptance. The goal remains active; publication fit for these measured scenarios does not freeze the ABI or close maximum-size acceptance.


#### Handoff regression and size evidence

The expanded applied handoff suite passed 12/12 (41.78 s): each fraudulent case additionally rejects a wrong authenticated-yield reference, missing zero withdrawal and malformed terminal witness before successful exact marking. The shared run passed 33/33 across five files (77.09 s), including all five installed retained-history workflows, eight staged original-Value reopening cases and both four-stage fabricated families. Forced timing reran after fixture lint: 2 passed, 4 unselected (13.53 s). SDK family/reference tests: 38 passed. Core deployment manifest/reference-authority tests: 39 passed. All four affected package typechecks passed (core, SDK, fault proofs, node). Targeted ESLint passed; the first SDK lint command used a wrong family path and was rerun with `src/fraud-proof/contracts/families/transition-trace.ts`.

Pinned focused Aiken guard passed 195 history + 154 transition cases (349 total). The new 19 handoff cases cover singleton actual dispatcher input, exact Spend reference, duplicate/missing dispatch, cancellation, inline datum and all timed role tags/malformed shapes. An initial test-helper draft returned a tuple, then an unused binding failed to force evaluation in negative cases; corrected the tests to compare the actual result so the guarded failure executes. No production condition changed. The fresh disposable build `aiken-transition-timing-handoff-final-build` contains no inherited cache/blueprint and reproduces `d8657aa788061e979ae0ab66c305cd803ded309de27e285c480b38aa73021109`. Its complete input snapshot has 1,224 files (including build scripts/guidance); this is broader than the earlier 1,187 contract/config-only snapshot. The 3,034-file TypeScript snapshot is `history-transition-timing-handoff-typescript-source.json`.

Recorded applied runs contain 2,598 unique complete signed transaction CBORs. Independent observed maxima: 15,769 bytes, 6,493,779 execution memory, 2,761,053,006 execution steps. Full fees/transaction sizes/budgets are in `history-transition-timing-handoff-transactions.csv`; `history-transition-timing-handoff-fit.json` names the source record directories. The maximum 500,000,000-lovelace fee is a setup transaction and is not a user lifecycle fee estimate. Root docs links passed for 296 Markdown/MDX files, root diff whitespace check passed, protected design/research/gaps hashes reverified. The cumulative 52-path TypeScript overlap audit finds no concurrent root changes; no patch applied.

The node `test` script contains an internal `--`, so the requested scoped arguments launched the broader node suite. That run is still active and its result is not yet acceptance. It has identified an obsolete canonical manifest fixture missing transition history bounds/retention addresses (fixture updated, assertions preserved), an expected full contract-registry golden change from the two new applied yields (awaiting exact result and independent subset verification), and an unrelated benchmark module collection failure (diagnosis pending). Retain all failures; run the intended subset directly after the broad suite releases its isolated test database shards.

Next size gate is now measured with additional 12,000-byte out-of-window deposit/withdrawal candidates at explicitly applied experimental 14,000-byte retention bounds. Initial withdrawal admission correctly rejected insufficient payout minimum coin for the enlarged L1 datum; the fixture now funds that actual payout adequately. These candidates are not passing acceptance. The next run will expose direct route proof duplication: current routing carries the same large source proof in both its redeemer and output datum. A remedy must authenticate existing proof transport and retain semantic proof binding; it must not relax event-data prepublication/authentication or production limits. The previously passing 12-case matrix remains separate from the two new maximum candidates.


#### Node regression disposition and large timing route migration

The inadvertently broad node run finished: 168 files passed, 4 failed, 2 existing skipped; 1,641 tests passed, 9 failed, 3 skipped and 1 todo (436.52 s). Failures were (a) seven canonical manifest assertions blocked by missing transition history metadata in their fixture; (b) one full-registry golden identity predating the new yields; (c) one stale generated DA contract fixture; and (d) a benchmark collection error because this isolated copy lacked two root CI helper modules. Copied those unchanged helpers into the isolate with recorded SHA-256 (`history-transition-timing-isolate-ci-prerequisites.json`), updated the manifest fixture, updated the registry golden while preserving the independently pinned queue/correction subset and complete stand-in inventory assertions, and regenerated the DA fixture through `MIDGARD_WRITE_DA_DEPLOYMENT_FIXTURE=1`. No tests or checks were skipped, weakened or edited to avoid failures.

Direct scoped node command `NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run tests/deployment-manifest.test.ts tests/contract-deployment-info.test.ts tests/midgard-contracts.test.ts tests/reference-publication-node.test.ts tests/reference-publication-chain.test.ts tests/fraud-proof-catalogue.test.ts` passed 56 tests, 1 existing opt-in skipped, 13.84 s. Direct DA-fixture/benchmark retry passed 41 tests, 8.26 s. These results validate the handoff blueprint d865; the full broad suite was not repeated or relabeled green.

The adequately funded 12,000-byte candidates both failed at direct routing, as expected from duplicated proof carriage: deposit 26,476 bytes; withdrawal 26,853 bytes, against unchanged 16,384 maximum. Retained failures and full nested errors in `transition-timing-max-funded-before-carriage/transition-timing-history.json`. Updated only proof routing: existing prepublished proof chunks can now open an exact timed proof into the final-6 inline datum once, with computation-thread header-envelope validation and exact output Data equality. Small timing proofs retain inline routing; larger than one existing 4,096-byte proof chunk use referenced transport. Both direct submit entrypoints and the installed workflow prerequisite planner use the same size decision. User event data still requires authenticated actual prior retention publication; proof chunks do not grant event authority.

Fresh clean pinned testnet build `aiken-transition-timing-carriage-first-build` produced blueprint `20b7fb3dd1b540cd0234b02a9259c035cc87303c69a28a7054d8d5874e33c5db` with a 12,700-byte unapplied route validator (two parameters); timing final and yield code unchanged. The 14-case matrix now includes both 12,000-byte candidates, missing-chunk-reference rejection, current event/yield/terminal marker tampering and absent zero withdrawals. Applied runs and typecheck are in progress. Source snapshot `history-transition-timing-carriage-first-source.json`. This is not full maximum-shape coverage or installed direct timing restart acceptance; workflow evidence and other readiness gates remain open.


#### Final timing-carriage checkpoint (2026-09-22)

The first new route run passed the original 12 scenarios, then correctly rejected the two large cases because the fixture applied 14,000-byte bounds only to its lists while leaving the proof validators at 5,000. Unified the explicit per-case bounds across the harness proof contracts and real history recipes; existing byte checks stayed unchanged. The final 14-case matrix passed, 46.62 s. Large cases also reject missing proof chunks before routing and all four final handoff mutations before successful terminal marking. No live or maximum-frontier closure is claimed.

Measured 12,000-byte deposit: route **13,711 bytes**, fee **1,091,210 lovelace**, **1,687,680 memory / 556,554,358 CPU**; final **1,825 bytes**, fee **947,499**, **2,400,826 / 2,031,123,731**. Withdrawal: route **13,897 bytes**, fee **1,132,548**, **2,119,642 / 670,691,934**; final **1,825 bytes**, fee **986,907**, **2,835,118 / 2,230,145,887**. These are route/final transaction costs; proof-chunk and event-data publication costs are separately retained in the full CSV and must not be omitted from total lifecycle estimates.

Final commands/results on blueprint `20b7fb3dd1b540cd0234b02a9259c035cc87303c69a28a7054d8d5874e33c5db`:

- `MIDGARD_AIKEN_BIN=/home/gumbo/.aiken-fork/bin/aiken MIDGARD_AIKEN_ENV=testnet node scripts/guard-focused-selector.mjs midgard/event_history midgard/fraud_proofs/transition_trace fraud_proofs/transition_trace`: **195 + 154 + 244 = 593 collected/passed**.
- `pnpm --dir demo/midgard-fault-proofs run test tests/submit-init-emulator-transition-timing-history.test.ts --testTimeout 120000`: **14 passed**, 46.62 s.
- `pnpm --dir demo/midgard-fault-proofs run test tests/transition-trace-installed-lifecycle.test.ts tests/submit-init-emulator-transition-history.test.ts tests/submit-init-emulator-fabricated-deposit.test.ts tests/submit-init-emulator-fabricated-withdrawal.test.ts --testTimeout 120000`: **21 passed**, 76.78 s.
- Forced timing selected replay with `MIDGARD_WRITE_FIT_LEDGER=1`: **2 passed, 4 unselected**, 13.19 s; generated isolated ledger updated for this blueprint.
- `pnpm --dir demo/midgard-sdk run test tests/fault-proof.test.ts tests/reference-scripts.test.ts`: **38 passed**.
- `NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run tests/deployment-manifest.test.ts tests/contract-deployment-info.test.ts tests/midgard-contracts.test.ts tests/reference-publication-chain.test.ts tests/fraud-proof-catalogue.test.ts tests/da-deployment-fixture-generation.test.ts tests/benchmark-regression.test.mjs`: **97 passed**, 17.05 s. The full-registry golden and DA fixture were updated for the changed router; independent queue/correction and stand-in inventory assertions remain intact. Live publication opt-in acceptance remains unrun.
- Final fault-proof typecheck and targeted ESLint: passed. Earlier four-package typechecks passed; core/SDK/node source did not change after those checks beyond node fixture identities.
- Root `pnpm --dir docs-site run check:links`: **297 Markdown/MDX files passed**. Root patch apply/whitespace checks passed; protected design/research/gaps hashes and root HEAD reverified.

Evidence identity: `artifacts/event-history/history-transition-timing-carriage-identity.json`, with 1,224 Aiken build inputs, 3,036 TypeScript/config/CI files, pinned compiler binary/revision, blueprint, applied deployment/manifests and hashed logs/records. `history-transition-timing-carriage-transactions.csv` contains **2,014 unique complete signed transactions** across final timing and shared applied runs; independent maxima **15,769 bytes / 6,488,343 memory / 2,768,190,982 CPU**. `history-transition-timing-carriage-fit.json` retains per-case route/final details. Setup fees remain explicitly distinguished from lifecycle estimates.

The cumulative 55-path TypeScript/generated-fixture integration patch passes `git apply --check` with zero root overlap and remains **unapplied**; it does not include Aiken changes. Current isolated source is coherent for this checkpoint, not a finished root integration. No external blocker, no live reset/redeployment. Goal remains active and acceptance incomplete.

Next gates: remove obsolete timed witness pointer/asset/withdrawal-override fields and old helper/caller paths together (including raw selective decoder field counts), preserve forced behavior; verify installed direct-timing chunk prerequisites/lease/restart/rollback/public L1 recovery; then complete retired-ID authority, safe full byte/node/Value frontiers, both user/node lifecycles and settlement/refund/pruning, coherent root integration and required full/live acceptance. Do not freeze ABI or relabel the broad node suite green based on the scoped retries. Existing unrelated fault-proof gaps remain open.


### Semantic timing witness cleanup (2026-09-22, in progress)

Previous goal turn classified as progress: production timing handoff and large proof route implemented, clean compiler identity and applied evidence retained. Revalidated root dirty state (239 entries) and isolated blueprint `20b7fb3dd1b540cd0234b02a9259c035cc87303c69a28a7054d8d5874e33c5db`; no root integration or unrelated source mutation.

Trace confirms the remaining broad `validate_transition_fault_proof` and `validate_l1_event_fault_proof` timing callers are Aiken test/library consumers, while production final-6 uses transaction-local authenticated history. Removing semantic fields requires replacing their old optimistic-datum branches, not keeping a second authoritative timing model. Plan: explicit transaction/history-data/reference context for timed library validation; optional context only at the broad multi-family dispatcher, required whenever a timed fault is selected; unchanged narrow non-timing families and forced submitted-source semantics. Migrate all existing timing fixtures to authenticated Order references and finite post-header capture bounds, retaining boundary/nonmembership/tampering assertions. Withdrawal verdict override is obsolete; a negative formerly based on an arbitrary override must instead authenticate the actual submitted body/signature, with added positive coverage for independently committed operator verdicts. Remove fields from Aiken/SDK typed variants, selective decoder field counts, builders and all fixture/generated consumers together; do not freeze ABI until resulting applied/replay/size checks pass.


#### Semantic cleanup implementation and first regression disposition

Removed `event_ref_input_index` and `event_asset_name` from all four timed deposit/withdrawal variants, and removed withdrawal `validity_override`. SDK and Aiken variants now carry only the source membership/nonmembership proof. Updated selective final-6 decoders to require exactly that field, source builders and replay evidence, and removed the unused `finalReferences` workflow argument. Forced timing keeps its separate existing witness fields/verdict domain. The old generic valid-deposit transition helper remains a separate explicit migration gate; it was not silently relabeled as history-backed.

Replaced legacy optimistic-datum timing library branches with the production authenticated-history predicates. The broad proof dispatcher now requires explicit `Some(L1EventEvidence)` for a timed fault and checks its transaction references against the supplied reference list; non-timing callers explicitly supply None. Narrow timing callers supply the actual finite transaction, history payload parameters and current reference indices. Existing withdrawal/deposit timing fixtures now build authenticated Order outputs. A mismatched ID now fails at the strict authenticated capture gate, so its existing negative test expects script failure instead of a returned False. The old arbitrary-withdrawal-override negative now mutates the submitted body; the positive independently committed operator-verdict case remains. The 154-case transition library guard passed after this migration.

TypeScript library run initially passed 94/100: six obsolete structural ABI field-layout assertions still expected removed fields. Updated their expected layout to the one source-proof field, retaining full blueprint field/domain/ID comparisons; added four explicit rejection cases for obsolete pointer-bearing bytes. Migrated all three withdrawal subvariant lifecycle fixtures from fabricated NFTs/optimistic datums to real two-list initialization and admission. All six subvariants now pass, including omitted/out-of-window withdrawal proof plus removal, honest late withdrawal rejection, forced exact/reason-mismatch cases and the unrelated count-fault route. The first migrated run caught fixture slot quantization (one millisecond after an excluded upper bound rounds into the same slot) and an overly distant header start in scenarios without admission. Late admission now uses the next actual slot; only admission scenarios request extra preparation lead time. No production clock, boundary or validator condition changed.

Regenerated 86 ABI fixtures (85 consumed independently by Aiken) with the existing generator. Its first invocation wrote new JSON but emitted Aiken from the JSON entries read before regeneration, leaving exactly 12 old deposit/withdrawal fault/proof/route vectors. A second invocation synchronized the existing generated artifacts; `pnpm --dir demo/midgard-node run fixtures:transition-trace-abi:check` passed. The generator/check implementation was not modified; its two-pass regeneration quirk remains outside this semantic change. Pinned first build produced blueprint `3ddf56af8e03652ff1bdfcc496b9c5c4d4d1999e347c180e71a216a19911eb7f`; explicit blueprint inspection confirms each of the four history timing variants has one source-proof field, while forced variants remain unchanged. Fresh final build with synchronized generated Aiken fixtures is running; final suites remain in progress.

Test-count correction: earlier totals of 593 Aiken executions added `midgard/fraud_proofs/transition_trace` to `fraud_proofs/transition_trace`, whose wildcard selection already includes that library group. They were passing executions, not 593 distinct cases. Current guards select only nonoverlapping history and full transition groups (195 + 244 = 439 distinct cases). Do not reuse the overlapping total as unique coverage.

#### Verified semantic timing checkpoint (2026-09-22)

Final blueprint `3ddf56af8e03652ff1bdfcc496b9c5c4d4d1999e347c180e71a216a19911eb7f` passed the clean pinned testnet build and **439 distinct Aiken cases** (195 history + 244 full transition, including library and ABI). Combined fault-proof command `pnpm --dir demo/midgard-fault-proofs run test tests/submit-init-emulator-transition-timing-history.test.ts tests/submit-init-emulator-transition-trace-subvariants.test.ts tests/structural-na-event-window-variants.test.ts tests/transition-trace-challenger.test.ts tests/transition-trace-l1-evidence.test.ts tests/ledger-delta-dense-trace-totality.test.ts tests/replay-prerequisite.test.ts tests/transition-trace-installed-lifecycle.test.ts tests/submit-init-emulator-transition-history.test.ts tests/submit-init-emulator-fabricated-deposit.test.ts tests/submit-init-emulator-fabricated-withdrawal.test.ts --testTimeout 180000` passed **145/145**, 11 files, 115.45 s. This comprises 104 library cases, 14 applied timing scenarios, 6 applied subvariant scenarios and 21 shared applied/installed cases; installed direct timing is still unverified.

SDK family/reference tests passed **38/38**. Direct node command `NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run tests/deployment-manifest.test.ts tests/contract-deployment-info.test.ts tests/midgard-contracts.test.ts tests/reference-publication-chain.test.ts tests/fraud-proof-catalogue.test.ts tests/da-deployment-fixture-generation.test.ts tests/benchmark-regression.test.mjs tests/sdk-abi-fixtures.test.ts` passed **105/105**, 8 files, 16.47 s. Initial node run passed 17/18 with only the obsolete full-registry golden; updated it to `4f2abbd328153905c2e149084fdf309f2062971ad49f41db30e36030d6c48ecb` and regenerated DA fixtures through the existing opt-in writer. The independently pinned queue/correction identity and complete stand-in inventory assertions are unchanged. The earlier broad node suite remains recorded as failed, not relabeled green by scoped retries.

SDK/fault-proof builds, SDK/node/final fault-proof typechecks, targeted ESLint and ABI fixture check passed. Root `pnpm --dir docs-site run check:links` passed **299 Markdown/MDX files**; `git diff --check` passed. The cumulative 63-path TypeScript/generated-fixture patch has zero root overlap and passes `git apply --check`; it remains unapplied and excludes Aiken. Protected documents and root HEAD were reverified.

Identity `artifacts/event-history/history-transition-semantic-fields-identity.json` binds 1,225 Aiken inputs, 3,036 TypeScript/config files, pinned compiler, applied parameters/deployment records, source and evidence hashes. The final record directory retains **2,417 unique complete signed transactions**; CSV independent maxima **15,769 bytes / 6,490,241 memory / 2,765,514,241 CPU**. Setup transactions include 500 ADA fees; those are not user lifecycle estimates. Current 12,000-byte deposit route: 13,706 bytes, 1,085,421 lovelace, 1,637,970 memory/542,394,143 CPU; final: 1,825 bytes, 947,809 lovelace, 2,409,416/2,033,783,837. Withdrawal route: 13,889 bytes, 1,123,358 lovelace, 2,027,031/645,536,976; final: 1,825 bytes, 987,425 lovelace, 2,846,344/2,233,584,577. Publication costs remain in the complete CSV. No production limits changed.

Closed only the four timed deposit/withdrawal semantic-witness field migration and its targeted library/applied/ABI regressions. Legacy valid-deposit generic helper, forced witness cleanup decision, installed direct timing recovery, full maximum/inline frontiers, root integration, both automatic user/node lifecycles, settlement/refund/pruning and required full/live acceptance remain gates. No external blocker; no live reset/redeployment. Continue with installed direct timing and public recovery coverage.

### Installed direct timing workflow (2026-09-22, in progress)

Extended the isolated installed transition fixture from five to thirteen cases: deposit and withdrawal omission, out-of-window source inclusion, honest late omitted events, and 12,000-byte external out-of-window payloads. Retained all five original staged deposit/accepted cases and their assertions. The timing payload fixture constructs complete source/count/event-to-step/trace roots; withdrawal committed verdict remains distinct from its authenticated submitted body/signature. Each positive recreates the installed workflow and disk journal at every boundary, churns the actual Order pointer before final-6, requires terminal removal and proof token, and checks mutation lease closure. Large cases require actual routed transaction references for prepublished proof transport. Added retained outcome/intent/confirmation, restart, lease and route measurement evidence.

First run: 7 passed, 6 failed. Both large direct timing cases and all five original cases passed. Four small cases completed proof/removal but failed the existing all-leases-released/no-failure assertion; two honest-late cases failed predecessor header commit after admission advanced past its validity window. Second run: 9 passed, 4 failed after moving honest-late admission between predecessor and current commits. Added lease diagnostics identify the remaining issue as fixture timing, **not a leaked production lease**: the first capture was attempted before header-end eligibility and correctly called `fail("... no usable validity window before merge")`; a later capture completed and released its separate lease. Preserve these failed logs as evidence of the preflight gate. The final positive fixture advances to two seconds after header end before starting the installed workflow, without changing production validity bounds or relaxing any assertions. Final third run/typecheck/lint are in progress. Aiken source and blueprint remain unchanged from the verified semantic checkpoint.

This targets installed timing/chunk/prerequisite/restart/current-pointer coverage. It does not establish live deployment, L1 rollback/L2 correction, retirement/ID reuse or maximum-safe-bound acceptance.

#### Verified installed timing checkpoint (2026-09-22)

`MIDGARD_EVENT_HISTORY_EVIDENCE_DIR=.../transition-installed-timing-third MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs run test tests/transition-trace-installed-lifecycle.test.ts --testTimeout 180000` passed **13/13**, 123.20 s. Six direct timing positives completed through proof mint/removal after actual pointer replacement: small omission and out-of-window cases took four cold workflow/disk-journal resumptions each; both 12,000-byte external cases took seven and used published proof chunks. Both honest late-event cases returned `no_fault_detected`. The five original installed cases passed unchanged assertions. Every acquired terminal mutation lease had three renewals, one release and no failure. Final fault-proof typecheck and targeted ESLint passed. No production source change was needed for the two diagnosed fixture ordering/eligibility issues.

`artifacts/event-history/history-transition-installed-timing-identity.json` binds this checkpoint to the unchanged `3ddf56af8e03652ff1bdfcc496b9c5c4d4d1999e347c180e71a216a19911eb7f` blueprint/compiler/1,225 Aiken inputs, the new 3,036-file TypeScript snapshot, applied deployment/parameter records, journals and fit ledger. Only the installed lifecycle test and its retained payload fixture changed after the verified semantic checkpoint. The 63-path cumulative TypeScript/generated patch still has zero root overlap, passes `git apply --check`, remains unapplied, and excludes Aiken. Root whitespace and protected-document checks passed.

Retained **1,340 unique complete signed transactions** in the installed run. Independent maxima: **15,769 bytes / 5,285,071 memory / 2,762,837,500 CPU**. Large deposit route: **13,706 bytes**, fee **1,085,640 lovelace**, **1,640,738 memory / 543,210,563 CPU**; final **1,825 bytes**, **945,821**, **2,385,537 / 2,025,328,118**. Large withdrawal route: **13,889 bytes**, **1,123,577**, **2,029,799 / 646,353,396**; final **1,825 bytes**, **985,751**, **2,826,427 / 2,226,302,046**. Complete publication/admission/churn/setup costs remain in `history-transition-installed-timing-transactions.csv`; 500 ADA setup fees are not user lifecycle estimates. Production limits unchanged.

Closed the installed direct timing/chunk prerequisite/queue lease/cold journal/current-pointer scenarios exercised here. These use fixture manifest binding and recorder-backed authenticated raw L1 observations; they do not establish live public transport or rollback/correction acceptance. Remaining gates: legacy valid-deposit generic witness/helper migration and forced-pointer ABI decision; public L1 rollback/L2 correction and retired-ID reuse; full maximum byte/node/Value and safe inline frontiers; both automatic user/node lifecycles, settlement/refund/pruning; coherent root integration, complete required package checks and live acceptance. No external blocker, no live reset/redeployment, ABI not frozen, goal active.

### Original-Value deposit semantic migration (2026-09-22, in progress)

Previous turn was progress: verified installed direct timing/chunk/lease/restart/current-pointer cases. Revalidated shared root dirty state (239 entries) and isolated blueprint `3ddf56af8e03652ff1bdfcc496b9c5c4d4d1999e347c180e71a216a19911eb7f`; no root implementation integration or live mutation.

Removed obsolete `event_ref_input_index` and `event_asset_name` from `ValidDepositTransition` in Aiken and SDK, proof builders and fixture callers. Updated the staged projection yield's selective field decoder from six fields to four; it derives/authenticates the full key through the actual history Order rather than a duplicate semantic asset-name field. Replaced the generic legacy optimistic-datum/NFT reader with `order_facts.capture_open` using explicit finite transaction/history-data/current-reference context, then projects the original admitted Value directly. Removed the unused legacy token-subtraction projection helpers and `AuthenticatedDepositReference`. Non-deposit branches keep their existing behavior. Generic dispatch requires matching transaction reference inputs when the deposit arm is selected, as timing does already.

Migrated Aiken generic deposit and exunit-frontier fixtures to real authenticated Order datums with 2 ADA original assets, 3 ADA separately refundable structural funds, the full event-key NFT and existing multiasset payload. Existing honest/false-root/arbitrary-descriptor/network assertions are retained. Added an SDK ABI rejection for old pointer-bearing deposit witness bytes and explicit four-field blueprint assertion. TypeScript typecheck passed after builder input cleanup; affected library cases and fresh pinned build are running. The first diagnostic caught a fixture nonce accidentally removed from two forced-event tests; those declarations were restored. The second caught a now-redundant four-field destructuring spread; removed it without changing assertions. One accidentally directed rebuild of the previous disposable checkpoint is recorded separately and is not evidence for this change. No shared compiler cache or production limit changed.

Still required: current-schema ABI regeneration, passing clean final build/Aiken guards, deployed staged and shared-family replay/fit tests, installed lifecycles, current node/generated identities, typechecks/lint/links and source-bound evidence. This migration does not close forced pointer cleanup, rollback/retired-ID authority, user/node lifecycles or live acceptance.

#### Deposit semantic applied regression disposition

First schema candidate `45b17415f53431448d74bcf2764218e399a98c05ea966111ac3c664b36880bab` passed a clean build, all 439 distinct history/transition Aiken cases, 92 selected TypeScript library cases, 38 SDK family/reference tests and SDK/node/fault-proof typechecks. ABI artifacts were regenerated with the existing two-pass generator and its check passed. Targeted ESLint passed; root docs links passed 303 files.

The required applied six-file run then exposed remaining raw decoder consumers: **39 passed, 10 failed**, 193.71 s. All eight staged original-Value cases and the two fraudulent installed deposit cases failed at replay/final phases because `accepted-transaction-yields` (deposit branch) and `deposit-v1` still unpacked the old six-field witness. Updated all three remaining `one_step_witness(fault, 3)` decoders to the four-field layout. All four raw deposit decoder sites are now traced explicitly. A new clean shared-consumer build is running; the failed candidate is not accepted despite its passing Aiken/ABI guards. Forced/timed/fabricated and shared accepted scenarios passed in that run, but must bind to the next blueprint before final checkpoint claims.

Also identified the older `tests/support/transition-trace-final-cases.ts` fixture as a remaining consumer gate: it still constructs legacy optimistic deposit datums under a scaffold mint policy. Its deep-deposit case carries 1,295 assets while testnet genuine admission caps deposits at ten tokens. Preserve its size/projection/adversarial assertions, distinguish synthetic proof-consumer stress from actual admissible user events, migrate its obsolete schema and external publication, and add genuine admission-limit coverage. Do not raise the production token bound or cite that synthetic case as full user-lifecycle acceptance.

#### Verified original-Value semantic checkpoint (2026-09-22)

Corrected blueprint **`98bd07b4ca894bc0900a3200adb35111df0dc9b2af5fdd98229a5efbdd7c006b`** passed the clean pinned testnet build and `MIDGARD_AIKEN_BIN=/home/gumbo/.aiken-fork/bin/aiken MIDGARD_AIKEN_ENV=testnet node scripts/guard-focused-selector.mjs midgard/event_history fraud_proofs/transition_trace`: **195 + 244 = 439 distinct cases**. Every deposit raw witness decoder now uses the same four-field layout, including shared accepted/deposit replay and staged reopening/finalization. The generic deposit path authenticates an actual Order and original Value; no legacy token-subtraction helper remains in that path.

Commands/results (isolated workspace, Node 22.22.2/pnpm 9.15.4):

- `pnpm --dir demo/midgard-fault-proofs run test tests/transition-trace-challenger.test.ts tests/transition-trace-l1-evidence.test.ts tests/ledger-delta-dense-trace-totality.test.ts tests/replay-prerequisite.test.ts`: **92 passed**, 9.83 s.
- `MIDGARD_EVENT_HISTORY_EVIDENCE_DIR=.../transition-deposit-semantics-shared MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs run test tests/transition-trace-installed-lifecycle.test.ts tests/submit-init-emulator-transition-history.test.ts tests/submit-init-emulator-transition-timing-history.test.ts tests/submit-init-emulator-transition-trace-subvariants.test.ts tests/submit-init-emulator-fabricated-deposit.test.ts tests/submit-init-emulator-fabricated-withdrawal.test.ts --testTimeout 180000`: **49 passed**, 6 files, 122.83 s. Includes all 13 installed cases, eight staged original-Value cases, fourteen direct timing cases, six semantic subvariants and eight fabricated-family cases.
- `pnpm --dir demo/midgard-sdk run test tests/fault-proof.test.ts tests/reference-scripts.test.ts`: **38 passed** on the corrected blueprint.
- `NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run tests/deployment-manifest.test.ts tests/contract-deployment-info.test.ts tests/midgard-contracts.test.ts tests/reference-publication-chain.test.ts tests/fraud-proof-catalogue.test.ts tests/da-deployment-fixture-generation.test.ts tests/benchmark-regression.test.mjs tests/sdk-abi-fixtures.test.ts`: **106 passed**, 8 files, 15.72 s. Includes rejection of old pointer-bearing deposit bytes. Full-registry golden updated to `aea85b5e916fd92ad86d90c45ef00e84f8bd02508dd363ba925e5cdd6d0ec9fb`; independent queue/correction and full stand-in inventory assertions unchanged. Generated DA fixture uses the existing explicit writer.
- SDK and fault-proof builds; SDK, node and final fault-proof typechecks; targeted ESLint; generated ABI check: passed.
- Root `pnpm --dir docs-site run check:links`: **303 files passed**. Root whitespace and 65-path cumulative TypeScript/generated patch apply checks passed, zero root overlap; patch remains unapplied and excludes Aiken.

The full descriptor execution-cost ledger **did not pass**: `MIDGARD_AIKEN_BIN=... MIDGARD_AIKEN_ENV=testnet node scripts/verify-transition-trace-descriptor-exec-ledger-v1.mjs --update` failed closed at 31/32 selectors because its existing `derivation_declines_an_empty_inline_datum` selector is absent; unchanged descriptor source instead defines `derivation_accepts_an_empty_inline_datum`. The ledger was not rewritten and the verifier/check source was not edited. Supplementary readings through the existing `measureModule` guard ran **34 tests in the two affected transition modules**, recorded in `history-transition-deposit-semantics-affected-exunits.json`; they do not replace the failed full gate. Current no-datum deposit whole-test: 11,346,180 memory / 4,974,743,370 CPU; fixture-only twin: 5,301,868 / 2,336,326,760. These are whole-test readings with fixture overhead, not deployed transaction budgets or proof of a safe inline frontier. Do not claim the stale descriptor ledger or unrelated descriptor gap closed.

Evidence identity `artifacts/event-history/history-transition-deposit-semantics-identity.json` binds the pinned compiler, 1,225 Aiken inputs, 3,036 TypeScript/config inputs, current blueprint, applied recipes/deployment identities and checks. The six final evidence files retain **3,223 unique complete signed transactions**, with CSV independent maxima **15,769 bytes / 6,482,237 memory / 2,763,729,747 CPU**. Large installed deposit route: 13,706 bytes, fee 1,085,018 lovelace, 1,636,730 memory/542,169,524 CPU; final: 1,825 bytes, 946,511, 2,394,141/2,028,004,859. Withdrawal route: 13,889 bytes, 1,122,557, 2,020,825/643,753,913; final: 1,825 bytes, 982,976, 2,791,779/2,215,541,798. Full publication/admission/churn costs remain in the CSV; 500 ADA setup fees are not user lifecycle estimates. No production limits changed.

Closed only the original-Value deposit semantic witness/helper/shared-decoder migration and these targeted regressions. Next: migrate the older synthetic maximum-shape deposit consumer fixture without weakening its assertions or conflating it with admissible ten-token deposits; then complete rollback/retired-ID authority, safe full frontiers and both user/node lifecycles, coherent root integration and full/live acceptance. The descriptor ledger gate remains incomplete. No external blocker or live reset/redeployment; goal active, ABI not frozen.

### Maximum-shape consumer fixture migration (2026-09-22, in progress)

The interrupted inspection-only continuation was no progress; revalidated root dirty state and isolated blueprint `98bd07b4ca894bc0900a3200adb35111df0dc9b2af5fdd98229a5efbdd7c006b`, then implemented the fixture migration. Root implementation remains untouched. The existing synthetic finalization scaffold now uses full history keys, Order facts, separately accounted 5 ADA structural funds, persisted original-Value openings, and prior retention publication referenced by the Order transaction. Existing content-corruption mutation now targets the current commitment schema. Applied proof recipes explicitly use the same experimental 512/14000/512 payload bounds as the earlier large-payload fixture; no production limit changed.

Measured the obsolete deep fixture with Lucid `assetsToValue`: 1,295 native assets plus the complete history NFT require 5,032 Value bytes. 1,287 assets with the complete NFT require exactly 5,000 bytes. Updated only that fixture asset count and retained its exact 5,000-byte assertion, depth 64, 256-byte datum and proof/removal assertions. This is synthetic proof-consumer stress under the existing scaffold policy, not evidence of admissible token count. Genuine admission maximum still needs explicit deployed boundary coverage (the Aiken rule counts flattened original Value, including ADA).

Fault-proof typecheck passed. Existing 19-case finalization file is running with its fit ledger enabled; no passing claim yet. No running Aiken process, no live environment mutation. All broader goal gates remain open as recorded above.

#### Maximum fixture regression and reopening budget (in progress)

First existing finalization run: 12 passed, seven deposit fixtures failed (85.79 s). Six hit Lucid's empty read-reference refusal; the external case encountered unrelated datums because optimized scaffold validators shared one policy/address. Fixed only fixture construction: omit empty `readFrom`, and use the repository's existing distinct always-succeeds fixture policy for deposits. Second run: **20 passed, one failed** (three files, 217.85 s). All 19 standard cases and the unchanged 1,304-asset accepted case passed; the 1,287-asset/depth-64 deposit failed at checkpoint kind 1/phase 7 with memory over budget. No assertion or production limit relaxed. Complete signed CBOR, fees, deployment metadata and fit ledgers retained under `transition-max-fixture-second` and `history-transition-max-fixture-second-*`.

Added a separate genuine applied-policy admission test: **four passed**, 8.01 s. Nine native assets plus ADA are accepted; ten plus ADA are rejected by local UPLC, with the event nonce and filler state unchanged. Both inline and prepublished external modes exercised. This correctly distinguishes the original-Value ten-entry cap (including ADA) from synthetic maximum-Value proof stress. Helper now returns its already-applied recipes; no production parameter change.

The phase-7 bottleneck reconstructs/validates the entire captured Value at every bounded four-asset step. Changed the staged source helper to verify the exact captured payload/Value hashes and retain original Value Data; the consumer decodes only requested policy entries and lovelace after that authentication. Generic `order_facts.reopen` is unchanged for other consumers. Existing source-name/quantity/index and mutated opening tests remain. First clean candidate build found a test helper's missing explicit Value-to-Data annotation; fixed and compiling a fresh disposable tree. ABI/parameters unchanged, generated script identity changes, so prior applied evidence is not evidence for this candidate. Pending clean build, Aiken guards, full maximum rerun, shared/applied regressions and updated deployment identity checks.

#### Current reopening candidate verification (2026-09-22, maximum run still active)

Clean pinned testnet blueprint `a9d59ec09a5b0d59b29ccac493e57a0a02f35449d69bf54537affa2be4fd6c08` built successfully. Only projection, Value and summary deposit yields changed (withdraw/else entries); ABI check passed unchanged. Focused Aiken guard passed **195 history + 244 transition = 439 distinct tests**. SDK family/reference **38 passed**. Node generated identity/manifest/catalogue/reference/ABI suite **106 passed**, 17.75 s, after the expected stale full-registry fixture failed 18/19 and was regenerated/updated. Current full-registry golden `88aa8acf324dad922fb7b3060b2adefbcbe255cd1a54a29e22a3513f53807e4e`; independent queue/correction and inventory assertions preserved.

Extended genuine staged tests from eight to twelve: nine native assets plus ADA, including 32-byte asset names and maximum signed-64-bit quantity, in inline/external modes with honest/faulty roots. All twelve passed, including pointer replacement and immutable reopening. Companion timing/fabricated/shared run passed all 40 test bodies, but its subvariant afterAll correctly rejected a stale source-bound fit ledger. Reran the six subvariants with the existing `MIDGARD_WRITE_FIT_LEDGER=1` writer: **six passed**, 21.53 s. Original failed suite log remains; no verifier/assertion edited.

The main six-file 46-case command has completed 19 standard finalization, 13 installed, eight earlier staged, four genuine admission boundary and the unchanged 1,304-asset accepted stress cases successfully; the 1,287-asset/depth-64 deposit stress is still running (session 86387, verified live). Do not claim the full maximum gate passed yet. Fault-proof build, final fault-proof/node typechecks, targeted ESLint and ABI check passed. Root docs links passed **306 Markdown/MDX files**. Current 67-path cumulative TypeScript/generated patch has zero overlap and passes apply check; remains unapplied, excludes Aiken. Reverified 1,225 Aiken/3,037 TypeScript/config source hashes, pinned compiler hash, protected docs and root whitespace.

Next independent consumer migration must include the existing watcher fold at `demo/midgard-watcher/src/indexers/user-event-indexer.ts` (still decodes `DepositDatumSchema` around 1348) and node submit-deposit datum reader (still `SDK.DepositDatum` around 143). Neither is covered by current proof tests. Do not infer node/watcher rollout or rollback support from successful installed proof fixtures. Full descriptor ledger, public rollback/L2 correction/retired-ID reuse, finalized retirement/merge/funds flows, safe full inline/payload frontiers, automatic user/node lifecycles, root integration and live acceptance remain incomplete. No external blocker or live reset/redeployment.

#### Verified maximum-Value reopening checkpoint (2026-09-22)

The pending run completed successfully without interruption or timeout changes: **46/46 passed in six files**, 1,076.23 s. The 1,287-asset, exactly 5,000-byte Value/depth-64 deposit completed proof and block removal in 1,066.218 s; the unchanged 1,304-asset accepted case also passed. These are synthetic proof-consumer stress cases under explicit scaffold event authority, not admissible user token counts. Together with the final twelve genuine staged cases, four genuine admission boundary cases, installed workflows and shared timing/fabricated regressions, this checkpoint verifies **78 distinct applied/installed scenarios** (the earlier eight staged cases and six ledger-regeneration reruns are not counted twice).

Main command (isolated workspace): `MIDGARD_EVENT_HISTORY_EVIDENCE_DIR=.../transition-max-reopening TRANSITION_TRACE_FIT_LEDGER_PATH=.../history-transition-max-reopening-fit-ledger.json MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs run test tests/submit-init-emulator-transition-trace-final.test.ts tests/submit-init-emulator-transition-trace-final-deep-deposit.test.ts tests/submit-init-emulator-transition-trace-final-many-assets.test.ts tests/submit-init-emulator-history-deposit-asset-bound.test.ts tests/submit-init-emulator-transition-history.test.ts tests/transition-trace-installed-lifecycle.test.ts`. Companion command ran the twelve-case staged file plus `tests/submit-init-emulator-transition-timing-history.test.ts`, `tests/submit-init-emulator-transition-trace-subvariants.test.ts`, `tests/submit-init-emulator-fabricated-deposit.test.ts`, and `tests/submit-init-emulator-fabricated-withdrawal.test.ts`; the stale ledger failure and six-case regeneration rerun remain recorded above.

`artifacts/event-history/history-transition-max-reopening-identity.json` binds blueprint `a9d59ec09a5b0d59b29ccac493e57a0a02f35449d69bf54537affa2be4fd6c08`, compiler, 1,225 Aiken and 3,037 TypeScript/config inputs, applied deployment parameters, logs, fit ledgers and complete transaction evidence. The combined CSV retains **7,267 unique complete signed transactions**. Independent maxima are **15,461 bytes / 12,556,125 memory / 6,929,687,360 CPU**, within the unchanged limits; the memory maximum also remains below the existing 13.2M stress assertion. The deep deposit fixture used 737 transactions including setup/publication and 1,630,664,103 lovelace in total fees including artificial setup fees. This is not a production user fee estimate.

Genuine nine-native-assets-plus-ADA admission measured **1,125 bytes**, fee **549,253 lovelace**, **1,766,193 memory / 744,675,345 CPU** inline; external admission measured **1,101 bytes**, fee **552,639**, **1,787,847 / 788,956,210**, referencing the separately published actual data output. Publication fees are separately retained in the CSV. Original deposited ADA remains 20M while 5M structural ADA and the authentication NFT are excluded. Both over-limit cases fail local UPLC without consuming their nonce or changing the filler.

This closes the previously failing synthetic maximum-Value staged reopening regression and the genuine asset-count boundary scenarios exercised here. It does not freeze the ABI or establish all safe inline/payload/node bounds. All processes are terminal. Root integration remains unapplied; no external blocker or live environment reset/redeployment. The next end-to-end consumer work must switch actual node policy construction and initialization together with node builders/ingestion and the watcher fold: the node still calls `buildDepositValidators`/`buildWithdrawalValidators`, and the watcher still expects legacy mint redeemers/datum witnesses. Operator archives remain local evidence, not L1 authority. Remaining full/live/rollback/retirement and descriptor-ledger gates are unchanged.

### Node bootstrap and deployment consumer migration (2026-09-22, in progress)

Previous continuation completed and recorded maximum-Value reopening verification. Revalidated the 239-entry root dirty state and unchanged isolated Aiken blueprint. The next production path still constructs legacy deposit/withdrawal policies; atomic SDK initialization creates no history roots, deployment readiness does not require them, and the watcher assumes legacy mint redeemers. A datum-only reader patch would not complete this migration.

Started a shared SDK deployment bundle, `user-events/history-deployment.ts`, applying both selected history lists from the same explicit one-shot bootstrap nonce and recording each exact recipe plus list mint/spend/reward, retention and retirement scripts. The wrapper reuses the existing validated parameter applicator; there are no implicit protection or payload bounds. Exported it for the node registry. SDK typecheck is running; no node policy switch or deployment mutation has happened yet.

Next steps: require these deployment recipes in the contract bundle/manifest; switch real policy construction; append both roots to the atomic bootstrap with dynamic nonce indices; register list observers before zero-withdrawal initialization without spending the reserved nonce; publish/record retention and retirement reference scripts; require both authenticated roots in deployment readiness. Migrate node submission/ingestion and watcher semantics on top of that coherent deployment. Preserve forced-order behavior and all existing initialization and identity assertions; regenerate obsolete fixtures explicitly. This implementation work is still isolated and unverified, distinct from the completed maximum reopening checkpoint.

#### Paired SDK/node bootstrap implemented; verification in progress

The isolated transition workspace now derives both list scripts from the same explicit initialization nonce, hub identity, protection duration and payload bounds, and carries the exact pair as deployment data. The explicit always-succeeds scaffold carries `eventHistory: null` and is rejected by history-dependent initialization/publication paths. Real deployment derivation requires `MIDGARD_EVENT_HISTORY_PROTECTION_DURATION_MS` as well as the existing three explicit bounds; no production value was selected implicitly.

SDK initialization composes both authenticated Root outputs after the prior nine protocol outputs. It resolves final nonce/input and Root/output indexes through Lucid redeemer callbacks, normalizes validity times to ledger slots, and separately funds the widest future pointer/timestamp encoding. The node registers both list and retirement observers before initialization using plain funding excluding the configured nonce; reference-script publication also reserves that nonce, including when wallets coincide. Deployment status now requires both authenticated roots and treats partial history state as non-empty. Four retention/retirement script roles, reference-script tokens, and complete list recipes are included in both core and node manifest registries and strict parsers. The real node derives the history policies in place of its legacy deposit/withdrawal policies.

Tests and failures are retained, not suppressed. The first SDK check exposed a type-name collision; `EventHistoryContracts`/`EventHistoryContractPair` now distinguish the script bundle from the existing query descriptor. Three applied SDK bootstrap scenarios passed. The first full node run had 2/8 successes: the old mock lacked slot conversion, and the old emulator put all operator funds into the reserved nonce. Its setup now reserves a separate nonce before script derivation, preserves the previous nine-output funding assertion, and adds assertions for the two funded history roots. The second run passed 7/8: direct test submission left the newly introduced registration wallet cache stale. The reinitialization fixture now uses the production submission/reconciliation helper and preserves the nonce-consumed/reinit-rejected assertions. The third run passed all 17 tests across initialization and registry in 281.08 seconds (including 117.716 seconds for reinitialization rejection). Independent queue/correction and stand-in inventory assertions remain unchanged; only the full applied-registry golden changed to include the actual history bundle.

Current verification: SDK, core, node and fault-proof typechecks passed; core manifest tests 30/30 passed; node manifest/parser tests 35/35 passed (registry tested separately in the 17-case run); SDK reference publication, role, ABI and funding tests 17/17 passed. Supplementary node consumers passed 27 bodies while six obsolete always-succeeds-only reference fixtures failed; these fixtures now supply real history scripts, with one expected-role vector still being extended from two to four entries per list. No acceptance check has been weakened. Lint found only five import-order errors, being corrected. Additional missing-root readiness assertions are being added before the focused final rerun.

Captured actual full bootstrap: `artifacts/event-history/node-bootstrap-node/node-atomic-history-bootstrap.json`, applied blueprint `a9d59ec09a5b0d59b29ccac493e57a0a02f35449d69bf54537affa2be4fd6c08`. This captures 442 accepted emulator transactions including reference publication and registration. The atomic root transaction is 4,452 signed bytes, fee 1,631,812 lovelace, 2,545,586 memory, 841,811,369 CPU, 2 spending inputs, 10 reference inputs and 12 outputs including change. Maximum signed size among the captured transactions is 16,283 bytes; maximum memory/CPU is the atomic transaction's. Total captured fees are 293,465,349 lovelace. Measurements are in `artifacts/event-history/history-node-bootstrap-node-measurements.json`. Limits remain 16,384 bytes / 16.5M memory / 10B CPU. These are bootstrap measurements, not user-flow or live acceptance evidence.

Still incomplete: exact final source/evidence identity for this new migration, all remaining consumer fixtures and checks, node ingestion/selection, watcher history folding, automatic user admission/publication/contention, settlement/refund/retirement/reclaim orchestration, restart/L1 rollback/L2 correction and retired-ID reuse across those consumers, complete maximum-size/race acceptance, safe root-workspace integration and live acceptance. The earlier 78-case transition/proof checkpoint remains separately bound to its then-current source; it is not being relabeled as covering these new node changes. No live reset/redeployment has occurred in this bootstrap phase.

#### Bootstrap verification checkpoint (2026-09-22)

The additional readiness assertions passed in the applied full-bootstrap case: both list and retirement observers are registered, the reserved nonce survives publication/registration until initialization, and hiding either history root from the provider produces `complete=false`, `empty=false`, and the appropriate missing component. This was a focused rerun (1 selected, 7 not selected) after the full 8 initialization cases and 9 registry cases had passed. The initial focused attempt failed during shared database setup with a migration advisory-lock error because two separate node Vitest processes were launched concurrently; its log is retained, and the serial rerun passed in 123.54 seconds. This is resolved test orchestration, not an external blocker.

The reference-script command fixture now expects retention and retirement roles in addition to the existing mint/spend roles. All 16 reference-script tests passed. The three applied SDK bootstrap cases passed again, and the selected operator early-activation scenario passed with history observer registration funded by the separate reference publisher. Every known atomic-initialization caller in operator, deposit-flow, published-workflow and watcher fixtures now registers history observers first; the live node bootstrap also uses its dedicated reference publisher for these permissionless registrations, so an operator nonce containing all operator funds need not be spent early. Full operator/deposit/watcher suites are still pending consumer migration.

Completed scoped checks: 30 core manifest cases; 86 distinct node cases across initialization/registry, manifest/parser, reference/auth/recovery/ABI consumers and the selected operator prerequisite scenario; 17 SDK consumer cases; 3 applied SDK bootstrap cases. Duplicate focused reruns are not counted again. SDK/core/node/fault-proof typechecks passed; the final node typecheck and changed-file lint passed. Expanded watcher typecheck failed with 4 errors in 2 files, and node-tools failed with 19 errors in 14 files: these are outstanding transition-history replay/opening consumers, not skipped gates or environment failures. Their complete logs identify the next migration work. Root `pnpm --dir docs-site run check:links` passed for 306 Markdown/MDX files; root `git diff --check` passed.

The final evidence set contains 454 distinct complete signed transactions under `artifacts/event-history/node-bootstrap-final/`. Its full protocol bootstrap is 4,450 bytes, fee 1,622,204 lovelace, 2,427,698 memory and 804,111,610 CPU; its standalone paired-history bootstrap is 962 bytes, fee 635,188 lovelace, 692,642 memory and 278,210,308 CPU. The final set's maximum signed size is 16,283 bytes and total fees are 299,265,824 lovelace. The earlier independent run above remains useful evidence of a slightly larger 4,452-byte / 2,545,586-memory / 841,811,369-CPU bootstrap with different generated keys and input ordering; the difference is not an optimization claim. All retain the original ledger limits. Final measurements: `artifacts/event-history/history-node-bootstrap-final-measurements.json`.

Current source inventory: `artifacts/event-history/history-node-bootstrap-source.json` records 3,086 source/config/fixture files and the unchanged Aiken blueprint. It distinguishes 29 previously inventoried files changed since the preceding transition checkpoint from newly inventoried paths (including four new implementation/test files and previously uninventoried existing files). All 1,225 recorded Aiken source hashes still match the pinned build used by the earlier 439 Aiken checks; no contract source or blueprint rebuild occurred in this bootstrap-only phase. No root-workspace source integration, ABI freeze, live deployment or end-to-end completion is claimed.

### Shared automatic history submission (2026-09-22, in progress)

Added the SDK `submitEventHistory` flow in the isolated transition workspace. Both kinds automatically select inline versus separately prepublished external payloads using the exact deployment bounds, reserve the same event nonce throughout, wait for the exact confirmed publication output, and reuse it across admission contention. The driver durably saves the completed transaction body/hash and expected output before broadcasting. A transport error or unresolved confirmation retains a pending checkpoint; resume reconciles that exact transaction before building or broadcasting anything else. Even previously completed admission receipts are revalidated against current L1 status on resume; local checkpoints are not chain authority. Checkpoints bind policy, complete payload, reclaim credential, locked assets and structural funding/refund identity. Only explicit definitive input rejection enables submission retry. Protected predecessor waits and fresh construction retain the 60-second lower-bound backoff and explicit bounded timing/attempts. The low-level builder now distinguishes protection and predecessor disappearance during unbroadcast construction.

First 11 applied flow cases passed, 8.40 seconds. Extended final new-flow suite passed all 13 cases: both kinds inline/external, real concurrent filler-pointer changes between construction and submit for both kinds, uncertain publication/admission replies after actual ledger confirmation, delayed exact publication visibility across restart, changed-funds checkpoint refusal, failure to persist before broadcast, unresolved restart without funding/build/broadcast, and completed-receipt reconciliation. The asset-bound companion passed all four existing cases. SDK build (ESM/CJS/declarations), SDK and fault-proof typechecks passed; 15 supporting SDK cases passed; changed-file lint passed after export sorting; root docs links passed. The first typecheck caught use of ES2022 Error options unsupported by this package target; the error now stores its typed cause directly.

The supplementary 36-case applied-list suite initially produced 33 passes and three wall-clock timeouts because the command omitted its previously documented `--testTimeout 60000` option (see the earlier dense-data/list verification commands in this log). That failed log is retained as `history-user-flow-final-tests.log` (50 passes total across three files, three failures). The unchanged full list suite is now rerunning with its existing 60-second runner setting; no source check, assertion or ledger budget was changed. Initial/new evidence lives under `user-flow-first/`, `user-flow-final/`, and the pending list rerun under `user-flow-lists/`.

This is a shared SDK orchestration prerequisite, not complete production user lifecycle acceptance. Node transport/journal adapters and the existing public deposit/withdrawal config/build APIs still require migration onto this flow; wallet nonce reservation/splitting, node ingestion/selection, watcher folding, retirement/refund/reclaim orchestration, real restart/L1 rollback/L2 correction and live acceptance remain pending. The completed-receipt test exercises unresolved driver status, not an actual L1 rollback. No source integration into the root workspace, live deployment or ABI freeze occurred.

#### Automatic submission verification checkpoint

The full unchanged list suite passed **36/36**, 64.61 seconds, with its documented `--testTimeout 60000`. Together with the **13 new flow cases**, **four original-asset boundary cases**, and **15 supporting SDK cases**, the scoped checkpoint has 53 distinct applied cases plus 15 library cases passing. The initially timed-out run and its evidence are preserved separately. The new SDK ESM bundle was imported directly and all four new flow/error exports were checked. There are no live test processes at this checkpoint.

Commands (from the isolated workspace except the root docs check; Node 22.22.2 / pnpm 9.15.4, docs pnpm 10.11.0):

```sh
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-history-user-flow.test.ts tests/submit-init-emulator-history-deposit-asset-bound.test.ts tests/submit-init-emulator-event-history-list.test.ts
# Above: 50 passed, 3 list wall-clock timeouts; unchanged complete list rerun:
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-event-history-list.test.ts --testTimeout 60000
pnpm --dir demo/midgard-sdk exec vitest run tests/event-history-capture.test.ts tests/event-history-payload.test.ts tests/event-history-funding.test.ts
pnpm --dir demo/midgard-sdk typecheck
pnpm --dir demo/midgard-fault-proofs typecheck
pnpm --dir demo/midgard-sdk build
pnpm --dir demo exec eslint midgard-sdk/src/user-events/history-submit.ts midgard-sdk/src/user-events/history-build.ts midgard-sdk/src/user-events/index.ts midgard-fault-proofs/tests/submit-init-emulator-history-user-flow.test.ts
pnpm --dir docs-site run check:links
```

`history-user-flow-measurements.json` records **470 unique accepted complete signed transactions** across the final flow, asset-bound and corrected full-list evidence. Independent maxima: **15,858 bytes**, **13,741,767 memory**, **8,440,708,851 CPU**; total fees **257,214,295 lovelace**, including fixture setup and stress. The new flow portion contains 112 accepted transactions including setup (maximum 12,848 bytes / 2,497,290 memory / 977,212,298 CPU), plus two separately recorded rejected contention transactions. These failed transactions are excluded from accepted totals. No production ledger limit changed.

Representative automatic inline deposit admission: 1,006 bytes, fee 533,022 lovelace, 1,669,091 memory / 669,887,618 CPU. External deposit admission: 982 bytes, fee 538,675, 1,719,259 / 722,787,543; its separate publication is 1,063 bytes and fee 202,329. Inline withdrawal admission: 1,235 bytes, fee 591,315, 2,170,239 / 937,580,843. External withdrawal admission: 978 bytes, fee 581,653, 2,163,213 / 966,030,570; separate publication is 1,293 bytes and fee 212,449. Both real contention cases preserve the prepublished output and event nonce, reject the stale transaction, refresh funding/predecessor, wait through protection, and admit against the changed successor.

`history-user-flow-source.json` records 3,088 files: two existing files changed and two files added since the bootstrap checkpoint. All 1,225 previously recorded Aiken source hashes, the blueprint (`a9d59ec09a5b0d59b29ccac493e57a0a02f35449d69bf54537affa2be4fd6c08`), pinned compiler binary, and three protected proposal/research/gaps documents were verified unchanged. `history-user-flow-identity.json` binds source, compiler, deployed recipes, complete transaction evidence, build outputs and actual check logs. The source remains isolated. Public build/config migration, concrete node persistence and submission adapters, all downstream consumer/retirement work and full/live acceptance remain required; this does not establish an end-to-end production flow.

### Public builder and signed-deposit journal migration (2026-09-22, in progress)

Replaced the public SDK deposit and withdrawal transaction builders' legacy witness-registration construction with actual history admission. New `prepareDepositSubmissionProgram` / `prepareWithdrawalSubmissionProgram` expose the selected stable nonce, complete payload, storage plan, deployment context and funding request before any signature. These prepared requests feed the shared automatic publication/retry driver; unsigned callers may separately publish and confirm data, then rebuild admission with the same nonce and actual external data output. The configuration permits an explicit nonce out-ref, validity interval, reclaim credential and structural refund key. The nonce resolves to its actual wallet UTxO instead of trusting caller-supplied UTxO fields. No legacy construction fallback remains in these public builders.

`history-user.ts` derives builder context from the declared deployment bundle, validates the selected policy/address/hub recipe binding, excludes datum/reference/history-token outputs from ordinary wallet funding, and quotes future-safe funding. Deposit quotes keep original Value unchanged and expose any structural ADA separately. Withdrawal quotes cover list-node, refund and payout output shapes and reject funding above the payout target. Explicit funding overrides remain checked. The low-level builder supports either a matching reference script or the exact declared attached list script; both attached-script cases passed under the unchanged transaction-size limit. Validity timestamps must be safe integers. Metadata records the actual Order output index and structural/locked lovelace; nonce metadata contains only the out-ref.

The node's signed-deposit journal parser now decodes Order facts, checks the complete 32-byte event key and output/inclusion/funding metadata, and subtracts structural ADA before comparing requested projected assets. Database metadata records structural lovelace as a string and the Order output index. The existing database and withdrawal CLI schema fixtures were extended without changing their assertions. Five new signed-body parser tests passed (native-token fixture, explicitly not history-policy acceptance), alongside eight deposit parsing tests and ten withdrawal CLI tests. All three existing `DepositSubmissionAttemptsDB` tests passed; 106 unrelated cases were not selected.

Initial eight public applied scenarios passed in 6.10 seconds; expanded 11 public scenarios plus the prior 13 retry scenarios passed 24/24 in 9.66 seconds. Public coverage includes both kinds inline/external, attached and reference scripts, exact nonce preservation through prepublication, no per-event certificates, public preparation connected to automatic external submission for both kinds, automatic structural funding, and refusal of missing publication or insufficient explicit structural funding. The full SDK suite passed 69 files / 595 cases in 21.44 seconds; SDK/node/fault-proof typechecks and SDK ESM/CJS/declaration build passed. Initial SDK typecheck caught one unused import; initial node typecheck caught the obsolete CLI metadata fixture; both were fixed. After the final nonce-out-ref resolution improvement, the same 24 applied scenarios, full SDK suite, build and typechecks are rerunning against the final source. No contract source or blueprint changed.

Still required before end-to-end completion: replace legacy SDK read models/Datum schema consumers and node ingestion/projection (currently still decode the old event layout), connect concrete durable node submission drivers and public staged unsigned APIs, reserve/split wallet nonces for publication when needed, migrate watcher pointer/retirement folding and settlement/refund/reclaim builders, then complete real rollback/restart/correction, full package/installed/fit/live acceptance. Existing reserve/payout fixtures still describe legacy event spending and will be migrated together with those builders, preserving their layout/output assertions. No root source integration or live deployment occurred.

#### Public builder verification checkpoint

Final-source reruns passed: **24 applied cases** (11 public builders + 13 shared automatic-flow cases), 19.39 seconds; **595 SDK cases / 69 files**, 28.12 seconds; SDK/node/fault-proof typechecks; SDK ESM/CJS/declaration build; 11-file targeted lint. Node checks passed **23 cases / three files** plus **three selected existing journal database cases** (106 unrelated database cases unselected). Root docs links passed for 306 files and `git diff --check` passed. No process remains active.

Commands from the isolated workspace used Node 22.22.2 and pnpm 9.15.4, with docs run from the root using pnpm 10.11.0:

```sh
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-history-public-builders.test.ts tests/submit-init-emulator-history-user-flow.test.ts
pnpm --dir demo/midgard-sdk test
pnpm --dir demo/midgard-node exec vitest run tests/deposit-submission-attempt.test.ts tests/submit-deposit.test.ts tests/withdrawal-cli-utils.test.ts
pnpm --dir demo/midgard-node exec vitest run tests/database.test.ts -t DepositSubmissionAttemptsDB
pnpm --dir demo/midgard-sdk typecheck
pnpm --dir demo/midgard-node typecheck
pnpm --dir demo/midgard-fault-proofs typecheck
pnpm --dir demo/midgard-sdk build
pnpm --dir demo exec eslint midgard-sdk/src/user-events/{deposit,withdrawal,history-build,history-user,index}.ts midgard-node/src/transactions/submit-deposit.ts midgard-node/src/database/depositSubmissionAttempts.ts midgard-node/tests/{deposit-submission-attempt,withdrawal-cli-utils,database}.test.ts midgard-fault-proofs/tests/submit-init-emulator-history-public-builders.test.ts
pnpm --dir docs-site run check:links
```

`public-builders-verified/` contains the exact applied recipes, full signed CBOR and measurements. The two evidence files contain **202 distinct accepted transactions**, plus two rejected contention transactions excluded from accepted totals. Independent maxima are **13,817 bytes / 2,497,290 memory / 978,277,503 CPU**; total fees **107,660,908 lovelace**, including fixture setup. The public-builder portion is 90 accepted transactions, total fees 48,942,616. Attached inline deposit admission measured 13,624 bytes and fee 906,648; attached inline withdrawal admission measured 13,817 bytes and fee 958,210. Reference-script inline admission measured 1,079 bytes / fee 543,224 for deposit and 1,272 bytes / fee 595,974 for withdrawal. All retain 16,384-byte / 16.5M-memory / 10B-CPU production limits. These representative shapes do not freeze maximum payload bounds.

`history-public-builders-source.json` inventories 3,091 files (eight prior paths changed, three added since the automatic-flow checkpoint). All 1,225 Aiken source hashes, blueprint, pinned compiler binary and protected design/research/gaps document hashes still match. `history-public-builders-identity.json` binds this source, deployed recipes, measured signed transactions, compiled SDK outputs and check logs; `history-public-builders-measurements.json` retains per-transaction numbers. The work remains isolated. Node automatic submission and read-side consumers, retirement/refund/reclaim, watcher folding, full replay/rollback/live and integration gates remain open.

### Authenticated readers and pointer-safe ingestion (in progress)

Started replacing the SDK read APIs with explicit history deployment descriptors, complete-list snapshots and actual retained data. The query layer now factors Order opening and provides full-list event scans: roots/fillers are excluded; a missing root or disconnected successor fails the scan instead of silently dropping events. `DepositUTxO` / `WithdrawalUTxO` now expose immutable event/facts, current authenticated history witness and original assets separately from the full node UTxO. The node deposit projection now calls the canonical original-Value projection helper. Forced-order fetch behavior is unchanged. Settlement reference lookup now explicitly requires the matching history deployment for deposits/withdrawals; its pre-existing disabled slashing construction remains disabled.

This migration is not yet verified. Initial SDK typecheck identified seven old withdrawal read-model accesses and two old converter calls; these were migrated. Initial node typecheck identified old reader config/field fixtures and the legacy reserve/payout fixture constructors. The straightforward config/field callers have been updated; reserve/payout fixture reconstruction and final checks remain pending. Database equality currently treats pointer transaction changes as immutable payload drift, and submission/status lookups still assume the original transaction hash. Those need deliberate updates that preserve event payload, inclusion time and projected state while treating current L1 locations as mutable. Current typed readers alone are not a completed ingestion, rollback, or retirement flow.

#### Reader and ingestion checkpoint (2026-09-22)

The SDK now reads both event kinds from authenticated history: complete list snapshots, explicit deployment descriptors, full-key identity checks, actual retained datum authentication, and separate original assets. Root/filler outputs and donations cannot become events. Inclusion filters use immutable Order facts. Ten new SDK cases cover both kinds inline/external, exact event field CBOR, funding separation, kind/identity/address/token errors, incomplete lists, provider failure and unavailable retained data. The full SDK suite passed **70 files / 605 cases** in 21.75 seconds.

The existing 11 public-builder applied scenarios now also read the admitted events through the public APIs. Six cases submit real successor insertions, consuming the original Order output; each subsequent scan returns exactly one event with unchanged identity, payload, inclusion time and original Value. Both inline and external modes pass for both kinds. The structural-funding case verifies that the reader returns the requested 1.5M lovelace instead of the larger locked amount. All 11 applied cases passed in 8.50 seconds, using actual parameterized history scripts and the existing native fixture hub. This does not establish node retirement or live acceptance.

Deposit and withdrawal upserts now refresh only the current observed L1 location while preserving projection/header assignment and withdrawal classification. Event contents, original projected output, inclusion time and refund data remain immutable. Conflicting locations for the same event in one batch are rejected. Each upsert batch is transactional: an immutable-content conflict rolls back all location updates. Tests preserve awaiting/projected/terminal state and reject payload/time drift. Testing exposed pre-existing JSONB double encoding in withdrawal classification and submission metadata; serialized JSON is now bound as text before its JSONB cast, preserving object/array values in retrieved rows. Failed intermediate SQL construction attempts are retained in the evidence logs; the final focused database run passed **27 cases** in 26.70 seconds, with 93 unrelated cases unselected.

Submission reconciliation now fetches current authenticated deposit history before consulting the saved intent. It checks event identity, actual history address/key, original assets, inclusion time, structural funding, L2 address/network and datum. It persists the same authenticated snapshot used for that decision. Moving an Order no longer requires its original transaction hash to remain the current output hash; an absent or mismatching event stays ambiguous, and a provider error remains an error. A cached row cannot by itself establish L1 confirmation. The intent journal now stores the requested L2 datum. Status lookup accepts either a current location hash or the journal's original submission hash while returning the latest observed location. Seven signed-body/intent helper cases and five status helper cases passed; these are read/journal checks, not policy-acceptance evidence. The database integration case proves moved-location reconciliation, preservation of projected state, refusal of cached authority after simulated history disappearance, mismatching inclusion time and provider failure. It is not a full canonical L1 rollback test.

Checks passed: SDK and fault-proof typechecks, SDK ESM/CJS/declaration build, and targeted lint/format for 23 touched TypeScript files. **Node typecheck remains failing** at the two old `datum` constructors in `tests/reserve-payout-builders.test.ts` (lines 378 and 394 at this checkpoint). Those fixtures still describe legacy event spending and must be migrated together with the actual reserve/retirement builders; no stand-in history witness or compatibility field was added to manufacture a pass. The old raw-draft assertion in the node submission fixture still uses its actual legacy draft schema, while the real SDK reader callers use Order facts. Full node suites and all live/rollback gates remain incomplete.

Commands from the isolated workspace used Node 22.22.2 and pnpm 9.15.4:

```sh
pnpm --dir demo/midgard-sdk exec vitest run tests/event-history-readers.test.ts
pnpm --dir demo/midgard-sdk test
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-history-public-builders.test.ts
pnpm --dir demo/midgard-node exec vitest run tests/database.test.ts -t 'authenticated history pointer persistence|DepositSubmissionAttemptsDB|persists an in-memory withdrawal classification|DepositsDB and MempoolLedgerDB exact-once projection'
pnpm --dir demo/midgard-node exec vitest run tests/database.test.ts tests/deposit-submission-attempt.test.ts tests/deposit-status.test.ts -t 'authenticated history pointer persistence|DepositsDB and MempoolLedgerDB exact-once projection|persists an in-memory withdrawal classification|DepositSubmissionAttemptsDB|history deposit submission journal|deposit-status command helpers'
pnpm --dir demo/midgard-sdk typecheck
pnpm --dir demo/midgard-fault-proofs typecheck
pnpm --dir demo/midgard-node typecheck # fails: two legacy reserve/payout fixture constructors
pnpm --dir demo/midgard-sdk build
# Targeted lint uses the explicit 23-file list in history-readers-touched-ts.json.
```

`readers-applied/history-public-builders.json` retains recipes, signed transactions and measurements: **96 accepted transactions**, including setup and six new pointer continuations; maximum **13,817 bytes / 2,497,290 memory / 963,180,871 CPU**; total fees **51,957,294 lovelace**. The continuations range from 1,184 to 1,479 bytes and 496,484 to 510,087 lovelace fees. Production ledger limits are unchanged. No Aiken source, blueprint, root source integration, deployment, or ABI freeze occurred in this phase. `history-readers-source.json` and `history-readers-identity.json` bind the partial checkpoint and explicitly retain the failing node typecheck.

Next required work: migrate reserve/settlement/payout/refund retirement builders and fixtures, replace remaining legacy SDK proof helper schemas, connect durable automatic node submission and staged unsigned APIs, update watcher folding and canonical rollback/correction behavior, integrate the isolated source while preserving parallel work, then run the outstanding full/installed-family/fit/live acceptance gates. Pointer refresh alone does not establish canonical rollback, retired-ID reuse, terminal retirement or recovery correctness. The full objective remains active.

### Retirement builder migration (in progress)

The next consumer trace found that the deployed payout mint policy still required a legacy withdrawal spending redeemer. It cannot authorize the new history retirement transaction. The selected migration pins the exact withdrawal-retirement observer hash as a second payout-mint deployment parameter, invokes its exact zero withdrawal, and binds its redeemer to the selected Order input, hub reference and `InitializeWithdrawalPayout` purpose. This explicit delegation keeps refund/absorption authorization distinct from payout initialization. The payout spending policy and burn behavior retain their existing predicates. The mint redeemer field is renamed to `retirement_withdraw_redeemer_index`; its constructor field order is retained, but its semantic target changes. SDK parameter application now derives the pinned observer from the declared history deployment.

The contract and schema edits invalidate earlier blueprint/source identity for new retirement acceptance. The clean build, compiler provenance and 27 passing payout tests are now recorded in the following integration section. SDK retirement builders, fixtures and watcher interpretation are being migrated in parallel; final deployment identity and integrated acceptance remain open.

### Parallel consumer integration (2026-09-23, in progress)

User requested three subagents; file ownership and shared interfaces were recorded before parallel edits in `artifacts/event-history/parallel-ownership.md`. Watcher agent owns indexers plus designated fixtures; builder agent owns reserve/payout/refund/reclaim builders and designated fixtures; third agent completed read-only consumer/acceptance audit. Root retains shared schemas, submission persistence, ingestion, reconciliation, storage, final integration and all deployments/live acceptance. Implementation remains in the isolated transition workspace; unrelated main-checkout edits preserved.

New normal `aiken build --env testnet` succeeded in the clean retirement source copy. Verified all copied `.ak` files against current implementation, generated payout mint arity two (`hub_oracle`, exact `withdrawal_retirement_observer`), and the `retirement_withdraw_redeemer_index` definition. Blueprint SHA-256 `7b7a0abae2db62e3ecb9e26e7907bbc1d4793eeee1a522470cc1c1014d1c8d74`, 1,162 validator entries. Payout unit suite collected/passed 27. Local compiler cargo installation metadata identifies CI revision `5adf7837cbddb5d329fd51d9c0cd73f561eaf95c`; version `v1.1.23+5adf783`, binary SHA-256 `87ace169cc4727c15ac269a4a16706e233f1b60b3e83a4a8575b500d63a603d3`. Its MD5 differs from the CI tagging build, as allowed by CI source-build provenance comments. Evidence: `history-retirement-build-identity.json`; earlier provenance-pending note is superseded. This build is not deployed acceptance or ABI freeze.

Root added shared retirement Args/reclaim schemas and reclaim export on agent request. Root migrated both watcher replay projections to authenticated Order original Value, using a shared SDK funds derivation, and migrated validation-dispute replay to immutable admitted history openings (root/filler exclusion, external payload and original assets), preserving forced-order behavior. Deposit journal now captures exact completed CBOR/body hash and immutable intent before signing/broadcast; recovery parsing checks body hash. This closes that ordering hole only: full durable automatic node driver, withdrawal journaling, staged public unsigned API and canonical L1 rollback remain open.

Root checks so far (Node 22.22.2, pnpm 9.15.4, isolated workspace; logs under `artifacts/event-history/parallel/`): SDK reader/payload 15 passed; watcher original-funds 5 passed (serialization/projection fixtures, not applied policy); deposit parser/status 13 passed; database selection `DepositSubmissionAttemptsDB|history pointer` 14 passed, 106 unselected; validation origin/catalogue/typed-reason suites 142 passed. SDK and fault-proof typechecks passed; root touched-file lint passed. Installed validation replay tests preserve normal/forced catalogue behavior but do not yet prove full new deposit/withdrawal installed replay. Independent final integrated checks still pending agent changes.

Read-only audit report `parallel/audit-consumer-acceptance-20260923.md` and source hashes identifies missing full durable submission, canonical L1 event rollback, legacy live-journey fixtures/public support consumers and final-source applied/replay/fit/live gates. Root has begun remediation of the watcher original-Value and validation-dispute consumers identified there. Broad acceptance remains incomplete. No live state reset/redeployment or limits increase performed in this phase.

Further root integration checks: full SDK suite passed 609 tests / 71 files; pinned testnet `midgard/event_v1_abi.{..}` collected/passed 14; node typecheck passed; root documentation links passed 309 Markdown/MDX files. Watcher typecheck remains an open gate while agent fixtures migrate. Its first central run exposed nine errors, including three old published-deposit trace consumers and a production catalogue constructor missing history settings. Root replaced those trace projections with actual authenticated readers/original assets and bound each fabricated-family history environment to its verified applied manifest/blueprint before constructing the catalogue replayer. Transcript records now carry per-kind history payload bytes instead of obsolete deposit/withdrawal witness credentials. These latest watcher consumers are linted, with full runtime/restart verification pending shared fixture migration. Read-only root-change review: `parallel/audit-root-integration-review.md` found no confirmed new regression but explicitly leaves durable resume and applied replay/crash boundaries unverified.

Builder handoff independently verified: central rerun of `tests/reserve-payout-builders.test.ts tests/deposit-submission-attempt.test.ts tests/deposit-status.test.ts` passed 39 tests (26 builder + 13 parser/status). `MIDGARD_REAL_BLUEPRINT_PATH` points to the new normal blueprint; independent signed evidence is `parallel/root-independent-builders-signed-transactions.json` (15 accepted transactions), separate from the agent's receipt. SDK ESM/CJS/DTS build passed after the stable builder handoff. The builder report `parallel/builders-handoff.md` lists ten changed files, exact commands and limitations; scoped acceptance uses real list/retirement/retention/payout scripts with seeded hub/frontier/settlement state. No full frontier/live claim. Root post-consumer watcher typecheck now fails only four errors in agent-owned indexer/support test fixtures; source consumers migrated in this phase have no reported diagnostics. Watcher agent remains active on those migrations.

Central checkpoint identity and exact root source/log hashes: `parallel/root-integration-checkpoint-identity.json`. Root has no active compiler/test process at this checkpoint. The original full objective remains active: unresolved durable automatic node driver/withdrawal persistence/staged public API, canonical rollback and correction integration, broad watcher fixtures/suites, installed event replay and fit, final root-source integration preserving unrelated work, deployment/live acceptance and maximum/race/frontier gates are still required. No goal completion claimed.

### Durable node submissions and watcher origin integration (2026-09-23, in progress)

All three requested agent roles completed their initial handoffs. Watcher ownership was extended within its existing indexer remit to origin fixture migration and canonical Plutus-data comparison; builder ownership was extended to four journey event/settlement fixture files. Exact assignments remain in `artifacts/event-history/parallel-ownership.md`. Root continues to own submission/persistence, ingestion, reconciliation, shared schemas, deployment binding/initialization, integration and live acceptance. Implementation is still isolated in `/home/gumbo/midgard-hub/event-history-transition-workspace`; the shared checkout still has the same 239 dirty entries. No unrelated source was overwritten, and no live deployment or state reset was performed.

Both node submission functions now call the shared SDK publication/admission state machine through a durable PostgreSQL adapter. A caller-provided submission ID binds kind, applied policy, wallet and canonical pre-nonce intent. The selected nonce and complete payload/funding/reclaim request are committed before building/signing. Resumption loads that request before any nonce selection. CBOR preserves Plutus maps and string quantities preserve integer precision. Checkpoint revision CAS rejects stale processes. Separate unique input reservations atomically commit with the nonce or completed pending body, including collateral, so another request cannot claim a pending transaction's funding as its nonce. A failed competing reservation rolls back its checkpoint and claims before signing. CLI deposit/withdrawal commands now require `--submission-id`; withdrawal resumption reads its saved body before a live L2 UTxO lookup, allowing recovery after that output was projected/spent.

The transport checks the actual signed body hash before broadcast and reconstructs the exact completed transaction on resume. A missing output, `not_found`, `pending`, provider failure or a loose BadInputs string does not authorize a replacement transaction. The SDK retains the completed publication attempt after confirmation and rechecks it before a resumed admission; local receipts confer no current L1 authority. Applied SDK admission now supplies explicit collateral candidates from its consumed wallet inputs and checks the completed collateral set, preventing hidden selection of another request's nonce or a permanently locked collateral-only coin.

This is not full submission-lifecycle completion. Automatic recovery from a definitively rejected/expired body is still missing from the node driver; it safely retains ambiguity instead of guessing from absence. Publication validity/expiry and single-UTxO funding splitting still need their full bounded lifecycle. Pending-input release after authoritative rejection, staged external-wallet public API, process-kill/canonical rollback coverage, and successive/concurrent same-wallet acceptance remain required. The new tables were added to the undeployed baseline and schema inventory; no existing deployment database was upgraded or reset. Applying this baseline to an existing environment requires the explicit deployment/schema disposition mandated by `docs/agents/state-reset.md`, centrally coordinated with the authorized redeploy and live acceptance.

Root verification (Node 22.22.2 / pnpm 9.15.4, logs in `artifacts/event-history/parallel/`):

- `root-durable-journal-tests.log`: 32 passed across journal, exact-body transport, deposit parser/status and migration suites. Includes independent database scopes, competing request/nonce/input claims, stale CAS, changed intent rejection, large quantities/maps, a fresh Lucid instance and actual signed-body mismatch rejection before submission.
- `root-durable-node-applied-tests.log`: both deposit and withdrawal passed production node journal/driver recovery after an injected signing failure. A saved external-publication body resumes unchanged, admission completes, and another resumption performs no preparation/nonce allocation. Real list scripts run under the fixture's explicit native hub; this is not live deployment/frontier acceptance.
- `root-durable-sdk-user-flow.log`: 26 applied user-flow/public-builder tests passed after the publication-receipt and collateral changes. The two new cases stop admission recovery when the original publication receipt is unresolved. Existing crash, contention, delayed-visibility and honest controls remain.
- `root-durable-sdk-suite.log`: all 609 tests / 71 files passed. SDK ESM/CJS/DTS build and node main/worker builds passed; node typecheck and touched-file lint passed. Exact logs retain commands/results; initial test-only address and emulator pre-genesis mistakes were corrected without relaxing assertions.
- `root-independent-watcher-tests.log`: 65 passed across the four primary agent indexer suites plus root original-funds coverage. Agent origin follow-up passed 32/32 broader cases and six focused origin regressions, plus typecheck/lint/format. Root independent rerun of that final origin handoff is tracked separately in `root-independent-watcher-origin-tests.log`.
- `root-history-initialization-tests.log`: all eight real initialization emulator tests passed. The actual accepted atomic initialization regenerated `demo/midgard-watcher/tests/fixtures/user-event-initialization.json`; it binds blueprint `7b7a0abae2db62e3ecb9e26e7907bbc1d4793eeee1a522470cc1c1014d1c8d74`, initialization transaction `d53c7d0b89aca966d5862227ab420a2e774ce6373ecb5a162bc00e9b3cf866cd`, and eight creating reference transactions. `parallel/history-initialization/node-atomic-history-bootstrap.json` retains accepted frames and exact emulator recipes. No hand-edited transaction CBOR or public-chain inclusion claim.

Root fixed the shared watcher binder to apply each signed history recipe and independently verify list, retention and retirement scripts against the exact signed blueprint/manifest. Agent origin parsing now compares canonical Plutus values while preserving actual definite/indefinite datum bytes; malformed data and policy mismatches remain rejected. The prior 23-failure origin status is superseded by the successful migrated agent run and the separately recorded root verification.

Builder journey handoff `parallel/builders-journeys-handoff.md` reports four focused tests and scoped lint/format passing. Its stable fixture interface uses an authenticated Order plus policy; durable captures use commitment/opening CBOR, preserving original Value and external data. Root canonicalized map datums and committed deposit leaves in the retained-transition helper and began migrating both journey staging readers to complete authenticated history, accepting pointer movement only when signed immutable facts and original funds still match. Broader journey consumers are not yet migrated: the tools typecheck remains red in legacy retained-event inputs and fabricated/catalogue history environments (`root-journey-integration-typecheck.log`). This is an open integration gate, not an unrelated baseline exemption.

Read-only review reports `audit-durable-submission-design.md`, `audit-durable-implementation-review.md` and its agent follow-up identified the funding-reservation and spent-L2-resume defects that root fixed. Collateral selection was checked against the installed Lucid implementation before the SDK correction. Full canonical L1 rollback/L2 correction integration, all installed-family replay/fit checks, maximum/churn/frontier/deadline scenarios, final source integration preserving unrelated work, and centrally coordinated deployment/live acceptance remain open. The original full objective remains active and incomplete.

### Frozen consumers and complete-journey priority (2026-09-23, active)

User steering is authoritative: freeze shared interfaces, prioritize one complete
deposit and withdrawal journey through settlement before extending failure
coverage, run narrow edit checks and broad suites at stable checkpoints, and
reuse one centrally verified deployment for coordinated live acceptance.
All three agents received this direction. No new rollback implementation was
started; the read-only findings remain recorded in
`parallel/audit-canonical-rollback-integration.md` for the subsequent phase.

Frozen consumer interfaces: SDK `DepositUTxO`/`WithdrawalUTxO` reader orders;
journey `StagedHistoryEvent = {order,policyId}`; captures as
`{commitmentCbor,openingCbor}`. Published deposit checkpoints now require
`depositHistory` and captured history for prior deposits. Canonical CBOR retains
arbitrary Data maps and original funds; archives grant no new L1 authority.
Watcher capture handoff: nine files, 4/4 focused tests, both package typechecks,
lint and format passed; 81 live-artifact tests were skipped and are not acceptance.
Root independently reran the capture tests: 4/4, 3.30s. Root independently reran
watcher origin/recovery: 32/32, four files, 167.29s. Exact agent commands/hashes
are in `parallel/watcher-capture-*` and `parallel/watcher-origin-*`.

Root added `event-history-bindings.ts` for journey verifiers. Transition,
settlement and both fabricated-family history environments derive from the same
actual manifest/blueprint. The transaction-only synthetic verifier remains
explicitly synthetic and rejects event-reader use. Core ESM/CJS/DTS rebuild
resolved the stale compiled export; both built CLI help checks now pass and show
required `--submission-id`. Root docs link check passed 316 Markdown/MDX files.

Builder's final consumer file `history-event-cases.test.ts` passes full tools
typecheck, lint and format. Its existing seven runtime cases still fail at a
later transaction submission; this gate is open and further repeated full
fixture deployments were stopped. Logs and exact scope are in
`parallel/builders-event-cases-handoff.md`.

Root is prioritizing the existing node test `runs deposit, reserve absorption,
withdrawal commitment, and payout to conclusion`, which uses one real emulator
fixture across both lifecycles. Its diagnostic submission helpers now call the
production durable node submission path with explicit test-wallet funding split;
they no longer use the legacy per-event witness workaround. A confirmed split
now refreshes the wallet override. The first run failed before admission because
that override was stale; this is not successful journey evidence.

Observer registration initially collected every publication change output and
exceeded the 16,384-byte limit. Selecting automatic preset funding exposed a
Lucid certificate-deposit selection gap on a 450-UTxO wallet (5 ADA selected for
8 ADA of stake deposits). Root now selects bounded explicit funding for all
certificate deposits, the protocol maximum fee bound and minimum change; actual
fees remain builder-computed and limits unchanged. Narrow registration checks
and the single complete journey are the next gates. Separate node Vitest
processes contended for the schema advisory lock once; all subsequent node test
commands are serialized. No live services, deployments or durable deployment
resets occurred. The full original objective remains incomplete and active.

Complete-journey gate now passed centrally: `root-history-complete-journey-4.log`
records the existing deposit → reserve absorption → L2 transfer → withdrawal
commitment/settlement → payout conclusion test passing in 50.62s (44.44s test).
Both event kinds used one applied emulator deployment, the durable production
node submission path, real node ingestion/selection/journals, actual state-queue
merge/frontier and settlement outputs, and node reserve/payout commands. The
other test in that file was intentionally not selected in this focused run.
This is emulator lifecycle evidence, not live acceptance. The clock advances
through the actual protocol maturity windows; production time constants and
limits were not shortened.

Retained artifact `parallel/complete-journey/node-complete-history-journey.json`
binds the current blueprint SHA256
`7b7a0abae2db62e3ecb9e26e7907bbc1d4793eeee1a522470cc1c1014d1c8d74`,
applied history recipes/scripts, hub/state-queue/settlement/payout identities,
protocol parameters, event IDs and two settled header hashes to 33 accepted
signed transactions. Capture starts after reference-script publication and
includes initialization and lifecycle transactions. Measured maxima: 4,416
signed bytes, 3,624,428 execution memory, 1,382,976,747 steps. Aggregate fees for
these 33 transactions: 18,628,602 lovelace; this excludes preceding reference
publication fees and is not a production fee estimate. Deposit header:
`ccb7b3bf6bfb49854cc077cb7fd15ddc986198b231328b9020e4dfce`; withdrawal header:
`57565c843d0936b7fda079d87222450df016d26f01c164d2b1699907`.

The final integration fix replaced generic reference target labels with the
actual published authenticated deposit/withdrawal spending and retirement roles.
Strict role-token verification and public builder interfaces remain unchanged.
Builder handoff: `parallel/builders-reference-roles-handoff.md`, 4/4 SDK tests,
SDK/node typechecks and scoped lint/format passed. Central registration tests
passed 2/2; the 450-output wallet used a 402-byte transaction, paid exactly four
stake deposits plus the actual fee, preserved nonces and resumed idempotently.
Read-only review: `parallel/audit-happy-journey-wiring.md` found no concrete
regression; no test assertions were weakened.

At this stable checkpoint root launched the full SDK suite, SDK rebuild, node
and tools typechecks, and the affected nine-file node submission/journal/
registration/deposit-flow suite. Results are tracked separately below. One
verified live deployment for coordinated acceptance remains future work; no
agent has permission to independently deploy or reset services.


### Central stable checkpoint and ordinary consumer control (2026-09-23)

The external-wallet fixture now authenticates the actual history Order instead
of an obsolete per-event witness datum, preserving external signing, exact
original value, destination/datum and inclusion facts. It explicitly requests
2 ADA structural funding and checks the exact separation from the 12 ADA
original deposit. A real node unsigned-builder defect was fixed: recreating
Lucid from explicit funding context now preserves its custom slot configuration.
Without that mapping, an advanced emulator clock produced a stale validity
window. The final focused external-wallet test passed 1/1 (one other test
intentionally unselected), 18.12s; node typecheck, scoped lint and rebuild passed.
The prior nine-file checkpoint had 26/27 passing before this correction. The
later required emulator suite also passed both tests in the submission file.

Named lower-layer checks were run from the isolated demo workspace:

- `pnpm run test:tx-prep:sdk`: passed, 175 Lucid tests and 613 SDK tests.
- `pnpm run test:tx-prep:node`: passed, 123 node tests and 51 tools tests.
- `pnpm run test:tx-prep:emulator`: node portion finished with 63 passed and
  one failed in 361.31s. The failure expected the obsolete generic reference
  label `history list`; actual authenticated publication correctly uses
  `withdrawal spending`. Root updated only that expected label, retaining the
  missing-script error and beneficiary-address assertions. Focused rerun passed
  1/1, 25 intentionally unselected, 8.35s. The original named invocation remains
  recorded as exit 1; its shell did not reach the proof package.
- The remaining proof-emulator command was started separately with the exact
  package/filter/pool from the named script; its final result is recorded below.

The builder agent added one ordinary healthy control while keeping the prior
seven scenarios and assertions. The fixture advances through actual list
protection, and the new control synchronizes only fake Date to the emulator;
production clock rules and public interfaces remain unchanged. Agent pass was
independently reproduced by root: 1 passed, 7 intentionally unselected, 38.13s.
It admits two deposits and a withdrawal and verifies healthy retained
classification on one published emulator fixture. This is not closure of the
seven remaining cases. Handoff: `parallel/builders-ordinary-history-handoff.md`;
central log: `parallel/root-independent-ordinary-history-case.log`.

The ordinary external-wallet and consumer checks are separate emulator fixtures;
the complete deposit-through-payout lifecycle itself reused its one applied
deployment. Live acceptance will use one centrally selected current deployment.
No independent agent deployment occurred.

Runbook preparation uses the current acceptance skill copied into the isolated
tree so currency validation inspects the current CLI. It now retains a stable
run-bound deposit submission ID, permits an explicit source root, and names all
four required history parameters with no production defaults. The isolated
`.env.example` includes protection duration and bounds. Runbook currency,
frontmatter and scoped formatting checks passed; these are documentation checks,
not live acceptance. Final integration must preserve the main example's newer
provider pins/bootstrap/governance guidance and committee CLI naming.

Read-only live prerequisite checks found Docker unreachable, its systemd unit
not installed (`LoadState=not-found`, inactive/dead), and both local Ogmios
1337 and Kupo 1442 endpoints refused connections. No service, persistent
state or deployment was altered. Existing inspected manifests also lack the
current history identities and have incompatible schema/blueprint bindings.
The authorized route is one fresh current identity with matching durable state,
preserving the old deployment; source/parameter and local provider prerequisites
remain unsatisfied. See `parallel/audit-deployment-acceptance-route.md` and
`parallel/root-live-prerequisite-checks.json`.

Root's preliminary integration inventory found 193 differing paths whose main
copy still matches the captured baseline, plus 23 paths requiring review.
Nothing was copied over the main source. Read-only review supplied exact CLI,
example and runbook merge hunks preserving unrelated work, in
`parallel/audit-cli-env-runbook-integration.md`; Aiken overlaps remain for root.
Main dirty-entry count remained 239; `git diff --check` passed.

Watcher reconciliation review found positive pointer/retirement authority
already available, but semantic-head rollback is not integrated into the local
publisher/restart path. Its checkpoint rollbackGeneration is carried unchanged,
not a durable rollback counter. A missing Order or unavailable watcher authority
must remain unknown, not orphan proof. Root has not added a misleading wrapper
or treated this as closed. Node canonical eligibility/generation fencing,
withdrawal correction reclassification with reversible journals, conflict/expiry
recovery, full staged wallet flow, remaining size/retention/pruning/replay cases,
final source integration and live acceptance remain open. Full objective active.


Further root integration analysis supplied the missing exact archived Aiken
baseline from `history-transition-raw-build-source.json`. The revised inventory
has 204 baseline-safe differing paths and 12 review paths; this replaces the
preliminary 193/23 count above. Exact-base three-way proposals are retained only
under `parallel/proposed-integration/`, with conflicts intact for review, never
applied as source. `parallel/root-aiken-three-way-review.json` identifies five
content conflicts, one clean merge that differs from candidate, one file absent from main,
and one candidate unchanged from baseline. The generated blueprint remains a
rebuild/identity gate, not a JSON merge target. Separately, 24 main-only demo
changes must be preserved (`parallel/root-main-only-source-changes.json`).
Root already merged the current main provider/governance guidance into the
isolated environment example, adding only history settings and correcting the
claim that every parameter has a default. Main itself remains unchanged.

The proof suite found a missing current yield-reference binding in the original
transition-trace fixture. Root now uses the existing authenticated
`publishTransitionTraceYields` helper before header-time sampling and merges its
17 actual role references into deployment info. All original test assertions
remain. Focused test passed 1/1 in 12.38s; lint import sorting and format passed.
Read-only review: `parallel/audit-transition-trace-yield-fixture.md`. Four payload
boundary/reclaim cases also hit the existing 5s test timeout in the broad run;
root is isolating those with one fork and the same timeout/assertions. No
production limit or test timeout was raised.


Provenance correction: the Aiken baseline is a staged build rather than an
established common ancestor of main and the candidate. Its conflict/absence
classifications are review pointers, not proof of intervening user changes or
deletions; the proposed merges remain unapplied. The demo baseline reconstruction
for CLI/environment was independently verified as recorded in the audit.
Boundary isolation completed with all four selected cases timing out under the
unchanged 5s limit (36.28s overall, 32 intentionally unselected). Their coverage
is still failed; no pass or production-bound closure is claimed.


The full proof-emulator remainder finished in 1040.04s: 581 passing and five
failing tests, plus one afterAll fit-ledger identity mismatch; 112 files passed
and three failed (115 total). The largest 1,287-asset/depth-64 deposit trace
passed in 1026.451s within its existing 1800s allowance. The yield-reference
failure was fixed and independently passed as documented; the four 5s boundary
timeouts remain failed in isolation. The additional afterAll failure is the
pinned forced-window fit ledger's old blueprint identity. Root is regenerating
it with the fixture's existing opt-in writer from actual current signed
transactions, followed by a fresh read-only comparison run. No ledger equality
or production budget assertion is being changed.


Fit-ledger generation passed 6/6 in 18.67s; a separate normal run with
`MIDGARD_WRITE_FIT_LEDGER=0` passed 6/6 in 18.54s, retaining pinned equality,
scenario roster, and all lifecycle assertions. The current generated source
`docs/fault-proofs/size-plans/transition-trace-forced-window-fit-ledger.json`
has 196 measurements and current blueprint SHA256 `7b7a0abae2db62e3ecb9e26e7907bbc1d4793eeee1a522470cc1c1014d1c8d74`.
Measured maxima: 15,461 signed bytes, 2,972,835 memory units, 1,015,598,302 CPU
units. Ledger digest `02178c3d3e62d08fd7a5c07a41e134eba0718a21eb367d60c8b107079e808c98`;
file SHA256 `e8e5957711b32695922719ba26338e7cabde7779da3d6703a922be3c699daaec`.
Logs and raw history evidence are under `parallel/root-current-forced-window-fit-*`
and `parallel/forced-window-fit-current/`. This updates the obsolete generated
identity from actual execution, not a relaxed fit check. The original broad
proof invocation remains recorded as failing; four timeout cases remain open.

Final central checks for this slice: node-tools and fault-proofs typechecks,
scoped fixture lint/format, node-tools build and compiled tools/deposit/withdrawal
help passed. Both compiled user commands show required `--submission-id`.
Documentation links passed for 326 Markdown/MDX files. Root command inventory:
`parallel/root-frozen-checkpoint-commands.json`; current source/compiler/build/
artifact hashes: `parallel/root-frozen-checkpoint-identity.json`. This identity
is a final isolated-source checkpoint, not an assertion that every prior run
executed after every later fixture/documentation edit. All test processes are
finished. No main source integration, deployment, reset, commit or live
acceptance was performed. The four timeouts, canonical rollback/correction and
the previously listed original-objective gates remain open.

## Shared-interface and correction recovery checkpoint — 2026-09-23

The three requested agents retained exclusive watcher, builder/fixture, and
read-only audit ownership. Root retained shared schemas, submission, ingestion,
reconciliation, integration, and all live/deployment decisions. Public history
and builder interfaces were unchanged in this slice; the new revision fields
are internal node persistence and journal state. All source edits remain in
`/home/gumbo/midgard-hub/event-history-transition-workspace`. Main source and
unrelated work remain unintegrated and preserved.

Root implemented withdrawal reclassification after authenticated state-queue
correction: the locked journal binds the correction transition digest, preserves
exact settlement bytes plus validity/detail and a classification digest, and
makes repeated apply/retract idempotent before any payload mutation. Reopening
clears only derived classification, records the removed header, advances a
revision and makes legitimately reopened overdue events selectable. Original
submission and L1 facts remain intact. Revision checks now cover classification,
projection, journal preparation and later header assignment. Retraction restores
the original classification and fences stale work; conflicting replacement
assignments are refused. Already-assigned exact-header reobservation may be
idempotent only when settlement bytes, validity and detail match under lock.
Unassigned rows and journal creation still require the exact selected revision.

This is an initial-schema replacement in the isolated undeployed candidate,
not a migration or reset of an existing deployment. The complete original
objective still requires canonical L1 eligibility/reconciliation, nested
correction/rollback provenance and whole-runtime integration. No such gate is
closed by these database tests.

Root narrow correction checks passed 3/3, including exact JSONB/digest reload,
reclassification, repeated correction preserving a newly computed unassigned
classification, retraction, stale revisions, ordinary overdue refusal,
replacement-header conflicts, same-header reobservation and digest tampering.
The broad six-file node checkpoint had 160 passing and one failing test in
70.79s. Both complete deposit-flow emulator tests passed (60.08s), including the
full deposit, reserve absorption, withdrawal commitment, settlement and payout
journey. The failure was shared test cleanup omitting withdrawals, allowing
prior cases to collide on their L1 location. Root added the missing cleanup;
the full database suite then passed **123/123**. The final same-header refinement
was separately checked by the three focused tests, all passing in 12.13s.
The required `pnpm run test:tx-prep:node` passed **123 node + 51 tools tests**.

Independent root replay of all eight staged wallet history scenarios passed
**8/8 in 226.81s**, preserving every assertion and production limit. The builder
agent applied the existing Date/emulator synchronization consistently across
these test stages; its seven deferred cases also passed independently. This
closes their replay gap, not full production external-wallet acceptance.
Boundary profiling confirms the four separate five-second timeout cases remain
open: their required local evaluation alone exceeds that allowance. No timeout,
payload shape, phase, assertion, or production limit was reduced or relaxed.

The watcher agent implemented positive canonical semantic replacement,
protected checkpoint CAS and durable rollback-generation advancement, retaining
the original archived equality readmission. Admission, pointer and retirement
rollback, live owner recovery, cold restart, interrupted CAS, retry and bounded
replay passed 13 focused checks. Root's independent complete two-file suite
then found **35 passing / 1 failing**: restart at the last native block wrongly
required an unavailable successor to infer its height. The agent replaced this
unnecessary dependency with exact hash/slot intersection; mandatory fresh
exact-head capture still binds height, bytes and finality. The original test
assertions remain unchanged. Final independent rerun results follow below.

The read-only consumer audit identified a separate top-level watcher integration
dependency: the coordinator currently awaits semantic recovery before its own
W13 replacement/finality recovery can run. Root must compose immediate
revocation, structural queue rewind, retained consumer-delivery holds, deferred
bridge preparation and serialized retry/wakeup, preserving quarantine and all
current authority checks. The six-file watcher implementation does not close
that production-service gate. Exact review and acceptance requirements are in
`artifacts/event-history/parallel/audit-watcher-rollback-integration.md`.

No Aiken source/build, live service, deployment, durable deployment reset or
commit occurred in this slice. The unchanged current blueprint identity is
`7b7a0abae2db62e3ecb9e26e7907bbc1d4793eeee1a522470cc1c1014d1c8d74`.
One verified current deployment will be selected/reused centrally once source
integration and local provider prerequisites are satisfied; the old manifest
cannot establish compatibility. Original remaining gates include L1 rollback
eligibility/generation fencing, correction ancestry, conflict/expiry recovery,
production wallet splitting, adversarial/maximum/retention/pruning/deadline
coverage, the four boundary failures, preservation-aware main integration and
one coordinated live acceptance run. The original goal remains active.

The independent watcher rerun passed **36/36 across both complete files in
173.93s** after the saved-tip fix. The unchanged 128-observation restart case
passed. The new probe also uses the native supervisor's existing cancellation;
root approved one additive optional `signal?: AbortSignal` transport-input
field so TypeScript exposes the behavior the supervisor already implements.
The watcher DTS build initially caught the missing declaration; it is not
recorded as a pass. Final build/static results and exact source hashes are
recorded in the checkpoint evidence after that declaration is applied.

Root's final node typecheck and scoped lint passed, and node plus node-tools
builds passed. The runbook currency check passed (17 commands, 22 recovery
drills). Earlier failed runs are retained alongside corrected results, including
the cleanup collision, empty mempool deletion on correction retraction, JSONB
binding review, stale revision checks and saved-tip restart failure. Evidence
files use `parallel/root-correction-*`, `root-watcher-semantic-*` and
`root-staged-history-*`; each agent supplied its changed-file inventory, exact
checks and unresolved gates. Top-level coordinator integration and live
acceptance remain open despite passing component suites.

Final watcher ESM and DTS build passed after exposing the existing optional
cancellation argument. Documentation links passed for **333 Markdown/MDX
files**, and main `git diff --check` passed. Final isolated-source, compiler,
blueprint, compiled-entrypoint and evidence inventory:
`parallel/root-correction-checkpoint-identity.json`; root command/results and
changed-file manifests: `parallel/root-correction-checkpoint-commands.json` and
`parallel/root-correction-changed-files.json`. This supersedes the prior source
checkpoint identity without retroactively changing earlier test results.
Main still has 239 dirty entries. No source integration, live deployment,
reset, commit or completion claim; coordinator and original-objective gates
listed above remain active.

## Watcher recovery composition and consumer checkpoint — 2026-09-23

Implementation remains isolated in
`/home/gumbo/midgard-hub/event-history-transition-workspace`; the main checkout
still has 239 dirty entries. Root owns shared interfaces, node persistence,
ingestion, reconciliation, final integration and all deployment/live operations.
The watcher agent supplied actual durable W13 recovery coverage and migrated two
manifest fixtures. The builder agent supplied reference-authorized reclaim
coverage, the installed-journey protection-time fixture, and W16 diagnosis.
The third agent remained read-only and reviewed recovery races, consumer gaps,
and node canonical eligibility. Every lane supplied changed files, exact checks,
and unresolved gaps. Existing public history schemas/receipts remained stable.

Root composed immediate rollback revocation, serialized structural rewind,
retained included/finalized delivery, deferred bridge preparation and automatic
retry/wakeup. A stable tip no longer requires a new block to deliver recovered
history. Arrival epochs fence an already-running resumed callback; explicit
retired-work errors distinguish cancellation from unrelated evidence failures.
Catch-up waits for both history readiness and delivery, so replay cannot exit
successfully with held work. Shutdown stops/drains the coordinator before closing
its providers and SQLite. Held blocks are retained with an explicit bounded
backlog refusal rather than silently pruned. Diagnostic state preserves failed
acquisition; quarantine still refuses delivery.

Cold startup now attempts existing authenticated W13 recovery before opening
an owner whose durable state is quarantined. The new real-SQLite/native-transport
test found that rollback replay attempted to reuse expired live transport
attestations before reaching authenticated retained-path verification. Root's
parser correction admits the existing private snapshot-evidence token only for
an exact complete snapshot: policy, store, rollback state and bootstrap must all
match the private MAC-authenticated durable authority. Fake/copied authority,
substituted fields and missing retained paths remain refused. The test then
opens the actual event owner on the replacement branch, advances semantic
generation once, and verifies idempotent reopen. Its native transport is
synthetic; this is not independent Cardano consensus/live acceptance.

The first broad watcher run exposed obsolete consumers: **1,465 passed, 57
failed, 24 skipped**, plus a setup failure among six failing files. All failures
were retained. Root/agents restored explicit history bindings in signed manifest,
classifier and transcript fixtures, including equal mint/spend roles. The W16
factory's apparent lease failure was an actual payout refusal: its caller gave
an empty withdrawal target while preserving original ADA. Root bound the caller
to the actual L2 output's Value/address/outref; no funds were dropped and the
original replay assertions passed (21/21). Concurrent fixture lease exclusivity
remains checked, now with useful refusal diagnostics.

The installed watcher journey uncovered a production node clock mismatch:
lease acquisition used process time, but renewal/expiry used PostgreSQL time.
Root changed acquisition to the same database clock and normalized TTL, without
changing duration. Both +/-30-day process-clock regression cases passed through
HTTP/PostgreSQL, including acquisition, renewal, competing-owner exclusion and
release. The lease/endpoint files passed 7/7; the full database suite passed
123/123. Required `pnpm run test:tx-prep:node` again passed **123 node + 51 tools**.

After that fix, the actual proof and correction confirmed. The old journey check
conflated the newly required deposit-history checkpoint lease with atomic
removal. It now verifies exactly one released database row bound to the sole
leased step_07 journal intent and proof-token transaction, while independently
requiring the sole atomic removal intent to have no lease, the false lease flag,
and the terminal correction transaction hash. It rejects extra/unmatched leases
and retains the no-active-lease requirement. This updates the checkpoint-aware
acceptance contract; it does not make arbitrary released leases acceptable.

Root's final broad watcher rerun passed **1,543 tests across all 116 files in
208.01s**, with three existing conditional skips (two retained-run bridge cases
and one optional store-cache benchmark). The installed service confirmed the proof and
correction, committed the next honest block, and classified it healthy. This
includes root's independent execution of the new durable recovery test, all
manifest/consumer fixtures, the owner/indexer rollback suites, and the 10 focused
composition checks. Watcher and node typechecks and changed-file lint passed.
Core/SDK/fault-proofs/node/tools/watcher package builds passed; final node,
node-tools and watcher rebuilds also passed after the runtime/clock changes.

The new real-builder reclaim fixture found an installed provider lifetime bug:
reference-native verification reused an already-freed signer list. Root added a
locked ESM/CJS `@lucid-evolution/provider@0.2.4` patch using a separately scoped
list from the same verified hashes. Authorization/timelock rules and versions
are unchanged. Candidate dependencies were isolated before applying it; main
ESM/CJS hashes and the main lockfile were independently checked unchanged. Both
reclaim lifecycles and four mismatched script/reference refusals pass unchanged
under the default timeout, including root's independent replay. Signed Deposit
and Withdrawal reclaim transactions are 3,465 bytes each, with fees 350,377 and
350,441 lovelace; execution is 448,033/448,797 memory and
160,052,272/160,327,598 steps respectively. The fixture uses a native hub and
never-admitted retained payloads; it does not establish finalized-frontier
retirement coverage.

With the isolated dependency fix, the full SDK suite again passed **613/613**.
Root again ran both applied node deposit-flow tests, **2/2 in 98.19s**, including
deposit, reserve absorption, withdrawal commitment, settlement and completed
payout. The full watcher acceptance above is a separate actual watcher service
emulator run. An optional evidence export now retains the successful watcher
manifest, parameters, durable workflow/decisions, accepted signed transactions
and native fixture blocks; its final focused evidence replay is recorded below.

No Aiken source/build, source integration into main, commit, live deployment,
service reset or durable deployment reset occurred. Blueprint SHA256 remains
`7b7a0abae2db62e3ecb9e26e7907bbc1d4793eeee1a522470cc1c1014d1c8d74`.
The original goal remains active. In particular, the read-only node audit still
finds positive-only L1 ingestion, missing canonical eligibility/incarnation gates,
timestamp-only barriers, candidates without L1 generation fences and cache
reloads that preserve speculative overlays. Root must implement their common
canonical recovery path; L2 classification revisions do not substitute for it.
See `parallel/audit-node-canonical-eligibility-fencing.md` for exact consumers,
proposed private interfaces and required race/restart/rollback checks.

Other original gates remain: correction ancestry/composition, conflict/expiry
resubmission recovery, production wallet splitting, full adversarial/maximum/
frontier/retention/pruning/deadline matrix, the four unchanged five-second
boundary failures, preservation-aware main integration, and one centrally
verified current live deployment/acceptance run. Local Docker/Kupmios remained
unavailable at the last recorded prerequisite check; no live acceptance is
claimed. Full composed live rollback during proof/queue delivery also remains
unverified despite component and normal service-journey coverage.

Exact root commands/results are in
`parallel/root-composition-checkpoint-commands.json`; prior failures and final
logs are retained beside agent handoffs. Final source/compiler/blueprint/built
artifact identity and remaining evidence checks follow below. This checkpoint
does not retroactively bind earlier source versions to later test results.


The final focused evidence replay passed **1/1 in 145.36s** with the optional
export enabled; the default broad run above preceded only this additive
artifact export. Successful deployment manifest, parameters, 78 workflow
records, decisions, 497 accepted signed transaction CBORs and native fixture
blocks are retained in `parallel/watcher-complete-journey/`. Independent root
verification recomputed every transaction hash, checked declared byte/execution
limits and linear base-fee lower bounds, matched all 15 workflow submissions to
confirmed journal entries, verified the finalized manifest/blueprint identity,
the required checkpoint lease, lease-free atomic removal and healthy successor.
Maximums across all 497 transactions are **16,136 signed bytes**, **4,042,014
memory**, and **1,492,669,879 steps**; maximum values may belong to different
transactions. This is emulator/fixture evidence, not live Cardano acceptance.
Manifest ID: `608ca427e831a673c1df86d8abea56c2f97a6e8871184011242508acb7adce55`.
Exact metrics and workflow hashes are in
`parallel/watcher-complete-journey/root-signed-receipt-verification.json`.
The exporter change passed final watcher typecheck and scoped lint. Runbook
currency passed (17 referenced commands, 22 recovery drills).

Final documentation link check passed for **347 Markdown/MDX files**. The isolated
source/compiler/blueprint/built-artifact identity is recorded in
`parallel/root-composition-checkpoint-identity.json`; changed-file ownership in
`parallel/root-composition-changed-files.json`. Dependency patches and native
chain-sync sources are explicitly included in this source inventory (the prior
inventory omitted those directories). Main remains unintegrated, with its
unrelated work preserved; all original unverified gates remain open.

All changed TS/JSON/Markdown source files passed the final scoped Prettier
check; generated pnpm lockfile and raw dependency patch were excluded from that
formatter invocation. Main `git diff --check` passed. No jobs or deployments
remain running from this checkpoint. Canonical node L1 recovery remains the next
root-owned implementation gate.

## Coherent node capture and retirement frontier checkpoint — 2026-09-23

The full objective remains active. Root retained submission persistence,
ingestion, reconciliation, shared schemas, deployment and integration ownership.
SDK event/witness/receipt schemas, manifests and builder signatures were frozen
for this parallel checkpoint. The watcher agent owned only
`demo/midgard-watcher/tests/indexers/user-event-history.test.ts`; the builder
agent produced profiling artifacts only; the consumer agent remained read-only.
All implementation changes remain in the isolated transition workspace. No main
implementation integration, reset, deployment or live acceptance ran.

Root added a node-private acquired Ogmios ledger snapshot reader and an adapter
that decodes both complete authenticated history lists and actual retained data
through the existing SDK interfaces. All required addresses come from one held
ledger state. Exact acquisition and held-state point checks, duplicate/outside
output refusals, lossless quantities, required address coverage and both-list
success precede returning a capture. Missing retained data never triggers an
unpinned provider read. Script-bearing donations remain ignorable; authenticated
history nodes carrying scripts are refused. The reused node Ogmios session now
accepts the pinned lossless parser and owns cancellation/cleanup during opening
as well as outstanding requests. A total capture deadline bounds the operation.

This is a **coherent capture layer, not canonical eligibility**. It is not yet
wired into production ingestion. An acquired state can survive a later rollback.
Pinned source/genesis and hub deployment binding, a node-owned ChainSync monitor,
durable generations, startup revalidation, canonical origin versus retirement
provenance, incarnation reconciliation, selection/projection/candidate/journal
and validation-cache fences remain required. Captured SDK event members are not
deeply immutable authority objects. No watcher-runtime dependency was introduced.
The implementation follows Ogmios acquired ledger-state semantics documented at
<https://ogmios.dev/mini-protocols/local-state-query/>. The installed client 6.9.0
was inspected; its connection API does not preserve configured URL paths, and its
acquisition helper discards the returned point, so the existing node transport
was reused with explicit point checks.

Root verification: **38/38 node tests, 3 files, 7.20s** across the new snapshot
transport (17), both-list inline/external adapter (10), and existing forced-order
L1 observation/ingestion (11). Node TypeScript and scoped ESLint passed. An initial
adapter test asserted a bigint for the SDK's Date field; the test now checks the
exact millisecond value, with the failure log retained. The read-only review's two
opening-lifetime findings were fixed and covered by new tests; the follow-up found
no further concrete capture/adapter bug by inspection. This is fixture/wire-seam
evidence, not a live Ogmios acceptance result.

Watcher agent added six native semantic publication cases: both deposit absorption
and withdrawal refund at inclusion−1, equality and inclusion+1. Premature frontier
refusal preserves the protected checkpoint, trusted head, archive bytes, CAS count
and active state. Successful retirement preserves origin/facts and publishes exactly
once. Root independently ran the entire changed file: **29/29, 171.75s**, including
existing admission/pointer/retirement rollback and restart cases. Agent TypeScript,
lint and formatting passed. These use native parsers with synthetic local transport;
they do not prove ledger execution or live finalized-frontier establishment.

The four unchanged five-second boundary failures remain open. External profiling
isolated roughly 0.91–0.95s per successful WASM evaluation and three distinct
fee/change/collateral contexts per exact-boundary completion. Each oversized
rejection made one throwing WASM call lasting 11.4–11.6s. Removing fixture setup
or broadening the exact-body evaluation cache is not an evidenced safe fix.
Final-context evaluation must remain intact; evaluator-internal profiling and
fee/collateral convergence are separate optimization work. No limits, payload
counts, assertions, evaluators or timeouts were changed to manufacture a pass.

Adding node's pinned `json-bigint` and types initially caused pnpm to re-resolve
unrelated peers. Root reconstructed the prior candidate lockfile and verified its
exact recorded SHA256 `81957b641c285da05267e77992c7d7ceaaeec3e90aa9f1071f2dd0e4756d62a9`
before adding only the two node importer entries. The final lockfile SHA256 is
`688c3cbbd13ccd950d08f79d881512a81254447a265c6f91d2e83a59e909cda4`;
offline frozen installation passed. Existing provider patch and both peer variants
are retained. Main dependencies and unrelated work remain preserved.

Evidence is under `artifacts/event-history/parallel`: `root-node-history-capture-*`,
`root-watcher-frontier-full-file.log`, `watcher-frontier-handoff-20260923.md`,
`builders-boundary-wasm-handoff.md`, `audit-node-canonical-authority-primitives.md`,
`audit-acquired-ledger-snapshot-review.md` and
`audit-history-snapshot-adapter-followup.md`. The previous complete applied
deposit/withdrawal settlement/payout and installed watcher journey checkpoint
remains separately bound to its recorded source. The next root implementation
step is the canonical authority owner and consumer integration; all previously
unverified original acceptance gates remain open.

Final node build and scoped formatting passed; documentation link check passed
for **352 Markdown/MDX files** and main `git diff --check` passed. Exact root
commands/results are in `parallel/root-snapshot-checkpoint-commands.json`;
eight changed implementation files and ownership are recorded in
`parallel/root-snapshot-changed-files.json`. Final source/compiler/blueprint and
node build identities are in `parallel/root-snapshot-checkpoint-identity.json`
(4,037 source files; aggregate
`ec3fc953f62ae7f4b49fee551b68ab9f8b5d73b7f65409a6e12239cfd9ff9ef7`).
Both patched provider peer variants and unchanged main provider/lock bytes were
independently checked. Main still has 239 dirty entries. No jobs or deployments
remain running from this checkpoint.

### Durable authority, cache recovery and static builder checkpoint — 2026-09-23

Root added a node-private durable authority row and generation token. Startup
revalidation, ownership takeover after expiry, rollback revocation, suspension,
renewal and readiness publication compare the exact deployment/owner/generation.
The lease uses PostgreSQL `clock_timestamp()` after acquiring the row lock and
again after mutation. Ready and recovery repair gates own the outer transaction;
an existing transaction is refused to avoid mistaking a savepoint release for
the durable commit. Recovery tokens cannot authorize ordinary ready-state writes,
and old owners cannot revoke successors during teardown. The table is added only
to the undeployed candidate's initial schema; no existing deployment was migrated
or reset.

Canonical cache retirement now immediately invalidates old validation sequences.
Recovery drains persistence, discards speculative overlays, reloads durable state,
resets ordering tails and resumes only its own current epoch. The read-only review
found two races, both fixed and covered: a delayed old poison recovery clearing a
newer poison, and new work waiting on a parked retired Phase A job's old tails.
Existing ordinary poison and incremental-cache behavior remains covered.

Root's recovery coordinator composes these pieces: local suspension precedes the
short durable revocation, then registered whole producer lifetimes drain before
SQL repair and cache reload. The readiness publication and local CAS finish as one
bounded uninterruptible phase; network capture and producer draining stay outside
it. Shutdown fences, conditionally releases only its own durable token and joins
producer/recovery lifetimes outside SQL and cache locks. Source shutdown must
cancel long-lived workers before awaiting this join. The review's cancellation
and teardown findings were fixed and new real-database tests cover interruption
just after Ready commits and an old owner's late producer after ownership takeover.
The final read-only follow-up found no additional concrete bug in this scoped
delta; it did not run tests.

This is a tested recovery coordinator, **not completed production canonical
eligibility**. It does not authenticate a supplied capture and is not yet wired
into production consumers. Pinned L1 source/genesis and hub identity, node-owned
ChainSync monitoring, admission-origin versus retirement provenance, incarnation
repair, every producer's registration and every selector/projector/candidate/
journal mutation gate remain required. A SQL generation does not itself establish
canonicality. Live rollback, MPF equality and subsequent honest continuation remain
open until that integration and acceptance are performed.

Root independently verified **160/160 tests across four node files, 43.26s**:
9 authority, 7 coordinator, 21 cache and 123 existing database tests. They cover
saved Ready on restart, expired/foreign owners, stale and nested writes, active SQL
versus revocation, whole producer draining across delayed postcommit publication,
overlapping rollback during repair, repair rollback/retry, cancellation, cache
retirement and fresh work. Earlier intermediate checks passed 8 authority tests,
then 27 authority/cache tests, then 152 authority/cache/database tests. One first
coordinator test run was interrupted after its fixture awaited the cache claim
lock while deliberately parking repair; the assertion now runs after releasing
that fixture barrier. The next run failed setup because the interrupted test's
probe table remained; its disposable test fixture now uses `CREATE TABLE IF NOT
EXISTS`. Neither run is counted as acceptance. Initial TypeScript errors in the
new error payload, delta fixture and deferred completion were fixed; final node
and SDK typechecks, scoped lint and formatting passed.

Watcher ownership remained with the watcher agent for
`demo/midgard-watcher/tests/indexers/user-event-history.test.ts`. It added retired-ID
reuse refusal after publisher reopen for both event kinds. Root independently ran
the whole file: **31/31, 180.56s**. These cases use the existing in-memory durable
fixture and synthetic native transport, not SQLite/process restart or a valid
L1 nonce re-spend. Later L2 headers containing reused IDs still require the separate
proof/acceptance coverage; this change does not claim that gate closed.

The builder agent produced an artifact-only Lucid optimization; root reviewed and
installed the tracked ESM/CJS patch centrally. Static explicit funding with the
default local evaluator performs one provisional pre-collateral evaluation, then
retains the full final fee/change/collateral convergence loop. The final built body
must contain enough collateral for its exact final fee; otherwise completion
refuses with the amount needed for a `setCollateral` rebuild. Automatic selection,
delayed redeemers and custom evaluators retain their existing paths. New tests use
real UPLC, including a fee-sensitive execution-cost fixture and insufficient
collateral refusal/retry, plus the unchanged existing completion regression tests.

Root's **actual installed** dependency, without any preload, passed both unchanged
five-second exact-1024 cases: **Deposit 3.877s, Withdrawal 3.777s** (2/2, 10.47s).
Installed ESM/CJS completion regressions passed **14/14, 1.81s**. The full SDK suite
passed **615/615 across 73 files, 19.78s**. The agent's separate deterministic
artifact comparison records all 29 evidence records and 22 signed transactions
identical to baseline, with 12 expensive evaluations reduced to 8. That exact
byte-comparison claim belongs to the agent's deterministic artifact run; the root
installed run uses the ordinary fixture and independently proves the checks pass.
The two oversized rejection/reclaim timeouts remain open; no production limits,
payload counts, assertions, evaluator checks or timeouts were relaxed.

The Lucid patch hash changed from `ty2t2vjend3shdta35lxmvupce` to
`pz54z4p2lng3dxmkioedakyile`. Only those twelve lockfile occurrences changed;
frozen offline installation passed with no dependency re-resolution. Candidate
lock SHA256 is `41a28261c0a04de448bbd8525be6c53e4ddacb6a5e5e15a00f1e6baf7b430e65`.
Both active Lucid peer variants are patched; the provider patch remains in use.
Root checked main's Lucid, provider and lock bytes remain unchanged. The required
`pnpm run test:tx-prep:node` passed **123 node and 51 tooling tests**.

Evidence is under `artifacts/event-history/parallel`: `root-history-*`,
`root-convergence-*`, `watcher-retired-id-handoff-20260923.md`,
`builders-boundary-convergence-handoff.md`, and the authority/cache/coordinator
audit follow-ups. Implementation remains in the isolated candidate; unrelated
main changes are preserved. No live service, deployment or main implementation
integration was changed in this checkpoint. The full original goal remains active.

The installed patch also passed the complete node deposit/reserve/withdrawal/
settlement/payout journey (2 tests) and the real availability-challenge SDK
lifecycles (5 tests): **7/7 across two files, 65.57s**. All six affected package
builds passed. Root verified nine active package links resolve to the new Lucid
patch and retained provider patch. Documentation links passed for **360
Markdown/MDX files**, and main `git diff --check` passed. These are candidate
emulator/database/build results; coordinated live acceptance is still pending.

Exact root commands/results are in
`parallel/root-history-convergence-checkpoint-commands.json`; fourteen changed
files and ownership are in `parallel/root-history-convergence-changed-files.json`.
The checkpoint source/compiler/blueprint/dependency/build identities are in
`parallel/root-history-convergence-checkpoint-identity.json` (4,043 source files;
aggregate `c3920413e828baefedab54ddf8dff2835b78a9ee511b8aa670861875303118e2`).
The remaining oversized-input investigation is separate artifact-only work. Its
first pinned evaluator build failed because Clang was missing. Root found sudo
requires a password and authorized an official-package compiler extracted only
under artifacts as a reversible remedy. No installed evaluator or contract change
is part of this checkpoint's verified result.

## Constant-cost evaluator and retired-ID classification — 2026-09-23

The two previously timing-out oversized Deposit/Withdrawal rejection and reclaim
cases now pass under the unchanged five-second limit. Root installed a reviewed,
version-specific local `@lucid-evolution/uplc@0.2.23` tarball in the isolated
candidate, using a frozen offline pnpm install. The Rust patch skips argument-size
traversal only when both configured cost components are constant; original
nonconstant costs, runtime checks and budget charging remain unchanged. Node and
bundler WASM targets were built separately. Public JavaScript/types/metadata were
preserved, with source, licenses, locks, compiler identities and reproduction
recipes retained in `demo/vendor`. Exact reproduction requires the documented
source paths; a relocated-source build differed and is not claimed reproducible.

The builder agent supplied 150 exact published/baseline/candidate replay results
(25 requests across both targets), 2,268 cost comparisons and 270 full-machine
budget/error comparisons. The read-only agent independently inspected the final
source/package and receipts. Root reviewed the patch, verified all 14 package
members and 12 active consumer links, and independently ran the installed gates.
Runtime package SHA256 is
`9c63778c1c8868925b262015425c4ce5fb5dcbe46979f0d7624f4f8e63e0b381`;
source bundle SHA256 is
`e5c7ac634aa288603c04f9c0d3a68d2e7a702dc81ffae148afb05eca7f93b194`.
Candidate lock SHA256 is
`840c4b9a737c7ff84b72b1fef55f5429e1cc80df83e935c8e6a83ff6ae816813`.
Main's lock/evaluator and all 25 prior lock/Lucid/provider file hashes remain
unchanged. No contract or blueprint change was made in this slice.

Verified root results so far:

- Applied list and retention gates: **45/45**, 15.51s. A separate complete list
  evidence run passed **36/36**, 23.53s, preserving signed transaction receipts.
  Root independently recomputed hashes, fees, bytes and execution units for
  **322 signed transactions**. Maxima were 15,858 bytes, 13,726,609 memory,
  8,440,535,893 steps and 2,294,211 lovelace fee; maxima may be different
  transactions. Unchanged limits are 16,384 bytes, 16,500,000 memory and
  10,000,000,000 steps. These fixtures explicitly distinguish native-issued
  frontier authorities from production settlement acceptance.
- SDK: **615/615**, 73 files, 20.55s. Required `test:tx-prep:node`:
  **123 node + 51 tooling tests**. Complete node deposit/reserve/withdrawal/
  settlement/payout plus availability lifecycles: **7/7**, 109.03s.
- Installed history journey file: **10/10**, 339.04s, independently rerun by root
  after the watcher agent's narrow checks. New cases genuinely commit, attest and
  merge source headers, retire via SDK deposit absorption or valid payout
  initialization, assert signed NFT burn of minus one and spent Order, then read
  fresh authenticated absence. An unchanged retired leaf in a later header is
  classified as fabricated; fresh eligible controls remain healthy and proof
  preparation refuses them. Full retired-ID CT stages and later-header removal
  remain unverified. These are emulator/installed-classifier results with
  synthetic transport/finality attestations, not live acceptance.
- Node-owned ChainSync transport: **51/51**, three files, 9.37s (23 monitor,
  17 acquired capture, 11 forced-order carriage tests). Exactly one `nextBlock`
  remains pending at a stable tip; a separate bounded heartbeat closes the whole
  session on failure. Exact intersection, parent/slot/height continuity, tip
  coherence, retained rollback height, deep rollback refusal and lifecycle
  draining are covered. The read-only reviewer identified two tip-height gaps;
  root fixed both and added regression cases. This monitor is still **unwired**:
  source/genesis/deployment authentication and canonical consumer ownership are
  not supplied by this transport module.
- Final node/tools typechecks, scoped ESLint and Prettier passed.

Two setup corrections are retained in the evidence. The new monitor fixture
initially mixed fake timers with real `node:timers/promises`; root corrected the
fixture to use real timers without increasing its bounds. The first broad
fault-proof command incorrectly set `NODE_ENV=emulator`, tripping a test-only
permit guard, and was interrupted. The isolated workspace also retained a stale
reference to the intentionally deleted `l1-services` launch path. Root copied
only the exact existing main test change into the candidate; no deleted config
was restored and no new assertion was relaxed. With normal Vitest environment,
the affected setup files passed **227/227**. The corrected full fault-proof suite
passed **4,707 tests**, with four existing skips across 411 files, in 1,070.38s.

Evidence is under `artifacts/event-history/parallel`: `root-uplc-*`,
`root-history-chain-*`, `builders-oversize-runtime-final-handoff.md`,
`watcher-retired-installed-handoff-20260923.md`, and
`audit-node-history-chain-monitor.md`. The working verification state is
`root-uplc-checkpoint-work-in-progress.json`. All six package builds subsequently passed. The frozen checkpoint is
`parallel/root-uplc-installed-checkpoint-identity.json`: 4,054 source files,
aggregate `67b3d0510d9946dbf2b33791f8bcef7913172e76df3ce2ee0ccde1ec9b944387`,
and 1,219 compiled files. The two new source-binding files separately passed
36 tests, node typecheck, scoped lint and formatting; they are not yet wired
into production entrypoints. Remaining original work includes production
source/genesis/hub binding, canonical origin/retirement/incarnation reconciliation,
all consumer fences, retired-ID full proof/removal, remaining recovery/adversarial
coverage, preservation-aware main integration, and one centrally coordinated
verified live deployment. No live service, deployment or reset was performed.
The full original goal remains active.

The in-progress source snapshot is `parallel/root-uplc-source-snapshot.json`: 4,052 files, 16 changed paths, aggregate `6f4755e2d1c1db96a9b3b8c41a65b86bf2921c3152304822ec042bc751a16798`. Root commands and intermediate results are in `parallel/root-uplc-checkpoint-commands.json`. Documentation links passed for 375 Markdown/MDX files; runbook currency, skill validation and main whitespace checks passed. This source identity does not promote the compiled outputs to success; the broad suite has since completed as recorded above.

## Bound node source sessions and native payout recovery — 2026-09-23

Shared SDK/history schemas stayed frozen. After the 4,054-file installed-runtime
checkpoint and six successful package builds, root opened disjoint edit windows:
builders own the two node-tools staging/case files plus a new FP test helper;
watcher owns only its history indexer test file; root owns the single test-support
export and every node source/persistence/interface change. The audit agent remains
read-only. No services, deployment, reset or main implementation integration ran.

Root added a lossless Shelley query-result pin, admitted manifest/network/endpoint
binding, and strict same-capture hub verification. Its private source wrappers
now authenticate each actual capture/follower socket before any ledger query or
ancestry callback. Capture requests both lists, both retention addresses and the
hub at one acquired point. Endpoint substitution, changed backend at the same
URL, handshake timeout, cancellation and wrapper-option replacement are checked;
a prior session's authentication cannot authorize a new socket. Existing slot
artifact digest semantics remain unchanged. The pin has no automatic adoption
path and still needs explicit production configuration and owner wiring.

The node-private transaction decoder preserves lossless values and signed mint,
canonical input/reference and withdrawal rosters, exact zero-withdrawal observer
pointers, output order and validity bounds. It retains explicit `spends`
disposition: collateral-only transactions cannot authenticate history observers.
Future reconciliation must also gate ordinary ledger effects on this disposition.
This decoder is not a canonical-origin or retirement proof and does not revalidate
a complete Cardano transaction. Existing forced-order carriage remains unchanged.
The bound follower now decodes the complete transaction roster before one owner
callback, rejects duplicate identities and refuses a malformed later transaction
without publishing a partial block. Primary-source review distinguished two
different encodings: the pinned Ogmios v7 Conway encoder always emits block
`transactions` (including `[]`), while it omits empty transaction `outputs`.
The decoder now accepts only that absent-as-empty output form; explicit null
still fails. Wire tests cover both validity dispositions and collateral-return
index zero. See the pinned
[block encoder](https://github.com/CardanoSolutions/ogmios/blob/b3a830a1bf2fbfd0ce1fd0fb238aee2d73d7cdbe/server/src/Ogmios/Data/Json/Conway.hs#L90-L106)
and [transaction body encoder](https://github.com/CardanoSolutions/ogmios/blob/b3a830a1bf2fbfd0ce1fd0fb238aee2d73d7cdbe/server/src/Ogmios/Data/Json/Conway.hs#L737-L773).
The watcher requires hash-bound CBOR and derives its outputs from that body;
read-only inspection found no analogous watcher change needed.
The first new parser run exposed an incorrect test expectation and an insertion-
order assumption; both were corrected using the pinned evaluator's reward-account
sort and the ledger's network/credential ordering. The primary definitions are
[AccountAddress](https://github.com/IntersectMBO/cardano-ledger/blob/master/libs/cardano-ledger-core/src/Cardano/Ledger/Address.hs)
and [Credential](https://github.com/IntersectMBO/cardano-ledger/blob/master/libs/cardano-ledger-core/src/Cardano/Ledger/Credential.hs).

Root checks: **117/117** across source, transaction decoder, ChainSync, acquired
capture, paired history adapter and existing forced-order observation tests,
10.19s, then **117/117**, 10.59s after the final type refactor and explicit
post-close response assertion. Node typecheck, scoped lint and formatting passed.
After complete-block decoding and the Ogmios v7 correction, the final six-file
regression passed **123/123**, 11.33s; node typecheck, lint and formatting passed
again. All six packages rebuilt successfully, and the built fault-proof export
check passed for both import and require. Shared SDK schemas stayed unchanged.
The named `test:tx-prep:sdk` gate also passed: **175 lucid-midgard + 615 SDK** tests. The additional post-close response
assertion passed its narrow test. Initial Effect error wrapping, literal widening,
redeemer-order expectation and one nullable raw-mint type error are retained in
logs rather than omitted from the record. Read-only audits found no concrete
regression within these explicitly limited helper scopes.

Watcher added native payout-initialization publication/restart and payout rollback
coverage, plus wrong Spend/other Withdraw retirement-pointer rejection with no
checkpoint/CAS mutation. Agent narrow checks passed 2 new and 13 existing cases;
root independently ran the entire file: **33/33**, 182.27s. These are synthetic
native transport/protected-runtime fixtures, not live or applied-validator
acceptance. Source is stable at the reported watcher handoff hash.

Builder full retired-ID proof/removal cases now pass separately for Deposit
(44.57s process) and Withdrawal (40.86s process). Each uses actual admission,
source commitment/DA attestation/mature merge, absorption or payout initialization,
fresh authenticated absence, then all four proof stages and exact child removal.
The fresh eligible parent reaches the existing exact-content refusal after real
init/01/02; its header/thread, original confirmed source and source settlement
remain. Removal transfers the child's 5,000,000 lovelace rent to its predecessor;
the assertion checks the full combined Value minus the burned child NFT. Root
independently recomputed all 22 signed receipts' hashes, fees, bytes/units and the
actual removal output's rent accounting. Maxima were 2,368 bytes, 3,561,832 memory,
1,349,038,828 steps and 500,000,000 lovelace fee (existing removal fee); maxima
may describe different transactions. Production limits were unchanged.

Fixture prover address/funding, proof-window and initial predecessor-Value
expectation failures remain in the logs. The builder agent was interrupted by
an automatic safety flag after its existing Withdrawal job had been launched;
read-only recovery confirmed successful terminal checks. Root's independent
full-file regression passed **10/10**, 317.70s. Its own 22 signed receipts also
passed independent CBOR/accounting verification (maximum 2,368 bytes, 3,543,562
memory and 1,343,425,689 steps). The named emulator gate passed **64 node + 587
fault-proof** tests, 355.95s and 470.27s respectively. This is still not live
acceptance or production-owner integration. An initial root
regression launch was interrupted before completion to correct its evidence-output
environment variable; that startup log is retained.

Evidence: `parallel/root-history-source-transaction-final.log`,
`root-history-bound-sessions-*`, `root-history-transaction-*`,
`root-history-late-genesis-response.log`, `root-watcher-native-payout-full.log`,
`watcher-native-payout-handoff-20260923.md`, and the three source/session/decoder
audit reports. Production owner/configuration, canonical admission/continuation/
retirement and incarnation reconciliation, consumer fencing, remaining failure
coverage, preservation-aware main integration and one centrally verified live
deployment remain open. The original goal is active.

Final checkpoint evidence is `parallel/root-source-sessions-checkpoint-identity.json`,
with 4,057 source files, the eleven owned source changes, compiled outputs and
unchanged runtime/blueprint pins. The Ogmios audit additionally records a future
integration requirement: explicitly constrain supported eras before a new hard
fork may acquire readiness; a `praos` block type alone is insufficient. No live
deployment or acceptance claim follows from this isolated checkpoint.

## Paired node transition projection — 2026-09-23

The previous goal turn made verified progress. Root then added three private node
files: `src/l1-event-history-transition.ts`, `src/l1-event-history-projection.ts`
and `tests/l1-event-history-transition.test.ts`. Shared SDK/history codecs,
contracts, blueprint, dependency pins and existing consumer interfaces stayed
unchanged. Watcher and consumer agents supplied read-only interface/integration
reviews; root retained all implementation ownership in this slice.

The transition interpreter requires an already admitted valid L1 transaction,
the complete pre-transaction list, the source manifest's contracts, its approved
slot clock and exact historical reference outputs. It separates initialization,
Order admission, filler insertion/promotion/reclamation, immutable-facts pointer
continuation and paired-observer retirement. Promotion is admission with no list
mint; continuation carries old/new location without creating admission or
retirement. Original Value excludes the authenticated list token and declared
structural ADA. Returned event facts and retirement witnesses use immutable CBOR
records; no mutable nested retirement witness escapes. Deposit absorption,
withdrawal payout initialization and invalid-withdrawal refund have distinct
retirement reasons. Current absence never fabricates one of these transitions.

The block projector stages both kinds against the same pre-transaction state,
then applies ledger effects in transaction order. Phase-2 failure applies only
collateral inputs/return. Exact tracked reference outputs take precedence over
archives; a previously spent reference or archive-only output within the complete
tracked address scope is refused. Earlier same-block retention publication is
available to a later admission. A whole-block failure leaves the caller's capture
unchanged. Before returning, the projector rechecks the exact hub and both complete
rooted lists at one new point; its ledger, point and output array are frozen.
Generation fencing, durable commit and rollback ancestry remain owner obligations.

Root verification: initial interpreter **15/15**, 6.89s; after immutable witness,
refund classification and clock validation, **139/139** across seven node files,
11.43s; initial paired projection **144/144**, 11.37s; final collateral/reference
coverage **147/147**, 11.40s. Final node typecheck and scoped lint passed. Initial
source typecheck caught a Lucid integer-schema typing mismatch and use of an
unexported SDK convenience function; both were fixed using public codecs and
pointer-independent CBOR records, without adding an SDK export. One later scoped
lint import-order error was fixed. Failed diagnostics remain in the evidence.

These are interpreter/projection fixtures with real deployment parameters and
synthetic transport/finality stand-ins, not applied validity or live acceptance.
They test both kinds in one transaction, exact external openings, original Value,
filler continuation/promotion/reclamation, retirement classification, whole-block
atomicity, source/parent mismatch, collateral-only effects and reference lifetime.
The earlier applied journey evidence remains separate. No package rebuild is
claimed for these still-unwired private modules; source typechecks/tests are the
current evidence. No main implementation integration or live operation occurred.

Builder follow-up has exclusive ownership of new positive-fixture paths
`tests/l1-event-history-projection-emulator.test.ts` and
`tests/helpers/history-projection-observations.ts`, subject to root review. It will
use ordinary accepted deposit/withdrawal transactions and provider equivalence,
with no changes to source APIs, production limits, package files or validators.
Its work is not covered by the 147-test result until independently verified.

Next integration requirements from `parallel/audit-node-ingestion-provenance-integration.md`:

- Persist branch-bound admission incarnations, pointer locations and retirement
  receipts separately from existing L2 event/status rows. Canonical retirement
  preserves origin and derived L2 effects; reverted admission does not.
- Establish actual initialization/ancestry coverage before claiming origins for
  current or already-retired events. The current manifest has an optional
  `steps.initProtocol.txHash` hint and the shared one-shot nonce, but no approved
  pre-init L1 point/nonce-live receipt. Kupo discovery and archives are not this
  authority. A private activation/bootstrap phase is still required.
- Integrate the existing recovery coordinator and outermost SQL generation fence;
  replace independent D/W scans with paired coverage. Gate selection, projection,
  barriers, candidates and confirmation by the captured incarnation/generation.
- Repair dependent accepted L2 transactions, journals and cache state after a
  reverted admission; deleting only the original deposit output is insufficient.
  Never publish Ready for an unresolved provenance or descendant-repair gap.

Evidence: `parallel/root-history-transition-*`, `root-history-projection-*`,
`audit-node-history-transition-decoder.md`, `audit-node-history-block-projection.md`,
`watcher-semantic-decoder-interfaces-20260923.md`. Durable provenance, production
owner/consumer integration, remaining adversarial/recovery coverage, preservation-
aware main integration and one verified live deployment remain open. The complete
original goal is active.


## Published positive projection journey and provenance staging — 2026-09-23

User priorities remain fixed: stable shared interfaces, one complete successful
Deposit/Withdrawal journey before extending failures, narrow edit checks and
broader checks at stable checkpoints, and one centrally coordinated verified live
deployment. The original full objective remains active. All implementation in this
section is in the isolated candidate; main implementation and its 239 dirty entries
are preserved. No deployment, live acceptance, reset or commit occurred.

Root independently passed the builder's manifest-bound positive admission fixture:
**1/1**, 17.925s case / 23.29s process. It accepts two ordinary deposits, guarantees
an existing Order pointer continuation, then admits an owner-signed withdrawal.
Exact projected/provider digests match after each transaction. Root independently
verified all three signed CBOR hashes, fees, bytes and execution units against the
unchanged manifest limits. This admission fixture does not establish L2 withdrawal
eligibility. Its source checkpoint has 4062 files, aggregate
`2d1cc4b93b6b8d511eae513b777dc679b0a4b8f7eea12daf596f72a716ea654d`;
all previous 4057 source paths matched the preceding checkpoint.

The builder then owned only two new lifecycle test/helper files and additive
observation utilities. Root retained the existing shared node test harness and
added an explicit published-fixture identity/cosigner seam. Commit, recovery,
merge, command, barrier and speculative helpers now receive the supplied admitted
manifest identity and real fixture configuration; existing derived fixtures retain
their defaults. The DA test helper accepts actual manifest ID/network/committee
keys/threshold and the exact deployment-info artifact hash. Synthetic transport
peer identities remain test-only. No production SDK/schema/export changed.
The read-only audit caught missing barrier/speculative config propagation; root
fixed both and passed fixture configuration through confirmation as well. Initial
root typecheck caught missing Effect service tags; using the service's real
`make` constructor fixed them without casts.

The new complete positive fixture uses one genuinely published deployment, its
actual DA cosigner, ordinary operator onboarding and the existing node pipeline.
It performs Deposit admission, real node commitment/attestation/mature merge,
reserve absorption, withdrawal of the real deposited L2 output, valid withdrawal
classification, another real commitment/attestation/mature merge, payout
initialization, funding and conclusion. Exact historical confirmed/settlement
references are captured before submission. The observer follows accepted signed
bytes and the pipeline's own confirmations without changing its clock or bytes.
Both complete rooted history snapshots match provider state throughout. Retirement
receipts preserve original facts/Value and distinguish absorption from payout
initialization; funding and conclusion create no false list transition.

Agent lifecycle result: **1/1**, 43.409s case / 49.28s process. Its unchanged admission
regression also passed **1/1**, 17.720s / 23.00s; final node noEmit and scoped
lint/format passed. Initial lifecycle failure correctly required the verified
deployment marker; the adapter now takes it and L1-finality settings from the same
verified manifest. A subsequent test-only absolute-slot/delta mismatch was corrected
to the published clock's `awaitLedgerTime`; intermediate evidence is retained.
No protocol maturity or production limit was changed.

Root independently ran the complete published lifecycle plus both existing
merge/payout tests: **3/3**, two files, **108.97s**. The new lifecycle case took
40.953s. Root separately parsed and verified **22/22** signed receipts: hashes,
fees, byte counts, total execution budgets and every projected/provider digest
pair, plus Deposit/Withdrawal/payout NFT mint-and-burn conservation. Root maxima were **2011 bytes**, **3,610,846 memory units** and
**1,375,894,470 CPU steps**, within unchanged 16384-byte / 16,500,000-memory /
10,000,000,000-step limits. The four semantic transitions are Deposit admission,
Deposit absorption, Withdrawal admission and Withdrawal payout initialization.
This closes a complete applied positive projection journey; canonical live-source
transport, durable provenance integration, refund/reclaim and rollback remain
separate gates.

Root also added private `l1-event-history-provenance.ts` plus its focused tests.
Immutable incarnation identity includes source binding, kind, event ID and admission
output. Canonical placement is separate: continuation changes location only,
retirement preserves canonical origin, reverted admission remains as an ineligible
orphan, identical transaction re-inclusion reuses identity, and a different
admission for the same event ID uses another incarnation. Retired canonical IDs
remain reserved. One pre-block before-image covers same-block admission,
continuation and retirement; rollback checks exact postimages and stages all
replacements before returning. Nested records are copied/frozen. These are L1
provenance fields only; the model cannot overwrite L2 classification state.

Focused provenance checks: **11/11**, 3.60s process; node noEmit and scoped ESLint
passed. The initial typecheck caught a test attempting to mutate a readonly alias;
the test now retains the actual mutable caller-owned reference and still verifies
that mutation cannot change the frozen result. Read-only review found no concrete
model defect under its stated validated-transition and complete-prestate
preconditions. It remains a staging model, not persistence or canonical authority.
The required node emulator suite then passed **64/64 across eight files**,
**557.15s**, exit 0. This includes initialization, operator lifecycle, submission,
commit selection, confirmation journals, recovery/invalidation, merge/payout and
reserve/payout builders. Required docs links passed for **397 Markdown/MDX files**;
`git diff --check` and final six-file formatting check passed. No jobs remain at
this checkpoint.

Durable integration decisions retained from the read-only audits:

- Four private tables: source-bound cursor, block applications/receipts, complete
  scoped live UTxOs and immutable incarnations with reversible canonical placement.
  Add the already-created authority table to migration completeness diagnostics.
- Keep immutable ledger/ordered-transition receipts separate from application
  undo. Reapplying a reverted block can have an orphan before-image where the
  first application had no row. Use monotone application revisions, exact cursor
  and parent-application links, and equality checks for immutable same-hash data.
- Preparation occurs outside SQL; authority generation is the first SQL lock,
  followed by exact cursor revision/head CAS. Persist both lists, UTxO deltas and
  all semantic receipts atomically. Do not infer retirement from current absence.
- Origin authority still requires source-admitted initialization and contiguous
  complete-block coverage. The optional manifest init hash is discovery only;
  shared nonce and exact deployment activation must be re-admitted. Pre-init
  retained data means an invented empty five-address snapshot is invalid.
- L1 rollback must reconcile dependent L2 descendants/journals/cache and candidate
  incarnation associations. Do not restore old full D/W rows over later L2
  classifications or publish Ready with unresolved repair.

Evidence lives under `artifacts/event-history/parallel/`: builder lifecycle
handoff/command manifest, independent root positive/lifecycle evidence and signed
receipt verifiers, root harness/provenance logs, and the read-only durable-schema,
repeated-block-application, provenance-staging and runtime-override reviews. The
existing full-suite/compiler/runtime evidence is retained; none is relabeled as
verification of unwired production behavior. Final main integration and live
acceptance remain open.


## Real refund/reclamation and journal storage foundation — 2026-09-23

After the verified published positive lifecycle checkpoint, the builder owned only
new `l1-event-history-projection-retention-emulator.test.ts` and
`l1-event-history-projection-refund-emulator.test.ts`. The previous lifecycle,
admission, observation helper and shared APIs remained unchanged. Root retained
schemas, codecs, independent verification and every production/live decision.

The external Deposit scenario prepublishes an ordinary 600-byte L2 datum, admits
it by authentic reference, commits/attests/matures/merges through the real node,
absorbs the deposit and reclaims the retained data with its key owner. The reserve
and deposited L2 output remain unchanged by reclaim. The invalid Withdrawal
scenario starts with a real settled Deposit, then an owner-signed request names
that real L2 output with the wrong Value. Actual node classification and mature
merge establish `IncorrectWithdrawalValue`; the refund builder consumes that exact
settlement verdict/membership, pays original assets and the committed refund datum,
and the owner then reclaims external retained data. The valid L2 output and reserve
remain intact. Reclamation emits no fabricated list retirement.

Both agent scenarios passed on their first emulator run; final scoped type/lint/
format checks passed. An initial test-only TypeScript union-narrowing error was
preserved and corrected without changing the expected validity assertion. Root
read both files and independently reran them with separate evidence: external
Deposit **1/1**, 29.884s case / **35.76s** process; invalid Withdrawal **1/1**,
41.068s case / **46.98s** process. All **13 + 23** accepted signed receipts had
matching projected/provider digests. Root separately verified signed hashes,
fees, bytes, execution budgets, history NFT mint/burn conservation, strict prior
publication/admission-reference ordering, and later exact retained-input spending;
the refund's signed output was also compared with the checked refund receipt.
Root maxima across the two cases were **3394 bytes**, **3,046,746 memory units**,
**1,239,322,040 CPU steps** and **1,110,661 lovelace fee**, within unchanged limits.
Synthetic observation labels remain explicit; these are applied emulator journeys,
not live-source or durable provenance acceptance.

Root added the initial private storage foundation: cursor, block applications,
live scoped outputs and incarnation tables in the unshipped candidate schema;
matching completeness/index names; and the previously missing authority-table
name. Cursor retains anchor point/digest and monotone revision. Each application
has its own undo record and exact parent application, while a repeated block's
immutable ledger receipt must be compared by the upcoming writer. Canonical
block/height and event-ID/key uniqueness are enforced. The writer must still
verify exact cursor/head/parent ancestry, receipt/digest/index consistency and
source/generation authority; SQL uniqueness alone does not establish these facts.
No deployed database or deployment state was reset. Only the standard test
setup recreated its named ephemeral shard after the candidate schema checksum
changed, retaining normal production checksum refusal.

`eventHistoryJournalCodec.ts` round-trips exact quantities using canonical decimal
strings, orders Value entries deterministically, rejects unknown/malformed fields
and contradictory datum/placement shapes, and deeply freezes decoded records.
Hex/plus/leading-zero/whitespace quantity strings are refused. These are typed
local-storage codecs, not substitutes for ledger or source authentication.
Root checks: first migration/codec pass **23/23**, 7.69s; refined codecs **18/18**,
3.62s; final schema+codec+locking **29/29**, **7.61s**, exit0. Node noEmit and scoped
lint passed. Read-only review identified the forthcoming writer boundaries and
mutable decoded records; root tightened semantic shape and freezing before the
final checks. No append/rollback writer or durable read path exists yet, so this
foundation does not close persistence/recovery acceptance.

Next root implementation remains the four-table writer: authenticated replay seed,
coherent cursor/output/incarnation load, exact revision-CAS append, duplicate-delivery
idempotence, application-specific rollback/undo, retained orphan audit records and
atomic L2 materialization/repair under the existing outermost authority fence.
Network/reference resolution and projection stay outside SQL locks. Reapplying a
rolled-back block gets a new monotone application revision; retries of the same
current committed application reuse it. Parent links include application revision,
and rollback to the retained anchor must match its saved snapshot digest. Once
that is proven, connect source-admitted initialization/continuous coverage and
all ingestion/selection/projection/candidate/journal/cache consumers. Preserve the
separate L2-correction authority and remain recovering for unresolved descendant
repair. Main integration and one centrally verified live deployment remain open.

Evidence: root `root-history-journal-*` and independent retention/refund logs,
`verify-root-history-reclaim-receipts.mjs` and its two verification reports; builder
retention/refund handoff and exact command manifest; read-only codec/schema review.
The earlier **64/64** broad node emulator pass belongs to the preceding published
harness checkpoint; the later schema delta is covered by the final migration/
codec checks and independent applied retention/refund runs. The original full goal
is active and incomplete.

## Durable journal and pointer cutoff checkpoint — 2026-09-23

Root implemented the candidate-only private `eventHistoryJournal.ts` adapter and
real PostgreSQL tests. The existing authority transaction now supplies a private
recovery capability; journal seed/append/undo compose inside that same outer
transaction rather than nesting another fence. Owner, manifest, generation, live
lease and recovering state remain checked by the existing authority. Ready-mode
append and the production source owner are not yet connected.

The journal reload holds a shared cursor lock while checking canonical application
ancestry, exact parent application revisions, receipt fields against indexed
columns, stored hashes/records, strict paired capture decoding and live incarnation
facts. Append compares the exact cursor revision/head and commits net UTxO images,
incarnation changes and caller-supplied bounded L2 SQL materialization atomically.
Current-head duplicate delivery skips the callback; reapplication after rollback
receives a new monotone revision and distinct undo images while retaining the same
immutable ledger receipt. Undo requires the exact canonical head, verifies all
postimages, restores pointer/retirement placement and retains new admissions as
orphans. It does not restore L2 classification columns. A callback failure or
expired lease rolls back the entire SQL mutation. Rollback below the replay anchor
refuses; saved cursors and origin-receipt digests remain recovery material, not L1
authority. The source owner must retain/re-admit the exact origin evidence and
complete descendant repair before readiness.

Read-only agent review caught mutable staged images, incomplete live-fact comparison
and missing receipt/index binding; root fixed these. An initial attempt to clone
whole SDK captures hit nonempty Buffer freezing; staging now copies/freezes only
plain ledger/transition/undo data and append reconstructs the SDK capture. Initial
SQL seed failed because the driver encoded a string parameter inferred as JSONB;
root used the existing explicit TEXT-to-JSONB pattern. All failed diagnostics are
retained. No production schema or public SDK/transition/projection ABI changed in
this slice. The new storage tests use real SQL and strict decoded list structures
with model block/source inputs; they are not applied-ledger or live-source claims.

Final root checkpoint: **217/217 tests across 15 files, 58.93s, exit 0**. It covers
journal (12 cases), authority/recovery, codecs/provenance, source/chain/transaction/
transition/snapshot decoding, submission persistence/transport, mempool cache and
migration/locking. Journal cases include concurrent sibling writers, late lease
expiry, final-L2-mutation failure, both-kind orphan/reinclusion, pointer and explicit
retirement reversal, same-block admission/retirement, restart reads, corruption,
anchor refusal and coherent reads behind uncommitted append. Earlier narrow runs
were 8/8 then 56/56; final node noEmit and scoped ESLint pass. Logs are
`root-history-journal-writer-*`; these do not replace remaining full acceptance.

Watcher agent exclusively changed `src/indexers/user-event-indexer.ts` and its
`tests/indexers/user-event-history.test.ts`. Header cutoff now uses the unique valid
nonce-consuming admission transaction from the authenticated origin block, while
current transaction/outref remain pointer navigation. Six new both-kind cases plus
four existing cutoff/rollback cases passed; root independently reran all ten:
**10 passed, 29 filtered, 24.61s**. The fix preserves shared schemas and opaque
runtime interfaces. The agent handoff includes the exact patch, commands and hashes.

Builder agent added only `l1-event-history-projection-native-assets-emulator.test.ts`
after the complete ADA journeys. It mints ordinary alpha/beta assets (5/7), deposits
12 ADA plus those assets, performs real node settlement/absorption, withdraws the
actual L2 Value, and settles a valid withdrawal. Payout initialization then fails
local evaluation (`Withdraw[1]` in the diagnostic run). Both attempts failed at that
same stage; no assertion, asset name/amount or production limit was weakened.
Nineteen accepted signed receipts and the nested error were preserved. Root
independently parsed all 19 for signed hashes, bytes, fees, budgets, matching paired
snapshot observations and partial NFT conservation. This is partial journey
evidence, not payout acceptance.

Pure-byte investigation confirmed actual admitted event serialization and classifier
bytes produce the saved settlement root exactly; reconstructed payout Value bytes
also match. A reader sorted-field serialization discrepancy exists, but classification
normalizes it, so it has not been established as the payout failure's cause. Next
root-owned diagnosis needs the failed final retirement context/output/index evidence.
The new fixture remains an honest failing acceptance gate. Static checks passed.

All implementation edits remain in the isolated candidate. Main still has 239 dirty
entries; unrelated work, dependency pins, validators and blueprint are preserved.
No deployment, reset or live operation occurred. Remaining work includes native-asset
payout diagnosis, production source/bootstrap and origin evidence retention,
ready-mode ingestion plus all eligibility/candidate/ledger/cache consumers, bounded
L1 rollback/L2 correction repair, the remaining adversarial matrix, preservation-aware
main integration, and one centrally verified deployment reused for live acceptance.
The full original goal remains active and incomplete.

## Ordered Plutus data and native-asset payout — 2026-09-23

Root isolated the native-asset payout failure using the exact failed evaluator
request, all nine resolved inputs/reference inputs, and a disposable verbose
Aiken build. The pinned normal blueprint and applied parameters reproduce the
failed observer hash. A condition-only diagnostic copy identifies the precise
false condition: the actual payout datum is unequal to the consumed withdrawal
body. All other retirement conditions pass. Lucid's recursive canonicalization
reversed the ordered Plutus token map from `alpha`, `beta` to `beta`, `alpha`.
Typed decode/re-encode comparisons had hidden that difference. The normal
validator, seven applied parameters, token names/amounts and production limits
remain unchanged. Diagnostic build/replay is not itself ledger acceptance.

The tracked Lucid patch now canonicalizes ledger containers while preserving
inline datums, witness datums and redeemer data. The same helper covers delayed
contexts, evaluation, completion and explicit canonical serialization; fixed-point
fingerprints distinguish ordered data changes. Minimum ADA uses the actual
preserved datum encoding. Root also changed SDK output matching to preserve
Plutus map order while accepting equivalent CBOR encodings. No shared API or
schema changed. Lock resolution changes only the Lucid patch hash. Installation
used the frozen lock without editing installed dependencies.

The builder agent exclusively added two SDK regression files. Six ESM/CommonJS
cases cover ordinary, explicit canonical and delayed completion, nested ordered
inline/witness/redeemer maps, reference inputs, metadata, minimum ADA, final real
UPLC evaluation and actual emulator acceptance. Explicit canonical serialization
and canonical/delayed signed submission preserve the signed body hash. Root
independently ran those six plus two output-matcher cases: **8/8, 5.55s**. Existing
completion/cache/collateral regressions also passed **14/14, 1.74s**. The read-only
agent verified identical module helpers and coverage of every optional Conway body
field; an auxiliary-data ownership concern was withdrawn after confirming CML's
ownership-transfer ABI. Legacy redeemer-array and uncommon optional-body-field
runtime coverage remain limited; this is not a claim of exhaustive library audit.

The complete applied native-asset journey now passes **1/1, 47.19s**: ordinary
native mint, 12 ADA plus alpha=5/beta=7 deposit, actual node L2 commitment,
attestation/mature merge, reserve absorption, owner-signed withdrawal, valid
classification, second commitment/mature merge, payout initialization, funding and
conclusion. Root independently parsed all **23 accepted signed receipts** for
hashes, exact bytes/fees/execution units, unchanged limits, matching projection and
provider snapshots, both event NFT burns and full native-asset continuity through
the final owner payout. This uses synthetic observation transport, not live L1
source acceptance. The later output-matcher correction is included in the final
emulator checkpoint below.

Final required SDK command passed **175 Lucid Midgard + 623 SDK tests**; SDK build,
noEmit, scoped ESLint and formatting passed. The required node preparation command
passed **123 node + 51 tools tests** before the output-matcher-only correction.
The first broad emulator run passed **64 node cases across 8 files, 577.61s**,
then its fault-proof phase exposed an encoding-size regression: the preserved
redeemer encoding cost 278 instead of the pinned 276 bytes per proof level. Root
reproduced that exact assertion separately and stopped the known-failing broad run
(exit 143) before replacing its installed dependency; no full fault-proof pass is
claimed for that run. The patch now keeps compact canonical redeemer encoding
only when exact CML JSON strings prove the ordered Plutus value unchanged. It
retains original data when canonicalization changes map order, and retains inline
and witness datums exactly. No test assertion, size limit or execution reserve was
changed. The refined patch passes **22/22 focused cases, 5.82s**, and the full
maximum-proof-fit file **6/6, 16.92s**, including the unchanged 276-byte assertion.
The refined complete native-asset journey passed **1/1, 53.76s**; root independently
verified all **23 signed receipts** again. Refined required SDK suites passed
**175 Lucid Midgard + 623 SDK tests**. Refined node preparation passed **123 node + 51 tools tests**. The required
broad emulator rerun passed **64 node tests across 8 files and 587 fault-proof
tests across 116 files**, exit 0; the latter took 475.45s. All 4076 frozen source
files were rehashed unchanged. This verifies the shared completion/SDK checkpoint;
subsequent additive bootstrap work remains outside that checkpoint and the full
original goal is still incomplete. Evidence is under
`artifacts/event-history/parallel/root-lucid-preservation-*`,
`root-native-assets-preservation-*`, the exact trace artifacts, and the builder and
read-only audit handoffs. Earlier failed attempts remain preserved.

Implementation remains isolated from the dirty main tree. No live deployment or
reset occurred. Remaining original work is production source/bootstrap and retained
origin authority, serialized journal ownership, incarnation-aware ingestion and
selection, rollback/L2-descendant repair, remaining adversarial acceptance,
preservation-aware main integration, and one verified deployment reused for live
acceptance. The full goal remains active and incomplete.

### 2026-09-23 initialization and exact-point capture checkpoint

Progress; original objective remains active/incomplete. Root retained all schema,
source, persistence and integration ownership. Builder agent completed only new
`demo/midgard-node/tests/l1-event-history-initialization-emulator.test.ts`; audit
agent performed read-only initialization/restart reviews; no deployments or live
mutations were delegated or performed. Main's 239 dirty entries remain preserved.

Root added complete pre-initialization five-address replay and extracted shared
whole-block staging without changing the initialized projection interface.
Atomic activation requires both lists, the exact shared nonce, and hub plus
correction-lock mint. Exact-point acquisition authenticates its actual socket and
refuses an unavailable/substituted parent; no tip fallback. The initialized reader
still requires the real hub and both list roots. Root protection prohibits honest
same-block post-initialization admissions: model fixtures now use a later valid
time, and the real fixture actually waits protection before public D/W admission.

Independent root verification: 93/93 across five files, 37.17s, including applied
initialization 1/1, 18.584s; node noEmit and scoped lint PASS. Initial lint reported
only import ordering, repaired and rerun. Wire/model tests cover exact capture,
source binding, collateral effects, complete retention dust, all-or-nothing replay,
and source refusal. Agent's separate applied run also passed 1/1. Eight signed
receipts and eight equal provider/projection snapshots retained in
`artifacts/event-history/parallel/root-history-initialization-evidence.json`.
Source snapshot `root-history-bootstrap-source.json` pins 4080 files; exact
commands/results in `root-history-bootstrap-commands.json`. These are emulator
and model observations, not live chain authority.

Remaining: durable full origin material with journal seed; source-derived replay
when activation parent state is too old; startup/restart and anchor rollback;
production owner + ingestion/selection/reconciliation; preservation-aware main
integration; one centrally coordinated verified deployment and full live acceptance.
The exact-point path is only a recent-state fast path: Ogmios explicitly bounds
historical local-state-query availability. No archived hash or manifest locator
has been promoted into canonical source authority.

### 2026-09-23 durable initialization origin checkpoint

Root completed origin evidence persistence with bootstrap. Initialization returns
canonical lossless replay material: bound source/genesis/manifest, exact nonce,
complete captured parent, whole normalized block and original activation index,
used historical references, and resulting snapshot digest. The journal stores
this text and its SHA atomically with seed state, verifies integrity on load and
compares it during cursor fencing. Receipt strings are copied with the validated
seed snapshot; the read-only audit found and confirmed that consistency fix.
Receipts retain normalized interpreter inputs, not full original Ogmios envelopes
or signed transaction witnesses. Source re-admission remains an owner obligation.

The applied initialization fixture now stores the actual accepted activation
capture under a real Postgres recovery transaction and reloads identical origin
material. The first mixed fixture run exposed a disposable worker DB authority
left by the prior journal test; 26 checks passed, one failed. That log is retained.
Fixture isolation now clears only its test history state before its fresh emulator
deployment, consistent with the existing test suite. No live state was reset.

Final root affected suite: **234/234, 21 files, 270.02s**, terminal exit 0. This
includes initialization, source/transport, submission persistence, authority,
recovery, provenance/journal, complete deposit/withdrawal settlement, native payout,
invalid-withdrawal refund and external retention/reclaim. Node noEmit, scoped
lint/format, main git diff check, and docs links **416 Markdown/MDX** pass. Root
independently parsed all eight initialization/admission signed receipts (exact
inputs/references/outputs, bytes, fees and execution budgets) and verified the
persisted origin digest. The first disposable verifier assumed CBOR input-set
iteration order equaled ledger outref order; its diagnostic is retained and the
verifier now applies the specified ledger ordering. No transaction, assertion,
production limit or test timeout was weakened.

Evidence: `root-history-origin-checkpoint-identity.json`, source snapshot of
**4080 unchanged files**, exact `root-history-origin-commands.json`, final applied
`root-history-origin-initialization-final-evidence.json` and independent receipt
verification under `artifacts/event-history/parallel/`. Main implementation is
still untouched; its 239 dirty entries and unrelated work remain preserved.

Architecture reassessment supersedes the earlier full-genesis prerequisite:
valid source-admitted atomic activation proves empty authenticated lists despite
unrelated preexisting dust/retention. A distinct authenticated-list replay state
can begin there, verify external creator-body preimages against exact referenced
outrefs, retain all historical origins, and join a genuinely complete fresh
five-address capture at one admitted point. A partial cache must never be labeled
an acquired complete ledger. Previously source-verified node-owned checkpoints
can support restart under an explicit storage-integrity contract; untrusted
imported archives cannot supply new L1 authority. See read-only
`audit-node-activation-replay-reassessment.md`. Root will implement this narrower
production route instead of making routine startup depend on genesis replay.
Remaining: runtime source owner, this replay/capture join, rollback through its
anchor with preserved audit history, producer/ingestion/reconciliation integration,
main integration, and one verified deployment for centrally coordinated live
acceptance. Original objective remains active and incomplete.

Final documentation link rerun after the audit report: **417 Markdown/MDX PASS**; main `git diff --check` remains clean. No running root or agent jobs at this checkpoint.

### 2026-09-23 authenticated replay and bounded persistence

The private list-replay interface is frozen: begin/advance return immutable
state, transitions and the exact per-block receipt; join requires a genuine
complete five-address capture at the identical admitted point. Replay tracks
only authenticated nodes and the hub, preserves retired origins, and verifies
historical reference outputs against the original creating BODY hash and the
observing transaction's actual reference roster. It never promotes a partial
retention cache into an acquired ledger snapshot.

The delegated applied fixture publishes actual external Deposit and Withdrawal
payloads before activation, admits both later through the public builders, and
joins the current full capture. Root independently verified nine accepted signed
transactions, 464 creator bodies and seven replay receipts for both the agent and
root runs. These fixtures use synthetic transport hashes and contiguous replay
heights; they prove applied transaction/reference composition, not live ancestry.
The stable replay checkpoint passed **250/250, 23 files, 301.17s**, including
complete ADA/native deposit-through-withdrawal settlement and existing refund and
reclaim coverage. Node noEmit, scoped lint and formatting passed.

Root added bounded recovery persistence and ChainSync backpressure. A recovery
handle can persist separately committed chunks while keeping producers and caches
closed. Persistence and completion share a semaphore so writes cannot interleave
between final repair, cache reload and Ready publication. The follower awaits
consumer acknowledgements while its heartbeat continues; source loss still
revokes promptly and observes late consumer rejection. The production owner must
track, cancel and join those consumer lifetimes and compare tip to its committed
frontier before claiming catch-up.

The journal now requires the full retained receipt range for authenticated-list
origins. Each immutable row records the exact lossless receipt and an incrementally
verified digest/count. Restrictive parent and activation links retain intermediate
chunks. Final seed/load checks bounded endpoints against the exact activation and
anchor, rather than rescanning all history under the recovery SQL lock. This is
node-owned recovery material under an explicit local storage-integrity contract;
imported archives do not become L1 authority.

The builder's applied persistence extension passed **1/1**: seven separately
committed receipts, a separate seed transaction, then exact origin/capture and
both incarnation reloads. The read-only audit caught native JSON parsing of large
integers and an omitted migration inventory entry; both are fixed. Root's focused
SQL tests passed **32/32** across journal and migration suites, including exact
9007199254740993 quantity persistence, missing-range refusal, immutable replay,
corruption, single-block origins, restrictive links and the applied database
frontier constraint. Earlier recovery/source/journal checks passed **91/91**;
no production limits or test timeouts changed. Lint-only import-order and
Promise-rejection diagnostics were corrected and their first logs retained.

Final integrated checkpoint verification ran against
`root-history-replay-persistence-source.json` (4087 files, SHA256
`67f4782a3907325934f7c7576ee613588af1702bc7b67c81544936821c270972`).
Evidence and exact commands remain in `artifacts/event-history/parallel/`.
Main implementation and lockfile remain preserved; 240 dirty entries were
observed on this continuation. No commit, deployment, live reset or live acceptance
was performed. Runtime source-owner wiring, transport/activation lookup, trusted
checkpoint restart, below-anchor rollback and L2 repair, consumer integration,
preservation-aware main integration and the single coordinated verified deployment
remain open. The original goal is active and incomplete.

Final integrated result: **275/275, 25 files, 308.64s**, terminal exit 0.
This reran the real preactivation external-payload replay with seven individual
receipt commits and final journal seed/load, all positive deposit/withdrawal
settlement journeys, existing negative paths, source/recovery/journal checks and
migration suites. Independent root parsing of the final evidence verified nine
signed transactions, 464 creator bodies, seven replay receipt digests and exact
persisted origin/capture/incarnations. Final node noEmit, scoped lint/format,
**426 Markdown/MDX links** and main whitespace checks passed. Source remained
identical to the frozen 4087-file checkpoint. Named node reliability checks also passed: **123 node tests + 51 tools tests**,
terminal exit 0. The final identity is
`root-history-replay-persistence-checkpoint-identity.json`; no root or delegated
jobs remain running. Production integration and live acceptance remain incomplete.

## Source owner composition in progress — 2026-09-23

- Shared SDK/list replay interfaces remain fixed. Root owns new private
  `l1-event-history-transport.ts` and `services/event-history-owner.ts` in the
  isolated candidate; no production listener integration or live action yet.
- Configured Kupo/Ogmios transports locate the immutable hub's candidate
  activation predecessor and preserve hash-bound creating-body bytes. Complete
  bound replay still authenticates activation; navigation is not authority.
  Cancellation/deadline checks include an idle HTTP stream whose cancellation
  cleanup never settles. Final narrow transport/reference result: 32/32 across
  two files, 8.28s, exit 0; logs under `artifacts/event-history/parallel`.
- Scoped owner composes real capture/follower/replay/journal/recovery, bounded
  receipt commits, forward projection, retained-head undo, fresh restart
  intersection, cache publication and whole-producer lifetime fencing. Mandatory
  SQL reconciliation must succeed before readiness; it has no permissive default.
  Close revokes recovery before joining callbacks. Startup lease renewal requires
  a fresh authenticated capture and stops when follower heartbeats take over.
- Builder agent owns only the new applied owner fixture and private transport
  helper. Watcher agent reviews lifecycle/transport read-only; third agent audits
  consumer/L2 repair gaps read-only. Exact reports are retained. Root independently
  verifies integration after those edits stabilize; current owner applied tests
  are not yet complete.
- Production materialization remains open: exact incarnation associations,
  accepted-transaction revalidation with retained CEK material, descendant repair,
  selection/submission/confirmation fences and published-header correction.
  Canonical retirement preserves credited L2 funds. Below-anchor rollback and
  explicit approved genesis configuration also remain open, along with main
  integration and one centrally coordinated verified live deployment.

### Verified source-owner checkpoint

Root independently ran 294/294 tests across 27 files (396.35s), including the
new applied source-owner journey, all preceding history/provenance/recovery and
submission checks, native-asset settlement, refund/reclaim and schema checks.
The applied journey uses one real emulator deployment, records its accepted
transactions, then exposes them through a synthetic transport to the actual
source/capture/replay/journal/recovery implementations. This is not live ancestry
or production consumer materialization evidence. The test-only SQL probe also
proves that a failed dependent repair rolls back its write, preserves the exact
checkpoint, refuses readiness/producers and closes to suspended authority.

`root-history-source-owner-source.json` pins 4092 candidate files, aggregate
`e1122da0fc9b654d6bef16062a6a4785679f53c3731d39fd7f861c669da910f8`
(sorted path, NUL, file SHA256, LF). Only five new candidate files were added;
all files from the previous 4087-file checkpoint are unchanged. Root noEmit,
five-file lint and format checks passed. The blueprint and main lock remain
unchanged. Independent parsing of both agent and root evidence verifies 25
signed lifecycle receipts, 488 creating transactions, 34 synthetic blocks,
10 retained replay receipts and 23 canonical journal applications with exact
receipt/undo digests, pointer continuation, both retirement reasons, restart
identity and refused-repair rollback. Exact command/evidence reports are retained
under `artifacts/event-history/parallel/root-history-source-owner-*`.

A read-only conversion audit identified an additional consumer gap. Root's pure
encoding comparison confirms that decoding then typed `Data.to` reencoding can
sort a raw deposit datum map before an outer preserving serializer runs. New
retained-event conversion must extract original raw CBOR field bytes. The live
SDK adapter and node provenance capture need the same review/fix, with applied
ordered-datum coverage; the encoding diagnostic alone is not ledger acceptance.
This remains open with production listener/configuration, incarnation association,
selection/submission/confirmation fences, L2 descendant repair, below-anchor
recovery, preservation-aware main integration and central live acceptance.

Required `test:tx-prep:node` also passed: 123 node checks (28.64s) and 51 tools
checks (14.93s), exit 0. Documentation verification passed 433 Markdown/MDX files;
main whitespace check passed. Final identity is
`root-history-source-owner-checkpoint-identity.json`; there are no pending jobs.
The main workspace retains its 242 dirty entries. No deployment, reset or main
implementation copy occurred in this checkpoint; the original goal remains active.

### Raw existing Data consumer checkpoint

Shared raw interfaces now preserve observed map order and repeated pairs without
changing event IDs or the onchain schema. Core adds validated constructor-field
replacement. SDK capture returns the original payload, facts and opening CBOR;
its raw opening preflight hashes those fields directly. Readers use the captured
raw evidence rather than mutable typed views. Node provenance and journal live
coverage compare the same raw facts; deposit projection and withdrawal datum
columns extract original fields. Watcher indexing, pointer continuation,
retirement, payout/refund observation and replay conversion retain raw fields.

Root's first integrated owner run exposed a journal comparison still using typed
facts reencoding, which kept the owner closed. That comparison was corrected;
synthetic journal fixtures now derive the same facts from their constructed raw
output. The owner test also surfaces a stopped follower directly instead of
masking its error behind the unchanged 20-second convergence timeout. Failed
runs remain in `root-raw-node-narrow.log` and `root-raw-node-narrow-2.log`; no
limits or timeout were raised. The subsequent focused journal suite passed
21/21, and the independently rerun source-owner journey passed with exact restart
and failed-repair rollback evidence.

The new applied fixture accepts both inline and external Deposit maps ordered
2,1 and repeated-key maps 2,1,2 under normal production parameters. Five actual
admissions force four raw Order continuations. Root independently reran it and
parsed both agent and root evidence: eight signed receipts, two external
publications, original five-million-lovelace deposits excluding three-million
structural ADA and the NFT, exact raw payload/facts/opening/info and L2 datum.
Synthetic transport points/genesis and pure conversion remain explicit limits;
this fixture alone does not prove durable L2 correction or live acceptance.

Completed checks: core raw codec 15/15; full SDK 628/628 across 75 files;
root watcher integration 122/122 across eight files (221.57s); core/SDK/node/
watcher noEmit; scoped 28-file lint and format. The broad node run finished
295 passed / 2 failed across 29 files (383.78s). Both failures were obsolete
typed facts expectations; root changed those and the third related fixture
to compare fields from actual datum bytes, then all three passed (68.26s).
Production source remained identical after the broad run. A second complete
29-file pass is not claimed. Required reliability passed 123 node and 51 tools
checks. Source snapshot `root-raw-source.json` pins 4094 files (28 changed),
aggregate
`74b69b886fff19326ec70bd275b3672d493542c01b85e8b2430a1124ec2a4f37`.
All evidence lives under `artifacts/event-history/parallel/`.

Remaining raw-data gates are production admission/funding/signing builders,
operator withdrawal classification preserving raw body/signature, raw bounds
counting every duplicate pair, and proof openings through final redeemer
composition. Semantically equivalent nonminimal bignum/constructor framing
has a concrete source-level normalization mismatch against pinned official
Plutus source (see `audit-history-tag-framing-parity.md`, 23 pure comparisons).
Deployment-version/reference-ledger parity and correction remain open; measured
ordered-map cases do not establish that broader claim. The next builder interfaces and eight-file ownership are agreed;
edits wait for root's shared raw-planner build. Production listener/source pins,
incarnation materialization, producer fences, L2 descendant/published repair,
below-anchor recovery, preservation-aware main integration and the original
acceptance matrix remain open. No live deployment/reset or main implementation
copy occurred; central live acceptance still requires one verified deployment.

The final raw-consumer identity and exact checks are recorded in
`root-raw-checkpoint-identity.json` and `root-raw-checks.json`. Main still has
242 dirty entries and its lock hash is unchanged. This is a candidate checkpoint,
not completion of the original objective or a live-acceptance result.


### Raw planner and production journey checkpoint (verification in progress)

The raw planner interface is built and frozen. Logical bounds now count every
raw Data map pair before typed decoding. Serialization normalizes nonminimal
constructor/integer framing while preserving map order and multiplicity. Root
ran the cached release-linked Haskell PlutusCore.Data 1.65.0.0 reference: 39
accepted and six refused framing vectors agree, plus 16 actual accepted
lifecycle Data samples. This is measured decoder/serializer parity for those
samples, not live deployment image provenance or full ledger-context parity.

Builders preserve raw bytes through publication, admission, predecessor updates,
funding, signing, payout and refund output assembly. Root withdrawal classification
verifies the raw signed body and changes only operator validity in settlement
info. Durable submission and CLI intent parsing now preserve opaque datum bytes
across JSON persistence and restart. Fabricated deposit/withdrawal preparation,
readmission, capture and final step-one/step-three redeemers retain raw data;
the transition opening helper also preserves its captured preimage. Remaining
transition and validation-dispute consumers are explicitly not complete.

Root independently reran the extended public SDK journey on one emulator
deployment: external raw deposit, real node settlement and reserve absorption,
invalid-signature withdrawal settlement and exact raw refund, then the original
valid raw-signed withdrawal against the same L2 source through classification,
settlement and final payout. All three authorized retained-data reclaims passed;
reclaim while the Order remained present was refused. Root parsed all 41 signed
receipts, three admissions/retirements and the exact 12-million-lovelace payout.
The first extension failure remains recorded: its successful manual refund spent
an operator input retained in the fixture wallet override; refreshing that
provider-backed fixture override after confirmed refund resolved the later
rewind failure. No production limits, waits or assertions were relaxed. Synthetic
transport/genesis and emulator execution do not establish live source authority.

The frozen candidate has 4100 source files, 47 changed since the prior checkpoint,
with aggregate `2d2932d56ee0cb3ab174cced0f6da39f1aad495dc1008a9ee53e12616f92ebea`
in `root-raw-consumers-source.json`. Passed checks: core 624/624 across 52 files;
full SDK 642/642 across 79 files; required SDK 175 LucidMidgard plus 642 SDK;
focused proof 73/73 across four files; node submission/classification 22/22;
watcher 57/57 across two files; independent broad node 326/326 across 33 files
(439.22s); required node 123/123 (15.60s) and tools 51/51 (10.28s). Six package
builds and noEmit checks and all 47 changed-file lint/format checks passed.
Required emulator verification finished: node 64/64 passed (366.24s); fault-proofs
585 passed and two withdrawal capture fixture failures (501.79s). The fixtures
reconstructed typed openings before checking expiry. Using the existing raw
opening helper fixed both sites; the agent full-file rerun passed 14/14 with all
original refusals unchanged. The failed run remains in
`root-raw-consumers-checkpoint-run.json`; no full-suite pass is claimed.

Independent root evidence verification also passed eight raw-deposit receipts,
five admissions and four continuations; the source-owner fixture's 25 signed
receipts, 488 creating transactions, ten replay receipts and 23 journal
applications; and nine list-replay receipts with 464 creating bodies and seven
persisted replay receipts. The owner fixture still uses SQL-probe reconciliation,
not production L2 repair. Reports and exact command records are in
`artifacts/event-history/parallel/root-raw-consumers-*`. Older checkpoint identities
remain historical. The next private raw-proof carriage and tooling scopes are
agreed; SDK/onchain interfaces remain frozen. The checkpoint source remained
unchanged through its terminal failure (`root-raw-consumers-checkpoint-identity.json`).
Root then integrated eight raw reconstruction/origin/replay and fixture files
from an isolated copy; 91 focused tests passed. New private proof material
retains committed leaf bytes through chunks, workflow artifacts and final
redeemers; branch/path and applied checks remain in progress. The watcher
agent completed six tooling consumer files, with nine pure and two existing
retired-ID applied cases passing. Root integrated verification and a fresh broad
checkpoint remain outstanding for this next source state.

No main implementation integration, deployment, reset or live acceptance occurred.
The original owner wiring, canonical incarnation eligibility, producer fencing,
L2 descendant/published correction, below-anchor recovery and full acceptance
matrix gates remain open. Main retains its unrelated dirty work.

### Active critical path — delivery checkpoint (updated 2026-09-24)

This is the single current critical-path checklist; preceding and subsequent dated
entries retain historical evidence. The user stopped recovery expansion after the
current observed-header case. That case passed; all agent implementation is now
frozen. Root owns consolidation and final verification. See
[event-history-delivery-checkpoint.md](event-history-delivery-checkpoint.md) for
the concrete delivered flow, commands, evidence and full-acceptance blockers.

| Gate | Implementation | Integrated verification | Live verification |
| --- | --- | --- | --- |
| Current recovery variant | Complete: authenticated full-range absence/expiry/finality, retained native plan, atomic journal/SQL inverse and exact lease release, cache/Globals before Ready. No further variants started | Checkpoint28 observed-header 1/1 and noEmit PASS. Checkpoint27 dependent spend, descendant and pending service restart PASS; reference case fails before the reference path at duplicate admission | Not run |
| Deposit AND withdrawal journey | Combined two sorted authenticated UTxO lists and affected validators, SDK, watcher, proof and production-owner consumers are in original checkout | Checkpoint25 actual D/W plus spend/descendant 3/3 PASS; root independently verified 41 signed receipts, exact refund/payout, retirements/reclaims and native/SQL agreement | Not run |
| Consolidation and preservation | APPLIED: 435 source paths, 413 verified build outputs and one native executable. Other work, Git HEAD/index and both conflicting ledgers retained | Root independently rehashed all 7,937 manifest paths; zero application/preservation mismatches. Original checkout dependency graph verified; noEmit and normal node/worker build PASS | Not applicable |
| Named required checks | Existing fixture defects corrected without removing assertions | Checkpoint28 node 125/125, emulator64/64, observed1/1 and noEmit PASS; source/dependencies unchanged. Final docs links PASS:543 Markdown/MDX files | Not applicable |
| Full recovery/crash matrix | Deferred at user delivery boundary; other signed/mixed/multi-header/published/merged/below-anchor shapes remain fenced or unverified | Reference fixture, OS-process crash and additional interruption/race acceptance remain open; service restart is not OS crash evidence | Not run |
| Final-identity proof/consumer and ABI readiness | Two sorted lists selected; no ABI freeze. Existing contract/proof work retained | Acceptance29 fresh pinned blueprint reproduces delivery; Aiken history195/195, proof noEmit and installed transition13/13 PASS. Ledger regenerated and1,340 receipts independently verified. Other installed families/packages, missing proof polarities and maximum-size/accounting/authentication/eligibility/retirement/deadline/pointer churn gates remain | Not run |
| Existing durable-state migration and one verified deployment | No DB reset, config change, service start or deployment performed | Initial-schema checksum change requires a reviewed existing-DB path. Docker/provider/config/parameter/manifest/reference-script prerequisites remain open | Both live lifecycles, contention/economics and recovery acceptance remain incomplete |

Historical context (superseded by the delivery checklist above). Proof snapshot: `artifacts/event-history/parallel/root-raw-proof-funding-fixed-source.json`;
exact checks: `root-raw-proof-checkpoint-run.json` (preserved failure) and
`root-raw-proof-funding-fixed-run.json` (passing current full-suite run). Independent receipt verification:
`root-transition-raw-verified.json`. Frozen snapshot tests continue while root and
builders work in `/home/gumbo/midgard-hub/event-history-node-materialization-staging`.
Only its copied `demo/midgard-node` is editable; other paths link to the frozen
candidate. No implementation has been copied into main, and no deployment or live
acceptance has run. The original objective remains active and incomplete.

Production composition checkpoint: owner-driven materialization and source-time
deposit projection are wired into listen before hydration; D/W polling fibers
are removed there. Commit IPC carries generation-bound checkpoint coverage, and
pending D/W members retain exact canonical incarnation pairs. Two targeted SQL
gate cases pass; the bounded audit closed both nested-candidate and first-owner
race findings. This does not establish complete producer/repair coverage.

The native owner represents the canonical empty trie explicitly, including
empty genesis, insertion, deletion back to empty and restart. Rust9/9 and native
service7/7 pass without changing limits. The builders production journey passed
1/1 (56.792s), and root independently reran the immutable snapshot1/1 (57.790s).
Independent verification checked all41 signed receipts,504 creating bodies,
36 Ready cache/SQL checkpoints,8 bootstrap receipts and47 journal applications.
All three retirements and reclaims and the exact12 ADA payout were verified.
The synthetic genesis/ancestry/transport scope remains explicit; this is not live
acceptance or complete producer/recovery coverage.

Frozen integration checks run in
`/home/gumbo/midgard-hub/event-history-production-owner-checkpoint` while root
continues isolated edits. Exact evidence and commands:
`root-production-owner-checkpoint-{source,run}.json`,
`root-streaming-production-run-5-{receipts,owner}-verified.json`.
Typecheck and required node123/123 checks passed; required emulator51 passed/13 failed at new ownership gates. The attempted fault-proof package script did not exist; its exact named workspace subcommand subsequently passed589/589 across117 files with frozen source unchanged. Failures are preserved in the checkpoint manifest.
The bounded audit identified startup retry reopening an existing native owner,
and legitimate empty-ledger restart taking the genesis branch. Root fixed
those; isolated native restart acceptance passed1/1 (43.022s), including missing-store refusal. Current full integration still needs fresh verification.
Current detailed prior node evidence remains
`root-node-owner-integration-work-in-progress.json`; the immutable milestone
manifest above supersedes its streaming-run status without rewriting failures.

Focused follow-up: recovery-only ingestion and preparation SQL rollback cases3/3
passed. The bounded auditor reviewed startup resource reuse, legitimate empty-root
restart, missing durable marker refusal before SQL writes and ingestion gating.
Full native/SQL/journal agreement is not closed. Native restart acceptance is
isolated on the prior passing composition plus exactly five native/test overlays,
while root continues current integration; it is not evidence for the unfinished
strict final event-interval gate. Test-only full-runtime setup verifies its shard
name before clearing prior history authority and MPF initialization metadata.

Fixed-window follow-up: `root-fixed-history-window-narrow-1.log` records7/7
passing narrow cases. `audit-fixed-history-commit-window.md` closes speculative
permit preservation, selected transaction timestamp bounds and scheduler-slot
rounding findings. The builders agent owns the streaming acceptance extension;
root retains pending-journal persistence and reconciliation. Native-only restart
evidence (`builders-native-only-run-1-evidence.json`) remains scoped to its exact
five-overlay snapshot and does not establish current temporal completeness.

Credential-interruption reconciliation: prior runners are terminal, not restarted.
`root-fixed-window-checkpoint-run.json` preserves journey1/1, noEmit and required
node123/123 passes and required emulator61pass/3fail. Root independent receipt,
owner-journal and signed-header timing verifiers pass (`root-fixed-window-*-verified.json`).
The three failed emulator cases remain assigned to bounded diagnosis; no broad
pass is claimed. Signed-intent narrow checks5/5 and consumer checks50/50 passed
before the latest prepared-body binding. An applied acceptance/response-loss
fixture is being implemented independently by builders; root retains schemas,
persistence, reconciliation and integration. Main has advanced to409dfdaa6 on
`colll78/canonical-v1-watcher-l1-source-checkpoint` with unrelated work preserved
in `root-resume-preservation-baseline.json`; no staging implementation copied to
main and no deployment/live run occurred. External main-tree test processes are
left untouched; further heavy tests are centrally scheduled after checking load.


### Resumption reconciliation — 2026-09-24 UTC (2026-09-23 local)

User resumed the full original goal and authorized up to four concurrent agents.
The previous assessment completed without source integration or deployment.
Its current staging `pnpm exec tsc --noEmit` returned exit0; the saved earlier
prepared-hash fixture type errors are superseded by that exact static check,
not by an applied response-loss result. The response-loss test file did not exist
at resumption. Prior root test runners are terminal. Six retained run-log hashes
and all538 files in the root fixed-window node snapshot were independently
rechecked with zero mismatches (`root-resumption-assessment-evidence.json`).
Candidate blueprint remains7b7a0abae2db62e3ecb9e26e7907bbc1d4793eeee1a522470cc1c1014d1c8d74.

Current original checkout HEAD is1f3280f93cfc903c2532bb7d041f5a7ff4cdb7aa,
on colll78/canonical-v1-watcher-l1-source-checkpoint. Its dirty state changed
from162 to170 entries between read-only assessments; external work is active.
Candidate and node staging are copied working directories, not Git worktrees;
only staging's demo/midgard-node is independently copied, while other paths
link to candidate. Keep those editing lanes. Build immutable combined snapshots
for verification; final integration into the original is a separate preservation
step. No directory-wide copy into the original is authorized by an old inventory.

At resumption, independent processes included a fault-proof Vitest parent711385
with eight workers in the original checkout, node Vitest653756, and Aiken704847
in an external scratchpad baseline. Leave them untouched; process IDs are only
a point-in-time record, and root must recheck before heavy tests. The process
inventory's first parser failed on command names containing spaces; corrected
parsing completed without acting on any process. Root owns test scheduling.

Current file ownership: root owns schemas, production source, ingestion,
persistence, T2 reconciliation, integration and this log. retirement_builders
owns only new node test l1-event-history-signed-intent-emulator.test.ts.
selection_fixture_fix owns only tests/deposit-flow-emulator-commit-selection.test.ts.
consumer_acceptance_audit reads only the T2 patch/call context and concrete
acceptance gaps. integration_inventory reads candidate/original baselines and
writes only its ignored inventory reports. Every agent must report changed
files, exact commands/results and remaining gaps; no agent starts emulator/PG
or heavy suites without the central lane assignment.

Shared interfaces stay fixed: preparedTxHash from the completed transaction body;
prepared_tx_hash/intended_tx_hash/signed_tx_cbor persisted before provider submit;
submitted_tx_hash records acknowledgement, while canonical header observation
may legitimately recover without it. Exact history binding/incarnation and owner
coverage remain authority. Queue pointer continuation cannot replace intended
transaction identity. No schema or public SDK interface change is planned for
the first failure/response-loss checkpoint.

Root applied the minimal T2 source correction: withHistoryWrite surrounds the
existing per-entry SQL transaction, before its reads/locks and all event/evidence
mutations. This source change has not yet passed the original T2 emulator case.
The bounded failure report is audit-fixed-window-failure-resumption.md.
Unpublished orphan-funded L2 repair remains unimplemented: current materializer
explicitly refuses readiness. No acceptance gap is closed by refusal alone.

Checks at this resumption: staged node noEmit exit0; runbook validator exit0
(17 command references,22 recovery drills); retained log/snapshot hash validation
passed. Exact commands and subsequent checks are recorded under
artifacts/event-history/parallel/root-resumption-*. Original required failures,
new signed-intent applied coverage, full recovery/ABI matrix, final integration
and live acceptance remain open. No on-chain/durable state was reset.


### Resumption checkpoint results and native recovery defect

Root created event-history-resumption-checkpoint-1 with 5,075 copied source files
(identity ab6af414c11ae1f23047a1c7b5c89a14df7f47ab49141b259658e0931b215d72).
Workspace dependency links point inside that checkpoint; only third-party pnpm
store content is shared. No source was copied into the original checkout.
Typecheck passed. All three original failing emulator cases now pass: two seed
authority-order fixtures and the original T2 recovery case, 3 passed / 4 filtered
out, 103.40 seconds, one fork and isolated midgard_test_history_resume_1 shards.
All checkpoint source hashes were unchanged. The prior required suite result
61/64 is retained; these narrow passes are not a rerun of the full required gate.

The new response-loss fixture failed 1/1 in 45.22 seconds. It independently read
committed prepared/intended hashes and exact signed bytes before real provider
acceptance, then verified retained pending intent, canonical acknowledgement and
SQL local-finalization recovery. Its native durable-root equality assertion
failed: empty native root versus the nonempty confirmed deposit root. Root and
auditor traced a production omission: after response loss there is no promotion
handle, and later successful local finalization finalized SQL without native
replay. This is not classified as a fixture failure. Logs, actual signed receipts
and failed assertions remain in root-resumption-signed-intent-{run,evidence}.json
and root-resumption-signed-intent.log, with unchanged source hashes.

Root implemented private native replay before local finalization. Production
parent and direct-worker fixture both check exact confirmed header/journal roots,
accepted or canonically observed status and canonical member associations, then
recover the native owner outside SQL and recheck authority. Intent alone cannot
authorize replay. A worker refuses SQL finalization when its supplied native
durable root differs from the confirmed header. No SDK/schema/native RPC change.
Bounded source review found no additional defect; applied verification remains.

Builders saved an additive service-restart helper and a new restart fixture: same
SQL, Level, deployment and transport, fresh Globals/cache/owner/runtime/native
child; no restart TRUNCATE or initialization. It explicitly excludes OS-process
crash and pre-acknowledgement pointer continuation. Checkpoint2 (5,077 files,
identity7f22f124d356647ffbc34132a9f6cac7f3d54213d1bb11905d355240c7c3824f)
retains the native fix and restart additions. Its typecheck found one missing
SqlClient provision in the new test's old-owner refusal assertion; builders is
correcting that fixture while this failed snapshot is retained. No runtime pass
is claimed for the native fix or restart yet.

Current original integration inventory has a verified Aiken common-ancestor
bridge and exact baseline content for all34 non-documentation overlaps. The
combined sample contains16 mechanically clean overlaps,10 text conflicts,7
original-deleted/task-modified paths,1 path changed during the probe and3 docs
without a saved copy baseline. Two original watcher files changed during the
final hash check; the seven deleted paths were already absent at initial capture.
See integration-resume-inventory.{md,json}. Preserve original additions, removals
and outside-scope work; rehash before any final application.

Additional current checks: scoped root/agent lint and formatting passed; root
docs command pnpm --dir docs-site run check:links passed480 Markdown/MDX files;
git diff --check on this progress log passed. No deployment/live acceptance,
ABI freeze or complete rollback/repair claim is made.


### Resumption checkpoint3 — independently verified journeys

Checkpoint2 remains a failed immutable typecheck record. Builders supplied the
missing Database.layer only in the new restart test; old-owner refusal assertions
are unchanged. Root captured checkpoint3 with5,077 files and source identity
61ca4bac45c4e11bec72dafeca861f4d734bafc423a5eb044563ff23ab33fb19.

Exact checks from its demo/midgard-node, Node22.22.2/pnpm9.15.4:

- `pnpm exec tsc --noEmit`: exit0.
- `pnpm exec vitest run tests/l1-event-history-signed-intent-emulator.test.ts tests/l1-event-history-signed-intent-restart-emulator.test.ts tests/l1-event-history-streaming-production-lifecycle-emulator.test.ts`:3/3 passed,146.97s, one fork, isolated midgard_test_history_resume_3 shards.
- Independent signed-CBOR receipt/header/native verification: response-loss8 receipts; service-restart4 receipts; full streaming journey41 receipts,3 admissions/retirements/publications/reclaims, exact12M withdrawal payout. Each decoded hash, signed bytes, fee and execution budget matched its retained receipt.
- Independent owner/timing verification:504 creating transactions,40 Ready SQL/cache checkpoints,48 journal applications,8 bootstrap replay receipts;3 exact signed header/TTL windows and2 known-future events; unchanged production limits.
- All5,077 checkpoint source hashes unchanged after execution.

Runner and reports are artifacts/event-history/parallel/
run-resumption-checkpoint-3-journeys.py,
root-resumption-checkpoint-3-journeys-run.json,
root-resumption-checkpoint-3-{signed-intent,restart}-verified.json and
root-resumption-checkpoint-3-streaming-{receipts,owner,timing}-verified.json.
Journey log SHA256 fd9b4330cb4af297c215561dc4d32254e5a52eb8a4302ff04c47baa36cdd2ef1.
Compiler/blueprint identities remain the previously verified pinned build; no
new Aiken build is claimed. Blueprint SHA256
7b7a0abae2db62e3ecb9e26e7907bbc1d4793eeee1a522470cc1c1014d1c8d74.

Response loss now demonstrably recovers the native root before SQL finalization
and reaches mature merge. Service restart recreates Globals/cache/source owner,
ManagedRuntime and native child while preserving SQL/Level/deployment/transport;
it is not an OS-process crash. Actual provider acceptance is exercised, but the
fixture does not execute the production FailureOutput-to-mutation-lease classifier.
Pointer continuation, cancellation, intent conflict/expiry/non-inclusion, dependent
L2 rollback repair and full recovery coverage remain open.

Current parallel ownership: builders adds only a new pointer-continuation fixture;
fixture agent adds only native-local-finalization negative tests after interface
review; auditor reads only the unpublished-orphan repair boundary; inventory agent
produces a watcher semantic-port plan. Root retains production changes, shared
interfaces, integration and all heavy/live scheduling. Required broad checks are
queued while the original checkout's external fault-proof suite is active.
No original implementation was overwritten, no deployment was reset, and live
acceptance plus final integration remain incomplete.


### Preservation-aware integration workspace and continuation checkpoint

Root captured current original source separately into
event-history-original-integration-workspace:7,358 files, identity
89aa37dda51fb5f8181e12bc81c2661ef19515d9c784f5feafadc1f456ca7a9a.
Initial capture encountered an inaccessible runtime wallet file under logs; the
partial root-created copy alone was discarded, runtime logs/env files excluded,
and the complete retry passed before/after hashes. Original source and state
were untouched. Exact source/composition records are
root-original-integration-base-source.json and
root-original-integration-composition.json. This is a mutable integration lane,
not the verified release and not an overwrite of original.

Composition applied366 task paths and2 subsequent node additions; preserved226
original changes/deletions;16 clean text merges still require semantic checks;
11 text conflicts,7 deletion conflicts,3 missing documentation baselines and the
generated blueprint required deliberate integration. Root resolved seven node
conflicts preserving newer credential validation, centralized deployment-script
catalogue, migrations and test resets. Authenticated-history retention/retirement
and L1-event/forced-timing entries now extend the central catalogue. The watcher
agent ports shared scanners/local recovery and meaningful tests onto original
local origin/publisher APIs, preserving removed global authority paths.

Bounded root-node integration review found a real reset interaction: the newer
reset helper replays everything after the seed marker, but task history DDL was
appended below it. Root moved only the unchanged singleton seed block to the end
of the initial migration. Source review confirms resolution; runtime verification
remains pending. The catalogue test receives real history fixture contracts; its
old full-order/count pins still need reviewed update plus added script assertions.
First node integration noEmit reported only9 missing json-bigint resolutions;
root linked the exact declared/pinned package and types from the verified store.
A subsequent integration typecheck and runtime gates are still required.

Checkpoint4 identity65d0e35c0fad5263c9f814678d8416d02cbdf51ab0ee82efa83106f8a2c6804d
contains5,079 source files. noEmit passed. Its actual signed-intent continuation
journey passed1/1: authorized operator submits an honest empty child before node
confirmation; parent outref changes, original header/full Value and intended
signed body remain unchanged. Canonical observation keeps acknowledgement NULL
instead of substituting the child transaction hash. Parent native finalization
restores exactly12M. Independent verifier decoded all5 signed receipts, verified
actual input/output/pointer/header/empty-child relations and native equality;
root-resumption-checkpoint-4-continuation-verified.json is passing evidence.
Scope excludes automatic-worker child scheduling, OS crash and the production
FailureOutput-to-lease classifier.

The same run's five new SQL authority-negative cases failed during fixture setup:
missing FK closure in reset. Checkpoint5 corrected reset/setup order but then
failed all5 because its modeled queue UTxO lacked an authentication NFT. Both
failures remain recorded; assertions were not weakened, and no negative-coverage
pass is claimed. Source unchanged in each run. The test author is correcting the
modeled queue serialization before another narrow immutable run.

Latest docs check: pinned pnpm --dir docs-site run check:links passed488
Markdown/MDX files. Full D/W measured maxima across41 independently decoded
transactions:3,394 signed bytes,1,111,140 lovelace fee,3,962,496 memory and
1,594,730,662 steps; maxima may come from different transactions. Limits unchanged.
Broad required checkpoint3 node/emulator gates and integrated pinned Aiken build
remain centrally queued behind the external original fault-proof suite.

Concrete orphan-repair plan is audit-unpublished-orphan-repair-plan.md. Existing
mempool deltas are UNLOGGED best-effort spend references, not durable consumed
output preimages; they cannot reverse accepted deposit-funded descendants. Full
suffix inverse/baseline replay, reference-input dependencies, recovery-only
requeue, published correction and below-anchor recovery remain implementation
gates. No refusal-only behavior is counted as completed repair.


### Current queued verification and retained original baseline

Required checkpoint3 node gate passed125/125 (7 files,26.84s); log SHA256
f50b43981c827b7996eab0aff7fd5aa5da583986d694f6b197d72d3d1b299f4c.
The scheduling guard deferred the emulator step when another external proof run
started, then resumed only the remaining step after those workers exited; no
repeat of the passing node gate. Required emulator is currently running in one
fork with separate test shards. See root-resumption-checkpoint-3-required-run.json.

Original capture bytes are retained in event-history-original-integration-baseline
against all7,358 captured hashes. A concurrently updated workflow-fit ledger was
recovered from its exact matching HEAD blob; progress log bytes came from the
unchanged integration copy. Original source was never replaced. Future final
application must three-way compare this retained baseline against then-current
original, preserving later edits.

First watcher port changes six files; preserves all16 candidate local-history
test groups and all six removed globals. Agent typecheck, scoped format and
lint passed; no watcher runtime acceptance claimed. Retired reserve/payout and
malformed observer/spend/forced-terminal assertions not yet relocated remain
explicit gaps with bounded follow-up owners.

Root captured integrated checkpoint1 with7,494 files, identity
119316ba4492ff7f4800698b6fad617b36ece1d7e4f418fddd149767bf02f506;
root-original-integration-checkpoint-1-source.json records every byte. Root node
typecheck is running on it. Its captured source contains no plutus.json;
a freshly rebuilt integrated identity is still required. Separately prepared1,226 Aiken source files
in event-history-integrated-aiken-build-1, identity
feb05fba9a26a6c6dcdcfd95110a74ee5c3f53b1e3e6044b5f384d0e033081c3,
without build cache or blueprint. Pinned compiler version and binary hash match
CI; compiler build awaits the central heavy lane.

Read-only live preflight: Docker default unix socket is unreachable, service
inactive and no dockerd process. Alternate context is a Windows npipe endpoint.
Existing deploymentInfo manifest is legacy v2/Preprod, identity
bbdfca85031de1c9adea89239f51180c1ba804e926f9c66c2b11294b666f7ea5,
40 contract entries and no history descriptors. It is preserved and has not been
validated for this source; it cannot supply final history acceptance identity.
root-resumption-live-prerequisites.json records observations. Service remediation
and one verified new/resumed deployment remain live prerequisites after lower
gates; no reset, service start or chain transaction has occurred this resumption.


### Required suite and first integrated narrow results

Checkpoint3 required emulator finished61 passed/3 failed in622.76s (8 files),
all source hashes unchanged. These are different remaining failures from the
previous61/64 run: original selection/T2 fixes pass. Two failures use synthetic
T1 stale recovery after an actual signed submission; the new unresolved-intent
guard correctly refuses abandonment without admitted correction/non-inclusion.
The third standalone deposit fixture omitted its explicit unowned fixture permit
for protected UTxO reads. All failed assertions are retained. Root supplied that
existing fixture context in staging/integration (four reads and startup boundary);
authenticated timeout-correction fixture work proceeds separately. General signed
expiry/non-inclusion disposition remains missing production functionality.
Full log hash7760f606e7ad1047660fdfe5843520988546290d55b642e48bf9b0ebe455dded.

Root integrated checkpoint1 node noEmit passed. Narrow command:
`pnpm exec vitest run tests/native-mpf-local-finalization.test.ts tests/migration-runner.test.ts tests/migration-locking.test.ts`
passed16/16 across3 files in34.68s with isolated midgard_test_history_integrate_1
shards. This includes all five corrected SQL/native-boundary model cases, actual
migration constraints and independent-pool migration locking; it does not turn
the native boundary spy into real native replay evidence. Log hash
d9d06ea0e9b6cd4dae9c23134db37fd368d781759dad2e5bf624f1840945c22a.

First integrated watcher command ran the four planned files with one worker.
Pure authenticated-history decoder7/7 passed; three suites collected0 because
the captured snapshot has no onchain/aiken/plutus.json. Record is
root-original-integration-checkpoint-1-narrow-run.json; no applied watcher pass
is claimed. All7,494 source hashes unchanged. Root started the prepared clean
pinned Aiken build with explicit testnet/verbose/all traces, recorded in
root-original-integration-aiken-build-1-run.json; schema/hash verification awaits
completion. No test timeout, production limit or assertion was weakened.

New watcher-only rejection fixture is saved separately in mutable integration:
26 malformed variants with five valid controls, typecheck/lint/format passed;
runtime and remaining reserve/payout applied negatives are pending. Root's scoped
integration node lint/format passed. Final integration and live acceptance remain
incomplete.


### Integrated normal blueprint and next bounded checkpoint

Fresh pinned normal command `/home/gumbo/.aiken-fork/bin/aiken build --env testnet`
completed successfully in clean event-history-integrated-aiken-build-2. All1,226
source hashes stayed unchanged; source identity
feb05fba9a26a6c6dcdcfd95110a74ee5c3f53b1e3e6044b5f384d0e033081c3.
Blueprint hash7b7a0abae2db62e3ecb9e26e7907bbc1d4793eeee1a522470cc1c1014d1c8d74
matches verified checkpoint3 exactly:1,162 validators,1,881 definitions, zero
changed script hashes. Root independently verified all10 history definitions,
complete retained datum, insertion/promotion reference fields and retirement
parameters. Normal build run/verification evidence is in
root-original-integration-aiken-build-2-{run,verified}.json. Earlier verbose
build1 is diagnostic only; its trace-dependent hashes are not deployment inputs.
No ABI freeze or final integrated acceptance follows from this build alone.

Root copied the verified normal blueprint and four bounded fixture files into
mutable original-integration only, with before/after hashes in
root-original-integration-new-fixture-ports.json. New timeout helper and two T1
callers retain unsafe stale injection as a refusal, then require an actual
accepted timeout removal and existing authenticated correction observer path.
Original terminal assertions remain; runtime/typecheck are pending. A separate
new reserve payout economics fixture has five evaluator-only negative variants
and submitted honest controls, with no runtime pass yet. Watcher malformed
retirement coverage remains pending runtime. Agent ownership is now catalogue
fixture only and read-only T1 review; root owns integration/runtime scheduling.

Process reconciliation found unrelated original proof Vitest and Aiken checks
active again, plus independent review tests. No additional heavy root tests were
launched during that overlap. Pending narrow runs will use an immutable second
integrated snapshot after catalogue expectations and fixture review settle.
General signed-intent non-inclusion/expiry, orphan-dependent L2 ledger repair,
final consumer integration and one verified live deployment remain open gates.


Integrated checkpoint2 captured7,498 immutable files, identity
cc73bc02c59926db6ed2e6d4e8e71040c02b6c5e6a10d99e07b7dfb6313bc774.
Node noEmit FAILED with four unsupported TransactionInputList.to_cbor_hex calls
in the new payout fixture only. All source hashes unchanged; failure retained in
root-original-integration-checkpoint-2-typecheck-{run.json,log}. Builders owns the
precise per-input comparison correction in staging, preserving assertions; no
runtime on this failed checkpoint is claimed. Catalogue static derivation verified
all six historical order digests after excluding exactly the six additions;
full counts534/527/526 and permanent old-projection assertions are retained.

Root read-only final-application preview compared captured original baseline,
checkpoint2 and current original:7,092 preserve-current,405 apply-task, one
conflict (generated transition-trace-workflow-fit-ledger.json), no concurrent
source drift during preview. No original implementation writes occurred. That
ledger contains measurements bound to differing source/blueprint identities;
retain both records and regenerate/check final integrated evidence, never merge
measurement claims textually or discard another task's evidence. Root original
progress changes are already preserved by the three-way baseline rule.
Docs links passed496 Markdown/MDX files after current log additions; exact command
`pnpm --dir docs-site run check:links`, pinned pnpm10.11.0/Node22.22.2.


User confirms the parallel Claude cleanup has finished editing original files;
retain its work during application. Remaining external verification processes are
separate, observed/read-only; no external jobs were terminated. Exact per-input
CML comparison remedy was ported into integrated checkpoint3 (7,498 files,
4f68b23170a71481212315acb6114fb219eaecb169e24800105619699234766a).
Node noEmit now PASSED with unchanged immutable hashes.

Checkpoint2 watcher runtime is active. Admission/retirement authority25/25 passed;
block replay currently20/21, rejection fixture4/5. Two diagnosed fixture defects:
public replay support used full L1 deposit assets including structural ADA; forced
settlement reference hashed pre-canonical object instead of the exact registered
serialized bytes. Scoped fixture repairs are authorized, assertions preserved.
CLI maxWorkers did not override watcher poolOptions: four file workers ran,
contrary to the intended one. This scheduling correction is explicitly retained;
next watcher run sets MIDGARD_WATCHER_FORKS=1. No broad root suite was launched.
Parent lease audit did not establish unsafe subsequent mutation: failed operation
lease may release, but signed journal guards replacement before provider submit.
A generic journal-dependent lease block would risk freezing recovery/merge and
was not added. Applied exact parent/classifier/SQL lease composition is still a
specific acceptance gap; a separate bounded fixture is being scoped.


### Root primary: dependent L2 rollback repair implementation started

Per user priority, root began production recovery work without waiting for remaining
fixture/catalogue integration. Private node schema adds logged
`event_history_l2_ledger_receipts`, ordered under the checked Ready authority
lock. New `eventHistoryLedgerReceipts.ts` captures complete net consumed rows,
reference dependencies, exact deposit incarnation rows and original canonical
transaction/CEK payloads before either optimized or fallback acceptance mutation.
Finish verifies net outputs and complete consumption in the same transaction,
before the original sidecar scrub. Canonical transaction IDs, inputs and outputs
are checked against the submitted body. Explicit isolated unowned fixtures do
not manufacture production inverse receipts.

Root wired markAccepted/markRejected SQL through the existing history write gate;
the complete validation tick now registers with runHistoryProducer. Recovery
adds a private drainBeforeRepair callback after producer drain and before SQL;
production composition uses real WriteBehind.flushNow. Failure cannot open Ready.
The production-owner lifecycle fixture now provides that real service and includes
the new FK table in its disposable initial cleanup. No deployment ABI changes.

Initial WIP noEmit found a missing DatabaseError type import and the fixture's
missing WriteBehind service; both corrected, next noEmit pending. Scoped format
passed; lint pending. This is implementation in progress, NOT validated rollback:
actual inverse application/requeue, native accepted-baseline proof, published and
possibly-broadcast constraints, unsigned candidate disposition, classified
withdrawal revalidation, receipt retention/pruning and all recovery matrix cases
remain open. Existing orphan readiness refusal is deliberately still in place.
Recovery fixtures are delegated against the fixed private begin/finish interface;
root retains all persistence, source authority, SQL repair and integration edits.

Integrated checkpoint3 narrow results: catalogue8/8 PASS; the two original T1
failures now2/2 PASS (five unrelated filtered tests not run); standalone original
deposit submission failure1/1 PASS (one unrelated filtered test not run). These
narrow filters do not replace required full-suite acceptance. All7,498 source
hashes remained unchanged. Payout economics FAILED at retirement-output matching;
agent traced installed dependency realization, not a reason to weaken raw-map
assertions. Current integration links resolve old Lucid patch despite unchanged
committed patch/lock; precise local dependency repair is underway. Node's passing
normal compiler blueprint identity remains separately verified.

Checkpoint2 watcher finished103 passed/2 failed over105 tests; failed fixture
repairs now authored and typechecked separately, runtime rerun pending. Runtime
log hash5c72ff51667eedc1e391e6a47f0c37e547114473f4870dc43a8a1fafe6fffd50.
The4-file run took419.70s; record the actual four-worker scheduling correction.

Live investigation found the installed Windows Docker Desktop backend stopped
under WSL2. Missing Linux dockerd/service is expected; do not install a competing
engine. Root can start existing Docker Desktop, recheck Ubuntu integration and
preserve original provider data mounts. Integrated CLIs/config, genesis pin,
measured history parameters and DA role configuration remain prerequisites.
Runbook/skill validators pass. No services, reset, deployment or value submission
occurred; audit-live-prerequisites-docker-wsl.md records exact nonsecret remedies.


### Dependent rollback: first production inverse wired; receipt checkpoint verified

Supersedes the WIP statement above: root implemented eventHistoryLedgerRepair.ts
and wired it into canonical materialization under the source-owned recovery SQL
transaction. Complete unpublished accepted batches reverse in descending durable
receipt sequence; exact after-images/payloads are checked, consumed ledger rows
and CEK payloads restored, accepted admissions requeued, and derived mempool,
deltas/address history and unassigned withdrawal classifications invalidated.
Signed/pending candidates, published/immutable membership, missing baselines and
image mismatches remain fenced. Auditor found immutable-table coverage missing;
root added it to baseline coverage and per-batch refusal. This is conservative
unpublished-overlay repair, not published/native baseline reconstruction.

Immutable resumption checkpoint6 identity
4692b3dabcc1ac37d0d5c521809a1527b3b7c78172e2097dda37ff7dab61ddff:
node tsc --noEmit PASS; vitest receipt/recovery/migration files37/37 PASS,
including14 new receipt cases. All4596 captured source hashes unchanged.
SQL log SHA25602a361ea56e5928f079a84820667ee6459f5b12b4cf7b8f9f0df2dc7984363f2.
This run does NOT exercise the inverse application, newly added drain fixture
or actual-owner dependent rollback. Those fixtures are the next narrow gate.

Root retains persistence/ingestion/reconciliation/final integration. Builders own
the new SQL inverse fixture and the exact obsolete refusal fixture update.
Selection agent owns new actual-owner rollback transport/test against fixed
private harness callbacks; root owns shared helper changes. Auditor owns bounded
signed-intent evidence review; inventory agent traces live configuration evidence.
No subagent runs heavy checks or live actions. New implementation stays in staging
until independently verified and preservation-aware integration completes.

Integrated checkpoint4 identity
2f9d1d0e21d9d804e492ff28d6873bbe2316517b67956f6d039c6a15ada6874f
contains repaired watcher fixtures and dependency realization: integration-local
Lucid/UPLC aliases now match the declared locked patches/vendor. Original checkout
and shared stores unchanged. Runtime revalidation and independent dependency
rehashing remain pending; checkpoint3 failed payout evidence remains retained.

Checkpoint7 (cf725067a1803b373f55c3eb7c67fa4519da29fb511e66848502b0c044af45f0)
noEmit PASS; inverse/drain/journal47 tests:44 PASS/3 FAIL. All source hashes
unchanged. Journal30/30 and deferred drain5/5 passed; inverse9/12 passed.
Retained failures: modeled reference-input acceptance lacks Phase B resolved
bytes; withdrawal assertion expected forward pointer after authenticated undo;
signed-pending setup inherited a SQL transaction which production correctly
refuses. Auditor owns those exact fixture remedies, with all safety assertions
preserved. Log SHA256e3642085f96faa73300a59c865ce7dc6cd63254e77e1b7b544aaf655d347a928.

Root integrated20 rollback production/fixture files into the mutable preservation
workspace using exact checkpoint3 baselines; original implementation untouched.
Schema conflict resolved by inserting exact new receipt DDL before the preserved
relocated singleton seed. root-rollback-integration-result.json binds before/after
hashes. Runtime validation of that combined state remains pending.

Root independently checked12 repaired aliases,18 critical packages/126 files and
all14 installed UPLC files against the locked vendor tar. Dependency identity
c8b1f41f815a074dee92a8a4c444b86a8cd9dd15ba13a3af08e158ce0ec689db.
Integrated checkpoint4 payout and two complete watcher fixture files now run
sequentially with explicit package fork count1. This immutable checkpoint does
not include new rollback source; edits continue separately.

Integrated checkpoint4 watcher rerun26/26 PASS (both complete affected files),
source and126 dependency hashes unchanged. Payout passed the former byte-preservation
blocker, then failed an obsolete new-fixture phase expectation: initialization
carries original withdrawal funds, so it is partially_funded. Root corrected the
expectation and added exact original/current/remaining Value plus NFT assertions;
production semantics and all adversarial assertions unchanged.

Integrated checkpoint5 (8eb8c0f97d6021a72137cd6293344a1bd617232ac76dfd203603481277e1ec42)
noEmit failed TS2742 from duplicated original/candidate Effect installation roots
after Lucid alias repair; runtime not started. Integration agent is repairing only
integration-local dependency aliases to the identical locked candidate graph.
Resumption checkpoint8 (adb02a6ae194dbf3d0a698bcdbc5236d62edabedbd608dbfe005673fd9fd7069)
noEmit found two new fixture interface errors: factory inferred private socket
class and nullable generated sidecar. Root exposes the existing transport protocol
type; author requires actual sidecar bytes explicitly. Runtime not started.
All failed checkpoints/logs retained; no check or production guard weakened.
Docs required command passed: pnpm --dir docs-site run check:links,514 files,
root-resumption-docs-links-after-rollback.log.

Dependency repair settled:22 integration-local Effect-family aliases now use one
locked installed graph. Root independently compared4790 files across12 affected
package roots, all byte-identical, identity
af0000a46055f8493f0efe15f537c70e4c9f94ea1bba2656a37dc6f9c3d1c480.
TS2742 portability errors gone. Integratedcp6 then caught one modeled source
binding literal widening; cp7 exposed anonymous socket private-type leakage.
Both fixture type interfaces corrected without changing checks: typed source
facts and WebSocketLike return boundary. Preserved failures; cp8 queued.

Next production recovery gap traced concretely: current correction APIs repair
assignments/journal membership only; confirmed-ledger reconstruction is forward,
native recover only base-to-candidate/idempotent. No authenticated published
baseline restoration API, signed-expiry/noninclusion authority or below-anchor
reconstruction exists in inspected path. Spent-key-only published deltas cannot
serve as inverse output evidence. Do not mark those gates closed from the new
unpublished overlay path; root owns that subsequent design/implementation.

Integrated checkpoint8 d7daf4c04beb52a0f6b874fd27f7515857ee8c048d1048adb1c1c6bb9f80db69:
noEmit PASS; all12 real-SQL unpublished inverse cases PASS,223.50s. Exact log
root-original-integration-checkpoint-8-repair.log SHA256
c3b788aaf1b53e7f1ce747d193ec31f25207c7cf1e328f3a3438ad07d7f13736.
This includes actual admission persistence + retained CEK/reference bytes, whole
spend-chain reversal/requeue, orphan withdrawal reclassification, same-ID fresh
incarnation, signed/published/immutable/missing/corrupt evidence refusals and
late-child-inverse atomic rollback. L1 ancestry is modeled in these SQL cases.
Actual deployed-validator/source-owner rollback, parent lease and economics
journeys are now running sequentially; those stronger gates remain pending.

### 2026-09-24 — Integrated checkpoint 9 and dependent-recovery correction

Root retained the original checkout and all prior failed snapshots. Immutable
integrated checkpoint 9 has source identity
`517a306bf76dbd1b5c67be0a0076b9d8ab7307fba1bfbcd586e4a261cd172bcb`.
The centrally serialized Node22/pnpm9 batch (`run-original-integration-checkpoint-9-recovery.py`)
completed: `tsc --noEmit` PASS; payout economics 1/1 PASS; actual parent-lease
signed-response-loss journey 1/1 PASS; actual owner rollback 0/1 FAIL;
`database.test.ts -t 'keeps submit latency isolated while the batch pool is held'`
1/1 PASS. Source and verified dependency hashes were unchanged. Exact commands,
timestamps and logs are retained in `root-original-integration-checkpoint-9-receipts-run.json`.
The complete three-journey invocation therefore FAILED; no full acceptance claim.

The rollback failure is a production readiness gap, not an assertion to relax:
`mempoolLedger.retrieveSpendable` excludes unassigned deposits. A genuine deposit
must first have a real confirmed header assignment before an L2 transaction can
spend it. The current conservative inverse repair refuses those assigned/published
events. Thus the previously passing 12 modeled SQL inverse cases prove their
bounded inverse mechanics, not a real orphan-funded L2 recovery journey. The
positive fixture remains required and failing. No fake header assignment, removal
of the spendability predicate, or replacement with an unrelated funding scenario
is authorized as a substitute.

The signed-intent offline auditor independently verified all 8 retained signed
receipts, one provider submission, distinct leases, exact intent and unchanged
journal, confirmation and final native/settlement state. Verifier source-loading
imports were corrected for the immutable source snapshot; all assertions retained.
See `audit-parent-lease-checkpoint-9-verification.{json,md}`. Payout independent
verification is still being reconciled against retained complete creator bytes.

The single active critical-path checklist above is updated with this result.
Native restoration is a prerequisite only; the positive assigned-deposit/L2
rollback, canonical signed-intent disposition, SQL baseline restoration, final
original integration, broad suites and live verification remain incomplete.

Assignments: root owns native restore, persistence/ingestion/reconciliation,
shared interfaces, SQL recovery and final integration. Selection agent owns only
new native recovery test file after the interface is fixed; original rollback
fixture is frozen pending real published recovery. Auditor reviews only new native
primitive races/failure ordering. Builders own offline payout/rollback evidence
verifiers. Inventory completed read-only native/SQL recovery traces. No concurrent
heavy test processes remain from checkpoint 9; future checks stay centrally scheduled.


### 2026-09-24 — Native primitive applied verification and pending source recovery

Root implemented `restoreCanonicalRoot` in the production native owner: exact
expected-root CAS, independently loaded retained trie closure, fresh child epoch,
per-recovery-ID durable plan record and synchronous marker batch, drained-operation
and generation refusal, and post-marker failure fencing until restart. All native
operations are tracked across their full asynchronous lifetime. Root captured the
caller's plan before any await after the auditor found a mutable-alias race.
This parent-only primitive does not authenticate L1 or authorize SQL rollback.

Root also added a typed pending reconciliation result. Production preflight
recognizes orphan-associated pending/assigned headers without changing dependent
SQL; the source journal continues collecting canonical blocks while cache and
producers remain fenced. On restart, convergence refreshes disposition under
recovery authority before native preparation. The auditor identified and root
fixed the initial same-head restart ordering gap. Strict inverse/materialization
refusals remain intact; no funds are released by this pending path.

Checkpoint10 retained its noEmit failure (TS7030, explicit undefined return fixed).
Immutable integrated checkpoint11 identity
`130eda560900ec58c21f9bc9b51481d4c47c1e2b7dbd5fe34cce5cb60d666e70`:
Node22/pnpm9 `tsc --noEmit` PASS;
`vitest run tests/mpf-native-canonical-recovery.test.ts tests/mpf-native-owner-service.test.ts`
16/16 PASS (9 new actual-binary/LevelDB recovery cases +7 existing cases);
`vitest run tests/history-source-owner-streaming.test.ts`8/8 PASS (transport model).
All7,508 source hashes and bound dependency hashes remained unchanged. Exact
commands, logs and timings: `root-original-integration-checkpoint-11-receipts-run.json`.
New pending-owner PostgreSQL/restart fixture is authored but not included in this
checkpoint; it still requires centralized applied verification.

Independent checkpoint9 payout evidence verification PASS:22 signed receipts,
485 complete hash-checked creating transactions, exact12M payout,2 honest evaluator
controls and5 evaluator-only mutants. The verifier corrected reference-set ordering
and indexed already-retained source-point creator bytes without weakening exact
output/accounting/index assertions. It did not rerun UPLC or establish live behavior.
See `builders-checkpoint-9-payout-verification.json` and verifier handoff.

Required root docs check with Node22/pnpm10:
`pnpm --dir docs-site run check:links` PASS525 Markdown/MDX files, log
`root-resumption-docs-links-after-native-recovery.log`. Scoped new production
native and pending-reconciliation ESLint checks PASS. No deployment, service reset,
original implementation application or broad-suite completion is claimed.

### 2026-09-24 — Signed range loader, acceptance progression and current ownership

Root owns new `eventHistoryCanonicalCoverage.ts`: under recovery authority, it
checks the exact current binding/generation/checkpoint, follows the retained
activation-to-anchor predecessor chain, then the canonical post-anchor applications.
It verifies complete receipt digests/domains/indexes, exact endpoints/ancestry and
all transaction-ID/disposition rosters, including failed transactions. It excludes
retained orphan applications. These are freshly source-bound recovery bytes;
an imported archive or merely persisted cursor never supplies L1 authority.
Root added an exact-point, socket-authenticated recovery ledger capture for extra
protocol addresses, including renewed hub verification, needed to establish the
canonical native/SQL target independently of old local journal roots.

Builders supplied the isolated signed-coverage classifier and27 authored fixtures.
Its result is inclusion, pending, or covered absence based on the exact signed
body hash/TTL, whole contiguous roster and required descendant depth. Queue absence
and elapsed wall-clock time are never release authority. Root still must establish
the earliest-inclusion boundary, recheck the source generation, persist the exact
plan, and implement authorized journal/SQL/native disposition. These transitions
are not yet implemented; the new components do not release funds.

Immutable integrated checkpoint12 retained the fixture timestamp-field noEmit
failure. Integration corrected it to the actual schema enum. Checkpoint13 identity
`c8a047ab00dced1dd10fb89c73e1a268e53d0aec3790be8fcbb276fd40354c27`:
noEmit PASS and complete production streaming D/W lifecycle1/1 PASS after the new
source-owner/native changes. Pending-owner fixture failed only on expected point
shape after its pending/restart assertions: runtime includes height. The fixture
now checks the exact full current point; assertions were not dropped.
Checkpoint14 retained four noEmit errors in the new evidence component/fixture;
source Array.isArray narrowing and closed Effect environments are being corrected
by their existing owners. No runtime pass for new coverage/pending fixtures yet.

The actual rollback acceptance fixture now uses a genuine deposit header commit,
canonical confirmation and native local finalization before spending. After
rollback it requires unchanged signed intent, accepted dependent receipt/native
root and closed producer/cache gates; it streams actual fork intervals past the
signed TTL plus configured finality before demanding repair. Fresh incarnation
must be genuinely recommitted before the queued signed L2 transaction is accepted
again. This positive remains blocked by the missing production disposition; no
synthetic assignment or unrelated-funding scenario replaced it. Reference-input,
multiple-descendant and restart extension follows this first real positive.

Root's next production composition must connect complete current-branch signed
coverage plus a bound state-queue snapshot to a durable recovery plan, authenticate
the restored baseline, execute native CAS outside SQL, then atomically dispose of
journal assignments and invert dependent SQL before cache reload/Ready. On restart,
a native owner must be opened for this controlled repair without replaying an
orphaned journal first. Multi-header/published-L2/merged confirmed-ledger cases also
need complete full-output preimages; the existing delta stores spent keys only.
These remain explicit implementation gates, not verified behavior.

Live preparation is artifact-only. Existing Desktop/WSL daemon is stopped;
starting it may auto-resume the legacy operator (`restart: always`). Root controls
startup containment and deployment, preserving provider DBs/Preprod markers. New
`live-readiness-root-plan.md`, read-only preflight script and current observations
record exact commands and prerequisites. No service, reset, deployment, SQL or live
transaction has been performed. Final original application and broad required
suites remain open; the single active checklist above remains authoritative.


### 2026-09-24 — Current narrow acceptance checkpoint 15 PASS

All current narrow failures are resolved without dropping assertions. Immutable
integrated checkpoint15 source identity:
`97c58cc1ff1942c9049fe3939f80009e44d77ad95295eac971dac1e11a22686f`.
Root ran centrally with pinned Node22.22.2/pnpm9.15.4:

- `pnpm exec tsc --noEmit` — PASS.
- `pnpm exec vitest run tests/signed-intent-canonical-coverage.test.ts tests/event-history-canonical-coverage.test.ts tests/history-source-owner-pending-recovery.test.ts`
  —29/29 PASS across3 files. Includes27 signed-body/roster cases, one actual SQL
  source-bound activation/branch/corruption case, and one complete pending rollback,
  continued ingestion, same-head restart, current-frontier readiness/source-loss case.
- Source7,513 files and all three bound dependency sets unchanged before/after.
- Root docs command `pnpm --dir docs-site run check:links` — PASS527 Markdown/MDX.

Exact commands, timestamps and hashes are retained in
`root-original-integration-checkpoint-15-receipts-run.json`; test log SHA256
`1242bfb422090f1151c290ce2f8fbb9026255a1f069531e2bd8b1a31c43e1e70`.
Root independently parsed the live-preflight script without executing environmental
commands. No heavy test job remains from checkpoint15. Earlier failed snapshots
and logs are retained, not relabeled as passing.

This is a verified foundation milestone, not completion of dependent L2 rollback.
The revised actual orphan-funded positive fixture is frozen in node staging at
`268f20cf414704793fd4039baba6f341944814cd4a1f76ef14c61c4ace7437aa` and is not
in checkpoint15. Root must still implement durable signed-header disposition and
atomic native/SQL/cache recovery before it can pass. The strict gate currently
keeps pending headers and funds fenced. New exact-point recovery capture is
compiled, but its use to authorize a state-queue/native recovery target remains
unverified. Original implementation application, required broad/applied fit gates,
ABI readiness, full adversarial matrix and one-deployment live acceptance remain
open. No unrelated proof gap or live gate is closed by these results.

### 2026-09-24 — Production signed-header recovery implementation in progress

Root added a durable SQL/native recovery plan and the production pending-repair
hook. The operation ID binds the immutable header, signed bytes, incarnation-bound
journal digest and native source/target roots; refreshed canonical evidence can
resume the same operation after an interrupted native CAS. Applied SQL receipts
are retained. The source owner repeats bounded reconciliation after pending
preparation and never uses callback success alone to publish Ready.

The initial production caller handles a deposit-only, locally finalized header
whose admissions are all orphaned, whose exact original confirmed queue root is
freshly restored at the current checkpoint, and whose signed transaction is absent
through its complete eligible range, finite TTL and configured descendant depth.
It verifies the retained confirmed SQL root, persists intent, restores native bytes,
then atomically reverses unpublished dependent receipts, clears the exact retired
assignment, retains signed journal/membership evidence and reconciles projection.
Published L2/merged baselines and broader signed-submission shapes remain fenced.
This implementation is staged and NOT yet typechecked or accepted end to end.

Existing agents have concrete non-overlapping follow-ons: builders own signed
coverage boundary fixtures and durable plan SQL fixtures; rollback owner owns exact
historical state-queue capture and pending-hook/producer-drain fixtures; integration
owner supplied the isolated strict SDK queue-target validator and22 cases; live
owner supplied offline trusted genesis pin provenance and root-run containment
commands. Root owns all production/SQL/runtime edits and final application.
No broad inventory/audit restarted. Immutable checkpoint16 is being prepared for
central narrow verification while independent fixture work continues separately.

Live scripts are preparation only. Normal Desktop startup cannot guarantee a
zero-execution window for the old restart:always producer before the daemon API
responds. No services/configuration/deployments/live transactions changed. The
independently derived Preprod genesis candidate is retained with image/config
provenance; actual running-provider comparison remains a live gate.

### 2026-09-24 — Integrated checkpoint17 narrow PASS; actual journey still open

Immutable integrated source identity
`acd5a76995a903a00cb57cb75965bf21ea2297da4647079d8eaa59744631dba6`:
root ran pinned Node22/pnpm9 `pnpm exec tsc --noEmit` PASS and
`pnpm exec vitest run tests/signed-intent-canonical-coverage.test.ts tests/history-recovery-state-queue.test.ts tests/history-source-owner-pending-recovery.test.ts`
PASS62/62 (39 signed boundary/expiry,22 strict queue-target,1 actual owner hook/drain/
restart case). Source7,518 files and all bound dependency sets unchanged.
Logs/results: `root-original-integration-checkpoint-17-receipts-run.json`;
coverage log SHA `b2a03b15338e62e1c177a1ae9ac9c3f48b43cfdcc97bada0fd13b570051563b5`.

Then, serially on that SAME immutable identity,
`pnpm exec vitest run tests/l1-event-history-owner-rollback-emulator.test.ts`
FAIL at first genuine commit: its historical commit end1788741024000 predates
scheduler refresh window1788744963000. No recovery-pass claim. Fixture owner is
correcting actual timing/source setup while preserving production timing checks.
Evidence retained in `root-original-integration-checkpoint-17-actual-rollback-run.json`
and its rollback evidence/log; no heavy job remains from these two runs.

A bounded review found a crash/source-supersession safety defect before further
acceptance: after native restore but before SQL disposition, re-canonicalized origins
could evade orphan-only pending detection. Root now makes every durable prepared
recovery plan independently gate Ready, even with no current orphan. This new guard
is staging-only after checkpoint17; component regression is being authored.
Resolving a changed branch's outstanding plan remains an explicit liveness gate,
not authority to cancel/erase its native obligation. Full production interruption
and restart evidence is still required; isolated native/SQL/hook passes do not
substitute for that journey.

Root independently reran the offline image/config/provenance-linked Preprod pin
derivation, exit0 (`root-preprod-pin-derivation.log`). Expected candidate remains
`f21798e4d0efc1747999aaa74257d08e8f24a268b800e7d18210557e52f7a759`.
Actual running-provider identity/result comparison and all live acceptance remain
unperformed. No services or runtime configuration were changed.

### 2026-09-24 — Durable SQL checkpoint18 PASS; first actual worker prerequisite

Checkpoint18 immutable source identity
`5d7f480ee587de7689a2f20658a62acba994d014257eade5da5913aa9eb42d52`:
pinned `pnpm exec tsc --noEmit` PASS, followed serially by
`pnpm exec vitest run tests/event-history-recovery-plans.test.ts tests/event-history-ledger-repair.test.ts`
PASS32/32 (20 durable plan cases and12 unchanged inverse-repair cases).
Includes generation0, immutable plan/caller snapshots, refreshed evidence,
prepared obligation despite canonical origins, authority/generation rejection,
transactional repair failure and idempotence. Complete source7,519 files and all
three dependency sets unchanged. Plan log SHA
`12db86326b12285b6a9ed2b57262291e682d090b3c0c66fa57a91dfd597d6927`.

Root added bounded Globals publication after successful SQL commit in the same
cancellation mask: exact confirmed queue/boundary, submission/local-finalization
flags, queue count and stale wake entries. Target decoding/serialization happens
before native mutation. Source supersession can keep readiness closed but cannot
skip the postcommit in-memory refresh. Process restart rebuilds these refs.
The bounded review found no further concrete defect; runtime interruption evidence
remains open. Fixture now asserts these globals against fresh queue state.

Checkpoint19 identity
`c970d2bc0cb71dccd71b3f39ad19aae0d52a5d13812dccc8340bbd77adc95b26`:
noEmit PASS. The corrected actual rollback journey now passes its original real
signed deposit header commitment/confirmation/native finalization, then FAILS at
first L2 validation because the immutable source snapshot excludes compiled
`dist/validation.js`. Failed log SHA
`1c1b8d76728b4505ae929aab7afe2144506d15bfbca74db251a2b21bf52c7d1d`.
This is not an accepted dependent rollback or permission to substitute the worker.
Integration owner is preparing an isolated declared-toolchain core/validation/
actual-worker build; root will execute centrally, bind every output to exact source
and dependencies, and include it before sealing the next immutable snapshot.

Root independently ran actual lossless genesis helper parity against checkpoint19,
exit0, preserving a separate `root-preprod-pin-node-parity.json`; expected pin and
2808 canonical bytes agree with the offline publisher/config derivation. Original
agent parity artifact retained. Required root docs command after these changes
PASS530 Markdown/MDX (`root-resumption-docs-links-after-dependent-recovery.log`).
No service/deployment/live action or original implementation application performed.

### Recovery checkpoint21 failure and checkpoint22 schema correction (2026-09-24)

Checkpoint21 source identity `17e5f2f6efbe24e4e089542188a160dfbdc7f5c8bb3128755856f046bfc07624`
passed node noEmit. Actual-owner acceptance failed after the genuine compiled
validation worker accepted the orphan-funded spend and native recovery restored
R0. Failure diagnostics retained a prepared recovery plan and Recovering authority;
SQL inverse did not commit. Log SHA256
`86033d2affc591ec1f372182c57e57025d1a75ccfcf042bfe4882f256837b5ea`.

Root identified incompatible archived-membership foreign keys to live event rows.
Archived signed headers must survive live-row removal and later public-ID reuse.
Staging and integration now bind member IDs to retained binding/incarnation/event
triples; pair MATCH FULL and nullable-association checks remain. This is not release
authority: signed coverage, canonical queue, native CAS and owned SQL gates remain.
Checkpoint22 contains 7,932 files including the 413 verified normal runtime outputs;
identity `7a64e28551a9f6de08edf34ca7d4a7798e35603e265282594d62a49dcecf6f61`.
Central noEmit and actual-owner rerun are active. Retired-member PostgreSQL regression
and actual reference/descendant fixture work continue separately. No original
implementation application, deployment or live acceptance occurred. Full recovery,
ABI readiness, original integration and live gates remain open.

Checkpoint22 terminal result: noEmit PASS; actual rollback FAIL during the fresh
recommit fixture assertion (native root already reflected the submitted header,
while fork synchronize still expected R0). The native/SQL inverse, durable applied
plan, exact admission/payload requeue, cache equality and recovered Globals checks
were reached and passed before this failure. Log SHA256
`9cdbe4940aeea7dd896a5e6358e16cf6aa4aaadc45c5beb76759fd534b61ef55`;
source and all dependency manifests unchanged. Fixture owner is correcting the
expected-root transition without dropping assertions. Full positive recovery is
not yet claimed. Read-only exact schema review found no integrity defect; existing
DBs with previous migration1 checksum refuse migration and require an explicit
deployment/upgrade decision. Required docs links check PASS (531 files), log
`root-resumption-docs-links-after-retained-membership.log`.

Checkpoint23 central run started on immutable identity
`329c9ab7b5090efc63dc4143863f0d234a49c5be861adcb08af5b232c7de3702`
(7,933 files). Root independently reviewed/applied the fixture-only expectation
patch: derive the native expectation from the retained signed transaction's exact
queue header, verify body hash/NFT/header hash/root against journal, then preserve
the native diagnostic equality before synchronization. Included ten PostgreSQL
retained-member cases (both event kinds); root caught and agent corrected the
new recovery-plan FK table omission in test cleanup. noEmit PASS; actual-owner
and retained-membership runtime checks active serially in one fork. Broader
reference/descendant fixtures and cancellation-across-COMMIT component coverage
continue in separate owned staging files. No production migration reset or live
service change.

### Production dependent recovery first positive — checkpoint23

Central noEmit PASS and runtime11/11 PASS (one actual-owner rollback + ten retained
member SQL cases). Command: pinned Node22/pnpm9 `exec vitest run
tests/l1-event-history-owner-rollback-emulator.test.ts
tests/event-history-retired-membership.test.ts`; runtime log SHA256
`de70d7786cdc5463f316785ecfae8a8ec2ad7be06dc683fb43899756f9f5305b`.
All source/dependency hashes unchanged. The emitted journey reaches complete:
real original signed header/native finalization, actual dependent L2 acceptance,
signed-TTL and finality fencing, retained native restoration and atomic SQL inverse,
cache/Globals agreement before Ready, fresh same-public-ID incarnation, real
recommit and reacceptance of the same signed L2 transaction. Modeled source fork
and emulator ledger remain explicit limitations. Independent offline verifier
initially failed due mixed ESM/CommonJS Constr instances; failed log retained,
artifact import fix delegated with every assertion preserved.

Stable milestone checkpoint24 is active (7,934 files; source identity
`3591688845574c778528b7ed799c6635972e53b5adbb5a697d2d392c25731a1c`):
node noEmit, real-SQL recovery cancellation/negative cases plus full production
D/W streaming journey, required `test:tx-prep:node` and
`test:tx-prep:emulator` scheduled serially. Three coordinator component cases
replace only native RPC, not SQL/authority/cache; they do not establish real
process-crash or exact post-COMMIT supersession acceptance.

Root recovery remains incomplete for other signed/non-finalized journal states,
multi-header or mixed canonical/orphan published members, merged immutable
confirmed-ledger reconstruction, changed-branch prepared-plan disposition and
below-anchor recovery. Reference-input/descendant/restart acceptance remains open.
Original checkout implementation and one verified live deployment are still gates;
no service mutation, state reset, deployment or ABI freeze occurred.

Root independently reran checkpoint23 offline verifier PASS, preserving failed
initial verifier log. Verifier fixes bind the ledger spend outref to decoded
public event transactionId/outputIndex (matching production deposits.toLedgerEntry),
while retaining both distinct hashed ledger_tx_id/tx_id assertions. No proof,
identity, original-Value, signature or native/SQL checks were removed. Evidence
SHA256 `79624f39ea14bce341a030b77ba601ad318e94bfed36e6bf6d2c001b9254b618`;
report `root-original-integration-checkpoint-23-rollback-verified.json`.

Checkpoint24 noEmit PASS; production D/W plus ordering4/4 PASS (log
`ace195710c0218aca282abf3b9f59727c6ec6f604923f3ee13c5a77dd7671283`);
required node125/125 PASS (log
`aacfc42479d50940cd6226416b291c174ba46ed036bf7e83a0bce4a9e5528207`).
Required emulator still active and has a concrete merge-payout fixture failure;
exact diagnosis delegated, no broad-suite pass claimed.

Descendant fixture captured independently in checkpoint25 (7,936 files, identity
`5c323772b8f825406afe2ebabb70c659c768f19a2a20d7cbe1fe96c22af00f30`).
Central execution queued after cp24 finishes; includes unchanged original spend
through extracted helper, genuine signed child rollback/reacceptance, and fresh
D/W evidence emission (cp24 D/W passed but its optional JSON env was unset).
Reference fixture continues in staging while frozen descendant is tested.

Checkpoint24 terminal required emulator result:63/64 PASS, one model-fixture
failure (`event_history_authority`: Current authenticated history producer is
required; Missing producer permit). Log SHA256
`ad5e891e7ad7b918979742d46d1ccb7744d96108b88be41b757f39421296fd2b`.
All source/dependency hashes unchanged. Agent corrected only direct modeled
markAccepted provisioning in merge-payout test using its existing explicit fixture
context; no production ownership bypass or assertion changes. Central rerun pending.
Checkpoint25 now active after cp24 terminal; source remains isolated.

Root next production slice (STAGING ONLY, not integrated/runtime verified):
`prepareRetainedNativeHistoryRecoveryPlan` fixes the interface for choosing drained
native R0/R1 and preserving a matching prepared operation across prior native CAS
and newer evidence/generation. Actual caller stages observed deposit-only recovery
before any local-finalization job/DA state. Both signed absence and exact fresh
queue/confirmed SQL baseline remain mandatory. It refuses any other native root,
keeps partial finalization fenced, and atomically CASes exact prior header status
to abandoned before inverse (failure rolls it back). Existing journal/member
archives remain; the native operation persists before native mutation. Root ESLint
and formatting PASS. Builders added13 realSQL selection/retry/refusal cases while
preserving the previous20; runtime pending. Separate actual observed fixture is
assigned, with promoted R1 first and response-loss R0 explicitly still a gate.

### User-directed delivery checkpoint boundary (2026-09-24)

User explicitly requested finishing the current observed-header recovery case,
then no further recovery variants. Root is consolidating candidate, node staging
and preserved original into a concrete delivery checkpoint, with remaining full
acceptance blockers enumerated separately. No R0 response-loss or additional
recovery implementation will be started. Existing descendant/reference/restart
fixtures are preserved; their current verification results will be reported.

Checkpoint25 noEmit and original-spend/descendant/full-DW journeys3/3 PASS;
source/dependencies unchanged. Root independent streaming owner and raw-receipt
verification PASS, plus repeated original-spend rollback verifier PASS.
Runtime log `ea8d146e5620d4b1cf31b4e5443a21a943740fdb73506b91c916e0b7885c08f6`.
Checkpoint26 stopped at typecheck: payout model helper signature omitted BatchSql
in its context; restored original scoped layers with local explicit fixture permit,
no cast or assertion change. Failed log retained
`c75e5334897b663e335f7c54c2ff8183c145c81102757b7006a5664a262ab382`.
Checkpoint27 identity `20d34fb70c3ed97462443d1f2cf21dcd67b605a6118e88cc9405b67b467b009e`
(7,937 files) passed noEmit. Current observed fixture reached recovery/retained
signed evidence/root/SQL/cache/Globals assertions then failed at final WriteBehind
service read because h.command tried to synchronize the sealed original recorder.
Owner is fixing only that final read, preserving depth/Ready assertions. Remaining
central checks continue on the immutable snapshot.

Delivery preview against cp27: sole source/evidence conflict is the preexisting
transition-trace workflow fit ledger. Original and candidate versions will remain
separately retained; current measurements cannot be text-merged. Ninety-five other
conflicts are regenerated dist outputs, not divergent authored source. Root will
archive prior outputs before applying verified replacements. Original progress log
and unrelated work remain preserved. A different preexisting native executable
will be retained in backup before installing the verified checkpoint executable;
no native durable data or environment/deployment state will be reset.

### Delivery checkpoint28 applied (2026-09-24)

Current observed-header recovery passed1/1; its depth and Ready assertions are
unchanged. Recovery development is stopped per the user's delivery boundary.
Checkpoint28 identity `c3ede1c6b0020e1e8c006d1fd0320c0116e435520bee1ac07563806a5e20a776`
contains7,937 files. Root applied435 source paths,413 verified generated outputs
and one verified native executable to the original checkout, preserving7,087 paths
and the original conflicting fit ledger, with both versions archived separately.
No source conflict was silently overwritten; no Git stage/commit or state reset.
Independent full-manifest rehash found zero mismatches and unchanged HEAD/index:
`root-delivery-checkpoint-28-independent-application-verification.json`.
Application report and prior-byte backups are retained alongside it. Required
node125/125 and noEmit passed; required emulator verification continues centrally.
The reference fixture still fails on duplicate admission before reference-only
recovery and is explicitly deferred, not weakened or removed.

Checkpoint28 terminal verification PASSED: noEmit, current observed1/1, required
node125/125 and required emulator64/64. No source/dependency hash changes.
Emulator log SHA256 `b09994b04608b06aa74a1de2502eb210bbc56330be17aed7deccd885a976385c`.
The delivery-only integration owner is aligning original installed package aliases
and generated wrappers with the verified candidate dependency set; source, locks,
package stores and recovery implementation remain frozen. Root will verify the
original checkout's typecheck/build after this bounded environment application.

### Delivered original-checkout verification (2026-09-24)

Root independently ran pinned Node22.22.2/pnpm9.15.4 `exec tsc --noEmit` and
`run build` in original `demo/midgard-node`: both exit0. The normal build covers
node CLI, all worker bundles and declarations; previous node dist files are backed
up. Build log SHA256 `d418ebd8ba9dfa3a81e9781e248fadbb44bc5cda7a96d71643bf582f6a849d84`.
Dependency owner applied245 aliases/107 wrappers with backups;42 workspace aliases
remain original-local. Static declared graph650 roots: no missing required deps or
old-store singleton split. Candidate package store remains an installed dependency;
keep workspaces until separately verifying a standard frozen install. No package
stores, source, lockfile or service state changed during dependency realization.
All implementation agents are idle; no more recovery variants are underway.
Full original acceptance remains incomplete for the seven explicit gate groups in
`event-history-delivery-checkpoint.md`; current checkpoint is uncommitted and
keeps the user's Git index/history intact.

Required root command `pnpm --dir docs-site run check:links` PASS (exit0),543
Markdown/MDX files. Pinned Node22.22.2/docs pnpm10.11.0; exact argv and log hash
retained in `root-delivery-docs-links.json`. No live services, deployment or
transactions were started. Checkpoint delivery is complete; full protocol
acceptance is explicitly not complete.

### Post-delivery acceptance continuation29 (2026-09-24)

Previous goal turn classified as progress: checkpoint28 source applied and
independently verified, original dependency alignment/typecheck/build completed,
required node125/125 and emulator64/64 plus docs543 passed. Full goal remains
incomplete. The user boundary forbidding further recovery variants remains in
force. Root now takes the available acceptance-only step: fresh pinned normal
Aiken build and installed transition-trace lifecycle/fit verification against a
copy of checkpoint28. Only the suite's generated measurement ledger/blueprint are
permitted output changes; all implementation inputs remain immutable. Old ledger
versions stay archived; no replacement claim until the complete suite passes.
Auditor reviews only that suite's concrete assertion coverage. No service,
deployment, DB migration or new recovery implementation is authorized by this
continuation plan beyond existing scope.

Acceptance29 terminal evidence: fresh pinned `aiken build --env testnet` PASS,
1,162 validator entries and exact delivered blueprint SHA256 unchanged; proof
package noEmit PASS; complete installed transition-trace suite13/13 PASS. Only
the expected generated fit ledger changed; no implementation input changes.
A separate pinned Aiken invocation selected all14 history test modules and
verified the exact195 expected names:195 passed,0 failed (all unit tests).
Root independently decoded1,340 signed receipts and matched body hashes, fees,
redeemer budgets and ledger entries. Original conflicting ledger was atomically
replaced only after checking its prior hash, with both prior versions retained.
New ledger SHA256 `66acac392bb0da73a66afb89e56ab532c4361362a05e53d73fefe6d49b117642`.
Run/evidence/application records are `root-delivery-acceptance-29-*.json`.

The bounded auditor review prevents overclaiming: this installed suite lacks
arbitrary absent IDs, substituted content, honest eligible withdrawals, direct
wrongful-challenge refusal and retired-ID reuse; its real emulator submissions
still use fixture catalogue/binding/header assumptions. Timing covers selected
start/end boundaries. Original deposit Value is checked as90M from95M total minus
5M structural funds, but not multiassets. Existing source tests were neither
weakened nor altered. No recovery variants, service operations or live acceptance
were added. Remaining gates stay in the single active checklist and delivery report.

### User-authorized checkpoint commit and push (2026-09-24)

Staged the combined source/cleanup/docs/generated blueprint and vendored source
receipts; excluded the untracked `.env` backup and ignored local artifacts.
Installed and ran the normal pre-commit hooks without bypasses: PASS. Formatting
changed27 files, including one sorted import. Fresh isolated pinned Aiken normal
build after formatting reproduced the delivered blueprint exactly. Original
node noEmit, required node125/125 and watcher original-asset projection tests
passed. Expected unified-patch blank context whitespace is retained in the
integrity-bound vendored patch; all Aiken source trailing whitespace normalized.
Commit message explicitly records incomplete full acceptance and references the
delivery report. No recovery variants, deployment or live transactions added.
