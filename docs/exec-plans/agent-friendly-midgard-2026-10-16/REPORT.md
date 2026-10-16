# Agent-friendly Midgard: priorities by impact and complexity

Date: 2026-10-16. Status: implementation authorized after the scope correction
below. Deliver the complete plan and coordinate the push to
`colll78/canonical-v1-watcher-l1-source-checkpoint` with its active lead.

The strongest recurring pattern is **leaving a domain decision in several
callers, then repairing each caller separately**. Prefer one enforced owner
for that decision. Deployment identity is the highest-priority architectural
improvement. The wallet item is a small hardening fix to the existing contract;
verification should exercise the installed product and consume declared prerequisites.

## Priorities

Impact follows correctness, safety, liveness, performance, then convenience.
Complexity includes migration and verification: low means one owner with no
durable-format change; medium crosses several callers/packages; high changes
lifecycle ownership or needs broad integration proof. These are estimates,
not measured implementation durations.

| Architectural priority | Change                                                                   | Impact      | Complexity                              | Smallest useful delivery                                                                                                    |
| ---------------------- | ------------------------------------------------------------------------ | ----------- | --------------------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| 1                      | Remove deployment-map mirrors; make descriptors complete by construction | High        | Low for mirrors; medium for descriptors | Import authoritative maps, then prove one complete family through construction, restoration and publication                 |
| 2                      | Exercise installed package boundaries in ordinary CI                     | High        | Medium                                  | Extend existing boundary checks to real package resolution, advertised runtime conditions and production dependency closure |
| 3                      | Give each package one executable test/build declaration                  | High        | Medium–high                             | Pilot watcher and node-tools, then migrate affected packages off shell/source guessing                                      |
| 4                      | Derive container build inputs from the package graph                     | Medium–high | Medium                                  | Replace the three images' manual package build lists, preserving build-time/native dependencies                             |

**Immediate hardening (former #1): enforce the existing input-selection contract
inside the shared completion helper.** Impact is bounded defensive correctness;
complexity is low. Existing callers already guard empty explicit pools, so the
evidence does not establish a currently unguarded production caller. Implement
this small fix first, but do not describe it as the highest-impact architectural
improvement or expand it into wallet authority types, role services, signing or
submission changes.

Deployment metadata has the strongest remaining architectural leverage: it
removes synchronized representations across several consumers and can make
omissions fail mechanically. Installed boundaries are the next verification
investment. Container derivation is preventive: the census found no current
missing runtime builds.

## Evidence and scope

The recommendation baseline is active integration
`43dc8e42b7706fc79978cfe1d4b4f5ec22144189`, compared with checkout
`17fdffd9bb04a529c29fdc729aebab64aeb63d28`. This avoids recommending work already
present on integration. Neither ref's merge/release readiness was assessed.

- Inventoried 703 non-merge commits reachable from checkout HEAD dated
  September 16–October 16. Inspected the specific incident diffs below,
  supplementing them with explicitly identified integration commits. The
  inventory is not a defect count or an attribution of authorship.
- Inventoried 161 top-level project transcript files; 80 contained records in
  that date window. After metadata checks and filtering, retained 598 unique
  human messages across 73 sessions. Manually selected 42 excerpts across 24
  sessions, coding 27 distinct interventions. Two identified desktop chats
  supplied another 19 human messages. Generated summaries, tool output,
  sidechains and private reasoning were excluded.
- This is a purposive sample, not an exhaustive frequency study. The strongest
  evidence is in the TypeScript runtime, SDK and contributor workflow. It is
  not a new validator/security audit or a comprehensive Haskell/spec review.
- [Selected steering index](evidence/selected-steering.json) retains source
  session, timestamp, line and message hash. Private quotations and whole
  transcripts are omitted from the published artifacts.
  [Executable probes](probes.mjs) read the frozen Git sources; their
  [results](evidence/design-probes.json) distinguish observations from proposals.

The selected intervention counts are: obsolete alternatives 4; incorrect
protocol boundaries 4; unrealistic/expensive verification 4; brittle tests or
invented equivalence requirements 3; wallet/economic policy reuse 3; retry
classification 2; build/artifact ownership 2; script storage/execution separation
2; upstream rather than vendored fixes 2; decoder/domain admission 1. These
counts overlap neither each other nor repeated messages for the same event.
They describe user steering, **not how many current defects remain**.

The steering is consistent: remove obsolete representations at cutover; keep
policy with its domain owner; test observable behavior; make realistic checks
cheap; preserve semantic coverage when optimizing tests. The October 13 desktop
review correction also retained independent two-pass consensus review while
batching related tooling/fixtures and rechecking affected interfaces. The plan
therefore reduces review burden without waiving consensus gates.

## Immediate hardening: the existing input-selection contract

The existing mechanisms already restrict available funding inputs: the node
passes its current wallet view as `presetWalletInputs`, other paths install
wallet overrides or restricted wallet APIs, and the proof owner enforces its
reserved action. These responsibilities stay with their current owners.

The concrete residual is in
[`tx-completion.ts`](https://github.com/Anastasia-Labs/midgard/blob/43dc8e42b7706fc79978cfe1d4b4f5ec22144189/demo/midgard-sdk/src/tx-completion.ts#L22):
`completeTxWithLocalUPLCEvalProgram` accepts an optional explicit wallet-input
pool but relies on callers to reject an empty one. Move that refusal into the
helper's Effect error path, before calling Lucid. Preserve the caller's error
mapping, local evaluation, omitted-input behavior and nonempty preset behavior.
Existing earlier refusals may retain transaction-specific diagnostics.

An empty Lucid preset falls back to `wallet.getUtxos()`. That read honors an
installed override or restricted wallet API; it reaches the provider only when
the selected wallet does so. Therefore an empty preset does not universally
bypass wallet restrictions. For this funding helper, an explicitly empty pool
must be refused rather than interpreted as permission to discover another pool.

The historical node reserved-input fix
[4e12fc832](https://github.com/Anastasia-Labs/midgard/commit/4e12fc832424f9b689614de3d8eeaf3dcecf6461)
and availability fresh-collateral fix
[da5736a78](https://github.com/Anastasia-Labs/midgard/commit/da5736a78850a82b296ce133249ce6e4bef60d31)
are already implemented. They explain why the existing policies differ; they
are not evidence that those policies need redesigning in this task.

Delivery is one shared-helper guard, focused regression coverage, and corrected
comments. No new wallet capability system, generic policy selector, signing
boundary, reservation owner or submission lifecycle is part of this item.
Eligible inputs remain distinct from mandatory collected inputs. Freshness,
authority and unknown-submit reconciliation remain with their existing owners.

Acceptance: show the regression red before the fix; empty explicit inputs must
fail through the supplied error mapper before completion or wallet discovery;
a nonempty explicit pool must complete without wallet discovery; omitted inputs
must retain wallet discovery. Run the applicable transaction-preparation and
package checks, recording every failure or unavailable prerequisite. Preserve
existing transaction-specific refusal behavior.

## 1. Derive deployment views from complete metadata

Two distinct fixed failures establish the cost:
[514e8de8e](https://github.com/Anastasia-Labs/midgard/commit/514e8de8e6339b5dd0b7306ca4cd67fcd5154aa7)
restored omitted min-ADA yields after operator registration failed;
[f5f112d17](https://github.com/Anastasia-Labs/midgard/commit/f5f112d172658818be1ff948e7aa989de03c54b2)
replaced placeholder validation-trace members that made 193 scripts appear
missing. Both bugs are fixed.

Frozen integration still maintains identical core/node maps of **522 roles**
and core/watcher maps of **52 categories**. Both comparisons have zero
differences. Its 41-name linear-family array uses a type constraint that checks
listed names but not completeness. Removing `minAda` still compiles; a total
mapped descriptor reports TS1360; the complete descriptor compiles.

Delivery: remove exact mirrors first. Core remains the owner of static
deployment identities; SDK owns construction recipes; node/watcher derive
their views. Pilot minAda through construction, manifest restore and publication
before migrating all families. One logical catalogue may use separate family
files; runtime registration and a new generator are unnecessary unless the
pilot shows they reduce work.

Required members must be checked against an **independent protocol/blueprint
contract universe**. Inferring the expected universe from the descriptor under
test merely makes omissions consistent. Preserve explicit manifest and
publication ordering, including cross-family interleaving. Keep callbacks out
of core and retain existing workflow dispatch ownership.

Acceptance: a missing historical yield must fail even when removed from all
derived views; swapping two valid members must fail identity/order checks.
Compare applied hashes, catalogue/role IDs, serialized manifests and publication
order. Preserve existing round-trip/deployment gates and any applicable golden,
ledger, Aiken and emulator checks. The type experiment alone does not establish
manifest correctness.

## 2. Test the package contract a real consumer receives

Distinct fixed failures: missing watcher export/build entry
[677691a06](https://github.com/Anastasia-Labs/midgard/commit/677691a069633e0d128faf3f01d3bcaab22ddcf2);
a lint-only TypeScript scanner entering the settlement worker runtime
[4da0943dd](https://github.com/Anastasia-Labs/midgard/commit/4da0943dd5f45dc4f5bb4ec33e6268b930604711);
and externalized runtime dependencies classified as development-only
[d4fea9e47](https://github.com/Anastasia-Labs/midgard/commit/d4fea9e47263e50090d49141ca7e2361f4515159)
(integration).

The existing owner is `checkBoundaries` in
[discovery.mjs](../../../scripts/contrib/discovery.mjs). It imports absolute
`.import` output paths and runs bins with `--help`. Across 12 package manifests,
109 runtime export entries include **81 advertised require branches** that this
selection does not exercise, plus five default/string entries it skips. Some
are test-support exports and need classification, not indiscriminate execution.
The actual package boundary command was not found in ordinary preflight/CI
scheduling; its guard tests are scheduled.

The experiment installs a tiny package into a disposable consumer directory:
package-specifier ESM succeeds while advertised CJS deliberately throws. This
demonstrates the coverage distinction, **not a broken current Midgard CJS build**.

Delivery: extend `contrib boundary` to resolve packed/installed package
specifiers and subpaths under supported Node conditions, with production
dependencies only. Exercise real bins and retained workers, retaining the
existing settlement graph/heap guard. Register execution in existing preflight
and CI IDs. No second smoke runner or export registry.

Acceptance: missing subpath, dev-only external dependency, broken CJS and
runtime compiler-leak controls must fail. The correct installed consumers must
start. Preserve the distinction between exported testing helpers and supported
production entrypoints; do not waive an advertised condition merely to pass.

## 3. Declare package tasks once

Fixed incidents include watcher environment selection
[16f5a1ea6](https://github.com/Anastasia-Labs/midgard/commit/16f5a1ea69fbbdd392b26b76b5fe0e035b9119ff),
committee tests missing their compiled output
[b23fdd76c](https://github.com/Anastasia-Labs/midgard/commit/b23fdd76c0ddf48ff26f9f6135148f5bea85595d),
dropped package flags/environment
[bed61e9c5](https://github.com/Anastasia-Labs/midgard/commit/bed61e9c5735222cb1cdd5120489cdf12be7a8df),
and node-tools missing its own CLI build
[b0719b263](https://github.com/Anastasia-Labs/midgard/commit/b0719b26366fa22eed2e0cca6bb13f5f1abb454f)
(last two in integration).

Current integration infers preparation from restricted shell parsing and test
source. Executing the exact `testRunningOwnDist` function finds
`"../dist/cli.js"` but misses equivalent `join("..", "dist", "cli.js")`.
This is a selector limitation, not proof that a current shipped test fails.

Delivery: one package-owned validated task declaration describes argv,
environment, compiled entrypoint prerequisites, blueprint/native/database needs
and outputs. Both package scripts and contrib execute that owner. Replace the
redundant shell recipe; do not add a parallel prerequisite registry. Reuse
existing graph, build identity, tracing, resources and receipts.

Pilot watcher and node-tools because they cover distinct environment and
compiled-CLI requirements. Compare accepted argv/environment and prerequisite
sets, then execute their real test paths from absent outputs. Computed path
spelling must no longer affect preparation. Preserve fail-closed unknown-input
handling and meaningful selected-test counts before broad migration.

## 4. Derive image build plans

Fixed image failures:
[8eeda7e5f](https://github.com/Anastasia-Labs/midgard/commit/8eeda7e5f1eed8993a7318c549b50e21a8000dc6)
omitted SDK dependencies after a prebuild change;
[a5c151a96](https://github.com/Anastasia-Labs/midgard/commit/a5c151a96b69ec0a875388c9b24db09df0ce2427)
omitted watcher output after a new runtime import.

The frozen three-image census found **40 manual build declarations and zero
missing workspace runtime builds**. Extra declarations may be required bundled
or build-time dependencies; the census does not justify deleting them.

Delivery: reuse the dependency/recipe owner behind `runtimeBuildClosure`, adding
explicit build-time edges where needed. Produce Docker-consumable copy/build
inputs, preserving native stages and cache boundaries. This can follow the task
pilot to avoid creating a second recipe format.

Acceptance: cold-build node, watcher and committee images; start their real
entrypoints; introduce a workspace runtime edge and prove the image plan follows
it. Retain the historical missing-dependency controls. A graph comparison is not
an image startup test.

## Design decisions and sequence

Two independent sketches compared (A) checked shared mechanics plus canonical
metadata and (B) role-owned transaction services plus composed family-local
metadata. Independent review scored each 16/20 on invariants, semantics,
interface depth, migration proof and proportionate complexity.

The discussion narrowed the wallet proposal after those sketches: implement
only the existing helper's empty-pool refusal and behavior tests. Discard the
proposed new wallet authority types and role-service migration. For deployment
metadata, retain one logical owner, an independent completeness contract and
exact ordering. The review score is a design judgment, not experimental proof.

All design comparison was same-provider. An attempted external CLI panel was
blocked by automatic approval review before launch; no cross-provider verdict
is claimed. The [design sketch](DESIGN.md) reflects the smaller wallet scope.

Implementation sequence:

1. Complete the shared-helper hardening and its regression proof.
2. Remove exact deployment-map mirrors; then prove and migrate complete family
   metadata while preserving construction, manifest and publication identities.
3. Extend installed-boundary execution and wire it into ordinary verification.
4. Pilot package-owned task declarations, then replace their redundant inferred
   configuration across affected packages using behavior parity checks.
5. Derive all three image plans from that package owner and run clean image checks.

Coordinate owned files and the integration base with the active lead before
shared changes or pushes. Use isolated checkouts for implementation, preserve
other agents' changes, and push the composed, verified changes to the requested
canonical branch through the agreed lead workflow. Each slice must work end to
end; remove its superseded path when replacement proof passes.

## Existing work and verification skill

Canonical follower ownership, depth/finality helpers and their lint rule are
already in integration, as are major contributor improvements, campaign
execution, reached-test selection and mutation/restoration tooling. Complete
their existing acceptance/cutover work. Flat script storage and execution/trace
separation are active work too. Historical steering supports these directions;
it does not justify reopening them as new recommendations.

Applied the verification-skill workflow by extending the existing
[local-test feature map](../../../.agents/skills/local-test-environment/features/README.md),
covering preparation planning, focused-test receipts and compiled boundaries.
The real planning walkthrough and refusal case were executed; that is the
validated feature, not a claim that all mapped features or Midgard acceptance
ran. Existing devnet/Preprod skills remain the live-system owners. Use
`/maintain-verification-skill` when these entrypoints change.

The initial planning pass made no production changes, commits, deployments or
resets. [VERIFICATION.md](VERIFICATION.md) records that pass separately from
subsequent implementation evidence; this plan is not a claim that its remaining
implementation or acceptance gates have passed.
