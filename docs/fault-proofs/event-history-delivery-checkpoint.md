# Authenticated event-history delivery checkpoint

Status: delivered to the original checkout; immutable checkpoint verification,
original-checkout typecheck and normal node/worker build passed. This is an integration checkpoint, not full acceptance or an ABI freeze. At the user's
request, recovery development stops after the current observed-header case;
remaining variants are listed below rather than implemented in this delivery.

## Contents and preservation

The delivery combines the broader contract/SDK/watcher/proof candidate, production
node staging, and the captured original checkout's unrelated work. The destination
is `/home/gumbo/midgard-hub/midgard`. The immutable source is
`/home/gumbo/midgard-hub/event-history-original-integration-checkpoint-28`:
7,937 files, identity
`c3ede1c6b0020e1e8c006d1fd0320c0116e435520bee1ac07563806a5e20a776`.

Application uses per-file original/baseline/task comparisons and verifies current
hashes immediately before writes. It preserves the current progress log and
unrelated work. Previous generated outputs and the native executable are backed
up before verified replacements. Workspaces and earlier verification evidence
remain available. No database, environment file, deployment or live service is
reset by consolidation. Consolidation preserved Git history and the prior index;
the user subsequently authorized committing and pushing this checkpoint.

The completed application contains 435 source paths, 413 verified normal build
outputs and one verified native executable. Root independently rehashed all 7,937
manifest paths: zero application or preservation mismatches; Git HEAD and index
were unchanged at application. The application report is
`artifacts/event-history/parallel/root-delivery-checkpoint-28-application.json`;
previous bytes and both measurement ledgers are retained in
`artifacts/event-history/parallel/delivery-before-checkpoint-28/`. One measurement-ledger conflict is
preserved explicitly: original and candidate
`transition-trace-workflow-fit-ledger.json` evidence remain separate; their claims
were not text-merged. Post-delivery acceptance29 regenerated the ledger by
running the complete installed transition-trace suite on the exact checkpoint
source, then independently checked all signed receipts before replacing it. Both
prior versions remain archived. Other proof/ABI acceptance gates remain open.

Installed dependencies in the original checkout now use the same verified package
set as the immutable test snapshot: 245 package aliases and 107 command wrappers
were aligned, while 42 workspace aliases still resolve inside the original
checkout. Previous links/wrappers are backed up; package stores, lockfile and
source were unchanged. Details: `delivery-dependency-realization-result.json`
in the same evidence directory. No fresh install or package-store edit occurred.
The declared dependency graph has 650 package roots, no missing required
dependencies and no mixed-store singleton split. Installed third-party links use
the retained candidate store: keep the workspace directories until a normal
frozen-lockfile install has been separately verified. Source imports resolve to
the original checkout; source consolidation does not depend on editing staging.

## Implemented flow

Authenticated event history uses the selected two sorted UTxO lists, preserving
public event IDs and distinguishing each actual admission incarnation from pointer
continuations and later reuse. The combined source includes validators/policies,
retention and filler handling, schemas/builders, raw proof construction and shared
consumers, watcher indexing, source-owner ingestion, journal persistence,
projection, settlement/refund/payout/reclamation, and deployment tooling.

Production-owner emulator journeys exercise a deposit and invalid/valid
withdrawals through signed commitment, attestation, mature merge, reserve
absorption, exact refund/payout and authorized reclamation. Large raw data is
prepublished and authenticated by the actual existing retention output. The
retained original deposit Value is 12,000,000 lovelace in the demonstrated payout.

The implemented recovery slice preserves signed intent until authenticated
canonical absence spans its signed validity range, expiry and configured finality.
It verifies the current queue/confirmed baseline, records a durable native/SQL
operation, restores a retained native root, atomically reverses dependent SQL,
retains signed header/member evidence, releases only the journal's lease, and
refreshes Globals/cache before Ready. The observed-before-local-finalization case
refuses partial finalization jobs or DA state. A restarted operation retains its
original identity after native restoration; queue absence or elapsed time alone
never authorizes abandonment.

## Verification and evidence

All recent runs use Node22.22.2, pnpm9.15.4 and one central test fork. Runners record
exact commands, exit codes, log hashes, source identity and dependency hashes before
and after execution. Evidence is under `artifacts/event-history/parallel/`.

| Check | Result and scope |
| --- | --- |
| Acceptance29 fresh pinned normal Aiken build | PASS; all 1,162 blueprint validator entries reproduce the delivered blueprint byte for byte |
| Acceptance29 Aiken event-history modules | 195/195 unit tests PASS across all 14 test modules; exact names/count verified |
| Acceptance29 fault-proof typecheck | PASS |
| Acceptance29 complete installed transition-trace lifecycle/fit suite | 13/13 PASS; regenerated ledger, 1,340 signed receipts independently checked |
| Original checkout documentation links | PASS: 543 Markdown/MDX files |
| Original checkout node typecheck | PASS after verified dependency alignment |
| Original checkout normal node/worker build | PASS, including declarations; existing generated outputs backed up |
| Final checkpoint28 node typecheck | PASS |
| Final checkpoint28 current observed recovery | 1/1 PASS; complete evidence recorded |
| Final checkpoint28 required node suite | 125/125 PASS |
| Final checkpoint28 required emulator suite | 64/64 PASS; source and dependency hashes unchanged |
| Checkpoint25 actual production D/W, original dependent spend, descendant spend | 3/3 PASS; node noEmit PASS |
| Checkpoint27 original dependent spend, descendant, pending service restart | 3 passed; service recreation is within one process, not an OS crash |
| Checkpoint27 reference-input journey | FAIL during original-deposit setup: `History event is already admitted`; reference-only recovery was not reached |
| Checkpoint27 retained-native selection | 13/13 selected cases PASS; 20 existing cases filtered from this narrow run, previously20/20 PASS on checkpoint18 |
| Checkpoint27 payout fixture correction | Selected end-to-end payout case PASS; no assertions removed |
| Checkpoint24 recovery ordering and production D/W | 4/4 PASS; cancellation across COMMIT, SQL rollback and native failure remain fenced |
| Checkpoint24 required node/emulator suites | 125/125 PASS; 63/64 emulator, sole fixture permit defect corrected and narrowly retested |
| Checkpoint23 retained archived memberships | 10/10 PASS across deposits and withdrawals |

Root independently verified the checkpoint25 D/W artifacts: 41 signed receipts,
504 creating transactions, 40 Ready checkpoints, three retirements and three
external-data reclaims. The signed-byte, fee and execution-budget measurements
remain per transaction in
`root-original-integration-checkpoint-25-streaming-receipts-verified.json`.
Across those 41 emulator receipts, signed sizes range from 390 to 3,394 bytes;
fees from 172,717 to 1,111,600 lovelace (19,838,790 total); maximum execution
memory is 3,946,906 and maximum steps 1,589,941,227. These are demonstrated
journey measurements, not maximum-admissible-size or live-contention acceptance.
Production limits were not raised. Signed receipt/native/SQL evidence for the
original rollback was also independently verified. Synthetic genesis, ancestry,
transport and emulator rollback are explicit; these are not live consensus proofs.

The normal Aiken blueprint used by these journeys has SHA256
`7b7a0abae2db62e3ecb9e26e7907bbc1d4793eeee1a522470cc1c1014d1c8d74`.
The verified native executable has SHA256
`6f2fe78b7dddbbeef5aedde84a63b946db10758f7615c74e320ce53a8207c000`.
Each journey retains its own compiler/artifact/parameter/manifest binding; emulator
manifest identities are not a live deployment approval.

Exact recent commands, executed from each immutable checkpoint's
`demo/midgard-node` with the pinned pnpm CLI:

```text
pnpm exec tsc --noEmit
pnpm exec vitest run tests/l1-event-history-observed-recovery-emulator.test.ts
pnpm exec vitest run tests/event-history-recovery-plans.test.ts -t 'retained native recovery root selection'
pnpm exec vitest run tests/deposit-flow-emulator-merge-payout.test.ts -t 'payout to conclusion'
pnpm exec vitest run tests/l1-event-history-owner-rollback-emulator.test.ts tests/l1-event-history-owner-rollback-dependencies-emulator.test.ts
pnpm run test:tx-prep:node
pnpm run test:tx-prep:emulator
```

Original-checkout checks use the same pinned Node/pnpm from
`/home/gumbo/midgard-hub/midgard/demo/midgard-node`:

```text
pnpm exec tsc --noEmit
pnpm run build
```

The full CLI prefix is
`/home/gumbo/.nvm/versions/node/v22.22.2/bin/node /home/gumbo/.cache/node/corepack/v1/pnpm/9.15.4/bin/pnpm.cjs`,
with the same Node bin directory first in `PATH`. Exact command arrays, exit codes
and log hashes are in `root-delivery-original-verification.json` and
`root-original-integration-checkpoint-28-receipts-run.json`.

The required documentation command runs from `/home/gumbo/midgard-hub/midgard`:

```text
pnpm --dir docs-site run check:links
```

It uses pinned Node22.22.2 and the docs workspace's pnpm10.11.0 CLI. Result and
exact command are retained in `root-delivery-docs-links.json`: exit0, 543
Markdown/MDX files checked.

## What prevents full acceptance

1. **Reference-input acceptance is failing in fixture setup.** Its duplicate public
   event ID must be corrected without weakening admission/rollback assertions,
   then the actual reference-only recovery path must be exercised.
2. **Remaining production recovery shapes are not delivered.** Other signed or
   non-finalized states, R0 response-loss lifecycle, mixed canonical/orphan or
   multi-header recovery, published/merged dependent L2 effects, imported or
   immutable confirmed baselines, changed-branch prepared-plan disposition and
   below-anchor recovery remain fenced or unverified. No new variants are started
   for this checkpoint.
3. **Crash/race coverage is incomplete.** Service restart and cancellation across
   SQL commit do not establish OS-process crash replay, every native/SQL/cache
   interruption window, exact post-COMMIT supersession, or an actually active
   crash-retained lease disposition.
4. **Final-identity proof and consumer acceptance remains.** Acceptance29 closes
   the fresh normal blueprint build, 195 existing Aiken history unit tests,
   fault-proof typecheck and the 13-case installed transition-trace suite/ledger
   regeneration for the delivered source. Other proof families, affected package
   suites and the full positive/negative, maximum-size, pruning, challenge/merge
   and contention matrix remain. The reviewed 13-case suite itself does not cover
   arbitrary nonexistent IDs, substituted payloads, honest eligible withdrawals,
   direct wrongful-challenge rejection or retirement followed by ID reuse. Its
   catalogue, binding and retained-header fixtures do not establish production
   catalogue governance or live L1 authority.
5. **ABI readiness remains open.** Final measured safe inline/external bounds,
   full-width authentication/accounting, eligibility/no-backdating, finalized-frontier
   retirement, proof deadlines versus merge and challenge availability under
   adversarial pointer churn require the complete acceptance evidence. Emulator
   profile values are not approved production limits.
6. **Existing database/deployment migration is unresolved.** The changed initial
   schema intentionally rejects an old migration checksum. No existing durable
   database was reset or metadata rewritten; a reviewed upgrade/redeployment path
   is still needed.
7. **Live prerequisites and the single verified deployment are missing.** Docker
   Desktop/WSL is stopped; normal startup can restart legacy producers before the
   API is available to contain them. Preserved provider state, actual provider
   genesis/source coverage, final parameters/manifest/reference scripts, DA and
   public-reader roles/configuration still need reconciliation. The independently
   derived genesis pin has not been compared with a running verified provider.
   No event-history live acceptance, live contention/economics or live recovery
   has run. The existing containment/preflight handoff records attempted remedies.

Acceptance29 evidence is under `artifacts/event-history/parallel/`:
`root-delivery-acceptance-29-run.json` records exact fresh-build, typecheck and
`pnpm exec vitest run tests/transition-trace-installed-lifecycle.test.ts` commands;
`root-delivery-acceptance-29-aiken-tests.json` records the exact 14 module selectors
and expected 195 names for `aiken check --env testnet -e --plain-numbers`;
`root-delivery-acceptance-29-installed-fit-verified.json` binds independently decoded
signed hashes, fees and budgets to the ledger. The regenerated ledger file SHA256
is `66acac392bb0da73a66afb89e56ab532c4361362a05e53d73fefe6d49b117642`.

Across the 1,340 installed-fixture receipts, maximum signed size is 15,461 bytes
(923-byte limit margin), memory 5,292,007 and CPU steps 2,760,367,733. The maximum
recorded fee is 500,000,000 lovelace, reflecting an authored fixture fee; these are
not live fee estimates. Cases use 512-byte inline and 14,000-byte payload bounds,
with 12,000-byte authored large datums; this does not approve every maximum encoding
or production bounds. No production limits or checks were changed.

The full original objective remains incomplete. This checkpoint makes the
combined implementation, demonstrated behavior and remaining gates reviewable;
it does not close unrelated proof-family gaps.

## Commit packaging verification

The user authorized committing and pushing the combined checkpoint on
`colll78/canonical-v1-watcher-l1-source-checkpoint`. The environment backup and
local-only artifacts are excluded. Normal pre-commit hooks passed and formatted
27 files, including one import-order correction. A fresh isolated pinned normal
Aiken build reproduced the same blueprint; original node noEmit, required
node125/125 and watcher original-assets tests passed after formatting. Exact
commands/results are in `artifacts/event-history/parallel/root-checkpoint-commit-verification.json`.
The only remaining `git diff --check` warning is a required blank context line
inside the vendored unified patch; its integrity-bound bytes were preserved.
This packaging does not close the remaining full-acceptance gates above.
