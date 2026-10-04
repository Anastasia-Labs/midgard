# Deterministic contributor tooling implementation

The 15 recurring workflow problems in [the transcript report](REPORT.md) now
have repository-owned commands, shared fixtures or named production gates.
[The contribution guide](../../agents/contrib.md) is the operational entry point;
`node scripts/contrib.mjs --help` is the command reference. The implementation
extends the existing preflight, artifact registry, disposable database setup,
publication drivers, runtime suites, e2e stack and exact-path commit helper.

## Delivery map

| Report problem                                             | Deterministic implementation                                                                                                                                   | Evidence owner                                                                                                          |
| ---------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| 1. Wrong focused runner, preparation or selection          | `prepare`, `test`; explicit files, package-local runner, runtime dependency preparation, nonzero actual counts and recorded seed                               | Contributor receipt/refusal tests; compiled worker lane                                                                 |
| 2. Shared checkout, compiler, database and port collisions | `resources`, guarded workspace/native builds, invocation database prefixes, deterministic devnet allocations and acceptance run leases                         | Resource serialization, inherited ownership and cancellation tests                                                      |
| 3. Initialization and cold proof delivery                  | `gate deployment-fit` runs existing signed publication, enabled role inventory, reference chain and proof publication drivers                                  | Production drivers emit signed measurement and workflow evidence, hashed into the receipt                               |
| 4. Invalid hand-assembled fixtures                         | Shared `chain-fixture`, typed `witness-fixture`, explicit malformed mutation                                                                                   | Core/SDK fixture tests use the production encoder                                                                       |
| 5. Database contamination and incomplete truncation        | Shared validated disposable identifiers; exact current-database attestation; migration catalog cleanup and uncatalogued-table refusal                          | Node database/migration tests and production history fixture                                                            |
| 6. Recovery and resource-retention seams                   | Shared `RecoveryScenario` generation/finality fixture and `gate recovery-scenarios` over production consumers                                                  | Terminal, dependent recovery, funding and durable restart suites                                                        |
| 7. Parent/worker lease string drift                        | Shared `commitLeaseOwner` constructor/parser used by three parents and the worker                                                                              | Parser polarity tests and compiled parent/worker emulator tests                                                         |
| 8. Repeated process polling and teardown                   | One bounded `runProcess`; owned groups, monotonic deadline, output ceiling, TERM/KILL escalation and joined cleanup; `gate lifecycle`                          | Real child/grandchild controls and production lifecycle suites                                                          |
| 9. Healthy quiet versus pending stalled work               | Read-only `diagnose`; explicit idle/working/stalled/held/dependency/dead/unknown classification; `gate runtime-progress`                                       | Classifier controls and node/watcher progress suites                                                                    |
| 10. Optional policies restrict ordinary operation          | `gate policy-matrix`                                                                                                                                           | Existing node, workflow funding and watcher persistence suites                                                          |
| 11. Private dependencies or stale generation               | `artifacts check/sync`, source/output digests, guarded native recipes, disposable frozen installs, `reproduce` public immutable-pin/build/native/artifact lane | Existing channel checks; copied-dist and emitted-dependency substitution controls                                       |
| 12. Splits lose owners or break runtime imports            | AST `locate --symbol`, declared package exports/bin inventory, guarded compiled `boundary` imports and CLI probes                                              | Root-resolution, ownership and output-substitution regression tests                                                     |
| 13. Invented or stale verification reports                 | Machine-emitted execution receipts; `receipts verify/render`; preflight step receipts                                                                          | Actual assertion totals, skips, zero execution, changed inputs/reports/logs/artifacts and version-only refusal controls |
| 14. Resume and integration scope reconstruction            | `workspace inspect`, versioned `program validate/render`, exact-base/content `packet create/verify/apply`                                                      | Cycle, issue relationship, stale base/content, staged path, symlink escape and cancellation rollback controls           |
| 15. Maximum inputs and first-fault order                   | `gate input-envelopes`, streaming exact-byte `measure`, signed publication measurements                                                                        | Existing envelope, adversarial/polarity, DA corpus and publication suites                                               |

## Adoption and enforcement

The demo agent guide and progressive agent documentation route to the CLI.
Existing test environment, devnet, acceptance, artifact, safe commit and report
skills point to it. Four short skills cover program resumption, read-only
progress diagnosis, cross-process contract changes and retirement of obsolete
orchestration. Skills retain decisions about protocol meaning and point to
executable owners for routine mechanics.

Every workspace package with a build recipe uses the guard, including newly
enrolled builds checked by `scripts/contrib/enroll-builds.mjs`. Both native
recipes use the same admission and provenance path. CI and preflight check
enrollment and the causal regression controls. Transaction-preparation SDK,
node and emulator lanes now appear in the preflight registry instead of relying
solely on remembering a manual documentation table.

The default output stays compact; bounded detailed logs and full content
identities live in the run registry. `MIDGARD_CONTRIB_VERBOSE=1` streams child
output when needed. No dependency version or lockfile was changed to install
the tooling.

## Review

Initial delivery base: `d6975d7a695745de7f1960c59f42ea6663768491`;
integration base: `1afce323e1db632ecc3b59d115cf726356e37661`.
The author and ten independent passes cover the complete source diff and the
state queue, fault-proof, user-event and DA invariant sets. Production changes
centralize the existing UUID-v4 lease predicate and strengthen disposable test
cleanup; they do not alter validator parameters, transaction economics,
canonical codecs, role bindings or finality rules.

Review closed races in boundary execution, dependency output substitution,
caller-relative paths, artifact input drift, staged deletion overlays, fixture
admission, duplicated CLI probing and unavailable worktree inventory. A final
composition control caught TypeScript cleaning away the watcher's Go owner.
TypeScript now preserves the native directory, excludes only the declared
binary from its own output identity, and leaves unowned siblings authenticated.
Reproduction checks every native and compiled owner after the artifact phase.
The original incorrect pass and the corrected deletion refusal were both
observed. Actual reproduction also exposed backward host UTC adjustments:
receipt creation and verification now share finite timestamp and nonnegative
monotonic-duration validation, preserve the raw observations, and flag clock
adjustments. The unchanged historical 43-step receipt fails the old verifier
and passes the corrected verifier; invalid timing remains refused. [The consolidated review](REVIEW.md) records closure and all twelve
lenses; `review-controls.mjs` carries 23 deliberately broken implementations
that must reach their named refusals, alongside the passing fixed controls.

Author lens results: parameter trust, deployed script application, pinned
decoders, value conservation, reference carriage, both polarities, execution
budgets, codec twins and ledger facts retain their existing owners. Anchoring,
non-vacuous gates and replacement guard coverage are exercised by the new
artifact, receipt and boundary controls. No protocol coverage gap is widened.

## Regression evidence and publication gates

Observed on 2026-10-04 UTC, using Node 22.22.2, after the output-owner and receipt timing fixes:

- `node --test scripts/contrib/*.test.mjs`: 45 collected, 45 passed, zero
  skips, exit 0. Tiny compiler fixtures test orchestration, not real compilation.
- `node --test scripts/preflight/run.test.mjs`: 18 collected and passed,
  zero skips, exit 0.
- `node scripts/contrib/review-controls.mjs`: fixed cases passed; all 23
  mutants failed at their intended assertions, overall exit 0.
- Canonical-foundation integration: workflow trigger, Git hook environment and
  complete-shard tests collected 23 assertions, all passed, zero skips, exit 0.

The 2026-10-03 implementation checkpoint also ran actual compiled node CLI
boundary checks, native-vector scratch synchronization, seven lease assertions,
and the deployment-fit gate (12 node and 94 fault-proof assertions). Those
receipts are historical evidence of the earlier input closure. Go portability
was verified with the actual native recipe: the original VCS-discovery failure
became an exit-0 build after disabling incidental Git stamping. Local Go was
1.26.0; hosted CI owns verification under its pinned Go 1.25.7.

Publishing requires full `node scripts/preflight.mjs --base <canonical SHA>
--json`, real `node scripts/contrib.mjs reproduce --execute`, and the latest
PR head's selected hosted checks. The PR retains the exact final commands,
counts, skipped assertions, head identity and outcomes. Two interrupted, superseded
preflights and two superseded reproductions are not overall passes. The actual
43-step reproduction at the previous source snapshot is historical evidence;
the corrected source still requires its own final reproduction and preflight. Generated
`plutus.json` remains untracked. [PROGRAM.json](PROGRAM.json) is a reviewed
source snapshot for all 15 items; it makes no live-acceptance or measured DevEx
claim. Future observations must establish whether repeated side scripts decline.

## Operational limits

Resource attestation currently uses Linux `/proc`. Guarded operations serialize
same-checkout writes; an unrelated raw command can bypass ownership, so consumers
also reject changed emitted contents. Stamps and receipts are local provenance,
not a hostile-user signing scheme. Invocation receipts and logs need preservation
outside temporary storage when used for durable handoffs.

Generating devnet assets leases allocation ports during generation; it does
not reserve them indefinitely or start/reset a chain. The acceptance wrapper
owns the existing e2e-stack execution and records evidence; an execution receipt
alone cannot certify the live acceptance criteria. Generic HTTP snapshots with
no timestamped progress contract classify as unknown. Synthetic chain/recovery
fixtures model assertions, while production adapters remain responsible for
authenticated chain facts and finality.

Executable artifact channels run their registered checks; four historical/manual
channels remain explicit gaps. Full clean reproduction can be expensive because
it includes all native builds and executable registered channels. Named gates
establish the coverage of their constituent production suites, rather than a
new claim that every protocol behavior has been exhaustively proven.

DevEx impact will be measured against later thread activity: guarded command
adoption, repeated environment/build/integration detours, stale or empty proofs,
and new throwaway orchestration. This implementation report does not claim a
measured improvement before that observation.
