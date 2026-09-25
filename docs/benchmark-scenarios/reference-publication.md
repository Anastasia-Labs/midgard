# Reference publication

Reference deployment, bootstrap, repair and initialization share
`ensureReferenceScriptTargetsProgram`. `REFERENCE_SCRIPT_PUBLICATION_MODE=serial`
is the default. Set it to `chained` explicitly to enable two funding lanes with
at most three unconfirmed transactions per lane. Both modes authenticate the
entire requested roster through the canonical provider before returning.
Initialization invokes this gate again; a finalized manifest is not permanent
L1 confirmation evidence.

The total unconfirmed signed-byte budget is 65,536 bytes. This is independent
of the per-lane count bound. Transactions still start with four targets and
split only when the existing SDK builder reports an oversized transaction.
The byte budget is conservative and must be tested against the actual local
submission stack before enabling chaining in an acceptance campaign.

The publisher allocates disjoint confirmed plain wallet inputs. If those inputs
cannot fund both lanes independently, it confirms a funding split. Only exact
plain outputs of signed, locally accepted parents can fund children. Reference
outputs are never funding inputs. Construction and signing are serial, so wallet
UTxO overrides cannot race between lanes. SDK completion keeps local UPLC
evaluation enabled.

Every SDK publication and lane split expires within five minutes of its build
slot, and publications also respect the existing auth-policy deadline. Every
new invocation with missing references waits through this maximum remaining
validity, then waits for Kupo to reach the exact canonical Ogmios checkpoint.
This includes fresh invocations: there is no persisted state that distinguishes
a fresh start from lost submission memory. Completed canonical references can
be reused immediately. All deployments use the bounded publisher; there is no
legacy publication compatibility mode.

A submit transport failure is ambiguous. A typed Ogmios rejection describes
that attempt, and does not disprove an earlier accepted copy. Affected lanes
pause child construction and retry identical signed bytes while valid. Missing
mempool observations never establish rejection. After unresolved attempts have
expired and the provider has caught up, the publisher rediscovers authenticated
outputs and rebuilds only missing targets from the lane's remaining canonical
plain outputs. The other lane can continue. If rollback withdraws several
generations of confirmations, an over-depth lane waits for canonical resolution
or expiry before replay. A global byte-budget overflow pauses all submission.
Rollback removes confirmation
evidence; missing references cannot pass the final gate. If earlier references
outside this invocation disappear, the call fails and requires restart
reconciliation. Expired authority is never replaced automatically.

There is no wallet lock, transaction journal, dependency file, or database state.
Only existing deployment identity/completion artifacts persist. The dedicated
publisher wallet must have one component owner. Keep that deployment identity
on restart. Cancellation stops further submissions; an in-flight submission
still needs the same restart reconciliation.

## Checks

From the repository root:

```bash
pnpm --dir demo/midgard-node exec vitest run \
  tests/reference-publication.test.ts \
  tests/reference-publication-indexer.test.ts \
  tests/reference-scripts.test.ts
pnpm --dir demo run test:tx-prep:sdk
pnpm --dir demo run test:tx-prep:node
pnpm --dir demo run test:tx-prep:emulator
```

## Local stack comparison

This is a bounded publication comparison, not L2 throughput or full deployment
acceptance. Use a separately generated phase4-process devnet with the verified
Preprod consensus and protocol parameters. Its local identity, genesis funding,
start time and network magic are the intended differences. Do not reset an
existing deployment or substitute faster consensus parameters.

The opt-in test takes a generated run directory containing `run.env`, genesis,
and a funded private `secrets/publication-probe.json` with a `seedPhrase` field.
Run serial and chained sequentially against that same dedicated wallet:

```bash
MIDGARD_PUBLICATION_LOCAL_DEVNET_DIR=/absolute/path/to/isolated-run \
MIDGARD_PUBLICATION_LOCAL_DEVNET_MODE=serial \
pnpm --dir demo/midgard-node exec vitest run tests/reference-publication-local-devnet.test.ts

MIDGARD_PUBLICATION_LOCAL_DEVNET_DIR=/absolute/path/to/isolated-run \
MIDGARD_PUBLICATION_LOCAL_DEVNET_MODE=chained \
pnpm --dir demo/midgard-node exec vitest run tests/reference-publication-local-devnet.test.ts
```

Both runs publish eight copies of the real blueprint PlutusV3 script
`fraud_proofs/cross_block_duplicate_event/step_01.main.spend` under distinct
authentication authorities and authenticated role names. The normal batching/splitting rule produces the
transactions. The chained comparison requires a child submission while its parent inputs are
still absent from Kupo and while the canonical tip remains unchanged from
before the parent submission through child acceptance. It authenticates the
full roster before success. The
private test authority is reconstructible from genesis time and wallet identity
for restart; signed transactions and dependencies are never written to disk.

The resulting `work/publication-*-measurements.json` files report total time,
startup reconciliation time, publication time, signed bytes, transaction count,
submission/indexing observation latencies and peak outstanding counts/bytes.
A rerun that only rediscovers references is labelled accordingly and is not a
second timing sample. Raw test logs and aggregate measurements are evidence,
not a scheduler recovery journal. This bounded repeated-script workload does not establish the
30–60-minute hypothesis for the entire production roster. A Preprod pilot remains
conditional on the required acceptance gates.

## Recovery probes

Use the same opt-in test with chained mode and
`MIDGARD_PUBLICATION_LOCAL_DEVNET_DRILL=ambiguous`, `expiry`, or `restart`. Each
drill uses a separate deterministic test authority, preserves its identity on
rerun, and checks the canonical roster for duplicate or malformed roles.

- `ambiguous` loses one acknowledgement after actual Ogmios acceptance.
- `expiry` withholds one transaction at the transport boundary until expiry,
  checks identical retries, and requires the other lane to progress before
  replacement. This is injected transport failure, not actual node rejection.
- `restart` with `MIDGARD_PUBLICATION_CRASH_AFTER_ACCEPT=1` kills the publisher
  worker after actual acceptance. Rerun without that flag, retaining the same
  wallet and authority; the new process must reconcile without transaction
  memory. The first process intentionally fails.
- `rollback` is manually coordinated with a preserved pre-publication fork of
  the isolated Cardano stack. After observing a canonical publication, the
  probe writes `work/rollback-observed` and pauses at the next observation.
  Stop the original isolated stack without deleting its data, start the saved
  fork at the same endpoints, and write `work/rollback-fork-ready` after its
  node and indexer synchronize. The probe requires lost canonical confirmation
  evidence and a fully authenticated final roster after recovery. Never use
  this drill against persistent Preprod services or a Midgard protocol deployment.

These marker files coordinate only the external fork operation. They contain
no publication transaction state and cannot reconstruct or resume a publisher.
All signed transactions and dependency records remain in process memory.

## Recorded local comparison (2026-09-25 UTC)

The isolated stack used Cardano node 11.1.0, Ogmios 7.0.0 and Kupo 2.11.0,
with one-second slots, active-slot coefficient 0.05 and security parameter 2160. Serial and chained ran sequentially with eight 12,245-byte blueprint
scripts. Each comparison used a fresh authentication authority and the same
funded publisher wallet.

| Measurement                                             |    Serial |   Chained |
| ------------------------------------------------------- | --------: | --------: |
| Total wall time                                         | 448.179 s | 425.865 s |
| Startup reconciliation                                  | 323.002 s | 326.532 s |
| First submission through verification                   | 125.177 s |  99.333 s |
| Transactions, including funding split                   |         8 |         9 |
| Signed bytes, including funding split                   |   102,470 |   102,801 |
| Peak outstanding transactions                           |         1 |         5 |
| Peak outstanding signed bytes                           |    12,822 |    63,968 |
| Children before parent confirmation and Kupo visibility |         0 |         4 |

The chained funding split took about 80 seconds to become observable; its
eight publications then completed in about 19 seconds. The split remains part
of the reported 99-second publication measurement. These are single samples
with variable block arrival times, not a throughput guarantee. Per-transaction
submission, inclusion and indexing-observation latencies are retained in the
aggregate measurement files.

Raw logs are `/tmp/issue-682-live-serial-plutus.log` and
`/tmp/issue-682-live-chained.log`; aggregate files are under
`/tmp/midgard-publication-682-20260925/work/`. A preliminary synthetic native
script probe was stopped after Kupo returned a differently encoded script
with a different hash. Canonical authentication refused completion. The real
Plutus workload passed, and the malformed-role refusal has a regression test.

The full workspace command `pnpm --dir demo test` was run. Workspace
typechecking passed; the watcher prelude stopped with 45 failing tests in 14
files, before the remaining package lanes could start. Most failures concern
the existing fault-proof supervisor deadline configuration; other failures
include attestation-timeout observations and historical capture. See
`/tmp/issue-682-full-suite.log` and the pinned-toolchain rerun
`/tmp/issue-682-node22-full-suite.log`, which reproduced the same 45 failures.
This blocks a readiness claim and the
conditional Preprod pilot. No gates or authority deadlines were relaxed.

The full-roster initialization emulator fixture explicitly enables chaining.
Serial publication of that roster crosses the unchanged authority safety
reserve in emulator chain time and is correctly refused. Smaller serial
equivalence and real-stack serial publication remain covered.

Required checks completed so far:

- `pnpm --dir demo run test:tx-prep:sdk`: 175 Lucid and 649 SDK tests passed.
- `pnpm --dir demo run test:tx-prep:node`: 125 node and 51 tooling tests passed.
- `pnpm --dir demo run test:tx-prep:emulator`: all 65 node emulator tests
  passed, including chained full-roster initialization. The following fault-proof
  suite failed: 263 failed, 352 passed, four skipped across 117 files. Failures
  include dispute timing/maturity checks; see
  `/tmp/issue-682-emulator-gate-chained-init.log`. The aggregate gate is red.
- Focused publication/indexer tests: 30 passed after the rollback retry-cap
  regression and fix. Registry and existing chain tests also passed.
- Workspace typechecking, SDK/node builds, changed-file ESLint and Prettier
  checks passed. The acceptance runbook and skill-frontmatter validators passed.

Two earlier emulator attempts were interrupted during repeated script hashing.
The content-hash cache and fresh canonical role index removed that unnecessary
work; these interrupted runs are not counted as passing evidence. The serial
full-roster deadline failure was retained as a diagnostic and the integration
fixture now explicitly tests chaining without changing deadlines.

The initial checks used the shell's Node 24.13.1. After noticing the watcher
engine constraint and `demo/.nvmrc`, the required gate sequence and workspace
command were rerun with the pinned Node 22.22.2. The Node 24 local devnet
measurements remain valid diagnostics for packages whose declared engine is
Node >=22.16.0; they are not a watcher compatibility claim.

Real-provider recovery results on the isolated devnet:

- Lost acknowledgement: passed; eight unique publications reached the
  authenticated canonical roster after 67 submission attempts.
- Expiry: passed; 292 identical withheld attempts, other-lane publication before
  expiry, then replacement after expiry and canonical catch-up.
- Process death: the worker was intentionally killed after Ogmios accepted its
  first publication. A fresh process waited 311 seconds before its first
  submission, rediscovered that publication, and submitted only seven missing
  roles. Final authentication passed.

Recovery logs are `/tmp/issue-682-live-ambiguous.log`,
`/tmp/issue-682-live-expiry.log`, `/tmp/issue-682-live-restart.log`, and
`/tmp/issue-682-live-restart-resume.log`. Per-drill aggregate measurements are
under `/tmp/midgard-publication-682-20260925/probes/*/work/`. The crash is an
intentional failed process followed by verified recovery, not a clean first run.

- Actual rollback: passed on two separately preserved forks of the isolated
  chain. The switch changed height 125 to 96 and removed previously confirmed
  reference outputs. The publisher withheld replay until validity expiry, then
  rebuilt eight missing roles and authenticated the entire replacement roster.
  Sixteen publication transactions were built across both branches; peak new
  outstanding submissions stayed at five and 64,076 bytes. Raw evidence is
  `/tmp/issue-682-live-rollback-final.log`,
  `/tmp/issue-682-rollback-before-health.json`,
  `/tmp/issue-682-rollback-after-health.json`, and
  `/tmp/issue-682-rollback-missing-reference.json`. Aggregate measurements are
  under `/tmp/midgard-publication-682-rollback-1/work/`. Both branch databases
  were preserved; the test services were stopped after the drills.

The committed probe uses the standard minimum authority reserve. All recorded
probes finished with more than three hours of their four-hour authority left.
The early probe version passed a one-millisecond minimum to the test seam; no
recorded publication relied on that reduced reserve. The production publisher
and full-roster initialization kept the standard reserve throughout.

Final focused verification under Node 22.22.2: 54 publication, indexer, registry
and existing-chain tests passed. The pinned-toolchain SDK, node/tooling and
node-emulator portions also passed with the same counts above.

The pinned aggregate completed with the same fault-proof totals: 263 failed,
352 passed and four skipped. Its complete SDK/node/emulator output is
`/tmp/issue-682-node22-required.log`; the aggregate remains an acceptance
blocker. Final focused output is `/tmp/issue-682-node22-focused.log`, with
formatting and lint output in `/tmp/issue-682-node22-format.log` and
`/tmp/issue-682-node22-lint.log`. Standards and Spec reviews have no remaining
implementation findings.
