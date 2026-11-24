# Reference publication

Reference deployment, bootstrap, repair and initialization share
`ensureReferenceScriptTargetsProgram`. Publication uses two funding lanes with
at most three unconfirmed transactions per lane. There is no runtime mode
setting; serial publication remains available only as an explicit test and
benchmark comparison. The publisher authenticates the entire requested roster
through the canonical provider before returning.
Initialization invokes this gate again; a finalized manifest is not permanent
L1 confirmation evidence.

The total unconfirmed signed-byte budget is 65,536 bytes. This is independent
of the per-lane count bound. Transactions still start with four targets and
split only when the existing SDK builder reports an oversized transaction.
The byte budget is conservative; the local-stack comparison below checks
chaining against the actual submission stack.

The publisher first consolidates fragmented plain wallet funding in confirmed
transfers of at most 100 inputs, retaining each funding state for rollback
reconciliation. Consolidations expire within five minutes. The publisher then
allocates disjoint confirmed plain wallet inputs. If those inputs
cannot fund both lanes independently, it confirms a funding split. Only exact
plain outputs of signed, locally accepted parents can fund children. Reference
outputs are never funding inputs. Construction and signing are serial, so wallet
UTxO overrides cannot race between lanes. SDK completion keeps local UPLC
evaluation enabled.

Every SDK publication and lane split expires within five minutes of its build
slot, and publications also end before the auth policy's expiry slot. Every
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
and a funded private `publication-probe.json`, in the run directory's `secrets`
directory, with a `seedPhrase` field.
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
`fraud_proofs/missing_redeemer/step_04.main.spend` under distinct
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
not a scheduler recovery journal. This bounded repeated-script workload does
not measure publication of the entire production roster.

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
