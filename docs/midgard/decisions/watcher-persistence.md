# Persist watcher progress over retained state

Status: Implemented and verified by the local installed runtime journey on
2026-09-09. This is emulator evidence, not live Preprod acceptance.

## Record inventory

The watcher already has one SQLite connection with WAL, FULL synchronization,
and `BEGIN IMMEDIATE` transactions. Its independently protected trusted head
remains the freshness authority. The following consumers determine what must
survive; a previous snapshot format does not itself justify retention.

| Existing record | Consumer and purpose | Retention and reuse |
| --- | --- | --- |
| Rollback authority revision, validation binding, finality, incident, and epoch anchor | `durable-runtime` reconciles startup with the independent head; the coordinator resumes and handles canonical replacement. | Keep the current materialized state and the bootstrap state needed by the active rollback epoch. A progress revision references that state; it does not need another copy of its records. |
| W03 L1 observations and chain points | W12 checks new configured-source agreement. W13 uses retained observations and ancestry for recovery; protocol UTxOs and confirmations reference chain points. | Store each distinct admitted observation once. Retain the release-bound recovery window and every older point still referenced by protocol state, an incident, or proof evidence. First/current observations may differ in depth and source capture; those are distinct finality inputs, not interchangeable duplicates. |
| W03 live/spent protocol UTxOs | Event topology, rollback restoration, and proof input construction. | Update changed out-refs. Keep spent records while a rollback, outstanding event, or challenge can require them. Current and bootstrap views may share unchanged records. |
| W03 DA inputs, reconstruction, decisions, faults, submissions, confirmations, retries, deadlines, and correction results | Verification and workflow reconciliation, including ambiguous submissions and exact correction economics. | Retain unresolved workflow dependencies and required public proof evidence. A completed decision is a marker bound to its inputs and resulting roots, not a copy of those inputs. |
| W03 derived caches | Lookup and record-integrity checks. | Reconstruct from authoritative records. Do not persist another copy of every cache entry in each progress revision. |
| `watcher_state_queue_observation_v1` | Sparse queue cursor and authenticated queue replay. | Existing 2,160-observation bound and native rollback revocation remain. This sparse cache cannot replace complete native user-event coverage. |
| `watcher_user_event_archive_v1` block evidence and reference evidence | Native-byte provenance, historical event lookup, rollback, and installed proof-family inputs. | Reuse its existing content-addressed immutable records. Preserve original bytes while referenced by active/terminal event evidence, retained recovery state, or a challenge. Do not create a second raw-block archive. |
| Event entry, current event state, and protected event checkpoint | Strict full-point successor processing, event lookup, and restart. | Persist changed event/state records and a small validated progress marker. Replace per-block full-store archives. Keep the existing bounded live suffix and pinned event provenance; archive indexes remain navigation, not new validation authority. |
| Historical native-script checkpoint | Ledger-output and script replay for an authenticated challenged header. | Reuse the exact common hash-chain prefix. After correction replaces a suffix, exclude its script occurrences from the new view, verify the ancestor payload and joining root, and advance the existing authenticated CAS. Original payload evidence remains available from its retained/archive owners. |
| Replay transcript and transcript head | Offline reproducibility and proof/readmission lineage. | Reuse the existing transcript store and its explicit resource ceilings; do not copy its evidence into progress records. |
| Funding reservations/leases/lineage and workflow journals | Prevent double spending/submission and reconcile crashes. | Existing owners and reconciliation rules remain. They are not catch-up state and must not be rewritten by a progress update. |

## Persistence boundary

Use the existing database and transaction owner. Materialize existing state
records once, apply inserts/updates/removals for a validated transition, and
advance its authenticated progress marker in the same transaction. Reuse the
immutable evidence archive and reference its records from current state and
rollback/proof dependencies. Publish the existing independent trusted head only
after that transaction commits.

Restart authenticates the saved marker, its dependencies, and its freshness;
completed semantic validation survives restart. Changed deployment/policy/code
validation bindings, unusable saved state, and actual chain replacement trigger
the corresponding explicit recovery path. They do not justify replaying all
unchanged history on every ordinary restart.

Deletion requires both absence of live references and expiry of the applicable
recovery/challenge retention requirement. Unresolved submissions, incidents, and
proofs pin their dependencies. Archive pruning must fail closed when a consumer
or retention deadline cannot be established. This change does not infer expiry
from process uptime or merely from a later successful commitment.

This uses neither an event-sourcing framework nor an additional checkpoint
service. State roots/progress markers refer to available records. Historical
full-state copies are not the persistence unit.

## Verification

Verify transaction crash boundaries, stale writers/heads, restart without
semantic prefix replay, corrupt/missing referenced records, caller mutation,
rollback inside and beyond finality, and retention of pinned proof inputs.
Measure bytes written for one new block against a retained large state. Finish
the installed runtime journey with the honest successor classified healthy.

### Recorded results

- The final installed journey passed in 2,896.92 seconds with Node 22.22.2:
  invalid commitment detection, confirmed proof and correction, and honest
  successor `9eea3475c48fdbc0c72c06c62934b5ca82b201cfc916c998e6a37338`
  classified healthy with its on-chain state-queue unit still present. The
  complete local log is `/var/tmp/watcher-catchup-journey-observed-final.log`.
- The focused watcher checks passed: 140 tests covering record storage,
  immutable ownership, publication, restart, history, coordinator recovery,
  crash boundaries, and independent trusted heads. Ordinary restart reads the
  fresh origin and saved head, retains the existing checkpoint, and publishes
  the next block without replaying the validated prefix. The sealed-history
  case also preserves older pinned evidence.
- Storage tests measured less than 24,576 newly written record/marker bytes for
  a one-record update over more than 1.5 MB of logical state. Event updates use
  the same bound; repeated raw block bytes are stored once. These are logical
  payload measurements, not SQLite file size or filesystem write amplification.
- Native-history and header-classifier checks passed all 22 tests, including
  corrected-suffix replacement matching fresh replay and retaining access to
  the original branch's evidence. Five journal tests and five installed
  transition-trace lifecycle tests also passed. The journey observer reopens
  the journal to read the runtime writer's durable decisions.
- Watcher and fault-proof TypeScript checks, scoped ESLint, and formatting
  passed. The mixed replay suite has 29 passing tests and three unchanged
  baseline failures: one caller-supplied historical replay authority fixture
  and two catalogue fixtures expecting a healthy classification. These were
  reproduced with the pre-change replay sources and remain unresolved.
