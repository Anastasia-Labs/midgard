The `recover-l1-source` command can clear one exact incident: a NEW unsigned
finalized candidate whose canonical point was missing, after complete native
replay proves the candidate at the unchanged configured deployment and source.
It requires an existing durable replay anchor and consumer cursor. It supports
JSON and PostgreSQL committee stores, with the availability responder disabled
and no configured availability journal. It does not recover ordinary
availability-enabled incidents.

Use the same deployment and configuration environment as the held daemon.
Stop and join that daemon before running the command. Both commands acquire
its existing exclusive store lease; neither bypasses a running writer.

```sh
recover-l1-source inspect
recover-l1-source --incident <digest printed by inspect> --timeout-ms 30000
```

`inspect` returns the current status, a digest of the complete incident, and
refusal residues. Exit 0 means inspection completed, not that recovery is safe.
Recovery reports `recovering`, then `recovered` (exit 0) or `recovery_refused`
(exit 78). Its final native fence and backend transaction compare the entire
incident, deployment and retirement metadata. A changed incident requires a
new inspection and proof. A write/commit failure can have an unknown outcome;
`inspectRequired` directs a fresh inspection rather than a reset or retry of
an external effect. Every started native read is directly awaited. The
monotonic timeout fences writes after expiry and supplies no successful-read
latency or transport-quiescence guarantee.

| Residue                                                                          | Required evidence before any future supported recovery                                                                                                                                                               |
| -------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `journal_exclusive_inventory_unavailable`                                        | A proven exclusive journal inventory barrier covering every actor/resource/workflow/intent/release family through committee CAS. No such operation is supplied here; counts alone never authorize a clear.           |
| `prior_*_requires_reconciliation`                                                | Per-effect canonical reconciliation of every retained signature, submission, broadcast, outbox, candidate, conflict and capacity liability. Existing posting/failed statuses do not authorize requeue or re-signing. |
| `conflicted_payload_cause_unknown` / `conflicted_header_requires_reconciliation` | Exact original cause and an authenticated per-record repair. This command preserves bytes and statuses and cannot infer a historical quarantine cause.                                                               |
| `protected_floor_proof_unavailable` / `retirement_floor_breached`                | Exact protected-floor continuity and breach resolution. Elapsed time or a new tip is insufficient.                                                                                                                   |
| `durable_replay_anchor_missing` / incomplete native replay / missing capability  | Complete existing native history, without a guessed point, empty replay, deferred skip or catch-up shortcut. Explicit test fixture providers lack that history and are refused.                                      |
| Unsupported quarantine reason or changed deployment/source                       | A separately proved reason-specific operation; no generic force-clear is provided.                                                                                                                                   |
| Exclusive store unavailable                                                      | Stop/join the owning daemon so its session ends and releases the store's instance lock; never terminate another live process's session or force database locks.                                                      |

Recovery changes only the held source's status and quarantine fields. It
preserves observations, payloads, signatures, outbox rows and every other
stored family. It performs no signing, financial effect, effect retry or
cursor acknowledgement. The next ordinary daemon tick must still replay and
consume native history through its usual guards. There is no `--force`, reset,
migration or executable repair command for the unsupported residues above.
