# Endpoints, ports and logs

Verified against the tree as of 2026-09-25. Ports are the defaults; a phase4 run
and the acceptance harness override them, so read the run's `run.env` or the
harness environment before building a URL.

## Endpoints

| Service                 | Default port                                                                      | Path                                   | Bar   | Answer                                                                                                                                                                                                                                        |
| ----------------------- | --------------------------------------------------------------------------------- | -------------------------------------- | ----- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| midgard-node            | `PORT`, 3000                                                                      | `/healthz`                             | Live  | 200 `{"status":"ok","now":…}` whenever the HTTP server runs                                                                                                                                                                                   |
| midgard-node            | `PORT`, 3000                                                                      | `/readyz`                              | Ready | 200 with `ready: true`, else 503 with `ready: false` and `reasons`                                                                                                                                                                            |
| midgard-node            | `PORT`, 3000                                                                      | `/pipeline-status`                     | Work  | Commit, confirmation and merge pipeline state                                                                                                                                                                                                 |
| midgard-node metrics    | `PROM_METRICS_PORT`, 9464                                                         | `/metrics`                             | Work  | Prometheus text; served only when `listen` runs with `--with-monitoring`                                                                                                                                                                      |
| DA committee node       | `DA_COMMITTEE_API_PORT`, 8787                                                     | `/healthz`                             | Live  | 200 `{"ok":true}`; `{"ok":true,"status":"held"}` while a failed startup holds it up, its `/readyz` 503 with `committee_startup_failed` and the failure in `detail`                                                                            |
| DA committee node       | `DA_COMMITTEE_API_PORT`, 8787                                                     | `/readyz`                              | Ready | 200 or 503 with the readiness snapshot: `l1Source.status` (`uninitialized`, `healthy`, `intervention` with `l1Source.intervention` naming the follower's reason), `counts.signatures`, `counts.submittedOrConfirmedL1Attestations`, `reasons` |
| DA committee node       | `DA_COMMITTEE_API_PORT`, 8787                                                     | `/v1/manifest`                         | -     | The committee's runtime manifest                                                                                                                                                                                                              |
| Watcher operations      | `WATCHER_OPERATIONS_PORT`, 7402                                                   | `/readyz`, `/v1/status`, `/v1/metrics` | Ready | `/readyz` 200, or 503 with `reasons` (see the watcher section below); `/v1/status` 200 whenever the server runs, which is before the L1-dependent startup stages; no bearer                                                                   |
| Kupo                    | 1442 (demo stack `KUPO_PORT`), 2442 (phase4)                                      | `/health`                              | Ready | 202 while replaying, 200 once caught up; JSON body with `connection_status` and `most_recent_checkpoint`                                                                                                                                      |
| Ogmios                  | 1337 (demo stack `OGMIOS_PORT`), 2337 (phase4)                                    | `/health`                              | Ready | Ogmios health JSON                                                                                                                                                                                                                            |
| Phase4 acceptance nodes | `MIDGARD_PHASE4_NODE_A_PORT` 3101, `_B_PORT` 3102; metrics `_A_METRICS_PORT` 4101 | `/readyz`, `/metrics`                  | Ready | As midgard-node                                                                                                                                                                                                                               |

Sources:

- Node routes and handlers: `demo/midgard-node/src/commands/listen-router.ts`
  (`HEALTH_ENDPOINT`, `READINESS_ENDPOINT`, `PIPELINE_STATUS_ENDPOINT`); port
  defaults in `demo/midgard-node/src/services/config.ts`; the metrics server in
  `demo/midgard-node/src/commands/listen.ts`, which logs
  `Prometheus metrics available at http://0.0.0.0:<port>/metrics`.
- DA committee: `demo/da-committee-node/src/api/server.ts`; port default in
  `demo/da-committee-node/src/config.ts`.
- Watcher: the healthchecks in `demo/midgard-watcher/compose.yaml`.
- Kupo and Ogmios: `demo/midgard-node/docker-compose.kupmios.yaml` and
  `demo/midgard-node-tools/devnet/phase4-process/scripts/common.sh`.
- Demo-stack host ports in the table are the main checkout's. A linked
  worktree running the stack through `demo/midgard-node/scripts/operator-compose.sh`
  publishes every one of them elsewhere; `--print-env` lists its ports.
- Acceptance ports: `demo/midgard-node-tools/src/commands/e2e-journal-kill-recovery-acceptance.run-journal-kill-recovery-acceptance.ts`.

### What each healthcheck really proves

- The demo `midgard-node` healthcheck fetches `/readyz` and passes on any 2xx
  (`demo/midgard-node/docker-compose.yaml`). A healthy container is bar 3.
- The kupmios Kupo healthcheck greps for `HTTP/… 200`, so a replaying Kupo
  (202) is unhealthy. The node does not wait on it: its compose service
  depends on Postgres and cardano-node only.
- Phase4 `bootstrap.sh` waits with `wait_http`, which is `curl --fail`: any
  status below 400 passes, **including Kupo's 202**. Bootstrap's wait is bar 2
  for Kupo; the later snapshot step demands `connection_status: "connected"`
  (`parse_kupo_checkpoint`). `[review]`
- Both watcher healthchecks accept 200 **or 401**, so they prove only that the
  HTTP server answers (bar 2); `/readyz` shows watcher readiness. The server
  checks no bearer (`demo/midgard-watcher/src/runtime/operations-http.ts`),
  so the 401 branch never fires. `[review]`
- The devnet journey's idle phase ends by waiting for node `/readyz` to return
  200 (`journey.waitReady()` in
  `demo/midgard-node-tools/src/devnet-stack/journey-scenario.ts`, up to the
  30-minute `readyMs` deadline). Any reason `/readyz` publishes, including one
  that stays raised after its cause is gone, stops the journey there.
- Node `/readyz` reasons you will meet at startup include
  `l1_follower_view_unapplied`, `l1_driver_recompute_pending`,
  `l1_follower_catching_up`, `provider_query_unhealthy:l1-provider`,
  `native_mpf_owner_unavailable` and `unfinished_local_mutation_jobs:<n>`, on
  top of the worker-heartbeat, backlog and database checks. Each is explained
  in the next section. `/readyz` never checks that blocks are committing; that
  is bar 4.

## Node `/readyz` reasons

Every reason the node can put in `reasons`, what it means and what to do. A
`:` form carries parameters after the name. The node exits only when a
restart could plausibly fix the failure: a transient failure that outlived
its bound. Otherwise no reason stops the process: `/healthz` stays live, and
"wait" means the node clears the reason itself once the cause is gone.
After startup, Postgres transients (the instance lock, the follower's store,
the driver's coalesced runner) are each bounded at 15 min in a row; past it
the node logs `node_transient_budget_exhausted source=<source>
reason=<reason>` and exits non-zero, its supervisor's restart being the
backoff. An L1 node or sidecar outage is not bounded. A startup step that
fails holds the node or exits it (next paragraph). Entries under `details`
are degradations that leave the node ready.

Only a transient failure is retried on a timer: a connection failure or
timeout, a retryable provider error, a database that is busy or restarting,
an L1 node or sidecar still starting. A failure that is not known to be
transient is not retried on a timer. While the node runs, a follower-driver
hold that a retry can clear (a wait, or a transient failure) runs again on
the driver's backoff (500 ms doubling to 30 s). A hold no timer can clear (a
verdict on the view, a failure not known to be transient, a missing schema)
is read again on the next follower change, which is every new L1 block at
the tip. The list is derived from
`demo/midgard-node/src` and the follower readiness reasons of
`demo/midgard-l1-follower/src` by
`scripts/ci/check-readiness-reasons-doc.mjs`, which fails when a reason here
has no text.

Startup (`src/commands/listen.startup-http.ts`; the step reasons are in
`src/services/startup-waiting.ts`). Until the node serves, `/readyz` answers
503 with the stage in `stage`, `startup_incomplete` first, then the stage's
own reasons, then those of every startup step that is waiting. A step that
meets a transient failure retries it with a backoff that grows to 30 s,
under the step's budget, and its reason goes once it succeeds. The budgets:
the database 15 min (`STARTUP_DATABASE_BUDGET`), the L1 node and its
sidecar 10 min (`STARTUP_L1_NODE_BUDGET`), the protocol deployment status
`STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS` (default 120) attempts 5 s
apart, and the DA provider assertions (with the DA capability quorum) the
same attempts. A failure that is not
transient fails the step at once; a transient one that outlives the budget
fails it too. Either way `/readyz` answers `startup_failed` with
`failedStage`, `failedStep` and `failedReason`, and the log line
`node_startup_failed stage=<stage> step=<step> reason=<reason>; <outcome>`
names the last cause. A failure not known to be transient holds: the node
stays up and unready, `/healthz` answers 200 with `status: "held"`, and
nothing retries it until the node is restarted. A transient that outlived
its budget exits non-zero (`<outcome>` says so, and the line starts
`node_transient_budget_exhausted` when a running source's bound ran out).
The supervisor's restart policy is the retry after an exit: `restart: always` in
`demo/midgard-node/docker-compose.yaml`, and the process devnet's supervisor
(backoff 1 s doubling to 60 s). Waiting on another actor (another process
holding the instance lock, the follower catching up to the tip) has no
deadline:

- `startup_incomplete`: the node has not finished starting. Read `stage`
  and the reasons after it.
- `startup_failed`: a startup step or stage failed for good; `failedStage`,
  `failedStep` and `failedReason` name it. A failure not known to be
  transient holds the node up and unready: fix what the reason and the log
  name, then restart the node. A transient that outlived its budget exits
  the node; wait for the restart if the outage has since cleared.
- `lucid_initialization_pending`: building a Lucid client (its
  protocol-parameter read) failed transiently. Check the L1 node and the
  follower.
- `l1_node_config_pending`: the cardano-node configuration files are not
  there yet, so the network magic cannot be read. Check the configured
  paths and that the node has written them.
- `l1_slot_mapping_pending`: the ledger's slot mapping read failed
  transiently. Check the L1 node and its sidecar.
- `database_unreachable`: PostgreSQL does not answer or refuses
  connections; a pool waits to open, or the schema check waits to run.
  Restore Postgres reachability.
- `database_connection_failed` (a `failedReason`): PostgreSQL refused the
  connection for a reason waiting does not clear (bad credentials, a missing
  database). Fix the connection settings.
- `schema_migration_in_progress`: a `db:migrate` holds the schema lock.
  Wait for it to finish.
- `database_schema_incompatible` (a `failedReason`): the database's schema
  is not the one this binary runs on (not migrated, unversioned, a ledger or
  shape mismatch). Run `migrate`, or point the node at the right database.
- `protocol_deployment_status_unavailable`: the protocol deployment status
  read failed on the L1 provider. Check the L1 node and the follower.
- `reward_account_status_unavailable`: the availability-challenge reward
  account reads failed on the L1 provider. Check the L1 node.
- `deployment_manifest_mismatch`, `deployment_manifest_unverifiable`,
  `deployment_manifest_missing` (each a `failedReason`): the configured
  deployment manifest does not match the deployment, cannot be read or
  verified, or is not configured though the deployment is complete. Fix the
  manifest path or the deployment configuration.
- `protocol_deployment_partial` (a `failedReason`): some, not all, of the
  protocol deployment is on chain. Finish the deployment.
- `availability_reward_account_unregistered` (a `failedReason`): an
  availability-challenge reward account is not registered. Register it.
- `runtime_reference_scripts_failed` (a `failedReason`): the node-runtime
  reference-script preflight failed. Publish the reference scripts it
  names.
- `protocol_initialization_failed`: the startup's protocol initialization
  (its transaction, the reference scripts after it, or the deployment-info
  write) failed; startup ends with it as `failedReason`. Read the log.
- `da_provider_assertions_unavailable`: the DA provider assertions read
  failed on the L1 provider. Check the L1 node.
- `da_capability_quorum_pending`: too few DA committee peers answered
  capably yet, and the quorum can still form. Check that the committee's
  peers are up.
- `admission_backlog_unread`: the first read of the durable admission
  backlog failed with a connection failure. Check Postgres.
- In the `l1_follower_catch_up` stage, `state_queue_unavailable`: the landed
  state queue could not be read at the follower's view. A transient database
  failure waits within the database budget; any other read failure ends
  startup with it as `failedReason`.
- In the `follower_view_apply` stage, a driver failure hold
  (`startup_preparation_failed`, `l1_driver_recompute_failed`,
  `l1_events_ingestion_failed`, `landed_block_rebase_failed`,
  `mpf_closure_missing`, `native_mpf_restore_index_cap_exceeded`) that the
  driver does not retry ends startup with it as `failedReason`; one it
  retries ends startup once it has stood for the database budget.

Core checks (`src/commands/readiness.ts`, the readiness handler):

- `operator_not_yet_active`: the operator set lists this operator as
  awaiting activation. Wait for activation.
- `db_unhealthy`: the database check failed. Check Postgres and the node's
  connection settings.
- `stale_heartbeat:<worker>:<age_ms>`: a worker (`blockCommitment`,
  `blockConfirmation`, `merge`, `txQueueProcessor`) has not beaten within
  `READINESS_MAX_HEARTBEAT_AGE_MS`. Read that worker's log lines; a worker
  that keeps failing names its error there.
- `queue_depth_exceeded:<depth>:<limit>`: the durable admission backlog is
  over `READINESS_MAX_DURABLE_ADMISSION_BACKLOG`. Wait for the queue
  processor to drain it; if it does not, check `stale_heartbeat` and the
  validation pool.
- `durable_admission_oldest_age_exceeded:<age_ms>:<limit_ms>`: the oldest
  admitted transaction has waited longer than
  `READINESS_MAX_DURABLE_ADMISSION_AGE_MS`. As for `queue_depth_exceeded`.
- `local_finalization_pending`: a submitted block's local finalization has
  not run yet. Wait.
- `validation_worker_pool_degraded:<live>:<configured>:<restarting>`: fewer
  validation workers are alive than configured. Wait for the restarts; read
  the worker errors in the log if they repeat.
- `validation_worker_job_timeout:<age_ms>:<limit_ms>`: a validation job has
  run longer than `VALIDATION_WORKER_JOB_TIMEOUT_MS`. Read the log for the
  job; the pool restarts a hung worker.
- `unresolved_block_submission:<age_ms>:<limit_ms>`: a submitted block has
  been neither confirmed nor resolved for longer than
  `UNCONFIRMED_BLOCK_MAX_AGE_MS`. Check that the L1 node is synced and that
  the submission reached it.
- `state_queue_lease_stale:<holder>:<remaining_ms>`: the state-queue
  mutation lease expired past its grace while still held. Check whether the
  named holder process is alive; a node releases a previous process's leases
  at start.
- `attestation_timeout_correction_failing:<header>:<failures>:<overdue_ms>`,
  `attestation_timeout_correction_stalled:<header>:<since_ms>:<bound_ms>`:
  an unattested header is past its DA-attestation deadline and its
  correction keeps failing, or has made no progress. Read the
  attestation-timeout correction log lines for the failure.
- `attestation_timeout_queue_unknown:<since_ms>:<bound_ms>`: the correction
  step has not read the state queue within its bound. Check the L1 node and
  the follower.
- `settlement_worker_failing:<count>:<error>`: the settlement worker died
  `<count>` times in a row for over two minutes. Read the named error; a
  provider or Postgres outage reports its own reason first.
- `unfinished_local_mutation_jobs:<n>`: local mutation journals are not
  finished. Usually transient at start while recovery replays them; if it
  stays, read the mutation-job log lines.
- `da_publication_conflict:<n>`: a DA committee member answered `conflict`
  for a retained payload this node owes. Read `last_error` on the
  `da_payload_publications` rows with status `conflict`.
- `native_mpf_owner_unavailable`, `native_mpf_owner_unhealthy`: the native
  MPF owner has not started, or its diagnostics call failed. Wait; read the
  owner's log if it stays.
- `provider_query_unhealthy:l1-provider`: the node's L1 provider has
  answered no query within its bound (derived from `L1_NODE_BEHIND_MAX_MS`).
  Check the local cardano-node and its sync.
- `l1_transport_unready:<reason>`: the local node transport is not ready;
  `<reason>` is one of the transport's reasons in
  `demo/l1-node-transport/README.md`. Check that cardano-node is up and its
  socket is mounted; the transport retries on its own.

Commitment and liveness (raised by fibers, cleared by them):

- `commit_base_pending`: the commit waits for a foreign tail block that
  landed-block processing has not applied yet. Wait.
- `commit_window_pending`: the commit waits for the next scheduler window,
  because an abandoned journal whose commit was signed holds its header
  hash. Wait.
- `commit_worker_failed`: the commit worker's last run failed. Read the
  commit worker's error in the log.
- `commit_anchor_unavailable`: the follower has no block the profile's
  commit-event depth d below the view the driver applied (a chain too short
  above the origin, or a pruned block while the applied view trails the
  tip), so nothing is committed. Wait for the follower to advance.
- `l1_own_block_event_orphaned`: a landed own block includes a deposit or
  withdrawal whose L1 admission left the chain. No block is committed, and
  that block and the landed blocks built on it are not merged; admissions
  (except spends of that deposit's output), older merges, DA and the
  follower continue. The detail names the header(s) and the event count. It
  clears by itself when a landed correction or a rollback removes the
  header from the landed queue; wait.
- `commit_da_frame_events_overflow`: a block's events alone overflow the DA
  frame. `commit_da_frame_ledger_ceiling`: the base ledger alone exceeds the
  DA frame. Both refuse the block on every tick. The ledger ceiling needs
  the ledger to shrink (merges and withdrawals); raise either with the
  deployment owner if it stays.
- `native_mpf_owner_stuck`: the native MPF child failed the same way
  `MPF_NATIVE_OWNER_RESTART_LIMIT` times in a row, or its binary is not the
  pinned one. The owner restarts it no more. Read the child's error, fix the
  cause, then restart the node.
- `native_mpf_owner_recovery_pending`: a committed canonical recovery is
  not installed yet. Wait.
- `native_mpf_promotion_index_cap_exceeded`: the owner refused to promote a
  root whose full index is over a cap; nothing was written. It clears when a
  smaller promotion or a canonical restore succeeds; a root over the cap
  needs a node build whose caps cover it.
- `operator_removed`: the operator set retired this operator; commitment,
  merge, settlement and the watchdog hold. Nothing to recover on this node.
- `operator_watchdog_manifest_mismatch`: the deployment manifest does not
  match this node's configuration, so the watchdog strikes nobody. Fix the
  manifest or the configuration; it is re-read every ten minutes.
- `operator_watchdog_manifest_unverified`: the manifest could not be read
  several times in a row. Check the manifest path; it keeps retrying.
- `retention_l1_view_stale`: the retention sweeper has had no L1 view for
  longer than `L1_VIEW_FATAL_MS` and stopped sweeping. Check the L1 node and
  the follower; it resumes on the first view.
- `l1_control_plane_wedged:holder=<scope>:overrun_ms=<n>`,
  `l1_control_plane_wedged:waiter=<scope>:wait_ms=<n>`: an L1 control-plane
  holder overran its deadline, or a waiter has been blocked far longer than
  any hold. `l1_control_plane_hold_timeouts:<scope>:<n>`: one scope's holds
  keep timing out. Read the named scope's log lines; restart the node if a
  holder never returns.

Node instance lock (`src/services/node-instance-lock.ts`):

- `node_instance_lock_held_elsewhere`: another live process holds this
  node's instance lock on the same database schema: another node, or an L1
  follower command on the node's follower tables. This node waits, or holds
  its operator duties, until that process's session ends. Stop the other
  node or follower command pointed at this database. If none should exist,
  check `pg_locks`/`pg_stat_activity` for the advisory holder. The node
  takes over by itself once the holder is gone.
- `node_instance_lock_unavailable`: no Postgres session could be opened to
  try the lock at startup. A connection failure is retried with a backoff
  that grows to 30 s, for at most the database budget (15 min); past it, or
  on any other failure, startup fails (step `instance_lock`). Restore
  Postgres reachability and credentials.
- `node_instance_lock_suspended`: the session holding the lock ended under a
  live node, or the server-side check (every 10 s, on a session of its own)
  found that the server no longer holds the lock for it. Commit, merge and
  watchdog start no new tick (one already running finishes) and settlement
  is stopped while it reconnects. Usually no action; if it persists, check
  Postgres stability.
- `node_instance_lock_failed`: a suspended lock stopped trying to take the
  lock again: a failure that is not a connection failure, or Postgres
  unreachable for longer than the reacquire budget (15 min) in a row. On
  the budget, the node exits non-zero (`node_transient_budget_exhausted
source=instance_lock`) and its supervisor restarts it; on any other
  failure it stays up with its duties held. Fix Postgres, then restart the
  node.

L1 follower and follower-change driver
(`src/services/l1-follower.readiness.ts`):

- The follower's own reasons (`l1_follower_catching_up`,
  `l1_follower_waiting`, `l1_follower_apply_stuck`, `l1_node_unavailable`,
  `l1_node_behind`, `tracked_set_changed`, `wallet_seed_pending`, and the
  interventions `rollback_beyond_k`, `intersection_outside_history`,
  `origin_not_on_chain`, `store_integrity`, `origin_mismatch`) are
  explained in `demo/midgard-l1-follower/README.md`. An intervention stops
  the follow loop until an operator investigates and runs
  `midgard-l1-follower reset --to-origin`.
- `origin_after_protocol_init`: the follower reached the node tip without
  seeing the transaction that spends `hubOracleOneShot`, so `l1Origin` is
  after protocol init. The loop keeps following. Correct `l1Origin` to a
  point before the `prepareHubOracleNonce` block and reset the follower to
  its origin.
- `l1_follower_migration_failed`: the follower store refused its migrations
  at start (a recorded migration whose text changed, a duplicate id, or one
  the schema lint refuses). The follow loop stops; the node stays up and
  unready. Fix the migration set in the deployed build, then restart the
  node.
- `l1_follower_transient_exhausted`: the follower's store did not answer
  (a transient failure) for 15 min with no event settled. The follow loop
  stops and the node exits non-zero (`node_transient_budget_exhausted
source=l1_follower`), its supervisor's restart being the backoff. Restore
  Postgres.
- `l1_follower_prune_failing`: three prune passes in a row failed, so facts
  past retention stay and the store grows. Read the follower's prune error
  in the log; the next successful pass clears it.
- `l1_follower_unconfigured`: the node has no follower; the detail names the
  missing configuration. Set it and restart.
- `l1_node_config_unreadable`: the cardano-node config files do not yield
  the network magic yet. Check the configured paths; it retries for the L1
  node budget (10 min).
- `l1_node_config_failed`: the network magic read failed for good: the
  files stayed unreadable past the L1 node budget, or the read failed in a
  way waiting does not clear. Fix the configured paths and restart.
- `l1_follower_view_unapplied`: no follower-change driver has applied a
  view yet. Normal at start; wait.
- `l1_driver_recompute_pending`: a recompute (rebase, orphan repair, first
  view) is under way. Wait.
- `l1_follower_view_stale`: a write was computed at a view that is no
  longer on the follower's chain (a rollback). Wait for the recompute.
- `startup_preparation_failed`: the startup preparation failed; the detail
  names the step. A landed state queue that is not ready yet shows there as
  `state_queue_unavailable` or `state_queue_unhealthy`. A transient failure
  is retried on the driver's backoff; any other fails startup (see
  `follower_view_apply` above).
- `l1_driver_recompute_failed`: a recompute failed. A transient failure is
  retried on the driver's backoff; any other is read again on the next
  follower change. Read the detail; it repeats until the cause is fixed.
- `l1_events_ingestion_waiting`: event ingestion waits for its write gate
  while a recovery runs. Wait.
- `l1_events_ingestion_failed`: ingestion refused or failed; the detail
  names why. `l1_events_hook_failed`: a ticket hook failed; the detail names
  the hook. A transient failure is retried on the driver's backoff, any
  other on the next follower change; read the detail.
- `l1_events_orphan_recovery`: orphaned admissions wait for the recovery
  that rejects their dependents: those an unfinished block journal holds
  until its disposition, those a foreign landed block holds until the
  header leaves the landed queue. The detail names each such foreign
  header: it includes an event no longer on L1, so it is fault-provable
  (fabricated deposit or withdrawal), and the node commits nothing that
  would build on it. The hold clears when a fault proof or an L1 rollback
  removes the header; check that a fault proof against it is under way.
  Otherwise wait. (One a landed own block holds is
  `l1_own_block_event_orphaned` instead.)
- `l1_event_identity_conflict`: a projected event's public id has a local
  row under another live admission, or none; the event is left out until it
  clears, which only a chain change can do (it is read again on each).
  Inspect the named event's rows.
- `l1_event_undecodable:<count>` (a detail): that many projected events do
  not decode into the node's rows and are left out; `refused` names them.
- `l1_own_block_forced_order_orphaned:<count>` (a detail): that many forced
  rows of landed own blocks have an order that left the chain. The node
  follows the block and stays ready; the forced-order hook logs a warning
  naming each header and order once. It clears when the header leaves the
  landed queue.
- `forced_order_carriage_pending`: a forced order's carriage resolved from
  no source yet; it is retried. If the detail leads with "no L1 tx content
  source is configured", set `L1_TX_CONTENT_SOURCES`.
- `forced_order_ingestion_failed`: a forced order could not be rebuilt.
  `forced_order_admission_stopped`: a ruled admission stop refused an
  order's transaction; the detail names the stop. Both hold the horizon;
  read the detail.
- `operator_set_unhealthy`: the operator directory is not well formed; the
  detail names why. `operator_set_unavailable` (in a detail): the driver
  has not published the set yet.
- `state_queue_unhealthy`: the landed state queue is not well formed; the
  detail names why.
- `wallet_seed_pending`, `intent_reconcile_failed`,
  `intent_reconcile_transient`: an owed wallet seed, or an intent reconcile
  pass that failed as a whole or for one intent. Wait; read the detail if it
  stays.
- `intent_resubmit_rejected`: the L1 node refused a live intent's resend at
  several tips in a row. It clears once the intent stops being live or a
  resend is accepted; read the node's rejection in the detail.
- `intent_commit_anchor_not_deep`: a commit waits for the follower tip to
  be d blocks above its commit anchor again after a rewind. Wait.
- `intent_journal_no_view`, `intent_journal_unavailable`: the intent journal
  has no follower view, or a database record or read failed. The second
  clears on the journal's next good read or record. Wait; check Postgres if
  it stays.
- `intent_input_untracked`, `intent_bytes_mismatch`, `intent_undecodable`,
  `intent_content_ref_missing`, `intent_gate_unjournaled`: the journal
  refused an intent (an input that is not a tracked fact, other bytes for
  the same transaction, bytes that do not decode, a missing content
  reference, a gate that did not journal). Nothing was sent. Each is a
  defect; report it with the detail.

Landed blocks and the confirmed ledger (`src/landed-blocks/holds.ts`; the
first hold by priority is the reason, the rest are in its detail):

- `landed_block_invalid`: a landed block does not replay to its header or
  link to its parent; it is never adopted, and no timer replays it again
  (the next follower change does). Report the block.
- `landed_block_own_journal_mismatch`: this node's own landed block
  disagrees with its journal. A local fault; processing stops until the
  journal is repaired (no timer retries it): restore the node database from a backup that holds
  the block's journal (plan §7.5, R5). Report it with the detail.
- `landed_block_follower_schema_missing`: the follower's admission tables
  are missing from the node database; no timer retries it. Run `migrate`.
- `landed_block_event_unknown`: a foreign block names an event the follower
  does not know at the view. Wait; it is re-read every run.
- `landed_block_forced_order_pending`: a forced order in a foreign block's
  window cannot be read back yet. Wait.
- `landed_block_awaiting_da`, `landed_block_da_refetch_pending`: a foreign
  block's DA payload is not available yet, or a retained one failed to
  verify and is being fetched again. Wait; check the DA committee if it
  stays.
- `mpf_closure_missing`: the rebase's native restore found no root of the
  processed landed chain retained in full in the native MPF store. Stop the
  node, install at `LEDGER_MPF_DB_PATH` a native MPF store that retains the
  root in full, and restart (plan §7.5, R6).
- `native_mpf_restore_index_cap_exceeded`: the restore target's full index
  is over a cap; the node needs a build whose caps cover it.
- `native_mpf_restore_read_transient`: reading the root's closure from the
  native MPF store failed; retried, escalated after ten minutes. Check the
  store's disk.
- `landed_block_rebase_failed`, `landed_block_replay_incomplete`,
  `landed_block_replay_failed`: a rebase, or the import or replay of a
  landed block, failed for a reason it could not pin on the block. A
  transient failure is retried on the driver's backoff; any other (and every
  `landed_block_replay_incomplete`) is read again on the next follower
  change. Read the detail.
- `confirmed_ledger_base_mismatch`: a fold's base is not what the block
  names; nothing is written. `confirmed_ledger_behind`: the merged queue
  root is on no lineage the confirmed ledger can reach. Neither occurs on an
  honest chain with an intact store, and no timer retries either; report it
  with the detail.
- `landed_blocks_waiting`: the follower write gate refused a write, or the
  follower moved off the run's view. Wait.
- `landed_block_own_revival_pending`, `confirmed_ledger_own_block_pending`,
  `landed_block_rebase_pending`: this node's own block waits to be revived
  or applied locally, or the working ledger waits for the rebase. Wait.

## Watcher `/readyz` reasons

The watcher's `/readyz` answers 200 when ready, else 503 with `reasons` and
`l1` (the L1 reasons with their detail); `/v1/status` carries the same
`readinessReasons` and the `l1Degradations`, which never fail readiness. No
reason below stops a running watcher. A startup stage that fails on a
transient a restart may clear (L1 reads past the startup budget, a system
error such as a port in use) exits the process non-zero, its supervisor's
restart policy (`restart: unless-stopped` in
`demo/midgard-watcher/compose.yaml`) being the retry after that. A follower
whose store transients outlast 15 min exits it the same way. A refused
process configuration exits 78, before the operations server binds. This list covers the reasons and
degradations added for the L1 follower; the full set is
`WatcherOperationsReadinessReason` in
`demo/midgard-watcher/src/runtime/operations-observability.watcher-operations-metrics.ts`.

- `startup:<stage>`: the operations server binds before the L1-dependent
  startup stages, and until the runtime's observability exists `/readyz`
  names the stage most recently begun (`l1_node_identity`,
  `workflow_readiness`, `protocol_parameters`). The body's `startup` field
  holds the stage's latest report; `outcome: "pending"` with `error` and
  `retryAfterMs` means it is waiting out an unanswering node. A stage waits
  out an unanswering follower or node transport for at most 10 min
  (`WATCHER_STARTUP_L1_BUDGET_MS`); past it the stage fails with
  `watcher_l1_unavailable` and the process exits. `/v1/status` answers 200
  meanwhile. Wait; check the node if it lasts.
- `startup_failed`: a startup stage failed on something no restart is known
  to repair (a configuration or deployment-identity refusal, a malformed
  read). The watcher stays up and unready under it; the detail names the
  stage and the error. Fix the cause, then restart the watcher.
- `fault_proof_objective_unreadable`: at startup an objective's workflow
  journal could not be read (a removal interrupted part way, a symlink, a
  corrupt journal). The objective is held, never run; clears once its header
  leaves the finalized queue, which forgets its rows. Report it with the
  detail.
- `fault_proof_objective_cleanup_failed`: a released or final objective's
  workflow directory, or a removal's tombstone under
  `<journal root>/fault-proofs/.removing`, could not be removed;
  `supervisor.objectiveCleanupFailures` names each. Every admission retries;
  fix the permissions or disk and wait.
- `fault_proof_l1_refused:store_inconsistent`: the follower's stored facts
  contradict one another (two unit histories place one transaction at
  different points). The objective is held; report it with the detail.
- `l1_follower_loop_failed`: the follow loop threw. The loop retries every
  transient chain, node and store failure itself, so a throw is a defect no
  retry is known to repair: the loop is not restarted, and the reason stands
  until the watcher is restarted. Report the detail, then restart the
  watcher.
- `deadline_at_risk`, `deadline_unsafe`: a proof deadline is near or past its
  safe start. The watcher keeps running and keeps proving; read
  `deadlineHealth` and `remainingSafeStartMs` in `/v1/metrics`.

Degradations (`l1Degradations`, status and metrics only):
`l1_tx_inputs_unreadable` and `l1_event_refusals_unreadable` name a failed
read of the tx-inputs assessment or of the refusals table, with count 1 and
the error as detail; they clear on the next good read.

## Logs

| Service                                                                                       | Where the log goes                                                                                                                                                               | Kept                                                                |
| --------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| Demo stack `midgard-node`, `midgard-node-migrate`, Prometheus, Loki, cAdvisor, Grafana, Tempo | Docker json-file: `docker compose $F logs --no-color midgard-node`                                                                                                               | One 1 MB file (`x-logging` anchor `default-logging`)                |
| Demo stack Postgres                                                                           | Docker json-file                                                                                                                                                                 | Three 10 MB files                                                   |
| Demo stack, aggregated                                                                        | Promtail ships container logs to Loki; browse them in Grafana on host port 3001                                                                                                  | Per the Loki configuration                                          |
| Kupmios cardano-node, Ogmios, Kupo                                                            | Docker default logging for their services in `docker-compose.kupmios.yaml`                                                                                                       | Docker default                                                      |
| Phase4 cardano-node, Ogmios, Kupo, Postgres                                                   | Docker default logging; read with `docker compose --project-name "$MIDGARD_PHASE4_COMPOSE_PROJECT" --file …/phase4-process/compose.yaml logs <service>` after sourcing `run.env` | Until the container is removed; `restart: "no"` keeps a crashed one |
| Phase4 script compose output                                                                  | A transient `work/compose.<pid>.log` in the run directory, printed when a compose command fails and then deleted                                                                 | Only on failure, on the terminal                                    |
| Phase4 acceptance nodes                                                                       | `<MIDGARD_PHASE4_PROCESS_RUN_DIR>/<scenario label>/<nodeId>.log`; default run dir `logs/phase4-process-<timestamp>` resolved against the command's working directory             | Kept                                                                |
| Watcher journeys                                                                              | `<workflow journal directory>/<command>.log`, opened in append mode, next to each attempt's JSON record (`devnet/watcher-journeys/process.ts`)                                   | Kept; append mode, so earlier attempts are in the same file         |
| A process you started with `e2e-start-service`                                                | The `--raw-log` path you gave, with the PID in `--pid-file`                                                                                                                      | Kept                                                                |
| DA committee node run by hand                                                                 | Its stdout and stderr                                                                                                                                                            | Wherever you redirected it                                          |

Reading a crash:

- **Copy a demo-stack log out before anything restarts it.**
  `midgard-node` has `restart: always` and a 1 MB single-file log, so a crash
  loop overwrites the first failure. `docker compose $F logs --no-color
midgard-node > /somewhere/node-crash.log`.
- For an append-mode log (watcher journeys), find the start of the attempt you
  care about; an old failure higher up is not the current one. `devnet-wait`
  scans only bytes written after it started for the same reason.
- The fatal startup lines are listed in the SKILL.md section "Reading a
  crash" and in `DEFAULT_FATAL_PATTERNS` in
  `.agents/skills/running-the-devnet/scripts/devnet-wait.mjs`.
