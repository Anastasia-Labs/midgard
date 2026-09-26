# Endpoints, ports and logs

Verified against the tree as of 2026-09-25. Ports are the defaults; a phase4 run
and the acceptance harness override them, so read the run's `run.env` or the
harness environment before building a URL.

## Endpoints

| Service                 | Default port                                                                      | Path                        | Bar   | Answer                                                                                                                                                                             |
| ----------------------- | --------------------------------------------------------------------------------- | --------------------------- | ----- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| midgard-node            | `PORT`, 3000                                                                      | `/healthz`                  | Live  | 200 `{"status":"ok","now":…}` whenever the HTTP server runs                                                                                                                        |
| midgard-node            | `PORT`, 3000                                                                      | `/readyz`                   | Ready | 200 with `ready: true`, else 503 with `ready: false` and `reasons`                                                                                                                 |
| midgard-node            | `PORT`, 3000                                                                      | `/pipeline-status`          | Work  | Commit, confirmation and merge pipeline state                                                                                                                                      |
| midgard-node metrics    | `PROM_METRICS_PORT`, 9464                                                         | `/metrics`                  | Work  | Prometheus text; served only when `listen` runs with `--with-monitoring`                                                                                                           |
| DA committee node       | `DA_COMMITTEE_API_PORT`, 8787                                                     | `/healthz`                  | Live  | 200 `{"ok":true}`                                                                                                                                                                  |
| DA committee node       | `DA_COMMITTEE_API_PORT`, 8787                                                     | `/readyz`                   | Ready | 200 or 503 with the readiness snapshot: `l1Source.status` (`uninitialized`, `healthy`, `quarantined`), `counts.signatures`, `counts.submittedOrConfirmedL1Attestations`, `reasons` |
| DA committee node       | `DA_COMMITTEE_API_PORT`, 8787                                                     | `/v1/manifest`              | -     | The committee's runtime manifest                                                                                                                                                   |
| Watcher authority       | `WATCHER_AUTHORITY_PORT`, 7401                                                    | `/v1/identity`              | Live  | 200, or 401 without a bearer                                                                                                                                                       |
| Watcher operations      | `WATCHER_OPERATIONS_PORT`, 7402                                                   | `/v1/status`, `/v1/metrics` | Ready | Need a bearer; 401 without one                                                                                                                                                     |
| Kupo                    | 1442 (demo stack `KUPO_PORT`), 2442 (phase4)                                      | `/health`                   | Ready | 202 while replaying, 200 once caught up; JSON body with `connection_status` and `most_recent_checkpoint`                                                                           |
| Ogmios                  | 1337 (demo stack `OGMIOS_PORT`), 2337 (phase4)                                    | `/health`                   | Ready | Ogmios health JSON                                                                                                                                                                 |
| Phase4 acceptance nodes | `MIDGARD_PHASE4_NODE_A_PORT` 3101, `_B_PORT` 3102; metrics `_A_METRICS_PORT` 4101 | `/readyz`, `/metrics`       | Ready | As midgard-node                                                                                                                                                                    |

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
- Acceptance ports: `demo/midgard-node-tools/src/commands/e2e-pipelined-commit-process-acceptance.ts`.

### What each healthcheck really proves

- The demo `midgard-node` healthcheck fetches `/readyz` and passes on any 2xx
  (`demo/midgard-node/docker-compose.yaml`). A healthy container is bar 3.
- The kupmios Kupo healthcheck greps for `HTTP/… 200`, so a replaying Kupo
  (202) is unhealthy and the node does not start against it.
- Phase4 `bootstrap.sh` waits with `wait_http`, which is `curl --fail`: any
  status below 400 passes, **including Kupo's 202**. Bootstrap's wait is bar 2
  for Kupo; the later snapshot step demands `connection_status: "connected"`
  (`parse_kupo_checkpoint`). `[review]`
- Both watcher healthchecks accept 200 **or 401**, so they prove only that the
  HTTP server answers (bar 2). Nothing without the operations bearer shows
  watcher readiness. `[review]`
- Node `/readyz` reasons you will meet at startup include
  `history_owner_not_ready`, `history_follower_lagging:<lag>:<max>`,
  `provider_query_unhealthy:l1-provider`, `native_mpf_owner_unavailable` and
  `unfinished_local_mutation_jobs:<n>`, on top of the worker-heartbeat,
  backlog and database checks. `/readyz` never checks that blocks are
  committing; that is bar 4.

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
| DA committee node run by hand                                                                 | Its stdout and stderr; the L1-view exit writes one JSON line to stderr                                                                                                           | Wherever you redirected it                                          |

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
