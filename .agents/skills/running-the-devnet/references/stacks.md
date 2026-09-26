# Launching, relaunching and resetting each stack

Verified against the tree as of 2026-09-25. Paths are from the repository root
unless a block says otherwise.

## Phase4 isolated devnet

A private Cardano chain (one pool, network magic 424242 by default) with
Ogmios, Kupo and Postgres, used by the pipelined-commit process gate. Its
README is the authority:
[`demo/midgard-node-tools/devnet/phase4-process/README.md`](../../../../demo/midgard-node-tools/devnet/phase4-process/README.md).
Devnet rules for this directory are in
[`demo/midgard-node-tools/devnet/AGENTS.md`](../../../../demo/midgard-node-tools/devnet/AGENTS.md).

Every run is a directory. The run directory holds the chain, the Kupo index,
Postgres data, the deployment manifest, secrets and snapshots, all bind-mounted
into one compose project. A run is created once and then reset to its snapshot;
it is never regenerated in place.

### Generate (no service starts, but Docker runs `cardano-cli`)

From `demo/midgard-node-tools`:

```bash
export MIDGARD_PHASE4_RUN_DIR=/absolute/fresh/run-dir      # must not exist
export MIDGARD_PHASE4_RUN_ID=<unique-id>                   # names the project
export MIDGARD_PHASE4_OGMIOS_PORT=<free> MIDGARD_PHASE4_KUPO_PORT=<free> \
  MIDGARD_PHASE4_POSTGRES_PORT=<free>
export MIDGARD_PHASE4_WALLET_ENV_FILE=/absolute/private/wallets.env
devnet/phase4-process/scripts/generate.sh
cp /absolute/private/node.env "$MIDGARD_PHASE4_RUN_DIR/secrets/node.env"
chmod 600 "$MIDGARD_PHASE4_RUN_DIR/secrets/node.env"
```

What the generator enforces [script: `phase4-process/scripts/generate.sh`]:

- The run directory must be absolute and must not exist.
- Network magic 1, 2 and 764824073 (public networks) are refused.
- The project and database are `midgard_phase4_process_<slug of run id>`; the
  run id defaults to the directory basename.
- Unset ports default to 2337 / 2442 / 5544 for every run. Two runs with
  default ports cannot run at once.
- Genesis starts two minutes after generation unless
  `MIDGARD_PHASE4_GENESIS_START_TIME` is set.
- `validate-custom-chain-config.sh` fails unless the genesis protocol major is
  exactly 11 and Conway starts at epoch zero.

It writes `run.env`, which every later script sources and which may not
redirect the run directory [script: `common.sh` `require_run_dir`].

### Bootstrap and snapshot

```bash
export MIDGARD_PHASE4_RUN_DIR=/absolute/fresh/run-dir
devnet/phase4-process/scripts/bootstrap.sh
devnet/phase4-process/scripts/capture-snapshot.sh
```

`bootstrap.sh` starts `cardano-node ogmios kupo postgres`, waits up to 180
attempts each for Ogmios and Kupo `/health` on the run's ports
[script: `common.sh` `wait_http`], funds the wallets, publishes the reference
scripts, initializes the protocol and registers the operator. Its last output
is `bootstrapComplete=true`. `capture-snapshot.sh` freezes the matched snapshot
that every reset restores.

### Run the acceptance gate

The command, with the environment it needs, is in the README section
"Acceptance reset command" (`pnpm run accept:phase4:pipelined-process` from
`demo/midgard-node-tools`). It starts its own `midgard-node listen` processes
and waits on their `/readyz`.

### Inspect a run

```bash
set -a; . "$MIDGARD_PHASE4_RUN_DIR/run.env"; set +a
docker compose --project-name "$MIDGARD_PHASE4_COMPOSE_PROJECT" \
  --file demo/midgard-node-tools/devnet/phase4-process/compose.yaml ps
docker compose --project-name "$MIDGARD_PHASE4_COMPOSE_PROJECT" \
  --file demo/midgard-node-tools/devnet/phase4-process/compose.yaml \
  logs --no-color --tail 200 kupo
```

`compose.yaml` needs `MIDGARD_PHASE4_COMPOSE_PROJECT`, `MIDGARD_PHASE4_RUN_DIR`,
`MIDGARD_PHASE4_POSTGRES_PASSWORD` and `MIDGARD_PHASE4_POSTGRES_DATABASE`, which
is why `run.env` is sourced first. Every phase4 service has `restart: "no"`: a
crashed container stays down and keeps its log.

### Relaunch and reset

- `reset.sh` restores the matched snapshot and restarts Cardano, then Ogmios,
  then Kupo. It needs `MIDGARD_PHASE4_SCENARIO_LABEL`, which the acceptance
  command supplies. It checks source, `dist`, image, configuration, snapshot,
  PHAS proof and transaction-body drift **before** stopping anything, so a
  rejected reset leaves the run untouched [script: `reset.sh`].
- Because that drift check binds the source and `dist` of both `midgard-node`
  and `midgard-node-tools`, a code change needs a **new run**: generate,
  bootstrap and snapshot again in a fresh directory.
- `t1-recover.sh` is the only accepted short-rollback command (README).
- There is no teardown script. To discard a run, stop its compose project
  (`... compose.yaml down` with `run.env` sourced) and remove the run directory
  as a whole. Removing only `postgres/`, `kupo/` or `cardano/` would pair new
  local state with old chain state, which
  [state-reset](../../../../docs/agents/state-reset.md) forbids. `[review]`

## Watcher-journey devnet

The same generator, wrapped so the watcher can read genesis at host paths.
From `demo/midgard-node-tools`:

```bash
export MIDGARD_WATCHER_JOURNEY_RUN_DIR=/absolute/fresh/run-dir
sh devnet/watcher-journeys/scripts/generate.sh
```

It sets `MIDGARD_PHASE4_RUN_ID=watcher_journeys_<basename>`, runs the phase4
generator, rewrites genesis paths in `config/config.json`, and writes
`compose.host-paths.json`. It prints `servicesStarted=false`. The comment in the
script says: never change genesis after starting these services.

The journeys themselves run as a vitest suite over
`devnet/watcher-journeys/**/*.test.ts` with
`vitest.watcher-journeys.config.ts` (one fork, one-hour test timeout); they
skip unless `MIDGARD_WATCHER_JOURNEY_RUN_DIR` is set. Status, retained runs and
their logs are recorded in
[`docs/fault-proofs/automatic-watcher-journeys.md`](../../../../docs/fault-proofs/automatic-watcher-journeys.md).

## Demo-node Docker stack

`demo/midgard-node/docker-compose.yaml` (node, one-shot migration, Postgres,
Prometheus, Loki, Promtail, cAdvisor, Grafana, Tempo) plus
`docker-compose.kupmios.yaml` (Mithril bootstrap, cardano-node, Ogmios, Kupo).
It follows the network named by `NETWORK`, restored from a Mithril snapshot;
it is not a private chain. The bring-up order is in
[`demo/midgard-node/README.md`](../../../../demo/midgard-node/README.md),
"With Docker"; the Preprod acceptance runbook on top of it is
[midgard-e2e-acceptance](../../midgard-e2e-acceptance/SKILL.md).

From `demo/midgard-node`, with `F="-f docker-compose.yaml -f docker-compose.kupmios.yaml"`:

| Step                    | Command                                                                      |
| ----------------------- | ---------------------------------------------------------------------------- |
| L1 and Postgres first   | `docker compose $F up -d postgres cardano-node-ogmios kupo`                  |
| Check health            | `docker compose $F ps`                                                       |
| Schema                  | `docker compose $F run --rm midgard-node-migrate`                            |
| Full stack              | `docker compose $F up -d`                                                    |
| Relaunch after a change | `docker compose $F stop midgard-node` then `docker compose $F up -d --build` |

- `up` runs `midgard-node-migrate` first and starts `midgard-node` only after
  it exits successfully (`depends_on: service_completed_successfully`).
- `midgard-node` waits for healthy Postgres, Ogmios and Kupo, and its own
  healthcheck is `/readyz` (5 s interval, 60 retries, 30 s start period).
- Kupo's healthcheck passes only on HTTP 200, not 202, so the node does not
  start against an index that is still replaying.
- `listen` fails closed without `deploymentInfo/contract-deployment-info.json`
  and the DA producer manifest (README step 6).
- `midgard-node` and Postgres have `restart: always`. A node that crashes on
  start restarts forever; check `docker compose $F ps` for a restart loop
  rather than waiting on it.
- Changing `NETWORK` needs `./cardano/db` and `./cardano/kupo` cleared first
  (README). Those directories are the L1 provider's state, not the Midgard
  deployment; the e2e skill forbids deleting them for a Midgard reset.

Without Docker, the node runs as `node dist/index.js listen` from
`demo/midgard-node` after `pnpm build` and `node dist/index.js db:migrate`
(README, "Without Docker").

## DA committee node

Built and run from `demo/da-committee-node` with `pnpm run build` and
`node dist/index.js`; configuration is environment variables, optionally from
`./config.yaml` (its README). Start enough committee members to meet the
threshold before the producer's DA preflight (e2e skill).
