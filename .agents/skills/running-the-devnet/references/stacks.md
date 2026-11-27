# Launching, relaunching and resetting each stack

Verified against the tree as of 2026-09-25. Paths are from the repository root
unless a block says otherwise.

## Isolated-chain devnet

A private Cardano chain (one pool, network magic 424242 by default) with
Ogmios, Kupo and Postgres, under the watcher journeys and `devnet-stack`. Its
README is the authority:
[`demo/midgard-node-tools/devnet/isolated-chain/README.md`](../../../../demo/midgard-node-tools/devnet/isolated-chain/README.md).
Devnet rules for this directory are in
[`demo/midgard-node-tools/devnet/AGENTS.md`](../../../../demo/midgard-node-tools/devnet/AGENTS.md).

Every run is a directory. The run directory holds the chain, the Kupo index,
Postgres data and the deployment manifest, all bind-mounted into one compose
project. A run is created once; it is never regenerated in place.

### Generate (no service starts, but Docker runs `cardano-cli`)

From `demo/midgard-node-tools`:

```bash
export MIDGARD_ISOLATED_CHAIN_RUN_DIR=/absolute/fresh/run-dir      # must not exist
export MIDGARD_ISOLATED_CHAIN_RUN_ID=<unique-id>                   # names the project
export MIDGARD_ISOLATED_CHAIN_OGMIOS_PORT=<free> MIDGARD_ISOLATED_CHAIN_KUPO_PORT=<free> \
  MIDGARD_ISOLATED_CHAIN_POSTGRES_PORT=<free>
devnet/isolated-chain/scripts/generate.sh
```

What the generator enforces [script: `isolated-chain/scripts/generate.sh`]:

- The run directory must be absolute and must not exist.
- Network magic 1, 2 and 764824073 (public networks) are refused.
- The project and database are `midgard_isolated_chain_<slug of run id>`; the
  run id defaults to the directory basename.
- Unset ports default to 2337 / 2442 / 5544 for every run. Two runs with
  default ports cannot run at once.
- Genesis starts two minutes after generation unless
  `MIDGARD_ISOLATED_CHAIN_GENESIS_START_TIME` is set.
- `validate-custom-chain-config.sh` fails unless the genesis protocol major is
  exactly 11 and Conway starts at epoch zero.

It writes `run.env`, which every later script sources and which may not
redirect the run directory [script: `common.sh` `require_run_dir`].

### Inspect a run

```bash
set -a; . "$MIDGARD_ISOLATED_CHAIN_RUN_DIR/run.env"; set +a
docker compose --project-name "$MIDGARD_ISOLATED_CHAIN_COMPOSE_PROJECT" \
  --file demo/midgard-node-tools/devnet/isolated-chain/compose.yaml ps
docker compose --project-name "$MIDGARD_ISOLATED_CHAIN_COMPOSE_PROJECT" \
  --file demo/midgard-node-tools/devnet/isolated-chain/compose.yaml \
  logs --no-color --tail 200 kupo
```

`compose.yaml` needs `MIDGARD_ISOLATED_CHAIN_COMPOSE_PROJECT`, `MIDGARD_ISOLATED_CHAIN_RUN_DIR`,
`MIDGARD_ISOLATED_CHAIN_POSTGRES_PASSWORD` and `MIDGARD_ISOLATED_CHAIN_POSTGRES_DATABASE`, which
is why `run.env` is sourced first. Every isolated-chain service has `restart: "no"`: a
crashed container stays down and keeps its log.

### Relaunch

- A code change needs a **new run** in a fresh directory.
- `scripts/up.sh <run dir>` starts the run's four L1 services as a block
  producer (or again after `down.sh`), appends each service's log to
  `<run dir>/logs/<service>.log` through a detached `compose logs --follow`,
  opens the node socket to the host user and waits for Ogmios and Kupo
  `/health`. `scripts/down.sh <run dir>` runs `compose down` without
  `--volumes` and stops those followers; the run directory and its state stay.
  Both read every setting from the run's `run.env`, never from the caller's
  shell, and add `compose.host-paths.json` when the run has one
  [script: `up.sh`, `down.sh`].
- To discard a run, `down.sh` it and remove the run directory as a whole.
  Removing only `postgres/`, `kupo/` or `cardano/` would pair new local state
  with old chain state, which
  [state-reset](../../../../docs/agents/state-reset.md) forbids. `[review]`

## Watcher-journey devnet

The same generator, wrapped so the watcher can read genesis at host paths.
From `demo/midgard-node-tools`:

```bash
export MIDGARD_WATCHER_JOURNEY_RUN_DIR=/absolute/fresh/run-dir
sh devnet/watcher-journeys/scripts/generate.sh
```

It sets `MIDGARD_ISOLATED_CHAIN_RUN_ID=watcher_journeys_<basename>`, runs the isolated-chain
generator, rewrites genesis paths in `<run dir>/config/config.json`, and writes
`compose.host-paths.json`. It prints `servicesStarted=false`. The comment in the
script says: never change genesis after starting these services. Start and
stop the services with the isolated-chain `up.sh` / `down.sh` above.

The journeys themselves run as a vitest suite over
`devnet/watcher-journeys/**/*.test.ts` with
`vitest.watcher-journeys.config.ts` (one fork, one-hour test timeout); they
skip unless `MIDGARD_WATCHER_JOURNEY_RUN_DIR` is set. The harness and its run
artifacts are described in
[`docs/fault-proofs/automatic-watcher-journeys.md`](../../../../docs/fault-proofs/automatic-watcher-journeys.md).

## Demo-node Docker stack

`demo/midgard-node/docker-compose.yaml` (node, Postgres,
Prometheus, Loki, Promtail, cAdvisor, Grafana, Tempo) plus
`docker-compose.kupmios.yaml` (Mithril bootstrap, cardano-node, Ogmios, Kupo).
It follows the network named by `NETWORK`, restored from a Mithril snapshot;
it is not a private chain. The bring-up order is in
[`demo/midgard-node/README.md`](../../../../demo/midgard-node/README.md),
"With Docker"; the Preprod acceptance runbook on top of it is
[midgard-e2e-acceptance](../../midgard-e2e-acceptance/SKILL.md).

From `demo/midgard-node`, with `F="-f docker-compose.yaml -f docker-compose.kupmios.yaml"`.
In a linked git worktree, replace `docker compose` with
`demo/midgard-node/scripts/operator-compose.sh`: the main checkout's stack is project
`midgard-node` with fixed container names (`prometheus`, `loki`, `promtail`,
`grafana`) and fixed host ports, and bare compose in a worktree would take
them over. The wrapper gives the worktree project `midgard-node-<path hash>`,
container names prefixed with it, and a block of 100 host ports from
20000 + 100 × slot; `--print-env` lists them. A
port or `COMPOSE_PROJECT_NAME` already set in the environment or `.env` wins.

| Step                    | Command                                                                      |
| ----------------------- | ---------------------------------------------------------------------------- |
| L1 and Postgres first   | `docker compose $F up -d postgres cardano-node cardano-config-export`        |
| Check health            | `docker compose $F ps`                                                       |
| Full stack              | `docker compose $F up -d`                                                    |
| Relaunch after a change | `docker compose $F stop midgard-node` then `docker compose $F up -d --build` |

- `midgard-node` creates its database schema when it starts, on an empty
  database.
- `midgard-node` waits for healthy Postgres and cardano-node and the finished
  config export, and its own healthcheck is `/readyz` (5 s interval, 60
  retries, 30 s start period).
- `listen` fails closed without `deploymentInfo/contract-deployment-info.json`
  and the DA producer manifest (README step 5).
- `midgard-node` and Postgres have `restart: always`. A node that crashes on
  start restarts forever; check `docker compose $F ps` for a restart loop
  rather than waiting on it.
- Changing `NETWORK` needs `./cardano/db` and `./cardano/kupo` cleared first
  (README). Those directories are the L1 provider's state, not the Midgard
  deployment; the e2e skill forbids deleting them for a Midgard reset.

Without Docker, the node runs as `node dist/index.js listen` from
`demo/midgard-node` after `pnpm build` (README, "Without Docker").

## DA committee node

Built and run from `demo/da-committee-node` with `pnpm run build` and
`node dist/index.js`; configuration is environment variables, optionally from
`./config.yaml` (its README). Start enough committee members to meet the
threshold before the producer's DA preflight (e2e skill).
