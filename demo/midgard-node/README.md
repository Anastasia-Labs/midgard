# Midgard Node

Server application with GET and POST endpoints for interacting with Midgard.

## How to Run

### With Docker

Using Docker, you can run Midgard node on `localhost:3000` (or another port)
quite easily.

0. If you don't have Docker yet or want to update, follow this [GUIDE](https://docs.docker.com/engine/install/). After installation, do not forget to execute also the [POST-INSTALLATION STEPS](https://docs.docker.com/engine/install/linux-postinstall/#manage-docker-as-a-non-root-user) to avoid using sudo with Docker.

1. Run Docker daemon if it's not running already:

   ```sh
   sudo dockerd
   ```

2. Pack the `midgard-sdk` tarball (see [here](../midgard-sdk/README.md)).

3. Prepare your `.env` file. You can use `.env.example` as your starting point:

   ```sh
   cd ../midgard-node
   cp .env.example .env
   ```

   1. When running with `L1_PROVIDER=Kupmios`, you do not need to fill out
      `L1_BLOCKFROST_API_URL` and `L1_BLOCKFROST_KEY`. The remaining fields need
      to be filled out.

4. Install all the dependencies:

   ```sh
   pnpm install --frozen-lockfile
   ```

   1. If the install fails with an incorrect SHA, that most likely means
      `midgard-sdk` was updated recently, but `pnpm-lock.yaml` still expects the
      old hash. Update the SHA value inside the `pnpm-lock.yaml` file with the
      new one.
   2. Rerun `pnpm install --frozen-lockfile`. Now it should install correctly.

5. Build the midgard-node:

   ```sh
   pnpm build
   ```

6. Run the application stack:

   ```sh
   docker compose up -d

   # or this for development:
   docker compose -f docker-compose.dev.yaml up -d
   ```

Midgard node should be running on port `PORT` (from your `.env`).

You can view logs of `midgard-node` with `docker`:

```sh
# Change container's name as needed:
docker logs -f midgard-node-midgard-node-1
```

If you made any changes to `midgard-node` and had an image running, restart it
with the 3 steps:

```sh
docker compose down -v
docker compose up -d --build
```

### Without Docker (No Monitoring)

For running the node itself, a running PostgreSQL server is also needed. The
fields you most likely want to modify in your `.env` file are:

```sh
POSTGRES_USER=postgres
POSTGRES_PASSWORD=postgres
POSTGRES_DB=midgard
POSTGRES_HOST=localhost
LEDGER_MPT_DB_PATH=midgard-ledger-mpt-db
MEMPOOL_MPT_DB_PATH=midgard-mempool-mpt-db
```

With a properly setup database, the following set of commands should start the
most up to date `midgard-node`:

```sh
# Optional
nix develop

# Bundle the SDK
cd ../midgard-sdk
pnpm install
pnpm repack

# Go back to `midgard-node` and force reinstallation of the SDK (faster than
# `pnpm install --force`)
cd ../midgard-node
rm -rf node_module
pnpm install
pnpm listen
```

## Testing

### With Docker

```sh
docker compose run --rm midgard-node-tests
```

### Without Docker

```sh
cd midgard-node
pnpm test
```

## Valid Throughput Stress Test

Run a high-throughput submitter that builds and submits **valid Midgard-native**
transactions (parallel dependent chains), then prints submit/accept/reject rates
from Prometheus metrics.

```sh
cd midgard-node
pnpm stress:valid
```

Useful overrides:

```sh
STRESS_CHAIN_LENGTH=500 \
STRESS_MAX_CHAINS=6 \
STRESS_TARGET_ACCEPTED_TPS=600 \
pnpm stress:valid
```

Notes:

- This script reads `TESTNET_GENESIS_WALLET_SEED_PHRASE_A/B/C` from `.env`.
- It uses `/utxos` to pick spendable UTxOs and submits to `/submit`.
- It reports counters from `validation_accept_count_total`,
  `validation_reject_count_total`, and `tx_count_total`.

## Nominal Sustained Activity Test

Run a lower-rate sustained generator that:

- queries current Midgard state via `/utxos`,
- builds fresh valid Midgard-native txs on-demand (no huge prebuild),
- submits at randomized intervals to emulate real network activity.

```sh
cd midgard-node
pnpm stress:nominal
```

Examples:

```sh
# Run for 5 minutes, stop after 100 successful submits.
pnpm stress:nominal -- --duration 5m --target-txs 100

# Run for 10 minutes with slower sporadic traffic.
pnpm stress:nominal -- --duration 10m --target-txs 120 --min-interval-ms 1000 --max-interval-ms 9000
```

Useful environment overrides:

```sh
ACTIVITY_DURATION=10m
ACTIVITY_TARGET_TXS=100
ACTIVITY_MIN_INTERVAL_MS=750
ACTIVITY_MAX_INTERVAL_MS=7000
ACTIVITY_WALLET_MODE=random
ACTIVITY_SUBMIT_ENDPOINT=http://127.0.0.1:3000
ACTIVITY_METRICS_ENDPOINT=http://127.0.0.1:9464/metrics
```
