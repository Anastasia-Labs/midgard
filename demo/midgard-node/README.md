# Midgard Node

Server application with GET and POST endpoints for interacting with Midgard.

## How to Run

### With Docker

Using Docker, you can run Midgard node on `localhost:3000` (or another port)
quite easily.

1. Run Docker deamon if it's not running already:

```sh
sudo dockerd
```

2. Pack the `midgard-sdk` tarball (see [here](../midgard-sdk/README.md)).

3. Prepare your `.env` file. You can use `.env.example` as your starting point:

```sh
cp .env.example .env
```

4. Install all the dependencies and build:

```sh
cd ../midgard-node
pnpm install && pnpm build
```

5. Run the application stack:

```sh
docker-compose up -d

# or this for development:
sudo docker-compose -f docker-compose.dev.yaml up -d
```

Midgard node should be running on port `PORT` (from your `.env`).

You can view logs of `midgard-node` with `docker`:

```sh
# Change container's name as needed:
sudo docker logs -f midgard-node-midgard-node-1
```

If you made any changes to `midgard-node` and had an image running, restart it
with the 3 steps:

```sh
docker-compose down -v
docker image rm midgard-node-midgard-node
docker-compose up -d
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
docker-compose run --rm midgard-node-tests
```

### Without Docker

```sh
cd midgard-node
pnpm test
```
