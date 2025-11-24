# Midgard Node

Server application with GET and POST endpoints for interacting with Midgard.

## How to Run

### With Docker

Using Docker, you can run Midgard node on `localhost:3000` (or another port)
quite easily.

1. Run Docker deamon if not already running:

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

### Local run with monitoring

- Run [starting script](local-run/start.sh) or see [the guide](local-run/MANUAL-RUN.md) for details.

### Local run without monitoring

- Build and run all necessary packages:

```sh
# Optional
nix develop

cd ../midgard-sdk
pnpm install
pnpm run repack

cd ../midgard-node
pnpm install
pnpm run listen
```

## Testing

### Docker

- For running tests inside docker container:

```sh
docker-compose run --rm midgard-node-tests
```

### Local run

- For running tests locally:
```sh
cd midgard-node
pnpm test
```
