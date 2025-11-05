# Midgard Node – Demo CLI Application

## How to Run

### Local Run (without monitoring)

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

### Local Run (with monitoring)

For running the app locally with **full monitoring capabilities** (Prometheus metrics and Grafana dashboards), use the local monitoring setup:

```sh
cd local-monitoring
./start.sh
```

This will start:
- Midgard Node (API: http://localhost:3000, Metrics: http://localhost:9464)
- Prometheus (http://localhost:9090)
- Grafana with dashboards (http://localhost:3001)

To stop all services:
```sh
cd local-monitoring
./stop.sh
```

See [local-monitoring/README.md](local-monitoring/README.md) for detailed documentation.

## Build image

### Start docker daemon

```sh
nix develop
sudo $(which dockerd)
```

### Build & run image

```sh
SEED_PHRASE="your seed phrase" nix develop
sudo chown --recursive $(whoami):$(whoami) /var/run/docker.sock

docker run --rm --publish 3000:3000 -it -e SEED_PHRASE="$SEED_PHRASE" \
  $(docker build --build-context sdk=../midgard-sdk -q .)
```

### Test node

```sh
curl http://localhost:3000
```

### Testing

For local testing run

```sh
pnpm test
```

For testing inside docker container run

```sh
docker-compose run --rm midgard-node-tests
```
