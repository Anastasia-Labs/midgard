# Midgard Demo

## Prerequisites

- Node.js 18+
- PNPM 9+
- Docker and Docker Compose

## Quick Start

2. Install SDK's dependencies:

```sh
cd midgard-sdk
pnpm install
```

2. Clean SDK if needed:

```sh
cd midgard-sdk
pnpm reset
```

3. Clean Node if needed:

```sh
cd ../midgard-node
pnpm clean
```

4. Run Docker daemon if it's not running already:

```sh
sudo dockerd
```

5. Start Services:

```sh
docker-compose up -d
```

This will start:

- Midgard Node
- PostgreSQL
- Prometheus metrics server
- OpenTelemetry collector

6. You can view your containers using `docker`:

```sh
docker ps -a
```

7. You can view logs of `midgard-node` with `docker`:

```sh
# Change container's name as needed:
sudo docker logs -f midgard-node-midgard-node-1
```

If you faced an error regarding `DATABASE_PATH`, use the following command:

```sh
# Optional: You can view your docker images to get the correct name:
docker images

# Delete the last image:
docker image rm midgard-node-midgard-node --force

# And restart the services:
docker-compose up -d
```

Now you should be able to interact with `midgard-node`.

8. If you made any changes to `midgard-node` and had an image running, restart it with the 3 steps:

```sh
docker-compose down -v
docker image rm midgard-node-midgard-node --force
docker-compose up -d
```

## Generate dummy transactions

See [Midgard Manager](./midgard-manager/README.md)

## Monitoring

Access monitoring interfaces at:

- Prometheus: http://localhost:9464/metrics

- OpenTelemetry: http://localhost:4318

- Grafana: http://localhost:3001
