# Midgard Demo

## Prerequisites

- Node.js 18+
- PNPM 9+
- Docker and Docker Compose

## Quick Start

1.Clean SDK:

```sh
cd midgard-sdk
pnpm reset
```

2.Clean Node:

```sh
cd ../midgard-node
pnpm clean
```

3.Start Services:

```sh
docker-compose up -d
```

This will start:

- Midgard Node
- PostgreSQL
- Prometheus metrics server
- OpenTelemetry collector

## Generate dummy transactions

See [Midgard Manager](https://github.com/HVining/midgard-manager#readme)

## Monitoring

Access monitoring interfaces at:

- Prometheus: http://localhost:9464/metrics

- OpenTelemetry: http://localhost:4318

- Grafana: http://localhost:3001
