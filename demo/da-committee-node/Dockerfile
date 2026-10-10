# syntax=docker/dockerfile:1.7
# Built from demo/. This isolated image owns its copied source and output tree.
# Use the same compiler recipes directly; shared-checkout leases and Git
# receipts belong to contributor builds, and the repository tooling is outside
# this demo context.

# The node transport sidecar the committee's L1 follower reads its local node
# through; the same recipe as CI and the node and watcher images.
FROM golang:1.26.5-bookworm AS l1-node-transport
WORKDIR /src/native
COPY l1-node-transport/native/go.mod l1-node-transport/native/go.sum ./
RUN --mount=type=cache,id=midgard-l1-node-transport-go-mod,target=/go/pkg/mod \
  go mod download && go mod verify
COPY l1-node-transport/native ./
RUN --mount=type=cache,id=midgard-l1-node-transport-go-mod,target=/go/pkg/mod \
  --mount=type=cache,id=midgard-l1-node-transport-go-build,target=/root/.cache/go-build \
  test "$(go version | awk '{print $3}')" = "go1.26.5" \
  && go test ./... \
  && CGO_ENABLED=0 go build -trimpath \
    -ldflags="-s -w -buildid=" \
    -o /out/midgard-l1-node-transport .

FROM node:22.22.2 AS build
RUN corepack enable
WORKDIR /workspace/demo
COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY patches ./patches
COPY vendor ./vendor
COPY lucid-midgard/package.json ./lucid-midgard/package.json
COPY l1-node-transport/package.json ./l1-node-transport/package.json
COPY midgard-core/package.json ./midgard-core/package.json
COPY midgard-sdk/package.json ./midgard-sdk/package.json
COPY midgard-validation/package.json ./midgard-validation/package.json
COPY midgard-fault-proofs/package.json ./midgard-fault-proofs/package.json
COPY midgard-l1-follower/package.json ./midgard-l1-follower/package.json
COPY midgard-node/package.json ./midgard-node/package.json
COPY midgard-node-tools/package.json ./midgard-node-tools/package.json
COPY midgard-watcher/package.json ./midgard-watcher/package.json
COPY midgard-test-support/package.json ./midgard-test-support/package.json
COPY da-committee-node/package.json ./da-committee-node/package.json
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm install --frozen-lockfile --filter da-committee-node...
COPY lucid-midgard ./lucid-midgard
COPY l1-node-transport ./l1-node-transport
COPY midgard-core ./midgard-core
COPY midgard-sdk ./midgard-sdk
COPY midgard-validation ./midgard-validation
COPY midgard-fault-proofs ./midgard-fault-proofs
COPY midgard-l1-follower ./midgard-l1-follower
COPY da-committee-node ./da-committee-node
RUN pnpm --filter @al-ft/lucid-midgard run build:contrib-raw && pnpm --filter @al-ft/l1-node-transport run build:contrib-raw && pnpm --filter @al-ft/midgard-core run build:contrib-raw && pnpm --filter @al-ft/midgard-sdk run build:contrib-raw && pnpm --filter @al-ft/midgard-validation run build:contrib-raw && pnpm --filter @al-ft/midgard-fault-proofs run build:contrib-raw && pnpm --filter @al-ft/midgard-l1-follower run build:contrib-raw && pnpm --filter da-committee-node run build:contrib-raw
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm --filter da-committee-node deploy --prod /prod/da
FROM node:22.22.2-bookworm-slim
WORKDIR /app
COPY --from=build --chown=node:node /prod/da ./
# CARDANO_L1_NODE_TRANSPORT_BINARY_PATH. The full stack mounts its own host
# build over another path instead (midgard-node-tools full-stack/native-ledger.ts).
COPY --from=l1-node-transport /out/midgard-l1-node-transport /usr/local/bin/midgard-l1-node-transport
RUN mkdir -p /var/lib/midgard-da && chown node:node /var/lib/midgard-da
USER node
ENV NODE_ENV=production
ENTRYPOINT ["node"]
CMD ["dist/index.js"]
