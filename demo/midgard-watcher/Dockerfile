# syntax=docker/dockerfile:1.7

FROM golang:1.25.7-bookworm AS native-chain-sync
WORKDIR /src/native-chain-sync
COPY midgard-watcher/native-chain-sync/go.mod midgard-watcher/native-chain-sync/go.sum ./
RUN --mount=type=cache,id=midgard-watcher-go-mod,target=/go/pkg/mod \
  go mod download && go mod verify
COPY midgard-watcher/native-chain-sync ./
RUN --mount=type=cache,id=midgard-watcher-go-mod,target=/go/pkg/mod \
  --mount=type=cache,id=midgard-watcher-go-build,target=/root/.cache/go-build \
  test "$(go version | awk '{print $3}')" = "go1.25.7" \
  && go test ./... \
  && CGO_ENABLED=0 go build -trimpath \
    -ldflags="-s -w -buildid=" \
    -o /out/midgard-chain-sync .

FROM node:22.22.2 AS workspace
RUN corepack enable
WORKDIR /workspace/demo
COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY patches ./patches
COPY lucid-midgard/package.json ./lucid-midgard/package.json
COPY midgard-core/package.json ./midgard-core/package.json
COPY midgard-validation/package.json ./midgard-validation/package.json
COPY midgard-sdk/package.json ./midgard-sdk/package.json
COPY midgard-fault-proofs/package.json ./midgard-fault-proofs/package.json
COPY midgard-watcher/package.json ./midgard-watcher/package.json
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm install --frozen-lockfile --filter midgard-watcher...

FROM workspace AS build
COPY lucid-midgard ./lucid-midgard
COPY midgard-core ./midgard-core
COPY midgard-validation ./midgard-validation
COPY midgard-sdk ./midgard-sdk
COPY midgard-fault-proofs ./midgard-fault-proofs
COPY midgard-watcher ./midgard-watcher
RUN pnpm --filter midgard-watcher... build
RUN pnpm --filter midgard-watcher deploy --prod /prod/midgard-watcher

FROM node:22.22.2-bookworm-slim AS final
WORKDIR /app
COPY --from=build /prod/midgard-watcher ./
COPY --from=native-chain-sync /out/midgard-chain-sync /usr/local/bin/midgard-chain-sync
ENV NODE_ENV=production
ENTRYPOINT ["node", "./dist/cli.js"]
CMD ["start", "--config", "/etc/midgard/watcher-process.json"]
