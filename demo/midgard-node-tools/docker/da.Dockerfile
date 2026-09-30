# syntax=docker/dockerfile:1.7
FROM golang:1.25.7-bookworm AS native-chain-sync
WORKDIR /src/native-chain-sync
COPY midgard-watcher/native-chain-sync/go.mod midgard-watcher/native-chain-sync/go.sum ./
RUN --mount=type=cache,id=midgard-watcher-go-mod,target=/go/pkg/mod go mod download && go mod verify
COPY midgard-watcher/native-chain-sync ./
COPY midgard-watcher/tests/support/conway-block.hex ../tests/support/conway-block.hex
RUN --mount=type=cache,id=midgard-watcher-go-mod,target=/go/pkg/mod --mount=type=cache,id=midgard-watcher-go-build,target=/root/.cache/go-build go test ./... && CGO_ENABLED=0 go build -trimpath -ldflags="-s -w -buildid=" -o /out/midgard-chain-sync .

FROM node:22.22.2 AS build
RUN corepack enable
WORKDIR /workspace/demo
COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY patches ./patches
COPY vendor ./vendor
COPY lucid-midgard/package.json ./lucid-midgard/package.json
COPY midgard-core/package.json ./midgard-core/package.json
COPY midgard-sdk/package.json ./midgard-sdk/package.json
COPY midgard-validation/package.json ./midgard-validation/package.json
COPY midgard-fault-proofs/package.json ./midgard-fault-proofs/package.json
COPY midgard-node/package.json ./midgard-node/package.json
COPY midgard-node-tools/package.json ./midgard-node-tools/package.json
COPY midgard-watcher/package.json ./midgard-watcher/package.json
COPY midgard-test-support/package.json ./midgard-test-support/package.json
COPY da-committee-node/package.json ./da-committee-node/package.json
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm install --frozen-lockfile --filter da-committee-node...
COPY lucid-midgard ./lucid-midgard
COPY midgard-core ./midgard-core
COPY midgard-sdk ./midgard-sdk
COPY midgard-validation ./midgard-validation
COPY midgard-fault-proofs ./midgard-fault-proofs
COPY da-committee-node ./da-committee-node
RUN pnpm --filter @al-ft/lucid-midgard build && pnpm --filter @al-ft/midgard-core build && pnpm --filter @al-ft/midgard-sdk build && pnpm --filter @al-ft/midgard-validation build && pnpm --filter @al-ft/midgard-fault-proofs build && pnpm --filter da-committee-node build
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm --filter da-committee-node deploy --prod /prod/da
FROM node:22.22.2-bookworm-slim
WORKDIR /app
COPY --from=build --chown=node:node /prod/da ./
COPY --from=native-chain-sync /out/midgard-chain-sync /usr/local/bin/midgard-chain-sync
RUN mkdir -p /var/lib/midgard-da && chown node:node /var/lib/midgard-da
USER node
ENV NODE_ENV=production
ENTRYPOINT ["node"]
CMD ["dist/index.js"]
