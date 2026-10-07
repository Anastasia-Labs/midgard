# syntax=docker/dockerfile:1.7

FROM node:22.22.2 AS base
RUN corepack enable

FROM rust:1.91.0-bookworm AS native-mpf-owner
WORKDIR /src
COPY midgard-node/native/mpf-event-flat-wasm ./mpf-event-flat-wasm
RUN cargo build --release --locked --bin architecture-g-owner \
  --manifest-path mpf-event-flat-wasm/Cargo.toml
RUN mkdir -p /native \
  && cp mpf-event-flat-wasm/target/release/architecture-g-owner /native/ \
  && sha256sum /native/architecture-g-owner \
    | cut -d ' ' -f 1 > /native/architecture-g-owner.sha256

FROM base AS workspace
WORKDIR /workspace/demo
COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY patches ./patches
# The vendored uplc build that package.json overrides resolve to.
COPY vendor ./vendor
# Every workspace manifest, so the frozen lockfile resolves each `workspace:*`
# edge (production and dev) that midgard-node declares; only the production
# chain's sources are copied and built below, and `deploy --prod` ships only
# that chain. midgard-node-tools is dev-only and never enters the image.
COPY lucid-midgard/package.json ./lucid-midgard/package.json
COPY l1-node-transport/package.json ./l1-node-transport/package.json
COPY midgard-core/package.json ./midgard-core/package.json
COPY midgard-validation/package.json ./midgard-validation/package.json
COPY midgard-fault-proofs/package.json ./midgard-fault-proofs/package.json
COPY midgard-sdk/package.json ./midgard-sdk/package.json
COPY midgard-watcher/package.json ./midgard-watcher/package.json
COPY da-committee-node/package.json ./da-committee-node/package.json
COPY midgard-node-tools/package.json ./midgard-node-tools/package.json
COPY midgard-test-support/package.json ./midgard-test-support/package.json
COPY midgard-node/package.json ./midgard-node/package.json
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm install --frozen-lockfile --filter midgard-node...

# This isolated image owns its copied source and output tree. Use the same
# compiler recipes directly; shared-checkout leases and Git receipts belong to
# contributor builds, and the repository tooling is outside this demo context.
FROM workspace AS build
COPY lucid-midgard ./lucid-midgard
COPY l1-node-transport ./l1-node-transport
COPY midgard-core ./midgard-core
COPY midgard-validation ./midgard-validation
COPY midgard-fault-proofs ./midgard-fault-proofs
COPY midgard-sdk ./midgard-sdk
COPY midgard-node ./midgard-node
# midgard-node's build compiles only itself, and its runtime imports each of
# these packages' dist, so every one is built here, dependencies first.
RUN pnpm --filter @al-ft/lucid-midgard run build:contrib-raw
RUN pnpm --filter @al-ft/l1-node-transport run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-core run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-sdk run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-validation run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-fault-proofs run build:contrib-raw
RUN pnpm --filter midgard-node run build:contrib-raw
# Same store as the install step: `deploy --prod` re-resolves the git-hosted
# merkle-patricia-forestry dependency, and only a store that already holds the
# prepared package avoids re-running its `prepare` build (which needs the
# package's dev tooling that a production install never fetches).
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm --filter midgard-node deploy --prod /prod/midgard-node

FROM base AS final
WORKDIR /app
COPY --from=build /prod/midgard-node ./
COPY --from=native-mpf-owner /native /app/native
COPY --from=onchain /plutus.json /onchain/aiken/plutus.json
COPY --from=onchain /plutus.json.deployment.json /onchain/aiken/plutus.json.deployment.json
ENV NODE_ENV=production
CMD ["node", "./dist/index.js", "listen"]
