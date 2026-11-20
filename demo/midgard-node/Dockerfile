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
COPY lucid-midgard/package.json ./lucid-midgard/package.json
COPY midgard-core/package.json ./midgard-core/package.json
COPY midgard-validation/package.json ./midgard-validation/package.json
COPY midgard-sdk/package.json ./midgard-sdk/package.json
COPY midgard-node/package.json ./midgard-node/package.json
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm install --frozen-lockfile --filter midgard-node...

FROM workspace AS build
COPY lucid-midgard ./lucid-midgard
COPY midgard-core ./midgard-core
COPY midgard-validation ./midgard-validation
COPY midgard-sdk ./midgard-sdk
COPY midgard-node ./midgard-node
RUN pnpm --filter midgard-node build
RUN pnpm --filter midgard-node deploy --prod /prod/midgard-node

FROM base AS final
WORKDIR /app
COPY --from=build /prod/midgard-node ./
COPY --from=native-mpf-owner /native /app/native
COPY --from=onchain /plutus.json /onchain/aiken/plutus.json
ENV NODE_ENV=production
CMD ["node", "./dist/index.js", "listen"]
