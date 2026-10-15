# syntax=docker/dockerfile:1.7@sha256:a57df69d0ea827fb7266491f2813635de6f17269be881f696fbfdf2d83dda33e

ARG MIDGARD_NODE_BUILD_IMAGE=node:22.22.2
ARG MIDGARD_NODE_RUNTIME_IMAGE=node:22.22.2-bookworm-slim
ARG MIDGARD_GO_BUILD_IMAGE=golang:1.26.5-bookworm
ARG MIDGARD_RUST_BUILD_IMAGE=rust:1.91.0-bookworm

# The node transport sidecar the watcher reads its node through.
FROM ${MIDGARD_GO_BUILD_IMAGE} AS l1-node-transport
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

FROM ${MIDGARD_NODE_BUILD_IMAGE} AS workspace
RUN corepack enable
WORKDIR /workspace/demo
COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY patches ./patches
# The vendored MPF package that a package.json override resolves to.
COPY vendor ./vendor
COPY lucid-midgard/package.json ./lucid-midgard/package.json
COPY l1-node-transport/package.json ./l1-node-transport/package.json
COPY midgard-core/package.json ./midgard-core/package.json
COPY midgard-validation/package.json ./midgard-validation/package.json
COPY midgard-sdk/package.json ./midgard-sdk/package.json
COPY midgard-fault-proofs/package.json ./midgard-fault-proofs/package.json
COPY midgard-l1-follower/package.json ./midgard-l1-follower/package.json
COPY midgard-watcher/package.json ./midgard-watcher/package.json
COPY midgard-test-support/package.json ./midgard-test-support/package.json
# Every other workspace manifest too: `pnpm deploy` resolves each
# `workspace:*` edge the watcher declares (midgard-node among them), even
# though only the production chain's sources are copied and built below.
COPY midgard-node/package.json ./midgard-node/package.json
COPY midgard-node-tools/package.json ./midgard-node-tools/package.json
COPY da-committee-node/package.json ./da-committee-node/package.json
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm install --frozen-lockfile --filter midgard-watcher...

# This isolated image owns its copied source and output tree. Use the same
# compiler recipes directly; shared-checkout leases and Git receipts belong to
# contributor builds, and the repository tooling is outside this demo context.
FROM workspace AS build
COPY lucid-midgard ./lucid-midgard
COPY l1-node-transport ./l1-node-transport
COPY midgard-core ./midgard-core
COPY midgard-validation ./midgard-validation
COPY midgard-sdk ./midgard-sdk
COPY midgard-fault-proofs ./midgard-fault-proofs
COPY midgard-l1-follower ./midgard-l1-follower
COPY midgard-watcher ./midgard-watcher
# The production chain only, dependencies first. `midgard-watcher...` would
# also select its dev-only workspace edges (midgard-node and the packages
# behind it), whose sources this image does not copy.
RUN pnpm --filter @al-ft/lucid-midgard run build:contrib-raw
RUN pnpm --filter @al-ft/l1-node-transport run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-core run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-sdk run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-validation run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-fault-proofs run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-l1-follower run build:contrib-raw
RUN pnpm --filter midgard-watcher run build:contrib-raw
# Same store as the install step: `deploy --prod` re-resolves the git-hosted
# merkle-patricia-forestry dependency, and only a store that already holds the
# prepared package avoids re-running its `prepare` build (which needs dev
# tooling a production install never fetches).
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm --filter midgard-watcher deploy --prod /prod/midgard-watcher

# Explicit public target: generate its actual profile in the image build,
# authenticate the independent onchain input, and rebuild the production closure.
FROM build AS public-build
ARG MIDGARD_DEPLOYMENT_PROFILE=preprod-public
ARG MIDGARD_RELEASE_REVISION
COPY --from=release-source /config/deployments /workspace/config/deployments
COPY --from=release-source /demo/scripts /workspace/demo/scripts
COPY --from=release-source /onchain/aiken /workspace/onchain/aiken
COPY --from=release-source /.github/workflows /workspace/.github/workflows
COPY --from=release-source /scripts/release/check-image-inputs.mjs /workspace/scripts/release/check-image-inputs.mjs
COPY --from=onchain /plutus.json /workspace/onchain/aiken/plutus.json
COPY --from=onchain /plutus.json.deployment.json /workspace/onchain/aiken/plutus.json.deployment.json
RUN node scripts/deployment-profiles.mjs generate "$MIDGARD_DEPLOYMENT_PROFILE" \
  && node ../scripts/release/check-image-inputs.mjs "$MIDGARD_DEPLOYMENT_PROFILE" "$MIDGARD_RELEASE_REVISION" /workspace/public-build-inputs.json
RUN pnpm --filter @al-ft/lucid-midgard run build:contrib-raw
RUN pnpm --filter @al-ft/l1-node-transport run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-core run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-sdk run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-validation run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-fault-proofs run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-l1-follower run build:contrib-raw
RUN pnpm --filter midgard-watcher run build:contrib-raw
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm --filter midgard-watcher deploy --prod /prod/public-midgard-watcher

FROM ${MIDGARD_NODE_RUNTIME_IMAGE} AS public-final
ARG MIDGARD_DEPLOYMENT_PROFILE=preprod-public
ARG MIDGARD_RELEASE_REVISION
LABEL org.opencontainers.image.revision=$MIDGARD_RELEASE_REVISION
WORKDIR /app
RUN groupadd --gid 10001 midgard \
  && useradd --uid 10001 --gid 10001 --no-create-home midgard \
  && mkdir -p /app/db /app/release /var/lib/midgard-da \
  && chown 10001:10001 /app/db /var/lib/midgard-da
COPY --from=public-build /prod/public-midgard-watcher ./
COPY --from=public-build /workspace/public-build-inputs.json /app/release/build-inputs.json
COPY --from=l1-node-transport /out/midgard-l1-node-transport /usr/local/bin/midgard-l1-node-transport
COPY --from=onchain /plutus.json /onchain/aiken/plutus.json
COPY --from=onchain /plutus.json.deployment.json /onchain/aiken/plutus.json.deployment.json
ENV NODE_ENV=production
USER 10001:10001
ENV MALLOC_MMAP_THRESHOLD_=131072
ENTRYPOINT ["node", "./dist/cli.js"]
CMD ["start", "--config", "/etc/midgard/watcher-process.json"]

FROM ${MIDGARD_NODE_RUNTIME_IMAGE} AS final
WORKDIR /app
COPY --from=build /prod/midgard-watcher ./
COPY --from=l1-node-transport /out/midgard-l1-node-transport /usr/local/bin/midgard-l1-node-transport
ENV NODE_ENV=production
# Must be set before Node starts; see README.md.
ENV MALLOC_MMAP_THRESHOLD_=131072
ENTRYPOINT ["node", "./dist/cli.js"]
CMD ["start", "--config", "/etc/midgard/watcher-process.json"]
