# syntax=docker/dockerfile:1.7@sha256:a57df69d0ea827fb7266491f2813635de6f17269be881f696fbfdf2d83dda33e

ARG MIDGARD_NODE_BUILD_IMAGE=node:22.22.2
ARG MIDGARD_NODE_RUNTIME_IMAGE=node:22.22.2-bookworm-slim
ARG MIDGARD_GO_BUILD_IMAGE=golang:1.26.5-bookworm
ARG MIDGARD_RUST_BUILD_IMAGE=rust:1.91.0-bookworm
# Built from demo/. This isolated image owns its copied source and output tree.
# Use the same compiler recipes directly; shared-checkout leases and Git
# receipts belong to contributor builds, and the repository tooling is outside
# this demo context.

# The node transport sidecar the committee's L1 follower reads its local node
# through; the same recipe as CI and the node and watcher images.
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

FROM ${MIDGARD_NODE_BUILD_IMAGE} AS build
RUN corepack enable
WORKDIR /workspace/demo
COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY patches ./patches
COPY vendor ./vendor
# BEGIN GENERATED manifests
COPY da-committee-node/package.json ./da-committee-node/package.json
COPY l1-node-transport/package.json ./l1-node-transport/package.json
COPY lucid-midgard/package.json ./lucid-midgard/package.json
COPY midgard-core/package.json ./midgard-core/package.json
COPY midgard-fault-proofs/package.json ./midgard-fault-proofs/package.json
COPY midgard-l1-follower/package.json ./midgard-l1-follower/package.json
COPY midgard-node/package.json ./midgard-node/package.json
COPY midgard-node-tools/package.json ./midgard-node-tools/package.json
COPY midgard-sdk/package.json ./midgard-sdk/package.json
COPY midgard-test-support/package.json ./midgard-test-support/package.json
COPY midgard-validation/package.json ./midgard-validation/package.json
COPY midgard-watcher/package.json ./midgard-watcher/package.json
# END GENERATED manifests
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm install --frozen-lockfile --filter da-committee-node...
# BEGIN GENERATED sources
COPY scripts/package-tasks.mjs ./scripts/package-tasks.mjs
COPY l1-node-transport ./l1-node-transport
COPY midgard-core ./midgard-core
COPY midgard-sdk ./midgard-sdk
COPY midgard-l1-follower ./midgard-l1-follower
COPY midgard-validation ./midgard-validation
COPY da-committee-node ./da-committee-node
# END GENERATED sources
# BEGIN GENERATED builds
RUN pnpm --filter @al-ft/l1-node-transport run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-core run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-sdk run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-l1-follower run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-validation run build:contrib-raw
RUN pnpm --filter da-committee-node run build:contrib-raw
# END GENERATED builds
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm --filter da-committee-node deploy --prod /prod/da
# Explicit public target: generate its actual profile in the image build,
# authenticate the independent onchain input, and rebuild the production closure.
FROM build AS public-build
ARG MIDGARD_DEPLOYMENT_PROFILE=preprod-public
ARG MIDGARD_RELEASE_REVISION
COPY --from=release-source /config/deployments /workspace/config/deployments
COPY --from=release-source /demo/scripts /workspace/demo/scripts
COPY --from=release-source /onchain/aiken /workspace/onchain/aiken
COPY --from=release-source /scripts/lib/aiken.mjs /workspace/scripts/lib/aiken.mjs
COPY --from=release-source /scripts/release/check-image-inputs.mjs /workspace/scripts/release/check-image-inputs.mjs
COPY --from=onchain /plutus.json /workspace/onchain/aiken/plutus.json
COPY --from=onchain /plutus.json.deployment.json /workspace/onchain/aiken/plutus.json.deployment.json
RUN node scripts/deployment-profiles.mjs generate "$MIDGARD_DEPLOYMENT_PROFILE" \
  && node ../scripts/release/check-image-inputs.mjs "$MIDGARD_DEPLOYMENT_PROFILE" "$MIDGARD_RELEASE_REVISION" /workspace/public-build-inputs.json
# BEGIN GENERATED public-builds
RUN pnpm --filter @al-ft/l1-node-transport run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-core run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-sdk run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-l1-follower run build:contrib-raw
RUN pnpm --filter @al-ft/midgard-validation run build:contrib-raw
RUN pnpm --filter da-committee-node run build:contrib-raw
# END GENERATED public-builds
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm --filter da-committee-node deploy --prod /prod/public-da-committee-node

FROM ${MIDGARD_NODE_RUNTIME_IMAGE} AS public-final
ARG MIDGARD_DEPLOYMENT_PROFILE=preprod-public
ARG MIDGARD_RELEASE_REVISION
LABEL org.opencontainers.image.revision=$MIDGARD_RELEASE_REVISION
WORKDIR /app
RUN groupadd --gid 10001 midgard \
  && useradd --uid 10001 --gid 10001 --no-create-home midgard \
  && mkdir -p /app/db /app/release /var/lib/midgard-da \
  && chown 10001:10001 /app/db /var/lib/midgard-da
COPY --from=public-build /prod/public-da-committee-node ./
COPY --from=public-build /workspace/public-build-inputs.json /app/release/build-inputs.json
COPY --from=l1-node-transport /out/midgard-l1-node-transport /usr/local/bin/midgard-l1-node-transport
COPY --from=onchain /plutus.json /onchain/aiken/plutus.json
COPY --from=onchain /plutus.json.deployment.json /onchain/aiken/plutus.json.deployment.json
ENV NODE_ENV=production
USER 10001:10001
ENTRYPOINT ["node"]
CMD ["dist/index.js"]

FROM ${MIDGARD_NODE_RUNTIME_IMAGE}
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
