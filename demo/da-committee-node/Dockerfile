# syntax=docker/dockerfile:1.7
# Built from demo/. midgard-chain-sync is not in the image: services mount the host
# build (midgard-watcher native:build), the one pinned Go build of that binary.
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
RUN mkdir -p /var/lib/midgard-da && chown node:node /var/lib/midgard-da
USER node
ENV NODE_ENV=production
ENTRYPOINT ["node"]
CMD ["dist/index.js"]
