# syntax=docker/dockerfile:1.7

FROM node:22 AS base
RUN corepack enable

FROM base AS workspace
WORKDIR /workspace/demo
COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY midgard-sdk/package.json ./midgard-sdk/package.json
COPY midgard-node/package.json ./midgard-node/package.json
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm install --frozen-lockfile --filter midgard-node...

FROM workspace AS build
COPY midgard-sdk ./midgard-sdk
COPY midgard-node ./midgard-node
RUN pnpm --filter midgard-node build
RUN pnpm --filter midgard-node deploy --prod /prod/midgard-node

FROM base AS final
WORKDIR /app
COPY --from=build /prod/midgard-node ./
COPY --from=onchain /plutus.json /onchain/aiken/plutus.json
ENV NODE_ENV=production
CMD ["node", "./dist/index.js", "listen"]
