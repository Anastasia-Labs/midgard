# Base image
FROM node:22 AS base
RUN corepack enable
FROM base AS sdk-build
WORKDIR /midgard-sdk
COPY --from=sdk . .
ENV CI=true
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm fetch --no-frozen-lockfile
ENV CI=true
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store \
  pnpm install --no-frozen-lockfile
RUN pnpm repack

FROM base AS build
WORKDIR /app
COPY --from=sdk-build /midgard-sdk/*.tgz /midgard-sdk/
COPY . .
ENV CI=true
RUN pnpm install --no-frozen-lockfile
RUN pnpm build

# Final Stage: Run Application
FROM base AS final
WORKDIR /app
COPY --from=build /app/node_modules /app/node_modules
COPY --from=build /app/dist /app/dist
ENV NODE_ENV=production
CMD ["node", "./dist/index.js", "listen"]
