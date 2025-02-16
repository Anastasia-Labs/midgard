FROM node:22 AS base


FROM base AS sdk-deps

RUN corepack enable
RUN mkdir -p /midgard-sdk
WORKDIR /midgard-sdk
COPY --from=sdk package.json pnpm-lock.yaml ./
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm fetch --frozen-lockfile
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm install --frozen-lockfile --prod


FROM base AS sdk-build

RUN corepack enable
RUN mkdir -p /midgard-sdk
WORKDIR /midgard-sdk
COPY --from=sdk package.json pnpm-lock.yaml ./
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm fetch --frozen-lockfile
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm install --frozen-lockfile
COPY --from=sdk . .
RUN pnpm repack


FROM base AS deps

RUN corepack enable

WORKDIR /app
COPY package.json pnpm-lock.yaml ./
COPY --from=sdk-build /midgard-sdk /midgard-sdk
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm install --lockfile-only @al-ft/midgard-sdk
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm fetch --frozen-lockfile
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm install --frozen-lockfile --prod


FROM base AS build

RUN corepack enable
WORKDIR /app
COPY package.json pnpm-lock.yaml ./
COPY --from=sdk-build /midgard-sdk /midgard-sdk
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm install --lockfile-only @al-ft/midgard-sdk
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm fetch --frozen-lockfile
RUN --mount=type=cache,id=pnpm,target=/root/.local/share/pnpm/store pnpm install --frozen-lockfile
COPY . .
RUN pnpm build


FROM base

WORKDIR /app
  COPY --from=deps /app/node_modules /app/node_modules
  COPY --from=build /app/dist /app/dist
ENV NODE_ENV=production
CMD ["node", "./dist/index.js", "listen", "--provider", "Koios"]
