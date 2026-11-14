#!/usr/bin/env node
/**
 * Runs a watcher-journey TypeScript entry from source, resolved as the
 * drill's own Vitest configuration resolves it (`vitest.watcher-journeys.config.ts`):
 * workspace packages through the `midgard-source` condition, including the
 * watcher's test support the stack context reaches, and `.sql` files as
 * text. A tooling bundle cannot run these entries: they import watcher
 * modules that only exist as source. Its one argument is the entry's path,
 * relative to this package.
 *
 * Plain JavaScript, so it loads with nothing built and no TypeScript step.
 */
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  midgardSourceEnvironments,
  rawSqlLoaderPlugin,
} from "@al-ft/midgard-test-support/vitest";
import { createServer, createServerModuleRunner } from "vite";

const packageRoot = fileURLToPath(new URL("../../", import.meta.url));
const entry = process.argv[2];
if (entry === undefined)
  throw new Error("Usage: run-journey-source.mjs <entry.ts>");

const server = await createServer({
  configFile: false,
  root: packageRoot,
  logLevel: "error",
  appType: "custom",
  plugins: [rawSqlLoaderPlugin()],
  server: { middlewareMode: true, hmr: false, ws: false },
  environments: { ssr: midgardSourceEnvironments().ssr },
});
const runner = createServerModuleRunner(server.environments.ssr, {
  hmr: false,
});
await runner.import(resolve(packageRoot, entry));
