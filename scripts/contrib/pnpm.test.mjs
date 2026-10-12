import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { resolve } from "node:path";
import test from "node:test";

import { buildPackage } from "./build.mjs";

test("a build in a checkout without demo/node_modules names the missing install and worktree setup", async (t) => {
  const root = mkdtempSync(resolve(tmpdir(), "contrib-pnpm-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  mkdirSync(resolve(root, "demo/app"), { recursive: true });
  writeFileSync(
    resolve(root, "demo/app/package.json"),
    JSON.stringify({
      name: "app",
      scripts: { "build:contrib-raw": "tsup src/index.ts" },
      devDependencies: { tsup: "8.0.0" },
    }),
  );
  await assert.rejects(
    buildPackage(root, "app"),
    /demo\/node_modules is missing .*contrib\.mjs worktree setup/u,
  );
});
