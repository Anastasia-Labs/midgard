import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  rmSync,
  statSync,
  utimesSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";
import { pathToFileURL } from "node:url";

import {
  buildWorkspaceBundle,
  MAX_BUNDLES,
  pruneWorkspaceBundles,
} from "../workspace-bundle.js";

const HOUR_MS = 60 * 60 * 1000;

const scratch = (t) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-bundle-test-"));
  t.after(() => rmSync(directory, { recursive: true, force: true }));
  return directory;
};

/** A one-package workspace in `directory` and the request that bundles it
 * into a cache of its own. */
const fixture = (directory) => {
  const pkg = join(directory, "pkg");
  const root = join(directory, "bundles");
  mkdirSync(join(pkg, "src"), { recursive: true });
  mkdirSync(root);
  writeFileSync(
    join(pkg, "src", "index.ts"),
    'export { answer } from "./answer.js";\n',
  );
  writeFileSync(
    join(pkg, "src", "answer.ts"),
    "export const answer: number = 1;\n",
  );
  const name = "@midgard-bundle-test/pkg";
  return {
    pkg,
    root,
    name,
    request: {
      analysis: {
        entries: new Map([[name, join(pkg, "src", "index.ts")]]),
        bundleable: [name],
        sourceRoot: join(directory, "tests"),
      },
      target: "es2022",
      conditions: ["node"],
      resolveExternal: async () => undefined,
      packages: new Map([
        [name, { name, directory: pkg, exports: {}, dependencies: [] }],
      ]),
      root,
    },
  };
};

const importAnswer = async (bundle, name) =>
  (await import(pathToFileURL(bundle.files.get(name)).href)).answer;

/** A pid no process holds: a child that has already exited. */
const endedPid = () => spawnSync(process.execPath, ["-e", ""]).pid;

/** A bundle directory last used `ageMs` ago, naming `users` as its users. */
const plantBundle = (root, name, ageMs, users = []) => {
  const directory = join(root, name);
  mkdirSync(join(directory, "users"), { recursive: true });
  writeFileSync(join(directory, "manifest.json"), "{}");
  for (const pid of users)
    writeFileSync(join(directory, "users", `${pid}`), "");
  const used = new Date(Date.now() - ageMs);
  utimesSync(directory, used, used);
  return directory;
};

test("reuses a bundle while its inputs are unchanged and rebuilds when any input changes", async (t) => {
  const { pkg, root, name, request } = fixture(scratch(t));

  const first = await buildWorkspaceBundle(request);
  assert.equal(await importAnswer(first, name), 1);
  const manifest = statSync(join(first.directory, "manifest.json"));

  const again = await buildWorkspaceBundle(request);
  assert.equal(again.key, first.key);
  assert.equal(
    statSync(join(again.directory, "manifest.json")).ino,
    manifest.ino,
    "an unchanged input set reuses the published bundle",
  );

  writeFileSync(
    join(pkg, "src", "answer.ts"),
    "export const answer: number = 2;\n",
  );
  const edited = await buildWorkspaceBundle(request);
  assert.notEqual(edited.key, first.key);
  assert.equal(await importAnswer(edited, name), 2);

  // A file no build reads still changes the key: it can change resolution.
  writeFileSync(join(pkg, "src", "unused.ts"), "export {};\n");
  const added = await buildWorkspaceBundle(request);
  assert.notEqual(added.key, edited.key);

  // Installed packages, build output and dotfiles are never bundled source.
  for (const ignored of ["node_modules", "dist", ".cache"]) {
    mkdirSync(join(pkg, ignored));
    writeFileSync(join(pkg, ignored, "noise.js"), "export {};\n");
  }
  assert.equal((await buildWorkspaceBundle(request)).key, added.key);

  // So is every option the build takes.
  const retargeted = await buildWorkspaceBundle({
    ...request,
    target: "es2020",
  });
  assert.notEqual(retargeted.key, added.key);

  assert.deepEqual(
    readdirSync(root).sort(),
    [first.key, edited.key, added.key, retargeted.key].sort(),
  );
  for (const bundle of [first, edited, added, retargeted])
    assert.ok(
      existsSync(join(bundle.directory, "users", `${process.pid}`)),
      "each run names itself a user of the bundle it loads",
    );
});

test("publishing a bundle prunes bundles unused for a day", async (t) => {
  const { root, request } = fixture(scratch(t));
  const stale = plantBundle(root, "stale", 25 * HOUR_MS);
  const recent = plantBundle(root, "recent", HOUR_MS);

  await buildWorkspaceBundle(request);

  assert.equal(existsSync(stale), false);
  assert.equal(existsSync(recent), true);
});

test("keeps only the most recently used bundles, never one a running process uses", (t) => {
  const root = scratch(t);
  const keep = plantBundle(root, "keep", 0);
  const recent = Array.from({ length: MAX_BUNDLES + 4 }, (_, index) =>
    plantBundle(
      root,
      `recent-${String(index).padStart(2, "0")}`,
      (index + 1) * 60_000,
    ),
  );
  const beyondCap = recent.slice(MAX_BUNDLES - 1);
  // Beyond the cap, but loaded by this live process.
  const inUse = plantBundle(root, "in-use", HOUR_MS, [process.pid]);
  // Named only by a process that has ended.
  const abandoned = plantBundle(root, "abandoned", HOUR_MS, [endedPid()]);
  // Past the age limit, but still loaded by this live process.
  const oldInUse = plantBundle(root, "old-in-use", 25 * HOUR_MS, [process.pid]);
  const staleStaging = plantBundle(root, ".staging-stale", 25 * HOUR_MS);
  const buildingStaging = plantBundle(root, ".staging-building", 0);

  pruneWorkspaceBundles(root, keep);

  for (const kept of [
    keep,
    ...recent.slice(0, MAX_BUNDLES - 1),
    inUse,
    oldInUse,
    buildingStaging,
  ])
    assert.equal(existsSync(kept), true, `${kept} is kept`);
  for (const removed of [...beyondCap, abandoned, staleStaging])
    assert.equal(existsSync(removed), false, `${removed} is removed`);
});
