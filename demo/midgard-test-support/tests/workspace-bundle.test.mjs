import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  statSync,
  utimesSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";
import { fileURLToPath, pathToFileURL } from "node:url";

import { workspaceBundleProjects } from "../vitest.js";
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
  writeFileSync(join(pkg, "src", "index.ts"), 'export * from "./answer.js";\n');
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

/** A source transform, defined in a module of its own in `directory`, that
 * rewrites `answer.ts` to export `value`. */
const answerTransform = async (directory, value) => {
  const module = join(directory, "answer-transform.mjs");
  if (!existsSync(module))
    writeFileSync(
      module,
      [
        "export const answerTransform = (value) => ({",
        '  name: "answer-rewrite",',
        "  options: { value },",
        "  modules: [import.meta.url],",
        "  transform: (text, path) =>",
        '    path.endsWith("answer.ts")',
        "      ? text.replace(/= \\d+;/u, `= ${String(value)};`)",
        "      : undefined,",
        "});",
        "",
      ].join("\n"),
    );
  return (await import(pathToFileURL(module).href)).answerTransform(value);
};

test("a source transform is applied to the bundled source, and edited source still changes the key and the served code", async (t) => {
  const { pkg, name, request } = fixture(scratch(t));
  const sourceTransforms = [await answerTransform(join(pkg, ".."), 7)];

  const first = await buildWorkspaceBundle({ ...request, sourceTransforms });
  assert.equal(await importAnswer(first, name), 7, "the rewrite is served");

  // A module that now throws on load: the next build must serve it.
  writeFileSync(
    join(pkg, "src", "answer.ts"),
    'throw new Error("edited source probe");\nexport const answer: number = 1;\n',
  );
  const edited = await buildWorkspaceBundle({ ...request, sourceTransforms });
  assert.notEqual(edited.key, first.key);
  await assert.rejects(importAnswer(edited, name), /edited source probe/u);

  // A rewritten module that reads import.meta keeps both rewrites.
  writeFileSync(
    join(pkg, "src", "answer.ts"),
    "export const where: string = import.meta.url;\nexport const answer: number = 1;\n",
  );
  const meta = await buildWorkspaceBundle({ ...request, sourceTransforms });
  const served = await import(pathToFileURL(meta.files.get(name)).href);
  assert.equal(served.answer, 7);
  assert.equal(served.where, pathToFileURL(join(pkg, "src", "answer.ts")).href);
  assert.notEqual(meta.key, edited.key);
});

test("editing a source transform's code or options changes the key", async (t) => {
  const directory = scratch(t);
  const { name, request } = fixture(directory);
  const seven = await answerTransform(directory, 7);

  const first = await buildWorkspaceBundle({
    ...request,
    sourceTransforms: [seven],
  });
  assert.equal(await importAnswer(first, name), 7);

  // Its options.
  const eight = await buildWorkspaceBundle({
    ...request,
    sourceTransforms: [await answerTransform(directory, 8)],
  });
  assert.notEqual(eight.key, first.key);
  assert.equal(await importAnswer(eight, name), 8);

  // Its module's text, even where the transform function's own text is
  // unchanged (a helper it calls could have changed).
  writeFileSync(
    join(directory, "answer-transform.mjs"),
    `${readFileSync(join(directory, "answer-transform.mjs"), "utf8")}// edited\n`,
  );
  const recoded = await buildWorkspaceBundle({
    ...request,
    sourceTransforms: [seven],
  });
  assert.notEqual(recoded.key, first.key);
  assert.notEqual(recoded.key, eight.key);

  // Its function's text.
  const rewritten = await buildWorkspaceBundle({
    ...request,
    sourceTransforms: [{ ...seven, transform: (text) => text }],
  });
  assert.notEqual(rewritten.key, recoded.key);
  assert.equal(await importAnswer(rewritten, name), 1);
});

test("a bundle built with a source transform is never served to a run without it, nor the reverse", async (t) => {
  const directory = scratch(t);
  const { name, request } = fixture(directory);
  const sourceTransforms = [await answerTransform(directory, 7)];

  const plain = await buildWorkspaceBundle(request);
  const rewritten = await buildWorkspaceBundle({
    ...request,
    sourceTransforms,
  });
  assert.notEqual(rewritten.key, plain.key);
  assert.notEqual(rewritten.directory, plain.directory);

  // Each kind reuses only its own bundle, whichever was built last.
  const plainAgain = await buildWorkspaceBundle(request);
  const rewrittenAgain = await buildWorkspaceBundle({
    ...request,
    sourceTransforms,
  });
  assert.equal(plainAgain.key, plain.key);
  assert.equal(rewrittenAgain.key, rewritten.key);
  assert.equal(await importAnswer(plainAgain, name), 1);
  assert.equal(await importAnswer(rewrittenAgain, name), 7);
  assert.deepEqual(
    JSON.parse(readFileSync(join(rewritten.directory, "manifest.json"), "utf8"))
      .options.sourceTransforms,
    [{ name: "answer-rewrite", options: { value: 7 } }],
  );
});

test("refuses a source transform that does not name the modules defining it", async (t) => {
  const { request } = fixture(scratch(t));
  await assert.rejects(
    buildWorkspaceBundle({
      ...request,
      sourceTransforms: [
        { name: "unkeyed", options: {}, modules: [], transform: () => "" },
      ],
    }),
    /source transform unkeyed/u,
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

test("a project's transforming plugins and its source transforms must pair up by name", (t) => {
  const previous = process.env.MIDGARD_TEST_WORKSPACE_BUNDLE;
  process.env.MIDGARD_TEST_WORKSPACE_BUNDLE = "1";
  t.after(() => {
    if (previous === undefined)
      delete process.env.MIDGARD_TEST_WORKSPACE_BUNDLE;
    else process.env.MIDGARD_TEST_WORKSPACE_BUNDLE = previous;
  });
  const packageDirectory = fileURLToPath(new URL("..", import.meta.url));
  const plugin = { name: "rewrite", transform: () => null };
  const sourceTransform = {
    name: "rewrite",
    options: {},
    modules: [import.meta.url],
    transform: () => undefined,
  };
  // No test file matches, so a project that passes the check is returned
  // as it is.
  const test = { name: "paired", include: ["./no-such-directory/**"] };

  assert.throws(
    () =>
      workspaceBundleProjects(
        { plugins: [plugin], test },
        { packageDirectory },
      ),
    /plugin rewrite transforms source/u,
  );
  assert.throws(
    () =>
      workspaceBundleProjects(
        { test },
        { packageDirectory, sourceTransforms: [sourceTransform] },
      ),
    /source transform rewrite has no plugin/u,
  );
  assert.deepEqual(
    workspaceBundleProjects(
      { plugins: [plugin], test },
      { packageDirectory, sourceTransforms: [sourceTransform] },
    ),
    [{ plugins: [plugin], test }],
  );
});
