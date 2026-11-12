import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  realpathSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, resolve } from "node:path";
import test from "node:test";

import { packageByName } from "./files.mjs";
import {
  candidatePackages,
  probeGraph,
  reachablePackages,
  reachedTests,
  reachIn,
  wholePackageReasons,
} from "./reached.mjs";

// A hook running these tests exports GIT_DIR; the `git init` below would
// re-initialise the outer repository under it.
for (const key of Object.keys(process.env))
  if (key.startsWith("GIT_")) delete process.env[key];

const repository = resolve(import.meta.dirname, "../..");

// A two-package workspace: `app` depends on `core`; `other` stands apart.
// Files are written for the text pre-filter; the import graph is given, as
// reached-probe.mjs would write it, so each rule is tested on its own.
const FILES = {
  "demo/core/package.json": JSON.stringify({ name: "core" }),
  "demo/core/src/x.ts": "export const x = 1;\n",
  "demo/core/src/index.ts": "export {};\n",
  "demo/app/package.json": JSON.stringify({
    name: "app",
    dependencies: { core: "workspace:*" },
  }),
  "demo/app/src/index.ts": 'export { x } from "../../core/src/x.js";\n',
  "demo/app/src/pool.ts":
    'new Worker(new URL("./worker.js", import.meta.url));\n',
  "demo/app/src/worker.ts": "export {};\n",
  "demo/app/src/contracts.ts": 'read("onchain/aiken/plutus.json");\n',
  "demo/app/src/unrelated.ts": "export {};\n",
  "demo/app/tests/imports.test.ts": 'import "../src/index.js";\n',
  "demo/app/tests/pool.test.ts": 'import "../src/pool.js";\n',
  "demo/app/tests/fixture.test.ts": 'read("fixtures/data.json");\n',
  "demo/app/tests/contracts.test.ts": 'import "../src/contracts.js";\n',
  "demo/app/tests/helper.test.ts": 'import "./helpers/child.js";\n',
  "demo/app/tests/helpers/child.ts": "await start();\n",
  "demo/app/tests/built.test.ts": 'spawn("dist/bundle.js");\n',
  "demo/app/tests/shared-name.test.ts": 'read("src/index.ts");\n',
  "demo/app/tests/other-cli.test.ts": 'spawn("../other/src/index.ts");\n',
  "demo/app/tests/walker.test.ts": 'readdirSync("fixtures");\n',
  "demo/app/tests/fixtures/data.json": "{}\n",
  "demo/app/tests/fixtures/orphan.json": "{}\n",
  "demo/app/tests/fixtures/listed.txt": "\n",
  "demo/app/assets/orphan.json": "{}\n",
  "demo/other/package.json": JSON.stringify({ name: "other" }),
  "demo/other/src/index.ts": "export {};\n",
  "demo/other/src/cli.ts": 'import "./index.js";\n',
  "demo/app/tests/other-tool.test.ts": 'spawn("../other/src/cli.ts");\n',
  "demo/app/tests/other-mention.test.ts": 'read("other", "src/index.ts");\n',
  "demo/app/tests/core-mention.test.ts": 'read("core", "src/index.ts");\n',
  "onchain/aiken/validators/v.ak": "\n",
};

const workspace = (t, files = FILES) => {
  const root = realpathSync(
    mkdtempSync(resolve(tmpdir(), "midgard-reached-test-")),
  );
  t.after(() => rmSync(root, { recursive: true, force: true }));
  for (const [path, text] of Object.entries(files)) {
    mkdirSync(dirname(resolve(root, path)), { recursive: true });
    writeFileSync(resolve(root, path), text);
  }
  execFileSync("git", ["init", "-q"], { cwd: root });
  return root;
};

const at = (root) => (path) => resolve(root, path);

// The graph of `app`, keyed by absolute path, as the probe writes it.
const appGraph = (root, { failed = {} } = {}) => {
  const a = at(root);
  const tests = Object.keys(FILES)
    .filter((path) => /^demo\/app\/tests\/[^/]+\.test\.ts$/u.test(path))
    .map(a);
  const edges = {
    [a("demo/app/tests/imports.test.ts")]: [a("demo/app/src/index.ts")],
    [a("demo/app/src/index.ts")]: [a("demo/core/src/x.ts")],
    [a("demo/core/src/x.ts")]: [],
    [a("demo/core/src/index.ts")]: [],
    [a("demo/app/tests/pool.test.ts")]: [a("demo/app/src/pool.ts")],
    [a("demo/app/src/pool.ts")]: [],
    [a("demo/app/src/worker.ts")]: [],
    [a("demo/app/tests/contracts.test.ts")]: [a("demo/app/src/contracts.ts")],
    [a("demo/app/src/contracts.ts")]: [],
    [a("demo/app/src/unrelated.ts")]: [],
    [a("demo/app/tests/helper.test.ts")]: [
      a("demo/app/tests/helpers/child.ts"),
    ],
    [a("demo/app/tests/helpers/child.ts")]: [],
  };
  for (const test of tests) edges[test] ??= [];
  const spelled = {
    [a("demo/app/src/pool.ts")]: { paths: ["./worker.js"], walks: false },
    [a("demo/app/src/contracts.ts")]: {
      paths: ["onchain/aiken/plutus.json"],
      walks: false,
    },
    [a("demo/app/tests/fixture.test.ts")]: {
      paths: ["fixtures/data.json"],
      walks: false,
    },
    [a("demo/app/tests/built.test.ts")]: {
      paths: ["dist/bundle.js"],
      walks: false,
    },
    [a("demo/app/tests/shared-name.test.ts")]: {
      paths: ["src/index.ts"],
      walks: false,
    },
    [a("demo/app/tests/other-cli.test.ts")]: {
      paths: ["../other/src/index.ts"],
      walks: false,
    },
    [a("demo/app/tests/other-tool.test.ts")]: {
      paths: ["../other/src/cli.ts"],
      walks: false,
    },
    [a("demo/app/tests/other-mention.test.ts")]: {
      paths: ["other", "src/index.ts"],
      walks: false,
    },
    [a("demo/app/tests/core-mention.test.ts")]: {
      paths: ["core", "src/index.ts"],
      walks: false,
    },
    [a("demo/app/tests/walker.test.ts")]: {
      paths: ["fixtures"],
      walks: true,
    },
  };
  for (const file of Object.keys(edges))
    if (!(file in failed)) spelled[file] ??= { paths: [], walks: false };
  return { tests, edges, failed, spelled };
};

const reach = (root, changed, graph = appGraph(root)) =>
  reachIn({ root, pkg: packageByName(root, "app"), graph, changed });

test("a change in another package reaches the tests importing it, through the package boundary", (t) => {
  // Vitest's own bundles hide this edge; the probe resolves from source.
  const root = workspace(t);
  const result = reach(root, ["demo/core/src/x.ts"]);
  assert.equal(result.whole, false);
  assert.ok(result.files.includes("tests/imports.test.ts"), result.files);
  assert.ok(!result.files.includes("tests/pool.test.ts"), result.files);
});

test("a worker entry named by a string reaches what imports the module spawning it", (t) => {
  // `vitest related` misses this: nothing imports the worker entry.
  const root = workspace(t);
  const result = reach(root, ["demo/app/src/worker.ts"]);
  assert.ok(result.files.includes("tests/pool.test.ts"), result.files);
  assert.ok(
    result.why.includes("demo/app/src/pool.ts names demo/app/src/worker.ts"),
    result.why,
  );
});

test("a name edge says why only when its namer leads to a reached test", (t) => {
  const root = workspace(t);
  const a = at(root);
  const graph = appGraph(root);
  // Nothing imports unrelated.ts: its name of the worker reaches no test.
  graph.spelled[a("demo/app/src/unrelated.ts")].paths = ["./worker.js"];
  const quiet = reach(root, ["demo/app/src/worker.ts"], graph);
  assert.ok(
    !quiet.why.includes(
      "demo/app/src/unrelated.ts names demo/app/src/worker.ts",
    ),
    quiet.why,
  );
  // A test naming unrelated.ts leads to it: both edges say why.
  graph.spelled[a("demo/app/tests/contracts.test.ts")].paths = [
    "src/unrelated.ts",
  ];
  const chained = reach(root, ["demo/app/src/worker.ts"], graph);
  assert.ok(chained.files.includes("tests/contracts.test.ts"), chained.files);
  for (const line of [
    "demo/app/tests/contracts.test.ts names demo/app/src/unrelated.ts",
    "demo/app/src/unrelated.ts names demo/app/src/worker.ts",
    "demo/app/src/pool.ts names demo/app/src/worker.ts",
  ])
    assert.ok(chained.why.includes(line), chained.why);
});

test("a JS spelling not on disk is imported as its TS sources and said to be missing; one on disk is itself", (t) => {
  const root = workspace(t, {
    ...FILES,
    "demo/app/src/legacy.js": "export {};\n",
    "demo/app/src/legacy.ts": "export {};\n",
    "demo/app/src/gone.ts": "export {};\n",
  });
  const a = at(root);
  const graph = appGraph(root);
  graph.edges[a("demo/app/tests/pool.test.ts")].push(
    a("demo/app/src/legacy.js"),
  );
  graph.edges[a("demo/app/tests/helper.test.ts")].push(
    a("demo/app/src/gone.mjs"),
  );
  // Each source change also reaches built.test.ts, which runs the dist.
  const files = (path) => reach(root, [path], graph).files;
  assert.deepEqual(files("demo/app/src/legacy.ts"), ["tests/built.test.ts"]);
  assert.deepEqual(files("demo/app/src/gone.ts"), ["tests/built.test.ts"]);
  const result = reach(root, ["demo/app/src/gone.mts"], graph);
  assert.deepEqual(result.files, [
    "tests/built.test.ts",
    "tests/helper.test.ts",
  ]);
  assert.ok(
    result.why.includes(
      "missing on disk: demo/app/src/gone.mjs (imported by demo/app/tests/helper.test.ts)",
    ),
    result.why,
  );
});

test("a fixture reaches the tests naming it; a data file nothing names runs the whole package", (t) => {
  const root = workspace(t);
  const named = reach(root, ["demo/app/tests/fixtures/data.json"]);
  assert.equal(named.whole, false);
  assert.ok(named.files.includes("tests/fixture.test.ts"), named.files);
  assert.ok(!named.files.includes("tests/imports.test.ts"), named.files);
  // Nothing spells assets/orphan.json, so it may be read through a computed
  // path: unsure, the reach widens rather than narrows.
  const orphan = reach(root, ["demo/app/assets/orphan.json"]);
  assert.equal(orphan.whole, true);
  assert.match(orphan.why[0], /nothing imports or names/u);
});

test("a module listing a directory it spells reaches the files under it", (t) => {
  const root = workspace(t);
  const result = reach(root, ["demo/app/tests/fixtures/listed.txt"]);
  assert.equal(result.whole, false);
  assert.ok(result.files.includes("tests/walker.test.ts"), result.files);
});

test("a blueprint input reaches the suites that read the compiled blueprint", (t) => {
  const root = workspace(t);
  const result = reach(root, ["onchain/aiken/validators/v.ak"]);
  assert.ok(result.files.includes("tests/contracts.test.ts"), result.files);
  assert.ok(
    candidatePackages(root, "onchain/aiken/validators/v.ak").has("app"),
  );
});

test("a module the probe cannot analyse always runs what imports it", (t) => {
  const root = workspace(t);
  const failed = {
    [resolve(root, "demo/app/tests/helpers/child.ts")]: "Transform failed",
  };
  const result = reach(
    root,
    ["demo/app/src/unrelated.ts"],
    appGraph(root, { failed }),
  );
  assert.ok(result.files.includes("tests/helper.test.ts"), result.files);
  assert.ok(result.why.some((line) => /could not be analysed/u.test(line)));
});

test("test code that spells dist is reached by a change to a built input of its closure", (t) => {
  const root = workspace(t);
  assert.ok(
    reach(root, ["demo/core/src/x.ts"]).files.includes("tests/built.test.ts"),
  );
  // A test fixture is not a build input.
  assert.ok(
    !reach(root, ["demo/app/tests/fixtures/data.json"]).files.includes(
      "tests/built.test.ts",
    ),
  );
});

test("a name several files share names another package's file only by a path that picks it", (t) => {
  const root = workspace(t);
  // `src/index.ts` is app's own and other's: spelled bare, it means app's.
  const own = reach(root, ["demo/app/src/index.ts"]);
  assert.ok(own.files.includes("tests/shared-name.test.ts"), own.files);
  const other = reach(root, ["demo/other/src/index.ts"]);
  assert.ok(!other.files.includes("tests/shared-name.test.ts"), other.files);
  // A path that picks other's file names it, though app does not depend on
  // other and the graph does not hold it; a bare shared name beside a
  // mention of other does not.
  assert.ok(other.files.includes("tests/other-cli.test.ts"), other.files);
  assert.ok(!other.files.includes("tests/other-mention.test.ts"), other.files);
  // A script of other the graph does not hold may import the changed file.
  assert.ok(other.files.includes("tests/other-tool.test.ts"), other.files);
  // Within the closure, a shared name beside the owner's directory names it.
  const closure = reach(root, ["demo/core/src/index.ts"]);
  assert.ok(
    closure.files.includes("tests/core-mention.test.ts"),
    closure.files,
  );
  assert.ok(
    !closure.files.includes("tests/shared-name.test.ts"),
    closure.files,
  );
});

test("a manifest, workspace configuration or the shared harness selects whole packages", (t) => {
  const root = workspace(t);
  const app = packageByName(root, "app");
  for (const path of [
    "demo/pnpm-lock.yaml",
    "demo/tsconfig.base.json",
    "demo/midgard-test-support/vitest.js",
    "demo/core/package.json",
    "demo/app/vitest.config.ts",
  ])
    assert.equal(wholePackageReasons(root, app, [path]).length, 1, path);
  for (const path of ["demo/other/package.json", "demo/app/README.md"])
    assert.deepEqual(wholePackageReasons(root, app, [path]), [], path);
});

test("the packages a change can reach: a test fixture its owner, a manifest every package, a note none", (t) => {
  const root = workspace(t);
  const reachable = (path) => [...reachablePackages(root, [path])].sort();
  assert.deepEqual(reachable("demo/app/tests/fixtures/data.json"), ["app"]);
  // core's users; `other` too, as its text contains `x`: a cheap text
  // search errs toward more.
  for (const name of ["app", "core"])
    assert.ok(reachable("demo/core/src/x.ts").includes(name), name);
  assert.deepEqual(reachable("demo/pnpm-lock.yaml"), ["app", "core", "other"]);
  assert.deepEqual(reachable("docs/notes.md"), []);
});

test("the pre-filter skips the probe when no text can reach a package, and a failed probe widens", async (t) => {
  const root = workspace(t);
  const app = packageByName(root, "app");
  const command = { exclude: [], env: {} };
  const unreachable = await reachedTests(
    root,
    app,
    command,
    ["docs/notes.md"],
    {
      probe: () => {
        throw new Error("the probe must not run");
      },
    },
  );
  assert.deepEqual(unreachable.files, []);
  assert.equal(unreachable.whole, false);
  const configured = await reachedTests(
    root,
    app,
    command,
    ["demo/pnpm-lock.yaml"],
    {
      probe: () => {
        throw new Error("the probe must not run");
      },
    },
  );
  assert.equal(configured.whole, true);
  const failed = await reachedTests(
    root,
    app,
    command,
    ["demo/core/src/x.ts"],
    {
      probe: async () => {
        throw new Error("the import-graph probe failed; log: /run/probe.log");
      },
    },
  );
  assert.equal(failed.whole, true);
  assert.match(failed.why[0], /probe failed/u);
});

test("the probe graphs a real package with Vitest's resolver: imports across packages, spelled names, untransformable entries", async (t) => {
  const vitest = resolve(repository, "demo/node_modules/vitest");
  if (!existsSync(vitest)) {
    t.skip("demo/node_modules/vitest is not installed");
    return;
  }
  const root = workspace(t, {
    "demo/core/package.json": JSON.stringify({ name: "core", type: "module" }),
    "demo/core/src/x.ts": "export const x: number = 1;\n",
    "demo/app/package.json": JSON.stringify({ name: "app", type: "module" }),
    "demo/app/src/pool.ts":
      'export const spawn = () => new URL("./worker.js", import.meta.url);\n',
    "demo/app/src/worker.ts":
      'import { x } from "../../core/src/x.js";\nexport const y = x;\n',
    "demo/app/tests/pool.test.ts":
      'import { test } from "vitest";\nimport { spawn } from "../src/pool.js";\ntest("spawns", () => { spawn(); });\n',
    // A module Vitest cannot transform, importing a file that is not there.
    "demo/app/vitest.config.mjs":
      'export default { plugins: [{ name: "refuse", transform(code, id) { if (id.endsWith("child.ts")) throw new Error("refused"); } }] };\n',
    "demo/app/tests/child.ts":
      'import { y } from "../src/worker.js";\nimport "./gone.js";\nawait Promise.resolve(y);\n',
  });
  mkdirSync(resolve(root, "demo/node_modules"));
  symlinkSync(realpathSync(vitest), resolve(root, "demo/node_modules/vitest"));
  const directory = resolve(root, "run");
  mkdirSync(directory);
  const app = packageByName(root, "app");
  const graph = await probeGraph(
    root,
    app,
    { exclude: [], env: {} },
    { env: process.env, directory },
  );
  const a = at(root);
  assert.deepEqual(graph.tests, [a("demo/app/tests/pool.test.ts")]);
  assert.deepEqual(graph.failed, {});
  assert.deepEqual(graph.edges[a("demo/app/src/worker.ts")], [
    a("demo/core/src/x.ts"),
  ]);
  assert.deepEqual(graph.edges[a("demo/app/tests/child.ts")], [
    a("demo/app/src/worker.ts"),
    a("demo/app/tests/gone.js"),
  ]);
  assert.ok(
    graph.spelled[a("demo/app/src/pool.ts")].paths.includes("./worker.js"),
  );
  assert.ok(
    !graph.spelled[a("demo/app/tests/pool.test.ts")].paths.includes(
      "../src/pool.js",
    ),
    "import specifiers are not names",
  );
  const result = reachIn({
    root,
    pkg: app,
    graph,
    changed: ["demo/core/src/x.ts"],
  });
  assert.deepEqual(result.files, ["tests/pool.test.ts"]);
});

test("a source change reaches the tests importing it, directly or not, and no test an absent import would drag in", async (t) => {
  const vitest = resolve(repository, "demo/node_modules/vitest");
  if (!existsSync(vitest)) {
    t.skip("demo/node_modules/vitest is not installed");
    return;
  }
  const root = workspace(t, {
    "demo/app/package.json": JSON.stringify({ name: "app", type: "module" }),
    "demo/app/src/x.ts": "export const x = 1;\n",
    "demo/app/src/y.ts": 'export { x as y } from "./x.js";\n',
    // Names the worker entry its build writes beside it.
    "demo/app/src/other.ts":
      'export const entry = () => new URL("./out.js", import.meta.url);\n',
    "demo/app/tests/direct.test.ts":
      'import { test } from "vitest";\nimport { x } from "../src/x.js";\ntest("x", () => x);\n',
    "demo/app/tests/transitive.test.ts":
      'import { test } from "vitest";\nimport { y } from "../src/y.js";\ntest("y", () => y);\n',
    "demo/app/tests/unrelated.test.ts":
      'import { test } from "vitest";\nimport { entry } from "../src/other.js";\ntest("entry", () => entry());\n',
    // Imports a source that is not there (deleted), and the unbuilt dist.
    "demo/app/tests/deleted.test.ts":
      'import { test } from "vitest";\nimport { z } from "../src/z.js";\ntest("z", () => z);\n',
    "demo/app/tests/dist.test.ts":
      'import { test } from "vitest";\nimport { out } from "../dist/out.js";\ntest("out", () => out);\n',
    // A worker that loads the unbuilt dist, or a module deliberately absent.
    "demo/app/tests/loader.mjs":
      'if (process.env.LOAD_ABSENT) await import("./absent.mjs");\nawait import("../dist/out.js");\n',
  });
  mkdirSync(resolve(root, "demo/node_modules"));
  symlinkSync(realpathSync(vitest), resolve(root, "demo/node_modules/vitest"));
  const directory = resolve(root, "run");
  mkdirSync(directory);
  const app = packageByName(root, "app");
  const graph = await probeGraph(
    root,
    app,
    { exclude: [], env: {} },
    { env: process.env, directory },
  );
  const result = reachIn({
    root,
    pkg: app,
    graph,
    changed: ["demo/app/src/x.ts"],
  });
  // A source change also reaches the test running the package's dist.
  assert.deepEqual(result.files, [
    "tests/direct.test.ts",
    "tests/dist.test.ts",
    "tests/transitive.test.ts",
  ]);
  assert.deepEqual(
    reachIn({ root, pkg: app, graph, changed: ["demo/app/src/other.ts"] })
      .files,
    ["tests/dist.test.ts", "tests/unrelated.test.ts"],
  );
  // Deleting a source reaches what still imports it by its JS spelling.
  assert.deepEqual(
    reachIn({ root, pkg: app, graph, changed: ["demo/app/src/z.ts"] }).files,
    ["tests/deleted.test.ts", "tests/dist.test.ts"],
  );
  // Unresolved imports resolve against their importer, and what is not
  // there is graphed as importing nothing, not as unanalysable.
  const a = at(root);
  assert.deepEqual(graph.edges[a("demo/app/tests/loader.mjs")].sort(), [
    a("demo/app/dist/out.js"),
    a("demo/app/tests/absent.mjs"),
  ]);
  assert.deepEqual(graph.failed, {});
  assert.ok(
    result.why.includes(
      "missing on disk: demo/app/src/z.js (imported by demo/app/tests/deleted.test.ts)",
    ),
    result.why,
  );
});
