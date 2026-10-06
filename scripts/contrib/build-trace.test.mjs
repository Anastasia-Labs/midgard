import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";

import { TRACER } from "./build-inputs.mjs";

// Each case runs a real Node process under the tracer, as a guarded build
// does, and reads back what it recorded.
const scratch = (t) => {
  const directory = realpathSync(
    mkdtempSync(resolve(tmpdir(), "midgard-build-trace-")),
  );
  t.after(() => rmSync(directory, { recursive: true, force: true }));
  return directory;
};
const traced = (directory, script, env = {}) => {
  const trace = resolve(directory, "reads.jsonl");
  rmSync(trace, { force: true });
  const entry = resolve(directory, script);
  const result = spawnSync(process.execPath, [entry], {
    cwd: directory,
    encoding: "utf8",
    env: {
      ...process.env,
      MIDGARD_CONTRIB_BUILD_TRACE: trace,
      NODE_OPTIONS: `--require ${JSON.stringify(TRACER)}`,
      ...env,
    },
  });
  assert.equal(result.status, 0, result.stderr);
  return existsSync(trace)
    ? readFileSync(trace, "utf8")
        .split("\n")
        .filter(Boolean)
        .map((line) => JSON.parse(line))
    : [];
};
const has = (records, kind, value) =>
  records.some(
    ([recorded, path]) =>
      recorded === kind &&
      (value instanceof RegExp ? value.test(path) : path === value),
  );

test("copies and renames record every source as a read", (t) => {
  const directory = scratch(t);
  mkdirSync(resolve(directory, "tree/nested"), { recursive: true });
  for (const file of [
    "one.txt",
    "two.txt",
    "three.txt",
    "four.txt",
    "five.txt",
  ])
    writeFileSync(resolve(directory, file), file);
  writeFileSync(resolve(directory, "tree/nested/deep.txt"), "deep");
  writeFileSync(
    resolve(directory, "copy.mjs"),
    `import fs from "node:fs";
fs.copyFileSync("one.txt", "out-one.txt");
fs.cpSync("tree", "out-tree", { recursive: true });
fs.renameSync("two.txt", "out-two.txt");
fs.linkSync("three.txt", "out-three.txt");
await fs.promises.cp("four.txt", "out-four.txt");
fs.mkdirSync("links");
fs.symlinkSync("../five.txt", "links/five.txt");
`,
  );
  const records = traced(directory, "copy.mjs");
  for (const source of [
    "one.txt",
    "tree/nested/deep.txt",
    "two.txt",
    "three.txt",
    "four.txt",
    "five.txt",
  ])
    assert.ok(has(records, "read", resolve(directory, source)), source);
  for (const destination of ["out-one.txt", "out-tree", "out-two.txt"])
    assert.ok(
      has(records, "write", resolve(directory, destination)),
      destination,
    );
});

test("a child process other than esbuild's service is recorded as untraced", (t) => {
  const directory = scratch(t);
  writeFileSync(resolve(directory, "secret.txt"), "outside");
  writeFileSync(
    resolve(directory, "spawn.mjs"),
    `import { execSync, spawnSync } from "node:child_process";
spawnSync("cat", ["secret.txt"]);
execSync("true");
`,
  );
  const records = traced(directory, "spawn.mjs");
  assert.ok(
    has(records, "untraced", /^child process spawnSync cat secret\.txt$/u),
    JSON.stringify(records),
  );
  assert.ok(has(records, "untraced", /^child process execSync true$/u));
  assert.ok(!records.some(([kind]) => kind === "service"));
});

test("ES module imports are recorded through the module hooks", (t) => {
  const directory = scratch(t);
  writeFileSync(resolve(directory, "dep.mjs"), "export const x = 1;\n");
  writeFileSync(
    resolve(directory, "main.mjs"),
    'import { x } from "./dep.mjs";\nif (x !== 1) process.exit(1);\n',
  );
  const records = traced(directory, "main.mjs");
  assert.ok(
    has(records, "read", resolve(directory, "dep.mjs")),
    JSON.stringify(records),
  );
  assert.ok(!records.some(([kind]) => kind === "untraced"));
  assert.ok(has(records, "process", resolve(directory, "main.mjs")));
});

test("only the named launcher process is exempt", (t) => {
  const directory = scratch(t);
  writeFileSync(resolve(directory, "dep.mjs"), "export const x = 1;\n");
  writeFileSync(
    resolve(directory, "main.mjs"),
    'import { x } from "./dep.mjs";\n',
  );
  assert.deepEqual(
    traced(directory, "main.mjs", {
      MIDGARD_CONTRIB_BUILD_LAUNCHER: resolve(directory, "main.mjs"),
    }),
    [],
  );
  assert.ok(
    has(
      traced(directory, "main.mjs", {
        MIDGARD_CONTRIB_BUILD_LAUNCHER: resolve(directory, "other.mjs"),
      }),
      "read",
      resolve(directory, "dep.mjs"),
    ),
  );
});

// esbuild reads sources in its own binary; the tracer forces a metafile and
// records each input, plus a marker per build, so a missing wrapper shows as
// a tsup process with no bundle rather than as a build that read nothing.
const checkout = fileURLToPath(new URL("../..", import.meta.url));
const tsup = resolve(checkout, "demo/midgard-core/node_modules/tsup");
const required = process.env.MIDGARD_REQUIRE_REAL_BUILDS === "1";
test(
  "esbuild builds report every input they bundle",
  {
    skip:
      !existsSync(tsup) && !required
        ? "demo dependencies are not installed (pnpm --dir demo install); set MIDGARD_REQUIRE_REAL_BUILDS=1 to make this a failure"
        : false,
  },
  (t) => {
    const directory = scratch(t);
    writeFileSync(resolve(directory, "outside.js"), "export const o = 1;\n");
    writeFileSync(
      resolve(directory, "entry.js"),
      'import { o } from "./outside.js";\nconsole.log(o);\n',
    );
    writeFileSync(
      resolve(directory, "bundle.cjs"),
      `const { createRequire } = require("node:module");
const esbuild = createRequire(${JSON.stringify(resolve(realpathSync(tsup), "package.json"))})("esbuild");
esbuild.buildSync({ entryPoints: ["entry.js"], bundle: true, write: false });
esbuild
  .build({ entryPoints: ["entry.js"], bundle: true, write: false })
  .then(() => esbuild.stop?.());
`,
    );
    const records = traced(directory, "bundle.cjs");
    assert.ok(
      has(records, "bundle", resolve(directory, "outside.js")),
      JSON.stringify(records),
    );
    assert.ok(has(records, "esbuild", "2"));
    assert.ok(has(records, "service", /[\\/]bin[\\/]esbuild$/u));
  },
);
