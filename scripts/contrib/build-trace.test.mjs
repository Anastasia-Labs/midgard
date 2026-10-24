import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";

import { TRACER, environmentRefusals } from "./build-inputs.mjs";
import nodeOptions from "./node-options.cjs";

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
  // Their own kind: only the hooks report a load, so a tracer without them
  // reports none, and the guard refuses a process that loaded nothing.
  for (const module of ["main.mjs", "dep.mjs"])
    assert.ok(
      has(records, "load", resolve(directory, module)),
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

test("an open reads unless it truncates, and only a successful truncating create excuses a later read", (t) => {
  const directory = scratch(t);
  for (const file of [
    "rplus.txt",
    "aplus.txt",
    "rdwr.txt",
    "blob.txt",
    "append.txt",
    "exists.txt",
  ])
    writeFileSync(resolve(directory, file), file);
  writeFileSync(
    resolve(directory, "open.mjs"),
    `import fs from "node:fs";
const { O_CREAT, O_RDWR, O_TRUNC, O_WRONLY } = fs.constants;
fs.closeSync(fs.openSync("rplus.txt", "r+"));
fs.closeSync(fs.openSync("aplus.txt", "a+"));
fs.closeSync(fs.openSync("rdwr.txt", O_RDWR));
await fs.openAsBlob("blob.txt");
fs.appendFileSync("append.txt", "more");
fs.writeFileSync("append.txt", "more", { flag: "a" });
for (const attempt of [
  () => fs.openSync("exists.txt", "wx"),
  () => fs.writeFileSync("exists.txt", "x", { flag: "wx" }),
  () => fs.copyFileSync("absent.txt", "failed-copy.txt"),
])
  try {
    attempt();
    process.exit(3);
  } catch {}
await fs.promises.writeFile("absent/out.txt", "x").catch(() => undefined);
await new Promise((done) => fs.writeFile("absent/callback.txt", "x", done));
fs.closeSync(fs.openSync("created.txt", "w"));
fs.closeSync(fs.openSync("numeric.txt", O_WRONLY | O_CREAT | O_TRUNC));
fs.writeFileSync("encoded.txt", "x", "binary");
await new Promise((done) =>
  fs.open("callback.txt", "w", (error, fd) => {
    fs.closeSync(fd);
    done();
  }),
);
fs.copyFileSync("rplus.txt", "copied.txt");
`,
  );
  const records = traced(directory, "open.mjs");
  const at = (file) => resolve(directory, file);
  for (const file of ["rplus.txt", "aplus.txt", "rdwr.txt", "blob.txt"])
    assert.ok(has(records, "read", at(file)), file);
  for (const file of [
    "created.txt",
    "numeric.txt",
    "encoded.txt",
    "callback.txt",
    "copied.txt",
  ])
    assert.ok(has(records, "write", at(file)), file);
  // An append keeps what was there; a create that failed wrote nothing.
  for (const file of [
    "append.txt",
    "exists.txt",
    "failed-copy.txt",
    "absent/out.txt",
    "absent/callback.txt",
  ])
    assert.ok(!has(records, "write", at(file)), file);
  assert.ok(!has(records, "read", at("encoded.txt")));
  assert.ok(!records.some(([kind]) => kind === "untraced"));
});

test("network use is recorded as untraced", (t) => {
  const directory = scratch(t);
  writeFileSync(
    resolve(directory, "network.mjs"),
    `import net from "node:net";
await fetch("http://127.0.0.1:1/").catch(() => undefined);
await new Promise((done) =>
  net.connect(1, "127.0.0.1").on("error", done).on("connect", done),
);
const socket = new WebSocket("ws://127.0.0.1:1/");
await new Promise((done) => socket.addEventListener("error", done));
`,
  );
  const records = traced(directory, "network.mjs");
  for (const use of [
    "network fetch",
    "network WebSocket",
    "network connection",
  ])
    assert.ok(has(records, "untraced", use), JSON.stringify(records));
});

test("a worker that overrides its options or environment is untraced", (t) => {
  const directory = scratch(t);
  writeFileSync(resolve(directory, "secret.txt"), "secret");
  writeFileSync(
    resolve(directory, "workers.mjs"),
    `import { Worker } from "node:worker_threads";
const run = (label, options) =>
  new Promise((done) =>
    new Worker(
      \`/*\${label}*/require("node:fs").readFileSync(\${JSON.stringify(process.cwd() + "/secret.txt")});\`,
      { eval: true, ...options },
    ).on("exit", done),
  );
await run("inherits", {});
await run("execArgv", { execArgv: [] });
await run("env", { env: {} });
await run("trace", { env: { ...process.env, MIDGARD_CONTRIB_BUILD_TRACE: "/dev/null" } });
delete process.env.MIDGARD_CONTRIB_BUILD_TRACE;
await run("deleted", {});
`,
  );
  const records = traced(directory, "workers.mjs");
  // A worker that inherits everything is traced like its parent.
  assert.ok(has(records, "read", resolve(directory, "secret.txt")));
  assert.ok(!has(records, "untraced", /^worker \/\*inherits/u));
  for (const label of ["execArgv", "env", "trace", "deleted"])
    assert.ok(
      has(records, "untraced", new RegExp(`^worker /\\*${label}\\*/`, "u")),
      `${label}: ${JSON.stringify(records)}`,
    );
});

test("a node option that loads other code is untraced", (t) => {
  const directory = scratch(t);
  writeFileSync(resolve(directory, "other.cjs"), "");
  writeFileSync(resolve(directory, "main.mjs"), "");
  const records = traced(directory, "main.mjs", {
    NODE_OPTIONS: `--require ${JSON.stringify(TRACER)} --require ./other.cjs`,
  });
  assert.ok(
    has(records, "untraced", "node option --require ./other.cjs"),
    JSON.stringify(records),
  );
  assert.ok(
    !traced(directory, "main.mjs").some(([kind]) => kind === "untraced"),
  );
});

test("NODE_OPTIONS splits as Node splits it, so a quoted path with a space is one argument", (t) => {
  assert.deepEqual(
    nodeOptions.nodeOptionArguments(
      '--require "/a b/c.cjs"  -r x --title="say \\"hi\\" --require y"',
    ),
    ["--require", "/a b/c.cjs", "-r", "x", '--title=say "hi" --require y'],
  );
  assert.deepEqual(
    environmentRefusals({
      NODE_OPTIONS: '--title="a --require b" --max-old-space-size=4096',
    }),
    [],
  );
  assert.deepEqual(
    environmentRefusals({ NODE_OPTIONS: '--require "/x/a b.cjs"' }),
    ["NODE_OPTIONS passes --require, which loads code the guard does not scan"],
  );
  // The tracer itself, from a checkout whose path has a space.
  const directory = scratch(t);
  const spaced = resolve(directory, "sp ace");
  mkdirSync(spaced);
  for (const file of ["build-trace.cjs", "node-options.cjs"])
    copyFileSync(resolve(dirname(TRACER), file), resolve(spaced, file));
  writeFileSync(resolve(spaced, "other.cjs"), "");
  writeFileSync(resolve(directory, "main.mjs"), "");
  const tracer = `--require ${JSON.stringify(resolve(spaced, "build-trace.cjs"))}`;
  const records = traced(directory, "main.mjs", { NODE_OPTIONS: tracer });
  assert.ok(has(records, "process", resolve(directory, "main.mjs")));
  assert.ok(
    !records.some(([kind]) => kind === "untraced"),
    JSON.stringify(records),
  );
  const other = resolve(spaced, "other.cjs");
  assert.ok(
    has(
      traced(directory, "main.mjs", {
        NODE_OPTIONS: `${tracer} --require ${JSON.stringify(other)}`,
      }),
      "untraced",
      `node option --require ${other}`,
    ),
  );
});
